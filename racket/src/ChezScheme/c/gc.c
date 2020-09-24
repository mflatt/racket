/* gc.c
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "system.h"
#include "sort.h"
#ifndef WIN32
#include <sys/wait.h>
#endif /* WIN32 */
#include "popcount.h"
#include <assert.h>

/* 
   GC Implementation
   -----------------

   The copying, sweeping, and marking operations that depend on
   object's shape are mostly implemented in "mkgc.ss". That script
   generates "gc-ocd.inc" (for modes where object counting and
   backpointers are disabled), "gc-oce.inc", and "gc-par.inc". The
   rest of the implementation here can still depend on representatoin
   details, though, especially for pairs, weak pairs, and ephemerons.

   GC Copying versus Marking
   -------------------------

   Generations range from 0 to `S_G.max_nonstatic_generation` plus a
   static generation. After an object moves to the static generation,
   it doesn't move anymore. In the case of code objects, relocations
   may be discarded when the code object moves into a static
   generation.

   For the most part, collecting generations 0 through MAX_CG (= max
   copied generation) to MIN_TG to MAX_TG (= target generation) means
   copying objects from old segments into fresh segments generations
   MIN_TG through MAX_TG. Note that MAX_TG is either the same as or
   one larger than MAX_CG. For objects in generation 0 through MAX_CG,
   the target generation is either one more than the current
   generation or it's MIN_TG.

   Objects might be marked [and swept] instead of copied [and swept]
   as triggered by two possibilities: one or more objects on the
   source segment are immobile (subsumes locked) or MAX_CG == MAX_TG
   and the object is on a MAX_CG segment that hasn't been disovered as
   sparse by a previous marking (non-copying) pass. Segments with
   marked objects are promoted to the target generation.

   As a special case, locking on `space_new` does not mark all objects
   on that segment, because dirty-write handling cannot deal with
   `space_new`; only locked objects stay on the old segment in that
   case, and they have to be marked by looking at a list of locked
   objects.

   During a collection, the `old_space` flag is set on a segment if
   objects aree being copied out of it or marked on it; that is,
   `old_space` is set if the segment starts out in one of the
   generations 0 through mgc. If a segment is being marked instead of
   copied, the `use_marks` bit is also set; note that the bit will not
   be set for a `space_new` segment, and locked objects in that space
   will be specially marked.

   Marking an object means setting a bit in `marked_mask`, which is
   allocated as needed. Any segments that ends up with a non-NULL
   `marked_mask` is kept in its new generation at the end of
   collection. If a marked object spans multiple segments, then
   `masked_mask` is created across all of the segments. It's possible
   for a segment to end up with `marked_mask` even though `use_marks`
   was not set: an marked object spanned into the segment, or it's a
   `space_new` segment with locked objects; in that case, other
   objects will be copied out of the segment, because `use_marks` is
   how relocation decides whether to copy or mark.

   If an object is copied, then its first word is set to
   `forward_marker` and its second word is set to the new address.
   Obviously, that doesn't happen if an object is marked. So, to test
   whether an object has been reached:

   * the object must be in an `old_space` segment, otherwise it counts
     as reached because it's in a generation older than MAX_CG;

   * the object either starts with `forward_marker` or its mark bit is
     set (and those are mutually exclusive).

   Besides the one bit for the start of an object in the mark mask,
   extra bits for the object content may be set as well. Those extra
   bits tell the dirty-object sweeper which words in a previously
   marked page should be swept and which should be skipped, so the
   extra bits are only needed for impure objects in certain kinds of
   spaces. Only every alternate word needs to be marked that way, so
   half of the mark bits are usually irrelevant; the exception is that
   flonums can be between normal object-start positions, so those mark
   bits can matter, at least if we're preserving `eq?` on flonums (but
   the bits are not relevant to dirty-object sweeping, since flonums
   don't have pointer fields).

   It's ok to sweep an object multiple times, but that's to be be
   avoided if possible.

   Pending Ephemerons and Guardians
   --------------------------------

   Ephemerons and guardians act as a kind of "and": an object stays
   reachable only if some other object (besdies the the
   ephemeron/guardian itself) is reachable or not. Instead of
   rechecking all guardians and ephemerons constantly, the collector
   queues pending guardians and ephemerons on the ssegment where the
   relevant object lives. If any object on that segment is discovered
   to be reachable (i.e., copied or marked), the guardian/ephemeron is
   put into a list of things to check again.

   Parallel Collection
   -------------------

   Parallel mode runs `sweep_generation` concurrently in multiple
   threads. It relies on a number of invariants:

    * There are no attempts to take tc_mutex suring sweeping. To the
      degree that locking is needed (e.g., to allocate new segments),
      `S_use_gc_tc_mutex` redirects to gc_tc_mutex. No other locks
      can be taken while that one is held.

    * To copy from or mark on a segment, a segment-specific lock must
      be taken.

      The lock must be taken before checking anything about objects on
      the page, including whether the object starts with a forwarding
      pointer. If a lock acquisition fails, everything must be
      retryable as the level of the object or segment sweep. For a
      segment sweep, objects may end up being swept multiple times.

      The lock is re-entrant, but re-locking information is held
      outside the locak in a local variable, instead of being part of
      the lock state. (That's why an ENABLE_LOCK_ACQUIRE declaration
      is required in functions that take locks.)

    * Lock acquisition must be failable everywhere, with one
      exception: when an object spans multiple segments, then `mark`
      may need to set mark bits on multiple segments. In that case, it
      can wait on locks for the extra pages, because there's an order
      for the lock-taking: the first segment's lock followed by each
      later segment.

    * A segment in the target generation is exposed to the pool of
      collecting threads only after a copy to the target segment is
      complete. That's a consequence of keeping segments to sweep in a
      thread-specific list.

    * The segment-table lock is required only for writing. When a
      thread allocates a new segment, that segment becomes relevant
      only to other threads at the point where an object in the new
      segment is exposed to the other threads. So, for example, the
      fence associated with taking a segment lock doubles to ensure
      that a write has exposed the object.

   Currently, counting and backreference modes do not support
   parallelism.

*/

/* locally defined functions */
static IGEN copy PROTO((ptr tc_in, ptr pp, seginfo *si, ptr *dest));
static IGEN mark_object PROTO((ptr tc_in, ptr pp, seginfo *si));
static void sweep PROTO((ptr tc_in, ptr p, IGEN from_g));
static void sweep_in_old PROTO((ptr tc_in, ptr p));
static void sweep_object_in_old PROTO((ptr tc_in, ptr p));
static IBOOL object_directly_refers_to_self PROTO((ptr p));
static ptr copy_stack PROTO((ptr tc_in, ptr old, iptr *length, iptr clength));
static void resweep_weak_pairs PROTO((ptr tc, seginfo *oldweakspacesegments));
static void forward_or_bwp PROTO((ptr *pp, ptr p));
static void sweep_generation PROTO((ptr tc));
static void sweep_from_stack PROTO((ptr tc));
static void enlarge_sweep_stack PROTO((ptr tc));
static uptr size_object PROTO((ptr p));
static iptr sweep_typed_object PROTO((ptr tc_in, ptr p, IGEN from_g));
static void sweep_symbol PROTO((ptr tc_in, ptr p, IGEN from_g));
static void sweep_port PROTO((ptr tc_in, ptr p, IGEN from_g));
static void sweep_thread PROTO((ptr tc_in, ptr p));
static void sweep_continuation PROTO((ptr tc_in, ptr p, IGEN from_g));
static void sweep_record PROTO((ptr tc_in, ptr x, IGEN from_g));
static IGEN sweep_dirty_record PROTO((ptr tc_in, ptr x, IGEN youngest));
static IGEN sweep_dirty_port PROTO((ptr tc_in, ptr x, IGEN youngest));
static IGEN sweep_dirty_symbol PROTO((ptr tc_in, ptr x, IGEN youngest));
static void sweep_code_object PROTO((ptr tc_in, ptr co, IGEN from_g));
static void record_dirty_segment PROTO((IGEN from_g, IGEN to_g, seginfo *si));
static void setup_sweep_dirty PROTO(());
static uptr sweep_dirty_segments PROTO((ptr tc_in, seginfo **dirty_segments));
static void resweep_dirty_weak_pairs PROTO((ptr tc));
static void mark_typemod_data_object PROTO((ptr tc_in, ptr p, uptr len, seginfo *si));
static void add_pending_guardian PROTO((ptr gdn, ptr tconc));
static void add_trigger_guardians_to_recheck PROTO((ptr ls));
static void add_ephemeron_to_pending PROTO((ptr tc, ptr p));
static void add_trigger_ephemerons_to_pending PROTO((ptr tc, ptr p));
static void check_triggers PROTO((ptr tc, seginfo *si));
static void check_ephemeron PROTO((ptr tc_in, ptr pe));
static void check_pending_ephemerons PROTO((ptr tc_in));
static int check_dirty_ephemeron PROTO((ptr tc_in, ptr pe, int youngest));
static void finish_pending_ephemerons PROTO((ptr tc, seginfo *si));
static void init_fully_marked_mask(ptr tc_in, IGEN g);
static void copy_and_clear_list_bits(ptr tc_in, seginfo *oldspacesegments);

#ifdef ENABLE_OBJECT_COUNTS
static uptr total_size_so_far();
static uptr list_length PROTO((ptr ls));
#endif
static uptr target_generation_space_so_far(ptr tc);

#ifdef ENABLE_MEASURE
static void init_measure(ptr tc, IGEN min_gen, IGEN max_gen);
static void finish_measure();
static void measure(ptr tc_in, ptr p);
static IBOOL flush_measure_stack(ptr tc_in);
static void init_measure_mask(ptr tc_in, seginfo *si);
static void init_counting_mask(ptr tc_in, seginfo *si);
static void push_measure(ptr tc_in, ptr p);
static void measure_add_stack_size(ptr stack, uptr size);
static void add_ephemeron_to_pending_measure(ptr tc, ptr pe);
static void add_trigger_ephemerons_to_pending_measure(ptr pe);
static void check_ephemeron_measure(ptr tc_in, ptr pe);
static void check_pending_measure_ephemerons(ptr tc_in);
#endif

#ifdef ENABLE_PARALLEL
/* # define ENABLE_TIMING */
#endif

#ifdef ENABLE_TIMING
#include <sys/time.h>
/* gets milliseconds of real time (not CPU time) */
static uptr get_time () {
  struct timeval now;
  gettimeofday(&now, NULL);
  return ((uptr) now.tv_sec) * 1000 + ((uptr) now.tv_usec) / 1000;
}
# define GET_TIME(x) uptr x = get_time()
# define ACCUM_TIME(a, y, x) uptr y = get_time() - x; a += y
# define REPORT_TIME(e) e
static uptr collect_accum, all_accum, par_accum;
#else
# define GET_TIME(x) do { } while (0)
# define ACCUM_TIME(a, y, x) do { } while (0)
# define REPORT_TIME(e) do { } while (0)
#endif

#if defined(MIN_TG) && defined(MAX_TG)
# if MIN_TG == MAX_TG
#  define NO_DIRTY_NEWSPACE_POINTERS
# endif
#endif

#define main_sweeper_index maximum_parallel_collect_threads

/* Use the `REMOTESWEEPER` field to assign the sweeper before sweepers
   actually start: */
#define WILL_BE_SWEEPER(tc) REMOTESWEEPER(tc)

/* #define DEBUG */

/* initialized and used each gc cycle.  any others should be defined in globals.h */
static ptr tlcs_to_rehash;
static ptr conts_to_promote;
static ptr recheck_guardians_ls;
static seginfo *resweep_weak_segments;

#ifdef ENABLE_OBJECT_COUNTS
static int measure_all_enabled;
static uptr count_root_bytes;
#endif

/* max_cg: maximum copied generation, i.e., maximum generation subject to collection.  max_cg >= 0 && max_cg <= static_generation.
 * min_tg: minimum target generation.  max_tg == 0 ? min_tg == 0 : min_tg > 0 && min_tg <= max_tg;
 * max_tg: maximum target generation.  max_tg == max_cg || max_tg == max_cg + 1.
 * Objects in generation g are collected into generation MIN(max_tg, MAX(min_tg, g+1)).
 */
#if defined(MAX_CG) && defined(MIN_TG) && defined(MAX_TG)
#else
static IGEN MAX_CG, MIN_TG, MAX_TG;
#endif

#if defined(MIN_TG) && defined(MAX_TG) && (MIN_TG == MAX_TG)
# define TARGET_GENERATION(si) MIN_TG
# define compute_target_generation(g) MIN_TG
# define CONSTANT_TARGET_GENERATION
#else
# define TARGET_GENERATION(si) si->generation
FORCEINLINE IGEN compute_target_generation(IGEN g) {
  return g == MAX_TG ? g : g < MIN_TG ? MIN_TG : g + 1;
}
#endif

static octet *fully_marked_mask[static_generation+1];

#define push_sweep(p) {                                                 \
    if (SWEEPSTACK(tc_in) == SWEEPSTACKLIMIT(tc_in)) enlarge_sweep_stack(tc_in); \
    *(ptr *)TO_VOIDP(SWEEPSTACK(tc_in)) = p;                            \
    SWEEPSTACK(tc_in) = (ptr)((uptr)SWEEPSTACK(tc_in) + ptr_bytes);    \
  }

#ifdef ENABLE_MEASURE
static uptr measure_total; /* updated by `measure` */
static IGEN min_measure_generation, max_measure_generation;
static ptr *measure_stack_start, *measure_stack, *measure_stack_limit;
static ptr measured_seginfos;
static ptr pending_measure_ephemerons;
#endif

#ifdef ENABLE_BACKREFERENCE
static ptr sweep_from;
# define BACKREFERENCES_ENABLED S_G.enable_object_backreferences
# define SET_SWEEP_FROM(p) if (S_G.enable_object_backreferences) sweep_from = p
# define WITH_TOP_BACKREFERENCE(v, e) SET_SWEEP_FROM(v); e; SET_SWEEP_FROM(Sfalse)
# define SET_BACKREFERENCE(p) sweep_from = p;
# define PUSH_BACKREFERENCE(p) ptr old_sweep_from = sweep_from; SET_SWEEP_FROM(p);
# define POP_BACKREFERENCE() SET_SWEEP_FROM(old_sweep_from);
# define ADD_BACKREFERENCE_FROM(p, from_p, tg) do { \
    IGEN TG = tg;                                                       \
    if ((S_G.enable_object_backreferences) && (TG < static_generation)) \
      S_G.gcbackreference[TG] = S_cons_in(tc_in, space_impure, TG,       \
                                          S_cons_in(tc_in, space_impure, TG, p, from_p), \
                                          S_G.gcbackreference[TG]);     \
  } while (0)
# define ADD_BACKREFERENCE(p, tg) ADD_BACKREFERENCE_FROM(p, sweep_from, tg)
#else
# define BACKREFERENCES_ENABLED 0
# define WITH_TOP_BACKREFERENCE(v, e) e
# define SET_BACKREFERENCE(p)
# define PUSH_BACKREFERENCE(p)
# define POP_BACKREFERENCE()
# define ADD_BACKREFERENCE_FROM(p, from_p, from_g)
# define ADD_BACKREFERENCE(p, from_g)
#endif

typedef struct remote_range {
  ISPC s;
  IGEN g;
  ptr start, end;
  struct remote_range *next;
} remote_range;

#if !defined(PTHREADS)
# undef ENABLE_PARALLEL
#endif

#ifdef ENABLE_PARALLEL

# define GC_TC_MUTEX_ACQUIRE() gc_tc_mutex_acquire()
# define GC_TC_MUTEX_RELEASE() gc_tc_mutex_release()

# define GET_LOCALRANGES_AT(tc, s, g) LOCALRANGES_AT(tc, s, g) 
# define SEGMENT_IS_LOCAL(si, p) ((SWEEPER(si->creator_tc) == SWEEPER(tc_in)) || marked(si, p))
# define RECORD_REMOTE_RANGE_TO(tc, start, size, sweeper) do { \
    ptr START = TO_PTR(UNTYPE_ANY(start));                     \
    ptr END = (ptr)((uptr)START + (size));                     \
    /* REMOVEME printf("@%d: %p-%p\n", (int)sweeper, START, END); */    \
    if ((uptr)START < (uptr)REMOTERANGESTART(tc))              \
      REMOTERANGESTART(tc) = START;                            \
    if ((uptr)END > (uptr)REMOTERANGEEND(tc))                  \
      REMOTERANGEEND(tc) = END;                                \
    REMOTESWEEPER(tc) = sweeper;                               \
  } while (0)
# define RECORD_REMOTE_RANGE(tc, start, size, si) RECORD_REMOTE_RANGE_TO(tc, start, size, SWEEPER(si->creator_tc))
# define FLUSH_REMOTE_RANGE(tc, s, g) do {        \
    if (REMOTERANGESTART(tc) != (ptr)(uptr)-1) {  \
      flush_remote_range(tc, s, g);               \
    }                                             \
  } while (0)

static void gather_active_sweepers();
static void reassign_segment_creator(ptr tc, seginfo *si);
static IBOOL sweeper_started(int i);
static void parallel_sweep_dirty_and_generation(ptr tc);
static iptr sweep_generation_trading_work(ptr tc);

static void flush_remote_range(ptr tc, ISPC s, IGEN g);
static remote_range *send_and_receive_remote_ranges(ptr tc);

#define SWEEPER_NONE             0
#define SWEEPER_READY            1
#define SWEEPER_SWEEPING         2
#define SWEEPER_WAITING_FOR_WORK 3

typedef struct {
  int status;
  s_thread_cond_t done_cond, work_cond;
  ptr sweep_tc;
  ptr thread; /* not 0 => thread to sweep on start */
  seginfo *dirty_segments[DIRTY_SEGMENT_LISTS];
  /* modified only by owning sweeper: */
  remote_range *ranges_to_send[maximum_parallel_collect_threads+1];
  /* modified with sweeper mutex held: */
  remote_range *ranges_received;
} gc_thread_data;

static gc_thread_data sweepers[maximum_parallel_collect_threads+1];
static int num_sweepers;

static seginfo *main_dirty_segments[DIRTY_SEGMENT_LISTS];

#else

# define GC_TC_MUTEX_ACQUIRE() do { } while (0)
# define GC_TC_MUTEX_RELEASE() do { } while (0)

# define GET_LOCALRANGES_AT(tc, s, g) NULL
# define SEGMENT_IS_LOCAL(si, p) 1
# define RECORD_REMOTE_RANGE_TO(tc, start, size, sweeper) do { } while (0)
# define RECORD_REMOTE_RANGE(tc, start, size, si) do { } while (0)
# define FLUSH_REMOTE_RANGE(tc, s, g) do { } while (0)

# define gather_active_sweepers() do { } while (0)
# define reassign_segment_creator(tc, si) do { } while (0)
# define parallel_sweep_dirty_and_generation(tc) do { sweep_dirty(tc); sweep_generation(tc); } while (0)
# define send_and_receive_remote_ranges(tc) NULL
static void sweep_dirty PROTO((ptr tc));

#endif

#define SWEEP_NO_CHANGE        0
#define SWEEP_CHANGE_PROGRESS  1

#if ptr_alignment == 2
# define record_full_marked_mask 0x55
# define record_high_marked_bit  0x40
# define mask_bits_to_list_bits_mask(m) ((m) | ((m) << 1))
#elif ptr_alignment == 1
# define record_full_marked_mask 0xFF
# define record_high_marked_bit  0x80
# define mask_bits_to_list_bits_mask(m) (m)
#endif

#define segment_sufficiently_compact_bytes ((bytes_per_segment * 3) / 4)
#define chunk_sufficiently_compact(nsegs) ((nsegs) >> 2)
  
/* Values for a guardian entry's `pending` field when it's added to a
   seginfo's pending list: */
enum {
  GUARDIAN_PENDING_HOLD,
  GUARDIAN_PENDING_FINAL
};

#ifdef ENABLE_OBJECT_COUNTS
uptr list_length(ptr ls) {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}
#endif

#define init_mask(tc, dest, tg, init) do {                                  \
    octet *MASK;                                                        \
    find_room_voidp(tc, space_data, tg, ptr_align(segment_bitmap_bytes), MASK); \
    memset(MASK, init, segment_bitmap_bytes);                           \
    STORE_FENCE();                                                      \
    dest = MASK;                                                        \
    BITMASKOVERHEAD(tc, tg) += ptr_align(segment_bitmap_bytes);         \
  } while (0)

#define marked(si, p) (si->marked_mask && (si->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))

#ifdef NO_NEWSPACE_MARKS
# define new_marked(si, p) 0
# define CAN_MARK_AND(x) 0
#else
# define new_marked(si, p) marked(si, p)
# define CAN_MARK_AND(x) x
#endif

static void init_fully_marked_mask(ptr tc_in, IGEN g) {
  GC_TC_MUTEX_ACQUIRE();
  if (!fully_marked_mask[g]) {
    init_mask(tc_in, fully_marked_mask[g], g, 0xFF);
  }
  GC_TC_MUTEX_RELEASE();
}

#ifdef PRESERVE_FLONUM_EQ

static void flonum_set_forwarded(ptr tc_in, ptr p, seginfo *si) {
  if (!si->forwarded_flonums)
    init_mask(tc_in, si->forwarded_flonums, 0, 0);
  si->forwarded_flonums[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
}

static int flonum_is_forwarded_p(ptr p, seginfo *si) {
  if (!si->forwarded_flonums)
    return 0;
  else
    return si->forwarded_flonums[segment_bitmap_byte(p)] & segment_bitmap_bit(p);
}

# define FLONUM_FWDADDRESS(p) *(ptr*)TO_VOIDP(UNTYPE(p, type_flonum))

# define FORWARDEDP(p, si) ((TYPEBITS(p) == type_flonum) ? flonum_is_forwarded_p(p, si) : (FWDMARKER(p) == forward_marker))
# define GET_FWDADDRESS(p) ((TYPEBITS(p) == type_flonum) ? FLONUM_FWDADDRESS(p) : FWDADDRESS(p))
#else
# define FORWARDEDP(p, si) (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum)
# define GET_FWDADDRESS(p) FWDADDRESS(p)
#endif

#ifdef ENABLE_OBJECT_COUNTS
# define ELSE_MEASURE_NONOLDSPACE(p) \
  else if (measure_all_enabled)      \
    push_measure(tc_in, p);
#else
# define ELSE_MEASURE_NONOLDSPACE(p) /* empty */
#endif

/* use relocate_pure for newspace fields that can't point to younger
   objects or where there's no need to track generations */

#define relocate_pure(ppp, start, size) do {    \
    ptr* PPP = ppp; ptr PP = *PPP;              \
    relocate_pure_help(PPP, PP, start, size);   \
  } while (0)

#define relocate_pure_help(ppp, pp, start, size) do {   \
    seginfo *SI;                             \
    if (!IMMEDIATE(pp) && (SI = MaybeSegInfo(ptr_get_segment(pp))) != NULL) {  \
      if (SI->old_space)                      \
        relocate_pure_help_help(ppp, pp, SI, start, size);   \
      ELSE_MEASURE_NONOLDSPACE(pp)            \
    }                                         \
  } while (0)

#define relocate_pure_help_help(ppp, pp, si, start, size) do {       \
    if (SEGMENT_IS_LOCAL(si, pp)) {                                  \
      if (FORWARDEDP(pp, si))                        \
        *ppp = GET_FWDADDRESS(pp);                   \
      else if (!new_marked(si, pp))                  \
        mark_or_copy_pure(ppp, pp, si);              \
    } else                                           \
      RECORD_REMOTE_RANGE(tc_in, start, size, si);   \
  } while (0)

#define relocate_code(pp, si, start, size) do { \
    if (si->old_space) {                        \
      if (SEGMENT_IS_LOCAL(si, pp)) {           \
        if (FWDMARKER(pp) == forward_marker)    \
          pp = GET_FWDADDRESS(pp);              \
        else if (!new_marked(si, pp))           \
          mark_or_copy_pure(&pp, pp, si);       \
      } else                                    \
        RECORD_REMOTE_RANGE(tc_in, start, size, si);    \
    } ELSE_MEASURE_NONOLDSPACE(pp)              \
  } while (0)

#define mark_or_copy_pure(dest, p, si) do {   \
    if (CAN_MARK_AND(si->use_marks))          \
      (void)mark_object(tc_in, p, si);        \
    else                                      \
      (void)copy(tc_in, p, si, dest);         \
  } while (0)


/* use relocate_impure for newspace fields that can point to younger objects */

#ifdef NO_DIRTY_NEWSPACE_POINTERS

# define relocate_impure_help(PPP, PP, FROM_G, start, size) do {(void)FROM_G; relocate_pure_help(PPP, PP, start, size);} while (0)
# define relocate_impure(PPP, FROM_G, start, size) do {(void)FROM_G; relocate_pure(PPP, start, size);} while (0)

#else /* !NO_DIRTY_NEWSPACE_POINTERS */

#define relocate_impure(ppp, from_g, start, size) do {          \
    ptr* PPP = ppp; ptr PP = *PPP; IGEN FROM_G = from_g;        \
    relocate_impure_help(PPP, PP, FROM_G, start, size);         \
  } while (0)

#define relocate_impure_help(ppp, pp, from_g, start, size) do {         \
    seginfo *SI;                                                        \
    if (!IMMEDIATE(pp) && (SI = MaybeSegInfo(ptr_get_segment(pp))) != NULL) { \
      if (SI->old_space)                                                \
        relocate_impure_help_help(ppp, pp, from_g, SI, start, size);    \
      ELSE_MEASURE_NONOLDSPACE(pp)                                      \
    }                                                                   \
  } while (0)

#define relocate_impure_help_help(ppp, pp, from_g, si, start, size) do { \
    IGEN __to_g;                                                        \
    if (SEGMENT_IS_LOCAL(si, pp)) {                                     \
      if (FORWARDEDP(pp, si)) {                                         \
        *ppp = GET_FWDADDRESS(pp);                                      \
        __to_g = TARGET_GENERATION(si);                                 \
      } else if (!new_marked(si, pp)) {                                 \
        mark_or_copy_impure(__to_g, ppp, pp, from_g, si);               \
      } else {                                                          \
        __to_g = TARGET_GENERATION(si);                                 \
      }                                                                 \
      if (__to_g < from_g) S_record_new_dirty_card(tc_in, ppp, __to_g); \
    } else                                                              \
      RECORD_REMOTE_RANGE(tc_in, start, size, si);                      \
  } while (0)

#define mark_or_copy_impure(to_g, dest, p, from_g, si) do {      \
    if (CAN_MARK_AND(si->use_marks))                             \
      to_g = mark_object(tc_in, p, si);                          \
    else                                                         \
      to_g = copy(tc_in, p, si, dest);                           \
  } while (0)

#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

#define relocate_dirty(PPP, YOUNGEST, start, size) do {                 \
    seginfo *_si; ptr *_ppp = PPP, _pp = *_ppp; IGEN _pg;               \
    if (!IMMEDIATE(_pp) && (_si = MaybeSegInfo(ptr_get_segment(_pp))) != NULL) { \
      if (!_si->old_space) {                                            \
        _pg = _si->generation;                                          \
      } else {                                                          \
        if (SEGMENT_IS_LOCAL(_si, _pp)) {                               \
          if (FORWARDEDP(_pp, _si)) {                                   \
            *_ppp = GET_FWDADDRESS(_pp);                                \
            _pg = TARGET_GENERATION(_si);                               \
          } else if (new_marked(_si, _pp)) {                            \
            _pg = TARGET_GENERATION(_si);                               \
          } else if (CAN_MARK_AND(_si->use_marks)) {                    \
            _pg = mark_object(tc_in, _pp, _si);                         \
          } else {                                                      \
            _pg = copy(tc_in, _pp, _si, _ppp);                          \
          }                                                             \
        } else {                                                        \
          RECORD_REMOTE_RANGE(tc_in, start, size, _si);                 \
          _pg = 0xff;                                                   \
        }                                                               \
      }                                                                 \
      if (_pg < YOUNGEST) YOUNGEST = _pg;                               \
    }                                                                   \
  } while (0)

#ifdef ENABLE_OBJECT_COUNTS
# define is_counting_root(si, p) (si->counting_mask && (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))
#endif

#ifdef ENABLE_PARALLEL
static void do_relocate_indirect(ptr tc_in, ptr p, ptr* start, uptr len) {
  relocate_pure(&p, start, len);
}
# define relocate_indirect(p, start, len) do_relocate_indirect(tc_in, p, start, len)
#else
static void do_relocate_indirect(ptr tc_in, ptr p) {
  relocate_pure(&p, NULL, 0);
}
# define relocate_indirect(p, start, len) do_relocate_indirect(tc_in, p)
#endif

FORCEINLINE void check_triggers(ptr tc_in, seginfo *si) {
  /* Registering ephemerons and guardians to recheck at the
     granularity of a segment means that the worst-case complexity of
     GC is quadratic in the number of objects that fit into a segment
     (but that only happens if the objects are ephemeron keys that are
     reachable just through a chain via the value field of the same
     ephemerons). */
  if (si->has_triggers) {
    if (si->trigger_ephemerons) {
      add_trigger_ephemerons_to_pending(tc_in, si->trigger_ephemerons);
      si->trigger_ephemerons = 0;
    }
    if (si->trigger_guardians) {
      add_trigger_guardians_to_recheck(si->trigger_guardians);
      si->trigger_guardians = 0;
    }
    si->has_triggers = 0;
  }
}

#if defined(ENABLE_PARALLEL)
# define SIZE 0
# include "gc-par.inc"
#elif !defined(ENABLE_OBJECT_COUNTS)
# include "gc-ocd.inc"
#else
# include "gc-oce.inc"
#endif

/* sweep_in_old() is like sweep(), but the goal is to sweep the
   object's content without copying the object itself, so we're sweep
   an object while it's still in old space. If an object refers back
   to itself, naively sweeping might copy the object while we're
   trying to sweep the old copy, which interacts badly with the words
   set to a forwarding marker and pointer. To handle that problem,
   sweep_in_old() is allowed to copy the object, since the object
   is going to get copied anyway. */
static void sweep_in_old(ptr tc_in, ptr p) {
  /* Detect all the cases when we need to give up on in-place
     sweeping: */
  if (object_directly_refers_to_self(p)) {
    relocate_pure(&p, NULL, 0);
    return;
  }

  /* We've determined that `p` won't refer immediately back to itself,
     so it's ok to sweep(), but only update `p` for pure relocations;
     impure oness must that will happen later, after `p` is
     potentially copied, so the card updates will be right. */
  sweep_object_in_old(tc_in, p);
}

static void sweep_dirty_object_if_space_new(ptr tc_in, ptr p) {
  seginfo *si = SegInfo(ptr_get_segment(p));
  if (si->space == space_new)
    (void)sweep_dirty_object(tc_in, p, 0);
}

static ptr copy_stack(ptr tc_in, ptr old, iptr *length, iptr clength) {
  iptr n, m; ptr new; IGEN newg;
  seginfo *si = SegInfo(ptr_get_segment(old));

  /* Don't copy non-oldspace stacks, since we may be sweeping a
     continuation that is older than target_generation.  Doing so would
     be a waste of work anyway. */
  if (!si->old_space) return old;

  newg = TARGET_GENERATION(si);

  n = *length;

#ifndef NO_NEWSPACE_MARKS
  if (si->use_marks) {
    if (!marked(si, old)) {
      mark_typemod_data_object(tc_in, old, n, si);
    
#ifdef ENABLE_OBJECT_COUNTS
      S_G.countof[newg][countof_stack] += 1;
      S_G.bytesof[newg][countof_stack] += n;
#endif
    }

    return old;
  }
#endif

  /* reduce headroom created for excessively large frames (typically resulting from apply with long lists) */
  if (n != clength && n > default_stack_size && n > (m = clength + one_shot_headroom)) {
    *length = n = m;
  }

  n = ptr_align(n);
#ifdef ENABLE_OBJECT_COUNTS
  S_G.countof[newg][countof_stack] += 1;
  S_G.bytesof[newg][countof_stack] += n;
#endif /* ENABLE_OBJECT_COUNTS */

  if (n == 0) {
    return (ptr)0;
  } else {
    find_room(tc_in, space_data, newg, typemod, n, new);
    n = ptr_align(clength);
    /* warning: stack may have been left non-double-aligned by split_and_resize */
    memcpy_aligned(TO_VOIDP(new), TO_VOIDP(old), n);

    /* also returning possibly updated value in *length */
    return new;
  }
}

#define NONSTATICINHEAP(si, x) (!IMMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && si->generation != static_generation)
#define ALWAYSTRUE(si, x) (si = SegInfo(ptr_get_segment(x)), 1)
#define partition_guardians(LS, FILTER) do {                    \
    ptr ls; seginfo *si;                                        \
    for (ls = LS; ls != Snil; ls = next) {                      \
      obj = GUARDIANOBJ(ls);                                    \
      next = GUARDIANNEXT(ls);                                  \
      if (FILTER(si, obj)) {                                    \
        if (!si->old_space || new_marked(si, obj)) {            \
          INITGUARDIANNEXT(ls) = pend_hold_ls;                  \
          pend_hold_ls = ls;                                    \
        } else if (FORWARDEDP(obj, si)) {                       \
          INITGUARDIANOBJ(ls) = GET_FWDADDRESS(obj);            \
          INITGUARDIANNEXT(ls) = pend_hold_ls;                  \
          pend_hold_ls = ls;                                    \
        } else {                                                \
          seginfo *t_si;                                        \
          tconc = GUARDIANTCONC(ls);                            \
          t_si = SegInfo(ptr_get_segment(tconc));               \
          if (!t_si->old_space || new_marked(t_si, tconc)) {    \
            INITGUARDIANNEXT(ls) = final_ls;                    \
            final_ls = ls;                                      \
          } else if (FWDMARKER(tconc) == forward_marker) {      \
            INITGUARDIANTCONC(ls) = FWDADDRESS(tconc);          \
            INITGUARDIANNEXT(ls) = final_ls;                    \
            final_ls = ls;                                      \
          } else {                                              \
            INITGUARDIANNEXT(ls) = pend_final_ls;               \
            pend_final_ls = ls;                                 \
          }                                                     \
        }                                                       \
      }                                                         \
    }                                                           \
  } while (0)

typedef struct count_root_t {
  ptr p;
  IBOOL weak;
} count_root_t;

ptr GCENTRY(ptr tc_in, ptr count_roots_ls) {
    ptr tc = tc_in;
    IGEN g; ISPC s;
    seginfo *oldspacesegments, *oldweakspacesegments, *si, *nextsi;
    ptr ls;
    bucket_pointer_list *buckets_to_rebuild;
    uptr pre_finalization_size, pre_phantom_bytes;
#ifdef ENABLE_OBJECT_COUNTS
    ptr count_roots_counts = Snil;
    iptr count_roots_len;
    count_root_t *count_roots;
#endif

    GET_TIME(astart);

   /* flush instruction cache: effectively clear_code_mod but safer */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr t_tc = (ptr)THREADTC(Scar(ls));
      S_flush_instruction_cache(t_tc);
    }

    tlcs_to_rehash = Snil;
    conts_to_promote = Snil;
#ifndef NO_DIRTY_NEWSPACE_POINTERS
    S_G.new_dirty_cards = NULL;
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */
    S_G.must_mark_gen0 = 0;

    /* map `tc`s of rendezvous threads to sweeping threads, setting WILL_BE_SWEEPER */
    gather_active_sweepers();

    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr t_tc = (ptr)THREADTC(Scar(ls));
      S_scan_dirty(TO_VOIDP(EAP(t_tc)), TO_VOIDP(REAL_EAP(t_tc)));
      EAP(t_tc) = REAL_EAP(t_tc) = AP(t_tc) = (ptr)0;

      /* Initially, map every context to the same sweeper, so
         we can relocate some initial objects */
      SWEEPER(t_tc) = main_sweeper_index;

      /* If WILL_BE_SWEEPER() is not already set right, set it to
         `main_sweeper_index`. */
#ifdef ENABLE_PARALLEL
      if (WILL_BE_SWEEPER(t_tc) != main_sweeper_index) {
        if ((WILL_BE_SWEEPER(t_tc) >= num_sweepers)
            || (sweepers[WILL_BE_SWEEPER(t_tc)].sweep_tc != t_tc))
          WILL_BE_SWEEPER(t_tc) = main_sweeper_index;
      }
#else
      WILL_BE_SWEEPER(t_tc) = main_sweeper_index;
#endif

      /* clear thread-local allocation: */
      for (g = 0; g <= MAX_CG; g++) {
        for (s = 0; s <= max_real_space; s++) {
          if (BASELOC_AT(t_tc, s, g)) {
            /* We close off, instead of just setting BASELOC to 0,
               in case the page ends up getting marked, in which
               case a terminator mark needed. */
            S_close_off_thread_local_segment(t_tc, s, g);
          }
        }
      }

      if ((t_tc != tc) && (WILL_BE_SWEEPER(t_tc) == main_sweeper_index)) {
        /* close off any current allocation in MAX_TG, and ensure that
           end-of-segment markers are otherwise set (in case that's
           needed for dirty-byte sweeping) */
        for (s = 0; s <= max_real_space; s++) {
          if (BASELOC_AT(t_tc, s, MAX_TG))
            S_close_off_thread_local_segment(t_tc, s, MAX_TG);
          for (g = MAX_TG + 1; g <= static_generation; g++) {
            ptr old = NEXTLOC_AT(t_tc, s, g);
            if (old != (ptr)0)
              *(ptr*)TO_VOIDP(old) = forward_marker;
          }
        }
      } else {
        /* set up context for sweeping --- effectively remembering the current
           allocation state so anything new is recognized as needing sweeping */
        SWEEPSTACKSTART(t_tc) = SWEEPSTACK(t_tc) = SWEEPSTACKLIMIT(t_tc) = (ptr)0;
        BITMASKOVERHEAD(t_tc, 0) = 0;
        for (g = MIN_TG; g <= MAX_TG; g++)
          BITMASKOVERHEAD(t_tc, g) = 0;
        for (s = 0; s <= max_real_space; s++) {
          /* need to save `NEXTLOC_AT` to ensure that dirty sweeping
             doesn't overshoot into newly allocated objects */
          ORIGNEXTLOC(t_tc, s) = NEXTLOC_AT(t_tc, s, MAX_TG);
          SWEEPLOC_AT(t_tc, s, MAX_TG) = NEXTLOC_AT(t_tc, s, MAX_TG);
          for (g = MIN_TG; g <= MAX_TG; g++)
            SWEEPNEXT_AT(t_tc, s, g) = (ptr)0;
        }
        /* for t_tc != tc, we add terminators to allocation pages just
           before sweep_dirty() */
      }
    }

   /* perform after ScanDirty */
    if (S_checkheap) S_check_heap(0, MAX_CG);

#ifdef DEBUG
(void)printf("max_cg = %x;  go? ", MAX_CG); (void)fflush(stdout); (void)getc(stdin);
#endif

    resweep_weak_segments = NULL;
    for (g = MIN_TG; g <= MAX_TG; g++) fully_marked_mask[g] = NULL;

  /* set up generations to be copied */
    for (g = 0; g <= MAX_CG; g++) {
      S_G.bytes_of_generation[g] = 0;
      for (s = 0; s <= max_real_space; s++) {
        S_G.bytes_of_space[g][s] = 0;
        S_G.bitmask_overhead[g] = 0;
      }
    }

  /* reset phantom size in generations to be copied, even if counting is not otherwise enabled */
    pre_phantom_bytes = 0;
    for (g = 0; g <= MAX_CG; g++) {
      pre_phantom_bytes += S_G.bytesof[g][countof_phantom];
      S_G.bytesof[g][countof_phantom] = 0;
    }
    for (g = MIN_TG; g <= MAX_TG; g++) {
      pre_phantom_bytes += S_G.bytesof[g][countof_phantom];
    }

  /* mark segments from which objects are to be copied or marked */
    oldspacesegments = oldweakspacesegments = (seginfo *)NULL;
    for (g = 0; g <= MAX_CG; g += 1) {
      IBOOL maybe_mark = ((g >= S_G.min_mark_gen) && (g >= MIN_TG));
      for (s = 0; s <= max_real_space; s += 1) {
        seginfo *saved;

        if (s == space_weakpair) {
          saved = oldspacesegments;
          oldspacesegments = oldweakspacesegments;
        } else
          saved = NULL;

        for (si = S_G.occupied_segments[g][s]; si != NULL; si = nextsi) {
          nextsi = si->next;
          si->next = oldspacesegments;
          oldspacesegments = si;
          si->old_space = 1;
          /* update generation now, both to compute the target generation,
             and so that any updated dirty references will record the correct
             new generation; also used for a check in S_dirty_set */
          si->generation = compute_target_generation(si->generation);
          if (si->must_mark
              || (maybe_mark
                  && (!si->marked_mask
                      || (si->marked_count >= segment_sufficiently_compact_bytes))
                  && (si->chunk->nused_segs >= chunk_sufficiently_compact(si->chunk->segs)))) {
            if (s != space_new) /* only lock-based marking is allowed on space_new */
              si->use_marks = 1;
          }
          si->marked_mask = NULL; /* clear old mark bits, if any */
          si->marked_count = 0;
          si->min_dirty_byte = 0; /* prevent registering as dirty while GCing */
          reassign_segment_creator(tc, si);
        }
        S_G.occupied_segments[g][s] = NULL;

        if (s == space_weakpair) {
          oldweakspacesegments = oldspacesegments;
          oldspacesegments = saved;
        }
      }
    }
    if (oldweakspacesegments) {
      /* make oldweakspacesegments a prefix of weakspacesegments */
      seginfo *p;
      for (p = oldweakspacesegments; p->next; p = p->next);
      p->next = oldspacesegments;
      oldspacesegments = oldweakspacesegments;
    }

#ifdef ENABLE_OBJECT_COUNTS
   /* clear object counts & bytes for copied generations; bump timestamp */
   {INT i;
    for (g = 0; g <= MAX_CG; g += 1) {
      for (i = 0; i < countof_types; i += 1) {
        S_G.countof[g][i] = 0;
        S_G.bytesof[g][i] = 0;
      }
      if (g == 0) {
        S_G.gctimestamp[g] += 1;
      } else {
        S_G.gctimestamp[g] = S_G.gctimestamp[0];
      }
    }
   }
#endif /* ENABLE_OBJECT_COUNTS */

   /* Clear any backreference lists for copied generations */
   for (g = 0; g <= MAX_CG; g += 1) {
      S_G.gcbackreference[g] = Snil;
   }

   SET_BACKREFERENCE(Sfalse) /* #f => root */

    /* Set mark bit for any locked object in `space_new`. Don't sweep until
       after handling counting roots. Note that the segment won't have
       `use_marks` set, so non-locked objects will be copied out. */
     for (g = 0; g <= MAX_CG; g += 1) {
       IGEN tg = compute_target_generation(g);
       for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
         ptr p = Scar(ls);
         seginfo *si = SegInfo(ptr_get_segment(p));
         if (si->space == space_new) {
           if (!si->marked_mask)
             init_mask(tc, si->marked_mask, tg, 0);
           si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
         }
       }
     }
   
#ifdef ENABLE_OBJECT_COUNTS
  /* set flag on count_roots objects so they get copied to space_count_root */
     if (count_roots_ls != Sfalse) {
       iptr i;

       count_roots_len = list_length(count_roots_ls);
       find_room_voidp(tc, space_data, 0, ptr_align(count_roots_len*sizeof(count_root_t)), count_roots);

       for (ls = count_roots_ls, i = 0; ls != Snil; ls = Scdr(ls), i++) {
         ptr p = Scar(ls);
         if (IMMEDIATE(p)) {
           count_roots[i].p = p;
           count_roots[i].weak = 0;
         } else {
           seginfo *ls_si = SegInfo(ptr_get_segment(ls));
           seginfo *si = SegInfo(ptr_get_segment(p));

           if (!si->counting_mask)
             init_counting_mask(tc, si);

           si->counting_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);

           count_roots[i].p = p;
           count_roots[i].weak = ((ls_si->space == space_weakpair)
                                  || (ls_si->space == space_ephemeron));
         }
       }
     } else {
       count_roots_len = 0;
       count_roots = NULL;
     }
#endif

#ifdef ENABLE_OBJECT_COUNTS
  /* sweep count_roots in order and accumulate counts */
     if (count_roots_len > 0) {
       ptr prev = 0; uptr prev_total = total_size_so_far();
       iptr i;

# ifdef ENABLE_MEASURE
       init_measure(tc, MAX_TG+1, static_generation);
# endif

       for (i = 0; i < count_roots_len; i++) {
         uptr total;
         ptr p = count_roots[i].p;
         if (IMMEDIATE(p)) {
           /* nothing to do */
         } else {
           seginfo *si = SegInfo(ptr_get_segment(p));

           si->counting_mask[segment_bitmap_byte(p)] -= segment_bitmap_bit(p);

           if (!si->old_space || FORWARDEDP(p, si) || marked(si, p)
               || !count_roots[i].weak) {
             /* reached or older; sweep transitively */
             relocate_pure(&p, NULL, 0);
             sweep(tc, p, TARGET_GENERATION(si));
             ADD_BACKREFERENCE(p, si->generation);
             sweep_generation(tc);
# ifdef ENABLE_MEASURE
             while (flush_measure_stack(tc)) {
               sweep_generation(tc);
             }
# endif

             /* now count this object's size, if we have deferred it before */
             si = SegInfo(ptr_get_segment(p));
             if ((si->space == space_count_pure) || (si->space == space_count_impure))
               count_root_bytes -= size_object(p);
           }
         }

         total = total_size_so_far();
         p = S_cons_in(tc, space_new, 0, FIX(total-prev_total), Snil);
         if (prev != 0)
           Scdr(prev) = p;
         else
           count_roots_counts = p;
         prev = p;
         prev_total = total;
       }

# ifdef ENABLE_MEASURE
       finish_measure();
# endif

       /* clear `counting_mask`s */
       for (i = 0; i < count_roots_len; i++) {
         ptr p = count_roots[i].p;
         if (!IMMEDIATE(p)) {
           seginfo *si = SegInfo(ptr_get_segment(p));
           si->counting_mask = NULL;
         }
       }
     }
#endif

    /* Gather and mark all younger locked objects.
       Any object on a `space_new` segment is already marked, but still
       needs to be swept. */
    {
       for (g = MAX_CG; g >= 0; g -= 1) {
         ptr locked_objects;
         IGEN tg = compute_target_generation(g);
         ls = S_G.locked_objects[g];
         S_G.locked_objects[g] = Snil;
         S_G.unlocked_objects[g] = Snil;
         locked_objects = S_G.locked_objects[tg];
         for (; ls != Snil; ls = Scdr(ls)) {
           ptr p = Scar(ls);
           seginfo *si = SegInfo(ptr_get_segment(p));
           if (si->space == space_new) {
             /* Retract the mark bit and mark properly, so anything that needs
                to happen with marking will happen. */
             if (!marked(si, p))
               S_error_abort("space_new locked object should have a mark bit set");
             si->marked_mask[segment_bitmap_byte(p)] -= segment_bitmap_bit(p);
             mark_object(tc, p, si);
           }
           /* non-`space_new` objects will be swept via new pair */
           locked_objects = S_cons_in(tc, space_impure, tg, p, locked_objects);
#ifdef ENABLE_OBJECT_COUNTS
           S_G.countof[tg][countof_pair] += 1;
           S_G.countof[tg][countof_locked] += 1;
           S_G.bytesof[tg][countof_locked] += size_object(p);
#endif /* ENABLE_OBJECT_COUNTS */
         }
         S_G.locked_objects[tg] = locked_objects;
       }
    }

  /* for each thread with a sweeper, sweep in that thread's context to
     make sure the sweeper will perform that thread's work; otherwise,
     sweep non-oldspace threads, since any thread may have an active
     stack */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr thread;
#ifdef ENABLE_PARALLEL
      ptr t_tc;
#endif

    /* someone may have their paws on the list */
      if (FWDMARKER(ls) == forward_marker) ls = FWDADDRESS(ls);

      thread = Scar(ls);

#ifdef ENABLE_PARALLEL
      t_tc = (ptr)THREADTC(thread);
      if (WILL_BE_SWEEPER(t_tc) != main_sweeper_index) {
        if (!OLDSPACE(thread)) {
          /* sweep in sweeper thread: */
          sweepers[WILL_BE_SWEEPER(t_tc)].thread = thread;
        } else {
          /* relocate now, so main sweeping will happen in
             sweeper thread */
          ptr tc_in = t_tc; /* shadows enclosing `tc_in` binding */
          relocate_pure(&thread, NULL, 0);
        }
      } else
#endif
        if (!OLDSPACE(thread))
          sweep_thread(tc, thread);
    }
    relocate_pure(&S_threads, NULL, 0);

    GET_TIME(start);

  /* relocate nonempty oldspace symbols and set up list of buckets to rebuild later */
    buckets_to_rebuild = NULL;
    for (g = 0; g <= MAX_CG; g += 1) {
      bucket_list *bl, *blnext; bucket *b; bucket_pointer_list *bpl; bucket **oblist_cell; ptr sym; iptr idx;
      for (bl = S_G.buckets_of_generation[g]; bl != NULL; bl = blnext) {
        blnext = bl->cdr;
        b = bl->car;
        /* mark this bucket old for the rebuilding loop */
        b->next = TO_VOIDP((uptr)TO_PTR(b->next) | 1);
        sym = b->sym;
        idx = UNFIX(SYMHASH(sym)) % S_G.oblist_length;
        oblist_cell = &S_G.oblist[idx];
        if (!((uptr)TO_PTR(*oblist_cell) & 1)) {
          /* mark this bucket in the set */
          *oblist_cell = TO_VOIDP((uptr)TO_PTR(*oblist_cell) | 1);
          /* repurpose the bucket list element for the list of buckets to rebuild later */
          /* idiot_checks verifies these have the same size */
          bpl = (bucket_pointer_list *)bl;
          bpl->car = oblist_cell;
          bpl->cdr = buckets_to_rebuild;
          buckets_to_rebuild = bpl;
        }
        if (FWDMARKER(sym) != forward_marker &&
            /* coordinate with alloc.c */
            (SYMVAL(sym) != sunbound || SYMPLIST(sym) != Snil || SYMSPLIST(sym) != Snil)) {
          seginfo *sym_si = SegInfo(ptr_get_segment(sym));
          if (!new_marked(sym_si, sym))
            mark_or_copy_pure(&sym, sym, sym_si);
        }
      }
      S_G.buckets_of_generation[g] = NULL;
    }

  /* relocate the protected C pointers */
    {uptr i;
     for (i = 0; i < S_G.protect_next; i++)
       relocate_pure(S_G.protected[i], NULL, 0);
    }

  /* sweep older locked and unlocked objects that are on `space_new` segments,
     because we can't find dirty writes there */
    for (g = MAX_CG + 1; g <= static_generation; INCRGEN(g)) {
      for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls))
        sweep_dirty_object_if_space_new(tc, Scar(ls));
      for (ls = S_G.unlocked_objects[g]; ls != Snil; ls = Scdr(ls))
        sweep_dirty_object_if_space_new(tc, Scar(ls));
    }

  /* prepare to sweep areas marked dirty by assignments into older generations */
    setup_sweep_dirty();

    parallel_sweep_dirty_and_generation(tc);
  /* since we will later resweep dirty weak pairs, make sure sweep_generation
     ends with a terminator in place for space_weakpair, at least in all threads
     other than this one that may have allocated there during sweep_generation */

    pre_finalization_size = target_generation_space_so_far(tc);

  /* handle guardians */
    {   ptr pend_hold_ls, final_ls, pend_final_ls, maybe_final_ordered_ls;
        ptr obj, rep, tconc, next;
        IBOOL do_ordered = 0;

      /* move each entry in guardian lists into one of:
       *   pend_hold_ls     if obj accessible
       *   final_ls         if obj not accessible and tconc accessible
       *   pend_final_ls    if obj not accessible and tconc not accessible
       * When a pend_hold_ls or pend_final_ls entry is tconc is
       * determined to be accessible, then it moves to hold_ls or
       * final_ls. When an entry in pend_hold_ls or pend_final_ls can't
       * be moved to final_ls or hold_ls, the entry moves into a
       * seginfo's trigger list (to avoid quadratic-time processing of
       * guardians). When the trigger fires, the entry is added to
       * recheck_guardians_ls, which is sorted back into pend_hold_ls
       * and pend_final_ls for another iteration.
       * Ordered and unordered guardian entries start out together;
       * when final_ls is processed, ordered entries are delayed by
       * moving them into maybe_final_ordered_ls, which is split back
       * into final_ls and pend_hold_ls after all unordered entries
       * have been handled. */
        pend_hold_ls = final_ls = pend_final_ls = maybe_final_ordered_ls = Snil;
        recheck_guardians_ls = Snil;

        for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
          ptr tc = (ptr)THREADTC(Scar(ls));
          partition_guardians(GUARDIANENTRIES(tc), NONSTATICINHEAP);
          GUARDIANENTRIES(tc) = Snil;
        }

        for (g = 0; g <= MAX_CG; g += 1) {
          partition_guardians(S_G.guardians[g], ALWAYSTRUE);
          S_G.guardians[g] = Snil;
        }

       /* invariants after partition_guardians:
        * for entry in pend_hold_ls, obj is !OLDSPACE
        * for entry in final_ls, obj is OLDSPACE
        * for entry in final_ls, tconc is !OLDSPACE
        * for entry in pend_final_ls, obj and tconc are OLDSPACE
        */

        while (1) {
            IBOOL relocate_rep = final_ls != Snil;

          /* relocate & add the final objects to their tconcs */
            ls = final_ls; final_ls = Snil;
            for (; ls != Snil; ls = next) {
                ptr old_end, new_end;

                next = GUARDIANNEXT(ls);

                rep = GUARDIANREP(ls);
              /* ftype_guardian_rep is a marker for reference-counted ftype pointer */
                if (rep == ftype_guardian_rep) {
                  INT b; iptr *addr;
                  rep = GUARDIANOBJ(ls);
                  if (FWDMARKER(rep) == forward_marker) rep = FWDADDRESS(rep);
                /* Caution: Building in assumption about shape of an ftype pointer */
                  addr = TO_VOIDP(RECORDINSTIT(rep, 0));
                  LOCKED_DECR(addr, b);
                  if (!b) continue;
                }

                if (!do_ordered && (GUARDIANORDERED(ls) == Strue)) {
                  /* Sweep from the representative, but don't copy the
                     representative itself; if the object stays uncopied by
                     the end, then the entry is really final, and we copy the
                     representative only at that point; crucially, the
                     representative can't itself be a tconc, so we
                     won't discover any new tconcs at that point. */
                  ptr obj = GUARDIANOBJ(ls);
                  seginfo *o_si = SegInfo(ptr_get_segment(obj));
                  if (FORWARDEDP(obj, o_si) || new_marked(o_si, obj)) {
                    /* Object is reachable, so we might as well move
                       this one to the hold list --- via pend_hold_ls, which
                       leads to a copy to move to hold_ls */
                    INITGUARDIANNEXT(ls) = pend_hold_ls;
                    pend_hold_ls = ls;
                  } else {
                    seginfo *si;
                    if (!IMMEDIATE(rep) && (si = MaybeSegInfo(ptr_get_segment(rep))) != NULL && si->old_space) {
                      /* mark things reachable from `rep`, but not `rep` itself, unless
                         `rep` is immediately reachable from itself */
                      PUSH_BACKREFERENCE(ls)
                      sweep_in_old(tc, rep);
                      POP_BACKREFERENCE()
                    }
                    INITGUARDIANNEXT(ls) = maybe_final_ordered_ls;
                    maybe_final_ordered_ls = ls;
                  }
                } else {
                /* if tconc was old it's been forwarded */
                  tconc = GUARDIANTCONC(ls);

                  WITH_TOP_BACKREFERENCE(tconc, relocate_pure(&rep, NULL, 0));

                  old_end = Scdr(tconc);
                  new_end = S_cons_in(tc, space_impure, 0, FIX(0), FIX(0));
#ifdef ENABLE_OBJECT_COUNTS
                  S_G.countof[0][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */

                  /* These assignments may trigger card marking or additions to `new_dirty_cards`: */
                  SETCAR(old_end,rep);
                  SETCDR(old_end,new_end);
                  SETCDR(tconc,new_end);
                }
            }

          /* copy each entry in pend_hold_ls into hold_ls if tconc accessible */
            ls = pend_hold_ls; pend_hold_ls = Snil;
            for ( ; ls != Snil; ls = next) {
              ptr p;
              seginfo *t_si;
#ifdef CONSTANT_TARGET_GENERATION
              g = MAX_TG;
#else
              seginfo *g_si;
              g_si = SegInfo(ptr_get_segment(ls));
              g = TARGET_GENERATION(g_si);
#endif

              next = GUARDIANNEXT(ls); 

              /* discard static pend_hold_ls entries */
              if (g == static_generation) continue;
              
              tconc = GUARDIANTCONC(ls);

              t_si = SegInfo(ptr_get_segment(tconc));
              
              if (t_si->old_space && !new_marked(t_si, tconc)) {
                if (FWDMARKER(tconc) == forward_marker)
                  tconc = FWDADDRESS(tconc);
                else {
                  INITGUARDIANPENDING(ls) = FIX(GUARDIAN_PENDING_HOLD);
                  add_pending_guardian(ls, tconc);
                  continue;
                }
              }
              
              rep = GUARDIANREP(ls);
              WITH_TOP_BACKREFERENCE(tconc, relocate_pure(&rep, NULL, 0));
              relocate_rep = 1;

#ifdef ENABLE_OBJECT_COUNTS
                S_G.countof[g][countof_guardian] += 1;
#endif /* ENABLE_OBJECT_COUNTS */

                /* In backreference mode, we rely on sweep of the guardian
                   entry not registering any backreferences. Otherwise,
                   bogus pair pointers would get created. */
                find_room(tc, space_pure, g, typemod, size_guardian_entry, p);
                INITGUARDIANOBJ(p) = GUARDIANOBJ(ls);
                INITGUARDIANREP(p) = rep;
                INITGUARDIANTCONC(p) = tconc;
                INITGUARDIANNEXT(p) = S_G.guardians[g];
                INITGUARDIANORDERED(p) = GUARDIANORDERED(ls);
                INITGUARDIANPENDING(p) = FIX(0);
                S_G.guardians[g] = p;
            }

            if (!relocate_rep && !do_ordered && maybe_final_ordered_ls != Snil) {
              /* Switch to finishing up ordered. Move all maybe-final
                 ordered entries to final_ls and pend_hold_ls */
              do_ordered = relocate_rep = 1;
              ls = maybe_final_ordered_ls; maybe_final_ordered_ls = Snil;
              for (; ls != Snil; ls = next) {
                ptr obj = GUARDIANOBJ(ls);
                seginfo *o_si = SegInfo(ptr_get_segment(obj));
                next = GUARDIANNEXT(ls);
                if (FORWARDEDP(obj, o_si) || new_marked(o_si, obj)) {
                  /* Will defintely move to hold_ls, but the entry
                     must be copied to move from pend_hold_ls to
                     hold_ls: */
                  INITGUARDIANNEXT(ls) = pend_hold_ls;
                  pend_hold_ls = ls;
                } else {
                  INITGUARDIANNEXT(ls) = final_ls;
                  final_ls = ls;
                }
              }
            }

            if (!relocate_rep) break;

            sweep_generation(tc);

            ls = recheck_guardians_ls; recheck_guardians_ls = Snil;
            for ( ; ls != Snil; ls = next) {
              next = GUARDIANNEXT(ls);
              if (GUARDIANPENDING(ls) == FIX(GUARDIAN_PENDING_HOLD)) {
                INITGUARDIANNEXT(ls) = pend_hold_ls;
                pend_hold_ls = ls;
              } else {
                INITGUARDIANNEXT(ls) = pend_final_ls;
                pend_final_ls = ls;
              }
            }
            
          /* move each entry in pend_final_ls into one of:
           *   final_ls         if tconc forwarded or marked
           *   pend_final_ls    if tconc not forwarded or marked
           * where the output pend_final_ls coresponds to pending in a segment */
            ls = pend_final_ls; pend_final_ls = Snil;
            for ( ; ls != Snil; ls = next) {
                tconc = GUARDIANTCONC(ls); next = GUARDIANNEXT(ls);

                if (FWDMARKER(tconc) == forward_marker) {
                    INITGUARDIANTCONC(ls) = FWDADDRESS(tconc);
                    INITGUARDIANNEXT(ls) = final_ls;
                    final_ls = ls;
                } else {
#ifndef NO_NEWSPACE_MARKS
                  seginfo *t_si = SegInfo(ptr_get_segment(tconc));
#endif
                  if (new_marked(t_si, tconc)) {
                    INITGUARDIANNEXT(ls) = final_ls;
                    final_ls = ls;
                  } else {
                    INITGUARDIANPENDING(ls) = FIX(GUARDIAN_PENDING_FINAL);
                    add_pending_guardian(ls, tconc);
                  }
                }
            }
        }
    }

    S_G.bytes_finalized = target_generation_space_so_far(tc) - pre_finalization_size;
    {
      iptr post_phantom_bytes = 0;
      for (g = MIN_TG; g <= MAX_TG; g++) {
        post_phantom_bytes += S_G.bytesof[g][countof_phantom];
      }
      S_adjustmembytes(post_phantom_bytes - pre_phantom_bytes);
    }

  /* handle weak pairs */
    resweep_dirty_weak_pairs(tc);
    resweep_weak_pairs(tc, oldweakspacesegments);

   /* still-pending ephemerons all go to bwp */
    finish_pending_ephemerons(tc, oldspacesegments);

    ACCUM_TIME(collect_accum, step, start);
    REPORT_TIME(fprintf(stderr, "%d col  +%ld ms  %ld ms\n", MAX_CG, step, collect_accum));

   /* post-gc oblist handling.  rebuild old buckets in the target generation, pruning unforwarded symbols */
    { bucket_list *bl; bucket *b, *bnext; bucket_pointer_list *bpl; bucket **pb; ptr sym;
      for (bpl = buckets_to_rebuild; bpl != NULL; bpl = bpl->cdr) {
        pb = bpl->car;
        for (b = TO_VOIDP((uptr)TO_PTR(*pb) - 1); b != NULL && ((uptr)TO_PTR(b->next) & 1); b = bnext) {
          bnext = TO_VOIDP((uptr)TO_PTR(b->next) - 1);
          sym = b->sym;
          si = SegInfo(ptr_get_segment(sym));
          if (new_marked(si, sym) || (FWDMARKER(sym) == forward_marker && ((sym = FWDADDRESS(sym)) || 1))) {
            IGEN g = si->generation;
            find_room_voidp(tc, space_data, g, ptr_align(sizeof(bucket)), b);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[g][countof_oblist] += 1;
            S_G.bytesof[g][countof_oblist] += sizeof(bucket);
#endif /* ENABLE_OBJECT_COUNTS */
            b->sym = sym;
            *pb = b;
            pb = &b->next;
            if (g != static_generation) {
              find_room_voidp(tc, space_data, g, ptr_align(sizeof(bucket_list)), bl);
#ifdef ENABLE_OBJECT_COUNTS
              S_G.countof[g][countof_oblist] += 1;
              S_G.bytesof[g][countof_oblist] += sizeof(bucket_list);
#endif /* ENABLE_OBJECT_COUNTS */
              bl->car = b;
              bl->cdr = S_G.buckets_of_generation[g];
              S_G.buckets_of_generation[g] = bl;
            }
          } else {
            S_G.oblist_count -= 1;
          }
        }
        *pb = b;
      }
    }

  /* rebuild rtds_with_counts lists, dropping otherwise inaccessible rtds */
    { IGEN g, newg; ptr ls, p; seginfo *si;
      int count = 0;
      for (g = MAX_CG; g >= 0; g -= 1) {
        for (ls = S_G.rtds_with_counts[g], S_G.rtds_with_counts[g] = Snil; ls != Snil; ls = Scdr(ls)) {
          count++;
          p = Scar(ls);
          si = SegInfo(ptr_get_segment(p));
          if (!si->old_space || new_marked(si, p)) {
            newg = TARGET_GENERATION(si);
            S_G.rtds_with_counts[newg] = S_cons_in(tc, space_impure, newg, p, S_G.rtds_with_counts[newg]);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif
          } else if (FWDMARKER(p) == forward_marker) {
            p = FWDADDRESS(p);
            newg = GENERATION(p);
            S_G.rtds_with_counts[newg] = S_cons_in(tc, space_impure, newg, p, S_G.rtds_with_counts[newg]);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif
          }
        }
      }
    }

#ifndef WIN32
  /* rebuild child_process list, reaping any that have died and refusing
     to promote into the static generation. */
    { IGEN g, newg; ptr ls, newls;
      for (g = MAX_CG; g >= 0; g -= 1) {
        newg = compute_target_generation(g);
        if (newg == static_generation) newg = S_G.max_nonstatic_generation;
        newls = newg == g ? Snil : S_child_processes[newg];
        for (ls = S_child_processes[g], S_child_processes[g] = Snil; ls != Snil; ls = Scdr(ls)) {
          INT pid = UNFIX(Scar(ls)), status, retpid;
          retpid = waitpid(pid, &status, WNOHANG);
          if (retpid == 0 || (retpid == pid && !(WIFEXITED(status) || WIFSIGNALED(status)))) {
            newls = S_cons_in(tc, space_impure, newg, FIX(pid), newls);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          }
        }
        S_child_processes[newg] = newls;
      }
    }
#endif /* WIN32 */

    copy_and_clear_list_bits(tc, oldspacesegments);

  /* move copied old space segments to empty space, and promote
     marked old space segments to the target generation */
    for (si = oldspacesegments; si != NULL; si = nextsi) {
      nextsi = si->next;
      si->old_space = 0;
      si->use_marks = 0;
      if (si->marked_mask != NULL) {
        IGEN tg;
        si->min_dirty_byte = 0xff;
        if (si->space != space_data) {
          int d;
          for (d = 0; d < cards_per_segment; d += sizeof(ptr)) {
            iptr *dp = (iptr *)(si->dirty_bytes + d);
            /* fill sizeof(iptr) bytes at a time with 0xff */
            *dp = -1;
          }
        }
        tg = si->generation;
        if (tg == static_generation) S_G.number_of_nonstatic_segments -= 1;
        s = si->space;
        si->next = S_G.occupied_segments[tg][s];
        S_G.occupied_segments[tg][s] = si;
        S_G.bytes_of_space[tg][s] += si->marked_count;
        si->trigger_guardians = 0;
#ifdef PRESERVE_FLONUM_EQ
        si->forwarded_flonums = 0;
#endif
      } else {
        chunkinfo *chunk = si->chunk;
        S_G.number_of_nonstatic_segments -= 1;
        S_G.number_of_empty_segments += 1;
        si->space = space_empty;
        si->next = chunk->unused_segs;
        chunk->unused_segs = si;
#ifdef WIPECLEAN
        memset((void *)build_ptr(si->number,0), 0xc7, bytes_per_segment);
#endif
        if ((chunk->nused_segs -= 1) == 0) {
          if (chunk->bytes != (minimum_segment_request + 1) * bytes_per_segment) {
            /* release oversize chunks back to the O/S immediately to avoid allocating
             * small stuff into them and thereby invite fragmentation */
            S_free_chunk(chunk);
          } else {
            S_move_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS]);
          }
        } else {
          S_move_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS-1]);
        }
      }
    }

    S_G.g0_bytes_after_last_gc = S_G.bytes_of_generation[0];

    if (MAX_CG >= S_G.min_free_gen) S_free_chunks();

    S_flush_instruction_cache(tc);

#ifndef NO_DIRTY_NEWSPACE_POINTERS
    /* mark dirty those newspace cards to which we've added wrong-way pointers */
    { dirtycardinfo *ndc;
      for (ndc = S_G.new_dirty_cards; ndc != NULL; ndc = ndc->next)
        S_mark_card_dirty(ndc->card, ndc->youngest);
    }
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

    if (S_checkheap) S_check_heap(1, MAX_CG);

   /* post-collection rehashing of tlcs.
      must come after any use of relocate.
      logically comes after gc is entirely complete */
    while (tlcs_to_rehash != Snil) {
      ptr b, next; uptr old_idx, new_idx;
      ptr tlc = Scar(tlcs_to_rehash);
      ptr ht = TLCHT(tlc);
      ptr vec = PTRFIELD(ht,eq_hashtable_vec_disp);
      uptr veclen = Svector_length(vec);
      ptr key = Scar(TLCKEYVAL(tlc));

     /* scan to end of bucket to find the index */
      for (b = TLCNEXT(tlc); !Sfixnump(b); b = TLCNEXT(b));
      old_idx = UNFIX(b);

      if (key == Sbwp_object && PTRFIELD(ht,eq_hashtable_subtype_disp) != FIX(eq_hashtable_subtype_normal)) {
       /* remove tlc */
        b = Svector_ref(vec, old_idx);
        if (b == tlc) {
          SETVECTIT(vec, old_idx, TLCNEXT(b));
        } else {
          for (;;) { next = TLCNEXT(b); if (next == tlc) break; b = next; }
          SETTLCNEXT(b,TLCNEXT(next));
        }
        INITTLCNEXT(tlc) = Sfalse;
        INITPTRFIELD(ht,eq_hashtable_size_disp) = FIX(UNFIX(PTRFIELD(ht,eq_hashtable_size_disp)) - 1);
      } else if ((new_idx = ((uptr)key >> primary_type_bits) & (veclen - 1)) != old_idx) {
       /* remove tlc from old bucket */
        b = Svector_ref(vec, old_idx);
        if (b == tlc) {
          SETVECTIT(vec, old_idx, TLCNEXT(b));
        } else {
          for (;;) { next = TLCNEXT(b); if (next == tlc) break; b = next; }
          SETTLCNEXT(b,TLCNEXT(next));
        }
       /* and add to new bucket */
        SETTLCNEXT(tlc, Svector_ref(vec, new_idx));
        SETVECTIT(vec, new_idx, tlc);
      }
      tlcs_to_rehash = Scdr(tlcs_to_rehash);
    }

    /* Promote opportunistic 1-shot continuations, because we can no
       longer cache one and we can no longer reliably fuse the stack
       back. */
    while (conts_to_promote != Snil) {
      S_promote_to_multishot(CONTLINK(Scar(conts_to_promote)));
      conts_to_promote = Scdr(conts_to_promote);
    }

    S_resize_oblist();

    /* tell profile_release_counters to look for bwp'd counters at least through max_tg */
    if (S_G.prcgeneration < MAX_TG) S_G.prcgeneration = MAX_TG;

    if (SWEEPSTACKSTART(tc_in) != SWEEPSTACK(tc_in))
      S_error_abort("gc: sweep stack ended non-empty");

    S_G.bitmask_overhead[0] += BITMASKOVERHEAD(tc_in, 0);
    BITMASKOVERHEAD(tc_in, 0) = 0;
    for (g = MIN_TG; g <= MAX_TG; g++)
      S_G.bitmask_overhead[g] += BITMASKOVERHEAD(tc_in, g);

    ACCUM_TIME(all_accum, astep, astart);
    REPORT_TIME(fprintf(stderr, "%d all  +%ld ms  %ld ms\n", MAX_CG, astep, all_accum));

    if (count_roots_ls != Sfalse) {
#ifdef ENABLE_OBJECT_COUNTS
      return count_roots_counts;
#else
      return Snil;
#endif
    } else
      return Svoid;
}

#ifdef ENABLE_PARALLEL

static void reassign_segment_creator(ptr tc, seginfo *si) {
  int i;

  for (i = 0; i < num_sweepers; i++)
    if (sweepers[i].sweep_tc == si->creator_tc)
      return;

  si->creator_tc = tc;
}

static void flush_remote_range(ptr tc, ISPC s, IGEN g) {
  remote_range *r;
  int me = SWEEPER(tc);
  int they = REMOTESWEEPER(tc);

  /* REMOVEME printf("%d flush %p-%p [%d,%d] to %d\n", me, REMOTERANGESTART(tc), REMOTERANGEEND(tc), s, g, they); */

  find_room_voidp(tc, space_data, 0, ptr_align(sizeof(remote_range)), r);
  BITMASKOVERHEAD(tc, 0) += ptr_align(sizeof(remote_range));
  r->s = s;
  r->g = g;
  r->start = REMOTERANGESTART(tc);
  r->end = REMOTERANGEEND(tc);
  r->next = sweepers[me].ranges_to_send[they];
  sweepers[me].ranges_to_send[they] = r;

  REMOTERANGESTART(tc) = (ptr)(uptr)-1;
  REMOTERANGEEND(tc) = (ptr)0;

  SWEEPCHANGE(tc) = SWEEP_CHANGE_PROGRESS;
}

#endif

#define sweep_space(s, from_g, body) do {                               \
    sweep_space_segments(s, from_g, body);                              \
    sweep_space_bump_range(s, from_g, body);                            \
    sweep_space_local_ranges(s, from_g, body);                          \
  } while (0)

#define sweep_space_segments(s, from_g, body) do {                      \
    while ((si = (seginfo *)TO_VOIDP(SWEEPNEXT_AT(tc_in, s, from_g))) != NULL) { \
      SWEEPNEXT_AT(tc_in, s, from_g) = TO_PTR(si->sweep_next);          \
      pp = TO_VOIDP(si->sweep_start);                                   \
      while ((p = *pp) != forward_marker)                               \
        body                                                            \
      COUNT_SWEPT_BYTES(si->sweep_start, pp);                           \
      FLUSH_REMOTE_RANGE(tc_in, s, from_g);                             \
      save_resweep(s, si);                                              \
    }                                                                   \
  } while (0)

#define sweep_space_bump_range(s, from_g, body) do {                    \
    slp = &SWEEPLOC_AT(tc_in, s, from_g);                               \
    nlp = &NEXTLOC_AT(tc_in, s, from_g);                                \
    while ((sl = TO_VOIDP(*slp)) != (nl = TO_VOIDP(*nlp))) {            \
      *slp = TO_PTR(nl);                                                \
      pp = sl;                                                          \
      while (pp != nl) {                                                \
        p = *pp;                                                        \
        body                                                            \
      }                                                                 \
      COUNT_SWEPT_BYTES(sl, nl);                                        \
      FLUSH_REMOTE_RANGE(tc_in, s, from_g);                             \
    }                                                                   \
  } while (0)

#define sweep_space_local_ranges(s, from_g, body) do {                  \
    local_ranges = TO_VOIDP(GET_LOCALRANGES_AT(tc_in, s, from_g));      \
    if (local_ranges != NULL) {                                         \
      LOCALRANGES_AT(tc_in, s, from_g) = (ptr)0;                        \
      while (local_ranges) {                                            \
        pp = TO_VOIDP(local_ranges->start);                             \
        nl = TO_VOIDP(local_ranges->end);                               \
        while (pp != nl) {                                              \
          p = *pp;                                                      \
          body                                                          \
        }                                                               \
        COUNT_SWEPT_BYTES(local_ranges->start, nl);                     \
        FLUSH_REMOTE_RANGE(tc_in, s, from_g);                           \
        local_ranges = local_ranges->next;                              \
      }                                                                 \
    }                                                                   \
  } while (0)

#define save_resweep(s, si) do {                  \
    if (s == space_weakpair) {                    \
      GC_TC_MUTEX_ACQUIRE();                      \
      si->sweep_next = resweep_weak_segments;     \
      resweep_weak_segments = si;                 \
      GC_TC_MUTEX_RELEASE();                      \
    }                                             \
  } while (0)

#ifdef ENABLE_TIMING
# define COUNT_SWEPT_BYTES(start, end) num_swept_bytes += ((uptr)TO_PTR(end) - (uptr)TO_PTR(start))
#else
# define COUNT_SWEPT_BYTES(start, end) do { } while (0)
#endif
    
static void resweep_weak_pairs(ptr tc_in, seginfo *oldweakspacesegments) {
    IGEN from_g;
    ptr *pp, p, *nl;
    seginfo *si;
  
    for (from_g = MIN_TG; from_g <= MAX_TG; from_g += 1) {
      /* By starting from `base_loc`, we may needlessly sweep pairs in `MAX_TG`
         that were allocated before the GC, but that's ok. */
      pp = TO_VOIDP(BASELOC_AT(tc_in, space_weakpair, from_g));
      nl = TO_VOIDP(NEXTLOC_AT(tc_in, space_weakpair, from_g));
      while (pp != nl) {
        p = *pp;
        forward_or_bwp(pp, p);
        pp += 2;
      }

#ifdef ENABLE_PARALLEL
      /* For each tc used by a sweeper thread, we need to similarly
         sweep from thread-local regions that are not closed off. */
      { 
        int i;
        for (i = 0; i < num_sweepers; i++) {
          ptr tc = sweepers[i].sweep_tc;
          pp = TO_VOIDP(BASELOC_AT(tc, space_weakpair, from_g));
          nl = TO_VOIDP(NEXTLOC_AT(tc, space_weakpair, from_g));
          while (pp != nl) {
            p = *pp;
            forward_or_bwp(pp, p);
            pp += 2;
          }
        }
      }
#endif
    }

   for (si = resweep_weak_segments; si != NULL; si = si->sweep_next) {
     pp = TO_VOIDP(build_ptr(si->number, 0));
     while ((p = *pp) != forward_marker) {
       forward_or_bwp(pp, p);
       pp += 2;
     }
   }

   for (si = oldweakspacesegments; si != NULL; si = si->next) {
     if (si->space != space_weakpair)
       break;
     if (si->marked_mask) {
       uptr i;
       for (i = 0; i < segment_bitmap_bytes; i++) {
         int mask = si->marked_mask[i];
         if (mask != 0) {
           /* Assuming 4 pairs per 8 words */
           pp = TO_VOIDP(build_ptr(si->number, (i << (log2_ptr_bytes+3))));
           if (mask & 0x1)
             forward_or_bwp(pp, *pp);
           pp += 2;
           if (mask & 0x4)
             forward_or_bwp(pp, *pp);
           pp += 2;
           if (mask & 0x10)
             forward_or_bwp(pp, *pp);
           pp += 2;
           if (mask & 0x40)
             forward_or_bwp(pp, *pp);
         }
       }
     }
   }
}

static void forward_or_bwp(pp, p) ptr *pp; ptr p; {
  seginfo *si;
 /* adapted from relocate */
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->old_space && !new_marked(si, p)) {
    if (FORWARDEDP(p, si)) {
      *pp = GET_FWDADDRESS(p);
    } else {
      *pp = Sbwp_object;
    }
  }
}

static iptr sweep_generation_pass(ptr tc_in) {
  ptr *slp, *nlp; ptr *pp, *ppn, p, *nl, *sl; IGEN from_g;
  seginfo *si;
  iptr num_swept_bytes = 0;
  remote_range *local_ranges, *dirty_ranges;

  do {
    SWEEPCHANGE(tc_in) = SWEEP_NO_CHANGE;

    sweep_from_stack(tc_in);

    for (from_g = MIN_TG; from_g <= MAX_TG; from_g += 1) {

# define SWEEP_TWO_WORDS_BODY {                                         \
        /* only pairs in theses spaces in backreference mode */         \
        SET_BACKREFERENCE(TYPE(TO_PTR(pp), type_pair));                 \
        relocate_impure_help(pp, p, from_g, pp, 2 * ptr_bytes);         \
        ppn = pp + 1;                                                   \
        p = *ppn;                                                       \
        relocate_impure_help(ppn, p, from_g, pp, 2 * ptr_bytes);        \
        pp = ppn + 1;                                                   \
    }
      
      sweep_space(space_impure, from_g, SWEEP_TWO_WORDS_BODY);
      SET_BACKREFERENCE(Sfalse)

      sweep_space(space_symbol, from_g, {
        p = TYPE(TO_PTR(pp), type_symbol);
        sweep_symbol(tc_in, p, from_g);
        pp += size_symbol / sizeof(ptr);
      });

      sweep_space(space_port, from_g, {
        p = TYPE(TO_PTR(pp), type_typed_object);
        sweep_port(tc_in, p, from_g);
        pp += size_port / sizeof(ptr);
      });

      sweep_space(space_weakpair, from_g, {
        SET_BACKREFERENCE(TYPE(TO_PTR(pp), type_pair))
        ppn = pp + 1;
        p = *ppn;
        relocate_impure_help(ppn, p, from_g, pp, size_pair);
        pp = ppn + 1;
      });
      SET_BACKREFERENCE(Sfalse)

      sweep_space(space_ephemeron, from_g, {
        p = TYPE(TO_PTR(pp), type_pair);
        add_ephemeron_to_pending(tc_in, p);
        pp += size_ephemeron / sizeof(ptr);
      });
      
      sweep_space(space_pure, from_g, {
        SET_BACKREFERENCE(TYPE(TO_PTR(pp), type_pair)) /* only pairs put here in backreference mode */
        relocate_impure_help(pp, p, from_g, pp, 2 * ptr_bytes);
        ppn = pp + 1;
        p = *ppn;
        relocate_impure_help(ppn, p, from_g, pp, 2 * ptr_bytes);
        pp = ppn + 1;
      });
      SET_BACKREFERENCE(Sfalse)

      sweep_space(space_continuation, from_g, {
        p = TYPE(TO_PTR(pp), type_closure);
        sweep_continuation(tc_in, p, from_g);
        pp += size_continuation / sizeof(ptr);
      });

      sweep_space(space_pure_typed_object, from_g, {
        p = TYPE(TO_PTR(pp), type_typed_object);
        pp = TO_VOIDP(((uptr)TO_PTR(pp) + sweep_typed_object(tc_in, p, from_g)));
      });

      sweep_space(space_code, from_g, {
        p = TYPE(TO_PTR(pp), type_typed_object);
        sweep_code_object(tc_in, p, from_g);
        pp += size_code(CODELEN(p)) / sizeof(ptr);
      });

      sweep_space(space_impure_record, from_g, {
        p = TYPE(TO_PTR(pp), type_typed_object);
        sweep_record(tc_in, p, from_g);
        pp = TO_VOIDP((iptr)TO_PTR(pp) +
               size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
      });

      /* space used only as needed for backreferences: */
      sweep_space(space_impure_typed_object, from_g, {
        p = TYPE(TO_PTR(pp), type_typed_object);
        pp = TO_VOIDP((uptr)TO_PTR(pp) + sweep_typed_object(tc_in, p, from_g));
      });

      /* space used only as needed for backreferences: */
      sweep_space(space_closure, from_g, {
        p = TYPE(TO_PTR(pp), type_closure);
        sweep(tc_in, p, from_g);
        pp = TO_VOIDP((uptr)TO_PTR(pp) + size_object(p));
      });

      /* spaces used only for ranges that were remote to another sweeper: */
      sweep_space_local_ranges(space_immobile_impure, from_g, SWEEP_TWO_WORDS_BODY);
      sweep_space_local_ranges(space_count_pure, from_g, SWEEP_TWO_WORDS_BODY);
      sweep_space_local_ranges(space_count_impure, from_g, SWEEP_TWO_WORDS_BODY);
    }

    dirty_ranges = send_and_receive_remote_ranges(tc_in);

    /* The ranges in `dirty_ranges` are old-generation objects from
       other parallel sweepers, which means they correspond to dirty
       sweeps in the originating sweeper. We handle them here like
       regular sweeping using `relocate_impure`, which will register a
       dirty-card update as needed. */
    while (dirty_ranges != NULL) {
      ISPC s = dirty_ranges->s;
      IGEN from_g = dirty_ranges->g;

      pp = TO_VOIDP(dirty_ranges->start);
      nl = TO_VOIDP(dirty_ranges->end);

      if ((s == space_impure)
          || (s == space_immobile_impure)
          || (s == space_count_impure)
          || (s == space_closure)
          || (s == space_impure_typed_object)) {
        /* REMOVEME if (s == space_impure) printf("sweep like dirty %p-%p [%d, %d]\n", pp, nl, s, from_g); */
        while (pp < nl) {
          p = *pp;
          relocate_impure_help(pp, p, from_g, pp, 2 * ptr_bytes);
          ppn = pp + 1;
          p = *ppn;
          relocate_impure_help(ppn, p, from_g, pp, 2 * ptr_bytes);
          pp = ppn + 1;
        }
      } else if ((s == space_pure_typed_object)
                 || (s == space_count_pure)) {
        /* can happen in the special case of a thread object: */
        while (pp < nl) {
          p = TYPE(TO_PTR(pp), type_typed_object);
          pp = TO_VOIDP(((uptr)TO_PTR(pp) + sweep_typed_object(tc_in, p, from_g)));
        }
      } else if (s == space_symbol) {
        while (pp < nl) {
          p = TYPE(TO_PTR(pp), type_symbol);
          sweep_symbol(tc_in, p, from_g);
          pp += size_symbol / sizeof(ptr);
        }
      } else if (s == space_port) {
        while (pp < nl) {
          p = TYPE(TO_PTR(pp), type_typed_object);
          sweep_port(tc_in, p, from_g);
          pp += size_port / sizeof(ptr);
        }
      } else if (s == space_weakpair) {
        while (pp < nl) {
          ppn = pp + 1;
          p = *ppn;
          relocate_impure_help(ppn, p, from_g, pp, size_pair);
          pp = ppn + 1;
        }
      } else if (s == space_ephemeron) {
        while (pp < nl) {
          p = TYPE(TO_PTR(pp), type_pair);
          add_ephemeron_to_pending(tc_in, p);
          pp += size_ephemeron / sizeof(ptr);
        }
      } else if (s == space_impure_record) {
        while (pp < nl) {
          p = TYPE(TO_PTR(pp), type_typed_object);
          sweep_record(tc_in, p, from_g);
          pp = TO_VOIDP((iptr)TO_PTR(pp) +
                        size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
        }
      } else {
        /* REMOVEME */ printf("failing on %p-%p [%d, %d]\n", pp, nl, s, from_g);
        S_error_abort("dirty range sweep: unexpected space");
      }
      FLUSH_REMOTE_RANGE(tc_in, s, from_g);
      dirty_ranges = dirty_ranges->next;
    }

    /* Waiting until sweeping doesn't trigger a change reduces the
       chance that an ephemeron must be reigistered as a
       segment-specific trigger or gets triggered for recheck, but
       it doesn't change the worst-case complexity. */
    if (SWEEPCHANGE(tc_in) == SWEEP_NO_CHANGE)
      check_pending_ephemerons(tc_in);

  } while (SWEEPCHANGE(tc_in) == SWEEP_CHANGE_PROGRESS);

  return num_swept_bytes;
}

static void sweep_generation(ptr tc_in) {
  sweep_generation_pass(tc_in);
}

void enlarge_sweep_stack(ptr tc_in) {
  uptr sz = ((uptr)SWEEPSTACKLIMIT(tc_in) - (uptr)SWEEPSTACKSTART(tc_in));
  uptr new_sz = 2 * ((sz == 0) ? 256 : sz);
  ptr new_sweep_stack;
  find_room(tc_in, space_data, 0, typemod, ptr_align(new_sz), new_sweep_stack);
  if (sz != 0)
    memcpy(TO_VOIDP(new_sweep_stack), TO_VOIDP(SWEEPSTACKSTART(tc_in)), sz);
  BITMASKOVERHEAD(tc_in, 0) += ptr_align(new_sz);
  SWEEPSTACKSTART(tc_in) = new_sweep_stack;
  SWEEPSTACKLIMIT(tc_in) = (ptr)((uptr)new_sweep_stack + new_sz);
  SWEEPSTACK(tc_in) = (ptr)((uptr)new_sweep_stack + sz);
}

void sweep_from_stack(ptr tc_in) {
  if (SWEEPSTACK(tc_in) > SWEEPSTACKSTART(tc_in)) {
    while (SWEEPSTACK(tc_in) > SWEEPSTACKSTART(tc_in)) {
      ptr p;
      seginfo *si;
      SWEEPSTACK(tc_in) = (ptr)((uptr)SWEEPSTACK(tc_in) - ptr_bytes);
      p = *(ptr *)TO_VOIDP(SWEEPSTACK(tc_in));
      /* Room for improvement: `si->generation` is needed only for
         objects that have impure fields, or in paralle mode for
         remote ranges. */
      si = SegInfo(ptr_get_segment(p));
      sweep(tc_in, p, si->generation);
      FLUSH_REMOTE_RANGE(tc_in, si->space, si->generation);
    }
  }
}
 
static iptr sweep_typed_object(ptr tc, ptr p, IGEN from_g) {
  ptr tf = TYPEFIELD(p);

  if (TYPEP(tf, mask_record, type_record)) {
    sweep_record(tc, p, from_g);
    return size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p))));
  } else if (TYPEP(tf, mask_thread, type_thread)) {
    sweep_thread(tc, p);
    return size_thread;
  } else {
    /* We get here only if backreference mode pushed other typed objects into
       a typed space or if an object is a counting root */
    sweep(tc, p, from_g);
    return size_object(p);
  }
}

typedef struct _weakseginfo {
  seginfo *si;
  IGEN youngest[cards_per_segment];
  struct _weakseginfo *next;
} weakseginfo;

static weakseginfo *weaksegments_to_resweep;

static void record_dirty_segment(IGEN from_g, IGEN to_g, seginfo *si) {
  if (si->min_dirty_byte != 0xff) {
    S_error_abort("record_dirty(gc): unexpected mutation while sweeping");
  }

  if (to_g < from_g) {
    seginfo *oldfirst;
    GC_TC_MUTEX_ACQUIRE();
    oldfirst = DirtySegments(from_g, to_g);
    DirtySegments(from_g, to_g) = si;
    si->dirty_prev = &DirtySegments(from_g, to_g);
    si->dirty_next = oldfirst;
    if (oldfirst != NULL) oldfirst->dirty_prev = &si->dirty_next;
    si->min_dirty_byte = to_g;
    GC_TC_MUTEX_RELEASE();
  }
}

static void add_weaksegments_to_resweep(weakseginfo *segs, weakseginfo *last_seg) {
  if (segs != NULL) {
    GC_TC_MUTEX_ACQUIRE();
    last_seg->next = weaksegments_to_resweep;
    weaksegments_to_resweep = segs;
    GC_TC_MUTEX_RELEASE();
  }
}

static void setup_sweep_dirty() {
  IGEN from_g, to_g;

  weaksegments_to_resweep = NULL;

  /* clear dirty segment lists for copied generations */
  for (from_g = 1; from_g <= MAX_CG; from_g += 1) {
    for (to_g = 0; to_g < from_g; to_g += 1) {
      DirtySegments(from_g, to_g) = NULL;
    }
  }

#ifdef ENABLE_PARALLEL
  for (from_g = MAX_CG + 1; from_g <= static_generation; INCRGEN(from_g)) {
    for (to_g = 0; to_g <= MAX_CG; to_g += 1) {
      seginfo *dirty_si, *nextsi;
      dirty_si = DirtySegments(from_g, to_g);
      DirtySegments(from_g, to_g) = NULL;
      for (; dirty_si != NULL; dirty_si = nextsi) {
        ISPC s;
        int i;
        
        nextsi = dirty_si->dirty_next;
        s = dirty_si->space;

        if (s == space_new) {
          /* Must be a space that has only locked objects, which we sweeep every time */
          continue;
        }

        for (i = 0; i < num_sweepers; i++)
          if (sweepers[i].sweep_tc == dirty_si->creator_tc) {
            dirty_si->dirty_next = DirtySegmentsAt(sweepers[i].dirty_segments, from_g, to_g);
            DirtySegmentsAt(sweepers[i].dirty_segments, from_g, to_g) = dirty_si;
            break;
          }
        
        if (i == num_sweepers) {
          dirty_si->dirty_next = DirtySegmentsAt(main_dirty_segments, from_g, to_g);
          DirtySegmentsAt(main_dirty_segments, from_g, to_g) = dirty_si;
        }
      }
    }
  }
#endif
}

static uptr sweep_dirty_segments(ptr tc_in, seginfo **dirty_segments) {
  IGEN youngest, min_youngest;
  ptr *pp, *ppn, *ppend, *nl, start;
  uptr seg, d;
  ISPC s;
  IGEN from_g, to_g;
  seginfo *dirty_si, *nextsi;
  uptr num_swept_bytes = 0;
  weakseginfo *local_weaksegments_to_resweep = NULL, *last_local_weaksegments_to_resweep = NULL;

  PUSH_BACKREFERENCE(Snil) /* '() => from unspecified old object */

  /* no new dirty registrations should happen while sweeping */
  for (from_g = MAX_CG + 1; from_g <= static_generation; INCRGEN(from_g)) {
    for (to_g = 0; to_g <= MAX_CG; to_g += 1) {
      dirty_si = DirtySegmentsAt(dirty_segments, from_g, to_g);
      DirtySegmentsAt(dirty_segments, from_g, to_g) = NULL;
      for (; dirty_si != NULL; dirty_si = nextsi) {
        nextsi = dirty_si->dirty_next;
        seg = dirty_si->number;
        s = dirty_si->space;

        /* reset min dirty byte so we can detect if byte is set while card is swept */
        dirty_si->min_dirty_byte = 0xff;

#ifndef ENABLE_PARALLEL
        if (s == space_new) {
          /* Must be a space that has only locked objects, which we sweeep every time */
          continue;
        }
#endif

        if (s == space_weakpair) {
          weakseginfo *next = local_weaksegments_to_resweep;
          find_room_voidp(tc_in, space_data, 0, ptr_align(sizeof(weakseginfo)), local_weaksegments_to_resweep);
          BITMASKOVERHEAD(tc_in, 0) += ptr_align(sizeof(weakseginfo));
          local_weaksegments_to_resweep->si = dirty_si;
          local_weaksegments_to_resweep->next = next;
          if (next == NULL)
            last_local_weaksegments_to_resweep = local_weaksegments_to_resweep;
        }
  
        min_youngest = 0xff;
        start = build_ptr(seg, 0);
        ppend = TO_VOIDP(start);

        /* The original allocation pointer may be relevant as the
           ending point. We assume that thread-local regions for all
           threads without a sweeper are terminated and won't get new
           allocations while dirty sweeping runs, while all
           allocations for a thread with a sweeper will be only using
           that tc, and no allocation happens for a non-target generation. */
        if (from_g == MAX_TG)
          nl = TO_VOIDP(ORIGNEXTLOC(tc_in, s));
        else
          nl = TO_VOIDP(NEXTLOC_AT(tc_in, s, from_g));

        d = 0;
        while (d < cards_per_segment) {
          uptr dend = d + sizeof(iptr);
          iptr *dp = (iptr *)(dirty_si->dirty_bytes + d);
          /* check sizeof(iptr) bytes at a time for 0xff */
          if (*dp == -1) {
            pp = ppend;
            ppend += bytes_per_card;
            d = dend;
          } else {
            while (d < dend) {
              pp = ppend;
              ppend += bytes_per_card / sizeof(ptr);
              if (pp <= nl && nl < ppend) ppend = nl;

              COUNT_SWEPT_BYTES(pp, ppend);

              if (dirty_si->dirty_bytes[d] <= MAX_CG) {
                /* start out with assumption that we won't find any wrong-way pointers */
                youngest = 0xff;

                if ((s == space_impure) || (s == space_immobile_impure)
                    || (s == space_impure_typed_object) || (s == space_count_impure)
                    || (s == space_closure)) {
                  if (dirty_si->marked_mask) {
                    while (pp < ppend) {
                      /* handle two pointers at a time */
                      if (marked(dirty_si, TO_PTR(pp))) {
                        relocate_dirty(pp, youngest, pp, 2 * ptr_bytes);
                        ppn = pp + 1;
                        relocate_dirty(ppn, youngest, pp, 2 * ptr_bytes);
                        pp = ppn + 1;
                      } else {
                        FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                        pp += 2;
                      }
                    }
                    FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                  } else {
                    // REMOVEME printf("dirty segment %p-%p (%d)\n", pp, ppend, youngest);
                    while (pp < ppend && *pp != forward_marker) {
                      /* handle two pointers at a time */
                      relocate_dirty(pp, youngest, pp, 2 * ptr_bytes);
                      ppn = pp + 1;
                      relocate_dirty(ppn, youngest, pp, 2 * ptr_bytes);
                      pp = ppn + 1;
                    }
                    // REMOVEME printf(" ended %p (%d)\n", pp, youngest);
                    FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                  }
                } else if (s == space_symbol) {
                  /* old symbols cannot overlap segment boundaries
                     since any object that spans multiple
                     segments begins at the start of a segment,
                     and symbols are much smaller (we assume)
                     than the segment size. */
                  pp = (ptr *)TO_VOIDP(build_ptr(seg,0)) +
                    ((pp - (ptr *)TO_VOIDP(build_ptr(seg,0))) /
                     (size_symbol / sizeof(ptr))) *
                    (size_symbol / sizeof(ptr));

                  /* might overshoot card by part of a symbol.  no harm. */
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    ptr p = TYPE(TO_PTR(pp), type_symbol);

                    if (!dirty_si->marked_mask || marked(dirty_si, p))
                      youngest = sweep_dirty_symbol(tc_in, p, youngest);
                    else
                      FLUSH_REMOTE_RANGE(tc_in, s, from_g);

                    pp += size_symbol / sizeof(ptr);
                  }
                  /* REMOVEME 
                  printf("symbol %d youngest %d at %p %ld{%ld} [<%p %p/%p !%p +>%d @%d]\n",
                         s, youngest, dirty_si, d, dend, ppend,
                         dirty_si->creator_tc, tc_in, nl, ((d < cards_per_segment) ? dirty_si->dirty_bytes[d+1] : -1), MAX_CG);
                  */
                  
                  FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                } else if (s == space_port) {
                  /* old ports cannot overlap segment boundaries
                     since any object that spans multiple
                     segments begins at the start of a segment,
                     and ports are much smaller (we assume)
                     than the segment size. */
                  pp = (ptr *)TO_VOIDP(build_ptr(seg,0)) +
                    ((pp - (ptr *)TO_VOIDP(build_ptr(seg,0))) /
                     (size_port / sizeof(ptr))) *
                    (size_port / sizeof(ptr));

                  /* might overshoot card by part of a port.  no harm. */
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    ptr p = TYPE(TO_PTR(pp), type_typed_object);

                    if (!dirty_si->marked_mask || marked(dirty_si, p))
                      youngest = sweep_dirty_port(tc_in, p, youngest);
                    else
                      FLUSH_REMOTE_RANGE(tc_in, s, from_g);

                    pp += size_port / sizeof(ptr);
                  }
                  
                  FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                } else if (s == space_impure_record) { /* abandon hope all ye who enter here */
                  ptr p;
                  if (dirty_si->marked_mask) {
                    /* To get to the start of a record, move backward as long as bytes
                       are marked and segment space+generation+marked is the same. */
                    uptr byte = segment_bitmap_byte(TO_PTR(pp));
                    uptr bit = segment_bitmap_bit(TO_PTR(pp));
                    uptr at_seg = seg;
                    seginfo *si = dirty_si;

                    while (si->marked_mask[byte] & (bit >> ptr_alignment))
                      bit >>= ptr_alignment;
                    if (bit == 1) {
                      /* try previous byte(s) */
                      while (1) {
                        if (byte == 0) {
                          seginfo *prev_si = MaybeSegInfo(at_seg-1);
                          if (prev_si
                              && (prev_si->space == si->space)
                              && (prev_si->generation == si->generation)
                              && prev_si->marked_mask
                              /* object can only continue from the previous segment
                                 if that segment is fully marked (including last words) */
                              && (prev_si->marked_mask[segment_bitmap_bytes-1] == record_full_marked_mask)) {
                            /* maybe the object continues from the previous segment, although
                               we don't really know... */
                            at_seg -= 1;
                            si = prev_si;
                            byte = segment_bitmap_bytes-1;
                          } else {
                            /* object does not continue from the previous segment */
                            break;
                          }
                        } else {
                          if (si->marked_mask[byte-1] == record_full_marked_mask) {
                            /* next byte is full, so keep looking */
                            byte--;
                          } else if (si->marked_mask[byte-1] & record_high_marked_bit) {
                            /* next byte continues, but is not full, so we can start
                               there */
                            if (at_seg != seg) {
                              /* in fact, we can start at the beginning of the
                                 next segment, because that segment's
                                 first object cannot start on this segment */
                              at_seg++;
                              byte = 0;
                              si = SegInfo(at_seg);
                            } else {
                              byte--;
                              bit = record_high_marked_bit;
                              /* find bit contiguous with highest bit */
                              while (si->marked_mask[byte] & (bit >> ptr_alignment))
                                bit >>= ptr_alignment;
                            }
                            break;
                          } else {
                            /* next byte is empty, so don't go there */
                            break;
                          }
                        }
                      }
                    }

                    /* `bit` and `byte` refer to a non-0 mark bit that must be
                       the start of an object */
                    p = build_ptr(at_seg, (byte << (log2_ptr_bytes+3)));
                    while (bit > ptr_alignment) {
                      p = (ptr)((uptr)p + byte_alignment);
                      bit >>= ptr_alignment;
                    }
                    p = TYPE(p, type_typed_object);

                    /* now sweep, but watch out for unmarked holes in the dirty region */
                    while ((ptr *)TO_VOIDP(UNTYPE(p, type_typed_object)) < ppend) {
                      seginfo *si = SegInfo(ptr_get_segment(p));
                      if (!marked(si, p)) {
                        /* skip unmarked words */
                        FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                        p = (ptr)((uptr)p + byte_alignment);
                      } else {
                        youngest = sweep_dirty_record(tc_in, p, youngest);
                        p = (ptr)((iptr)p +
                            size_record_inst(UNFIX(RECORDDESCSIZE(
                                  RECORDINSTTYPE(p)))));
                      }
                    }

                    FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                  } else {
                    uptr j; ptr pnext; seginfo *si;

                    /* synchronize on first record that overlaps the dirty
                       area, then relocate any mutable pointers in that
                       record and those that follow within the dirty area. */

                    /* find first segment of group of like segments */
                    j = seg - 1;
                    while ((si = MaybeSegInfo(j)) != NULL &&
                           si->space == s &&
                           si->generation == from_g &&
                           !si->marked_mask)
                      j -= 1;
                    j += 1;

                    /* now find first record in segment seg */
                    /* we count on following fact: if an object spans two
                       or more segments, then it starts at the beginning
                       of a segment */
                    for (;;) {
                      p = TYPE(build_ptr(j,0),type_typed_object);
                      pnext = (ptr)((iptr)p +
                                    size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
                      if (ptr_get_segment(pnext) >= seg) break;
                      j = ptr_get_segment(pnext) + 1;
                    }

                    /* now find first within dirty area */
                    while ((ptr *)TO_VOIDP(UNTYPE(pnext, type_typed_object)) <= pp) {
                      p = pnext;
                      pnext = (ptr)((iptr)p +
                                    size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
                    }

                    /* now sweep */
                    while ((ptr *)TO_VOIDP(UNTYPE(p, type_typed_object)) < ppend) {
                      /* quit on end of segment */
                      if (FWDMARKER(p) == forward_marker) break;

                      youngest = sweep_dirty_record(tc_in, p, youngest);
                      p = (ptr)((iptr)p +
                          size_record_inst(UNFIX(RECORDDESCSIZE(
                                RECORDINSTTYPE(p)))));
                    }

                    FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                  }
                } else if (s == space_weakpair) {
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    /* skip car field and handle cdr field */
                    if (!dirty_si->marked_mask || marked(dirty_si, TO_PTR(pp))) {
                      ptr *ppn = pp + 1;
                      relocate_dirty(ppn, youngest, pp, size_pair);
                      pp = ppn + 1;
                    } else {
                      FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                      pp += 2;
                    }
                  }

                  FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                } else if (s == space_ephemeron) {
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    ptr p = TYPE(TO_PTR(pp), type_pair);
                    if (!dirty_si->marked_mask || marked(dirty_si, p))
                      youngest = check_dirty_ephemeron(tc_in, p, youngest);
                    else
                      FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                    pp += size_ephemeron / sizeof(ptr);
                  }

                  FLUSH_REMOTE_RANGE(tc_in, s, from_g);
                } else {
                  S_error_abort("sweep_dirty(gc): unexpected space");
                }

                if (s == space_weakpair) {
                  local_weaksegments_to_resweep->youngest[d] = youngest;
                } else {
                  dirty_si->dirty_bytes[d] = youngest < from_g ? youngest : 0xff;
                }
                if (youngest < min_youngest) min_youngest = youngest;
              } else {
                if (dirty_si->dirty_bytes[d] < min_youngest) min_youngest = dirty_si->dirty_bytes[d];
              }
              d += 1;
            }
          }
        }
        if (s != space_weakpair) {
          record_dirty_segment(from_g, min_youngest, dirty_si);
        }
      }
    }
  }

  add_weaksegments_to_resweep(local_weaksegments_to_resweep, last_local_weaksegments_to_resweep);

  POP_BACKREFERENCE()

  return num_swept_bytes;
}

#ifndef ENABLE_PARALLEL
static void sweep_dirty(ptr tc) {
  (void)sweep_dirty_segments(tc, S_G.dirty_segments);
}
#endif

static void resweep_dirty_weak_pairs(ptr tc) {
  weakseginfo *ls;
  ptr *pp, *ppend, p;
  IGEN from_g, min_youngest, youngest;
  uptr d;

  /* Make sure terminator is in place for allocation areas relevant to this thread */
  for (from_g = MIN_TG; from_g <= static_generation; from_g++) {
    ptr old;
    old = NEXTLOC_AT(tc, space_weakpair, from_g);
    if (old != (ptr)0)
      *(ptr*)TO_VOIDP(old) = forward_marker;
  }

  for (ls = weaksegments_to_resweep; ls != NULL; ls = ls->next) {
    seginfo *dirty_si = ls->si;
    from_g = dirty_si->generation;
    ppend = TO_VOIDP(build_ptr(dirty_si->number, 0));
    min_youngest = 0xff;
    d = 0;
    while (d < cards_per_segment) {
      uptr dend = d + sizeof(iptr);
      iptr *dp = (iptr *)(dirty_si->dirty_bytes + d);
      /* check sizeof(iptr) bytes at a time for 0xff */
      if (*dp == -1) {
        d = dend;
        ppend += bytes_per_card;
      } else {
        while (d < dend) {
          pp = ppend;
          ppend += bytes_per_card / sizeof(ptr);
          if (dirty_si->dirty_bytes[d] <= MAX_CG) {
            youngest = ls->youngest[d];
            while (pp < ppend) {
              if (!dirty_si->marked_mask && *pp == forward_marker)
                break;
              if (!dirty_si->marked_mask || marked(dirty_si, TO_PTR(pp))) {
                p = *pp;
                seginfo *si;

                /* handle car field */
                if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
                  if (si->old_space) {
                    if (new_marked(si, p)) {
                      youngest = TARGET_GENERATION(si);
                    } else if (FORWARDEDP(p, si)) {
                      IGEN newpg;
                      *pp = FWDADDRESS(p);
                      newpg = TARGET_GENERATION(si);
                      if (newpg < youngest) youngest = newpg;
                    } else {
                      *pp = Sbwp_object;
                    }
                  } else {
                    IGEN pg = si->generation;
                    if (pg < youngest) youngest = pg;
                  }
                }
              }

              /* skip cdr field */
              pp += 2;
            }

            dirty_si->dirty_bytes[d] = youngest < from_g ? youngest : 0xff;
            if (youngest < min_youngest) min_youngest = youngest;
          } else {
            if (dirty_si->dirty_bytes[d] < min_youngest) min_youngest = dirty_si->dirty_bytes[d];
          }
          d += 1;
        }
      }
    }
    record_dirty_segment(from_g, min_youngest, dirty_si);
  }
}

static void add_pending_guardian(ptr gdn, ptr tconc)
{
  seginfo *si = SegInfo(ptr_get_segment(tconc));
  INITGUARDIANNEXT(gdn) = si->trigger_guardians;
  si->trigger_guardians = gdn;
  si->has_triggers = 1;
}

static void add_trigger_guardians_to_recheck(ptr ls)
{
  ptr last = ls, next;

  GC_TC_MUTEX_ACQUIRE();

  next = GUARDIANNEXT(ls);
  while (next != 0) {
    last = next;
    next = GUARDIANNEXT(next);
  }
  INITGUARDIANNEXT(last) = recheck_guardians_ls;
  recheck_guardians_ls = ls;

  GC_TC_MUTEX_RELEASE();
}

static void ephemeron_remove(ptr pe) {
  ptr next = EPHEMERONNEXT(pe);
  *((ptr *)TO_VOIDP(EPHEMERONPREVREF(pe))) = next;
  if (next)
    EPHEMERONPREVREF(next) = EPHEMERONPREVREF(pe);
  EPHEMERONPREVREF(pe) = 0;
  EPHEMERONNEXT(pe) = 0;
}

static void ephemeron_add(ptr *first, ptr pe) {
  ptr last_pe = pe, next_pe = EPHEMERONNEXT(pe), next;
  while (next_pe != 0) {
    last_pe = next_pe;
    next_pe = EPHEMERONNEXT(next_pe);
  }
  next = *first;
  *first = pe;
  EPHEMERONPREVREF(pe) = TO_PTR(first);
  EPHEMERONNEXT(last_pe) = next;
  if (next)
    EPHEMERONPREVREF(next) = TO_PTR(&EPHEMERONNEXT(last_pe));
}

static void add_ephemeron_to_pending(ptr tc_in, ptr pe) {
  /* We could call check_ephemeron directly here, but the indirection
     through `PENDINGEPHEMERONS` can dramatically decrease the number
     of times that we have to trigger re-checking, especially since
     check_pending_pehemerons() is run only after all other sweep
     opportunities are exhausted. */
  if (EPHEMERONPREVREF(pe)) ephemeron_remove(pe);
  ephemeron_add(&PENDINGEPHEMERONS(tc_in), pe);
}

static void add_trigger_ephemerons_to_pending(ptr tc_in, ptr pe) {
  ephemeron_add(&PENDINGEPHEMERONS(tc_in), pe);
}

static void check_ephemeron(ptr tc_in, ptr pe) {
  ptr p;
  seginfo *si;
  IGEN from_g;
  PUSH_BACKREFERENCE(pe);

  EPHEMERONNEXT(pe) = 0;
  EPHEMERONPREVREF(pe) = 0;
  
  from_g = GENERATION(pe);
  
  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->old_space) {
    if (SEGMENT_IS_LOCAL(si, p)) {
      if (new_marked(si, p)) {
#ifndef NO_DIRTY_NEWSPACE_POINTERS
        IGEN tg = TARGET_GENERATION(si);
        if (tg < from_g) S_record_new_dirty_card(tc_in, &INITCAR(pe), tg);
#endif
        relocate_impure(&INITCDR(pe), from_g, pe, size_ephemeron);
      } else if (FORWARDEDP(p, si)) {
#ifndef NO_DIRTY_NEWSPACE_POINTERS
        IGEN tg = TARGET_GENERATION(si);
        if (tg < from_g) S_record_new_dirty_card(tc_in, &INITCAR(pe), tg);
#endif
        INITCAR(pe) = FWDADDRESS(p);
        relocate_impure(&INITCDR(pe), from_g, pe, size_ephemeron);
      } else {
        /* Not reached, so far; install as trigger */
        ephemeron_add(&si->trigger_ephemerons, pe);
        si->has_triggers = 1;
      }
    } else {
      RECORD_REMOTE_RANGE_TO(tc_in, pe, size_ephemeron, SWEEPER(si->creator_tc));
    }
  } else {
    relocate_impure(&INITCDR(pe), from_g, pe, size_ephemeron);
  }

  FLUSH_REMOTE_RANGE(tc_in, space_ephemeron, from_g);

  POP_BACKREFERENCE();
}

static void check_pending_ephemerons(ptr tc_in) {
  ptr pe, next_pe;

  pe = PENDINGEPHEMERONS(tc_in);
  PENDINGEPHEMERONS(tc_in) = 0;

  while (pe != 0) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron(tc_in, pe);
    pe = next_pe;
  }
}

/* Like check_ephemeron(), but for a dirty, old-generation
   ephemeron (that was not yet added to the pending list), so we can
   be less pessimistic than setting `youngest` to the target
   generation: */
static IGEN check_dirty_ephemeron(ptr tc_in, ptr pe, IGEN youngest) {
  ptr p;
  seginfo *si;
  IGEN pg;
  PUSH_BACKREFERENCE(pe);
 
  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
    if (si->old_space) {
      if (SEGMENT_IS_LOCAL(si, p)) {
        if (new_marked(si, p)) {
          relocate_dirty(&INITCDR(pe), youngest, pe, size_ephemeron);
        } else if (FORWARDEDP(p, si)) {
          INITCAR(pe) = GET_FWDADDRESS(p);
          relocate_dirty(&INITCDR(pe), youngest, pe, size_ephemeron);
        } else {
          /* Not reached, so far; add to pending list */
          add_ephemeron_to_pending(tc_in, pe);

          /* Make the consistent (but pessimistic w.r.t. to wrong-way
             pointers) assumption that the key will stay live and move
             to the target generation. That assumption covers the value
             part, too, since it can't end up younger than the target
             generation. */
          if (youngest != MIN_TG && (pg = TARGET_GENERATION(si)) < youngest)
            youngest = pg;
        }
      } else {
        RECORD_REMOTE_RANGE_TO(tc_in, pe, size_ephemeron, SWEEPER(si->creator_tc));
        return youngest;
      }
    } else {
      if (youngest != MIN_TG && (pg = si->generation) < youngest)
        youngest = pg;
      relocate_dirty(&INITCDR(pe), youngest, pe, size_ephemeron);
    }
  } else {
    /* Non-collectable key means that the value determines
       `youngest`: */
    relocate_dirty(&INITCDR(pe), youngest, pe, size_ephemeron);
  }

  POP_BACKREFERENCE()

  return youngest;
}

static void finish_pending_ephemerons(ptr tc_in, seginfo *si) {
  /* Any ephemeron still in a trigger list is an ephemeron
     whose key was not reached. */
  if (PENDINGEPHEMERONS(tc_in) != 0)
    S_error_abort("clear_trigger_ephemerons(gc): non-empty pending list");

  for (; si != NULL; si = si->next) {
    if (si->trigger_ephemerons) {
      ptr pe, next_pe;
      for (pe = si->trigger_ephemerons; pe != 0; pe = next_pe) {
        INITCAR(pe) = Sbwp_object;
        INITCDR(pe) = Sbwp_object;
        next_pe = EPHEMERONNEXT(pe);
        EPHEMERONPREVREF(pe) = 0;
        EPHEMERONNEXT(pe) = 0;
      }
      si->trigger_ephemerons = 0;
    }
  }
}

#ifdef ENABLE_OBJECT_COUNTS
static uptr total_size_so_far() {
  IGEN g;
  int i;
  uptr total = 0;

  for (g = 0; g <= static_generation; g += 1) {
    for (i = 0; i < countof_types; i += 1) {
      uptr bytes;
      bytes = S_G.bytesof[g][i];
      if (bytes == 0) bytes = S_G.countof[g][i] * S_G.countof_size[i];
      total += bytes;
    }
  }

  return total - count_root_bytes;
}
#endif

static uptr target_generation_space_so_far(ptr tc) {
  IGEN g;
  ISPC s;
  uptr sz = 0;

  for (g = MIN_TG; g <= MAX_TG; g++) {
    sz += S_G.bytesof[g][countof_phantom];
    
    for (s = 0; s <= max_real_space; s++) {
      sz += S_G.bytes_of_space[g][s];
      if (NEXTLOC_AT(tc, s, g) != FIX(0))
        sz += (uptr)NEXTLOC_AT(tc, s, g) - (uptr)BASELOC_AT(tc, s, g);
    }
  }

  return sz;
}

void copy_and_clear_list_bits(ptr tc_in, seginfo *oldspacesegments) {
  seginfo *si;
  int i;

  /* Update bits that are used by `list-assuming-immutable?`. */

  for (si = oldspacesegments; si != NULL; si = si->next) {
    if (si->list_bits) {
      if ((si->generation == 1) && !si->marked_mask) {
        /* drop (former) generation-0 bits, because probably the relevant pairs
           were short-lived, and it's ok to recompute them if needed */
      } else {
        if (si->marked_mask) {
          /* Besides marking or copying `si->list_bits`, clear bits
             where there's no corresponding mark bit, so we don't try to
             check forwarding in a future GC */
          seginfo *bits_si = SegInfo(ptr_get_segment(TO_PTR(si->list_bits)));
        
          if (bits_si->old_space) {
            if (bits_si->use_marks) {
              if (!bits_si->marked_mask)
                init_mask(tc_in, bits_si->marked_mask, bits_si->generation, 0);
              bits_si->marked_mask[segment_bitmap_byte(TO_PTR(si->list_bits))] |= segment_bitmap_bit(TO_PTR(si->list_bits));
            } else {
              octet *copied_bits;
              find_room_voidp(tc_in, space_data, bits_si->generation, ptr_align(segment_bitmap_bytes), copied_bits);
              memcpy_aligned(copied_bits, si->list_bits, segment_bitmap_bytes);
              si->list_bits = copied_bits;
              S_G.bitmask_overhead[bits_si->generation] += ptr_align(segment_bitmap_bytes);
            }
          }

          for (i = 0; i < segment_bitmap_bytes; i++) {
            int m = si->marked_mask[i];
            si->list_bits[i] &= mask_bits_to_list_bits_mask(m);
          }
        }

        if (si->use_marks) {
          /* No forwarding possible from this segment */
        } else {
          /* For forwarded pointers, copy over list bits */
          for (i = 0; i < segment_bitmap_bytes; i++) {
            if (si->list_bits[i]) {
              int bitpos;
              for (bitpos = 0; bitpos < 8; bitpos += ptr_alignment) {
                int bits = si->list_bits[i] & (list_bits_mask << bitpos);
                if (bits != 0) {
                  ptr p = build_ptr(si->number, ((i << (log2_ptr_bytes+3)) + (bitpos << log2_ptr_bytes)));
                  if (FWDMARKER(p) == forward_marker) {
                    ptr new_p = FWDADDRESS(p);
                    seginfo *new_si = SegInfo(ptr_get_segment(new_p));
                    if (!new_si->list_bits)
                      init_mask(tc_in, new_si->list_bits, new_si->generation, 0);
                    bits >>= bitpos;
                    new_si->list_bits[segment_bitmap_byte(new_p)] |= segment_bitmap_bits(new_p, bits);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/* **************************************** */

#ifdef ENABLE_PARALLEL

static int sweep_mutex_initialized = 0;
static s_thread_mutex_t sweep_mutex;
static s_thread_cond_t sweep_cond;

static int num_running_sweepers;

static void gather_active_sweepers() {
  int i, n;

  /* assign a tc for each sweeper to run in parallel */
  for (n = 0, i = 0; (n < maximum_parallel_collect_threads) && (i < S_collect_waiting_threads); i++) {
    if ((i < maximum_parallel_collect_threads) && (S_collect_waiting_tcs[i] != (ptr)0)) {
      ptr tc = S_collect_waiting_tcs[i];
      if (sweeper_started(n)) {
        sweepers[n].sweep_tc = tc;
        WILL_BE_SWEEPER(tc) = n;
        n++;
      } else
        break;
    }
  }

  num_sweepers = n;
}

static s_thread_rv_t start_sweeper(void *_data) {
  gc_thread_data *data = _data;
  ptr tc;
  iptr num_swept_bytes;
  IGEN g;
#ifdef ENABLE_TIMING
  uptr sweep_accum = 0;
#endif

  (void)s_thread_mutex_lock(&sweep_mutex);
  while (1) {
    while (data->status != SWEEPER_SWEEPING) {
      s_thread_cond_wait(&sweep_cond, &sweep_mutex);
    }
    GET_TIME(start);
    /* REMOVEME printf("START %ld\n", SWEEPER(data->sweep_tc)); */
    (void)s_thread_mutex_unlock(&sweep_mutex);

    tc = data->sweep_tc;
    s_thread_setspecific(S_tc_key, tc);

    if (data->thread) {
      /* sweep tc in this sweeper, so that things it references are
         more likely handled in this sweeper: */
      seginfo *t_si = SegInfo(ptr_get_segment(data->thread));
      sweep_thread(tc, data->thread);
      FLUSH_REMOTE_RANGE(tc, t_si->space, t_si->generation);
      data->thread = (ptr)0;
    }

    num_swept_bytes = 0;
    num_swept_bytes += sweep_dirty_segments(tc, data->dirty_segments);
    num_swept_bytes += sweep_generation_trading_work(tc);

    /* ensure terminators on any segment where sweeper may have allocated: */
    {
      ISPC s; IGEN g;
      for (s = 0; s <= max_real_space; s++) {
        for (g = MIN_TG; g <= MAX_TG; g++) {
          ptr old = NEXTLOC_AT(tc, s, g);
          if (old != (ptr)0)
            *(ptr*)TO_VOIDP(old) = forward_marker;
        }
      }
    }
    
    (void)s_thread_mutex_lock(&sweep_mutex);
    /* REMOVEME printf("FINISH %ld\n", SWEEPER(tc)); */
    S_G.bitmask_overhead[0] += BITMASKOVERHEAD(tc, 0);
    BITMASKOVERHEAD(tc, 0) = 0;
    for (g = MIN_TG; g <= MAX_TG; g++)
      S_G.bitmask_overhead[g] += BITMASKOVERHEAD(tc, g);
    data->status = SWEEPER_READY;
    SWEEPER(tc) = main_sweeper_index;
    ACCUM_TIME(sweep_accum, step, start);
    REPORT_TIME(fprintf(stderr, "%d swp  +%ld ms  %ld ms  %ld bytes  [%p]\n", MAX_CG, step, sweep_accum, num_swept_bytes, tc));

    s_thread_cond_signal(&data->done_cond);
  }

  s_thread_return;
}

static IBOOL sweeper_started(int i) {
  if (!sweep_mutex_initialized) {
    s_thread_mutex_init(&sweep_mutex);
    s_thread_cond_init(&sweep_cond);
    s_thread_cond_init(&sweepers[main_sweeper_index].work_cond);
    sweep_mutex_initialized = 1;
  }

  if (sweepers[i].status == SWEEPER_NONE) {
    int status;
    
    sweepers[i].status = SWEEPER_READY;
    s_thread_cond_init(&sweepers[i].done_cond);
    s_thread_cond_init(&sweepers[i].work_cond);
    
    if ((status = s_thread_create(start_sweeper, &sweepers[i])) != 0) {
      /* eror creating a thread; just go with as many as we have */
      sweepers[i].status = SWEEPER_NONE;
      s_thread_cond_destroy(&sweepers[i].done_cond);
      return 0;
    }
  }

  return 1;
}

static void parallel_sweep_dirty_and_generation(ptr tc) {
  int i;
  iptr num_swept_bytes;

  REPORT_TIME(fprintf(stderr, "------\n"));
  GET_TIME(start);

  S_use_gc_tc_mutex = 1;

  /* start other sweepers */
  (void)s_thread_mutex_lock(&sweep_mutex);
  sweepers[main_sweeper_index].status = SWEEPER_SWEEPING;
  for (i = 0; i < num_sweepers; i++) {
    sweepers[i].status = SWEEPER_SWEEPING;
    SWEEPER(sweepers[i].sweep_tc) = i;
    num_running_sweepers++;
  }
  s_thread_cond_broadcast(&sweep_cond);
  num_running_sweepers++;
  /* REMOVEME printf("START %ld %d/%d/%d\n", SWEEPER(tc), MAX_CG, MIN_TG, MAX_TG); */
  (void)s_thread_mutex_unlock(&sweep_mutex);
  
  /* sweep in the main thread */
  num_swept_bytes = 0;
  num_swept_bytes += sweep_dirty_segments(tc, main_dirty_segments);
  num_swept_bytes += sweep_generation_trading_work(tc);
  
  /* wait for other sweepers */
  (void)s_thread_mutex_lock(&sweep_mutex);
  /* REMOVEME printf("FINISH %ld\n", SWEEPER(tc)); */
  for (i = 0; i < num_sweepers; i++) {
    while (sweepers[i].status != SWEEPER_READY) {
      s_thread_cond_wait(&sweepers[i].done_cond, &sweep_mutex);
    }
    S_flush_instruction_cache(sweepers[i].sweep_tc);
  }
  (void)s_thread_mutex_unlock(&sweep_mutex);

  ACCUM_TIME(par_accum, step, start);
  REPORT_TIME(fprintf(stderr, "%d par  +%ld ms  %ld ms  %ld bytes  [%p]\n", MAX_CG, step, par_accum, num_swept_bytes, tc));

  S_use_gc_tc_mutex = 0;
}

static iptr sweep_generation_trading_work(ptr tc) {
  iptr num_swept_bytes = 0;

  num_swept_bytes += sweep_generation_pass(tc);
  
  s_thread_mutex_lock(&sweep_mutex);
  --num_running_sweepers;
  while (1) {
    int me = SWEEPER(tc);
    if ((num_running_sweepers == 0)
        && (sweepers[me].ranges_received == NULL)) {
      /* everyone is done */
      int i, they = main_sweeper_index;
      /* REMOVEME printf("zero %ld\n", SWEEPER(tc)); */
      for (i = -1; i < num_sweepers; i++) {
        s_thread_cond_signal(&sweepers[they].work_cond);
        they = i + 1;
      }
      s_thread_mutex_unlock(&sweep_mutex);
      return num_swept_bytes;
    } else {
      /* wait for work */
      if (sweepers[me].ranges_received != NULL) {
        /* some work appeared since we last checked */
        num_running_sweepers++;
      } else {
        sweepers[me].status = SWEEPER_WAITING_FOR_WORK;
        /* REMOVEME printf("sleep %d\n", me); */
        s_thread_cond_wait(&sweepers[me].work_cond, &sweep_mutex);
        /* REMOVEME printf("wake %d\n", me); */
      }
      if (sweepers[me].status != SWEEPER_WAITING_FOR_WORK) {
        /* got work; num_running_sweepers was incremented, too */
        s_thread_mutex_unlock(&sweep_mutex);
        num_swept_bytes += sweep_generation_pass(tc);
        s_thread_mutex_lock(&sweep_mutex);
        --num_running_sweepers;
      } else if (num_running_sweepers == 0) {
        /* other sweeper noticed that everyone is done */
        s_thread_mutex_unlock(&sweep_mutex);
        return num_swept_bytes;
      } else {
        /* not clear why we were woken, so just go around again */
      }
    }
  }
}

static remote_range *send_and_receive_remote_ranges(ptr tc) {
  int i, me = SWEEPER(tc), they;
  remote_range *r, *next, *dirty_ranges = NULL;

  s_thread_mutex_lock(&sweep_mutex);

  they = main_sweeper_index;
  for (i = -1; i < num_sweepers; i++) {
    if (sweepers[me].ranges_to_send[they] != NULL) {
      SWEEPCHANGE(tc) = SWEEP_CHANGE_PROGRESS;
      r = sweepers[me].ranges_to_send[they];
      sweepers[me].ranges_to_send[they] = NULL;
      for (next = r; next->next != NULL; next = next->next);
      next->next = sweepers[they].ranges_received;
      sweepers[they].ranges_received = r;
      if (sweepers[they].status == SWEEPER_WAITING_FOR_WORK) {
        num_running_sweepers++;
        sweepers[they].status = SWEEPER_SWEEPING;
        s_thread_cond_signal(&sweepers[they].work_cond);
      }
    }
    they = i + 1;
  }

  r = sweepers[me].ranges_received;
  sweepers[me].ranges_received = NULL;

  s_thread_mutex_unlock(&sweep_mutex);

  if (r != NULL) {
    SWEEPCHANGE(tc) = SWEEP_CHANGE_PROGRESS;
    for (; r != NULL; r = next) {
      /* REMOVEME printf("%d receive %p-%p [%d, %d]\n", me, r->start, r->end, r->s, r->g); */
      next = r->next;
      if (r->g > MAX_TG) {
        r->next = dirty_ranges;
        dirty_ranges = r;
      } else {
        r->next = LOCALRANGES_AT(tc, r->s, r->g);
        LOCALRANGES_AT(tc, r->s, r->g) = r;
      }
    }
  }

  return dirty_ranges;
}

#endif

/* **************************************** */

#ifdef ENABLE_MEASURE

static void init_measure(ptr tc, IGEN min_gen, IGEN max_gen) {
  uptr init_stack_len = 1024;

  min_measure_generation = min_gen;
  max_measure_generation = max_gen;
  
  find_room_voidp(tc, space_data, 0, ptr_align(init_stack_len), measure_stack_start);
  S_G.bitmask_overhead[0] += ptr_align(init_stack_len);
  measure_stack = TO_VOIDP(measure_stack_start);
  measure_stack_limit = TO_VOIDP((uptr)TO_PTR(measure_stack_start) + init_stack_len);

  measured_seginfos = Snil;

  measure_all_enabled = 1;
}

static void finish_measure() {
  ptr ls;

  for (ls = measured_seginfos; ls != Snil; ls = Scdr(ls)) {
    ptr pe, next_pe;
    seginfo *si = TO_VOIDP(Scar(ls));
    si->measured_mask = NULL;
    for (pe = si->trigger_ephemerons; pe != 0; pe = next_pe) {
      next_pe = EPHEMERONNEXT(pe);
      EPHEMERONPREVREF(pe) = 0;
      EPHEMERONNEXT(pe) = 0;
    }
    si->trigger_ephemerons = 0;
  }

  measure_all_enabled = 0;
}

static void init_counting_mask(ptr tc_in, seginfo *si) {
  init_mask(tc_in, si->counting_mask, 0, 0);
}

static void init_measure_mask(ptr tc_in, seginfo *si) {
  init_mask(tc_in, si->measured_mask, 0, 0);
  measured_seginfos = S_cons_in(tc_in, space_new, 0, TO_PTR(si), measured_seginfos);
}

#define measure_unreached(si, p) \
  (!si->measured_mask \
   || !(si->measured_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))

#define measure_mask_set(mm, si, p) \
  mm[segment_bitmap_byte(p)] |= segment_bitmap_bit(p)
#define measure_mask_unset(mm, si, p) \
  mm[segment_bitmap_byte(p)] -= segment_bitmap_bit(p)

static void push_measure(ptr tc_in, ptr p)
{
  seginfo *si = MaybeSegInfo(ptr_get_segment(p));

  if (!si)
    return;

  if (si->old_space) {
    /* We must be in a GC--measure fusion, so switch back to GC */
    relocate_pure_help_help(&p, p, si, NULL, 0);
    return;
  }

  if (si->generation > max_measure_generation)
    return;
  else if (si->generation < min_measure_generation) {
    /* this only happens in fusion mode, too; si must be a new segment */
    return;
  } else {
    uptr byte = segment_bitmap_byte(p);
    uptr bit = segment_bitmap_bit(p);

    if (!si->measured_mask)
      init_measure_mask(tc_in, si);
    else if (si->measured_mask[byte] & bit)
      return;

    si->measured_mask[byte] |= bit;
  }

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = 0;
  }

  if (measure_stack == measure_stack_limit) {
    uptr sz = ptr_bytes * (measure_stack_limit - measure_stack_start);
    uptr new_sz = 2*sz;
    ptr *new_measure_stack;
    find_room_voidp(tc_in, space_data, 0, ptr_align(new_sz), new_measure_stack);
    S_G.bitmask_overhead[0] += ptr_align(new_sz);
    memcpy(new_measure_stack, measure_stack_start, sz);
    measure_stack_start = new_measure_stack;
    measure_stack_limit = TO_VOIDP((uptr)TO_PTR(new_measure_stack) + new_sz);
    measure_stack = TO_VOIDP((uptr)TO_PTR(new_measure_stack) + sz);
  }
  
  *(measure_stack++) = p;
}

static void measure_add_stack_size(ptr stack, uptr size) {
  seginfo *si = SegInfo(ptr_get_segment(stack));
  if (!(si->old_space)
      && (si->generation <= max_measure_generation)
      && (si->generation >= min_measure_generation))
    measure_total += size;
}

static void add_ephemeron_to_pending_measure(ptr tc_in, ptr pe) {
  /* If we're in hybrid mode and the key in `pe` is in the
     old space, then we need to use the regular pending list
     instead of the measure-specific one */
  seginfo *si;
  ptr p = Scar(pe);

  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->old_space)
    add_ephemeron_to_pending(tc_in, pe);
  else {
    if (EPHEMERONPREVREF(pe))
      S_error_abort("add_ephemeron_to_pending_measure: ephemeron is in some list");
    ephemeron_add(&pending_measure_ephemerons, pe);
  }
}

static void add_trigger_ephemerons_to_pending_measure(ptr pe) {
  ephemeron_add(&pending_measure_ephemerons, pe);
}

static void check_ephemeron_measure(ptr tc_in, ptr pe) {
  ptr p;
  seginfo *si;

  EPHEMERONPREVREF(pe) = 0;
  EPHEMERONNEXT(pe) = 0;

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL
      && (si->generation <= max_measure_generation)
      && (si->generation >= min_measure_generation)
      && (!(si->old_space) || !FORWARDEDP(p, si))
      && (measure_unreached(si, p)
          || (si->counting_mask
              && (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))))) {
    /* Not reached, so far; install as trigger */
    ephemeron_add(&si->trigger_ephemerons, pe);
    if (!si->measured_mask)
      init_measure_mask(tc_in, si); /* so triggers are cleared at end */
    return;
  }

  p = Scdr(pe);
  if (!IMMEDIATE(p))
    push_measure(tc_in, p);
}

static void check_pending_measure_ephemerons(ptr tc_in) {
  ptr pe, next_pe;

  pe = pending_measure_ephemerons;
  pending_measure_ephemerons = 0;
  while (pe != 0) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron_measure(tc_in, pe);
    pe = next_pe;
  }
}

void gc_measure_one(ptr tc_in, ptr p) {
  seginfo *si = SegInfo(ptr_get_segment(p));

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = 0;
  }
  
  measure(tc_in, p);

  (void)flush_measure_stack(tc_in);
}

IBOOL flush_measure_stack(ptr tc_in) {
  if ((measure_stack <= measure_stack_start)
      && !pending_measure_ephemerons)
    return 0;
  
  while (1) {
    while (measure_stack > measure_stack_start)
      measure(tc_in, *(--measure_stack));

    if (!pending_measure_ephemerons)
      break;
    check_pending_measure_ephemerons(tc_in);
  }

  return 1;
}

ptr S_count_size_increments(ptr ls, IGEN generation) {
  ptr l, totals = Snil, totals_prev = 0;
  ptr tc = get_thread_context();

  tc_mutex_acquire();

  init_measure(tc, 0, generation);

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));

      if (!si->measured_mask)
        init_measure_mask(tc, si);
      measure_mask_set(si->measured_mask, si, p);

      if (!si->counting_mask)
        init_counting_mask(tc, si);
      measure_mask_set(si->counting_mask, si, p);
    }
  }

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);

    measure_total = 0;

    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));
      measure_mask_unset(si->counting_mask, si, p);
      gc_measure_one(tc, p);
    }

    p = Scons(FIX(measure_total), Snil);
    if (totals_prev)
      Scdr(totals_prev) = p;
    else
      totals = p;
    totals_prev = p;
  }

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));
      si->counting_mask = NULL;
    }
  }

  finish_measure();

  tc_mutex_release();

  return totals;
}

#endif
