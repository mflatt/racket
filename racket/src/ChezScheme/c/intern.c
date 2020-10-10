/* intern.c
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

/* locally defined functions */
static void init_oblist(intern_oblist *oblist);
static void oblist_insert PROTO((ptr tc, intern_oblist *oblist, ptr sym, iptr idx, IGEN g));
static iptr hash PROTO((const unsigned char *s, iptr n));
static iptr hash_sc PROTO((const string_char *s, iptr n));
static iptr hash_uname PROTO((const string_char *s, iptr n));
static ptr mkstring PROTO((const string_char *s, iptr n));

#define OBINDEX(hc, len) ((hc) & ((len) - 1))
#define MIN_OBLIST_LENGTH 4096

void S_intern_init() {
    if (!S_boot_time) return;

    init_oblist(&S_G.main_oblist);
}

/* need tc mutex and allocation mutex */
static void init_oblist(intern_oblist *oblist) {
  IGEN g;

  oblist->length = MIN_OBLIST_LENGTH;
  oblist->count = 0;
  oblist->oblist = S_getmem(oblist->length * sizeof(bucket *), 1);

  oblist->refcount = 1;

  for (g = 0; g < static_generation; g += 1)
    oblist->buckets_of_generation[g] = NULL;

#ifdef PTHREADS
  s_thread_mutex_init(&oblist->mutex.pmutex);
  oblist->mutex.owner = 0;
  oblist->mutex.count = 0;
#endif

  S_G.bytesof[static_generation][countof_oblist] += oblist->length * sizeof(bucket *);
}

static void oblist_insert(ptr tc, intern_oblist *oblist, ptr sym, iptr idx, IGEN g) {
  bucket *b, *oldb, **pb;
    
  find_room_voidp(tc, g == 0 ? space_new : space_data, g, ptr_align(sizeof(bucket)), b);
  b->sym = sym;
  if (g == 0) {
    b->next = oblist->oblist[idx];
    STORE_FENCE(); /* so oblists chained to this one can read without a lock */
    oblist->oblist[idx] = b;
  } else {
    for (pb = &oblist->oblist[idx]; (oldb = *pb) != NULL && SegmentGeneration(addr_get_segment(TO_PTR(oldb))) < g; pb = &oldb->next);
    b->next = oldb;
    STORE_FENCE(); /* so oblists chained to this one can read without a lock */
    *pb = b;
  }

  if (g != static_generation) {
    bucket_list *bl;
    find_room_voidp(tc, g == 0 ? space_new : space_data, g, ptr_align(sizeof(bucket_list)), bl);
    bl->car = b;
    bl->cdr = oblist->buckets_of_generation[g];
    oblist->buckets_of_generation[g] = bl;
  }

  oblist->count += 1;
}

void S_resize_oblist(intern_oblist *oblist) {
  bucket **new_oblist, *b, *oldb, **pb, *bnext;
  iptr new_oblist_length, i, idx, inc = 0, dinc = 0;
  ptr sym;
  IGEN g;

  new_oblist_length = MIN_OBLIST_LENGTH;
  while ((new_oblist_length >> 1) < oblist->count)
    new_oblist_length <<= 1;

  if (new_oblist_length == oblist->length)
    return;

  new_oblist = S_getmem(new_oblist_length * sizeof(bucket *), 1);

  for (i = 0; i < oblist->length; i += 1) {
    for (b = oblist->oblist[i]; b != NULL; b = bnext) {
      int done = 0;
      bnext = b->next;
      sym = b->sym;
      idx = OBINDEX(UNFIX(SYMHASH(sym)), new_oblist_length);
      g = GENERATION(sym);

      for (pb = &new_oblist[idx]; (oldb = *pb) != NULL && SegmentGeneration(addr_get_segment(TO_PTR(oldb))) < g; pb = &oldb->next) {
        inc++;
        if (done)
          dinc++;
        done = 1;
      }
      b->next = oldb;
      *pb = b;
    }
  }

  S_freemem(oblist->oblist, oblist->length * sizeof(bucket *));
  alloc_mutex_acquire();
  S_G.bytesof[static_generation][countof_oblist] += (new_oblist_length - oblist->length) * sizeof(bucket *);
  alloc_mutex_release();

  oblist->length = new_oblist_length;
  oblist->oblist = new_oblist;
}

void S_push_intern_table(int pop) {
  ptr tc = get_thread_context();
  intern_oblist *oblist;

  return;

  oblist = malloc(sizeof(intern_oblist));

  tc_mutex_acquire();

  alloc_mutex_acquire();
  init_oblist(oblist);
  alloc_mutex_release();

  oblist->all_prev = &S_G.main_oblist;
  oblist->all_next = S_G.main_oblist.all_next;
  S_G.main_oblist.all_next = oblist;
  if (oblist->all_next)
    oblist->all_next->all_prev = oblist;

  oblist->next = THREAD_GC(tc)->oblist;
  while ((pop > 0) && (oblist->next != &S_G.main_oblist)) {
    --pop;
    oblist->next = oblist->next->next;
  }

  S_oblist_retain(oblist->next);
  S_oblist_release(THREAD_GC(tc)->oblist);

  THREAD_GC(tc)->oblist = oblist;

  tc_mutex_release();
}

/* tc mutex must be held */
void S_oblist_retain(intern_oblist *oblist) {
  oblist->refcount++;
}

/* tc mutex must be held */
void S_oblist_release(intern_oblist *oblist) {
  while (1) {
    if (--oblist->refcount == 0) {
      intern_oblist *next = oblist->next;

      if (oblist == &S_G.main_oblist)
        S_error_abort("main oblist is no longer retained");

      oblist->all_prev->all_next = oblist->all_next;
      if (oblist->all_next)
        oblist->all_next->all_prev = oblist->all_prev;

      alloc_mutex_acquire();
      S_G.bytesof[static_generation][countof_oblist] -= oblist->length * sizeof(bucket *);
      alloc_mutex_release();

      s_thread_mutex_destroy(&oblist->mutex.pmutex);

      free(oblist);

      oblist = next;
    } else
      break;
  }
}

#define MIX_HASH(hc) (hc += (hc << 10), hc ^= (hc >> 6))

static iptr hash(const unsigned char *s, iptr n) {
  uptr h = (uptr)n + 401887359;
  while (n--) { h += *s++; MIX_HASH(h); }
  return (iptr)h & most_positive_fixnum;
}

static iptr hash_sc(const string_char *s, iptr n) {
  uptr h = (uptr)n + 401887359;
  while (n--) { h += Schar_value(*s++); MIX_HASH(h); }
  return (iptr)h & most_positive_fixnum;
}

static iptr hash_uname(const string_char *s, iptr n) {
  /* attempting to get dissimilar hash codes for gensyms created in the same session */
  iptr i = n, h = 0; iptr pos = 1; int d, c;

  while (i-- > 0) {
    if ((c = Schar_value(s[i])) == '-') {
      if (pos <= 10) break;
      return (h + 523658599) & most_positive_fixnum;
    }
    d = c - '0';
    if (d < 0 || d > 9) break;
    h += d * pos;
    pos *= 10;
  }

  return hash_sc(s, n);
}

static ptr mkstring(const string_char *s, iptr n) {
  iptr i;
  ptr str = S_string(NULL, n);
  for (i = 0; i != n; i += 1) STRIT(str, i) = s[i];
  STRTYPE(str) |= string_immutable_flag;
  return str;
}

ptr S_mkstring(const string_char *s, iptr n) {
  return mkstring(s, n);
}

/* handles single-byte characters, implicit length */
ptr S_intern(const unsigned char *s) {
  ptr tc = get_thread_context();
  intern_oblist *oblist = THREAD_GC(tc)->oblist, *obl;
  iptr n = strlen((const char *)s);
  iptr hc = hash(s, n);
  ptr sym, str;
  bucket *b;

  oblist_mutex_acquire(oblist);

  for (obl = oblist; obl != NULL; obl = obl->next) {
    iptr idx = OBINDEX(hc, obl->length);
    b = obl->oblist[idx];
    while (b != NULL) {
      sym = b->sym;
      if (!GENSYMP(sym)) {
        ptr str = SYMNAME(sym);
        if (Sstring_length(str) == n) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == n) {
              oblist_mutex_release(oblist);
              return sym;
            }
            if (Sstring_ref(str, i) != s[i]) break;
          }
        }
      }
      b = b->next;
    }
  }

  str = S_string((const char *)s, n);
  STRTYPE(str) |= string_immutable_flag;

  sym = S_symbol(str);
  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(tc, oblist, sym, OBINDEX(hc, oblist->length), 0);

  oblist_mutex_release(oblist);
  return sym;
}

/* handles string_chars, explicit length */
ptr S_intern_sc(ptr tc, const string_char *name, iptr n, ptr name_str) {
  intern_oblist *oblist = THREAD_GC(tc)->oblist, *obl;
  iptr hc = hash_sc(name, n);
  ptr sym;
  bucket *b;

  oblist_mutex_acquire(oblist);

  for (obl = oblist; obl != NULL; obl = obl->next) {
    iptr idx = OBINDEX(hc, obl->length);
    b = obl->oblist[idx];
    while (b != NULL) {
      sym = b->sym;
      if (!GENSYMP(sym)) {
        ptr str = SYMNAME(sym);
        if (Sstring_length(str) == n) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == n) {
              oblist_mutex_release(oblist);
              return sym;
            }
            if (STRIT(str, i) != name[i]) break;
          }
        }
      }
      b = b->next;
    }
  }

  if ((name_str == Sfalse) || !(STRTYPE(name_str) & string_immutable_flag))
    name_str = mkstring(name, n);
  sym = S_symbol(name_str);
  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(tc, oblist, sym, OBINDEX(hc, oblist->length), 0);

  oblist_mutex_release(oblist);
  return sym;
}

ptr S_intern3(ptr tc, const string_char *pname, iptr plen, const string_char *uname, iptr ulen, ptr pname_str, ptr uname_str) {
  intern_oblist *oblist = THREAD_GC(tc)->oblist, *obl;
  iptr hc = hash_uname(uname, ulen);
  ptr sym;
  bucket *b;

  oblist_mutex_acquire(oblist);

  for (obl = oblist; obl != NULL; obl = obl->next) {
    iptr idx = OBINDEX(hc, obl->length);
    b = obl->oblist[idx];
    while (b != NULL) {
      sym = b->sym;
      if (GENSYMP(sym)) {
        ptr str = Scar(SYMNAME(sym));
        if (Sstring_length(str) == ulen) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == ulen) {
              oblist_mutex_release(oblist);
              return sym;
            }
            if (STRIT(str, i) != uname[i]) break;
          }
        }
      }
      b = b->next;
    }
  }

  if ((pname_str == Sfalse) || !(STRTYPE(pname_str) & string_immutable_flag))
    pname_str = mkstring(pname, plen);
  if ((uname_str == Sfalse)  || !(STRTYPE(uname_str) & string_immutable_flag))
    uname_str = mkstring(uname, ulen);
  sym = S_symbol(Scons(uname_str, pname_str));
  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(tc, oblist, sym, OBINDEX(hc, oblist->length), 0);

  oblist_mutex_release(oblist);
  return sym;
}

void S_intern_gensym(sym) ptr sym; {
  ptr tc = get_thread_context();
  intern_oblist *oblist = THREAD_GC(tc)->oblist, *obl;
  ptr uname_str = Scar(SYMNAME(sym));
  const string_char *uname = &STRIT(uname_str, 0);
  iptr ulen = Sstring_length(uname_str);
  iptr hc = hash_uname(uname, ulen);
  bucket *b;

  oblist_mutex_acquire(oblist);

  for (obl = oblist; obl != NULL; obl = obl->next) {
    iptr idx = OBINDEX(hc, obl->length);
    b = obl->oblist[idx];
    while (b != NULL) {
      ptr x = b->sym;
      if (GENSYMP(x)) {
        ptr str = Scar(SYMNAME(x));
        if (Sstring_length(str) == ulen) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == ulen) {
              oblist_mutex_release(oblist);
              S_error1("intern-gensym", "unique name ~s already interned", uname_str);
            }
            if (STRIT(str, i) != uname[i]) break;
          }
        }
      }
      b = b->next;
    }
  }

  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(tc, oblist, sym, OBINDEX(hc, oblist->length), GENERATION(sym));

  oblist_mutex_release(oblist);
}

/* must hold oblist mutex */
ptr S_intern4(ptr tc, ptr sym) {
  intern_oblist *oblist = THREAD_GC(tc)->oblist, *obl;
  ptr name = SYMNAME(sym);
  ptr uname_str = (Sstringp(name) ? name : Scar(name));
  const string_char *uname = &STRIT(uname_str, 0);
  iptr ulen = Sstring_length(uname_str);
  iptr hc = UNFIX(SYMHASH(sym));
  bucket *b;

  for (obl = oblist; obl != NULL; obl = obl->next) {
    iptr idx = OBINDEX(hc, obl->length);
    b = obl->oblist[idx];
    while (b != NULL) {
      ptr x = b->sym;
      ptr x_name = SYMNAME(x);
      if (Sstringp(name) == Sstringp(x_name)) {
        ptr str = (Sstringp(x_name) ? x_name : Scar(x_name));
        if (Sstring_length(str) == ulen) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == ulen) {
              return x;
            }
            if (STRIT(str, i) != uname[i]) break;
          }
        }
      }
      b = b->next;
    }
  }

  oblist_insert(tc, oblist, sym, OBINDEX(hc, oblist->length), GENERATION(sym));

  return sym;
}

/* retrofit existing symbols once nonprocedure_code is available */
void S_retrofit_nonprocedure_code() {
  ptr npc, sym, val; bucket_list *bl;

  npc = S_G.nonprocedure_code;

  /* assuming this happens early, before collector has been called, so need look only for generation 0 symbols */
  for (bl = S_G.main_oblist.buckets_of_generation[0]; bl != NULL; bl = bl->cdr) {
    sym = bl->car->sym;
    val = SYMVAL(sym);
    SETSYMCODE(sym, Sprocedurep(val) ? CLOSCODE(val) : npc);
  }
}
