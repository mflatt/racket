#if ((defined(_MSC_VER) || defined(__MINGW32__))                        \
     && (defined(__WIN32__) || defined(WIN32) || defined(_WIN32)))
# define RKTIO_SYSTEM_WINDOWS
#else
# define RKTIO_SYSTEM_UNIX
#endif


#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif

#if RKTIO_SYSTEM_WINDOWS
# define USE_FAR_RKTIO_FDCALLS
#endif
#ifdef USE_DYNAMIC_FDSET_SIZE
# define USE_FAR_RKTIO_FDCALLS
#endif
#ifdef HAVE_POLL_SYSCALL
# define USE_FAR_RKTIO_FDCALLS
#endif

/************************************************************/
/* Globals, as gathered into `rktio_t`                      */
/************************************************************/

struct rktio_t {
  intptr_t errid;
  int errkind;
#ifdef RKTIO_SYSTEM_UNIX
  struct group_member_cache_entry_t *group_member_cache;
  int external_event_fd;
  int put_external_event_fd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  int windows_nt_or_later;
  HANDLE break_semaphore;
#endif
#ifdef USE_FAR_RKTIO_FDCALLS
  /* A single fdset that can be reused for immediate actions: */
  struct rktio_poll_set_t *rktio_global_fd_set;
#endif
};

/************************************************************/
/* Poll sets                                                */
/************************************************************/

typedef struct rktio_poll_set_t rktio_poll_set_t;

void rktio_alloc_global_poll_set(rktio_t *rktio);
int rktio_initialize_signal(rktio_t *rktio);

#ifdef USE_FAR_RKTIO_FDCALLS
# define DECL_FDSET(n, c) fd_set *n
# define INIT_DECL_FDSET(r, w, e) { \
   r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 0 ); \
   w = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 1 ); \
   e = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 2 ); \
 }
# define INIT_DECL_RD_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 0 )
# define INIT_DECL_WR_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 1 )
# define INIT_DECL_ER_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 2 )

# define RKTIO_GET_FDSET(p, n) rktio_get_fdset(p, n)
# define RKTIO_FD_ZERO(p) rktio_fdzero(p)
# define RKTIO_FD_SET(n, p) rktio_fdset(p, n)
# define RKTIO_FD_CLR(n, p) rktio_fdclr(p, n)
# define RKTIO_FD_ISSET(n, p) rktio_fdisset(p, n)

# if !defined(HAVE_POLL_SYSCALL) && !defined(RKTIO_SYSTEM_WINDOWS)
#  define RKTIO_FDS(p) ((fd_set *)fds)
# endif

#else

#include <sys/select.h>
struct rktio_poll_set_t { fd_set data; };

# define DECL_FDSET(n, c) rktio_poll_set_t n[c]
# define INIT_DECL_FDSET(r, w, e) /* empty */
# define INIT_DECL_RD_FDSET(r) /* empty */
# define INIT_DECL_WR_FDSET(r) /* empty */
# define INIT_DECL_ER_FDSET(r) /* empty */

# define RKTIO_FDS(p) (&(p)->data)

# define RKTIO_GET_FDSET(p, n) ((p)+(n))
# define RKTIO_FD_ZERO(p) FD_ZERO(RKTIO_FDS(p))
# define RKTIO_FD_SET(n, p) FD_SET(n, RKTIO_FDS(p))
# define RKTIO_FD_CLR(n, p) FD_CLR(n, RKTIO_FDS(p))
# define RKTIO_FD_ISSET(n, p) FD_ISSET(n, RKTIO_FDS(p))

#endif

rktio_poll_set_t *rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds);
void rktio_clean_fd_set(rktio_poll_set_t *fds);
int rktio_get_fd_limit(rktio_poll_set_t *fds);

/************************************************************/
/* Misc                                                     */
/************************************************************/

#ifdef RKTIO_SYSTEM_WINDOWS
# define MSC_IZE(n) _ ## n
# define MSC_W_IZE(n) _w ## n
# define MSC_WIDE_PATH_temp(n) WIDE_PATH_temp(n)
#else
# define MSC_IZE(n) n
# define MSC_W_IZE(n) MSC_IZE(n)
# define MSC_WIDE_PATH_temp(n) n
#endif

void rktio_get_posix_error(rktio_t *rktio);
#define get_posix_error() rktio_get_posix_error(rktio)

void rktio_set_racket_error(rktio_t *rktio, int errid);
#define set_racket_error(e) rktio_set_racket_error(rktio, e)

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_get_windows_error(rktio_t *rktio);
# define get_windows_error() rktio_get_windows_error(rktio)
#endif

#if defined(USE_FCNTL_O_NONBLOCK)
# define RKTIO_NONBLOCKING O_NONBLOCK
#else
# define RKTIO_NONBLOCKING FNDELAY
#endif
