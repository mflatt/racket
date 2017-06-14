#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <stdlib.h>
#ifdef RKTIO_SYSTEM_UNIX
# include <sys/stat.h>
# include <fcntl.h>
# include <unistd.h>
# include <sys/select.h>
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif
#ifdef HAVE_POLL_SYSCALL
# include <poll.h>
#endif

/*========================================================================*/
/* fd struct                                                              */
/*========================================================================*/

struct rktio_fd_t {
  int modes;

#ifdef RKTIO_SYSTEM_UNIX
  intptr_t fd;
# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  int bufcount;
  char buffer[1];
# endif
#endif
  
#ifdef RKTIO_SYSTEM_WINDOWS
  union {
    HANDLE fd;
    int sock; /* when `modes & RKTIO_OPEN_SOCKET` */
  };
  struct Win_FD_Input_Thread *th; /* input mode */
  struct Win_FD_Output_Thread *oth; /* output mode */
  int unblocked; /* whether non-blocking mode is installed */
  char *buffer; /* shared with reading thread */
#endif
};

/*========================================================================*/
/* Windows I/O helper structs                                             */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS

typedef struct Win_FD_Input_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  volatile int avail, err, checking;
  int *refcount;
  HANDLE eof;
  unsigned char *buffer;
  HANDLE checking_sema, ready_sema, you_clean_up_sema;
  HANDLE thread;
} Win_FD_Input_Thread;

typedef struct Win_FD_Output_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  int nonblocking;  /* non-zero => an NT pipe where non-blocking WriteFile
		       works. We still use a thread to detect when the
		       write has ben flushed, which in turn is needed to
		       know whether future writes will immediately succeed. */
  volatile int flushed, needflush; /* Used for non-blocking, only. The flushed
                                      flag communicates from the flush-testing thread
                                      to the main thread. For efficiency, we request
                                      flush checking only when needed (instead of
                                      after every write); needflush indicates that
                                      a flush check is currently needed, but hasn't
                                      been started. */
  volatile int done, err_no;
  volatile unsigned int buflen, bufstart, bufend; /* used for blocking, only */
  unsigned char *buffer; /* used for blocking, only */
  int *refcount;
  HANDLE lock_sema, work_sema, ready_sema, you_clean_up_sema;
  /* lock_sema protects the fields, work_sema starts the flush or
     flush-checking thread to work, ready_sema indicates that a flush
     finished, and you_clean_up_sema is essentially a reference
     count */
  HANDLE thread;
} Win_FD_Output_Thread;

# define RKTIO_FD_BUFFSIZE 4096

static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th);
static void WindowsFDICleanup(Win_FD_Input_Thread *th);

static long WINAPI WindowsFDWriter(Win_FD_Output_Thread *oth);
static void WindowsFDOCleanup(Win_FD_Output_Thread *oth);

typedef BOOL (WINAPI* CSI_proc)(HANDLE);

static CSI_proc get_csi(void)
{
  static int tried_csi = 0;
  static CSI_proc csi;
  
  if (!tried_csi) {
    HMODULE hm;
    hm = LoadLibrary("kernel32.dll");
    if (hm)
      csi = (CSI_proc)GetProcAddress(hm, "CancelSynchronousIo");
    else
      csi = NULL;
    tried_csi = 1;
  }
  return csi;
}

#endif

/*========================================================================*/
/* creating an fd                                                         */
/*========================================================================*/

static void init_read_fd(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  rfd->bufcount = 0;
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!rktio_fd_is_regular_file(rktio, rfd)) {
    /* To get non-blocking I/O for anything that can block, we create
       a separate reader thread.

       Yes, Windows NT pipes support non-blocking reads, but there
       doesn't seem to be any way to use WaitForSingleObject to sleep
       until characters are ready. PeekNamedPipe can be used for
       polling, but not sleeping. */

    Win_FD_Input_Thread *th;
    DWORD id;
    HANDLE h;
    HANDLE sm;
    char *bfr;

    th = malloc(sizeof(Win_FD_Input_Thread));
    rfd->th = th;

    /* Replace buffer with a malloced one: */
    bfr = malloc(RKTIO_FD_BUFFSIZE);
    rfd->buffer = bfr;
    th->buffer = (unsigned char *)bfr;

    th->fd = rfd->fd;
    th->avail = 0;
    th->err = 0;
    th->eof = NULL;
    th->checking = 0;
    
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->checking_sema = sm;
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->ready_sema = sm;
    sm = CreateSemaphore(NULL, 1, 1, NULL);
    th->you_clean_up_sema = sm;

    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDReader, th, 0, &id);

    th->thread = h;
  }
#endif
}

rktio_fd_t *rktio_system_fd(rktio_t *rktio, intptr_t system_fd, int modes)
{
  rktio_fd_t *rfd;

  rfd = malloc(sizeof(rktio_fd_t));
  rfd->modes = modes;  

#ifdef RKTIO_SYSTEM_UNIX
  rfd->fd = system_fd;
  if (!(modes & (RKTIO_OPEN_REGFILE | RKTIO_OPEN_NOT_REGFILE | RKTIO_OPEN_SOCKET))) {
    struct stat buf;
    int cr;
    do {
      cr = fstat(rfd->fd, &buf);
    } while ((cr == -1) && (errno == EINTR));
    if (S_ISREG(buf.st_mode))
      rfd->modes |= RKTIO_OPEN_REGFILE;
  }
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  if (modes & RKTIO_OPEN_SOCKET)
    rfd->sock = system_fd;
  else
    rfd->fd = (HANDLE)system_fd;
  if (!(modes & (RKTIO_OPEN_REGFILE | RKTIO_OPEN_NOT_REGFILE | RKTIO_OPEN_SOCKET))) {
    if ((GetFileType(rfd->fd) == FILE_TYPE_DISK))
      rfd->modes |= RKTIO_OPEN_REGFILE;
  }
#endif
  
  if (modes & RKTIO_OPEN_READ)
    init_read_fd(rktio, rfd);
  
  return rfd;
}

intptr_t rktio_fd_system_fd(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return rfd->fd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rfd->sock;
  else
    return (intptr_t)rfd->fd;
#endif
}

int rktio_fd_is_regular_file(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_REGFILE) ? 1 : 0);
}

int rktio_fd_is_socket(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_SOCKET) ? 1 : 0);
}

int rktio_fd_is_udp(rktio_t *rktio, rktio_fd_t *rfd)
{
  return ((rfd->modes & RKTIO_OPEN_UDP) ? 1 : 0);
}

int rktio_system_fd_is_terminal(rktio_t *rktio, intptr_t fd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return isatty(fd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (GetFileType((HANDLE)fd) == FILE_TYPE_CHAR) {
    DWORD mode;
    if (GetConsoleMode((HANDLE)fd, &mode))
      return 1;
    else
      return 0;
  } else
    return 0;
#endif
}

int rktio_fd_is_terminal(rktio_t *rktio, rktio_fd_t *rfd)
{
  return rktio_system_fd_is_terminal(rktio, (intptr_t)rfd->fd);
}

rktio_fd_t *rktio_dup(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  intptr_t nfd;

  do {
    nfd = dup(rfd->fd);
  } while (nfd == -1 && errno == EINTR);

  if (nfd == -1) {
    get_posix_error();
    return NULL;
  } else
    return rktio_system_fd(rktio, nfd, rfd->modes);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET) {
    return rktio_socket_dup(rktio, rfd);
  } else {
    HANDLE  newhandle;
    BOOL rc;

    rc = DuplicateHandle(GetCurrentProcess(), rfd->fd,
                         GetCurrentProcess(), &newhandle,
                         0, FALSE, DUPLICATE_SAME_ACCESS);

    if (rc == FALSE) {
      get_windows_error();
      return NULL;
    } else {
      return rktio_system_fd(rktio, (intptr_t)newhandle, rfd->modes);
    }
  }
#endif
}

/*========================================================================*/
/* closing                                                                */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_UNIX
void rktio_reliably_close(intptr_t s) {
  int cr;
  do { 
    cr = close(s);
  } while ((cr == -1) && (errno == EINTR));
}
#endif

int rktio_close(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
# ifdef RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
  if (!(rfd->modes & RKTIO_OPEN_SOCKET))
    rktio_release_lockf(rktio, rfd->fd);
# endif

  rktio_reliably_close(rfd->fd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_close(rktio, rfd);
  
  if (rfd->th) {
    CSI_proc csi;

    /* -1 for checking means "shut down" */
    rfd->th->checking = -1;
    ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);

    if (rfd->th->eof && (rfd->th->eof != INVALID_HANDLE_VALUE)) {
      ReleaseSemaphore(rfd->th->eof, 1, NULL);
      rfd->th->eof = NULL;
    }

    csi = get_csi();
    if (csi) {
      /* Helps thread wake up. Otherwise, it's possible for the
         thread to stay stuck trying to read, in which case the
         file handle (probably a pipe) doesn't get closed. */
      csi(rfd->th->thread);
    }

    /* Try to get out of cleaning up the records (since they can't be
       cleaned until the thread is also done: */
    if (WaitForSingleObject(rfd->th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* The other thread exited and left us with clean-up: */
      WindowsFDICleanup(rfd->th);
    } /* otherwise, thread is responsible for clean-up */
  }
  if (rfd->oth) {
    CSI_proc csi;

    csi = get_csi();

    if (csi) {
      /* See also call to csi in fd_close_input */
      csi(rfd->oth->thread);
    }
    CloseHandle(rfd->oth->thread);
    rfd->oth->done = 1;
    ReleaseSemaphore(rfd->oth->work_sema, 1, NULL);

    /* Try to leave clean-up to the other thread: */
    if (WaitForSingleObject(rfd->oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* Other thread is already done, so we're stuck with clean-up: */
      WindowsFDOCleanup(rfd->oth);
    } /* otherwise, thread is responsible for clean-up */
  }
  if (!rfd->th && !rfd->oth) {
    CloseHandle(rfd->fd);
  }
#endif

  free(rfd);
  
  return 1;
}

void rktio_forget(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET) {
    rktio_socket_forget(rktio, rfd);
    return;
  }
#endif
  free(rfd);
}

/*========================================================================*/
/* polling                                                                */
/*========================================================================*/

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
static int try_get_fd_char(int fd, int *ready)
{
  int old_flags, c;
  unsigned char buf[1];

  old_flags = fcntl(fd, F_GETFL, 0);
  if (!(old_flags & RKTIO_NONBLOCKING))
    fcntl(fd, F_SETFL, old_flags | RKTIO_NONBLOCKING);
  do {
    c = read(fd, buf, 1);
  } while ((c == -1) && errno == EINTR);
  if (!(old_flags & RKTIO_NONBLOCKING))
    fcntl(fd, F_SETFL, old_flags);

  if (c < 0) {
    *ready = 0;
    return 0;
  } else {
    *ready = 1;
    if (!c)
      return EOF;
    else
      return buf[0];
  }
}
#endif

int rktio_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  if (rktio_fd_is_regular_file(rktio, rfd))
    return RKTIO_POLL_READY;

#ifdef RKTIO_SYSTEM_UNIX
  {
    int r;

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    if (rfd->bufcount)
      return RKTIO_POLL_READY;
# endif

# ifdef HAVE_POLL_SYSCALL
    struct pollfd pfd[1];
    pfd[0].fd = rfd->fd;
    pfd[0].events = POLLIN;
    do {
      r = poll(pfd, 1, 0);
    } while ((r == -1) && (errno == EINTR));
# else
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_RD_FDSET(readfds);
    INIT_DECL_ER_FDSET(exnfds);

    RKTIO_FD_ZERO(readfds);
    RKTIO_FD_ZERO(exnfds);
    RKTIO_FD_SET(rfd->fd, readfds);
    RKTIO_FD_SET(rfd->fd, exnfds);

    do {
      r = select(rfd->fd + 1, RKTIO_FDS(readfds), NULL, RKTIO_FDS(exnfds), &time);
    } while ((r == -1) && (errno == EINTR));
# endif

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    /* Try a non-blocking read: */
    if (!r && !(rfd->modes & RKTIO_OPEN_SOCKET) && !rfd->textmode) {
      int c, ready;

      c = try_get_fd_char(rfd->fd, &ready);
      if (ready) {
        if (c != EOF) {
          rfd->buffpos = 0;
          rfd->buffer[0] = (unsigned char)c;
          rfd->bufcount = 1;
        }
        r = 1;
      }
    }
# endif

    return r != 0;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_poll_read_ready(rktio, rfd);
  
  if (!rfd->th) {
    /* No thread -- so wait works. This case isn't actually used
       right now, because wait doesn't seem to work reliably for
       anything that we can recognize other than regfiles, which are
       handled above. */
    if (WaitForSingleObject(rfd->fd, 0) == WAIT_OBJECT_0)
      return RKTIO_POLL_READY;
  } else {
    /* Has the reader thread pulled in data? */
    if (rfd->th->checking) {
      /* The thread is still trying, last we knew. Check the
         data-is-ready sema: */
      if (WaitForSingleObject(rfd->th->ready_sema, 0) == WAIT_OBJECT_0) {
        rfd->th->checking = 0;
        return RKTIO_POLL_READY;
      }
    } else if (rfd->th->avail || rfd->th->err || rfd->th->eof)
      return RKTIO_POLL_READY; /* other thread found data */
    else {
      /* Doesn't have anything, and it's not even looking. Tell it
         to look: */
      rfd->th->checking = 1;
      ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);
    }
  }

  return 0;
#endif
}

int poll_write_ready_or_flushed(rktio_t *rktio, rktio_fd_t *rfd, int check_flushed)
{
#ifdef RKTIO_SYSTEM_UNIX
  if (check_flushed)
    return RKTIO_POLL_READY;
  else {
    int sr;
# ifdef HAVE_POLL_SYSCALL
    struct pollfd pfd[1];
    pfd[0].fd = rfd->fd;
    pfd[0].events = POLLOUT;
    do {
      sr = poll(pfd, 1, 0);
    } while ((sr == -1) && (errno == EINTR));
# else
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
  
    INIT_DECL_WR_FDSET(writefds);
    INIT_DECL_ER_FDSET(exnfds);
  
    RKTIO_FD_ZERO(writefds);
    RKTIO_FD_ZERO(exnfds);
    RKTIO_FD_SET(rfd->fd, writefds);
    RKTIO_FD_SET(rfd->fd, exnfds);
  
    do {
      /* Mac OS X 10.8 and 10.9: select() seems to claim that a pipe
         is always ready for output. To work around that problem,
         kqueue() support might be used for pipes, but that has different
         problems. The poll() code above should be used, instead. */
      sr = select(rfd->fd + 1, NULL, RKTIO_FDS(writefds), RKTIO_FDS(exnfds), &time);
    } while ((sr == -1) && (errno == EINTR));
# endif

    if (sr == -1) {
      get_posix_error();
      return RKTIO_POLL_ERROR;
    } else
      return (sr != 0);
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_poll_write_ready(rktio, rfd);
  
  if (rfd->oth) {
    /* Pipe output that can block... */
    int retval;
    Win_FD_Output_Thread *oth = rfd->oth;

    WaitForSingleObject(oth->lock_sema, INFINITE);
    if (oth->nonblocking) {
      if (oth->needflush) {
	oth->needflush = 0;
	oth->flushed = 0;
	ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
	retval = 0;
      } else
	retval = oth->flushed;
    } else
      retval = (oth->err_no || (check_flushed
                                ? !oth->buflen
                                : (oth->buflen < RKTIO_FD_BUFFSIZE)));
    if (!retval && !check_flushed)
      WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
    ReleaseSemaphore(oth->lock_sema, 1, NULL);

    return retval;
  } else
    return RKTIO_POLL_READY; /* non-blocking output, such as a console, or haven't written yet */
#endif
}

int rktio_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  return poll_write_ready_or_flushed(rktio, rfd, 0);
}

int rktio_poll_write_flushed(rktio_t *rktio, rktio_fd_t *rfd)
{
  return poll_write_ready_or_flushed(rktio, rfd, 1);
}

void rktio_poll_add(rktio_t *rktio, rktio_fd_t *rfd, rktio_poll_set_t *fds, int modes)
{
#ifdef RKTIO_SYSTEM_UNIX
  rktio_poll_set_t *fds2;

  if (modes & RKTIO_POLL_READ) {
    RKTIO_FD_SET(rfd->fd, fds);
  }
  if (modes & RKTIO_POLL_WRITE) {
    fds2 = RKTIO_GET_FDSET(fds, 1);
    RKTIO_FD_SET(rfd->fd, fds2);
  }
  fds2 = RKTIO_GET_FDSET(fds, 2);
  RKTIO_FD_SET(rfd->fd, fds2);
#endif  
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET) {
    /* RKTIO_FD_SET(), etc., for Windows expects sockets */
    rktio_poll_set_t *fds2;

    if (modes & RKTIO_POLL_READ) {
      RKTIO_FD_SET(rfd->sock, fds);
    }
    if (modes & RKTIO_POLL_WRITE) {
      fds2 = RKTIO_GET_FDSET(fds, 1);
      RKTIO_FD_SET(rfd->sock, fds2);
    }
    fds2 = RKTIO_GET_FDSET(fds, 2);
    RKTIO_FD_SET(rfd->sock, fds2);
  } else {
    if (modes & RKTIO_POLL_READ) {
      if (rfd->th) {
        /* See fd_byte_ready */
        if (!rfd->th->checking) {
          if (rfd->th->avail || rfd->th->err || rfd->th->eof) {
            /* Data is ready. We shouldn't be trying to sleep, so force an
               immediate wake-up: */
            rktio_poll_set_add_nosleep(rktio, fds);
          } else {
            rfd->th->checking = 1;
            ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);
            rktio_poll_set_add_handle(rktio, (intptr_t)rfd->th->ready_sema, fds, 1);
          }
        } else
          rktio_poll_set_add_handle(rktio, (intptr_t)rfd->th->ready_sema, fds, 1);
      } else if (rktio_fd_is_regular_file(rktio, rfd)) {
        /* regular files never block */
        rktio_poll_set_add_nosleep(rktio, fds);
      } else {
        /* This case is not currently used. See fd_byte_ready. */
        rktio_poll_set_add_handle(rktio, (intptr_t)rfd->fd, fds, 0);
      }
    }

    if (modes & RKTIO_POLL_WRITE) {
      if (rfd->oth && !rktio_poll_write_ready(rktio, rfd))
        rktio_poll_set_add_handle(rktio, (intptr_t)rfd->oth->ready_sema, fds, 1);
      else
        rktio_poll_set_add_nosleep(rktio, fds);
    }
  }
#endif
}

/*========================================================================*/
/* reading                                                                */
/*========================================================================*/

intptr_t rktio_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
#ifdef RKTIO_SYSTEM_UNIX
  intptr_t bc;

  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_read(rktio, rfd, buffer, len);

  if (rktio_fd_is_regular_file(rktio, rfd)) {
    /* Reading regular file never blocks */
    do {
      bc = read(rfd->fd, buffer, len);
    } while ((bc == -1) && (errno == EINTR));

    if (bc == -1) {
      get_posix_error();
      return RKTIO_READ_ERROR;
    } else if (bc == 0)
      return RKTIO_READ_EOF;
    else
      return bc;
  } else {
    /* We use a non-blocking read here, even though we've waited
       for input above, because an external process might have
       gobbled the characters that we expected to get. */
    int old_flags;
    
    old_flags = fcntl(rfd->fd, F_GETFL, 0);
    if (!(old_flags & RKTIO_NONBLOCKING))
      fcntl(rfd->fd, F_SETFL, old_flags | RKTIO_NONBLOCKING);
    
    do {
      bc = read(rfd->fd, buffer, len);
    } while ((bc == -1) && errno == EINTR);

    if ((bc == -1) && (errno != EAGAIN))
      get_posix_error();
    
    if (!(old_flags & RKTIO_NONBLOCKING))
      fcntl(rfd->fd, F_SETFL, old_flags);
    
    if (bc == -1) {
      if (errno == EAGAIN)
        return 0; /* no bytes from a non-blocking read */
      else
        return RKTIO_READ_ERROR;
    } else if (bc == 0)
      return RKTIO_READ_EOF;
    else
      return bc;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_read(rktio, rfd, buffer, len);
  
  if (!rfd->th) {
    /* We can read directly. This must be a regular file, where
       reading never blocks. */
    DWORD rgot; 
    
    if (!ReadFile((HANDLE)rfd->fd, buffer, len, &rgot, NULL)) {
      get_windows_error();
      return RKTIO_READ_ERROR;
    }
    
    if (!rgot)
      return RKTIO_READ_EOF;
    else
      return rgot;
  } else {
    if (!rktio_poll_read_ready(rktio, rfd))
      return 0;
    
    /* If we get this far, there's definitely data available.
       Extract data made available by the reader thread. */
    if (rfd->th->eof) {
      if (rfd->th->eof != INVALID_HANDLE_VALUE) {
        ReleaseSemaphore(rfd->th->eof, 1, NULL);
        rfd->th->eof = NULL;
      }
      return RKTIO_READ_EOF;
    } else if (rfd->th->err) {
      set_windows_error(rfd->th->err);
      return RKTIO_READ_ERROR;
    } else {
      intptr_t bc = rfd->th->avail;
      rfd->th->avail = 0;
      memcpy(buffer, rfd->buffer, bc);
      return bc;
    }
  }
#endif
}

#ifdef RKTIO_SYSTEM_WINDOWS

static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th)
{
  DWORD toget, got;
  int perma_eof = 0;
  HANDLE eof_wait = NULL;

  if (GetFileType((HANDLE)th->fd) == FILE_TYPE_PIPE) {
    /* Reading from a pipe will return early when data is available. */
    toget = RKTIO_FD_BUFFSIZE;
  } else {
    /* Non-pipe: get one char at a time: */
    toget = 1;
  }

  while (!perma_eof && !th->err) {
    /* Wait until we're supposed to look for input: */
    WaitForSingleObject(th->checking_sema, INFINITE);

    if (th->checking < 0)
      break;

    if (ReadFile(th->fd, th->buffer, toget, &got, NULL)) {
      th->avail = got;
      if (!got) {
	/* We interpret a send of 0 bytes as a mid-stream EOF. */
	eof_wait = CreateSemaphore(NULL, 0, 1, NULL);
	th->eof = eof_wait;
      }
    } else {
      int err;
      err = GetLastError();
      if (err == ERROR_BROKEN_PIPE) {
	th->eof = INVALID_HANDLE_VALUE;
	perma_eof = 1;
      } else
	th->err = err;
    }

    /* Notify main program that we found something: */
    ReleaseSemaphore(th->ready_sema, 1, NULL);

    if (eof_wait) {
      WaitForSingleObject(eof_wait, INFINITE);
      eof_wait = NULL;
    }
  }

  /* We have to clean up if the main program has abandoned us: */
  if (WaitForSingleObject(th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDICleanup(th);
  } /* otherwise, main program is responsible for clean-up */

  return 0;
}

static void WindowsFDICleanup(Win_FD_Input_Thread *th)
{
  CloseHandle(th->checking_sema);
  CloseHandle(th->ready_sema);
  CloseHandle(th->you_clean_up_sema);

  CloseHandle(th->fd);

  free(th->buffer);
  free(th);
}

#endif

/*========================================================================*/
/* writing                                                                */
/*========================================================================*/

intptr_t rktio_write(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
#ifdef RKTIO_SYSTEM_UNIX
  int flags, errsaved;
  intptr_t amt;

  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_write(rktio, rfd, buffer, len);
  
  flags = fcntl(rfd->fd, F_GETFL, 0);
  if (!(flags & RKTIO_NONBLOCKING))
    fcntl(rfd->fd, F_SETFL, flags | RKTIO_NONBLOCKING);
  
  amt = len;
  
  do {
    do {
      len = write(rfd->fd, buffer, amt);
    } while ((len == -1) && (errno == EINTR));
    
    /* If there was no room to write `amt` bytes, then it's
       possible that writing fewer bytes will succeed. That seems
       to be the case with FIFOs on Mac OS X, for example. */
    amt = amt >> 1;
  } while ((len == -1) && (errno == EAGAIN) && (amt > 0));

  if (len == -1) {
    errsaved = errno;
    get_posix_error();
  }
  
  if (!(flags & RKTIO_NONBLOCKING))
    fcntl(rfd->fd, F_SETFL, flags);

  if (len == -1) {
    if (errsaved == EAGAIN)
      return 0;
    else
      return RKTIO_WRITE_ERROR;
  } else
    return len;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  DWORD winwrote;

  if (rfd->modes & RKTIO_OPEN_SOCKET)
    return rktio_socket_write(rktio, rfd, buffer, len);

  if (rktio_fd_is_regular_file(rktio, rfd)) {
    /* Regular files never block, so this code looks like the Unix
       code.  We've cheated in the make_fd proc and called
       consoles regular files, because they cannot block, either. */

    /* If we try to write too much at once, the result
       is ERROR_NOT_ENOUGH_MEMORY (as opposed to a partial write). */
    int ok;
    intptr_t towrite = len;
    int err;
    
    while (1) {
      ok = WriteFile((HANDLE)rfd->fd, buffer, towrite, &winwrote, NULL);
      if (!ok)
        err = GetLastError();
	    
      if (!ok && (err == ERROR_NOT_ENOUGH_MEMORY)) {
        towrite = towrite >> 1;
        if (!towrite)
          break;
      } else
        break;
    }

    if (!ok) {
      get_windows_error();
      return RKTIO_WRITE_ERROR;
    }

    return winwrote;
  } else {
    /* If we don't have a thread yet, we'll need to start it. If
       we have a non-blocking pipe, we can try the write (and
       we'll still need the thread to determine when the data is
       flushed). */
    intptr_t out_len = 0;
    int ok, errsaved;
    
    if (!rfd->oth || rfd->oth->nonblocking) {
      int nonblocking;

      /* If we don't have a thread, this is our first write attempt.
         Determine whether this is a non-blocking pipe: */
      if (!rfd->oth) {
        /* The FILE_TYPE_PIPE test is currently redundant, I think,
           but better safe than sorry. */
        nonblocking = (rktio->windows_nt_or_later
                       && (GetFileType((HANDLE)rfd->fd) == FILE_TYPE_PIPE));
      } else
        nonblocking = 1; /* must be, or we would not have gotten here */

      if (nonblocking) {
        /* Unless we're still trying to flush old data, write to the
           pipe and have the other thread start flushing it. */
        DWORD nonblock = PIPE_NOWAIT;
        int flushed;

        if (rfd->oth) {
          if (rfd->oth->needflush) {
            /* Not flushed, but we haven't promised not to block: */
            flushed = 1;
          } else {
            WaitForSingleObject(rfd->oth->lock_sema, INFINITE);
            flushed = rfd->oth->flushed;
            ReleaseSemaphore(rfd->oth->lock_sema, 1, NULL);
          }
        } else
          flushed = 1; /* haven't written anything before */

        if (flushed) {
          /* Put the pipe in non-blocking mode and write. */
          int towrite;

          towrite = len;

          /* Apparently, the semantics of non-blocking pipe writes
             is not partial writes, but giving up entirely when
             the other end isn't being read. In other words, if we
             try to write too much and nothing is being pulled
             from the pipe, winwrote will be set to 0. Also, if
             we try to write too much at once, the result is a
             ERROR_NOT_ENOUGH_MEMORY error. Account for these
             behaviors by trying to write less each iteration when the
             write fails. (Yuck.) */
          while (1) {
            if (!rfd->unblocked) {
              ok = SetNamedPipeHandleState((HANDLE)rfd->fd, &nonblock, NULL, NULL);
              if (ok)
                rfd->unblocked = 1;
              else
                errsaved = GetLastError();
            } else
              ok = 1;
            if (ok) {
              ok = WriteFile((HANDLE)rfd->fd, buffer, towrite, &winwrote, NULL);
              if (!ok)
                errsaved = GetLastError();
            }

            if ((ok && !winwrote)
                || (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY))) {
              towrite = towrite >> 1;
              if (!towrite) {
                get_windows_error();
                return RKTIO_WRITE_ERROR;
              }
            } else
              break;
          }
        } else {
          /* Don't try to write while flushing. */
          ok = 1;
          winwrote = 0;
        }

        if (ok)
          out_len = winwrote;
      }
      /* and create the writer thread... */

      if (!rfd->oth) {
        /* We create a thread even for pipes that can be put in
           non-blocking mode, because that seems to be the only
           way to get evt behavior. */
        Win_FD_Output_Thread *oth;
        HANDLE h;
        DWORD id;
        unsigned char *bfr;
        HANDLE sm;

        oth = malloc(sizeof(Win_FD_Output_Thread));
        rfd->oth = oth;

        oth->nonblocking = nonblocking;

        if (!nonblocking) {
          bfr = malloc(RKTIO_FD_BUFFSIZE);
          oth->buffer = bfr;
          oth->flushed = 0;
          oth->needflush = 0;
        } else {
          oth->buffer = NULL;
          oth->flushed = 1;
          oth->needflush = 1;
        }

        oth->buflen = 0;
        oth->bufstart = 0;
        oth->bufend = 0;

        oth->fd = (HANDLE)rfd->fd;
        oth->err_no = 0;
        oth->done = 0;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->lock_sema = sm;
        sm = CreateSemaphore(NULL, 0, 1, NULL);
        oth->work_sema = sm;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->ready_sema = sm;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->you_clean_up_sema = sm;
            
        h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDWriter, oth, 0, &id);

        oth->thread = h;
      }
    }

    /* We have a thread, if only to watch when the flush is
       done... */
    
    if (!rfd->oth->nonblocking) {
      /* This case is for Win 95/98/Me anonymous pipes and
         character devices.  We haven't written anything yet! We
         write to a buffer read by the other thread, and return --
         the other thread takes care of writing. Thus, as long as
         there's room in the buffer, we don't block, and we can
         tell whether there's room. Technical problem: if multiple
         ports are attched to the same underlying pipe (different
         handle, same "device"), the port writes can get out of
         order. We try to avoid the problem by sleeping. */

      Win_FD_Output_Thread *oth = rfd->oth;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (oth->err_no) {
        errsaved = oth->err_no;
        ok = 0;
      } else if (oth->buflen == RKTIO_FD_BUFFSIZE) {
        WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
        ok = 1;
      } else {
        intptr_t topp;
        int was_pre;

        if (!oth->buflen) {
          /* Avoid fragmenting in circular buffer: */
          oth->bufstart = 0;
          oth->bufend = 0;
        }

        /* Write to top part of circular buffer, then bottom part
           if anything's left. */

        if (oth->bufstart <= oth->bufend) {
          was_pre = 1;
          topp = RKTIO_FD_BUFFSIZE;
        } else {
          was_pre = 0;
          topp = oth->bufstart;
        }

        winwrote = topp - oth->bufend;
        if (winwrote > len)
          winwrote = len;

        memcpy(oth->buffer + oth->bufend, buffer, winwrote);
        oth->buflen += winwrote;
        out_len = winwrote;

        oth->bufend += winwrote;
        if (oth->bufend == RKTIO_FD_BUFFSIZE)
          oth->bufend = 0;

        if (was_pre) {
          if (winwrote < len) {
            /* Try continuing with a wrap-around: */
            winwrote = oth->bufstart - oth->bufend;
            if (winwrote > len - out_len)
              winwrote = len - out_len;

            memcpy(oth->buffer + oth->bufend, buffer + out_len, winwrote);
            oth->buflen += winwrote;
            oth->bufend += winwrote;
            out_len += winwrote;
          }
        }
        /* Let the other thread know that it should start trying
           to write, if it isn't already: */
        ReleaseSemaphore(oth->work_sema, 1, NULL);
        Sleep(0); /* to decrease the chance of re-ordering flushes */

        ok = 1;
      }
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    } else {
      if (out_len > 0) {
        /* We've already written, which implies that no flush is
           in progress. We'll need a flush check in the future. */
        rfd->oth->needflush = 1;
      }
      ok = 1;
    }

    if (ok)
      return out_len;

    set_windows_error(errsaved);
    return RKTIO_WRITE_ERROR;
  }
#endif
}

#ifdef RKTIO_SYSTEM_WINDOWS

static long WINAPI WindowsFDWriter(Win_FD_Output_Thread *oth)
{
  DWORD towrite, wrote, start;
  int ok, more_work = 0, err_no;

  if (oth->nonblocking) {
    /* Non-blocking mode (Win NT pipes). Just flush. */
    while (!oth->done) {
      WaitForSingleObject(oth->work_sema, INFINITE);

      FlushFileBuffers(oth->fd);

      WaitForSingleObject(oth->lock_sema, INFINITE);
      oth->flushed = 1;
      ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
  } else {
    /* Blocking mode. We do the writing work.  This case is for
       Win 95/98/Me anonymous pipes and character devices (such 
       as LPT1). */
    while (!oth->err_no) {
      if (!more_work)
	WaitForSingleObject(oth->work_sema, INFINITE);

      if (oth->done)
	break;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      towrite = oth->buflen;
      if (towrite > (RKTIO_FD_BUFFSIZE - oth->bufstart))
	towrite = RKTIO_FD_BUFFSIZE - oth->bufstart;
      start = oth->bufstart;
      ReleaseSemaphore(oth->lock_sema, 1, NULL);

      ok = WriteFile(oth->fd, oth->buffer + start, towrite, &wrote, NULL);
      if (!ok)
	err_no = GetLastError();
      else
	err_no = 0;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (!ok)
	oth->err_no = err_no;
      else {
	oth->bufstart += wrote;
	oth->buflen -= wrote;
	if (oth->bufstart == RKTIO_FD_BUFFSIZE)
	  oth->bufstart = 0;
	more_work = oth->buflen > 0;
      }
      if ((oth->buflen < RKTIO_FD_BUFFSIZE) || oth->err_no)
	ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
  }
  if (WaitForSingleObject(oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDOCleanup(oth);
  } /* otherwise, main thread is responsible for clean-up */

  return 0;
}

static void WindowsFDOCleanup(Win_FD_Output_Thread *oth)
{
  CloseHandle(oth->lock_sema);
  CloseHandle(oth->work_sema);
  CloseHandle(oth->you_clean_up_sema);
  
  CloseHandle(oth->fd);

  if (oth->buffer)
    free(oth->buffer);
  free(oth);
}

#endif