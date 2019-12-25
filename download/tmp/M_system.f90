!>
!!##NAME
!!    M_system(3fm) - [M_system::INTRO] Fortran interface to C system interface
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Public objects:
!!
!!    ! ENVIRONMENT
!!    use m_system, only : set_environment_variable, system_unsetenv, &
!!    system_putenv, system_getenv
!!
!!    use m_system, only :  system_intenv, system_readenv, system_clearenv
!!    ! FILE SYSTEM
!!    use M_system, only : system_getcwd, system_link,       &
!!    system_mkfifo, system_remove, system_rename,           &
!!    system_umask, system_unlink, fileglob,                 &
!!    system_rmdir, system_chdir, system_mkdir,              &
!!    system_stat, system_isdir, system_islnk, system_isreg, &
!!    system_isblk, system_ischr, system_isfifo,             &
!!    system_realpath,                                       &
!!    system_access,                                         &
!!    system_utime,                                          &
!!    system_issock, system_perm, system_stat_print,         &
!!    system_memcpy
!!
!!    !!use M_system, only : system_getc, system_putc
!!    ! ERROR PROCESSING
!!    use M_system, only : system_errno, system_perror
!!    ! INFO
!!    use M_system, only : system_getegid, system_geteuid, system_getgid, &
!!    system_gethostname, system_getpid, system_getppid, system_setsid, &
!!    system_getsid, system_getuid, system_uname
!!    ! SIGNALS
!!    use M_system, only : system_kill
!!    ! RANDOM NUMBERS
!!    use M_system, only : system_rand, system_srand
!!    ! PROCESS INFORMATION
!!    use M_system, only : system_cpu_time
!!
!!##DESCRIPTION
!!    M_system(3fm) is a collection of Fortran procedures that call C
!!    or a C wrapper using the ISO_C_BINDING interface to access system calls.
!!    System calls are a special set of functions used by programs to communicate
!!    directly with an operating system.
!!
!!    Generally, system calls are slower than normal function calls because
!!    when you make a call control is relinquished to the operating system
!!    to perform the system call. In addition, depending on the nature of the
!!    system call, your program may be blocked by the OS until the system call
!!    has finished, thus making the execution time of your program even longer.
!!
!!    One rule-of-thumb that should always be followed when calling a system
!!    call -- Always check the return value.
!!##ENVIRONMENT ACCESS
!!        o  system_putenv(3f):     call putenv(3c)
!!        o  system_getenv(3f):     function call to get_environment_variable(3f)
!!        o  system_unsetenv(3f):   call unsetenv(3c) to remove variable from environment
!!        o  set_environment_variable(3f): set environment variable by calling setenv(3c)
!!
!!        o  system_initenv(3f):    initialize environment table for reading
!!        o  system_readenv(3f):    read next entry from environment table
!!        o  system_clearenv(3f):   emulate clearenv(3c) to clear environment
!!##FILE SYSTEM
!!        o  system_chdir(3f):      call chdir(3c) to change current directory of a process
!!        o  system_getcwd(3f):     call getcwd(3c) to get pathname of current working directory
!!
!!        o  system_stat(3f):       determine system information of file by name
!!        o  system_stat_print(3f): determine system information of file by name
!!        o  system_perm(3f):       create string representing file permission and type
!!        o  system_access(3f):     determine filename access or existence
!!        o  system_isdir(3f):      determine if filename is a directory
!!        o  system_islnk(3f):      determine if filename is a link
!!        o  system_isreg(3f):      determine if filename is a regular file
!!        o  system_isblk(3f):      determine if filename is a block device
!!        o  system_ischr(3f):      determine if filename is a character device
!!        o  system_isfifo(3f):     determine if filename is a fifo - named pipe
!!        o  system_issock(3f):     determine if filename is a socket
!!        o  system_realpath(3f):   resolve a pathname
!!
!!        o  system_chmod(3f):      call chmod(3c) to set file permission mode
!!        o  system_chown(3f):      call chown(3c) to set file owner
!!        o  system_getumask(3f):   call umask(3c) to get process permission mask
!!        o  system_setumask(3f):   call umask(3c) to set process permission mask
!!
!!        o  system_mkdir(3f):      call mkdir(3c) to create empty directory
!!        o  system_mkfifo(3f):     call mkfifo(3c) to create a special FIFO file
!!        o  system_link(3f):       call link(3c) to create a filename link
!!
!!        o  system_rename(3f):     call rename(3c) to change filename
!!
!!        o  system_remove(3f):     call remove(3c) to remove file
!!        o  system_rmdir(3f):      call rmdir(3c) to remove empty directory
!!        o  system_unlink(3f):     call unlink(3c) to remove a link to a file
!!        o  system_utime(3f):      call utime(3c) to set file access and modification times
!!
!!        o  fileglob(3f): Returns list of files using a file globbing pattern
!!
!!##STREAM IO
!!        o  system_getc(3f): get a character from stdin
!!        o  system_putc(3f): put a character on stdout
!!##RANDOM NUMBERS
!!        o  system_srand(3f): call srand(3c)
!!        o  system_rand(3f): call rand(3c)
!!##C ERROR INFORMATION
!!        o  system_errno(3f): return errno(3c)
!!        o  system_perror(3f): call perror(3c) to display last C error message
!!##QUERIES
!!        o  system_geteuid(3f): call geteuid(3c)
!!        o  system_getuid(3f): call getuid(3c)
!!        o  system_getegid(3f): call getegid(3c)
!!        o  system_getgid(3f): call getgid(3c)
!!        o  system_getpid(3f): call getpid(3c)
!!        o  system_getppid(3f): call getppid(3c)
!!        o  system_gethostname(3f): get name of current host
!!        o  system_uname(3f): call my_uname(3c) which calls uname(3c)
!!        o  system_getlogin(3f): get login name
!!        o  system_getpwuid(3f): get login name associated with given UID
!!        o  system_getgrgid(3f): get group name associated with given GID
!!        o  system_cpu_time(3f) : get processor time in seconds using times(3c)
!!
!!##FUTURE DIRECTIONS
!!    A good idea of what system routines are commonly required is to refer
!!    to the POSIX binding standards. (Note: IEEE 1003.9-1992 was withdrawn 6
!!    February 2003.) The IEEE standard covering Fortran 77 POSIX bindings
!!    is available online, though currently (unfortunately) only from
!!    locations with appropriate subscriptions to the IEEE server (e.g.,
!!    many university networks). For those who do have such access, the link
!!    is: POSIX Fortran 77 Language Interfaces (IEEE Std 1003.9-1992) (pdf)
!!
!!##SEE ALSO
!!    Some vendors provide their own way to access POSIX functions and make
!!    those available as modules; for instance ...
!!
!!       o the IFPORT module of Intel
!!       o or the f90_* modules of NAG.
!!       o There are also other compiler-independent efforts to make the
!!         POSIX procedures accessible from Fortran...
!!
!!          o Posix90 (doc),
!!          o flib.a platform/files and directories,
!!          o fortranposix.
!===================================================================================================================================
module M_system
use,intrinsic     :: iso_c_binding,   only : c_float, c_int, c_char
use,intrinsic     :: iso_c_binding,   only : c_ptr, c_f_pointer, c_null_char, c_null_ptr
use,intrinsic     :: iso_c_binding
use,intrinsic     :: iso_fortran_env, only : int8, int16, int32, int64 !!, real32, real64, real128, dp=>real128

implicit none
private
! C types. Might be platform dependent
integer,parameter,public :: mode_t=int32

public :: system_rand
public :: system_srand

!!public :: system_getc
!!public :: system_putc

public :: system_getpid                  ! return process ID
public :: system_getppid                 ! return parent process ID
public :: system_getuid, system_geteuid  ! return user ID
public :: system_getgid, system_getegid  ! return group ID
public :: system_setsid
public :: system_getsid
public :: system_kill                    ! (pid, signal) kill process (defaults: pid=0, signal=SIGTERM)

public :: system_errno
public :: system_perror

public :: system_putenv
public :: system_getenv
public :: set_environment_variable
public :: system_unsetenv

public :: system_initenv
public :: system_readenv
public :: system_clearenv

public :: system_stat                    ! call stat(3c) to determine system information of file by name
public :: system_stat_print              ! call stat(3f) and print principal pathname information
public :: system_perm                    ! create string representing file permission and type
public :: system_access                  ! determine filename access or existence
public :: system_isdir                   ! determine if filename is a directory
public :: system_islnk                   ! determine if filename is a link
public :: system_isreg                   ! determine if filename is a regular file
public :: system_isblk                   ! determine if filename is a block device
public :: system_ischr                   ! determine if filename is a character device
public :: system_isfifo                  ! determine if filename is a fifo - named pipe
public :: system_issock                  ! determine if filename is a socket
public :: system_realpath                ! resolve pathname

public :: system_chdir
public :: system_rmdir
public :: system_remove
public :: system_rename

public :: system_mkdir
public :: system_mkfifo
public :: system_chmod
public :: system_chown
public :: system_link
public :: system_unlink
public :: system_utime

public :: system_setumask
public :: system_getumask
private :: system_umask

public :: system_getcwd

public :: system_opendir
public :: system_readdir
public :: system_rewinddir
public :: system_closedir

public :: system_cpu_time

public :: system_uname
public :: system_gethostname
public :: system_getlogin
public :: system_getpwuid
public :: system_getgrgid
public :: fileglob

public :: system_alarm
public :: system_calloc
public :: system_clock
public :: system_time
!public :: system_time
!public :: system_qsort

public :: system_realloc
public :: system_malloc
public :: system_free
public :: system_memcpy

public :: R_GRP,R_OTH,R_USR,RWX_G,RWX_O,RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR,DEFFILEMODE,ACCESSPERMS
public :: R_OK,W_OK,X_OK,F_OK  ! for system_access

public test_suite_M_system
!===================================================================================================================================
type, bind(C) :: dirent_SYSTEMA
  integer(c_long)    :: d_ino
  integer(c_long)    :: d_off; ! __off_t, check size
  integer(c_short)   :: d_reclen
  character(len=1,kind=c_char) :: d_name(256)
end type

type, bind(C) :: dirent_CYGWIN
  integer(c_int)       :: d_version
  integer(c_long)      :: d_ino
  character(kind=c_char)    :: d_type
  character(kind=c_char)    :: d_unused1(3)
  integer(c_int)       :: d_internal1
  character(len=1,kind=c_char) ::  d_name(256)
end type

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    function system_alarm(seconds) bind(c, name="alarm")
      import c_int
      integer(kind=c_int), value :: seconds
      integer(kind=c_int) system_alarm
    end function system_alarm
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    function system_calloc(nelem, elsize) bind(c, name="calloc")
      import C_SIZE_T, C_INTPTR_T
      integer(C_SIZE_T), value :: nelem, elsize
      integer(C_INTPTR_T) system_calloc
    end function system_calloc
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    pure function system_clock() bind(c, name="clock")
      import C_LONG
      integer(C_LONG) system_clock
    end function system_clock
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Copy N bytes of SRC to DEST, no aliasing or overlapping allowed.
! extern void *memcpy (void *dest, const void *src, size_t n);
interface
  subroutine  system_memcpy(dest, src, n) bind(C,name='memcpy')
     import C_INTPTR_T, C_SIZE_T
     INTEGER(C_INTPTR_T), value  :: dest
     INTEGER(C_INTPTR_T), value  :: src
     integer(C_SIZE_T), value    :: n
  end subroutine system_memcpy
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    subroutine system_free(ptr) bind(c, name="free")
      import C_INTPTR_T
      integer(C_INTPTR_T), value :: ptr
    end subroutine system_free
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    function system_malloc(size) bind(c, name="malloc")
      import C_SIZE_T, C_INTPTR_T
      integer(C_SIZE_T), value :: size
      integer(C_INTPTR_T) system_malloc
    end function system_malloc
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    function system_realloc(ptr, size) bind(c, name="realloc")
      import C_SIZE_T, C_INTPTR_T
      integer(C_INTPTR_T), value :: ptr
      integer(C_SIZE_T), value :: size
      integer(C_INTPTR_T) system_realloc
    end function system_realloc
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  interface
    function system_time(tloc) bind(c, name="time")
      ! tloc argument should be loaded via C_LOC from iso_c_binding
      import C_PTR, C_LONG
      type(C_PTR), value :: tloc
      integer(C_LONG) system_time
    end function system_time
  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!  abstract interface
!    integer(4) function compar_iface(a, b)
!      import c_int
!      integer, intent(in) :: a, b
!! Until implement TYPE(*)
!      integer(kind=c_int) :: compar_iface
!    end function compar_iface
!  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!  interface
!    subroutine system_qsort(base, nel, width, compar) bind(c, name="qsort")
!      import C_SIZE_T, compar_iface
!      integer :: base
!! Until implement TYPE(*)
!      integer(C_SIZE_T), value :: nel, width
!      procedure(compar_iface) compar
!    end subroutine system_qsort
!  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_srand(3f) - [M_system] set seed for pseudo-random number generator system_rand(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine system_srand()
!!
!!##DESCRIPTION
!!    system_srand(3f) calls the C routine srand(3c) The
!!    srand(3c)/system_srand(3f) function uses its argument as the seed
!!    for a new sequence of pseudo-random integers to be returned by
!!    system_rand(3f)/rand(3c). These sequences are repeatable by calling
!!    system_srand(3f) with the same seed value. If no seed value is
!!    provided, the system_rand(3f) function is automatically seeded with
!!    a value of 1.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_system_srand
!!       use M_system, only : system_srand, system_rand
!!       implicit none
!!       integer :: i,j
!!       do j=1,2
!!          call system_srand(1001)
!!          do i=1,10
!!             write(*,*)system_rand()
!!          enddo
!!          write(*,*)
!!       enddo
!!       end program demo_system_srand
!!   expected results:
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!!
!!##SEE ALSO
!!    drand48(3c), random(3c)
!===================================================================================================================================
character(len=*),parameter :: ident_srand="@(#) M_system::system_srand(3f): call srand(3c)"
! void srand_system(int *seed)
interface
   subroutine system_srand(seed) bind(c,name='srand')
      import c_int
      integer(kind=c_int),intent(in) :: seed
   end subroutine system_srand
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_kill(3f) - [M_system] send a signal to a process or a group of processes
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_kill(pid,sig)
!!
!!       integer,intent(in) :: pid
!!       integer,intent(in) :: sig
!!
!!##DESCRIPTION
!!
!!    The kill() function shall send a signal to a process or a group of
!!    processes specified by pid. The signal to be sent is specified by sig
!!    and is either one from the list given in <signal.h> or 0. If sig is 0
!!    (the null signal), error checking is performed but no signal is actually
!!    sent. The null signal can be used to check the validity of pid.
!!
!!    For a process to have permission to send a signal to a process designated
!!    by pid, unless the sending process has appropriate privileges, the real
!!    or effective user ID of the sending process shall match the real or
!!    saved set-user-ID of the receiving process.
!!
!!    If pid is greater than 0, sig shall be sent to the process whose process
!!    ID is equal to pid.
!!
!!    If pid is 0, sig shall be sent to all processes (excluding an unspecified
!!    set of system processes) whose process group ID is equal to the process
!!    group ID of the sender, and for which the process has permission to send
!!    a signal.
!!
!!    If pid is -1, sig shall be sent to all processes (excluding an unspecified
!!    set of system processes) for which the process has permission to send
!!    that signal.
!!
!!    If pid is negative, but not -1, sig shall be sent to all processes
!!    (excluding an unspecified set of system processes) whose process group
!!    ID is equal to the absolute value of pid, and for which the process has
!!    permission to send a signal.
!!
!!    If the value of pid causes sig to be generated for the sending process,
!!    and if sig is not blocked for the calling thread and if no other thread
!!    has sig unblocked or is waiting in a sigwait() function for sig, either
!!    sig or at least one pending unblocked signal shall be delivered to the
!!    sending thread before kill() returns.
!!
!!    The user ID tests described above shall not be applied when sending
!!    SIGCONT to a process that is a member of the same session as the sending
!!    process.
!!
!!    An implementation that provides extended security controls may impose
!!    further implementation-defined restrictions on the sending of signals,
!!    including the null signal. In particular, the system may deny the
!!    existence of some or all of the processes specified by pid.
!!
!!    The kill() function is successful if the process has permission to send
!!    sig to any of the processes specified by pid. If kill() fails, no signal
!!    shall be sent.
!!
!!
!!##RETURN VALUE
!!
!!    Upon successful completion, 0 shall be returned. Otherwise, -1 shall be
!!    returned and errno set to indicate the error.
!!
!!##ERRORS
!!    The kill() function shall fail if:
!!
!!    EINVAL  The value of the sig argument is an invalid or unsupported
!!            signal number.
!!    EPERM   The process does not have permission to send the signal to
!!            any receiving process.
!!    ESRCH   No process or process group can be found corresponding to
!!            that specified by pid. The following sections are informative.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_kill
!!    use M_system, only : system_kill
!!    use M_system, only : system_perror
!!    implicit none
!!    integer           :: i,pid,ios,ierr,signal=9
!!    character(len=80) :: argument
!!
!!       do i=1,command_argument_count()
!!          ! get arguments from command line
!!          call get_command_argument(i, argument)
!!          ! convert arguments to integers assuming they are PID numbers
!!          read(argument,'(i80)',iostat=ios) pid
!!          if(ios.ne.0)then
!!             write(*,*)'bad PID=',trim(argument)
!!          else
!!             write(*,*)'kill SIGNAL=',signal,' PID=',pid
!!          ! send signal SIGNAL to pid PID
!!             ierr=system_kill(pid,signal)
!!          ! write message if an error was detected
!!             if(ierr.ne.0)then
!!                call system_perror('*demo_system_kill*')
!!             endif
!!          endif
!!       enddo
!!    end program demo_system_kill
!!
!!##SEE ALSO
!!    getpid(), raise(), setsid(), sigaction(), sigqueue(),
!===================================================================================================================================
character(len=*),parameter :: ident_kill="@(#) M_system::system_kill(3f): call kill(3c) to send a signal to a process"
! int kill(pid_t pid, int sig);
interface
   function system_kill(c_pid,c_signal) bind(c,name="kill") result(c_ierr)
      import c_int
      integer(kind=c_int),value,intent(in)   :: c_pid
      integer(kind=c_int),value,intent(in)   :: c_signal
      integer(kind=c_int)                    :: c_ierr
   end function
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_errno(3f) - [M_system] C error return value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_errno()
!!
!!##DESCRIPTION
!!    Many C routines return an error code which can be queried by errno.
!!    The M_system(3fm) is primarily composed of Fortran routines that call
!!    C routines. In the cases where an error code is returned vi system_errno(3f)
!!    these routines will indicate it.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_errno
!!    use M_system, only : system_errno, system_unlink, system_perror
!!    implicit none
!!    integer :: stat
!!    stat=system_unlink('not there/OR/anywhere')
!!    if(stat.ne.0)then
!!            write(*,*)'err=',system_errno()
!!            call system_perror('*demo_system_errno*')
!!    endif
!!    end program demo_system_errno
!!
!!   Typical Results:
!!
!!    err=           2
!!    *demo_system_errno*: No such file or directory
!===================================================================================================================================
character(len=*),parameter :: ident_errno="@(#) M_system::system_errno(3f): return errno(3c)"

interface
   integer(kind=c_int) function system_errno() bind (C,name="my_errno")
      import c_int
   end function system_errno
end interface
!!  if a macro on XLF
!!  interface system_errno
!!    function ierrno_() bind(c, name="ierrno_")
!!      import c_int
!!      integer(kind=c_int) :: ierrno_
!!    end function system_errno
!!  end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_geteuid(3f) - [M_system:QUERY] get effective UID of current process from Fortran by calling geteuid(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_geteuid()
!!
!!##DESCRIPTION
!!        The system_geteuid(3f) function shall return the effective user
!!        ID of the calling process. The geteuid() function shall always be
!!        successful and no return value is reserved to indicate the error.
!!##EXAMPLE
!!
!!   Get group ID from Fortran:
!!
!!    program demo_system_geteuid
!!    use M_system, only : system_geteuid
!!    implicit none
!!       write(*,*)'EFFECTIVE UID=',system_geteuid()
!!    end program demo_system_geteuid
!===================================================================================================================================
character(len=*),parameter :: ident_euid="@(#) M_system::system_geteuid(3f): call geteuid(3c)"
interface
   integer(kind=c_int) function system_geteuid() bind (C,name="geteuid")
      import c_int
   end function system_geteuid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getuid(3f) - [M_system:QUERY] get real UID of current process from Fortran by calling getuid(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getuid()
!!
!!##DESCRIPTION
!!        The system_getuid(3f) function shall return the real user ID
!!        of the calling process. The getuid() function shall always be
!!        successful and no return value is reserved to indicate the error.
!!##EXAMPLE
!!
!!   Get group ID from Fortran:
!!
!!    program demo_system_getuid
!!    use M_system, only : system_getuid
!!    implicit none
!!       write(*,*)'UID=',system_getuid()
!!    end program demo_system_getuid
!!
!!   Results:
!!
!!    UID=      197609
!===================================================================================================================================
character(len=*),parameter :: ident_uid="@(#) M_system::system_getuid(3f): call getuid(3c)"
interface
   integer(kind=c_int) function system_getuid() bind (C,name="getuid")
      import c_int
   end function system_getuid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getegid(3f) - [M_system:QUERY] get the effective group ID (GID) of current process from Fortran by calling getegid(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getegid()
!!##DESCRIPTION
!!        The getegid() function returns the effective group ID of the
!!        calling process.
!!
!!##RETURN VALUE
!!        The getegid() should always be successful and no return value is
!!        reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!        getegid(), system_geteuid(), getuid(), setegid(), seteuid(), setgid(),
!!        setregid(), setreuid(), setuid()
!!
!!##EXAMPLE
!!
!!   Get group ID from Fortran
!!
!!    program demo_system_getegid
!!    use M_system, only : system_getegid
!!    implicit none
!!       write(*,*)'GID=',system_getegid()
!!    end program demo_system_getegid
!===================================================================================================================================
character(len=*),parameter :: ident_egid="@(#) M_system::system_getegid(3f): call getegid(3c)"
interface
   integer(kind=c_int) function system_getegid() bind (C,name="getegid")
      import c_int
   end function system_getegid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getgid(3f) - [M_system:QUERY] get the real group ID (GID) of current process from Fortran by calling getgid(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getgid()
!!##DESCRIPTION
!!        The getgid() function returns the real group ID of the calling process.
!!
!!##RETURN VALUE
!!        The getgid() should always be successful and no return value is
!!        reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!        getegid(), system_geteuid(), getuid(), setegid(), seteuid(), setgid(),
!!        setregid(), setreuid(), setuid()
!!
!!##EXAMPLE
!!
!!   Get group ID from Fortran
!!
!!    program demo_system_getgid
!!    use M_system, only : system_getgid
!!    implicit none
!!       write(*,*)'GID=',system_getgid()
!!    end program demo_system_getgid
!===================================================================================================================================
character(len=*),parameter :: ident_gid="@(#) M_system::system_getgid(3f): call getgid(3c)"
interface
   integer(kind=c_int) function system_getgid() bind (C,name="getgid")
      import c_int
   end function system_getgid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_setsid(3f) - [M_system:QUERY] create session and set the process group ID of a session leader
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        integer(kind=c_int) function system_setsid(pid)
!!        integer(kind=c_int) :: pid
!!##DESCRIPTION
!!        The  setsid()  function creates a new session, if the calling process is not a process group leader. Upon return the
!!        calling process shall be the session leader of this new session, shall be the process  group  leader  of  a  new  process
!!        group,  and  shall  have  no  controlling terminal. The process group ID of the calling process shall be set equal to the
!!        process ID of the calling process. The calling process shall be the only process in the new process group  and  the  only
!!        process in the new session.
!!
!!##RETURN VALUE
!!        Upon  successful  completion,  setsid() shall return the value of the new process group ID of the calling process. Other‐
!!        wise, it shall return −1 and set errno to indicate the error.
!!##ERRORS
!!        The setsid() function shall fail if:
!!
!!         o The calling process is already a process group leader
!!         o the process group ID of a process other than the calling process matches the process ID of the calling process.
!!##EXAMPLE
!!
!!   Set SID from Fortran
!!
!!    program demo_system_setsid
!!    use M_system,      only : system_setsid
!!    implicit none
!!       write(*,*)'SID=',system_setsid()
!!    end program demo_system_setsid
!===================================================================================================================================
character(len=*),parameter :: ident_setsid="@(#) M_system::system_setsid(3f): call setsid(3c) to set session leader for given pid"
interface
   integer(kind=c_int) function system_setsid() bind (C,name="setsid")
      import c_int
   end function system_setsid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_getsid(3f) - [M_system:QUERY] get the process group ID of a session leader
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        integer(kind=c_int) function system_getsid(pid)
!!        integer(kind=c_int) :: pid
!!##DESCRIPTION
!!        The system_getsid() function obtains the process group ID of the
!!        process that is the session leader of the process specified by pid.
!!        If pid is 0, it specifies the calling process.
!!##RETURN VALUE
!!        Upon successful completion, system_getsid() shall return the process group
!!        ID of the session leader of the specified process. Otherwise,
!!        it shall return -1 and set errno to indicate the error.
!!##EXAMPLE
!!
!!   Get SID from Fortran
!!
!!    program demo_system_getsid
!!    use M_system,      only : system_getsid
!!    use ISO_C_BINDING, only : c_int
!!    implicit none
!!       write(*,*)'SID=',system_getsid(0_c_int)
!!    end program demo_system_getsid
!===================================================================================================================================
character(len=*),parameter :: ident_getsid="@(#) M_system::system_getsid(3f): call getsid(3c) to get session leader for given pid"
interface
   integer(kind=c_int) function system_getsid(c_pid) bind (C,name="getsid")
      import c_int
      integer(kind=c_int) :: c_pid
   end function system_getsid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getpid(3f) - [M_system:QUERY] get PID (process ID) of current process from Fortran by calling getpid(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer function system_getpid()
!!##DESCRIPTION
!!        The system_getpid() function returns the process ID of the
!!        calling process.
!!##RETURN VALUE
!!        The value returned is the integer process ID. The system_getpid()
!!        function shall always be successful and no return value is reserved
!!        to indicate an error.
!!##EXAMPLE
!!
!!   Get process PID from Fortran
!!
!!    program demo_system_getpid
!!    use M_system, only : system_getpid
!!    implicit none
!!       write(*,*)'PID=',system_getpid()
!!    end program demo_system_getpid
!===================================================================================================================================
character(len=*),parameter :: ident_pid="@(#) M_system::system_getpid(3f): call getpid(3c)"

interface
   pure integer(kind=c_int) function system_getpid() bind (C,name="getpid")
      import c_int
   end function system_getpid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getppid(3f) - [M_system:QUERY] get parent process ID (PPID) of current process from Fortran by calling getppid(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getppid()
!!##DESCRIPTION
!!        The system_getppid() function returns the parent process ID of
!!        the calling process.
!!
!!##RETURN VALUE
!!        The system_getppid() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!        exec, fork(), getpgid(), getpgrp(), getpid(), kill(),
!!        setpgid(), setsid()
!!
!!##EXAMPLE
!!
!!   Get parent process PID (PPID) from Fortran
!!
!!    program demo_system_getppid
!!    use M_system, only : system_getppid
!!    implicit none
!!
!!    write(*,*)'PPID=',system_getppid()
!!
!!    end program demo_system_getppid
!===================================================================================================================================
character(len=*),parameter :: ident_ppid="@(#) M_system::system_getppid(3f): call getppid(3c)"
interface
   integer(kind=c_int) function system_getppid() bind (C,name="getppid")
   import c_int
   end function system_getppid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_umask(3fp) - [M_system] set and get the file mode creation mask
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_umask(umask_value)
!!
!!##DESCRIPTION
!!        The system_umask() function shall set the file mode creation mask of the
!!        process to cmask and return the previous value of the mask. Only
!!        the file permission bits of cmask (see <sys/stat.h>) are used;
!!        the meaning of the other bits is implementation-defined.
!!
!!        The file mode creation mask of the process is used to turn off
!!        permission bits in the mode argument supplied during calls to
!!        the following functions:
!!
!!         *  open(), openat(), creat(), mkdir(), mkdirat(), mkfifo(), and mkfifoat()
!!         *  mknod(), mknodat()
!!         *  mq_open()
!!         *  sem_open()
!!
!!        Bit positions that are set in cmask are cleared in the mode of the created file.
!!
!!##RETURN VALUE
!!        The file permission bits in the value returned by umask() shall be
!!        the previous value of the file mode creation mask. The state of any
!!        other bits in that value is unspecified, except that a subsequent
!!        call to umask() with the returned value as cmask shall leave the
!!        state of the mask the same as its state before the first call,
!!        including any unspecified use of those bits.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_umask
!!    use M_system, only : system_getumask, system_setumask
!!    implicit none
!!    integer value
!!    integer mask
!!    mask=O'002'
!!    value=system_setumask(mask)
!!    write(*,'(a,"octal=",O4.4," decimal=",i0)')'OLD VALUE=',value,value
!!    value=system_getumask()
!!    write(*,'(a,"octal=",O4.4," decimal=",i0)')'MASK=',mask,mask
!!    write(*,'(a,"octal=",O4.4," decimal=",i0)')'NEW VALUE=',value,value
!!    end program demo_system_umask
!!
!!   Expected results:
!!
!!    OLD VALUE=octal=0022 decimal=18
!!    MASK=octal=0002 decimal=2
!!    NEW VALUE=octal=0002 decimal=2
!===================================================================================================================================
character(len=*),parameter :: ident_umask="@(#) M_system::system_umask(3f): call umask(3c)"
interface
   integer(kind=c_int) function system_umask(umask_value) bind (C,name="umask")
   import c_int
   integer(kind=c_int),value :: umask_value
   end function system_umask
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_rand(3f) - [M_system] call pseudo-random number generator rand(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) :: function system_rand()
!!##DESCRIPTION
!!    Use rand(3c) to generate pseudo-random numbers.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_system_rand
!!       use M_system, only : system_srand, system_rand
!!       implicit none
!!       integer :: i
!!
!!       call system_srand(1001)
!!       do i=1,10
!!          write(*,*)system_rand()
!!       enddo
!!       write(*,*)
!!
!!       end program demo_system_rand
!!   expected results:
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!===================================================================================================================================
character(len=*),parameter :: ident_rand="@(#) M_system::system_rand(3f): call rand(3c)"
interface
   integer(kind=c_int) function system_rand() bind (C,name="rand")
      import c_int
   end function system_rand
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
interface
  subroutine c_flush() bind(C,name="my_flush")
  end subroutine c_flush
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_initenv(3f) - [M_system:ENVIRONMENT] initialize environment table pointer and size so table can be read by readenv(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine system_initenv()
!!##DESCRIPTION
!!    A simple interface allows reading the environment variable table
!!    of the process. Call system_initenv(3f) to initialize reading the
!!    environment table, then call system_readenv(3f) until a blank line
!!    is returned. If more than one thread reads the environment or the
!!    environment is changed while being read the results are undefined.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_initenv
!!    use M_system, only : system_initenv, system_readenv
!!    character(len=:),allocatable :: string
!!       call system_initenv()
!!       do
!!          string=system_readenv()
!!          if(string.eq.'')then
!!             exit
!!          else
!!             write(*,'(a)')string
!!          endif
!!       enddo
!!    end program demo_system_initenv
!!
!!   Sample results:
!!
!!    USERDOMAIN_ROAMINGPROFILE=buzz
!!    HOMEPATH=\Users\JSU
!!    APPDATA=C:\Users\JSU\AppData\Roaming
!!    MANPATH=/home/urbanjs/V600/LIBRARY/libGPF/download/tmp/man:/home/urbanjs/V600/doc/man:::
!!    DISPLAYNUM=0
!!    ProgramW6432=C:\Program Files
!!    HOSTNAME=buzz
!!    XKEYSYMDB=/usr/share/X11/XKeysymDB
!!    PUBLISH_CMD=
!!    OnlineServices=Online Services
!!         :
!!         :
!!         :
!===================================================================================================================================

integer(kind=c_long),bind(c,name="longest_env_variable") :: longest_env_variable

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
character(len=*),parameter :: ident_initenv="@(#) M_system::system_initenv(3f): initialize environment table for reading"
interface
   subroutine system_initenv() bind (C,NAME='my_initenv')
   end subroutine system_initenv
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!!type(c_ptr),bind(c,name="environ") :: c_environ

integer(kind=mode_t),bind(c,name="FS_IRGRP") ::R_GRP
integer(kind=mode_t),bind(c,name="FS_IROTH") ::R_OTH
integer(kind=mode_t),bind(c,name="FS_IRUSR") ::R_USR
integer(kind=mode_t),bind(c,name="FS_IRWXG") ::RWX_G
integer(kind=mode_t),bind(c,name="FS_IRWXO") ::RWX_O
integer(kind=mode_t),bind(c,name="FS_IRWXU") ::RWX_U
integer(kind=mode_t),bind(c,name="FS_IWGRP") ::W_GRP
integer(kind=mode_t),bind(c,name="FS_IWOTH") ::W_OTH
integer(kind=mode_t),bind(c,name="FS_IWUSR") ::W_USR
integer(kind=mode_t),bind(c,name="FS_IXGRP") ::X_GRP
integer(kind=mode_t),bind(c,name="FS_IXOTH") ::X_OTH
integer(kind=mode_t),bind(c,name="FS_IXUSR") ::X_USR
integer(kind=mode_t),bind(c,name="FDEFFILEMODE") :: DEFFILEMODE
integer(kind=mode_t),bind(c,name="FACCESSPERMS") :: ACCESSPERMS

! Host names are limited to {HOST_NAME_MAX} bytes.
integer(kind=mode_t),bind(c,name="FHOST_NAME_MAX") :: HOST_NAME_MAX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! for system_access(3f)
!integer(kind=c_int),bind(c,name="F_OK") :: F_OK
!integer(kind=c_int),bind(c,name="R_OK") :: R_OK
!integer(kind=c_int),bind(c,name="W_OK") :: W_OK
!integer(kind=c_int),bind(c,name="X_OK") :: X_OK
! not sure these will be the same on all systems, but above did not work
integer(kind=c_int),parameter           :: F_OK=0
integer(kind=c_int),parameter           :: R_OK=4
integer(kind=c_int),parameter           :: W_OK=2
integer(kind=c_int),parameter           :: X_OK=1
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_access(3f) - [M_system] checks accessibility or existence of a pathname
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_access(pathname,amode)
!!
!!    character(len=*),intent(in) :: pathname
!!    integer,intent(in)          :: amode
!!
!!##DESCRIPTION
!!
!!    The system_access(3f) function checks pathname existence and access
!!    permissions. The function checks the pathname for accessibility
!!    according to the bit pattern contained in amode, using the real user
!!    ID in place of the effective user ID and the real group ID in place
!!    of the effective group ID.
!!
!!    The value of amode is either the bitwise-inclusive OR of the access
!!    permissions to be checked (R_OK, W_OK, X_OK) or the existence test (F_OK).
!!
!!##OPTIONS
!!        pathname   a character string representing a directory pathname. Trailing spaces are ignored.
!!        amode      bitwise-inclusive OR of the values R_OK, W_OK, X_OK, or F_OK.
!!
!!##RETURN VALUE
!!        If not true an error occurred or the requested access is not granted
!!
!!##EXAMPLE
!!
!!   check if filename is accessible
!!
!!        Sample program:
!!
!!           program demo_system_access
!!           Use M_system, only : system_access, F_OK, R_OK, W_OK, X_OK
!!           implicit none
!!           integer                     :: i
!!           character(len=80),parameter :: names(*)=[ &
!!           '/usr/bin/bash   ', &
!!           '/tmp/NOTTHERE   ', &
!!           '/usr/local      ', &
!!           '.               ', &
!!           'PROBABLY_NOT    ']
!!           do i=1,size(names)
!!              write(*,*)' does ',trim(names(i)),' exist?    ', system_access(names(i),F_OK)
!!              write(*,*)' is ',trim(names(i)),' readable?     ', system_access(names(i),R_OK)
!!              write(*,*)' is ',trim(names(i)),' writeable?    ', system_access(names(i),W_OK)
!!              write(*,*)' is ',trim(names(i)),' executable?   ', system_access(names(i),X_OK)
!!           enddo
!!           end program demo_system_access
!===================================================================================================================================
function system_access(pathname,amode)
implicit none

character(len=*),parameter::ident_1="@(#)M_system::system_access(3f): checks accessibility or existence of a pathname"

character(len=*),intent(in) :: pathname
integer,intent(in)          :: amode
logical                     :: system_access

interface
  function c_access(c_pathname,c_amode) bind (C,name="my_access") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: c_pathname(*)
  integer(kind=c_int),value               :: c_amode
  integer(kind=c_int)                     :: c_ierr
  end function c_access
end interface

   if(c_access(str2_carr(trim(pathname)),int(amode,kind=c_int)).eq.0)then
      system_access=.true.
   else
      system_access=.false.
    !!if(system_errno().ne.0)then
    !!   call perror('*system_access*')
    !!endif
   endif

end function system_access
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_utime(3f) - [M_system] set file access and modification times
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        function utime(pathname,times)
!!
!!         character(len=*),intent(in) :: pathname
!!         integer,intent(in),optional :: times(2)
!!         logical                     :: utime
!!
!!##DESCRIPTION
!!        The system_utime(3f) function sets the access and modification
!!        times of the file named by the path argument by calling utime(3c).
!!
!!        If times() is not present the access and modification times of
!!        the file shall be set to the current time.
!!
!!        To use system_utime(3f) the effective user ID of the process must
!!        match the owner of the file, or the process has to have write
!!        permission to the file or have appropriate privileges,
!!
!!##OPTIONS
!!        times     If present, the values will be interpreted as the access
!!                  and modification times as Unix Epoch values. That is,
!!                  they are times measured in seconds since the Unix Epoch.
!!
!!        pathname  name of the file whose access and modification times
!!                  are to be updated.
!!
!!##RETURN VALUE
!!        Upon successful completion .TRUE. is returned. Otherwise,
!!        .FALSE. is returned and errno shall be set to indicate the error,
!!        and the file times remain unaffected.
!!
!!##ERRORS
!!        The underlying utime(3c) function fails if:
!!
!!        EACCES  Search permission is denied by a component of the path
!!                prefix; or the times argument is a null pointer and the
!!                effective user ID of the process does not match the owner
!!                of the file, the process does not have write permission
!!                for the file, and the process does not have appropriate
!!                privileges.
!!
!!        ELOOP  A loop exists in symbolic links encountered during
!!               resolution of the path argument.
!!
!!        ENAMETOOLONG   The length of a component of a pathname is longer
!!                       than {NAME_MAX}.
!!
!!        ENOENT   A component of path does not name an existing file
!!                 or path is an empty string.
!!
!!        ENOTDIR  A component of the path prefix names an existing file
!!                 that is neither a directory nor a symbolic link to a
!!                 directory, or the path argument contains at least one
!!                 non-<slash> character and ends with one or more trailing
!!                 <slash> characters and the last pathname component
!!                 names an existing file that is neither a directory nor
!!                 a symbolic link to a directory.
!!
!!        EPERM  The times argument is not a null pointer and the effective
!!               user ID of the calling process does not match the owner
!!               of the file and the calling process does not have
!!               appropriate privileges.
!!
!!        EROFS  The file system containing the file is read-only.
!!
!!   The utime() function may fail if:
!!
!!        ELOOP  More than {SYMLOOP_MAX} symbolic links were encountered
!!               during resolution of the path argument.
!!
!!        ENAMETOOLONG  The length of a pathname exceeds {PATH_MAX}, or
!!                      pathname resolution of a symbolic link produced
!!                      an intermediate result with a length that exceeds
!!                      {PATH_MAX}.
!!
!!##EXAMPLES
!!
!!      Sample program
!!
!!       program demo_system_utime
!!       use M_system, only : system_utime, system_perror
!!       implicit none
!!       character(len=4096) :: pathname
!!       integer             :: times(2)
!!       integer             :: i
!!          do i=1,command_argument_count()
!!             call get_command_argument(i, pathname)
!!             if(.not.system_utime(pathname,times))then
!!                call system_perror('*demo_system_utime*')
!!             endif
!!          enddo
!!       end program demo_system_utime
!===================================================================================================================================
function system_utime(pathname,times)
use M_time, only   : d2u
implicit none

character(len=*),parameter::ident_2="@(#)M_system::system_utime(3f): set access and modification times of a pathname"

character(len=*),intent(in) :: pathname
integer,intent(in),optional :: times(2)
integer                     :: times_local(2)
logical                     :: system_utime

!! int my_utime(const char *path, int times[2])
interface
  function c_utime(c_pathname,c_times) bind (C,name="my_utime") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: c_pathname(*)
  integer(kind=c_int),intent(in)          :: c_times(2)
  integer(kind=c_int)                     :: c_ierr
  end function c_utime
end interface
   if(present(times))then
      times_local=times
   else
      times_local=int(d2u())
   endif
   if(c_utime(str2_carr(trim(pathname)),int(times_local,kind=c_int)).eq.0)then
      system_utime=.true.
   else
      system_utime=.false.
      !!if(system_errno().ne.0)then
      !!   call perror('*system_utime*')
      !!endif
   endif

end function system_utime
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_realpath(3f) - [M_system] call realpath(3c) to resolve a pathname
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!       function system_realpath(input) result(output)
!!
!!        character(len=*),intent(in)  :: input
!!        character(len=:),allocatable :: output
!!##DESCRIPTION
!!        system_realpath(3f) calls the C routine realpath(3c) to obtain the absolute pathname of given path
!!##OPTIONS
!!
!!        INPUT     pathname to resolve
!!
!!##RETURN VALUE
!!        OUTPUT    The absolute pathname of the given input pathname.
!!                  The pathname shall contain no components that are dot or dot-dot,
!!                  or are symbolic links. It is equal to the NULL character if an error occurred.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_realpath
!!    use M_system, only : system_realpath, system_perror
!!    implicit none
!!    ! resolve each pathname given on command line
!!    character(len=:),allocatable :: pathi,patho
!!    integer                      :: i
!!    integer                      :: filename_length
!!       do i = 1, command_argument_count()
!!          ! get pathname from command line arguments
!!          call get_command_argument (i , length=filename_length)
!!          allocate(character(len=filename_length) :: pathi)
!!          call get_command_argument (i , value=pathi)
!!          !
!!          ! resolve each pathname
!!          patho=system_realpath(pathi)
!!          if(patho.ne.char(0))then
!!             write(*,*)trim(pathi),'=>',trim(patho)
!!          else
!!             call system_perror('*system_realpath* error for pathname '//trim(pathi)//':')
!!             write(*,*)trim(pathi),'=>',trim(patho)
!!          endif
!!          deallocate(pathi)
!!       enddo
!!       ! if there were no pathnames given resolve the pathname "."
!!       if(i.eq.1)then
!!          patho=system_realpath('.')
!!          write(*,*)'.=>',trim(patho)
!!       endif
!!    end program demo_system_realpath
!!
!!  Example usage:
!!
!!   demo_system_realpath
!!   .=>/home/urbanjs/V600
!!
!!   cd /usr/share/man
!!   demo_system_realpath . .. NotThere
!!   .=>/usr/share/man
!!   ..=>/usr/share
!!   *system_realpath* error for pathname NotThere:: No such file or directory
!!   NotThere=>NotThere
!===================================================================================================================================
function system_realpath(input) result(string)

character(len=*),parameter::ident_3="&
&@(#)M_system::system_realpath(3f):call realpath(3c) to get pathname of current working directory"

character(len=*),intent(in)    :: input
type(c_ptr)                    :: c_output
character(len=:),allocatable   :: string
interface
   function c_realpath(c_input) bind(c,name="my_realpath") result(c_buffer)
      import c_char, c_size_t, c_ptr, c_int
      character(kind=c_char) ,intent(in)  :: c_input(*)
      type(c_ptr)                         :: c_buffer
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   c_output=c_realpath(str2_carr(trim(input)))
   if(.not.c_associated(c_output))then
      string=char(0)
   else
      string=C2F_string(c_output)
   endif
end function system_realpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_issock(3f) - [M_system] checks if argument is a socket
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_issock(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_issock
!!
!!##DESCRIPTION
!!        The issock(3f) function checks if path is a path to a socket
!!
!!##OPTIONS
!!        path   a character string representing a socket pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_issock() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a socket
!!
!!    program demo_system_issock
!!    Use M_system, only : system_issock
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'sock.test       ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a socket? ', system_issock(names(i))
!!    enddo
!!    end program demo_system_issock
!===================================================================================================================================
function system_issock(pathname)
implicit none

character(len=*),parameter::ident_4="@(#)M_system::system_issock(3f): determine if pathname is a socket"

character(len=*),intent(in) :: pathname
logical                     :: system_issock

interface
  function c_issock(pathname) bind (C,name="my_issock") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: pathname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_issock
end interface

   if(c_issock(str2_carr(trim(pathname))).eq.1)then
      system_issock=.true.
   else
      system_issock=.false.
   endif

end function system_issock
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_isfifo(3f) - [M_system] checks if argument is a fifo - named pipe
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_isfifo(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isfifo
!!
!!##DESCRIPTION
!!        The isfifo(3f) function checks if path is a path to a fifo - named pipe.
!!
!!##OPTIONS
!!        path   a character string representing a fifo - named pipe pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isfifo() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a FIFO file
!!
!!    program demo_system_isfifo
!!    Use M_system, only : system_isfifo
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'fifo.test       ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a fifo(named pipe)? ', system_isfifo(names(i))
!!    enddo
!!    end program demo_system_isfifo
!===================================================================================================================================
function system_isfifo(pathname)
implicit none

character(len=*),parameter::ident_5="@(#)M_system::system_isfifo(3f): determine if pathname is a fifo(named pipe)"

character(len=*),intent(in) :: pathname
logical                     :: system_isfifo

interface
  function c_isfifo(pathname) bind (C,name="my_isfifo") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: pathname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_isfifo
end interface

   if(c_isfifo(str2_carr(trim(pathname))).eq.1)then
      system_isfifo=.true.
   else
      system_isfifo=.false.
   endif

end function system_isfifo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_ischr(3f) - [M_system] checks if argument is a character device
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_ischr(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_ischr
!!
!!##DESCRIPTION
!!        The ischr(3f) function checks if path is a path to a character device.
!!
!!##OPTIONS
!!        path   a character string representing a character device pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_ischr() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a character file
!!
!!    program demo_system_ischr
!!    Use M_system, only : system_ischr
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'char_dev.test   ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a character device? ', system_ischr(names(i))
!!    enddo
!!    end program demo_system_ischr
!!
!!   Results:
!===================================================================================================================================
function system_ischr(pathname)
implicit none

character(len=*),parameter::ident_6="@(#)M_system::system_ischr(3f): determine if pathname is a link"

character(len=*),intent(in) :: pathname
logical                     :: system_ischr

interface
  function c_ischr(pathname) bind (C,name="my_ischr") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: pathname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_ischr
end interface

   if(c_ischr(str2_carr(trim(pathname))).eq.1)then
      system_ischr=.true.
   else
      system_ischr=.false.
   endif

end function system_ischr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_isreg(3f) - [M_system] checks if argument is a regular file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_isreg(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isreg
!!
!!##DESCRIPTION
!!        The isreg(3f) function checks if path is a regular file
!!
!!##OPTIONS
!!        path   a character string representing a pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isreg() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_islnk(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a regular file
!!
!!    program demo_system_isreg
!!    Use M_system, only : system_isreg
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    'test.txt        ', &
!!    '.               ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a regular file? ', system_isreg(names(i))
!!    enddo
!!    end program demo_system_isreg
!===================================================================================================================================
function system_isreg(pathname)
implicit none

character(len=*),parameter::ident_7="@(#)M_system::system_isreg(3f): determine if pathname is a regular file"

character(len=*),intent(in) :: pathname
logical                     :: system_isreg

interface
  function c_isreg(pathname) bind (C,name="my_isreg") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: pathname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_isreg
end interface

   if(c_isreg(str2_carr(trim(pathname))).eq.1)then
      system_isreg=.true.
   else
      system_isreg=.false.
   endif

end function system_isreg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_islnk(3f) - [M_system] checks if argument is a link
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function system_islnk(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_islnk
!!
!!##DESCRIPTION
!!        The islnk(3f) function checks if path is a path to a link.
!!
!!##OPTIONS
!!    path          a character string representing a link
!!                  pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!    system_islnk  The system_islnk() function should always be
!!                  successful and no return value is reserved to
!!                  indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_system_islnk
!!    Use M_system, only : system_islnk
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'link.test       ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a link? ', system_islnk(names(i))
!!    enddo
!!    end program demo_system_islnk
!!
!!   Results:
!===================================================================================================================================
function system_islnk(pathname)
implicit none

character(len=*),parameter::ident_8="@(#)M_system::system_islnk(3f): determine if pathname is a link"

character(len=*),intent(in) :: pathname
logical                     :: system_islnk

interface
  function c_islnk(pathname) bind (C,name="my_islnk") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: pathname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_islnk
end interface

   if(c_islnk(str2_carr(trim(pathname))).eq.1)then
      system_islnk=.true.
   else
      system_islnk=.false.
   endif

end function system_islnk
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! system_isblk(3f) - [M_system] checks if argument is a block device
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_isblk(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isblk
!!
!!##DESCRIPTION
!! The isblk(3f) function checks if path is a path to a block device.
!!
!!##OPTIONS
!! path   a character string representing a block device pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isblk() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a block device
!!
!!    program demo_system_isblk
!!    Use M_system, only : system_isblk
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'block_device.tst', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!        write(*,*)' is ',trim(names(i)),' a block device? ', system_isblk(names(i))
!!    enddo
!!    end program demo_system_isblk
!!
!!   Results:
!===================================================================================================================================
function system_isblk(pathname)
implicit none

character(len=*),parameter::ident_9="@(#)M_system::system_isblk(3f): determine if pathname is a block device"

character(len=*),intent(in) :: pathname
logical                     :: system_isblk

interface
  function c_isblk(pathname) bind (C,name="my_isblk") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: pathname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_isblk
end interface

   if(c_isblk(str2_carr(trim(pathname))).eq.1)then
      system_isblk=.true.
   else
      system_isblk=.false.
   endif

end function system_isblk
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_chown(3f) - [M_system] change file owner and group
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_chown(path,owner,group)
!!
!!    character(len=*),intent(in) :: path
!!    integer,intent(in)          :: owner
!!    integer,intent(in)          :: group
!!
!!##DESCRIPTION
!!        The chown(3f) function changes owner and group of a file
!!
!!       The path argument points to a pathname naming a file. The
!!       user ID and group ID of the named file shall be set to the numeric
!!       values contained in owner and group, respectively.
!!
!!       Only processes with an effective user ID equal to the user ID of
!!       the file or with appropriate privileges may change the ownership
!!       of a file.
!!
!!##OPTIONS
!!       path   a character string representing a file pathname.
!!              Trailing spaces are ignored.
!!       owner  UID of owner that ownership is to be changed to
!!       group  GID of group that ownership is to be changed to
!!
!!##RETURN VALUE
!!       The system_chown(3f) function should return zero 0 if successful.
!!       Otherwise, these functions shall return 1 and set errno to
!!       indicate the error. If 1 is returned, no changes are made in
!!       the user ID and group ID of the file.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_system_chown
!!    Use M_system, only : system_chown
!!    Use M_system, only : system_getuid
!!    Use M_system, only : system_getgid
!!    use M_system, only : system_perror
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[character(len=80) :: 'myfile1','/usr/local']
!!    do i=1,size(names)
!!       if(.not.  system_chown(&
!!       & trim(names(i)),  &
!!       & system_getuid(), &
!!       & system_getgid()) &
!!          )then
!!          call system_perror('*demo_system_chown* '//trim(names(i)))
!!       endif
!!    enddo
!!    end program demo_system_chown
!===================================================================================================================================

function system_chown(dirname,owner,group)
implicit none

character(len=*),parameter::ident_10="&
&@(#)M_system::system_chown(3f): change owner and group of a file relative to directory file descriptor"

character(len=*),intent(in) :: dirname
integer,intent(in)          :: owner
integer,intent(in)          :: group
logical                     :: system_chown

! int chown(const char *path, uid_t owner, gid_t group);
interface
  function c_chown(c_dirname,c_owner,c_group) bind (C,name="my_chown") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: c_dirname(*)
  integer(kind=c_int),intent(in),value    :: c_owner
  integer(kind=c_int),intent(in),value    :: c_group
  integer(kind=c_int)                     :: c_ierr
  end function c_chown
end interface

   if(c_chown(str2_carr(trim(dirname)),int(owner,kind=c_int),int(group,kind=c_int)).eq.1)then
      system_chown=.true.
   else
      system_chown=.false.
   endif

end function system_chown
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_isdir(3f) - [M_system] checks if argument is a directory path
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function system_isdir(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isdir
!!
!!##DESCRIPTION
!!        The system_isdir(3f) function checks if path is a directory.
!!
!!##OPTIONS
!!        path   a character string representing a directory pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isdir() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_islnk(3f), system_stat(3f), isreg(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a directory
!!
!!      program demo_system_isdir
!!      Use M_system, only : system_isdir
!!      implicit none
!!      integer                     :: i
!!      character(len=80),parameter :: names(*)=[ &
!!      '/tmp            ', &
!!      '/tmp/NOTTHERE   ', &
!!      '/usr/local      ', &
!!      '.               ', &
!!      'PROBABLY_NOT    ']
!!      do i=1,size(names)
!!         write(*,*)' is ',trim(names(i)),' a directory? ', system_isdir(names(i))
!!      enddo
!!      end program demo_system_isdir
!!
!!   Results:
!!
!!      is /tmp a directory?  T
!!      is /tmp/NOTTHERE a directory?  F
!!      is /usr/local a directory?  T
!!      is . a directory?  T
!!      is PROBABLY_NOT a directory?  F
!===================================================================================================================================
function system_isdir(dirname)
implicit none

character(len=*),parameter::ident_11="@(#)M_system::system_isdir(3f): determine if DIRNAME is a directory name"

character(len=*),intent(in) :: dirname
logical                     :: system_isdir

interface
  function c_isdir(dirname) bind (C,name="my_isdir") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: dirname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_isdir
end interface

   if(c_isdir(str2_carr(trim(dirname))).eq.1)then
      system_isdir=.true.
   else
      system_isdir=.false.
   endif

end function system_isdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_cpu_time(3f) - [M_system] get processor time by calling times(3c)
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine system_cpu_time(c_user, c_system, c_total)
!!
!!         real,intent(out) :: c_total
!!         real,intent(out) :: c_user
!!         real,intent(out) :: c_system
!!
!!##DESCRIPTION
!!
!!##OUTPUT
!!         c_total   total processor time ( c_user + c_system )
!!         c_user    processor user time
!!         c_system  processor system time
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##EXAMPLES
!!
!!
!!   Sample program:
!!
!!    program demo_system_cpu_time
!!
!!    use M_system, only : system_cpu_time
!!    use ISO_C_BINDING, only : c_float
!!    implicit none
!!    real    :: user_start, system_start, total_start
!!    real    :: user_finish, system_finish, total_finish
!!    integer :: i
!!    integer :: itimes=1000000
!!    real    :: value
!!
!!       call system_cpu_time(total_start,user_start,system_start)
!!
!!       value=0.0
!!       do i=1,itimes
!!          value=sqrt(real(i)+value)
!!       enddo
!!       write(10,*)value
!!       flush(10)
!!       write(*,*)'average sqrt value=',value/itimes
!!       call system_cpu_time(total_finish,user_finish,system_finish)
!!       write(*,*)'USER ......',user_finish-user_start
!!       write(*,*)'SYSTEM ....',system_finish-system_start
!!       write(*,*)'TOTAL .....',total_finish-total_start
!!
!!    end program demo_system_cpu_time
!!
!!   Typical Results:
!===================================================================================================================================
!! GET ERRORS ABOUT MISSING LONGEST_ENV_VARIABLE IN GFORTRAN 6.4.0 IF JUST USE INTERFACE INSTEAD OF MAKING SUBROUTINE
!!character(len=*),parameter :: ident_cpu_time="@(#) M_system::system_cpu_time(3f): get processor time using times(3c)"
!!interface
!!   subroutine system_cpu_time(c_total,c_user,c_system) bind (C,NAME='my_cpu_time')
!!      import c_float
!!      real(kind=c_float) :: c_user,c_system,c_total
!!   end subroutine system_cpu_time
!!end interface
subroutine system_cpu_time(total,user,system)

character(len=*),parameter :: ident_cpu_time="@(#) M_system::system_cpu_time(3f): get processor time using times(3c)"

real,intent(out)   :: user,system,total
real(kind=c_float) :: c_user,c_system,c_total

interface
   subroutine c_cpu_time(c_total,c_user,c_system) bind (C,NAME='my_cpu_time')
      import c_float
      real(kind=c_float) :: c_total,c_user,c_system
   end subroutine c_cpu_time
end interface

call c_cpu_time(c_total,c_user,c_system)
user=c_user
system=c_system
total=c_total
end subroutine system_cpu_time
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_link(3f) - [M_system] link one file to another file relative to two directory file descriptors
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function link(oldpath,newpath);
!!
!!     character(len=*),intent(in) :: oldpath
!!     character(len=*),intent(in) :: newpath
!!
!!##DESCRIPTION
!!        The link() function shall create a new link (directory entry) for the existing file, path1.
!!
!!        The path1 argument points to a pathname naming an existing file. The path2 argument points to a pathname naming the new
!!        directory entry to be created. The link() function shall atomically create a new link for the existing file and the link
!!        count of the file shall be incremented by one.
!!
!!        If path1 names a directory, link() shall fail unless the process has appropriate privileges and the implementation
!!        supports
!!        using link() on directories.
!!
!!        If path1 names a symbolic link, it is implementation-defined whether link() follows the symbolic link, or creates a new
!!        link to the symbolic link itself.
!!
!!        Upon successful completion, link() shall mark for update the last file status change timestamp of the file. Also, the
!!        last data modification and last file status change timestamps of the directory that contains the new entry shall be
!!        marked for update.
!!
!!        If link() fails, no link shall be created and the link count of the file shall remain unchanged.
!!
!!        The implementation may require that the calling process has permission to access the existing file.
!!
!!        The linkat() function shall be equivalent to the link() function except that symbolic links shall be handled as specified
!!        by the value of flag (see below) and except in the case where either path1 or path2 or both are relative paths. In this
!!        case a relative path path1 is interpreted relative to the directory associated with the file descriptor fd1 instead of
!!        the current working directory and similarly for path2 and the file descriptor fd2. If the file descriptor was opened
!!        without O_SEARCH, the function shall check whether directory searches are permitted using the current permissions of the
!!        directory underlying the file descriptor. If the file descriptor was opened with O_SEARCH, the function shall not perform
!!        the check.
!!
!!        Values for flag are constructed by a bitwise-inclusive OR of flags from the following list, defined in <fcntl.h>:
!!
!!        AT_SYMLINK_FOLLOW
!!              If path1 names a symbolic link, a new link for the target of the symbolic link is created.
!!
!!        If linkat() is passed the special value AT_FDCWD in the fd1 or fd2 parameter, the current working directory shall be used
!!        for the respective path argument. If both fd1 and fd2 have value AT_FDCWD, the behavior shall be identical to a call to
!!        link(), except that symbolic links shall be handled as specified by the value of flag.
!!
!!        Some implementations do allow links between file systems.
!!
!!        If path1 refers to a symbolic link, application developers should use linkat() with appropriate flags to select whether
!!        or not the symbolic link should be resolved.
!!
!!        If the AT_SYMLINK_FOLLOW flag is clear in the flag argument and the path1 argument names a symbolic link, a new link is
!!        created for the symbolic link path1 and not its target.
!!
!!##RETURN VALUE
!!        Upon successful completion, these functions shall return 0. Otherwise, these functions shall return -1 and set errno to
!!        indicate the error.
!!
!!##EXAMPLES
!!
!!   Creating a Link to a File
!!
!!    program demo_system_link
!!    use M_system, only : system_link, system_perror
!!    integer :: ierr
!!    ierr = system_link('myfile1','myfile2')
!!    if(ierr.ne.0)then
!!       call system_perror('*demo_system_link*')
!!    endif
!!    end program demo_system_link
!===================================================================================================================================
function system_link(oldname,newname) result(ierr)

character(len=*),parameter::ident_12="@(#)M_system::system_link(3f): call link(3c) to create a file link"

character(len=*),intent(in) :: oldname
character(len=*),intent(in) :: newname
integer                     :: ierr
integer(kind=c_int)         :: c_ierr

interface
  function c_link(c_oldname,c_newname) bind (C,name="link") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: c_oldname(*)
  character(kind=c_char,len=1),intent(in) :: c_newname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_link
end interface

   c_ierr=c_link(str2_carr(trim(oldname)),str2_carr(trim(newname)))
   ierr=c_ierr

end function system_link
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_unlink(3f) - [M_system] remove a directory entry relative to directory file descriptor
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function unlink(path);
!!
!!     character(len=*) :: path
!!
!!##DESCRIPTION
!!    The unlink() function shall remove a link to a file. If path names a
!!    symbolic link, unlink() shall remove the symbolic link named by path
!!    and shall not affect any file or directory named by the contents of
!!    the symbolic link. Otherwise, unlink() shall remove the link named by
!!    the pathname pointed to by path and shall decrement the link count of
!!    the file referenced by the link.
!!
!!    When the file's link count becomes 0 and no process has the file open,
!!    the space occupied by the file shall be freed and the file shall no
!!    longer be accessible. If one or more processes have the file open when
!!    the last link is removed, the link shall be removed before unlink()
!!    returns, but the removal of the file contents shall be postponed until
!!    all references to the file are closed.
!!
!!    The path argument shall not name a directory unless the process has
!!    appropriate privileges and the implementation supports using unlink()
!!    on directories.
!!
!!    Upon successful completion, unlink() shall mark for update the last
!!    data modification and last file status change timestamps of the parent
!!    directory. Also, if the file's link count is not 0, the last file status
!!    change timestamp of the file shall be marked for update.
!!
!!    Values for flag are constructed by a bitwise-inclusive OR of flags from
!!    the following list, defined in <fcntl.h>:
!!
!!       AT_REMOVEDIR
!!
!!     Remove the directory entry specified by fd and path as a
!!     directory, not a normal file.
!!
!!##RETURN VALUE
!!
!!    Upon successful completion, these functions shall return 0. Otherwise,
!!    these functions shall return -1 and set errno to indicate the error. If
!!    -1 is returned, the named file shall not be changed.
!!
!!##EXAMPLES
!!
!!   Removing a link to a file
!!
!!    program demo_system_unlink
!!    use M_system, only : system_unlink, system_perror
!!    integer :: ierr
!!    ierr = system_unlink('myfile1')
!!    if(ierr.ne.0)then
!!       call system_perror('*demo_system_unlink*')
!!    endif
!!    end program demo_system_unlink
!===================================================================================================================================
function system_unlink(fname) result (ierr)

character(len=*),parameter::ident_13="@(#)M_system::system_unlink(3f): call unlink(3c) to rm file link"

character(len=*),intent(in) :: fname
integer                     :: ierr

interface
  function c_unlink(c_fname) bind (C,name="unlink") result (c_ierr)
  import c_char, c_int
  character(kind=c_char,len=1) :: c_fname(*)
  integer(kind=c_int)          :: c_ierr
  end function c_unlink
end interface
   ierr=c_unlink(str2_carr(trim(fname)))
end function system_unlink
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_setumask(3f) - [M_system] set the file mode creation umask
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer function system_setumask(new_umask) result (old_umask)
!!
!!     integer,intent(in)  :: new_umask
!!     integer(kind=c_int) :: umask_c
!!
!!##DESCRIPTION
!!        The system_umask(3f) function sets the file mode creation mask of the
!!        process to cmask and return the previous value of the mask. Only
!!        the file permission bits of cmask (see <sys/stat.h>) are used;
!!        the meaning of the other bits is implementation-defined.
!!
!!        The file mode creation mask of the process is used to turn off
!!        permission bits in the mode argument supplied during calls to
!!        the following functions:
!!
!!         *  open(), openat(), creat(), mkdir(), mkdirat(), mkfifo(), and mkfifoat()
!!         *  mknod(), mknodat()
!!         *  mq_open()
!!         *  sem_open()
!!
!!        Bit positions that are set in cmask are cleared in the mode of
!!        the created file.
!!
!!##RETURN VALUE
!!        The file permission bits in the value returned by umask() shall be
!!        the previous value of the file mode creation mask. The state of any
!!        other bits in that value is unspecified, except that a subsequent
!!        call to umask() with the returned value as cmask shall leave the
!!        state of the mask the same as its state before the first call,
!!        including any unspecified use of those bits.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_setumask
!!    use M_system, only : system_getumask, system_setumask
!!    integer :: newmask
!!    integer :: i
!!    integer :: old_umask
!!    write(*,101)(system_getumask(),i=1,4)
!!    101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
!!    newmask=63
!!    old_umask=system_setumask(newmask)
!!    write(*,*)'NEW'
!!    write(*,101)(system_getumask(),i=1,4)
!!    end program demo_setumask
!!
!!   Expected output
!!
!!     18 O'022' Z"12' B'000010010"
!!     NEW
!!     63 O'077' Z"3F' B'000111111"
!===================================================================================================================================
integer function system_setumask(umask_value) result (old_umask)
integer,intent(in)  :: umask_value
integer(kind=c_int) :: umask_c

   umask_c=umask_value
   old_umask=system_umask(umask_c) ! set current umask

end function system_setumask
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getumask(3f) - [M_system] get current umask
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   integer function system_getumask() result (umask_value)
!!##DESCRIPTION
!!   The return value from getumask(3f) is the value of the file
!!   creation mask, obtained by using umask(3c).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_getumask
!!    use M_system, only : system_getumask, system_setumask
!!    integer :: i
!!    write(*,101)(system_getumask(),i=1,4)
!!    101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
!!    end program demo_getumask
!!
!!   Expected output
!!
!!     18 O'022' Z"12' B'000010010"
!===================================================================================================================================
integer function system_getumask() result (umask_value)
! The return value from umask() is just the previous value of the file
! creation mask, so that this system call can be used both to get and
! set the required values. Sadly, however, there is no way to get the old
! umask value without setting a new value at the same time.

! This means that in order just to see the current value, it is necessary
! to execute a piece of code like the following function:
integer             :: idum
integer(kind=c_int) :: old_umask
   old_umask=system_umask(0_c_int) ! get current umask but by setting umask to 0 (a conservative mask so no vulnerability is open)
   idum=system_umask(old_umask)    ! set back to original mask
   umask_value=old_umask
end function system_getumask
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      perror(3f) - [M_system] print error message for last C error on stderr
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      subroutine system_perror(prefix)
!!
!!       character(len=*),intent(in) :: prefix
!!
!!##DESCRIPTION
!!    Use system_perror(3f) to print an error message on stderr
!!    corresponding to the current value of the C global variable errno.
!!    Unless you use NULL as the argument prefix, the error message will
!!    begin with the prefix string, followed by a colon and a space
!!    (:). The remainder of the error message produced is one of the
!!    strings described for strerror(3c).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_perror
!!    use M_system, only : system_perror,system_rmdir
!!    implicit none
!!    character(len=:),allocatable :: DIRNAME
!!    DIRNAME='/NOT/THERE/OR/ANYWHERE'
!!    ! generate an error with a routine that supports errno and perror(3c)
!!    if(system_rmdir(DIRNAME).ne.0)then
!!       call system_perror('*demo_system_perror*:'//DIRNAME)
!!    endif
!!    write(*,'(a)')"That's all Folks!"
!!    end program demo_system_perror
!!
!!   Expected results:
!!
!!    *demo_system_perror*:/NOT/THERE/OR/ANYWHERE: No such file or directory
!!    That's all Folks!
!===================================================================================================================================
subroutine system_perror(prefix)
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment

character(len=*),parameter::ident_14="@(#)M_system::system_perror(3f): call perror(3c) to display error message"

character(len=*),intent(in) :: prefix
   integer                  :: ios

interface
  subroutine c_perror(c_prefix) bind (C,name="perror")
  import c_char
  character(kind=c_char) :: c_prefix(*)
  end subroutine c_perror
end interface

   flush(unit=ERROR_UNIT,iostat=ios)
   flush(unit=OUTPUT_UNIT,iostat=ios)
   flush(unit=INPUT_UNIT,iostat=ios)
   call c_perror(str2_carr((trim(prefix))))
   call c_flush()

end subroutine system_perror
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_chdir(3f) - [M_system] call chdir(3c) from Fortran to change working directory
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_chdir(path, err)
!!
!!     character(len=*)               :: path
!!     integer, optional, intent(out) :: err
!!
!!##DESCRIPTION
!!
!!    system_chdir(3f) changes the current working directory of the calling
!!    process to the directory specified in path. The current working
!!    directory is the starting point for interpreting relative pathnames
!!    (those not starting with '/').
!!
!!##RETURN VALUE
!!
!!    On success, zero is returned. On error, -1 is returned, and errno is
!!    set appropriately.
!!
!!
!!    Depending on the file system, other errors can be returned. The more
!!    general errors for chdir() are listed below, by their C definitions:
!!
!!    Errors
!!    EACCES        Search permission is denied for one of the components of path.
!!                  (See also path_resolution(7).)
!!    EFAULT        path points outside your accessible address space.
!!    EIO           An I/O error occurred.
!!    ELOOP         Too many symbolic links were encountered in resolving path.
!!    ENAMETOOLONG  path is too long.
!!    ENOENT        The file does not exist.
!!    ENOMEM        Insufficient kernel memory was available.
!!    ENOTDIR       A component of path is not a directory.
!!
!!##SEE ALSO
!!
!!    chroot(2), getcwd(3), path_resolution(7)
!!
!!##EXAMPLE
!!
!!    Change working directory from Fortran
!!
!!      program demo_system_chdir
!!      use M_system, only : system_chdir
!!      implicit none
!!      integer :: ierr
!!
!!      call execute_command_line('pwd')
!!      call system_chdir('/tmp',ierr)
!!      call execute_command_line('pwd')
!!      write(*,*)'*CHDIR TEST* IERR=',ierr
!!
!!      end program demo_system_chdir
!!
!!##RESULTS:
!!   Sample run output:
!!
!!      /home/urbanjs/V600
!!      /tmp
!!      *CHDIR TEST* IERR=           0
!===================================================================================================================================
subroutine system_chdir(path, err)

character(len=*),parameter::ident_15="@(#)M_system::system_chdir(3f): call chdir(3c)"

character(len=*)               :: path
integer, optional, intent(out) :: err

interface
   integer(kind=c_int)  function c_chdir(c_path) bind(C,name="chdir")
      import c_char, c_int
      character(kind=c_char)   :: c_path(*)
   end function
end interface
   integer                     :: loc_err
!-----------------------------------------------------------------------------------------------------------------------------------
   loc_err=c_chdir(str2_carr(trim(path)))
   if(present(err))then
      err=loc_err
   endif
end subroutine system_chdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      system_remove(3f) - [M_system] call remove(3c) to remove file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_remove(path) result(err)
!!
!!    character(*),intent(in) :: path
!!    integer(c_int)          :: err
!!
!!##DESCRIPTION
!!    Fortran supports scratch files via the OPEN(3c) command; but does
!!    not otherwise allow for removing files. The system_remove(3f) command
!!    allows for removing files by name that the user has the authority to
!!    remove by calling the C remove(3c) function.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_remove
!!    use M_system, only : system_remove
!!    character(len=*),parameter :: FILE='MyJunkFile.txt'
!!    integer :: ierr
!!    write(*,*)'BEFORE CREATED '//FILE
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    ! note intentionally causes error if file exists
!!    open(unit=10,file=FILE,status='NEW')
!!    write(*,*)'AFTER OPENED '//FILE
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    write(10,'(a)') 'This is a file I want to delete'
!!    close(unit=10)
!!    write(*,*)'AFTER CLOSED '
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    ierr=system_remove(FILE)
!!    write(*,*)'AFTER REMOVED',IERR
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    end program demo_system_remove
!!
!!   Expected Results:
!!
!!    >  BEFORE CREATED MyJunkFile.txt
!!    > ls: cannot access 'MyJunkFile.txt': No such file or directory
!!    >
!!    >  AFTER OPENED MyJunkFile.txt
!!    > -rw-r--r-- 1 JSU None 0 Nov 19 19:32 MyJunkFile.txt
!!    >
!!    >  AFTER CLOSED
!!    > -rw-r--r-- 1 JSU None 32 Nov 19 19:32 MyJunkFile.txt
!!    >
!!    >  AFTER REMOVED           0
!!    > ls: cannot access 'MyJunkFile.txt': No such file or directory
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_remove(path) result(err)

character(len=*),parameter::ident_16="@(#)M_system::system_remove(3f): call remove(3c) to remove file"

character(*),intent(in) :: path
integer(c_int)          :: err

interface
   function c_remove(c_path) bind(c,name="remove") result(c_err)
      import c_char,c_int
      character(kind=c_char,len=1),intent(in) :: c_path(*)
      integer(c_int)                          :: c_err
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   err= c_remove(str2_carr(trim(path)))
end function system_remove
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      system_rename(3f) - [M_system] call rename(3c) to rename a system file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_rename(input,output) result(ierr)
!!
!!    character(*),intent(in)    :: input,output
!!    integer                    :: ierr
!!##DESCRIPTION
!!     Rename a file by calling rename(3c). It is not recommended that the
!!     rename occur while either filename is being used on a file currently
!!     OPEN(3f) by the program.
!!
!!     Both the old and new names must be on the same device.
!!##OPTIONS
!!     INPUT   system filename of an existing file to rename
!!     OUTPUT  system filename to be created or overwritten by INPUT file.
!!             Must be on the same device as the INPUT file.
!!##RETURNS
!!     IERR    zero (0) if no error occurs. If not zero a call to
!!             system_errno(3f) or system_perror(3f) is supported
!!             to diagnose error
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_system_rename
!!      use M_system, only : system_rename
!!      use M_system, only : system_remove
!!      use M_system, only : system_perror
!!      implicit none
!!      character(len=256) :: string
!!      integer            :: ios, ierr
!!
!!      ! try to remove junk files just in case
!!      ierr=system_remove('_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      call system_perror('*demo_system_rename*')
!!      ierr=system_remove('_renamed_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      call system_perror('*demo_system_rename*')
!!
!!      ! create scratch file to rename
!!      open(unit=10,file='_scratch_file_',status='new')
!!      write(10,'(a)') 'Test by renaming "_scratch_file_" to "_renamed_scratch_file_"'
!!      write(10,'(a)') 'IF YOU SEE THIS ON OUTPUT THE RENAME WORKED'
!!      close(10)
!!      ! rename scratch file
!!      ierr=system_rename('_scratch_file_','_renamed_scratch_file_')
!!      if(ierr.ne.0)then
!!         write(*,*)'ERROR RENAMING FILE ',ierr
!!      endif
!!      ! read renamed file
!!      open(unit=11,file='_renamed_scratch_file_',status='old')
!!      INFINITE: do
!!         read(11,'(a)',iostat=ios)string
!!         if(ios.ne.0)exit INFINITE
!!         write(*,'(a)')trim(string)
!!      enddo INFINITE
!!      close(unit=11)
!!
!!      ! clean up
!!      ierr=system_remove('_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      ierr=system_remove('_renamed_scratch_file_')
!!      write(*,'(a,i0)') 'should be zero ',ierr
!!
!!      end program demo_system_rename
!!
!!   Expected output:
!!
!!    > should not be zero -1
!!    > *demo_system_rename*: No such file or directory
!!    > should not be zero -1
!!    > *demo_system_rename*: No such file or directory
!!    > Test by renaming "_scratch_file_" to "_renamed_scratch_file_"
!!    > IF YOU SEE THIS ON OUTPUT THE RENAME WORKED
!!    > should not be zero -1
!!    > should be zero 0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_rename(input,output) result(ierr)

character(len=*),parameter::ident_17="@(#)M_system::system_rename(3f): call rename(3c) to change filename"

character(*),intent(in)    :: input,output
integer                    :: ierr
interface
   function c_rename(c_input,c_output) bind(c,name="rename") result(c_err)
      import c_char, c_int
      character(kind=c_char),intent(in) :: c_input(*)
      character(kind=c_char),intent(in) :: c_output(*)
      integer(c_int)                    :: c_err
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr= c_rename(str2_carr(trim(input)),str2_carr(trim(output)))
end function system_rename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_chmod(3f) - [M_system] call chmod(3c) to change permission mode of a file relative to directory file descriptor
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function system_chmod(filename,mode) result(ierr)
!!
!!       character(len=*),intent(in)  :: filename
!!       integer,value,intent(in)     :: mode
!!       integer                      :: ierr
!!
!!##DESCRIPTION
!!        The system_chmod(3f) function shall change UID, _ISGID, S_ISVTX, and the
!!        file permission bits of the file named by the pathname pointed
!!        to by the path argument to the corresponding bits in the mode
!!        argument. The application shall ensure that the effective user
!!        ID of the process matches the owner of the file or the process
!!        has appropriate privileges in order to do this.
!!
!!        S_ISUID, S_ISGID, S_ISVTX, and the file permission bits are
!!        described in <sys/stat.h>.
!!
!!        If the calling process does not have appropriate privileges,
!!        and if the group ID of the file does not match the effective
!!        group ID or one of the supplementary group IDs and if the file
!!        is a regular file, bit S_ISGID (set-group-ID on execution) in the
!!        file's mode shall be cleared upon successful return from chmod().
!!
!!        Additional implementation-defined restrictions may cause the
!!        S_ISUID and S_ISGID bits in mode to be ignored.
!!
!!        Upon successful completion, system_chmod() marks for update the
!!        last file status change timestamp of the file.
!!
!!        Values for flag are constructed by a bitwise-inclusive OR of
!!        flags from the following list, defined in <fcntl.h>:
!!
!!        AT_SYMLINK_NOFOLLOW
!!              If path names a symbolic link, then the mode of the symbolic
!!              link is changed.
!!
!!
!!##RETURN VALUE
!!        Upon successful completion, system_chmod(3f) returns 0.
!!        Otherwise, it returns -1 and sets errno to indicate the error. If
!!        -1 is returned, no change to the file mode occurs.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_system_chmod
!!    use M_system, only : system_chmod
!!    use M_system, only : system_stat
!!    use M_system, only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
!!    use M_system, only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    use,intrinsic     :: iso_fortran_env, only : int64
!!    implicit none
!!    integer         :: ierr
!!    integer         :: status
!!    integer(kind=int64) :: buffer(13)
!!       !Setting Read Permissions for User, Group, and Others
!!       ! The following example sets read permissions for the owner, group, and others.
!!       open(file='_test1',unit=10)
!!       write(10,*)'TEST FILE 1'
!!       close(unit=10)
!!       ierr=system_chmod('_test1', IANY([R_USR,R_GRP,R_OTH]))
!!
!!       !Setting Read, Write, and Execute Permissions for the Owner Only
!!       ! The following example sets read, write, and execute permissions for the owner, and no permissions for group and others.
!!       open(file='_test2',unit=10)
!!       write(10,*)'TEST FILE 2'
!!       close(unit=10)
!!       ierr=system_chmod('_test2', RWX_U)
!!
!!       !Setting Different Permissions for Owner, Group, and Other
!!       ! The following example sets owner permissions for CHANGEFILE to read, write, and execute, group permissions to read and
!!       ! execute, and other permissions to read.
!!       open(file='_test3',unit=10)
!!       write(10,*)'TEST FILE 3'
!!       close(unit=10)
!!       ierr=system_chmod('_test3', IANY([RWX_U,R_GRP,X_GRP,R_OTH]));
!!
!!       !Setting and Checking File Permissions
!!       ! The following example sets the file permission bits for a file named /home/cnd/mod1, then calls the stat() function to
!!       ! verify the permissions.
!!
!!       ierr=system_chmod("home/cnd/mod1", IANY([RWX_U,RWX_G,R_OTH,W_OTH]))
!!       call system_stat("home/cnd/mod1", buffer,status)
!!
!!       ! In order to ensure that the S_ISUID and S_ISGID bits are set, an application requiring this should use stat() after a
!!       ! successful chmod() to verify this.
!!
!!       !    Any files currently open could possibly become invalid if the mode
!!       !    of the file is changed to a value which would deny access to
!!       !    that process.
!!
!!    end program demo_system_chmod
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_chmod(filename,mode) result(ierr)
   character(len=*),intent(in)  :: filename
   integer,value,intent(in)     :: mode
   integer                      :: ierr
   interface
      function c_chmod(c_filename,c_mode) bind(c,name="chmod") result(c_err)
         import c_char,c_int
         character(kind=c_char),intent(in) :: c_filename(*)
         integer(c_int),value,intent(in)   :: c_mode
         integer(c_int)                    :: c_err
      end function
   end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=c_chmod(str2_carr(trim(filename)),int(mode,kind(0_c_int)))
end function system_chmod
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_getcwd(3f) - [M_system] call getcwd(3c) to get the pathname of the current working directory
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine system_getcwd(output,ierr)
!!
!!        character(len=:),allocatable,intent(out) :: output
!!        integer,intent(out)                      :: ierr
!!##DESCRIPTION
!!        system_getcwd(3f) calls the C routine getcwd(3c) to obtain the absolute pathname of the current working directory.
!!
!!##RETURN VALUE
!!        OUTPUT   The absolute pathname of the current working directory
!!                 The pathname shall contain no components that are dot or dot-dot,
!!                 or are symbolic links.
!!        IERR     is not zero if an error occurs.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_system_getcwd
!!      use M_system, only : system_getcwd
!!      implicit none
!!      character(len=:),allocatable :: dirname
!!      integer                      :: ierr
!!      call system_getcwd(dirname,ierr)
!!      if(ierr.eq.0)then
!!         write(*,*)'CURRENT DIRECTORY ',trim(dirname)
!!      else
!!         write(*,*)'ERROR OBTAINING CURRENT DIRECTORY NAME'
!!      endif
!!      end program demo_system_getcwd
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_getcwd(output,ierr)

character(len=*),parameter::ident_18="@(#)M_system::system_getcwd(3f):call getcwd(3c) to get pathname of current working directory"

character(len=:),allocatable,intent(out) :: output
integer,intent(out)                      :: ierr
integer(kind=c_long),parameter           :: length=4097_c_long
character(kind=c_char,len=1)             :: buffer(length)
type(c_ptr)                              :: buffer2
interface
   function c_getcwd(buffer,size) bind(c,name="getcwd") result(buffer_result)
      import c_char, c_size_t, c_ptr
      character(kind=c_char) ,intent(out) :: buffer(*)
      integer(c_size_t),value,intent(in)  :: size
      type(c_ptr)                         :: buffer_result
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   buffer=' '
   buffer2=c_getcwd(buffer,length)
   if(.not.c_associated(buffer2))then
      output=''
      ierr=-1
   else
      output=trim(arr2str(buffer))
      ierr=0
   endif
end subroutine system_getcwd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_rmdir(3f) - [M_system] call rmdir(3c) to remove empty directories
!!       (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function system_rmdir(dirname) result(err)
!!
!!     character(*),intent(in) :: dirname
!!     integer(c_int) :: err
!!
!!##DESCRIPTION
!!        DIRECTORY  The name of a directory to remove if it is empty
!!        err        zero (0) if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_rmdir
!!    use M_system, only : system_perror
!!    use M_system, only : system_rmdir, system_mkdir
!!    use M_system, only : RWX_U
!!    implicit none
!!    integer :: ierr
!!    write(*,*)'BEFORE TRY TO CREATE _scratch/'
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO CREATE _scratch/'
!!    ierr=system_mkdir('_scratch',RWX_U)
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO REMOVE _scratch/'
!!    ierr=system_rmdir('_scratch')
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO REMOVE _scratch when it should be gone/'
!!    ierr=system_rmdir('_scratch')
!!    call system_perror('*test of system_rmdir*')
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    end program demo_system_rmdir
!!
!!   Expected output:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_rmdir(dirname) result(err)

character(len=*),parameter::ident_19="@(#)M_system::system_rmdir(3f): call rmdir(3c) to remove empty directory"

character(*),intent(in) :: dirname
integer(c_int) :: err

interface
   function c_rmdir(c_path) bind(c,name="rmdir") result(c_err)
      import c_char,c_int
      character(kind=c_char,len=1),intent(in) :: c_path(*)
      integer(c_int)                          :: c_err
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   err= c_rmdir(str2_carr(trim(dirname)))
   if(err.ne.0) err=system_errno()
end function system_rmdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_mkfifo(3f)  - [M_system] make a FIFO special file relative to directory file descriptor
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_mkfifo(pathname,mode) result(ierr)
!!
!!    character(len=*),intent(in)       :: pathname
!!    integer,intent(in)                :: mode
!!    integer :: ierr
!!
!!##DESCRIPTION
!!    A regular pipe can only connect two related processes. It is created by
!!    a process and will vanish when the last process closes it.
!!
!!    A named pipe, also called a FIFO for its behavior, can be used to connect
!!    two unrelated processes and exists independently of the processes;
!!    meaning it can exist even if no one is using it. A FIFO is created using
!!    the mkfifo() library function.
!!
!!    The mkfifo() function creates a new FIFO special file named by the
!!    pathname.
!!
!!    The file permission bits of the new FIFO are initialized from mode.
!!
!!    The file permission bits of the mode argument are modified by the
!!    process' file creation mask.
!!
!!    When bits in mode other than the file permission bits are set, the
!!    effect is implementation-defined.
!!
!!    If path names a symbolic link, mkfifo() shall fail and set errno to
!!    [EEXIST].
!!
!!    The FIFO's user ID will be set to the process' effective user ID.
!!
!!    The FIFO's group ID shall be set to the group ID of the parent
!!    directory or to the effective group ID of the process.
!!
!!    Implementations shall provide a way to initialize the FIFO's group
!!    ID to the group ID of the parent directory.
!!
!!    Implementations may, but need not, provide an implementation-defined
!!    way to initialize the FIFO's group ID to the effective group ID of
!!    the calling process.
!!
!!    Upon successful completion, mkfifo() shall mark for update the
!!    last data access, last data modification, and last file status change
!!    timestamps of the file.
!!
!!    Also, the last data modification and last file status change
!!    timestamps of the directory that contains the new entry shall be
!!    marked for update.
!!
!!    Predefined variables are typically used to set permission modes.
!!
!!    You can bytewise-OR together these variables to to create the most
!!    common permissions mode:
!!
!!     User:    R_USR  (read),  W_USR  (write),  X_USR(execute)
!!     Group:   R_GRP  (read),  W_GRP  (write),  X_GRP(execute)
!!     Others:  R_OTH  (read),  W_OTH  (write),  X_OTH(execute)
!!
!!    Additionally, some shortcuts are provided (basically a bitwise-OR
!!    combination of the above):
!!
!!      Read + Write + Execute: RWX_U (User), RWX_G (Group), RWX_O (Others)
!!      DEFFILEMODE: Equivalent of 0666 =rw-rw-rw-
!!      ACCESSPERMS: Equivalent of 0777 = rwxrwxrwx
!!
!!    Therefore, to give only the user rwx (read+write+execute) rights whereas
!!    group members and others may not do anything, you can use any of the
!!    following mkfifo() calls equivalently:
!!
!!      ierr= mkfifo("myfile", IANY([R_USR, W_USR, X_USR]));
!!      ierr= mkfifo("myfile", RWX_U);
!!
!!    In order to give anyone any rights (mode 0777 = rwxrwxrwx), you can
!!    use any of the following calls equivalently:
!!
!!      ierr= mkfifo("myfile",IANY([R_USR,W_USR,X_USR,R_GRP,W_GRP,X_GRP,R_OTH,W_OTH,X_OTH]));
!!      ierr= mkfifo("myfile",IANY([RWX_U,RWX_G,RWX_O]));
!!      ierr= mkfifo("myfile",ACCESSPERMS);
!!##RETURN VALUE
!!    Upon successful completion, return 0.
!!    Otherwise, return -1 and set errno to indicate the error.
!!    If -1 is returned, no FIFO is created.
!!
!!##EXAMPLES
!!
!!   The following example shows how to create a FIFO file named
!!   /home/cnd/mod_done, with read/write permissions for owner, and
!!   with read permissions for group and others.
!!
!!    program demo_system_mkfifo
!!    use M_system, only : system_mkfifo, system_perror
!!    use M_system, only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
!!    use M_system, only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    implicit none
!!       integer :: status
!!       status = system_mkfifo("/tmp/buffer", IANY([W_USR, R_USR, R_GRP, R_OTH]))
!!       if(status.ne.0)then
!!          call system_perror('*mkfifo* error:')
!!       endif
!!    end program demo_system_mkfifo
!!
!!   Now some other process (or this one) can read from /tmp/buffer while this program
!!   is running or after, consuming the data as it is read.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_mkfifo(pathname,mode) result(err)

character(len=*),parameter::ident_20="@(#)M_system::system_mkfifo(3f): call mkfifo(3c) to create a new FIFO special file"

character(len=*),intent(in)       :: pathname
integer,intent(in)                :: mode
   integer                        :: c_mode
   integer                        :: err

interface
   function c_mkfifo(c_path,c_mode) bind(c,name="mkfifo") result(c_err)
      import c_char, c_int
      character(len=1,kind=c_char),intent(in) :: c_path(*)
      integer(c_int),intent(in),value         :: c_mode
      integer(c_int)                          :: c_err
   end function c_mkfifo
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   c_mode=mode
   err= c_mkfifo(str2_carr(trim(pathname)),c_mode)
end function system_mkfifo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_mkdir(3f) - [M_system] call mkdir(3c) to create a new directory
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!    Predefined variables are typically used to set permission modes.
!!    You can bytewise-OR together these variables to to create the most common
!!    permissions mode:
!!
!!     User:    R_USR  (read),  W_USR  (write),  X_USR(execute)
!!     Group:   R_GRP  (read),  W_GRP  (write),  X_GRP(execute)
!!     Others:  R_OTH  (read),  W_OTH  (write),  X_OTH(execute)
!!
!!    Additionally, some shortcuts are provided (basically a bitwise-OR combination of the above):
!!
!!      Read + Write + Execute: RWX_U (User), RWX_G (Group), RWX_O (Others)
!!      DEFFILEMODE: Equivalent of 0666 =rw-rw-rw-
!!      ACCESSPERMS: Equivalent of 0777 = rwxrwxrwx
!!
!!    Therefore, to give only the user rwx (read+write+execute) rights whereas
!!    group members and others may not do anything, you can use any of the
!!    following mkdir() calls equivalently:
!!
!!      ierr= mkdir("mydir", IANY([R_USR, W_USR, X_USR]));
!!      ierr= mkdir("mydir", RWX_U);
!!
!!    In order to give anyone any rights (mode 0777 = rwxrwxrwx), you can
!!    use any of the following calls equivalently:
!!
!!      ierr= mkdir("mydir",IANY([R_USR,W_USR,X_USR,R_GRP,W_GRP,X_GRP,R_OTH,W_OTH,X_OTH]));
!!      ierr= mkdir("mydir",IANY([RWX_U,RWX_G,RWX_O]));
!!      ierr= mkdir("mydir",ACCESSPERMS);
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_mkdir
!!    use M_system, only : system_perror
!!    use M_system, only : system_mkdir
!!    use M_system, only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
!!    use M_system, only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    implicit none
!!    integer :: ierr
!!    ierr=system_mkdir('_scratch',IANY([R_USR,W_USR,X_USR]))
!!    end program demo_system_mkdir
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_mkdir(dirname,mode) result(ierr)

character(len=*),parameter::ident_21="@(#)M_system::system_mkdir(3f): call mkdir(3c) to create empty directory"

character(len=*),intent(in)       :: dirname
integer,intent(in)                :: mode
   integer                        :: c_mode
   integer(kind=c_int)            :: err
   integer                        :: ierr

interface
   function c_mkdir(c_path,c_mode) bind(c,name="mkdir") result(c_err)
      import c_char, c_int
      character(len=1,kind=c_char),intent(in) :: c_path(*)
      integer(c_int),intent(in),value         :: c_mode
      integer(c_int)                          :: c_err
   end function c_mkdir
end interface
interface
    subroutine my_mkdir(string,c_mode,c_err) bind(C, name="my_mkdir")
      use iso_c_binding, only : c_char, c_int
      character(kind=c_char) :: string(*)
      integer(c_int),intent(in),value         :: c_mode
      integer(c_int)                          :: c_err
    end subroutine my_mkdir
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   c_mode=mode
   if(index(dirname,'/').ne.0)then
      call my_mkdir(str2_carr(trim(dirname)),c_mode,err)
   else
      err= c_mkdir(str2_carr(trim(dirname)),c_mode)
   endif
   ierr=err                                          ! c_int to default integer kind
end function system_mkdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_opendir(3f) - [M_system] open directory stream by calling opendir(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine system_opendir(dirname,dir,ierr)
!!
!!    character(len=*), intent(in) :: dirname
!!    type(c_ptr)                  :: dir
!!    integer,intent(out)          :: ierr
!!
!!##DESCRIPTION
!!        The system_opendir(3f) procedure opens a directory stream
!!        corresponding to the directory named by the dirname argument.
!!        The directory stream is positioned at the first entry.
!!
!!##RETURN VALUE
!!        Upon successful completion, a pointer to a C dir type is returned.
!!        Otherwise, these functions shall return a null pointer and set
!!        IERR to indicate the error.
!!
!!##ERRORS
!!
!!        An error corresponds to a condition described in opendir(3c):
!!
!!        EACCES    Search permission is denied for the component of the
!!                  path prefix of dirname or read permission is denied
!!                  for dirname.
!!
!!        ELOOP     A loop exists in symbolic links encountered during
!!                  resolution of the dirname argument.
!!
!!        ENAMETOOLONG  The length of a component of a pathname is longer than {NAME_MAX}.
!!
!!        ENOENT        A component of dirname does not name an existing directory or dirname is an empty string.
!!
!!        ENOTDIR       A component of dirname names an existing file that is neither a directory nor a symbolic link to a directory.
!!
!!        ELOOP         More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the dirname argument.
!!
!!        EMFILE        All file descriptors available to the process are currently open.
!!
!!        ENAMETOOLONG  The length of a pathname exceeds {PATH_MAX},
!!                      or pathname resolution of a symbolic link produced an intermediate
!!                      result with a length that exceeds {PATH_MAX}.
!!
!!        ENFILE        Too many files are currently open in the system.
!!
!!##APPLICATION USAGE
!!        The opendir() function should be used in conjunction with readdir(), closedir(), and rewinddir() to examine the contents
!!        of the directory (see the EXAMPLES section in readdir()). This method is recommended for portability.
!!##OPTIONS
!!       dirname name of directory to open a directory stream for
!!##RETURNS
!!       dir   pointer to directory stream. If an
!!             error occurred, it will not be associated.
!!       ierr  0 indicates no error occurred
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_opendir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_closedir
!!    use iso_c_binding
!!    implicit none
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    if(ierr.eq.0)then
!!       !--- read directory stream
!!       do
!!          call system_readdir(dir,filename,ierr)
!!          if(filename.eq.' ')exit
!!          write(*,*)filename
!!       enddo
!!    endif
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!    end program demo_system_opendir
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_opendir(dirname,dir,ierr)
character(len=*), intent(in) :: dirname
type(c_ptr)                  :: dir
integer,intent(out)          :: ierr

interface
   function c_opendir(c_dirname) bind(c,name="opendir") result(c_dir)
      import c_char, c_int, c_ptr
      character(kind=c_char),intent(in) :: c_dirname(*)
      type(c_ptr)                       :: c_dir
   end function c_opendir
end interface

   ierr=0
   dir = c_opendir(str2_carr(trim(dirname)))
   if(.not.c_associated(dir)) then
      write(*,'(a)')'*system_opendir* Error opening '//trim(dirname)
      ierr=-1
   endif

end subroutine system_opendir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_readdir(3f) - [M_system] read a directory using readdir(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine system_readdir(dir,filename,ierr)
!!
!!  type(c_ptr),value                         :: dir
!!  character(len=:),intent(out),allocatable  :: filename
!!  integer,intent(out)                       :: ierr
!!
!!##DESCRIPTION
!!
!!    system_readdir(3f) returns the name of the directory entry at the
!!    current position in the directory stream specified by the argument
!!    DIR, and positions the directory stream at the next entry. It returns
!!    a null name upon reaching the end of the directory stream.
!!
!!##OPTIONS
!!
!!    DIR       A pointer to the directory opened by system_opendir(3f).
!!
!!##RETURNS
!!
!!    FILENAME  the name of the directory entry at the current position in
!!              the directory stream specified by the argument DIR, and
!!              positions the directory stream at the next entry.
!!
!!              The readdir() function does not return directory entries
!!              containing empty names. If entries for dot or dot-dot exist,
!!              one entry is returned for dot and one entry is returned
!!              for dot-dot.
!!
!!              The entry is marked for update of the last data access
!!              timestamp each time it is read.
!!
!!              reaching the end of the directory stream, the name is a blank name.
!!
!!    IERR      If IERR is set to non-zero on return, an error occurred.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_readdir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_rewinddir,system_closedir
!!    use iso_c_binding
!!    implicit none
!!
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: i, ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    if(ierr.eq.0)then
!!       !--- read directory stream twice
!!       do i=1,2
!!          write(*,'(a,i0)')'PASS ',i
!!          do
!!             call system_readdir(dir,filename,ierr)
!!             if(filename.eq.' ')exit
!!             write(*,*)filename
!!          enddo
!!          call system_rewinddir(dir)
!!       enddo
!!    endif
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!
!!    end program demo_system_readdir
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_readdir(dir,filename,ierr)
type(c_ptr),value                         :: dir
character(len=:),intent(out),allocatable  :: filename
integer,intent(out)                       :: ierr
integer(kind=c_int)                       :: ierr_local

   character(kind=c_char,len=1)           :: buf(4097)

interface
   subroutine c_readdir(c_dir, c_filename,c_ierr) bind (C,NAME='my_readdir')
      import c_char, c_int, c_ptr
      type(c_ptr),value                   :: c_dir
      character(kind=c_char)              :: c_filename(*)
      integer(kind=c_int)                 :: c_ierr
   end subroutine c_readdir
end interface

   buf=' '
   ierr_local=0
   call c_readdir(dir,buf,ierr_local)
   filename=trim(arr2str(buf))
   ierr=ierr_local

end subroutine system_readdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_rewinddir(3f) - [M_system] call rewinddir(3c) to rewind directory stream
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_rewinddir(dir)
!!
!!     type(c_ptr),value :: dir
!!
!!##DESCRIPTION
!!     Return to pointer to the beginning of the list for a currently open directory list.
!!
!!##OPTIONS
!!     DIR  A C_pointer assumed to have been allocated by a call to SYSTEM_OPENDIR(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_rewinddir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_rewinddir,system_closedir
!!    use iso_c_binding
!!    implicit none
!!
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: i, ierr
!!    !>>> open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !>>> read directory stream twice
!!    do i=1,2
!!       write(*,'(a,i0)')'PASS ',i
!!       do
!!          call system_readdir(dir,filename,ierr)
!!          if(filename.eq.' ')exit
!!          write(*,*)filename
!!       enddo
!!       !>>> rewind directory stream
!!       call system_rewinddir(dir)
!!    enddo
!!    !>>> close directory stream
!!    call system_closedir(dir,ierr)
!!
!!    end program demo_system_rewinddir
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_rewinddir(dir)
type(c_ptr),value            :: dir

interface
   subroutine c_rewinddir(c_dir) bind(c,name="rewinddir")
      import c_char, c_int, c_ptr
      type(c_ptr),value :: c_dir
   end subroutine c_rewinddir
end interface

   call c_rewinddir(dir)

end subroutine system_rewinddir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_closedir(3f) - [M_system] close a directory stream by calling closedir(3c)
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        subroutine system_closedir(dir,ierr)
!!
!!         type(c_ptr)         :: dir
!!         integer,intent(out) :: ierr
!!##DESCRIPTION
!!        The SYSTEM_CLOSEDIR(3f) function closes the directory stream referred to by the argument DIR.
!!        Upon return, the value of DIR may no longer point to an accessible object.
!!##OPTIONS
!!        dir     directory stream pointer opened by SYSTEM_OPENDIR(3f).
!!        ierr    Upon successful completion, SYSTEM_CLOSEDIR(3f) returns 0;
!!                otherwise, an error has occurred.
!!##ERRORS
!!        system_closedir(3f) may fail if:
!!
!!        EBADF    The dirp argument does not refer to an open directory stream.
!!        EINTR    The closedir() function was interrupted by a signal.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_system_closedir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_closedir, system_rewinddir
!!    use iso_c_binding, only : c_ptr
!!    implicit none
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !--- read directory stream
!!    do
!!       call system_readdir(dir,filename,ierr)
!!       if(filename.eq.' ')exit
!!       write(*,*)filename
!!    enddo
!!    call system_rewinddir(dir)
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!    end program demo_system_closedir
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_closedir(dir,ierr)
use iso_c_binding
type(c_ptr),value            :: dir
integer,intent(out),optional :: ierr
   integer                   :: ierr_local

interface
   function c_closedir(c_dir) bind(c,name="closedir") result(c_err)
      import c_char, c_int, c_ptr
      type(c_ptr),value      :: c_dir
      integer(kind=c_int)    :: c_err
   end function c_closedir
end interface

    ierr_local = c_closedir(dir)
    if(present(ierr))then
       ierr=ierr_local
    else
       if(ierr_local /= 0) then
          print *, "*system_closedir* error", ierr_local
          stop 3
       endif
    endif

end subroutine system_closedir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_putenv(3f) - [M_system:ENVIRONMENT] set environment variable from Fortran by calling putenv(3c)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine system_putenv(string, err)
!!
!!     character(len=*),intent(in)    :: string
!!     integer, optional, intent(out) :: err
!!
!!##DESCRIPTION
!!    The system_putenv() function adds or changes the value of environment variables.
!!
!!##OPTIONS
!!    string  string of format "NAME=value".
!!            If name does not already exist in the environment, then string is added to the environment.
!!            If name does exist, then the value of name in the environment is changed to value.
!!            The string passed to putenv(3c) becomes part of the environment,
!!            so this routine creates a string each time it is called that increases the amount of
!!            memory the program uses.
!!    err     The system_putenv() function returns zero on success, or nonzero if an error occurs.
!!            A non-zero error usually indicates sufficient memory does not exist to store the
!!            variable.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!     program demo_system_putenv
!!     use M_system, only : system_putenv
!!     use iso_c_binding
!!     implicit none
!!     integer :: ierr
!!        !
!!        write(*,'(a)')'no environment variables containing "GRU":'
!!        call execute_command_line('env|grep GRU')
!!        !
!!        call system_putenv('GRU=this is the value',ierr)
!!        write(*,'(a,i0)')'now "GRU" should be defined: ',ierr
!!        call execute_command_line('env|grep GRU')
!!        !
!!        call system_putenv('GRU2=this is the second value',ierr)
!!        write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined: ',ierr
!!        call execute_command_line('env|grep GRU')
!!        !
!!        call system_putenv('GRU2',ierr)
!!        call system_putenv('GRU',ierr)
!!        write(*,'(a,i0)')'should be gone, varies with different putenv(3c): ',ierr
!!        call execute_command_line('env|grep GRU')
!!        write(*,'(a)')'system_unsetenv(3f) is a better way to remove variables'
!!        !
!!     end program demo_system_putenv
!!
!!   Results:
!!
!!    no environment variables containing "GRU":
!!    now "GRU" should be defined: 0
!!    GRU=this is the value
!!    now "GRU" and "GRU2" should be defined: 0
!!    GRU2=this is the second value
!!    GRU=this is the value
!!    should be gone, varies with different putenv(3c): 0
!!    system_unsetenv(3f) is a better way to remove variables
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_putenv(string, err)

character(len=*),parameter::ident_22="@(#)M_system::system_putenv(3f): call putenv(3c)"

interface
   integer(kind=c_int)  function c_putenv(c_string) bind(C,name="putenv")
      import c_int, c_char
      character(kind=c_char)   :: c_string(*)
   end function
end interface

character(len=*),intent(in)    :: string
integer, optional, intent(out) :: err
   integer                     :: loc_err
   integer                     :: i

   ! PUTENV actually adds the data to the environment so the string passed should be saved or will vanish on exit
   character(len=1,kind=c_char),save, pointer :: memleak(:)

   allocate(memleak(len(string)+1))
   do i=1,len(string)
      memleak(i)=string(i:i)
   enddo
   memleak(len(string)+1)=c_null_char

   loc_err =  c_putenv(memleak)
   if (present(err)) err = loc_err

end subroutine system_putenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getenv(3f) - [M_system:ENVIRONMENT] get environment variable from Fortran by calling get_environment_variable(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function system_getenv(name)
!!
!!     character(len=:),allocatable   :: system_getenv
!!     character(len=*),intent(in)    :: name
!!
!!##DESCRIPTION
!!    The system_getenv() function gets the value of an environment variable.
!!
!!##OPTIONS
!!    name    Return the value of the specified environment variable or
!!            blank if the variable is not defined.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_system_getenv
!!    use M_system, only : system_getenv
!!    implicit none
!!    integer :: ierr
!!
!!       write(*,'("USER     : ",a)')system_getenv('USER')
!!       write(*,'("LOGNAME  : ",a)')system_getenv('LOGNAME')
!!       write(*,'("USERNAME : ",a)')system_getenv('USERNAME')
!!
!!    end program demo_system_getenv
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_getenv(name) result(var)

character(len=*),parameter::ident_23="@(#)M_system::system_getenv(3f): call get_environment_variable(3f)"

character(len=*),intent(in)  :: name
integer                      :: howbig
character(len=:),allocatable :: var

   call get_environment_variable(name, length=howbig)  ! get length required to hold value
   if(howbig.ne.0)then
      allocate(character(len=howbig) :: var)           ! make string to hold value of sufficient size
      var(:)=' '
      call get_environment_variable(name, var)         ! get value
   else
      var=''
   endif

end function system_getenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_environment_variable(3f) - [M_system:ENVIRONMENT] call setenv(3c) to set environment variable
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine set_environment_variable(NAME, VALUE, STATUS)
!!
!!    character(len=*)               :: NAME
!!    character(len=*)               :: VALUE
!!    integer, optional, intent(out) :: STATUS
!!
!!##DESCRIPTION
!!    The set_environment_variable() procedure adds or changes the value of environment variables.
!!
!!##OPTIONS
!!    NAME    If name does not already exist in the environment, then string is added to the environment.
!!            If name does exist, then the value of name in the environment is changed to value.
!!    VALUE   Value to assign to environment variable NAME
!!    STATUS  returns zero on success, or nonzero if an error occurs.
!!            A non-zero error usually indicates sufficient memory does not exist to store the
!!            variable.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_set_environment_variable
!!    use M_system, only : set_environment_variable
!!    use iso_c_binding
!!    implicit none
!!    integer :: ierr
!!       !!
!!       write(*,'(a)')'no environment variables containing "GRU":'
!!       call execute_command_line('env|grep GRU')
!!       !!
!!       call set_environment_variable('GRU','this is the value',ierr)
!!       write(*,'(a,i0)')'now "GRU" should be defined, status=',ierr
!!       call execute_command_line('env|grep GRU')
!!       !!
!!       call set_environment_variable('GRU2','this is the second value',ierr)
!!       write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined, status =',ierr
!!       !!
!!       call execute_command_line('env|grep GRU')
!!    end program demo_set_environment_variable
!!
!!   Results:
!!
!!    no environment variables containing "GRU":
!!    now "GRU" should be defined, status=0
!!    GRU=this is the value
!!    now "GRU" and "GRU2" should be defined, status =0
!!    GRU2=this is the second value
!!    GRU=this is the value
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine set_environment_variable(NAME, VALUE, STATUS)

character(len=*),parameter::ident_24="@(#)M_system::set_environment_variable(3f): call setenv(3c) to set environment variable"

   character(len=*)               :: NAME
   character(len=*)               :: VALUE
   integer, optional, intent(out) :: STATUS
   integer                        :: loc_err

interface
   integer(kind=c_int) function c_setenv(c_name,c_VALUE) bind(C,NAME="setenv")
      import c_int, c_char
      character(kind=c_char)   :: c_name(*)
      character(kind=c_char)   :: c_VALUE(*)
   end function
end interface

   loc_err =  c_setenv(str2_carr(trim(NAME)),str2_carr(VALUE))
   if (present(STATUS)) STATUS = loc_err
end subroutine set_environment_variable
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_clearenv(3f) - [M_system:ENVIRONMENT] clear environment by calling clearenv(3c)
!!    (LICENSE:PD)
!!
!!
!!##SYNOPSIS
!!
!!    subroutine system_clearenv(ierr)
!!
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!    The clearenv() procedure clears the environment of all name-value
!!    pairs.  Typically used in security-conscious applications or ones where
!!    configuration control requires ensuring specific variables are set.
!!
!!##RETURN VALUES
!!    ierr  returns zero on success, and a nonzero value on failure. Optional.
!!          If not present and an error occurs the program stops.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!      program demo_system_clearenv
!!      use M_system, only : system_clearenv
!!      implicit none
!!      ! environment before clearing
!!      call execute_command_line('env|wc')
!!      ! environment after clearing (not necessarily blank!!)
!!      call system_clearenv()
!!      call execute_command_line('env')
!!      end program demo_system_clearenv
!!
!!   Typical output:
!!
!!      89     153    7427
!!      PWD=/home/urbanjs/V600
!!      SHLVL=1
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_clearenv(ierr)
!  emulating because not available on some platforms

character(len=*),parameter::ident_25="@(#)M_system::system_clearenv(3f): emulate clearenv(3c) to clear environment"

integer,intent(out),optional    :: ierr
   character(len=:),allocatable :: string
   integer                      :: ierr_local1, ierr_local2
   ierr_local2=0
   INFINITE: do
      call system_initenv()                     ! important -- changing table causes undefined behavior so reset after each unsetenv
      string=system_readenv()                                           ! get first name=value pair
      if(string.eq.'') exit INFINITE
      call system_unsetenv(string(1:index(string,'=')-1) ,ierr_local1)  ! remove first name=value pair
      if(ierr_local1.ne.0)ierr_local2=ierr_local1
   enddo INFINITE
   if(present(ierr))then
      ierr=ierr_local2
   elseif(ierr_local2.ne.0)then                                         ! if error occurs and not being returned, stop
      write(*,*)'*system_clearenv* error=',ierr_local2
      stop
   endif
end subroutine system_clearenv
!--subroutine system_clearenv(ierr)
!--! clearenv(3c) not available on some systems I tried
!--! Found reference that if it is unavailable the assignment "environ = NULL;" will probably do but emulating instead
!--$@ (#)M_system::system_clearenv(3f): call clearenv(3c) to clear environment"
!--integer,intent(out),optional :: ierr
!--   integer                   :: ierr_local
!--
!--interface
!--   integer(kind=c_int) function c_clearenv() bind(C,NAME="clearenv")
!--   import c_int
!--   end function
!--end interface
!--
!--   ierr_local = c_clearenv()
!--   if(present(ierr))then
!--      ierr=ierr_local
!--   elseif(ierr_local.ne.0)then ! if error occurs and not being returned, stop
!--      write(*,*)'*system_clearenv* error=',ierr_local
!--      stop
!--   endif
!--
!--end subroutine system_clearenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_unsetenv(3f) - [M_system:ENVIRONMENT] delete an environment variable by calling unsetenv(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine system_unsetenv(name,ierr)
!!
!!    character(len=*),intent(in)  :: name
!!    integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!
!!    The system_unsetenv(3f) function deletes the variable name from the
!!    environment.
!!
!!##OPTIONS
!!    name   name of variable to delete.
!!           If name does not exist in the environment, then the
!!           function succeeds, and the environment is unchanged.
!!
!!    ierr   The system_unsetenv(3f) function returns zero on success, or -1 on error.
!!           name is NULL, points to a string of length 0, or contains an '=' character.
!!           Insufficient memory to add a new variable to the environment.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_system_unsetenv
!!      use M_system, only : system_unsetenv, system_putenv
!!      implicit none
!!      call system_putenv('GRU=this is the value')
!!      write(*,'(a)')'The variable GRU should be set'
!!      call execute_command_line('env|grep GRU')
!!      call system_unsetenv('GRU')
!!      write(*,'(a)')'The variable GRU should not be set'
!!      call execute_command_line('env|grep GRU')
!!      end program demo_system_unsetenv
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_unsetenv(name,ierr)

character(len=*),parameter::ident_26="@(#)M_system::system_unsetenv(3f): call unsetenv(3c) to remove variable from environment"

character(len=*),intent(in)  :: name
integer,intent(out),optional :: ierr
   integer                   :: ierr_local

! int unsetenv(void)
interface
   integer(kind=c_int) function c_unsetenv(c_name) bind(C,NAME="unsetenv")
   import c_int, c_char
   character(len=1,kind=c_char) :: c_name(*)
   end function
end interface

   ierr_local =  c_unsetenv(str2_carr(trim(NAME)))

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then ! if error occurs and not being returned, stop
      write(*,*)'*system_unsetenv* error=',ierr_local
      stop
   endif

end subroutine system_unsetenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_readenv(3f) - [M_system:ENVIRONMENT] step thru and read environment table
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       function system_readenv() result(string)
!!
!!        character(len=:),allocatable  :: string
!!##DESCRIPTION
!!    A simple interface allows reading the environment variable table of the process. Call
!!    system_initenv(3f) to initialize reading the environment table, then call system_readenv(3f) can
!!    be called until a blank line is returned. If more than one thread
!!    reads the environment or the environment is changed while being read the results are undefined.
!!##OPTIONS
!!    string  the string returned from the environment of the form "NAME=VALUE"
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_readenv
!!    use M_system, only : system_initenv, system_readenv
!!    character(len=:),allocatable :: string
!!       call system_initenv()
!!       do
!!          string=system_readenv()
!!          if(string.eq.'')then
!!             exit
!!          else
!!             write(*,'(a)')string
!!          endif
!!       enddo
!!    end program demo_system_readenv
!!
!!   Sample results:
!!
!!    USERDOMAIN_ROAMINGPROFILE=buzz
!!    HOMEPATH=\Users\JSU
!!    APPDATA=C:\Users\JSU\AppData\Roaming
!!    MANPATH=/home/urbanjs/V600/LIBRARY/libGPF/download/tmp/man:/home/urbanjs/V600/doc/man:::
!!    DISPLAYNUM=0
!!    ProgramW6432=C:\Program Files
!!    HOSTNAME=buzz
!!    XKEYSYMDB=/usr/share/X11/XKeysymDB
!!    PUBLISH_CMD=
!!    OnlineServices=Online Services
!!         :
!!         :
!!         :
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_readenv() result(string)

character(len=*),parameter::ident_27="@(#)M_system::system_readenv(3f): read next entry from environment table"

character(len=:),allocatable  :: string
character(kind=c_char)        :: c_buff(longest_env_variable+1)

interface
   subroutine c_readenv(c_string) bind (C,NAME='my_readenv')
      import c_char, c_int, c_ptr, c_size_t
      character(kind=c_char),intent(out)  :: c_string(*)
   end subroutine c_readenv
end interface

  c_buff=' '
  c_buff(longest_env_variable+1:longest_env_variable+1)=c_null_char
  call c_readenv(c_buff)
  string=trim(arr2str(c_buff))

end function system_readenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   fileglob(3f) - [M_system] Read output of an ls(1) command from Fortran
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine fileglob(glob,list)
!!
!!    character(len=*),intent(in)   :: glob
!!    character(len=*),pointer      :: list(:)
!!
!!##DESCRIPTION
!!    Non-portable procedure uses the shell and the ls(1) command to expand a filename
!!    and returns a pointer to a list of expanded filenames.
!!
!!##OPTIONS
!!    glob   Pattern for the filenames (like: *.txt)
!!    list   Allocated list of filenames (returned), the caller must deallocate it.
!!
!!##EXAMPLE
!!
!!   Read output of an ls(1) command from Fortran
!!
!!    program demo_fileglob  ! simple unit test
!!       call tryit('*.*')
!!       call tryit('/tmp/__notthere.txt')
!!    contains
!!
!!    subroutine tryit(string)
!!       use M_system, only : fileglob
!!       character(len=255),pointer :: list(:)
!!       character(len=*) :: string
!!       call fileglob(string, list)
!!       write(*,*)'Files:',size(list)
!!       write(*,'(a)')(trim(list(i)),i=1,size(list))
!!       deallocate(list)
!!    end subroutine tryit
!!
!!    end program demo_fileglob  ! simple unit test
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine fileglob(glob, list) ! NON-PORTABLE AT THIS POINT. REQUIRES ls(1) command, assumes 1 line per file
!  The length of the character strings in list() must be long enough for the filenames.
!  The list can be zero names long, it is still allocated.
use M_io,only : notopen,uniq                             ! notopen: needed to open unique scratch file for holding file list
                                                        ! uniq:    adds number suffixs to a filename to make it uniq
implicit none

character(len=*),parameter::ident_28="@(#)M_system::fileglob(3f): Returns list of files using a file globbing pattern"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)   :: glob                   ! Pattern for the filenames (like: *.txt)
character(len=*),pointer      :: list(:)                ! Allocated list of filenames (returned), the caller must deallocate it.
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=255)            :: tmpfile             ! scratch filename to hold expanded file list
   character(len=255)            :: cmd                 ! string to build system command in
   integer                       :: iotmp               ! needed to open unique scratch file for holding file list
   integer                       :: i,ios,icount
   iotmp=notopen(90,1000)                               ! get unused logical I/O unit
   tmpfile='/tmp/__filelist__'                          ! preliminary scratch file name
   tmpfile=uniq(tmpfile,70)                             ! make sure unique scratch filename
   cmd='ls -d '//trim(glob)//'>'//trim(tmpfile)//' '    ! build command string
   call execute_command_line(cmd )                      ! Execute the command specified by the string.
   open(iotmp,file=tmpfile,iostat=ios)                  ! open unique scratch filename
   if(ios.ne.0) return                                  ! the open failed
   icount=0                                             ! number of filenames in expanded list
   do                                                   ! count the number of lines (assumed ==files) so know what to allocate
       read(iotmp,'(a)', iostat=ios)                    ! move down a line in the file to count number of lines
       if(ios .ne. 0)exit                               ! hopefully, this is because end of file was encountered so done
       icount=icount+1                                  ! increment line count
   enddo
   rewind(iotmp)                                        ! rewind file list so can read and store it
   allocate(list(icount))                               ! allocate and fill the array
   do i=1,icount
      read(iotmp,'(a)')list(i)                          ! read a filename from a line
   enddo
   close(iotmp, status='delete',iostat=ios)             ! close and delete scratch file
end subroutine fileglob
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   system_uname(3f) - [M_system] call a C wrapper that calls uname(3c) to get current system information from Fortran
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_uname(WHICH,NAMEOUT)
!!
!!     character(KIND=C_CHAR),intent(in) :: WHICH
!!     character(len=*),intent(out)      :: NAMEOUT
!!##DESCRIPTION
!!        Given a letter, return a corresponding description of the current operating system.
!!        The NAMEOUT variable is assumed sufficiently large enough to hold the value.
!!
!!        s   return the kernel name
!!        r   return the kernel release
!!        v   return the kernel version
!!        n   return the network node hostname
!!        m   return the machine hardware name
!!        T   test mode -- print all information, in the following order - srvnm
!!
!!##EXAMPLE
!!
!!   Call uname(3c) from Fortran
!!
!!    program demo_system_uname
!!       use M_system, only : system_uname
!!       implicit none
!!       integer,parameter          :: is=100
!!       integer                    :: i
!!       character(len=*),parameter :: letters='srvnmxT'
!!       character(len=is)          :: string=' '
!!
!!       do i=1,len(letters)
!!          write(*,'(80("="))')
!!          call system_uname(letters(i:i),string)
!!          write(*,*)'=====> TESTING system_uname('//letters(i:i)//')--->'//trim(string)
!!       enddo
!!
!!    end program demo_system_uname
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_uname(WHICH,NAMEOUT)
implicit none

character(len=*),parameter::ident_29="@(#)M_system::system_uname(3f): call my_uname(3c) which calls uname(3c)"

character(KIND=C_CHAR),intent(in) :: WHICH
character(len=*),intent(out)      :: NAMEOUT

! describe the C routine to Fortran
! void system_uname(char *which, char *buf, int *buflen);
interface
   subroutine system_uname_c(WHICH,BUF,BUFLEN) bind(C,NAME='my_uname')
      import c_char, c_int
      implicit none
      character(KIND=C_CHAR),intent(in)  :: WHICH
      character(KIND=C_CHAR),intent(out) :: BUF(*)
      integer(kind=c_int),intent(in)     :: BUFLEN
   end subroutine system_uname_c
end interface

   NAMEOUT='unknown'
   call system_uname_c(WHICH,NAMEOUT, INT(LEN(NAMEOUT),kind(0_c_int)))

end subroutine system_uname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_gethostname(3f) - [M_system:QUERY] get name of current host
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine system_gethostname(string,ierr)
!!
!!        character(len=:),allocatable,intent(out) :: NAME
!!        integer,intent(out)                      :: IERR
!!##DESCRIPTION
!!        The system_gethostname(3f) procedure returns the standard host
!!        name for the current machine.
!!
!!##OPTIONS
!!        string  returns the hostname. Must be an allocatable CHARACTER variable.
!!        ierr    Upon successful completion, 0 shall be returned; otherwise, -1
!!                shall be returned.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_gethostname
!!
!!    use M_system, only : system_gethostname
!!    implicit none
!!    character(len=:),allocatable :: name
!!    integer                      :: ierr
!!
!!       call system_gethostname(name,ierr)
!!       if(ierr.eq.0)then
!!          write(*,'("hostname[",a,"]")')name
!!       else
!!          write(*,'(a)')'ERROR: could not get hostname'
!!       endif
!!
!!    end program demo_system_gethostname
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_gethostname(NAME,IERR)
implicit none

character(len=*),parameter::ident_30="@(#)M_system::system_gethostname(3f): get name of current host by calling gethostname(3c)"

character(len=:),allocatable,intent(out) :: NAME
integer,intent(out)                      :: IERR
   character(kind=c_char,len=1)          :: C_BUFF(HOST_NAME_MAX+1)

! describe the C routine to Fortran
!int gethostname(char *name, size_t namelen);
interface
   function system_gethostname_c(c_buf,c_buflen) bind(C,NAME='gethostname')
      import c_char, c_int
      implicit none
      integer(kind=c_int)                  :: system_gethostname_c
      character(KIND=C_CHAR),intent(out)   :: c_buf(*)
      integer(kind=c_int),intent(in),value :: c_buflen
   end function system_gethostname_c
end interface

   C_BUFF=' '
   ierr=system_gethostname_c(C_BUFF,HOST_NAME_MAX) ! Host names are limited to {HOST_NAME_MAX} bytes.
   NAME=trim(arr2str(C_BUFF))

end subroutine system_gethostname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getlogin(3f) - [M_system:QUERY] get login name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function system_getlogin() result (fname)
!!
!!    character(len=:),allocatable :: FNAME
!!
!!##DESCRIPTION
!!
!!    The system_getlogin(3f) function returns a string containing the user
!!    name associated by the login activity with the controlling terminal
!!    of the current process. Otherwise, it returns a null string and sets
!!    errno to indicate the error.
!!
!!    Three names associated with the current process can be determined:
!!
!!       o system_getpwuid(system_getuid()) returns the name associated with the real user ID of the process.
!!       o system_getpwuid(system_geteuid()) returns the name associated with the effective user ID of the process
!!       o system_getlogin() returns the name associated with the current login activity
!!
!!##RETURN VALUE
!!    fname  returns the login name.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_getlogin
!!    use M_system, only : system_getlogin
!!    implicit none
!!    character(len=:),allocatable :: name
!!    name=system_getlogin()
!!    write(*,'("login[",a,"]")')name
!!    end program demo_system_getlogin
!!
!!   Results:
!!
!!    login[JSU]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!--       The following example calls the getlogin() function to obtain the name of the user associated with the calling process,
!--       and passes this information to the getpwnam() function to get the associated user database information.
!--           ...
!--           char *lgn;
!--           struct passwd *pw;
!--           ...
!--           if ((lgn = getlogin()) == NULL || (pw = getpwnam(lgn)) == NULL) {
!--               fprintf(stderr, "Get of user information failed.\n"); exit(1);
!--               }
!--APPLICATION USAGE
!--SEE ALSO
!--       getpwnam(), getpwuid(), system_geteuid(), getuid()
function system_getlogin() result (fname)
character(len=:),allocatable :: fname
   type(c_ptr)               :: username

interface
   function c_getlogin() bind(c,name="getlogin") result(c_username)
      import c_int, c_ptr
      type(c_ptr)           :: c_username
   end function c_getlogin
end interface

   username = c_getlogin()
   if(.not.c_associated(username)) then
      !! in windows 10 subsystem running Ubunto does not work
      !!write(*,'(a)')'*system_getlogin* Error getting username. not associated'
      !!fname=c_null_char
      fname=system_getpwuid(system_geteuid())
   else
      fname=c2f_string(username)
   endif

end function system_getlogin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_perm(3f) - [M_system] get file type and permission as a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function system_perm(mode) result (perms)
!!
!!    integer(kind=int64),intent(in)   :: MODE
!!    character(len=:),allocatable :: PERMS
!!
!!##DESCRIPTION
!!
!!    The system_perm(3f) function returns a string containing the type
!!    and permission of a file implied by the value of the mode value.
!!
!!##RETURN VALUE
!!    PERMS  returns the permission string in a format similar to that
!!           used by Unix commands such as ls(1).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_perm
!!    use M_system, only : system_perm, system_stat
!!    use,intrinsic     :: iso_fortran_env, only : int64
!!    implicit none
!!    character(len=4096) :: string
!!    integer(kind=int64)     :: values(13)
!!    integer             :: ierr
!!    character(len=:),allocatable :: perms
!!       values=0
!!       call get_command_argument(1, string)  ! get pathname from command line
!!       call system_stat(string,values,ierr)  ! get pathname information
!!       if(ierr.eq.0)then
!!          perms=system_perm(values(3))       ! convert permit mode to a string
!!          ! print permits as a string, decimal value, and octal value
!!          write(*,'("for ",a," permits[",a,"]",1x,i0,1x,o0)') &
!!                  trim(string),perms,values(3),values(3)
!!       endif
!!    end program demo_system_perm
!!
!!   Results:
!!
!!    demo_system_perm /tmp
!!
!!    for /tmp permits[drwxrwxrwx --S] 17407 41777
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_perm(mode) result (perms)
use M_anything, only : anyinteger_to_64bit
class(*),intent(in)          :: mode
character(len=:),allocatable :: perms
   type(c_ptr)               :: permissions
   integer(kind=c_long)      :: mode_local
interface
   function c_perm(c_mode) bind(c,name="my_get_perm") result(c_permissions)
      import c_int, c_ptr, c_long
      integer(kind=c_long),value  :: c_mode
      type(c_ptr)                 :: c_permissions
   end function c_perm
end interface

   mode_local=int(anyinteger_to_64bit(mode),kind=c_long)
   permissions = c_perm(mode_local)
   if(.not.c_associated(permissions)) then
      write(*,'(a)')'*system_perm* Error getting permissions. not associated'
      perms=c_null_char
   else
      perms=c2f_string(permissions)
   endif

end function system_perm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getgrgid(3f) - [M_system:QUERY] get groupd name associated with a GID
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_getgrgid(gid) result (gname)
!!
!!    class(*),intent(in)          :: gid   ! any INTEGER type
!!    character(len=:),allocatable :: gname
!!
!!##DESCRIPTION
!!
!!    The system_getlogin() function returns a string containing the group
!!    name associated with the given GID. If no match is found
!!    it returns a null string and sets errno to indicate the error.
!!
!!##OPTION
!!    gid    GID to try to look up associated group for. Can be of any
!!           INTEGER type.
!!
!!##RETURN VALUE
!!    gname  returns the group name. Blank if an error occurs
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_getgrgid
!!    use M_system, only : system_getgrgid
!!    use M_system, only : system_getgid
!!    implicit none
!!    character(len=:),allocatable :: name
!!    name=system_getgrgid( system_getgid() )
!!    write(*,'("group[",a,"] for ",i0)')name,system_getgid()
!!    end program demo_system_getgrgid
!!
!!   Results:
!!
!!    group[default] for 197121
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_getgrgid(gid) result (gname)
use M_anything, only : anyinteger_to_64bit
class(*),intent(in)                        :: gid
character(len=:),allocatable               :: gname
   character(kind=c_char,len=1)            :: groupname(4097)  ! assumed long enough for any groupname
   integer                                 :: ierr
   integer(kind=c_long_long)               :: gid_local

interface
   function c_getgrgid(c_gid,c_groupname) bind(c,name="my_getgrgid") result(c_ierr)
      import c_int, c_ptr, c_char,c_long_long
      integer(kind=c_long_long),value,intent(in) :: c_gid
      character(kind=c_char),intent(out)         :: c_groupname(*)
      integer(kind=c_int)                        :: c_ierr
   end function c_getgrgid
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   gid_local=anyinteger_to_64bit(gid)
   ierr = c_getgrgid(gid_local,groupname)
   if(ierr.eq.0)then
      gname=trim(arr2str(groupname))
   else
      gname=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function system_getgrgid
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getpwuid(3f) - [M_system:QUERY] get login name associated with a UID
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_getpwuid(uid) result (uname)
!!
!!    class(*),intent(in)          :: uid    ! any INTEGER type
!!    character(len=:),allocatable :: uname
!!
!!##DESCRIPTION
!!
!!    The system_getpwuid() function returns a string containing the user
!!    name associated with the given UID. If no match is found it returns
!!    a null string and sets errno to indicate the error.
!!
!!##OPTION
!!    uid    UID to try to look up associated username for. Can be of any
!!           INTEGER type.
!!
!!##RETURN VALUE
!!    uname  returns the login name.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_getpwuid
!!    use M_system, only : system_getpwuid
!!    use M_system, only : system_getuid
!!    use,intrinsic     :: iso_fortran_env, only : int64
!!    implicit none
!!    character(len=:),allocatable :: name
!!    integer(kind=int64)              :: uid
!!       uid=system_getuid()
!!       name=system_getpwuid(uid)
!!       write(*,'("login[",a,"] has UID ",i0)')name,uid
!!    end program demo_system_getpwuid
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function system_getpwuid(uid) result (uname)
use M_anything, only : anyinteger_to_64bit
class(*),intent(in)                        :: uid
character(len=:),allocatable               :: uname
   character(kind=c_char,len=1)            :: username(4097)  ! assumed long enough for any username
   integer                                 :: ierr
   integer(kind=c_long_long)               :: uid_local

interface
   function c_getpwuid(c_uid,c_username) bind(c,name="my_getpwuid") result(c_ierr)
      import c_int, c_ptr, c_char, c_long_long
      integer(kind=c_long_long),value,intent(in) :: c_uid
      character(kind=c_char),intent(out)   :: c_username(*)
      integer(kind=c_int)                  :: c_ierr
   end function c_getpwuid
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   uid_local=anyinteger_to_64bit(uid)
   ierr = c_getpwuid(uid_local,username)
   if(ierr.eq.0)then
      uname=trim(arr2str(username))
   else
      uname=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function system_getpwuid
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function arr2str(array)  result (string)

character(len=*),parameter::ident_31="@(#)M_system::arr2str(3fp): function copies null-terminated char array to string"

character(len=1),intent(in)  :: array(:)
character(len=size(array))   :: string
integer                      :: i

   string=' '
   do i = 1,size(array)
      if(array(i).eq.char(0))then
         exit
      else
         string(i:i) = array(i)
      endif
   enddo

end function arr2str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function str2_carr(string) result (array)

character(len=*),parameter::ident_32="@(#)M_system::str2_carr(3fp): function copies string to null terminated char array"

character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)
   integer                      :: i

   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(i:i)=c_null_char

end function str2_carr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function C2F_string(c_string_pointer) result(f_string)

! gets a C string (pointer), and returns the corresponding Fortran string up to 4096(max_len) characters;
! If the C string is null, it returns string C "null" character:

type(c_ptr), intent(in)                       :: c_string_pointer
character(len=:), allocatable                 :: f_string
character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
integer,parameter                             :: max_len=4096
character(len=max_len)                        :: aux_string
integer                                       :: i
integer                                       :: length

   length=0
   call c_f_pointer(c_string_pointer,char_array_pointer,[max_len])

   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string)
     f_string=c_null_char
     return
   endif

   aux_string=" "

   do i=1,max_len
     if (char_array_pointer(i)==c_null_char) then
       length=i-1; exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo

   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)
end function C2F_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    SYSTEM_STAT - [M_system] Get file status information
!!    (LICENSE:PD)
!!
!!##SYNTAX
!!   CALL SYSTEM_STAT(NAME, VALUES [, STATUS],[DEBUG])
!!
!!    character(len=*),intent(in)          :: NAME
!!    integer(kind=int64),intent(out)      :: values(13)
!!    integer,optional,intent(out)         :: status
!!    integer,intent(in)                   :: debug
!!
!!##DESCRIPTION
!!
!!    This function returns information about a file. No permissions are
!!    required on the file itself, but execute (search) permission is required
!!    on all of the directories in path that lead to the file. The elements
!!    that are obtained and stored in the array VALUES:
!!
!!       VALUES(1) Device ID
!!       VALUES(2) Inode number
!!       VALUES(3) File mode
!!       VALUES(4) Number of links
!!       VALUES(5) Owner's uid
!!       VALUES(6) Owner's gid
!!       VALUES(7) ID of device containing directory entry for file (0 if not available)
!!       VALUES(8) File size (bytes)
!!       VALUES(9) Last access time as a Unix Epoch time rounded to seconds
!!       VALUES(10) Last modification time as a Unix Epoch time rounded to seconds
!!       VALUES(11) Last file status change time as a Unix Epoch time rounded to seconds
!!       VALUES(12) Preferred I/O block size (-1 if not available)
!!       VALUES(13) Number of blocks allocated (-1 if not available)
!!
!!    Not all these elements are relevant on all systems. If an element is
!!    not relevant, it is returned as 0.
!!
!!##OPTIONS
!!
!!    NAME    The type shall be CHARACTER, of the default kind and a valid
!!            path within the file system.
!!    VALUES  The type shall be INTEGER(8), DIMENSION(13).
!!    STATUS  (Optional) status flag of type INTEGER(4). Returns 0 on success
!!            and a system specific error code otherwise.
!!    DEBUG   (Optional) print values being returned from C routine being
!!            called if value of 0 is used
!!
!!##EXAMPLE
!!
!!   program demo_system_stat
!!
!!    use M_system, only : system_stat, system_getpwuid, system_getgrgid
!!    use M_time, only :   fmtdate, u2d
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!
!!    integer(kind=int64)  :: buff(13)
!!    integer(kind=int32)  :: status
!!    character(len=*),parameter :: fmt_date='year-month-day hour:minute:second'
!!
!!    integer(kind=int64) :: &
!!     Device_ID, Inode_number,     File_mode, Number_of_links, Owner_uid,        &
!!     Owner_gid, Directory_device, File_size, Last_access,     Last_modification,&
!!     Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
!!    equivalence                                    &
!!       ( buff(1)  , Device_ID                  ) , &
!!       ( buff(2)  , Inode_number               ) , &
!!       ( buff(3)  , File_mode                  ) , &
!!       ( buff(4)  , Number_of_links            ) , &
!!       ( buff(5)  , Owner_uid                  ) , &
!!       ( buff(6)  , Owner_gid                  ) , &
!!       ( buff(7)  , Directory_device           ) , &
!!       ( buff(8)  , File_size                  ) , &
!!       ( buff(9)  , Last_access                ) , &
!!       ( buff(10) , Last_modification          ) , &
!!       ( buff(11) , Last_status_change         ) , &
!!       ( buff(12) , Preferred_block_size       ) , &
!!       ( buff(13) , Number_of_blocks_allocated )
!!
!!    CALL SYSTEM_STAT("/etc/hosts", buff, status)
!!
!!    if (status == 0) then
!!       write (*, FMT="('Device ID(hex/decimal):',      &
!!       & T30, Z0,'h/',I0,'d')") buff(1),buff(1)
!!       write (*, FMT="('Inode number:',                &
!!       & T30, I0)") buff(2)
!!       write (*, FMT="('File mode (octal):',           &
!!       & T30, O19)") buff(3)
!!       write (*, FMT="('Number of links:',             &
!!       & T30, I0)") buff(4)
!!       write (*, FMT="('Owner''s uid/username:',       &
!!       & T30, I0,1x, A)") buff(5), system_getpwuid(buff(5))
!!       write (*, FMT="('Owner''s gid/group:',          &
!!       & T30, I0,1x, A)") buff(6), system_getgrgid(buff(6))
!!       write (*, FMT="('Device where located:',        &
!!       & T30, I0)") buff(7)
!!       write (*, FMT="('File size(bytes):',            &
!!       & T30, I0)") buff(8)
!!       write (*, FMT="('Last access time:',            &
!!       & T30, I0,1x, A)") buff(9), fmtdate(u2d(int(buff(9))),fmt_date)
!!       write (*, FMT="('Last modification time:',      &
!!       & T30, I0,1x, A)") buff(10),fmtdate(u2d(int(buff(10))),fmt_date)
!!       write (*, FMT="('Last status change time:',     &
!!       & T30, I0,1x, A)") buff(11),fmtdate(u2d(int(buff(11))),fmt_date)
!!       write (*, FMT="('Preferred block size(bytes):', &
!!       & T30, I0)") buff(12)
!!       write (*, FMT="('No. of blocks allocated:',     &
!!       & T30, I0)") buff(13)
!!    endif
!!
!!    end program demo_system_stat
!!
!!   Results:
!!
!!    Device ID(hex/decimal):      3E6BE045h/1047257157d
!!    Inode number:                1407374886070599
!!    File mode (octal):                        100750
!!    Number of links:             1
!!    Owner's uid/username:        18 SYSTEM
!!    Owner's gid/group:           18 SYSTEM
!!    Device where located:        0
!!    File size(bytes):            824
!!    Last access time:            1557983191 2019-05-16 01:06:31
!!    Last modification time:      1557983191 2019-05-16 01:06:31
!!    Last status change time:     1557983532 2019-05-16 01:12:12
!!    Preferred block size(bytes): 65536
!!    No. of blocks allocated:     4
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_stat(pathname,values,ierr)
implicit none

character(len=*),parameter::ident_33="@(#)M_system::system_stat(3f): call stat(3c) to get pathname information"

character(len=*),intent(in)          :: pathname

integer(kind=int64),intent(out)      :: values(13)
integer(kind=c_long)                 :: cvalues(13)

integer,optional,intent(out)         :: ierr
integer(kind=c_int)                  :: cierr

interface
   subroutine c_stat(buffer,cvalues,cierr,cdebug) bind(c,name="my_stat")
      import c_char, c_size_t, c_ptr, c_int, c_long
      character(kind=c_char),intent(in)   :: buffer(*)
      integer(kind=c_long),intent(out)    :: cvalues(*)
      integer(kind=c_int)                 :: cierr
      integer(kind=c_int),intent(in)      :: cdebug
   end subroutine c_stat
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   call c_stat(str2_carr(trim(pathname)),cvalues,cierr,0_c_int)
   values=cvalues
   if(present(ierr))then
      ierr=cierr
   endif
end subroutine system_stat
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_stat_print(3f) - [M_system] print the principal info obtained for a pathname from system_stat(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine system_stat_print(filename)
!!
!!    character(len=*),intent(in)  :: filename
!!    integer,intent(in),optional :: lun
!!##DESCRIPTION
!!      Call the system_stat(3f) routine and print the results
!!##OPTIONS
!!    filename   pathname to print information for
!!    lun        unit number to write to. Optional
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_system_stat_print
!!    use M_system, only : system_stat_print
!!    implicit none
!!       call system_stat_print('/tmp')
!!       call system_stat_print('/etc/hosts')
!!    end program demo_system_stat_print
!!
!!   Sample Result
!!
!!     41777 drwxrwxrwx --S 1  JSU      None     0    2018-10-19T21:10:39 /tmp
!!    100750 -rwxr-x--- --- 1  SYSTEM   SYSTEM   824  2018-08-17T01:21:55 /etc/hosts
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine system_stat_print(filename,lun)
!!use M_system, only      : system_getpwuid, system_getgrgid, system_perm, system_stat
use M_time, only          : fmtdate, u2d
use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT
implicit none
character(len=*),intent(in)  :: filename
integer,intent(in),optional  :: lun
integer                      :: lun_local
character(len=*),parameter   :: dfmt='year-month-dayThour:minute:second'
integer                      :: ierr
integer(kind=int64)              :: values(13)
integer(kind=int64) :: &
   Device_ID,           Inode_number,          File_mode,                  Number_of_links,  Owner_uid,         &
   Owner_gid,           Directory_device,      File_size,                  Last_access,      Last_modification, &
   Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
EQUIVALENCE                                      &
   ( VALUES(1)  , Device_ID                  ) , &
   ( VALUES(2)  , Inode_number               ) , &
   ( VALUES(3)  , File_mode                  ) , &
   ( VALUES(4)  , Number_of_links            ) , &
   ( VALUES(5)  , Owner_uid                  ) , &
   ( VALUES(6)  , Owner_gid                  ) , &
   ( VALUES(7)  , Directory_device           ) , &
   ( VALUES(8)  , File_size                  ) , &
   ( VALUES(9)  , Last_access                ) , &
   ( VALUES(10) , Last_modification          ) , &
   ( VALUES(11) , Last_status_change         ) , &
   ( VALUES(12) , Preferred_block_size       ) , &
   ( VALUES(13) , Number_of_blocks_allocated )

   if(present(lun))then
      lun_local=lun
   else
      lun_local=OUTPUT_UNIT
   endif

   !write(lun, FMT="('Inode number:',                T30, I0)",advance='no') values(2)
   !write(lun, FMT="(' No. of blocks allocated:',     I0)",advance='no') values(13)

   call system_stat(filename,values,ierr)
   if(ierr.eq.0)then
      write(lun_local, FMT="(o6.0,t7,1x,a)",advance='no') File_mode,system_perm(File_mode)
      write(lun_local, FMT="(1x,I0,t4)",advance='no')  Number_of_links
      write(lun_local, FMT="(1x,A,t10)",advance='no')  system_getpwuid(Owner_uid)
      write(lun_local, FMT="(1x,A,t10)",advance='no')  system_getgrgid(Owner_gid)
      write(lun_local, FMT="(1x,bn,I0,t10)",advance='no') File_size
      write(lun_local, FMT="(1x,A)",advance='no')      fmtdate(u2d(int(max(Last_access,Last_modification,Last_status_change))),dfmt)
      write(lun_local, FMT="(1x,a)")filename
   endif

end subroutine system_stat_print
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_suite_M_system()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
use M_process ,only: process_readall
!! setup
call unit_check_msg('M_system','try to test OS interface routines, given difficulty of trying to test')
call unit_check_msg('M_system','functions not intrinsically part of Fortran and system-dependent.')
call unit_check_msg('M_system','Many assumptions are made, including assuming a GNU Linux/Unix system.')
call unit_check_msg('M_system','Examine the tests on other platforms, as it may well be the assumptions made')
call unit_check_msg('M_system','about the system and not the routines that are generating an error.')
call test_set_environment_variable()
call test_system_rename()
call test_system_getlogin()
call test_system_geteuid()
call test_system_getegid()
call test_system_getgid()
call test_system_getuid()
call test_system_getpid()
call test_system_getppid()
call test_system_isdir()
call test_system_chdir()
call test_system_rmdir()
call test_system_mkdir()
call test_system_opendir()
call test_system_readdir()
call test_system_rewinddir()
call test_system_closedir()
call test_system_putenv()
call test_system_unsetenv()
call test_system_getenv()
call test_system_initenv()
call test_system_readenv()
call test_system_remove()
call test_system_getcwd()

   call test_system_clearenv()
   call test_system_access()
   call test_system_chmod()
   call test_system_chown()
   call test_system_cpu_time()
   call test_system_errno()
   call test_system_getgrgid()
   call test_system_gethostname()
   call test_fileglob()

   call test_system_getpwuid()
   call test_system_getsid()
   call test_system_setsid()
   call test_system_getumask()
   call test_system_isblk()
   call test_system_ischr()
   call test_system_isfifo()
   call test_system_islnk()
   call test_system_isreg()
   call test_system_issock()
   call test_system_kill()
   call test_system_link()
   call test_system_mkfifo()
   call test_system_perm()
   call test_system_perror()
   call test_system_rand()
   call test_system_srand()
   call test_system_realpath()
   call test_system_setumask()
   call test_system_stat()
   call test_system_stat_print()
   call test_system_uname()
   call test_system_unlink()
   call test_system_utime()
   call test_system_memcpy()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_stat_print()
   call unit_check_start('system_stat_print',msg='')
   call system_stat_print('/tmp')
   call system_stat_print('/etc/hosts')
   !!call unit_check('system_stat_print', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_stat_print',msg='')
end subroutine test_system_stat_print
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_srand()
integer :: i,j
   do j=1,2
      call system_srand(1001)
      do i=1,10
         write(*,*)system_rand()
      enddo
      write(*,*)
   enddo
   call unit_check_start('system_srand',msg='')
   !!call unit_check('system_srand', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_srand',msg='')
end subroutine test_system_srand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_kill()
integer           :: i,pid,ios,ierr,signal=9
character(len=80) :: argument

   do i=1,command_argument_count()
! get arguments from command line
      call get_command_argument(i, argument)
! convert arguments to integers assuming they are PID numbers
      read(argument,'(i80)',iostat=ios) pid
      if(ios.ne.0)then
         write(*,*)'bad PID=',trim(argument)
      else
         write(*,*)'kill SIGNAL=',signal,' PID=',pid
! send signal SIGNAL to pid PID
         ierr=system_kill(pid,signal)
! write message if an error was detected
         if(ierr.ne.0)then
            call system_perror('*test_system_kill*')
         endif
      endif
   enddo
   call unit_check_start('system_kill',msg='')
   !!call unit_check('system_kill', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_kill',msg='')
end subroutine test_system_kill
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_errno()
integer :: stat
   stat=system_unlink('not there/OR/anywhere')
   if(stat.ne.0)then
      write(*,*)'err=',system_errno()
      call system_perror('*test_system_errno*')
   endif
   call unit_check_start('system_errno',msg='')
   !!call unit_check('system_errno', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_errno',msg='')
end subroutine test_system_errno
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_geteuid()
                     integer :: ierr
character(len=:),allocatable :: string
integer                      :: uid_command
integer                      :: uid
   call unit_check_start('system_geteuid',msg='check using command "id -u"')
   string=process_readall('id -u',ierr=ierr)
   !!call unit_check('system_geteuid', ierr.eq.0, msg=msg('using command "id -u" ierr=',ierr,'effective UID=',string))
   call unit_check('system_geteuid', string.ne.' ', msg=msg('using command "id -u" ierr=',ierr,'effective UID=',string))
   uid=system_geteuid();
   if(string.ne.'')then
      read(string,*)uid_command
      call unit_check('system_geteuid', uid.eq.uid_command, msg=msg('uid=',uid))
      call unit_check_done('system_geteuid',msg='')
   else
      call unit_check_bad('system_geteuid', msg=msg(' assuming bad because system command did not work. uid=',uid))
   endif
end subroutine test_system_geteuid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getuid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: uid_command
integer                      :: uid
integer                      :: ios
   call unit_check_start('system_getuid',msg='check using command "id -u -r"')
   string=process_readall('id -u -r',ierr=ierr)
   !!call unit_check('system_getuid', ierr.eq.0, msg=msg('using command "id -u -r" ierr=',ierr,'UID=',string))
   call unit_check('system_getuid', string.ne.' ', msg=msg('using command "id -u -r" ierr=',ierr,'UID=',string))
   uid=system_getuid();
   if(string.ne.' ')then
      read(string,*,iostat=ios)uid_command
      call unit_check('system_getuid', ios.eq.0, msg=msg('read uid=',uid_command))
      call unit_check('system_getuid', uid.eq.uid_command, msg=msg('uid=',uid))
      call unit_check_done('system_getuid',msg='')
   else
      call unit_check_bad('system_getuid', msg=msg(' assuming bad because system command did not work. uid=',uid))
   endif
end subroutine test_system_getuid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getegid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: gid_command
integer                      :: gid
character(len=*),parameter   :: cmd='id -g'
   call unit_check_start('system_getegid',msg=msg('check using command',cmd))
   string=process_readall(cmd,ierr=ierr)
   !!call unit_check('system_getegid', string.ne.' ', msg=msg('using command "',cmd,'" ierr=',ierr,'GID=',string))
   gid=system_getegid();
   if(string.ne.' ')then
      read(string,*)gid_command
      call unit_check('system_getegid', gid.eq.gid_command, msg=msg('gid=',gid))
      call unit_check_done('system_getegid',msg='')
   else
      call unit_check_bad('system_getegid', msg=msg(' assuming bad because system command did not work. gid=',gid))
   endif
end subroutine test_system_getegid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getgid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: gid_command
integer                      :: gid
character(len=*),parameter   :: cmd='id -g -r'
   call unit_check_start('system_getgid',msg=msg('check using command',cmd))
   string=process_readall(cmd,ierr=ierr)
   !!call unit_check('system_getgid', ierr.eq.0, msg=msg('using command "',cmd,'" ierr=',ierr,'GID=',string))
   call unit_check('system_getgid', string.ne.' ', msg=msg('using command "',cmd,'" ierr=',ierr,'GID=',string))
   gid=system_getgid();
   if(string.ne.' ')then
      read(string,*)gid_command
      call unit_check('system_getgid', gid.eq.gid_command, msg=msg('gid=',gid))
      call unit_check_done('system_getgid',msg='')
   else
      call unit_check_bad('system_getgid', msg=msg(' assuming bad because system command did not work. gid=',gid))
   endif
end subroutine test_system_getgid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getsid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: sid_command
integer                      :: sid
character(len=*),parameter   :: cmd='UNKNOWN'
   call unit_check_start('system_getsid',msg=msg('check using command',cmd))
!!   string=process_readall(cmd,ierr=ierr)
!!   call unit_check('system_getsid', ierr.eq.0, msg=msg('using command "',cmd,'" ierr=',ierr,'sid=',string))
   sid=system_getsid(0_c_int);
!!   if(string.ne.' ')then
!!      read(string,*)sid_command
!!      call unit_check('system_getsid', sid.eq.sid_command, msg=msg('sid=',sid))
      call unit_check_done('system_getsid',msg='')
!!   else
!!      call unit_check_bad('system_getsid', msg=msg(' assuming bad because system command did not work. sid=',sid))
!!   endif
end subroutine test_system_getsid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_setsid()
integer                      :: ierr
integer                      :: pid
   call unit_check_start('system_setsid',msg=msg(''))
   pid=system_setsid();
   !!call unit_check('system_setsid', pid.ge.0, msg=msg('just checking PID>0 pid=',pid))
   call unit_check_done('system_setsid',msg='')
end subroutine test_system_setsid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getpid()
integer                      :: ierr
integer                      :: pid
   call unit_check_start('system_getpid',msg=msg('PID (process ID) of current process'))
   pid=system_getpid();
   call unit_check('system_getpid', pid.ge.0, msg=msg('just checking PID>0 pid=',pid))
   call unit_check_done('system_getpid',msg='')
end subroutine test_system_getpid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getppid()
integer                      :: ierr
integer                      :: ppid
call unit_check_start('system_getppid',msg=msg('only make sure call does not work and returns value >0'))
   ppid=system_getppid();
   call unit_check('system_getppid', ppid.ge.0, msg=msg('ppid=',ppid))
   call unit_check_done('system_getppid',msg='')
end subroutine test_system_getppid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rand()
integer :: i

   call system_srand(1001)
   do i=1,10
      write(*,*)system_rand()
   enddo
   write(*,*)

   call unit_check_start('system_rand',msg='')
   !!call unit_check('system_rand', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_rand',msg='')
end subroutine test_system_rand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_initenv()
character(len=:),allocatable :: string
integer                      :: i
integer                      :: ierr
character(len=:),allocatable :: home
character(len=4096)          :: envname
   call unit_check_start('system_initenv',msg='assuming system always has environment variable HOME set')
   i=0
   home=''
   ! read environment table and look for HOME= at beginning of line
   call system_initenv()
   do
      string=system_readenv()
      if(index(string,'HOME=').eq.1)then
        home=string
      endif
      if(string.eq.'')then
         exit
      else
         i=i+1
      endif
   enddo
   call get_environment_variable("HOME",value=envname, status=ierr)
   envname='HOME='//trim(envname)
   call unit_check('system_initenv',home.eq.envname, msg=msg('HOME',home,envname))
   call unit_check_done('system_initenv',msg='')
end subroutine test_system_initenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_realpath()
! resolve each pathname given on command line
character(len=:),allocatable :: pathi,patho
integer                      :: i
integer                      :: filename_length
integer                      :: ierr
   do i = 1, command_argument_count()
! get pathname from command line arguments
      call get_command_argument (i , length=filename_length)
      allocate(character(len=filename_length) :: pathi)
      call get_command_argument (i , value=pathi)
!
! resolve each pathname
      patho=system_realpath(pathi)
      if(ierr.eq.0)then
         write(*,*)trim(pathi),'=>',trim(patho)
      else
         call system_perror('*system_realpath* error for pathname '//trim(pathi)//':')
         write(*,*)trim(pathi),'=>',trim(patho)
      endif
      deallocate(pathi)
   enddo
! if there were no pathnames give resolve the pathname "."
   if(i.eq.1)then
      patho=system_realpath('.')
      write(*,*)'.=>',trim(patho)
   endif
   call unit_check_start('system_realpath',msg='')
   !!call unit_check('system_realpath', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_realpath',msg='')
end subroutine test_system_realpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileglob()
   call unit_check_start('fileglob',msg='')
   !!call unit_check('fileglob', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('fileglob',msg='')
end subroutine test_fileglob
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_set_environment_variable()

integer :: ierr
character(len=4096) :: value
   call unit_check_start('set_environment_variable',msg='')
!! CHECK NOT_THERE_S_E_V IS NOT THERE FOR TEST
   call get_environment_variable("NOT_THERE_S_E_V", status=ierr)
   call unit_check('set_environment_variable',ierr.eq.1,msg=msg('make sure variable does not exist,status=',ierr))
!! SET THE VARIABLE NOT_THERE_S_E_V
   call set_environment_variable('NOT_THERE_S_E_V','this is the value',ierr)
!! CHECK VARIABLE IS NOW SET
   call unit_check('set_environment_variable',ierr.eq.0,msg=msg('setting, status should be zero when setting=',ierr))
   call get_environment_variable("NOT_THERE_S_E_V", value=value,status=ierr)
   call unit_check('set_environment_variable',ierr.eq.0,msg=msg('status should be zero when getting=',ierr))
   call unit_check('set_environment_variable',value.eq.'this is the value',msg=msg('value is set to:',value))
!! REPLACE VALUE
   call set_environment_variable('NOT_THERE_S_E_V','this is the new value',ierr)
   call unit_check('set_environment_variable',ierr.eq.0,msg=msg('setting, status should be zero when setting=',ierr))
   call get_environment_variable("NOT_THERE_S_E_V", value=value,status=ierr)
   call unit_check('set_environment_variable',ierr.eq.0,msg=msg('status should be zero when getting=',ierr))
   call unit_check('set_environment_variable',value.eq.'this is the new value',msg=msg('value is set to:',value))

   call unit_check_done('set_environment_variable',msg='')
end subroutine test_set_environment_variable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_access()

integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/usr/bin/bash   ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' does ',trim(names(i)),' exist?    ', system_access(names(i),F_OK)
      write(*,*)' is ',trim(names(i)),' readable?     ', system_access(names(i),R_OK)
      write(*,*)' is ',trim(names(i)),' writeable?    ', system_access(names(i),W_OK)
      write(*,*)' is ',trim(names(i)),' executable?   ', system_access(names(i),X_OK)
   enddo
   call unit_check_start('system_access',msg='')
   !!call unit_check('system_access', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_access',msg='')
end subroutine test_system_access
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_chdir()
character(len=:),allocatable :: dirname
character(len=:),allocatable :: hold
integer             :: ierr
   call unit_check_start('system_chdir',msg='test system_chdir(3f) assuming Unix-like file system and system_getwd(3f) works')
   call system_getcwd(hold,ierr)

   call system_chdir('/tmp',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_chdir', dirname.eq.'/tmp', msg=msg('checking /tmp to',trim(dirname)))

   call system_chdir('/',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_chdir', dirname.eq.'/', msg=msg('checking / to',dirname))

   call system_chdir(hold,ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_chdir', dirname.eq.hold, msg=msg('checking ',hold,' to',dirname))

   call unit_check_done('system_chdir',msg='')
end subroutine test_system_chdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_chmod()

integer             :: ierr
integer             :: status
integer(kind=int64) :: buffer(13)
integer             :: ios
character(len=4096) :: message

!Setting Read Permissions for User, Group, and Others
! The following example sets read permissions for the owner, group, and others.
   open(file='_test1',unit=10)
   write(10,*,iostat=ios,iomsg=message)'TEST FILE 1'
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

   flush(unit=10,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

   close(unit=10,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

   ierr=system_chmod('_test1', IANY([R_USR,R_GRP,R_OTH]))

   open(file='_test1',unit=10)
   close(unit=10,status='delete',iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

!Setting Read, Write, and Execute Permissions for the Owner Only
! The following example sets read, write, and execute permissions for the owner, and no permissions for group and others.
   open(file='_test2',unit=10)
   write(10,*)'TEST FILE 2'
   close(unit=10)
   ierr=system_chmod('_test2', RWX_U)
   open(file='_test2',unit=10)
   close(unit=10,status='delete')

!Setting Different Permissions for Owner, Group, and Other
! The following example sets owner permissions for CHANGEFILE to read, write, and execute, group permissions to read and
! execute, and other permissions to read.
   open(file='_test3',unit=10)
   write(10,*)'TEST FILE 3'
   close(unit=10)
   ierr=system_chmod('_test3', IANY([RWX_U,R_GRP,X_GRP,R_OTH]));
   open(file='_test3',unit=10)
   close(unit=10,status='delete')

!Setting and Checking File Permissions
! The following example sets the file permission bits for a file named /home/cnd/mod1, then calls the stat() function to
! verify the permissions.

   ierr=system_chmod("home/cnd/mod1", IANY([RWX_U,RWX_G,R_OTH,W_OTH]))
   call system_stat("home/cnd/mod1", buffer,status)

! In order to ensure that the S_ISUID and S_ISGID bits are set, an application requiring this should use stat() after a
! successful chmod() to verify this.

!    Any files currently open could possibly become invalid if the mode
!    of the file is changed to a value which would deny access to
!    that process.

   call unit_check_start('system_chmod',msg='')
   !!call unit_check('system_chmod', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_chmod',msg='')
end subroutine test_system_chmod
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_chown()

integer                     :: i
character(len=80),parameter :: names(*)=[character(len=80) :: 'myfile1','/usr/local']
   do i=1,size(names)
      if(.not.  system_chown(&
      & trim(names(i)),  &
      & system_getuid(), &
      & system_getgid()) &
         )then
         call system_perror('*test_system_chown* '//trim(names(i)))
      endif
   enddo
   call unit_check_start('system_chown',msg='')
   !!call unit_check('system_chown', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_chown',msg='')
end subroutine test_system_chown
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_clearenv()

! environment before clearing
   call execute_command_line('env|wc -l')
! environment after clearing (not necessarily blank!!)
   call system_clearenv()
   call execute_command_line('env')
   call unit_check_start('system_clearenv',msg='')
   !!call unit_check('system_clearenv', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_clearenv',msg='')
end subroutine test_system_clearenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_closedir()

character(len=4096) :: envname
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: ierr
call unit_check_start('system_closedir',msg='test if can read from current directory, assumed non-empty and close and retry')
   call system_opendir('.',dir,ierr)      !--- open directory stream to read from
   call system_readdir(dir,filename,ierr) !--- read directory stream
   call unit_check('system_closedir', filename.ne.'', msg=msg('found a file named',filename))
   call system_closedir(dir,ierr)         !--- close directory stream
   call unit_check('system_closedir', ierr.eq.0, msg=msg('closing gave ierr=',ierr))
   !!!!!!! TRYING BAD OPERATION HANGS SYSTEMS. CANNOT FIND GENERIC TEST TO SEE IF OPEN
   !!call system_readdir(dir,filename,ierr)
   !!call unit_check('system_closedir', ierr.ne.0, msg=msg('try reading now should give error ierr=',ierr))
   !!!!!!!
   call unit_check_done('system_closedir',msg='')
end subroutine test_system_closedir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_cpu_time()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level

real    :: user_start, system_start, total_start
real    :: user_finish, system_finish, total_finish
integer :: i
real    :: value

   call system_cpu_time(total_start,user_start,system_start)

   value=0.0
   do i=1,1000000
      value=sqrt(real(i)+value)
   enddo
   write(*,*)'average sqrt value=',value/1000000.0
   call system_cpu_time(total_finish,user_finish,system_finish)
   write(*,*)'USER ......',user_finish-user_start
   write(*,*)'SYSTEM ....',system_finish-system_start
   write(*,*)'TOTAL .....',total_finish-total_start

   call unit_check_start('system_cpu_time',msg='')
   !!call unit_check('system_cpu_time', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_cpu_time',msg='')
end subroutine test_system_cpu_time
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getcwd()
character(len=:),allocatable :: dirname
character(len=:),allocatable :: hold
integer                      :: ierr
   call unit_check_start('system_getcwd',msg='test system_getcwd(3f) assuming Unix-like file system')
   ! cache current directory so can return
   call system_getcwd(hold,ierr)
   call unit_check('system_getcwd', ierr.eq.0 , msg=msg('checking ierr on getting current directory=',ierr))

   call system_chdir('/tmp',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_getcwd', dirname.eq.'/tmp', msg=msg('checking /tmp to',dirname))

   call system_chdir('/',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_getcwd', dirname.eq.'/', msg=msg('checking / to',dirname))
   ! back to original
   call system_chdir(hold,ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_getcwd', dirname.eq.hold, msg=msg('checking ',hold,' to',dirname))

   call unit_check_done('system_getcwd',msg='')
end subroutine test_system_getcwd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getenv()
character(len=256)           :: var
character(len=256)           :: envname
character(len=:),allocatable :: name
character(len=*),parameter   :: names(*)=[character(len=10)::'USER','HOME','LOGNAME','USERNAME']
integer                      :: i
integer                      :: ierr
   call unit_check_start('system_getenv',msg='')
   do i=1,size(names)
      var=system_getenv(names(i))
      call get_environment_variable(names(i),value=envname, status=ierr)
      call unit_check('system_getenv', envname.eq.var, msg=msg(names(i),var,envname))
   enddo
   call unit_check_done('system_getenv',msg='')
end subroutine test_system_getenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getgrgid()
integer(kind=int64)          :: gid
character(len=:),allocatable :: name
   gid=system_getgid()
   name=system_getgrgid( gid )
   write(*,'("group[",a,"] for ",i0)')name,system_getgid()
   call unit_check_start('system_getgrgid',msg='')
   !!call unit_check('system_getgrgid', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_getgrgid',msg='')
end subroutine test_system_getgrgid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_gethostname()
character(len=:),allocatable :: name
integer                      :: ierr

   call system_gethostname(name,ierr)
   if(ierr.eq.0)then
      write(*,'("hostname[",a,"]")')name
   else
      write(*,'(a)')'ERROR: could not get hostname'
   endif

   call unit_check_start('system_gethostname',msg='')
   !!call unit_check('system_gethostname', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_gethostname',msg='')
end subroutine test_system_gethostname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getlogin()
character(len=80) :: envname
character(len=:),allocatable :: name
integer                      :: ierr
   call unit_check_start('system_getlogin',msg=' test system_getlogin(3f) against environment variable')
   call get_environment_variable("USER",value=envname, status=ierr)
   if(envname.eq.'')then
      call get_environment_variable("LOGNAME",value=envname, status=ierr)
   endif
   if(envname.eq.'')then
      call get_environment_variable("USERNAME",value=envname, status=ierr)
   endif
   if(envname.eq.'')then
      call unit_check_msg('system_getlogin',' did not find username in environment, test invalid')
   else
      name=system_getlogin()
      call unit_check('system_getlogin', name.eq.envname, msg=msg('checking',envname,'versus',name))
   endif
   call unit_check_done('system_getlogin',msg='')
end subroutine test_system_getlogin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getpwuid()
character(len=:),allocatable :: name
integer(kind=int64)          :: uid
   uid=system_getuid()
   name=system_getpwuid(uid)
   write(*,'("login[",a,"] has UID ",i0)')name,uid
   call unit_check_start('system_getpwuid',msg='')
   !!call unit_check('system_getpwuid', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_getpwuid',msg='')
end subroutine test_system_getpwuid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getumask()
integer :: i
   write(*,101)(system_getumask(),i=1,4)
101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
   call unit_check_start('system_getumask',msg='')
   !!call unit_check('system_getumask', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_getumask',msg='')
end subroutine test_system_getumask
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isblk()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'block_device.tst', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a block device? ', system_isblk(names(i))
   enddo
   call unit_check_start('system_isblk',msg='')
   !!call unit_check('system_isblk', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_isblk',msg='')
end subroutine test_system_isblk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_ischr()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'char_dev.test   ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a character device? ', system_ischr(names(i))
   enddo
   call unit_check_start('system_ischr',msg='')
   !!call unit_check('system_ischr', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_ischr',msg='')
end subroutine test_system_ischr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isdir()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/bin/           ', &
      '.               ', &
      'PROBABLY_NOT    ']
logical,parameter           :: expected(*)=[.true., .false., .true., .true., .false.]
logical                     :: answer
   call unit_check_start('system_isdir',msg='')
   do i=1,size(names)
      answer=system_isdir(names(i))
      call unit_check('system_isdir', answer.eqv.expected(i), msg=msg(names(i)))
   enddo
   call unit_check_done('system_isdir',msg='')
end subroutine test_system_isdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isfifo()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'fifo.test       ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a fifo(named pipe)? ', system_isfifo(names(i))
   enddo
   call unit_check_start('system_isfifo',msg='')
   !!call unit_check('system_isfifo', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_isfifo',msg='')
end subroutine test_system_isfifo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_islnk()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'link.test       ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a link? ', system_islnk(names(i))
   enddo
   call unit_check_start('system_islnk',msg='')
   !!call unit_check('system_islnk', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_islnk',msg='')
end subroutine test_system_islnk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isreg()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      'test.txt        ', &
      '.               ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a regular file? ', system_isreg(names(i))
   enddo
   call unit_check_start('system_isreg',msg='')
   !!call unit_check('system_isreg', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_isreg',msg='')
end subroutine test_system_isreg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_issock()

integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'sock.test       ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a socket? ', system_issock(names(i))
   enddo
   call unit_check_start('system_issock',msg='')
   !!call unit_check('system_issock', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_issock',msg='')
end subroutine test_system_issock
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_link()

integer :: ierr
   ierr = system_link('myfile1','myfile2')
   if(ierr.ne.0)then
      call system_perror('*test_system_link*')
   endif
   call unit_check_start('system_link',msg='')
   !!call unit_check('system_link', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_link',msg='')
end subroutine test_system_link
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_mkdir()

integer :: ierr
   call unit_check_start('system_mkdir',msg='make and remove _scratch/')
   ierr=system_mkdir('_scratch',IANY([R_USR,W_USR,X_USR]))
   call unit_check('system_mkdir', ierr.eq.0, msg=msg('make _scratch/, ierr=',ierr))
   call unit_check_msg('system_mkdir',system_isdir('_scratch'),'looks like the directory was made')
   call system_chdir('_scratch',ierr)
   call system_chdir('..',ierr)
   call unit_check_msg('system_mkdir',ierr.eq.0,'looks like it can be entered')
   ierr=system_rmdir('_scratch')
   call unit_check('system_mkdir', ierr.eq.0, msg=msg('remove _scratch/, ierr=',ierr))
   call unit_check_done('system_mkdir',msg='')
end subroutine test_system_mkdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_mkfifo()

integer :: status
   status = system_mkfifo("/home/cnd/mod_done", IANY([W_USR, R_USR, R_GRP, R_OTH]))
   if(status.ne.0)then
      call system_perror('*mkfifo* error:')
   endif
   call unit_check_start('system_mkfifo',msg='')
   !!call unit_check('system_mkfifo', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_mkfifo',msg='')
end subroutine test_system_mkfifo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_opendir()
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: i
integer                      :: ierr
   call unit_check_start('system_opendir',msg='')
   call system_opendir('.',dir,ierr)                                              !--- open directory stream to read from
   call unit_check('system_opendir', ierr.eq.0, msg=msg('checking ierr=',ierr))
   i=0
   do                                                                             !--- read directory stream
      call system_readdir(dir,filename,ierr)
      if(filename.eq.' ')exit
      i=i+1
   enddo
   call system_closedir(dir,ierr)                                                 !--- close directory stream
   call unit_check_done('system_opendir',msg='')
end subroutine test_system_opendir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_perm()

character(len=4096) :: string
integer(kind=int64) :: values(13)
integer             :: ierr
character(len=:),allocatable :: perms
   values=0
   call get_command_argument(1, string)  ! get pathname from command line
   call system_stat(string,values,ierr)  ! get pathname information
   if(ierr.eq.0)then
      perms=system_perm(values(3))       ! convert permit mode to a string
! print permits as a string, decimal value, and octal value
      write(*,'("for ",a," permits[",a,"]",1x,i0,1x,o0)') &
         trim(string),perms,values(3),values(3)
   endif
   call unit_check_start('system_perm',msg='')
   !!call unit_check('system_perm', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_perm',msg='')
end subroutine test_system_perm
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_perror()

character(len=:),allocatable :: DIRNAME
   DIRNAME='/NOT/THERE/OR/ANYWHERE'
! generate an error with a routine that supports errno and perror(3c)
   if(system_rmdir(DIRNAME).ne.0)then
      call system_perror('*test_system_perror*:'//DIRNAME)
   endif
   write(*,'(a)')"That's all Folks!"
   call unit_check_start('system_perror',msg='')
   !!call unit_check('system_perror', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_perror',msg='')
end subroutine test_system_perror
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_putenv()

character(len=4096) :: value
integer             :: ierr
   call unit_check_start('system_putenv',msg='')
!! CHECK NOT_THERE_S_P IS NOT THERE FOR TEST
   call get_environment_variable("NOT_THERE_S_P", status=ierr)
   call unit_check('system_putenv',ierr.eq.1,msg=msg('make sure variable does not exist,status=',ierr))
!! SET THE VARIABLE NOT_THERE_S_P
   call system_putenv('NOT_THERE_S_P=this is the value',ierr)
!! CHECK VARIABLE IS NOW SET
   call unit_check('system_putenv',ierr.eq.0,msg=msg('setting, status should be zero when setting=',ierr))
   call get_environment_variable("NOT_THERE_S_P", value=value,status=ierr)
   call unit_check('system_putenv',ierr.eq.0,msg=msg('status should be zero when getting=',ierr))
   call unit_check('system_putenv',value.eq.'this is the value',msg=msg('value is set to:',value))
!! REPLACE VALUE
   call system_putenv('NOT_THERE_S_P=this is the new value',ierr)
   call unit_check('system_putenv',ierr.eq.0,msg=msg('setting, status should be zero when setting=',ierr))
   call get_environment_variable("NOT_THERE_S_P", value=value,status=ierr)
   call unit_check('system_putenv',ierr.eq.0,msg=msg('status should be zero when getting=',ierr))
   call unit_check('system_putenv',value.eq.'this is the new value',msg=msg('value is set to:',value))
!! DELETE VALUE
   call system_putenv('NOT_THERE_S_P',ierr)
   call get_environment_variable("NOT_THERE_S_P", status=ierr)
   call unit_check('system_putenv',ierr.eq.1,msg=msg('should be gone, varies with different putenv(3c)',ierr))
   call unit_check_msg('system_putenv','system_unsetenv(3f) is a better way to remove variables')
!!
   call unit_check_done('system_putenv',msg='')
end subroutine test_system_putenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_readdir()

type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: i
integer                      :: ierr
character(len=256)           :: message
integer                      :: ios
integer                      :: lun
logical                      :: found1,found2
   call unit_check_start('system_readdir',msg='make some scratch files and look for their name in current directory')
   found1=.false.
   found2=.false.
!--- create two scratch files of known names

   open(newunit=lun,file='__scratch_1__',iostat=ios,iomsg=message)
   if(ios.eq.0)then
      write(lun,*)'SCRATCH FILE 1'
   else
      call unit_check_msg('system_readdir','error:',message)
   endif
   close(unit=lun,iostat=ios,iomsg=message)

   open(newunit=lun,file='__scratch_2__',iostat=ios,iomsg=message)
   if(ios.eq.0)then
      write(lun,*)'SCRATCH FILE 2'
   else
      call unit_check_msg('system_readdir','error:',message)
   endif
   close(unit=lun,iostat=ios,iomsg=message)

!--- open directory stream to read from
   call system_opendir('.',dir,ierr)
   call unit_check('system_opendir', ierr.eq.0, msg=msg('system_opendir ierr=',ierr))
!--- read directory stream and look for scratch file names
      do
         call system_readdir(dir,filename,ierr)
         if(filename.eq.' ') exit
         call unit_check('system_readdir', ierr.eq.0, msg=msg('system_readdir ierr=',ierr,'filename=',filename))
         if(ierr.ne.0) exit
         if(filename.eq.'__scratch_1__')found1=.true.
         if(filename.eq.'__scratch_2__')found2=.true.
      enddo
!--- close directory stream
   call system_closedir(dir,ierr)
   call unit_check('system_readdir', ierr.eq.0, msg=msg('system_closedir ierr=',ierr))

   call unit_check('system_readdir', found1, msg=msg('__scratch__1',found1))
   call unit_check('system_readdir', found2, msg=msg('__scratch__2',found2))

!--- remove scratch files
   open(newunit=lun,file='__scratch_1__',iostat=ios,iomsg=message)
   close(unit=lun,iostat=ios,iomsg=message,status='delete')
   open(newunit=lun,file='__scratch_2__',iostat=ios,iomsg=message)
   close(unit=lun,iostat=ios,iomsg=message,status='delete')

   call unit_check_done('system_readdir',msg='')
end subroutine test_system_readdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_readenv()
character(len=:),allocatable :: string
integer                      :: i
integer                      :: ierr
character(len=:),allocatable :: home
character(len=4096)          :: envname
   call unit_check_start('system_readenv',msg='assuming system always has environment variable HOME set')
   i=0
   home=''
   ! read environment table and look for HOME= at beginning of line
   call system_initenv()
   do
      string=system_readenv()
      if(index(string,'HOME=').eq.1)then
        home=string
      endif
      if(string.eq.'')then
         exit
      else
         i=i+1
      endif
   enddo
   call get_environment_variable("HOME",value=envname, status=ierr)
   envname='HOME='//trim(envname)
   call unit_check('system_readenv',home.eq.envname, msg=msg('HOME',home,envname))
   call unit_check_done('system_readenv',msg='')
end subroutine test_system_readenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_remove()
character(len=*),parameter :: FILE='__MyJunkFile.txt'
integer                    :: ierr
integer                    :: ios
character(len=256)         :: message
   call unit_check_start('system_remove',msg='')
   ierr=system_remove(FILE) ! note intentionally causes error if file exists
   open(unit=10,file=FILE,iostat=ios,status='NEW')
   if(ios.eq.0)then
      write(10,'(a)',iostat=ios)'This is a file to be deleted by the test of system_remove(3f)'
      close(unit=10,iostat=ios)
      call unit_check('system_remove',system_isreg(FILE),msg='checking if test file exists before remove')
   else
      call unit_check('system_remove', ios.eq.0, msg=msg('bad I/O IOSTAT=',ios,message))
   endif
   ierr=system_remove(FILE)
   call unit_check('system_remove', ierr.eq.0, msg=msg('checking return code',ierr))
   call unit_check('system_remove',.not.system_isreg(FILE),msg='checking if test file exists after remove')
   call unit_check('system_remove',.not.system_access(FILE,F_OK),msg='checking if test file exists after remove')
   call unit_check_done('system_remove',msg='')
end subroutine test_system_remove
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rename()

character(len=256) :: string
character(len=256) :: message
integer            :: ios
integer            :: ierr
   call unit_check_start('system_rename',msg='check system_rename(3f) renaming "_scratch_file_" to "_renamed_scratch_file_"')
   message=''
! try to remove junk files just in case
   ierr=system_remove('_scratch_file_')
   ierr=system_remove('_renamed_scratch_file_')
! create scratch file to rename
   close(unit=10,iostat=ios,status='delete')
   open(unit=10,file='_scratch_file_',status='new',iostat=ios)
   call unit_check('system_rename', ios.eq.0, msg=msg('message from OPEN(3f) is:',message,' ios is',ios))
   write(10,'(a)',iostat=ios,iomsg=message) 'IF YOU SEE THIS RENAME WORKED'
   close(unit=10)
! rename scratch file
   ierr=system_rename('_scratch_file_','_renamed_scratch_file_')
   call unit_check('system_rename', ierr.eq.0, msg=msg('ierr',ierr))
! read renamed file
   open(unit=11,file='_renamed_scratch_file_',status='old')
   read(11,'(a)',iostat=ios)string
   call unit_check('system_rename', ios.eq.0, msg=msg('ios',ierr))
   call unit_check('system_rename', string.eq.'IF YOU SEE THIS RENAME WORKED', msg=msg(string))
   close(unit=11)
! clean up
   ierr=system_remove('_scratch_file_')
   call unit_check('system_rename', ierr.ne.0, msg=msg('cleanup',ierr))
   ierr=system_remove('_renamed_scratch_file_')
   call unit_check('system_rename', ierr.eq.0, msg=msg('cleanup',ierr))
   call unit_check_done('system_rename',msg='')
end subroutine test_system_rename
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rewinddir()
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: sum(2)
integer                      :: i
integer                      :: j
integer                      :: ierr
   call unit_check_start('system_rewinddir',msg='')
   call system_opendir('.',dir,ierr)                   !>>> open directory stream to read from
   do i=1,2                                            !>>> read directory stream twice
      j=0
      do
         call system_readdir(dir,filename,ierr)
         if(filename.eq.' ')exit
         j=j+1
      enddo
      sum(i)=j
      call system_rewinddir(dir)                       !>>> rewind directory stream
   enddo
   call system_closedir(dir,ierr)                      !>>> close directory stream
   call unit_check('system_rewinddir', sum(1).eq.sum(2), msg=msg('number of files','PASS 1:',sum(1),'PASS 2:',sum(2)))
   call unit_check_done('system_rewinddir',msg='')
end subroutine test_system_rewinddir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rmdir()

   integer :: ierr
   character(len=*),parameter :: dirname='_scratch_rmdir'
!! setup
   call unit_check_start('system_rmdir',msg='')
   if(system_isdir(dirname))then ! TRY TO CREATE
      call unit_check_msg('system_rmdir',dirname,'directory existed')
   endif
   ierr=system_mkdir(dirname,RWX_U)
   call unit_check('system_rmdir',ierr.eq.0,msg=msg('try to create',dirname))
   call unit_check('system_rmdir',system_isdir(dirname),msg=msg('check if',dirname,'exists and is a directory'))
!! test
   ierr=system_rmdir(dirname) ! TRY TO REMOVE
   call unit_check('system_rmdir',ierr.eq.0,msg=msg('check ierr',ierr))
   call unit_check('system_rmdir',.not.system_isdir(dirname),msg=msg('check if',dirname,'is still a directory'))

   if(system_isdir(dirname))then
      call unit_check_bad('system_rmdir',msg=msg('testing went bad,',dirname,'is still a directory'))
   else
      ierr=system_rmdir(dirname) ! TRY TO REMOVE scratch directory when it should be gone
      call unit_check('system_rmdir',ierr.ne.0,msg=msg('check ierr',ierr))
      call system_perror('*test of system_rmdir*')
   endif

   call unit_check_done('system_rmdir',msg='')
end subroutine test_system_rmdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_setumask()

integer :: newmask
integer :: old_umask
integer :: i
   write(*,101)(system_getumask(),i=1,4)
101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
   newmask=63
   old_umask=system_setumask(newmask)
   write(*,*)'NEW'
   write(*,101)(system_getumask(),i=1,4)
   call unit_check_start('system_setumask',msg='')
   !!call unit_check('system_setumask', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_setumask',msg='')
end subroutine test_system_setumask
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_stat()

use M_time, only :   fmtdate, u2d

integer(kind=int64)  :: buff(13)
integer(kind=int32)  :: status
character(len=*),parameter :: fmt_date='year-month-day hour:minute:second'
integer(kind=int64)  :: &
   Device_ID,           Inode_number,          File_mode,                  Number_of_links,  Owner_uid,         &
   Owner_gid,           Directory_device,      File_size,                  Last_access,      Last_modification, &
   Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
equivalence                                    &
   ( buff(1)  , Device_ID                  ) , &
   ( buff(2)  , Inode_number               ) , &
   ( buff(3)  , File_mode                  ) , &
   ( buff(4)  , Number_of_links            ) , &
   ( buff(5)  , Owner_uid                  ) , &
   ( buff(6)  , Owner_gid                  ) , &
   ( buff(7)  , Directory_device           ) , &
   ( buff(8)  , File_size                  ) , &
   ( buff(9)  , Last_access                ) , &
   ( buff(10) , Last_modification          ) , &
   ( buff(11) , Last_status_change         ) , &
   ( buff(12) , Preferred_block_size       ) , &
   ( buff(13) , Number_of_blocks_allocated )

   CALL SYSTEM_STAT("/etc/hosts", buff, status)

   if (status == 0) then
      write (*, FMT="('Device ID(hex/decimal):',      T30, Z0,'h/',I0,'d')") buff(1),buff(1)
      write (*, FMT="('Inode number:',                T30, I0)") buff(2)
      write (*, FMT="('File mode (octal):',           T30, O19)") buff(3)
      write (*, FMT="('Number of links:',             T30, I0)") buff(4)
      write (*, FMT="('Owner''s uid/username:',       T30, I0,1x, A)") buff(5), system_getpwuid(buff(5))
      write (*, FMT="('Owner''s gid/group:',          T30, I0,1x, A)") buff(6), system_getgrgid(buff(6))
      write (*, FMT="('Device where located:',        T30, I0)") buff(7)
      write (*, FMT="('File size(bytes):',            T30, I0)") buff(8)
      write (*, FMT="('Last access time:',            T30, I0,1x, A)") buff(9), fmtdate(u2d(int(buff(9))),fmt_date)
      write (*, FMT="('Last modification time:',      T30, I0,1x, A)") buff(10),fmtdate(u2d(int(buff(10))),fmt_date)
      write (*, FMT="('Last status change time:',     T30, I0,1x, A)") buff(11),fmtdate(u2d(int(buff(11))),fmt_date)
      write (*, FMT="('Preferred block size(bytes):', T30, I0)") buff(12)
      write (*, FMT="('No. of blocks allocated:',     T30, I0)") buff(13)
   endif

   call unit_check_start('system_stat',msg='')
   !!call unit_check('system_stat', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_stat',msg='')
end subroutine test_system_stat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_uname()

integer,parameter          :: is=100
integer                    :: i
character(len=*),parameter :: letters='srvnmxT'
character(len=is)          :: string=' '

   write(*,'(80("="))')
   do i=1,len(letters)
      call system_uname(letters(i:i),string)
      write(*,*)'=====> TESTING system_uname('//letters(i:i)//')--->'//trim(string)
   enddo
   write(*,'(80("="))')
   call unit_check_start('system_uname',msg='')
   !!call unit_check('system_uname', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_uname',msg='')
end subroutine test_system_uname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_unlink()

integer :: ierr
   ierr = system_unlink('myfile1')
   if(ierr.ne.0)then
      call system_perror('*test_system_unlink*')
   endif
   call unit_check_start('system_unlink',msg='')
   !!call unit_check('system_unlink', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_unlink',msg='')
end subroutine test_system_unlink
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_unsetenv()

integer :: ierr
character(len=4096) :: value

   call system_unsetenv('GRU')

   call unit_check_start('system_unsetenv',msg='')

!! SET THE VARIABLE NOT_THERE_S_U
   call set_environment_variable('NOT_THERE_S_U','this is the value',ierr)
!! CHECK VARIABLE IS NOW SET
   call get_environment_variable("NOT_THERE_S_U", value=value,status=ierr)
   call unit_check('system_unsetenv',ierr.eq.0,msg=msg('status should be zero when getting=',ierr))
   call unit_check('system_unsetenv',value.eq.'this is the value',msg=msg('value is set to:',value))
   !! REMOVE
   call system_unsetenv('NOT_THERE_S_U',ierr)
   call unit_check('system_unsetenv',ierr.eq.0,msg=msg('should be zero ierr=',ierr))
   !! CHECK IF GONE
   call get_environment_variable("NOT_THERE_S_U", value=value,status=ierr)
   call unit_check('system_unsetenv',ierr.eq.1,msg=msg('should be zero ierr=',ierr))
   call unit_check('system_unsetenv',value.eq.' ',msg=msg('value should be blank=',value))

   call unit_check_done('system_unsetenv',msg='')

end subroutine test_system_unsetenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_memcpy()
   call unit_check_start('system_memcpy',msg='')
   !!call unit_check('system_memcpy', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_memcpy',msg='')
end subroutine test_system_memcpy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_utime()
character(len=4096) :: pathname
integer             :: times(2)
integer             :: i
   call unit_check_start('system_utime',msg='')
   do i=1,command_argument_count()
      call get_command_argument(i, pathname)
      if(.not.system_utime(pathname,times))then
         call system_perror('*test_system_utime*')
      endif
   enddo
   !!call unit_check('system_utime', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('system_utime',msg='')
end subroutine test_system_utime
!===================================================================================================================================
end subroutine test_suite_M_system
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_system
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
