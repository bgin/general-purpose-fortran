!>
!!##NAME
!!    path(3f) - [M_path] OOP interface for a GNU Linux or Unix pathname
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    type path
!!
!!       ! COMPONENTS:
!!       character(len=:),allocatable :: name
!!    contains
!!
!!       ! METHODS:
!!       procedure    :: branch
!!       procedure    :: leaf
!!       procedure    :: stem
!!       procedure    :: bud
!!       procedure    :: init
!!       procedure    :: is_dir
!!       procedure    :: stat
!!       procedure    :: readable
!!       procedure    :: writable
!!       procedure    :: executable
!!       procedure    :: exists
!!       procedure    :: realpath
!!
!!       ! OVERLOADED OPERATORS FOR TYPE(path)
!!       procedure,private :: eq
!!       generic           :: operator(==)  => eq
!!    end type path
!!
!!##OPTIONS
!!
!!        FILENAME  pathname
!!
!!##RETURNS
!!    %branch()  Output FILENAME with its last non-slash component and trailing
!!               slashes removed. if FILENAME contains no '/' character, output
!!               '.' (meaning the current directory).
!!
!!               Assumes leaf separator is a slash ('/') and that filename does
!!               not contain trailing spaces.
!!    %leaf()    Output FILENAME with anything up to and including the right-most
!!               slashes removed.
!!    %stem()    Output FILENAME leaf with any right-most suffix removed. A suffix
!!               is anything from the rightmost period in the filename leaf to the
!!               end of the pathname.
!!    %bud()     Output FILENAME right-most suffix.
!!    %is_dir()  a logical specifying if path is currently a directory pathname.
!!    %stat()    an array of integers describing the current status of the file.
!!               returns the same data array as the SYSTEM_STAT(3f) function with
!!               the difference that the status of the call is element 14.
!!    %readable()    determine if file is readable
!!    %writable()    determine if file is writable
!!    %executable()  determine if file is executable
!!    %exists()      determine if file exists
!!    %realpath()    resolve the pathname using the Posix C routine realpath(3c)
!!
!!            Assumes leaf separator is a slash ('/') and that filename does
!!            not contain trailing spaces.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_path
!!    use M_path, only   : path
!!    use M_system, only : system_getpwuid, system_getgrgid
!!    use M_time,   only : fmtdate, u2d
!!    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
!!    character(len=*),parameter               :: fmt_date='year-month-day hour:minute:second'
!!    type(path)                               :: file
!!    character(len=:),allocatable             :: filename
!!    integer(kind=int64)                      :: buff(14)
!!    integer                                  :: i
!!       do i = 1 , command_argument_count()
!!          call getname(i,filename)
!!
!!          file%name=filename
!!          ! or
!!          call file%init(filename)
!!
!!          deallocate(filename)
!!          !
!!          write(*,*)'name........ ',file%name
!!          write(*,*)'branch...... ',file%branch()
!!          write(*,*)'leaf........ ',file%leaf()
!!          write(*,*)'stem........ ',file%stem()
!!          write(*,*)'bud......... ',file%bud()
!!          write(*,*)'is_dir...... ',file%is_dir()
!!          write(*,*)'readable.... ',file%readable()
!!          write(*,*)'writable.... ',file%writable()
!!          write(*,*)'executable.. ',file%executable()
!!          write(*,*)'exists...... ',file%exists()
!!          write(*,*)'realpath.... ',file%realpath()
!!          write(*,*)'stat........ '
!!          buff=file%stat()
!!          if(buff(14) == 0) then
!!             write (*, FMT="(9x,'Device ID(hex/decimal):',      T40, Z0,'h/',I0,'d')") buff(1),buff(1)
!!             write (*, FMT="(9x,'Inode number:',                T40, I0)") buff(2)
!!             write (*, FMT="(9x,'File mode (octal):',           T40, O19)") buff(3)
!!             write (*, FMT="(9x,'Number of links:',             T40, I0)") buff(4)
!!             write (*, FMT="(9x,'Owner''s uid/username:',       T40, I0,1x, A)") buff(5), system_getpwuid(buff(5))
!!             write (*, FMT="(9x,'Owner''s gid/group:',          T40, I0,1x, A)") buff(6), system_getgrgid(buff(6))
!!             write (*, FMT="(9x,'Device where located:',        T40, I0)") buff(7)
!!             write (*, FMT="(9x,'File size(bytes):',            T40, I0)") buff(8)
!!             write (*, FMT="(9x,'Last access time:',            T40, I0,1x, A)") buff(9), fmtdate(u2d(int(buff(9))),fmt_date)
!!             write (*, FMT="(9x,'Last modification time:',      T40, I0,1x, A)") buff(10),fmtdate(u2d(int(buff(10))),fmt_date)
!!             write (*, FMT="(9x,'Last status change time:',     T40, I0,1x, A)") buff(11),fmtdate(u2d(int(buff(11))),fmt_date)
!!             write (*, FMT="(9x,'Preferred block size(bytes):', T40, I0)") buff(12)
!!             write (*, FMT="(9x,'No. of blocks allocated:',     T40, I0)") buff(13)
!!          else
!!             write (*,*) '*path%stat* error: ',file%name,' status= ',status
!!          endif
!!          write(*,*)
!!       enddo
!!
!!    contains
!!    subroutine getname(i,fn)
!!    integer,intent(in)                       :: i
!!    character(len=:),allocatable,intent(out) :: fn
!!    integer                                  :: fn_length
!!    ! get pathname from command line arguments
!!       call get_command_argument (i , length=fn_length)
!!       allocate(character(len=fn_length) :: fn)
!!       call get_command_argument (i , value=fn)
!!    end subroutine getname
!!    end program demo_path
!!   Results:
!!
!!   Sample program executions:
!!
!!     demo_path $HOME $HOME/.bashrc
!!
!!     name........ /home/urbanjs/V600
!!     branch...... /home/urbanjs
!!     leaf........ V600
!!     stem........ V600
!!     bud.........
!!     is_dir......  T
!!     readable....  T
!!     writable...   T
!!     executable..  T
!!     exists......  T
!!     realpath....  /home/urbanjs/V600
!!     stat........
!!             Device ID(hex/decimal):       3E6BE045h/1047257157d
!!             Inode number:                 281474977443215
!!             File mode (octal):                          40700
!!             Number of links:              1
!!             Owner's uid/username:         197609 JSU
!!             Owner's gid/group:            197121 None
!!             Device where located:         0
!!             File size(bytes):             0
!!             Last access time:             1559495702 2019-06-02 13:15:02
!!             Last modification time:       1559495702 2019-06-02 13:15:02
!!             Last status change time:      1559495702 2019-06-02 13:15:02
!!             Preferred block size(bytes):  65536
!!             No. of blocks allocated:      92
!!
!!     name........ /home/urbanjs/V600/.bashrc
!!     branch...... /home/urbanjs/V600
!!     leaf........ .bashrc
!!     stem........ .bashrc
!!     bud.........
!!     is_dir......  F
!!     readable....  T
!!     writable...   T
!!     executable..  T
!!     exists......  T
!!     realpath....  /home/urbanjs/V600/.bashrc
!!     stat........
!!             Device ID(hex/decimal):       3E6BE045h/1047257157d
!!             Inode number:                 59672695062674129
!!             File mode (octal):                         100744
!!             Number of links:              1
!!             Owner's uid/username:         197609 JSU
!!             Owner's gid/group:            4294967295 Unknown+Group
!!             Device where located:         0
!!             File size(bytes):             8744
!!             Last access time:             1434310665 2015-06-14 15:37:45
!!             Last modification time:       1533428694 2018-08-04 20:24:54
!!             Last status change time:      1533428694 2018-08-04 20:24:54
!!             Preferred block size(bytes):  65536
!!             No. of blocks allocated:      12
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! submodule is not supported by the compiler used to develop this yet or it would be worth a try
!!submodule (M_io) M_path
!!end submodule M_path
!
module M_path
!
! Define an OOP (Object-Oriented Programming) interface for common pathname-related operations
!
! Leveraging the existing procedural functions to do the operations allows
! this to simply be a definition of a derived type ( TYPE(path) ) and the
! methods it supports and overloading of operators to support the new data type.
!
use M_io,     only : dirname   ! strip last component from filename
use M_io,     only : splitpath ! split a Unix pathname into components
Use M_system, only : system_isdir
use M_system, only : system_stat, system_realpath, system_perror
Use M_system, only : system_access, F_OK, R_OK, W_OK, X_OK
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use,intrinsic :: iso_fortran_env, only : dp=>real128

implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------
   public path
   public test_suite_M_path
!-----------------------------------------------------------------------------------------------------------------------------------
!DERIVED TYPE FILENAME
!
type path
   ! COMPONENTS:
   character(len=:),allocatable :: name
contains
   ! METHODS:
   procedure          :: branch
   procedure          :: leaf
   procedure          :: stem   ! leaf - prefix
   procedure          :: bud
   procedure          :: init       => init_path
   procedure          :: is_dir     => path_isdir
   procedure          :: stat       => path_stat
   procedure          :: exists     => path_exists
   procedure          :: readable   => path_readable
   procedure          :: writable  => path_writable
   procedure          :: executable => path_executable
   procedure          :: realpath  => path_realpath
   !procedure         :: group
   !procedure         :: permits
   !procedure         :: owner
   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(path)

!   procedure,private :: plus_strings
!   generic           :: operator(+) => plus_strings
!
!-! procedure         :: construct_from_dat
!-! generic           :: assignment(=)  => construct_from_dat

!   procedure,private :: minus_seconds
!   procedure,private :: minus_path
!   generic           :: operator(-)  => minus_seconds
!   generic           :: operator(-)  => minus_path

!   procedure,private :: eq
!   generic           :: operator(==)  => eq
end type path
!===================================================================================================================================
! User-defined constructors are created by defining a generic interface
! with the same name as the derived type they're supposed to construct.
interface path
   module procedure construct_from_dat
end interface path
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this function is used internally in the module, but is also declared to be a constructor for creating TYPE(DATE_TYPE) structures
!
function construct_from_dat(dat)
character(len=*),parameter::ident_1="@(#)M_io::construct_from_dat(3f): construct TYPE(path) with DAT date-time array"
character(len=*),intent(in)          :: dat
type(path)                      :: construct_from_dat
   construct_from_dat%name=dat
end function construct_from_dat
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! DEFINE THE METHODS FOR THE TYPE
! These functions are privately used to define the methods that TYPE(path) will support
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine init_path(self,name)
!
! allow for path object to be initialized. Default is xxxxxxxxxxxxxx
!
class(path)                     :: self
character(len=*),intent(in),optional :: name
   if(present(name))then
      self%name=name
   endif
end subroutine init_path
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! FUNCTIONS
!===================================================================================================================================
function branch(self) result (dirnm)
use M_io, only : dirname
class(path),intent(in)    :: self
!type(path)                :: dirnm
character(len=:),allocatable   :: dirnm
   dirnm=dirname(self%name)
end function branch
!===================================================================================================================================
function leaf(self) result (name)
use M_io, only : dirname
class(path),intent(in)    :: self
character(len=:),allocatable   :: name
integer,parameter :: maxlen=4096
character(len=maxlen)  :: split_dir, split_name, split_basename, split_ext
   call splitpath(self%name, split_dir, split_name, split_basename, split_ext)
   name=trim(split_name)
end function leaf
!===================================================================================================================================
function stem(self) result (basename)
class(path),intent(in)    :: self
character(len=:),allocatable   :: basename
integer,parameter :: maxlen=4096
character(len=maxlen)  :: split_dir, split_name, split_basename, split_ext
   call splitpath(self%name, split_dir, split_name, split_basename, split_ext)
   basename=trim(split_basename)
end function stem
!===================================================================================================================================
function bud(self) result (ext)
class(path),intent(in)    :: self
character(len=:),allocatable   :: ext
integer,parameter :: maxlen=4096
character(len=maxlen)  :: split_dir, split_name, split_basename, split_ext
   call splitpath(self%name, split_dir, split_name, split_basename, split_ext)
   ext=trim(split_ext)
end function bud
!===================================================================================================================================
function path_realpath(self) result (fullname)
class(path),intent(in)        :: self
character(len=:),allocatable  :: fullname
   fullname=system_realpath(self%name)
!!   hangs gfortran if the function is called from an I/O statement
!!   if(fullname.eq.char(0))then
!!      call system_perror('*path_realpath* error for pathname '//trim(self%name)//':')
!!   endif
end function path_realpath
!===================================================================================================================================
function path_stat(self) result (buff)
class(path),intent(in)    :: self
integer(kind=int64)       :: buff(14)
integer(kind=int32)       :: status
   call system_stat(self%name, buff(1:13), status)
   buff(14)=int(status,kind=int64)
end function path_stat
!===================================================================================================================================
function path_readable(self) result (truth)
logical                   :: truth
class(path),intent(in)    :: self
   truth=system_access(self%name,R_OK)
end function path_readable
!===================================================================================================================================
function path_writable(self) result (truth)
logical                   :: truth
class(path),intent(in)    :: self
   truth=system_access(self%name,W_OK)
end function path_writable
!===================================================================================================================================
function path_executable(self) result (truth)
logical                   :: truth
class(path),intent(in)    :: self
   truth=system_access(self%name,X_OK)
end function path_executable
!===================================================================================================================================
function path_exists(self) result (truth)
logical                   :: truth
class(path),intent(in)    :: self
   truth=system_access(self%name,F_OK)
end function path_exists
!===================================================================================================================================
function path_isdir(self) result (truth)
class(path),intent(in)    :: self
logical                   :: truth
   truth=system_isdir(self%name)
end function path_isdir
!===================================================================================================================================
logical function eq(self,other)
character(len=*),parameter::ident_2="@(#)M_io::eq(3f): compare derived type path objects (eq,lt,gt,le,ge,ne)"
class(path),intent(in)   :: self
type(path),intent(in)    :: other
   eq=.true.  !! PLACEHOLDER
end function eq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_suite_M_path()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
!! setup
   if(unit_check_level.ne.0)then
     write(*,*)'TEST_SUITE_M_PATH'
   endif
   call test_branch()
   call test_bud()
   call test_construct_from_dat()
   call test_init_path()
   call test_leaf()
   call test_path_executable()
   call test_path_exists()
   call test_path_isdir()
   call test_path_readable()
   call test_path_realpath()
   call test_path_stat()
   call test_path_writable()
   call test_stem()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_branch()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
   call unit_check_start('branch',msg='')
   call unit_check('branch', file%branch().eq.'/home/user/', msg=msg(file%name,file%branch()))
   call unit_check_done('branch',msg='')
end subroutine test_branch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bud()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
   call unit_check_start('bud',msg='')
   call unit_check('bud', file%bud().eq.'.txt', msg=msg(file%name,file%bud()))
   call unit_check_done('bud',msg='')
end subroutine test_bud
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_construct_from_dat()
implicit none
   type(path)                               :: file
   file%name='/home/user/file.txt'
   call unit_check_start('construct_from_dat',msg='')
   call unit_check('construct_from_dat', file%name.eq.'/home/user/file.txt', msg=msg('/home/user/file.txt',file%name))
   call unit_check_done('construct_from_dat',msg='')
end subroutine test_construct_from_dat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_path()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
   call unit_check_start('init_path',msg='')
   call unit_check('init_path', file%name.eq.'/home/user/file.txt', msg=msg('/home/user/file.txt',file%name))
   call unit_check_done('init_path',msg='')
end subroutine test_init_path
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_leaf()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
   call unit_check_start('leaf',msg='')
   call unit_check('leaf', file%leaf().eq.'file.txt', msg=msg(file%name,file%leaf()))
   call unit_check_done('leaf',msg='')
end subroutine test_leaf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_executable()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'executable.. ',file%executable()
   call unit_check_start('path_executable',msg='')
   !!call unit_check('path_executable', 0.eq.0, msg=msg(file%name,file%executable()))
   call unit_check_done('path_executable',msg='')
end subroutine test_path_executable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_exists()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'exists...... ',file%exists()
   call unit_check_start('path_exists',msg='')
   !!call unit_check('path_exists', 0.eq.0, msg=msg(file%name,file%exists()))
   call unit_check_done('path_exists',msg='')
end subroutine test_path_exists
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_isdir()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'is_dir...... ',file%is_dir()
   call unit_check_start('path_isdir',msg='')
   !!call unit_check('path_isdir', 0.eq.0, msg=msg(file%name,file%is_dir()))
   call unit_check_done('path_isdir',msg='')
end subroutine test_path_isdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_readable()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'readable.... ',file%readable()
   call unit_check_start('path_readable',msg='')
   !!call unit_check('path_readable', 0.eq.0, msg=msg(file%name,file%readable()))
   call unit_check_done('path_readable',msg='')
end subroutine test_path_readable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_realpath()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'realpath.... ',file%realpath()
   call unit_check_start('path_realpath',msg='')
   !!call unit_check('path_realpath', 0.eq.0, msg=msg(file%name,file%realpath()))
   call unit_check_done('path_realpath',msg='')
end subroutine test_path_realpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_stat()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
   call unit_check_start('path_stat',msg='')
   !!call unit_check('path_stat', 0.eq.0, msg=msg(file%name,100))
   call unit_check_done('path_stat',msg='')
end subroutine test_path_stat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_writable()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'writable.... ',file%writable()
   call unit_check_start('path_writable',msg='')
   !!call unit_check('path_writable', 0.eq.0, msg=msg(file%name,file%writeable()))
   call unit_check_done('path_writable',msg='')
end subroutine test_path_writable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stem()
implicit none
   type(path)                               :: file
   call file%init('/home/user/file.txt')
   call unit_check_start('stem',msg='')
   call unit_check('stem', file%stem().eq.'file', msg=msg(file%name,file%stem()))
   call unit_check_done('stem',msg='')
end subroutine test_stem
!===================================================================================================================================
end subroutine test_suite_M_path
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_path
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
