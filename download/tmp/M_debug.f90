!>
!!##NAME
!!    M_debug(3fm) - [M_debug] a collection of Fortran routines for supporting code development by
!!                   providing error processing, debugging procedures and unit testing.
!!##SYNOPSIS
!!
!!    use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
!!    use M_debug, only : unit_check_limit, unit_check_keep_going, unit_check_command
!!    use M_debug, only : unit_check_msg, msg
!!    use M_debug, only : debug
!!    use M_debug, only : fstop
!!    use M_debug, only : stderr
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##DESCRIPTION
!!
!!    The M_debug(3fm) Fortran module provides procedures and variables useful
!!    for providing error processing, debugging capabilities, and unit testing.
!!
!!     o allows for a user-defined command to be called to collect results.
!!     o supports different levels
!!     o allows stopping on first failure or continuing
!!
!!    UNIT TESTS
!!    unit_check_start(3f)   call command "goodbad NAME start ..."
!!    unit_check(3f)         if expression is .F. call command "goodbad
!!                           NAME bad" and stop program (by default)
!!
!!    unit_check_done(3f)    call command "goodbad NAME good" if no failures
!!                           else call "goodbad NAME bad"
!!    unit_check_good(3f)    call command "goodbad NAME good"
!!    unit_check_bad(3f)     call command "goodbad NAME bad" and stop program
!!                           (by default)
!!    unit_check_msg(3f)     write message
!!    msg(3f)                concatenate values into a string for building
!!                           messages
!!    unit_check_keep_going  logical variable that can be used to turn off
!!                           program termination on errors.
!!    unit_check_level       An integer that can be used to specify different debug
!!                           levels
!!    unit_check_command     name of command to execute. Defaults to the name
!!                           "goodbad".
!!
!!    BASIC DEBUGGING
!!    fstop(3f)             calls 'STOP VALUE' passing in a value (1-32), with
!!                          optional message
!!    pdec(3f)              write ASCII Decimal Equivalent (ADE) numbers vertically
!!                          beneath string
!!    stderr(3f)            Write message on stderr
!!    debug                 logical variable that can be tested by routines as a
!!                          flag to process debug statements.
!!
!!    For unit testing, the existence of a command called "goodbad" is
!!    initially assumed. This is generally a script that makes entries
!!    for each unit in an SQLite data file which is then used to create
!!    CSV and HTML reports on the status of each unit. A sample goodbad(1)
!!    command written in the bash(1) shell and using the sqlite3(1) command
!!    should be included in this distribution.
!!
!!    The flexibility introduced by calling an external script or program
!!    is that The goodbad(1) command can be changed as desired to write CSV
!!    files or simple logs or to notify developers with e-mail as desired.
!!
!!    RELATED FUNCTIONS
!!
!!    The routines in M_debug(3f) are often combined with the M_hashkeys(3fm)
!!    routines and various math and statistical routines to quickly create
!!    unit tests.
!!
!!    Comparisions of real values can be done with a tolerance with
!!    M_Compare_Float_Numbers(3fm), for example.
!!
!!    The intrinsics ANY(3f) and ALL(3f) are particularly useful in calls
!!    to unit_check(3f).
!!
!!##EXAMPLES
!!
!!
!! Sample program
!!
!!     !!program demo_unit_tests
!!     module M_demo
!!     private
!!     public one !! regular routines
!!     public two !! regular routines
!!     public test_suite_M_demo !! special name for use with test_suite(1bash).
!!     contains
!!
!!     !!  regular routines
!!     subroutine one()
!!     end subroutine one
!!
!!     subroutine two()
!!     end subroutine two
!!
!!     !! unit test
!!     subroutine test_suite_M_demo
!!     use M_debug, only: unit_check_start, unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad, unit_check_done
!!     use M_debug, only: unit_check_msg, msg
!!     implicit none
!!     integer :: i, j, k
!!     integer,allocatable :: array(:)
!!     integer :: arr(4)=[21,51,14,45]
!!     integer :: a=21, b=51, c=14, d=45
!!     i=1
!!     j=2
!!     k=3
!!     array=[10,20,30,40,50,60,70]
!!     call test_one()
!!     call test_two()
!!     contains
!!
!!     subroutine test_one()
!!     !  register an entry for specified name ("one") in database with status of zero (0)
!!     call unit_check_start('one')
!!
!!     !  if mask test fails, change database status for specified entry to -1 and stop program, else continue
!!     call unit_check('one',i.gt.0,msg='I > 0')
!!
!!
!!     call unit_check('one',all([i,j,k].gt.0),      'testing if everyone greater than zero')
!!     ! display message built of scalars as well
!!     call unit_check('one',all(.not.[i,j,k].eq.4),msg=msg(i,j,k,'testing if no one is equal to four'))
!!
!!     ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
!!     if(i+j+k.lt.1)then
!!        call unit_check_bad('one')
!!     endif
!!
!!     call unit_check_done('one','checks on "one" ended')
!!     end subroutine test_one
!!
!!     subroutine test_two
!!     ! use of all(3f), any(3f), merge(3f) can be useful
!!     ! if you know what these would produce
!!     ! write(*,*)['A','X','X','X','X','B'].eq.'B'      ! this would return an array, the last element having the value T, else F
!!     ! write(*,*)all(['A','X','X','X','X','X'].eq.'X') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','X'].eq.'B') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','B'].eq.'B') ! this would return T
!!     ! write(*,*).not.all(array.lt.100)
!!     ! write(*,*)all(array.lt.100)
!!     ! write(*,*)all([a,b,c,d].eq.[21,51,14,45]) ! compare a list. This would return T
!!     ! write(*,*)all(arr.eq.[21,51,14,45])       ! compare an array. This would return T
!!     ! you know how valuable ANY(3f) and ALL(3f) will be
!!     call unit_check_start('two','check on "two" passed')
!!     call unit_check('two', 1.gt.0 .and. abs(10.10000-10.10001).lt.0.0001,msg='two looks good')
!!     call unit_check_done('two','checks on "two" ended')
!!     end subroutine test_two
!!
!!     end subroutine test_suite_M_demo
!!
!!     end module M_demo
!!
!!     program demo_M_debug
!!     use M_demo,  only: test_suite_M_demo
!!     use M_debug, only: unit_check_command, unit_check_keep_going,unit_check_level
!!     unit_check_command=''
!!     unit_check_keep_going=.true.
!!     unit_check_level=0
!!       call test_suite_M_demo
!!     end program demo_M_debug
!!
!!   Expected output:
!!
!!     unit_check:       one                  SUCCESS:I > 0
!!     unit_check:       one                  SUCCESS:testing if everyone greater than zero
!!     unit_check:       one                  SUCCESS:1 2 3 testing if no one is equal to four
!!     unit_check_done:  one                  PASSED   GOOD:3  BAD:0
!!
!!     unit_check:       two                  SUCCESS:two looks good
!!     unit_check_done:  two                  PASSED   GOOD:1  BAD:0
!!
!!
!!
!!    TEST-DRIVEN DEVELOPMENT
!!
!!    set-up       perform initialization operations common to all tests within a module
!!    tear-down    perform finalization operations common to all tests within a module
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module m_debug
use iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
implicit none
private

integer,save,public :: io_debug=ERROR_UNIT            ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
integer,save,public :: unit_check_lun=ERROR_UNIT      ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
logical,save,public :: debug=.false.

logical,save,public :: unit_check_keep_going=.false.  ! logical variable that can be used to turn off program termination on errors.
integer,save,public :: unit_check_level=0             ! a level that can be used to select different debug levels
character(len=4096),public ::  unit_check_command='goodbad'  ! name of command to execute. Defaults to the name "goodbad".

integer,parameter,public   :: EXIT_SUCCESS=0
integer,parameter,public   :: EXIT_FAILURE=1

logical,save ::  STOP_G=.true.                        ! global value indicating whether unit checks should stop program or not
integer,save :: IPASSED_G=0
integer,save :: IFAILED_G=0

public stderr
public pdec
public fstop
public unit_check_start
public unit_check
public unit_check_good
public unit_check_bad
public unit_check_done
public unit_check_msg
public msg

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_msg(3f) - [M_debug] converts up to nine standard scalar values to a message for unit testing
!!##SYNOPSIS
!!
!!    function unit_check_msg(name,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: name
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    unit_check_msg(3f) builds a string from up to nine scalar values and prints it to the error long.
!!
!!##OPTIONS
!!    name    name of unit being tested
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check_msg
!!    use M_debug, only : unit_check_start,unit_check_msg,unit_check_done
!!    implicit none
!!    character(len=:),allocatable :: pr
!!
!!    call unit_check_start('myroutine')
!!    call unit_check_msg('myroutine','HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    call unit_check_msg('myroutine','real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    call unit_check_msg('myroutine','doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    call unit_check_msg('myroutine','complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    call unit_check_done('myroutine')
!!
!!    end program demo_unit_check_msg
!===================================================================================================================================
subroutine unit_check_msg(name,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

character(len=*),parameter::ident_1="&
&@(#)M_debug::unit_check_msg(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: name
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   call stderr('unit_check_msg:   '//atleast(name,20)//' INFO    :'//msg(g1,g2,g3,g4,g5,g6,g7,g8,g9))

end subroutine unit_check_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    msg(3f) - [M_debug] converts any standard scalar type to a string
!!##SYNOPSIS
!!
!!    function msg(g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     character(len=:),allocatable :: msg
!!##DESCRIPTION
!!    msg(3f) builds a string from up to nine scalar values. Since the routine
!!    uses internal WRITE statements to build the returned string it should not
!!    be used in a WRITE or PRINT statement, as Fortran does not yet support
!!    recursive I/O.
!!
!!##OPTIONS
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!##RETURNS
!!    msg     description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_debug, only : msg
!!    implicit none
!!    character(len=:),allocatable :: pr
!!
!!    pr=msg('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    write(*,'(a)')pr
!!    pr=msg('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    write(*,'(a)')pr
!!    pr=msg('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    write(*,'(a)')pr
!!    pr=msg('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    write(*,'(a)')pr
!!
!!    ! although it will often work, using msg(3f) in an I/O statement is not recommended
!!    write(*,*)msg('program will now stop')
!!
!!    end program demo_msg
!===================================================================================================================================
function msg(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9)
implicit none

character(len=*),parameter::ident_2="@(#)M_debug::msg(3f): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
character(len=:), allocatable :: msg
   character(len=4096)        :: line
   integer                    :: istart

   istart=1
   line=' '
   if(present(generic1))call append_generic(generic1)
   if(present(generic2))call append_generic(generic2)
   if(present(generic3))call append_generic(generic3)
   if(present(generic4))call append_generic(generic4)
   if(present(generic5))call append_generic(generic5)
   if(present(generic6))call append_generic(generic6)
   if(present(generic7))call append_generic(generic7)
   if(present(generic8))call append_generic(generic8)
   if(present(generic9))call append_generic(generic9)
   msg=trim(line)
contains
!===================================================================================================================================
subroutine append_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic
character(len=4096)          :: message
integer                      :: ios
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)',iostat=ios,iomsg=message) generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)',iostat=ios,iomsg=message) generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)',iostat=ios,iomsg=message) generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)',iostat=ios,iomsg=message) generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)',iostat=ios,iomsg=message) generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)',iostat=ios,iomsg=message) generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)',iostat=ios,iomsg=message) generic
      !type is (real(kind=real256));     write(line(istart:),'(1pg0)',iostat=ios,iomsg=message) generic
      !type is (real);                   write(line(istart:),'(1pg0)',iostat=ios,iomsg=message) generic
      !type is (doubleprecision);        write(line(istart:),'(1pg0)',iostat=ios,iomsg=message) generic
      type is (logical);                write(line(istart:),'(1l)',iostat=ios,iomsg=message) generic
      type is (character(len=*));       write(line(istart:),'(a)',iostat=ios,iomsg=message) trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")',iostat=ios,iomsg=message) generic
      class default
         write(line(istart:),'(a)',iostat=ios,iomsg=message) '????UNKNOWN_TYPE????'
         !!stop 'unknown type in *append_generic*'
   end select
   istart=len_trim(line)+2
   if(ios.ne.0)then
      !!line(istart:)=message
      call stderr('*msg* '//message)
      !!stop '*msg* error in converting to string'
   endif
end subroutine append_generic
!===================================================================================================================================
end function msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stderr(3f) - [M_debug] write message to stderr
!!##SYNOPSIS
!!
!!    subroutine stderr(msg,[generic])
!!
!!     !character(len=*),intent(in)  :: msg
!!     class(*),intent(in),optional :: msg
!!     class(*),intent(in),optional :: generic1,generic2,generic3,generic4,generic5
!!     class(*),intent(in),optional :: generic6,generic7,generic8,generic9
!!##DESCRIPTION
!!    STDERR(3f) writes a message to standard error using a standard f2003 method.
!!    Up to nine generic options are available.
!!##OPTIONS
!!    msg           - description to print
!!    generic[1-9]  - optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_stderr
!!    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!    use,intrinsic :: iso_fortran_env, only : real32, real64, real128
!!    use,intrinsic :: iso_fortran_env, only : real=> real32, integer=> int32
!!    use M_debug, only: stderr
!!    implicit none
!!
!!    call stderr('A simple message')
!!    call stderr('error: RVALUE=',3.0/4.0)
!!    call stderr('error: IVALUE=',123456789)
!!    call stderr('error: LVALUE=',.true.)
!!
!!    SEVERAL: block
!!    integer :: least=10, most=999, ival=-10
!!    call stderr('error: value',ival,'should be between',least,'and',most)
!!    endblock SEVERAL
!!
!!    call stderr('real32  :',huge(0.0_real32),0.0_real32,12345.6789_real32,tiny(0.0_real32))
!!    call stderr('real64  :',huge(0.0_real64),0.0_real64,12345.6789_real64,tiny(0.0_real64))
!!    call stderr('real128 :',huge(0.0_real128),0.0_real128,12345.6789_real128,tiny(0.0_real128))
!!    call stderr('complex :',cmplx(huge(0.0_real),tiny(0.0_real)))
!!
!!    call stderr('error: program will now stop')
!!    stop 1
!!
!!    end program demo_stderr
!!
!!   Results:
!!     A simple message
!!     error: RVALUE= 0.750000000
!!     error: IVALUE= 123456789
!!     error: LVALUE= T
!!     error: value -10 should be between 10 and 999
!!     real32  : 3.40282347E+38 ...
!!               0.00000000 ...
!!               12345.6787 ...
!!               1.17549435E-38
!!     real64  : 1.7976931348623157E+308 ...
!!               0.0000000000000000 ...
!!               12345.678900000001 ...
!!               2.2250738585072014E-308
!!     real128 : 1.18973149535723176508575932662800702E+4932 ...
!!               0.00000000000000000000000000000000000  ...
!!               12345.6789000000000000000000000000002 ...
!!               3.36210314311209350626267781732175260E-4932
!!     complex : (3.40282347E+38,1.17549435E-38)
!!     error: program will now stop
!!     STOP 1
!! ================================================================================
!===================================================================================================================================
subroutine stderr(msg, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9)
implicit none

character(len=*),parameter::ident_3="@(#)M_debug::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: msg
class(*),intent(in),optional :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional :: generic6 ,generic7 ,generic8 ,generic9
   integer                   :: ios

   if(present(msg))     call print_generic(msg)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   write(error_unit,'(a)',iostat=ios)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   write(error_unit,'(1x)',advance='no')
   select type(generic)
      type is (integer(kind=int8));     write(error_unit,'(i0)',advance='no') generic
      type is (integer(kind=int16));    write(error_unit,'(i0)',advance='no') generic
      type is (integer(kind=int32));    write(error_unit,'(i0)',advance='no') generic
      type is (integer(kind=int64));    write(error_unit,'(i0)',advance='no') generic
      type is (real(kind=real32));      write(error_unit,'(1pg0)',advance='no') generic
      type is (real(kind=real64));      write(error_unit,'(1pg0)',advance='no') generic
      type is (real(kind=real128));     write(error_unit,'(1pg0)',advance='no') generic
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      !type is (real);                   write(error_unit,'(1pg0)',advance='no') generic
      !type is (doubleprecision);        write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(error_unit,'(1l)',advance='no') generic
      type is (character(len=*));       write(error_unit,'(a)',advance='no') trim(generic)
      type is (complex);                write(error_unit,'("(",1pg0,",",1pg0,")")',advance='no') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
end subroutine print_generic
!===================================================================================================================================
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fstop(3f) - [M_debug] call stop with both a number and a message
!!##SYNOPSIS
!!
!!    subroutine fstop(ierr,stdout,stderr)
!!
!!     integer,intent(in)                   :: ierr
!!     character(len=*),intent(in),optional :: stdout
!!     character(len=*),intent(in),optional :: stderr
!!##DESCRIPTION
!!
!!    FSTOP(3f) call STOP(3f). What a call to STOP does is very system
!!    dependent, so using an abstraction layer is useful, as it allows just
!!    the fstop() routine to be changed; and STOP does not allow a variable
!!    to be used on the numeric access status (this has changed at f2015).
!!
!!##OPTIONS
!!    ierr    - value in range 0 to 32
!!    stdout  - description to be printed to standard output
!!    stderr  - description to be printed to standard error
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_fstop
!!    use M_debug, only: fstop
!!    implicit none
!!    integer :: int
!!    write(*,*)'Enter stop value'
!!    read(*,*) int
!!    select case(int)
!!    case(10) ; call fstop(int)
!!    case(20) ; call fstop(int,stderr='error: program will now stop')
!!    case(25) ; call fstop(int,stdout='stdout message',stderr='stderr message')
!!    case(30) ; call fstop(int,stdout='error: program will now stop')
!!    case default
!!               call fstop(int)
!!    endselect
!!
!!    end program demo_fstop
!!
!!   Results:
!!
!===================================================================================================================================
subroutine fstop(ierr,stdout,stderr)

character(len=*),parameter::ident_4="@(#)M_debug::fstop(3f): calls 'STOP VALUE' passing in a value (1-32), with optional message"

integer,intent(in)                   :: ierr
character(len=*),optional,intent(in) :: stdout
character(len=*),optional,intent(in) :: stderr
   character(len=132)                :: message
! The standard states:
!   If the stop-code is an integer, it is recommended that the value also be used as the process exit status, if the
!   processor supports that concept. If the integer stop-code is used as the process exit status, the processor
!   might be able to interpret only values within a limited range, or only a limited portion of the integer value
!   (for example, only the least-significant 8 bits).

!   If the stop-code is of type character or does not appear, or if an END PROGRAM statement is executed,
!   it is recommended that the value zero be supplied as the process exit status, if the processor supports that
!   concept.
!   A STOP statement or ALL STOP statement shall not be executed during execution of an input/output statement.
!
! Conforming varients I have encountered include
!    o printing a message such as 'STOP nnn' when the integer value is called
!    o having a limit on the length of the message string passed
!    o prefixing the message with the string 'STOP '
!    o different ranges on allowed integer values, and/or not having a one-to-one correspondence between the argument
!      value and what the system is given (usually encountered with large values, which are masked or run thru modulo math, ...)
!    o whether messages appear on stdout or stderr.
!    o no value being returned to the system at all.
!
!  So it is best to test (csh/tcsh sets $status, sh/ksh/bash/... sets $?) to verify what exit codes are supported.
!  What happens with negative values, values above 256; how long of a message is supported? Are messages line-terminated?
!
!  And for some reason STOP only takes constant values. I sometimes want to be able to pass a variable value.
!  Only allowing constants would have the advantage of letting the compiler detect values invalid for a particular system,
!  but I sometimes want to return variables.
!
!  So, using STOP with an argument is not as straight-forward as one might guess, especially if you do not want a message
!  to appear when using integer values
!
!  In practice the C exit(int signal) routine seems to work successfully when called from Fortran but I consider it risky
!  as it seems reasonable to assume Fortran cleanup operations such as removing scratch files and closing and flushing Fortran
!  files may not be properly performed. So it is tempting to call the C function, especially on systems where C returns a
!  value to the system and Fortran does not, but I do not recommend it.
!
!  Note that the C function "exit(int signal)" not only works more consistently but that the global values EXIT_SUCCESS and
!  EXIT_FAILURE are defined for portability, and that the signal value can be a variable instead of a constant.
!
!  If the system supports calls to produce a traceback on demand, that is a useful option to add to this procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
!STOP       'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab'
!-----------------------------------------------------------------------------------------------------------------------------------
if(present(stdout))then       ! write message to stdout, assuming string length is allowed
   if(stdout.ne.'')then
      write(*,'(a)')trim(stdout)
   endif
endif
if(present(stderr))then       ! write message to stderr, assuming string length is allowed
   if(stderr.ne.'')then
      write(error_unit,'(a)')trim(stderr)
   endif
endif
select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
   case(0); stop 0
   case(1); stop 1
   case(2); stop 2
   case(3); stop 3
   case(4); stop 4
   case(5); stop 5
   case(6); stop 6
   case(7); stop 7
   case(8); stop 8
   case(9); stop 8
   case(10); stop 10
   case(11); stop 11
   case(12); stop 12
   case(13); stop 13
   case(14); stop 14
   case(15); stop 15
   case(16); stop 16
   case(17); stop 17
   case(18); stop 18
   case(19); stop 19
   case(20); stop 20
   case(21); stop 21
   case(22); stop 22
   case(23); stop 23
   case(24); stop 24
   case(25); stop 25
   case(26); stop 26
   case(27); stop 27
   case(28); stop 28
   case(29); stop 29
   case(30); stop 30
   case(31); stop 31
   case(32); stop 32
case default
   write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
   write(error_unit,'(a)')trim(message) ! write message to standard error
   stop 1
end select
end subroutine fstop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check(3f) - [M_debug] if logical expression is false, call command "goodbad NAME bad" and stop program by default
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check(name,expression,msg)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    unit_check(3f) tests the expression and if it is false, calls the shell command
!!
!!         goodbad NAME bad
!!
!!    and stops the program.
!!##OPTIONS
!!     NAME         the unit test name passed on to the goodbad(1) command
!!     EXPRESSION   the logical expression to evaluate
!!     MSG          optional message to display when performing test
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check
!!    use M_debug, only: unit_check
!!    use M_debug, only: unit_check_start, unit_check_good
!!    use M_math,  only: almost
!!
!!    !!use M_debug, only: unit_check_keep_going         ! default is unit_check_keep_going=.false.
!!    !!use M_debug, only: debug              ! default is .false.
!!    !!use M_debug, only: unit_check_command ! default is unit_check_command='goodbad'
!!
!!    implicit none
!!    integer :: i
!!    integer :: x
!!    integer,allocatable :: arr(:)
!!    real,allocatable :: arr1(:)
!!    real,allocatable :: arr2(:)
!!
!!       !!unit_check_command=''
!!       x=10
!!       arr1=[1.0,10.0,100.0]
!!       arr2=[1.0001,10.001,100.01]
!!       call unit_check_start('myroutine')
!!
!!       call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!       call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!       do i=1,size(arr1)
!!          call unit_check('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
!!       enddo
!!
!!       arr=[10,20,30]
!!       call unit_check('myroutine', .not.any(arr.lt.0) ,'test if any negative values in array ARR')
!!       call unit_check('myroutine', all(arr.lt.100) ,'test if all values less than 100 in array ARR')
!!
!!       call unit_check_done('myroutine',msg='checks on "myroutine" all passed')
!!
!!    end program demo_unit_check
!!
!!   Sample output (varies with what goodbad(1) command is used):
!!
!!    unit_check:      myroutine        SUCCESS:test if big enough
!!    unit_check:      myroutine        SUCCESS:test if small enough
!!    unit_check:      myroutine        SUCCESS:test if any negative values in array ARR
!!    unit_check:      myroutine        SUCCESS:test if all values less than 100 in array ARR
!!     *almost* for values 1.00000000 1.00010002 agreement of 3.99997139 digits out of requested 3.90000010
!!     *almost* for values 10.0000000 10.0010004 agreement of 3.99986792 digits out of requested 3.90000010
!!     *almost* for values 100.000000 100.010002 agreement of 3.99995065 digits out of requested 3.90000010
!!    unit_check_good: myroutine        PASSED:checks on "myroutine" all passed
!!
!!
!===================================================================================================================================
subroutine unit_check(name,logical_expression,msg)

character(len=*),parameter::ident_5="@(#)M_debug::unit_check(3f):if .not.expression call 'goodbad NAME bad' & stop program"

character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(msg))then
     msg_local=msg
   else
     msg_local=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.logical_expression)then
      call stderr('unit_check:       '//atleast(name,20)//' FAILURE : '//trim(msg))  ! write message to standard error
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad')
      endif
      if(.not.unit_check_keep_going) then
         call stderr('unit_check:         STOPPING PROGRAM ON FAILED TEST OF '//trim(name))    ! write to standard error
         call fstop(1)
      endif
      IFAILED_G=IFAILED_G+1
   else
      call stderr('unit_check:       '//atleast(name,20)//' SUCCESS : '//trim(msg))  ! write message to standard error
      IPASSED_G=IPASSED_G+1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_start(3f) - [M_debug] call command "goodbad NAME start" and optionally set options
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_start(name,options,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: options
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    unit_check_start(3f) is an initialization command that by default
!!    calls the shell command
!!
!!       goodbad NAME start [options]
!!
!!    The command can be changed by setting the environment variable
!!    UNIT_CHECK_COMMAND or the global module variable UNIT_CHECK_COMMAND.
!!    The environment variable overrides the global module variable.
!!
!!    By default if a unit_check(3f) logical expression is false or the
!!    unit_check_bad(3f) procedure is called the program will be stopped.
!!
!!    This has the same effect as setting the environment
!!    variable M_DEBUG_STOP to "FALSE" or the global module variable
!!    UNIT_CHECK_KEEP_GOING to .FALSE. . Set the value to .true. and the
!!    program will continue even when tests fail.
!!
!!##OPTIONS
!!       NAME  name of the shell command to execute. If blank, no command
!!             is executed.
!!    OPTIONS  pass additional options to the shell command
!!
!!    MSG      print message
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_start
!!     use M_debug, only: unit_check_start
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: ival
!!     call unit_check_start('myroutine')
!!     ! the goodbad(1) command called here takes many options
!!     ! used to build an SQLite3 entry
!!     call unit_check_start('myroutine_long',' &
!!       & -section        3                    &
!!       & -library        libGPF               &
!!       & -filename       `pwd`/M_debug.FF     &
!!       & -documentation  y                    &
!!       & -ufpp           y                    &
!!       & -ccall          n                    &
!!       & -archive        GPF.a                &
!!       & ')
!!
!!     ival=10
!!     call unit_check('myroutine', ival.gt.3 ,   msg='test if big enough')
!!     call unit_check('myroutine', ival.lt.100 , msg='test if small enough')
!!
!!     call unit_check_done('myroutine',msg='completed checks of "myroutine"')
!!
!!     end program demo_unit_check_start
!===================================================================================================================================
subroutine unit_check_start(name,options,msg)

character(len=*),parameter::ident_6="@(#)M_debug::unit_check_start(3f): call 'goodbad NAME start'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: options
character(len=*),intent(in),optional :: msg
character(len=4096)                  :: var
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_environment_variable('UNIT_CHECK_COMMAND',var)
   if(var.ne.'')unit_check_command=var
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(options))then
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start '//trim(options))
      endif
   else
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start')
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(msg))then
     if(msg.ne.'')then
        call stderr('unit_check_start: '//atleast(name,20)//' START   :'//trim(msg)) ! write message to standard error
     endif
   endif
   call get_environment_variable('M_DEBUG_STOP',var)
   select case(var)
   case('FALSE','false')
         unit_check_keep_going=.false.
   case('1')
         unit_check_keep_going=.false.
   case('no','NO')
         unit_check_keep_going=.false.
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_done(3f) - [M_debug] call command "goodbad NAME good" or "goodbad NAME bad" depending on whether failures were found
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_done(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    If there have been no failures the shell command
!!
!!         goodbad NAME good [opts]
!!
!!    is executed, else the command
!!
!!         goodbad NAME bad [opts]
!!
!!    is executed and by default stops the program if their have been
!!    any failures.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_done
!!     use M_debug, only: unit_check_start
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_done, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_done ('myroutine',msg='checks on "myroutine"' ) ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_done
!===================================================================================================================================
subroutine unit_check_done(name,opts,msg)

character(len=*),parameter::ident_7="@(#)M_debug::unit_check_done(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
   character(len=:),allocatable      :: msg_local
   character(len=:),allocatable      :: opts_local
   character(len=4096)               :: out
   character(len=9)                  :: pf
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(unit_check_command.ne.'')then                           ! if system command name is not blank call system command
      if(ifailed_g.eq.0)then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad '//trim(opts))
         if(.not.unit_check_keep_going) call fstop(1)            ! stop program depending on mode
      else
         call execute_command_line(unit_check_command//' '//trim(name)//' good '//trim(opts))
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   PF=merge('PASSED  :','FAILED  :',ifailed_G.eq.0)
   if(PF.eq.'PASSED  :'.and.ipassed_G.eq.0)then
      PF='UNTESTED:'
   endif
   write(out,'("unit_check_done:  ",a,1x,a," GOOD:",i0,1x," BAD:",i0)') atleast(name,20),PF,IPASSED_G,IFAILED_G
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
      call stderr('')
   else
      call stderr(trim(out))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_done
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_bad(3f) - [M_debug] call command "goodbad NAME bad" and stop program
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_bad(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    unit_check_bad(3f) calls the shell command
!!
!!         goodbad NAME bad [opts]
!!
!!    and stops the program. It is just a shortcut for calling
!!         call unit_check(name,.false.)
!!         call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_bad
!!     use M_debug, only: unit_check_start
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_bad
!===================================================================================================================================
subroutine unit_check_bad(name,opts,msg)

character(len=*),parameter::ident_8="@(#)M_debug::unit_check_bad(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
   character(len=:),allocatable      :: msg_local
   character(len=:),allocatable      :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.false.)
   call unit_check_done(name,opts_local,msg_local)
end subroutine unit_check_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_good(3f) - [M_debug] call command "goodbad NAME good"
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_good(name,opts,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    A shortcut for
!!
!!       call unit_check(name,.true.)
!!       call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_good
!!     use M_debug, only: unit_check_start, unit_check_done
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     call unit_check_good('myroutine',msg='checks on "myroutine" ')
!!
!!     end program demo_unit_check_good
!===================================================================================================================================
subroutine unit_check_good(name,opts,msg)

character(len=*),parameter::ident_9="@(#)M_debug::unit_check_good(3f): call 'goodbad NAME good'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
   character(len=:),allocatable      :: msg_local
   character(len=:),allocatable      :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.true.,msg=msg_local)
   call unit_check_done(name,opts_local)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      pdec(3f) - [M_debug] write out string with ASCII decimal equivalent vertically under it
!!
!!##SYNOPSIS
!!
!!    Usage:
!!
!!     subroutine pdec(string)
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!      Given a string to print, PDEC() writes out the ASCII Decimal equivalent of
!!      the string directly underneath it. This can help you to locate
!!      unprintable characters or non-standard white-space such as a
!!      backspace character or tab character in input strings that your
!!      program could not interpret. On output, non-printable characters
!!      are replaced with a space, and trailing spaces are ignored.
!!
!!      You read the numbers vertically.
!!
!!      1. ignore trailing spaces
!!      2. print the character if it has an ADE of 32 on up
!!      3. print a space if it has an ADE of less than 32
!!      4. underneath each character print the ADE value vertically
!!      5. strings are assumed under 32767 characters in length.
!!         Format integer constants > 32767 are not supported on HP-UX
!!         when newer compilers are available use unlimited
!!
!!##EXAMPLES
!!
!!
!!    Sample program:
!!
!!       program demo_pdec
!!       use M_debug, only : pdec
!!       call pdec(' ABCDEFG abcdefg    ')
!!       end program demo_pdec
!!
!!    would produce (notice trailing space is trimmed):
!!
!!      > ABCDEFG abcdefg
!!      >0000000000001111
!!      >3666667739990000
!!      >2567890127890123
!!
!!##AUTHOR
!!      John S. Urban
!===================================================================================================================================
subroutine pdec(string)

character(len=*),parameter::ident_10="@(#)M_debug::pdec(3f): write ASCII Decimal Equivalent (ADE) numbers vertically beneath string"

character(len=*),intent(in) :: string  ! the string to print
   integer :: ilen  ! number of characters in string to print
   integer :: i     ! counter used to step thru string
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(string(:len(string)))  ! get trimmed length of input string

   write(*,101)(char(max(32,ichar(string(i:i)))),i=1,ilen) ! replace lower unprintable characters with spaces

   ! print ADE value of character underneath it
   write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
   write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
   write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
101   format(32767a1:)  ! format for printing string characters
202   format(32767i1:)  ! format for printing ADE values
end subroutine pdec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function atleast(line,length) result(strout)

character(len=*),parameter::ident_11="@(#)M_debug::atleast(3fp): return string padded to at least specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=max(length,len(trim(line)))) ::  strout
   strout=line
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_debug
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
