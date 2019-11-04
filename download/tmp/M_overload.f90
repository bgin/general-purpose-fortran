!>
!!##NAME
!!    M_overload(3fm) - [M_overload] overloads of standard operators and intrinsic procedures
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!  overloads on LOGICAL values
!!
!!   logical==logical
!!   logical/=logical
!!
!!  overloads on INTRINSICS
!!
!!   int('string')
!!   real('string')
!!   dble('string')
!!
!!##DESCRIPTION
!!
!!    ==,/=   Allow the syntax "L1 == L2"  and "L1 /= L2" where L1 and L2 are
!!            type LOGICAL as an alternative to the standard expressions
!!            "L1 .EQV. L2" and "L1 .NEQV. L2".
!!
!!            It should be pointed out that
!!
!!               L1 == L2   !! should be L1 .eqv. L2
!!
!!            and
!!
!!               L1 /= L2   !! should be L1 .neqv. L2
!!
!!            should NOT work by default; but often do (probably because
!!            the compiler silently converts LOGICAL to INTEGER when a
!!            LOGICAL appears where a numeric value is required?). If your
!!            compiler supports this non-standard (but intuitive-looking)
!!            syntax without using an overload as provided here using it
!!            makes your code much less portable.
!!
!!    int(), real(), dble()  allow strings to be converted to numeric values
!!                           using the standard intrinsic names
!!
!!   Operator and function overloads have a wide range of applications
!!   from allowing existing Fortran routines to be used with almost no
!!   source-code changes to produce versions using arbitrary precision
!!   or cumulative error bounds on floating-point calculations to adding
!!   intuitive syntax for standard Fortran operations.
!!
!!##EXAMPLES
!!
!!   Sample usage:
!!
!!    program demo_M_overload
!!    use M_compare_float_numbers, only : operator(.EqualTo.)
!!
!!    use M_overload, only : int          ! allow strings to be converted to integers
!!    use M_overload, only : real,dble    ! allow strings to be converted to floating point
!!    use M_overload, only : operator(==) ! use == like .eqv.
!!    use M_overload, only : operator(/=) ! use /= like .neqv.
!!
!!    if(int('1234')               .eq.1234)                      write(*,*)'int("STRING") works '
!!    if(real('1234.56789')        .EqualTo.1234.56789)           write(*,*)'real("STRING") works '
!!    if(dble('1234.5678901234567').EqualTo.1234.5678901234567d0) write(*,*)'dble("STRING") works '
!!    if (.true. == .true. )  write(*,*)'== works like .eqv. for LOGICAL values'
!!    if (.true. /= .false. ) write(*,*)'/= works like .neqv. for LOGICAL values'
!!
!!    end program demo_M_overload
!!
!!   Expected output:
!!
!!     int("STRING") works
!!     real("STRING") works
!!     dble("STRING") works
!!     == works like .eqv. for LOGICAL values
!!     /= works like .neqv. for LOGICAL values
!===================================================================================================================================
module m_overload
use M_strings, only : s2v
implicit none
character(len=*),parameter::ident_1="@(#)M_overload(3fm): overloads of standard operators and intrinsic procedures"
private
public boolean_equal, boolean_notequal      !
public operator(==)
public operator(/=)
public int, real, dble                      ! extend intrinsics to accept CHARACTER values
public test_suite_M_overload

interface operator ( == )
   module procedure boolean_equal
end interface operator ( == )

interface operator ( /= )
   module procedure boolean_notequal
end interface operator ( /= )

! extend intrinsics to accept CHARACTER values
interface int;     module procedure int_s2v;           end interface
interface real;    module procedure real_s2v;          end interface
interface dble;    module procedure dble_s2v;          end interface
interface int;     module procedure ints_s2v;          end interface
interface real;    module procedure reals_s2v;         end interface
interface dble;    module procedure dbles_s2v;         end interface

contains
!-----------------------------------------------------------------------------------------------------------------------------------
logical function boolean_equal(logical_val1,logical_val2)
logical, intent (in) :: logical_val1
logical, intent (in) :: logical_val2

   if (logical_val1 .eqv. logical_val2 )then
     boolean_equal=.true.
   else
     boolean_equal=.false.
   endif

end function boolean_equal
!-----------------------------------------------------------------------------------------------------------------------------------
logical function boolean_notequal(logical_val1,logical_val2)
logical, intent (in) :: logical_val1
logical, intent (in) :: logical_val2

   if (logical_val1 .eqv. logical_val2 )then
     boolean_notequal=.false.
   else
     boolean_notequal=.true.
   endif

end function boolean_notequal
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!-----------------------------------------------------------------------------------------------------------------------------------
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
   integer                  :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
   integer                  :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
   integer                  :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_overload()
use M_math,                  only : almost
use M_compare_float_numbers, only : operator(.EqualTo.)

!! setup
   call test_boolean_equal()
   call test_boolean_notequal()
   call test_dble_s2v()
   call test_dbles_s2v()
   call test_int_s2v()
   call test_ints_s2v()
   call test_real_s2v()
   call test_reals_s2v()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_equal()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : operator(==)
   call unit_check_start('boolean_equal',' &
         & -description "overload == to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_equal',.true. == .true. ,msg=msg('== works like .eqv. for LOGICAL values'))
   call unit_check_done('boolean_equal',msg='')
end subroutine test_boolean_equal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_notequal()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : operator(/=)
   call unit_check_start('boolean_notequal',' &
         & -description "overload /= to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_notequal', (.true. /= .false. ),msg=msg('/= works like .neqv. for LOGICAL values'))
   call unit_check_done('boolean_notequal',msg='')
end subroutine test_boolean_notequal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble_s2v()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : int, real, dble
   call unit_check_start('dble_s2v',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(dble('0.3570726221234567').eq. 0.3570726221234567d0)then
      call unit_check_good('dble_s2v')                                             ! string passed to dble
   elseif(dble('0.3570726221234567') .EqualTo. 0.3570726221234567d0 )then
      call unit_check_good('dble_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dble_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_dble_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dbles_s2v()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : int, real, dble
   call unit_check_start('dbles_s2v',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(all(dble(['10.0d0','20.0d0']).eq. [10.0d0,20.0d0]))then
      call unit_check_good('dbles_s2v')                                             ! string passed to dble
   elseif(all(dble(['10.0d0','20.0d0']) .EqualTo. [10.0d0,20.0d0]))then
      call unit_check_good('dbles_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dbles_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_dbles_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_s2v()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : int, real, dble
   call unit_check_start('int_s2v',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('int_s2v',int('1234').eq.1234,'string passed to int')
   call unit_check_done('int_s2v',msg='')
end subroutine test_int_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ints_s2v()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('ints_s2v',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(all(int(['100','200']).eq. [100,200]))then
      call unit_check_good('ints_s2v')                                             ! string passed to int
   else
      call unit_check_bad('ints_s2v')                                              ! returned value not equal to expected value
   endif
   call unit_check_done('ints_s2v',msg='')
end subroutine test_ints_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_s2v()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : int, real, dble
   call unit_check_start('real_s2v','&
         & -description "overload REAL() to take string arguments" &
         & -section 3                    &
         & -library libGPF               &
         & -filename `pwd`/M_overload.FF &
         & -documentation y              &
         &  -ufpp         y              &
         &  -ccall        n              &
         &  -archive      GPF.a          &
         & ')
   if(REAL('0.357072622').eq. 0.357072622)then
      call unit_check_good('real_s2v')
   elseif(almost(real('0.357072622'), 0.357072622,7.0) )then
      call unit_check_good('real_s2v')                                          ! string passed to real but not exactly
   else
      call unit_check_bad('real_s2v')                                           ! returned value not equal to expected value
   endif
end subroutine test_real_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reals_s2v()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
!use M_overload,              only : int, real, dble
   call unit_check_start('reals_s2v','&
         & -description "overload REAL() to take string arguments" &
         & -section 3                    &
         & -library libGPF               &
         & -filename `pwd`/M_overload.FF &
         & -documentation y              &
         &  -ufpp         y              &
         &  -ccall        n              &
         &  -archive      GPF.a          &
         & ')
   if(all(real(['0.357072622','200.0      ']).eq. [0.357072622,200.0]))then
      call unit_check_good('reals_s2v')                                             ! string passed to int
   elseif(all(real(['0.357072622','200.0      ']) .EqualTo. [0.357072622,200.0]))then
      call unit_check_good('reals_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('reals_s2v')                                              ! returned value not equal to expected value
   endif

end subroutine test_reals_s2v
!===================================================================================================================================
end subroutine test_suite_M_overload
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module m_overload
