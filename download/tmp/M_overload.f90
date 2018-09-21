!>
!!##NAME
!!    M_overload(3fm) - [M_overload] overloads of standard operators and intrinsic procedures
!!##SYNOPSIS
!!
!!    ==,/=   Allow the syntax "L1 == L2"  and "L1 /= L2" where L1 and L2 are
!!            type LOGICAL as an alternative to the standard expressions
!!            "L1 .EQV. L2" and "L1 .NEQV. L2".
!!
!!    int(), real(), dble()  allow strings to be converted to numeric values
!!                           using the standard intrinsic names
!!##DESCRIPTION
!!   Operator and function overloads have a wide range of applications from
!!   allowing existing Fortran routines to be used with almost no source-code
!!   changes to produce versions using arbitrary precision or cumulative error
!!   bounds on floating-point calculations to adding intuitive syntax for
!!   standard Fortran operations.
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
character(len=*),parameter::ident="@(#)M_overload(3fm): overloads of standard operators and intrinsic procedures"
private
public boolean_equal, boolean_notequal      !
public operator(==)
public operator(/=)
public int, real, dble                      ! extend intrinsics to accept CHARACTER values

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
!------------------------------------------------------------------------------------------------------------------------------------------
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
end module m_overload
!-----------------------------------------------------------------------------------------------------------------------------------
