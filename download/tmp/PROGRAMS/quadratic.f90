subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       quadratic(1f) - [MATH] Calculate and print the roots of a quadratic formula even if they are complex',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       quadratic A B C |[--help|--version]                                      ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given the equation                                                           ',&
'                                                                                ',&
'      A*x**2 + B*x + C = 0                                                      ',&
'                                                                                ',&
'   Use the quadratic formula to determine the root values of the equation.      ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       A,B,C    coefficients                                                    ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'   quadratic 1.0  5.0  2.0                                                      ',&
'    for    1.00000000     *x**2+   5.00000000     *x+   2.00000000     =0       ',&
'    the roots are real:                                                         ',&
'    z1 = -0.438447237                                                           ',&
'    z2 =  -4.56155300                                                           ',&
'    discriminant =   17.0000000                                                 ',&
'                                                                                ',&
'   quadratic 1.0  2.0  5.0 # There are no real roots (Discriminant = -16)!      ',&
'    for    1.00000000     *x**2+   2.00000000     *x+   5.00000000     =0       ',&
'    the roots are complex:                                                      ',&
'    z1 = ( -1.00000000    ,  2.00000000    )                                    ',&
'    z2 = ( -1.00000000    , -2.00000000    )                                    ',&
'    discriminant =  -16.0000000                                                 ',&
'                                                                                ',&
'   quadratic 9 12 4                                                             ',&
'    for    9*x**2 + 12*x + 4 = 0                                                ',&
'    the roots are real and equal:                                               ',&
'    z1 = z2 = -0.666666687                                                      ',&
'    discriminant =   0.00000000                                                 ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        quadratic(1f) - [MATH] Calculate and print the roots of a quadratic formula even if they are complex
!!
!!##SYNOPSIS
!!
!!        quadratic A B C |[--help|--version]
!!
!!##DESCRIPTION
!!    Given the equation
!!
!!       A*x**2 + B*x + C = 0
!!
!!    Use the quadratic formula to determine the root values of the equation.
!!
!!##OPTIONS
!!        A,B,C    coefficients
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    quadratic 1.0  5.0  2.0
!!     for    1.00000000     *x**2+   5.00000000     *x+   2.00000000     =0
!!     the roots are real:
!!     z1 = -0.438447237
!!     z2 =  -4.56155300
!!     discriminant =   17.0000000
!!
!!    quadratic 1.0  2.0  5.0 # There are no real roots (Discriminant = -16)!
!!     for    1.00000000     *x**2+   2.00000000     *x+   5.00000000     =0
!!     the roots are complex:
!!     z1 = ( -1.00000000    ,  2.00000000    )
!!     z2 = ( -1.00000000    , -2.00000000    )
!!     discriminant =  -16.0000000
!!
!!    quadratic 9 12 4
!!     for    9*x**2 + 12*x + 4 = 0
!!     the roots are real and equal:
!!     z1 = z2 = -0.666666687
!!     discriminant =   0.00000000
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        quadratic(1f)>',&
'@(#)DESCRIPTION:    Calculate and print the roots of a quadratic formula even if they are complex>',&
'@(#)VERSION:        1.0, 20170717>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Tue, Aug 22nd, 2017 4:41:54 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_quadratic
use M_kracken, only : kracken, rgets, lget
use M_math,    only : quadratic
use M_strings, only : v2s
implicit none
character(len=*),parameter::ident="@(#)quadratic(1f)"
integer,parameter :: a=1, b=2, c=3 ! coefficients
real              :: coeff(3)=0.0
complex           :: z1, z2  ! roots
real              :: discriminant
logical           :: verbose
   call kracken('quadratic',' --help .false. --version .false. -verbose .false.')
   call help_usage(lget('quadratic_help'))          ! check for help text
   call help_version(lget('quadratic_version'))     ! check for version text
   coeff=rgets('quadratic_oo')
   verbose=lget('quadratic_verbose')

   call quadratic(coeff(a),coeff(b),coeff(c),z1,z2,discriminant) !  Calculate the roots

   if(verbose)then
      print *, "a ............... ", coeff(a)
      print *, "b ............... ", coeff(b)
      print *, "c ............... ", coeff(c)
      print *, "z1 .............. ", z1
      print *, "z2 .............. ", z2
      print *, "discriminant .... ", discriminant
      print *
   endif

!  Print the roots

   write(*,*)'for ',v2s(coeff(a)),'*x**2 + ',v2s(coeff(b)),'*x + ',v2s(coeff(c)),' = 0'
   if (discriminant == 0) then
      write(*,*) 'the roots (ie. "x intercepts") are repeated (real and equal) so the parabola just touches the x-axis at:'
      print *, "z1 = z2 =", v2s(real(z1))
   else if (discriminant > 0) then
      write(*,*) 'the roots (ie. "x intercepts") are real so the parabola crosses the x-axis at two points:'
      print *, "z1 =", v2s(real(z1))
      print *, "z2 =", v2s(real(z2))
   else
      write(*,*) 'the roots(ie. "x intercepts")  are complex:'
      print *, "z1 =", z1
      print *, "z2 =", z2
   endif

   print *, "discriminant =", v2s(discriminant)

end program demo_quadratic
