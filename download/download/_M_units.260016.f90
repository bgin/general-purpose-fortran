program testit
use M_units
use M_debug, only : unit_check_good
real             :: x=real(PI)
real,allocatable :: values(:)

   write(*,'(80("="),/,a)') 'Checking Constants'
   call  testit_p('pi',      real(PI)      ,  real(3.141592653589793238462643383279500d0)  )
   call  testit_p('e',       real(E)       ,  real(2.718281828459045235360d0)              )
   call  testit_p('radian',  real(RADIAN)  ,  real(57.29577951310d0)                       )
   call  testit_p('degree',  real(DEGREE)  ,  real(0.0174532925199430d0)                   )

   write(*,'(80("="),/,a)') 'Temperature Conversions'
   call  testit_p('f2c',     f2c(32.0)  ,   0.0)
   call  testit_p('f2c',     f2c(212.0) , 100.0)
   call  testit_p('f2c',     f2c(-40.0) , -40.0)

   call  testit_p('c2f',     c2f(0.0)   ,  32.0)
   call  testit_p('c2f',     c2f(100.0) , 212.0)
   call  testit_p('c2f',     c2f(-40.0) , -40.0)

   write(*,'(80("="),/,a)') 'Angular Conversions'
   call  testit_p('d2r',  d2r(  0.0)    ,  0.0            )
   call  testit_p('d2r',  d2r(  45.0)   ,  real(PI)/4.0   )
   call  testit_p('d2r',  d2r(  -45.0)  ,  real(-PI)/4.0  )
   call  testit_p('d2r',  d2r(  90.0)   ,  real(PI)/2     )
   call  testit_p('d2r',  d2r(  180.0)  ,  real(PI)       )

   call  testit_p('r2d',  r2d(  0.0)    ,  0.0            )
   call  testit_p('r2d',  r2d(  x/4)    ,  45.0           )
   call  testit_p('r2d',  r2d(  -x/4)   ,  -45.0          )
   call  testit_p('r2d',  r2d(  x/2)    ,  90.0           )
   call  testit_p('r2d',  r2d(  x)      ,  180.0          )

   values=[0.0, 30.0, 45.0, 60.0, 90.0, 120.0, 135.0, 150.0, 180.0, 210.0, 240.0, 270.0, 300.0, 330.0, 360.0, -45.0]

   write(*,'(80("="))')
   write(*,*)'Trig Functions passed degrees for', values
   do i=1,size(values)
      write(*,'(80("-"))')
      write(*,*)'ANGLE(degrees)=',values(i)
      call  testit_p('sind',   sind(values(i))             ,  sin(d2r(values(i)))  )
      call  testit_p('cosd',   cosd(values(i))             ,  cos(d2r(values(i)))  )
      call  testit_p('tand',   tand(values(i))             ,  tan(d2r(values(i)))  )
   enddo

   ! need to work on a good test for arc routines
   write(*,'(80("="))')

   do i=1,size(values)
      if(abs(d2r(values(1))).le.1)then
         write(*,*)'test asind and acosd for ',values(1),d2r(values(1))
         call testit_p( 'asind', asin(sin(d2r(values(i)))) , d2r(asind(sind(values(i)))) )
         call testit_p( 'acosd', acos(cos(d2r(values(i)))) , d2r(acosd(cosd(values(i)))) )
      endif
   enddo

   !write(*,'(80("="))')
   !call testit_p('acosd', ACOS(0.54030231) , d2r(ACOSD(r2d(0.54030231))) )
   !call testit_p('atand', ATAN(1.5574077)  , d2r(ATAND(r2d(1.5574077 ))) )

   call unit_check_good('M_units')
end program testit
!===================================================================================================================================
subroutine testit_p(label,v1,v2)
USE M_Compare_Float_Numbers
use M_math, only : accdig
use M_debug, only : unit_check_good, unit_check_bad
real,intent(in)    :: v1, v2
character(len=*)   :: label
logical            :: stat
real               :: significant_digits
   stat=v1 .EqualTo. v2
!-----------------------
   if(.not.stat)then
!     INPUT ...
!     real,intent(in) :: x           ! First  of two real numbers to be compared.
!     real,intent(in) :: y           ! Second of two real numbers to be compared.
!     real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
!     integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
!                                    ! = 1, If tolerance is not satisified.
!     real,intent(out) :: acurcy     ! = - LOG10 (ABS((X-Y)/Y)))
      significant_digits=int(log10(2.0**digits(0.0)))     ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A REAL NUMBER.
      write(*,*)'SIG=',significant_digits
      call accdig (v1,v2,significant_digits,ACURCY,IND)
      if(ind.eq.0)stat=.true.
   endif
!-----------------------
   if(stat)then
      write(*,*)label,stat, v1, v2
      call unit_check_good(trim(label))
   else
      write(*,*)'error:',label,stat,v1,v2
      call unit_check_bad(trim(label))
      stop 1
   endif
end subroutine testit_p
!  call check_unit('M_units', a .eq. b )
