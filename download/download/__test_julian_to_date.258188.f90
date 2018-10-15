program demo_julian_to_date
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : julian_to_date, fmtdate, realtime
   implicit none
   real(kind=realtime)     :: juliandate
   integer                 :: dat(8)
   integer                 :: ierr
   ! set sample Julian Date
   juliandate=2457589.129d0
   ! create DAT array for this date
   call julian_to_date(juliandate,dat,ierr)

   call unit_check_start('julian_to_date')

   call julian_to_date( 2457589.129d0, dat, ierr)

!!   call  unit_check('julian_to_date', answer .eq. NN_realtime)
!!   call  unit_check('julian_to_date', answer .eq. NN_realtime)
!!   call  unit_check('julian_to_date', answer .eq. NN_realtime)
!!   call  unit_check('julian_to_date', answer .eq. NN_realtime)
!!   call  unit_check('julian_to_date', answer .eq. NN_realtime)

!!   call unit_check_good('julian_to_date')

    end program demo_julian_to_date
