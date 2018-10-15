program test_d2u
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : d2u
implicit none
   call unit_check_start('d2u')
!  Note that time zones are usually -HHMM or -HH:MM and not MM, which is what the DAT array uses
!  Comparing to Unix date(1) command:
!    date --date "Wed Mar 29 01:46:47 EDT 2017" +%s      ! 1490766407
!    date --date "Wed Mar 29 01:46:47 2017" +%s          ! 1490766407
!    date --date "Wed Mar 29 01:46:47 -400 2017" +%s     ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-400 2017" +%s  ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-4:00 2017" +%s ! 1490766407
   write(*,*)d2u([2017,03,29,-240,01,46,47,0])
   call unit_check('d2u',nint(d2u([2017,03,29,-240,01,46,47,0])+0.5).eq.1490766407)
   call unit_check_good('d2u')
end program test_d2u

