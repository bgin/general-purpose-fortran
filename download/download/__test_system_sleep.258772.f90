program test_system_sleep
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only: system_sleep
implicit none
   integer :: systemcount1, countrate1
   integer :: systemcount2, countrate2
   real    :: slept_in_seconds
   call unit_check_start('system_sleep','-library libGPF') ! indicate beginning tests
   call system_clock(count=systemcount1,count_rate=countrate1)
   call system_sleep(10)
   call system_clock(count=systemcount2,count_rate=countrate2)
   slept_in_seconds=real(systemcount2-systemcount1)/real(countrate1)
   write(*,*)'Asked to sleep 10 seconds, slept ',slept_in_seconds,' seconds'
   ! a sluggish system might make more than 12 seconds pass
   call unit_check('system_sleep',  slept_in_seconds .ge. 9.5 .and. slept_in_seconds .le. 12.0 )
   call unit_check_good('system_sleep') ! assume if got here passed checks
end program test_system_sleep
