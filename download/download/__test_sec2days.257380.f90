program test_sec2days
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : sec2days
implicit none
   call unit_check_start('sec2days')
   call unit_check('sec2days',sec2days(129860).eq.             '1-12:04:20')
   call unit_check('sec2days',sec2days(80000.0d0).eq.          '0-22:13:20')
   call unit_check('sec2days',sec2days(80000,crop=.true.).eq.    '22:13:20')
   call unit_check('sec2days',sec2days('1day 2hr 3 min 4s').eq.'1-02:03:04')
   call unit_check_good('sec2days')
end program test_sec2days
