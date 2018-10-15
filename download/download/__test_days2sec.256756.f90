program test_days2sec
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only  : days2sec, realtime
implicit none
   call unit_check_start('days2sec')
   call unit_check('days2sec',days2sec('1-12:04:20').eq.  129860.00000000000_realtime)
   call unit_check('days2sec',days2sec('1').eq.  1.0000000000000000_realtime)
   call unit_check('days2sec',days2sec('1:00').eq.  60.000000000000000_realtime)
   call unit_check('days2sec',days2sec('1:00:00').eq.  3600.0000000000000_realtime)
   call unit_check('days2sec',days2sec('1-00:00:00').eq.86400.000000000000_realtime)
   call unit_check('days2sec',days2sec('1d2h 3.0 minutes 4sec').eq.93784.000000000000_realtime)

   call unit_check('days2sec',nint(days2sec(' 1-12:04:20              ')) .eq. 129860)
   call unit_check('days2sec',nint(days2sec(' 1.5 days                ')) .eq. 129600)
   call unit_check('days2sec',nint(days2sec(' 1.5 days 4hrs 30minutes ')) .eq. 145800)
   call unit_check('days2sec',nint(days2sec(' 1.5d                    ')) .eq. 129600)
   call unit_check('days2sec',nint(days2sec(' 1d2h3m4s                ')) .eq. 93784)
          ! duplicates
   call unit_check('days2sec',nint(days2sec(' 1d1d1d                  ')) .eq. 259200)
          ! negative values
   call unit_check('days2sec',nint(days2sec(' 4d-12h                  ')) .eq. 302400)
   call unit_check('days2sec',nint(days2sec(' 3  d  1 2   h           ')) .eq. 302400)

   call unit_check_good('days2sec')

end program test_days2sec
