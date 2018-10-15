program testit
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time,     only : mo2v
call unit_check('mo2v', mo2v('jan')       .eq.  1   ,'Check January')
call unit_check('mo2v', mo2v('Feb')       .eq.  2   ,'Check Febuary')
call unit_check('mo2v', mo2v('March')     .eq.  3   ,'Check March')
call unit_check('mo2v', mo2v('APR')       .eq.  4   ,'Check April')
call unit_check('mo2v', mo2v('may')       .eq.  5   ,'Check May')
call unit_check('mo2v', mo2v('jun')       .eq.  6   ,'Check Jun')
call unit_check('mo2v', mo2v('july')      .eq.  7   ,'Check July')
call unit_check('mo2v', mo2v('Aug')       .eq.  8   ,'Check August')
call unit_check('mo2v', mo2v('Sept')      .eq.  9   ,'Check September')
call unit_check('mo2v', mo2v('Oct')       .eq.  10  ,'Check October')
call unit_check('mo2v', mo2v('Nov')       .eq.  11  ,'Check November')
call unit_check('mo2v', mo2v('December')  .eq.  12  ,'Check December')
call unit_check('mo2v', mo2v('jax')       .eq.  1   ,'Check "jax"')
call unit_check('mo2v', mo2v('ja')        .eq.  1   ,'Check "ja"')
call unit_check('mo2v', mo2v('j')         .eq. -1   ,'Check "j"')
call unit_check('mo2v', mo2v('')          .eq. -1   ,'Check ""')
call unit_check_good('mo2v') ! assume if got here passed checks
end program testit
