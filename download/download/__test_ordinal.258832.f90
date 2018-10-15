program test_ordinal
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : o2d, ordinal_to_date, d2o
implicit none
integer                      :: iday,iyear,omonth,oday,rday
integer                      :: i,dat(8)

character(len=40),parameter  :: tests(*)=[ &
   'ordinal  year  month  month_day  ',  &
   '100      2004  4      9          ',  &
   '100      2005  4      10         ',  &
   '100      2006  4      10         ',  &
   '100      2007  4      10         ',  &
   '100      2008  4      9          ',  &
   '100      2016  4      9          ']

    write(*,*)'begin test_ordinal'

   call unit_check_start('ordinal_to_date','-library libGPF') ! indicate beginning tests
    do i=2,size(tests)
       read(tests(i),*)iday,iyear,omonth,oday
       call ordinal_to_date(iyear,iday,dat)
       call unit_check('ordinal_to_date',dat(2).eq.omonth.and.dat(3).eq.oday)
    enddo
    call unit_check_good('ordinal_to_date') ! assume if got here passed checks

    call unit_check_start('o2d','-library libGPF') ! indicate beginning tests
    call unit_check_start('d2o','-library libGPF') ! indicate beginning tests
    do i=2,size(tests)
       read(tests(i),*)iday,iyear,omonth,oday
       dat=o2d(iday,iyear)
       call unit_check('o2d',dat(2).eq.omonth.and.dat(3).eq.oday)
       rday=d2o(dat)
       call unit_check('d2o',iday.eq.rday)
    enddo
    call unit_check_good('o2d') ! assume if got here passed checks
    call unit_check_good('d2o') ! assume if got here passed checks

    write(*,*)'finished test_ordinal'

end program test_ordinal
