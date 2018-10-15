program test_easter
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time,  only : easter
implicit none
character(len=20),parameter  :: tests(*)=[ &
'1980,4,6  ',  &
'1981,4,19 ',  &
'1982,4,11 ',  &
'1983,4,3  ',  &
'1984,4,22 ',  &
'1985,4,7  ',  &
'1986,3,30 ',  &
'1987,4,19 ',  &
'1988,4,3  ',  &
'1989,3,26 ',  &
'1990,4,15 ',  &
'1991,3,31 ',  &
'1992,4,19 ',  &
'1993,4,11 ',  &
'1994,4,3  ',  &
'1995,4,16 ',  &
'1996,4,7  ',  &
'1997,3,30 ',  &
'1998,4,12 ',  &
'1999,4,4  ',  &
'2000,4,23 ',  &
'2001,4,15 ',  &
'2002,3,31 ',  &
'2003,4,20 ',  &
'2004,4,11 ',  &
'2005,3,27 ',  &
'2006,4,16 ',  &
'2007,4,8  ',  &
'2008,3,23 ',  &
'2009,4,12 ',  &
'2010,4,4  ',  &
'2011,4,24 ',  &
'2012,4,8  ',  &
'2013,3,31 ',  &
'2014,4,20 ',  &
'2015,4,5  ',  &
'2016,3,27 ',  &
'2017,4,16 ',  &
'2018,4,1  ',  &
'2019,4,21 ',  &
'2020,4,12 ',  &
'2021,4,4  ',  &
'2022,4,17 ',  &
'2023,4,9  ',  &
'2024,3,31 '   ]

integer :: tmonth, tday
integer :: inyear, outmonth, outday
integer :: ii

integer            :: day(1980:2024), month(1980:2024), year, y2, y3
character (len=5)  :: mon(3:4) = (/ 'march', 'april' /)

! display sample calls

do year = 1980, 2024
  call easter(year, month(year), day(year))
end do

do year = 1980, 1994
  y2 = year + 15
  y3 = y2 + 15

  write(*, '(3("  ", i4, "  ", a5, i3, "      "))')  &
           year, mon(month(year)), day(year),  &
           y2, mon(month(y2)), day(y2),  &
           y3, mon(month(y3)), day(y3)
end do

! run unit tests

call unit_check_start('easter','-library libGPF') ! indicate beginning tests

do ii=1,size(tests)
   read(tests(ii),*)inyear,tmonth,tday
   call easter(inyear,outmonth,outday)
   call unit_check('easter', tmonth.eq.outmonth.and.tday.eq.outday )
enddo

call unit_check_good('easter') ! assume if got here passed checks

end program test_easter
