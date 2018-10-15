!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program testit_d2w
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : d2w
implicit none
integer          :: dat(8)
integer          :: iyear,iweek,iweekday
character(len=10):: name
   call date_and_time(values=dat)
   write(*,*)'========================================================================='
   write(*,*)'current time is ',dat
   call d2w(dat,iyear,iweek,iweekday,name)
   write(*,'(i4.4,"-W",i2.2,"-",i1,1x,a)')iyear,iweek,iweekday,name
   write(*,*)'========================================================================='
   write(*,*)'Monday 29 December 2008 is written "2009-W01-1"'
   call d2w([2008,12,29,dat(4),0,0,0,0],iyear,iweek,iweekday,name) ! Monday 29 December 2008 is written "2009-W01-1"
   write(*,'(i4.4,"-W",i2.2,"-",i1,1x,a)')iyear,iweek,iweekday,name
   write(*,*)'========================================================================='
   write(*,*)'Sunday  3  January 2010 is written "2009-W53-7"'
   call d2w([2010,01,03,dat(4),0,0,0,0],iyear,iweek,iweekday,name) ! Sunday  3  January 2010 is written "2009-W53-7"
   write(*,'(i4.4,"-W",i2.2,"-",i1,1x,a)')iyear,iweek,iweekday,name
   write(*,*)'========================================================================='
   write(*,*) "Examples of contemporary dates around New Year's Day"
   dat=[2005,01,01,dat(4),0,0,0,0] !  Sat 1 Jan 2005 2005-01-01 2004-W53-6
   call showme("2004-W53-6")
   dat=[2005,01,02,dat(4),0,0,0,0] !  Sun 2 Jan 2005 2005-01-02 2004-W53-7
   call showme("2004-W53-7")
   dat=[2005,12,31,dat(4),0,0,0,0] !  Sat 31 Dec 2005 2005-12-31 2005-W52-6
   call showme("2005-W52-6")
   dat=[2007,01,01,dat(4),0,0,0,0] !  Mon 1 Jan 2007 2007-01-01 2007-W01-1 Both years 2007 start with the same day.
   call showme("2007-W01-1")
   dat=[2007,12,30,dat(4),0,0,0,0] !  Sun 30 Dec 2007 2007-12-30 2007-W52-7
   call showme("2007-W52-7")
   dat=[2007,12,31,dat(4),0,0,0,0] !  Mon 31 Dec 2007 2007-12-31 2008-W01-1
   call showme("2008-W01-1")
   dat=[2008,01,01,dat(4),0,0,0,0] !  Tue 1 Jan 2008 2008-01-01 2008-W01-2
                                   !     Gregorian year 2008 is a leap year. ...
                                   !     ISO year 2008 is 2 days shorter: 1 day longer at the start, 3 days shorter at the end.
   call showme("2008-W01-2")
   dat=[2008,12,28,dat(4),0,0,0,0] !  Sun 28 Dec 2008 2008-12-28 2008-W52-7 ...
                                   !     ISO year 2009 begins three days before the end of Gregorian 2008.
   call showme("2008-W52-7")
   dat=[2008,12,29,dat(4),0,0,0,0] !  Mon 29 Dec 2008 2008-12-29 2009-W01-1
   call showme("2009-W01-1")
   dat=[2008,12,30,dat(4),0,0,0,0] !  Tue 30 Dec 2008 2008-12-30 2009-W01-2
   call showme("2009-W01-2")
   dat=[2008,12,31,dat(4),0,0,0,0] !  Wed 31 Dec 2008 2008-12-31 2009-W01-3
   call showme("2009-W01-3")
   dat=[2009,01,01,dat(4),0,0,0,0] !  Thu 1 Jan 2009 2009-01-01 2009-W01-4
   call showme("2009-W01-4")
   dat=[2009,12,31,dat(4),0,0,0,0] !  Thu 31 Dec 2009 2009-12-31 2009-W53-4  ...
                                   !     ISO year 2009 has 53 weeks and ends three days into Gregorian year 2010.
   call showme("2009-W53-4")
   dat=[2010,01,01,dat(4),0,0,0,0] !  Fri 1 Jan 2010 2010-01-01 2009-W53-5
   call showme("2009-W53-5")
   dat=[2010,01,02,dat(4),0,0,0,0] !  Sat 2 Jan 2010 2010-01-02 2009-W53-6
   call showme("2009-W53-6")
   dat=[2010,01,03,dat(4),0,0,0,0] !  Sun 3 Jan 2010 2010-01-03 2009-W53-7
   call showme("2009-W53-7")
   write(*,*)'========================================================================='
   call unit_check_good('d2w') ! assume if got here passed checks

contains

subroutine showme(string)
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
character(len=*) :: string
   call d2w(dat,iyear,iweek,iweekday,name)
   write(*,'(i4.4,"-W",i2.2,"-",i1,/,1x,a,/,1x,a)')iyear,iweek,iweekday,name,string
   call unit_check('d2w', name.eq.string ,string)
end subroutine showme

end program testit_d2w
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
