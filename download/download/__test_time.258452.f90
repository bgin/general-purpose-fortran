program test_date_to_julian
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time,    only : date_to_julian, now, fmtdate, date_to_unix, realtime
implicit none
real(kind=realtime) :: julian
real(kind=realtime) :: unixtime
character(len=1024) :: format
integer             :: values(8)
integer             :: ierr
   call date_to_julian( [1970, 1, 1,0, 0,0,0,0] ,julian,ierr); write(*,*)julian

   write(*,*)'Checking Julian Date:'
   call unit_check_start('date_to_julian','-library libGPF')
   call date_to_julian( [1995, 1, 1,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2449719 ,"Jan  1st, 1995 12:00(2449719)")
   call date_to_julian( [1995,10,19,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450010, "Oct 19th, 1995 12:00(2450010)")
   call date_to_julian( [1995,12,31,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450083, "Dec 31st, 1995 12:00(2450083)")
   call date_to_julian( [1996, 1, 1,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450084, "Jan  1st, 1996 12:00(2450084)")
   call date_to_julian( [1996,12,31,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450449, "Dec 31th, 1996 12:00(2450449)")

   call unit_check_good('date_to_julian') ! assume if got here passed checks

   write(*,*) now()
   write(*,*) now('')
   write(*,*) now('Now is %M-%D-%Y %h:%m:%s ')
!-------------------------------------------------------------------------------
   call date_and_time(values=values)
   call date_to_unix(values,unixtime,ierr)
   write(*,*)'UNIXTIME ',unixtime
!-------------------------------------------------------------------------------
   format=' %Y %M %D %h %m %s '
   write(*,*) fmtdate(values,format)
   write(*,*) fmtdate(values,'%Y/%M/%D %h:%m:%s %z')
   write(*,*) fmtdate(values,'Julian Date is %J (how many digits)')
   write(*,*) fmtdate(values,format)
   write(*,*) fmtdate(values,'Day of week %W')
   write(*,*) fmtdate(values,'Day of week %w')
!-------------------------------------------------------------------------------
!   call system_clock (count=count, count_rate=count_rate, count_max=count_max)
!-----------------------------------------------------------------------------------------------------------------------------------
end program test_date_to_julian
