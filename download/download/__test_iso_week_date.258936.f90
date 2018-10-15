program iso_week_date_demo
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : guessdate, w2d, d2w, fmtdate
implicit none
character(len=80)  :: date1
character(len=80)  :: date2
character(len=80)  :: iso_week_date
character(len=132) :: comment
character(len=10)  :: iso_name
character(len=10)  :: status
character(len=372),allocatable :: line(:)
integer            :: dat(8)
integer            :: dat2(8)
integer            :: iso_year
integer            :: iso_week
integer            :: iso_weekday
integer            :: i

! the data file with dates to read and expected answers and comments
line=[ character(len=372) :: &

& ' "Sat 1 Jan 2005",  "2005-01-01", "2004-W53-6", " " ', &
& ' "Sun 2 Jan 2005",  "2005-01-02", "2004-W53-7", " " ', &
& ' "Sat 31 Dec 2005", "2005-12-31", "2005-W52-6", " " ', &
& ' "Mon 1 Jan 2007",  "2007-01-01", "2007-W01-1", "Both years 2007 start with the same day." ', &
& ' "Sun 30 Dec 2007", "2007-12-30", "2007-W52-7", " " ', &
& ' "Mon 31 Dec 2007", "2007-12-31", "2008-W01-1", " " ', &
& ' "Tue 1 Jan 2008",  "2008-01-01", "2008-W01-2", &
& "Gregorian year 2008 is a leap year. ISO year 2008 is 2 days shorter: 1 day longer at the start,  3 days shorter at the end." ', &
& ' "Sun 28 Dec 2008", "2008-12-28", "2008-W52-7", " ISO year 2009 begins three days before the end of Gregorian 2008." ', &
& ' "Mon 29 Dec 2008", "2008-12-29", "2009-W01-1", " " ', &
& ' "Tue 30 Dec 2008", "2008-12-30", "2009-W01-2", " " ', &
& ' "Wed 31 Dec 2008", "2008-12-31", "2009-W01-3", " " ', &
& ' "Thu 1 Jan 2009",  "2009-01-01", "2009-W01-4", " " ', &
& ' "Thu 31 Dec 2009", "2009-12-31", "2009-W53-4", "ISO year 2009 has 53 weeks and ends three days into Gregorian year 2010." ', &
& ' "Fri 1 Jan 2010",  "2010-01-01", "2009-W53-5", " " ', &
& ' "Sat 2 Jan 2010",  "2010-01-02", "2009-W53-6", " " ', &
& ' "Sun 3 Jan 2010",  "2010-01-03", "2009-W53-7", " " ', &

&' ' ]
do i=1,size(line)-1
   read(line(i),*)date1,date2,iso_week_date,comment
   write(*,'(a)')repeat("=",80)
   write(*,'(a,a)')'GIVEN:',trim(date1)
   if(comment.ne.' ')then
      write(*,'(3x,a)')trim(comment)
   endif
   write(*,'(a)')'CALCULATE:'
   call guessdate(date1,dat)                                         ! convert date string to DAT
   write(*,'(3x,"DAT:",*(i0:,","))')dat                              ! write raw DAT date-time calculated by guessdate
   write(*,'(3x,a)')fmtdate(dat)                                     ! print DAT using default format
   status=merge('PASSED','ERROR!',fmtdate(dat,'year-month-day').eq.trim(date2))
   write(*,'(3x,3(a:,3x))')fmtdate(dat,'year-month-day'),trim(date2),status ! print DAT using default format
   if(status.ne.'PASSED')then
      write(*,*)'ERROR IN program iso_week_date_demo'
      stop 2
   endif
   call d2w(dat,iso_year,iso_week,iso_weekday,iso_name)              ! convert DAT to ISO week date
   call w2d(iso_year,iso_week,iso_weekday,dat2)                      ! convert ISO week date to DAT
   write(*,'(3x,"DAT2:",*(i0:,","))')dat2                            ! write raw DAT date-time created by w2d
                                                         ! print DAT as ISO week date, expected value, d2w-w2d value, d2w output
                                                         ! all generated dates should match ISO week date
   status=merge('PASSED','ERROR!', all([fmtdate(dat,"%I"),fmtdate(dat2,"%I"),trim(iso_name)].eq.iso_week_date)) ! all should match
   write(*,'(3x,5(a,3x))')fmtdate(dat,"%I"),trim(iso_week_date),fmtdate(dat2,"%I"),trim(iso_name),status
   if(status.ne.'PASSED')then
      write(*,*)'ERROR IN program iso_week_date_demo'
      stop 2
   endif
   call unit_check('w2d', .true.)
enddo
end program iso_week_date_demo
