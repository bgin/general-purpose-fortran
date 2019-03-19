!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module m_time
use M_strings, only : upper, lower,  substitute
use M_debug, only : stderr
implicit none
private
private stderr
private upper
!-----------------------------------------------------------------------------------------------------------------------------------
! UNIT TESTS
   public test_suite_M_string

! EPOCH TIME (UT starts at 0000 on 1 Jan. 1970)
   public date_to_unix   !(dat,UNIXTIME,IERR)                 ! Convert date array to Unix Time
   public unix_to_date   !(unixtime,DAT,IERR)                 ! Convert Unix Time to date array
   public d2u            !(dat) result (UNIXTIME)             ! Convert date array to Unix Time
   public u2d            !(unixtime) result (DAT)             ! Convert Unix Time to date array
! JULIAN
   public julian_to_date !(julian,DAT,IERR)                   ! Convert Julian Date to date array
   public date_to_julian !(dat,JULIAN,IERR)                   ! Convert date array to Julian Date
   public d2j            !(dat) result (JULIAN)               ! Convert date array to Julian Date
   public j2d            !(julian) result (DAT)               ! Convert Julian Date to date array
! DAY OF WEEK
   public dow            !(dat,[WEEKDAY],[DAY],IERR)          ! Convert date array to day of the week as number(Mon=1) and name
! WEEK OF YEAR
   public d2w  !(dat,ISO_YEAR,ISO_WEEK,ISO_WEEKDAY,ISO_NAME)  ! Calculate iso-8601 Week-numbering year date yyyy-Www-d
   public w2d  !(iso_year,iso_week,iso_weekday,DAT)           ! given iso-8601 Week-numbering year date yyyy-Www-d calculate date
! ORDINAL DAY
   public d2o            !(dat) result(ORDINAL)               ! given date array return ordinal day of year, Jan 1st=1
   public o2d            !(ordinal) result(DAT)               ! given ordinal day of year return date array, Jan 1st=1
   public ordinal_to_date!(year,ordinal_day,DAT)              ! given ordinal day of year return date array, Jan 1st=1
! PRINTING DATES
   public fmtdate        !(dat,format) result (TIMESTR)       ! Convert date array to string using format
   public fmtdate_usage  !(indent)                            ! display macros recognized by fmtdate(3f)
   public now            !(format) result (NOW)               ! return string representing current time given format
   public box_month      !(dat,CALEN)                         ! print specified month into character array
! PRINTING DURATIONS
   public sec2days       !(seconds) result (dhms)             ! converts seconds to string D-HH:MM:SS
   public days2sec       !(str) result (seconds)              ! converts string D-HH:MM:SS to seconds from small to large
! MONTH NAME
   public mo2v           !(month_name) result (MONTH_NUMBER)  ! given month name return month number
   public v2mo           !(month_number) result (MONTH_NAME)  ! given month number return month name
   public mo2d           !(month_name) result (DAT)           ! return date array for first day of given month name in current year
! ASTROLOGICAL
   public easter         !(year,month,day)                    ! calculate month and day Easter falls on for given year
   public moon_fullness  !(datin) result(FULLNESS)            ! percentage of moon phase from new to full
   public phase_of_moon  !(datin) result(PHASE)               ! return name for phase of moon for given date
   public ephemeris      !(dat,planet,DD,DM,DC,AH,AM)         ! ephemeris position of planets for adjusting an equatorial telescope
! READING DATES
   public guessdate      !(anot,dat)                          ! Converts a date string to a date array, in various formats
! C INTERFACE
   public system_sleep   !(wait_seconds)                      ! Call sleep(3c)
   private call_sleep
   private call_usleep
!-----------------------------------------------------------------------------------------------------------------------------------
   integer,parameter,public       :: realtime=kind(0.0d0)     ! type for 1 epoch time and julian days
!-----------------------------------------------------------------------------------------------------------------------------------
! INTERNAL
   real(kind=realtime),parameter,private :: SECDAY=86400.0d0  ! 24:00:00 hours as seconds
!-----------------------------------------------------------------------------------------------------------------------------------
!  integer,parameter       :: igreg_1582=15+31*(10+12*1582)   ! ASSUMES: Gregorian Calendar was adopted 15 Oct. 1582 (588829)
!  integer,parameter       :: igreg_1752=03+31*( 9+12*1752)   ! ASSUMES: Gregorian Calendar was adopted 3 Sep. 1752 (652026)
!  integer,save            :: igreg=igreg_1582
!-----------------------------------------------------------------------------------------------------------------------------------
! CONVENIENT CONSTANTS FOR USE WITH + AND - OPERATORS
real(kind=realtime),public,parameter :: dt_minute=60.0d0      ! one minute in seconds
real(kind=realtime),public,parameter :: dt_hour=3600.0d0      ! one hour in seconds
real(kind=realtime),public,parameter :: dt_day=86400.0d0      ! 24:00:00 hours in seconds
real(kind=realtime),public,parameter :: dt_week=dt_day*7.0d0  ! one week in seconds
!-----------------------------------------------------------------------------------------------------------------------------------
 contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_julian(3f) - [M_time] converts DAT date-time array to Julian Date
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_julian(dat,juliandate,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: juliandate
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!    Converts a DAT date-time array to a Unix Epoch Time (UET) value.
!!    UET is the number of seconds since 00:00 on January 1st, 1970, UTC.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!##RETURNS
!!    juliandate  A Julian Ephemeris Date (JED) is the number of days since
!!                noon (not midnight) on January 1st, 4713 BC.
!!    ierr        Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_date_to_julian
!!     use M_time, only : date_to_julian,realtime
!!     implicit none
!!     integer             :: dat(8)
!!     real(kind=realtime) :: juliandate
!!     integer             :: ierr
!!        ! generate DAT array
!!        call date_and_time(values=dat)
!!        ! show DAT array
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        ! convert DAT to Julian Date
!!        call date_to_julian(dat,juliandate,ierr)
!!        write(*,*)'Julian Date is ',juliandate
!!        write(*,*)'ierr is ',ierr
!!     end program demo_date_to_julian
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:11:3:13:821
!!     Julian Date is    2457589.1272432986
!!     ierr is            0
!===================================================================================================================================
subroutine date_to_julian(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!! AUTHOR:    John S. Urban
!!##VERSION:   1.0 2015-12-21
!! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
!===================================================================================================================================
! * There is no year zero
! * Julian Date must be non-negative
! * Julian Date starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_1="&
&@(#)M_time::date_to_julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date"

integer,intent(in)               :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  :: julian   ! Julian Date (non-negative, but may be non-integer)
integer,intent(out)              :: ierr     ! Error return: 0 =successful execution,-1=invalid year,-2=invalid month,-3=invalid day
                                             ! -4=invalid date (29th Feb, non leap-year)
   integer                       :: year, month, day, utc, hour, minute
   real(kind=realtime)           :: second
   integer                       :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian.lt.0.d0) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine date_to_julian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_date_to_julian()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time,    only : date_to_julian, now, fmtdate, date_to_unix, realtime
implicit none
real(kind=realtime) :: julian
integer             :: ierr

   call unit_check_start('date_to_julian',msg='Checking Julian Date') ! assume if got here passed checks

   call date_to_julian( [1970, 1, 1,0, 0,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',abs(julian-2440587.5).lt.0.00001 ,msg="Dec 31st, 1969  8:00(2440587.5)")

   call date_to_julian( [1995, 1, 1,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2449719 ,msg="Jan  1st, 1995 12:00(2449719)")

   call date_to_julian( [1995,10,19,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450010, msg="Oct 19th, 1995 12:00(2450010)")

   call date_to_julian( [1995,12,31,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450083, msg="Dec 31st, 1995 12:00(2450083)")

   call date_to_julian( [1996, 1, 1,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450084, msg="Jan  1st, 1996 12:00(2450084)")

   call date_to_julian( [1996,12,31,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450449, msg="Dec 31th, 1996 12:00(2450449)")

   call unit_check_done('date_to_julian')

end subroutine test_date_to_julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    julian_to_date(3f) - [M_time] converts a JED(Julian Ephemeris Date) to a DAT date-time array.
!!
!!##SYNOPSIS
!!
!!    subroutine julian_to_date(julian,dat,ierr)
!!
!!     real(kind=realtime),intent(in) :: julian
!!     integer,intent(out)            :: dat(8)
!!     integer,intent(out)            :: ierr
!!
!!##DESCRIPTION
!!    Converts a Unix Epoch Time (UET) value to a DAT date-time array.
!!    UET is the number of seconds since 00:00 on January 1st, 1970, UTC.
!!
!!##OPTIONS
!!     julian   Julian Date (days)
!!     dat      Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f).
!!     ier      0 for successful execution
!!
!!               dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC.
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_julian_to_date
!!     use M_time, only : julian_to_date, fmtdate, realtime
!!     implicit none
!!     real(kind=realtime)     :: juliandate
!!     integer                 :: dat(8)
!!     integer                 :: ierr
!!        ! set sample Julian Date
!!        juliandate=2457589.129d0
!!        ! create DAT array for this date
!!        call julian_to_date(juliandate,dat,ierr)
!!        write(*,*)'Sample Date=',fmtdate(dat)
!!        ! go back one day
!!        call julian_to_date(juliandate-1.0d0,dat,ierr)
!!        write(*,*)'Day Before =',fmtdate(dat)
!!        ! go forward one day
!!        call julian_to_date(juliandate+1.0d0,dat,ierr)
!!        write(*,*)'Day After  =',fmtdate(dat)
!!     end program demo_julian_to_date
!!
!!    results:
!!
!!     Sample Date=Tuesday, July 19th, 2016 11:05:45 AM UTC-04:00
!!     Day Before =Monday, July 18th, 2016 11:05:45 AM UTC-04:00
!!     Day After  =Wednesday, July 20th, 2016 11:05:45 AM UTC-04:00
!===================================================================================================================================
subroutine julian_to_date(julian,dat,ierr)

character(len=*),parameter::ident_2="@(#)M_time::julian_to_date(3f): Converts Julian Date to DAT date-time array"

real(kind=realtime),intent(in)   :: julian            ! Julian Date (non-negative)
integer,intent(out)              :: dat(8)
integer,intent(out)              :: ierr              ! 0 for successful execution, otherwise 1
   integer                 :: tz
   real(kind=realtime)     :: second
   integer                 :: year
   integer                 :: month
   integer                 :: day
   integer                 :: hour
   integer                 :: minute
   integer                 :: jalpha,ja,jb,jc,jd,je,ijul

   if(julian.lt.0.d0) then                      ! Negative Julian Date not allowed
      ierr=1
      return
   else
      ierr=0
   endif
   tz=get_timezone()

   ijul=idint(julian)                           ! Integral Julian Date
   second=sngl((julian-dble(ijul))*secday)      ! Seconds from beginning of Jul. Day
   second=second+(tz*60)

   if(second.ge.(secday/2.0d0)) then            ! In next calendar day
      ijul=ijul+1
      second=second-(secday/2.0d0)              ! Adjust from noon to midnight
   else                                         ! In same calendar day
      second=second+(secday/2.0d0)              ! Adjust from noon to midnight
   endif

   if(second.ge.secday) then                    ! Final check to prevent time 24:00:00
      ijul=ijul+1
      second=second-secday
   endif

   minute=int(second/60.0)                      ! Integral minutes from beginning of day
   second=second-float(minute*60)               ! Seconds from beginning of minute
   hour=minute/60                               ! Integral hours from beginning of day
   minute=minute-hour*60                        ! Integral minutes from beginning of hour

   !---------------------------------------------
   jalpha=idint((dble(ijul-1867216)-0.25d0)/36524.25d0) ! Correction for Gregorian Calendar
   ja=ijul+1+jalpha-idint(0.25d0*dble(jalpha))
   !---------------------------------------------

   jb=ja+1524
   jc=idint(6680.d0+(dble(jb-2439870)-122.1d0)/365.25d0)
   jd=365*jc+idint(0.25d0*dble(jc))
   je=idint(dble(jb-jd)/30.6001d0)
   day=jb-jd-idint(30.6001d0*dble(je))
   month=je-1

   if(month.gt.12)then
      month=month-12
   endif

   year=jc-4715
   if(month.gt.2)then
      year=year-1
   endif

   if(year.le.0)then
      year=year-1
   endif

   dat=[ year, month, day, tz, hour, minute, int(second), int((second-int(second))*1000.0)]
   ierr=0

end subroutine julian_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julian_to_date()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : julian_to_date, fmtdate, realtime
   implicit none
   real(kind=realtime)          :: juliandate
   integer                      :: dat(8)
   integer                      :: ierr
   character(len=:),allocatable :: expected

   call unit_check_start('julian_to_date')

   juliandate=2457589.129d0                 ! set sample Julian Date
   call julian_to_date(juliandate,dat,ierr) ! create DAT array for this date
   expected='Tuesday, July 19th, 2016 11:05:45 AM'
   expected='2016-07-19 11:05:45'
   write(*,*)fmtdate(dat,'year-month-day hour:minute:second')
   call unit_check('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second').eq.expected,msg=msg(juliandate,'==>',expected))

   call julian_to_date(juliandate-1.0d0,dat,ierr) ! go back one day
   expected='Monday, July 18th, 2016 11:05:45 AM'
   expected='2016-07-18 11:05:45'
   call unit_check('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second').eq.expected,msg=msg(juliandate,'==>',expected))

   call julian_to_date(juliandate+1.0d0,dat,ierr) ! go forward one day
   expected='Wednesday, July 20th, 2016 11:05:45 AM'
   expected='2016-07-20 11:05:45'
   call unit_check('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second').eq.expected,msg=msg(juliandate,'==>',expected))

   call unit_check_done('julian_to_date')

end subroutine test_julian_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_unix(3f) - [M_time] converts DAT date-time array to Unix Epoch Time
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_unix(dat,unixtime,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: unixtime
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!
!!    Converts a DAT date-time array to a UET (Unix Epoch Time).
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC.
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_date_to_unix
!!     use M_time, only : date_to_unix, realtime
!!     implicit none
!!     integer             :: dat(8)
!!     real(kind=realtime) :: unixtime
!!     integer             :: ierr
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        call date_to_unix(dat,unixtime,ierr)
!!        write(*,*)'Unix Epoch time is ',unixtime
!!        write(*,*)'ierr is ',ierr
!!     end program demo_date_to_unix
!!
!!    results:
!!
!!     Today is:2016:7:18:-240:23:44:20:434
!!     Unix Epoch time is    1468899860.4340105
!!     ierr is            0
!===================================================================================================================================
subroutine date_to_unix(dat,unixtime,ierr)

character(len=*),parameter::ident_3="@(#)M_time::date_to_unix(3f): Convert DAT date-time array to Unix Epoch Time"

integer,intent(in)              :: dat(8)       ! date time array similar to that returned by DATE_AND_TIME
real(kind=realtime),intent(out) :: unixtime     ! Unix time (seconds)
integer,intent(out)             :: ierr         ! return 0 on success, otherwise 1
   real(kind=realtime)          :: julian
   real(kind=realtime),save     :: julian_at_epoch
   logical,save                 :: first=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
if(first) then                                        ! Convert zero of Unix Epoch Time to Julian Date and save
   call date_to_julian([1970,1,1,0,0,0,0,0],julian_at_epoch,ierr)
   if(ierr.ne.0) return                               ! Error
   first=.false.
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_to_julian(dat,julian,ierr)
   if(ierr.ne.0) return                               ! Error
   unixtime=(julian-julian_at_epoch)*secday
end subroutine date_to_unix
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_date_to_unix
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
real(kind=realtime)              :: unixtime
integer                          :: ierr

call unit_check_start('date_to_unix')

call date_to_unix([2017,03,29,-240,01,46,47,0],unixtime,ierr)
call unit_check('d2u',abs(unixtime-1490766407).lt.0.001 ,msg=msg(d2u([2017,03,29,-240,01,46,47,0]) ))

call unit_check_done('date_to_unix')

end subroutine test_date_to_unix
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unix_to_date(3f) - [M_time] converts Unix Epoch Time to DAT date-time array
!!
!!##SYNOPSIS
!!
!!    subroutine unix_to_date(unixtime,dat,ierr)
!!
!!     real(kind=realtime),intent(in) :: unixtime ! Unix time (seconds)
!!     integer,intent(out)            :: dat(8)   ! date and time array
!!     integer,intent(out)            :: ierr     ! 0 for successful execution
!!
!!##DESCRIPTION
!!     Converts a Unix Epoch Time (UET) to a DAT date-time array.
!!
!!##OPTIONS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC; of type real(kind=realtime).
!!
!!##RETURNS
!!     dat      Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!               dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_unix_to_date
!!     use M_time, only : unix_to_date, u2d, fmtdate, realtime
!!     implicit none
!!     real(kind=realtime)           :: unixtime
!!     real(kind=realtime),parameter :: DAY=86400.0d0 ! seconds in a day
!!     integer                       :: dat(8)
!!     integer                       :: ierr
!!        unixtime=1468939038.4639933d0            ! sample Unix Epoch time
!!        call unix_to_date(unixtime,dat,ierr)     ! create DAT array for today
!!        write(*,*)'Sample Date=',fmtdate(dat)
!!        call unix_to_date(unixtime-DAY,dat,ierr) ! go back one day
!!        write(*,*)'Day Before =',fmtdate(dat)    ! subtract day and print
!!        call unix_to_date(unixtime+DAY,dat,ierr) ! go forward one day
!!        write(*,*)'Day After  =',fmtdate(dat)    ! add day print
!!     end program demo_unix_to_date
!!
!!    results:
!!
!!     Sample Date=Tuesday, July 19th, 2016 10:37:18 AM
!!     Day Before =Monday, July 18th, 2016 10:37:18 AM
!!     Day After  =Wednesday, July 20th, 2016 10:37:18 AM
!===================================================================================================================================
! REF:JRH:1991-05-23
! REF:JSU:2015-12-12
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine unix_to_date(unixtime,dat,ierr)

character(len=*),parameter::ident_4="@(#)M_time::unix_to_date(3f): Converts Unix Time to DAT date-time array"

class(*),intent(in)              :: unixtime                            ! Unix time (seconds)
integer,intent(out)              :: dat(8)                              ! date and time array
integer,intent(out)              :: ierr                                ! 0 for successful execution, otherwise 1
   real(kind=realtime)           :: julian                              ! Unix time converted to a Julian Date
   real(kind=realtime)           :: local_unixtime
   real(kind=realtime),save      :: Unix_Origin_as_Julian               ! start of Unix Time as Julian Date
   logical,save            :: first=.TRUE.
   !  Notice that the value UNIXTIME can be any of several types ( INTEGER,REAL,REAL(KIND=REALTIME))
   select type(unixtime)
   type is (integer);             local_unixtime=dble(unixtime)
   type is (real);                local_unixtime=dble(unixtime)  ! typically not precise enough for UET values.
   type is (real(kind=realtime)); local_unixtime=unixtime
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   if(first)then                                                             ! Initialize calculated constants on first call
      call date_to_julian([1970,1,1,0,0,0,0,0],Unix_Origin_as_Julian,ierr)   ! Compute start of Unix Time as a Julian Date
      if(ierr.ne.0) return                                                   ! Error
      first=.FALSE.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   julian=(local_unixtime/secday)+Unix_Origin_as_Julian           ! convert seconds from Unix Epoch to Julian Date
   call julian_to_date(julian,dat,ierr)                           ! calculate date-time array from Julian Date
   !dat(4)=get_timezone()                                          ! need to get time zone
end subroutine unix_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unix_to_date
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('unix_to_date')
call unit_check_done('unix_to_date')
end subroutine test_unix_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2o(3f) - [M_time] converts DAT date-time array to Ordinal day
!!
!!##SYNOPSIS
!!
!!    function d2o(dat) result (ordinal)
!!
!!     integer,intent(in) :: dat(8)   ! date time array
!!     integer            :: ordinal  ! the returned day of the year
!!
!!##DESCRIPTION
!!     Given a date in the form of a "DAT" array return the Ordinal Day,
!!     (ie. "the day of the year").
!!
!!##OPTIONS
!!     dat  Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!##RETURNS
!!     ordinal  The day of the year calculated for the given input date,
!!              where Jan 1st=1.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2o
!!     use M_time, only : d2o, mo2d
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Day of year is:',d2o(dat)
!!
!!        ! year, month, day, timezone, hour, minute, seconds, milliseconds
!!        dat=[2020,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2021,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2022,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2023,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2024,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!     end program demo_d2o
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:20:1:19:829
!!     Day of year is:         201
!!            2020  Days in year is:         366
!!            2021  Days in year is:         365
!!            2022  Days in year is:         365
!!            2023  Days in year is:         365
!!            2024  Days in year is:         366
!===================================================================================================================================
function d2o(dat) result (ordinal)

character(len=*),parameter::ident_5="@(#)M_time::d2o(3f): Converts DAT date-time array to Ordinal day"

! JSU 2015-12-13
integer,intent(in)         :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
integer                    :: ordinal                 ! the returned number of days
   real(kind=realtime)           :: unixtime                ! Unix time (seconds)
   real(kind=realtime)           :: unix_first_day
   integer                 :: ierr                    ! return 0 on success, otherwise 1 from date_to_unix(3f)
   call date_to_unix(dat,unixtime,ierr)               ! convert date to Unix Epoch Time
   if(ierr.ne.0)then
      call stderr('*d2o* bad date array')
      ordinal=-1                                      ! initialize to bad value
   else
      call date_to_unix([dat(1),1,1,dat(4),0,0,0,0],unix_first_day,ierr)
      ordinal=int((unixtime-unix_first_day)/secday)+1
   endif
end function d2o
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2o()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : d2o
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

   call unit_check_start('d2o')

    do i=2,size(tests)
       read(tests(i),*)iday,iyear,omonth,oday
       dat=o2d(iday,iyear)
       rday=d2o(dat)
       call unit_check('d2o',iday.eq.rday,msg=tests(i))
    enddo

   call unit_check_done('d2o')

end subroutine test_d2o
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     ordinal_to_date(3f) - [M_time] when given a valid year and day of the year returns the DAT array for the date
!!##SYNOPSIS
!!
!!      subroutine ordinal_to_date(yyyy, ddd, dat)
!!
!!       integer, intent(in)   :: yyyy
!!       integer, intent(in)   :: ddd
!!       integer, intent(out)  :: dat
!!##DESCRIPTION
!!     When given a valid year, YYYY, and day of the year, DDD, returns the date as a DAT date array
!!##OPTIONS
!!       yyyy  known year
!!       ddd   known ordinal day of the year
!!##RETURNS
!!       dat   DAT array describing the date
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_datesub
!!    use m_time, only : ordinal_to_date
!!    implicit none
!!    INTEGER            :: yyyy, ddd, mm, dd
!!    integer            :: dat(8)
!!    integer            :: ios
!!
!!      INFINITE: do
!!         write(*,'(a)',advance='no')'Enter year YYYY and ordinal day of year DD '
!!         read(*,*,iostat=ios)yyyy,ddd
!!         if(ios.ne.0)exit INFINITE
!!         ! recover month and day from year and day number.
!!         call ordinal_to_date(yyyy, ddd, dat)
!!         mm=dat(2)
!!         dd=dat(3)
!!       enddo INFINITE
!!
!!    end program demo_datesub
!!
!!##REFERENCE
!!         See ACM algorithm 398, Tableless Date Conversion, by
!!         Dick Stone, CACM 13(10):621.
!===================================================================================================================================
subroutine ordinal_to_date(yyyy, ddd, dat)
implicit none

character(len=*),parameter::ident_6="&
&@(#)M_time::ordinal_to_date(3f): when given a valid year and day of the year returns the DAT array for the date"

integer, intent(in)   :: yyyy
integer, intent(in)   :: ddd
integer,intent(out)   :: dat(8)
   integer               :: mm
   integer               :: dd
   integer :: t

   t = 0

   IF(MOD(yyyy, 4) == 0)then
      t = 1
   endif

   IF(MOD(yyyy, 400) /= 0 .AND. MOD(yyyy, 100) == 0) then   ! this test is necessary if yyyy is < 1900 or > 2100.
      t = 0
   endif

   dd = ddd

   if(ddd > 59+t) then
      dd = dd + 2 - t
   endif

   mm = ((dd+91)*100)/3055
   dd = (dd+91) - (mm*3055)/100
   mm = mm - 2

   if(mm >= 1 .and. mm <= 12) then                          ! mm will be correct if ddd is correct for yyyy.
      call date_and_time(values=dat)                        ! get current time zone
      dat(1)=yyyy
      dat(2)=mm
      dat(3)=dd
      dat(5)=12
      dat(6)=0
      dat(7)=0
      dat(8)=0
   else
      WRITE(*,"('*error* ordinal_to_date: DAY OF THE YEAR INPUT =', i11,  ' IS OUT OF RANGE.')") ddd
      stop
   endif

end subroutine ordinal_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ordinal_to_date()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : o2d, ordinal_to_date, d2o
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

   call unit_check_start('ordinal_to_date')

   do i=2,size(tests)
      read(tests(i),*)iday,iyear,omonth,oday
      call ordinal_to_date(iyear,iday,dat)
      call unit_check('ordinal_to_date',dat(2).eq.omonth.and.dat(3).eq.oday,msg=msg('year',iyear,'ordinal',iday))
   enddo

   call unit_check_done('ordinal_to_date')

end subroutine test_ordinal_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    o2d(3f) - [M_time] converts Ordinal day to DAT date-time array
!!
!!##SYNOPSIS
!!
!!    function o2d(ordinal,[year]) result (dat)
!!
!!     integer,intent(in) :: ordinal  ! the day of the year
!!     integer,optional   :: year     ! year
!!     integer            :: dat(8)   ! date time array
!!
!!##DESCRIPTION
!!     Given an Ordinal day of the year return a date in the form of a
!!     "DAT" array.
!!
!!##OPTIONS
!!     ordinal  The day of the year for the given year, where Jan 1st=1.
!!
!!     year     An optional year for the ordinal day. If not present the
!!              current year is assumed.
!!
!!##RETURNS
!!     dat   Integer array holding a "DAT" array, similar in structure
!!           to the array returned by the intrinsic DATE_AND_TIME(3f).
!!           The timezone value is from the current time on the current platform.
!!
!!            dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_o2d
!!     use M_time, only : o2d,fmtdate
!!     implicit none
!!     integer :: year
!!        do year=2004,2008
!!           write(*,*)'100th day of ',year,' is ',fmtdate(o2d(100,year))
!!        enddo
!!        write(*,*)'100th day of this year is ',fmtdate(o2d(100))
!!     end program demo_o2d
!!
!!    results:
!!
!!     100th day of 2004 is Friday, April 9th, 2004 00:00:00 PM UTC-02:40
!!     100th day of 2005 is Sunday, April 10th, 2005 00:00:00 PM UTC-02:40
!!     100th day of 2006 is Monday, April 10th, 2006 00:00:00 PM UTC-02:40
!!     100th day of 2007 is Tuesday, April 10th, 2007 00:00:00 PM UTC-02:40
!!     100th day of 2008 is Wednesday, April 9th, 2008 00:00:00 PM UTC-02:40
!!     100th day of this year is Saturday, April 9th, 2016 00:00:00 PM UTC-02:40
!===================================================================================================================================
function o2d(ordinal,year) result (dat)

character(len=*),parameter::ident_7="@(#)M_time::o2d(3f): Converts ordinal day to DAT date-time array"

integer                    :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
integer,intent(in)         :: ordinal                 ! the returned number of days
integer,optional           :: year
   real(kind=realtime)     :: unixtime                ! Unix time (seconds)
   integer                 :: ierr                    ! return 0 on success, otherwise 1 from date_to_unix(3f)
   if(present(year))then
      dat=[year,1,ordinal,get_timezone(),0,0,0,0]     ! initialize DAT with parameters and set timezone, set HH:MM:SS.XX to zero
   else
      call date_and_time(values=dat)                  ! set year and timezone to current values
      dat=[dat(1),1,ordinal,dat(4),0,0,0,0]           ! apply ordinal parameter to January of current year, set HH:MM:SS.XX to zero
   endif
   ierr=0
   call date_to_unix(dat,unixtime,ierr)               ! convert date to Unix Epoch Time
   if(ierr.ne.0)then
      call stderr('*o2d* bad date array')
   else
      dat=u2d(unixtime)
   endif
end function o2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_o2d()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : o2d, ordinal_to_date, d2o
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

  call unit_check_start('o2d')

   do i=2,size(tests)
      read(tests(i),*)iday,iyear,omonth,oday
      dat=o2d(iday,iyear)
      call unit_check('o2d',dat(1).eq.iyear.and.dat(2).eq.omonth.and.dat(3).eq.oday,msg=tests(i))
   enddo

   call unit_check_done('o2d')

end subroutine test_o2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    v2mo(3f) - [M_time] returns the month name of a Common month number
!!
!!##SYNOPSIS
!!
!!    function v2mo(imonth) result(month_name)
!!
!!     integer,intent(in)           :: imonth      ! month number (1-12)
!!     character(len=:),allocatable :: month_name  ! month name
!!
!!##DESCRIPTION
!!    Given a Common Calendar month number, return the name of the month
!!    as a string.
!!
!!##OPTIONS
!!    imonth      Common month number (1-12). If out of the allowable range
!!                the month name returned will be 'UNKNOWN'.
!!##RETURNS
!!    month_name  A string representing a month name or the word 'UNKNOWN'
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_v2mo
!!     use M_time, only : v2mo
!!     implicit none
!!     integer :: i
!!        do i=1,13
!!           write(*,*)v2mo(i)
!!        enddo
!!     end program demo_v2mo
!!
!!    results:
!!
!!     January
!!     February
!!     March
!!     April
!!     May
!!     June
!!     July
!!     August
!!     September
!!     October
!!     November
!!     December
!!     UNKNOWN.
!===================================================================================================================================
function v2mo(imonth) result(month_name)

character(len=*),parameter::ident_8="@(#)M_time::v2mo(3f): returns the month name of a Common month number"

! JSU 2015-12-13
character(len=:),allocatable :: month_name                                        ! string containing month name or abbreviation.
integer,intent(in)           :: imonth                                            ! the number of the month(1-12)
   character(len=*),parameter   :: names(12)=[                                    &
   &'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', &
   &'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ']

   select case(imonth)
   case (1:12);        month_name=trim(names(imonth))
   case default;       month_name='UNKNOWN'
   end select

end function v2mo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2mo
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('v2mo')
call unit_check_done('v2mo')
end subroutine test_v2mo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    mo2d(3f) - [M_time] given month name return DAT date-time array for beginning of that month in current year
!!
!!##SYNOPSIS
!!
!!       function mo2d(month_name) result (dat)
!!
!!        character(len=*),intent(in) :: month_name
!!        integer                     :: dat(8)
!!
!!##DESCRIPTION
!!    Given a Common Calendar month name, return the date as a "DAT" array
!!    in the current year, for the 1st day of the month.
!!
!!##OPTIONS
!!    month_name  A string representing a Common Calendar month name.
!!##RETURNS
!!    dat         An integer array that has the same structure as the array
!!                returned by the Fortran intrinsic DATE_AND_TIME(3f).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_mo2d
!!     use M_time, only : mo2d
!!     implicit none
!!        write(*,'(*(i0:,":"))')mo2d('March')
!!     end program demo_mo2d
!!
!!    results:
!!
!!       2016:3:1:-240:0:0:0:0
!===================================================================================================================================
function mo2d(month_name) result (dat)

character(len=*),parameter::ident_9="@(#)M_time::mo2d(3f): month name to DAT date-time array for 1st of that month in current year"

integer                     :: dat(8)
character(len=*),intent(in) :: month_name
   call date_and_time(values=dat)
   dat(2)=mo2v(month_name) ! convert given month name to a number
   if(dat(2).le.0)then
      call stderr('*mo2d* bad month name '//trim(month_name))
      dat(2)=1
   endif
   dat(3)=1 ! set day to first of month
   dat(5)=0 ! set hour to zero
   dat(6)=0 ! set minutes to zero
   dat(7)=0 ! set seconds to zero
   dat(8)=0 ! set milliseconds to zero
end function mo2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mo2d
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('mo2d')
call unit_check_done('mo2d')
end subroutine test_mo2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    mo2v(3f) - [M_time] given month name return month number (1-12) of that month
!!
!!##SYNOPSIS
!!
!!    function mo2v(month_name) result(imonth)
!!
!!      character(len=*),intent(in):: month_name ! month name
!!      integer                    :: imonth     ! month number
!!
!!##DESCRIPTION
!!       Given a string representing the name or abbreviation of a Gregorian
!!       Calendar month return a number representing the position of the
!!       month in the calendar starting with 1 for January and ending with
!!       12 for December.
!!
!!##OPTIONS
!!    month_name  name or abbreviation of month. Case is ignored
!!                Once enough characters are found to uniquely identify a
!!                month the rest of the name is ignored.
!!##RETURNS
!!    imonth      month number returned. If the name is not recognized a -1
!!                is returned.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_mo2v
!!     use M_time, only : mo2v
!!     implicit none
!!        write(*,*)mo2v("April")
!!        write(*,*)mo2v('Apr')
!!        ! NOTE: still matches September, as "SE" was enough
!!        write(*,*)mo2v('sexember')
!!        write(*,*)mo2v('unknown')  ! returns -1
!!     end program demo_mo2v
!!
!!    results:
!!
!!       >  4
!!       >  4
!!       >  9
!!       > -1
!===================================================================================================================================
function mo2v(month_name) result(imonth)

character(len=*),parameter::ident_10="@(#)M_time::mo2v(3f): given month name return month number (1-12) of that month"

! JSU 2015-12-13
character(len=*),intent(in):: month_name   ! string containing month name or abbreviation.
integer                    :: imonth       ! the number of the month(1-12), or -1 if the name could not be recognized.
  character(len=3)         :: string
  string = upper(month_name)     ! Case is ignored; test string now guaranteed to have three characters
  imonth = 0
  FIND: select case(string(1:1)) ! The month name has to match up to the unique beginning of a month name, and the rest is ignored.
  case('F'); imonth=2      ! February
  case('S'); imonth=9      ! September
  case('O'); imonth=10     ! October
  case('N'); imonth=11     ! November
  case('D'); imonth=12     ! December
  case default
     select case(string(1:2))
     case('JA'); imonth=1    ! JAnuary
     case('AP'); imonth=4    ! APril
     case('AU'); imonth=8    ! AUgust
     case default
        select case(string(1:3))
        case('MAR'); imonth=3 ! MARch
        case('MAY'); imonth=5 ! MAY
        case('JUN'); imonth=6 ! JUNe
        case('JUL'); imonth=7 ! JULy
        case default
           imonth=-1
        end select
     end select
  end select FIND
end function mo2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mo2v()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time,     only: mo2v

call unit_check_start('mo2v')

call unit_check('mo2v', mo2v('jan')       .eq.  1   ,msg='Check January')
call unit_check('mo2v', mo2v('Feb')       .eq.  2   ,msg='Check Febuary')
call unit_check('mo2v', mo2v('March')     .eq.  3   ,msg='Check March')
call unit_check('mo2v', mo2v('APR')       .eq.  4   ,msg='Check April')
call unit_check('mo2v', mo2v('may')       .eq.  5   ,msg='Check May')
call unit_check('mo2v', mo2v('jun')       .eq.  6   ,msg='Check Jun')
call unit_check('mo2v', mo2v('july')      .eq.  7   ,msg='Check July')
call unit_check('mo2v', mo2v('Aug')       .eq.  8   ,msg='Check August')
call unit_check('mo2v', mo2v('Sept')      .eq.  9   ,msg='Check September')
call unit_check('mo2v', mo2v('Oct')       .eq.  10  ,msg='Check October')
call unit_check('mo2v', mo2v('Nov')       .eq.  11  ,msg='Check November')
call unit_check('mo2v', mo2v('December')  .eq.  12  ,msg='Check December')
call unit_check('mo2v', mo2v('jax')       .eq.  1   ,msg='Check "jax"')
call unit_check('mo2v', mo2v('ja')        .eq.  1   ,msg='Check "ja"')
call unit_check('mo2v', mo2v('j')         .eq. -1   ,msg='Check "j"')
call unit_check('mo2v', mo2v('')          .eq. -1   ,msg='Check ""')

call unit_check_done('mo2v') ! assume if got here passed checks

end subroutine test_mo2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    now(3f) - [M_time] return string representing current time given format
!!
!!##SYNOPSIS
!!
!!    function now(format) RESULT (timestr)
!!
!!     character(len=*),intent(in)     :: format  ! input format string
!!     character(len=:),allocatable    :: timestr ! formatted date
!!
!!##DESCRIPTION
!!     The now(3f) function is a call to the fmtdate(3f) function using the
!!     current date and time. That is, it is a convenient way to print the
!!     current date and time.
!!
!!##OPTIONS
!!     format      string describing how to format the current date and time.
!!                 For a complete description of the formatting macros
!!                 supported see fmtdate_usage(3f).
!!##RETURNS
!!     timestr     formatted output string representing date
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_now
!!     use M_time, only : now
!!     implicit none
!!        write(*,*)now("The current date is %w, %l %d, %Y %H:%m:%s %N")
!!        call showme()
!!     contains
!!     subroutine showme() ! see all formatting options
!!     use M_time, only : fmtdate_usage
!!        call fmtdate_usage() ! see all formatting options
!!     end subroutine showme
!!
!!     end program demo_now
!!
!!    results:
!!
!!       The current date is Sun, Jul 17th, 2016 01:21:35 PM
!!        ::
!!        :: description of all formatting options will appear here
!!        ::
!===================================================================================================================================
function now(format)

character(len=*),parameter::ident_11="@(#)M_time::now(3f): return string representing current time given format"

! JSU 2015-10-24
character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now
   integer                           :: values(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_and_time(values=values)
   if(present(format))then
      now=fmtdate(values,format)
   else
      now=trim(fmtdate(values))
   endif
end function now
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_now
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('now')
call unit_check_done('now')
end subroutine test_now
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmtdate(3f) - [M_time] given DAT date-time array return date as string using specified format
!!
!!##SYNOPSIS
!!
!!    function fmtdate(values,format) RESULT (timestr)
!!
!!     integer,dimension(8),intent(in)      :: values
!!     character(len=*),intent(in),optional :: format
!!     character(len=:),allocatable         :: timestr
!!
!!##DESCRIPTION
!!     The fmtdate(3f) procedure lets you reformat a DAT array in
!!     many common formats using a special string containing macro names
!!     beginning with '%'. To see the allowable macros call or see the
!!     fmtdate_usage(3f) routine.
!!
!!##OPTIONS
!!     values   date in a "DAT" array, which is the same format as
!!              the values returned by the intrinsic DATE_AND_TIME(3f).
!!
!!              dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!     format   string describing how to format the "DAT" array.
!!              For a complete description of the formatting macros
!!              supported see fmtdate_usage(3f).
!!##RETURNS
!!     timestr  formatted output string representing date
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_fmtdate
!!     use M_time, only : fmtdate
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,*)fmtdate(dat,"current date: %w, %l %d, %Y %H:%m:%s %N")
!!        call showme()
!!     contains
!!     subroutine showme()
!!        use M_time, only : fmtdate_usage
!!        call fmtdate_usage() ! see all formatting options
!!     end subroutine showme
!!     end program demo_fmtdate
!!
!!    results:
!!
!!       The current date is Sun, Jul 17th, 2016 01:21:35 PM
!!        ::
!!        :: An up-to-date description of all the
!!        :: formatting options will appear here
!!        ::
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!===================================================================================================================================
function fmtdate(values,format) RESULT (timestr)

character(len=*),parameter::ident_12="@(#)M_time::fmtdate(3f): given DAT date-time array return date as string using format"

! JSU 2015-10-24
integer,dimension(8),intent(in)      :: values    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
character(len=*),intent(in),optional :: format    ! input format string
character(len=:),allocatable         :: timestr
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   integer,dimension(8)              :: valloc    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
   integer,parameter                 :: longest=4096
   character(len=1)                  :: chara     ! character being looked at in format string
   character(len=10)                 :: iso_name
   character(len=2)                  :: dayend
   character(len=9)                  :: day       ! day of week
   character(len=:),allocatable      :: local_format
   character(len=longest)            :: text      ! character array
   character(len=longest)            :: xxxx
   integer                           :: i,ii,i10
   integer                           :: ierr
   integer                           :: iout
   integer                           :: iso_year, iso_week, iso_weekday
   integer                           :: systemclock, countrate
   integer                           :: weekday
   integer,save                      :: called=0
   logical                           :: keyword   ! flag that previous character was a % character
   logical,save                      :: since=.FALSE.
   real(kind=realtime)               :: cputime
   real(kind=realtime)               :: julian
   real(kind=realtime)               :: unixtime
   real(kind=realtime),save          :: unixtime_last

   valloc=values
   if(present(format))then
      local_format=format
   else
      local_format=' '
   endif

   select case(local_format)
   case('iso-8601W','isoweek') ; local_format='%I'                    ! 2016-W24-5 (yyyy-Www-d)
   case('iso-8601','iso')      ; local_format='%Y-%M-%DT%h:%m:%s%z'   ! 2006-08-14T02:34:56-0600
   case('sql')       ; local_format='"%Y-%M-%D %h:%m:%s.%x"'          !
   case('sqlday')    ; local_format='"%Y-%M-%D"'                      !
   case('sqltime')   ; local_format='"%h:%m:%s.%x"'                   !
   case('rfc-2822')  ; local_format='%w, %D %l %Y %h:%m:%s %T'        ! Mon, 14 Aug 2006 02:34:56 -0600
   case('rfc-3339')  ; local_format='%Y-%M-%DT%h:%m:%s%z'             ! 2006-08-14 02:34:56-06:00
   case('suffix')    ; local_format='%Y%D%M%h%m%s'                    ! 20170122210327
   case('date')      ; local_format='%w %l %D %h:%m:%s UTC%z %Y'      ! Mon Jul 25 03:19:21 UTC-4:00 2016
   case('short')     ; local_format='%w, %l %d, %Y %H:%m:%s %N UTC%z' ! Fri, Jun 17th, 2016 06:31:00 PM UTC-04:00
   case('long')      ; local_format='%W, %L %d, %Y %H:%m:%s %N UTC%z' ! Friday, June 17th, 2016 06:31:00 PM UTC-04:00
   case(' ')         ; local_format='%W, %L %d, %Y %H:%m:%s %N UTC%z' ! Friday, June 17th, 2016 06:31:00 PM UTC-04:00
   case('formal')    ; local_format='The %d of %L %Y'                 ! The 9th of November 2014
   case('lord')  ; local_format='the %d day of %L in the year of our Lord %Y' ! the 9th day of November in the year of our Lord 2014
   case('easter')
      call easter(values(1), valloc(2), valloc(3))                    ! given year get month and day Easter falls on
      ! fill out a date_and_time array with information for Easter so can print the date using fmtdate(3f)
      valloc(5:8)=[12,0,0,0]                                          ! year,month,day,tz, hour,minute,second,millisecond
      local_format="Easter day: the %d day of %L in the year of our Lord %Y"
   case('all')
     local_format='&
     & Civil Date:%t%t%Y-%M-%D %h:%m:%s %z%n&
     & Julian Date:%t%t%J%n&
     & Unix Epoch Time:%t%E%n&
     & Civil Calendar:%t%W %L %d%n&
     & ISO-8601 week:%t%t%I%n&
     & Day Of Year:%t%t%O'
   case default
      xxxx=local_format
      if(index(xxxx,'%').eq.0)then               ! if no % characters try to guess what macros are present
         call substitute(xxxx,'year','%Y')
         call substitute(xxxx,'month','%M')

         call substitute(xxxx,'weekday','%u')
         call substitute(xxxx,'today','%Y%M%D')
         call substitute(xxxx,'day','%D')

         call substitute(xxxx,'hour','%h')
         call substitute(xxxx,'minute','%m')
         call substitute(xxxx,'timezone','%z')

         call substitute(xxxx,'millisecond','%x')
         call substitute(xxxx,'second','%s')

         call substitute(xxxx,'epoch','%e')
         call substitute(xxxx,'julian','%j')
         call substitute(xxxx,'ordinal','%O')

         if(index(xxxx,'%').eq.0)then            ! if no % characters change every char to %char if a format macro letter
            do i=65,122
             select case(char(i))
             case('B':'E','H':'J','L':'Q','S','T','U','W','Y','Z','b':'e','h':'m','n','o':'q','s':'u','w','x','z')
                 call substitute(xxxx,char(i),'%'//char(i))
             end select
            enddo
         endif
      endif
      local_format=trim(xxxx)
   end select

   text=' '
!  write string, when encounter a percent character do a substitution
   keyword=.FALSE.
   iout=1

   do i10=1,len(local_format)                  ! Read the FORMAT string and replace the "%" strings per the following rules:
      chara=local_format(i10:i10)
      if(chara.eq.'%'.and..not.keyword)then
            keyword=.TRUE.
            cycle
      endif
      if(keyword)then
         keyword=.FALSE.
         select case(chara)
         !=====================================================================================
         case('%'); write(text(iout:),'(A1)')chara                        ! literal percent character
         !=====================================================================================
         case('b'); write(text(iout:),'(A1)')' '                          ! space character
         !=====================================================================================
         case('B'); write(text(iout:),'(A1)')'!'                          ! exclamation (bang) character
         !=====================================================================================
         case('c'); call cpu_time(cputime)                                ! CPU_TIME()
                    write(text(iout:),'(G0)')cputime
         !=====================================================================================
         case('C'); called = called + 1                                   ! number of times this routine called
                    write(text(iout:),'(I0)')called
         !=====================================================================================
         case('d');                                                       ! the day of the month 1st..31st
                    dayend='  '
                    select case(valloc(3))
                    case(1,21,31); dayend='st'
                    case(2,22); dayend='nd'
                    case(3,23); dayend='rd'
                    case(4:20,24:30); dayend='th'
                    case default
                    end select
                    write(text(iout:),'(I0,a)')valloc(3),dayend
         !=====================================================================================
         case('D'); write(text(iout:),'(I2.2)')valloc(3)                  ! the day of the month 1..31
         !=====================================================================================
         case('e'); call date_to_unix(valloc,unixtime,ierr)               ! integer Unix Epoch time in seconds
                    write(text(iout:),'(G0)')nint(unixtime)
         !=====================================================================================
         case('E'); call date_to_unix(valloc,unixtime,ierr)               ! Unix Epoch time in seconds
                    write(text(iout:),'(G0)')unixtime
         !=====================================================================================
         case('h'); write(text(iout:),'(I2.2)')valloc(5)                  ! the hour of the day, in the range of 0 to 23
         !=====================================================================================
         case('H'); ii=mod(valloc(5),12)                                  ! hour of day in range 1..12
                    if(ii.eq.0)then
                       ii=12
                    endif
                    write(text(iout:),'(I0)')ii
         !=====================================================================================
         case('i'); call d2w(valloc,iso_year,iso_week,iso_weekday,iso_name) ! ISO week of year
                    write(text(iout:),'(I0)')iso_week
         !=====================================================================================
         case('I'); call d2w(valloc,iso_year,iso_week,iso_weekday,iso_name) ! iso-8601 Week-numbering year date
                    write(text(iout:),'(a)')iso_name
         !=====================================================================================
         case('j'); call date_to_julian(valloc,julian,ierr)               ! integer Julian Day (truncated to integer)
                    write(text(iout:),'(I0)')int(julian)
         !=====================================================================================
         case('J'); call date_to_julian(valloc,julian,ierr)               ! Julian Date out to milliseconds
                    !write(text(iout:),'(I0,".",i3.3)')int(julian),nint((julian-int(julian))*1000.0)
                    write(text(iout:),'(g0)')julian
         !=====================================================================================
         case('k'); call system_clock(count=systemclock,count_rate=countrate)  ! systemclock/countrate
                    write(text(iout:),'(G0)')real(systemclock)/countrate
         !=====================================================================================
         case('K'); call system_clock(count=systemclock,count_rate=countrate)  ! system clock count
                    write(text(iout:),'(I0)') systemclock
         !=====================================================================================
         case('l'); write(text(iout:),'(A3)')v2mo(valloc(2))              ! three characters of the name of the month of the year
         !=====================================================================================
         case('L'); write(text(iout:),'(A)')v2mo(valloc(2))               ! name of the month of the year
         !=====================================================================================
         case('m'); write(text(iout:),'(I2.2)')valloc(6)                  ! the minutes of the hour, in the range 0 to 59
         !=====================================================================================
         case('M'); write(text(iout:),'(I2.2)')valloc(2)                  ! month of year (1..12)
         !=====================================================================================
         case('N'); if( valloc(5).ge.12)then                              ! AM||PM
                       write(text(iout:),'("PM")')
                    else
                       write(text(iout:),'("AM")')
                    endif
         !=====================================================================================
         case('n');
                    write(text(iout:),'(a)')new_line("A")
         !=====================================================================================
         case('O'); write(text(iout:),'(I3.3)')d2o(valloc)                ! Ordinal day of year
         !=====================================================================================
         case('o'); call date_to_unix(valloc,unixtime,ierr)               ! integer Unix Epoch time in seconds
                    write(text(iout:),'(G0)')floor(unixtime/86400)        ! number of whole days since Epoch time
         !=====================================================================================
         !=====================================================================================
         case('p'); write(text(iout:),'(A)')phase_of_moon(valloc)         ! phase of moon
         !=====================================================================================
         case('P'); write(text(iout:),'(i0,"%")')moon_fullness(valloc)    ! percent of fullness
         !=====================================================================================
         case('q'); write(text(iout:),'("''")')                           ! single quote (apostrophe)
         !=====================================================================================
         case('Q'); write(text(iout:),'(''"'')')                          ! double quote
         !=====================================================================================
         case('s'); write(text(iout:),'(I2.2)')valloc(7)                  ! the seconds of the minute, in the range 0 to 59
         !=====================================================================================
         case('S'); if(.not.since)then                                    ! seconds since last called
                       since=.TRUE.
                       call date_to_unix(valloc,unixtime_last,ierr)
                    endif
                    call date_to_unix(valloc,unixtime,ierr)
                    write(text(iout:),'(G0)')unixtime-unixtime_last
                    unixtime_last=unixtime
         !=====================================================================================
         case('t'); write(text(iout:),'(A1)')CHAR(9)                      ! tab character
         !=====================================================================================
         case('T'); write(text(iout:),'(SP,I3.2,SS,I2.2)')int(valloc(4)/60),abs(mod(valloc(4),60)) ! time from UTC as +-hhmm
         !=====================================================================================
         case('U'); call dow(valloc,weekday,day,ierr)
                    write(text(iout:),'(I1)')mod(weekday+7,7)+1           ! Return the day of the week, 1..7 Sunday=1
         !=====================================================================================
         case('u'); call dow(valloc,weekday,day,ierr)                     ! Return the day of the week, 1..7 Monday=1
                    write(text(iout:),'(I1)')weekday
         !=====================================================================================
         case('W'); call dow(valloc,weekday,day,ierr)                     ! Return the name of the day of the week
                    write(text(iout:),'(a)')day
         !=====================================================================================
         case('w'); call dow(valloc,weekday,day,ierr)                     ! Return the first three characters of the day of the week
                    write(text(iout:),'(A3)')day(1:3)
         !=====================================================================================
         case('x'); write(text(iout:),'(I3.3)')valloc(8)                  ! the milliseconds of the second, in the range 0 to 999
         !=====================================================================================
         case('Y'); write(text(iout:),'(I0.4)')valloc(1)                  ! the year, including the century (for example, 1990)
         !=====================================================================================
         case('Z'); write(text(iout:),'(SP,I5.4)')valloc(4)               ! time difference with respect to UTC in minutes
         !=====================================================================================
         case('z'); write(text(iout:),'(SP,I3.2,":",SS,I2.2)')int(valloc(4)/60),abs(mod(valloc(4),60)) ! time from UTC as +-hh:mm
         !=====================================================================================
         case default
            write(text(iout:),'(A1)')chara
         !=====================================================================================
         end select
         !=====================================================================================
         iout=len_trim(text)+1
         if(iout.ge.longest)exit
      else
         write(text(iout:),'(A1)')chara;iout=iout+1
      endif
   enddo
   timestr=trim(text)
end function fmtdate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmtdate
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only: guessdate, fmtdate
implicit none
character(len=80)  :: date1
character(len=80)  :: date2
character(len=80)  :: iso_week_date
character(len=132) :: comment
character(len=372),allocatable :: line(:)
integer            :: dat(8)
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

call unit_check_start('fmtdate')

do i=1,size(line)-1
   read(line(i),*)date1,date2,iso_week_date,comment
   if(unit_check_level.gt.0)then
      call unit_check_msg('fmtdate','GIVEN:'//trim(date1)//' '//trim(comment))
   endif

   call guessdate(date1,dat)                                         ! convert date string to DAT
   call unit_check('fmtdate',fmtdate(dat,'year-month-day').eq.trim(date2),msg=msg(fmtdate(dat,'year-month-day')))

   ! convert DAT to ISO week date, all generated dates should match ISO week date
   call unit_check('fmtdate',fmtdate(dat,"%I").eq.iso_week_date, msg=iso_week_date)
enddo

call unit_check_done('fmtdate')

end subroutine test_fmtdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmtdate_usage(3f) - [M_time] display macros recognized by fmtdate(3f) and now(3f)
!!
!!##SYNOPSIS
!!
!!    subroutine fmtdate_usage(indent)
!!
!!     integer,intent(in),optional      :: indent
!!
!!##DESCRIPTION
!!
!!     The fmtdate_usage(3f) subroutine displays the formatting options
!!     available for use in procedures such as fmtdate(3f) and now(3f).
!!     It is typically used to produce up-to-date help text in commands
!!     that use the M_time(3fm) module, so that the formatting information
!!     only needs maintained in one place (this routine) and is easily
!!     displayed so users can quickly obtain a description of the formatting
!!     macros.
!!
!!##OPTIONS
!!     indent      how many spaces to prefix the output with, so that
!!                 calling programs can position the output. Default
!!                 for this optional parameter is three (3).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_fmtdate_usage
!!     use M_time, only : fmtdate_usage
!!     implicit none
!!        call fmtdate_usage() ! see all formatting options
!!     end program demo_fmtdate_usage
!!
!!    results (actually call the routine to ensure this is up to date):
!!
!!     Description                                        Example
!!
!!     Base time array:
!!     (1) %Y -- year, yyyy                                2016
!!     (2) %M -- month of year, 01 to 12                   07
!!     (3) %D -- day of month, 01 to 31                    29
!!         %d -- day of month, with suffix (1st, 2nd,...)  29th
!!     (4) %Z -- minutes from UTC                          -0240
!!         %z -- -+hh:mm from UTC                          -04:00
!!         %T -- -+hhmm  from UTC                          -0400
!!     (5) %h -- hours, 00 to 23                           10
!!         %H -- hour (1 to 12, or twelve-hour clock)      10
!!         %N -- midnight< AM <=noon; noon<= PM <midnight  AM
!!     (6) %m -- minutes, 00 to 59                         54
!!     (7) %s -- sec, 00 to 59                             08
!!     (8) %x -- milliseconds 000 to 999                   521
!!     Conversions:
!!         %E -- Unix Epoch time                           1469804048.5220029
!!         %e -- integer value of Unix Epoch time          1469804049
!!         %J -- Julian  date                              2457599.121
!!         %j -- integer value of Julian Date(Julian Day)  2457599
!!         %O -- Ordinal day (day of year)                 211
!!         %o -- Whole days since Unix Epoch date          17011
!!         %U -- day of week, 1..7 Sunday=1                6
!!         %u -- day of week, 1..7 Monday=1                5
!!         %i -- ISO week of year 1..53                    30
!!         %I -- iso-8601 week-numbering date(yyyy-Www-d)  2016-W30-5
!!      Names:
!!         %l -- abbreviated month name                    Jul
!!         %L -- full month name                           July
!!         %w -- first three characters of weekday         Fri
!!         %W -- weekday name                              Friday
!!         %p -- phase of moon                             New
!!         %P -- percent of way from new to full moon      -1%
!!      Literals:
!!         %% -- a literal %                               %
!!         %t -- tab character
!!         %b -- blank character
!!         %B -- exclamation(bang) character
!!         %n -- new line (system dependent)
!!         %q -- single quote (apostrophe)
!!         %Q -- double quote
!!      Program timing:
!!         %c -- CPU_TIME(3f) output                       .21875000000000000
!!         %C -- number of times this routine is used      1
!!         %S -- seconds since last use of this format     .0000000000000000
!!         %k -- time in seconds from SYSTEM_CLOCK(3f)     723258.812
!!         %K -- time in clicks from SYSTEM_CLOCK(3f)      723258812
!!
!!    If no percent (%) is found in the format one of several
!!    alternate substitutions occurs.
!!
!!    If the format is composed entirely of one of the following
!!    keywords the following substitutions occur:
!!
!!      "iso-8601",
!!      "iso"        ==> %Y-%M-%DT%h:%m:%s%z
!!      "iso-8601W",
!!      "isoweek"    ==> %I 2016-W30-5
!!      "sql"        ==> "%Y-%M-%D %h:%m:%s.%x"
!!      "sqlday"     ==> "%Y-%M-%D"
!!      "sqltime"    ==> "%h:%m:%s.%x"
!!      "rfc-2822"   ==> %w, %D %l %Y %h:%m:%s %T
!!      "rfc-3339"   ==> %Y-%M-%DT%h:%m:%s%z
!!      "date"       ==> %w %l %D %h:%m:%s UTC%z %Y
!!      "short"      ==> %w, %l %d, %Y %H:%m:%s %N UTC%z
!!      "long"," "   ==> %W, %L %d, %Y %H:%m:%s %N UTC%z
!!      "suffix"     ==> %Y%D%M%h%m%s
!!      "formal"     ==> The %d of %L %Y
!!      "lord"       ==> the %d day of %L in the year of our Lord %Y
!!      "easter"     ==> FOR THE YEAR OF THE CURRENT DATE:
!!                       Easter day: the %d day of %L in the year of our Lord %Y
!!      "all"        ==> A SAMPLE OF DATE FORMATS
!!
!!    otherwise the following words are replaced with the most
!!    common macros:
!!
!!       year     %Y  2016
!!       month    %M  07
!!       day      %D  29
!!       hour     %h  10
!!       minute   %m  54
!!       second   %s  08
!!       epoch    %e  1469804049
!!       julian   %j  2457599
!!       ordinal  %O  211
!!       weekday  %u  5
!!
!!    if none of these keywords are found then every letter that
!!    is a macro is assumed to have an implied percent in front
!!    of it. For example:
!!
!!       YMDhms ==> %Y%M%D%h%m%s ==> 20160729105408
!===================================================================================================================================
subroutine fmtdate_usage(indent)

character(len=*),parameter::ident_13="@(#)M_time::fmtdate_usage(3f): display macros recognized by fmtdate(3f)"

! JSU 2015-10-24
integer,intent(in),optional       :: indent
   character(len=128),allocatable :: usage(:)
   integer                        :: i,ii
   character(len=:),allocatable   :: blanks
   if(present(indent))then ! set indent to passed value or, if value is not present, set indent to 3
      ii=indent
   else
      ii=3
   endif
   blanks=repeat(' ',ii)   ! define a prefix string to create specified indent
usage=[ CHARACTER(LEN=128) :: &
! 123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890 123456789 123456789 123456789 123456789 12345678
&'Description                                        Example%b ',&
&'%b                                                           ',&
&'%bBase time array:                                           ',&
&' (1) %%Y -- year, yyyy                                %Y     ',&
&' (2) %%M -- month of year, 01 to 12                   %M     ',&
&' (3) %%D -- day of month, 01 to 31                    %D     ',&
&'     %%d -- day of month, with suffix (1st, 2nd,...)  %d     ',&
&' (4) %%Z -- minutes from UTC                          %Z     ',&
&'     %%z -- -+hh:mm from UTC                          %z     ',&
&'     %%T -- -+hhmm  from UTC                          %T     ',&
&' (5) %%h -- hours, 00 to 23                           %h     ',&
&'     %%H -- hour (1 to 12, or twelve-hour clock)      %H     ',&
&'     %%N -- midnight< AM <=noon; noon<= PM <midnight  %N     ',&
&' (6) %%m -- minutes, 00 to 59                         %m     ',&
&' (7) %%s -- sec, 00 to 59                             %s     ',&
&' (8) %%x -- milliseconds 000 to 999                   %x     ',&
&'%bConversions:                                               ',&
&'     %%E -- Unix Epoch time                           %E     ',&
&'     %%e -- integer value of Unix Epoch time          %e     ',&
&'     %%J -- Julian  date                              %J     ',&
&'     %%j -- integer value of Julian Date(Julian Day)  %j     ',&
&'     %%O -- Ordinal day (day of year)                 %O     ',&
&'     %%o -- Whole days since Unix Epoch date          %o     ',&
&'     %%U -- day of week, 1..7 Sunday=1                %U     ',&
&'     %%u -- day of week, 1..7 Monday=1                %u     ',&
&'     %%i -- ISO week of year 1..53                    %i     ',&
&'     %%I -- iso-8601 week-numbering date(yyyy-Www-d)  %I     ',&
&'%b Names:                                                    ',&
&'     %%l -- abbreviated month name                    %l     ',&
&'     %%L -- full month name                           %L     ',&
&'     %%w -- first three characters of weekday         %w     ',&
&'     %%W -- weekday name                              %W     ',&
&'     %%p -- phase of moon                             %p     ',&
&'     %%P -- percent of way from new to full moon      %P     ',&
&'%b Literals:                                                 ',&
&'     %%%% -- a literal %%                               %%   ',&
&'     %%t -- tab character                             %t     ',&
&'     %%b -- blank character                           %b     ',&
&'     %%B -- exclamation(bang) character               %B     ',&
&'     %%n -- new line (system dependent)               %n     ',&
&'     %%q -- single quote (apostrophe)                 %q     ',&
&'     %%Q -- double quote                              %Q     ',&
&'%b Program timing:                                           ',&
&'     %%c -- CPU_TIME(3f) output                       %c     ',&
&'     %%C -- number of times this routine is used      %C     ',&
&'     %%S -- seconds since last use of this format     %S     ',&
&'     %%k -- time in seconds from SYSTEM_CLOCK(3f)     %k     ',&
&'     %%K -- time in clicks from SYSTEM_CLOCK(3f)      %K     ',&
&'%b                                                           ',&
&'%bIf no percent (%%) is found in the format one of several   ',&
&'%balternate substitutions occurs.                            ',&
&'%b                                                           ',&
&'%bIf the format is composed entirely of one of the following ',&
&'%bkeywords the following substitutions occur:                ',&
&'%b  "iso-8601",                                              ',&
&'%b  "iso"        ==> %%Y-%%M-%%DT%%h:%%m:%%s%%z             %Y-%M-%DT%h:%m:%s%z     ',&
&'%b  "iso-8601W",                                                                    ',&
&'%b  "isoweek"    ==> %%I                              %I                            ',&
&'%b  "sql"        ==> "%%Y-%%M-%%D %%h:%%m:%%s.%%x"          "%Y-%M-%D %h:%m:%s.%x"  ',&
&'%b  "sqlday"     ==> "%%Y-%%M-%%D"                      "%Y-%M-%D"                  ',&
&'%b  "sqltime"    ==> "%%h:%%m:%%s.%%x"                   "%h:%m:%s.%x"              ',&
&'%b  "rfc-2822"   ==> %%w, %%D %%l %%Y %%h:%%m:%%s %%T        ',&
&'%b                   %w, %D %l %Y %h:%m:%s %T                ',&
&'%b  "rfc-3339"   ==> %%Y-%%M-%%DT%%h:%%m:%%s%%z             %Y-%M-%DT%h:%m:%s%z     ',&
&'%b  "date"       ==> %%w %%l %%D %%h:%%m:%%s UTC%%z %%Y      ',&
&'%b                   %w %l %D %h:%m:%s UTC%z %Y              ',&
&'%b  "short"      ==> %%w, %%l %%d, %%Y %%H:%%m:%%s %%N UTC%%z',&
&'%b                   %w, %l %d, %Y %H:%m:%s %N UTC%z         ',&
&'%b  "long"," "   ==> %%W, %%L %%d, %%Y %%H:%%m:%%s %%N UTC%%z',&
&'%b                   %W, %L %d, %Y %H:%m:%s %N UTC%z         ',&
&'%b  "suffix"     ==> %%Y%%D%%M%%h%%m%%s                    %Y%D%M%h%m%s             ',&
&'%b  "formal"     ==> The %%d of %%L %%Y                 The %d of %L %Y             ',&
&'%b  "lord"       ==> the %%d day of %%L in the year of our Lord %%Y                 ',&
&'%b                   the %d day of %L in the year of our Lord %Y                    ',&
&'%b  "easter"     ==> FOR THE YEAR OF THE CURRENT DATE:       ',&
&'%b                     Easter day: the %%d day of %%L in the year of our Lord %%Y   ',&
&'%b  "all"        ==> A SAMPLE OF DATE FORMATS                ',&
&'%botherwise the following words are replaced with the most   ',&
&'%bcommon macros:                                             ',&
&'%b   year          %%Y  %Y                                   ',&
&'%b   month         %%M  %M                                   ',&
&'%b   day           %%D  %D                                   ',&
&'%b   timezone      %%z  %z                                   ',&
&'%b   hour          %%h  %h                                   ',&
&'%b   minute        %%m  %m                                   ',&
&'%b   second        %%s  %s                                   ',&
&'%b   millisecond   %%x  %x                                   ',&
&'%b   epoch         %%e  %e                                   ',&
&'%b   julian        %%j  %j                                   ',&
&'%b   ordinal       %%O  %O                                   ',&
&'%b   weekday       %%u  %u                                   ',&
&'%bif none of these keywords are found then every letter that ',&
&'%bis a macro is assumed to have an implied percent in front  ',&
&'%bof it. For example:                                        ',&
&'%b   YMDhms ==> %%Y%%M%%D%%h%%m%%s ==> %Y%M%D%h%m%s          ',&
&'%b                                                           ']
   write(*,'(a,a)')(blanks,(trim(now(usage(i)))),i=1,size(usage))
end subroutine fmtdate_usage
!-----------------------------------------------------------------------------------------------------------------------------------
! C for reference
! %U     week number of year, with Sunday as first day of week (00..53)
! %Z     alphabetic time zone abbreviation (e.g., EDT)
!        By default, date pads numeric fields with zeroes. The following optional flags may follow '%':
!        -      (hyphen) do not pad the field
!        _      (underscore) pad with spaces
!        0      (zero) pad with zeros
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmtdate_usage
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('fmtdate_usage')
call unit_check_done('fmtdate_usage')
end subroutine test_fmtdate_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    guessdate(3f) - [M_time] reads in a date, in various formats
!!
!!##SYNOPSIS
!!
!!    subroutine guessdate(anot,dat)
!!
!!     character(len=*),intent(in) :: anot
!!     integer,intent(out)         :: dat(8)
!!
!!##DESCRIPTION
!!
!!    Read in strings and except for looking for month names remove
!!    non-numeric characters and try to convert a string assumed to represent
!!    a date to a date-time array.
!!
!!    Years should always be expressed as four-digit numbers, and except for
!!    the special format yyyy-mm-dd the day should come after the year. Named
!!    months are preferred. If ambiguous the order is assumed to be day -
!!    month - year. Times are assumed to be of the form HH:MM:SS
!!
!!    It is planned that this routine will be superseded. As an alternative,
!!    a C routine exists in the standard C libraries that allows for
!!    expansive features when reading dates that can be called via the
!!    ISO_C_BINDING interface.
!!
!!##OPTIONS
!!    anot  A string assumed to represent a date including a year, month and day.
!!
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_guessdate
!!     use M_time, only : guessdate, fmtdate
!!     implicit none
!!     character(len=20),allocatable :: datestrings(:)
!!     character(len=:),allocatable  :: answer
!!     integer                       :: dat(8)
!!     integer                       :: i
!!        datestrings=[ &
!!        & 'January 9th, 2001   ',&
!!        & ' Tue Jul 19 2016    ',&
!!        & ' 21/12/2016         ',&
!!        & ' 4th of Jul 2004    ' ]
!!        do i=1,size(datestrings)
!!           write(*,'(a)')repeat('-',80)
!!           write(*,*)'TRYING ',datestrings(i)
!!           call guessdate(datestrings(i),dat)
!!           write(*,*)'DAT ARRAY ',dat
!!           answer=fmtdate(dat)
!!           write(*,*)'FOR '//datestrings(i)//' GOT '//trim(answer)
!!        enddo
!!     end program demo_guessdate
!!
!!    results:
!!
!!     ---------------------------------------------------------------------
!!     TRYING January 9th, 2001
!!     DAT ARRAY         2001  1  9   -240    0   0   0    0
!!     FOR January 9th, 2001    GOT Tuesday, January 9th, 2001 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  Tue Jul 19 2016
!!     DAT ARRAY         2016  7  19  -240    0   0   0    0
!!     FOR  Tue Jul 19 2016     GOT Tuesday, July 19th, 2016 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  21/12/2016
!!     DAT ARRAY         2016  12 21  -240    0   0   0    0
!!     FOR  21/12/2016          GOT Wednesday, December 21st, 2016 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  4th of Jul 2004
!!     DAT ARRAY         2004  7  4   -240    0   0   0    0
!!     FOR  4th of Jul 2004     GOT Sunday, July 4th, 2004 12:00:00 AM
!!
!!
!!##AUTHOR
!!     JRH 1991-03-19
!===================================================================================================================================
subroutine guessdate(datestring,dat,ier)
use M_strings, only: string_to_values, s2v, split, v2s

character(len=*),parameter::ident_14="@(#)M_time::guessdate(3f): Guess format of a date string to create a DAT date-time array"

! based on - JRH 1991-03-19
! modified - JSU, 20160729
!
! makes an odd number of assumptions trying to guess what date format is being used. If you know the format of your date
! values READ(3f) and parse them directly instead of using this procedure, even though it does a good job with common USA formats.
!
!! REDO more rigorously with regular expressions and recognize standard formats directly


! NOTE : Main constraint is that day is input BEFORE year unless use YYYY-MM-DD and a : implies HH:MM:SS, no timezone names
!        Not rigorous. Gets most common formats but can easily make errors in all but simple unambiguous common date formats
character(len=*),intent(in)       :: datestring ! Date in string format
character(len=:),allocatable      :: datestring_local ! Date in string format
character(len=:),allocatable      :: temp
integer,intent(out)               :: dat(8)
integer,optional                  :: ier
   integer                        :: ier_local
   integer                        :: iye,mon,idy  ! Year, Month, Day
   integer                        :: ihr,imi,ise  ! Hour, Minute, Second
   integer                        :: itz, imill   ! Timezone, Milliseconds
   character(len=len(datestring)*2) :: buff
   integer                        :: i,idum,ind
   logical                        :: alpha
   integer                        :: ios
   integer                        :: itries
   character(len=3),parameter     :: amon(12)=['JAN','FEB','MAR','APR','MAY','JUN', 'JUL','AUG','SEP','OCT','NOV','DEC']
   integer,parameter              :: idmon(12)=[31 , 28  , 31  , 30  , 31  , 30 , 31 , 31  , 30  , 31  , 30  , 31]
   character(len=256),allocatable :: scratch(:)
   integer,parameter              :: isize=40
   real                           :: rvalues(isize)
   character(len=2)               :: ampm
   integer                        :: iend, inums, ierr
   logical                        :: number
   logical                        :: verbose
   integer                        :: loops

   call date_and_time(values=dat)                           ! get time zone of current process and set defaults
   iye=dat(1)
   mon=dat(2)
   idy=dat(3)
   itz=dat(4)                                               ! default is to use current timezone
   ihr=0
   imi=0
   ise=0
   imill=0

   ier_local=0
   rvalues=0.0
   datestring_local=''
   verbose=.false.

!-----------------------------------------------------------------------------------------------------------------------------------
   temp=' '//trim(upper(datestring))
   if(len(temp).ge.2)then
      if(temp(2:2).eq.'?')then
         verbose=.true.
         temp=temp(3:)
      endif
   endif
   if(verbose)write(*,*)'GOT HERE a ',temp,'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   number=.false.                                        ! when transition from letter to number add a space
   do i=1,len(temp)
      select case(temp(i:i))
      case('A':'Z','/')
         if(number)then
            datestring_local=datestring_local//' '
         endif
         number=.false.
      case default
         number=.true.
      end select
      datestring_local=datestring_local//temp(i:i)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------

   if(verbose)write(*,*)'GOT HERE b ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
   datestring_local=datestring_local//'                 '  ! pad string so substitute will fit if old string shorter than new string
   !make sure spaces are around month names
   call substitute(datestring_local,'JANUARY',' JAN ')
   call substitute(datestring_local,'FEBRUARY',' FEB ')
   call substitute(datestring_local,'MARCH',' MAR ')
   call substitute(datestring_local,'APRIL',' APR ')
   call substitute(datestring_local,'MAY',' MAY ')
   call substitute(datestring_local,'JUNE',' JUN ')
   call substitute(datestring_local,'JULY',' JUL ')
   call substitute(datestring_local,'AUGUST',' AUG ')
   call substitute(datestring_local,'SEPTEMBER',' SEP ')
   call substitute(datestring_local,'OCTOBER',' OCT ')
   call substitute(datestring_local,'NOVEMBER',' NOV ')
   call substitute(datestring_local,'DECEMBER',' DEC ')
   call substitute(datestring_local,'SEPT',' SEP ')

   call substitute(datestring_local,'JAN',' JAN ')
   call substitute(datestring_local,'FEB',' FEB ')
   call substitute(datestring_local,'MAR',' MAR ')
   call substitute(datestring_local,'APR',' APR ')
   call substitute(datestring_local,'MAY',' MAY ')
   call substitute(datestring_local,'JUN',' JUN ')
   call substitute(datestring_local,'JUL',' JUL ')
   call substitute(datestring_local,'AUG',' AUG ')
   call substitute(datestring_local,'SEP',' SEP ')
   call substitute(datestring_local,'OCT',' OCT ')
   call substitute(datestring_local,'NOV',' NOV ')
   call substitute(datestring_local,'DEC',' DEC ')


   ! assume T[0=9] is from yyyyy-mm-ddThh:mm:ss.xx ISO-8601 format (or SEPTnn,OCTnn AUGUSTnn, where space was added or name changed)
   call substitute(datestring_local,'T0',' 0')
   call substitute(datestring_local,'T1',' 1')
   call substitute(datestring_local,'T2',' 2')
   call substitute(datestring_local,'T3',' 3')
   call substitute(datestring_local,'T4',' 4')
   call substitute(datestring_local,'T5',' 5')
   call substitute(datestring_local,'T6',' 6')
   call substitute(datestring_local,'T7',' 7')
   call substitute(datestring_local,'T8',' 8')
   call substitute(datestring_local,'T9',' 9')

   call substitute(datestring_local,': ',':')
   call substitute(datestring_local,' :',':')

   if(verbose)write(*,*)'GOT HERE A ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   call substitute(datestring_local,'UTC',' ')
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(datestring_local,scratch,' ;,"''')
   if(verbose)write(*,*)'GOT HERE B ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                       ! a leading +/- is assumed to be a timezone
      if( index("+-",scratch(i)(1:1)) .ne. 0)then
         if(index(scratch(i),':').ne.0)then                                   ! assumed to be +-hh:mm
            call string_to_values(scratch(i),isize,rvalues,inums,':',ierr)
            if(inums.ge.2)then
               itz=60*nint(rvalues(1))+nint(rvalues(2))
            elseif(inums.eq.1)then
               itz=60*nint(rvalues(1))
            endif
         else                                                                ! assumed to be +-mm
            itz=nint(s2v(scratch(i)))
         endif
         scratch(i)=' '
      endif
   enddo
   if(verbose)write(*,*)'GOT HERE C ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                      ! AM and PM are assumed to only occur significantly (not end of day or month name, ...)
      if(len_trim(scratch(i)).ge.2)then
         iend=len_trim(scratch(i))
         ampm=scratch(i)(iend-1:iend)
         select case (ampm)
         case('AM')
            call substitute(scratch(i),'AM',':')
         case('PM')
            ihr=ihr+12
            call substitute(scratch(i),'PM',':')
         end select
      endif
   enddo
   if(verbose)write(*,*)'GOT HERE E ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                      ! look for HH:MM:SS
      if(index(scratch(i),':').ne.0)then
         buff=scratch(i)
         call substitute(buff,'-',' -')
         call substitute(buff,'+',' +')
         call string_to_values(buff,isize,rvalues,inums,':/',ierr)
         if(inums.ge.1) ihr=ihr+nint(rvalues(1))
         if(inums.ge.2) imi=nint(rvalues(2))
         if(inums.ge.3) ise=nint(rvalues(3))
         if(inums.ge.4) itz=nint(rvalues(4))
         scratch(i)=' '
      endif
   enddo
   if(verbose)write(*,*)'GOT HERE F ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                       ! assume yyyy-mm-dd if found a dash
      if(index(scratch(i),"-").ne.0)then
            call string_to_values(scratch(i),isize,rvalues,inums,'-',ierr)
            select case(inums)
            case(3)
               iye=nint(rvalues(1))
               mon=nint(rvalues(2))
               idy=nint(rvalues(3))
               scratch(i)=v2s(nint(rvalues(3)))//' '//v2s(nint(rvalues(2)))//' '//v2s(nint(rvalues(1)))
            case(2)
               iye=nint(rvalues(1))
               mon=nint(rvalues(2))
               scratch(i)=v2s(nint(rvalues(2)))//' '//v2s(nint(rvalues(1)))
            case default
            end select
      endif
   enddo
   if(verbose)write(*,*)'GOT HERE D ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   datestring_local=''
   do i=1,size(scratch)
      datestring_local=datestring_local//' '//adjustl(trim(scratch(i)))
   enddo
   if(verbose)write(*,*)'GOT HERE G ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   if(datestring_local.eq.' ')then
     loops=0
   else
     loops=1000
   endif
   if(verbose)write(*,*)'GOT HERE Ga',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill,loops
   INFINITE: do itries=1,loops                              ! give up after 1000 passes
      buff=datestring_local                                 ! copy to buffer
      alpha=.false.

      do i=1,12
         ind=index(buff,amon(i))
         if(ind.ne.0) then                                  ! Found a matching month
            mon=i
            buff(ind:ind+2)='   '                           ! Delete month
            alpha=.true.                                    ! Alphabetic month
            exit
         endif
      enddo

      do i=1,len(buff)                                      ! First remove all non-numeric characters
         idum=ichar(buff(i:i))
         if(idum.lt.48.or.idum.gt.57)then
            buff(i:i)=' '
         endif
      enddo

      if(alpha) then                                        ! Alphabetic month
         read(buff,*,iostat=ios) idy,iye
         if(ios.ne.0)cycle INFINITE
      else
         read(buff,*,iostat=ios) idy,mon,iye
         if(ios.ne.0)cycle INFINITE
      endif
      !!if(iye.le.99)then
      !!   iye=iye+2000                                       ! Cope with two digit year (assume 21st century.)
      !!endif
      if(mon.lt.1.or.mon.gt.12) cycle INFINITE              ! Check range of months
      if(mon.eq.2) then                                     ! Special check for Feb.
         if((iye/4)*4.eq.iye) then                          ! Leap year
            if(idy.lt.1.or.idy.gt.29) cycle INFINITE
         else                                               ! Non-leap year
            if(idy.lt.1.or.idy.gt.28) cycle INFINITE
         endif
      else
         if(idy.lt.1.or.idy.gt.idmon(mon)) cycle INFINITE   ! Error ..... re-input
      endif
      exit
   enddo INFINITE
   if(verbose)write(*,*)'GOT HERE H ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
   if(itries.ge.1000)then
      write(*,*)'*guessdate* ERROR: could not extract date for '//trim(datestring)
   endif
   dat(1)=iye
   dat(2)=mon
   dat(3)=idy
   dat(4)=itz
   dat(5)=ihr
   dat(6)=imi
   dat(7)=ise
   dat(8)=imill
   if(present(ier))ier=ier_local
end subroutine guessdate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_guessdate
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only: guessdate, w2d, d2w, fmtdate
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

call unit_check_start('guessdate')

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
   call guessdate(date1,dat)                                         ! convert date string to DAT
   call unit_check('guessdate',fmtdate(dat,"%I").eq.iso_week_date,msg=date1)
   call unit_check('guessdate',fmtdate(dat,"year-month-day").eq.date2,msg=date2)
enddo

call unit_check_done('guessdate')

end subroutine test_guessdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dow(3f) - [M_time] given a date-time array DAT return the day of the week
!!
!!##SYNOPSIS
!!
!!    subroutine dow(values, weekday, day, ierr)
!!
!!     integer,intent(in) :: values(8)
!!     integer,intent(out),optional :: weekday
!!     character(len=*),intent(out),optional :: day
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!     Given a date array DAT
!!     return the day of the week as a number and a name, Mon=1.
!!
!!##OPTIONS
!!    values   "DAT" array (an integer array of the same format as
!!             the array returned by the intrinsic DATE_AND_TIME(3f))
!!             describing the date to be used to calculate the day
!!             of the week.
!!##RETURNS
!!    weekday  The numeric day of the week, starting with Monday=1.
!!             Optional.
!!    day      The name of the day of the week.
!!             Optional.
!!    ierr     Error code
!!
!!             o [ 0] correct
!!             o [-1] invalid input date
!!             o [-2] neither day nor weekday return values specified
!!
!!             If the error code is not returned and an error occurs,
!!             the program is stopped.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_dow
!!     use M_time, only : dow
!!     implicit none
!!     integer          :: dat(8)     ! input date array
!!     integer          :: weekday
!!     character(len=9) :: day
!!     integer          :: ierr
!!
!!       call date_and_time(values=dat)
!!       call dow(dat, weekday, day, ierr)
!!       write(*,'(a,i0)')'weekday=',weekday
!!       write(*,'(a,a)')'day=',trim(day)
!!       write(*,'(a,i0)')'ierr=',ierr
!!
!!     end program demo_dow
!!
!!    results:
!!
!!     weekday=1
!!     day=Monday
!!     ierr=0
!===================================================================================================================================
subroutine dow(values, weekday, day, ierr)

character(len=*),parameter::ident_15="@(#)M_time::dow(3f): Given DAT date-time array return the day of the week"

integer,intent(in)                    :: values(8) ! date and time array used to get time zone
integer,intent(out),optional          :: weekday   ! The day of the week, 1 = Monday, 7 = Sunday
character(len=*),intent(out),optional :: day       ! The name of the day of the week, e.g. 'Sunday'. Minimum length = 9
integer,intent(out),optional          :: ierr      ! Error code,0=correct,-1=invalid input date,-2=neither day nor weekday specified
   real(kind=realtime)                :: julian    ! the Julian Date for which the weekday is required,
   integer                            :: iweekday
   integer                            :: ierr_local

   call date_to_julian(values,julian,ierr_local)   ! need Julian Date to calculate day of week for first day of month
   ierr_local = 0
   iweekday=0  ! bad value.

   if(julian < 0) then
      ierr_local = -1
   elseif(.not.present(day).and. .not.present(weekday)) then
      ierr_local=-2
   else
      ! Julian Day is in Z time zone and starts at noon so add 1/2 day; and add time zone
      iweekday = mod(int((julian+dble(values(4)/60.0d0/24.0d0)+0.5d0)+1.0d0), 7)
      iweekday = iweekday +1  ! change range from 0 to 6 to 1 to 7
      iweekday = mod(iweekday+5,7)+1  ! change from Sunday=1 to Monday=1

      if(present(day)) then
         select case(iweekday)
         case(1)     ;day = 'Monday'
         case(2)     ;day = 'Tuesday'
         case(3)     ;day = 'Wednesday'
         case(4)     ;day = 'Thursday'
         case(5)     ;day = 'Friday'
         case(6)     ;day = 'Saturday'
         case(7)     ;day = 'Sunday'
         case default;day = 'error'
         end select
      endif

   endif

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then
      write(*,*)'*dow* Unprocessed Error ',ierr_local,' stopping.'
      stop 2
   endif

   if(present(weekday))then
      weekday=iweekday
   endif

end subroutine dow
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dow
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : dow
implicit none
integer          :: dat(8)     ! input date array
integer          :: weekday
character(len=9) :: day
integer          :: ierr
call unit_check_start('dow')

   call dow(dat, weekday, day, ierr)
   write(*,'(a,i0)')'weekday=',weekday
   write(*,'(a,a)')'day=',trim(day)
   write(*,'(a,i0)')'ierr=',ierr

call unit_check_done('dow')

end subroutine test_dow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2w(3f) - [M_time] calculate iso-8601 Week-numbering year date yyyy-Www-d given DAT date-time array
!!
!!##SYNOPSIS
!!
!!    subroutine d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
!!
!!     integer,intent(in)              :: dat(8)     ! input date array
!!     integer,intent(out)             :: iso_year, iso_week, iso_weekday
!!     character(len=10),intent(out)   :: iso_name
!!
!!##DESCRIPTION
!!     Given a "DAT" array defining a date and time, return the ISO-8601
!!     Week in two formats -- as three integer values defining the ISO year,
!!     week of year and weekday; and as a string of the form "yyyy-Www-d".
!!
!!##OPTIONS
!!    dat          "DAT" array (an integer array of the same format as
!!                 the array returned by the intrinsic DATE_AND_TIME(3f))
!!                 describing the date, which is the basic time description
!!                 used by the other M_time(3fm) module procedures.
!!##RETURNS
!!    iso_year     ISO-8601 year number for the given date
!!    iso_week     ISO-8601 week number for the given date
!!    iso_weekday  ISO-8601 weekday number for the given date
!!    iso_name     ISO-8601 Week string for the data in the form "yyyy-Www-d".
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2w
!!     use M_time, only : d2w
!!     implicit none
!!     integer           :: dat(8)     ! input date array
!!     integer           :: iso_year, iso_week, iso_weekday
!!     character(len=10) :: iso_name
!!
!!        call date_and_time(values=dat)
!!        call d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
!!        write(*,'("ISO-8601 Week:   ",a)')iso_name
!!        write(*,'(a,i0)')'ISO-8601 year    ',iso_year
!!        write(*,'(a,i0)')'ISO-8601 week    ',iso_week
!!        write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
!!     end program demo_d2w
!!
!!    results:
!!
!!     ISO-8601 Week:   2016-W29-1
!!     ISO-8601 year    2016
!!     ISO-8601 week    29
!!     ISO-8601 weekday 1
!!
!!##DEFINITION
!!    The ISO-8601 date and time standard was issued by the International Organization for Standardization (ISO).
!!    It is used (mainly) in government and business for fiscal years, as well as in timekeeping.
!!    The system specifies a week year atop the Gregorian calendar by defining a notation for ordinal weeks of the year.
!!
!!    An ISO week-numbering year (also called ISO year informally) has 52 or 53 full weeks.
!!    That is 364 or 371 days instead of the usual 365 or 366 days.
!!    The extra week is referred to here as a leap week, although ISO-8601 does not use this term.
!!    Weeks start with Monday.
!!    The first week of a year is the week that contains the first Thursday of the year (and, hence, always contains 4 January).
!!    ISO week year numbering therefore slightly deviates from the Gregorian for some days close to January 1st.
!!
!!##CALCULATION
!!    The ISO-8601 week number of any date can be calculated, given its ordinal date (i.e. position within the year)
!!    and its day of the week.
!!
!!##METHOD
!!     Using ISO weekday numbers (running from 1 for Monday to 7 for Sunday),
!!     subtract the weekday from the ordinal date, then add 10. Divide the result
!!     by 7. Ignore the remainder; the quotient equals the week number. If
!!     the week number thus obtained equals 0, it means that the given date
!!     belongs to the preceding (week-based) year. If a week number of 53 is
!!     obtained, one must check that the date is not actually in week 1 of the
!!     following year.
!!
!!     These two statements are assumed true when correcting the dates around January 1st:
!!
!!     o The number of weeks in a given year is equal to the corresponding week number of 28 December.
!!     o January 4th is always in the first week.
!!
!!##ISO_NAME
!!     Week date representations are in the format YYYYWww-D.
!!
!!     o [YYYY] indicates the ISO week-numbering year which is slightly different from the traditional Gregorian calendar year.
!!     o [Www] is the week number prefixed by the letter W, from W01 through W53.
!!     o [D] is the weekday number, from 1 through 7, beginning with Monday and ending with Sunday.
!!
!!    For example, the Gregorian date 31 December 2006 corresponds to the Sunday of the 52nd week of 2006, and is written
!!
!!     2006-W52-7 (extended form)
!!     or
!!     2006W527 (compact form).
!!
!!##REFERENCE
!!    From Wikipedia, the free encyclopedia 2015-12-19
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!===================================================================================================================================
subroutine d2w(dat,iso_year,iso_week,iso_weekday,iso_name)

character(len=*),parameter::ident_16="@(#)M_time::d2w(3f): DAT date-time array to iso-8601 Week-numbering year date yyyy-Www-d"

integer,intent(in)              :: dat(8)     ! input date array
integer,intent(out)             :: iso_year, iso_week, iso_weekday
character(len=10),intent(out)   :: iso_name
integer                         :: shared_weekday
integer                         :: last_week_this_year
integer                         :: dec28_lastyear(8)   ! December 28th is always in last week
integer                         :: dec28_thisyear(8)   ! December 28th is always in last week
character(len=9)                :: day
integer                         :: ierr
   iso_year=dat(1)                                               ! initially assume the iso_year is the same as the data array year
   iso_week=uncorrected_week_of_year(dat)                        ! this is the week number unless around January 1st
   iso_weekday=shared_weekday                                    ! this is the number of the day of the week assuming Monday=1
   dec28_thisyear=[dat(1),12,28,dat(4),0,0,0,0]                  ! Dec 28th is always in last week; use this to get number of weeks
   last_week_this_year=uncorrected_week_of_year(dec28_thisyear)  ! get the number of the last week of the year (52 or 53)

   ! correct dates around January 1st
   if(iso_week  < 1)then                                         ! if week < 1 then week = lastWeek(year -1)
      dec28_lastyear=[dat(1)-1,12,28,dat(4),0,0,0,0]             ! Dec 28th is always in last week, we want its week number
      iso_week=uncorrected_week_of_year(dec28_lastyear)          ! got the week number for the last week of last year (52 or 53)
      iso_year=dat(1)-1                                          ! our date belongs to last year
   elseif(iso_week >last_week_this_year)then                     ! if week > lastweek(year) then week = 1
      iso_week=iso_week-last_week_this_year                      ! our date belongs to next year
      iso_year=dat(1)+1
   endif

   write(iso_name,'(i4.4,"-W",i2.2,"-",i1)')iso_year,iso_week,iso_weekday ! create ISO string designation for our date

contains
   function uncorrected_week_of_year(datin)
   implicit none
   integer            :: uncorrected_week_of_year
   integer,intent(in) :: datin(8)
      integer         :: ordinal
      call dow(datin,shared_weekday,day,ierr)                 ! formula needs day of week 1..7 where Monday=1
      ordinal=d2o(datin)                                      ! formula needs ordinal day of year where Jan 1st=1
      uncorrected_week_of_year=(ordinal-shared_weekday+10)/7
   end function uncorrected_week_of_year

end subroutine d2w
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2w()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : d2w
implicit none
integer          :: dat(8)
integer          :: iyear,iweek,iweekday
character(len=10):: name

   call unit_check_start('d2w',msg='Examples of contemporary dates around New Year''s Day')
   call date_and_time(values=dat)

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

   call unit_check_done('d2w') ! assume if got here passed checks

contains

subroutine showme(string)
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
character(len=*) :: string
   call d2w(dat,iyear,iweek,iweekday,name)
   call unit_check('d2w', name.eq.string ,msg=msg(iyear,iweek,iweekday,name,string))
end subroutine showme

end subroutine test_d2w
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    w2d(3f) - [M_time] calculate DAT date-time array from iso-8601 Week-numbering year date yyyy-Www-d
!!
!!##SYNOPSIS
!!
!!    subroutine w2d(iso_year,iso_week,iso_weekday,dat)
!!
!!     integer,intent(in)      :: iso_year, iso_week, iso_weekday
!!     integer,intent(out)     :: dat(8)     ! output date array
!!
!!##DESCRIPTION
!!    Given an ISO-8601 week return a "DAT" array defining a date and time,
!!    The ISO-8601 is supplied as three integer values defining the ISO
!!    year, week of year and weekday.
!!
!!##OPTIONS
!!    iso_year     ISO-8601 year number for the given date
!!    iso_week     ISO-8601 week number for the given date
!!    iso_weekday  ISO-8601 weekday number for the given date
!!    iso_name     ISO-8601 Week string for the data in the form "yyyy-Www-d".
!!
!!##RETURNS
!!    dat          "DAT" array (an integer array of the same format as
!!                 the array returned by the intrinsic DATE_AND_TIME(3f))
!!                 describing the date to be used, which is the basic
!!                 time description used by the other M_time(3fm) module
!!                 procedures.
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_w2d
!!     use M_time, only : w2d, fmtdate
!!     implicit none
!!        write(*,'(a)')'Given Monday 29 December 2008 is written "2009-W01-1"'
!!        call printit(2009,1,1)
!!        write(*,'(a)')'Given Sunday 3 January 2010 is written "2009-W53-7"'
!!        call printit(2009,53,7)
!!        write(*,'(a)')'Given the Gregorian date Sun 31 December 2006 is written 2006-W52-7'
!!        call printit(2006,52,7)
!!        write(*,'(a)')'Given 27 September 2008 is 2008-W39-6'
!!        call printit(2008,39,6)
!!     contains
!!     subroutine printit(iso_year,iso_week,iso_weekday)
!!     integer  :: iso_year, iso_week, iso_weekday ! ISO-8601 Week:   2016-W29-1
!!     integer  :: dat(8)                          ! input date array
!!        call w2d(iso_year,iso_week,iso_weekday,dat)
!!        write(*,'(a,i0)')'GIVEN:           '
!!        write(*,'(a,i0)')'ISO-8601 year    ',iso_year
!!        write(*,'(a,i0)')'ISO-8601 week    ',iso_week
!!        write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
!!        write(*,'(a,i0)')'RESULT:          '
!!        write(*,'(a,*(i0:,","))')'   DAT array        ',dat
!!        write(*,'(a,/,77("="))')'    '//fmtdate(dat,'long')
!!     end subroutine printit
!!     end program demo_w2d
!!
!!    Results:
!!
!!     Given Monday 29 December 2008 is written "2009-W01-1"
!!     GIVEN:
!!     ISO-8601 year    2009
!!     ISO-8601 week    1
!!     ISO-8601 weekday 1
!!     RESULT:
!!        DAT array        2008,12,29,-240,0,0,0,0
!!         Monday, December 29th, 2008 12:00:00 AM UTC-04:00
!!     =============================================================================
!!     Given Sunday 3 January 2010 is written "2009-W53-7"
!!     GIVEN:
!!     ISO-8601 year    2009
!!     ISO-8601 week    53
!!     ISO-8601 weekday 7
!!     RESULT:
!!        DAT array        2010,1,3,-240,0,0,0,0
!!         Sunday, January 3rd, 2010 12:00:00 AM UTC-04:00
!!     =============================================================================
!!     Given the Gregorian date Sun 31 December 2006 is written 2006-W52-7
!!     GIVEN:
!!     ISO-8601 year    2006
!!     ISO-8601 week    52
!!     ISO-8601 weekday 7
!!     RESULT:
!!        DAT array        2006,12,31,-240,0,0,0,0
!!         Sunday, December 31st, 2006 12:00:00 AM UTC-04:00
!!     =============================================================================
!!     Given 27 September 2008 is 2008-W39-6
!!     GIVEN:
!!     ISO-8601 year    2008
!!     ISO-8601 week    39
!!     ISO-8601 weekday 6
!!     RESULT:
!!        DAT array        2008,9,27,-240,0,0,0,0
!!         Saturday, September 27th, 2008 12:00:00 AM UTC-04:00
!!     =============================================================================
!!
!!##DEFINITION
!!    The ISO-8601 date and time standard was issued by the International
!!    Organization for Standardization (ISO). It is used (mainly) in
!!    government and business for fiscal years, as well as in timekeeping.
!!    The system specifies a week year atop the Gregorian calendar by
!!    defining a notation for ordinal weeks of the year.
!!
!!    An ISO week-numbering year (also called ISO year informally) has
!!    52 or 53 full weeks. That is 364 or 371 days instead of the usual
!!    365 or 366 days. The extra week is referred to here as a leap week,
!!    although ISO-8601 does not use this term. Weeks start with Monday.
!!    The first week of a year is the week that contains the first Thursday
!!    of the year (and, hence, always contains 4 January). ISO week year
!!    numbering therefore slightly deviates from the Gregorian for some
!!    days close to January 1st.
!!
!!##METHOD
!!     Calculating a date given the year, week number and weekday
!!
!!     This method requires that one know the weekday of 4 January of the year
!!     in question. Add 3 to the number of this weekday, giving a correction
!!     to be used for dates within this year.
!!
!!     Method: Multiply the week number by 7, then add the weekday. From this
!!     sum subtract the correction for the year. The result is the ordinal
!!     date, which can be converted into a calendar date.
!!     If the ordinal date thus obtained is zero or negative,
!!     the date belongs to the previous calendar year; if greater than the
!!     number of days in the year, to the following year.
!!
!!     Example: year 2008, week 39, Saturday (day 6)
!!     Correction for 2008: 5 + 3 = 8
!!     (39 x 7) + 6 = 279
!!     279 - 8 = 271
!!     Ordinal day 271 of a leap year is day 271 - 244 = 27 September
!!     Result: 27 September 2008
!!
!!##ISO_NAME
!!     Week date representations are in the format YYYYWww-D.
!!
!!     o [YYYY] indicates the ISO week-numbering year which is slightly
!!       different from the traditional Gregorian calendar year.
!!     o [Www] is the week number prefixed by the letter W, from W01 through W53.
!!     o [D] is the weekday number, from 1 through 7, beginning with Monday
!!       and ending with Sunday.
!!
!!    For example, the Gregorian date 31 December 2006 corresponds to the
!!    Sunday of the 52nd week of 2006, and is written
!!
!!     2006-W52-7 (extended form)
!!     or
!!     2006W527 (compact form).
!!
!!##REFERENCE
!!    From Wikipedia, the free encyclopedia 2016-08-08
!!
!!##AUTHOR
!!    John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine w2d(iso_year,iso_week,iso_weekday,dat)

character(len=*),parameter::ident_17="&
&@(#)M_time::w2d(3f): convert iso-8601 Week-numbering year date yyyy-Www-d to DAT date-time array"

integer,intent(in)              :: iso_year, iso_week, iso_weekday
integer,intent(out)             :: dat(8)     ! output date array
   integer                      :: jan4weekday
   integer                      :: correction
   integer                      :: ordinal
   integer                      :: ierr
   call dow( [iso_year,1,4,0,12,0,0,0], jan4weekday, ierr=ierr) ! get day of week for January 4th where Sun=1
   correction=jan4weekday+3                      ! calculate correction
   ordinal=iso_week*7+iso_weekday-correction     ! calculate ordinal day
   dat=o2d(ordinal,iso_year)                     ! convert ordinal to DAT (routine works with negative values or days past year end)
end subroutine w2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_w2d
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only: w2d
implicit none
character(len=372),allocatable :: line(:)
integer            :: y,m,d
integer            :: iso_year
integer            :: iso_week
integer            :: iso_weekday
integer            :: dat(8)
integer            :: i

call unit_check_start('w2d')

! the data file with dates to read and expected answers and comments
line=[ character(len=372) :: &

& ' 2005 01 01  2004 53 6   ', &
& ' 2005 01 02  2004 53 7   ', &
& ' 2005 12 31  2005 52 6   ', &
& ' 2007 01 01  2007 01 1   ', &
& ' 2007 12 30  2007 52 7   ', &
& ' 2007 12 31  2008 01 1   ', &
& ' 2008 01 01  2008 01 2   ', &
& ' 2008 12 28  2008 52 7   ', &
& ' 2008 12 29  2009 01 1   ', &
& ' 2008 12 30  2009 01 2   ', &
& ' 2008 12 31  2009 01 3   ', &
& ' 2009 01 01  2009 01 4   ', &
& ' 2009 12 31  2009 53 4   ', &
& ' 2010 01 01  2009 53 5   ', &
& ' 2010 01 02  2009 53 6   ', &
& ' 2010 01 03  2009 53 7   ', &
& '                          ' ]
do i=1,size(line)-1
   read(line(i),*)y,m,d,iso_year,iso_week,iso_weekday
   call w2d(iso_year,iso_week,iso_weekday,dat)                                      ! convert ISO week date to DAT
   call unit_check('w2d',dat(1).eq.y.and.dat(2).eq.m.and.dat(3).eq.d,msg=line(i))   ! all should match
enddo

call unit_check_done('w2d')

end subroutine test_w2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    box_month(3f) - [M_time] create specified month in a character array
!!
!!##SYNOPSIS
!!
!!    subroutine box_month(dat,calen)
!!
!!     integer,intent(in)    :: dat(8)
!!     character(len=21)     :: calen(8)
!!
!!##DESCRIPTION
!!    box_month(3f) uses a year and month from a date array to populate
!!    a small character array with a calendar representing the month.
!!
!!##OPTIONS
!!    dat  "DAT" array (an integer array of the same format as
!!          the array returned by the intrinsic DATE_AND_TIME(3f))
!!          describing the date to be used to specify what calendar
!!          month to produce.
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!##RETURNS
!!    calen  returned character array holding a display of the
!!           specified month
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_box_month
!!     use M_time, only : box_month
!!     implicit none
!!     integer           :: dat(8)
!!     character(len=21) :: calendar(8)
!!        call date_and_time(values=dat)
!!        call box_month(dat,calendar)
!!        write(*,'(a)')calendar
!!     end program demo_box_month
!!
!!    results:
!!
!!      >     July 2016
!!      >Mo Tu We Th Fr Sa Su
!!      >             1  2  3
!!      > 4  5  6  7  8  9 10
!!      >11 12 13 14 15 16 17
!!      >18 19 20 21 22 23 24
!!      >25 26 27 28 29 30 31
!===================================================================================================================================
subroutine box_month(dat,calen)
use M_strings, only : adjustc, v2s

character(len=*),parameter::ident_18="@(#)M_time::box_month(3f): generate month specified by DAT date-time array in character array"

integer,parameter                :: wklen=3*7
!-----------------------------------------------------------------------------------------------------------------------------------
! uses year and month from date array DAT to populate a small character array with a calendar representing the month
integer,intent(in)               :: dat(8)
character(len=wklen)             :: calen(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   real(kind=realtime)           :: julian
   integer                       :: weekday
   integer                       :: dat_1st(8)
   integer                       :: dat_nextday(8)
   integer                       :: location,ierr,i
!-----------------------------------------------------------------------------------------------------------------------------------
   calen(:)='                    '                                 ! initialize output array to spaces
   dat_1st=[dat(1),dat(2),1,dat(4),0,0,0,0]                        ! create date array for first day in month specified
   call dow(dat_1st, weekday, ierr=ierr)                           ! return the day of the week for first of month
!-----------------------------------------------------------------------------------------------------------------------------------
   calen(1)=adjustc(v2mo(dat(2))//' '//v2s(dat(1)),len(calen(1)))  ! build first line with month and year centered
   calen(2)='Mo Tu We Th Fr Sa Su'                                 ! build second line with days of week
!-----------------------------------------------------------------------------------------------------------------------------------
   location=1+((weekday-1)*3)                                      ! if data were one row where would 3-character day value start?
   call date_to_julian(dat_1st,julian,ierr)                        ! get Julian Date for 1st day of month
   MNTH: do i=1,31                                                 ! put dates into rest of array starting at third line
      write(calen(location/wklen+3)(mod(location,wklen):),'(i2)')i
      if(i.ge.28)then                                              ! is tomorrow in another month?
         call julian_to_date(julian+i,dat_nextday,ierr)
         if(dat_nextday(2).ne.dat(2))then
            exit MNTH
         endif
      endif
      location=location+3
   enddo MNTH
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine box_month
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_box_month
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('box_month')
call unit_check_done('box_month')
end subroutine test_box_month
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2j(3f) - [M_time] given DAT date-time array returns Julian Date
!!
!!##SYNOPSIS
!!
!!    function d2j(dat) result (julian)
!!
!!     integer,intent(in)  :: dat(8)
!!     real(kind=realtime) :: julian
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!    dat       Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f).
!!              If not present, use current time.
!!
!!              dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!##RETURNS
!!    julian    The Julian Date.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_dj
!!     use M_time, only : d2j
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Julian Date is ',d2j(dat)
!!     end program demo_dj
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:2:11:50:885
!!     Julian Date is    2457588.7582278359
!===================================================================================================================================
function d2j(dat) result (julian)

character(len=*),parameter::ident_19="@(#)M_time::d2j(3f): Given DAT date-time array returns Julian Date"

integer,intent(in),optional :: dat(8)
real(kind=realtime)         :: julian
   integer                  :: ierr
   integer                  :: dat_local(8)

   if(present(dat))then                      ! if dat array is present use value contained in it
      call date_to_julian(dat,julian,ierr)
   else                                      ! if dat array is not present create one containing current time
      call date_and_time(values=dat_local)
      call date_to_julian(dat_local,julian,ierr)
   endif

end function d2j
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2j
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
real(kind=realtime)         :: julian

call unit_check_start('d2j')

   call unit_check_start('d2j',msg='Checking Julian Date') ! assume if got here passed checks

   julian=d2j( [1970, 1, 1,0, 0,0,0,0])
   call unit_check('d2j',abs(julian-2440587.5).lt.0.00001 ,msg="Dec 31st, 1969  8:00(2440587.5)")

   julian=d2j( [1995, 1, 1,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2449719 ,msg="Jan  1st, 1995 12:00(2449719)")

   julian=d2j( [1995,10,19,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450010, msg="Oct 19th, 1995 12:00(2450010)")

   julian=d2j( [1995,12,31,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450083, msg="Dec 31st, 1995 12:00(2450083)")

   julian=d2j( [1996, 1, 1,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450084, msg="Jan  1st, 1996 12:00(2450084)")

   julian=d2j( [1996,12,31,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450449, msg="Dec 31th, 1996 12:00(2450449)")

call unit_check_done('d2j')

end subroutine test_d2j
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    j2d(3f) - [M_time] given a JED (Julian Ephemeris Date) returns a date-time array DAT.
!!
!!##SYNOPSIS
!!
!!    function j2d(julian) result (dat)
!!
!!     real(kind=realtime),intent(in),optional :: julian
!!     integer                                 :: dat(8)
!!
!!##DESCRIPTION
!!
!!     Converts a Julian Ephemeris Date to a DAT date-time array.
!!
!!##OPTIONS
!!    julian    A Julian Ephemeris Date (JED) is the number of days since
!!              noon (not midnight) on January 1st, 4713 BC.
!!              If not present, use current time.
!!
!!##RETURNS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_jd
!!     use M_time, only : j2d, d2j, fmtdate, realtime
!!     implicit none
!!     real(kind=realtime) :: today
!!     integer :: dat(8)
!!        call date_and_time(values=dat) ! get the date using intrinsic
!!        today=d2j(dat)                  ! convert today to Julian Date
!!        write(*,*)'Today=',fmtdate(j2d(today))
!!        ! math is easy with Julian Days and Julian Dates
!!        write(*,*)'Yesterday=',fmtdate(j2d(today-1.0d0))
!!        write(*,*)'Tomorrow=',fmtdate(j2d(today+1.0d0))
!!     end program demo_jd
!!
!!    results:
!!
!!     Today=Tuesday, July 19th, 2016 08:48:20 AM
!!     Yesterday=Monday, July 18th, 2016 08:48:20 AM
!!     Tomorrow=Wednesday, July 20th, 2016 08:48:20 AM
!===================================================================================================================================
function j2d(julian) result (dat)

character(len=*),parameter::ident_20="@(#)M_time::j2d(3f): Given Julian Date returns DAT date-time array"

real(kind=realtime),intent(in)   :: julian
integer                          :: dat(8)
   integer                       :: ierr
   call julian_to_date(julian,dat,ierr)
end function j2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_j2d
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
real(kind=realtime)          :: juliandate
integer                      :: dat(8)
integer                      :: ierr
character(len=:),allocatable :: expected

call unit_check_start('j2d')


   juliandate=2457589.129d0                 ! set sample Julian Date

   expected='2016-07-19 11:05:45'
   call unit_check('j2d',fmtdate(j2d(juliandate),'year-month-day hour:minute:second').eq.expected, &
   & msg=msg(juliandate,'==>',expected))

   ! go back one day
   expected='2016-07-18 11:05:45'
   call unit_check('j2d',fmtdate(j2d(juliandate-1.0d0),'year-month-day hour:minute:second').eq.expected, &
   & msg=msg(juliandate,'==>',expected))

   ! go forward one day
   expected='2016-07-20 11:05:45'
   call unit_check('j2d',fmtdate(j2d(juliandate+1.0d0),'year-month-day hour:minute:second').eq.expected, &
   & msg=msg(juliandate,'==>',expected))

call unit_check_done('j2d')

end subroutine test_j2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2u(3f) - [M_time] given DAT date-time array returns Unix Epoch Time (UET starts at 0000 on 1 Jan. 1970, UTC)
!!
!!##SYNOPSIS
!!
!!    function d2u(dat) result (unixtime)
!!
!!       integer,intent(in),optional :: dat(8)
!!       real(kind=realtime)         :: unixtime
!!
!!##DESCRIPTION
!!    Converts a DAT date-time array to a Unix Epoch Time value. Typically
!!    mathematical operations such as sums, sorting and comparison are
!!    performed with simple UET numeric values, and then they are converted
!!    back.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f).
!!          If not present the current time is used
!!
!!           dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_du
!!     use M_time, only : d2u
!!     implicit none
!!     integer           :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Unix Epoch time is ',d2u(dat)
!!     end program demo_du
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:2:0:48:561
!!     Unix Epoch time is    1468908048.5610321
!===================================================================================================================================
function d2u(dat) result (unixtime)

character(len=*),parameter::ident_21="@(#)M_time::d2u(3f): Given DAT date-time array returns Unix Epoch time"

real(kind=realtime)           :: unixtime
integer,intent(in),optional   :: dat(8)
   integer                    :: datlocal(8)
   integer                    :: ierr
   if(present(dat))then
      datlocal=dat
   else
      call date_and_time(values=datlocal)  ! current time is placed in array
   endif
   call date_to_unix(datlocal,unixtime,ierr)
end function d2u
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2u()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : d2u
implicit none

!  Note that time zones are usually -HHMM or -HH:MM and not MM, which is what the DAT array uses
!  Comparing to Unix date(1) command:
!    date --date "Wed Mar 29 01:46:47 EDT 2017" +%s      ! 1490766407
!    date --date "Wed Mar 29 01:46:47 2017" +%s          ! 1490766407
!    date --date "Wed Mar 29 01:46:47 -400 2017" +%s     ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-400 2017" +%s  ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-4:00 2017" +%s ! 1490766407

   call unit_check_start('d2u')

   call unit_check('d2u',nint(d2u([2017,03,29,-240,01,46,47,0])+0.5).eq.1490766407,msg=msg(d2u([2017,03,29,-240,01,46,47,0]) ))

   call unit_check_done('d2u')

end subroutine test_d2u
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    u2d(3f) - [M_time] given Unix Epoch Time returns DAT date-time array
!!
!!##SYNOPSIS
!!
!!    function u2d(unixtime) result (dat)
!!
!!     class(*),intent(in),optional      :: unixtime
!!     ! integer
!!     ! real
!!     ! real(kind=realtime)
!!
!!     integer                           :: dat(8)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC. If not present, use current time.
!!
!!##RETURNS
!!    dat       Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f).
!!
!!               dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_ud
!!     use M_time, only : u2d, d2u, fmtdate, realtime
!!     implicit none
!!     real(kind=realtime) :: today
!!     integer :: dat(8)
!!        call date_and_time(values=dat) ! get the date using intrinsic
!!        today=d2u(dat)                 ! convert today to Julian Date
!!        write(*,*)'Today=',fmtdate(u2d(today))
!!        write(*,*)'Yesterday=',fmtdate(u2d(today-86400.0d0)) ! subtract day
!!        write(*,*)'Tomorrow=',fmtdate(u2d(today+86400.0d0))  ! add day
!!     end program demo_ud
!!
!!    results:
!!
!!     Today=Tuesday, July 19th, 2016 11:10:08 AM
!!     Yesterday=Monday, July 18th, 2016 11:10:08 AM
!!     Tomorrow=Wednesday, July 20th, 2016 11:10:08 AM
!===================================================================================================================================
function u2d(unixtime) result (dat)

character(len=*),parameter::ident_22="@(#)M_time::u2d(3f): Given Unix Epoch Time returns DAT date-time array"

class(*),intent(in),optional      :: unixtime
integer                           :: dat(8)
   real(kind=realtime)            :: local_unixtime
   integer                        :: ierr

   if(present(unixtime))then
      select type(unixtime)
      type is (integer);             local_unixtime=unixtime
      type is (integer(kind=8));     local_unixtime=unixtime
      type is (real);                local_unixtime=unixtime
      type is (real(kind=realtime)); local_unixtime=unixtime
      end select
   else
      local_unixtime=d2u()
   endif

   call unix_to_date(local_unixtime,dat,ierr)

end function u2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_u2d
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('u2d')
call unit_check_done('u2d')
end subroutine test_u2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_timezone() result(tz)
implicit none
integer :: tz
  integer :: timezone(8)
   call date_and_time(values=timezone)
   tz=timezone(4)
end function get_timezone
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sec2days(3f) - [M_time] convert seconds to string of form dd-hh:mm:ss
!!
!!##SYNOPSIS
!!
!!    function sec2days(seconds,crop) result(dhms)
!!
!!     real(kind=realtime),intent(in) :: seconds
!!       or
!!     integer,intent(in)             :: seconds
!!       or
!!     real,intent(in)                :: seconds
!!       or
!!     character(len=*)               :: seconds
!!
!!     logical,intent(in),optional    :: crop
!!     character(len=:),allocatable   :: dhms
!!
!!##DESCRIPTION
!!    Given a number of seconds convert it to a string of the form
!!
!!       dd-hh:mm:ss
!!
!!    where dd is days, hh hours, mm minutes and ss seconds.
!!
!!##OPTIONS
!!    seconds    number of seconds to convert to string of form dd-hh:mm:ss. May
!!               be of type INTEGER, REAL, REAL(KIND=REALTIME), or CHARACTER.
!!
!!               CHARACTER strings may be of the form
!!               NNdNNhNNmNNs. Case,spaces and underscores are
!!               ignored. Allowed aliases for d,h,m, and s units are
!!
!!                   d -  days,day
!!                   m -  minutes,minute,min
!!                   h -  hours,hour,hrs,hr
!!                   s -  seconds,second,sec
!!
!!               The numeric values may represent floating point numbers.
!!
!!    crop       if .true., remove leading zero day values or day and hour values.
!!               Optional, defaults to .false. .
!!##RETURNS
!!    dmhs       the returned string of form [d:h:]m:s
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_sec2days
!!     use M_time, only : sec2days
!!     implicit none
!!        write(*,*)sec2days(129860)
!!        write(*,*)sec2days(80000.0d0)
!!        write(*,*)sec2days(80000.0,crop=.true.)
!!        write(*,*)sec2days('1 day 2.0hr 100 min 300.0seconds')
!!     end program demo_sec2days
!!
!!    results:
!!
!!     1-12:04:20
!!     0-22:13:20
!!     22:13:20
!!     1-03:45:00
!===================================================================================================================================
function sec2days(seconds,crop) result(dhms)
use M_strings, only: split, compact, s2v, transliterate

character(len=*),parameter::ident_23="&
&@(#)M_time::sec2days(3f): converts seconds or string of form IId JJh KKm LLs to string showing days of form D-HH:MM:SS"

!use iso_fortran_env, only : int64
! on this platform, (select_int_kind(i),i=1,100) returns
! 1:2=1 ,3:4=2 ,5:9=4 ,10:18= 8 ,19:38=16 ,39:=-1
integer                  :: i
integer,parameter        :: k(38)=[(selected_int_kind(i),i=1,38)]
integer,parameter        :: int128=k(38)
class(*),intent(in)               :: seconds
logical,intent(in),optional       :: crop
character(len=:),allocatable      :: dhms
   real(kind=realtime), parameter :: units_hl(4)=[ 86400.0d0, 3600.0d0, 60.0d0, 1.0d0 ]
   character(len=40)              :: scratch
   integer(kind=int128)           :: days, hours, minutes, secsleft
   integer,parameter              :: one_day=86400
   integer,parameter              :: one_hour=3600
   integer,parameter              :: one_minute=60
   logical                        :: crop_local
   integer                        :: iprint
   logical                        :: negative
   integer                        :: ilast
   character(len=:),allocatable   :: strlocal
   character(len=80),allocatable  :: array(:)
   doubleprecision                :: dtime

   !  Convert input value to nearest integer
   !  Notice that the value SECONDS can be any of several types ( INTEGER,REAL,REAL(KIND=REALTIME))
   select type(seconds)
   type is (integer);               secsleft=seconds
   type is (real);                  secsleft=nint(seconds)
   type is (real(kind=realtime));   secsleft=nint(seconds)
   type is (character(len=*))

      ! note _ is removed from input strings to allow use of _ every three digits in a number as sometimes seen in Java, perl, ...
      strlocal=compact(lower(transliterate(seconds," _',",'')),'')//'                '   ! add whitespace to make room for spaces

      call substitute(strlocal,'days','d')                      ! from long names to short names substitute common aliases for units
      call substitute(strlocal,'day','d')
      call substitute(strlocal,'hours','h')
      call substitute(strlocal,'hour','h')
      call substitute(strlocal,'hrs','h')
      call substitute(strlocal,'hr','h')
      call substitute(strlocal,'minutes','m')
      call substitute(strlocal,'minute','m')
      call substitute(strlocal,'min','m')
      call substitute(strlocal,'seconds','s')
      call substitute(strlocal,'second','s')
      call substitute(strlocal,'secs','s')
      call substitute(strlocal,'sec','s')
      call substitute(strlocal,'weeks','w')
      call substitute(strlocal,'week','w')
      call substitute(strlocal,'wks','w')
      call substitute(strlocal,'wk','w')

      call substitute(strlocal,'s','s ')          ! assuming only one suffix character and not too many to exceed length of strlocal
      call substitute(strlocal,'m','m ')
      call substitute(strlocal,'h','h ')
      call substitute(strlocal,'d','d ')
      call substitute(strlocal,'w','w ')

      dtime=0.0d0
      call split(strlocal,array,' ')

      do i=1,size(array)
         ilast=len_trim(array(i))
         select case(array(i)(ilast:ilast))
         case('w'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(1)*7
         case('d'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(1)
         case('h'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(2)
         case('m'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(3)
         case('s'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(4)
         case default
            dtime=dtime+s2v(array(i))
         end select
      enddo
      secsleft=int(dtime,kind=k(38))
   end select

   if(present(crop))then    ! whether to trim cases where(days=0) and (hours=0 when days=0) from output or always show dd-hh:mm:ss
      crop_local=crop
   else
      crop_local=.false.
   endif

   if(secsleft.lt.0)then
      secsleft=-secsleft
      negative=.true.
   else
      negative=.false.
   endif

   iprint=4

   days=secsleft/one_day                  ! get whole number of days
   if(days.eq.0) iprint=3
   secsleft=secsleft-days*one_day         ! calculate remainder

   hours=secsleft/one_hour                ! get whole number of hours
   if(days.eq.0.and.hours.eq.0) iprint=2
   secsleft=secsleft-hours*one_hour

   minutes=secsleft/one_minute            ! get whole number of minutes
   secsleft=secsleft-minutes*one_minute

   if(.not.crop_local)then
      iprint=4
   endif

   select case(iprint)                    ! select format if cropping is on and leading zero values are present
   case(2)
      write(scratch,'(i2.2,":",i2.2)')minutes,secsleft
   case(3)
      write(scratch,'(i2.2,":",i2.2,":",i2.2)')hours,minutes,secsleft
   case default
      write(scratch,'(i0,"-",i2.2,":",i2.2,":",i2.2)')days,hours,minutes,secsleft
   end select

   if(negative)then
      dhms='-'//trim(scratch)
   else
      dhms=trim(scratch)
   endif

end function sec2days
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sec2days()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only: sec2days
implicit none

   call unit_check_start('sec2days')

   call unit_check('sec2days',sec2days(129860).eq.             '1-12:04:20',msg=msg('129860 is 1-12:04:20'))
   call unit_check('sec2days',sec2days(80000.0d0).eq.          '0-22:13:20',msg=msg('80000.0d0 is 0-22:13:20'))
   call unit_check('sec2days',sec2days(80000,crop=.true.).eq.    '22:13:20',msg=msg('80000 is 22:13:20'))
   call unit_check('sec2days',sec2days('1day 2hr 3 min 4s').eq.'1-02:03:04',msg=msg('1day 2hr 3 min 4s is 1-02:03:04'))

   call unit_check_done('sec2days')

end subroutine test_sec2days
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    days2sec(3f) - [M_time] convert string of form [[-]dd-]hh:mm:ss.nn to seconds
!!
!!##SYNOPSIS
!!
!!    function days2sec(str) result(time)
!!
!!     character(len=*),intent(in)       :: str
!!     real(kind=realtime)               :: time
!!
!!##DESCRIPTION
!!      Given a string representing a duration of the form
!!      "[-][[[dd-]hh:]mm:]ss"  or NNdNNhNNmNNsNNw
!!      return a value representing seconds
!!
!!      If "dd-" is present, units for the numbers are assumed to
!!      proceed from day to hour to minute to second. But if no
!!      day is present, the units are assumed to proceed from second
!!      to minutes to hour from left to right. That is ...
!!
!!         [-]dd-hh:mm:ss
!!         [-]dd-hh:mm
!!         [-]dd-hh
!!
!!         hh:mm:ss
!!         mm:ss
!!         ss
!!
!!          Where dd is days, hh hours, mm minutes and ss seconds.
!!
!!          A decimal fraction is supported on the seconds (Actually,
!!          any of the numeric values may represent positive floating
!!          point numbers). Spaces are ignored.
!!
!!        NNdNNhNNmNNs
!!          Simple numeric values may also be used with unit suffixes; where
!!          s,m,h, or d represents seconds, minutes, hours or days and w
!!          represents a week. Allowed aliases for w,d,h,m, and s units are
!!
!!                           d -  days,day
!!                           m -  minutes,minute,min,mins
!!                           h -  hours,hour,hr,hrs
!!                           s -  seconds,second,sec,secs
!!                           w -  week, weeks, wk, wks
!!
!!          The numeric values may represent floating point numbers.
!!
!!          Spaces, commas and case are ignored.
!!
!!##OPTIONS
!!       str   string of the general form dd-hh:mm:ss.nn
!!##RETURNS
!!       time  the number of seconds represented by the input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_days2sec
!!     use M_time, only : days2sec
!!     implicit none
!!        write(*,*)days2sec('1-12:04:20')
!!        write(*,*)'one second ',days2sec('1')
!!        write(*,*)'one minute ',days2sec('1:00')
!!        write(*,*)'one hour ',days2sec('1:00:00')
!!        write(*,*)'one day ',days2sec('1-00:00:00')
!!        write(*,*)nint(days2sec(' 1-12:04:20              ')) .eq. 129860
!!        write(*,*)nint(days2sec(' 1.5 days                ')) .eq. 129600
!!        write(*,*)nint(days2sec(' 1.5 days 4hrs 30minutes ')) .eq. 145800
!!        write(*,*)nint(days2sec(' 1.5d                    ')) .eq. 129600
!!        write(*,*)nint(days2sec(' 1d2h3m4s                ')) .eq. 93784
!!        ! duplicates
!!        write(*,*)nint(days2sec(' 1d1d1d                  ')) .eq. 259200
!!        ! negative values
!!        write(*,*)nint(days2sec(' 4d-12h                  ')) .eq. 302400
!!     end program demo_days2sec
!!
!!    Results:
!!
!!     129860.00000000000
!!     one second    1.0000000000000000
!!     one minute    60.000000000000000
!!     one hour    3600.0000000000000
!!     one day    86400.000000000000
!!     T
!!     T
!!     T
!!     T
!!     T
!!     T
!!     T
!===================================================================================================================================
function days2sec(str) result(time)
use M_strings, only: split, compact, s2v, transliterate
implicit none

character(len=*),parameter::ident_24="&
&@(#)M_time::days2sec(3f): convert string [[-]dd-]hh:mm:ss.nn to seconds or string IId JJh KKm LLs to seconds"

character(len=*),intent(in)       :: str
real(kind=realtime)               :: time
! Supported input syntax:
!    [-]dd-hh:mm:ss
!          hh:mm:ss
!          mm:ss
!          ss
!
   character(len=:),allocatable   :: strlocal
   character(len=128),allocatable :: array(:)
   real(kind=realtime), parameter :: units_lh(4)=[ 1.0d0, 60.0d0, 3600.0d0, 86400.0d0 ]
   real(kind=realtime), parameter :: units_hl(4)=[ 86400.0d0, 3600.0d0, 60.0d0, 1.0d0 ]
   integer                        :: i, icount, iwords, ilast
   logical                        :: negative

   time=0.0d0
   strlocal=compact(str,'')                              ! remove whitespace
   strlocal=transliterate(strlocal,"_',",'')             ! remove single quotes,underscores sometimes used in numbers
   strlocal=lower(strlocal)//repeat(' ',len(strlocal))   ! change to lowercase and add whitespace to make room for spaces

   if(len(strlocal).eq.0)then
      time=0.0d0
   elseif(scan(strlocal,'smhdw').ne.0)then               ! unit code values not DD-HH:MM:SS either plain number or unit numbers
      call substitute(strlocal,'days','d')               ! from long names to short names substitute common aliases for units
      call substitute(strlocal,'day','d')
      call substitute(strlocal,'hours','h')
      call substitute(strlocal,'hour','h')
      call substitute(strlocal,'hrs','h')
      call substitute(strlocal,'hr','h')
      call substitute(strlocal,'minutes','m')
      call substitute(strlocal,'minute','m')
      call substitute(strlocal,'mins','m')
      call substitute(strlocal,'min','m')
      call substitute(strlocal,'seconds','s')
      call substitute(strlocal,'second','s')
      call substitute(strlocal,'secs','s')
      call substitute(strlocal,'sec','s')
      call substitute(strlocal,'weeks','w')
      call substitute(strlocal,'week','w')
      call substitute(strlocal,'wks','w')
      call substitute(strlocal,'wk','w')

      call substitute(strlocal,'s','s ')          ! assuming only one suffix character and not too many to exceed length of strlocal
      call substitute(strlocal,'m','m ')
      call substitute(strlocal,'h','h ')
      call substitute(strlocal,'d','d ')
      call substitute(strlocal,'w','w ')
      call split(strlocal,array,' ')
      iwords=size(array)
      icount=0
      do i=iwords,1,-1
         icount=icount+1
         ilast=len_trim(array(i))
         select case(array(i)(ilast:ilast))
         case('w'); time=time+s2v(array(i)(:ilast-1))*units_hl(1)*7
         case('d'); time=time+s2v(array(i)(:ilast-1))*units_hl(1)
         case('h'); time=time+s2v(array(i)(:ilast-1))*units_hl(2)
         case('m'); time=time+s2v(array(i)(:ilast-1))*units_hl(3)
         case('s'); time=time+s2v(array(i)(:ilast-1))*units_hl(4)
         case default
            time=time+s2v(array(i))
         end select
      enddo
   else

      if(strlocal(1:1).eq.'-')then          ! allow negative prefix as first character but remove it and change sign of value at end
         negative=.true.
         strlocal(1:1)=' '
      else
         negative=.false.
      endif

      call split(trim(strlocal),array,' -:')
      iwords=size(array)

      if(iwords.gt.4)then
         write(*,*)'*days2sec* error: too many values in '//trim(strlocal)
         iwords=4
      endif

      if(index(strlocal,'-').gt.0)then                ! found a dash, assume has days and form DD-HH:MM:SS, DD-, DD-HH, DD-HH:MM
         do i=1,iwords
            time=time+s2v(array(i))*units_hl(i)
         enddo
      else                                            ! no dash, assume no days, either HH:MM:SS or MM:SS, SS
         icount=0
         do i=iwords,1,-1
            icount=icount+1
            ilast=len_trim(array(i))
            time=time+s2v(array(i))*units_lh(icount)
         enddo
      endif

      if(negative)time=-time

   endif

end function days2sec
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_days2sec()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only  : days2sec, realtime
implicit none

   call unit_check_start('days2sec')

   call unit_check('days2sec',nint(days2sec('1')).eq.             1, msg='1')
   call unit_check('days2sec',nint(days2sec('1:00')).eq.         60, msg='1:00')
   call unit_check('days2sec',nint(days2sec('1:00:00')).eq.    3600, msg='1:00:00')
   call unit_check('days2sec',nint(days2sec('1-00:00:00')).eq.86400, msg='1-00:00:00')
   call unit_check('days2sec',nint(days2sec('1d2h 3.0 minutes 4sec')).eq.93784,msg='1d2h 3.0 minutes 4sec')

   call unit_check('days2sec',nint(days2sec(' 1-12:04:20              ')) .eq. 129860,msg='1-12:04:20')
   call unit_check('days2sec',nint(days2sec(' 1.5 days                ')) .eq. 129600,msg='1.5 days')
   call unit_check('days2sec',nint(days2sec(' 1.5 days 4hrs 30minutes ')) .eq. 145800,msg='1.5 days 4 hrs 30 minutes')
   call unit_check('days2sec',nint(days2sec(' 1.5d                    ')) .eq. 129600,msg='1.5d')
   call unit_check('days2sec',nint(days2sec(' 1d2h3m4s                ')) .eq. 93784,msg='1d2h3m4s')
   call unit_check('days2sec',nint(days2sec(' 1d1d1d                  ')) .eq. 259200,msg='DUPLICATES: 1d1d1d')
   call unit_check('days2sec',nint(days2sec(' 4d-12h                  ')) .eq. 302400,msg='NEGATIVE VALUES: 4d-12h')
   call unit_check('days2sec',nint(days2sec(' 3  d  1 2   h           ')) .eq. 302400,msg='WHITESPACE: 3 d 1 2 h')

   call unit_check_done('days2sec')

end subroutine test_days2sec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     phase_of_moon(3f) - [M_time] return name for phase of moon for given date
!!##SYNOPSIS
!!
!!   function phase_of_moon(datin)
!!
!!    integer,intent(in)            :: datin(8)
!!    character(len=:),allocatable  :: phase_of_moon
!!
!!##DESCRIPTION
!!  Phases Of The Moon
!!
!!  This procedure is used to support the %p field descriptor for the
!!  fmtdate(3f) routine.
!!
!!  The moon circles the earth every 29.530588853 days on average, so pick a
!!  starting point and count. A new moon occurred at Julian date 2451550.1
!!  (January 6, 2000, 18:14 UTC). Then it is easy to count the number of
!!  days since the last new moon. This is an approximate calculation.
!!
!!  There are eight generally recognized phases of the moon in common use
!!
!!    o new or dark
!!    o waxing crescent
!!    o first quarter
!!    o waxing gibbous
!!    o full
!!    o waning gibbous
!!    o laster quarter
!!    o waning crescent
!!
!!  To calculate the phase of the moon simply divide the days since the
!!  last new moon by eight and select the appropriate phase.
!!
!!  Note that technically the four states (new, first quarter, full, third
!!  quarter) are events not phases. That is to say, the moon is technically
!!  only new for an instant.
!!
!!##EXAMPLES
!!
!!   Sample:
!!
!!    program demo_phase_of_moon
!!    use M_time, only : now
!!    use M_time, only : phase_of_moon
!!    use M_time, only : moon_fullness
!!    implicit none
!!    integer             :: dat(8)
!!       ! generate DAT array
!!       call date_and_time(values=dat)
!!       ! show DAT array
!!       write(*,'(" Today is:",*(i0:,":"))')dat
!!       ! the %p and %P fields are supported by fmtdate(3f)
!!       write(*,*)now('The phase of the moon is %p, with a fullness of %P')
!!       write(*,'(1x,*(a))',advance='no')'The phase of the moon is ',trim( phase_of_moon(dat)),','
!!       write(*,'(1x,a,i0,a)')'with a fullness of ', moon_fullness(dat),'%'
!!    end program demo_phase_of_moon
!!
!!   Sample output:
!!
!!     Today is:2018:11:3:-240:20:18:44:245
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!===================================================================================================================================
function phase_of_moon(datin)
implicit none

character(len=*),parameter::ident_25="@(#)M_time::phase_of_moon(3f): return name for phase of moon for given date"

integer,intent(in)            :: datin(8)
character(len=:),allocatable  :: phase_of_moon

real(kind=realtime),parameter :: syndonic_month=29.530588853_realtime ! average period of a lunar cycle, or days per lunation
integer,parameter             :: reference(*)= [2000,1,6,0,18,14,0,0] ! new moon of January 2000 was January 6, 18:14 UTC.
character(len=20),parameter   :: phase_names(*)=[ "New            ", "Waxing crescent", &
                                                  "First quarter  ", "Waxing gibbous ", &
                                                  "Full           ", "Waning gibbous ", &
                                                  "Last quarter   ", "Waning crescent"  ]
real(kind=realtime),parameter :: phase_length=syndonic_month/8_realtime  ! days per phase
integer                       :: phase
real(kind=realtime)           :: days

days= d2j(datin)-d2j(reference)                               ! days between reference date and input date
days = mod(days + phase_length/2.0, syndonic_month)           ! modulo calculation of which phase rounding up
if(days.lt.0)days=days+syndonic_month                         ! correct for days before reference date
phase = int( days * ( size(phase_names) / syndonic_month ))+1 ! index into phase names
phase_of_moon=phase_names(phase)

end function phase_of_moon
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_phase_of_moon
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('phase_of_moon')
call unit_check_done('phase_of_moon')
end subroutine test_phase_of_moon
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     moon_fullness(3f) - [M_time] return percentage of moon phase from new to full
!!##SYNOPSIS
!!
!!   function moon_fullness(datin)
!!
!!    integer,intent(in)            :: datin(8)
!!    integer                       :: moon_fullness
!!
!!##DESCRIPTION
!!
!!  This procedure is used to support the %P field descriptor for the
!!  fmtdate(3f) routine.
!!
!!  The moon circles the earth every 29.530588853 days on average, so pick
!!  a starting point and count. A new moon occurred at January 6, 2000,
!!  18:14 UTC. Then it is easy to count the number of days since the last
!!  new moon. This is an approximate calculation.
!!
!!##OPTIONS
!!
!!  dat        DAT Date array describing input date
!!
!!##RESULTS
!!
!!  moon_fullness  0 is a new or dark moon, 100 is a full moon, + for waxing
!!                 and - for waning.
!!
!!##EXAMPLES
!!
!!   Sample:
!!
!!    program demo_moon_fullness
!!    use M_time, only : now
!!    use M_time, only : phase_of_moon
!!    use M_time, only : moon_fullness
!!    implicit none
!!    integer             :: dat(8)
!!       ! generate DAT array
!!       call date_and_time(values=dat)
!!       ! show DAT array
!!       write(*,'(" Today is:",*(i0:,":"))')dat
!!       ! the %p and %P fields are supported by fmtdate(3f)
!!       write(*,*)now('The phase of the moon is %p, with a fullness of %P')
!!       write(*,'(1x,*(a))',advance='no')'The phase of the moon is ',trim( phase_of_moon(dat)),','
!!       write(*,'(1x,a,i0,a)')'with a fullness of ', moon_fullness(dat),'%'
!!    end program demo_moon_fullness
!!
!!   Sample output:
!!
!!     Today is:2018:11:3:-240:20:18:44:245
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!===================================================================================================================================
function moon_fullness(datin)
implicit none

character(len=*),parameter::ident_26="@(#)M_time::moon_fullness(3f): return percentage of moon phase from new to full"

integer,intent(in)            :: datin(8)
integer                       :: moon_fullness

real(kind=realtime),parameter :: syndonic_month=29.530588853_realtime  ! average period of a lunar cycle, or days per lunation
integer,parameter             :: reference(*)= [2000,1,6,0,18,14,0,0]  ! new moon of January 2000 was January 6, 18:14 UTC.
real(kind=realtime)           :: days_into_cycle

days_into_cycle = mod(d2j(datin)-d2j(reference) , syndonic_month)      ! number of days into lunar cycle
if(days_into_cycle.lt.0)days_into_cycle=days_into_cycle+syndonic_month ! correct for input date being before reference date

if(days_into_cycle.le.syndonic_month/2.0_realtime)then                 ! if waxing from new to full report as 0% to 100%
   moon_fullness=int((days_into_cycle/syndonic_month)*200.0_realtime+0.5_realtime)
else                                                                   ! if waning from full to new report as -99% to -1%
   moon_fullness=-(200-int((days_into_cycle/syndonic_month)*200.0_realtime))
endif

end function moon_fullness
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_moon_fullness
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('moon_fullness')
call unit_check_done('moon_fullness')
end subroutine test_moon_fullness
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    easter(3f) - [M_time] calculate date for Easter given a year
!!
!!##SYNOPSIS
!!
!!
!!   subroutine easter(year,month,day)
!!
!!     integer, intent(in)   :: year
!!     integer, intent(out)  :: day, month
!!
!!##DESCRIPTION
!!     The Date of Easter (Sunday)
!!
!!     The algorithm is due to J.-M. Oudin (1940) and is reprinted in the Explanatory
!!     Supplement to the Astronomical Almanac, ed. P. K. Seidelmann (1992).
!!     See Chapter 12, "Calendars", by L. E. Doggett.
!!
!!     The following are dates of Easter from 1980 to 2024:
!!
!!        1980  April  6        1995  April 16        2010  April  4
!!        1981  April 19        1996  April  7        2011  April 24
!!        1982  April 11        1997  March 30        2012  April  8
!!        1983  April  3        1998  April 12        2013  March 31
!!        1984  April 22        1999  April  4        2014  April 20
!!        1985  April  7        2000  April 23        2015  April  5
!!        1986  March 30        2001  April 15        2016  March 27
!!        1987  April 19        2002  March 31        2017  April 16
!!        1988  April  3        2003  April 20        2018  April  1
!!        1989  March 26        2004  April 11        2019  April 21
!!        1990  April 15        2005  March 27        2020  April 12
!!        1991  March 31        2006  April 16        2021  April  4
!!        1992  April 19        2007  April  8        2022  April 17
!!        1993  April 11        2008  March 23        2023  April  9
!!        1994  April  3        2009  April 12        2024  March 31
!!
!!     N.B. The date of Easter for the Eastern Orthodox Church may be different.
!!
!!##OPTIONS
!!      year    Year for which to calculate day that Easter falls on
!!##RESULTS
!!      month   Month on which Easter falls for a given year
!!      day     Day of month on which Easter falls for a given year
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_easter
!!    use m_time, only : easter, fmtdate
!!    implicit none
!!    integer :: year, month, day
!!    integer :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
!!      call date_and_time(values=dat)  ! get current year
!!      year=dat(1)
!!      call easter(year, month, day)
!!      ! fill out a date_and_time array
!!      dat=[dat(1),month,day,dat(4),12,0,0,0]
!!      write(*,*)fmtdate(dat,&
!!      "Easter day: the %d day of %L in the year of our Lord %Y")
!!    end program demo_easter
!!
!!   Sample output:
!!
!!    Easter day: the 16th day of April in the year of our Lord 2017
!===================================================================================================================================
!>
!!   U.S. Naval Observatory Astronomical Applications Department
!!
!!   This code assembled by Alan Miller
!!   Reference web site:
!!   http://aa.usno.navy.mil/faq/docs/easter.html
!!   Latest revision 8 April 2002
!===================================================================================================================================
SUBROUTINE Easter(year, month, day)
implicit none

character(len=*),parameter::ident_27="@(#)M_time::easter(3f): calculate date for Easter given a year"

integer, intent(in)   :: year
integer, intent(out)  :: day, month

   integer  :: c, i, j, k, l, n

   c = year / 100
   n = year - 19 * ( year / 19 )
   k = ( c - 17 ) / 25
   i = c - c / 4 - ( c - k ) / 3 + 19 * n + 15
   i = i - 30 * ( i / 30 )
   i = i - (i / 28) * (1 - (i / 28) * (29 / (i + 1 )) * ( (21 - n) / 11) )
   j = year + year / 4 + i + 2 - c + c / 4
   j = j - 7 * ( j / 7 )
   l = i - j
   month = 3 + ( l + 40 ) / 44
   day = l + 28 - 31 * ( month / 4 )

end subroutine Easter
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_easter()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time,  only : easter
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
character (len=5)  :: mon(3:4) = (/ 'march', 'april' /)

call unit_check_start('easter') ! assume if got here passed checks

do ii=1,size(tests)
   read(tests(ii),*)inyear,tmonth,tday
   call easter(inyear,outmonth,outday)
   call unit_check('easter', tmonth.eq.outmonth.and.tday.eq.outday ,msg=msg(tests(ii),'month=',mon(outmonth)))
enddo

call unit_check_done('easter') ! assume if got here passed checks

end subroutine test_easter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    ephemeris(3f) - [M_time] ephemeris position of planets for adjusting an equatorial telescope
!!
!!##SYNOPSIS
!!
!!
!!    subroutine ephemeris(itime,planet,declination_d,declination_m,declination_compass,ascent_hours,ascent_minutes)
!!
!!     integer,parameter            :: dp=kind(0.0d0)
!!     integer,intent(in)           :: itime(8)
!!     class(*),intent(in)          :: planet
!!      !!integer                   :: planet
!!      !!character(len=*)          :: planet
!!
!!     integer,intent(out)          :: declination_d, declination_m
!!     character(len=1),intent(out) :: declination_compass
!!     integer,intent(out)          :: ascent_hours,ascent_minutes
!!
!!##DESCRIPTION
!!
!!    ephemeris(3f) calculates the ephemeris of a planet in our solar system
!!    in order to adjust an equatorial telescope.
!!
!!     Ref.: "Mathematiques par l'informatique individuelle
!!           - Programmes en BASIC, MASSON,
!!           Paris, 1982, p 100 - 105" [BIBLI 06].
!!
!!##OPTIONS
!!
!!      itime   provide the date and time using the same eight values used
!!              by the DATE_AND_TIME(3f) intrinsic.
!!
!!              1. The year
!!              2. The month
!!              3. The day of the month
!!              4. Time difference with UTC in minutes
!!              5. The hour of the day
!!              6. The minutes of the hour
!!              7. The seconds of the minute
!!              8. The milliseconds of the second
!!
!!      planet  Planet number  1 to 8  or planet name (Mercury:1 Venus:2
!!              Mars:4 Jupiter:5 Saturn:6 Uranus:7 Neptune:8)
!!
!!##RESULTS
!!
!!    declination_d,        :
!!    declination_m,        :
!!    declination_compass   Declination in degrees and minutes (-90 to 90 North or South)
!!    ascent_hours,         :
!!    ascent_minutes        Ascent in hours (0 to 24) and minutes
!!
!!##EXAMPLE
!!
!!    Find ascent and declination of planet Mars on March 10th, 1982 at 6h UT
!!
!!      program demo_ephemeris
!!      use M_time, only : ephemeris, fmtdate
!!      implicit none
!!      integer            :: itime(8)
!!      integer            :: planet
!!      integer            :: declination_d, declination_m
!!      character(len=1)   :: declination_compass
!!      integer            :: ascent_hours, ascent_minutes
!!
!!      planet=4
!!      itime=[1982,3,10,0,6,0,0,0]
!!      call ephemeris(itime, planet,                    &
!!      declination_d,declination_m,declination_compass, &
!!      ascent_hours,ascent_minutes)
!!
!!      write(*, '(" For: ",a)')fmtdate(itime)
!!      write(*, "(' Planet: ',I1,1X)",advance='no')                       &
!!              planet
!!      write(*, "(' Ascent: ',I2,' H ',I2,' MN',1X)",advance='no')        &
!!              ascent_hours, ascent_minutes
!!      write(*, "(' Declination: ',I2,' D ',I2,' MN ',A1)",advance='yes') &
!!              declination_d, declination_m, declination_compass
!!
!!      end program demo_ephemeris
!!
!!    Expected output:
!!
!!      For: Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
!!      Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S
!!
!!##AUTHOR
!!
!!    o F90 Version By J-P Moreau, Paris. (www.jpmoreau.fr)
!!    o Revised By John S. Urban, 20170910
!===================================================================================================================================
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine ephemeris(itime,planet,declination_d,declination_m,declination_compass,ascent_hours,ascent_minutes)
implicit none

character(len=*),parameter::ident_28="&
&@(#)M_time::ephemeris(3f): ephemeris position of planets for adjusting an equatorial telescope"

integer,parameter            :: dp=kind(0.0d0)
integer,intent(in)           :: itime(8)
class(*),intent(in)          :: planet

integer,intent(out)          :: declination_d, declination_m
character(len=1),intent(out) :: declination_compass
integer,intent(out)          :: ascent_hours,ascent_minutes

! initialize calendar constants
real(kind=dp),parameter  :: B(12)=[0.0d0, 31.0d0, 59.0d0, 25.0d0, 90.0d0, 25.0d0, 120.0d0, 25.0d0, 151.0d0, 25.0d0, 181.0d0, 25.0d0]
real(kind=dp)            :: declination,PI,t,x,y,z
real(kind=dp)            :: ascent,gg
real(kind=dp)            :: ascent_hours8
integer                  :: year, month, day
real(kind=dp)            :: hours
integer                  :: planet_number
real(kind=dp)            :: dtime(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   select type(planet)
   type is (integer)
      planet_number=planet
   type is (character(len=*))
      select case(lower(planet))
      case('mercury');       planet_number=1
      case('venus');         planet_number=2
      case('earth','terra'); planet_number=3
      case('mars');          planet_number=4
      case('jupiter');       planet_number=5
      case('saturn');        planet_number=6
      case('uranus');        planet_number=7
      case('neptune');       planet_number=8
      case default;          planet_number=0
         write(*,*)'*ephemeris* ERROR: unknown planet name '//trim(planet)
      end select
   end select
   if(planet_number.lt.1.or.planet_number.gt.8)then
      write(*,*)'*ephemeris* ERROR: unknown planet number ',planet_number
      planet_number=3
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
! calculate time t
year=itime(1)
month=itime(2)
day=itime(3)
dtime=real(itime,kind=dp)
hours=dtime(5)-dtime(4)/60.0_dp+dtime(6)/60.0_dp+dtime(7)/3600.0_dp
t=365.25_dp*(year-1901)+B(month)+day
t=INT(t) + hours/24.0_dp
!-----------------------------------------------------------------------------------------------------------------------------------
                                      ! calculate earth coordinates
call planet_coordinates(3,t,x,y,z)    ! planet #3 coordinates
gg=x
ascent_hours8=y                       ! save earth coordinates

call planet_coordinates(planet_number,t,x,y,z)    ! calculate coordinates of planet #n
!-----------------------------------------------------------------------------------------------------------------------------------
                                      ! calculate geocentric equatorial coordinates
x=x-gg
y=y-ascent_hours8
t=y*.917484_dp-z*.397772_dp
z=y*.397772_dp+z*.917484_dp
y=t
!-----------------------------------------------------------------------------------------------------------------------------------
! calculate ascent and declination
ascent=DATAN2(y,x)
declination=DATAN2(z,DSQRT(x*x+y*y))
!-----------------------------------------------------------------------------------------------------------------------------------
! conversion
PI=4.d0*DATAN(1.d0)
ascent=ascent*12.0_dp/PI
if (ascent<0.d0) then
   ascent=24.d0+ascent
endif
ascent_hours=INT(ascent)
ascent_minutes=INT(60*(ascent-ascent_hours))
!-----------------------------------------------------------------------------------------------------------------------------------
! conversion
declination=declination*180.d0/PI
if (declination<0.d0) then
   declination_compass='S'
else
   declination_compass='N'
endif
declination=ABS(declination)
declination_d=INT(declination)
declination_m=INT(60*(declination-declination_d))
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine planet_coordinates(planet_number,t,x,y,z)
implicit none
!import   none
integer,parameter         :: dp=kind(0.0d0)
integer,intent(in)        :: planet_number
real(kind=dp),intent(in)  :: t
real(kind=dp),intent(out) :: x,y,z
   real(kind=dp),save     :: A(9,8)
   real(kind=dp)          :: xl,xm,o,p,q,e,xi,xa,xj,r,u,v
   integer                :: i
!initialize planetary constants (fill table A by columns)
data (a(i,1),i=1,9)/&
     4.01166d0, 0.071425454d0,  1.32493d0,  0.000000742289d0, 0.823045d0, 0.000000566185d0, 0.205615d0, 0.122225d0,   0.387099d0 /
data (a(i,2),i=1,9)/&
     3.60861d0, 0.027963119d0,  2.271616d0, 0.00000065572d0,  1.32291d0,  0.000000436681d0, 0.006816d0, 0.0592301d0,  0.723332d0 /
data (a(i,3),i=1,9)/&
     1.72727d0, 0.0172028d0,    1.76688d0,  0.000000818559d0, 0.0d0,      0.0d0,            0.016751d0, 0.0d0,        1.0d0      /
data (a(i,4),i=1,9)/&
     2.17756d0, 0.0091467658d0, 5.83378d0,  0.000000879297d0, 0.851616d0, 0.000000371232d0, 0.093309d0, 0.0322939d0,  1.5236d0   /
data (a(i,5),i=1,9)/&
     4.68279d0, 0.00145099d0,   0.2289d0,   0.000000857d0,    1.73578d0,  0.000000482933d0, 0.048376d0, 0.0228418d0,  5.202799d0 /
data (a(i,6),i=1,9)/&
     4.8567d0,  0.00058484d0,   1.5974d0,   0.000000412d0,    1.96856d0,  0.000000417308d0, 0.054311d0, 0.0435026d0,  9.552098d0 /
data (a(i,7),i=1,9)/&
     4.3224d0,  0.000205424d0,  2.9523d0,   0.000000762d0,    1.2825d0,   0.000000238237d0, 0.047319d0, 0.013482d0,  19.21694d0  /
data (a(i,8),i=1,9)/&
     1.5223d0,  0.000105061d0,  0.7637d0,   0.000000393d0,    2.28102d0,  0.00000052517d0,  0.008262d0, 0.0310536d0, 30.12912d0  /

!calculate planetary constants
   XL=A(1,planet_number)+A(2,planet_number)*t
   O=A(3,planet_number)+A(4,planet_number)*t
   P=A(5,planet_number)+A(6,planet_number)*t
   E=A(7,planet_number)
   XI=A(8,planet_number)
   XA=A(9,planet_number)
!solve Kepler's equation
   xm=XL-O
   u=xm

   do i=1, 10
     u=xm+E*dsin(u)
   end do

!formula (3) of reference book
   r=XA*(cos(u)-E)
   v=XA*DSQRT(1.d0-E*E)*dsin(u)
   O=O-P
   xm=dsin(O)
   O=dcos(O)
   q=dsin(P)
   P=dcos(P)
   xj=dsin(XI)
   XI=dcos(XI)
   x=(P*O-XI*q*xm)*r+(-P*xm-XI*q*O)*v
   y=(q*O+XI*P*xm)*r+(-q*xm+XI*P*O)*v
   z=xj*xm*r+xj*O*v

end subroutine planet_coordinates
!-----------------------------------------------------------------------------------------------------------------------------------
END subroutine ephemeris
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ephemeris()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only : ephemeris, fmtdate
implicit none
     integer,parameter  :: itime(8)=[1982,3,10,0,6,0,0,0] ! For: Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
     integer            :: planet
     integer            :: D_d, D_m, A_h, A_m
     character(len=1)   :: D_c

block
integer :: io=6
if(unit_check_level.gt.0)then
write(io,'(a)')'   Find ascent and declination of planets on March 10th, 1982 at 6h UT'
write(io,'(a)')''
write(io,'(a)')'      Planet     Ascent     Declination'
write(io,'(a)')'      Mercury    21 H 51    14 ° 45 mn S'
write(io,'(a)')'      Venus      20 H 26    14 ° 57 mn S'
write(io,'(a)')'      Mars       13 H 08     3 ° 45 mn S'
write(io,'(a)')'      Jupiter    14 H 32    13 ° 30 mn S'
write(io,'(a)')'      Saturn     13 H 22     5 ° 42 mn S'
write(io,'(a)')''
endif
end block

call unit_check_start('ephemeris')

     do planet=1,8
        if(planet.eq.3)cycle
        call ephemeris(itime, planet, D_d,D_m,D_c, A_h,A_m)
        call unit_check('ephemeris',D_c.eq.'S','Compass direction S')
        select case(planet)
        case(1); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[21,51,14,45]),'Mercury')
        case(2); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[20,26,14,57]),'Venus')
        case(3);
        case(4); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[13,08,03,45]),'Mars')
        case(5); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[14,32,13,30]),'Jupiter')
        case(6); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[13,22,05,42]),'Saturn')
        case(7); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[16,11,20,54]),'Uranus')
        case(8); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[17,46,22,07]),'Nepture')
        end select

     enddo

call unit_check_done('ephemeris')

end subroutine test_ephemeris
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!   XXXX
!  X    X
! X
! X
! X
! X
! X
!  X    X
!   XXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_sleep(3f) - [M_time] call C sleep(3c) or usleep(3c) procedure
!!##SYNOPSIS
!!
!!    subroutine system_sleep(wait_seconds)
!!
!!       integer,intent(in)  :: wait_seconds
!!          or
!!       real,intent(in)  :: wait_seconds
!!
!!##DESCRIPTION
!!    The system_sleep(3f) routine uses the intrinsic ISO_C_BINDING
!!    interface to call the C sleep(3c) procedure or usleep(3c)
!!    routine.
!!
!!##OPTIONS
!!     wait_seconds  integer,real or doubleprecision number of seconds for process to sleep.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_system_sleep
!!     use M_time, only : system_sleep, now
!!     implicit none
!!     integer :: i
!!        !
!!        write(*,*)"Time before integer call is: ",now()
!!        call system_sleep(4)
!!        write(*,*)"Time after  integer call is: ",now()
!!        !
!!        write(*,*)"Time before real call is: ",now()
!!        call system_sleep(4.0)
!!        write(*,*)"Time after  real call is: ",now()
!!        !
!!        write(*,*)"Time before loop is: ",now()
!!        do i=1,1000
!!           call system_sleep(4.0/1000.0)
!!        enddo
!!        write(*,*)"Time after loop  is: ",now()
!!        !
!!     end program demo_system_sleep
!!
!!    results
!!
!!     Time before integer call is: Sunday, July 17th, 2016 2:29:45 AM UTC-0240
!!     Time after integer call is: Sunday, July 17th, 2016 2:29:49 AM UTC-0240
!!     Time before real call is: Sunday, July 17th, 2016 2:29:49 AM UTC-0240
!!     Time after  real call is: Sunday, July 17th, 2016 2:29:53 AM UTC-0240
!!     Time before loop is: Sunday, July 17th, 2016 2:29:53 AM UTC-0240
!!     Time after loop  is: Sunday, July 17th, 2016 2:30:09 AM UTC-0240
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine system_sleep(seconds)
use,intrinsic                 :: iso_c_binding, only: c_int

character(len=*),parameter::ident_29="@(#)M_time::system_sleep(3f): call sleep(3c) or usleep(3c)"

class(*),intent(in)           :: seconds
integer(kind=c_int)           :: cint
   select type(seconds)
   type is (integer);             cint=seconds                  ; call call_sleep(cint)
   type is (real);                cint=nint(seconds*1000000.0)  ; call call_usleep(cint)
   type is (real(kind=realtime)); cint=nint(seconds*1000000.0)  ; call call_usleep(cint)
   end select
end subroutine system_sleep
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_sleep()
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
!!use M_time, only: system_sleep
implicit none
   integer :: systemcount1, countrate1
   integer :: systemcount2, countrate2
   real    :: slept_in_seconds

   call unit_check_start('system_sleep')

   call system_clock(count=systemcount1,count_rate=countrate1)
   call system_sleep(10)
   call system_clock(count=systemcount2,count_rate=countrate2)
   slept_in_seconds=real(systemcount2-systemcount1)/real(countrate1)

   ! a sluggish system might make more than 12 seconds pass
   call unit_check('system_sleep',  slept_in_seconds .ge. 9.5 .and. slept_in_seconds .le. 12.0 , &
      & msg=msg('asked to sleep 10 seconds, slept',slept_in_seconds,'seconds'))

   call unit_check_done('system_sleep')

end subroutine test_system_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine call_sleep(wait_seconds)
use,intrinsic                   :: iso_c_binding, only: c_int

character(len=*),parameter::ident_30="@(#)M_time::call_sleep(3fp): call sleep(3c)"

integer(kind=c_int),intent(in)  :: wait_seconds
integer(kind=c_int)             :: how_long
interface
   function c_sleep(seconds) bind (C,name="sleep")
      import
      integer(c_int)       :: c_sleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
      integer(c_int), intent(in), VALUE :: seconds
   end function c_sleep
end interface
   if(wait_seconds.gt.0)then
      how_long=c_sleep(wait_seconds)
   endif
end subroutine call_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine call_usleep(wait_seconds)
use,intrinsic                   :: iso_c_binding, only: c_int
character(len=*),parameter::ident_31="@(#)M_time::call_usleep(3fp): call usleep(3c)"
integer(kind=c_int),intent(in)  :: wait_seconds
integer(kind=c_int)             :: how_long
interface
   function c_usleep(seconds) bind (C,name="usleep")
      import
      integer(c_int)       :: c_usleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
      integer(c_int), intent(in), VALUE :: seconds
   end function c_usleep
end interface
   if(wait_seconds.gt.0)then
      how_long=c_usleep(wait_seconds)
   endif
end subroutine call_usleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_suite_M_string
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
use,intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char

implicit none
integer :: ierr
character(len=*),parameter :: SAME='-library libGPF -section 3 -description'

!! no not use M_system version or will create a circular dependency
call put_environment_variable('TZ','America/New_York',ierr) ! some of the test values assume EST

call unit_check_msg('M_time','This section contains unit tests for procedures in the M_time(3f) module.')

call unit_check_start('box_month      ',SAME//' "print specified month into character array" ')
call test_box_month()
call unit_check_start('d2j            ',SAME//' "Convert date array to Julian Date" ')
call test_d2j()
call unit_check_start('d2o            ',SAME//' "Converts date-time array to Ordinal day" ')
call test_d2o()
call unit_check_start('d2u            ',SAME//' "Convert date array to Unix Time" ')
call test_d2u()
call unit_check_start('d2w            ',SAME//' "Calculate iso-8601 Week-numbering year date yyyy-Www-d" ')
call test_d2w()
call unit_check_start('date_to_julian ',SAME//' "Converts Proleptic Gregorian date array to Julian Date" ')
call test_date_to_julian()
call unit_check_start('date_to_unix   ',SAME//' "Converts date array to Unix Time (UT starts at 0000 on 1 Jan. 1970, UTC)" ')
call test_date_to_unix()
call unit_check_start('days2sec       ',SAME//' "converts string D-HH:MM:SS to seconds from small to large" ')
call test_days2sec()
call unit_check_start('dow            ',SAME//' "Return the day of the week" ')
call test_dow()
call unit_check_start('easter         ',SAME//' "Determine month and day Easter falls on for given year" ')
call test_easter()
call unit_check_start('ephemeris      ',SAME//' "ephemeris position of planets for adjusting an equatorial telescope" ')
call test_ephemeris()
call unit_check_start('fmtdate        ',SAME//' "given date array return date as string using format" ')
call test_fmtdate()
call unit_check_start('fmtdate_usage  ',SAME//' "display macros recognized by fmtdate(3f)" ')
call test_fmtdate_usage()
call unit_check_start('guessdate      ',SAME//' "Reads in a date, in various formats" ')
call test_guessdate()
call unit_check_start('j2d            ',SAME//' "Convert Julian Date to date array" ')
call test_j2d()
call unit_check_start('julian_to_date ',SAME//' "Converts Julian Date to (year, month, day, hour, minute, second)" ')
call test_julian_to_date()
call unit_check_start('mo2d           ',SAME//' "given month name return date array for beginning of that month in current year" ')
call test_mo2d()
call unit_check_start('mo2v           ',SAME//' "given month as name return month number (1-12) of that month" ')
call test_mo2v()
call unit_check_start('moon_fullness  ',SAME//' "return name for phase of moon for given date" ')
call test_moon_fullness()
call unit_check_start('now            ',SAME//' "return string representing current time given format" ')
call test_now()
call unit_check_start('now_ex         ',SAME//' "use of now(3f) outside of a module" ')
call test_now_ex()
call unit_check_start('o2d            ',SAME//' "given ordinal day of year return date array, Jan 1st=1" ')
call test_o2d()
call unit_check_start('ordinal_to_date',SAME//' "given ordinal day of year return date array, Jan 1st=1" ')
call test_ordinal_to_date()
call unit_check_start('phase_of_moon  ',SAME//' "percentage of moon phase from new to full" ')
call test_phase_of_moon()
call unit_check_start('sec2days       ',SAME//' "converts seconds to string D-HH:MM:SS" ')
call test_sec2days()
call unit_check_start('system_sleep   ',SAME//' "call sleep(3c)" ')
call test_system_sleep()
call unit_check_start('u2d            ',SAME//' "Convert Unix Time to date array" ')
call test_u2d()
call unit_check_start('unix_to_date   ',SAME//' "Converts Unix Time to date array" ')
call test_unix_to_date()
call unit_check_start('v2mo           ',SAME//' "returns the month name of a Common month" ')
call test_v2mo()
call unit_check_start('w2d            ',SAME//' "Given iso-8601 Week-numbering year date yyyy-Www-d calculate date" ')
call test_w2d()

contains
!===================================================================================================================================
subroutine put_environment_variable(name,value,status)

!  This is an private copy of the set_environment_variable routine(3f) routine from
!  M_system.FF that is duplicated in order to prevent a circular dependency.

character(len=*),parameter::ident_32="@(#)M_system::put_environment_variable(3f): call setenv(3c) to set environment variable"

   character(len=*)               :: NAME
   character(len=*)               :: VALUE
   integer, optional, intent(out) :: STATUS
   integer                        :: loc_err

interface
   integer(kind=c_int) function c_setenv(c_name,c_VALUE) bind(C,NAME="setenv")
      import c_int, c_char
      character(kind=c_char)   :: c_name(*)
      character(kind=c_char)   :: c_VALUE(*)
   end function
end interface

   loc_err =  c_setenv(str2arr(trim(NAME)),str2arr(VALUE))
   if (present(STATUS)) STATUS = loc_err
end subroutine put_environment_variable
!===================================================================================================================================
pure function str2arr(string) result (array)


character(len=*),parameter::ident_33="@(#)M_system::str2arr(3fp): function copies string to null terminated char array"

character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)
   integer                      :: i

   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(size(array))=c_null_char

end function str2arr
!===================================================================================================================================
end subroutine test_suite_M_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_time
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! M_time calls now_ex as a regular procedure to prevent a dependency loop
function now_ex(format)
use M_time, only: now

character(len=*),parameter::ident_34="@(#)M_time::now_ex(3f): use of now(3f) outside of a module"

character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now_ex
   now_ex=now(format)
end function now_ex
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_now_ex
use M_debug, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level, msg
call unit_check_start('now_ex')
call unit_check_done('now_ex')
end subroutine test_now_ex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!     XX                                             X
!      X            X                       X
!      X            X                       X
!  XXXXX   XXXX    XXXX    XXXXX           XXXX    XXX    XXX X    XXXXX
! X    X       X    X     X     X           X        X     X X X  X     X
! X    X   XXXXX    X     XXXXXXX           X        X     X X X  XXXXXXX
! X    X  X    X    X     X                 X        X     X X X  X
! X    X  X    X    X  X  X     X           X  X     X     X X X  X     X
!  XXXXXX  XXXX X    XX    XXXXX             XX    XXXXX  XX X XX  XXXXX
!
!                                XXXXXXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! submodule is not supported by the compiler used to develop this yet or it would be worth a try
!!submodule (M_time) M_time_oop
!!end submodule M_time_oop
!
module M_time_oop
!
! Define an OOP (Object-Oriented Programming) interface for the M_time module.
!
! Leveraging the existing procedural functions in module M_TIME to do the calculations allows
! this to simply be a definition of a derived type ( TYPE(DATE_TIME) ) and the
! methods it supports and overloading of operators to support the new data type.
!
use M_time, only : d2u, u2d, fmtdate, d2o, dow, fmtdate_usage, days2sec, realtime
use M_time, only : j2d, d2j
use M_strings, only : upper
implicit none
private
private upper
!-----------------------------------------------------------------------------------------------------------------------------------
   public date_time

   private dt2d
   private epoch
   private julian
   private ordinal
   private weekday
   private format
   private delta
   private init_dt
!-----------------------------------------------------------------------------------------------------------------------------------
!DERIVED TYPE DATE_TIME
!
type date_time
   ! COMPONENTS:
   ! Eight integer element integer elements containing year, month, day, Time zone difference
   ! from UTC in minutes, hour, minutes, seconds, and milliseconds of the second in one-to-one
   ! correspondence with the elements of the array generated by the intrinsic function DATE_AND_TIME()...
   integer :: year=1970
   integer :: month=1
   integer :: day=1
   integer :: tz=0
   integer :: hour=0
   integer :: minute=0
   integer :: second=0
   integer :: millisecond=0
contains
   ! METHODS:
   procedure         :: datout => dt2d
   procedure         :: epoch
   procedure         :: julian
   procedure         :: ordinal
   procedure         :: weekday
   procedure         :: format
   procedure         :: delta
   procedure         :: init => init_dt
   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(DATE_TIME)

   procedure,private :: plus_seconds
   generic           :: operator(+) => plus_seconds

   procedure,private :: eq
   generic           :: operator(==) => eq

   procedure,private :: lt
   generic           :: operator(<)  => lt

   procedure,private :: gt
   generic           :: operator(>)  => gt

   procedure,private :: ge
   generic           :: operator(>=) => ge

   procedure,private :: le
   generic           :: operator(<=) => le

   procedure,private :: ne
   generic           :: operator(/=) => ne

!-! procedure         :: construct_from_dat
!-! generic           :: assignment(=)  => construct_from_dat

   procedure,private :: minus_seconds                     ! subtracts seconds, returns a new date_time
   procedure,private :: minus_date_time                   ! returns seconds
   generic           :: operator(-)  => minus_seconds
   generic           :: operator(-)  => minus_date_time
end type
!===================================================================================================================================
! User-defined constructors are created by defining a generic interface
! with the same name as the derived type they're supposed to construct.
interface date_time
   module procedure construct_from_dat
   !-!module procedure construct_from_jed
   !-!module procedure construct_from_uet
end interface date_time
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this function is used internally in the module, but is also declared to be a constructor for creating TYPE(DATE_TYPE) structures
!
function construct_from_dat(dat)
character(len=*),parameter::ident_35="@(#)M_time::construct_from_dat(3f): construct TYPE(DATE_TIME) with DAT date-time array"
integer,intent(in)          :: dat(:)                       ! (maybe partial) date time array
integer                     :: datlocal(8)                  ! date time array similar to that returned by DATE_AND_TIME
type(date_time)             :: construct_from_dat

   datlocal=u2d(0.0d0)                                      ! initialize to start of Unix Epoch Time using local time zone
   if(size(dat).gt.0)then                                   ! allow for partial DAT arrays
      datlocal(:size(dat))=dat
   endif
   construct_from_dat%year=datlocal(1)
   construct_from_dat%month=datlocal(2)
   construct_from_dat%day=datlocal(3)
   construct_from_dat%tz=datlocal(4)
   construct_from_dat%hour=datlocal(5)
   construct_from_dat%minute=datlocal(6)
   construct_from_dat%second=datlocal(7)
   construct_from_dat%millisecond=datlocal(8)
end function construct_from_dat
!===================================================================================================================================
function construct_from_jed(jed)
character(len=*),parameter::ident_36="&
&@(#)M_time::construct_from_jed(3f): construct TYPE(DATE_TIME) with REAL Julian JED date-time value"
real(kind=realtime),intent(in)   :: jed
type(date_time)                 :: construct_from_jed
   construct_from_jed=construct_from_dat(j2d(jed))
end function construct_from_jed
!===================================================================================================================================
function construct_from_uet(uet)
character(len=*),parameter::ident_37="&
&@(#)M_time::construct_from_uet(3f): construct TYPE(DATE_TIME) with INTEGER Unix UET date-time value"
integer,intent(in)   :: uet
type(date_time)                 :: construct_from_uet
   construct_from_uet=construct_from_dat(u2d(real(uet,kind=realtime)))
end function construct_from_uet
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! DEFINE THE METHODS FOR THE TYPE
! These functions are privately used to define the methods that TYPE(DATE_TIME) will support
!===================================================================================================================================
function dt2d(self) result (dat)
character(len=*),parameter::ident_38="@(#)M_time::dt2d(3f): convert derived type date_time to DAT date-time array"
class(date_time),intent(in) :: self
integer                     :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
   dat=[self%year, self%month, self%day, self%tz, self%hour, self%minute, self%second, self%millisecond]
end function dt2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function epoch(self) result (epoch_seconds)
character(len=*),parameter::ident_39="@(#)M_time::epoch(3f): convert derived type date_time to unix epoch seconds"
class(date_time),intent(in) :: self
real(kind=realtime)         :: epoch_seconds
   epoch_seconds=d2u(dt2d(self))
end function epoch
!===================================================================================================================================
function format(self,fmt) result (string)
character(len=*),parameter::ident_40="@(#)M_time::format(3f): convert derived type date_time to formatted string"
class(date_time),intent(in)           :: self
character(len=*),intent(in),optional  :: fmt
   character(len=:),allocatable          :: fmtlocal
   character(len=:),allocatable          :: string
   character(len=*),parameter            :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%z'
   character(len=*),parameter            :: usa_fmt='%W, %L %d, %Y %H:%m:%s %N'
   character(len=*),parameter            :: ymd_fmt='%Y-%M-%D %h:%m:%s.%x%z'
   character(len=*),parameter            :: mdy_fmt='%M/%D/%Y %h:%m:%s.%x%z'
   if(present(fmt))then
      fmtlocal=fmt
   else
      fmtlocal=iso_fmt
   endif
   if(index(fmtlocal,'%').eq.0)then       ! if a percent(%) in string assume it is a string to be passed to fmtdat
      select case(upper(fmtlocal))
      case("ISO","ISO-8601",""); fmtlocal=iso_fmt
      case("USA");               fmtlocal=usa_fmt
      case("YMD");               fmtlocal=ymd_fmt
      case("MDY");               fmtlocal=mdy_fmt
      case default
         !! usually used in a WRITE(3f) or PRINT(3f) so should not write output
         !write(*,*)'date_time%format: unknown format name'
         !write(*,*)'use predefined names ("iso","usa","ymd","mdy")'
         !write(*,*)'or a user-supplied format including the following macros:'
         !call fmtdate_usage()
         fmtlocal='date_time%format: unknown format name '//trim(fmtlocal)//'(not "iso","usa","ymd","mdy" or %macros)'//iso_fmt
      end select
   endif
   string=fmtdate(dt2d(self),fmtlocal)
end function format
!===================================================================================================================================
function julian(self) result (julian_days)
character(len=*),parameter::ident_41="@(#)M_time::julian(3f): convert derived type date_time to julian date"
class(date_time),intent(in) :: self
real(kind=realtime)         :: julian_days
    julian_days=d2j(dt2d(self))
end function julian
!===================================================================================================================================
function ordinal(self) result (ordinal_days)
character(len=*),parameter::ident_42="@(#)M_time::ordinal(3f): convert derived type date_time to ordinal date"
class(date_time),intent(in) :: self
integer                     :: ordinal_days
    ordinal_days=d2o(dt2d(self))
end function ordinal
!===================================================================================================================================
function weekday(self) result (iday)
character(len=*),parameter::ident_43="@(#)M_time::weekday(3f): convert derived type date_time to weekday (1=Monday,7=Sunday)"
class(date_time),intent(in)   :: self
integer                       :: iday
   integer                    :: ierr      ! Error return,0=correct,-1=invalid Julian Date,-2=neither day nor weekday specified
   call dow(dt2d(self),weekday=iday,ierr=ierr)
end function weekday
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function delta(self,year,month,day,tz,hour,minute,second,millisecond,week,duration)
!
! allow keyword addition and subtraction from each member of a date_time object
! even though there is no specific size for month and year, this is conventionally what is meant by "a year from now"
! or "a month from now". Once the arbitrary values are used to change the original date_time value convert it to
! Epoch time and back to make sure you get a valid date.
!
character(len=*),parameter::ident_44="@(#)M_time::delta(3f): add times to a type(date_time)"
class(date_time),intent(in)           :: self
integer,intent(in),optional           :: year, month, day, tz, hour, minute, second, millisecond, week
character(len=*),intent(in),optional  :: duration
type(date_time)                       :: delta

   delta=self
   if(present(year))then;        delta%year=self%year               + year        ;else; delta%year=self%year               ; endif
   if(present(month))then;       delta%month=self%month             + month       ;else; delta%month=self%month             ; endif
   if(present(day))then;         delta%day=self%day                 + day         ;else; delta%day=self%day                 ; endif
   if(present(tz))then;          delta%tz=self%tz                   + tz          ;else; delta%tz=self%tz                   ; endif
   if(present(hour))then;        delta%hour=self%hour               + hour        ;else; delta%hour=self%hour               ; endif
   if(present(minute))then;      delta%minute=self%minute           + minute      ;else; delta%minute=self%minute           ; endif
   if(present(second))then;      delta%second=self%second           + second      ;else; delta%second=self%second           ; endif
   if(present(millisecond))then; delta%millisecond=self%millisecond + millisecond ;else; delta%millisecond=self%millisecond ; endif

   if(present(week))then;        delta%day=delta%day                + week*7                  ; endif
   if(present(duration))then;    delta%second=delta%second          + int(days2sec(duration)) ; endif

   delta=construct_from_dat(u2d(d2u(dt2d(delta)))) ! to get a valid date after arbitrary math convert to Epoch and back

end function delta
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine init_dt(self,year,month,day,tz,hour,minute,second,millisecond,type,dat)
!
! allow for date_time object to be initialized. Default is current time
! If the 8-element dat array is present use it to initialize the date_time object
! If not, initialize to the current time or start of epoch depending on TYPE=["now"|"epoch"]
! Then, apply specific values, typically specified by keyword value
!
character(len=*),parameter::ident_45="@(#)M_time::init_dt(3f): initialize TYPE(DATE_TIME)"
class(date_time)                     :: self
type(date_time)                      :: holddt
integer,intent(in),optional          :: year, month, day, tz, hour, minute, second, millisecond
character(len=*),intent(in),optional :: type
integer,intent(in),optional          :: dat(8)
   character(len=10)                 :: typelocal
   integer                           :: datlocal(8)

   if(present(dat))then

      holddt=construct_from_dat(dat)

   else

      if(present(type))then
         typelocal=type
      else
         typelocal="now"
      endif

      select case(typelocal)
      case("now","NOW")
         call date_and_time(values=datlocal)             ! current time is placed in array using standard procedure
         holddt=construct_from_dat(datlocal)             ! convert date array to date_time type
      case("epoch","EPOCH")
         holddt=construct_from_dat([1970,1,1,0,0,0,0,0])
      case default
         call date_and_time(values=datlocal)             ! current time is placed in array using standard procedure
         holddt=construct_from_dat(datlocal)             ! convert date array to date_time type
      end select

   endif

   if(present(year))         holddt%year=year
   if(present(month))        holddt%month=month
   if(present(day))          holddt%day=day
   if(present(tz))           holddt%tz=tz
   if(present(hour))         holddt%hour=hour
   if(present(minute))       holddt%minute=minute
   if(present(second))       holddt%second=second
   if(present(millisecond))  holddt%millisecond=millisecond

   holddt=construct_from_dat(u2d(d2u(dt2d(holddt)))) ! to get a valid date after arbitrary values convert to Epoch and back

   self%year=holddt%year
   self%month=holddt%month
   self%day=holddt%day
   self%tz=holddt%tz
   self%hour=holddt%hour
   self%minute=holddt%minute
   self%second=holddt%second
   self%millisecond=holddt%millisecond

end subroutine init_dt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! FUNCTIONS FOR DEFINING OVERLOADED OPERATORS
!===================================================================================================================================
function plus_seconds(self,seconds) result (dattim)
character(len=*),parameter::ident_46="@(#)M_time::plus_seconds(3f): add derived type date_time object and seconds"
class(date_time),intent(in)    :: self
real(kind=realtime),intent(in) :: seconds
type(date_time)                :: dattim
   ! convert TYPE(DATE_TIME) to a DAT array for input into d2u (DAT to UNIX time) and add seconds
   ! take the seconds (ie. the Unix Epoch time) and convert it back into a DAT array and call the
   ! construct_from_dat() function to convert the DAT array back to a TYPE(DATE_TIME)
   dattim=construct_from_dat(u2d(d2u(dt2d(self))+seconds))
end function plus_seconds
!===================================================================================================================================
function minus_seconds(self,seconds) result (dattim)
character(len=*),parameter::ident_47="@(#)M_time::minus_seconds(3f): subtract seconds from derived type date_time object"
class(date_time),intent(in)    :: self
real(kind=realtime),intent(in) :: seconds
type(date_time)                :: dattim
   dattim=construct_from_dat(u2d(d2u(dt2d(self))-seconds))
end function minus_seconds
!===================================================================================================================================
function minus_date_time(self,other) result (seconds)
character(len=*),parameter::ident_48="@(#)M_time::minus_date_time(3f): add derived type date_time object and seconds"
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
real(kind=realtime)           :: seconds
   seconds= d2u(dt2d(self))- d2u(dt2d(other))
end function minus_date_time
!===================================================================================================================================
logical function eq(self,other)
character(len=*),parameter::ident_49="@(#)M_time::eq(3f): compare derived type date_time objects (eq,lt,gt,le,ge,ne)"
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   eq= int(d2u(dt2d(self))) .eq. int(d2u(dt2d(other)))
end function eq

logical function lt(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   lt= int(d2u(dt2d(self))) .lt. int(d2u(dt2d(other)))
end function lt

logical function gt(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   gt= int(d2u(dt2d(self))) .gt. int(d2u(dt2d(other)))
end function gt

logical function le(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   le= int(d2u(dt2d(self))) .le. int(d2u(dt2d(other)))
end function le

logical function ge(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   ge= int(d2u(dt2d(self))) .ge. int(d2u(dt2d(other)))
end function ge

logical function ne(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   ne= int(d2u(dt2d(self))) .ne. int(d2u(dt2d(other)))
end function ne
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_time_oop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
