           program demo_M_time
              call procedural()
              call object_oriented()
           !===============================================================================
           contains
           !===============================================================================
           subroutine procedural()
           use M_time, only:  j2d, d2j, u2d, d2u, fmtdate, realtime
           integer                      :: dat(8)
           real(kind=realtime)          :: julian, unixtime
           character(len=*),parameter   :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%z'
           character(len=:),allocatable :: friendly

              friendly='%W, %L %d, %Y %H:%m:%s %N' ! a nice friendly format

              call date_and_time(values=dat)  ! current time is placed in array

              write(*,*)'Today'
              write(*,*)'ISO       ',fmtdate(dat,iso_fmt)
              write(*,*)'Friendly  ',fmtdate(dat,friendly)
              write(*,*)'ISO week  ',fmtdate(dat,'%I')

              julian=d2j(dat)
              unixtime=d2u(dat)

              write(*,*)'Yesterday' ! subtract a day from scalar time and print
              write(*,*)'          ',fmtdate(u2d(unixtime-86400),iso_fmt)
              write(*,*)'          ',fmtdate(j2d(julian-1.0),friendly)
              write(*,*)'          ',fmtdate(j2d(julian-1.0),'%I')

              write(*,*)'Tomorrow'  ! add a day to scalar time and print
              write(*,*)'          ',fmtdate(u2d(unixtime+86400),iso_fmt)
              write(*,*)'          ',fmtdate(j2d(julian+1.0),friendly)
              write(*,*)'          ',fmtdate(j2d(julian+1.0),'%I')

              write(*,*)'Next Week'  ! add a week to scalar time and print
              write(*,*)'          ',fmtdate(u2d(unixtime+7*86400),iso_fmt)
              write(*,*)'          ',fmtdate(j2d(julian+7.0),friendly)
              write(*,*)'          ',fmtdate(j2d(julian+7.0),'%I')

           end subroutine procedural
           !=========================================================================================
           subroutine object_oriented()
           !
           ! This is an example using the object-oriented class/type model
           ! This is essentially the same functionality as the procedures
           ! described above, but if you prefer this type of syntax this may
           ! seem more intuitive ...
           !
           use M_time_oop,only : date_time
           !!use M_time_oop,only : operator(+),operator(-),operator(>),operator(<)
           !!use M_time_oop,only : operator(<=),operator(>=),operator(==),operator(/=)
           implicit none
           integer         :: dat(8)
           TYPE(date_time) :: event
           TYPE(date_time) :: otherdate
           TYPE(date_time) :: answer

           character(len=*),parameter   :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%z'
              ! DIFFERENT INITIALIZATION STYLES (Still debating on how best to do this)
              write(*,*)
              write(*,*)'Various initialization styes'

              ! DEFINE TYPE(DATE_TIME) WITH CONSTRUCTOR
              otherdate=date_time()
              print *,'DEFAULT CONSTRUCTOR %FORMAT()               ',otherdate%format()
              print *,'DEFAULT CONSTRUCTOR %FORMAT("")             ',otherdate%format("")
              print *,'DEFAULT CONSTRUCTOR %FORMAT(user-specified) ',otherdate%format(iso_fmt)
              print *,'DEFAULT CONSTRUCTOR %FORMAT("USA")          ',otherdate%format("USA")

              otherdate=date_time(1492,10,12,0,0,0,0,0)
              print *,'DEFAULT CONSTRUCTOR SETTING VALUES          ',otherdate%format()

              otherdate=date_time(2016,6,11)
              print *,'DEFAULT CONSTRUCTOR WITH PARTIAL VALUES     ',otherdate%format()

              otherdate=date_time(year=2016,month=6,day=11,tz=-240,hour=21,minute=09,second=11,millisecond=500)
              print *,'DEFAULT CONSTRUCTOR WITH VALUES BY NAME     ',otherdate%format()

              otherdate=date_time([1776,7,4,0,0,0,0,0])
              print *,'CONSTRUCTOR WITH A DAT ARRAY                ',otherdate%format()

              otherdate=date_time([1776,7,4])
              print *,'CONSTRUCTOR WITH A PARTIAL DAT ARRAY        ',otherdate%format()

              ! the init() method supports several methods
              call otherdate%init()                           ! initialize to current time using INIT
              call otherdate%init(type="now")                 ! initialize to current time using INIT

              call otherdate%init(type="epoch")               ! initialize to beginning of Unix Epoch Time
              ! Note
              ! currently, DATE_TIME DATE array is set to Unix Epoch start USING LOCAL TIMEZONE
              ! whereas default constructor is using default of Unix Epoch start using Z time (GMT or UTC time)

              ! initialize with a DAT array using INIT, compatible with DATE_AND_TIME VALUES(8)
              call otherdate%init(dat=[1970,1,1,0,0,0,0,0])
              call otherdate%init(2016,6,11,-300,23,1,0,0)    ! using INIT with ordered values
              ! using INIT with names
              call otherdate%init(year=2016,month=6,day=11,tz=-300,hour=23,minute=1,second=0,millisecond=0)
              !============================================================================
              ! take current date and exercise the OOP interface
              call event%init()                                           ! initialize to current time using INIT
              write(*,*)
              write(*,*)'Print members of type(DATE_TIME)'
              write(*,404)'EVENT=',event                                  ! show derived type
              404 format(a,i0,*(",",i0:))

              ! MEMBERS ( basic time values are all integers)
              write(*,101)'%year        Year................... ',event%year           ! print members of type
              write(*,101)'%month       Month.................. ',event%month
              write(*,101)'%day         Day.................... ',event%day
              write(*,101)'%tz          Timezone............... ',event%tz
              write(*,101)'%hour        Hour................... ',event%hour
              write(*,101)'%minute      Minute................. ',event%minute
              write(*,101)'%second      Second................. ',event%second
              write(*,101)'%millisecond Millisecond............ ',event%millisecond

              ! PRINT METHODS OF TYPE
              write(*,*)'Print methods of type(DATE_TIME)'
              write(*,101)'%ordinal     Ordinal day of year.... ',  event%ordinal()
              write(*,101)'%weekday     Weekday................ ',  event%weekday()
              101 format(1x,a,i0)
              ! DOUBLE PRECISION VALUES EASILY MANIPULATED MATHEMATICALLY
              write(*,202)'%epoch      Unix epoch time........ ',  event%epoch()
              write(*,202)'%julian     Julian date............ ',  event%julian()
              202 format(1x,a,g0)

              ! FORMATTED STRINGS (many strings possible. Takes the same format string as fmtdate(3f))
              write(*,*)
              write(*,*)'Formatted Strings (%format("STRING") -- see fmtdate(3f) for format descriptions'
              write(*,303)'Short month............ ',event%format("%l")  ! abbreviated month name             %l  Dec
              write(*,303)'Month.................. ',event%format("%L")  ! full month name                    %L  December
              write(*,303)'Short week............. ',event%format("%w")  ! first three characters of weekday  %w  Sat
              write(*,303)'Week .................. ',event%format("%W")  ! weekday name                       %W  Saturday
              ! with no percent (%) characters
              write(*,303)'Calendar Time ......... ',event%format("Y-M-D h:m:s.x z")
              ! keywords with no percent (%) characters
              write(*,303)'Calendar Time ......... ',event%format('"year-month-day hour:minute:second.millisecond timezone"')
              write(*,*)event%format('Longer format.......... "%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
              303 format(1x,a,'"',a,'"')

              dat=event%datout()            ! convert date_time to integer array (maybe to use with module M_TIME base procedures)
              write(*,*)
              write(*,404)'DAT=',dat

              ! OVERLOADED OPERATORS (add and subtract)
              answer=event+1*86400.0d0   ! a date_time object can have seconds added
              write(*,*)answer%format('TOMORROW="%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
              answer=event-1*86400.0d0   ! a date_time object can have seconds subtracted
              write(*,*)answer%format('YESTERDAY=="%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
              ! if both operands are DATE_TIME objects a subtraction finds the time in seconds between the two dates
              write(*,*)'DIFFERENCE (subtracting one date_time from another)=',answer-event

              ! OVERLOADED OPERATORS (logical comparisons)
              ! NOTE COMPARISONS ARE PERFORMED BY CONVERTING TIMES TO INTEGER SECONDS
              write(*,*)event.eq.event   ,event.lt.event   ,event.gt.event   ,event.le.event   ,event.ge.event   ,event.ne.event
              write(*,*)event.eq.answer  ,event.lt.answer  ,event.gt.answer  ,event.le.answer  ,event.ge.answer  ,event.ne.answer
              write(*,*)answer.eq.event  ,answer.lt.event  ,answer.gt.event  ,answer.le.event  ,answer.ge.event  ,answer.ne.event

              ! %DELTA easily lets you change dates by common increments
              write(*,*)
              write(*,404)'%DELTA tests starting with date ',event%delta()
              write(*,*) event%format("                             %W, %L %d, %Y %H:%m:%s %N")

              write(*,*)'Remember years and months are not constant units'

              answer=event%delta(year=1)
              write(*,*)answer%format("FOR %%DELTA(YEAR=+1)            %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(year=-1)
              write(*,*)answer%format("FOR %%DELTA(YEAR=-1)            %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(month=24)
              write(*,*)answer%format("FOR %%DELTA(MONTH=+24)          %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(month=-24)
              write(*,*)answer%format("FOR %%DELTA(MONTH=-24)          %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(week=1)
              write(*,*)answer%format("FOR %%DELTA(WEEK=+1)            %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(week=-1)
              write(*,*)answer%format("FOR %%DELTA(WEEK=-1)            %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(day=1)
              write(*,*)answer%format("FOR %%DELTA(DAY=+1)             %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(day=-1)
              write(*,*)answer%format("FOR %%DELTA(DAY=-1)             %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(hour=4)
              write(*,*)answer%format("FOR %%DELTA(HOUR=+4)            %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(hour=-4)
              write(*,*)answer%format("FOR %%DELTA(HOUR=-4)            %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(minute=180)
              write(*,*)answer%format("FOR %%DELTA(MINUTE=+180)        %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(minute=-180)
              write(*,*)answer%format("FOR %%DELTA(MINUTE=-180)        %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(second=1800)
              write(*,*)answer%format("FOR %%DELTA(SECOND=+1800)       %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(second=-1800)
              write(*,*)answer%format("FOR %%DELTA(SECOND=-1800)       %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(millisecond=10000)
              write(*,*)answer%format("FOR %%DELTA(MILLISECOND=+10000) %W, %L %d, %Y %H:%m:%s %N")
              answer=event%delta(millisecond=-10000)
              write(*,*)answer%format("FOR %%DELTA(MILLISECOND=-10000) %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(year=3,month=2,day=100,hour=200,week=-1,minute=300,second=1000,millisecond=-10000)
              write(*,*)answer%format(&
              &"FOR %%DELTA(year=3,month=2,day=100,hour=200,&
              &week=-1,minute=300,second=1000,millisecond=100000)  %W, %L %d, %Y %H:%m:%s %N")

              answer=event%delta(duration="1-20:30:40.50")
              write(*,*)answer%format("FOR %%DELTA(DURATION='1-20:30:40.50')      %W, %L %d, %Y %H:%m:%s %N")

           end subroutine object_oriented
           end program demo_M_time
