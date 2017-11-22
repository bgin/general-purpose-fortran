subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   _sleep - [TIME] pause for specified duration                                 ',&
'SYNOPSIS                                                                        ',&
'   _sleep [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d]] --version|--help                 ',&
'DESCRIPTION                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh            ',&
'   hours, mm minutes and ss.xxx seconds pause for the specified amount          ',&
'   of time.                                                                     ',&
'                                                                                ',&
'   Alternatively, the time may be specified by a number of seconds              ',&
'   immediately followed by a unit letter, where s is seconds, m is              ',&
'   minutes, h is hours and d is days.                                           ',&
'                                                                                ',&
'   If the suffix r is used, a random time between zero and the specified        ',&
'   number of seconds is used. This is useful for spreading out cron(1)          ',&
'   tasks in a HPC cluster.                                                      ',&
'                                                                                ',&
'   Given multiple arguments, pause for the time specified by the sum of         ',&
'   the values.                                                                  ',&
'OPTIONS                                                                         ',&
'   dd-hh:mm:ss  Given a string representing a duration of time in the           ',&
'                following forms:                                                ',&
'                                                                                ',&
'                  dd-hh:mm:ss[.xx]                                              ',&
'                     hh:mm:ss[.xx]                                              ',&
'                        mm:ss[.xx]                                              ',&
'                           ss[.xx]                                              ',&
'      or                                                                        ',&
'   xx[.yy]SUFFIX  where Suffix may be s for seconds, m for minutes, h for hours,',&
'                  or d for days.                                                ',&
'   --help         display this help and exit                                    ',&
'   --version      output version information and exit                           ',&
'EXAMPLE                                                                         ',&
'  usage:                                                                        ',&
'                                                                                ',&
'   _sleep 0.10     # pause one tenth of a second                                ',&
'   _sleep 3m 10s   # pause three minutes and 10 seconds                         ',&
'   _sleep 1:00:00  # pause for one hour                                         ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    _sleep - [TIME] pause for specified duration
!!##SYNOPSIS
!!
!!    _sleep [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d]] --version|--help
!!##DESCRIPTION
!!    Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh
!!    hours, mm minutes and ss.xxx seconds pause for the specified amount
!!    of time.
!!
!!    Alternatively, the time may be specified by a number of seconds
!!    immediately followed by a unit letter, where s is seconds, m is
!!    minutes, h is hours and d is days.
!!
!!    If the suffix r is used, a random time between zero and the specified
!!    number of seconds is used. This is useful for spreading out cron(1)
!!    tasks in a HPC cluster.
!!
!!    Given multiple arguments, pause for the time specified by the sum of
!!    the values.
!!##OPTIONS
!!    dd-hh:mm:ss  Given a string representing a duration of time in the
!!                 following forms:
!!
!!                   dd-hh:mm:ss[.xx]
!!                      hh:mm:ss[.xx]
!!                         mm:ss[.xx]
!!                            ss[.xx]
!!       or
!!    xx[.yy]SUFFIX  where Suffix may be s for seconds, m for minutes, h for hours,
!!                   or d for days.
!!    --help         display this help and exit
!!    --version      output version information and exit
!!##EXAMPLE
!!
!!   usage:
!!
!!    _sleep 0.10     # pause one tenth of a second
!!    _sleep 3m 10s   # pause three minutes and 10 seconds
!!    _sleep 1:00:00  # pause for one hour
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        _sleep(1f)>',&
'@(#)DESCRIPTION:    given string of form days-hh:mm:ss pause for specified amount of time>',&
'@(#)VERSION:        1.0, 20170822>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)COMPILED:       Sat, Oct 21st, 2017 8:55:27 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_sleep
use M_kracken, only: kracken, sgets, lget
use M_time, only :   days2sec, realtime, system_sleep
use M_strings, only: substitute
implicit none
real(kind=realtime)           :: delay_value
character(len=80),allocatable :: delays(:)
integer                       :: i
integer, allocatable          :: seed(:)
integer                       :: n
real                          :: chance
integer                       :: values(1:8)
   call kracken('_sleep',' -oo 0.0 -help .F. -version .F.')    ! parse command line
   call help_usage(lget('_sleep_help'))                        ! display help information and stop if true
   call help_version(lget('_sleep_version'))                   ! display version information and stop if true
!===================================================================================================================================
   ! determine value of cyclical pause duration
   delays=sgets('_sleep_oo')
   do i=1,size(delays)
      if(index(delays(i),'r').ne.0)then                                  ! random number seconds between zero and value specified
         call substitute(delays(i),'r','s')                              ! change 'r' suffix to 's'

         call date_and_time(values=values)                               ! jump through hoops to get a random number from 0 to 1
         call random_seed(size=n)
         allocate(seed(1:n))
         seed(:) = values(8)
         call random_seed(put=seed)
         call random_number(chance)

         delay_value=max( 0.0d0, days2sec(delays(i)) )                   ! get value converted to seconds
         delay_value=delay_value*chance                                  ! randomize the number
      else                                                               ! simply convert to seconds
         delay_value=max( 0.0d0, days2sec(delays(i)) )
      endif
      call system_sleep(delay_value)
   enddo
!===================================================================================================================================
end program demo_system_sleep
