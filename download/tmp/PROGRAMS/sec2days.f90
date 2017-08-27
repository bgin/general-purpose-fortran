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
'   sec2days - [TIME] Convert seconds to string of form dd-hh:mm:ss              ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   sec2days nnnn[.xxx] [-crop]| --version| --help                               ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a numeric string representing seconds convert it to a string           ',&
'   of the form                                                                  ',&
'                                                                                ',&
'      dd-hh:mm:ss                                                               ',&
'                                                                                ',&
'   where dd is days, hh hours, mm minutes and ss seconds.                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   nnnn[.xxx]  number of seconds to convert to string of form dd-hh:mm:ss.      ',&
'               nnnn may be interspersed with unit codes d,h,m,s. Spaces,        ',&
'               commas and case are ignored. Allowed aliases for the unit        ',&
'               codes are                                                        ',&
'                 d  days and day                                                ',&
'                 h  hours,hour,hrs, and hr                                      ',&
'                 m  minutes,minute and min                                      ',&
'                 s  seconds,second and sec                                      ',&
'                                                                                ',&
'   -crop       trim leading zero values from output                             ',&
'   --help      display this help and exit                                       ',&
'   --version   output version information and exit                              ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
' usage                                                                          ',&
'                                                                                ',&
'   sec2days 129860                                                              ',&
'   1-12:04:20                                                                   ',&
'   sec2days 1d2h3m4s                                                            ',&
'   1-02:03:04                                                                   ',&
'   sec2days 1.0 days 2 hours 3 minutes 4 seconds                                ',&
'   1-02:03:04                                                                   ',&
'   sec2days 1.5d                                                                ',&
'   1-12:00:00                                                                   ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    sec2days - [TIME] Convert seconds to string of form dd-hh:mm:ss
!!
!!##SYNOPSIS
!!
!!    sec2days nnnn[.xxx] [-crop]| --version| --help
!!
!!##DESCRIPTION
!!    Given a numeric string representing seconds convert it to a string
!!    of the form
!!
!!       dd-hh:mm:ss
!!
!!    where dd is days, hh hours, mm minutes and ss seconds.
!!
!!##OPTIONS
!!    nnnn[.xxx]  number of seconds to convert to string of form dd-hh:mm:ss.
!!                nnnn may be interspersed with unit codes d,h,m,s. Spaces,
!!                commas and case are ignored. Allowed aliases for the unit
!!                codes are
!!                  d  days and day
!!                  h  hours,hour,hrs, and hr
!!                  m  minutes,minute and min
!!                  s  seconds,second and sec
!!
!!    -crop       trim leading zero values from output
!!    --help      display this help and exit
!!    --version   output version information and exit
!!
!!##EXAMPLE
!!
!!  usage
!!
!!    sec2days 129860
!!    1-12:04:20
!!    sec2days 1d2h3m4s
!!    1-02:03:04
!!    sec2days 1.0 days 2 hours 3 minutes 4 seconds
!!    1-02:03:04
!!    sec2days 1.5d
!!    1-12:00:00
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
'@(#)PROGRAM:        sec2days(1f)>',&
'@(#)DESCRIPTION:    convert seconds to string of form dd-hh:mm:ss>',&
'@(#)VERSION:        1.0, 2016-06-17>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Sat, Aug 26th, 2017 2:11:09 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_sec2days
use M_kracken, only: kracken, lget, sget
use M_time, only :   sec2days, realtime
implicit none
character(len=*),parameter :: ident="@(#) given string of form days-hh:mm:ss or IId JJh KKm LLs convert to seconds'"
real(kind=realtime), parameter :: units_hl(4)=[ 86400.0d0, 3600.0d0, 60.0d0, 1.0d0 ]
character(len=:),allocatable   :: strlocal
   call kracken('sec2days',' -oo -crop .F -help .F. -version .F.')    ! parse command line
   call help_usage(lget('sec2days_help'))                             ! display help information and stop if true
   call help_version(lget('sec2days_version'))                        ! display version information and stop if true
   strlocal=sec2days(trim(sget('sec2days_oo')),lget('sec2days_crop')) ! get command line option and convert to dd-hh:mm:ss string
   write(*,'(a)')strlocal
end program demo_sec2days
