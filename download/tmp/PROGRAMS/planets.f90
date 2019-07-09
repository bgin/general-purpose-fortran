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
'                                                                                ',&
'   planets(1f) - [FUNIX] ephemeris position of planets for adjusting an equatorial telescope',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'                                                                                ',&
'   planets yyyy mm dd utc hh mm ss [-planet [N|name] ]                          ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   planets(1) calculates the ephemeris of a planet in our solar system          ',&
'   in order to  adjust an equatorial telescope. See ephemeris(3f) for           ',&
'   more details. The outputs are                                                ',&
'                                                                                ',&
'     o Ascent in hours (0 to 24) and minutes (0 to 60)                          ',&
'     o Declination in degrees and minutes (-90 to 90 North or South)            ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'                                                                                ',&
'     date    provide the same eight values used by the DATE_AND_TIME(3f)        ',&
'             intrinsic.                                                         ',&
'                                                                                ',&
'             value(1)  The year                                                 ',&
'             value(2)  The month                                                ',&
'             value(3)  The day of the month                                     ',&
'             value(4)  Time difference with UTC in minutes                      ',&
'             value(5)  The hour of the day                                      ',&
'             value(6)  The minutes of the hour                                  ',&
'             value(7)  The seconds of the minute                                ',&
'             value(8)  The milliseconds of the second                           ',&
'                                                                                ',&
'     N|Name  Planet numbers in range 1 to 8 (Mercury:1 Venus:2 Mars:4           ',&
'             Jupiter:5 Saturn:6 Uranus:7 Neptune:8). If not specified           ',&
'             the default is "1 2 4 5 6 7 8".                                    ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'   (Find ascent and declination of planet Mars on March 10th, 1982 at 6h UT)    ',&
'                                                                                ',&
'    planets 1982 03 10 00 06 00 00 00  -planet  4                               ',&
'                                                                                ',&
'     Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00                           ',&
'     Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S                   ',&
'                                                                                ',&
'    no planet number(s) specified:                                              ',&
'                                                                                ',&
'    planets 1982 03 10 00 06 00 00 00                                           ',&
'                                                                                ',&
'     Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00                           ',&
'     Planet: 1  Ascent: 21 H 51 MN  Declination: 14 D 45 MN S                   ',&
'     Planet: 2  Ascent: 20 H 26 MN  Declination: 14 D 57 MN S                   ',&
'     Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S                   ',&
'     Planet: 5  Ascent: 14 H 32 MN  Declination: 13 D 30 MN S                   ',&
'     Planet: 6  Ascent: 13 H 22 MN  Declination:  5 D 42 MN S                   ',&
'     Planet: 7  Ascent: 16 H 11 MN  Declination: 20 D 54 MN S                   ',&
'     Planet: 8  Ascent: 17 H 46 MN  Declination: 22 D  7 MN S                   ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!
!!    planets(1f) - [FUNIX] ephemeris position of planets for adjusting an equatorial telescop
!!
!!##SYNOPSIS
!!
!!
!!    planets yyyy mm dd utc hh mm ss [-planet [N|name] ]
!!
!!##DESCRIPTION
!!
!!    planets(1) calculates the ephemeris of a planet in our solar system
!!    in order to  adjust an equatorial telescope. See ephemeris(3f) for
!!    more details. The outputs are
!!
!!      o Ascent in hours (0 to 24) and minutes (0 to 60)
!!      o Declination in degrees and minutes (-90 to 90 North or South)
!!
!!##OPTIONS
!!
!!      date    provide the same eight values used by the DATE_AND_TIME(3f)
!!              intrinsic.
!!
!!              value(1)  The year
!!              value(2)  The month
!!              value(3)  The day of the month
!!              value(4)  Time difference with UTC in minutes
!!              value(5)  The hour of the day
!!              value(6)  The minutes of the hour
!!              value(7)  The seconds of the minute
!!              value(8)  The milliseconds of the second
!!
!!      N|Name  Planet numbers in range 1 to 8 (Mercury:1 Venus:2 Mars:4
!!              Jupiter:5 Saturn:6 Uranus:7 Neptune:8). If not specified
!!              the default is "1 2 4 5 6 7 8".
!!
!!##EXAMPLE
!!
!!    (Find ascent and declination of planet Mars on March 10th, 1982 at 6h UT)
!!
!!     planets 1982 03 10 00 06 00 00 00  -planet  4
!!
!!      Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
!!      Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S
!!
!!     no planet number(s) specified:
!!
!!     planets 1982 03 10 00 06 00 00 00
!!
!!      Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
!!      Planet: 1  Ascent: 21 H 51 MN  Declination: 14 D 45 MN S
!!      Planet: 2  Ascent: 20 H 26 MN  Declination: 14 D 57 MN S
!!      Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S
!!      Planet: 5  Ascent: 14 H 32 MN  Declination: 13 D 30 MN S
!!      Planet: 6  Ascent: 13 H 22 MN  Declination:  5 D 42 MN S
!!      Planet: 7  Ascent: 16 H 11 MN  Declination: 20 D 54 MN S
!!      Planet: 8  Ascent: 17 H 46 MN  Declination: 22 D  7 MN S
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
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        planets(1)>',&
'@(#)DESCRIPTION:    ephemeris position of planets for adjusting an equitorial telescope>',&
'@(#)VERSION:        1.0, 20170910>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Wed, Jul 3rd, 2019 11:43:29 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program planets
use M_kracken, only : kracken, lget, sget, iget, igets
use M_time, only    : guessdate, fmtdate, ephemeris
implicit none
integer,parameter            :: dp=kind(0.0d0)
integer                      :: declination_d,declination_m
integer                      :: ascent_hours, ascent_minutes
character(len=1)             :: declination_compass
integer                      :: itime(8)
integer,allocatable          :: gettime(:)
integer,allocatable          :: planet_numbers(:)
character(len=:),allocatable :: datetime
integer                      :: i
!-----------------------------------------------------------------------------------------------------------------------------------
call kracken('planets',' -date -planet -version .F. -help .F.')
call help_usage(lget('planets_help'))                    ! if -help option is present, display help text and exit
call help_version(lget('planets_version'))               ! if -version option is present, display version text and exit

call date_and_time(values=itime)
datetime=sget('planets_date')
if(datetime.ne.'')then
   call guessdate(datetime,itime)
else
   gettime=igets('planets_oo')
   itime(:size(gettime))=gettime
endif
write(*,'(1x,"For: ",a)')fmtdate(itime)
!-----------------------------------------------------------------------------------------------------------------------------------
planet_numbers=igets('planets_planet')     ! planet number : Mercury:1 Venus:2 Mars:4 Jupiter:5 Saturn:6 Uranus:7 Neptune:8
if ( size(planet_numbers).eq.0)then
   planet_numbers=[1,2,4,5,6,7,8]
endif
!-----------------------------------------------------------------------------------------------------------------------------------
do i=1,size(planet_numbers)
   call ephemeris(itime,planet_numbers(i),declination_d,declination_m,declination_compass,ascent_hours,ascent_minutes)
   write(*, "(' Planet: ',I1,1X)",advance='no')                       planet_numbers(i)
   write(*, "(' Ascent: ',I2,' H ',I2,' MN',1X)",advance='no')        ascent_hours, ascent_minutes
   write(*, "(' Declination: ',I2,' D ',I2,' MN ',A1)",advance='yes') declination_d, declination_m, declination_compass
enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end program planets
