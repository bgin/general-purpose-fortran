program test_ephemeris
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_time, only : ephemeris, fmtdate
implicit none
     integer,parameter  :: itime(8)=[1982,3,10,0,6,0,0,0] ! For: Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
     integer            :: planet
     integer            :: D_d, D_m, A_h, A_m
     character(len=1)   :: D_c

   call unit_check_start('ephemeris',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_time.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')

     do planet=1,8
        if(planet.eq.3)cycle
        call ephemeris(itime, planet, D_d,D_m,D_c, A_h,A_m)
        call unit_check('ephemeris',D_c.eq.'S','Compass direction')
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

        call unit_check_good('ephemeris')

     enddo

end program test_ephemeris
