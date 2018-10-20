            program demo_ephemeris
            use M_time, only : ephemeris, fmtdate
            implicit none
            integer            :: itime(8)
            integer            :: planet
            integer            :: declination_d, declination_m
            character(len=1)   :: declination_compass
            integer            :: ascent_hours, ascent_minutes

            planet=4
            itime=[1982,3,10,0,6,0,0,0]
            call ephemeris(itime, planet,                    &
            declination_d,declination_m,declination_compass, &
            ascent_hours,ascent_minutes)

            write(*, '(" For: ",a)')fmtdate(itime)
            write(*, "(' Planet: ',I1,1X)",advance='no')                       &
                    planet
            write(*, "(' Ascent: ',I2,' H ',I2,' MN',1X)",advance='no')        &
                    ascent_hours, ascent_minutes
            write(*, "(' Declination: ',I2,' D ',I2,' MN ',A1)",advance='yes') &
                    declination_d, declination_m, declination_compass

            end program demo_ephemeris
