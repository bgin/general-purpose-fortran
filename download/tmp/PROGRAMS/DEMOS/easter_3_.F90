          program demo_easter
          use m_time, only : easter, fmtdate
          implicit none
          integer :: year, month, day
          integer :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
            call date_and_time(values=dat)  ! get current year
            year=dat(1)
            call easter(year, month, day)
            ! fill out a date_and_time array
            dat=[dat(1),month,day,dat(4),12,0,0,0]
            write(*,*)fmtdate(dat,&
            "Easter day: the %d day of %L in the year of our Lord %Y")
          end program demo_easter
