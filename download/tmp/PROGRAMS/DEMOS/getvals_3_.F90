             program demo_getvals
             use M_strings, only: getvals
             implicit none
             character(len=256) :: line
             real               :: values(256/2+1)
             integer            :: ios,icount,ierr
             INFINITE: do
                read(*,'(a)',iostat=ios) line
                if(ios.ne.0)exit INFINITE
                call getvals(line,values,icount,ierr)
                write(*,*)'VALUES=',values(:icount)
             enddo INFINITE
             end program demo_getvals
