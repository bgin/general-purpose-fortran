          program demo_read_line
          use M_io, only : read_line
          implicit none
          character(len=:),allocatable :: line
             INFINITE: do while (read_line(line)==0)
                write(*,'(a)')'['//line//']'
             enddo INFINITE
          end program demo_read_line
