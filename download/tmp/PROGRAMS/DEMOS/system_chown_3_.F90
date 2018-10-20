          program demo_system_chown
          Use M_system, only : system_chown
          implicit none
          integer                     :: i
          character(len=80),parameter :: names(*)=[ 'myfile1','/usr/local']
          do i=1,size(names)
             ierr=chown(names(i))
             write(*,*)' for ',trim(names(i)),' ownership is ', system_chown(names(i))
          enddo
          end program demo_system_chown
