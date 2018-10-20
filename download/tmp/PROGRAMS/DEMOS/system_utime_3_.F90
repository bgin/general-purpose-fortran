             program demo_system_utime
             use M_system, only : system_utime, system_perror
             use M_time, only   : d2u
             implicit none
             character(len=4096) :: pathname
             integer             :: times(2)
                do i=1,command_argument_count()
                   call get_command_argument(i, pathname)
                   if(.not.system_utime(pathname,times))then
                      call system_perror('*demo_system_utime*')
                   endif
                enddo
             end program demo_system_utime
