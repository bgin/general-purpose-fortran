          program demo_system_getpwuid
          use M_system, only : system_pwuid
          use M_system, only: system_getuid
          implicit none
          character(len=:),allocatable :: name
             name=system_getpwuid(system_getuid))
             write(*,'("login[",a,"] for ",i0)')name,uid
          end program demo_system_getpwuid
