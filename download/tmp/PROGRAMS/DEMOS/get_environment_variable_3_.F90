          program demo_getenv
            character(len=:),allocatable :: var
            character(len=:),allocatable :: homedir
            integer :: howbig
            var='HOME'
            ! get length required to hold value
            call get_environment_variable(var, length=howbig)
            ! make string to hold value of sufficient size
            allocate(character(len=howbig) :: homedir)
            ! get value
            call get_environment_variable(var, homedir)
            ! print environment variable name value
            write (*,'(a,"=""",a,"""")')var,trim(homedir)
          end program demo_getenv
