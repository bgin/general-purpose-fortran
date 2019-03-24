          program demo_verify
            write(*,*) verify("fortran", "ao")           ! 1, found 'f'
            write(*,*) verify("fortran", "foo")          ! 3, found 'r'
            write(*,*) verify("fortran", "c++")          ! 1, found 'f'
            write(*,*) verify("fortran", "c++", .true.)  ! 7, found 'n'
            write(*,*) verify("fortran", "fortran")      ! 0' found none
          end program demo_verify
