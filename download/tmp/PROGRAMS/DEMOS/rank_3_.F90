          program demo_rank
            integer :: a
            real, allocatable :: b(:,:)

            print *, rank(a), rank(b) ! Prints:  0  2
          end program demo_rank
