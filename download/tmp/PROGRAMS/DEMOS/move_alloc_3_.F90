          program demo_move_alloc
              integer, allocatable :: a(:), b(:)

              allocate(a(3))
              a = [ 1, 2, 3 ]
              call move_alloc(a, b)
              print *, allocated(a), allocated(b)
              print *, b
          end program demo_move_alloc
