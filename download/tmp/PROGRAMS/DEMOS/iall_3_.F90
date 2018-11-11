          program demo_iall
            integer(1) :: a(2)

            a(1) = b'00100100'
            a(2) = b'01101010'

            ! prints 00100000
            print '(b8.8)', iall(a)
          end program demo_iall
