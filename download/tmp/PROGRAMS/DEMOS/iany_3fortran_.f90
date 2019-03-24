          program demo_iany
            integer(1) :: a(2)

            a(1) = b'00100100'
            a(2) = b'01101010'

            ! prints 01101110
            print '(b8.8)', iany(a)
          end program demo_iany
