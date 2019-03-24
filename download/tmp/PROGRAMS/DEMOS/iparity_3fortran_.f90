          program demo_iparity
            integer, dimension(2) :: a

            a(1) = b'00100100'
            a(2) = b'01101010'

            ! prints 01001110
            print '(b8.8)', iparity(a)
          end program demo_iparity
