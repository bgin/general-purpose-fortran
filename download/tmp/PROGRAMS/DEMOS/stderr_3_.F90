          program demo_stderr
          use M_debug, only: stderr
          implicit none

          call stderr('error: RVALUE=',3.0/4.0)
          call stderr('error: IVALUE=',123456789)
          call stderr('error: LVALUE=',.true.)

          call stderr('error: program will now stop')
          stop 1

          end program demo_stderr
