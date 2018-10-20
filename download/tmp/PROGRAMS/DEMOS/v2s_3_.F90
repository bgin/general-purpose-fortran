          program demo_v2s
          use M_strings, only: v2s
          write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
          write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
          write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
          end program demo_v2s
