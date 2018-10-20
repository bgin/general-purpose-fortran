          program demo_matchw
          use M_strings, only : matchw

          ! first match is not all of string so F
          write(*,*)matchw('*c*ax ','abcdefgaxaxaxax')
          ! true
          write(*,*)matchw('*c*ax*','abcdefgaxaxaxax')

          write(*,*)merge('MATCH','ERROR',matchw('abcdefgaxaxaxax','*c*ax*'))
          write(*,*)merge('MATCH','ERROR',matchw('abcdefgaxaxaxax','*c??f*'))
          write(*,*)merge('ERROR','NO   ',matchw('abcdefgaxaxaxax','*a??f'))
          write(*,*)merge('ERROR','NO   ',matchw('abcdefgaxaxaxax','*y'))

          end program demo_matchw
