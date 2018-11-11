           program demo_matchw
           call demo1()
           call demo2()
           contains
           !!
           ! basic example
           !!
           subroutine demo1()
           use M_strings, only : matchw
           ! first match is not all of string so F
           write(*,*)matchw('*c*ax ','abcdefgaxaxaxax')
           ! true
           write(*,*)matchw('*c*ax*','abcdefgaxaxaxax')
           !
           write(*,*)merge('MATCH','ERROR',matchw('abcdefgaxaxaxax','*c*ax*'))
           write(*,*)merge('MATCH','ERROR',matchw('abcdefgaxaxaxax','*c??f*'))
           write(*,*)merge('ERROR','NO   ',matchw('abcdefgaxaxaxax','*a??f'))
           write(*,*)merge('ERROR','NO   ',matchw('abcdefgaxaxaxax','*y'))
           end subroutine demo1
           !!
           ! More extensive example
           !!
           subroutine demo2()
           use M_strings, only : matchw
           !implicit none
           integer np, ns
           parameter (np =  19, ns =  6)
           character pattern(np)*8, string(ns)*12
           character pattern2(np)*8
           integer s, p
           data pattern /'*','a*a','a*','ab*','*a','a*a','a?d?','a?d*','abra',&
           & 'aa','a','ab','*','?','????','?*','*?','***?','****?'/
           data pattern2/'*','a**a','a*d?','ab*','*a','a*a','a?d?','a?d*','alda',&
           & 'aa','a','ab','*','?','???a','????','**','***a','?????'/
           data string / 'abracadabra', 'aldabra', 'alda', 'carta', 'abdc', 'abra'/
              !
              write(*,'("TABLE 1",t18, *(a6))') pattern
              do s = 1,ns
                 write(*, '(a, 100L6)') &
                  & string(s),(matchw(string(s),pattern(p)), p=1,np)
              enddo
              !
              write(*,'("TABLE 2",t18, *(a6))') pattern2
              do s = 1,ns
                 write(*, '(a, 100L6)') &
                  & string(s),(matchw(string(s),pattern2(p)), p=1,np)
              enddo
              !
              stop
              !
              do s = 1,ns
                 do p=1,np
                 write(*, '(a,a,L7)') &
                  & string(s),pattern2(p),matchw(string(s),pattern2(p))
                 enddo
              enddo
           end subroutine demo2
           !
           end program demo_matchw
