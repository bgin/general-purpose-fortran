               program demo_m_calculator_plus
               use M_calculator_plus, only : rnum0
               implicit none
               real              :: rval
               character(len=80) :: string
               integer           :: ierr
               string='A=sind(30)'
               rval=rnum0(string,ierr)
               if(ierr.eq.0)then
                  write(*,*) rval
               else
                  write(*,*) 'error evaluating '//trim(string)
               endif
               rval=rnum0('A',ierr)
               write(*,*) rval
               end program demo_m_calculator_plus
