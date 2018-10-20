          program demo_igetvalue
          use M_calculator_plus, only : rnum0
          use M_calculator, only: igetvalue
          value1=rnum0('A=100/2') ! store something into calculator
          write(*,*)value1,igetvalue('A')
          end program demo_igetvalue
