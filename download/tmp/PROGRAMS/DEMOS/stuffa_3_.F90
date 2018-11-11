          program demo_stuffa
          use M_calculator, only : stuffa
          implicit none
          integer :: ii
             call stuffa('$A','',ii,'')
             call stuffa('$mystring','this is the value of the string',ii,'')
          end program demo_stuffa
