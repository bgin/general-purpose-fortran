          program demo_rd
          use M_io, only : rd
          character(len=:),allocatable :: mystring
             mystring=rd('Enter string:',default='Today')
             write(*,*)mystring
          end program demo_rd
