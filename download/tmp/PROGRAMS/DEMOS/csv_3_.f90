          program demo_csv
          use M_csv, only : csv
          implicit none
          character(len=:),allocatable :: pr

             pr=csv('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
             write(*,'(a)')pr
             pr=csv('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
             write(*,'(a)')pr
             pr=csv('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
             write(*,'(a)')pr
             pr=csv('complex         :',cmplx(huge(0.0),tiny(0.0)) )
             write(*,'(a)')pr

             write(*,*)csv('program will now stop')

              end program demo_csv
