          program demo_msg
          use M_debug, only : msg
          implicit none
          character(len=:),allocatable :: pr

          pr=msg('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
          write(*,'(a)')pr
          pr=msg('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
          write(*,'(a)')pr
          pr=msg('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
          write(*,'(a)')pr
          pr=msg('complex         :',cmplx(huge(0.0),tiny(0.0)) )
          write(*,'(a)')pr

          ! although it will often work, using msg(3f) in an I/O statement is not recommended
          write(*,*)msg('program will now stop')

          end program demo_msg
