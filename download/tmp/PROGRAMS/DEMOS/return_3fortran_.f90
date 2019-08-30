          program demo_return
          call one()
          contains
          subroutine one()
          write(*,*)'*one* started'
          if(1.eq.1)return
          write(*,*)'*one* ended -- will not get here'
          return
          end subroutine one
          end program demo_return
          program demo_return
          implicit none
             call one(2,*10,*20,*30)
             write(*,*)'did not select alternate return'
             goto 999
          10 continue
             write(*,*)'picked first alternate return'
             goto 999
          20 continue
             write(*,*)'picked second alternate return'
             goto 999
          30 continue
             write(*,*)'picked third alternate return'
             goto 999
          999 end
          subroutine one(ipick,*,*,*)
          integer :: ipick

             select case(ipick)
              case(1)
                write(*,*)'first alternate return selected'
                return 1
              case(2)
                write(*,*)'second alternate return selected'
                return 2
              case(3)
                write(*,*)'thir alternate return selected'
                return 3
             end select

             write(*,*)'no alternate return selected'

              end subroutine one

              !end program demo_return
