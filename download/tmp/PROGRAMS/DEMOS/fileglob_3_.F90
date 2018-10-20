          program demo_fileglob  ! simple unit test
             call testit('*.*')
             call testit('/tmp/__notthere.txt')
          contains

          subroutine testit(string)
             use M_system, only : fileglob
             character(len=255),pointer :: list(:)
             character(len=*) :: string
             call fileglob(string, list)
             write(*,*)'Files:',size(list)
             write(*,'(a)')(trim(list(i)),i=1,size(list))
             deallocate(list)
          end subroutine testit

          end program demo_fileglob  ! simple unit test
