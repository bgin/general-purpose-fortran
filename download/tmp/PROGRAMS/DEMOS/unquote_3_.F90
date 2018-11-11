          program demo_unquote
             use M_strings, only : unquote
             implicit none
             character(len=128)           :: quoted_str
             character(len=:),allocatable :: unquoted_str
             character(len=1),parameter   :: esc='\'
             integer                      :: ios
             do
                read(*,'(a)',iostat=ios)quoted_str
                if(ios.ne.0)exit
                write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
                unquoted_str=unquote(trim(quoted_str),esc)
                write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
             enddo
          end program demo_unquote
