          program demo_swallow
          use M_io,      only : swallow
          use M_strings, only : notabs
          implicit none
          character(len=4096)          :: FILENAME   ! file to read
          character(len=:),allocatable :: pageout(:) ! array to hold file in memory
          integer                      :: longest, lines, i, ilen
          character(len=:),allocatable :: line
             ! get a filename
             call get_command_argument(1, FILENAME)
             ! allocate character array and copy file into it
             call swallow(FILENAME,pageout)
             if(.not.allocated(pageout))then
                write(*,*)'*demo_swallow* failed to load file '//FILENAME
             else
                ! write file from last line to first line
                longest=len(pageout)
                lines=size(pageout)
                allocate(character(len=longest)::line)
                write(*,*)'number of lines is ',lines
                write(*,*)'and length of lines is ',longest
                write(*,'(a)')repeat('%',longest+2)
                do i=lines,1,-1
                   call notabs(pageout(i),line,ilen)
                   write(*,'("%",a,"%")')line
                enddo
                write(*,'(a)')repeat('%',longest+2)
                deallocate(pageout)  ! release memory
             endif
          end program demo_swallow
