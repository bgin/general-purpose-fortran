          program demo_isdir
          Use M_io, only : isdir
          implicit none
          integer                     :: i
          character(len=80),parameter :: names(*)=[ &
          '/tmp            ', &
          '/tmp/NOTTHERE   ', &
          '/usr/local      ', &
          '.               ', &
          'PROBABLY_NOT    ']
          do i=1,size(names)
             write(*,*)' is ',trim(names(i)),' a directory? ', isdir(names(i))
          enddo
          end program demo_isdir
