           program demo_scratch
           use M_io, only : scratch
           implicit none
           write(*,*)'find good scratch file name candidates; one should test if writable'
           write(*,*)scratch('JUNK:')
           write(*,*)scratch('')
           write(*,*)scratch()
           end program demo_scratch
