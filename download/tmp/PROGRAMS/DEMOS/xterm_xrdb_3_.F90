           program demo_xterm_xrdb
           use M_xterm, only : xterm_xrdb
           character(len=:),allocatable :: cache
              call xterm_xrdb('FAVORITES')
           enddo
           end program demo_xterm_xrdb
