          program demo_xterm_get_geometry
          use M_xterm, only : xterm_get_geometry
          implicit none
          integer :: irows, icols
             call xterm_get_geometry(irows,icols)
             write(*,*)'rows=',rows,' cols=',cols
          end program demo_xterm_get_geometry
