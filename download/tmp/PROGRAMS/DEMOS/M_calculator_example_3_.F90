          program demo_M_calculator

             !     line mode calculator that calls jucalc
             !
             use m_calculator, only: jucalc,iclen_calc
             implicit none
             integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
             character(len=iclen_calc) :: event, line
             character(len=iclen_calc) :: outlin
             integer                   :: ios
             integer                   :: ierr
             real(kind=k_dbl)          :: rvalue
             INFINITE: do
                read(*,'(a)',iostat=ios)line
                if(ios.ne.0)exit INFINITE
                call jucalc(line,outlin,event,rvalue,ierr)
                ! line   -- input expression
                ! outlin -- result as a string
                ! event  --
                ! rvalue -- result as a numeric value
                ! ierr   -- return status
                !
                ! several different meaning to the status flag ...
                select case(ierr)
                case(0)  ! a numeric value was returned without error
                  write(6,'(a,a,a)')trim(outlin),' = ',trim(line)
                case(2)  ! a string value was returned without error
                  write(6,'(a)')trim(event)
                case(1)  ! a request for a message has been returned
                         ! (from DUMP or FUNC)
                  write(6,'(a,a)')'message===>',trim(event)
                case(-1) ! an error has occurred
                  write(6,'(a,a)')'error===>',trim(event)
                case default ! this should not occur
                  write(6,'(a)')'warning===> unexpected ierr value from jucalc'
                end select
             enddo INFINITE
             end program demo_M_calculator
