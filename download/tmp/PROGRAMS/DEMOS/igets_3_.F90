          program demo_igets
          use M_kracken, only: kracken, igets
          implicit none
          integer,allocatable  :: vals(:)
          integer              :: i
            ! define command arguments and parse user command
            call kracken('demo','-nums 1 2 3 100 1000 10000 100,000 11.11111 77.77777 -77.7777' )
            ! get any values specified for -nums
            vals=igets('demo_nums')
            if(size(vals).gt.0)then
               ! print the requested values
               write(*,'(*(i0:,","))')( vals(i),i=1,size(vals))
            endif
          end program demo_igets
