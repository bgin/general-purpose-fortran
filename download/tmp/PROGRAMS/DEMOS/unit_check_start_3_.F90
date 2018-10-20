           program demo_unit_check_start
           use M_debug, only: unit_check_start
           use M_debug, only: unit_check
           use M_debug, only: unit_check_good, unit_check_bad

           implicit none
           integer :: x
           x=10
           call unit_check_start('myroutine')

           call unit_check_start('myroutine_long',' &
             & -section        3                    &
             & -library        libGPF             &
             & -filename       `pwd`/M_debug.FF     &
             & -documentation  y                    &
             & -ufpp           y                    &
             & -ccall          n                    &
             & -archive        GPF.a              &
             & ')

           call unit_check('myroutine', x.gt.3 ,   'test if big enough')
           call unit_check('myroutine', x.lt.100 , 'test if small enough')

           call unit_check_good('myroutine','store that checks passed')

           end program demo_unit_check_start
