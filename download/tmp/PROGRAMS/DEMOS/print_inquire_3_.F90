             program demo_print_inquire
             use M_io, only : print_inquire

             call print_inquire(5,'')

             call print_inquire(19,'')

             open(unit=20)
             call print_inquire(20,'')

             open(unit=21,status='scratch')
             call print_inquire(21,'')

             open(unit=22,file='junko')
             write(22,*)'WRITE TO JUNKO'
             close(unit=22)
             call print_inquire(22,'')
             call print_inquire(-1,'junko')

             end program demo_print_inquire
