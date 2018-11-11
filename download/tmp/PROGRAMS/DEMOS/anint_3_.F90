           program demo_anint
             real(4) x4
             real(8) x8
             x4 = 1.234E0_4
             x8 = 4.321_8
             print *, anint(x4), dnint(x8)
             x8 = anint(x4,8)
           end program demo_anint
