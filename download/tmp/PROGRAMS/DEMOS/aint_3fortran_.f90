           program demo_aint
             real(4) x4
             real(8) x8
             x4 = 1.234E0_4
             x8 = 4.321_8
             print *, aint(x4), dint(x8)
             x8 = aint(x4,8)
           end program demo_aint
