           program demo_nint
             real(4) x4
             real(8) x8
             x4 = 1.234E0_4
             x8 = 4.321_8
             print *, nint(x4), idnint(x8)
           end program demo_nint
