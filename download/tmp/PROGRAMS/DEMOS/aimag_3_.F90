           program demo_aimag
             complex(4) z4
             complex(8) z8
             z4 = cmplx(1.e0_4, 0.e0_4)
             z8 = cmplx(0.e0_8, 1.e0_8)
             print *, aimag(z4), dimag(z8)
           end program demo_aimag
