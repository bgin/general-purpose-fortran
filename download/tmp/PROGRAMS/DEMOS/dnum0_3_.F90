           program demo_dnum0
           use M_calculator, only : dnum0
           doubleprecision x,y,z
           ! NOTE: user must supply the JUOWN1 and C procedures.
           X=DNUM0('20/3.4')
           Y=DNUM0('CI = 10 * sin(3.1416/4)')
           Z=DNUM0('CI')
           write(*,*)x,y,z
           end program demo_dnum0
