          program demo_erfc_scaled
            real(kind(0.0d0)) :: x = 0.17_8
            x = erfc_scaled(x)
            print *, x ! prints approx. 0.83375830214998126
          end program demo_erfc_scaled
