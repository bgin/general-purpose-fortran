          program demo_population
            print *, popcnt(127),       poppar(127)
            print *, popcnt(huge(0_4)), poppar(huge(0_4))
            print *, popcnt(huge(0_8)), poppar(huge(0_8))
          end program demo_population
