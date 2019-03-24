           program demo_conjg
               complex :: z = (2.0, 3.0)
               complex(8) :: dz = (2.71_8, -3.14_8)
               z= conjg(z)
               print *, z
               dz = dconjg(dz)
               print *, dz
           end program demo_conjg
