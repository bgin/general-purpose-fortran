           program demo_dim
               integer :: i
               real(8) :: x
               i = dim(4, 15)
               x = dim(4.345_8, 2.111_8)
               print *, i
               print *, x
           end program demo_dim
