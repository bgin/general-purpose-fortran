          program demo_M_html
             use M_html
             implicit none
             integer :: i,j
             real    :: arr(10,20)=[(i-1*30.0+j,i=1,10,j=1,20)]
             integer :: io=20
             call h_open(io,'table.html')
             call h_array(io,arr)
             call h_close(io)

          end program demo_M_html
