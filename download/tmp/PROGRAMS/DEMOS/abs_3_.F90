            program demo_abs
            integer :: i = -1, iout
            real :: x = -1.e0, xout, zout
            complex :: z = (-3.e0,4.e0)
               write(*,*)'INPUTS:',i,x,z
               iout = abs(i)
               xout = abs(x)
               zout = abs(z)
               write(*,*)'OUTPUTS:',iout,xout,zout
               ! 3 - 4 -5 right triangle test :
               write(*,*)'The abs() of (3.0,4.0) should be 5.0',abs((3.0,4.0))
            end program demo_abs
