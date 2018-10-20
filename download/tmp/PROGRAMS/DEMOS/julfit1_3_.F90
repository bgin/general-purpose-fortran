             program demo_julfit1
                use M_math, only : julfit1
                implicit none
                intrinsic random_number
                integer :: points
                real    :: slope, intercept
                write(*,*)'For y=m*x+b enter M and B and number of points N:'
                read(*,*)slope,intercept,points
                call testit()
             contains

                subroutine testit()
                   real    :: x(points), y(points)
                   real    :: slope_out, intercept_out, r2
                   integer :: i
                   real    :: rndnum
                   do i=1,points
                      x(i)=i*0.10
                      ! assigned pseudorandom numbers from the uniform distribution in the interval 0  x < 1.
                      call random_number(rndnum)
                      y(i)=slope*(x(i)+4.0*(rndnum-0.5))+intercept
                   enddo
                   !write(*,*)(ii,x(ii),y(ii),new_line('A'),ii=1,points)
                   call julfit1(x,y,points,slope_out,intercept_out,r2)
                   write(*,*)'SLOPE AND INTERCEPT IN  ',slope,intercept
                   write(*,*)'SLOPE AND INTERCEPT OUT ',slope_out,intercept_out,r2
                end subroutine testit

              end program demo_julfit1
