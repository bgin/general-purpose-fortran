           program demo_is_nan
           use M_units, only : is_nan
           real :: x=huge(0.0d0)
           character(len=3) :: line='NaN'
           real,parameter :: arr(*)=[-100.0,100.0,huge(0.0)]
              write(*,*)is_nan(x),x   ! note Infinity is not a Nan
              write(*,*)is_nan(-x),-x
              read(line,*)x
              write(*,*)is_nan(x),x
              write(*,*)x==x,x  ! note Nan is never equal to another value
              write(*,*)is_nan(arr),arr
           end program demo_is_nan
