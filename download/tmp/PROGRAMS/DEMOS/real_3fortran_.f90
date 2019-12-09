          program demo_real
             use,intrinsic :: iso_fortran_env, only : dp=>real64
             complex              :: zr = (1.0, 2.0)
             doubleprecision      :: xd=huge(3.0d0)
             complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)

             print *, real(zr), aimag(zr)
             print *, dble(zd), dimag(zd)

             write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)
          end program demo_real
