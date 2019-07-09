           program demo_verify

           write(*,*) verify("fortran", "ao")           ! 1, found 'f'
           write(*,*) verify("fortran", "fo")           ! 3, found 'r'
           write(*,*) verify("fortran", "c++")          ! 1, found 'f'
           write(*,*) verify("fortran", "c++", .true.)  ! 7, found 'n'
           write(*,*) verify("fortran", "nartrof")      ! 0' found none

           !=======================================================
           check : block ! check if string is of form NN-HHHHH
           logical                    :: lout
           character(len=*),parameter :: int='0123456789'
           character(len=*),parameter :: hex='abcdef0123456789'
           character(len=80)          :: chars

           chars='32-af43d'
           lout=.true.
           lout = lout.and.(verify(chars(1:2), int) == 0)
           lout = lout.and.(verify(chars(3:3), '-') == 0)
           lout = lout.and.(verify(chars(4:8), hex) == 0)
           if(lout)then
              write(*,*)trim(chars),' passed'
           endif

           write(*,*)'nonblank ',verify(chars, ' ') ! loc. of first nonblank
           write(*,*)'length ',verify(chars, ' ', back = .true.)  ! == len_trim

           endblock check
           !=======================================================
           end program demo_verify
