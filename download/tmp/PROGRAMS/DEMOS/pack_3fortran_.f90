           program demo_pack
           ! Sample program gathering nonzero elements from an array:
           call test1()
           ! Gathering nonzero elements from an array and appending elements from VECTOR:
           call test2()
           contains
           !
           subroutine test1()
           integer :: m(6)
             m = (/ 1, 0, 0, 0, 5, 0 /)
             write(*, fmt="(6(i0, ' '))") pack(m, m /= 0)  ! "1 5"
           end subroutine test1
           !
           subroutine test2()
           integer :: m(4)
             m = (/ 1, 0, 0, 2 /)
             write(*, fmt="(4(i0, ' '))") pack(m, m /= 0, (/ 0, 0, 3, 4 /))  ! "1 2 3 4"
           end subroutine test2
           !
           end program demo_pack
