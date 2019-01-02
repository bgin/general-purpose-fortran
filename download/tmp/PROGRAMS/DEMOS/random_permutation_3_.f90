           program demo_random_permutation
           use M_random, only : random_permutation
           implicit none
           integer                    :: array(10)
           character(len=*),parameter :: list(*)=[character(len=5) :: &
                   & 'one','two','three','four','five',&
                   & 'six','seven','eight','nine','ten']
           integer                    :: i, j
           do i = 1,8
              call random_permutation(array)
              write(*,'(*(i5,1x))') array
              ! use random values as indices to randomize another array
              write(*,'(*(a,1x))') (adjustr(list(array(j))),j=1,size(array))
           enddo
           end program demo_random_permutation
