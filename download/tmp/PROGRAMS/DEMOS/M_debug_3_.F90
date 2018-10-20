           program demo_unit_tests
           use M_debug, only: unit_check_start, unit_check
           use M_debug, only: unit_check_good, unit_check_bad
           implicit none
           integer :: i, j, k
           integer,allocatable :: array(:)
           integer :: arr(4)=[21,51,14,45]
           integer :: a=21, b=51, c=14, d=45
           i=1
           j=2
           k=3
           array=[10,20,30,40,50,60,70]

           !  register an entry for specified name in database with status of zero (0)
           call unit_check_start('myroutine')

           !  if mask test fails, change database status for specified entry to -1 and stop program, else continue
           call unit_check('myroutine',i.gt.0)

           ! use of all(3f), any(3f), merge(3f) can be useful
           ! if you know what these would produce
           ! write(*,*)['A','X','X','X','X','B'].eq.'B'      ! this would return an array, the last element having the value T, else F
           ! write(*,*)all(['A','X','X','X','X','X'].eq.'X') ! this would return F
           ! write(*,*)any(['A','X','X','X','X','X'].eq.'B') ! this would return F
           ! write(*,*)any(['A','X','X','X','X','B'].eq.'B') ! this would return T
           ! write(*,*).not.all(array.lt.100)
           ! write(*,*)all(array.lt.100)
           ! write(*,*)all([a,b,c,d].eq.[21,51,14,45]) ! compare a list. This would return T
           ! write(*,*)all(arr.eq.[21,51,14,45])       ! compare an array. This would return T

           ! this will make sense ...

           call unit_check('myroutine',all([i,j,k].gt.0),      'testing if everyone greater than zero')
           call unit_check('myroutine',all(.not.[i,j,k].eq.4), 'testing if no one is equal to four')

           ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
           if(i+j+k.lt.1)then
              call unit_check_bad('myroutine')
           endif

           ! it is assumed if you got here you should set status in the database to one, meaning tests were conducted and passed
           write(*,*)'check on "myroutine" passed'
           call unit_check_good('myroutine')

           end program demo_unit_tests
