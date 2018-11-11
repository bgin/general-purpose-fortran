           program demo_i_is_prime
           use m_factor, only: i_is_prime
           implicit none
           integer  :: i
           integer  :: icount=0
           integer  :: isum=0
           integer,parameter :: n= 10000

           do i=2, n
              if(i_is_prime(i))then
                 icount=icount+1
                 isum=isum+i
                 write(*,*)icount,i
              endif
           enddo

           write(*,*)'number of primes between 2 and ',n,' is ',icount
           write(*,*)'sum of primes between 2 and ',n,' is ',isum

           end program demo_i_is_prime
