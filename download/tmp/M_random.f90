!>
!!##NAME
!!    M_random(3f) - [M_random::INTRO] Routines for generating random numbers and strings
!!##SYNOPSIS
!!
!!   See the routines:
!!
!!    use M_random, only : init_random_seed_by_system_clock, init_random_seed_by_dat, init_random_seed
!!    use M_random, only : random_string, random_hex
!!    use M_random, only : random_kiss64
!!    use M_random, only : mtprng_state, mtprng_init_array, mtprng_rand64, mtprng_rand_real1
!!    use M_random, only : mtprng_int, mtprng_int_by_array
!!    use M_random, only : mtprng_rand64, mtprng_rand, mtprng_rand_range
!!    use M_random, only : mtprng_rand_real3, mtprng_rand_real2, mtprng_rand_real1
!!    use M_random, only : random_permutations
!!    use M_random, only : scramble
!!
!!##QUOTE
!!
!!   The generation of random numbers is too important to be left to chance -- Robert R. Coveyou
!!
!!##DESCRIPTION
!!
!!    The M_random(3fm) module contains routines to support random number generation. This includes
!!    supplements for the Fortran intrinsic random_seed(3f).
!!
!!   SUPPLEMENTING INTRINSIC RANDOM_SEED
!!    o init_random_seed_by_system_clock(3f): initialize random_number(3f) to return a single value with system clock
!!    o init_random_seed_by_dat(3f): initialize random_number(3f) to return a single value using date_and_time(3f)
!!    o init_random_seed(3f): initialize random_number(3f) to return a single value with single integer seed like srand(3c
!!
!!    o random_string(3f): create random string composed of provided characters of specified length
!!    o random_hex(3f): create random hexadecimal string of specified length
!!
!!   MISCELLANEOUS
!!    o random_kiss64(3f): A 64-bit KISS random number generator by George Margaglia.
!!    o random_permutation(3f): populate integer array with a random permutation of the values 1 to size(array)
!!    o scramble(3f): generate an integer array of specified size populated with a random permutation of 1 to size(array)
!!
!!   MERSENNE TWISTER ALGORITHM
!!    o mtprng_int(3f): Initializes the Mersenne Twister random number generator with
!!    o mtprng_int_by_array(3f): Initialize with an array of seeds
!!
!!    o mtprng_rand64(3f): Obtain the next 64-bit integer in the pseudo-random sequence in the range 0 to 2^32-1
!!    o mtprng_rand(3f): Obtain the next 32-bit integer in the pseudo-random sequence in the range 0 to 2^31-1
!!    o mtprng_rand_range(3f): Obtain a pseudo-random integer in the range [lo,hi]
!!
!!    o mtprng_rand_real3(3f): Obtain a pseudo-random real number .gt. 0 and .lt. 1.
!!    o mtprng_rand_real2(3f): Obtain a pseudo-random real number .ge. 0.0 and .lt. 1.0
!!    o mtprng_rand_real1(3f): Obtain a pseudo-random real number .ge. 0 and .le.= 1.
!===================================================================================================================================
module M_random
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64  !  1 2 4 8
implicit none
private                                           ! Everything is private unless explicitly made public private
public init_random_seed_by_system_clock
public init_random_seed_by_dat
public init_random_seed

public random_string
public random_hex

public random_kiss64
public random_permutation
public scramble

public :: mtprng_state, mtprng_init, mtprng_init_by_array, mtprng_rand64, mtprng_rand
public :: mtprng_rand_range, mtprng_rand_real1, mtprng_rand_real2, mtprng_rand_real3

public test_suite_M_random
!==================================================================================================================================!
! Kind types for IEEE 754/IEC 60559 single- and double-precision reals
integer, parameter :: IEEE32 = selected_real_kind(  6,  37 )
integer, parameter :: IEEE64 = selected_real_kind( 15, 307 )
! Constants
integer(INT32), parameter :: N = 624_INT32
integer(INT32), parameter :: M = 397_INT32
! types
type mtprng_state
   integer(INT32)                   :: mti = -1
   integer(INT64), dimension(0:N-1) :: mt
end type
!==================================================================================================================================!
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_string(3f) - [M_random] create random string composed of provided characters of specified lengt
!!
!!##SYNOPSIS
!!
!!    function random_string(chars,length) result(out)
!!
!!     character(len=*),intent(in)     :: chars
!!     integer,intent(in)              :: length
!!     character(len=:),allocatable    :: out
!!
!!##DESCRIPTION
!!    Given a set of characters and a length, generate a random string of
!!    the specified length composed of the given set of characters.
!!
!!##OPTIONS
!!    chars   list of characters to generate random string with
!!    length  number of characters to place in output string
!!
!!##RESULT
!!    out     string of LENGTH characters randomly filled with characters from CHARS
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_random_string
!!     use M_random, only : random_string, init_random_seed_by_dat
!!        character(len=64) :: hexstring
!!        ! use date and time to create a seed for calling random_seed(3f)
!!        call init_random_seed_by_dat()
!!        hexstring=random_string('0123456789abcdef',len(hexstring))
!!        ! write random hexadecimal value for use
!!        ! as something like an X11 authorization key
!!        write(*,'(a)')hexstring
!!     end program demo_random_string
!!
!!    Results
!!
!!     2363a3589736e23be0137ec7ebc9d74297a963f27958a176daea3dd850ed8487
!===================================================================================================================================
function random_string(chars,length) result(out)

character(len=*),parameter::ident_1="&
&@(#)M_random::random_string(3f): create random string composed of provided characters of specified length"

character(len=*),intent(in)     :: chars
integer,intent(in)              :: length
character(len=:),allocatable    :: out
   real                         :: x
   integer                      :: ilen   ! length of list of characters
   integer                      :: which
   integer                      :: i
   ilen=len(chars)
   out=''
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_string
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_hex(3f) - [M_random] create random string composed of provided characters of specified lengt
!!
!!##SYNOPSIS
!!
!!    function random_hex(chars,length) result(out)
!!
!!     character(len=*),intent(in)     :: chars
!!     integer,intent(in)              :: length
!!     character(len=:),allocatable    :: out
!!
!!##DESCRIPTION
!!    Given a length, generate a random string of the specified length
!!    representing a hexadecimal value
!!
!!##OPTIONS
!!    chars   list of characters to generate random string with
!!    length  number of characters to place in output string
!!
!!##RESULT
!!    out     string of LENGTH characters randomly filled with characters
!!            from CHARS
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_random_hex
!!     use M_random, only : random_hex, init_random_seed_by_dat
!!        character(len=64) :: hexstring
!!        ! use date and time to create a seed for calling random_seed(3f)
!!        call init_random_seed_by_dat()
!!        ! write random hexadecimal value for use
!!        ! as something like an X11 authorization key
!!        hexstring=random_hex(len(hexstring))
!!        write(*,'(a)')hexstring
!!     end program demo_random_hex
!!
!!    Results
!!
!!     2363a3589736e23be0137ec7ebc9d74297a963f27958a176daea3dd850ed8487
!===================================================================================================================================
function random_hex(length) result(out)

character(len=*),parameter::ident_2="@(#)M_random::random_hex(3f): create random hexadecimal string of specified length"

integer,intent(in)              :: length
character(len=:),allocatable    :: out
   out=random_string('0123456789abcdef',length)
end function random_hex
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_permutation(3f) - [M_random] Populate an integer array with the values 1 to size(array
!!
!!##SYNOPSIS
!!
!!    subroutine random_permutation( array )
!!    integer,intent(inout) :: array(:)
!!
!!##DESCRIPTION
!!    Populate the given integer array with the numbers 1 to size(array) arranged in
!!    random order.
!!
!!##OPTIONS
!!    array    Integer array that will be filled with integers 1 to N in
!!             random order
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_random_permutation
!!    use M_random, only : random_permutation
!!    implicit none
!!    integer                    :: array(10)
!!    character(len=*),parameter :: list(*)=[character(len=5) :: &
!!    & 'one','two','three','four','five','six','seven','eight','nine','ten']
!!    integer                    :: i, j
!!    do i = 1,8
!!       call random_permutation(array)
!!       write(*,'(*(i5,1x))') array
!!       ! use random values as indices to randomize another array
!!       write(*,'(*(a,1x))') (adjustr(list(array(j))),j=1,size(array))
!!    enddo
!!    end program demo_random_permutation
!!
!!   Example output
!!
!!        6     5     4    10     3     2     1     7     8     9
!!      six  five  four   ten three   two   one seven eight  nine
!!        5     3     1     2    10     7     4     9     6     8
!!     five three   one   two   ten seven  four  nine   six eight
!!       10     1     9     5     3     4     2     6     7     8
!!      ten   one  nine  five three  four   two   six seven eight
!!        7     5     1     8    10     2     6     9     3     4
!!    seven  five   one eight   ten   two   six  nine three  four
!!        6     8     1    10     9     7     4     5     3     2
!!      six eight   one   ten  nine seven  four  five three   two
!!        2     4     8     9     7     3     6     1    10     5
!!      two  four eight  nine seven three   six   one   ten  five
!!        6     5     2     9     8    10     1     7     3     4
!!      six  five   two  nine eight   ten   one seven three  four
!!        5     3     4     9     6     2    10     1     7     8
!!     five three  four  nine   six   two   ten   one seven eight
!===================================================================================================================================
subroutine random_permutation( array )

character(len=*),parameter::ident_3="&
&@(#)M_random::random_permutation(3f): populate an integer array with a random permutation of the integers 1 to size(array)"

integer,intent(inout) :: array(:)

integer               :: i
integer               :: j
integer               :: n
integer               :: number_of_values
integer               :: temp
real                  :: random

   number_of_values = size(array)
   array=[(i,i=1,number_of_values)]

   n=number_of_values
   do i=1,number_of_values-1
      n=n-1
      call random_number(random)
      j=1+n*random
      if(j>=n+1)j=1
      ! switch values
      temp=array(i+j)
      array(i+j)=array(i)
      array(i)=temp
   enddo

end subroutine random_permutation
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    scramble(3f) - [M_random] return an integer array prepopulated with the values 1 to size(array) in random orde
!!
!!##SYNOPSIS
!!
!!    function scramble( number_of_values )
!!    integer,intent(in) :: number_of_values
!!
!!##DESCRIPTION
!!    Return an integer array of the size specified populated with the
!!    numbers 1 to "number_of_values" in random order.
!!
!!    A simple way to randomly scramble a list of any type is to create
!!    a random permutation of all the index values of the array and then
!!    access the original list elements using that list of indices. The
!!    list itself can be re-ordered very succintly using array syntax.
!!    Given a list size ..
!!
!!    1. create an INTEGER array of the specified size N
!!    2. populate it with the values from 1 to N
!!    3. randomly switche values in the array to randomize it
!!    4. return the newly created array for use as indices
!!
!!    The resulting random permutation of the indices can then be used to
!!    access essentially any type of list in random order.
!!
!!##OPTIONS
!!    number_of_values  size of integer array to create
!!
!!##RETURNS
!!    scramble    Integer array filled with integers 1 to NUMBER_OF_VALUES in random order
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_scramble
!!     use M_random, only : scramble
!!     implicit none
!!     character(len=*),parameter :: list(*)=[character(len=5) :: &
!!     & 'one','two','three','four','five',&
!!     & 'six','seven','eight','nine','ten']
!!     integer                    :: i
!!     integer                    :: n=size(list)
!!     character(len=len(list))   :: newlist(n)
!!     do i = 1,8
!!        ! use random values as indices to randomize array
!!        newlist=list(scramble(n))
!!        write(*,'(*(a,1x))') newlist
!!     enddo
!!     end program demo_scramble
!!
!!   Example output
!!
!!    ten   six   eight one   four  nine  two   five  three seven
!!    three five  ten   nine  one   four  six   seven two   eight
!!    four  eight ten   two   seven nine  six   three one   five
!!    three one   nine  seven ten   five  two   six   eight four
!!    two   seven nine  one   four  three eight ten   five  six
!!    three one   nine  six   ten   five  eight two   four  seven
!!    four  five  six   eight one   ten   three nine  seven two
!!    three nine  four  two   one   seven ten   five  six   eight
!===================================================================================================================================
function scramble( number_of_values ) result(array)

character(len=*),parameter::ident_4="@(#)M_random::scramble(3f): return an integer array of random values 1 to N."

integer,intent(in)    :: number_of_values
integer,allocatable   :: array(:)

integer               :: i, j
integer               :: n
integer               :: temp
real                  :: random

   array=[(i,i=1,number_of_values)]

   n=number_of_values
   do i=1,number_of_values-1
      n=n-1
      call random_number(random)
      j=1+n*random
      if(j>=n+1)j=1
      ! switch values
      temp=array(i+j)
      array(i+j)=array(i)
      array(i)=temp
   enddo

end function scramble
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!       random_kiss64 - [M_random] A 64-bit KISS random number generator by George Margaglia
!!##SYNOPSIS
!!
!!    function random_kiss64()
!!    integer, parameter         :: i8b = selected_int_kind(18)  ! eight-byte integer
!!    integer(i8b)               :: random_kiss64
!!
!!##DESCRIPTION
!!    A simple random number generator that returns a random 64-bit INTEGER. The same
!!    sequence is returned.
!!##EXAMPLE
!!
!!
!!   Sample usage:
!!
!!     program demo_random_kiss64
!!     use M_random, only : random_kiss64
!!     implicit none
!!     integer, parameter    :: i8b = selected_int_kind(18)  ! eight-byte integer
!!     integer(i8b)          :: i, t
!!
!!        write(*,*)'HUGE=',huge(0_i8b)
!!
!!        do i = 1, 100000000
!!           t = random_kiss64()
!!           if(mod(i,1000000+1)==1000000)write(*,*)i,' T=',T
!!        enddo
!!
!!        if (t .eq. 1666297717051644203_i8b) then
!!           print *, "100 million calls to KISS() OK"
!!        else
!!           print *, "Fail"
!!        endif
!!     end program demo_random_kiss64
!===================================================================================================================================
function random_kiss64()

character(len=*),parameter::ident_5="@(#)M_random::random_kiss64(3f): A 64-bit KISS random number generator by George Margaglia."

! From: FortranWiki.org
! Originally posted to comp.lang.fortran in the message 64-bit KISS RNGs.
! No license was specified.
!
! Revised on April 14, 2010 21:40:44 by Jason Blevins (75.178.9.182)
! This version was modified by Jason Blevins to use "implicit none"
! and to portably declare the 64-bit/eight-byte integer type.
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter         :: i8b = selected_int_kind(18)  ! eight-byte integer
   integer(i8b), save         :: x, y, z, c
   integer(i8b)               :: t, k, m, s, random_kiss64
   data x, y, z, c &
      / 1234567890987654321_i8b, &
      362436362436362436_i8b, &
      1066149217761810_i8b, &
      123456123456123456_i8b /
!-----------------------------------------------------------------------------------------------------------------------------------
   m(x,k) = ieor(x, ishft(x,k))  ! statement function
   s(x) = ishft(x, -63)          ! statement function
!-----------------------------------------------------------------------------------------------------------------------------------
   t = ishft(x, 58) + c
   if (s(x) .eq. s(t)) then
      c = ishft(x, -6) + s(x)
   else
      c = ishft(x, -6) + 1 - s(x + t)
   endif
   x = t + x
   y = m(m(m(y,13_i8b),-17_i8b), 43_i8b)
   z = 6906969069_i8b * z + 1234567
   random_kiss64 = x + y + z
end function random_kiss64
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    init_random_seed_by_system_clock(3f) - [M_random] seed random_number(3f) with system clock value
!!
!!##SYNOPSIS
!!
!!    subroutine init_random_seed_by_system_clock()
!!
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the system clock to initialize the seed so you ca
!!    easily call random_number(3f) with varying pseudo-random real number sequences
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_init_random_seed_by_system_clock
!!     use M_random, only : init_random_seed_by_system_clock
!!     integer :: i
!!     real    :: x
!!        call init_random_seed_by_system_clock()
!!        do i=1,10
!!           ! generate real pseudo-random numbers from 0 to <1.0
!!           call random_number(x)
!!           write(*,*)i,x
!!        enddo
!!     end program demo_init_random_seed_by_system_clock
!!
!!    Results
!!
!!     >   1  0.661672294
!!     >   2  0.274969578
!!     >   3  0.683666587
!!     >   4   7.35652447E-02
!!     >   5  0.457893968
!!     >   6  0.826303899
!!     >   7  0.727411628
!!     >   8  0.542535722
!!     >   9  0.983459771
!!     >  10  0.527638793
!===================================================================================================================================
subroutine init_random_seed_by_system_clock()

character(len=*),parameter::ident_6="&
&@(#)M_random::init_random_seed_by_system_clock(3f): initialize random_number(3f) to return a single value with system clock"

   integer :: i, n, clock
   integer, dimension(:), allocatable :: seed
   call random_seed(size = n)
   allocate(seed(n))
   call system_clock(count=clock)
   seed = clock + 37 * (/ (i - 1, i = 1, n) /)
!   write(*,*)seed
!   write(*,*)(/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)

   deallocate(seed)
end subroutine init_random_seed_by_system_clock
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    init_random_seed_by_dat(3f) - [M_random] seed random_number(3f) with values from date_and_time(3f
!!
!!##SYNOPSIS
!!
!!    subroutine init_random_seed_by_dat()
!!
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the date_and_time(3f)
!!    intrinsic to initialize the seed so you can easily call
!!    random_number(3f) with varying pseudo-random real number sequences
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_init_random_seed_by_dat
!!     use M_random, only : init_random_seed_by_dat
!!     integer :: i
!!     real    :: x
!!        call init_random_seed_by_dat()
!!        do i=1,10
!!           ! generate real pseudo-random numbers from 0 to <1.0
!!           call random_number(x)
!!           write(*,*)i,x
!!        enddo
!!     end program demo_init_random_seed_by_dat
!!
!!    Results
!!
!!      >     1  0.644704163
!!      >     2  0.244343698
!!      >     3  0.516471267
!!      >     4  0.296542704
!!      >     5  0.681771278
!!      >     6  0.449223280
!!      >     7  0.915870190
!!      >     8  0.466257989
!!      >     9  0.912388682
!!      >    10  0.597788215
!===================================================================================================================================
subroutine init_random_seed_by_dat()

character(len=*),parameter::ident_7="&
&@(#)M_random::init_random_seed_by_dat(3f): initialize random_number(3f) to return a single value using date_and_time(3f)"

! Initially based on a post on comp.lang.fortran.
  integer :: ival(8)
  integer :: n, v(3), i
  integer, allocatable :: seed(:)
  call date_and_time(values=ival)
  v(1) = ival(8) + 2048*ival(7)
  v(2) = ival(6) + 64*ival(5)                     ! skip value(4) because it is the timezone, which is typically constant
  v(3) = ival(3) + 32*ival(2) + 32*8*ival(1)
  call random_seed(size=n)
  allocate(seed(n))
  call random_seed()                              ! give the seed an implementation-dependent kick
  call random_seed(get=seed)
  do i=1, n
     seed(i) = seed(i) + v(mod(i-1, 3) + 1)
  enddo
  call random_seed(put=seed)
  deallocate(seed)
end subroutine init_random_seed_by_dat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    init_random_seed(3f) - [M_random] seed random_number(3f) with single value like srand(3c) usag
!!
!!##SYNOPSIS
!!
!!    subroutine init_random_seed(mine)
!!
!!     integer,intent(in) :: mine
!!
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the single given
!!    integer to initialize the seed so you can easily call random_number(3f)
!!    with varying pseudo-random real number sequences simply, much like
!!    srand(3c) and rand(3c).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_init_random_seed
!!     use M_random, only : init_random_seed
!!     integer :: iseed
!!     integer :: i
!!     real    :: x
!!        iseed=218595421
!!        call init_random_seed(iseed)
!!        do i=1,10
!!           ! generate real pseudo-random numbers from 0 to <1.0
!!           call random_number(x)
!!           write(*,*)i,x
!!        enddo
!!     end program demo_init_random_seed
!!
!!    Results
!!
!!      >     1  0.989341617
!!      >     2  0.296594143
!!      >     3  0.805420995
!!      >     4   4.00894880E-03
!!      >     5   5.73359132E-02
!!      >     6  0.805290103
!!      >     7  0.944527864
!!      >     8  0.789443851
!!      >     9  0.327288270
!!      >    10  0.710926533
!===================================================================================================================================
subroutine init_random_seed(mine)

character(len=*),parameter::ident_8="&
&@(#)M_random::init_random_seed(3f): initialize random_number(3f) to return a single value with single integer seed like srand(3c)"

! to make this start with a single number like srand(3c) take the seed and
! use the value to fill the seed array, adding 37 to each subsequent value
! till the array is filled.
integer,intent(in) :: mine
   integer         :: i, n
   integer, dimension(:), allocatable :: seed
   call random_seed(size = n)
   allocate(seed(n))
   seed = mine + 37 * (/ (i - 1, i = 1, n) /)
  !write(*,*)seed
  !write(*,*)(/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)
   deallocate(seed)
end subroutine init_random_seed
!  An implementation of the Mersenne Twister algorithm for generating pseudo-random sequences.
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! This notice applies specifically to the MTPRNG_* routines ....

! From the Algorithmic Conjurings of Scott Robert Ladd comes...
!
!  mtprng.f90 (a Fortran 95 module)
!
!  An implementation of the Mersenne Twister algorithm for generating
!  pseudo-random sequences.
!
!  History
!  -------
!   1.0.0   Initial release
!
!   1.1.0   6 February 2002
!           Updated to support algorithm revisions posted
!           by Matsumoto and Nishimura on 26 January 2002
!
!   1.5.0   12 December 2003
!           Added to hypatia project
!           Minor style changes
!           Tightened code
!           Now state based; no static variables
!           Removed mtprng_rand_real53
!
!   2.0.0   4 January 2004
!           Corrected erroneous unsigned bit manipulations
!           Doubled resolution by using 64-bit math
!           Added mtprng_rand64
!
!    2.0.1  26 April 2018
!           Added IMPLICIT NONE,
!           defined kinds internally to remove need for
!           separate module STDTYPES. Included this
!           module into existing module M_RANDOM.
!
!  ORIGINAL ALGORITHM COPYRIGHT
!  ============================
!  Copyright (C) 1997,2002 Makoto Matsumoto and Takuji Nishimura.
!  Any feedback is very welcome. For any question, comments, see
!  http://www.math.keio.ac.jp/matumoto/emt.html or email
!  matumoto@math.keio.ac.jp
!---------------------------------------------------------------------
!
!  COPYRIGHT NOTICE, DISCLAIMER, and LICENSE:
!
!  This notice applies *only* to this specific expression of this
!  algorithm, and does not imply ownership or invention of the
!  implemented algorithm.
!
!  If you modify this file, you may insert additional notices
!  immediately following this sentence.
!
!  Copyright 2001, 2002, 2004 Scott Robert Ladd.
!  All rights reserved, except as noted herein.
!
!  This computer program source file is supplied "AS IS". Scott Robert
!  Ladd (hereinafter referred to as "Author") disclaims all warranties,
!  expressed or implied, including, without limitation, the warranties
!  of merchantability and of fitness for any purpose. The Author
!  assumes no liability for direct, indirect, incidental, special,
!  exemplary, or consequential damages, which may result from the use
!  of this software, even if advised of the possibility of such damage.
!
!  The Author hereby grants anyone permission to use, copy, modify, and
!  distribute this source code, or portions hereof, for any purpose,
!  without fee, subject to the following restrictions:
!
!      1. The origin of this source code must not be misrepresented.
!
!      2. Altered versions must be plainly marked as such and must not
!         be misrepresented as being the original source.
!
!      3. This Copyright notice may not be removed or altered from any
!         source or altered source distribution.
!
!  The Author specifically permits (without fee) and encourages the use
!  of this source code for entertainment, education, or decoration. If
!  you use this source code in a product, acknowledgment is not required
!  but would be appreciated.
!
!  Acknowledgement:
!      This license is based on the wonderful simple license that
!      accompanies libpng.
!
!-----------------------------------------------------------------------
!
!  For more information on this software package, please visit
!  Scott's web site, Coyote Gulch Productions, at:
!
!      http://www.coyotegulch.com
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_init(3f) - [M_random:MERSENNE TWISTER] Initialize the Mersenne Twister random number generator with "seed
!!
!!##SYNOPSIS
!!
!!    subroutine mtprng_init(seed, state)
!!    integer(INT32),     intent(in)  :: seed
!!    type(mtprng_state), intent(out) :: state
!!
!!##DESCRIPTION
!!    Initializes the Mersenne Twister random number generator with "seed"
!!
!!##OPTIONS
!!    seed   A seed value is used to start a specific sequence of pseudo-random numbers
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_init
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      seed = nint(100*secnds(0.))
!!      call mtprng_init(seed, state)
!!      ! returns a INT64 integer with a range in 0 .. 2^32-1
!!      write(*,*) mtprng_rand64(state)
!!    end program demo_mtprng_init
!!
!!   Sample Results:
!!
!!      867010878
!===================================================================================================================================
subroutine mtprng_init(seed, state)

character(len=*),parameter::ident_9="&
&@(#)M_random::mtprng_int(3f): Initializes the Mersenne Twister random number generator with seed"

! arguments
integer(INT32),     intent(in)  :: seed
type(mtprng_state), intent(out) :: state
   ! working storage
   integer :: i
   ! save seed
   state%mt(0) = seed

   ! Set the seed using values suggested by Matsumoto & Nishimura, using
   !   a generator by Knuth. See original source for details.
   do i = 1, N - 1
      state%mt(i) = iand(4294967295_INT64,1812433253_INT64 * ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64)) + i)
   enddo

   state%mti = N

end subroutine mtprng_init
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_init_by_array(3f) - [M_random:MERSENNE TWISTER] Initialize the Mersenne Twister random number generator with "seed" arra
!!
!!##SYNOPSIS
!!
!!    subroutine mtprng_init_by_array(init_key, state)
!!    integer(INT32), dimension(:), intent(in) :: init_key
!!    type(mtprng_state), intent(out) :: state
!!
!!##DESCRIPTION
!!    Initialize the Mersenne Twister random number generator with "seed" array
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_init_by_array
!!    use M_random, only : mtprng_state, mtprng_init_by_array
!!    use M_random, only : mtprng_rand64, mtprng_rand_real1
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32)     :: init_key(3)
!!    type(mtprng_state) :: state
!!      init_key(1) = nint(11*secnds(0.))
!!      init_key(2) = nint(37*secnds(0.))
!!      init_key(3) = nint(97*secnds(0.))
!!      call mtprng_init_by_array(init_key, state )
!!      ! returns a INT64 integer with a range in 0 .. 2^32-1
!!      write(*,*) mtprng_rand64(state)
!!      ! returns a IEEE64 real, may be used as double precision
!!      write(*,*) mtprng_rand_real1(state)
!!    end program demo_mtprng_init_by_array
!===================================================================================================================================
subroutine mtprng_init_by_array(init_key, state)

character(len=*),parameter::ident_10="@(#)M_random::mtprng_int_by_array(3f): Initialize with an array of seeds"

! arguments
integer(INT32),intent(in)       :: init_key(:)
type(mtprng_state), intent(out) :: state

   ! working storage
   integer :: key_length
   integer :: i
   integer :: j
   integer :: k

   call mtprng_init(19650218_INT32,state)

   i = 1
   j = 0
   key_length = size(init_key)

   do k = max(N,key_length), 0, -1
      state%mt(i) = ieor(state%mt(i),(ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64) * 1664525_INT64))) + init_key(j) + j

      i = i + 1
      j = j + 1

      if (i >= N) then
         state%mt(0) = state%mt(N-1)
         i = 1
      endif

      if (j >= key_length) j = 0
   enddo

   do k = N-1, 0, -1
      state%mt(i) = ieor(state%mt(i),(ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64) * 1566083941_INT64))) - i

      i = i + 1

      if (i>=N) then
         state%mt(0) = state%mt(N-1)
         i = 1
      endif
   enddo

   state%mt(0) = 1073741824_INT64 ! 0x40000000, assuring non-zero initial array

end subroutine mtprng_init_by_array
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand64(3f) - [M_random:MERSENNE TWISTER] Obtain the next 64-bit integer in the pseudo-random sequenc
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand64(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer(INT64) :: r
!!
!!##DESCRIPTION
!!    Obtain the next 64-bit integer in the pseudo-random sequence in the range 0 to 2^32-1.
!!    Note that the range is considerably below the value of HUGE(0_int64).
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##RETURNS
!!    r      next pseudo-random value in the range 0 to 2^32-1
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand64
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      seed = nint(100*secnds(0.))
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand64(state)
!!    end program demo_mtprng_rand64
!===================================================================================================================================
function mtprng_rand64(state) result(r)

character(len=*),parameter::ident_11="@(#)M_random::mtprng_rand64(3f): Obtain the next 64-bit integer in the pseudo-random sequence"

! arguments
type(mtprng_state), intent(inout) :: state
!return type
integer(INT64) :: r

   ! internal constants
   integer(INT64), dimension(0:1), parameter :: mag01 = (/ 0_INT64, -1727483681_INT64 /)

   ! Period parameters
   integer(INT64), parameter :: UPPER_MASK =  2147483648_INT64
   integer(INT64), parameter :: LOWER_MASK =  2147483647_INT64

   ! Tempering parameters
   integer(INT64), parameter :: TEMPERING_B = -1658038656_INT64
   integer(INT64), parameter :: TEMPERING_C =  -272236544_INT64

   ! Note: variable names match those in original example
   integer(INT32) :: kk

   ! Generate N words at a time
   if (state%mti >= N) then
      ! The value -1 acts as a flag saying that the seed has not been set.
      if (state%mti == -1) call mtprng_init(4357_INT32,state)

      ! Fill the mt array
      do kk = 0, N - M - 1
         r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
         state%mt(kk) = ieor(ieor(state%mt(kk + M),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
      enddo

      do kk = N - M, N - 2
         r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
         state%mt(kk) = ieor(ieor(state%mt(kk + (M - N)),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
      enddo

      r = ior(iand(state%mt(N-1),UPPER_MASK),iand(state%mt(0),LOWER_MASK))
      state%mt(N-1) = ieor(ieor(state%mt(M-1),ishft(r,-1)),mag01(iand(r,1_INT64)))

      ! Start using the array from first element
      state%mti = 0
   endif

   ! Here is where we actually calculate the number with a series of
   !   transformations
   r = state%mt(state%mti)
   state%mti = state%mti + 1

 !-------------------------
 !!r = ieor(r,ishft(r,-11))
   r = ieor(r,ishft(iand(4294967295_INT64,r),-11)) ! Added a 32-bit mask to first r shift
 !-------------------------

   r = iand(4294967295_INT64,ieor(r,iand(ishft(r, 7),TEMPERING_B)))
   r = iand(4294967295_INT64,ieor(r,iand(ishft(r,15),TEMPERING_C)))
   r = ieor(r,ishft(r,-18))

end function mtprng_rand64
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand(3f) - [M_random:MERSENNE TWISTER] Obtain the next 32-bit integer in the pseudo-random sequenc
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer(INT32) :: r
!!
!!##DESCRIPTION
!!    Obtain the next 32-bit integer in the pseudo-random sequence
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##RETURNS
!!    r      The next 32-bit integer in the pseudo-random sequence
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      seed = nint(100*secnds(0.))
!!      call mtprng_init(seed, state)
!!      ! returns a INT64 integer with a range in 0 .. 2^31-1
!!      write(*,*) mtprng_rand(state)
!!    end program demo_mtprng_rand
!===================================================================================================================================
function mtprng_rand(state) result(r)

character(len=*),parameter::ident_12="@(#)M_random::mtprng_rand(3f): Obtain the next 32-bit integer in the pseudo-random sequence"

! arguments
type(mtprng_state), intent(inout) :: state
!return type
integer(INT32) :: r

   ! working storage
   integer(INT64) :: x

   ! done
   x = mtprng_rand64(state)

   if (x > 2147483647_INT64) then
      r = x - 4294967296_INT64
   else
      r = x
   endif

end function mtprng_rand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_range(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random integer in the range [lo,hi
!!##SYNOPSIS
!!
!!    function mtprng_rand_range(state, lo, hi) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer, intent(in) :: lo
!!    integer, intent(in) :: hi
!!    integer(INT32) :: r
!!
!!##DESCRIPTION
!!    Obtain a pseudo-random integer in the range [lo,hi]
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!    lo     lowest value in desired range of values to return
!!    hi     highest value in desired range of values to return
!!
!!##RETURNS
!!    r      returned pseudo-random value in range from LO to HI
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand_range
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand_range
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      seed = nint(100*secnds(0.))
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand_range(state,20,30)
!!    end program demo_mtprng_rand_range
!===================================================================================================================================
function mtprng_rand_range(state, lo, hi) result(r)

character(len=*),parameter::ident_13="@(#)M_random::mtprng_rand_range(3f): Obtain a pseudo-random integer in the range [lo,hi]"

! arguments
type(mtprng_state), intent(inout) :: state
integer, intent(in) :: lo
integer, intent(in) :: hi
! return type
integer(INT32) :: r

   ! Use real value to calculate range
   r = lo + floor((hi - lo + 1.0_IEEE64) * mtprng_rand_real2(state))

end function mtprng_rand_range
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_real1(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random real number in the range [0.0,1.0
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand_real1(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    real(IEEE64) :: r
!!##DESCRIPTION
!!    Obtain a pseudo-random real number in the range [0,1], i.e., a number
!!    greater than or equal to 0 and less than or equal to 1.
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!     r      ...
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_real1
!!    use M_random, only : mtprng_init
!!    use M_random, only : mtprng_state
!!    use M_random, only : mtprng_rand_real1
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      seed = nint(100*secnds(0.))
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand_real1(state)
!!    end program demo_mtprng_real1
!===================================================================================================================================
function mtprng_rand_real1(state) result(r)

character(len=*),parameter::ident_14="@(#)M_random::mtprng_rand_real1(3f): Obtain a pseudo-random real number .ge. 0 and .le.= 1."

! arguments
type(mtprng_state), intent(inout) :: state
! return type
real(IEEE64) :: r

   ! Local constant; precalculated to avoid division below
   real(IEEE64), parameter :: factor = 1.0_IEEE64 / 4294967295.0_IEEE64

   ! compute
   r = real(mtprng_rand64(state),IEEE64) * factor

end function mtprng_rand_real1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_real2(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random real number in the range [0,<1
!!
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    Obtain a pseudo-random real number in the range [0,1), i.e., a number
!!    greater than or equal to 0 and less than 1.
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_real2
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand_real2
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32)     :: seed
!!    type(mtprng_state) :: state
!!      seed = nint(100*secnds(0.))
!!      call mtprng_init(seed, state)
!!      ! returns a IEEE64 real, may be used as double precision
!!      write(*,*) mtprng_rand_real2(state)
!!    end program demo_mtprng_real2
!===================================================================================================================================
function mtprng_rand_real2(state) result(r)
character(len=*),parameter::ident_15="@(#)M_random::mtprng_rand_real2(3f): Obtain a pseudo-random real number .ge. 0.0 and .lt. 1.0"

type(mtprng_state), intent(inout) :: state                                   ! arguments
real(IEEE64)                      :: r                                       ! return type
   real(IEEE64), parameter        :: factor=1.0_IEEE64 / 4294967296.0_IEEE64 ! Local constant; precalculated to avoid division below

   r = real(mtprng_rand64(state),IEEE64) * factor                            ! compute

end function mtprng_rand_real2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_real3(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random real number in the range (0< XXX <1
!!
!!##SYNOPSIS
!!
!!     function mtprng_rand_real3(state) result(r)
!!     type(mtprng_state), intent(inout) :: state
!!     real(IEEE64) :: r
!!
!!##DESCRIPTION
!!    Obtain a pseudo-random real number in the range (0,1), i.e., a number
!!    greater than 0 and less than 1.
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!    r      a pseudo-random real number greater than 0 and less than 1.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_mtprng_real3
!!     use M_random, only : mtprng_state, mtprng_init, mtprng_rand_real3
!!     use, intrinsic :: iso_fortran_env, only : int32
!!     implicit none
!!     integer(INT32) :: seed
!!     type(mtprng_state) :: state
!!       seed = nint(100*secnds(0.))
!!       call mtprng_init(seed, state)
!!       write(*,*) mtprng_rand_real3(state)
!!     end program demo_mtprng_real3
!===================================================================================================================================
function mtprng_rand_real3(state) result(r)

character(len=*),parameter::ident_16="@(#)M_random::mtprng_rand_real3(3f): Obtain a pseudo-random real number .gt. 0 and .lt. 1."

! arguments
type(mtprng_state), intent(inout) :: state
! return type
real(IEEE64) :: r

   ! Local constant; precalculated to avoid division below
   real(IEEE64), parameter :: factor = 1.0_IEEE64 / 4294967296.0_IEEE64

   r = (real(mtprng_rand64(state),IEEE64) + 0.5_IEEE64) * factor

end function mtprng_rand_real3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_random()

!! setup
   call test_init_random_seed()
   call test_init_random_seed_by_dat()
   call test_init_random_seed_by_system_clock()
   call test_mtprng_init()
   call test_mtprng_init_by_array()
   call test_mtprng_rand()
   call test_mtprng_rand64()
   call test_mtprng_rand_range()
   call test_mtprng_rand_real1()
   call test_mtprng_rand_real2()
   call test_mtprng_rand_real3()
   call test_random_hex()
   call test_random_kiss64()
   call test_random_permutation()
   call test_random_string()
   call test_scramble()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_random_seed()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('init_random_seed',msg='')
   !!call unit_check('init_random_seed', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('init_random_seed',msg='')
end subroutine test_init_random_seed
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_random_seed_by_dat()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('init_random_seed_by_dat',msg='')
   !!call unit_check('init_random_seed_by_dat', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('init_random_seed_by_dat',msg='')
end subroutine test_init_random_seed_by_dat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_random_seed_by_system_clock()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('init_random_seed_by_system_clock',msg='')
   !!call unit_check('init_random_seed_by_system_clock', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('init_random_seed_by_system_clock',msg='')
end subroutine test_init_random_seed_by_system_clock
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_init()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_init',msg='')
   !!call unit_check('mtprng_init', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_init',msg='')
end subroutine test_mtprng_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_init_by_array()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_init_by_array',msg='')
   !!call unit_check('mtprng_init_by_array', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_init_by_array',msg='')
end subroutine test_mtprng_init_by_array
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_rand',msg='')
   !!call unit_check('mtprng_rand', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_rand',msg='')
end subroutine test_mtprng_rand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand64()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_rand64',msg='')
   !!call unit_check('mtprng_rand64', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_rand64',msg='')
end subroutine test_mtprng_rand64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_range()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_rand_range',msg='')
   !!call unit_check('mtprng_rand_range', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_rand_range',msg='')
end subroutine test_mtprng_rand_range
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_real1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_rand_real1',msg='')
   !!call unit_check('mtprng_rand_real1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_rand_real1',msg='')
end subroutine test_mtprng_rand_real1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_real2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_rand_real2',msg='')
   !!call unit_check('mtprng_rand_real2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_rand_real2',msg='')
end subroutine test_mtprng_rand_real2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_real3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('mtprng_rand_real3',msg='')
   !!call unit_check('mtprng_rand_real3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('mtprng_rand_real3',msg='')
end subroutine test_mtprng_rand_real3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_hex()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('random_hex',msg='')
   !!call unit_check('random_hex', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('random_hex',msg='')
end subroutine test_random_hex
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_kiss64()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('random_kiss64',msg='')
   !!call unit_check('random_kiss64', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('random_kiss64',msg='')
end subroutine test_random_kiss64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_permutation()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('random_permutation',msg='')
   !!call unit_check('random_permutation', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('random_permutation',msg='')
end subroutine test_random_permutation
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_string()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('random_string',msg='')
   !!call unit_check('random_string', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('random_string',msg='')
end subroutine test_random_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scramble()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('scramble',msg='')
   !!call unit_check('scramble', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('scramble',msg='')
end subroutine test_scramble
!===================================================================================================================================
end subroutine test_suite_M_random
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_random
