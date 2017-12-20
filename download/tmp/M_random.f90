module M_random
implicit none
private
public init_random_seed_by_system_clock
public init_random_seed_by_dat
public init_random_seed
public random_kiss64
public random_string
public random_hex
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_string(3f) - [M_random] create random string composed of provided characters of specified length
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

character(len=*),parameter::ident="&
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
!!    random_hex(3f) - [M_random] create random string composed of provided characters of specified length
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
integer,intent(in)              :: length
character(len=:),allocatable    :: out
   out=random_string('0123456789abcdef',length)
end function random_hex
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!       random_kiss64 - [M_random] A 64-bit KISS random number generator by George Margaglia.
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##EXAMPLE
!!
!!
!!    Sample usage:
!!
!!     program test_random_kiss64
!!     use M_random, only : random_kiss64
!!       implicit none
!!       integer, parameter    :: i8b = selected_int_kind(18)  ! eight-byte integer
!!       integer(i8b)          :: i, t
!!       integer(i8b),external :: random_kiss64
!!
!!       do i = 1, 100000000
!!          t = random_kiss64()
!!          if(i.eq.100)write(*,*)'T=',T
!!       end do
!!
!!       if (t .eq. 1666297717051644203_i8b) then
!!          print *, "100 million calls to KISS() OK"
!!       else
!!          print *, "Fail"
!!       end if
!!     end program test_random_kiss64
!===================================================================================================================================
function random_kiss64()

character(len=*),parameter::ident="@(#)A 64-bit KISS random number generator by George Margaglia."

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
!!##SYNOPSIS
!!
!!    subroutine init_random_seed_by_system_clock()
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the system clock to initialize the seed so you can
!!    easily call random_number(3f) with varying pseudo-random real number sequences
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
!!         1  0.661672294
!!         2  0.274969578
!!         3  0.683666587
!!         4   7.35652447E-02
!!         5  0.457893968
!!         6  0.826303899
!!         7  0.727411628
!!         8  0.542535722
!!         9  0.983459771
!!        10  0.527638793
!===================================================================================================================================
subroutine init_random_seed_by_system_clock()

character(len=*),parameter::ident="&
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
!!    init_random_seed_by_dat(3f) - [M_random] seed random_number(3f) with values from date_and_time(3f)
!!##SYNOPSIS
!!
!!    subroutine init_random_seed_by_dat()
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the date_and_time(3f)
!!    intrinsic to initialize the seed so you can easily call
!!    random_number(3f) with varying pseudo-random real number sequences
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
!!            1  0.644704163
!!            2  0.244343698
!!            3  0.516471267
!!            4  0.296542704
!!            5  0.681771278
!!            6  0.449223280
!!            7  0.915870190
!!            8  0.466257989
!!            9  0.912388682
!!           10  0.597788215
!===================================================================================================================================
subroutine init_random_seed_by_dat()

character(len=*),parameter::ident="&
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
!!    init_random_seed(3f) - [M_random] seed random_number(3f) with single value like srand(3c) usage
!!##SYNOPSIS
!!
!!    subroutine init_random_seed(mine)
!!
!!     integer,intent(in) :: mine
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the single given
!!    integer to initialize the seed so you can easily call random_number(3f)
!!    with varying pseudo-random real number sequences simply, much like
!!    srand(3c) and rand(3c).
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
!!            1  0.989341617
!!            2  0.296594143
!!            3  0.805420995
!!            4   4.00894880E-03
!!            5   5.73359132E-02
!!            6  0.805290103
!!            7  0.944527864
!!            8  0.789443851
!!            9  0.327288270
!!           10  0.710926533
!===================================================================================================================================
subroutine init_random_seed(mine)

character(len=*),parameter::ident="&
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
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_random
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
