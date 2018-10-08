!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module sha3mod

  ! This module implements the SHA-3 hash function, according to
  ! FIPS PUB 202, SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions,
  ! a NIST publication.
  !
  ! Originally based on routines from http://alcinoe.net/fortran.html

  ! In this module, we focus on hashing strings of bytes (as opposed to strings
  ! of bits whose length is not a multiple of 8). We also focus on providing
  ! a fixed-length digest, rather than extendable output. For us, bytes mean
  ! integers of kind 1.

  ! There are two ways of using the module: 
  !
  !   - a functional form, in which the whole array of bytes to hash is passed
  !     to a function, which returns an array of bytes:
  !
  !        digest = sha3( buffer, d )
  !
  !     where d is an integer (default kind) that specifies the digest length
  !     in bits (so that 'digest' should have a size of d/8)
  !
  !   - a subroutine form, which is typically used like this:
  !
  !         type(sha3_state) :: S
  !         call sha3_update( S, buffer1, d )
  !         call sha3_update( S, buffer2 )
  !         ...
  !         call sha3_digest( S, digest )
  !     where you pass the data to hash little by little with 'sha3_update', and
  !     finish the process with 'sha3_digest' (after you which can start anew with the
  !     same state)
  !
  ! According to the standard, the digest size d may be one of 224, 256, 384, 512,
  ! which results in arrays of bytes of size 28, 32, 48 and 64. These arrays of
  ! bytes can be converted into a hexadecimal string of length 56, 64, 96 and 128
  ! by calling the 'sha3_hexdigest' function:
  !      hd = sha3_hexdigest( digest )
  ! 
  ! If the data to hash is a string, one may convert it to an array of bytes or
  ! integer(1) using the transfer intrinsic:
  !
  !    buffer = transfer( string, buffer )
  !
  ! where size(buffer) = len(string)
  !
  ! The final routine exported by the module is sha3_auto_test(), which hashes some
  ! test vectors, as found on:
  !      http://www.di-mgt.com.au/sha_testvectors.html
  ! and some files in the directory 'test_vectors', for which the digest was
  ! found using the Python implementation from https://github.com/gvanas/KeccakCodePackage.

  use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64
  implicit none

  private

  ! this is one set of parameters for Keccak (standard one for SHA-3)
  ! with this set of parameters, a lane is encoded with an integer(8) (64 bits)
  integer, parameter :: LANE = 8
  integer, parameter :: W    = 64
  integer, parameter :: ELL  = 6

  integer(LANE), dimension(5,5) :: sbuf

  ! pre-computed values of the RC parameter in function iota
  integer(LANE),parameter,dimension(24) :: RC_C = [ &
      transfer(z'8000000000000000',0_int64), transfer(z'4101000000000000',0_int64), transfer(z'5101000000000001',0_int64), &
      transfer(z'0001000100000001',0_int64), transfer(z'D101000000000000',0_int64), transfer(z'8000000100000000',0_int64), &
      transfer(z'8101000100000001',0_int64), transfer(z'9001000000000001',0_int64), transfer(z'5100000000000000',0_int64), &
      transfer(z'1100000000000000',0_int64), transfer(z'9001000100000000',0_int64), transfer(z'5000000100000000',0_int64), &
      transfer(z'D101000100000000',0_int64), transfer(z'D100000000000001',0_int64), transfer(z'9101000000000001',0_int64), &
      transfer(z'C001000000000001',0_int64), transfer(z'4001000000000001',0_int64), transfer(z'0100000000000001',0_int64), &
      transfer(z'5001000000000000',0_int64), transfer(z'5000000100000001',0_int64), transfer(z'8101000100000001',0_int64), &
      transfer(z'0101000000000001',0_int64), transfer(z'8000000100000000',0_int64), transfer(z'1001000100000001',0_int64) ]

  type sha3_state
     integer :: d ! size of digest in bits
     integer :: c ! capacity in bits
     integer :: r ! rate, in bits
     integer(LANE), dimension(5,5) :: S ! state
     integer(1), dimension(:), pointer :: buffer
     integer                           :: bufsize = -1 ! the number of bytes actually usable in buffer
  end type sha3_state


  public :: sha3, sha3_update, sha3_state, sha3_digest, sha3_hexdigest, sha3_file, sha3_auto_test

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains

!================================================================================
subroutine sha3_file( d, fname, hdigest )
!================================================================================
! hashes a file and either returns the sha3_hexdigest in a string, or display it to
! stdout, along with the file name. d is the digest size in bits (224,256,384,512)
  use M_system, only : system_stat
  integer,                    intent(in)  :: d
  character(len=*),           intent(in)  :: fname
  character(len=*), optional, intent(out) :: hdigest

  integer(1), dimension(d/8)              :: digest

  logical                                 :: fexist
  integer                                 :: fsize, i, j, nread, nrem
  type(sha3_state)                        :: S
  integer(1), dimension(:), allocatable   :: buffer
  integer(kind=int64), dimension(13)      :: values
  character(len=128)                      :: dg

  ! does this file exist? if yes, what is its size?
  inquire( file=trim(adjustl(fname)), exist=fexist )
  if ( .not. fexist ) then
     print *, 'file not found.'
     return
  end if
  call system_stat( trim(fname), values )
  fsize = int(values(8))

  ! read the file into a buffer with the appropriate size
  allocate( buffer(4096) )
  open( unit=39, file=trim(adjustl(fname)), form='unformatted', access='direct', recl=1 )
  nrem = fsize
  j    = 0
  do
     nread = min(nrem,4096)
     do i = 1, nread
        j = j + 1
        read( 39, rec=j ) buffer(i)
     end do
     if ( nread == 4096 ) then
        call sha3_update( S, buffer, d )
     else
        call sha3_update( S, buffer(1:nread), d )
     end if
     nrem = nrem - nread
     if ( nrem <= 0 ) exit
  end do
  close( 39 )

  call sha3_digest( S, digest )
  dg = sha3_hexdigest( digest )
  if ( present(hdigest) ) then
     hdigest = trim(dg)
  else
     print '(3a)', trim(dg), ' ', trim(fname)
  end if

  deallocate( buffer )

end subroutine sha3_file
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

!================================================================================
function sha3_hexdigest( d )
!================================================================================
! returns a digest d (a list of bytes) as an hexadecimal string

  integer(1), dimension(:), intent(in) :: d
  character(len=size(d)*2) :: sha3_hexdigest

  write( sha3_hexdigest, '(100Z2.2)' ) d

end function sha3_hexdigest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_update( state, buffer, d )
!================================================================================
! this routine 

  type(sha3_state),         intent(inout) :: state
  integer(1), dimension(:), intent(in)    :: buffer
  integer, optional,        intent(in)    :: d

  integer, save :: r8
  integer       :: j
  
  if ( state%bufsize == -1 ) then
     ! means we never, ever called sha3_update before, and thus the buffer pointer
     ! in state is in limbo
     nullify( state%buffer )
  end if

  if ( state%bufsize < 0 ) then
     ! means that we start working on a new input
     if ( present(d) ) then
        state%d = d
     else
        state%d = 224
     end if
     if ( state%d == 224 ) then
        state%c = 448
     else if ( state%d == 256 ) then
        state%c = 512
     else if ( state%d == 384 ) then
        state%c = 768
     else if ( state%d == 512 ) then
        state%c = 1024
     else
        ! todo
     end if
     state%r = 25*W - state%c
     ! initialize state
     state%S = 0_LANE
     allocate( state%buffer(state%r / 4 ) )
     state%bufsize = 0 ! buffer allocated, but empty
     r8 = state%r / 8
  end if

  ! in case there was data left in the *state* buffer from a previous call
  ! to sha3_update, we append the received data to it
  if ( state%bufsize > 0 ) then
     ! complete the state buffer
     j = min( size(buffer), r8 - state%bufsize ) ! how many bytes from buffer to use
     state%buffer( state%bufsize+1 : state%bufsize+j ) = buffer(1:j)
     state%bufsize = state%bufsize + j
     if ( state%bufsize >= r8 ) then
        call sha3_block( state%S, state%buffer(1:r8), r8 )
        state%bufsize = 0
        ! hash the remainder of the data (if any)
        do 
           if ( j+r8 >= size(buffer) ) exit
           ! hash this block, w
           call sha3_block( state%S, buffer(j+1:j+r8), r8 )
           ! go to next input block
           j = j + r8
        end do
     else
        return
     end if
  else
     ! hash what we can from buffer
     j = 0
     do
        if ( j+r8 >= size(buffer) ) exit
        ! hash this block, w
        call sha3_block( state%S, buffer(j+1:j+r8), r8 )
        ! go to next input block
        j = j + r8
     end do
  end if

  ! add the remainder to state%buffer:
  ! just accumulate data, because this cannot be hashed without taking
  ! padding into account
  if ( state%bufsize + (size(buffer) - j) > size(state%buffer) ) then
     print *, 'error, buffer is too small ???'
  else
     state%buffer( state%bufsize+1 : state%bufsize+size(buffer)-j ) = buffer( j+1:size(buffer) )
     state%bufsize = state%bufsize + size(buffer) - j
     if ( state%bufsize < 0 ) print *, 'error, buffer size < 0'
  end if

  ! is buffer large enough to process a block ?
  if ( state%bufsize >= r8 ) then
     call sha3_block( state%S, state%buffer(1:r8), r8 )
     ! "resize" buffer
     state%buffer(1:state%bufsize-r8) = state%buffer(r8+1:state%bufsize)
     state%bufsize = state%bufsize - r8
  end if

end subroutine sha3_update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_block( S, buffer, r8 )
!================================================================================
! given a state matrix S, a full buffer of bytes (i.e., r8 bytes), this routine
! absorbs the content of buffer into the sponge.

  integer(LANE), dimension(5,5), intent(inout) :: S
  integer(1),    dimension(:),   intent(in)    :: buffer
  integer,                       intent(in)    :: r8

  integer :: i, k, a, b
  integer(1), dimension(LANE) :: bytes

  a = 1 ; b = 1
  do i = 1, r8 / LANE ! loop on each lane
     do k = 1, LANE ! revert the bytes in each lane
        bytes(9-k) = sha3_reverse( buffer((i-1)*8+k) )
     end do
     ! XOR the message with state
     S(a,b) = ieor( S(a,b), transfer( bytes, S(a,b) ) )
     a = a + 1
     if ( a == 6 ) then
        a = 1 ; b = b + 1
     end if
  end do

  ! apply the sha3_keccak_p function on the state
  do i = 2*ELL + 12 - (2*ELL+12), 2*ELL + 12 - 1
     call sha3_round( S, i )
  end do

end subroutine sha3_block
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_digest( state, digest )
!================================================================================

  type(sha3_state),         intent(inout) :: state
  integer(1), dimension(:), intent(out)   :: digest

  integer :: i, j
  integer(1), dimension(25*LANE) :: string

  ! it remains to apply padding the the current buffer, add this to sponge
  ! apply keccak, and squeeze

  ! the problem may be that, depending on the size of the buffer, we may have
  ! one or two r-bits blocks after padding
  digest = 0_1

  ! proceed to padding. in here, we know that bufsize is strictly less than r/8 bytes
  ! (contrary to the sha3 function)
  i = mod( state%bufsize + 1, state%r/8 ) ! how many bytes to add
  if ( i == 0 ) then
     ! just add one byte for padding, and we have a full block ready to hash
     state%buffer( state%r/8 ) = transfer(b'10000110',state%buffer(1))
  else
     state%buffer( state%bufsize + 1 ) = transfer(b'00000110',state%buffer(1))
     state%buffer( state%bufsize + 2 : state%r/8 - 1 ) = 0_1
     state%buffer( state%r/8 ) = transfer(b'10000000',state%buffer(1))
  end if

  ! absorb this last block...
  call sha3_block( state%S, state%buffer(1:state%r/8), state%r/8 )

  ! ...and squeeze
  if ( state%d < state%r ) then
     ! go back from state matrix to string
     string = sha3_state2string2( state%S, 25*W/8 )
     digest = string(1:state%d/8)
     do i = 1, state%d/8
        digest(i) = sha3_reverse(digest(i))
     end do
  else
     j = 0 ! number of bytes currently outputted
!!$     do
!!$        i = min( r/8, d/8 - j )
!!$        sha3_sponge(j+1:j+i) = S(1:i) ! get r bits from state
!!$        j = j + i ! update the number of bytes outputted
!!$        ! exit when we have enough
!!$        if ( j >= d/8 ) exit
!!$        ! otherwise, continue squeezing
!!$        S = sha3_keccak_p( S, 25*W, 2*ELL+12 )
!!$     end do
  end if

  ! once the digest has been provide, there are some tasks to perform
  ! (reinit the state and deallocation)
  deallocate( state%buffer )
  nullify( state%buffer )
  state%bufsize = -1
  
end subroutine sha3_digest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================


!================================================================================
function sha3( buffer, d )
!================================================================================
! SHA3 can produce variable-length digests, having length d in bits

! we assume that d is a multiple of 8

  integer(1), dimension(:), intent(in) :: buffer
  integer,                  intent(in) :: d ! output length
  integer(1), dimension(d/8) :: sha3

  select case ( d )
     case ( 224 ) ! SHA3 224
        sha3 = sha3_keccak( buffer, 224, 448 )
     case ( 256 ) ! SHA3 256
        sha3 = sha3_keccak( buffer, 256, 512 )
     case ( 384 ) ! SHA3 384
        sha3 = sha3_keccak( buffer, 384, 768 )
     case ( 512 ) ! SHA3 512
        sha3 = sha3_keccak( buffer, 512, 1024 )
     case default
        if ( d > 0 ) then
           sha3 = sha3_keccak( buffer, d, 256 )
        else
           sha3 = sha3_keccak( buffer, -d, 512 )
        end if
  end select

end function sha3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================


!================================================================================
function sha3_keccak( M, d, c )
!================================================================================
  integer(1), dimension(:), intent(in) :: M
  integer,                  intent(in) :: d ! output length of digest
  integer,                  intent(in) :: c ! capacity (distinguishes variants of K)
  integer(1), dimension(d/8) :: sha3_keccak

  ! here, M should have been padded with '1111' in XOF mode, '01' otherwise
  sha3_keccak = sha3_sponge( M, d, 25*W - c )
  
end function sha3_keccak
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
function sha3_sponge( M, d, r )
!================================================================================
  integer(1), dimension(:), intent(in) :: M
  integer,                  intent(in) :: d ! output length of digest
  integer,                  intent(in) :: r ! rate
  integer(1), dimension(d/8) :: sha3_sponge

  integer :: i, c, n, j, k

  integer(1), dimension(25*LANE) :: S        ! state, as a string
  integer(1), dimension(r/8)     :: padding

  ! capacity is b - rate
  c = 25*W - r
  n = 0

  ! 0. PADDING------------------------------------------------------
  ! our goal is to determine 'padding', which is an array of r/8 bytes
  ! that contains the end of the message M plus the required padding
  if ( d == 224 .or. d == 256 .or. d == 384 .or. d == 512 ) then
     ! classic hashing: append '01', plus Pad10*1, such that message
     ! length (in bits) is a multiple of r, or rather, for us, such that
     ! message length in bytes is a multiple of r/8
     i = mod( size(M) + 1, r/8 ) ! how many bytes to add
     if ( i > 0 ) i = r/8 - i
     if ( i == 0 ) then
        ! it's ok to add just one byte
        do j = 1, r/8-1
           padding(j) = sha3_reverse( M(size(M)-(r/8-1)+j) )
        end do
        padding(r/8) = transfer(b'01100001',padding(1))
        n = (size(M) - (r/8-1)) / (r/8)
     else
        padding = 0_int8
        do j = 1, r/8-1-i
           padding(j) = sha3_reverse( M( size(M)-(r/8-1-i)+j ) )
        end do
        padding(r/8-i) = transfer(b'01100000',padding(1))
        padding(r/8)   = transfer(b'00000001',padding(1))
        n = (size(M) - (r/8-1-i)) / (r/8)
     end if
  else
     ! XOF mode: append '1111', plus Pad10*1
     !TODO
  end if

  ! n is the number of r-bits = r/8 bytes blocks in the message that are
  ! not affected by padding. For short messages, n = 0, because the message
  ! *with* padding fits in a single r-bits block (block "padding")

  j = 0      ! indices the sub-block of M that is treated
  S = 0_int8 ! state starts initially full of 0

  if ( n == 0 ) then ! message is sufficiently short to be fully inside padding
     ! initial XOR'd state
     do k = 1, r/8
        S(k) = ieor( S(k), padding(k) )
     end do
  else
     ! 1. ABSORBING----------------------------------------------------
     do i = 1, n
        ! xor S and the next block of input to hash (byte by byte)
        do k = 1, r/8
           S(k) = ieor( S(k), sha3_reverse( M(j+k) ) )
        end do
        ! for the remainder of S, it is xor'd with 0, i.e., unchanged
        j = j + r/8
        S = sha3_keccak_p( S, 25*W, 2*ELL+12 )
     end do
     ! the last block has in general been padded (this last block may be the first!!)
     do k = 1, r/8
        S(k) = ieor( S(k), padding(k) )
     end do
  end if

  ! this is the last
  S = sha3_keccak_p( S, 25*W, 2*ELL+12 )

  ! 2. SQUEEZING---------------------------------------------------
  if ( d < r ) then
     sha3_sponge = S(1:d/8)
  else
     j = 0 ! number of bytes currently outputted
     do
        i = min( r/8, d/8 - j )
        sha3_sponge(j+1:j+i) = S(1:i) ! get r bits from state
        j = j + i ! update the number of bytes outputted
        ! exit when we have enough
        if ( j >= d/8 ) exit
        ! otherwise, continue squeezing
        S = sha3_keccak_p( S, 25*W, 2*ELL+12 )
     end do
  end if

  ! reverse the bytes we output
  do i = 1, d/8
     sha3_sponge(i) = sha3_reverse( sha3_sponge(i) )
  end do

  !print '(a,100(z2.2))', 'sponge = ', sha3_sponge

end function sha3_sponge
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
function sha3_keccak_p( S, b, nr )
!================================================================================
  integer(1), dimension(:), intent(in) :: S  ! input "string"
  integer,                  intent(in) :: b  ! size of input, in bits
  integer,                  intent(in) :: nr ! number of rounds
  integer(1), dimension(b/8) :: sha3_keccak_p

  integer(LANE), dimension(5,5) :: state
  integer :: ir

  ! convert S to state
  state = sha3_string2state( S )

  ! perform rounds
  do ir = 2*ELL + 12 - nr, 2*ELL + 12 - 1
     call sha3_round( state, ir )
  end do

  ! convert from state to string
  sha3_keccak_p = sha3_state2string2( state, b/8 )

end function sha3_keccak_p
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_round( state, round_index )
!================================================================================
  integer(LANE), dimension(5,5), intent(inout) :: state
  integer,                       intent(in)    :: round_index

  ! the five steps of a round are made of the theta, rho, pi, khi and iota steps

  call sha3_theta( state )
  call sha3_rho( state )
  call sha3_pi( state )
  call sha3_khi( state )
  ! iota is simple, no need to call a function for that
  state(1,1) = ieor( state(1,1), RC_C(round_index+1) )

end subroutine sha3_round
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_theta( A )
!================================================================================
  integer(LANE), dimension(5,5), intent(inout)  :: A

  integer(LANE), dimension(5) :: C, D
  integer :: x, y

  do x = 1, 5
     C(x) = ieor( A(x,1), ieor( A(x,2), ieor( A(x,3), ieor( A(x,4), A(x,5) ) ) ) ) 
  end do

  D(1) = ieor( C(5), ishftc( C(2), -1 ) )
  D(2) = ieor( C(1), ishftc( C(3), -1 ) )
  D(3) = ieor( C(2), ishftc( C(4), -1 ) )
  D(4) = ieor( C(3), ishftc( C(5), -1 ) )
  D(5) = ieor( C(4), ishftc( C(1), -1 ) )

  do y = 1, 5
     do x = 1, 5
        A(x,y) = ieor( A(x,y), D(x) )
     end do
  end do

end subroutine sha3_theta
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_rho( A )
!================================================================================
  integer(LANE), dimension(5,5), intent(inout)  :: A

  integer :: x, y, z, t

  x = 1 ; y = 0
  do t = 0, 23
     z = (t+1)*(t+2)/2
     A(x+1,y+1) = ishftc( A(x+1,y+1), -mod( z, 64 ) )
     z = y
     y = mod( 2*x + 3*y, 5 )
     x = z
  end do

end subroutine sha3_rho
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_pi( A )
!================================================================================
  integer(LANE), dimension(5,5), intent(inout)  :: A

  integer(LANE) :: t

  t = A(4,4)
  A(4,4) = A(3,4)
  A(3,4) = A(2,3)
  A(2,3) = A(3,2)
  A(3,2) = A(1,3)
  A(1,3) = A(2,1)
  A(2,1) = A(2,2)
  A(2,2) = A(5,2)
  A(5,2) = A(3,5)
  A(3,5) = A(5,3)
  A(5,3) = A(1,5)
  A(1,5) = A(3,1)
  A(3,1) = A(3,3)
  A(3,3) = A(4,3)
  A(4,3) = A(5,4)
  A(5,4) = A(4,5)
  A(4,5) = A(1,4)
  A(1,4) = A(5,1)
  A(5,1) = A(5,5)
  A(5,5) = A(2,5)
  A(2,5) = A(4,2)
  A(4,2) = A(2,4)
  A(2,4) = A(1,2)
  A(1,2) = A(4,1)
  A(4,1) = t

end subroutine sha3_pi
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_khi( A )
!================================================================================

  integer(LANE), dimension(5,5), intent(inout)  :: A

  integer :: x, y, x1, x2

  sbuf = A

  do x = 1, 5
     x1 = x + 1
     if ( x == 5 ) x1 = 1
     x2 = x + 2
     if ( x2 > 5 ) x2 = x2 - 5
     do y = 1, 5
        A(x,y) = ieor( sbuf(x,y), iand( not( sbuf(x1,y) ), sbuf(x2,y) ) )
     end do
  end do

end subroutine sha3_khi
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
function sha3_reverse(b)
!================================================================================
! reverses the order of the bits in byte b

  integer(1), intent(in) :: b
  integer(1) :: sha3_reverse
  integer(1),parameter :: Z0F=transfer(z'0F',0_int8), &
                          Z33=transfer(z'33',0_int8), &
                          Z55=transfer(z'55',0_int8), &
                          ZAA=transfer(z'AA',0_int8), &
                          ZCC=transfer(z'CC',0_int8), &
                          ZF0=transfer(z'F0',0_int8)

  sha3_reverse = ior( ishft( iand( b, zF0 ), -4 ), ishft( iand( b, z0F ), 4 ) )
  sha3_reverse = ior( ishft( iand( sha3_reverse, zCC ), -2 ), ishft( iand( sha3_reverse, z33 ), 2 ) )
  sha3_reverse = ior( ishft( iand( sha3_reverse, zAA ), -1 ), ishft( iand( sha3_reverse, z55 ), 1 ) )

end function sha3_reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
function sha3_string2state( S )
!================================================================================
! an input string is (in principle) as string of bits of length b, but always
! encoded as an array of bytes (b/8 bytes)
! w/8 consecutive bytes form a lane, and lanes are stored in a state matrix
! in the order  A(1,1)  A(2,1)  A(3,1)  A(4,1)  A(5,1)   A(1,2) ... A(5,5)

  integer(1), dimension(:), intent(in) :: S  ! input "string" as a list of bytes
  integer(LANE), dimension(5,5) :: sha3_string2state

  integer(1), dimension(8) :: reve
  integer :: x, y, z, i

  z = 0
  do y = 1, 5
     do x = 1, 5
        do i = 1, 8
           reve(9-i) = S(z+i)
        end do
        sha3_string2state(x,y) = transfer( reve, sha3_string2state(x,y) )
        z = z + LANE
     end do
  end do

end function sha3_string2state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================


!================================================================================
function sha3_state2string2( S, sz )
!================================================================================
! convert a state S to a string (array) of sz bytes

  integer,                       intent(in) :: sz
  integer(LANE), dimension(5,5), intent(in) :: S
  integer(1), dimension(sz) :: sha3_state2string2  ! input "string" as a list of bytes

  integer(1), dimension(8) :: bytes
  integer :: x, y, z, i

  ! convert S to state
  z = LANE + 1
  do y = 1, 5
     do x = 1, 5
        bytes(1:8) = transfer( S(x,y), bytes(1:8) )
        do i = 1, 8
           sha3_state2string2(z-i) = bytes(i)
        end do
        z = z + LANE
     end do
  end do

end function sha3_state2string2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!================================================================================
subroutine sha3_auto_test()
!================================================================================
  !call sha3_test11()
  call sha3_test21()
  call sha3_test31()
  call sha3_test41()
  call sha3_test51()
  call sha3_test61()
contains
!================================================================================
subroutine sha3_test11()
!================================================================================
  integer(1), dimension(512/8) :: digest
  integer(1), dimension(:), allocatable :: buffer
  type(sha3_state) :: S

  print *
  print *, 'TEST11  : hash empty string'
  print '(a,a128)', '         ', sha3_hexdigest( sha3(buffer,512))
  allocate( buffer(0) )
  call sha3_update( S, buffer, 512 )
  call sha3_digest( S, digest )
  print '(a,a128)', '         ',sha3_hexdigest( digest )
  print '(a,2a128)', '         A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80', &
       'A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26'
end subroutine sha3_test11
!================================================================================
subroutine sha3_test21()
!================================================================================
  character(len=1024) :: m
  integer(1), dimension(224/8) :: digest
  integer(1), dimension(:), allocatable :: buffer
  type(sha3_state) :: S

  print *
  print *, 'TEST21  : hash "abc"'
  m = 'abc'
  allocate( buffer(len_trim(m)) )
  buffer = transfer( trim(m), buffer )
  print *, '        ', sha3_hexdigest( sha3( buffer, 224 ) )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  print *, '        ', sha3_hexdigest( digest )
  print *, '        E642824C3F8CF24AD09234EE7D3C766FC9A3A5168D0C94AD73B46FDF'

  deallocate( buffer )

end subroutine sha3_test21
!================================================================================
subroutine sha3_test31()
!================================================================================
  character(len=1024) :: m
  integer(1), dimension(224/8) :: digest
  integer(1), dimension(:), allocatable :: buffer
  type(sha3_state) :: S

  print *
  print *, 'TEST31  : hash "abc...stu"'

  m = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  allocate( buffer(len_trim(m)) )
  buffer = transfer( trim(m), buffer )
  print *, '        ', sha3_hexdigest( sha3( buffer, 224 ) )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  print *, '        ', sha3_hexdigest( digest )
  print *, '        543E6868E1666C1A643630DF77367AE5A62A85070A51C14CBF665CBC'

  deallocate( buffer )

end subroutine sha3_test31
!================================================================================
subroutine sha3_test41()
!================================================================================
  integer, parameter :: N = 1000*1000
  integer, parameter :: M = 100
  integer(1), dimension(224/8) :: digest
  integer(1), dimension(:), allocatable :: buffer
  type(sha3_state) :: S
  integer :: i, j
  real :: t1, t2, d1, d2, d3

  print *
  print *, 'TEST41  : hash "a"*',N

  allocate( buffer(N) )
  do i = 1, N
     buffer(i) = 97_int8
  end do

  call cpu_time( t1 )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  call cpu_time( t2 )
  d1 = t2 - t1
  print *, '        ', sha3_hexdigest( digest )
  call cpu_time( t1 )
  digest = sha3( buffer, 224 )
  call cpu_time( t2 )
  d2 = t2 - t1
  ! now provide it in small packets
  call cpu_time( t1 )
  j = 0
  do i = 1, N/M
     call sha3_update( S, buffer(j+1:j+M) )
     j = j + M
  end do
  call sha3_digest( S, digest )
  call cpu_time( t2 )
  d3 = t2 - t1
  print *, '        ', sha3_hexdigest( digest )
  print *, '        D69335B93325192E516A912E6D19A15CB51C6ED5C15243E7A7FD653C'
  deallocate( buffer )

  !print *, 'timings: ', d1, d2, d3

  !call sha3_file( 'sha3.f90', 224, digest )

end subroutine sha3_test41
!================================================================================
subroutine sha3_test51()
!================================================================================
  integer               :: i, j
  character(len=128)    :: digest, fname, fname2
  character(len=256)    :: line
  integer, dimension(4) :: dv, mds

  dv = (/ 224, 256, 384, 512 /)
  mds = (/ 56, 64, 96, 128 /)

  print *
  print *, 'TEST 51 : hash files and compare digests with reference'

  ! loop on test vectors
  do i = 1, 5
     write( fname2, '(a,i3.3,a)' ) 'test_vectors/test_', i, '.digests'
     open( unit=12, file=trim(fname2) )
     print *, '   file #', i
     ! loop on SHA3 variant
     do j = 1, 4
        write( fname, '(a,i3.3,a)' ) 'test_vectors/test_', i, '.msg'
        call sha3_file( dv(j), fname, digest )
        write( *, '(10x,i3,1x,a)' ) dv(j), trim(digest)
        read( 12, '(a)' ) line
        write( *, '(10x,a)' ) trim(line)
        print *
     end do
     close( 12 )
     print *
  end do

end subroutine sha3_test51
!================================================================================
subroutine sha3_test61()
!================================================================================
  integer, parameter :: N = 100*1024*1024
  integer(1), dimension(224/8) :: digest
  integer(1), dimension(:), allocatable :: buffer
  type(sha3_state) :: S
  integer :: i
  real :: t1, t2, d1

  print *
  print *, 'TEST61  : speed test (hash 100 MiB)'

  allocate( buffer(N) )
  do i = 1, N
     buffer(i) = 97_int8
  end do

  call cpu_time( t1 )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  call cpu_time( t2 )
  d1 = t2 - t1
  print *, '        ', sha3_hexdigest( digest )

  print *, 'timings: ', d1, 's'
  deallocate( buffer )

end subroutine sha3_test61

end subroutine sha3_auto_test
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module sha3mod
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

program ktest

  use sha3mod

  character(len=128) :: fname, arg

  call get_command_argument( 1, arg )
  if ( arg(1:1) .eq. '-' ) then
     if ( trim(arg) .eq. '-a' ) then
        call sha3_auto_test()
     else
        call get_command_argument( 2, fname )
        if ( trim(arg) .eq. '-224' ) then
           call sha3_file( 224, trim(fname) )
        else if ( trim(arg) .eq. '-256' ) then
           call sha3_file( 256, trim(fname) )
        else if ( trim(arg) .eq. '-384' ) then
           call sha3_file( 384, trim(fname) )
        else if ( trim(arg) .eq. '-512' ) then
           call sha3_file( 512, trim(fname) )
        else
           print *, 'usage: sha3 -a     or    sha3 (-224|-256|-384|-512) fname'
        end if
     end if
  else
     print *, 'usage: sha3 -a     or    sha3 (-224|-256|-384|-512) fname'
  end if

end program ktest
