!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_hashkeys
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
use,intrinsic ::  iso_c_binding
use M_anything,                   only : anything_to_bytes
use M_debug,                      only : debug
implicit none
integer,parameter :: int128 = selected_real_kind(2*precision(1.0_int64))
private

! key hash
public b3hs_hash_key_jenkins

! cyclic redundancy check
public crc32_hash
interface crc32_hash
   module procedure crc32_hash_arr
   module procedure crc32_hash_scalar
end interface crc32_hash
!
! string hashes
!
! bucket hash
public  int128

public  djb2

public  djb2_hash    ! this string hash algorithm written in C was first reported by Dan J. Bernstein many years ago in comp.lang.c.
interface djb2_hash
   module procedure djb2_hash_arr
   module procedure djb2_hash_scalar
end interface djb2_hash

public  sdbm_hash

interface sdbm_hash
   module procedure sdbm_hash_arr
   module procedure sdbm_hash_scalar
end interface sdbm_hash

! WARNING: because there is currently no unsigned INTEGER in standard Fortran, use 128-bit INTEGER, which is not always available
! WARNING: not tested, but almost certainly get different results with different Endians

!-----------------------------------------------------------------------------------------------------------------------------------
! Defines the public interface for sha256
public sha256
public dirty_sha256
! Public for the sake of unit-testing.
public test_suite_sha256
private sha256b
private ms0
private ms1
private cs0
private cs1
private maj
private ch
private swap32
private swap64
private swap64a
private consume_chunk

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!
!!    For the sha256 and dirty_sha256 procedures and supporting private routines:
!!
!!    Copyright (c) 2014 Mikael Leetmaa
!!
!!    This software is provided 'as-is', without any express or implied
!!    warranty. In no event will the authors be held liable for any damages
!!    arising from the use of this software.
!!
!!    Permission is granted to anyone to use this software for any purpose,
!!    including commercial applications, and to alter it and redistribute it
!!    freely, subject to the following restrictions:
!!
!!       1. The origin of this software must not be misrepresented; you must not
!!          claim that you wrote the original software. If you use this software
!!          in a product, an acknowledgment in the product documentation would be
!!          appreciated but is not required.
!!
!!       2. Altered source versions must be plainly marked as such, and must not be
!!          misrepresented as being the original software.
!!
!!       3. This notice may not be removed or altered from any source
!!          distribution.
!===================================================================================================================================
!>
!!##NAME
!!    sha256(3f) - [M_hashkeys] generate a SHA-256 hashing
!!
!!##SYNOPSIS
!!
!!   function sha256(str)
!!
!!    character(len=64)            :: sha256
!!    character(len=*), intent(in) :: str
!!
!!##DESCRIPTION
!!
!!    A Fortran module for SHA-256 hashing.
!!
!!    Note that this code will not produce the same results on big-endian
!!    machines and the module was only tested on a little-endian Ubuntu LTS
!!    12.04 system using gfortran 4.6.3.
!!
!!##OPTIONS
!!    str      The message to digest.
!!
!!##RETURNS
!!    sha256   The SHA-256 digest as a string of length 64.
!!
!!##COMPILE NOTES
!!
!!    The '-fno-range-check' flag is required on gfortran(1) since the
!!    Fortran standard otherwise doesn't currently allow us to work with
!!    all bits in the integers (as if they were unsigned).
!!
!!##AUTHOR
!!
!!    This routine is heavily based on the SHA-256 routines by
!!    Mikael Leetmaa <leetmaa@kth.se>, 2014-01-05. changes have
!!    been made to incorporate it into the GPF (General Purpose Fortran)
!!    framework.
!!
!!    If you found this useful, please let Mikael Leetmaa know.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sha256
!!    use M_hashkeys, only : sha256, dirty_sha256
!!    implicit none
!!    character(len=:),allocatable :: str
!!    character(len=64)            :: ref
!!
!!    ! Test the sha256 function with a set of reference strings.
!!
!!    str=""
!!    ref="E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"
!!    call unit_check(sha256(str)==ref,'test sha256 1')
!!
!!    str="abc"
!!    ref="BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"
!!    call unit_check(sha256(str)==ref,'test sha256 2')
!!
!!    str="abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
!!    ref="248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1"
!!    call unit_check(sha256(str)==ref,'test sha256 3')
!!
!!    str="abcdefghbcdefghicdefghijdefghijkefghijklfghi&
!!         &jklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
!!    ref="CF5B16A778AF8380036CE59E7B0492370B249B11E8F07A51AFAC45037AFEE9D1"
!!    call unit_check(sha256(str)==ref,'test sha256 4')
!!
!!    str=repeat("a",1000000)
!!    ref="CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0"
!!    call unit_check(sha256(str)==ref,'test sha256 5')
!!
!!    str="message digest"
!!    ref="F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650"
!!    call unit_check(sha256(str)==ref,'test sha256 6')
!!
!!    str="secure hash algorithm"
!!    ref="F30CEB2BB2829E79E4CA9753D35A8ECC00262D164CC077080295381CBD643F0D"
!!    call unit_check(sha256(str)==ref,'test sha256 7')
!!
!!    str="SHA256 is considered to be safe"
!!    ref="6819D915C73F4D1E77E4E1B52D1FA0F9CF9BEAEAD3939F15874BD988E2A23630"
!!    call unit_check(sha256(str)==ref,'test sha256 8')
!!
!!    str="For this sample, this 63-byte string will be used as input data"
!!    ref="F08A78CBBAEE082B052AE0708F32FA1E50C5C421AA772BA5DBB406A2EA6BE342"
!!    call unit_check(sha256(str)==ref,'test sha256 9')
!!
!!    str="This is exactly 64 bytes long, not counting the terminating byte"
!!    ref="AB64EFF7E88E2E46165E29F2BCE41826BD4C7B3552F6B382A9E7D3AF47C245F8"
!!    call unit_check(sha256(str)==ref,'test sha256 10')
!!
!!    ! Check the quick and dirty implementation as well.
!!    ref="69E3FACD5F08321F78117BD53476E5321845433356F106E7013E68EC367F3017"
!!    call unit_check(dirty_sha256(str)==ref,'test dirtysha256 1')
!!
!!    !!str=repeat("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno",16777216)
!!    !!ref="50E72A0E26442FE2552DC3938AC58658228C0CBFB1D2CA872AE435266FCD055E"
!!    !!call unit_check(sha256(str)==ref,'test sha256 11 -- long test')
!!
!!    contains
!!    subroutine unit_check(test,message)
!!    logical,intent(in)          :: test
!!    character(len=*),intent(in) :: message
!!       write(*,'(a)') repeat("=",64)
!!       write(*,'(a)') sha256(str)
!!       write(*,'(a)') ref
!!       if(test)then
!!          write(*,*)"PASSED: ",trim(message)
!!       else
!!          write(*,*)"FAILED: ",trim(message)
!!       endif
!!    end subroutine unit_check
!!    !
!!    end program demo_sha256
!!
!!##UNIT TEST
!!
!!   When porting to a new programming environment use the
!!   built-in unit test ...
!!
!!    program test_sha256
!!    use M_hashkeys, only : test_suite_sha256
!!       call test_suite_sha256()
!!    end program test_sha256
!===================================================================================================================================
function sha256(str)
implicit none

character(len=*),parameter::ident="@(#)M_hashkeys::sha256(3f): SHA-256 interface function"

! Define the interface.
character(len=64) :: sha256           ! The SHA-256 digest as a string of length 64.
character(len=*), intent(in) :: str   ! (in) The message to digest.
! Call the work horse with proper bit swapping.
      sha256 = sha256b(str, 1)
end function sha256
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dirtys_sha256(3f) - [M_hashkeys] generate a SHA-256 hashing
!!##SYNOPSIS
!!
!!   function dirtys_sha256(str)
!!
!!    character(len=64)            :: dirtys_sha256
!!    character(len=*), intent(in) :: str
!!
!!
!!##DESCRIPTION
!!
!!    A Fortran module for SHA-256 hashing.
!!
!!    The quick and dirty routine (dirtys_sha256(3f)) operates on whatever
!!    bits that come in, without swapping to big-endian words, and does
!!    therefore not pass any of the standard tests - but works at roughly
!!    twice the speed. Use this if you want a good hash function but don't
!!    care about following the SHA-256 standard specifications.
!!
!!    Note that this code will not produce the same results on big-endian
!!    machines and the module was only tested on a little-endian Ubuntu
!!    LTS 12.04 system using gfortran 4.6.3 and CygWin using Gortran 7.3.0.
!!
!!##OPTIONS
!!    str      The message to digest.
!!
!!##RETURNS
!!    dirtys_sha256   The SHA-256 digest as a string of length 64.
!!
!!##AUTHOR
!!
!!    This routine is heavily based on the SHA-256 routines by Mikael Leetmaa
!!    <leetmaa@kth.se>, 2014-01-05. changes have been made to incorporate
!!    it into the GPF (General Purpose Fortran) framework.
!!
!!    If you found this useful, please let Mikael Leetmaa know.
!!
!!##EXAMPLES
!!
!!    Using slurp(3f) and switch(3f) from the GPF (General Purpose Fortran)
!!    collection to read in a file and convert it into a string, generate
!!    digest values for a list of files. Note that this example reads the
!!    entire input file into memory twice, and so requires very large
!!    amounts of memory if very large files are processed.
!!
!!     program demo_dirty_sha256
!!     use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
!!     use M_hashkeys,                   only : sha256, dirty_sha256
!!     use M_io,                         only : slurp
!!     use M_strings,                    only : switch
!!     implicit none
!!     character(len=1),allocatable :: text(:) ! array to hold file in memory
!!     character(len=:),allocatable :: string
!!     integer                      :: i
!!     character(len=4096)          :: filename
!!        do i=1,command_argument_count()  ! step through filenames on command line
!!           call get_command_argument(i, filename)
!!           call slurp(filename,text) ! allocate character array and copy file into it
!!           if(.not.allocated(text))then
!!              write(ERROR_UNIT,*)'*rever* ERROR: failed to load file '//trim(filename)
!!           else
!!              string=switch(text) ! switch array to a single character variable
!!              deallocate(text)    ! release memory
!!              write(*,*)dirty_sha256(string),len(string),trim(filename) ! write digest value
!!           endif
!!        enddo
!!     end program demo_dirty_sha256
!!
!!   Sample output:
!!
!!     FA9D11011034F1081A367D4F2F1EB909AC0849FF090A9320B6824156C5628DFD        2011 dynamic_dummy_arrays.f90
!!     FE48473BC7B9C13067EC2C108CB8A650A186605D5F905736D9CB9DE76E9A1A21        5444 fspiro.f90
!!     306CDB5BB2A8C30C711FA5D35A6A12F4FDB4F003ED77438E922B56BBA1024F49       27108 pprint.f90
!===================================================================================================================================
function dirty_sha256(str)
implicit none

character(len=*),parameter::ident="@(#)M_hashkeys::dirty_sha256(3f): Quick and dirty SHA-256 interface function (no bit-swapping)."

! Define the interface.
character(len=64) :: dirty_sha256   ! The SHA-256 digest as a string of length 64.
character(len=*), intent(in) :: str ! The message to digest.
! Call the work horse - no bit swapping.
   dirty_sha256 = sha256b(str, 0)
end function dirty_sha256
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha256b(str, swap)
   !  Calculate the SHA-256 hash of the incoming string.
   implicit none
   ! -----------------------------------
   ! Define the interface.
   character(len=64)            :: sha256b  ! return the SHA-256 digest as a string of length 64.
   character(len=*), intent(in) :: str      ! The message to take digest.
   integer,          intent(in) :: swap     ! Flag to indicate if swapping to big-endian input (swap=1) should be used.
                                            ! swap=1 is needed for the routine to pass the standard tests,
                                            ! but decreases speed with a factor 2.
   ! -----------------------------------
   ! Helper variables.
   integer(kind=c_int64_t) :: length
   integer(kind=c_int32_t) :: temp1
   integer(kind=c_int32_t) :: temp2
   integer(kind=c_int32_t) :: i
   integer :: break
   integer :: pos0

   ! Parameters for the cruncher.
   integer(kind=c_int64_t),parameter :: h0_ref(8)= [&
   & z'6a09e667', z'bb67ae85', z'3c6ef372', z'a54ff53a', z'510e527f', z'9b05688c', z'1f83d9ab', z'5be0cd19']
   integer(kind=c_int64_t),parameter :: k0_ref(64)=[ &
   & z'428a2f98', z'71374491', z'b5c0fbcf', z'e9b5dba5', z'3956c25b', z'59f111f1', z'923f82a4', z'ab1c5ed5',&
   & z'd807aa98', z'12835b01', z'243185be', z'550c7dc3', z'72be5d74', z'80deb1fe', z'9bdc06a7', z'c19bf174',&
   & z'e49b69c1', z'efbe4786', z'0fc19dc6', z'240ca1cc', z'2de92c6f', z'4a7484aa', z'5cb0a9dc', z'76f988da',&
   & z'983e5152', z'a831c66d', z'b00327c8', z'bf597fc7', z'c6e00bf3', z'd5a79147', z'06ca6351', z'14292967',&
   & z'27b70a85', z'2e1b2138', z'4d2c6dfc', z'53380d13', z'650a7354', z'766a0abb', z'81c2c92e', z'92722c85',&
   & z'a2bfe8a1', z'a81a664b', z'c24b8b70', z'c76c51a3', z'd192e819', z'd6990624', z'f40e3585', z'106aa070',&
   & z'19a4c116', z'1e376c08', z'2748774c', z'34b0bcb5', z'391c0cb3', z'4ed8aa4a', z'5b9cca4f', z'682e6ff3',&
   & z'748f82ee', z'78a5636f', z'84c87814', z'8cc70208', z'90befffa', z'a4506ceb', z'bef9a3f7', z'c67178f2']

   ! Work areas.
   integer(kind=c_int32_t) :: h0(8)
   integer(kind=c_int32_t) :: k0(64)
   integer(kind=c_int32_t) :: a0(8)
   integer(kind=c_int32_t) :: w0(64)

   h0 = transfer(h0_ref,h0(1))
   k0 = transfer(k0_ref,k0(1))
   ! -----------------------------------
   ! Function body implementation.

   break  = 0
   pos0   = 1
   length = len(trim(str))

   do while (break .ne. 1)

      ! Get the next 16 32bit words to consume.
      call consume_chunk(str, length, w0(1:16), pos0, break, swap)

      ! Extend the first 16 words to fill the work schedule array.
      do i=17,64
         w0(i) = ms1(w0(i-2)) + w0(i-16) + ms0(w0(i-15)) + w0(i-7)
      enddo

      ! Initialize the workin variables with the current version of the hash.
      a0 = h0

      ! Run the compression loop.
      do i=1,64

         temp1 = a0(8) + cs1(a0(5)) + ch(a0(5),a0(6),a0(7)) + k0(i) + w0(i)
         temp2 = cs0(a0(1)) + maj(a0(1),a0(2),a0(3))

         a0(8) = a0(7)
         a0(7) = a0(6)
         a0(6) = a0(5)
         a0(5) = a0(4) + temp1
         a0(4) = a0(3)
         a0(3) = a0(2)
         a0(2) = a0(1)
         a0(1) = temp1 + temp2

      enddo

      ! Update the state.
      h0 = h0 + a0

   enddo

   ! Write the result to the output variable.
   write(sha256b,'(8z8.8)') h0(1), h0(2), h0(3), h0(4), h0(5), h0(6), h0(7), h0(8)

end function sha256b
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> Swap the byte order on a 32bit integer.
   !! @param inp : (in) The integer to byte swap.
   !! @return    : The byte swapped integer.
   function swap32(inp)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: swap32
      integer(kind=c_int32_t), intent(in)  :: inp
      ! -----------------------------------
      call mvbits(inp, 24, 8, swap32,  0)
      call mvbits(inp, 16, 8, swap32,  8)
      call mvbits(inp,  8, 8, swap32, 16)
      call mvbits(inp,  0, 8, swap32, 24)
   end function swap32
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> Swap the byte order on a 64 bit integer.
   !! @param inp : (in) The integer to byte swap.
   !! @return    : The byte swapped integer.
   function swap64(inp)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int64_t) :: swap64
      integer(kind=c_int64_t), intent(in)  :: inp
      ! -----------------------------------
      call mvbits(inp, 56, 8, swap64,  0)
      call mvbits(inp, 48, 8, swap64,  8)
      call mvbits(inp, 40, 8, swap64, 16)
      call mvbits(inp, 32, 8, swap64, 24)
      call mvbits(inp, 24, 8, swap64, 32)
      call mvbits(inp, 16, 8, swap64, 40)
      call mvbits(inp,  8, 8, swap64, 48)
      call mvbits(inp,  0, 8, swap64, 56)
   end function swap64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> Swap the byte order on a 64bit integer as if
   !! each half was a 32bit integer to swap.
   !! @param inp : (in) The integer to byte swap.
   !! @return    : The byte swapped integer.
   function swap64a(inp)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int64_t) :: swap64a
      integer(kind=c_int64_t), intent(in)  :: inp
      ! -----------------------------------
      call mvbits(inp,  0, 8, swap64a, 32)
      call mvbits(inp,  8, 8, swap64a, 40)
      call mvbits(inp, 16, 8, swap64a, 48)
      call mvbits(inp, 24, 8, swap64a, 56)
      call mvbits(inp, 32, 8, swap64a,  0)
      call mvbits(inp, 40, 8, swap64a,  8)
      call mvbits(inp, 48, 8, swap64a, 16)
      call mvbits(inp, 56, 8, swap64a, 24)
   end function swap64a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> The 'ch' function in SHA-2.
   !! @param a : (in) The a input integer.
   !! @param b : (in) The b input integer.
   !! @param c : (in) The c input integer.
   !! @return  : ch(a,b,c), see the code.
   function ch(a, b, c)
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: ch
      integer(kind=c_int32_t), intent(in) :: a
      integer(kind=c_int32_t), intent(in) :: b
      integer(kind=c_int32_t), intent(in) :: c
      ! -----------------------------------
      ch = ieor(iand(a, b), (iand(not(a), c)))
   end function ch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   function maj(a, b, c)
   !> The 'maj' function in SHA-2.
   !! @param a : (in) The a input integer.
   !! @param b : (in) The b input integer.
   !! @param c : (in) The c input integer.
   !! @return  : maj(a,b,c), see the code.
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: maj
      integer(kind=c_int32_t), intent(in) :: a
      integer(kind=c_int32_t), intent(in) :: b
      integer(kind=c_int32_t), intent(in) :: c
      ! -----------------------------------
      maj = ieor(iand(a, b), ieor(iand(a, c), iand(b, c)))
   end function maj
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> The '\Sigma_0' function in SHA-2.
   !! @param a : (in) The a input integer.
   !! @return  : cs0(a), see the code.
   function cs0(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: cs0
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      cs0 = ieor(ishftc(a, -2), ieor(ishftc(a, -13), ishftc(a, -22)))
   end function cs0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> The '\Sigma_1' function in SHA-2.
   !! @param a : (in) The a input integer.
   !! @return  : cs1(a), see the code.
   function cs1(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: cs1
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      cs1 = ieor(ishftc(a, -6), ieor(ishftc(a, -11), ishftc(a, -25)))
   end function cs1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> The '\sigma_0' function in SHA-2.
   !! @param a : (in) The a input integer.
   !! @return  : ms0(a), see the code.
   function ms0(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: ms0
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      ms0 = ieor(ishftc(a, -7), ieor(ishftc(a, -18), ishft(a, -3)))
   end function ms0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> The '\sigma_1' function in SHA-2.
   !! @param a : (in) The a input integer.
   !! @return  : ms1(a), see the code.
   function ms1(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: ms1
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      ms1 = ieor(ishftc(a, -17), ieor(ishftc(a, -19), ishft(a, -10)))
   end function ms1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   !> Copy 16 32bit words of data from str(pos0) to inp(1:16). The
   !! data is padded as required by the SHA-256 algorithm.
   !! @param str    : (in) The message to take a chunk from.
   !! @param length : (in) The length of the message in 8bit words.
   !! @param inp    : (inout) The work area to copy the data to.
   !! @param pos0   : (inout) Variable to store the start of the next chunk.
   !! @param break  : (inout) Indicates the position in the work flow.
   !!                 break=0 on entry -> continue to consume a chunk, pad if needed.
   !!                 break=2 on entry -> continue to consume, padding was allready done.
   !!                 break=1 one exit -> the last chunk was consumed.
   !! @param swap   : (in) Flag to indicate if swapping to big-endian
   !!                 input (swap=1) should be used. swap=1 is needed
   !!                 for the routine to pass the standard tests, but
   !!                 decreases speed with a factor 2.
   subroutine consume_chunk(str, length, inp, pos0, break, swap)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      character(len=*),        intent(in)    :: str
      integer(kind=c_int64_t), intent(in)    :: length
      integer(kind=c_int32_t), intent(inout) :: inp(*)
      integer,                 intent(inout) :: pos0
      integer,                 intent(inout) :: break
      integer,                 intent(in)    :: swap
      ! -----------------------------------
      ! Internal variables.
      character(len=4)        :: last_word
      integer(kind=c_int64_t) :: rest
      integer(kind=c_int32_t) :: to_pad
      integer(kind=c_int32_t) :: leftover
      integer(kind=c_int32_t) :: space_left
      integer(kind=c_int32_t),parameter :: zero= b'00000000000000000000000000000000'
      integer(kind=c_int8_t),parameter  :: ipad0=transfer( b'00000000', 0_c_int8_t)
      integer(kind=c_int8_t),parameter  :: ipad1=transfer( b'10000000', 0_c_int8_t)
      integer(kind=c_int8_t)  :: i

      ! Calculate the rest.
      rest = length - pos0 + 1

      ! If we are far from the end.
      if (rest .ge. 64) then

         ! Copy the data over.
         inp(1:16) = transfer(str(pos0:pos0+64-1), inp(1:16))

         ! Big-endian.
         if (swap .eq. 1) then
            do i=1,16
               inp(i) = swap32(inp(i))
            enddo
         endif
         pos0 = pos0 + 64                ! Increment the starting position for the next roundx.
      else
         space_left = 16                 ! Space left in the input chunk.
         leftover   = rest/4             ! number of leftover full 32bit words.

         ! Copy any leftovers.
         if (leftover .gt. 0) then
            inp(1:leftover) = transfer(str(pos0:pos0+leftover*4-1), inp(1:16))

            ! Big-endian.
            if (swap .eq. 1) then
               do i=1,leftover
                  inp(i) = swap32(inp(i))
               enddo
            endif

            ! Increment the starting position.
            pos0 = pos0 + leftover*4
            rest = length - pos0 + 1
            space_left = space_left - leftover

         endif

         if (space_left .gt. 0) then

            if (break .ne. 2) then
               ! Add any remaining incomplete 32bit word.
               if (rest .gt. 0) then
                  last_word(1:rest) = str(pos0:pos0+rest-1)
                  pos0 = pos0 + rest                                             ! Increment the pos0.
               endif

               last_word(rest+1:rest+1) = transfer(ipad1, last_word(1:1))        ! Add the '10000000' padding.
               to_pad = 4 - rest - 1                                             ! Add zeros for a full 32bit word.
               do i=1,to_pad
                  last_word(rest+1+i:rest+1+i) = transfer(ipad0, last_word(1:1))
               enddo
               inp(17-space_left) = transfer(last_word(1:4), inp(1))             ! Copy the last full (padded) word over.
               if (swap .eq. 1) then
                  inp(17-space_left) = swap32(inp(17-space_left))
               endif
               space_left = space_left - 1                                       ! Decrement the space left.
               break = 2                                                         ! Set the flag to indicate that we have padded.

            endif

            if (space_left .eq. 1) then                                          ! If not enough space to finish, add zeros.
               inp(16) = zero
               space_left = 0
            endif
            rest = 0

         endif

         if ((rest .eq. 0) .and. (space_left .ge. 2)) then           ! Continue with the last part if there is enough space left.
            do while (space_left .gt. 2)                             ! Add zeros until 64 bits left.
               inp(17-space_left) = zero
               space_left = space_left - 1
            enddo
            inp(15:16) = transfer(swap64a(length*8), inp(15:16))     ! Add the two last 32bit words.
            break = 1                                                ! Set break flag indicating we are done with the whole message.
         endif

      endif

   end subroutine consume_chunk
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! TEST SUITE FOR THE bitsy SHA-256 FORTRAN IMPLEMENTATION
! Author: Mikael Leetmaa
! Date:   05 Jan 2014
subroutine test_suite_sha256()
use M_debug, only : unit_check

implicit none

integer(kind=int32),parameter :: ipad1              =transfer(b'00000000000000000000000000000011',0_int32)
integer(kind=int32),parameter :: ipad2              =transfer(b'11111111111111111111111111111111',0_int32)
integer(kind=int32),parameter :: ipad3              =transfer(b'10010000101001110011001110010011',0_int32)
integer(kind=int32),parameter :: ipad4              =transfer(b'11001001101001110011001110010011',0_int32)
integer(kind=int32),parameter :: ipad5              =transfer(b'10000001101001010011000110100001',0_int32)
integer(kind=int32),parameter :: ipad6              =transfer(b'11000000000000000000000000000000',0_int32)

integer(kind=int32),parameter :: shftc4_r2          =transfer(b'11110010011010011100110011100100',0_int32)
integer(kind=int32),parameter :: shftc4_l12         =transfer(b'01110011001110010011110010011010',0_int32)
integer(kind=int32),parameter :: shft5_r8           =transfer(b'00000000100000011010010100110001',0_int32)
integer(kind=int32),parameter :: shft5_l11          =transfer(b'00101001100011010000100000000000',0_int32)
integer(kind=int32),parameter :: abc_bin            =transfer(b'00000001011000110110001001100001',0_int32)
integer(kind=int32),parameter :: a_bin              =transfer(b'00000000000000000000000101100001',0_int32)
integer(kind=int32),parameter :: empty_str_bin      =transfer(b'00000000000000000000000000000001',0_int32)
integer(kind=int32),parameter :: empty_bin          =transfer(b'00000000000000000000000000000000',0_int32)
integer(kind=int32),parameter :: abc_bin_flip       =transfer(b'01100001011000100110001110000000',0_int32)
integer(kind=int32),parameter :: empty_str_bin_flip =transfer(b'10000000000000000000000000000000',0_int32)
integer(kind=int32),parameter :: a_bin_flip         =transfer(b'01100001100000000000000000000000',0_int32)

integer(kind=int32),parameter :: abca_bin           =transfer(b'01100001011000110110001001100001',0_int32)
integer(kind=int32),parameter :: bcab_bin           =transfer(b'01100010011000010110001101100010',0_int32)
integer(kind=int32),parameter :: cabc_bin           =transfer(b'01100011011000100110000101100011',0_int32)
integer(kind=int32),parameter :: ca_one_zero        =transfer(b'00000000000000010110000101100011',0_int32)
integer(kind=int32),parameter :: big_endian_464     =transfer(b'11010000000000010000000000000000',0_int32)
integer(kind=int32),parameter :: little_endian_464  =transfer(b'00000000000000000000000111010000',0_int32)

integer(kind=int32),parameter :: abc_bin_ref        =transfer(b'00000001011000110110001001100001',0_int32)
integer(kind=int32),parameter :: abc_bin_swap       =transfer(b'01100001011000100110001100000001',0_int32)

integer(kind=int32),parameter :: abca_bin_flip      =transfer(b'01100001011000100110001101100001',0_int32)
integer(kind=int32),parameter :: bcab_bin_flip      =transfer(b'01100010011000110110000101100010',0_int32)
integer(kind=int32),parameter :: cabc_bin_flip      =transfer(b'01100011011000010110001001100011',0_int32)
integer(kind=int32),parameter :: ca_one_zero_flip   =transfer(b'01100011011000011000000000000000',0_int32)

   call test_swap32
   call test_ishftc
   call test_ishft
   call pad_message1
   call pad_message2
   call test_ch
   call test_maj
   call test_cs0
   call test_cs1
   call test_ms0
   call test_ms1
   call test_sha256_1
   call test_sha256_5
   call test_sha256_6
   call test_sha256_11
contains
! Test the swap function.
   subroutine test_swap32
      call unit_check('swap32',swap32(abc_bin)==abc_bin_swap,'test swap32 function')
      call unit_check('swap32',abc_bin==abc_bin_ref,'test swap value')
   end subroutine test_swap32

   subroutine test_ishftc
! Make sure the intrinsic ishftc function does what we think.
      integer(kind=int32) :: a
      a = ishftc(ipad4, -2)
      call unit_check('ishftc',a==shftc4_r2,'verify ishftc A')
      a = ishftc(ipad4, 12)
      call unit_check('ishftc',a==shftc4_l12,'verify ishftc B')
   end subroutine test_ishftc

! Make sure the intrinsic ishft function does what we think.
   subroutine test_ishft
      integer(kind=int32) :: a
      a = ishft(ipad5, -8)
      call unit_check('ishft',a==shft5_r8,'verify ishft A')
      a = ishft(ipad5, 11)
      call unit_check('ishft',a==shft5_l11,'verify ishft B')
   end subroutine test_ishft

! Test the message padding.
   subroutine pad_message1
      character(len=1000) :: str
      integer(kind=int32) :: inp(16)
      integer(kind=8) :: length
      integer :: pos0, break
      integer :: swap = 1

      ! Set the message to "".
      str = ""
      pos0   = 1
      break  = 0
      length = 0
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the first word.
      call unit_check('',inp(1)==empty_str_bin_flip,'message padding A')

      ! Set the message to "abc".
      str = "abc"
      pos0   = 1
      break  = 0
      length = 3
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the first word.
      call unit_check('',inp(1)==abc_bin_flip,'message padding B')

      ! Set the message to "a".
      str = "a"
      pos0   = 1
      break  = 0
     length = 1
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the first word.
      call unit_check('',inp(1)==a_bin_flip,'message padding C')

   end subroutine pad_message1

! Test the message padding.
   subroutine pad_message2
      character(len=1024) :: str
      integer(kind=int32) :: inp(16)
      integer(kind=8) :: length
      integer :: pos0, break
      integer :: swap = 1

      ! Set the message.
      str = "abcabcabcabcabcaabcabcabcabcabcaabcabcabcabcabcaabcabcabca"

      pos0   = 1
      break  = 0
      length = 58
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the whole message.
      call unit_check('',inp(1)== abca_bin_flip,'message padding 2')
      call unit_check('',inp(2)== bcab_bin_flip,'message padding 2')
      call unit_check('',inp(3)== cabc_bin_flip,'message padding 2')
      call unit_check('',inp(4)== abca_bin_flip,'message padding 2')
      call unit_check('',inp(5)== abca_bin_flip,'message padding 2')
      call unit_check('',inp(6)== bcab_bin_flip,'message padding 2')
      call unit_check('',inp(7)== cabc_bin_flip,'message padding 2')
      call unit_check('',inp(8)== abca_bin_flip,'message padding 2')
      call unit_check('',inp(9)== abca_bin_flip,'message padding 2')
      call unit_check('',inp(10)==bcab_bin_flip,'message padding 2')
      call unit_check('',inp(11)==cabc_bin_flip,'message padding 2')
      call unit_check('',inp(12)==abca_bin_flip,'message padding 2')
      call unit_check('',inp(13)==abca_bin_flip,'message padding 2')
      call unit_check('',inp(14)==bcab_bin_flip,'message padding 2')
      call unit_check('',inp(15)==ca_one_zero_flip,'message padding 2')
      call unit_check('',inp(16)==empty_bin,'message padding 2')

      call consume_chunk(str, length, inp, pos0, break, swap)

      call unit_check('',inp(1)== empty_bin,'message padding 2')
      call unit_check('',inp(2)== empty_bin,'message padding 2')
      call unit_check('',inp(3)== empty_bin,'message padding 2')
      call unit_check('',inp(4)== empty_bin,'message padding 2')
      call unit_check('',inp(5)== empty_bin,'message padding 2')
      call unit_check('',inp(6)== empty_bin,'message padding 2')
      call unit_check('',inp(7)== empty_bin,'message padding 2')
      call unit_check('',inp(8)== empty_bin,'message padding 2')
      call unit_check('',inp(9)== empty_bin,'message padding 2')
      call unit_check('',inp(10)==empty_bin,'message padding 2')
      call unit_check('',inp(11)==empty_bin,'message padding 2')
      call unit_check('',inp(12)==empty_bin,'message padding 2')
      call unit_check('',inp(13)==empty_bin,'message padding 2')
      call unit_check('',inp(14)==empty_bin,'message padding 2')
      call unit_check('',inp(15)==empty_bin,'message padding 2')
      call unit_check('',inp(16)==little_endian_464,'message padding 2')
   end subroutine pad_message2
! Test the ch function.
   subroutine test_ch
      integer(kind=int32) :: e, f, g
      integer(kind=int32) :: aa, bb
      e = ipad1
      f = ipad2
      g = ipad3
      aa = iand(not(e),g)
      bb = iand(e,f)
      call unit_check('',ieor(aa,bb)==maj(e,f,g),'test the ch function')
   end subroutine test_ch

! Test the maj function.
   subroutine test_maj
      integer(kind=int32) :: a, b, c
      integer(kind=int32) :: aa, bb, cc

      a = ipad1
      b = ipad2
      c = ipad3
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_check('',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      a = ipad2
      b = ipad3
      c = ipad4
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_check('',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      a = ipad3
      b = ipad4
      c = ipad5
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_check('',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      a = ipad4
      b = ipad5
      c = ipad6
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_check('',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

   end subroutine test_maj

! Test the major sigma-0 function.
   subroutine test_cs0
      integer(kind=int32) :: a, b, c
      a   = ishftc(ipad1, -2)
      b   = ishftc(ipad1, -13)
      c   = ishftc(ipad1, -22)
      call unit_check('',ieor(a,ieor(b,c))==cs0(ipad1),'test the major sigma-9 function')

      a   = ishftc(ipad2, -2)
      b   = ishftc(ipad2, -13)
      c   = ishftc(ipad2, -22)
      call unit_check('',ieor(a,ieor(b,c))==cs0(ipad2),'test the major sigma-9 function')

      a   = ishftc(ipad3, -2)
      b   = ishftc(ipad3, -13)
      c   = ishftc(ipad3, -22)
      call unit_check('',ieor(a,ieor(b,c))==cs0(ipad3),'test the major sigma-9 function')

      a   = ishftc(ipad4, -2)
      b   = ishftc(ipad4, -13)
      c   = ishftc(ipad4, -22)
      call unit_check('',ieor(a,ieor(b,c))==cs0(ipad4),'test the major sigma-9 function')

      a   = ishftc(ipad5, -2)
      b   = ishftc(ipad5, -13)
      c   = ishftc(ipad5, -22)
      call unit_check('',ieor(a,ieor(b,c))==cs0(ipad5),'test the major sigma-9 function')

      a   = ishftc(ipad6, -2)
      b   = ishftc(ipad6, -13)
      c   = ishftc(ipad6, -22)
      call unit_check('',ieor(a,ieor(b,c))==cs0(ipad6),'test the major sigma-9 function')
   end subroutine test_cs0

! Test the major sigma-1 function.
   subroutine test_cs1
      integer(kind=int32) :: a, b, c
      a   = ishftc(ipad1, -6)
      b   = ishftc(ipad1, -11)
      c   = ishftc(ipad1, -25)
      call unit_check('',ieor(a,ieor(b,c))==cs1(ipad1),'test the major sigma-9 function')

      a   = ishftc(ipad2, -6)
      b   = ishftc(ipad2, -11)
      c   = ishftc(ipad2, -25)
      call unit_check('',ieor(a,ieor(b,c))==cs1(ipad2),'test the major sigma-9 function')

      a   = ishftc(ipad3, -6)
      b   = ishftc(ipad3, -11)
      c   = ishftc(ipad3, -25)
      call unit_check('',ieor(a,ieor(b,c))==cs1(ipad3),'test the major sigma-9 function')

      a   = ishftc(ipad4, -6)
      b   = ishftc(ipad4, -11)
      c   = ishftc(ipad4, -25)
      call unit_check('',ieor(a,ieor(b,c))==cs1(ipad4),'test the major sigma-9 function')

      a   = ishftc(ipad5, -6)
      b   = ishftc(ipad5, -11)
      c   = ishftc(ipad5, -25)
      call unit_check('',ieor(a,ieor(b,c))==cs1(ipad5),'test the major sigma-9 function')

      a   = ishftc(ipad6, -6)
      b   = ishftc(ipad6, -11)
      c   = ishftc(ipad6, -25)
      call unit_check('',ieor(a,ieor(b,c))==cs1(ipad6),'test the major sigma-9 function')

   end subroutine test_cs1

! Test the minor sigma-0 function.
   subroutine test_ms0
      integer(kind=int32) :: a, b, c

      a   = ishftc(ipad1, -7)
      b   = ishftc(ipad1, -18)
      c   =  ishft(ipad1, -3)
      call unit_check('',ieor(a,ieor(b,c))==ms0(ipad1),'test the minor sigma-0 function')

      a   = ishftc(ipad2, -7)
      b   = ishftc(ipad2, -18)
      c   =  ishft(ipad2, -3)
      call unit_check('',ieor(a,ieor(b,c))==ms0(ipad2),'test the minor sigma-0 function')

      a   = ishftc(ipad3, -7)
      b   = ishftc(ipad3, -18)
      c   =  ishft(ipad3, -3)
      call unit_check('',ieor(a,ieor(b,c))==ms0(ipad3),'test the minor sigma-0 function')

      a   = ishftc(ipad4, -7)
      b   = ishftc(ipad4, -18)
      c   =  ishft(ipad4, -3)
      call unit_check('',ieor(a,ieor(b,c))==ms0(ipad4),'test the minor sigma-0 function')

      a   = ishftc(ipad5, -7)
      b   = ishftc(ipad5, -18)
      c   =  ishft(ipad5, -3)
      call unit_check('',ieor(a,ieor(b,c))==ms0(ipad5),'test the minor sigma-0 function')

      a   = ishftc(ipad6, -7)
      b   = ishftc(ipad6, -18)
      c   =  ishft(ipad6, -3)
      call unit_check('',ieor(a,ieor(b,c))==ms0(ipad6),'test the minor sigma-0 function')

   end subroutine test_ms0

! Test the minor sigma-1 function.
   subroutine test_ms1
      integer(kind=int32) :: a, b, c

      a   = ishftc(ipad1, -17)
      b   = ishftc(ipad1, -19)
      c   =  ishft(ipad1, -10)
      call unit_check('',ieor(a,ieor(b,c))==ms1(ipad1),'test the minor sigma-1 function')

      a   = ishftc(ipad2, -17)
      b   = ishftc(ipad2, -19)
      c   =  ishft(ipad2, -10)
      call unit_check('',ieor(a,ieor(b,c))==ms1(ipad2),'test the minor sigma-1 function')

      a   = ishftc(ipad3, -17)
      b   = ishftc(ipad3, -19)
      c   =  ishft(ipad3, -10)
      call unit_check('',ieor(a,ieor(b,c))==ms1(ipad3),'test the minor sigma-1 function')

      a   = ishftc(ipad4, -17)
      b   = ishftc(ipad4, -19)
      c   =  ishft(ipad4, -10)
      call unit_check('',ieor(a,ieor(b,c))==ms1(ipad4),'test the minor sigma-1 function')

      a   = ishftc(ipad5, -17)
      b   = ishftc(ipad5, -19)
      c   =  ishft(ipad5, -10)
      call unit_check('',ieor(a,ieor(b,c))==ms1(ipad5),'test the minor sigma-1 function')

      a   = ishftc(ipad6, -17)
      b   = ishftc(ipad6, -19)
      c   =  ishft(ipad6, -10)
      call unit_check('',ieor(a,ieor(b,c))==ms1(ipad6),'test the minor sigma-1 function')

   end subroutine test_ms1

! Test the sha256 function with a set of reference strings.
   subroutine test_sha256_1
      character(len=1000000) :: str
      str = ""
      call unit_check('',sha256(str)=="E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855",'sha256 1')
      str = "abc"
      call unit_check('',sha256(str)=="BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD",'sha256 2')
      str = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      call unit_check('',sha256(str)=="248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1",'sha256 3')
      str = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
      call unit_check('',sha256(str)=="CF5B16A778AF8380036CE59E7B0492370B249B11E8F07A51AFAC45037AFEE9D1",'test sha256 4')
   end subroutine test_sha256_1

   subroutine test_sha256_5
      character(len=1000000) :: str
      character(len=64)      :: ref
      integer :: i
      do i=1,1000000
         str(i:i) = "a"
      enddo
      call unit_check('',sha256(str)=="CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0",'test sha256 5')
      ! Check the quick and dirty implementation as well.
      ref = "69E3FACD5F08321F78117BD53476E5321845433356F106E7013E68EC367F3017"
      call unit_check('',dirty_sha256(str)==ref,'test sha256 6')
   end subroutine test_sha256_5

   subroutine test_sha256_6
      character(len=1000000) :: str
      str = "message digest"
      call unit_check('',sha256(str)=="F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650",'test sha256 6')
      str = "secure hash algorithm"
      call unit_check('',sha256(str)=="F30CEB2BB2829E79E4CA9753D35A8ECC00262D164CC077080295381CBD643F0D",'test sha256 7 ')
      str = "SHA256 is considered to be safe"
      call unit_check('',sha256(str)=="6819D915C73F4D1E77E4E1B52D1FA0F9CF9BEAEAD3939F15874BD988E2A23630",'test sha256 8 ')
      str = "For this sample, this 63-byte string will be used as input data"
      call unit_check('',sha256(str)=="F08A78CBBAEE082B052AE0708F32FA1E50C5C421AA772BA5DBB406A2EA6BE342",'test sha256 9 ')
      str = "This is exactly 64 bytes long, not counting the terminating byte"
      call unit_check('',sha256(str)=="AB64EFF7E88E2E46165E29F2BCE41826BD4C7B3552F6B382A9E7D3AF47C245F8",'test sha256 10 ')
   end subroutine test_sha256_6

   subroutine test_sha256_11
      integer,parameter     :: big=16777216
      character(len=big*64) :: str
      integer :: i
      write(*,*)'A long test'
      do i=1,big
         str(1+(i-1)*64:i*64) = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
      enddo
      call unit_check('',sha256(str)=="50E72A0E26442FE2552DC3938AC58658228C0CBFB1D2CA872AE435266FCD055E",'test sha256 11 ')
   end subroutine test_sha256_11

end subroutine test_suite_sha256

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    djb2_hash(3f) - [M_hashkeys:bucket_hash] djb2 string hash (algorithm by Daniel J. Bernstein)
!!
!!##SYNOPSIS
!!
!!    function djb2_hash_arr(anything,continue) result(hash_128)
!!
!!     class(*),intent(in)          :: anything(:)
!!     logical,intent(in),optional  :: continue
!!     !! use,intrinsic : ISO_FORTRAN_ENV, only : int64
!!     integer,parameter            :: int128 = selected_real_kind(2*precision(1.0_int64))
!!     integer(kind=int128)         :: hash_128
!!
!!##DESCRIPTION
!!     djb2_hash(3f) is based on the string hash routine commonly known as
!!     djb2(3c). This algorithm was first described by Dan J. Bernstein many
!!     years ago in comp.lang.c. This version returns a value calculated
!!     using a 64-bit hash, which is returned as a 128bit value (not always
!!     available in Fortran) to allow the value to always be a positive
!!     value; as Fortran does not (currently) support a standard unsigned
!!     integer. If the value is changed to be a 64-bit value on platforms
!!     that do not support 128-bit INTEGER values the value may be negative,
!!     but is otherwise usuable.
!!
!!     Such non-reversible hashes may be used for data or file fingerprints,
!!     to confirm unchanging results during regression testing, ...
!!
!!     More information is widely available on string hashes (including the
!!     well-known djb2(3c) algorithm) on such sources as Wikipedia. Consult
!!     such resources to confirm the suitability of this algorithm for your
!!     use. This algorithm was probably first proposed as a bucket hash.
!!
!!     The algorithm does not consider the Endian of the programming
!!     environment.
!!
!!##OPTIONS
!!     STR    May be a CHARACTER string or an array of common intrinsic
!!            types. Currently, the types defined in the procedure
!!            are character(len=*); complex; integer(kind=int8);
!!            integer(kind=int16); integer(kind=int32); integer(kind=int64);
!!            integer(kind=int128); real(kind=real32); real(kind=real64);
!!            real(kind=real128).
!!
!!     CONTINUE   indicate whether to continue accumulating the hash value
!!                from the last call. This is not threadsafe. This allows
!!                for continued hashes so that a hash can be calculated for
!!                a series of calls.
!!
!!##RETURNS
!!     djb2_hash   A 128-bit INTEGER hash value for the (possibly accumulated) data.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_djb2_hash
!!     use M_hashkeys, only : djb2_hash, int128
!!     implicit none
!!     integer(kind=int128)         :: hash
!!     character(len=:),allocatable :: string
!!     integer                      :: i
!!     ! string
!!     string='test djb2_hash'
!!     hash=djb2_hash(string)
!!     write(*,*)'string=',string,' hash=',hash
!!     ! array of characters
!!     hash=djb2_hash(['t','e','s','t',' ','d','j','b','2','_','h','a','s','h'])
!!     write(*,*)'string=',string,' hash=',hash
!!     ! continued hash
!!     hash=djb2_hash(['t','e','s','t'])
!!     hash=djb2_hash([' ','d','j','b','2'],continue=.true.)
!!     hash=djb2_hash(['_','h','a','s','h'],continue=.true.)
!!     write(*,*)'string=',string,' hash=',hash
!!     ! array of integers
!!     hash=djb2_hash([(i,i=0,100)])
!!     write(*,*)'hash for values 0 to 100 is ',hash
!!     !
!!     end program demo_djb2_hash
!===================================================================================================================================
function djb2_hash_arr(anything,continue) result(hash_128)
implicit none

character(len=*),parameter::ident="@(#)djb2_hash(3fp): DJB2 hash of array (algorithm by Daniel J. Bernstein )"

class(*),intent(in)          :: anything(:)
logical,intent(in),optional  :: continue
integer                      :: i
integer(kind=int128)         :: hash_128
integer(kind=int64),save     :: hash_64=5381
character(len=1),allocatable :: chars(:)

   if(present(continue))then
      hash_64 = hash_64
   else
      hash_64 = 5381_int64
   endif

   chars=anything_to_bytes(anything)

   do i=1,size(chars)
      hash_64 = (ishft(hash_64,5) + hash_64) + ichar(chars(i),kind=int64)
   end do
   hash_128=transfer([hash_64,0_int64],hash_128)

   if(debug)then
      DEBUG : block
         integer :: ios
         write(6,'("*djb2_hash*       hashing string=",*(a))',advance='no')chars
         write(6,'(1x,"hash=",i0,1x,"hex hash=",z32.32)')hash_128,hash_128
         ;flush(6,iostat=ios)
      endblock DEBUG
   endif

end function djb2_hash_arr
!-----------------------------------------------------------------------------------------------------------------------------------
function djb2_hash_scalar(anything,continue) result(hash_128)
implicit none

character(len=*),parameter::ident="@(#)djb2_hash(3fp): djb2 hash of scalar"

class(*),intent(in)          :: anything
logical,intent(in),optional  :: continue
integer(kind=int128)         :: hash_128
character(len=1),allocatable :: chars(:)

   chars=anything_to_bytes(anything)

if(present(continue))then
   hash_128=djb2_hash_arr(chars,continue)
else
   hash_128=djb2_hash_arr(chars)
endif

end function djb2_hash_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    crc32_hash(3f) - [M_hashkeys] CRC (Cyclic Redundancy Check)
!!
!!##SYNOPSIS
!!
!!   function crc32_hash(a,continue) result (crc)
!!
!!    class(*),intent(in)          :: anything(:)
!!    logical,intent(in),optional  :: continue
!!    integer(int64)               :: crc_out
!!
!!##DESCRIPTION
!!    This ia 32-bit version of the Cyclic Redundancy Check(CRC).
!!    This variant of CRC-32 uses LSB-first order, sets the initial CRC to
!!    FFFFFFFF_int32, and complements the final CRC.
!!
!!    The result should be in accordance with ISO 3309, ITU-T V.42, Gzip
!!    and PNG.
!!
!!##OPTIONS
!!    anything  input value to generate a CRC check for. May be a scalar
!!              or string value of type CHARACTER, int8, int16, int32,
!!              int64, real32, real64, real128
!!    continue  optional parameter. If not present or .F. starts new
!!              CRC sum. If .T. continues a CRC starting with last CRC
!!              calculated.
!!##RETURNS
!!    crc       The calculated CRC sum. It is calculated as a 32-bit value
!!              but returned as a 64-bit value, as Fortran does not
!!              currently support unsigned integers.
!!
!!##REFERENCES
!!    Algorithms are described in "Computation of CRC" in Wikipedia.
!!    Also see
!!
!!       https://en.wikipedia.org/wiki/Cyclic_redundancy_check
!!
!!##AUTHOR
!!    This was derived from an unattributed example on http://rosettacode.org,
!!    but has been modified.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_crc32_hash
!!    use,intrinsic :: ISO_FORTRAN_ENV, only : int64
!!    use M_hashkeys, only : crc32_hash
!!    implicit none
!!    integer :: i
!!    integer(int64) :: crc
!!    character(*), parameter :: s = "The quick brown fox jumps over the lazy dog"
!!       ! string
!!       crc=crc32_hash(s)
!!       print "(Z8)", crc
!!       print "(i0)", crc
!!       ! character array
!!       print "(i0)", crc32_hash([ &
!!               & 'T','h','e',' ',&
!!               & 'q','u','i','c','k',' ',&
!!               & 'b','r','o','w','n',' ',&
!!               & 'f','o','x',' '])
!!       print "(i0)", crc32_hash([ &
!!               & 'j','u','m','p','s',' ',&
!!               & 'o','v','e','r',' ',&
!!               & 't','h','e',' ',&
!!               & 'l','a','z','y',' ',&
!!               & 'd','o','g'],continue=.true.)
!!       ! numeric array
!!       print "(i0)", crc32_hash([(i,i=1,100)])
!!    end program demo_crc32_hash
!!
!!   Expected output:
!!
!!    414FA339
!!    1095738169
!!    2293265890
!!    1095738169
!!    1783575711
!!
!===================================================================================================================================
function crc32_hash_arr(anything,continue) result (crc_64)
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
implicit none

character(len=*),parameter::ident="@(#)M_hashkeys::crc32_hash_arr: CRC (Cyclic Redundancy Check) calculation"

class(*),intent(in)          :: anything(:)
logical,intent(in),optional  :: continue
character(len=1),allocatable :: a(:)
integer(int64)               :: crc_64
integer(int32),save          :: crc
integer                      :: i
integer(int32),save          :: crc_table(0:255)
integer,save                 :: icalled=0
   if(present(continue))then
      if(continue .eqv. .false.)then
         crc=0_int32
      endif
   else
      crc=0_int32
   endif

   a=anything_to_bytes(anything)

   if(icalled.eq.0)then         ! on first call generate table and use table for speed
      INIT_TABLE: block
         integer :: i, j
         integer(int32) :: k

         do i = 0, 255
            k = i
            do j = 1, 8
               if (btest(k, 0)) then
                  k = ieor(shiftr(k, 1), -306674912_int32)
               else
                  k = shiftr(k, 1)
               endif
            enddo
            crc_table(i) = k
         enddo
      endblock INIT_TABLE
      icalled=1
   endif

   crc = not(crc)
   do i = 1, size(a)
      crc = ieor(shiftr(crc, 8), crc_table(iand(ieor(crc, iachar(a(i))), 255)))
   enddo
   crc = not(crc)
   crc_64=transfer([crc,0_int32],crc_64)
   if(debug)then
      DEBUG : block
         integer :: ios
         write(6,'("*crc32_hash*       hashing string=",*(a))',advance='no')a
         write(6,'(1x,"hash=",i0,1x,"hex hash=",z32.32)')crc_64,crc_64
         ;flush(6,iostat=ios)
      endblock DEBUG
   endif
end function crc32_hash_arr
!-----------------------------------------------------------------------------------------------------------------------------------
function crc32_hash_scalar(anything,continue) result(hash_64)
implicit none

character(len=*),parameter::ident="@(#)crc32_hash_scalar(3fp): crc32 hash of scalar"

class(*),intent(in)          :: anything
logical,intent(in),optional  :: continue
integer(kind=int64)         :: hash_64
character(len=1),allocatable :: chars(:)

   chars=anything_to_bytes(anything)

if(present(continue))then
   hash_64=crc32_hash_arr(chars,continue)
else
   hash_64=crc32_hash_arr(chars)
endif

end function crc32_hash_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sdbm_hash(3f) - [M_hashkeys:bucket_hash] sdbm string hash
!!
!!##SYNOPSIS
!!
!!    use,intrinsic : ISO_FORTRAN_ENV, only : int64
!!    function sdbm_hash_arr(anything,continue) result(hash_128)
!!
!!     class(*),intent(in)          :: anything(:)
!!     logical,intent(in),optional  :: continue
!!     integer,parameter            :: int128 = selected_real_kind(2*precision(1.0_int64))
!!     integer(kind=int128)         :: hash_128
!!
!!##DESCRIPTION
!!    sdbm_hash(3f) is based on the string hash routine commonly known as
!!    sdbm(3c).
!!
!!    this algorithm was created for the sdbm (a public-domain
!!    reimplementation of ndbm) database library. It was found to do well
!!    in scrambling bits, causing good distribution of the keys and fewer
!!    splits. it also happens to be a good general hashing function with
!!    good distribution. the actual function is
!!
!!       hash(i) = hash(i - 1) * 65599 + str[i]
!!
!!    what is available here is the faster version used
!!    in gawk. [there is even a faster, duff-device version]. The magic
!!    constant 65599 was picked out of thin air while experimenting with
!!    different constants, and turns out to be a prime. this is one of the
!!    algorithms used in berkeley db (see sleepycat) and elsewhere.
!!
!!    This version returns a value calculated using a 64-bit hash, which
!!    is returned as a 128bit value (not always available in Fortran) to
!!    allow the value to always be a positive value; as Fortran does not
!!    (currently) support a standard unsigned integer. If the value is
!!    changed to be a 64-bit value on platforms that do not support 128-bit
!!    INTEGER values the value may be negative, but is otherwise usuable.
!!
!!    Such non-reversible hashes may be used for data or file fingerprints,
!!    to confirm unchanging results during regression testing, ...
!!
!!    More information is widely available on string hashes (including the
!!    well-known sdbm(3c) algorithm) on such sources as Wikipedia. Consult
!!    such resources to confirm the suitability of this algorithm for
!!    your use.
!!
!!    The algorithm does not consider the Endian of the programming
!!    environment.
!!
!!##OPTIONS
!!     STR    May be a CHARACTER string or an array of common intrinsic
!!            types. Currently, the types defined in the procedure
!!            are character(len=*); complex; integer(kind=int8);
!!            integer(kind=int16); integer(kind=int32); integer(kind=int64);
!!            integer(kind=int128); real(kind=real32); real(kind=real64);
!!            real(kind=real128).
!!
!!     CONTINUE   indicate whether to continue accumulating the hash value
!!                from the last call. This is not threadsafe. This allows
!!                for continued hashes so that a hash can be calculated for
!!                a series of calls.
!!
!!##RETURNS
!!     sdbm_hash   A 128-bit INTEGER hash value for the (possibly accumulated) data.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_sdbm_hash
!!     use M_hashkeys, only : sdbm_hash, int128
!!     implicit none
!!     integer(kind=int128)         :: hash
!!     character(len=:),allocatable :: string
!!     integer                      :: i
!!     ! string
!!     string='test sdbm_hash'
!!     hash=sdbm_hash(string)
!!     write(*,*)'string=',string,' hash=',hash
!!     ! array of characters
!!     hash=sdbm_hash(['t','e','s','t',' ','s','d','b','m','_','h','a','s','h'])
!!     write(*,*)'string=',string,' hash=',hash
!!     ! continued hash
!!     hash=sdbm_hash(['t','e','s','t'])
!!     hash=sdbm_hash([' ','s','d','b','m'],continue=.true.)
!!     hash=sdbm_hash(['_','h','a','s','h'],continue=.true.)
!!     write(*,*)'string=',string,' hash=',hash
!!     ! array of integers
!!     hash=sdbm_hash([(i,i=0,100)])
!!     write(*,*)'hash for values 0 to 100 is ',hash
!!     !
!!     end program demo_sdbm_hash
!===================================================================================================================================
function sdbm_hash_arr(anything,continue) result(hash_128)
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
implicit none

character(len=*),parameter::ident="@(#)sdbm_hash_arr(3fp): sdbm hash of array"

class(*),intent(in)          :: anything(:)
logical,intent(in),optional  :: continue
integer                      :: i
integer(kind=int128)         :: hash_128
integer(kind=int64),save     :: hash_64=5381
character(len=1),allocatable :: chars(:)

   if(present(continue))then
      hash_64 = hash_64
   else
      hash_64 = 0_int64
   endif

   chars=anything_to_bytes(anything)

   do i=1,size(chars)
      hash_64 = ichar(chars(i),kind=int64) + ishft(hash_64,6) + ishft(hash_64,16) - hash_64
   end do
   hash_128=transfer([hash_64,0_int64],hash_128)

   if(debug)then
      DEBUG : block
         integer :: ios
         write(6,'("*sdbm_hash*       hashing string=",*(a))',advance='no')chars
         write(6,'(1x,"hash=",i0,1x,"hex hash=",z32.32)')hash_128,hash_128
         ;flush(6,iostat=ios)
      endblock DEBUG
   endif

end function sdbm_hash_arr
!-----------------------------------------------------------------------------------------------------------------------------------
function sdbm_hash_scalar(anything,continue) result(hash_128)
implicit none

character(len=*),parameter::ident="@(#)sdbm_hash_scalar(3fp): sdbm hash of scalar"

class(*),intent(in)          :: anything
logical,intent(in),optional  :: continue
integer(kind=int128)         :: hash_128
character(len=1),allocatable :: chars(:)

   chars=anything_to_bytes(anything)


   if(present(continue))then
      hash_128=sdbm_hash_arr(chars,continue)
   else
      hash_128=sdbm_hash_arr(chars)
   endif

   if(debug)then
      DEBUG : block
      integer :: i
      integer :: ios
      write(6,'("*sdbm scalar          hashing string=",*(a))',advance='no')(chars(i),i=1,size(chars))
      write(6,'(1x,"hash=",i0,1x,"hex hash=",z32.32)')hash_128,hash_128
      flush(6,iostat=ios)
      endblock DEBUG
   endif

end function sdbm_hash_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function djb2(anything)
implicit none

character(len=*),parameter::ident="@(#)djb2(3f): call C routine djb2(3c) with a Fortran CHARACTER variable"

! extern int djb2(char *s);
interface
   function djb2_F(S) bind(C,NAME='C_djb2')
      use ISO_C_BINDING, only : C_LONG, C_CHAR
      implicit none
      integer(KIND=C_LONG)              :: djb2_F
      character(KIND=C_CHAR),intent(in) :: S(*)
   end function djb2_F
end interface

class(*),intent(in)          :: anything(:)
integer(kind=int128)         :: djb2
character(len=1),allocatable :: chars(:)

   chars=anything_to_bytes(anything)

   djb2=transfer([djb2_F([chars,char(0)]),0_int64],djb2)

   if(debug)then
      DEBUG : block
      integer :: i
      integer :: ios
      write(6,'("*djb2 FORTRAN*        hashing string=",*(a))',advance='no')(chars(i),i=1,size(chars))
      write(6,'(1x,"hash=",i0,1x,"hex hash=",z32.32)')djb2,djb2
      flush(6,iostat=ios)
      endblock DEBUG
   endif

end function djb2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   b3hs_hash_key_jenkins(3f) - [M_hashkeys] hash key algorithm by Bob Jenkins
!!
!!##SYNOPSIS
!!
!!   function b3hs_hash_key_jenkins (key, range) result (code)
!!
!!    character(*), intent(in) :: key
!!    integer, intent(in)      :: range
!!    integer                  :: code
!!
!!##DESCRIPTION
!!   Based on implementation of Bob Jenkins hash function by Rich Townsen,
!!   posted 2008-03-23 at
!!
!!      http://computer-programming-forum.com/49-fortran/0596e59d0fa2e5e4.htm
!!
!!##OPTIONS
!!   KEY    string to generate a hash key for
!!   RANGE  range should be a power of 2. Note that the 32-bit algorithm is used
!!
!!##RETURNS
!!   CODE   returned hash value in range specified by RANGE
!===================================================================================================================================
function b3hs_hash_key_jenkins (key, range) result (code)
character(*), intent(in) :: key
integer, intent(in)      :: range
integer                  :: code

integer                  :: len_key
integer(int32)           :: a
integer(int32)           :: b
integer(int32)           :: c
integer                  :: k

   ! Hash the key into a code, using the algorithm described by Bob Jenkins at:
   !  http://burtleburtle.net/bob/hash/doobs.html
   !
   ! Note that range should be a power of 2, and that the 32-bit algorithm is used

   len_key = LEN_TRIM(key)

   a = -1640531527_int32 ! 0x9E3779B9
   b = a
   c = 305419896_int32   ! 0x12345678

   k = 1

   char_loop : do

      if(len_key < 12) exit char_loop

      ! Pack the key into 32 bits

      a = a + ICHAR(key(k+0:k+0))  + ISHFT(ICHAR(key(k+1:k+1)), 8) + &
      &       ISHFT(ICHAR(key(k+2:k+2)), 16) + ISHFT(ICHAR(key(k+3:k+3)), 24)
      b = b + ICHAR(key(k+4:k+4))  + ISHFT(ICHAR(key(k+5:k+5)), 8) + &
      &       ISHFT(ICHAR(key(k+6:k+6)), 16) + ISHFT(ICHAR(key(k+7:k+7)), 24)
      c = c + ICHAR(key(k+8:k+8))  + ISHFT(ICHAR(key(k+9:k+9)), 8) + &
      &       ISHFT(ICHAR(key(k+10:k+10)), 16) + ISHFT(ICHAR(key(k+11:k+11)), 24)

      ! Mix it up

      call b3hs_hash_key_jenkins_mix_()

      k = k + 12

      len_key = len_key - 12

   end do char_loop

   c = c + len_key

   ! Process remaining bits

   select case(len_key)
    case(11)
      c = c + ISHFT(ICHAR(key(k+10:k+10)), 24) + ISHFT(ICHAR(key(k+9:k+9)), 16) + &
      &       ISHFT(ICHAR(key(k+8:k+8)), 8)
      b = b + ISHFT(ICHAR(key(k+7:k+7)), 24) + ISHFT(ICHAR(key(k+6:k+6)), 16) + &
      &       ISHFT(ICHAR(key(k+5:k+5)), 8) + ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(10)
      c = c + ISHFT(ICHAR(key(k+9:k+9)), 16) + ISHFT(ICHAR(key(k+8:k+8)), 8)
      b = b + ISHFT(ICHAR(key(k+7:k+7)), 24) + ISHFT(ICHAR(key(k+6:k+6)), 16) + &
      &       ISHFT(ICHAR(key(k+5:k+5)), 8) + ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(9)
      c = c + ISHFT(ICHAR(key(k+8:k+8)), 8)
      b = b + ISHFT(ICHAR(key(k+7:k+7)), 24) + ISHFT(ICHAR(key(k+6:k+6)), 16) + &
      &       ISHFT(ICHAR(key(k+5:k+5)), 8) + ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(8)
      b = b + ISHFT(ICHAR(key(k+7:k+7)), 24) + ISHFT(ICHAR(key(k+6:k+6)), 16) + &
      &       ISHFT(ICHAR(key(k+5:k+5)), 8) + ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(7)
      b = b + ISHFT(ICHAR(key(k+6:k+6)), 16) + ISHFT(ICHAR(key(k+5:k+5)), 8) + &
      &       ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(6)
      b = b + ISHFT(ICHAR(key(k+5:k+5)), 8) + ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(5)
      b = b + ICHAR(key(k+4:k+4))
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(4)
      a = a + ISHFT(ICHAR(key(k+3:k+3)), 24) + ISHFT(ICHAR(key(k+2:k+2)), 16) + &
      &       ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(3)
      a = a + ISHFT(ICHAR(key(k+2:k+2)), 16) + ISHFT(ICHAR(key(k+1:k+1)), 8) + &
      &       ICHAR(key(k:k))
    case(2)
      a = a + ISHFT(ICHAR(key(k+1:k+1)), 8) + ICHAR(key(k:k))
    case(1)
      a = a + ICHAR(key(k:k))
   end select

   call b3hs_hash_key_jenkins_mix_()

   code = IAND(c, range - 1) + 1

   ! Finish
contains

   subroutine b3hs_hash_key_jenkins_mix_

      ! Mix a, b and c

      a = IEOR(a - b - c, ISHFT(c, -13))
      b = IEOR(b - c - a, ISHFT(a, 8))
      c = IEOR(c - a - b, ISHFT(b, -13))

      a = IEOR(a - b - c, ISHFT(c, -12))
      b = IEOR(b - c - a, ISHFT(a, 16))
      c = IEOR(c - a - b, ISHFT(b, -5))

      a = IEOR(a - b - c, ISHFT(c, -3))
      b = IEOR(b - c - a, ISHFT(a, 10))
      c = IEOR(c - a - b, ISHFT(b, -15))

      ! Finish
   end subroutine b3hs_hash_key_jenkins_mix_

end function b3hs_hash_key_jenkins
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_hashkeys
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================