module M_uuid
!>
!!##NAME
!!    M_uuid(3f) - [M_uuid] a module of UUID (Universally Unique IDentifier) procedures
!!
!!##SYNOPSIS
!!
!!    use m_uuid, only : generate_uuid
!!
!!##QUOTE
!!    Remember you are unique, just like everyone else.
!!
!!##DESCRIPTION
!!
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems.
!!
!!    When generated according to the standard methods, UUIDs are for
!!    practical purposes unique, without depending for their uniqueness
!!    on a central registration authority or coordination between the
!!    parties generating them, unlike most other numbering schemes. While
!!    the probability that a UUID will be duplicated is not zero, it is
!!    close enough to zero to be negligible.
!!
!!    Thus, anyone can create a UUID and use it to identify something
!!    with near certainty that the identifier does not duplicate one
!!    that has already been, or will be, created to identify something
!!    else. Information labeled with UUIDs by independent parties can
!!    therefore be later combined into a single database, or transmitted
!!    on the same channel, without needing to resolve conflicts between
!!    identifiers.
!!
!!    Adoption of UUIDs and GUIDs is widespread, with many computing
!!    platforms providing support for generating them, and for parsing
!!    their textual representation
!!
!!    RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
!!    A UUID presented as a URN appears as follows:[2]
!!
!!       urn:uuid:123e4567-e89b-12d3-a456-426655440000
!!
!! -- Wikipedia
!!
!!##PROCEDURES
!!
!!    generate_uuid(version)   generate 36-character UUID string
!===================================================================================================================================
use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
implicit none
private

character(len=*),parameter::ident="@(#)M_uuid::M_uid(3fm): generate UUIDs according to RFC 4122"

! Only versions  0(Nil), 1 (time-based) and 4 (pseudo-RNG-based) are implemented.

integer, parameter       :: i4b = selected_int_kind(9)
integer, parameter       :: i8b = selected_int_kind(18)
type(mtprng_state), save :: rng_state
logical, save            :: initialized = .false.
integer, save            :: values_save              ! must be default for date_and_time
integer(kind=i4b), save  :: hires_count = 0
integer, save            :: clock_seq = 0 ! a random number constant for the lifetime of the process. best we can do per S 4.1.5

public                   :: generate_uuid

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!! ! generate_uuid(3f) was originally derived from the xmlf90 codebase, (c) Alberto Garcia & Jon Wakelin, 2003-2004.
!! ! It also calls RNG routines from Scott Ladd <scott.ladd@coyotegulch.com>, and the libFoX modules. Although
!! ! some sections have been replaced, generate_uuid(3f) was originally based on the libFoX version, with
!! ! licensing as follows:
!! !
!! ! (c) 2005-2009 Toby White <tow@uszla.me.uk>
!! ! (c) 2007-2009 Gen-Tao Chiang <gtc25@cam.ac.uk>
!! ! (c) 2008-2012 Andrew Walker <a.walker@ucl.ac.uk>
!! !
!! ! All rights reserved.
!! !
!! ! * Redistribution and use in source and binary forms, with or without
!! ! modification, are permitted provided that the following conditions are
!! ! met:
!! !
!! ! * Redistributions of source code must retain the above copyright notice,
!! ! this list of conditions and the following disclaimer.
!! !
!! ! * Redistributions in binary form must reproduce the above copyright
!! ! notice, this list of conditions and the following disclaimer in the
!! ! documentation and/or other materials provided with the distribution.
!! !
!! ! * Neither the name of the copyright holder nor the names of its
!! ! contributors may be used to endorse or promote products derived from
!! ! this software without specific prior written permission.
!! !
!! !
!! ! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!! ! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!! ! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!! ! A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
!! ! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
!! ! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
!! ! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
!! ! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
!! ! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!! ! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!! ! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!===================================================================================================================================
!>
!!##NAME
!!    generate_uuid(3f) - [M_uuid] generate(approximately) a UUID (Universally Unique IDentifier) string per RFC 4122
!!
!!##SYNOPSIS
!!
!!    function generate_uuid(version) result(uuid)
!!    integer, intent(in), optional :: version
!!    character(len=36) :: uuid
!!
!!##DESCRIPTION
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems. When generated according
!!    to standard methods UUIDs are for practical purposes unique.
!!    generate_uuid(3f) converts the UUID to s standard string format
!!    per RFC 4122.
!!
!!##AUTHORS
!!    based on previous work from Alberto Garcia & Jon Wakelin, 2003-2004.
!!    RNG routines from Scott Ladd <scott.ladd@coyotegulch.com>, and
!!    the libFoX library( Toby White <tow@uszla.me.uk>, Gen-Tao Chiang
!!    <gtc25@cam.ac.uk>, Andrew Walker <a.walker@ucl.ac.uk>).
!!
!!##OPTIONS
!!    version  Indicates which standard method as described in RFC 4122
!!             is used to generate the string. Versions 0,1, and 4 are supported.
!!
!!             0.  Nil UUID (ie. '00000000-0000-0000-0000-000000000000')
!!             1.  time-based UUID
!!             2.  Not implemented
!!             3.  Not implemented
!!             4.  pseudo-RNG(Random Number Generator) based
!!             5.  Not implemented
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    program uuidgen
!!    use M_uuid, only : generate_uuid
!!    implicit none
!!    character(len=36) :: uuid
!!       uuid=generate_uuid(1)  ! version 1 (time-based UUID)
!!       write(*,'(a36)')uuid
!!       uuid=generate_uuid(4)  ! version 4 (pseudo-RNG-based), default
!!       ! RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
!!       write(*,'("urn:uuid:",a36)')uuid
!!    end program uuidgen
!!
!!   Typical output:
!!
!!     e769adf4-4af7-11e8-7421-3c9dfbfe9aab
!!     urn:uuid:5b0946b8-0eb4-4966-619d-047b7f7e2056
!===================================================================================================================================
function generate_uuid(version) result(uuid)

character(len=*),parameter::ident="&
&@(#)M_uuid::generate_uuid(3f): generate(approximately) a UUID (Universally Unique IDentifier) string per RFC 4122"

integer, intent(in), optional :: version
character(len=36) :: uuid

integer(kind=i8b) :: timestamp, node
integer(kind=i4b) :: clock_sequence

integer(kind=i4b) :: time_low, time_mid, time_hi_and_version
integer(kind=i4b) :: clk_seq_hi_res, clk_seq_low

integer :: values(8) ! must be default for date_and_time
integer(kind=i4b) :: variant, v

   if (.not.initialized) then
     ! Use the current date and time to init mtprng but this gives limited variability, so mix the result up.
     ! Can we do better? In any case, this gets passed through a quick generator inside mtprng_init.
     call date_and_time(values=values)
     values(7) = values(7)*1000+values(5)*100+values(3)*10+values(1)
     values(8) = values(2)*1000+values(4)*100+values(6)*10+values(8)
     call mtprng_init(int(values(7)*10000+values(8), i4b), rng_state)
     clock_seq = int(mtprng_rand64(rng_state), i4b)
     initialized = .true.
   endif

   variant = 1

   if (present(version)) then
     v = version
   else
     v = 4
   endif

   select case (v)
   case (0)
     uuid='00000000-0000-0000-0000-000000000000' ! Nil UUID
     return
   case(1)                                                                           !  version 1(time-based)
     call date_and_time(values=values)
     ! In case of too-frequent requests, we will replace time_low with the count below ...
     if (all(values==values_save)) then
       hires_count = hires_count + 1
     else
       hires_count = 0
     endif
     timestamp = get_utc_since_1582(values)
     clock_sequence = clock_seq                                                      ! clock sequence (14 bits)
     node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))       ! node ( 48 bits)
     ! No MAC address accessible - see section 4.5 !FIXME
   case(2-3,5) ! Unimplemented
     uuid = ''
     return
   case(4)                                                                           ! version 4 (pseudo-RNG-based)
     timestamp = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 28))
     clock_sequence = int(mtprng_rand64(rng_state), i4b)                             ! clock sequence (14 bits)
     node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))       ! node ( 48 bits)
   case default ! Unspecified
     uuid = ''
     return
   end select

   time_low = ibits(timestamp, 0, 32)
   time_mid = ibits(timestamp, 32, 16)

   if (hires_count==0) then
     time_hi_and_version = ior(int(ibits(timestamp, 48, 12), i4b), ishft(v, 12))
   else
     time_hi_and_version = ior(hires_count, ishft(v, 12))
   endif

   clk_seq_low = ibits(clock_sequence, 0, 8)
   clk_seq_hi_res = ior(ibits(clock_sequence, 8, 6), ishft(variant, 6))

   uuid = int32ToHexOctets(time_low, 4)//"-"// &
     int32ToHexOctets(time_mid, 2)//"-"// &
     int32ToHexOctets(time_hi_and_version, 2)//"-"// &
     int32ToHexOctets(clk_seq_hi_res, 1)// &
     int32ToHexOctets(clk_seq_low, 1)//"-"// &
     int64ToHexOctets(node, 6)

contains
!==================================================================================================================================!
function int32ToHexOctets(b, n) result(s)
integer(i4b), intent(in) :: b
integer, intent(in)      :: n ! number of octets to print
character(len=2*n)       :: s
   character, parameter  :: hexdigits(0:15) = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
   integer               :: i

   do i = 0, 2*n-1
      s(2*n-i:2*n-i) = hexdigits(ibits(b, i*4, 4))
   enddo

end function int32ToHexOctets
!==================================================================================================================================!
function int64ToHexOctets(b, n) result(s)
integer(i8b), intent(in) :: b
integer, intent(in)      :: n ! number of octets to print
character(len=2*n)       :: s
   character, parameter  :: hexdigits(0:15) = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
   integer               :: i

   do i = 0, 2*n-1
      s(2*n-i:2*n-i) = hexdigits(ibits(b, i*4, 4))
   enddo

end function int64ToHexOctets
!==================================================================================================================================!
end function generate_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function get_utc_since_1582(values) result(ns)
use M_time, only : date_to_unix, realtime
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, dp=>real128
implicit none

! returns the number of 100-ns intervals since 1582-10-15T00:00:00-0

! Not really: Assuming only used as an internal routine for M_UUID(3fm)
!   Fortran date time arrays only report up to the millisecond,
!   and assuming any date given is after 2017 (because added leapseconds up till that date)
!   and not taking account of leapseconds after 2017, and assuming
!   if get same answer on multiple calls that caller will correct or try again, as goal is to generate unique values

integer,intent(in)  :: values(8)
integer(kind=i8b)   :: ns
real(kind=realtime) :: unixtime
real(kind=realtime) :: starttime
integer             :: ierr
integer             :: clicks,maxclicks
real                :: rate
real(kind=dp)       :: rate8,frac8
integer(kind=i8b)   :: frac
   call date_to_unix([1582,10,15,0,0,0,0,0,0],starttime,ierr)                  ! seconds from 1582-10-15-00-00-00 to Unix Epoch Time
   call date_to_unix(values,unixtime,ierr)                                     ! seconds from given date to Unix Epoch Time
   ! if system clock is higher resolution use it even though that makes fractional second wrong
   call system_clock(count=clicks,count_rate=rate,count_max=maxclicks)
   if(rate.gt.1000)then                                                        ! system clock available and higher resolution
      rate8=real(rate,kind=dp)
      frac8=mod(real(clicks,kind=dp),rate8)/rate8*10000000_i8b                 ! MOD(A,P) == A - INT (A/P) * P.
      frac=int(frac8)                                                          ! truncate to one remainder of one second
      ns=int((unixtime-starttime)*10000000_i8b,kind=i8b)+frac                  ! get date and time to nearest second and add frac
   else                                                                        ! use date even though accurate only to 1/1000 second
      ns=int(unixtime*10000000_i8b,kind=i8b)-int(starttime*10000000_i8b,kind=i8b)
   endif
   ns=ns+26_i8b                                                                ! leap seconds as of 2016 at 23:59:60 UTC
end function get_utc_since_1582
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
