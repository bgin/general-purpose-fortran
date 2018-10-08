!===================================================================================================================================
! This module and the example function squarei() that uses it shows how you
! can use polymorphism to allow arguments of different types generically
!===================================================================================================================================
module M_anything
use ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
private

! on this platform, (select_int_kind(i),i=1,100) returns
! 1:2=1 ,3:4=2 ,5:9=4 ,10:18= 8 ,19:38=16 ,39:=-1

! on this platform, (select_real_kind(i),i=1,100) returns
! 1:6=   4, 7:15 = 8, 16:18= 10, 19:33= 16, 34:  = -1

integer,parameter        :: k(38)=[(selected_int_kind(i),i=1,38)]
integer,public,parameter :: int128=k(38)

integer,parameter        :: r(34)=[(selected_int_kind(i),i=1,34)]
integer,public,parameter :: real256=r(34)

public anyinteger_to_128bit ! convert integer parameter of any kind to 128-bit integer
public anyscalar_to_real    ! convert integer or real parameter of any kind to real
public anyscalar_to_double  ! convert integer or real parameter of any kind to doubleprecision

public anything_to_bytes
interface anything_to_bytes
   module procedure anything_to_bytes_arr
   module procedure anything_to_bytes_scalar
end interface anything_to_bytes

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anything_to_bytes(3f) - [M_anything] convert standard types to bytes (character(len=1):: array(:))
!!
!!##SYNOPSIS
!!
!!    function anything_to_bytes_arr(anything) result(chars)
!!
!!     class(*),intent(in)  :: anything
!!             or
!!     class(*),intent(in)  :: anything(:)
!!
!!     character(len=1),allocatable :: chars(:)
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different
!!    types. It is used to create other procedures that can take many
!!    argument types as input options and convert them to a single type
!!    to simplify storing arbitrary data, to simplify generating data
!!    hashes, ...
!!
!!##OPTIONS
!!
!!    VALUEIN  input array or scalar to convert to type CHARACTER(LEN=1).
!!             May be of KIND INTEGER(kind=int8), INTEGER(kind=int16),
!!             INTEGER(kind=int32), INTEGER(kind=int64),
!!             INTEGER(kind=int128), REAL(kind=real32, REAL(kind=real64),
!!             REAL(kind=real128), complex, or CHARACTER(len=*)
!!    RESULTS
!!             The returned value is an array of bytes (character(len=1)).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_anything_to_bytes
!!    use M_anything,      only : anything_to_bytes
!!    !!use M_anything,      only : int128, real256
!!    !!use iso_fortran_env, only : int8, int16, int32, int64
!!    !!use iso_fortran_env, only : real32, real64, real128
!!    implicit none
!!    integer :: i
!!       write(*,'(/,4(1x,z2.2))')anything_to_bytes([(i*i,i=1,10)])
!!       write(*,'(/,4(1x,z2.2))')anything_to_bytes([11.11,22.22,33.33])
!!       write(*,'(/,4(1x,z2.2))')anything_to_bytes('This is a string')
!!    end program demo_anything_to_bytes
!!
!!   Expected output
!!
!!     01 00 00 00
!!     04 00 00 00
!!     09 00 00 00
!!     10 00 00 00
!!     19 00 00 00
!!     24 00 00 00
!!     31 00 00 00
!!     40 00 00 00
!!     51 00 00 00
!!     64 00 00 00
!!
!!     8F C2 31 41
!!     8F C2 B1 41
!!     EC 51 05 42
!!
!!     54 68 69 73
!!     20 69 73 20
!!     61 20 73 74
!!     72 69 6E 67
!===================================================================================================================================
function anything_to_bytes_arr(anything) result(chars)
implicit none

character(len=*),parameter::ident="@(#)M_haskeys::sdbm_hash(3fp): anything to bytes (an array of CHARACTER(LEN=1) variables)"

class(*),intent(in)          :: anything(:)
character(len=1),allocatable :: chars(:)
   select type(anything)

    type is (character(len=*));     chars=transfer(anything,chars)
    type is (complex);              chars=transfer(anything,chars)
    type is (integer(kind=int8));   chars=transfer(anything,chars)
    type is (integer(kind=int16));  chars=transfer(anything,chars)
    type is (integer(kind=int32));  chars=transfer(anything,chars)
    type is (integer(kind=int64));  chars=transfer(anything,chars)
    type is (integer(kind=int128)); chars=transfer(anything,chars)
    type is (real(kind=real32));    chars=transfer(anything,chars)
    type is (real(kind=real64));    chars=transfer(anything,chars)
    type is (real(kind=real128));   chars=transfer(anything,chars)

   end select
end function anything_to_bytes_arr
!-----------------------------------------------------------------------------------------------------------------------------------
function  anything_to_bytes_scalar(anything) result(chars)
implicit none

character(len=*),parameter::ident="@(#)M_haskeys::sdbm_hash(3fp): anything to bytes (an array of CHARACTER(LEN=1) variables)"

class(*),intent(in)          :: anything
character(len=1),allocatable :: chars(:)
   select type(anything)

    type is (character(len=*));     chars=transfer(anything,chars)
    type is (complex);              chars=transfer(anything,chars)
    type is (integer(kind=int8));   chars=transfer(anything,chars)
    type is (integer(kind=int16));  chars=transfer(anything,chars)
    type is (integer(kind=int32));  chars=transfer(anything,chars)
    type is (integer(kind=int64));  chars=transfer(anything,chars)
    type is (integer(kind=int128)); chars=transfer(anything,chars)
    type is (real(kind=real32));    chars=transfer(anything,chars)
    type is (real(kind=real64));    chars=transfer(anything,chars)
    type is (real(kind=real128));   chars=transfer(anything,chars)

   end select
end function  anything_to_bytes_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anyscalar_to_double(3f) - [M_anything] convert integer or real parameter of any kind to doubleprecision
!!
!!##SYNOPSIS
!!
!!    pure elemental function anyscalar_to_double(valuein) result(d_out)
!!
!!     class(*),intent(in)  :: valuein
!!     doubleprecision      :: d_out
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different
!!    types. It is used to create other procedures that can take many
!!    scalar arguments as input options.
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type DOUBLEPRECISION.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=int128, kind=real32, kind=real64, kind=real128,
!!             or kind=real256
!!##RESULTS
!!             The returned value is of kind DOUBLEPRECISION and is the value of VALUIN
!!             converted to doubleprecision (assuming it is actually in the range of
!!             type DOUBLEPRECISION).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program scalars
!!     use M_anything,     only : int128, real256
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     use iso_fortran_env, only : real32, real64, real128
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2_int128)
!!        write(*,*)squarei(2_real32)
!!        write(*,*)squarei(2_real64)
!!        write(*,*)squarei(2_real128)
!!        write(*,*)squarei(2_real256)
!!     contains
!!
!!     function squarei(invalue) result (dvalue)
!!     use M_anything, only : anyscalar_to_double
!!     implicit none
!!     class(*),intent(in)  :: invalue
!!     doubleprecision      :: invalue_local
!!     doubleprecision      :: dvalue
!!        invalue_local=anyscalar_to_double(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program scalars
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
character(len=*),parameter::ident="&
&@(#)M_anything::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"
class(*),intent(in)     :: valuein
doubleprecision      :: d_out
   select type(valuein)
   type is (integer(kind=int8));   d_out=real(valuein)
   type is (integer(kind=int16));  d_out=real(valuein)
   type is (integer(kind=int32));  d_out=real(valuein)
   type is (integer(kind=int64));  d_out=real(valuein)
   type is (integer(kind=int128)); d_out=real(valuein)
   type is (real(kind=real32));    d_out=real(valuein)
   type is (real(kind=real64));    d_out=real(valuein)
   type is (real(kind=real128));   d_out=real(valuein)
!   type is (real(kind=real256));   d_out=real(valuein)
   end select
end function anyscalar_to_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anyscalar_to_real(3f) - [M_anything] convert integer or real parameter of any kind to real
!!
!!##SYNOPSIS
!!
!!    pure elemental function anyscalar_to_real(valuein) result(r_out)
!!
!!     class(*),intent(in)  :: valuein
!!     real                 :: r_out
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different types.
!!    It is used to create other procedures that can take
!!    many scalar arguments as input options.
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type REAL.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=int128, kind=real32, kind=real64, kind=real128,
!!             or kind=real256
!!##RESULTS
!!             The returned value is of kind REAL and is the value of VALUIN
!!             converted to real (assuming it is actually in the range of
!!             type REAL).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program scalars
!!     use M_anything,     only : int128, real256
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     use iso_fortran_env, only : real32, real64, real128
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2_int128)
!!        write(*,*)squarei(2_real32)
!!        write(*,*)squarei(2_real64)
!!        !!write(*,*)squarei(2_real128)
!!        write(*,*)squarei(2_real256)
!!     contains
!!
!!     function squarei(invalue) result (dvalue)
!!     use M_anything, only : anyscalar_to_real
!!     implicit none
!!     class(*),intent(in)  :: invalue
!!     real                 :: invalue_local
!!     real                 :: dvalue
!!        invalue_local=anyscalar_to_real(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program scalars
!===================================================================================================================================
pure elemental function anyscalar_to_real(valuein) result(r_out)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
character(len=*),parameter::ident="@(#)M_anything::anyscalar_to_real(3f): convert integer or real parameter of any kind to real"
class(*),intent(in)     :: valuein
   real              :: r_out
   select type(valuein)
   type is (integer(kind=int8));   r_out=real(valuein)
   type is (integer(kind=int16));  r_out=real(valuein)
   type is (integer(kind=int32));  r_out=real(valuein)
   type is (integer(kind=int64));  r_out=real(valuein)
   type is (integer(kind=int128)); r_out=real(valuein)
   type is (real(kind=real32));    r_out=real(valuein)
   type is (real(kind=real64));    r_out=real(valuein)
   type is (real(kind=real128));   r_out=real(valuein)
!   type is (real(kind=real256));   r_out=real(valuein)
   end select
end function anyscalar_to_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    anyinteger_to_128bit(3f) - [M_anything] convert integer any kind to integer(kind=128)
!!
!!##SYNOPSIS
!!
!!
!!    pure elemental function anyinteger_to_128bit(intin) result(ii38)
!!
!!     integer(kind=int128) function anyinteger_to_128bit(value)
!!     class(*),intent(in)     :: intin
!!     integer(kind=int8|int16|int32|int64|int128) :: value
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow arguments of different types
!!    generically. It is used to create other procedures that can take
!!    many scalar arguments as input options, equivalent to  passing the
!!    parameter VALUE as int(VALUE,0_int128).
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type REAL(KIND=128).
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=int128, kind=real32, kind=real64, kind=real128,
!!             or kind=real256
!!##RESULTS
!!             The returned value is of kind REAL(KIND=REAL128)  and is
!!             the value of VALUIN converted to real(KIND=REAL128).
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program scalars
!!     use M_anything,     only : int128
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2_int128)
!!     contains
!!
!!     function squarei(invalue)
!!     use M_anything, only : anyinteger_to_128bit, int128
!!     implicit none
!!     class(*),intent(in)  :: invalue
!!     real                 :: invalue_local
!!     integer(kind=int128) :: squarei
!!        invalue_local=anyinteger_to_128bit(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program scalars
!===================================================================================================================================
pure elemental function anyinteger_to_128bit(intin) result(ii38)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
character(len=*),parameter::ident="@(#)M_anything::anyinteger_to_128(3f): convert integer parameter of any kind to 128-bit integer"
class(*),intent(in)     :: intin
   integer(kind=int128) :: ii38
   select type(intin)
   type is (integer(kind=int8));   ii38=int(intin,kind=int128)
   type is (integer(kind=int16));  ii38=int(intin,kind=int128)
   type is (integer(kind=int32));  ii38=intin
   type is (integer(kind=int64));  ii38=intin
   type is (integer(kind=int128)); ii38=intin
   !class default
      !write(error_unit,*)'ERROR: unknown integer type'
      !stop 'ERROR: *anyinteger_to_128* unknown integer type'
   end select
end function anyinteger_to_128bit
!===================================================================================================================================
end module M_anything
!===================================================================================================================================
