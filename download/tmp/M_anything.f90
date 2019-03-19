!===================================================================================================================================
! This module and the example function squarei() that uses it shows how you
! can use polymorphism to allow arguments of different types generically
!===================================================================================================================================
module M_anything
!!use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, dp=>real128
use ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
implicit none
private

integer                  :: i

! on this platform, (select_int_kind(i),i=1,100) returns
! 1:2=1 ,3:4=2 ,5:9=4 ,10:18= 8 ,19:38=16 ,39:=-1
integer,parameter        :: k(38)=[(selected_int_kind(i),i=1,38)]
integer,public,parameter :: int128=k(38)

! on this platform, (select_real_kind(i),i=1,100) returns
! 1:6=   4, 7:15 = 8, 16:18= 10, 19:33= 16, 34:  = -1
integer,parameter        :: r(34)=[(selected_int_kind(i),i=1,34)]
integer,public,parameter :: real256=r(34)

public anyinteger_to_64bit  ! convert integer parameter of any kind to 64-bit integer
public anyinteger_to_128bit ! convert integer parameter of any kind to 128-bit integer
public anyscalar_to_real    ! convert integer or real parameter of any kind to real
public anyscalar_to_double  ! convert integer or real parameter of any kind to doubleprecision
public test_suite_M_anything

public anything_to_bytes
interface anything_to_bytes
   module procedure anything_to_bytes_arr
   module procedure anything_to_bytes_scalar
end interface anything_to_bytes
!===================================================================================================================================
!   Because there is no builtin "empty array" object, I've tried to mimic
!   it with some user-defined type (just for fun).  -- spectrum
!
! So, if there is a language support, it might be not too difficult
! to think of a common "empty array" thing (though not sure if it is
! sufficiently useful).
!
public empty, assignment(=)

   type Empty_t
   endtype
   type(Empty_t) empty   !! singleton

   interface assignment(=)
       module procedure      &
       & ints_from_empty,    &
       & reals_from_empty,   &
       & doubles_from_empty,   &
       & strings_from_empty
   endinterface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    empty(3f) - [M_anything] set an alloctable array to zero
!!##SYNOPSIS
!!
!!    use M_anything, only : empty, assignment(=)
!!##DESCRIPTION
!!    A convenience routine that sets an array to an empty set.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_empty
!!    use M_anything, only : empty, assignment(=)
!!    integer, allocatable      :: ints(:)
!!    character(:), allocatable :: strs(:)
!!    real, allocatable      :: reals(:)
!!       ints=empty
!!       write(*,*)size(ints)
!!
!!       write(*,*)'give them some size ...'
!!       reals = [1.0,2.0,3.0]
!!       ints = [1,2,3]
!!       strs = [character(len=10) :: "one","two","three","four"]
!!       write(*,*)size(ints)
!!       write(*,*)size(reals)
!!       write(*,*)size(strs)
!!
!!       ints=empty
!!       reals=empty
!!       strs=empty
!!       write(*,*)'back to empty ...'
!!       write(*,*)size(ints)
!!       write(*,*)size(reals)
!!       write(*,*)size(strs)
!!
!!    end program demo_empty
!!
!!   Expected output:
!!
!!               0
!!     give them some size ...
!!               3
!!               3
!!               4
!!     back to empty ...
!!               0
!!               0
!!               0
!===================================================================================================================================
   subroutine ints_from_empty( x, emp )
       integer, allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( x( 0 ) )
   end subroutine ints_from_empty

   subroutine doubles_from_empty( x, emp )
       doubleprecision, allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( x( 0 ) )
   end subroutine doubles_from_empty

   subroutine reals_from_empty( x, emp )
       real, allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( x( 0 ) )
   end subroutine reals_from_empty

   subroutine strings_from_empty( x, emp )
       character(:), allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( character(0) :: x( 0 ) )
   end subroutine strings_from_empty
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
!!##RETURN
!!
!!    CHARS    The returned value is an array of bytes (character(len=1)).
!!
!!##EXAMPLE
!!
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

character(len=*),parameter::ident_1="@(#)M_haskeys::sdbm_hash(3fp): anything to bytes (an array of CHARACTER(LEN=1) variables)"

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

character(len=*),parameter::ident_2="@(#)M_haskeys::sdbm_hash(3fp): anything to bytes (an array of CHARACTER(LEN=1) variables)"

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
!!
!!    D_OUT    The value of VALUIN converted to doubleprecision (assuming
!!             it is actually in the range of type DOUBLEPRECISION).
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_anyscalar_to_double
!!     use M_anything,      only : int128, real256
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
!!     class(*),intent(in)  :: invalue
!!     doubleprecision      :: invalue_local
!!     doubleprecision      :: dvalue
!!        invalue_local=anyscalar_to_double(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program demo_anyscalar_to_double
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

character(len=*),parameter::ident_3="&
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
   class default
     d_out=0.0d0
     !!stop '*M_anything::anyscalar_to_double: unknown type'
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
!!
!!    R_OUT    The value of VALUIN converted to real (assuming it is actually
!!             in the range of type REAL).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_anyscalar_to_real
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
!!     class(*),intent(in)  :: invalue
!!     real                 :: invalue_local
!!     real                 :: dvalue
!!        invalue_local=anyscalar_to_real(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program demo_anyscalar_to_real
!===================================================================================================================================
pure elemental function anyscalar_to_real(valuein) result(r_out)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

character(len=*),parameter::ident_4="@(#)M_anything::anyscalar_to_real(3f): convert integer or real parameter of any kind to real"

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
!!    anyinteger_to_64bit(3f) - [M_anything] convert integer any kind to integer(kind=64)
!!
!!##SYNOPSIS
!!
!!
!!    pure elemental function anyinteger_to_64bit(intin) result(ii38)
!!
!!     integer(kind=int64) function anyinteger_to_64bit(value)
!!     class(*),intent(in)     :: intin
!!     integer(kind=int8|int16|int32|int64) :: value
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow arguments of different types
!!    generically. It is used to create other procedures that can take
!!    many scalar arguments as input options, equivalent to passing the
!!    parameter VALUE as int(VALUE,0_int64).
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type INTEGER(KIND=64).
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64.
!!##RESULTS
!!             The value of VALUIN converted to INTEGER(KIND=INT64).
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_anyinteger_to_64bit
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(huge(0_int8)),huge(0_int8) , &
!!        & '16129'
!!        write(*,*)squarei(huge(0_int16)),huge(0_int16) , &
!!        & '1073676289'
!!        write(*,*)squarei(huge(0_int32)),huge(0_int32) , &
!!        & '4611686014132420609'
!!        write(*,*)squarei(huge(0_int64)),huge(0_int64) , &
!!        & '85070591730234615847396907784232501249'
!!     contains
!!     !
!!     function squarei(invalue)
!!     use M_anything, only : anyinteger_to_64bit
!!     class(*),intent(in)  :: invalue
!!     doubleprecision      :: invalue_local
!!     doubleprecision      :: squarei
!!        invalue_local=anyinteger_to_64bit(invalue)
!!        squarei=invalue_local*invalue_local
!!     end function squarei
!!     !
!!     end program demo_anyinteger_to_64bit
!!
!!   Results
!!
!!    16129.000000000000       127 !!    16129
!!    1073676289.0000000       32767 !!    1073676289
!!    4.6116860141324206E+018  2147483647 !!    4611686014132420609
!!    8.5070591730234616E+037  9223372036854775807 !!    85070591730234615847396907784232501249
!!    2.8948022309329049E+076 170141183460469231731687303715884105727 !!    28948022309329048855892746252171976962977213799489202546401021394546514198529
!===================================================================================================================================
pure elemental function anyinteger_to_64bit(intin) result(ii38)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

character(len=*),parameter::ident_5="@(#)M_anything::anyinteger_to_64(3f): convert integer parameter of any kind to 64-bit integer"

class(*),intent(in)     :: intin
   integer(kind=int64) :: ii38
   select type(intin)
   type is (integer(kind=int8));   ii38=int(intin,kind=int64)
   type is (integer(kind=int16));  ii38=int(intin,kind=int64)
   type is (integer(kind=int32));  ii38=intin
   type is (integer(kind=int64));  ii38=intin
   !class default
      !write(error_unit,*)'ERROR: unknown integer type'
      !stop 'ERROR: *anyinteger_to_64* unknown integer type'
   end select
end function anyinteger_to_64bit
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    anyinteger_to_128bit(3f) - [M_anything] convert integer of any kind to integer(kind=128)
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
!!    many scalar arguments as input options, equivalent to passing the
!!    parameter VALUE as int(VALUE,0_int128).
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type INTEGER(KIND=128).
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=int128.
!!##RESULTS
!!             The value of VALUIN converted to INTEGER(KIND=INT128).
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_anyinteger_to_128bit
!!     use M_anything,     only : int128
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(huge(0_int8)),huge(0_int8) , &
!!        & '16129'
!!        write(*,*)squarei(huge(0_int16)),huge(0_int16) , &
!!        & '1073676289'
!!        write(*,*)squarei(huge(0_int32)),huge(0_int32) , &
!!        & '4611686014132420609'
!!        write(*,*)squarei(huge(0_int64)),huge(0_int64) , &
!!        & '85070591730234615847396907784232501249'
!!        write(*,*)squarei(huge(0_int128)),huge(0_int128) , &
!!        & '28948022309329048855892746252171976962977213799489202546401021394546514198529'
!!     contains
!!     !
!!     function squarei(invalue)
!!     use M_anything, only : anyinteger_to_128bit, int128
!!     class(*),intent(in)  :: invalue
!!     doubleprecision      :: invalue_local
!!     doubleprecision      :: squarei
!!        invalue_local=anyinteger_to_128bit(invalue)
!!        squarei=invalue_local*invalue_local
!!     end function squarei
!!     !
!!     end program demo_anyinteger_to_128bit
!!
!!   Results
!!
!!    16129.000000000000       127 !!    16129
!!    1073676289.0000000       32767 !!    1073676289
!!    4.6116860141324206E+018  2147483647 !!    4611686014132420609
!!    8.5070591730234616E+037  9223372036854775807 !!    85070591730234615847396907784232501249
!!    2.8948022309329049E+076 170141183460469231731687303715884105727 !!    28948022309329048855892746252171976962977213799489202546401021394546514198529
!===================================================================================================================================
pure elemental function anyinteger_to_128bit(intin) result(ii38)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

character(len=*),parameter::ident_6="&
&@(#)M_anything::anyinteger_to_128(3f): convert integer parameter of any kind to 128-bit integer"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_anything()

!! setup
   call test___copy_COMPLEX_4()
   call test___copy_INTEGER_16()
   call test___copy_INTEGER_1()
   call test___copy_INTEGER_2()
   call test___copy_INTEGER_4()
   call test___copy_INTEGER_8()
   call test___copy_REAL_16()
   call test___copy_REAL_4()
   call test___copy_REAL_8()
   call test___copy_character_1()
   call test___copy_m_anything_Empty_t()
   call test_anyinteger_to_128bit()
   call test_anyinteger_to_64bit()
   call test_anyscalar_to_double()
   call test_anyscalar_to_real()
   call test_anything_to_bytes_arr()
   call test_anything_to_bytes_scalar()
   call test_doubles_from_empty()
   call test_ints_from_empty()
   call test_reals_from_empty()
   call test_strings_from_empty()
!!teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_COMPLEX_4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_COMPLEX_4',msg='')
   !!call unit_check('__copy_COMPLEX_4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_COMPLEX_4',msg='')
end subroutine test___copy_COMPLEX_4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_INTEGER_16()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_INTEGER_16',msg='')
   !!call unit_check('__copy_INTEGER_16', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_INTEGER_16',msg='')
end subroutine test___copy_INTEGER_16
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_INTEGER_1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_INTEGER_1',msg='')
   !!call unit_check('__copy_INTEGER_1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_INTEGER_1',msg='')
end subroutine test___copy_INTEGER_1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_INTEGER_2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_INTEGER_2',msg='')
   !!call unit_check('__copy_INTEGER_2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_INTEGER_2',msg='')
end subroutine test___copy_INTEGER_2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_INTEGER_4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_INTEGER_4',msg='')
   !!call unit_check('__copy_INTEGER_4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_INTEGER_4',msg='')
end subroutine test___copy_INTEGER_4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_INTEGER_8()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_INTEGER_8',msg='')
   !!call unit_check('__copy_INTEGER_8', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_INTEGER_8',msg='')
end subroutine test___copy_INTEGER_8
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_REAL_16()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_REAL_16',msg='')
   !!call unit_check('__copy_REAL_16', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_REAL_16',msg='')
end subroutine test___copy_REAL_16
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_REAL_4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_REAL_4',msg='')
   !!call unit_check('__copy_REAL_4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_REAL_4',msg='')
end subroutine test___copy_REAL_4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_REAL_8()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_REAL_8',msg='')
   !!call unit_check('__copy_REAL_8', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_REAL_8',msg='')
end subroutine test___copy_REAL_8
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_character_1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_character_1',msg='')
   !!call unit_check('__copy_character_1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_character_1',msg='')
end subroutine test___copy_character_1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_m_anything_Empty_t()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('__copy_m_anything_Empty_t',msg='')
   !!call unit_check('__copy_m_anything_Empty_t', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('__copy_m_anything_Empty_t',msg='')
end subroutine test___copy_m_anything_Empty_t
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyinteger_to_128bit()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('anyinteger_to_128bit',msg='')
   !!call unit_check('anyinteger_to_128bit', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('anyinteger_to_128bit',msg='')
end subroutine test_anyinteger_to_128bit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyinteger_to_64bit()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('anyinteger_to_64bit',msg='')
   !!call unit_check('anyinteger_to_64bit', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('anyinteger_to_64bit',msg='')
end subroutine test_anyinteger_to_64bit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_double()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('anyscalar_to_double',msg='')
   !!call unit_check('anyscalar_to_double', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('anyscalar_to_double',msg='')
end subroutine test_anyscalar_to_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_real()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('anyscalar_to_real',msg='')
   !!call unit_check('anyscalar_to_real', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('anyscalar_to_real',msg='')
end subroutine test_anyscalar_to_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anything_to_bytes_arr()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('anything_to_bytes_arr',msg='')
   !!call unit_check('anything_to_bytes_arr', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('anything_to_bytes_arr',msg='')
end subroutine test_anything_to_bytes_arr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anything_to_bytes_scalar()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('anything_to_bytes_scalar',msg='')
   !!call unit_check('anything_to_bytes_scalar', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('anything_to_bytes_scalar',msg='')
end subroutine test_anything_to_bytes_scalar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_doubles_from_empty()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('doubles_from_empty',msg='')
   !!call unit_check('doubles_from_empty', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('doubles_from_empty',msg='')
end subroutine test_doubles_from_empty
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ints_from_empty()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('ints_from_empty',msg='')
   !!call unit_check('ints_from_empty', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('ints_from_empty',msg='')
end subroutine test_ints_from_empty
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reals_from_empty()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('reals_from_empty',msg='')
   !!call unit_check('reals_from_empty', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('reals_from_empty',msg='')
end subroutine test_reals_from_empty
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_strings_from_empty()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('strings_from_empty',msg='')
   !!call unit_check('strings_from_empty', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('strings_from_empty',msg='')
end subroutine test_strings_from_empty
!===================================================================================================================================
end subroutine test_suite_M_anything
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_anything
!===================================================================================================================================
