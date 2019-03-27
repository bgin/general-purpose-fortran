!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this is a utility program. It is typically built using ccall(1).
!-----------------------------------------------------------------------------------------------------------------------------------
program uuidgen
use M_debug,   only : stderr
use M_kracken, only : kracken, lget, sgets, iget
use M_uuid, only : generate_uuid
implicit none
integer :: version
integer :: repeat
integer :: i,j
character(len=10),allocatable :: methods(:)
character(len=:),allocatable  :: prefix
!-----------------------------------------------------------------------------------------------------------------------------------
   ! define arguments, default values and crack command line
   call kracken('uuidgen','-help .false. -version .false. --method --urn .f. --repeat 1')
   call help_usage(lget('uuidgen_help'))                         ! if -help option is present, display help text and exit
   call help_version(lget('uuidgen_version'))                    ! if -version option is present, display version text and exit
!!==============================
GET_METHODS : BLOCK
!! GET RUN_TIME ERROR ON NOT HAVING SAME STRING LENGTHS IN CONSTRUCTOR
character(len=10),allocatable :: methods1(:)
character(len=10),allocatable :: methods2(:)
!! methods=[[character(len=10) :: sgets('uuidgen_method')], &    ! get value of command line argument -method
!!         &[character(len=10) :: sgets('uuidgen_oo')]]          ! add default parameters to method list
   methods1=[character(len=10) :: sgets('uuidgen_method')]       ! get value of command line argument -method
   methods2=[character(len=10) :: sgets('uuidgen_oo')]           ! add default parameters to method list
   if(size(methods1).ne.0.and.size(methods2).ne.0)then
      !! GET RUN_TIME ERROR IF ONE OF THE ARRAYS IS OF SIZE 0
      methods=[methods1,methods2]
   elseif(size(methods2).ne.0)then
      methods=methods2
   elseif(size(methods1).ne.0)then
      methods=methods1
   else
      methods=['']
   endif
ENDBLOCK GET_METHODS
!!==============================
   repeat=iget('uuidgen_repeat')                                 ! get value of command line argument -repeat
   prefix=merge('urn:uuid:','         ',lget('uuidgen_urn'))     ! get value of command line argument -urn
   prefix=trim(prefix)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(methods)
      select case(methods(i))
      case(   '0','nil');    version=0
      case(   '1','time');   version=1
      case('','4','random'); version=4
      case default;          version=4
         call stderr('*uuidgen* unknown method'//methods(i))
         !!stop 1
      endselect
      do j=1,repeat
         write(*,'(2a)')prefix,generate_uuid(version)
      enddo
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
contains
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        _uuidgen(1)>',&
'@(#)DESCRIPTION:    output a UUID (Universally Unique ID)>',&
'@(#)VERSION:        1.0, 20180427>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Mon, Mar 25th, 2019 12:07:32 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'    _uuidgen(1f) - [FUNIX] generate a UUID (Universally Unique ID) string per RFC 4122',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    _uuidgen [[--method NAME][-urn][-repeat N]]|[--help|--version]              ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   _uuidgen(3f) generates UUID strings according to the RFC 4122                ',&
'   standard.                                                                    ',&
'                                                                                ',&
'   A universally unique identifier (UUID) is a 128-bit number used to           ',&
'   identify information in computer systems. When generated according           ',&
'   to standard methods UUIDs are for practical purposes unique.                 ',&
'                                                                                ',&
'   Standard methods 0,1, and 4 are supported as described in RFC 4122.          ',&
'                                                                                ',&
'   UUID strings are particularly useful as keys for relational database         ',&
'   entries, and for building unique temporary file names (especially in         ',&
'   cross-mounted filesystems that more than one OS is utilizing).               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    --method NAME  Select the UUID version type. Supported methods are          ',&
'                   nil|0, random|4, time|1.                                     ',&
'                                                                                ',&
'                   0. Nil UUID (ie. ''00000000-0000-0000-0000-000000000000'')   ',&
'                   1. time-based UUID                                           ',&
'                   2. Not implemented                                           ',&
'                   3. Not implemented                                           ',&
'                   4. pseudo-RNG(Random Number Generator) based                 ',&
'                   5. Not implemented                                           ',&
'                                                                                ',&
'    --urn       RFC 4122 defines a Uniform Resource Name (URN)                  ',&
'                namespace for UUIDs. IE., the output is                         ',&
'                prefixed with "urn:uuid:".                                      ',&
'                                                                                ',&
'    --repeat N  Number of UUID strings to generate                              ',&
'                                                                                ',&
'    --help      display this help and exit                                      ',&
'                                                                                ',&
'    --version   output version information and exit                             ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'  Sample commands                                                               ',&
'                                                                                ',&
'   _uuidgen                                                                     ',&
'   4bb8051e-4af3-11e8-6603-4254ffee9a14                                         ',&
'                                                                                ',&
'   _uuidgen -urn                                                                ',&
'   urn:uuid:e9fd7cab-69f2-4cd6-4b5e-d54b9fbf617a                                ',&
'                                                                                ',&
'   _uuidgen -method time -repeat 4                                              ',&
'   3d684844-4b33-11e8-465d-426c6bd7f9d4                                         ',&
'   3d686f8e-4b33-11e8-465d-3251f8fa45ae                                         ',&
'   3d686f8e-4b33-11e8-465d-46ec7c7b05e1                                         ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     _uuidgen(1f) - [FUNIX] generate a UUID (Universally Unique ID) string per RFC 4122
!!
!!##SYNOPSIS
!!
!!     _uuidgen [[--method NAME][-urn][-repeat N]]|[--help|--version]
!!
!!##DESCRIPTION
!!    _uuidgen(3f) generates UUID strings according to the RFC 4122
!!    standard.
!!
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems. When generated according
!!    to standard methods UUIDs are for practical purposes unique.
!!
!!    Standard methods 0,1, and 4 are supported as described in RFC 4122.
!!
!!    UUID strings are particularly useful as keys for relational database
!!    entries, and for building unique temporary file names (especially in
!!    cross-mounted filesystems that more than one OS is utilizing).
!!
!!##OPTIONS
!!     --method NAME  Select the UUID version type. Supported methods are
!!                    nil|0, random|4, time|1.
!!
!!                    0. Nil UUID (ie. '00000000-0000-0000-0000-000000000000')
!!                    1. time-based UUID
!!                    2. Not implemented
!!                    3. Not implemented
!!                    4. pseudo-RNG(Random Number Generator) based
!!                    5. Not implemented
!!
!!     --urn       RFC 4122 defines a Uniform Resource Name (URN)
!!                 namespace for UUIDs. IE., the output is
!!                 prefixed with "urn:uuid:".
!!
!!     --repeat N  Number of UUID strings to generate
!!
!!     --help      display this help and exit
!!
!!     --version   output version information and exit
!!
!!##EXAMPLES
!!
!!   Sample commands
!!
!!    _uuidgen
!!    4bb8051e-4af3-11e8-6603-4254ffee9a14
!!
!!    _uuidgen -urn
!!    urn:uuid:e9fd7cab-69f2-4cd6-4b5e-d54b9fbf617a
!!
!!    _uuidgen -method time -repeat 4
!!    3d684844-4b33-11e8-465d-426c6bd7f9d4
!!    3d686f8e-4b33-11e8-465d-3251f8fa45ae
!!    3d686f8e-4b33-11e8-465d-46ec7c7b05e1
!===================================================================================================================================
end program uuidgen
