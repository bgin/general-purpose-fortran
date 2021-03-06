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
'   cprint(1f) - [FILE FILTER] filter prints specified columns                   ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   cprint [ columns ][ -delimiters delim] |-help|-version                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   cprint is a filter that prints the specified columns                         ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    columns      numbers indicating which columns to print                      ',&
'    -delimiters  input column delimiter character(s) (default: whitespace)      ',&
'    -help        display command help and exit                                  ',&
'    -version     display command metadata and exit                              ',&
'EXAMPLES                                                                        ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'      >echo d e h l o r w|cprint 3 2 4 4 5 7 5 6 4 1                            ',&
'      h e l l o w o r l d                                                       ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    cprint(1f) - [FILE FILTER] filter prints specified columns
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    cprint [ columns ][ -delimiters delim] |-help|-version
!!
!!##DESCRIPTION
!!    cprint is a filter that prints the specified columns
!!
!!##OPTIONS
!!     columns      numbers indicating which columns to print
!!     -delimiters  input column delimiter character(s) (default: whitespace)
!!     -help        display command help and exit
!!     -version     display command metadata and exit
!!##EXAMPLES
!!
!!   Sample usage:
!!
!!       >echo d e h l o r w|cprint 3 2 4 4 5 7 5 6 4 1
!!       h e l l o w o r l d
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
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
'@(#)PROGRAM:        cprint(1f)>',&
'@(#)DESCRIPTION:    filter to print specified columns>',&
'@(#)VERSION:        1.0, 20170224>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Fri, Nov 29th, 2019 9:53:37 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program cprint
use M_kracken, only : kracken, igets,  sget, lget, sgets
use M_strings, only : split
use M_io, only : read_line
implicit none

character(len=*),parameter::ident_1="@(#)cprint(1f): filter to specified columns"

character(len=:),allocatable       :: line
character(len=:),allocatable       :: delimiters ! characters used to delimit columns
character(len=:),allocatable       :: array(:)
integer,allocatable                :: icols(:)
character(len=4096),allocatable    :: acols(:)
integer                            :: isize
integer                            :: i
logical                            :: verbose
!-----------------------------------------------------------------------------------------------------------------------------------
call kracken('cprint',      &                    ! define command options and default values and then process command-line arguments
'&
& -delimiters               &
& -help .F.                 &
& -version .F.              &
& -verbose .F. ')
call help_usage(lget('cprint_help'))             ! if -help option is present, display help text and exit
call help_version(lget('cprint_version'))        ! if -version option is present, display version text and exit
delimiters=sget('cprint_delimiters')             ! get -delimiters values
icols=igets('cprint_oo')
acols=sgets('cprint_oo')
verbose=lget('cprint_verbose')
if(verbose)then
   write(*,'("COLUMNS=",*("[",i0,"]":","))')icols
endif
!-----------------------------------------------------------------------------------------------------------------------------------
INFINITE: do while (read_line(line)==0)
   call split(line,array,delimiters)             ! split line into columns
   isize=size(array)
   do i=1,size(icols)
      if( icols(i).gt.0 .and. icols(i).le.isize )then
         write(*,'(a,1x)',advance='no')trim(array(icols(i)))
      else
         write(*,'(a,1x)',advance='no')trim(acols(i))
      endif
   enddo
   write(*,*)
enddo INFINITE
end program
!===================================================================================================================================
