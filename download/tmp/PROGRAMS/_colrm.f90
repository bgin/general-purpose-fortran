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
'       _colrm(1f) - [FUNIX]remove columns from a file                           ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       _colrm [first [last]]                                                    ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       _colrm removes selected columns from a file.  Input is taken from        ',&
'       standard input.  Output is sent to standard output.                      ',&
'                                                                                ',&
'       If  called  with  one  parameter  the  columns of each line will         ',&
'       be removed starting with the specified first column.  If called          ',&
'       with two parameters the columns from the first column to the last        ',&
'       column will be removed.                                                  ',&
'                                                                                ',&
'       Column numbering starts with column 1. Tabs are NOT expanded.            ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       first                                                                    ',&
'       last                                                                     ',&
'       --version  Display version information and exit.                         ',&
'       --help     Display help text and exit.                                   ',&
'                                                                                ',&
'HISTORY                                                                         ',&
'       The colrm(1) command appeared in 3.0BSD.                                 ',&
'EXAMPLE                                                                         ',&
'       # trim file so no line is longer than 72 characters                      ',&
'       cat FILENAME|_colrm 73                                                   ',&
'       # remove first three characters in each line                             ',&
'       cat FILENAME|_colrm 1 3                                                  ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _colrm(1f) - [FUNIX]remove columns from a file
!!
!!##SYNOPSIS
!!
!!        _colrm [first [last]]
!!
!!##DESCRIPTION
!!        _colrm removes selected columns from a file.  Input is taken from
!!        standard input.  Output is sent to standard output.
!!
!!        If  called  with  one  parameter  the  columns of each line will
!!        be removed starting with the specified first column.  If called
!!        with two parameters the columns from the first column to the last
!!        column will be removed.
!!
!!        Column numbering starts with column 1. Tabs are NOT expanded.
!!
!!##OPTIONS
!!        first
!!        last
!!        --version  Display version information and exit.
!!        --help     Display help text and exit.
!!
!!##HISTORY
!!        The colrm(1) command appeared in 3.0BSD.
!!##EXAMPLE
!!
!!        # trim file so no line is longer than 72 characters
!!        cat FILENAME|_colrm 73
!!        # remove first three characters in each line
!!        cat FILENAME|_colrm 1 3
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        _colrm(1)>',&
'@(#)DESCRIPTION:    remove a numeric range of characters from stdin>',&
'@(#)VERSION:        1.0, 20180324>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Mon, Jun 4th, 2018 8:59:21 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program colrm
use M_kracken, only : kracken, lget, igets
use M_io, only      : read_all
use M_debug, only   : stderr
implicit none
integer,allocatable          :: columns(:)
character(len=:),allocatable :: line
integer                      :: right
integer                      :: lower
integer                      :: higher
integer                      :: ilen

   call kracken('colrm','-help .false. -version .false. ') ! define command arguments,default values and crack command line
   call help_usage(lget('colrm_help'))                     ! if -help option is present, display help text and exit
   call help_version(lget('colrm_version'))                ! if -version option is present, display version text and exit
   columns=igets('colrm_oo')                               ! get numbers

   select case(size(columns))
   case(0)
      ALL: do while (read_all(line)==0)
         write(*,'(a)')line
      enddo ALL
   case(1)
      right=max(1,columns(1))
      LEFT: do while (read_all(line)==0)
         write(*,'(a)')line(:min(len(line),right-1))
      enddo LEFT
   case(2)
      lower=max(1,min(columns(1),columns(2)))
      higher=max(1,max(columns(1),columns(2)))
      INFINITE: do while (read_all(line)==0)
         ilen=len(line)
         if(lower.gt.1)then
            write(*,'(a)',advance='no')line(:min(lower-1,ilen))
         endif
         if(higher.gt.1)then
            if(higher+1.le.ilen)then
               write(*,'(a)',advance='yes')line(higher+1:)
            else
               write(*,'(a)')
            endif
         endif
      enddo INFINITE
   case default
      call stderr('*colrm* incorrect number of values=',size(columns))
   endselect
end program colrm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
