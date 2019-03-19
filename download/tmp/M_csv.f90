module M_csv
use M_strings, only   : substitute, v2s
use M_anything, only : anyscalar_to_double
implicit none
private
character(len=1),public       :: G_separator   =','
character(len=10),save,public :: G_quotes      ='default'
integer,save,public           :: G_LUN

!     This module uses the following default rules:
!     - items are always separated by a single comma (,)
!     - string items are delimited by double quotes (")
!     - embedded double quotes are treated by doubling the quote
!     - trailing blanks are considered irrelevant
! OPTIONS
!    build allocatable string and output
!    allow title line
!    allow delimiter
!    quote just strings with delimiter, quote all strings, quote all values
!    allow for printing DAT date_and_time array as an "SQL date"
!    should doubles have D or E exponent?

! build as a line and output line or
! output each item one at a time
! or build internal array or multi-line string and output all at once

! open file
! write items to specific lun or to one file at a time

! subroutine csv_open(
! subroutine csv_write( integer|real|doubleprecision,character, maybe complex?)  ! scalar
! subroutine csv_write( integer|real|doubleprecision,character, maybe complex?)  ! one-dimensional array as one row
! subroutine csv_write( integer|real|doubleprecision,character, maybe complex?)  ! two-dimensional table
! subroutine csv_close

!     module for reading and writing CSV-files
!
!     For convenience, the generic name "csv_write" can be used instead of the individual routines.
!
!     The file to write to must already be opened as a LU-number is passed.
!
!     Layout of the CSV-file:
!     - single items are written to the end of the current record
!     - one-dimensional items are also written to the end of the current record
!     - two-dimensional items are written to separate records, one for each row
!     - except for the two-dimensional versions, all routines allow you to suppress advancing to the next record:
!       - for single items you must indicate whether to advance or not
!       - for one-dimensional items, the argument is optional. Default is to advance.
!
character(len=*),parameter::ident_1="@(#)M_csv::csv_write(3f): write scalar intrinsic type using current CSV style"

public csv_write

interface csv_write
   module procedure csv_write_scalar
   module procedure csv_write_row
   module procedure csv_write_table
end interface

public test_suite_M_csv
contains
!>
!!##NAME
!!      csv_write_scalar(3fp) - Write a single integer/real/double precision real to the CSV-file
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!       The value is written to the current record of the CSV-file
!!##OPTION
!!      lun        LU-number of the CSV-file
!!      value      Value to write
!!      advance    Advance (.true.) or not, so that more items can be
!!##OUTPUT
!!                 written to the same record
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine csv_write_scalar( value, advance, lun )

class(*),intent(in)             :: value
logical,intent(in),optional     :: advance
integer,intent(in),optional     :: lun
   character(len=3)             :: advance_local
   character(len=:),allocatable :: separator
   integer                      :: lun_local
   character(len=:),allocatable :: outstr

   if(present(lun))then
      lun_local=lun
   else
      lun_local=G_lun
   endif
   if(present(advance))then
      if(advance.eqv..true.)then
         advance_local='yes'
      else
         advance_local='no '
      endif
   else
      advance_local='no '
   endif
   if(advance_local.eq.'yes')then
      separator=''
   else
      separator=G_separator
   endif
   select type(value)
   type is (integer)
   type is (real)
   type is (character(len=*))
      allocate(character(len=2*len(value)) :: outstr)
      outstr(:)=' '
      outstr(:len(value))=value
      call substitute(outstr,'"','""')
      write(lun_local,'(a)',advance=advance_local) '"'//trim(outstr)//'"'//separator
      return
   type is (doubleprecision)
   end select

   write(lun_local,'(a)',advance=advance_local) '"'//trim(adjustl(v2s(anyscalar_to_double(value))))//'"'//separator

end subroutine csv_write_scalar
!>
!!##NAME
!!   csv_write_row(3f) - [M_csv] Write a one-dimensional array of items to the CSV-file
!!##SYNOPSIS
!!
!!##OPTIONS
!!       lun        LU-number of the CSV-file
!!       array      Array to write
!!       advance    Advance (.true.) or not, so that more items can be
!!                  written to the same record
!!##RESULT
!!       The array is written to the current record of the CSV-file
!===================================================================================================================================
subroutine csv_write_row( array, advance, lun )
class(*),intent(in)            :: array(:)
logical,intent(in),optional    :: advance
integer,intent(in),optional    :: lun
   logical                     :: advance_local
   integer                     :: i
   integer                     :: ii
   integer                     :: lun_local

   advance_local = .true.
   if ( present(advance) ) advance_local = advance
   lun_local = G_lun
   if ( present(lun) ) lun_local = lun
   write(6,*)'got here b ',advance,anyscalar_to_double(array)
   ii=size(array)
   do i = 1,ii-1
      write(6,*)'got here b.2 ',i,advance,anyscalar_to_double(array(i))
      call csv_write_scalar( array(i), advance=.false. ,lun=lun_local)
   enddo
   write(6,*)'got here c',ii
   call csv_write_scalar( array(ii), advance=advance_local, lun=lun_local )

end subroutine csv_write_row
!>
!!##NAME
!!   csv_write_table(3f) - [M_csv]Write a two-dimensional array of items to the CSV-file
!!##SYNOPSIS
!!
!!##OPTIONS
!!       array      Array to write
!!       lun        LU-number of the CSV-file
!!
!!##RESULT
!!       The array is written to the current CSV-file. One row
!!       generates one line of the file.
!===================================================================================================================================
subroutine csv_write_table( array, lun )
class(*),intent(in)             :: array(:,:)
integer,intent(in),optional     :: lun
    integer                     :: i
    integer                     :: lun_local
    lun_local=G_lun
    if(present(lun))lun_local=lun
    write(6,*)'got here a.1 ',size(array,2)
    write(6,*)'got here a.2 ',size(array,1)
    write(6,*)'got here a.3 ',anyscalar_to_double(array)
    write(6,*)'got here a.3.a ',anyscalar_to_double(array(:,1))
    write(6,*)'got here a.3.b ',anyscalar_to_double(array(:,2))
    write(6,*)'got here a.3.c ',anyscalar_to_double(array(:,3))
    write(6,*)'got here a.3.d ',anyscalar_to_double(array(:,4))
    write(6,*)'got here a.3.e ',anyscalar_to_double(array(1,:))
    write(6,*)'got here a.3.f ',anyscalar_to_double(array(2,:))
    write(6,*)'got here a.3.g ',anyscalar_to_double(array(3,:))
    do i = 1,size(array,2)
        write(6,*)'got here a.4 ',anyscalar_to_double(array(:,i))
        call csv_write_row( array(:,i), advance=.true., lun=lun_local )
    enddo
end subroutine csv_write_table
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_csv()

!! setup
   call test_csv_write_row()
   call test_csv_write_scalar()
   call test_csv_write_table()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv_write_row()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('csv_write_row',msg='')
   !!call unit_check('csv_write_row', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('csv_write_row',msg='')
end subroutine test_csv_write_row
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv_write_scalar()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('csv_write_scalar',msg='')
   !!call unit_check('csv_write_scalar', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('csv_write_scalar',msg='')
end subroutine test_csv_write_scalar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv_write_table()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('csv_write_table',msg='')
   !!call unit_check('csv_write_table', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('csv_write_table',msg='')
end subroutine test_csv_write_table
!===================================================================================================================================
end subroutine test_suite_M_csv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_csv
