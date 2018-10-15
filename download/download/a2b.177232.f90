program a2b
!  @(#) convert ASCII USH sequential file to BINARY format
use M_journal,  only : journal
use M_kracken,  only : kracken, lget, sget
use M_xyplot,   only : imaxq2
use M_zebra
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit ! access computing environment
implicit none

!#include "header.h"
!=======================================================================--------====================================================
!  I/O units
integer,parameter   :: iin=55
integer,parameter   :: iout=66
character(len=1024) :: infile
character(len=1024) :: ofile
!=======================================================================--------====================================================
!  components of a data record
integer             :: icode
character(len=8)    :: varid
integer             :: node,subnode,tertiary
integer             :: units
character(len=20)   :: alpha
character(len=60)   :: alpha3
real                :: rpad(9)
character(len=80)   :: alpha2
integer             :: time
integer             :: extra(5)
integer             :: i10
integer             :: icount
integer             :: iend80
integer             :: ierr
integer             :: ii
integer             :: ilen
integer             :: info
integer             :: ios
integer             :: isets
integer             :: istyle
integer             :: ivars
real                :: rmax
real                :: rmin
logical             :: debug
character(len=256)  :: msg
!=======================================================================--------====================================================
!  storage array
real array(IMAXQ2)
!=======================================================================--------====================================================
character(len=80)   :: title
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!=======================================================================--------====================================================
!  GET THE COMMAND LINE PARAMETERS
   call iftrail() ! open unit 92 for jun*() routines as trail file if $USHTRAIL is set
!=======================================================================--------====================================================
!  define the command options and default values and apply arguments from user command line
   call kracken("a2b", " -i -o -help F -version F -debug F")
   call help_usage(lget('a2b_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('a2b_version'))                           ! if -version option is present, display version text and exit
   debug=merge(.true.,.false.,lget('a2b_debug'))
   call journal(debug,'debug')
   call journal('D','*a2b*')
!=======================================================================--------====================================================
   isets=0    ! count number of data vectors successfully read
!=======================================================================--------====================================================
!  OPEN INPUT FILE
   ilen=len(infile)
   infile=sget("a2b_i")
   icount=0
   do while(len_trim(infile).eq.0)
      write(stdout,'(a)',advance='no')' Enter portable input data file''s name: '
      read(stdin,'(a)',iostat=ios)infile
      icount=icount+1
      if(icount.gt.5)stop 1
   enddo
   open(unit=iin,file=infile,iostat=ios,access='sequential', form='formatted',status='old')
   if(ios.ne.0)then
      call journal('*a2b* error opening ASCII input file ['//infile(:len_trim(infile))//']')
      goto 999
   endif
   rewind(iin)
   call journal('D','*a2b* opened input file ',infile)
!=======================================================================--------====================================================
!  OPEN OUTPUT FILE
   ilen=len(ofile)
   ofile=sget("a2b_o")
   icount=0
   do while(len_trim(ofile).eq.0)
      write(stdout,'(a)',advance='no')' Enter binary output data file''s name: '
      read(stdin,'(a)',iostat=ios)ofile
      icount=icount+1
      if(icount.gt.5)stop 2
   enddo
   call openbig(iout,ofile,'new',' ',ierr,msg)
   if(ierr.ne.0)then
      call journal('*a2b* error opening binary output file ['//ofile(:len_trim(ofile))//']')
      goto 999
   endif
   rewind(iout)
   call journal('D','*a2b* opened output file ',ofile)
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!=======================================================================--------====================================================
   istyle=0
   call journal('D','*a2b* read title lines')
   do ! read title lines until a line does not end in backslash
      read(iin,'(a)',iostat=ios)title
      if(ios.ne.0)then
         call journal('*a2b* error reading title')
         goto 999
      endif
      call slashto80(title,iend80)
      call journal(title)
      if(title(1:14).eq.'#USHBIN1.0'.and.istyle.eq.0)istyle=1
      if(title(1:14).eq.'#USHBIN2.0'.and.istyle.eq.0)istyle=2
      write(iout)title
      ! if last character of title is \, get another title line
      ilen=max(1,len_trim(title))
      if(title(ilen:ilen).ne.char(92))exit
   enddo
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!=======================================================================--------====================================================
   if(istyle.eq.0)istyle=1
   alpha2=' '
   do i10=1,9
      rpad(i10)=0.0
   enddo
   rmin=0.0
   rmax=0.0

   ivars=0
   call journal('D','*a2b* read headers:')
   do
      if(istyle.eq.1)then
         read(iin,'(i5,a8,4(i5),a20,i5,5(i5))',iostat=ios)icode,varid,node,subnode,tertiary, units,alpha,time,extra
         if(ios.ne.0)then
            call journal('*a2b* error reading type 1 header line')
            goto 999
         endif
         write(iout)icode,varid,node,subnode,tertiary, units,alpha,time,extra
      else
         read(iin,404,iostat=ios)icode,varid,node,subnode,tertiary, units,alpha,alpha3,time,extra,rmin,rmax,alpha2,rpad
         if(ios.ne.0)then
            call journal('*a2b* error reading type 2 header line')
            goto 999
         endif
         write(iout)icode,varid,node,subnode,tertiary, units,alpha,alpha3,time,extra,rmin,rmax,alpha2,rpad
      endif
      ivars=ivars+1
      if(varid.eq.'END') exit
   enddo

   ivars=ivars-1
   call journal('sc','*a2b* number of variables is ',ivars)
   IF(ivars.GT.IMAXQ2)then
         info=IMAXQ2
         call journal('sc','*a2b* error exceeded max var limit ',info)
         goto 999
   endif
   DO
      READ(iin,'(4G20.13)',iostat=ios,END=999)(array(ii),ii=1,ivars)
      if(ios.ne.0)then
         call journal('*a2b* error reading data line')
         goto 999
      endif
      WRITE(iout) (array(ii),ii=1,ivars)
      isets=isets+1
   ENDDO
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!=======================================================================--------====================================================
999 continue
   call journal('sc','*a2b* number of sets of data is ',isets)
404 format(i5,a8,4(i5),a20,a60,i5,5(i5),2g20.13,a80,9g20.13)
   call journal('D','*a2b* exit')
contains
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
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
'@(#)PROGRAM:        a2b(1)>',&
'@(#)DESCRIPTION:    Convert ASCII version of a standard USH input file to a binary standard USH file.>',&
'@(#)VERSION:        2.0, 20180811>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Thu, Sep 20th, 2018 6:04:24 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
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
'    a2b(1f) - [SHELL] Convert ASCII version of a standard USH input file to a binary standard USH file.',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    a2b -i INFILE -o OUTFILE [-help] [-version]                                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Convert an ASCII version of a standard USH input file to a standard          ',&
'   binary USH input file.                                                       ',&
'                                                                                ',&
'   Formatted ASCII USH files are typically generated by a program. This         ',&
'   utility is sometimes used in conjunction with b2a(1f) to convert             ',&
'   binary files on one platform to the more portable text version,              ',&
'   which is then transferred to a new platform and converted back to            ',&
'   binary with a2b(1f).                                                         ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    -i INFILE   input file                                                      ',&
'    -o OUTFILE  output file                                                     ',&
'    --help      display this help and exit                                      ',&
'    --version   output version information and exit                             ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'   Sample commands                                                              ',&
'                                                                                ',&
'    a2b -i nuplot.ascii -o nuplot.bin                                           ',&
'                                                                                ',&
'REPORTING BUGS                                                                  ',&
'   Report a2b(1f) bugs to <http://www.urbanjost.altervista.org/index.html>      ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'   az2bz(1f), b2a(1f), b2z(1f), bz2az(1f), c2b(1f)                              ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     a2b(1f) - [SHELL] Convert ASCII version of a standard USH input file to a binary standard USH file.
!!
!!##SYNOPSIS
!!
!!     a2b -i INFILE -o OUTFILE [-help] [-version]
!!
!!##DESCRIPTION
!!    Convert an ASCII version of a standard USH input file to a standard
!!    binary USH input file.
!!
!!    Formatted ASCII USH files are typically generated by a program. This
!!    utility is sometimes used in conjunction with b2a(1f) to convert
!!    binary files on one platform to the more portable text version,
!!    which is then transferred to a new platform and converted back to
!!    binary with a2b(1f).
!!
!!##OPTIONS
!!     -i INFILE   input file
!!     -o OUTFILE  output file
!!     --help      display this help and exit
!!     --version   output version information and exit
!!
!!##EXAMPLES
!!
!!    Sample commands
!!
!!     a2b -i nuplot.ascii -o nuplot.bin
!!
!!##REPORTING BUGS
!!    Report a2b(1f) bugs to <http://www.urbanjost.altervista.org/index.html>
!!
!!##SEE ALSO
!!    az2bz(1f), b2a(1f), b2z(1f), bz2az(1f), c2b(1f)
!===================================================================================================================================
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end program a2b
!=======================================================================--------====================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
