program b2a                                               ! @(#) convert BINARY USH sequential file to ASCII format
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
use M_kracken, only : kracken, lget, sget,setprompts             ! command line argument parsing
use M_zebra
use M_xyplot,  only : imaxq2
!-----------------------------------------------------------------------------------------------------------------------------------
!#include "header.h"
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter   :: iout=66,iin=55                     ! unit numbers for input and output files
character(len=1024) :: filein                             ! input file to read data from
character(len=1024) :: fileout                            ! output file to generate
character(len=256)  :: msg
!-----------------------------------------------------------------------------------------------------------------------------------
integer             :: icode
character(len=8)    :: varid
integer             :: node,subnode,tertiary
integer             :: units
character(len=20)   :: alpha
character(len=60)   :: alpha3
character(len=80)   :: alpha2
real                :: rmin, rmax
real                :: rpad(9)
integer             :: time
integer             :: extra(5)
real                :: array(IMAXQ2)
character(len=80)   :: title
!-----------------------------------------------------------------------------------------------------------------------------------
   call iftrail()                  ! if $USHTRAIL environment variable set, set trail file to it; else set trail to a scratch file
   isets=0 ! count of number of data sets written
!-----------------------------------------------------------------------------------------------------------------------------------
!   GET THE COMMAND LINE PARAMETERS
!   define the command option prompts
    call setprompts('b2a', '                              &
   & -oo "#N#"                                            &
   & -i name of sequential binary input file              &
   & -o name of sequential ASCII output file              &
   & -help "#N#"                                          &
   & -version "#N#"                                       &
   & ')
!  define the command options and default values and apply arguments from user command line
   call kracken("b2a", " -i -o -help F -version F")
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget("b2a_help"))then                 ! -help present?
      call journal('================================================================================')
      call journal(' b2a -i INFILE -o OUTFILE [-help] [-version]                                    ')
      call journal('                                                                                ')
      call journal(' Convert a standard binary USH input file to an ASCII USH file.                 ')
      call journal('                                                                                ')
      call journal('================================================================================')
      stop
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget("b2a_version"))then              ! -version present?
      call journal(' b2a version 3.0-20110528')
      stop
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  GET INPUT FILE NAME
   filein=sget("b2a_i")
   icount=0
   do while(len_trim(filein).eq.0)
      write(*,'(a)',advance='no')' Enter name of input binary USH data file: '
      read(*,'(a)',iostat=ios)filein
      icount=icount+1
      if(icount.gt.5)stop
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!  GET OUTPUT FILE NAME
   fileout=sget("b2a_o")
   icount=0
   do while(len_trim(fileout).eq.0)
      write(*,'(a)',advance='no')' Enter ASCII USH output data file''s name:   '
      read(*,'(a)',iostat=ios)fileout
      icount=icount+1
      if(icount.gt.5)stop
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
! OPEN INPUT FILE
      call openbig(iin,filein,'old',' ',ierr,msg)
      if(ierr.ne.0)then
         call journal(' *b2a* error opening binary input file')
         goto 999
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
! OPEN OUTPUT FILE
      open(unit=iout,file=fileout,iostat=ios,access='sequential', form='formatted',status='new')
      if(ios.ne.0)then
         call journal(' *b2a* error opening ascii output file')
         goto 999
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ititle=0
      istyle=0
      TITLELOOP: do
         read(iin,iostat=ios)title
         if(ios.ne.0)then
            call journal('sc',' *b2a* error reading title=',ititle+1)
            goto 999
         endif
         if(title(1:14).eq.'#USHBIN1.0'.and.istyle.eq.0)istyle=1
         if(title(1:14).eq.'#USHBIN2.0'.and.istyle.eq.0)istyle=2
         ititle=ititle+1
         if(index('$#',title(1:1)).eq.0)then
            call journal(title)
         endif
         call slashto80(title,iend80)
         write(iout,'(a)')title
         ! if last character of title is \, get another title line
         ilen=max(1,len_trim(title))
      if(title(ilen:ilen).ne.char(92))exit TITLELOOP
      enddo TITLELOOP
!-----------------------------------------------------------------------------------------------------------------------------------
      if(istyle.eq.0)istyle=1
!-----------------------------------------------------------------------------------------------------------------------------------
      ivars=0
      READHEADERS: do
         if(istyle.eq.1)then
            read(iin,iostat=ios)icode,varid,node,subnode,tertiary,units,alpha,time,extra
            if(ios.ne.0)then
               call journal('sc',' *b2a* error reading header style 1. line=',ivars+1)
               goto 999
            endif
            write(iout,202)icode      ,varid  ,node   ,subnode,tertiary,units  ,alpha   ,time   ,extra
202         format        (i5         ,a8     ,i5     ,i5     ,i5      ,i5     ,a20     ,i5     ,5(i5  ))

         else
            read(iin,iostat=ios)icode,varid,node,subnode,tertiary,units,alpha,alpha3,time,extra,rmin,rmax,alpha2,rpad
            if(ios.ne.0)then
               call journal('sc',' *b2a* error reading header line=',ivars+1)
               goto 999
            endif
            write(iout,404)icode,varid,node,subnode,tertiary,units,alpha,alpha3,time,extra,rmin,rmax,alpha2,rpad
404         format(i5,a8,i5,i5,i5,i5,a20,a60,i5,5(i5),2g20.13,a80,9g20.13)
         endif
         ivars=ivars+1
         if(varid.eq.'END')exit READHEADERS
      enddo READHEADERS
!-----------------------------------------------------------------------------------------------------------------------------------
      ivars=ivars-1
         call journal('sc',' *b2a* number of variables is ',ivars)
      info=IMAXQ2
      if(ivars.gt.info)then
         call journal('sc',' *b2a* error exceeded max var limit ',info)
         goto 999
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      do
         read(iin,err=7,end=999)(array(ii),ii=1,ivars)
         write(iout,'(4g20.13)') (array(ii),ii=1,ivars)
         isets=isets+1
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      goto 999
7     continue
      call journal('sc',' *b2a* error reading data line=',isets+1)
      goto 999
999   continue
      call journal('sc',' *b2a* number of sets of data written is ',isets)
      close(iin)
      close(iout)
      end program b2a
!-----------------------------------------------------------------------------------------------------------------------------------
