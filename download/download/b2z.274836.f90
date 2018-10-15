program b2z
use M_journal, only : journal
use M_kracken, only : kracken, lget, sget
use M_debug,   only : debug
use M_zebra
implicit none

character(len=*),parameter :: ident="$@(#)b2z(1f): convert standard binary or standard ASCII file to a zebra file"

character(len=1024) :: filein      ! name of input file
character(len=1024) :: fileout     ! name of output file
character(len=20)   :: itype       ! type of input file
character(len=20)   :: otype       ! type of output file
integer             :: icount
integer             :: ios
integer             :: ierr
!=======================================================================--------====================================================
   call iftrail() ! if $USHTRAIL environment variable set, set trail file to it; else set trail to a scratch file
!=======================================================================--------====================================================
!  define the command options and default values and apply arguments from user command line
   call kracken("b2z", " -i -o -itype -otype -help F -version F -debug F")
   if(lget("b2z_version"))then              ! -version present?
      !!call journal(' b2z version 2.0-20110528')
      call journal(' b2z version 3.0-20180810')
      stop
   endif
   if(lget("b2z_help"))then                 ! -help present?
      call journal('================================================================================')
      call journal(' b2z -i INFILE -o OUTFILE -itype std|amap|ascii -otype bin|ascii [-help] [-version]  ')
      call journal('                                                                                ')
      call journal(' Convert a standard binary USH input file to a zebra format file.           ')
      call journal(' -itype std|amap|ascii                                                          ')
      call journal(' -otype bin|ascii                                                               ')
      call journal('================================================================================')
      stop
   endif
   debug=merge(.true.,.false.,lget('b2z_debug'))
   call journal(debug,'debug')
!=======================================================================--------====================================================
!  GET INPUT FILE NAME
   filein=sget("b2z_i")
   icount=0
   if(icount.gt.5)stop
   do while(len_trim(filein).eq.0)
      write(*,'(a)',advance='no')' Enter name of standard input USH file: '
      read(*,'(a)',iostat=ios)filein
      icount=icount+1
   enddo
!=======================================================================--------====================================================
!  INPUT FILE TYPE
   itype=sget("b2z_itype")
   icount=0
   do while(len_trim(itype).eq.0)
      write(*,'(a)',advance='no')' Enter type of input file (std,amap,ascii): '
      read(*,'(a)',iostat=ios)itype
      icount=icount+1
      if(icount.gt.5)stop
      CHECK_ITYPE: SELECT CASE ( itype )
       CASE ('std','amap','ascii')
       CASE DEFAULT
         call journal('*b2z* error -- unknown input file type (-itype) value')
         call journal(' std  -- standard USH binary file')
         call journal(' amap -- possibly incomplete USH binary file')
         call journal('ascii-- standard USH file in ASCII format')
      END SELECT CHECK_ITYPE
   enddo
!=======================================================================--------====================================================
!  OPEN OUTPUT FILE
   fileout=sget("b2z_o")
   icount=0
   do while(len_trim(fileout).eq.0)
      write(*,'(a)',advance='no')' Enter name of zebra file to create: '
      read(*,'(a)',iostat=ios)fileout
      icount=icount+1
      if(icount.gt.5)stop
   enddo
!=======================================================================--------====================================================
!  INPUT FILE TYPE
   otype=sget("b2z_otype")
   icount=0
   do while(len_trim(otype).eq.0)
      write(*,'(a)',advance='no')' Enter zebra file type (bin,ascii): '
      read(*,'(a)',iostat=ios)otype
      icount=icount+1
      if(icount.gt.5)stop
      CHECK_OTYPE: SELECT CASE ( otype )
       CASE ('bin','ascii')
       CASE DEFAULT
         call journal('*b2z* error -- unknown output file type (-otype) value')
         call journal(' bin  -- standard USH binary zebra file')
         call journal(' ascii-- standard USH ASCII zebra file')
         otype=''
      END SELECT CHECK_OTYPE
   enddo
!=======================================================================--------====================================================
   ierr=0
   if(debug)write(*,*)'DEBUG: *b2z* call bin2zebra'
   call bin2zebra(filein,itype,fileout,otype,ierr)
   if(ierr.ne.0)then
      stop 1
   endif
end program b2z
!=======================================================================--------====================================================
