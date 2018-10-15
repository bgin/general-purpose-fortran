program bz2az
!  @(#) convert binary zebra to ASCII zebra file
use M_journal, only : journal
use M_kracken, only: kracken, lget, sget
use M_debug, only: fstop
use M_zebra
character(len=1024) :: filein
character(len=1024) :: fileout
!=======================================================================--------====================================================
   call iftrail() ! if $USHTRAIL environment variable set, set trail file to it; else set trail to a scratch file
!  define the command options and default values and apply arguments from user command line
   call kracken("bz2az", " -i -o -itype -help F -version F")
!=======================================================================--------====================================================
   if(lget("bz2az_help"))then                 ! -help present?
      call journal('================================================================================')
      call journal(' bz2az -i INFILE -o OUTFILE [-help] [-version]                                  ')
      call journal('                                                                                ')
      call journal(' Convert a binary standard USH input file to an ASCII ZEBRA output file.        ')
      call journal('================================================================================')
      stop
   endif
!=======================================================================--------====================================================
   if(lget("bz2az_version"))then              ! -version present?
      call journal(' bz2az version 2.0-20110528')
      stop
   endif
!=======================================================================--------====================================================
!  GET INPUT FILE NAME
   filein=sget("bz2az_i")
   icount=0
   do while(len_trim(filein).eq.0)
      write(*,'(a)',advance='no')'Enter name of input binary zebra file: '
      read(*,'(a)',iostat=ios)filein
      icount=icount+1
      if(icount.gt.5)stop
   enddo
!=======================================================================--------====================================================
!  OPEN OUTPUT FILE
   fileout=sget("bz2az_o")
   icount=0
   do while(len_trim(fileout).eq.0)
      write(*,'(a)',advance='no')'Enter name of ASCII zebra file to create: '
      read(*,'(a)',iostat=ios)fileout
      lennum=len_trim(fileout)
      if(lennum.lt.1)then
         call get_environment_variable('USHZEBRA',fileout)
         il2=len_trim(fileout)
         if(il2.eq.0)then
            fileout='CONVERT.zebra'
         else
            fileout=fileout(:il2)//'CONVERT.zebra'
         endif
      endif
      icount=icount+1
      if(icount.gt.5)stop
   enddo
!=======================================================================--------====================================================
   call jubz2az(filein(:len_trim(filein)),fileout(:len_trim(fileout)),ierr)
   call fstop(ierr)
   end program bz2az
!=======================================================================--------====================================================
