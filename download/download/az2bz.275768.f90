program az2bz
!  @(#) convert ASCII zebra to binary zebra file
use M_kracken, only: kracken, lget, sget
use M_debug, only: fstop
use M_zebra
character(len=1024) :: filein
character(len=1024) :: fileout
!=======================================================================--------====================================================
   call iftrail() ! if $USHTRAIL environment variable set, set trail file to it; else set trail to a scratch file
!  define the command options and default values and apply arguments from user command line
   call kracken("az2bz", " -i -o -itype -help F -version F")
!=======================================================================--------====================================================
   if(lget("az2bz_help"))then                 ! -help present?
      write(*,*)'================================================================================'
      write(*,*)' az2bz -i INFILE -o OUTFILE [-help] [-version]                                  '
      write(*,*)'                                                                                '
      write(*,*)' Convert an ASCII USH ZEBRA input file to a binary zebra file.                  '
      write(*,*)'================================================================================'
      stop
   endif
!=======================================================================--------====================================================
   if(lget("az2bz_version"))then              ! -version present?
      write(*,'(a)')' az2bz version 2.0-20110528'
      stop
   endif
!=======================================================================--------====================================================
!  GET INPUT FILE NAME
   filein=sget("az2bz_i")
   icount=0
   do while(len_trim(filein).eq.0)
      write(*,'(a)',advance='no')'Enter name of input binary zebra file: '
      read(*,'(a)',iostat=ios)filein
      icount=icount+1
      if(icount.gt.5)stop
   enddo
!=======================================================================--------====================================================
!  OPEN OUTPUT FILE
   fileout=sget("az2bz_o")
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
   call juaz2bz(filein(:len_trim(filein)),fileout(:len_trim(fileout)),ierr)
   call fstop(ierr)
   end program az2bz
!=======================================================================--------====================================================
