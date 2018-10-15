program test
   call readit('ls -l')
   call writeit('cat -n')
end program test
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine readit(cmd)
   use M_process ,ONLY: process_open_read, process_readline, streampointer
   type(streampointer) :: fp                              ! C file pointer returned by process_open()
   character(len=*)    :: cmd                             ! command line executed to start process
   character(len=4096) :: line                            ! line of data to read (assumed long enough to hold any input line)
   integer ierr
   call process_open_read(cmd,fp,ierr)                    ! open process to read from
   write(*,*)'TEST: process is opened with status ',ierr
   ierr=0
   do while(ierr .eq. 0)
      call process_readline(line,fp,ierr)                 ! read a line from the process
      if(ierr.ne.0)then
         write(*,*)'TEST: ierr is ',ierr
         exit
      endif
      write(*,*)'TEST: line:'//trim(line)
   enddo
end subroutine readit
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine writeit(cmd)
use M_process ,ONLY: process_open_write, process_writeline, streampointer, process_close
type(streampointer) :: fp                 ! C file pointer returned by process_open()
character(len=*) :: cmd                   ! command line executed to start process
   character(len=4096) :: line            ! line of data to write (assumed long enough to hold any output line)
   integer ierr
   integer i
   call process_open_write(cmd,fp,ierr)   ! open process to write to
   write(*,*)'TEST: process is opened'
   ierr=0
   do i=1,10
      write(line,'("TEST: line ",i0)')i
      call process_writeline(line,fp,ierr)
      if(ierr.ne.0)then
         write(*,*)'TEST: process write error ',ierr
         exit
      endif
   enddo
   call process_close(fp,ierr)
   write(*,*)'TEST: process closed with status ',ierr
end subroutine writeit
