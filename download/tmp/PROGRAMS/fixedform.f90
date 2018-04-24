!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
program fifo
   use M_kracken
   use M_fixedform
   character(len=:),allocatable :: filename
   character(len=:),allocatable :: answers
!  define command arguments, default values and crack command line
   call kracken('cmd','-f test.dat')
!  get commandline values. Allow filename as -oo or -f
   filename=trim(sget('cmd_oo'))
   if(filename.eq.' ')then
      filename=trim(sget('cmd_f'))
   endif
!  all done parsing; do something with the values
   page_ptr=>page_pd
   icount_ptr=>icount_pd
   call loaddata(filename)      ! fill the page(*) with user data
   do
      call fixedform(tabs=answers)
      write(*,*)answers
   enddo
end program fifo
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
