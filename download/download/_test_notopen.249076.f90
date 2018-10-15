program test_notopen ! test the NOTOPEN(3f) function
   use M_io,    only: notopen
   use M_debug, only: unit_check, unit_check_start, unit_check_good

   call unit_check_start('notopen')
   write(*,*)'check for preassigned files from unit 0 to unit 1000'
   write(*,*)'(5 and 6 always return -1)'

   do i10=0,1000
      if(notopen(i10,i10,ierr) .ne. i10)then
         write(*,*)'INUSE:',i10,ierr, notopen(i10,i10,ierr2)
      endif
   enddo

   do i20=10,30,1
     open(unit=i20,status="scratch")
   enddo

   close(25)
   close(28)
   call unit_check('notopen', notopen(10,30)           .eq. 25 )
   call unit_check('notopen', notopen()                .eq. 25 )
   call unit_check('notopen', notopen(start=12,end=30) .eq. 25 )
   call unit_check('notopen', notopen(26)              .eq. 28 )
   call unit_check('notopen', notopen(26,99)           .eq. 28 )

   ! if got here without being stopped assume passed test
   call unit_check_good('notopen')

end program test_notopen
