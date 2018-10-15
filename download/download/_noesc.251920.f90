!===================================================================================================================================
program test_noesc  ! test noesc
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only : noesc
   character(len=23) :: in,out,clr
   integer :: i10
  ! Use goodbad(1) to indicate the test sequence was begun
   call unit_check_start('noesc',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   do i10=0,127
      write(in, '(i3.3,1x,4a)')i10,char(i10),char(i10),char(i10),' eol'
      write(clr,'(i3.3,1x,"    eol")')i10
      out=noesc(in)
      write(*,'(a)')trim(in)
      write(*,'(a)')trim(out)
      SELECT CASE (i10)
      CASE (:31,127)
        if(out.ne.clr)then
           write(*,*)'Error: noesc did not replace a string with blanks that it should have'
           call unit_check_bad('noesc')
        endif
      CASE DEFAULT
        if(in.ne.out)then
           write(*,*)'Error: noesc changed a string it should not have'
           call unit_check_bad('noesc')
        endif
      END SELECT
   enddo
   call unit_check_good('noesc')
end program test_noesc
!===================================================================================================================================
