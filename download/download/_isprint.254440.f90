!-----------------------------------------------------------------------------------------------------------------------------------
program test_isprint
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isprint
implicit none
integer :: i
   call unit_check_start('isprint',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   write(*,*)'isprint'
   do i=1,255
      SELECT CASE (i)
      CASE (32:126)
         if (isprint(char(i)) .eqv. .false.)then
            write(*,*)'   ',i,isprint(char(i))
            call unit_check_bad('isprint')
            stop 2
         endif
      CASE DEFAULT
         if (isprint(char(i)) .eqv. .true.)then
            write(*,*)'   ',i,isprint(char(i))
            call unit_check_bad('isprint')
            stop 3
         endif
      END SELECT
   enddo
call unit_check_good('isprint')
end program test_isprint
!-----------------------------------------------------------------------------------------------------------------------------------
