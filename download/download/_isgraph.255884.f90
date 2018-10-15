program test_isgraph
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isgraph
implicit none
integer :: i
   call unit_check_start('isgraph',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   do i=1,255
      SELECT CASE (i)
      CASE (33:126)
         if (isgraph(char(i)) .eqv. .false.)then
            write(*,*)'   ',i,isgraph(char(i))
            call unit_check_bad('isgraph')
            stop 2
         endif
      CASE DEFAULT
         if (isgraph(char(i)) .eqv. .true.)then
            write(*,*)'   ',i,isgraph(char(i))
            call unit_check_bad('isgraph')
            stop 3
         endif
      END SELECT
   enddo
   call unit_check_good('isgraph')
end program test_isgraph
!-----------------------------------------------------------------------------------------------------------------------------------
