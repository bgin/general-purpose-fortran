program test_isspace
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isspace
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isspace',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0,9:13,32)
         if (isspace(char(i)) .eqv. .false.)then
            write(*,*)'isspace: failed on character ',i,isspace(char(i))
            call unit_check_bad('isspace')
            stop 1
         endif
      CASE DEFAULT
         if (isspace(char(i)) .eqv. .true.)then
            write(*,*)'isspace: failed on character ',i,isspace(char(i))
            call unit_check_bad('isspace')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isspace')
end program test_isspace
