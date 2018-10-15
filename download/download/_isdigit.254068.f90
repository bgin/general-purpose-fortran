program test_isdigit
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isdigit
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isdigit',' &
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
      CASE (48:57)
         if (isdigit(char(i)) .eqv. .false.)then
            write(*,*)'isdigit: failed on character ',i,isdigit(char(i))
            call unit_check_bad('isdigit')
            stop 1
         endif
      CASE DEFAULT
         if (isdigit(char(i)) .eqv. .true.)then
            write(*,*)'isdigit: failed on character ',i,isdigit(char(i))
            call unit_check_bad('isdigit')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isdigit')
end program test_isdigit
