program test_isascii
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isascii
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isascii',' &
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
      CASE (0:127)
         if (isascii(char(i)) .eqv. .false.)then
            write(*,*)'isascii: failed on character ',i,isascii(char(i))
            call unit_check_bad('isascii')
            stop 1
         endif
      CASE DEFAULT
         if (isascii(char(i)) .eqv. .true.)then
            write(*,*)'isascii: failed on character ',i,isascii(char(i))
            call unit_check_bad('isascii')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isascii')
end program test_isascii
