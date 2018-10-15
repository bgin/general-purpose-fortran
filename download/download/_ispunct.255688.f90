program test_ispunct
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: ispunct
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('ispunct',' &
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
      CASE (33:47, 58:64, 91:96, 123:126)
         if (ispunct(char(i)) .eqv. .false.)then
            write(*,*)'ispunct: failed on character ',i,ispunct(char(i))
            call unit_check_bad('ispunct')
            stop 1
         endif
      CASE DEFAULT
         if (ispunct(char(i)) .eqv. .true.)then
            write(*,*)'ispunct: failed on character ',i,ispunct(char(i))
            call unit_check_bad('ispunct')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('ispunct')
end program test_ispunct
