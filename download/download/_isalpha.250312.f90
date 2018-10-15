!-----------------------------------------------------------------------------------------------------------------------------------
program test_isalpha
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isalpha
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_check_start('isalpha',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z')
         if (isalpha(ch) .eqv. .false.)then
            write(*,*)'isalpha: failed on character ',i,isalpha(ch)
            call unit_check_bad('isalpha')
            stop 1
         endif
      CASE DEFAULT
         if (isalpha(ch) .eqv. .true.)then
            write(*,*)'isalpha: failed on character ',i,isalpha(ch)
            call unit_check_bad('isalpha')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isalpha')
end program test_isalpha
!-----------------------------------------------------------------------------------------------------------------------------------
