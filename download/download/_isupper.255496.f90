program test_isupper
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isupper
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isupper',' &
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
      CASE ('A':'Z')
         if (isupper(ch) .eqv. .false.)then
            write(*,*)'isupper: failed on character ',i,isupper(ch)
            call unit_check_bad('isupper')
            stop 1
         endif
      CASE DEFAULT
         if (isupper(ch) .eqv. .true.)then
            write(*,*)'isupper: failed on character ',i,isupper(ch)
            call unit_check_bad('isupper')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isupper')
end program test_isupper
