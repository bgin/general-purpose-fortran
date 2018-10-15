program test_isalnum
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isalnum
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isalnum',' &
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
      CASE ('a':'z','A':'Z','0':'9')
         if (isalnum(char(i)) .eqv. .false.)then
            write(*,*)'isalnum: failed on character ',i,isalnum(char(i))
            call unit_check_bad('isalnum')
            stop 1
         endif
      CASE DEFAULT
         if (isalnum(char(i)) .eqv. .true.)then
            write(*,*)'isalnum: failed on character ',i,isalnum(char(i))
            call unit_check_bad('isalnum')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isalnum')
end program test_isalnum
