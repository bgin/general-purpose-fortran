program test_isxdigit
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isxdigit
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isxdigit',' &
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
      CASE ('a':'f','A':'F','0':'9')
         if (isxdigit(char(i)) .eqv. .false.)then
            write(*,*)'isxdigit: failed on character ',i,isxdigit(char(i))
            call unit_check_bad('isxdigit')
            stop 1
         endif
      CASE DEFAULT
         if (isxdigit(char(i)) .eqv. .true.)then
            write(*,*)'isxdigit: failed on character ',i,isxdigit(char(i))
            call unit_check_bad('isxdigit')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isxdigit')
end program test_isxdigit
