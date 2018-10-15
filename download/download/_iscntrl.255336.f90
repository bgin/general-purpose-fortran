program test_iscntrl
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: iscntrl
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('iscntrl',' &
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
      CASE (0:31,127)
         if (iscntrl(char(i)) .eqv. .false.)then
            write(*,*)'iscntrl: failed on character ',i,iscntrl(char(i))
            call unit_check_bad('iscntrl')
            stop 1
         endif
      CASE DEFAULT
         if (iscntrl(char(i)) .eqv. .true.)then
            write(*,*)'iscntrl: failed on character ',i,iscntrl(char(i))
            call unit_check_bad('iscntrl')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('iscntrl')
end program test_iscntrl
