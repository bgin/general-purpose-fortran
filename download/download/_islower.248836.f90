program test_islower
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: islower
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('islower',' &
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
      CASE ('a':'z')
         if (islower(ch) .eqv. .false.)then
            write(*,*)'islower: failed on character ',i,islower(ch)
            call unit_check_bad('islower')
            stop 1
         endif
      CASE DEFAULT
         if (islower(ch) .eqv. .true.)then
            write(*,*)'islower: failed on character ',i,islower(ch)
            call unit_check_bad('islower')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('islower')
end program test_islower
