program test_isblank
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: isblank
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isblank',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      select case (i)
      case (9,32)
         if (isblank(char(i)) .eqv. .false.)then
            write(*,*)'isblank: failed on character ',i,isblank(char(i))
            call unit_check_bad('isblank')
            stop 1
         endif
      case default
         if (isblank(char(i)) .eqv. .true.)then
            write(*,*)'isblank: failed on character ',i,isblank(char(i))
            call unit_check_bad('isblank')
            stop 2
         endif
      end select
   enddo
   call unit_check_good('isblank')
end program test_isblank
