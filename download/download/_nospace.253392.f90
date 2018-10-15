!-----------------------------------------------------------------------------------------------------------------------------------
program test_nospace
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: nospace
implicit none
   character(len=:),allocatable :: string
   string='  This     is      a     test  '
   string=nospace(string)
   call unit_check_start('nospace',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   write(*,*)'nospace'
   if (string .ne. 'Thisisatest')then
      call unit_check_bad('nospace')
      stop 1
   endif
   call unit_check_good('nospace')
end program test_nospace
!-----------------------------------------------------------------------------------------------------------------------------------
