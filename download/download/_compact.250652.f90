!-----------------------------------------------------------------------------------------------------------------------------------
program test_compact
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: compact
implicit none
   call unit_check_start('compact',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   write(*,*)'compact'
   if (compact('  This  is     a    test  ') .ne. 'This is a test')then
      call unit_check_bad('compact')
      stop 1
   endif
   if (compact('This is a test') .ne. 'This is a test')then
      call unit_check_bad('compact')
      stop 2
   endif
   if (compact('This-is-a-test') .ne. 'This-is-a-test')then
      call unit_check_bad('compact')
      stop 3
   endif
   if (compact('  This  is     a    test  ',char='') .ne. 'Thisisatest')then
      call unit_check_bad('compact')
      stop 4
   endif
   if (compact('  This  is     a    test  ',char='t') .ne. 'Thististattest')then
      call unit_check_bad('compact')
      stop 5
   endif
   call unit_check_good('compact')
end program test_compact
!-----------------------------------------------------------------------------------------------------------------------------------
