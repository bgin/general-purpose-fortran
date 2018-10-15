program test_strings
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: transliterate, reverse
use M_strings, only: lower,upper
use M_strings, only: switch
use M_strings, only: isgraph,isprint
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
character(len=1)            :: chars(36)
integer :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('transliterate',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
write(*,*)'Transliterate'
write(*,*)transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',lc,uc)
write(*,*)transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',uc,lc)
if(transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',lc,uc).eq.uc(1:26))then
   call unit_check_good('transliterate')
else
   write(*,*)'error: transliterate '
   write(*,*)'['//transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',lc,uc)//']'
   write(*,*)'['//uc//']'
   call unit_check_bad('transliterate')
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('reverse',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
write(*,*)'reverse'
write(*,*)reverse(lc)
write(*,*)reverse('Madam, I''m Adam')
if(reverse(lc).eq.'9876543210zyxwvutsrqponmlkjihgfedcba')then
   call unit_check_good('reverse')
else
   write(*,*)'error: reverse '
   write(*,*)'iN:  ['//lc//']'
   write(*,*)'OUT: ['//reverse(lc)//']'
   call unit_check_bad('reverse')
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('upper',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
if(upper(lc).eq.uc)then
   call unit_check_good('upper')
else
   call unit_check_bad('upper')
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('lower',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
if(lower(uc).eq.lc)then
   call unit_check_good('lower')
else
   call unit_check_bad('lower')
endif
!-----------------------------------------------------------------------------------------------------------------------------------
write(*,*)'switch:' ! switch: switch between single string and an array of single characters; generic name for {a2s,s2a}
write(*,*)'switch LC string to an array'
write(*,'(i0,1x,*(a,1x))') size(switch(lc)),switch(lc)
write(*,*)'switch UC string to an array'
write(*,'(i0,1x,*(a,1x))') size(switch(uc)),switch(uc)
write(*,*)'switch reversed UC string to an array'
write(*,'(i0,1x,*(a,1x))') size(switch(uc)),switch(reverse(uc))
write(*,'(i0,1x,*(a,1x))') size(switch(uc)),switch(switch(reverse(uc)))
   call unit_check_start('switch',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
if(size(switch(uc)).ne.36)then
   call unit_check_bad('switch')
endif
chars=switch(uc)
do i=1,size(chars)
   if(chars(i).ne.uc(i:i))then
      call unit_check_bad('switch')
   endif
enddo


!==================================================================================
!APPARENTLY, YOU CANNOT SUBSCRIPT A RETURNED ARRAY ??? IS THERE A SYNTAX FOR THIS?
!xx chars=switch(uc)(36:1:-1)  ! reverse string like reverse
!xx chars=[switch(uc](36:1:-1) ! reverse string like reverse

chars='X'
write(*,*)'put string UC into array CHARS'
chars=switch(uc)
write(*,*)'put CHARS array into CHARS array in reverse order like reverse'
chars=chars(36:1:-1)
write(*,*)'put CHARS array into string reversed and compare to original UC string'
if( uc .ne. switch(chars(36:1:-1)) )then
      write(*,*)reverse(uc)
      write(*,*)switch(chars(36:1:-1))
      call unit_check_bad('switch')
endif
call unit_check_good('switch')
!==================================================================================
! COMBINED TESTS
chars=switch(uc)     ! convert string to character array
chars=chars(36:1:-1) ! reverse order of characters
write(*,*)'character array to character string which is reversed and minisculed'
if(lower(reverse(switch(chars))).eq.lc)then
   write(*,*)'WORKS'
else
   write(*,*)lower(reverse(switch(chars)))
   write(*,*)lc
   write(*,*)'DOES NOT WORK'
   stop 4
endif
!-----------------------------------------------------------------------------------------------------------------------------------
write(*,*)'isprint'
write(*,*)'   letter a      ',isprint('a')
write(*,*)'   horizontal tab',isprint(char(9))
write(*,*)'   array of letters;.',isprint([';','.',' '])
write(*,*)'   array of letters',isprint(switch(uc))
write(*,*)'   array of letters',isprint(uc)
!-----------------------------------------------------------------------------------------------------------------------------------
write(*,*)'isgraph'
write(*,*)'   letter a      ',isgraph('a')
write(*,*)'   horizontal tab',isgraph(char(9))
write(*,*)'   array of letters;.',isgraph([';','.',' '])
write(*,*)'   array of letters',isgraph(switch(uc))
write(*,*)'   array of letters',isgraph(uc)
!-----------------------------------------------------------------------------------------------------------------------------------
!==================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
end program test_strings
