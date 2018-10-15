program test_matchw
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only : matchw
!implicit none
integer np, ns
parameter (np =  19, ns =  6)
character pattern(np)*8, string(ns)*12
character pattern2(np)*8
integer s, p
data pattern /'*','a*a','a*','ab*','*a','a*a','a?d?','a?d*','abra','aa','a','ab','*','?','????','?*','*?','***?','****?'/
data pattern2/'*','a**a','a*d?','ab*','*a','a*a','a?d?','a?d*','alda','aa','a','ab','*','?','???a','????','**','***a','?????'/
data string / 'abracadabra', 'aldabra', 'alda', 'carta', 'abdc', 'abra'/

   write(*,'(t18, *(a6))',advance="no") pattern
   write(*,'("TABLE 1")')
   do s = 1,ns
      write(*, '(a, 100L6)') string(s),(matchw(string(s),pattern(p)), p=1,np)
   enddo

   write(*,'(t18, *(a6))',advance="no") pattern2
   write(*,'("TABLE 2")')
   do s = 1,ns
      write(*, '(a, 100L6)') string(s),(matchw(string(s),pattern2(p)), p=1,np)
   enddo

   stop

   do s = 1,ns
      do p=1,np
         write(*, '(a,a,L7)') string(s),pattern2(p),matchw(string(s),pattern2(p))
      enddo
   enddo

   end program test_matchw
