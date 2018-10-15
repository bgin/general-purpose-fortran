!===================================================================================================================================
PROGRAM test_chomp
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only : chomp
character(len=:),allocatable  :: str
character(len=:),allocatable  :: token
character(len=66),allocatable :: delimiters
   call unit_check_start('chomp',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   str = 'a b ccc ddd x12#$)$*#@Z1!( ab cd ef'
   delimiters=' #@$)*!('
   ipass=0
   do while ( chomp(str,token,delimiters) .ge. 0 )
      ipass=ipass+1
      print *, ipass,'TOKEN=['//trim(token)//']'
      select case(ipass)
      case(1); call unit_check('chomp', token.eq.'a' )
      case(2); call unit_check('chomp', token.eq.'b' )
      case(3); call unit_check('chomp', token.eq.'ccc' )
      case(4); call unit_check('chomp', token.eq.'ddd' )
      case(5); call unit_check('chomp', token.eq.'x12' )
      case(6); call unit_check('chomp', token.eq.'Z1' )
      case(7); call unit_check('chomp', token.eq.'ab' )
      case(8); call unit_check('chomp', token.eq.'cd' )
      case(9); call unit_check('chomp', token.eq.'ef' )
      end select
   enddo

   call unit_check_good('chomp')
end program test_chomp
