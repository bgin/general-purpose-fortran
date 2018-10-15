program test_describe
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: describe
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
! initialize database description of routine
   call unit_check_start('describe',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

! call all descriptions to exercise procedure
do i=0,number_of_chars-1
   write(*,*)i,char(i),' ',describe(char(i))
enddo

! unit tests
call unit_check('describe', describe(char( 23) ) .eq.  'ctrl-W (ETB) end of transmission block' , 'describe ctrl-W')
call unit_check('describe', describe(char( 33) ) .eq.  '! exclamation point'                    , 'describe exlamation point')
call unit_check('describe', describe(char( 52) ) .eq.  '4 four'                                 , 'describe four')
call unit_check('describe', describe(char( 63) ) .eq.  '? question mark'                        , 'describe question mark')
call unit_check('describe', describe(char( 64) ) .eq.  '@ at sign'                              , 'describe at sign')
call unit_check('describe', describe(char( 74) ) .eq.  'J majuscule J'                          , 'describe J')
call unit_check('describe', describe(char( 117)) .eq.  'u miniscule u'                          , 'describe u')
call unit_check('describe', describe(char( 126)) .eq.  '~ tilde'                                , 'describe tilde')
call unit_check_good('describe')

end program test_describe
