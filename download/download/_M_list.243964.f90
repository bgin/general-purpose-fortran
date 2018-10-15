!===================================================================================================================================
program test_M_list  ! test noesc
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_list, only : locate, add, delete, insert, remove
character(len=*),parameter ::  share=' -library libGPF -filename `pwd`/M_list.FF -documentation y -ufpp y -ccall n -archive GPF.a'
character(len=20),allocatable :: dict(:)

   ! Use goodbad(1) to indicate the test sequence was begun for each procedure and give their descriptions

   call unit_check_start('list::locate',&
   & ' -description "locate string in allocatable string array sorted in descending order" '//share)
   call unit_check_start('list::insert',&
   & ' -description "insert value into allocatable array by index"  '//share)
   call unit_check_start('list::remove',&
   & ' -description "remove value from allocatable array by index"  '//share)
   call unit_check_start('list::add',   &
   & ' -description "add string into allocatable string array by name" '//share)
   call unit_check_start('list::delete',&
   & ' -description "delete string by name from allocatable string array" '//share)

   call add('A',dict)
   write(*,*)'ARRAY=',dict !  ARRAY=A
   write(*,*) all(dict.eq.[character(len=20) :: 'A'])
   call unit_check('M_list::add',all(dict.eq.[character(len=20) :: 'A']),'string adds (checkpoint 1)')

   call add('b',dict)
   call add('c',dict)
   call add('z',dict)
   write(*,*)'ARRAY=',dict ! ARRAY=z c b A
   write(*,*) all(dict.eq.[character(len=20) :: 'z','c','b','A'])
   call unit_check('M_list::add',all(dict.eq.[character(len=20) :: 'z','c','b','A']),'string adds (checkpoint 2)')

   call add('ZZ',dict)
   call add('not this one',dict)
   call add('ZZZ',dict)
   call add('Z',dict)
   write(*,*)'ARRAY=',dict ! ARRAY=z not this one c b ZZZ ZZ Z A
   call unit_check('M_list::add',all(dict.eq.[character(len=20) :: 'z','not this one','c','b','ZZZ','ZZ','Z','A']),'string adds ')

   call delete('not this one',dict)
   call delete('Z',dict)
   call delete('X',dict)
   write(*,*)'ARRAY=',dict ! ARRAY=z c b ZZZ ZZ A
   call unit_check('M_list::delete',all(dict.eq.[character(len=20) :: 'z','c','b','ZZZ','ZZ','A']),'string deletes ')

   call unit_check_good('M_list::add')     ! if got here everthing passed so put in good status code
   call unit_check_good('M_list::delete')
   call unit_check_good('M_list::locate')
   call unit_check_good('M_list::insert')
   call unit_check_good('M_list::remove')

end program test_M_list
!===================================================================================================================================
