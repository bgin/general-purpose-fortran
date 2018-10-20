           program demo_add
           use M_sort, only : sort_shell
           use M_list, only : add
           implicit none
           character(len=20),allocatable :: arr(:)
           integer                       :: i

           arr=[character(len=20) :: 'ZZZ', 'aaa', 'b', 'xxx' ]
           ! make sure sorted in descending order
           call sort_shell(arr,order='d')

           write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add('b',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add('^',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add(' ',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add('c',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add('ZZ',   arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add('ZZZZ', arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  add('z',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           101 format (1x,*("[",a,"]",:,","))
           end program demo_add
