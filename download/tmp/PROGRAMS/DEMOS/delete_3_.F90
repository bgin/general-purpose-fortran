           program demo_delete
           use M_sort, only : sort_shell
           use M_list, only : delete
           implicit none
           character(len=20),allocatable :: arr(:)
           integer                       :: i

           arr=[character(len=20) :: 'ZZZ','aaa','b','b','c','xxx','ZZ','z' ]
           ! make sure sorted in descending order
           call sort_shell(arr,order='d')

           write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete('c',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete('^',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete(' ',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete('b',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete('ZZ',   arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete('ZZZZ', arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           call  delete('z',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
           101 format (1x,*("[",a,"]",:,","))
           end program demo_delete
