           program demo_insert
           use M_sort, only : sort_shell
           use M_list, only : locate, insert
           implicit none
           character(len=20),allocatable :: arr(:)
           integer                       :: i

           arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
           ! make sure sorted in descending order
           call sort_shell(arr,order='d')

           call update(arr,'b')
           call update(arr,'[')
           call update(arr,'c')
           call update(arr,'ZZ')
           call update(arr,'ZZZZ')
           call update(arr,'z')

           contains
           subroutine update(arr,string)
           character(len=*),allocatable :: arr(:)
           character(len=*)             :: string
           integer                      :: place, end

           end=size(arr)
           write(*,'("ORIGINAL ARRAY WHERE SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)

           ! find where string is or should be
           call locate(string,arr,place)
           write(*,*)'for "'//string//'" index is ',place, size(arr)
           ! if string was not found insert it
           if(place.lt.1)then
              write(*,*)'NOT FOUND SO INSERT'
              call insert(string,arr,abs(place))
           endif

           ! show array
           write(*,*)'ARRAY IS NOW'
           end=size(arr)
           write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)

           end subroutine update
           end program demo_insert
