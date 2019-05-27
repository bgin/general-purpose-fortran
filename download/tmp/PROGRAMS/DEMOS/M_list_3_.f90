          program demo_M_list
          use M_list, only : insert, locate
          ! Find if a string is in a sorted array,
          ! and insert the string into the dictionary
          ! if it is not present ...

           use M_list, only : locate, insert
           implicit none
           character(len=20),allocatable :: keyword(:)
           character(len=20)             :: key
           character(len=100),allocatable :: value(:)
           character(len=100)             :: val
           integer                       :: i

           keyword=[character(len=20) ::] ! initialize dictionary
           value=[character(len=100)::]   ! initialize dictionary

           key='b';val='value of b'
           call update(key,val)
           key='a';val='value of a'
           call update(key,val)
           key='c';val='value of c'
           call update(key,val)
           key='c';val='value of c again'
           call update(key,val)
           key='d';val='value of d'
           call update(key,val)
           key='a';val='value of a again'
           call update(key,val)
           ! show array
           write(*,'(*(a,"==>",a,/))')(trim(keyword(i)),trim(value(i)),i=1,size(keyword))

           contains
           subroutine update(key,val)
           character(len=*),intent(in)  :: key
           character(len=*),intent(in)  :: val
           integer                      :: place

           ! find where string is or should be
           call locate(key,keyword,place)
           ! if string was not found insert it
           if(place.lt.1)then
              call insert(key,keyword,abs(place))
              call insert(val,value,abs(place))
           else
              value(place)=val
           endif

           end subroutine update
       end program demo_M_list
