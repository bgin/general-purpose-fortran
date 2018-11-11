module M_list
use M_debug, only : debug
implicit none
private

public binary_search ! [M_list] binary search of a sorted array.

public locate        ! [M_list] find PLACE in sorted character array where VARNAME can be found or should be placed
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_r
   private insert_i
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_r
   private remove_i
public add           ! [M_list] insert entry by name into a sorted allocatable character array if it is not present
public delete        ! [M_list] delete entry by name from a sorted allocatable character array if it is present

character(len=*),parameter :: ident1=&
                 &"@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_r, insert_i
end interface

character(len=*),parameter :: ident2=&
                 &"@(#)M_list::insert(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_r, remove_i
end interface

contains
!>
!!##NAME
!!    binary_search(3f) - [M_list] binary search of a sorted integer array.
!!##SYNTAX
!!   pure function binary_search(id,arr) result(jloc)
!!
!!    integer,intent(in)              :: id
!!    integer,dimension(:),intent(in) :: arr
!!    integer                         :: jloc
!!
!!##DESCRIPTION
!!
!!    Binary search is a search algorithm that finds the position of a target
!!    value within an integer sorted array.
!!
!!##OPTIONS
!!
!!      ID    Keyword to match in ARR.
!!      ARR   Array to search.
!!      JLOC  If found, 'JLOC' is the matched position in 'ARR'.
!!            If not found, returns zero (0).
!===================================================================================================================================
pure function binary_search(id,arr) result(jloc)
implicit none

character(len=*),parameter::ident="@(#)M_list::binary_search(3f): binary search of a sorted integer array."

integer,intent(in)              :: id        !! key word to match in `arr`
integer,dimension(:),intent(in) :: arr       !! array to search (it is assumed to be sorted)
integer                         :: jloc      !! the first matched index in 'arr' (if not found, 0 is returned)

integer           :: j,k,khi,klo,n
integer,parameter :: iswtch = 16

   n = size(arr)
   jloc = 0

   if ( n<iswtch ) then                         ! sequential search more efficient
      do j = 1 , n
         if ( arr(j)==id ) then
            jloc = j
            return
         elseif (arr(j)>id) then
            return ! error
         endif
      enddo
      return ! error
   else
      klo = 1
      khi = n
      k = (klo+khi+1)/2
      do
         j = k
         if ( id<arr(j) ) then
            khi = k
         elseif ( id==arr(j) ) then
            jloc = j
            return
         else
            klo = k
         endif
         if ( khi-klo<1 ) then
            return ! error
         elseif ( khi-klo==1 ) then
            if ( k==klo ) then
               k = khi
            else
               k = klo
            endif
            klo = khi
         else
            k = (klo+khi+1)/2
         endif
      enddo
   endif

end function binary_search
!>
!!##NAME
!!    locate(3f) - [M_list] finds the index where a string is found or should be in a sorted array
!!
!!##SYNOPSIS
!!
!!   subroutine locate(varname,dictionary,place,ier,mssg)
!!
!!    character(len=*),intent=(in)          :: VARNAME
!!    character(len=*),allocatable          :: DICTIONARY(:)
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the string VARNAME is found or should
!!    be found in an array. The string array must be sorted in descending
!!    order (highest at top). If VARNAME is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!##OPTIONS
!!
!!
!!    VARNAME       the entry name to locate in the dictionary.
!!
!!    DICTIONARY    is the dictionary array.
!!
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  dictionary alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array,
!!    and insert the string into the dictionary
!!    if it is not present ...
!!
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate
!!     implicit none
!!     character(len=20),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=*),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(string,arr,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end.eq.0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus.eq.1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus.eq.end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update
!!     end program demo_locate
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!===================================================================================================================================
subroutine locate(varname,dictionary,place,ier,errmsg)

character(len=*),parameter::ident="&
&@(#)M_list::locate(3f): find PLACE in sorted character array where VARNAME can be found or should be placed"

!     Assuming an alphabetized array of character strings where it is
!     is assumed all variable names are lexically greater than a blank string.
!
!      1. If it is not found report where it should be placed as a NEGATIVE index number.
!

character(len=*),intent(in)             :: varname
integer,intent(out)                     :: place
character(len=*),allocatable            :: dictionary(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
   integer                              :: i
   character(len=:),allocatable         :: message
   integer                              :: arraysize
   integer                              :: maxtry
   integer                              :: imin, imax
   integer                              :: error
   if(.not.allocated(dictionary))then
      dictionary=[character(len=max(len(varname),4096)) :: ]
   endif
   arraysize=size(dictionary)
   if(debug)write(*,*)'START LOCATE place=',place,' ARRAYSIZE=',size(dictionary)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(varname.eq.dictionary(PLACE))then
         exit LOOP
      else if(varname.gt.dictionary(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of dictionary. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of dictionary. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(*,*)message//' VARNAME=',trim(varname)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(*,*)'END LOCATE place=',place,' ARRAYSIZE=',size(dictionary)
end subroutine locate
!>
!!##NAME
!!    remove(3f) - [M_list] remove entry from an allocatable array at specified position
!!
!!##SYNOPSIS
!!
!!   subroutine remove(dictionary,place)
!!
!!    character(len=*),intent(in)   :: DICTIONARY(:)
!!          or
!!    real,intent(in)               :: DICTIONARY(:)
!!          or
!!    integer,intent(in)            :: DICTIONARY(:)
!!
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!
!!##OPTIONS
!!
!!    DICTIONARY    is the dictionary array.
!!
!!    PLACE         is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!!    Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, remove
!!     implicit none
!!     character(len=20),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!===================================================================================================================================
subroutine remove_c(dictionary,place)

character(len=*),parameter::ident="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=*),allocatable :: dictionary(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(*,*) 'START REMOVE_C PLACE=',place,' ORIGINALSIZE=',size(dictionary)
   if(.not.allocated(dictionary))then
      dictionary=[character(len=4096) :: ]
   endif
   ii=len(dictionary)
   end=size(dictionary)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      dictionary=[character(len=ii) :: dictionary(:place-1) ]
   else
      dictionary=[character(len=ii) :: dictionary(:place-1), dictionary(place+1:) ]
   endif
   if(debug)write(*,*) 'END REMOVE_C PLACE=',place,' NEWSIZE=',size(dictionary)
end subroutine remove_c
subroutine remove_r(dictionary,place)

character(len=*),parameter::ident="@(#)M_list::remove_r(3fp): remove value from allocatable array at specified position"

real,allocatable    :: dictionary(:)
integer,intent(in)  :: place
integer             :: end
   if(debug) write(*,*) 'START REMOVE_R PLACE=',place,' ORIGINALSIZE=',size(dictionary)
   if(.not.allocated(dictionary))then
      dictionary=[real :: ]
   endif
   end=size(dictionary)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      dictionary=[ dictionary(:place-1)]
   else
      dictionary=[ dictionary(:place-1), dictionary(place+1:) ]
   endif
   if(debug)write(*,*) 'END REMOVE_R PLACE=',place,' NEWSIZE=',size(dictionary)

end subroutine remove_r
subroutine remove_i(dictionary,place)

character(len=*),parameter::ident="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: dictionary(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(*,*) 'START REMOVE_I PLACE=',place,' ORIGINALSIZE=',size(dictionary)
   if(.not.allocated(dictionary))then
      dictionary=[integer :: ]
   endif
   end=size(dictionary)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      dictionary=[ dictionary(:place-1)]
   else
      dictionary=[ dictionary(:place-1), dictionary(place+1:) ]
   endif
   if(debug)write(*,*) 'END REMOVE_I PLACE=',place,' NEWSIZE=',size(dictionary)

end subroutine remove_i
!>
!!##NAME
!!    insert(3f) - [M_list] insert entry into a string array at specified position
!!
!!##SYNOPSIS
!!
!!   subroutine insert(varname,dictionary,place)
!!
!!    character(len=*),intent=(in)  :: VARNAME
!!    character(len=*),intent(in)   :: DICTIONARY(:)
!!          or
!!    real,intent=(in)              :: VARNAME
!!    real,intent(in)               :: DICTIONARY(:)
!!          or
!!    integer,intent=(in)           :: VARNAME
!!    integer,intent(in)            :: DICTIONARY(:)
!!
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    Insert a string into an allocatable array at the specified index.
!!
!!##OPTIONS
!!
!!    VARNAME       the string value to place in the array
!!
!!    DICTIONARY    is the dictionary array.
!!
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array,
!!    and insert the string into the dictionary
!!    if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, insert
!!     implicit none
!!     character(len=20),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=*),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     write(*,'("ORIGINAL ARRAY WHERE SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     ! find where string is or should be
!!     call locate(string,arr,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        write(*,*)'NOT FOUND SO INSERT'
!!        call insert(string,arr,abs(place))
!!     endif
!!
!!     ! show array
!!     write(*,*)'ARRAY IS NOW'
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!    Expected output
!!
!!      ORIGINAL ARRAY WHERE SIZE=5 xxx,b,aaa,ZZZ,,
!!       for "b" index is            2           5
!!       ARRAY IS NOW
!!      SIZE=5 xxx,b,aaa,ZZZ,,
!!      ORIGINAL ARRAY WHERE SIZE=5 xxx,b,aaa,ZZZ,,
!!       for "[" index is           -4           5
!!       NOT FOUND SO INSERT
!!       ARRAY IS NOW
!!      SIZE=6 xxx,b,aaa,[,ZZZ,,
!!      ORIGINAL ARRAY WHERE SIZE=6 xxx,b,aaa,[,ZZZ,,
!!       for "c" index is           -2           6
!!       NOT FOUND SO INSERT
!!       ARRAY IS NOW
!!      SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!      ORIGINAL ARRAY WHERE SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!       for "ZZ" index is           -7           7
!!       NOT FOUND SO INSERT
!!       ARRAY IS NOW
!!      SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!      ORIGINAL ARRAY WHERE SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!       for "ZZZZ" index is           -6           8
!!       NOT FOUND SO INSERT
!!       ARRAY IS NOW
!!      SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!      ORIGINAL ARRAY WHERE SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!       for "z" index is           -1           9
!!       NOT FOUND SO INSERT
!!       ARRAY IS NOW
!!      SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!===================================================================================================================================
subroutine insert_c(varname,dictionary,place)

character(len=*),parameter::ident="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: varname
character(len=*),allocatable :: dictionary(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(*,*) 'START INSERT_C VARNAME=',varname,' PLACE=',place,' ORIGINALSIZE=',size(dictionary)
   if(.not.allocated(dictionary))then
      dictionary=[character(len=max(len(varname),4096)) :: ]
   endif
   ii=len(dictionary)
   end=size(dictionary)
   if(end.eq.0)then                                          ! empty array
      dictionary=[character(len=ii) :: varname ]
   elseif(place.eq.1)then                                    ! put in front of array
      dictionary=[character(len=ii) :: varname, dictionary]
   elseif(place.gt.end)then                                  ! put at end of array
      dictionary=[character(len=ii) :: dictionary, varname ]
   else                                                      ! put in middle of array
      dictionary=[character(len=ii) :: dictionary(:place-1), varname,dictionary(place:) ]
   endif
   if(debug)write(*,*) 'END INSERT_C VARNAME=',varname,' PLACE=',place,' NEWSIZE=',size(dictionary)
end subroutine insert_c
subroutine insert_r(varname,dictionary,place)

character(len=*),parameter::ident="@(#)M_list::insert_r(3fp): place value into allocatable array at specified position"

real,intent(in)       :: varname
real,allocatable      :: dictionary(:)
integer,intent(in)    :: place
integer               :: end
   if(debug) write(*,*) 'START INSERT_R VARNAME=',varname,' PLACE=',place,' ORIGINALSIZE=',size(dictionary)
   if(.not.allocated(dictionary))then
      dictionary=[real :: ]
   endif
   end=size(dictionary)
   if(end.eq.0)then                                          ! empty array
      dictionary=[varname]
   elseif(place.eq.1)then                                    ! put in front of array
      dictionary=[varname, dictionary]
   elseif(place.gt.end)then                                  ! put at end of array
      dictionary=[dictionary, varname ]
   else                                                      ! put in middle of array
      dictionary=[dictionary(:place-1), varname,dictionary(place:) ]
   endif
   if(debug)write(*,*) 'END INSERT_I VARNAME=',varname,' PLACE=',place,' NEWSIZE=',size(dictionary)
end subroutine insert_r
subroutine insert_i(varname,dictionary,place)

character(len=*),parameter::ident="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: varname
integer,allocatable   :: dictionary(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(dictionary))then
      dictionary=[integer :: ]
   endif
   end=size(dictionary)
   if(debug)write(*,*) 'START INSERT_I VARNAME=',varname,' PLACE=',place,' ORIGINALSIZE=',size(dictionary)
   if(end.eq.0)then                                          ! empty array
      dictionary=[varname]
   elseif(place.eq.1)then                                    ! put in front of array
      dictionary=[varname, dictionary]
   elseif(place.gt.end)then                                  ! put at end of array
      dictionary=[dictionary, varname ]
   else                                                      ! put in middle of array
      dictionary=[dictionary(:place-1), varname,dictionary(place:) ]
   endif
   if(debug)write(*,*) 'END INSERT_I VARNAME=',varname,' PLACE=',place,' NEWSIZE=',size(dictionary)
end subroutine insert_i
!>
!!##NAME
!!    delete(3f) - [M_list] delete entry by name from an allocatable sorted string array if it is present
!!
!!##SYNOPSIS
!!
!!   subroutine delete(varname,dictionary)
!!
!!    character(len=*),intent=(in) :: VARNAME
!!    character(len=*),allocatable :: DICTIONARY(:)
!!
!!##DESCRIPTION
!!
!!    Find if a string is in a sorted array, and delete the string
!!    from the dictionary if it is present.
!!
!!##OPTIONS
!!
!!    VARNAME       the string value to find and delete from the array.
!!
!!    DICTIONARY    is the dictionary array. It must be sorted in descending order.
!!
!!##EXAMPLES
!!
!!
!!    If string is found in a sorted array, delete the string
!!
!!     program demo_delete
!!     use M_sort, only : sort_shell
!!     use M_list, only : delete
!!     implicit none
!!     character(len=20),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: 'ZZZ','aaa','b','b','c','xxx','ZZ','z' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete('c',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete('^',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete(' ',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete('b',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete('ZZ',   arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete('ZZZZ', arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  delete('z',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     101 format (1x,*("[",a,"]",:,","))
!!     end program demo_delete
!!
!!    Results:
!===================================================================================================================================
subroutine delete(varname,dictionary)

character(len=*),parameter::ident="@(#)M_list::delete(3f): remove string from sorted allocatable string array if present"

character(len=*),intent(in)  :: varname
character(len=*),allocatable :: dictionary(:)
   integer                   :: place
   INFINITE: do
      call locate(varname,dictionary,place)
      if(place.ge.1)then
         call remove(dictionary,place)
      else
         exit INFINITE
      endif
   enddo INFINITE
end subroutine delete
!>
!!##NAME
!!    add(3f) - [M_list] insert entry into an allocatable sorted string array if it is not present
!!
!!##SYNOPSIS
!!
!!   subroutine add(varname,dictionary)
!!
!!    character(len=*),intent=(in) :: VARNAME
!!    character(len=*),allocatable :: DICTIONARY(:)
!!
!!##DESCRIPTION
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the dictionary if it is not present.
!!
!!##OPTIONS
!!
!!    VARNAME       the string value to place in the array.
!!
!!    DICTIONARY    is the dictionary array. It must be sorted in descending order.
!!
!!##EXAMPLES
!!
!!
!!    If string is not found in a sorted array, insert the string
!!
!!     program demo_add
!!     use M_sort, only : sort_shell
!!     use M_list, only : add
!!     implicit none
!!     character(len=20),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add('b',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add('^',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add(' ',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add('c',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add('ZZ',   arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add('ZZZZ', arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     call  add('z',    arr); write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     101 format (1x,*("[",a,"]",:,","))
!!     end program demo_add
!!
!!    Results:
!!
!!     [xxx],[b],[aaa],[ZZZ]
!!     [xxx],[b],[aaa],[ZZZ]
!!     [xxx],[b],[aaa],[^],[ZZZ]
!!     [xxx],[b],[aaa],[^],[ZZZ],[]
!!     [xxx],[c],[b],[aaa],[^],[ZZZ],[]
!!     [xxx],[c],[b],[aaa],[^],[ZZZ],[ZZ],[]
!!     [xxx],[c],[b],[aaa],[^],[ZZZZ],[ZZZ],[ZZ],[]
!!     [z],[xxx],[c],[b],[aaa],[^],[ZZZZ],[ZZZ],[ZZ],[]
!===================================================================================================================================
subroutine add(varname,dictionary)

character(len=*),parameter::ident="@(#)M_list::add(3f): place string into sorted allocatable string array if not present"

character(len=*),intent(in)  :: varname
character(len=*),allocatable :: dictionary(:)
   integer                   :: place
   call locate(varname,dictionary,place)
   if(place.lt.1)then
      call insert(varname,dictionary,abs(place))
   endif
end subroutine add
end module M_list
