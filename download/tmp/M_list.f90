!>
!!##NAME
!!    M_list(3f) - [M_list] maintain simple lists
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!    The M_list(3fm) module allows for maintaining an array as a sorted
!!    list. An example is given that creates a keyword-value dictionary
!!    using the lists.
!!
!!    The lists are maintained as simple allocatable arrays. Each time an
!!    entry is added or deleted the array is re-allocated. Because of the
!!    expense of reallocating the data these routines are best suited for
!!    maintaining small lists that do not change size frequently.
!!
!!    BASIC LIST
!!    subroutine locate(list,value,place,ier,errmsg)  finds the index where a
!!                                                    value is found or should
!!                                                    be in a sorted array and
!!                                                    flag if the value exists
!!                                                    already
!!    subroutine insert(list,value,place)     insert entry into a array at
!!                                            specified position
!!    subroutine replace(list,value,place)    replace entry in an array at
!!                                            specified position
!!    subroutine remove(list,place)           remove entry from an allocatable
!!                                            array at specified position
!!
!!    BASIC DICTIONARY
!!
!!    Due to bugs in gfortran up to at least 7.4.0, this next section
!!    does not work.
!!
!!    type dictionary
!!
!!       character(len=:),allocatable :: key(:)
!!       character(len=:),allocatable :: value(:)
!!       integer,allocatable          :: count(:)
!!
!!    %get      get value from type(dictionary) given an existing key
!!    %set      set or replace value for type(dictionary) given a key
!!    %del      delete an existing key from type(dictionary)
!!    %clr      empty a type(dictionary)
!!
!!    MISCELLANEOUS
!!    binary_search  binary search of a sorted integer array.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_list
!!    use M_list, only : insert, locate, replace, remove
!!    ! create a dictionary with character keywords, values, and value lengths
!!    ! using the routines for maintaining a list
!!
!!     use M_list, only : locate, insert, replace
!!     implicit none
!!     character(len=:),allocatable   :: keywords(:)
!!     character(len=:),allocatable   :: values(:)
!!     integer,allocatable            :: counts(:)
!!     integer                        :: i
!!     ! insert and replace entries
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords)
!!     ! remote some entries
!!     call update('a')
!!     call update('c')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords)
!!     ! get some values
!!     write(*,*)'get b=>',get('b')
!!     write(*,*)'get d=>',get('d')
!!     write(*,*)'get notthere=>',get('notthere')
!!
!!     contains
!!     subroutine update(key,valin)
!!     character(len=*),intent(in)           :: key
!!     character(len=*),intent(in),optional  :: valin
!!     integer                               :: place
!!     integer                               :: ilen
!!     character(len=:),allocatable          :: val
!!     if(present(valin))then
!!        val=valin
!!        ilen=len_trim(val)
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        ! if string was not found insert it
!!        if(place.lt.1)then
!!           call insert(keywords,key,iabs(place))
!!           call insert(values,val,iabs(place))
!!           call insert(counts,ilen,iabs(place))
!!        else
!!           call replace(values,val,place)
!!           call replace(counts,ilen,place)
!!        endif
!!     else
!!        call locate(keywords,key,place)
!!        if(place.gt.0)then
!!           call remove(keywords,place)
!!           call remove(values,place)
!!           call remove(counts,place)
!!        endif
!!     endif
!!     end subroutine update
!!     function get(key) result(valout)
!!     character(len=*),intent(in)   :: key
!!     character(len=:),allocatable  :: valout
!!     integer                       :: place
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        if(place.lt.1)then
!!           valout=''
!!        else
!!           valout=values(place)(:counts(place))
!!        endif
!!     end function get
!!    end program demo_M_list
!!
!!   Results:
!!
!!    d==>[value of d]
!!    c==>[value of c again]
!!    b==>[value of b]
!!    a==>[value of a again]
!!
!!    d==>[value of d]
!!    b==>[value of b]
!!
!!     get b=>value of b
!!     get d=>value of d
!!     get notthere=>
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_list
use M_debug, only : debug
implicit none
private

public binary_search ! [M_list] binary search of a sorted array.

public locate        ! [M_list] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
   private locate_r
   private locate_i
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_r
   private insert_i
public replace       ! [M_list] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_r
   private replace_i
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_r
   private remove_i

character(len=*),parameter::ident_1="&
&@(#)M_list::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d, locate_r, locate_i
end interface

character(len=*),parameter::ident_2="&
&@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_r, insert_i
end interface

character(len=*),parameter::ident_3="&
&@(#)M_list::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_r, replace_i
end interface

character(len=*),parameter::ident_4="&
&@(#)M_list::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_r, remove_i
end interface

public test_suite_m_list
!-----------------------------------------------------------------------------------------------------------------------------------
public dictionary

type dictionary
   character(len=:),allocatable :: key(:)
   character(len=:),allocatable :: value(:)
   integer,allocatable          :: count(:)
   contains
      procedure,private :: get => dict_get
      procedure,private :: set => dict_add    ! insert entry by name into a sorted allocatable character array if it is not present
      procedure,private :: del => dict_delete ! delete entry by name from a sorted allocatable character array if it is present
end type dictionary
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
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
!!    Binary search is a search algorithm that finds the position of a targe
!!    value within an integer sorted array.
!!
!!##OPTIONS
!!      ID    Keyword to match in ARR.
!!      ARR   Array to search.
!!
!!##RESULT
!!      JLOC  If found, 'JLOC' is the matched position in 'ARR'.
!!            If not found, returns zero (0).
!===================================================================================================================================
pure function binary_search(id,arr) result(jloc)
implicit none

character(len=*),parameter::ident_5="@(#)M_list::binary_search(3f): binary search of a sorted integer array."

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
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_list] finds the index where a string is found or should be in a sorted arra
!!
!!##SYNOPSIS
!!
!!   subroutine locate(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    list          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
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
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
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
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(arr,string,place)
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
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!===================================================================================================================================
subroutine locate_c(list,value,place,ier,errmsg)

character(len=*),parameter::ident_6="&
&@(#)M_list::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

! Assuming an alphabetized array of character strings where it is
! assumed all variable names are lexically greater than a blank string.
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.
!

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
   integer                              :: i
   character(len=:),allocatable         :: message
   integer                              :: arraysize
   integer                              :: maxtry
   integer                              :: imin, imax
   integer                              :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)
   if(debug)write(*,*)'START LOCATE_C ARRAYSIZE=',size(list)

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

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(*,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(*,*)'END LOCATE_C PLACE=',place,' ARRAYSIZE=',size(list),' LENGTH=',len(list)
end subroutine locate_c
subroutine locate_d(list,value,place,ier,errmsg)

character(len=*),parameter::ident_7="&
&@(#)M_list::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

doubleprecision,allocatable            :: list(:)
doubleprecision,intent(in)             :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   arraysize=size(list)
   if(debug)write(*,*)'START LOCATE_D ARRAYSIZE=',size(list)

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

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(*,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(*,*)'END LOCATE_D PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_d
subroutine locate_r(list,value,place,ier,errmsg)

character(len=*),parameter::ident_8="&
&@(#)M_list::locate_r(3f): find PLACE in sorted real array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

real,allocatable                       :: list(:)
real,intent(in)                        :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[real :: ]
   endif
   arraysize=size(list)
   if(debug)write(*,*)'START LOCATE_R ARRAYSIZE=',size(list)

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

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(*,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(*,*)'END LOCATE_R PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_r
subroutine locate_i(list,value,place,ier,errmsg)

character(len=*),parameter::ident_9="&
&@(#)M_list::locate_i(3f): find PLACE in sorted integer array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

integer,allocatable                    :: list(:)
integer,intent(in)                     :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   arraysize=size(list)
   if(debug)write(*,*)'START LOCATE_I ARRAYSIZE=',size(list)

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

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(*,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(*,*)'END LOCATE_I PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove(3f) - [M_list] remove entry from an allocatable array at specified positio
!!
!!##SYNOPSIS
!!
!!   subroutine remove(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
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
!!     character(len=:),allocatable :: arr(:)
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
!!   Results:
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
subroutine remove_c(list,place)

character(len=*),parameter::ident_10="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(*,*)'START REMOVE_C PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
   if(debug)write(*,*)'END REMOVE_C PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine remove_c
subroutine remove_d(list,place)

character(len=*),parameter::ident_11="&
&@(#)M_list::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(*,*)'START REMOVE_D PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(*,*)'END REMOVE_D PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_d
subroutine remove_r(list,place)

character(len=*),parameter::ident_12="@(#)M_list::remove_r(3fp): remove value from allocatable array at specified position"

real,allocatable    :: list(:)
integer,intent(in)  :: place
integer             :: end
   if(debug) write(*,*)'START REMOVE_R PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(*,*)'END REMOVE_R PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_r
subroutine remove_i(list,place)

character(len=*),parameter::ident_13="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(*,*)'START REMOVE_I PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(*,*)'END REMOVE_I PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_list] replace entry in a string array at specified position
!!
!!##SYNOPSIS
!!
!!   subroutine replace(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!   Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_list, only  : insert, locate, replace
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords)
!!
!!     call locate(keywords,'a',place)
!!     if(place.gt.0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate(keywords,key,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(keywords,key,abs(place))
!!        call insert(values,val,abs(place))
!!     else ! replace
!!        call replace(values,val,place)
!!     endif
!!
!!     end subroutine update
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!===================================================================================================================================
subroutine replace_c(list,value,place)
use M_time, only : now

character(len=*),parameter::ident_14="@(#)M_list::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(debug) write(*,*)'START REPLACE_C VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(*,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
   if(debug)write(*,*)'END REPLACE_C VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine replace_c
subroutine replace_d(list,value,place)

character(len=*),parameter::ident_15="&
&@(#)M_list::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(*,*)'START REPLACE_D VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(*,*)'*replace_d* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(*,*)'END REPLACE_I VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_d
subroutine replace_r(list,value,place)

character(len=*),parameter::ident_16="@(#)M_list::replace_r(3fp): place value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(debug) write(*,*)'START REPLACE_R VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(*,*)'*replace_r* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(*,*)'END REPLACE_I VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_r
subroutine replace_i(list,value,place)

character(len=*),parameter::ident_17="@(#)M_list::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(*,*)'START REPLACE_I VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(*,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(*,*)'END REPLACE_I VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_list] insert entry into a string array at specified positio
!!
!!##SYNOPSIS
!!
!!   subroutine insert(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array,
!!    and insert the string into the list
!!    if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, insert
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!===================================================================================================================================
subroutine insert_c(list,value,place)
use M_time, only : now

character(len=*),parameter::ident_18="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end
   if(debug) write(*,*)'START INSERT_C VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(*,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(*,*)'END INSERT_C VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_c
subroutine insert_r(list,value,place)

character(len=*),parameter::ident_19="@(#)M_list::insert_r(3fp): place real value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end

   if(debug) write(*,*)'START INSERT_R VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif

   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(*,*)'*insert_r* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(*,*)'END INSERT_R VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_r
subroutine insert_d(list,value,place)

character(len=*),parameter::ident_20="&
&@(#)M_list::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(debug) write(*,*)'START INSERT_D VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(*,*)'*insert_d* error: index out of range. end=',end,' index=',place,' value=',value
   endif
   if(debug)write(*,*)'END INSERT_D VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_d
subroutine insert_i(list,value,place)

character(len=*),parameter::ident_21="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(*,*)'START INSERT_I VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(*,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(*,*)'END INSERT_I VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_delete(3f) - [M_list] delete entry by name from an allocatable sorted string array if it is presen
!!
!!##SYNOPSIS
!!
!!   subroutine dict_delete(key,dict)
!!
!!    character(len=*),intent(in) :: key
!!    type(dictionary)            :: dict
!!
!!##DESCRIPTION
!!
!!    Find if a string is in a sorted array, and delete the string
!!    from the dictionary if it is present.
!!
!!##OPTIONS
!!
!!    KEY           the key name to find and delete from the dictionary.
!!    DICTIONARY    the dictionary.
!!
!!##EXAMPLES
!!
!!
!!    delete a key from a dictionary by name.
!!
!!     program demo_dict_delete
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: caps
!!     integer                       :: i
!!     call caps%set(caps,'A','aye')
!!     call caps%set(caps,'B','bee')
!!     call caps%set(caps,'C','see')
!!     call caps%set(caps,'D','dee')
!!
!!     write(*,101)(trim(arr(i)),i=1,size(caps%keys)) ! show array
!!     call  caps%del(caps,'A')
!!     call  caps%del(caps,'C')
!!     call  caps%del(caps,'z')
!!     write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     101 format (1x,*("[",a,"]",:,","))
!!     end program demo_dict_delete
!!
!!    Results:
!!
!!     [z],[xxx],[c],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [xxx],[aaa],[ZZZ]
!===================================================================================================================================
subroutine dict_delete(self,key)

character(len=*),parameter::ident_22="@(#)M_list::dict_delete(3f): remove string from sorted allocatable string array if present"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
integer                         :: place

   call locate(self%key,key,place)
   if(place.ge.1)then
      call remove(self%key,place)
      call remove(self%value,place)
      call remove(self%count,place)
   endif

end subroutine dict_delete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_get(3f) - [M_list] get value of key-value pair in a dictionary given ke
!!
!!##SYNOPSIS
!!
!!   subroutine dict_get(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!
!!    get a value given a key from a key-value dictionary
!!
!!    If key is not found in dictionary , return a blank
!!
!!##OPTIONS
!!
!!    DICT     is the dictionary.
!!    KEY      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!
!!     program demo_locate
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary)             :: table
!!     character(len=:),allocatable :: val
!!     integer          :: i
!!
!!     call table%set('A','value for A')
!!     call table%set('B','value for B')
!!     call table%set('C','value for C')
!!     call table%set('D','value for D')
!!     call table%set('E','value for E')
!!     call table%set('F','value for F')
!!     call table%set('G','value for G')
!!     write(*,*)table%get('A')
!!     write(*,*)table%get('B')
!!     write(*,*)table%get('C')
!!     write(*,*)table%get('D')
!!     write(*,*)table%get('E')
!!     write(*,*)table%get('F')
!!     write(*,*)table%get('G')
!!     write(*,*)table%get('H')
!!     end program demo_locate
!!
!!    Results:
!===================================================================================================================================
function dict_get(self,key) result(value)

character(len=*),parameter::ident_23="@(#)M_list::dict_get(3f): get value of key-value pair in dictionary, given key"

!!class(dictionary),intent(inout) :: self
class(dictionary)               :: self
character(len=*),intent(in)     :: key
character(len=:),allocatable    :: value
integer                         :: place
   call locate(self%key,key,place)
   if(place.lt.1)then
      value=''
   else
      value=self%value(place)(:self%count(place))
   endif
end function dict_get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_add(3f) - [M_list] add or replace a key-value pair in a dictionary
!!
!!##SYNOPSIS
!!
!!   subroutine dict_add(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!    Add or replace a key-value pair in a dictionary.
!!
!!##OPTIONS
!!    DICT     is the dictionary.
!!    key      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!    If string is not found in a sorted array, insert the string
!!
!!     program demo_add
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: dict
!!     integer          :: i
!!
!!     call dict%set('A','b')
!!     call dict%set('B','^')
!!     call dict%set('C',' ')
!!     call dict%set('D','c')
!!     call dict%set('E','ZZ')
!!     call dict%set('F','ZZZZ')
!!     call dict%set('G','z')
!!     call dict%set('A','new value for A')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(dict%key(i)),dict%value(i)(:dict%count(i)),i=1,size(dict%key)
!!     end program demo_add
!!
!!    Results:
!===================================================================================================================================
subroutine dict_add(self,key,value)

character(len=*),parameter::ident_24="@(#)M_list::dict_add(3f): place key-value pair into dictionary, adding the key if required"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
character(len=*),intent(in)     :: value
integer                         :: place
integer                         :: place2
   call locate(self%key,key,place)
   if(place.lt.1)then
      place2=iabs(place)
      call insert( self%key,   key,             place2 )
      call insert( self%value, value,           place2 )
      call insert( self%count, len_trim(value), place2 )
   elseif(place.gt.0)then  ! replace instead of insert
      call insert( self%value, value,           place )
      call insert( self%count, len_trim(value), place )
   endif
end subroutine dict_add
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_m_list
use M_sort, only : sort_shell
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done, unit_check_level, msg
!!use M_list, only : locate, insert, remove, replace, dictionary
character(len=*),parameter   :: share=' -library libGPF -filename `pwd`/M_list.FF -documentation y -ufpp y -ccall n -archive GPF.a'
integer                      :: place
   ! list
   call test_locate()
   call test_insert()
   call test_remove()
   call test_replace()
   ! dictionary
   call test_dict_set()
   call test_dict_delete()
   call test_dict_get()
   !
   call test_binary_search()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_set
type(dictionary) :: dict
   call unit_check_start('dict%set',   &
   & ' -description "add string into allocatable string array by name" '//share)
   call  dict%set('A','value for a')

   !!call unit_check('dict%set',all(dict.eq.[character(len=20) :: 'A']),msg='array should be A')

   call dict%set('b','value for b')
   call dict%set('c','value for c')
   call dict%set('z','value for z')
   !!call unit_check('dict%set',all(dict.eq.[character(len=20) :: 'z','c','b','A']),'array should be z c b A')

   call dict%set('ZZ','value for ZZ')
   call dict%set('NOT','not this one')
   call dict%set('ZZZ','value for ZZZ')
   call dict%set('Z','value for Z')
   !!call unit_check('dict%set',all(dict.eq.[character(len=20) :: 'z','not this one','c','b','ZZZ','ZZ','Z','A']),'string adds ')

   call unit_check_done('dict%set')
end subroutine test_dict_set
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_delete
type(dictionary) :: dict
   call unit_check_start('dict_delete',&
   & ' -description "delete string by name from allocatable string array" '//share)

   call dict%del('A')
   call dict%del('Z')
   call dict%del('X')
   !!call unit_check('dict_delete',all(dict.eq.[character(len=20) :: 'z','c','b','ZZZ','ZZ','A']),'string deletes ')

   call unit_check_done('dict_delete')
end subroutine test_dict_delete
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_get
type(dictionary)             :: dict
character(len=:),allocatable :: val
   call unit_check_start('dict_get',&
   & ' -description "locate and get value from key-value pair by key name from dictionary" '//share)
   val=dict%get('A')
   val=dict%get('Z')
   val=dict%get('X')
   !!call unit_check('dict%get',all(dict.eq.[character(len=20) :: 'z','c','b','ZZZ','ZZ','A']),'string deletes ')

   call unit_check_done('dict_get')
end subroutine test_dict_get
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_locate
character(len=:),allocatable :: lst(:)
   lst=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
   ! make sure sorted in descending order
   call sort_shell(lst,order='d')
   call unit_check_start('locate',&
   & ' -description "locate string in allocatable string array sorted in descending order" '//share)
   call locate(lst,'ZZZ',place)
   call unit_check('locate',place.eq.4,msg=msg('ZZZ',place,'should be ',4))
   call locate(lst,'zqj',place)
   call unit_check('locate',place.eq.-1,msg=msg('zqj',place,'should be ',-1))
   call unit_check_done('locate')
end subroutine test_locate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_insert
implicit none
character(len=:),allocatable :: arr(:)
integer                      :: place
character(len=:),allocatable :: newkey
integer                      :: i1,i2
   call unit_check_start('insert',&
   & ' -description "insert value into allocatable array by index"  '//share)
   ! make sure sorted in descending order
   arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
   call sort_shell(arr,order='d')
   newkey='NEW'
   i1=size(arr)
   call locate(arr,newkey,place)         ! find where string is or should be
   call unit_check('insert',place.lt.0,msg=msg('should not be located',place))
   if(place.lt.1)then                    ! if string was not found insert it
      call insert(arr,newkey,abs(place)) ! not found so insert
      call locate(arr,newkey,place)      ! find where string is or should be
      if(place.gt.0)then
         call unit_check('insert',arr(place).eq.'NEW',msg=msg(arr(place),'should be "NEW"'))
      else
         call unit_check('insert',.false.,msg=msg(arr(place),'should be positive for "NEW"'))
      endif
   else
      call unit_check('insert',place.le.0,msg=msg('found but should not have been',place))
   endif
   i2=size(arr)
   call unit_check('insert',i1+1.eq.i2,msg=msg('array now bigger',i1,'to',i2))
   call unit_check_done('insert')
end subroutine test_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_remove
use M_sort, only : sort_shell
character(len=:),allocatable  :: arr(:)
integer                       :: place
integer                       :: isize
   call unit_check_start('remove',' -description "remove value from allocatable array by index"  '//share)
   arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'x', 'ab', 'bb', 'xxx' ]
   call sort_shell(arr,order='d')     ! make sure sorted in descending order

   isize=size(arr)

   call locate(arr,'ab',place)        ! find where string is or should be
   call unit_check('remove',place.gt.0,msg=msg('found the element to remove',place))
   call remove(arr,place)
   call locate(arr,'ab',place)        ! find where string is or should be
   call unit_check('remove',place.lt.0,msg=msg('did not find the element to remove',place))

   call locate(arr,'bb',place)        ! find where string is or should be
   call remove(arr,place)
   call unit_check('remove',isize-2.eq.size(arr),msg=msg('shrunk by two'))

   call unit_check_done('remove')
end subroutine test_remove
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replace
use M_sort, only : sort_shell
character(len=:),allocatable  :: arr(:)
integer                       :: place1
integer                       :: isize
   call unit_check_start('replace',&
   & ' -description "replace value from allocatable array by index"  '//share)
   arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'x', 'ab', 'bb', 'xxx' ]
   call sort_shell(arr,order='d')     ! make sure sorted in descending order
   isize=size(arr)
   call locate(arr,'ab',place1) ! find where string is or should be
   call unit_check('replace',place1.gt.0,msg=msg('location=',place1))
   call replace(arr,'new value for ab',place1)
   call unit_check('replace',size(arr).eq.isize,msg=msg('no change in size',size(arr)))
   if(place1.gt.0.and.place1.le.isize)then
      call unit_check('replace',arr(place1).eq.'new value for ab',msg=msg(arr(place1)))
   else
      call unit_check('replace',.false.,msg=msg('bad location',place1))
   endif
   call unit_check_done('replace')
end subroutine test_replace
!===================================================================================================================================
subroutine test_binary_search
   call unit_check_start('binary_search',&
   & ' -description "binary search of a sorted array"  '//share)
   call unit_check_done('binary_search')
end subroutine test_binary_search
!===================================================================================================================================
end subroutine test_suite_m_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
