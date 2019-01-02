Module M_sort
implicit none
integer,parameter :: cd=kind(0.0d0)
private
public swap
public sort_shell
public sort_quick_rx
public unique
!===================================================================================================================================
! SORT_SHELL is a Generic Interface in a module with PRIVATE specific procedures. This means the individual subroutines
! cannot be called from outside of this module.

! the PRIVATE declaration requires use of a module.

!===================================================================================================================================
character(len=*),parameter::ident2="M_sort::sort_shell(3f): Generic subroutine sorts the array X using a shell sort"
interface sort_shell
   module procedure sort_shell_integers, sort_shell_reals, sort_shell_strings
   module procedure sort_shell_complex, sort_shell_doubles, sort_shell_complex_double
end interface
!===================================================================================================================================
character(len=*),parameter::ident3="M_sort::unique(3f): assuming an array is sorted, return array with duplicate values removed"
interface unique
   module procedure unique_integers, unique_reals, unique_strings
   module procedure unique_complex, unique_doubles, unique_complex_double
end interface
!===================================================================================================================================
character(len=*),parameter::ident1="M_sort::swap(3f): swap two variables of like type (real,integer,complex,character,double)"
interface swap
   module procedure r_swap, i_swap, c_swap, s_swap, d_swap, l_swap, cd_swap
end interface
!===================================================================================================================================

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    M_sort(3fm) - [M_sort] Fortran module containing sorting algorithms for arrays of standard scalar types
!!
!!##SYNOPSIS
!!
!!    use M_sort, only :: sort_shell, sort_quick_rx, unique
!!
!!##DESCRIPTION
!!    Under development. Currently only provides a few common routines, but it is intended that
!!    other procedures will provide a variety of sort methods, including ...
!!
!!
!!    Exchange sorts      Bubble sort, Cocktail shaker sort, Odd   even sort, Comb sort, Gnome sort, Quicksort, Stooge sort, Bogosort
!!    Selection sorts     Selection sort, Heapsort, Smoothsort, Cartesian tree sort, Tournament sort, Cycle sort
!!    Insertion sorts     Insertion sort, Shellsort, Splaysort, Tree sort, Library sort, Patience sorting
!!    Merge sorts         Merge sort, Cascade merge sort, Oscillating merge sort, Polyphase merge sort
!!    Distribution sorts  American flag sort, Bead sort, Bucket sort, Burstsort, Counting sort, Pigeonhole sort, Proxmap sort,
!!                        Radix sort, Flashsort
!!    Concurrent sorts    Bitonic sorter, Batcher odd   even mergesort, Pairwise sorting network
!!    Hybrid sorts        Block merge sortTimsort, Introsort, Spreadsort
!!    Other               Topological sorting,Pancake sorting, Spaghetti sort
!!
!!    and an overview of topics concerning sorting
!!
!!    Theory              Computational complexity theory, Big O notation, Total orderLists, InplacementStabilityComparison sort,
!!                        Adaptive sort, Sorting network, Integer sorting, X + Y sorting, Transdichotomous model, Quantum sort
!!
!!    In the mean time those keywords can be useful in locating materials on the WWW, especially in Wikipedia.
!!
!!##QUICKSORT
!!
!!    Quicksort, also known as partition-exchange sort, uses these steps
!!
!!     o Choose any element of the array to be the pivot.
!!     o Divide all other elements (except the pivot) into two partitions.
!!     o All elements less than the pivot must be in the first partition.
!!     o All elements greater than the pivot must be in the second partition.
!!     o Use recursion to sort both partitions.
!!     o Join the first sorted partition, the pivot, and the second sorted partition.
!!
!!    The best pivot creates partitions of equal length (or lengths differing
!!    by 1).
!!
!!    The worst pivot creates an empty partition (for example, if the pivot
!!    is the first or last element of a sorted array).
!!
!!    The run-time of Quicksort ranges from    O(n log n)    with the best
!!    pivots, to    O(n2)    with the worst pivots, where    n    is the
!!    number of elements in the array.
!!
!!    Quicksort has a reputation as the fastest sort. Optimized variants of
!!    quicksort are common features of many languages and libraries.
!===================================================================================================================================
!>
!!##NAME
!!    sort_shell(3f) - [M_sort] Generic subroutine sorts the array X using Shell's method
!!##SYNOPSIS
!!
!!    Usage:
!!
!!    for real, integer data:
!!       call sort_shell(X,ORDER='A|D')
!!
!!     X          input/output numeric array
!!     order      Ascending (a-z) or Descending (z-a) sort order
!!
!!     for complex, complex(kind=kind(0.0d0)) data:
!!        call sort_shell(X,order='A|D',type='R|I|S')
!!
!!     X          input/output complex array
!!     order      Ascending (a-z) or Descending (z-a) sort order
!!     type       Sort by Real component, Imaginary component, or Sqrt(R**2+I**2)
!!
!!     for character data:
!!        call sort_shell(X,order='A|D',[startcol=NN,endcol=MM])
!!
!!     X          input/output character array
!!     order      Ascending (a-z) or Descending (z-a) sort order
!!     startcol   character position in strings which starts search field
!!     endcol     character position in strings which ends search field
!!
!!##DESCRIPTION
!!
!!       subroutine sort_shell(3f) sorts an array over a specified field in numeric or alphanumeric order.
!!
!!       From Wikipedia, the free encyclopedia:
!!
!!       The step-by-step process of replacing pairs of items during the shell
!!       sorting algorithm. Shellsort, also known as Shell sort or Shell's
!!       method, is an in-place comparison sort. It can be seen as either a
!!       generalization of sorting by exchange (bubble sort) or sorting by
!!       insertion (insertion sort).[3] The method starts by sorting pairs of
!!       elements far apart from each other, then progressively reducing the gap
!!       between elements to be compared. Starting with far apart elements, it
!!       can move some out-of-place elements into position faster than a simple
!!       nearest neighbor exchange. Donald Shell published the first version
!!       of this sort in 1959.[4][5] The running time of Shellsort is heavily
!!       dependent on the gap sequence it uses. For many practical variants,
!!       determining their time complexity remains an open problem.
!!
!!       Shellsort is a generalization of insertion sort that allows the
!!       exchange of items that are far apart. The idea is to arrange the list
!!       of elements so that, starting anywhere, considering every hth element
!!       gives a sorted list. Such a list is said to be h-sorted. Equivalently,
!!       it can be thought of as h interleaved lists, each individually sorted.[6]
!!       Beginning with large values of h, this rearrangement allows elements
!!       to move long distances in the original list, reducing large amounts
!!       of disorder quickly, and leaving less work for smaller h-sort steps to
!!       do. If the file is then k-sorted for some smaller integer k, then the
!!       file remains h-sorted. Following this idea for a decreasing sequence of
!!       h values ending in 1 is guaranteed to leave a sorted list in the end.
!!
!!     F90 NOTES:
!!
!!      o  procedure names are declared private in this module so they are not accessible except by their generic name
!!      o  procedures must include a "use M_sort" to access the generic name SORT_SHELL
!!      o  if these routines are recompiled, routines with the use statement should then be recompiled and reloaded.
!!
!!##OPTIONS
!!
!!     X      is a vector or integer, real, complex, doubleprecision, character,
!!            or doubleprecision complex values to be sorted
!!
!!     order  sort order
!!            o A for ascending
!!            o D for descending (default)
!!
!!     type       Sort by Real component, Imaginary component, or Sqrt(R**2+I**2)
!!                Only applies to complex types.
!!
!!     startcol   character position in strings which starts sort field.
!!                Only applies to character values. Defaults to 1. Optional.
!!     endcol     character position in strings which ends sort field
!!                Only applies to character values. Defaults to end of string.
!!                Optional.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_sort_shell
!!       use M_sort, only : sort_shell
!!       character(len=:),allocatable :: array(:)
!!
!!       array= [ character(len=20) ::                               &
!!       & 'red',    'green', 'blue', 'yellow', 'orange',   'black', &
!!       & 'white',  'brown', 'gray', 'cyan',   'magenta',           &
!!       & 'purple']
!!
!!       write(*,'(a,*(a:,","))')'BEFORE ',(trim(array(i)),i=1,size(array))
!!       call sort_shell(array,order='a')
!!       write(*,'(a,*(a:,","))')'A-Z    ',(trim(array(i)),i=1,size(array))
!!       do i=1,size(array)-1
!!          if(array(i).gt.array(i+1))then
!!             write(*,*)'Error in sorting strings a-z'
!!          endif
!!       enddo
!!
!!       array= [ character(len=20) ::                               &
!!       & 'RED',    'GREEN', 'BLUE', 'YELLOW', 'ORANGE',   'BLACK', &
!!       & 'WHITE',  'BROWN', 'GRAY', 'CYAN',   'MAGENTA',           &
!!       & 'PURPLE']
!!
!!       write(*,'(a,*(a:,","))')'BEFORE ',(trim(array(i)),i=1,size(array))
!!       call sort_shell(array,order='d')
!!       write(*,'(a,*(a:,","))')'Z-A    ',(trim(array(i)),i=1,size(array))
!!       do i=1,size(array)-1
!!          if(array(i).lt.array(i+1))then
!!             write(*,*)'Error in sorting strings z-a'
!!          endif
!!       enddo
!!
!!       end program demo_sort_shell
!!
!!    Expected output
!!
!!       BEFORE red,green,blue,yellow,orange,black,white,brown,gray,cyan,magenta,purple
!!       A-Z    black,blue,brown,cyan,gray,green,magenta,orange,purple,red,white,yellow
!!       BEFORE RED,GREEN,BLUE,YELLOW,ORANGE,BLACK,WHITE,BROWN,GRAY,CYAN,MAGENTA,PURPLE
!!       Z-A    YELLOW,WHITE,RED,PURPLE,ORANGE,MAGENTA,GREEN,GRAY,CYAN,BROWN,BLUE,BLACK
!!
!!##REFERENCE
!!       1.  ALGORITHM 201, SHELLSORT, J. BOOTHROYD, CACM VOL. 6, NO. 8, P 445, (1963)
!!       2.  D. L. SHELL, CACM, VOL. 2, P. 30, (1959)
!!
!!##AUTHOR
!!      John S. Urban, 19970201
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_strings(lines,order,startcol,endcol)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_strings(3fp):sort strings over specified field using shell sort"

character(len=*),  intent(inout)          :: lines(:)       ! input/output array
character(len=*),  intent(in)             :: order          ! sort order 'ascending'|'descending'
integer,           optional,intent(in)    :: startcol,  endcol  ! beginning and ending column to sort by
   integer                                :: imax, is, ie

   imax=len(lines(1))                                       ! maximum length of the character variables being sorted
   if(imax.eq.0)return

   if(present(startcol))then                                  ! if the optional parameter is present, use it
     is=min(max(1,startcol),imax)
   else
     is=1
   endif

   if(present(endcol))then                                    ! if the optional parameter is present, use it
     ie=min(max(1,endcol),imax)
   else
     ie=imax
   endif

   if(order(1:1).eq.'a' .or. order(1:1).eq.'A') then
      call sort_shell_strings_lh(lines,is,ie)               ! sort a-z
   else
      call sort_shell_strings_hl(lines,is,ie)               ! sort z-a
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_strings_lh(lines,startcol,endcol)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_strings_lh(3fp):sort strings(a-z) over specified field using shell sort"

!  1989 John S. Urban
!  lle to sort 'a-z', lge to sort 'z-a'
!  should carefully check for bad input values,
!  return flag indicating whether any strings were equal,
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*) :: lines(:)
integer,intent(in),optional     :: startcol, endcol
integer                         :: startcol_local, endcol_local
   character(len=:),allocatable :: ihold
   integer           :: n
   integer           :: igap
   integer           :: i,j,k
   integer           :: jg
!-----------------------------------------------------------------------------------------------------------------------------------
   n=size(lines)
   startcol_local=merge(startcol,1,present(startcol))
   endcol_local=merge(endcol,size(lines),present(endcol))
   if(n.gt.0)then
      allocate(character(len=len(lines(1))) :: ihold)
   else
      ihold=''
   endif
   igap=n
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(lle(lines(j)(startcol_local:endcol_local),lines(jg)(startcol_local:endcol_local)))exit INSIDE
            ihold=lines(j)
            lines(j)=lines(jg)
            lines(jg)=ihold
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_strings_lh
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_strings_hl(lines,startcol,endcol)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_strings_hl(3fp):sort strings(z-a) over specified field using shell sort"

!  1989 John S. Urban
!  lle to sort 'a-z', lge to sort 'z-a'
!  should carefully check for bad input values,
!  return flag indicating whether any strings were equal,
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*) :: lines(:)
integer,intent(in),optional     :: startcol, endcol
integer                         :: startcol_local, endcol_local
   character(len=:),allocatable :: ihold
   integer           :: n
   integer           :: igap
   integer           :: i,j,k
   integer           :: jg
!-----------------------------------------------------------------------------------------------------------------------------------
   n=size(lines)
   startcol_local=merge(startcol,1,present(startcol))
   endcol_local=merge(endcol,size(lines),present(endcol))
   if(n.gt.0)then
      allocate(character(len=len(lines(1))) :: ihold)
   else
      ihold=''
   endif
   igap=n
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(lge(lines(j)(startcol_local:endcol_local),lines(jg)(startcol_local:endcol_local)))exit INSIDE
            ihold=lines(j)
            lines(j)=lines(jg)
            lines(jg)=ihold
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_strings_hl
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine sort_shell_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_integers(iarray,order)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_integers(3fp):sort integer array using Shell sort and specified order"

integer,intent(inout)          :: iarray(:)   ! iarray input/output array
character(len=*),  intent(in)  ::  order      ! sort order 'ascending'|'descending'

   if(order(1:1).eq.'a' .or. order(1:1).eq.'A') then
      call sort_shell_integers_lh(iarray)
   else
      call sort_shell_integers_hl(iarray)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_integers_hl(iarray)
! Copyright (C) 1989,1996 John S. Urban;  all rights reserved

character(len=*),parameter::ident="@(#)M_sort::sort_shell_integers_hl(3fp):sort integer array using Shell sort (high to low)"

integer,intent(inout)      :: iarray(:)  ! input/output array
integer                    :: n          ! number of elements in input array (iarray)
integer                    :: igap, i, j, k, jg
   n=size(iarray)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(iarray(j).ge.iarray(jg)) exit INSIDE
            call swap(iarray(j),iarray(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_integers_hl
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_integers_lh(iarray) ! sort an integer array in ascending order (low to high)
! Copyright (C) 1989,1996 John S. Urban;  all rights reserved

character(len=*),parameter::ident="@(#)M_sort::sort_shell_integers_lh(3fp):sort integer array using Shell sort low to high"

integer,intent(inout) :: iarray(:)      ! iarray input/output array
   integer            :: n
   integer            :: igap, i, j, k, jg

   n=size(iarray)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(iarray(j).le.iarray(jg))exit INSIDE
            call swap(iarray(j),iarray(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_integers_lh
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine sort_shell_integers
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_reals(array,order)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals(3fp):sort real array using Shell sort and specified order"

real,intent(inout)          :: array(:)   ! input/output array
character(len=*),intent(in) :: order      ! sort order 'ascending'|'descending'

   if(order(1:1).eq.'a' .or. order(1:1).eq.'A') then
      call sort_shell_reals_lh(array)
   else
      call sort_shell_reals_hl(array)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_reals_hl(array)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals_hl(3fp):sort real array using Shell sort (high to low)"

!  Copyright(C) 1989 John S. Urban
real,intent(inout) :: array(:) ! input array
   integer         :: n        ! number of elements in input array (array)
   integer         :: i, j, k, igap, jg
   n=size(array)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(array(j).ge.array(jg))exit INSIDE
            call swap(array(j),array(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_reals_hl
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_reals_lh(array)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals_lh(3fp):sort real array using Shell sort (low to high)"

!  Copyright(C) 1989 John S. Urban
real,intent(inout) :: array(:)            ! input array
integer         :: n                   ! number of elements in input array (array)
integer         :: i, j, k, igap, jg
   n=size(array)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(array(j).le.array(jg))exit INSIDE
            call swap(array(j),array(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_reals_lh
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine sort_shell_reals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_doubles(array,order)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_doubles(3fp):sort double array using Shell sort and specified order"

doubleprecision,intent(inout)          :: array(:)   ! input/output array
character(len=*),intent(in) :: order      ! sort order 'ascending'|'descending'

   if(order(1:1).eq.'a' .or. order(1:1).eq.'A') then
      call sort_shell_doubles_lh(array)
   else
      call sort_shell_doubles_hl(array)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_doubles_hl(array)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_doubles_hl(3fp):sort double array using Shell sort (high to low)"

!  Copyright(C) 1989 John S. Urban
doubleprecision,intent(inout) :: array(:) ! input array
integer         :: n        ! number of elements in input array (array)
integer         :: i, j, k, igap, jg
   n=size(array)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(array(j).ge.array(jg))exit INSIDE
            call swap(array(j),array(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_doubles_hl
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_doubles_lh(array)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_doubles_lh(3fp):sort double array using Shell sort (low to high)"

!  Copyright(C) 1989 John S. Urban
doubleprecision,intent(inout) :: array(:)            ! input array
   integer         :: n                   ! number of elements in input array (array)
   integer         :: i, j, k, igap, jg
   n=size(array)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(array(j).le.array(jg))exit INSIDE
            call swap(array(j),array(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_doubles_lh
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine sort_shell_doubles
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_complex(array,order,type)  ! select ascending or descending order

character(len=*),parameter::ident="@(#)M_sort::sort_shell_complex(3fp):sort complex array using Shell sort"

complex,intent(inout)         :: array(:)   ! array  input/output array
character(len=*),  intent(in) :: order      ! sort order 'ascending'|'descending'
character(len=*),  intent(in) :: type       ! sort by real part, imaginary part, or sqrt(R**2+I**2) ('R','I','S')

if(order(1:1).eq.'a' .or. order(1:1).eq.'A') then
   call sort_shell_complex_lh(array,type)
else
   call sort_shell_complex_hl(array,type)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_complex_hl(array,type)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals_hl(3fp):sort complex array using Shell sort (high to low)"

!     Copyright(C) 1989 John S. Urban   all rights reserved
   complex,intent(inout)       :: array(:)            ! input array
   character(len=*),intent(in) :: type
   integer                     :: n                   ! number of elements in input array (array)
   integer                     :: igap, k, i, j, jg
   doubleprecision             :: csize1, csize2
   n=size(array)
   igap=n
   if(len(type).le.0)return
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            select case(type(1:1))
            case('r','R')
               if(real(array(j)).ge.real(array(jg)))exit INSIDE
            case('i','I')
               if(aimag(array(j)).ge.aimag(array(jg)))exit INSIDE
            case default
               csize1=sqrt(dble(array(j))**2+dble(array(j))**2)
               csize2=sqrt(dble(array(jg))**2+dble(array(jg))**2)
               if(csize1.ge.csize2)exit INSIDE
            end select
            call swap(array(j),array(jg))
            j=j-igap
         if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
      if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_complex_hl
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_complex_lh(array,type)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals_lh(3fp):sort complex array using Shell sort (low to high)"

!  Copyright(C) 1989 John S. Urban   all rights reserved
!  array    input array
!  n        number of elements in input array (array)
   complex,intent(inout)         :: array(:)
   character(len=*),  intent(in) :: type       ! sort by real part, imaginary part, or sqrt(R**2+I**2) ('R','I','S')
   integer                       :: n
   integer                       :: igap, k, i, j, jg
   doubleprecision               :: csize1, csize2
   n=size(array)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            select case(type(1:1))
            case('r','R')
               if(real(array(j)).le.real(array(jg)))exit INSIDE
            case('i','I')
               if(aimag(array(j)).le.aimag(array(jg)))exit INSIDE
            case default
               csize1=sqrt(dble(array(j))**2+dble(array(j))**2)
               csize2=sqrt(dble(array(jg))**2+dble(array(jg))**2)
               if(csize1.le.csize2)exit INSIDE
            end select
            call swap(array(j),array(jg))
            j=j-igap
         if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_complex_lh
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine sort_shell_complex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_complex_double(array,order,type)  ! select ascending or descending order

character(len=*),parameter::ident="@(#)M_sort::sort_shell_complex_double(3fp):sort double complex array using Shell sort"


complex(kind=cd),intent(inout)         :: array(:)   ! array  input/output array
character(len=*),  intent(in) :: order      ! sort order 'ascending'|'descending'
character(len=*),  intent(in) :: type       ! sort by real part, imaginary part, or sqrt(R**2+I**2) ('R','I','S')

if(order(1:1).eq.'a' .or. order(1:1).eq.'A') then
   call sort_shell_complex_double_lh(array,type)
else
   call sort_shell_complex_double_hl(array,type)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_complex_double_hl(array,type)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals_hl(3fp):sort double complex array using Shell sort (high to low)"

!     Copyright(C) 1989 John S. Urban   all rights reserved
   complex(kind=cd),intent(inout)       :: array(:)            ! input array
   character(len=*),intent(in) :: type
   integer                     :: n                   ! number of elements in input array (array)
   integer                     :: igap, k, i, j, jg
   doubleprecision             :: cdsize1, cdsize2
   n=size(array)
   igap=n
   if(len(type).le.0)return
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            select case(type(1:1))
            case('r','R')
               if(dble(array(j)).ge.dble(array(jg)))exit INSIDE
            case('i','I')
               if(aimag(array(j)).ge.aimag(array(jg)))exit INSIDE
            case default
               cdsize1=sqrt(dble(array(j))**2+dble(array(j))**2)
               cdsize2=sqrt(dble(array(jg))**2+dble(array(jg))**2)
               if(cdsize1.ge.cdsize2)exit INSIDE
            end select
            call swap(array(j),array(jg))
            j=j-igap
         if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
      if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_complex_double_hl
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine sort_shell_complex_double_lh(array,type)

character(len=*),parameter::ident="@(#)M_sort::sort_shell_reals_lh(3fp):sort double complex array using Shell sort (low to high)"

!  Copyright(C) 1989 John S. Urban   all rights reserved
!  array    input array
!  n        number of elements in input array (array)
   complex(kind=cd),intent(inout)         :: array(:)
   character(len=*),  intent(in) :: type       ! sort by real part, imaginary part, or sqrt(R**2+I**2) ('R','I','S')
   integer                       :: n
   integer                       :: igap, k, i, j, jg
   doubleprecision               :: cdsize1, cdsize2
   n=size(array)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            select case(type(1:1))
            case('r','R')
               if(dble(array(j)).le.dble(array(jg)))exit INSIDE
            case('i','I')
               if(aimag(array(j)).le.aimag(array(jg)))exit INSIDE
            case default
               cdsize1=sqrt(dble(array(j))**2+dble(array(j))**2)
               cdsize2=sqrt(dble(array(jg))**2+dble(array(jg))**2)
               if(cdsize1.le.cdsize2)exit INSIDE
            end select
            call swap(array(j),array(jg))
            j=j-igap
         if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_complex_double_lh
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine sort_shell_complex_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      sort_quick_rx(3f) - [M_sort] indexed hybrid quicksort of a real array
!!##SYNOPSIS
!!
!!      subroutine sort_quick_rx(data,index)
!!
!!       real,intent(in)         :: data(:)
!!       integer,intent(out)     :: indx(size(data))
!!
!!##DESCRIPTION
!!
!!    From Leonard J. Moss of SLAC:
!!
!!    Here's a hybrid QuickSort I wrote a number of years ago. It's
!!    based on suggestions in Knuth, Volume 3, and performs much better
!!    than a pure QuickSort on short or partially ordered input arrays.
!!
!!    This routine performs an in-memory sort of the first N elements of
!!    array DATA, returning into array INDEX the indices of elements of
!!    DATA arranged in ascending order. Thus,
!!
!!       DATA(INDX(1)) will be the smallest number in array DATA;
!!       DATA(INDX(N)) will be the largest number in DATA.
!!
!!    The original data is not physically rearranged. The original order
!!    of equal input values is not necessarily preserved.
!!
!!    sort_quick_rx(3f) uses a hybrid QuickSort algorithm, based on several
!!    suggestions in Knuth, Volume 3, Section 5.2.2. In particular, the
!!    "pivot key" [my term] for dividing each subsequence is chosen to be
!!    the median of the first, last, and middle values of the subsequence;
!!    and the QuickSort is cut off when a subsequence has 9 or fewer
!!    elements, and a straight insertion sort of the entire array is done
!!    at the end. The result is comparable to a pure insertion sort for
!!    very short arrays, and very fast for very large arrays (of order 12
!!    micro-sec/element on the 3081K for arrays of 10K elements). It is
!!    also not subject to the poor performance of the pure QuickSort on
!!    partially ordered data.
!!
!!    o Created: sortrx(3f): 15 Jul 1986, Len Moss
!!    o saved from url=(0044)http://www.fortran.com/fortran/quick_sort2.f
!!    o changed to update syntax from F77 style; John S. Urban 20161021
!!##EXAMPLE
!!
!!  Sample usage:
!!
!!    program demo_sort_quick_rx
!!    use M_sort, only : sort_quick_rx
!!    implicit none
!!    integer,parameter            :: isz=10000000
!!    real                         :: rr(isz)
!!    integer                      :: ii(isz)
!!    integer                      :: i
!!    write(*,*)'initializing array with ',isz,' random numbers'
!!    CALL RANDOM_NUMBER(RR)
!!    rr=rr*450000.0
!!    write(*,*)'sort real array with sort_quick_rx(3f)'
!!    call sort_quick_rx(rr,ii)
!!    write(*,*)'checking index of sort_quick_rx(3f)'
!!    do i=1,isz-1
!!       if(rr(ii(i)).gt.rr(ii(i+1)))then
!!          write(*,*)'Error in sorting reals small to large ',i,rr(ii(i)),rr(ii(i+1))
!!       endif
!!    enddo
!!    write(*,*)'test of sort_quick_rx(3f) complete'
!!    end program demo_sort_quick_rx
!===================================================================================================================================
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine sort_quick_rx(data,indx)

character(len=*),parameter::ident="@(#)M_sort::sort_quick_rx(3f): indexed hybrid quicksort of a real array"

real,intent(in)         :: data(:)
integer,intent(out)     :: indx(:)

integer  :: n
integer  :: lstk(31),rstk(31),istk
integer  :: l,r,i,j,p,indexp,indext
real     :: datap

!  QuickSort Cutoff
!
!  Quit QuickSort-ing when a subsequence contains M or fewer elements and finish off at end with straight insertion sort.
!  According to Knuth, V.3, the optimum value of M is around 9.

integer,parameter :: M=9
!===================================================================================================================================
n=size(data)
if(size(indx).lt.n)then  ! if index is not big enough, only sort part of the data
  write(*,*)'*sort_quick_rx* ERROR: insufficient space to store index data'
  n=size(indx)
endif
!===================================================================================================================================
!  Make initial guess for INDEX

do i=1,n
   indx(i)=i
enddo

!  If array is short go directly to the straight insertion sort, else execute a QuickSort
if (N.gt.M)then
   !=============================================================================================================================
   !  QuickSort
   !
   !  The "Qn:"s correspond roughly to steps in Algorithm Q, Knuth, V.3, PP.116-117, modified to select the median
   !  of the first, last, and middle elements as the "pivot key" (in Knuth's notation, "K").  Also modified to leave
   !  data in place and produce an INDEX array.  To simplify comments, let DATA[I]=DATA(INDX(I)).

   ! Q1: Initialize
   istk=0
   l=1
   r=n
   !=============================================================================================================================
   TOP: do

      ! Q2: Sort the subsequence DATA[L]..DATA[R].
      !
      !  At this point, DATA[l] <= DATA[m] <= DATA[r] for all l < L, r > R, and L <= m <= R.
      !  (First time through, there is no DATA for l < L or r > R.)

      i=l
      j=r

      ! Q2.5: Select pivot key
      !
      !  Let the pivot, P, be the midpoint of this subsequence, P=(L+R)/2; then rearrange INDX(L), INDX(P), and INDX(R)
      !  so the corresponding DATA values are in increasing order.  The pivot key, DATAP, is then DATA[P].

      p=(l+r)/2
      indexp=indx(p)
      datap=data(indexp)

      if (data(indx(l)) .gt. datap) then
         indx(p)=indx(l)
         indx(l)=indexp
         indexp=indx(p)
         datap=data(indexp)
      endif

      if (datap .gt. data(indx(r))) then

         if (data(indx(l)) .gt. data(indx(r))) then
            indx(p)=indx(l)
            indx(l)=indx(r)
         else
            indx(p)=indx(r)
         endif

         indx(r)=indexp
         indexp=indx(p)
         datap=data(indexp)
      endif

      !  Now we swap values between the right and left sides and/or move DATAP until all smaller values are on the left and all
      !  larger values are on the right.  Neither the left or right side will be internally ordered yet; however, DATAP will be
      !  in its final position.
      Q3: do
         ! Q3: Search for datum on left >= DATAP
         !   At this point, DATA[L] <= DATAP.  We can therefore start scanning up from L, looking for a value >= DATAP
         !   (this scan is guaranteed to terminate since we initially placed DATAP near the middle of the subsequence).
         I=I+1
         if (data(indx(i)).lt.datap)then
            cycle Q3
         endif
         !-----------------------------------------------------------------------------------------------------------------------
         ! Q4: Search for datum on right <= DATAP
         !
         !   At this point, DATA[R] >= DATAP.  We can therefore start scanning down from R, looking for a value <= DATAP
         !   (this scan is guaranteed to terminate since we initially placed DATAP near the middle of the subsequence).
         Q4: do
            j=j-1
            if (data(indx(j)).le.datap) then
               exit Q4
            endif
         enddo Q4
         !-----------------------------------------------------------------------------------------------------------------------
         ! Q5: Have the two scans collided?
         if (i.lt.j) then
            ! Q6: No, interchange DATA[I] <--> DATA[J] and continue
            indext=indx(i)
            indx(i)=indx(j)
            indx(j)=indext
            cycle Q3
         else
            ! Q7: Yes, select next subsequence to sort
            !   At this point, I >= J and DATA[l] <= DATA[I] == DATAP <= DATA[r], for all L <= l < I and J < r <= R.
         !   If both subsequences are more than M elements long, push the longer one on the stack
            !   and go back to QuickSort the shorter; if only one is more than M elements long, go back and QuickSort it;
         !   otherwise, pop a subsequence off the stack and QuickSort it.
            if (r-j .ge. i-l .and. i-l .gt. m) then
               istk=istk+1
               lstk(istk)=j+1
               rstk(istk)=r
               r=i-1
            else if (i-l .gt. r-j .and. r-j .gt. m) then
               istk=istk+1
               lstk(istk)=l
               rstk(istk)=i-1
               l=j+1
            else if (r-j .gt. m) then
               l=j+1
            else if (i-l .gt. m) then
               r=i-1
            else
               ! Q8: Pop the stack, or terminate QuickSort if empty
               if (istk.lt.1) then
                  exit TOP
               endif
               l=lstk(istk)
               r=rstk(istk)
               istk=istk-1
            endif
            cycle TOP
         endif
         ! never get here, as cycle Q3 or cycle TOP
      enddo Q3
      exit TOP
   enddo TOP
endif
!===================================================================================================================================
! Q9: Straight Insertion sort
do i=2,n
   if (data(indx(i-1)) .gt. data(indx(i))) then
      indexp=indx(i)
      datap=data(indexp)
      p=i-1
      INNER: do
         indx(p+1) = indx(p)
         p=p-1
         if (p.le.0)then
            exit INNER
         endif
         if (data(indx(p)).le.datap)then
            exit INNER
         endif
      enddo INNER
      indx(p+1) = indexp
   endif
enddo
!===================================================================================================================================
!     All done
end subroutine sort_quick_rx
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    unique(3f) - [M_sort] assuming an array is sorted, return array with duplicate values removed
!!##SYNOPSIS
!!
!!    subroutine unique(array,ivals)
!!
!!     class(*),intent(inout)  :: array(:)
!!     integer,intent(out)     :: ivals
!!
!!##DESCRIPTION
!!     Assuming an array is sorted, return the array with duplicate values removed.
!!
!!##OPTIONS
!!    array      may be of type INTEGER, REAL, CHARACTER, COMPLEX, DOUBLEPRECISION,
!!               or complex doubleprecision (that is, complex(kind=kind(0.0d0)) ).
!!    ivals      number of unique values packed into beginning of array
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_unique
!!     use M_sort, only : unique
!!     implicit none
!!     !!character(len=:),allocatable :: strings(:)
!!     character(len=2),allocatable :: strings(:)
!!     integer                      :: icount
!!
!!     strings=[character(len=2) :: '1','1','2','3','4','4','10','20','20','30']
!!     write(*,'(a,*(a3,1x))')'ORIGINAL:',strings
!!     write(*,'("SIZE=",i0)')size(strings)
!!
!!     call unique(strings,icount)
!!
!!     write(*,*)
!!     write(*,'(a,*(a3,1x))')'AFTER   :',strings(1:icount)(:2)
!!     write(*,'("SIZE=",i0)')size(strings)
!!     write(*,'("ICOUNT=",i0)')icount
!!
!!     end program demo_unique
!!
!!    Expected output
!!
!!     ORIGINAL: 1   1   2   3   4   4   10  20  20  30
!!     SIZE=10
!!
!!     AFTER   : 1   2   3   4   10  20  30
!!     SIZE=10
!!     ICOUNT=7
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_integers(array,ivals)
integer,intent(inout)  :: array(:)
integer,intent(out)    :: ivals
   integer             :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
         if(array(i).ne.array(i-1))then
            ivals=ivals+1
            array(ivals)=array(i)
         endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_integers
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_reals(array,ivals)
real,intent(inout)  :: array(:)
integer,intent(out) :: ivals
   integer          :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
           array(ivals)=array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_reals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_strings(array,ivals)
character(len=*),intent(inout),allocatable  :: array(:)
integer,intent(out)                         :: ivals
   integer                                  :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
           array(ivals)=array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_allocatable_strings(array,ivals)
character(len=:),intent(inout),allocatable  :: array(:)
integer,intent(out)                         :: ivals
   integer                                  :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
           array(ivals)=array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_allocatable_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_complex(array,ivals)
complex,intent(inout)  :: array(:)
integer,intent(out)    :: ivals
   integer             :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
           array(ivals)=array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_complex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_doubles(array,ivals)
doubleprecision,intent(inout)  :: array(:)
integer,intent(out)            :: ivals
   integer                     :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
           array(ivals)=array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_doubles
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine unique_complex_double(array,ivals)
complex(kind=cd),intent(inout) :: array(:)   ! array  input/output array
integer,intent(out)            :: ivals
   integer                     :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
           array(ivals)=array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_complex_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!! DESCRIPTION: swap(3f):subroutine swaps two variables of like type (real,integer,complex,character,double)
!!##VERSION:     1.0 19970201
!! AUTHOR:      John S. Urban
!!
!!     M_sort::swap(3f): swap two variables of like type (real,integer,complex,character,double)
!!
!!     SWAP is a Generic Interface in a module with PRIVATE specific procedures.
!!     This means the individual subroutines cannot be called from outside of the M_sort(3fm) module.
!!
!!      o procedure names are declared private in this module so they are not accessible except by their generic name
!!      o procedures must include a "use M_sort" to access the generic name "swap"
!!      o if these routines are recompiled, routines with the USE statement should then be recompiled and reloaded.
!===================================================================================================================================
!===================================================================================================================================
!>
!!##NAME
!!      swap(3f) - [M_sort] elemental subroutine swaps two standard type variables of like type
!!##SYNOPSIS
!!
!!      subroutine swap(X,Y)
!!##DESCRIPTION
!!    Generic subroutine SWAP(GEN1,GEN2) swaps two variables of like type
!!    (real, integer, complex, character, double, logical).
!!
!!    On output, the values of X and Y have been interchanged.
!!    Swapping is commonly required in procedures that sort data.
!!
!!    SWAP(3f) is elemental, so it can operate on vectors and arrays as well
!!    as scalar values.
!!
!!##EXAMPLE
!!
!!   Example program:
!!
!!    program demo_swap
!!    use M_sort, only : swap
!!    integer             :: iarray(2)=[10,20]
!!    real                :: rarray(2)=[11.11,22.22]
!!    doubleprecision     :: darray(2)=[1234.56789d0,9876.54321d0]
!!    complex             :: carray(2)=[(1234,56789),(9876,54321)]
!!    logical             :: larray(2)=[.true.,.false.]
!!    character(len=16)   :: string(2)=["First string    ","The other string"]
!!
!!    integer             :: one(13)=1
!!    integer             :: two(13)=2
!!
!!    integer             :: one2(3,3)=1
!!    integer             :: two2(3,3)=2
!!
!!       print *, "integers before swap ", iarray
!!       call swap (iarray(1), iarray(2))
!!       print *, "integers after swap  ", iarray
!!
!!       print *, "reals before swap ", rarray
!!       call swap (rarray(1), rarray(2))
!!       print *, "reals after swap  ", rarray
!!
!!       print *, "doubles before swap ", darray
!!       call swap (darray(1), darray(2))
!!       print *, "doubles after swap  ", darray
!!
!!       print *, "complexes before swap ", carray
!!       call swap (carray(1), carray(2))
!!       print *, "complexes after swap  ", carray
!!
!!       print *, "logicals before swap ", larray
!!       call swap (larray(1), larray(2))
!!       print *, "logicals after swap  ", larray
!!
!!       print *, "strings before swap ", string
!!       call swap (string(1), string(2))
!!       print *, "strings after swap ", string
!!
!!       write(*,*)'swap two vectors'
!!       write(*,'("one before: ",*(i0,:","))') one
!!       write(*,'("two before: ",*(i0,:","))') two
!!       call swap(one,two)
!!       write(*,'("one after: ",*(i0,:","))') one
!!       write(*,'("two after: ",*(i0,:","))') two
!!
!!       write(*,*)'given these arrays initially each time '
!!       one2=1
!!       two2=2
!!       call printarrays()
!!
!!       write(*,*)'swap two rows'
!!       one2=1
!!       two2=2
!!       call swap(one2(2,:),two2(3,:))
!!       call printarrays()
!!
!!       write(*,*)'swap two columns'
!!       one2=1
!!       two2=2
!!       call swap(one2(:,2),two2(:,2))
!!       call printarrays()
!!
!!       write(*,*)'swap two arrays with same number of elements'
!!       one2=1
!!       two2=2
!!       call swap(one2,two2)
!!       call printarrays()
!!
!!       contains
!!       subroutine printarrays()
!!       integer :: i
!!       do i=1,size(one2(1,:))
!!          write(*,'(*(i0,:","))') one2(i,:)
!!       enddo
!!       write(*,*)
!!       do i=1,size(two2(1,:))
!!          write(*,'(*(i0,:","))') two2(i,:)
!!       enddo
!!       end subroutine printarrays
!!
!!    end program demo_swap
!!
!!   Expected Results:
!!
!!    > integers before swap           10          20
!!    > integers after swap            20          10
!!    > reals before swap    11.1099997       22.2199993
!!    > reals after swap     22.2199993       11.1099997
!!    > doubles before swap    1234.5678900000000        9876.5432099999998
!!    > doubles after swap     9876.5432099999998        1234.5678900000000
!!    > complexes before swap  (  1234.00000    ,  56789.0000    ) (  9876.00000    ,  54321.0000    )
!!    > complexes after swap   (  9876.00000    ,  54321.0000    ) (  1234.00000    ,  56789.0000    )
!!    > logicals before swap  T F
!!    > logicals after swap   F T
!!    > strings before swap First string    The other string
!!    > strings after swap The other stringFirst string
!!    > swap two vectors
!!    >one before: 1,1,1,1,1,1,1,1,1,1,1,1,1
!!    >two before: 2,2,2,2,2,2,2,2,2,2,2,2,2
!!    >one after: 2,2,2,2,2,2,2,2,2,2,2,2,2
!!    >two after: 1,1,1,1,1,1,1,1,1,1,1,1,1
!!    > given these arrays initially each time
!!    >1,1,1
!!    >1,1,1
!!    >1,1,1
!!    >
!!    >2,2,2
!!    >2,2,2
!!    >2,2,2
!!    > swap two rows
!!    >1,1,1
!!    >2,2,2
!!    >1,1,1
!!    >
!!    >2,2,2
!!    >2,2,2
!!    >1,1,1
!!    > swap two columns
!!    >1,2,1
!!    >1,2,1
!!    >1,2,1
!!    >
!!    >2,1,2
!!    >2,1,2
!!    >2,1,2
!!    > swap two arrays with same number of elements
!!    >2,2,2
!!    >2,2,2
!!    >2,2,2
!!    >
!!    >1,1,1
!!    >1,1,1
!!    >1,1,1
!===================================================================================================================================
!===================================================================================================================================
elemental subroutine d_swap(x,y)
character(len=*),parameter::ident="@(#)M_sort::d_swap(3fp): swap two double variables"
doubleprecision, intent(inout) :: x,y
doubleprecision                :: temp
   temp = x; x = y; y = temp
end subroutine d_swap
!===================================================================================================================================
elemental subroutine r_swap(x,y)
character(len=*),parameter::ident="@(#)M_sort::r_swap(3fp): swap two real variables"
real, intent(inout) :: x,y
real                :: temp
   temp = x; x = y; y = temp
end subroutine r_swap
!===================================================================================================================================
elemental subroutine i_swap(i,j)
character(len=*),parameter::ident="@(#)M_sort::i_swap(3fp): swap two integer variables"
integer, intent(inout) :: i,j
integer                :: itemp
   itemp = i; i = j; j = itemp
end subroutine i_swap
!===================================================================================================================================
elemental subroutine l_swap(l,ll)
character(len=*),parameter::ident="@(#)M_sort::l_swap(3fp): swap two logical variables"
logical, intent(inout) :: l,ll
logical                :: ltemp
   ltemp = l; l = ll; ll = ltemp
end subroutine l_swap
!===================================================================================================================================
elemental subroutine c_swap(xx,yy)
character(len=*),parameter::ident="@(#)M_sort::c_swap(3fp): swap two complex variables"
complex, intent(inout) :: xx,yy
complex                :: tt
   tt = xx; xx = yy; yy = tt
end subroutine c_swap
!===================================================================================================================================
elemental subroutine cd_swap(xx,yy)
character(len=*),parameter::ident="@(#)M_sort::cd_swap(3fp): swap two double complex variables"
complex(kind=cd), intent(inout) :: xx,yy
complex(kind=cd)                :: tt
   tt = xx; xx = yy; yy = tt
end subroutine cd_swap
!===================================================================================================================================
elemental subroutine s_swap(string1,string2)

!>
!!   F90 NOTE:
!!    string_temp is an automatic character object whose size is not a constant expression.
!!    Automatic objects cannot be saved or initialized.
!!    Note that the len of a dummy argument can be used to calculate the automatic variable length.
!!    Therefore, you can make sure len is at least max(len(string1),len(string2)) by adding the two lengths together:
!===================================================================================================================================

character(len=*),parameter::ident="@(#)M_sort::s_swap(3fp): swap two double variables"
character(len=*), intent(inout)             :: string1,string2
!character( len=len(string1) + len(string2)) :: string_temp
character( len=max(len(string1),len(string2))) :: string_temp
   string_temp = string1; string1 = string2; string2 = string_temp
end subroutine s_swap
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_sort
!===================================================================================================================================
