!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
Module M_sort
implicit none                       ! declare that all variables must be explicitly declared
integer,parameter :: cd=kind(0.0d0)
private                             ! the PRIVATE declaration requires use of a module, and changes the default from PUBLIC
public sort_quick_rx
public sort_shell

public :: swap
!-public :: exchange
public :: swap_any

public unique

public tree_insert
public tree_print
public tree_node

public test_suite_m_sort
!===================================================================================================================================

character(len=*),parameter::ident_1="@(#)M_sort::sort_shell(3f): Generic subroutine sorts the array X using a shell sort"

! SORT_SHELL is a Generic Interface in a module with PRIVATE specific procedures. This means the individual subroutines
! cannot be called from outside of this module.
interface sort_shell
   module procedure sort_shell_integers, sort_shell_reals, sort_shell_strings
   module procedure sort_shell_complex, sort_shell_doubles, sort_shell_complex_double
end interface
!===================================================================================================================================

character(len=*),parameter::ident_2="&
&@(#)M_sort::unique(3f): assuming an array is sorted, return array with duplicate values removed"

interface unique
   module procedure unique_integers, unique_reals, unique_strings_allocatable_len !!, unique_strings
   module procedure unique_complex, unique_doubles, unique_complex_double
end interface
!===================================================================================================================================

character(len=*),parameter::ident_3="@(#)M_sort::swap(3f): swap two variables of like type (real,integer,complex,character,double)"

interface swap
   module procedure r_swap, i_swap, c_swap, s_swap, d_swap, l_swap, cd_swap
end interface

!-interface exchange
!-   module procedure exchange_scalar, exchange_array
!-end interface

interface swap_any
   module procedure swap_any_scalar, swap_any_array
end interface
!===================================================================================================================================
! For TREE SORT
type tree_node
   integer :: value
   type (tree_node), pointer :: left, right
end type tree_node
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
!!    Exchange sorts      Bubble sort, Cocktail shaker sort, Odd-even sort, Comb sort, Gnome sort, Quicksort, Stooge sort, Bogosort
!!    Selection sorts     Selection sort, Heapsort, Smoothsort, Cartesian tree sort, Tournament sort, Cycle sort
!!    Insertion sorts     Insertion sort, Shellsort, Splaysort, Tree sort, Library sort, Patience sorting
!!    Merge sorts         Merge sort, Cascade merge sort, Oscillating merge sort, Polyphase merge sort
!!    Distribution sorts  American flag sort, Bead sort, Bucket sort, Burstsort, Counting sort, Pigeonhole sort, Proxmap sort,
!!                        Radix sort, Flashsort
!!    Concurrent sorts    Bitonic sorter, Batcher odd-even mergesort, Pairwise sorting network
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
!!    The run-time of Quicksort ranges from O(n log n) with the best
!!    pivots, to O(n2) with the worst pivots, where n is the
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
!!    subroutine sort_shell(lines,order[,startcol,endcol])
!!
!!     character(len=*),intent(inout) :: lines(:)
!!     character(len=*),intent(in)    :: order
!!     integer,optional,intent(in)    :: startcol, endcol
!!
!!    subroutine sort_shell(ints,order)
!!
!!     integer,intent(inout)          :: ints(:)
!!     character(len=*),intent(in)    :: order
!!
!!    subroutine sort_shell(reals,order)
!!
!!     real,intent(inout)             :: reals(:)
!!     character(len=*),intent(in)    :: order
!!
!!    subroutine sort_shell(complexs,order,type)
!!
!!     character(len=*),intent(inout) :: lines(:)
!!     character(len=*),intent(in)    :: order
!!     character(len=*),intent(in)    :: type
!!
!!
!!##DESCRIPTION
!!
!!       subroutine sort_shell(3f) sorts an array over a specified field
!!       in numeric or alphanumeric order.
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
!!    Usage:
!!
!!     X          input/output array to sort of type CHARACTER, INTEGER,
!!                REAL, DOUBLEPRECISION, COMPLEX, or DOUBLEPRECISION COMPLEX.
!!     order      sort order
!!                o A for Ascending  (a-z for strings, small to large for values)
!!                o D for Descending (z-a for strings, large to small for values, default)
!!
!!    FOR CHARACTER DATA:
!!
!!     startcol   character position in strings which starts sort field.
!!                Only applies to character values. Defaults to 1. Optional.
!!     endcol     character position in strings which ends sort field
!!                Only applies to character values. Defaults to end of string.
!!                Optional.
!!
!!    FOR COMPLEX AND COMPLEX(KIND=KIND(0.0D0)) DATA:
!!
!!     type       Sort by
!!
!!                  R  for Real component,
!!                  I  for Imaginary component,
!!                  S  Sqrt(R**2+I**2)
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

character(len=*),parameter::ident_4="@(#)M_sort::sort_shell_strings(3fp):sort strings over specified field using shell sort"

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

character(len=*),parameter::ident_5="&
&@(#)M_sort::sort_shell_strings_lh(3fp):sort strings(a-z) over specified field using shell sort"

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

character(len=*),parameter::ident_6="&
&@(#)M_sort::sort_shell_strings_hl(3fp):sort strings(z-a) over specified field using shell sort"

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

character(len=*),parameter::ident_7="@(#)M_sort::sort_shell_integers(3fp):sort integer array using Shell sort and specified order"

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

character(len=*),parameter::ident_8="@(#)M_sort::sort_shell_integers_hl(3fp):sort integer array using Shell sort (high to low)"

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

character(len=*),parameter::ident_9="@(#)M_sort::sort_shell_integers_lh(3fp):sort integer array using Shell sort low to high"

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

character(len=*),parameter::ident_10="@(#)M_sort::sort_shell_reals(3fp):sort real array using Shell sort and specified order"

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

character(len=*),parameter::ident_11="@(#)M_sort::sort_shell_reals_hl(3fp):sort real array using Shell sort (high to low)"

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

character(len=*),parameter::ident_12="@(#)M_sort::sort_shell_reals_lh(3fp):sort real array using Shell sort (low to high)"

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

character(len=*),parameter::ident_13="@(#)M_sort::sort_shell_doubles(3fp):sort double array using Shell sort and specified order"

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

character(len=*),parameter::ident_14="@(#)M_sort::sort_shell_doubles_hl(3fp):sort double array using Shell sort (high to low)"

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

character(len=*),parameter::ident_15="@(#)M_sort::sort_shell_doubles_lh(3fp):sort double array using Shell sort (low to high)"

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

character(len=*),parameter::ident_16="@(#)M_sort::sort_shell_complex(3fp):sort complex array using Shell sort"

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

character(len=*),parameter::ident_17="@(#)M_sort::sort_shell_reals_hl(3fp):sort complex array using Shell sort (high to low)"

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
               csize1=sqrt(dble(array(j))**2+aimag(array(j))**2)
               csize2=sqrt(dble(array(jg))**2+aimag(array(jg))**2)
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

character(len=*),parameter::ident_18="@(#)M_sort::sort_shell_reals_lh(3fp):sort complex array using Shell sort (low to high)"

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
               csize1=sqrt(dble(array(j))**2+aimag(array(j))**2)
               csize2=sqrt(dble(array(jg))**2+aimag(array(jg))**2)
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

character(len=*),parameter::ident_19="@(#)M_sort::sort_shell_complex_double(3fp):sort double complex array using Shell sort"

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

character(len=*),parameter::ident_20="@(#)M_sort::sort_shell_reals_hl(3fp):sort double complex array using Shell sort (high to low)"

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
               cdsize1=sqrt(dble(array(j))**2+aimag(array(j))**2)
               cdsize2=sqrt(dble(array(jg))**2+aimag(array(jg))**2)
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

character(len=*),parameter::ident_21="@(#)M_sort::sort_shell_reals_lh(3fp):sort double complex array using Shell sort (low to high)"

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
               cdsize1=sqrt(dble(array(j))**2+aimag(array(j))**2)
               cdsize2=sqrt(dble(array(jg))**2+aimag(array(jg))**2)
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

character(len=*),parameter::ident_22="@(#)M_sort::sort_quick_rx(3f): indexed hybrid quicksort of a real array"

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
!!     character(len=:),allocatable :: strings(:)
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
subroutine unique_strings_allocatable_len(array,ivals)
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
end subroutine unique_strings_allocatable_len
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
!!    swap(3f) - [M_sort] elemental subroutine swaps two standard type variables of like type
!!##SYNOPSIS
!!
!!    subroutine swap(X,Y)
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
character(len=*),parameter::ident_23="@(#)M_sort::d_swap(3fp): swap two double variables"
doubleprecision, intent(inout) :: x,y
doubleprecision                :: temp
   temp = x; x = y; y = temp
end subroutine d_swap
!===================================================================================================================================
elemental subroutine r_swap(x,y)
character(len=*),parameter::ident_24="@(#)M_sort::r_swap(3fp): swap two real variables"
real, intent(inout) :: x,y
real                :: temp
   temp = x; x = y; y = temp
end subroutine r_swap
!===================================================================================================================================
elemental subroutine i_swap(i,j)
character(len=*),parameter::ident_25="@(#)M_sort::i_swap(3fp): swap two integer variables"
integer, intent(inout) :: i,j
integer                :: itemp
   itemp = i; i = j; j = itemp
end subroutine i_swap
!===================================================================================================================================
elemental subroutine l_swap(l,ll)
character(len=*),parameter::ident_26="@(#)M_sort::l_swap(3fp): swap two logical variables"
logical, intent(inout) :: l,ll
logical                :: ltemp
   ltemp = l; l = ll; ll = ltemp
end subroutine l_swap
!===================================================================================================================================
elemental subroutine c_swap(xx,yy)
character(len=*),parameter::ident_27="@(#)M_sort::c_swap(3fp): swap two complex variables"
complex, intent(inout) :: xx,yy
complex                :: tt
   tt = xx; xx = yy; yy = tt
end subroutine c_swap
!===================================================================================================================================
elemental subroutine cd_swap(xx,yy)
character(len=*),parameter::ident_28="@(#)M_sort::cd_swap(3fp): swap two double complex variables"
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

character(len=*),parameter::ident_29="@(#)M_sort::s_swap(3fp): swap two double variables"
character(len=*), intent(inout)             :: string1,string2
!character( len=len(string1) + len(string2)) :: string_temp
character( len=max(len(string1),len(string2))) :: string_temp
   string_temp = string1; string1 = string2; string2 = string_temp
end subroutine s_swap
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!-$DOCUMENT COMMENT -file exchange.3.man
!-NAME
!-   exchange(3f) - [M_sort] subroutine exchanges two variables of like type
!-SYNOPSIS
!-   subroutine exchange(X,Y)
!-DESCRIPTION
!-   Generic subroutine exchange(GEN1,GEN2) exchanges two variables of
!-   like type.
!-
!-   On output, the values of X and Y have been interchanged. Swapping is
!-   commonly required in procedures that sort data.
!-
!-   This routine uses the memcpy(3c) procedure, so data is assumed to be
!-   contiguous and to not overlap.
!-
!-   DO NOT CURRENTLY USE WITH CHARACTER VALUES WITH gfortran, and do not
!-   use with anything but scalar values.
!-
!-EXAMPLE
!-  Example program:
!-
!-   program demo_exchange
!-   use M_sort, only : exchange
!-   integer             :: iarray(2)=[10,20]
!-   real                :: rarray(2)=[11.11,22.22]
!-   doubleprecision     :: darray(2)=[1234.56789d0,9876.54321d0]
!-   complex             :: carray(2)=[(1234,56789),(9876,54321)]
!-   logical             :: larray(2)=[.true.,.false.]
!-   character(len=16)   :: string(2)=["First string    ","The other string"]
!-
!-   integer             :: one(13)=1
!-   integer             :: two(13)=2
!-
!-   integer             :: one2(3,3)=1
!-   integer             :: two2(3,3)=2
!-
!-      print *, "integers before exchange ", iarray
!-      call exchange (iarray(1), iarray(2))
!-      print *, "integers after exchange  ", iarray
!-
!-      print *, "reals before exchange ", rarray
!-      call exchange (rarray(1), rarray(2))
!-      print *, "reals after exchange  ", rarray
!-
!-      print *, "doubles before exchange ", darray
!-      call exchange (darray(1), darray(2))
!-      print *, "doubles after exchange  ", darray
!-
!-      print *, "complexes before exchange ", carray
!-      call exchange (carray(1), carray(2))
!-      print *, "complexes after exchange  ", carray
!-
!-      print *, "logicals before exchange ", larray
!-      call exchange (larray(1), larray(2))
!-      print *, "logicals after exchange  ", larray
!-
!-      write(*,*)'GETS THIS WRONG IN GFORTRAN'
!-      print *, "strings before exchange ", string
!-      call exchange (string(1), string(2))
!-      print *, "strings after exchange ", string
!-
!-      write(*,*)'exchange two vectors'
!-      write(*,'("one before: ",*(i0,:","))') one
!-      write(*,'("two before: ",*(i0,:","))') two
!-      call exchange(one,two)
!-      write(*,'("one after: ",*(i0,:","))') one
!-      write(*,'("two after: ",*(i0,:","))') two
!-
!-      write(*,*)'given these arrays initially each time '
!-      one2=1
!-      two2=2
!-      call printarrays()
!-
!-      write(*,*)'GETS THIS WRONG'
!-      write(*,*)'exchange two rows'
!-      one2=1
!-      two2=2
!-      call exchange(one2(2,:),two2(3,:))
!-      call printarrays()
!-
!-      write(*,*)'GETS THIS WRONG'
!-      write(*,*)'exchange two columns'
!-      one2=1
!-      two2=2
!-      call exchange(one2(:,2),two2(:,2))
!-      call printarrays()
!-
!-      write(*,*)'CANNOT DO MULTI-DIMENSIONAL ARRAYS YET'
!-      write(*,*)'exchange two arrays with same number of elements'
!-      one2=1
!-      two2=2
!-      !call exchange(one2,two2)
!-      !call printarrays()
!-
!-      contains
!-      subroutine printarrays()
!-      integer :: i
!-      do i=1,size(one2(1,:))
!-         write(*,'(*(i0,:","))') one2(i,:)
!-      enddo
!-      write(*,*)
!-      do i=1,size(two2(1,:))
!-         write(*,'(*(i0,:","))') two2(i,:)
!-      enddo
!-      end subroutine printarrays
!-
!-   end program demo_exchange
!-
!-  Expected Results:
!-
!-   > integers before exchange           10          20
!-   > integers after exchange            20          10
!-   > reals before exchange    11.1099997       22.2199993
!-   > reals after exchange     22.2199993       11.1099997
!-   > doubles before exchange    1234.5678900000000        9876.5432099999998
!-   > doubles after exchange     9876.5432099999998        1234.5678900000000
!-   > complexes before exchange  (  1234.00000    ,  56789.0000    ) (  9876.00000    ,  54321.0000    )
!-   > complexes after exchange   (  9876.00000    ,  54321.0000    ) (  1234.00000    ,  56789.0000    )
!-   > logicals before exchange  T F
!-   > logicals after exchange   F T
!-   > strings before exchange First string    The other string
!-   > strings after exchange The other stringFirst string
!-   > exchange two vectors
!-   >one before: 1,1,1,1,1,1,1,1,1,1,1,1,1
!-   >two before: 2,2,2,2,2,2,2,2,2,2,2,2,2
!-   >one after: 2,2,2,2,2,2,2,2,2,2,2,2,2
!-   >two after: 1,1,1,1,1,1,1,1,1,1,1,1,1
!-   > given these arrays initially each time
!-   >1,1,1
!-   >1,1,1
!-   >1,1,1
!-   >
!-   >2,2,2
!-   >2,2,2
!-   >2,2,2
!-   > exchange two rows
!-   >1,1,1
!-   >2,2,2
!-   >1,1,1
!-   >
!-   >2,2,2
!-   >2,2,2
!-   >1,1,1
!-   > exchange two columns
!-   >1,2,1
!-   >1,2,1
!-   >1,2,1
!-   >
!-   >2,1,2
!-   >2,1,2
!-   >2,1,2
!-   > exchange two arrays with same number of elements
!-   >2,2,2
!-   >2,2,2
!-   >2,2,2
!-   >
!-   >1,1,1
!-   >1,1,1
!-   >1,1,1
!-
!-$DOCUMENT END
!-subroutine exchange_scalar(lhs,rhs)
!-use iso_c_binding, only : c_ptr, c_size_t
!-use M_system,      only : system_memcpy
!-implicit none
!-class(*),intent(inout) :: lhs, rhs
!-class(*), allocatable :: temp
!-type(c_ptr) :: tmp
!-   temp=lhs
!-   if(same_type_as(lhs,rhs))then
!-      call system_memcpy(loc(lhs),  loc(rhs), storage_size(lhs,kind=c_size_t)/8_c_size_t )
!-      call system_memcpy(loc(rhs), loc(temp), storage_size(rhs,kind=c_size_t)/8_c_size_t )
!-   else
!-      write(*,*)'error: exchange(3f) called with values of different type'
!-      stop 4
!-   endif
!-end subroutine exchange_scalar
!===================================================================================================================================
!-subroutine exchange_array(lhs,rhs)
!-class(*) :: lhs(:),rhs(:)
!-integer  :: i
!-   if(size(lhs).eq.size(rhs))then
!-      do i=1,size(lhs)
!-         call exchange_scalar(lhs(i),rhs(i))
!-      enddo
!-   else
!-      write(*,*)'error: exchange(3f) called with arrays of different sizes'
!-      stop 5
!-   endif
!-end subroutine exchange_array
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    swap_any(3f) - [M_sort] subroutine swap_anys two variables of like type
!!##SYNOPSIS
!!
!!    subroutine swap_any(X,Y)
!!##DESCRIPTION
!!    Generic subroutine swap_any(GEN1,GEN2) swap_anys two variables of
!!    like type.
!!
!!    On output, the values of X and Y have been interchanged. Swapping is
!!    commonly required in procedures that sort data.
!!
!!    DO NOT CURRENTLY USE WITH WITH ANYTHING BUT SCALAR VALUES.
!!
!!##EXAMPLE
!!
!!   Example program:
!!
!!    program demo_swap_any
!!    use M_sort, only : swap_any
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
!!       print *, "integers before swap_any ", iarray
!!       call swap_any (iarray(1), iarray(2))
!!       print *, "integers after swap_any  ", iarray
!!
!!       print *, "reals before swap_any ", rarray
!!       call swap_any (rarray(1), rarray(2))
!!       print *, "reals after swap_any  ", rarray
!!
!!       print *, "doubles before swap_any ", darray
!!       call swap_any (darray(1), darray(2))
!!       print *, "doubles after swap_any  ", darray
!!
!!       print *, "complexes before swap_any ", carray
!!       call swap_any (carray(1), carray(2))
!!       print *, "complexes after swap_any  ", carray
!!
!!       print *, "logicals before swap_any ", larray
!!       call swap_any (larray(1), larray(2))
!!       print *, "logicals after swap_any  ", larray
!!
!!       print *, "strings before swap_any ", string
!!       call swap_any (string(1), string(2))
!!       print *, "strings after swap_any ", string
!!
!!       write(*,*)'swap_any two vectors'
!!       write(*,'("one before: ",*(i0,:","))') one
!!       write(*,'("two before: ",*(i0,:","))') two
!!       call swap_any(one,two)
!!       write(*,'("one after: ",*(i0,:","))') one
!!       write(*,'("two after: ",*(i0,:","))') two
!!
!!       write(*,*)'given these arrays initially each time '
!!       one2=1
!!       two2=2
!!       call printarrays()
!!
!!       write(*,*)'GETS THIS WRONG'
!!       write(*,*)'swap_any two rows'
!!       one2=1
!!       two2=2
!!       call swap_any(one2(2,:),two2(3,:))
!!       call printarrays()
!!
!!       write(*,*)'GETS THIS WRONG'
!!       write(*,*)'swap_any two columns'
!!       one2=1
!!       two2=2
!!       call swap_any(one2(:,2),two2(:,2))
!!       call printarrays()
!!
!!       write(*,*)'CANNOT DO MULTI-DIMENSIONAL ARRAYS YET'
!!       write(*,*)'swap_any two arrays with same number of elements'
!!       one2=1
!!       two2=2
!!       !call swap_any(one2,two2)
!!       !call printarrays()
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
!!    end program demo_swap_any
!!
!!   Expected Results:
!!
!!    > integers before swap_any           10          20
!!    > integers after swap_any            20          10
!!    > reals before swap_any    11.1099997       22.2199993
!!    > reals after swap_any     22.2199993       11.1099997
!!    > doubles before swap_any    1234.5678900000000        9876.5432099999998
!!    > doubles after swap_any     9876.5432099999998        1234.5678900000000
!!    > complexes before swap_any  (  1234.00000    ,  56789.0000    ) (  9876.00000    ,  54321.0000    )
!!    > complexes after swap_any   (  9876.00000    ,  54321.0000    ) (  1234.00000    ,  56789.0000    )
!!    > logicals before swap_any  T F
!!    > logicals after swap_any   F T
!!    > strings before swap_any First string    The other string
!!    > strings after swap_any The other stringFirst string
!!    > swap_any two vectors
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
!!    > swap_any two rows
!!    >1,1,1
!!    >2,2,2
!!    >1,1,1
!!    >
!!    >2,2,2
!!    >2,2,2
!!    >1,1,1
!!    > swap_any two columns
!!    >1,2,1
!!    >1,2,1
!!    >1,2,1
!!    >
!!    >2,1,2
!!    >2,1,2
!!    >2,1,2
!!    > swap_any two arrays with same number of elements
!!    >2,2,2
!!    >2,2,2
!!    >2,2,2
!!    >
!!    >1,1,1
!!    >1,1,1
!!    >1,1,1
!===================================================================================================================================
subroutine swap_any_scalar( lhs, rhs )
use M_anything, only : anything_to_bytes, bytes_to_anything
class(*) :: rhs
class(*) :: lhs
character(len=1),allocatable :: templ(:)
character(len=1),allocatable :: tempr(:)
   tempr=anything_to_bytes(rhs)
   templ=anything_to_bytes(lhs)
   call bytes_to_anything(templ,rhs)
   call bytes_to_anything(tempr,lhs)
end subroutine swap_any_scalar

subroutine swap_any_array( lhs, rhs )
class(*) :: rhs(:)
class(*) :: lhs(:)
integer  :: i
   do i=1,size(lhs)
      call swap_any_scalar(lhs(i),rhs(i))
   enddo
end subroutine swap_any_array
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!
! XXXXXXX XXXXXX  XXXXXXX XXXXXXX
! X  X  X  X    X  X    X  X    X
!    X     X    X  X       X
!    X     X    X  X  X    X  X
!    X     XXXXX   XXXX    XXXX
!    X     X  X    X  X    X  X
!    X     X  X    X       X
!    X     X   X   X    X  X    X
!   XXX   XXX  XX XXXXXXX XXXXXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    tree_insert(3f) - [M_sort] sort a number of integers by building a tree, sorted in infix order
!!##SYNOPSIS
!!
!!   subroutine tree_insert(t,number)
!!
!!    type(tree_node), pointer :: t
!!    integer             :: number
!!
!!##DESCRIPTION
!!   Sorts a number of integers by building a tree, sorted in infix order.
!!   This sort has expected behavior n log n, but worst case (input is
!!   sorted) n ** 2.
!!
!!##AUTHOR
!!   Copyright (c) 1990 by Walter S. Brainerd, Charles H. Goldberg,
!!   and Jeanne C. Adams. This code may be copied and used without
!!   restriction as long as this notice is retained.
!!
!!##EXAMPLE
!!
!!  sample program
!!
!!    program tree_sort
!!    use M_sort, only : tree_node, tree_insert, tree_print
!!    implicit none
!!    type(tree_node), pointer :: t     ! A tree
!!    integer             :: number
!!    integer             :: ios
!!    nullify(t)                        ! Start with empty tree
!!    infinite: do
!!       read (*,*,iostat=ios) number
!!       if(ios.ne.0)exit infinite
!!       call tree_insert(t,number)     ! Put next number in tree
!!    enddo infinite
!!    call tree_print(t)                ! Print nodes of tree in infix order
!!    end program tree_sort
!===================================================================================================================================
recursive subroutine tree_insert (t, number)
implicit none

character(len=*),parameter::ident_30="&
&@(#)M_sort::tree_insert(3f): sort a number of integers by building a tree, sorted in infix order"

type (tree_node), pointer :: t  ! a tree
integer, intent (in) :: number
   ! if (sub)tree is empty, put number at root
   if (.not. associated (t)) then
      allocate (t)
      t % value = number
      nullify (t % left)
      nullify (t % right)
      ! otherwise, insert into correct subtree
   else if (number < t % value) then
      call tree_insert (t % left, number)
   else
      call tree_insert (t % right, number)
   endif
end subroutine tree_insert
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    tree_print(3f) - [M_sort] print a sorted integer tree generated by tree_insert(3f)
!!##SYNOPSIS
!!
!!   subroutine tree_print(t)
!!
!!    type(tree_node), pointer :: t
!!
!!##DESCRIPTION
!!   Print a tree of sorted integers created by insert_tree(3f).
!!
!!##AUTHOR
!!   Copyright (c) 1990 by Walter S. Brainerd, Charles H. Goldberg,
!!   and Jeanne C. Adams. This code may be copied and used without
!!   restriction as long as this notice is retained.
!!
!!##EXAMPLE
!!
!!  sample program
!!
!!    program tree_sort
!!    use M_sort, only : tree_node, tree_insert, tree_print
!!    implicit none
!!    type(tree_node), pointer :: t     ! A tree
!!    integer             :: number
!!    integer             :: ios
!!    nullify(t)                        ! Start with empty tree
!!    infinite: do
!!       read (*,*,iostat=ios) number
!!       if(ios.ne.0)exit infinite
!!       call tree_insert(t,number)     ! Put next number in tree
!!    enddo infinite
!!    call tree_print(t)                ! Print nodes of tree in infix order
!!    end program tree_sort
!===================================================================================================================================
recursive subroutine tree_print(t)
implicit none

character(len=*),parameter::ident_31="@(#)M_sort::tree_print(3f):"

type (tree_node), pointer :: t  ! a tree

   if (associated (t)) then
      call tree_print (t % left)
      print *, t % value
      call tree_print (t % right)
   endif
end subroutine tree_print
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_m_sort()
   call test_sort_shell()
   call test_sort_quick_rx()
   call test_unique()
   call test_swap()

   call test_tree_insert()
   call test_tree_print()

end subroutine test_suite_m_sort
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tree_insert()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('insert',msg='')
   !!call unit_check('insert', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('insert',msg='')
end subroutine test_tree_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tree_print()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('tree_print',msg='')
   !!call unit_check('tree_print', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('tree_print',msg='')
end subroutine test_tree_print
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_sort_shell()
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
implicit none
integer,parameter            :: cd=kind(0.0d0)
integer,parameter            :: isz=10000
complex(kind=cd)             :: ccdd(isz)
complex                      :: cc(isz)
doubleprecision              :: dd(isz)
real                         :: rr(isz), rr2(isz)
integer                      :: ii(isz)
character(len=:),allocatable :: array(:)
integer                      :: csz
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_check_start('sort_shell','-library libGPF') ! start tests
!-----------------------------------------------------------------------------------------------------------------------------------
array= [ character(len=20) :: &
   'red',   'green', 'blue', 'yellow', 'orange',   'black', 'white', 'brown', 'gray', 'cyan',   'magenta',  'purple']
csz=size(array)
call sort_shell(array,order='a')
call unit_check('sort_shell',all(array(1:csz-1) .le. array(2:csz)),msg='sort string array, ascending')  ! verify in ascending order
!-----------------------------------------------------------------------------------------------------------------------------------
array= [ character(len=20) :: &
   'RED',   'GREEN', 'BLUE', 'YELLOW', 'ORANGE',   'BLACK', 'WHITE', 'BROWN', 'GRAY', 'CYAN',   'MAGENTA',  'PURPLE']
csz=size(array)
call sort_shell(array,order='d')
call unit_check('sort_shell',all(array(1:csz-1) .ge. array(2:csz)),msg='sort string array, descending') ! verify in descending order
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)                                           ! RR contains uniformly distributed random numbers from 0.0 to <1.0
II=RR*HUGE(1)                                                    ! spread values out along range of INTEGER
call sort_shell(ii,order='a')
call unit_check('sort_shell',all(ii(1:isz-1) .le. ii(2:isz)),msg='sort integer, ascending array')  ! verify in ascending order
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
II=RR*HUGE(1)
call sort_shell(ii,order='d')
call unit_check('sort_shell',all(ii(1:isz-1) .ge. ii(2:isz)),msg='sort integer, descending array')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
call sort_shell(rr,order='a')
call unit_check('sort_shell',all(rr(1:isz-1) .le. rr(2:isz)),msg='sort real, ascending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
call sort_shell(rr,order='d')
call unit_check('sort_shell',all(rr(1:isz-1) .ge. rr(2:isz)),msg='sort real, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
dd=RR*2000.0d0
call sort_shell(dd,order='a')
call unit_check('sort_shell',all(dd(1:isz-1) .le. dd(2:isz)),msg='sort doubleprecision, ascending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
dd=RR*2000.0d0
call sort_shell(dd,order='d')
call unit_check('sort_shell',all(dd(1:isz-1) .ge. dd(2:isz)),msg='sort doubleprecision, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
CALL RANDOM_NUMBER(RR2)

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='a',type='real')
call unit_check('sort_shell',all(real(cc(1:isz-1)) .le. real(cc(2:isz))),msg='sort complex by real component, ascending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='d',type='real')
call unit_check('sort_shell',all(real(cc(1:isz-1)) .ge. real(cc(2:isz))),msg='sort complex by real component, descending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='a',type='imaginary')
call unit_check('sort_shell',all(aimag(cc(1:isz-1)).le.aimag(cc(2:isz))),msg='sort complex by imaginary component, ascending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='d',type='imaginary')
call unit_check('sort_shell',all(aimag(cc(1:isz-1)) .ge. aimag(cc(2:isz))),msg='sort complex by imaginary component, descending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='a',type='size')
call unit_check('sort_shell', &
   all(sqrt( dble(cc(1:isz-1))**2 +aimag(cc(1:isz-1))**2) .le. sqrt(dble(cc(2:isz))**2+aimag(cc(2:isz))**2)), &
   msg='sort complex array by magnitude, ascending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='d',type='size')
call unit_check('sort_shell', &
   all(sqrt( dble(cc(1:isz-1))**2 +aimag(cc(1:isz-1))**2) .ge. sqrt(dble(cc(2:isz))**2+aimag(cc(2:isz))**2)), &
   msg='sort complex array by magnitude, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
CALL RANDOM_NUMBER(RR2)

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='a',type='real')
call unit_check('sort_shell',all(real(ccdd(1:isz-1)).le.real(ccdd(2:isz))), msg='sort double complex by real component, ascending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='d',type='real')
call unit_check('sort_shell',all(real(ccdd(1:isz-1)).ge.real(ccdd(2:isz))), msg='sort double complex by real component, descending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='a',type='imaginary')
call unit_check('sort_shell', &
   all(aimag(ccdd(1:isz-1)).le.aimag(ccdd(2:isz))), msg='sort double complex by imaginary component, ascending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='d',type='imaginary')
call unit_check('sort_shell', &
   all(aimag(ccdd(1:isz-1)).ge.aimag(ccdd(2:isz))), msg='sort double complex by imaginary component, descending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='a',type='size')
call unit_check('sort_shell', &
   all(sqrt(real(ccdd(1:isz-1))**2+aimag(ccdd(1:isz-1))**2) .le. sqrt(real(ccdd(2:isz))**2+aimag(ccdd(2:isz))**2)),  &
   msg='sort double complex by magnitude, ascending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='d',type='size')
call unit_check('sort_shell', &
   all(sqrt(real(ccdd(1:isz-1))**2+aimag(ccdd(1:isz-1))**2) .ge. sqrt(real(ccdd(2:isz))**2+aimag(ccdd(2:isz))**2)),  &
   msg='sort double complex by magnitude, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_check_done('sort_shell') ! assume if got here passed checks
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_sort_shell
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_sort_quick_rx
use M_debug,   only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
implicit none
integer,parameter            :: cd=kind(0.0d0)
integer,parameter            :: isz=10000000
real                         :: rr(isz)
integer                      :: ii(isz)
integer                      :: i
logical                      :: gb
call unit_check_start('sort_quick_rx', '-library libGPF') ! start tests

CALL RANDOM_NUMBER(RR)
rr=rr*45000
gb=.true.
call sort_quick_rx(rr,ii)
do i=1,isz-1
   if(rr(ii(i)).gt.rr(ii(i+1)))then
      call unit_check_bad('sort_quit_rx',msg='Error in sorting reals from small to large')
      gb=.false.
   endif
enddo
if(gb)call unit_check_good('sort_quick_rx',msg='sort real array')

end subroutine test_sort_quick_rx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_unique
use M_debug, only : unit_check_start, unit_check, unit_check_bad, unit_check_good, unit_check_done,msg
implicit none
integer,allocatable :: ints(:)
integer             :: ic
call unit_check_start('unique', '-library libGPF') ! start tests

ints=[1,1,2,3,4,4,10,20,20,30]
call unique(ints,ic)
call unit_check('unique',ic.eq.7.and.all(ints(:ic).eq.[1, 2, 3, 4, 10, 20, 30]),msg=msg('expect 7 ic=',ic, 'ints=',msg(ints(:ic))))

ints=[integer ::]
call unique(ints,ic)
call unit_check('unique',ic.eq.0 .and. all(ints.eq.[integer::]),msg='check empty array ')

ints=[10]
call unique(ints,ic)
call unit_check('unique',ic.eq.1 .and. all(ints(:ic).eq.[10]),msg='check array of one element')

ints=[10,10,10,10]
call unique(ints,ic)
call unit_check('unique',ic.eq.1 .and. all(ints(:ic).eq.[10,10,10,10]),msg='all duplicates')

ints=[10,20,30,40]
call unique(ints,ic)
call unit_check('unique',ic.eq.4 .and. all(ints(:ic).eq.[10, 20, 30, 40]),msg='no change required')

call unit_check_done('unique',msg='test of unique(3f) completed') ! assume if got here passed checks
end subroutine test_unique
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_swap
use M_debug,   only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
implicit none
integer             :: iarray2(2)=[20,10],iarray(2)=[10,20]
real                :: rarray2(2)=[22.22,11.11],rarray(2)=[11.11,22.22]
doubleprecision     :: darray2(2)=[9876.54321d0,1234.56789d0],darray(2)=[1234.56789d0,9876.54321d0]
complex             :: carray2(2)=[(9876,54321),(1234,56789)],carray(2)=[(1234,56789),(9876,54321)]
logical             :: larray2(2)=[.false.,.true.],larray(2)=[.true.,.false.]
character(len=16)   :: string2(2)=["The other string","First string    "],string(2)=["First string    ", "The other string"]

   call unit_check_start('swap',' -library libGPF') ! start tests
   call swap (iarray(1), iarray(2)); call unit_check('swap',all(iarray.eq.iarray2),'integer test')
   call swap (rarray(1), rarray(2)); call unit_check('swap',all(rarray.eq.rarray2),'real test')
   call swap (darray(1), darray(2)); call unit_check('swap',all(darray.eq.darray2),'double test')
   call swap (carray(1), carray(2)); call unit_check('swap',all(carray.eq.carray2),'complex test')
   call swap (larray(1), larray(2)); call unit_check('swap',all(larray.eqv.larray2),'logical test')
   call swap (string(1), string(2)); call unit_check('swap',all(string.eq.string2),'string test')
   call unit_check_done('swap')

end subroutine test_swap
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_sort
!===================================================================================================================================
