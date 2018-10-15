!===================================================================================================================================
PROGRAM TREE_SORT
!>
!!
!!   Copyright (c) 1990 by Walter S. Brainerd, Charles H. Goldberg,
!!   and Jeanne C. Adams.  This program may be copied and used without
!!   restriction as long as this notice is retained.
!!
!!   Sorts a file of integers by building a tree, sorted in infix order.
!!   This sort has expected behavior n log n,
!!   but worst case (input is sorted) n ** 2.
!===================================================================================================================================
!===================================================================================================================================
   USE M_TREESORT, ONLY : NODE, INSERT, PRINT_TREE
   IMPLICIT NONE
   TYPE(NODE), POINTER :: T                           ! A tree
   INTEGER             :: NUMBER
   INTEGER             :: IOS
   NULLIFY(T)                                         ! Start with empty tree
   INFINITE: DO
      READ (*,*,IOSTAT=IOS) NUMBER
      IF(IOS.NE.0)EXIT INFINITE
      CALL INSERT(T,NUMBER)                           ! Put next number in tree
   ENDDO INFINITE
   CALL PRINT_TREE(T)                                 ! Print nodes of tree in infix order
END PROGRAM TREE_SORT
!===================================================================================================================================
