module m_treesort  ! @(#) simple tree sort of integers as they are added to the sort
   TYPE NODE
      INTEGER :: VALUE
      TYPE (NODE), POINTER :: LEFT, RIGHT
   END TYPE NODE

public test_suite_M_treesort

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
RECURSIVE SUBROUTINE INSERT (T, NUMBER)
IMPLICIT NONE
TYPE (NODE), POINTER :: T  ! A tree
INTEGER, INTENT (IN) :: NUMBER

   ! If (sub)tree is empty, put number at root
   IF (.NOT. ASSOCIATED (T)) THEN
      ALLOCATE (T)
      T % VALUE = NUMBER
      NULLIFY (T % LEFT)
      NULLIFY (T % RIGHT)
      ! Otherwise, insert into correct subtree
   ELSE IF (NUMBER < T % VALUE) THEN
      CALL INSERT (T % LEFT, NUMBER)
   ELSE
      CALL INSERT (T % RIGHT, NUMBER)
   ENDIF

END SUBROUTINE INSERT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
RECURSIVE SUBROUTINE PRINT_TREE (T)
! Print tree in infix order
 
IMPLICIT NONE
TYPE (NODE), POINTER :: T  ! A tree
 
   IF (ASSOCIATED (T)) THEN
      CALL PRINT_TREE (T % LEFT)
      PRINT *, T % VALUE
      CALL PRINT_TREE (T % RIGHT)
   ENDIF
 
END SUBROUTINE PRINT_TREE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_treesort()

!! setup
   call test_insert()
   call test_print_tree()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_insert()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('insert',msg='')
   !!call unit_check('insert', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('insert',msg='')
end subroutine test_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_tree()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('print_tree',msg='')
   !!call unit_check('print_tree', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('print_tree',msg='')
end subroutine test_print_tree
!===================================================================================================================================
end subroutine test_suite_M_treesort
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module m_treesort
