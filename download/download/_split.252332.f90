PROGRAM demo_split
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
   USE M_strings, ONLY: split
   INTRINSIC SIZE
   CHARACTER(LEN=:),ALLOCATABLE    :: line
   CHARACTER(LEN=:),ALLOCATABLE    :: order
   CHARACTER(LEN=:),ALLOCATABLE    :: dlm
!! CHARACTER(LEN=:),ALLOCATABLE    :: array(:)
   CHARACTER(LEN=256),ALLOCATABLE  :: array(:)
   character(len=10)               :: orders(3)=['sequential', '          ', 'reverse   ' ]
   ! return strings composed of delimiters or not IGNORE|RETURN|IGNOREEND
   character(len=10)               :: nulls(3)=['ignore    ', 'return    ', 'ignoreend ' ]
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=''
   LINE='abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   order=orders(3)
   CALL testit()
   CALL split(line,array,dlm,order,nulls(1))
   write(*,*)size(array)
   order=orders(2)
   CALL split(line,array,dlm,order,nulls(2))
   write(*,*)size(array)
   order=orders(1)
   CALL split(line,array,dlm,order,nulls(3))
   write(*,*)size(array)
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE=' abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE='        abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE=' aABCDEF  ; b;;c d e;  ;  '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=';'
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm='; '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=';'
   LINE=';;;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;;'
   CALL testit()
   LINE=';;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE=';abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE='abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc'
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   line='a b c d e f g h i j k l m n o p q r s t u v w x y z'
   CALL split(line,array)
   if(size(array).ne.26)then
      call unit_check_bad('split')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=' '
   CALL split(line,array,dlm)
   if(size(array).ne.26)then
      call unit_check_bad('split')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_good('split')
!-----------------------------------------------------------------------------------------------------------------------------------
   CONTAINS
!-----------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE testit()
         WRITE(*,'(80("="))')
         WRITE(*,'(A)')'parsing ['//TRIM(line)//']'//'with delimiters set to ['//dlm//'] and order '//trim(order)//''
         CALL split(line,array,dlm,order)
         WRITE(*,'("number of tokens found=",i0)')SIZE(array)
         WRITE(*,'(I0,T10,A)')(i,TRIM(array(i)),i=1,SIZE(array))
      END SUBROUTINE testit
!-----------------------------------------------------------------------------------------------------------------------------------
END PROGRAM demo_split
!-----------------------------------------------------------------------------------------------------------------------------------
