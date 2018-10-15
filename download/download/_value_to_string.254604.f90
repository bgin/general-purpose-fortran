!===================================================================================================================================
PROGRAM DEMO_value_to_string
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: value_to_string
implicit none
CHARACTER(LEN=80) :: STRING
doubleprecision   :: DVALUE
real              :: RVALUE
integer           :: IVALUE
integer           :: ILEN
integer           :: IERR
integer           :: IERRSUM=0
!===================================================================================================================================
   call unit_check_start('value_to_string',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   DVALUE=5.5555555555555555555555d0
   call value_to_string(DVALUE,STRING,ILEN,IERR)
   write(*,*)'value_to_string: DOUBLE TEST VALUE=',dvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   IERRSUM=IERRSUM+IERR
   if(ILEN.le.0)IERRSUM=IERRSUM+1000

   RVALUE=3.3333333333333333333333
   call value_to_string(RVALUE,STRING,ILEN,IERR)
   write(*,*)'value_to_string: REAL TEST VALUE=',rvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   IERRSUM=IERRSUM+IERR
   if(ILEN.le.0)IERRSUM=IERRSUM+10000

   IVALUE=1234567890
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   write(*,*)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   IERRSUM=IERRSUM+IERR
   if(string.ne.'1234567890')then
       IERRSUM=IERRSUM+100000
   endif
   if(ILEN.ne.10)then
       IERRSUM=IERRSUM+1000000
   endif

   IVALUE=0
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   write(*,*)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr

   IVALUE=-12345
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   write(*,*)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   if(string.ne.'-12345')then
       IERRSUM=IERRSUM+1000000
   endif
   if(ILEN.ne.6)then
       IERRSUM=IERRSUM+10000000
   endif
!===================================================================================================================================
   write(*,'(80("="))')
   if(ierrsum.ne.0)then
      call unit_check_bad('value_to_string')
      stop
   endif
   write(*,'(80("="))')
!===================================================================================================================================
   call unit_check_good('value_to_string')
!===================================================================================================================================
END PROGRAM DEMO_value_to_string
!===================================================================================================================================
