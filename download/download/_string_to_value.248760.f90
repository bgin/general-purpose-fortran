!===================================================================================================================================
PROGRAM DEMOstring_to_value
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: string_to_value, s2v, v2s
CHARACTER(len=80) :: STRING
real RVALUE
doubleprecision DVALUE
doubleprecision SUM, SUM2, DELTA
integer IVALUE
integer GOOD
!===================================================================================================================================
   call unit_check_start('string_to_value',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
!===================================================================================================================================
   write(*,'(80("="))')
   STRING=' -40.5e-2 '
   CALL string_to_value(STRING,RVALUE,IERR)
   CALL string_to_value(STRING,DVALUE,IERR)
   CALL string_to_value(STRING,IVALUE,IERR)
   WRITE(*,*) 'string_to_value: real value is ',-40.5e-2
   WRITE(*,*) 'string_to_value: double value is ',-40.5d-2
   WRITE(*,*) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
   WRITE(*,*) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
   WRITE(*,*) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   STRING=' -40.5d-2 '
   CALL string_to_value(STRING,RVALUE,IERR)
   CALL string_to_value(STRING,DVALUE,IERR)
   CALL string_to_value(STRING,IVALUE,IERR)
   WRITE(*,*) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
   WRITE(*,*) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
   WRITE(*,*) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   good=0
   if(rvalue.eq.-40.5e-2)then
      good=good*10+1
      write(*,*)'string_to_value: good ',good
   endif
   if(dvalue.eq.-40.5d-2)then
      good=good*10+1
      write(*,*)'string_to_value: good ',good
   endif
   if(dvalue-spacing(dvalue).le.-40.5d-2.and.dvalue+spacing(dvalue).ge.-40.5d-2)then
      good=good*10+1
      write(*,*)'string_to_value: good ',good
   else
      call unit_check_bad('string_to_value')
      stop 1
   endif
   if(rvalue-spacing(rvalue).le.-40.5e-2.and.rvalue+spacing(rvalue).ge.-40.5e-2)then
      good=good*10+1
      write(*,*)'string_to_value: good ',good
   else
      call unit_check_bad('string_to_value')
      stop 2
   endif
!===================================================================================================================================
   write(*,'(80("="))')
   SUM=0.0d0
   string='5.555555555555555555555555555555555'
   CALL string_to_value(STRING,RVALUE,IERR)
   SUM=SUM+RVALUE
   CALL string_to_value(STRING,DVALUE,IERR)
   SUM=SUM+DVALUE
   CALL string_to_value(STRING,IVALUE,IERR)
   SUM=SUM+IVALUE
!===================================================================================================================================
   WRITE(*,*) 'string_to_value: real value is ', 5.555555555555555555555555555555555e0
   WRITE(*,*) 'string_to_value: double value is ', 5.555555555555555555555555555555555d0
   WRITE(*,*) 'string_to_value: value of string ['//trim(STRING)//'] is ',RVALUE
   WRITE(*,*) 'string_to_value: value of string ['//trim(STRING)//'] is ',DVALUE
   WRITE(*,*) 'string_to_value: value of string ['//trim(STRING)//'] is ',IVALUE
   WRITE(*,*) 'string_to_value: SUM=', SUM
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   WRITE(*,*) 'string_to_value: SUM2=', SUM2
   DELTA=spacing(0.0d0)+spacing(0.0)
   WRITE(*,*) 'string_to_value: DELTA=', DELTA
   if(sum.eq.sum2)then
      good=good*10+1
      write(*,*)'string_to_value: good ',good
   else
      call unit_check_bad('string_to_value')
   endif
   if(sum+delta.ge.sum2.and.sum-delta.le.sum2)then
      good=good*10+1
      write(*,*)'string_to_value: good ',good
   else
      call unit_check_bad('string_to_value')
   endif
   write(*,'(80("="))')
!===================================================================================================================================
      call unit_check_good('string_to_value')
!===================================================================================================================================
! s2v TEST
!===================================================================================================================================
   call unit_check_start('s2v',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   SUM=s2v('5.55555555555555555555555555e0')+REAL(s2v('5.55555555555555555555555555d0'))+INT(s2v('5.55555555555555555555555555'))
   if(sum.eq.sum2)then
      good=good*10+1
      write(*,*)'s2v: good ',good
   else
      WRITE(*,*) 's2v: SUM2=', SUM2
      WRITE(*,*) 's2v: SUM=', SUM
      call unit_check_bad('s2v')
   endif
   if(sum+delta.ge.sum2.and.sum-delta.le.sum2)then
      good=good*10+1
      write(*,*)'s2v: good ',good
   else
      call unit_check_bad('s2v')
      WRITE(*,*) 's2v: SUM2=', SUM2
      WRITE(*,*) 's2v: SUM=', SUM
      stop 6
   endif
   write(*,'(80("="))')
   call unit_check_good('s2v')
!===================================================================================================================================
! v2s TEST
!===================================================================================================================================
   call unit_check_start('v2s',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   SUM=0
   SUM=s2v(v2s(5.55555555555555555555555555d0))
   SUM=SUM+REAL(s2v(v2s(5.55555555555555555555555555e0)))
   SUM=SUM+INT(s2v(v2s(5)))
   if(sum.eq.sum2)then
      good=good*10+1
      write(*,*)'v2s: good ',good
   else
      call unit_check_bad('v2s')
      WRITE(*,*) 'v2s: SUM2=', SUM2
      WRITE(*,*) 'v2s: SUM=', SUM
      stop 5
   endif
   if(sum+delta.ge.sum2.and.sum-delta.le.sum2)then
      good=good*10+1
      write(*,*)'v2s: good ',good
   else
      call unit_check_bad('v2s')
      WRITE(*,*) 'v2s: SUM2=', SUM2
      WRITE(*,*) 'v2s: SUM=', SUM
      stop 6
   endif
   write(*,'(80("="))')
   call unit_check_good('v2s')
!===================================================================================================================================
END PROGRAM DEMOstring_to_value
!===================================================================================================================================
