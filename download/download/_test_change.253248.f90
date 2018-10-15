program testit
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only : change
!character(len=132) :: direc
character(len=132) :: line=' The rain in Spain falls mainly on the plain. '
   write(*,*)' LINE='//trim(line)
   ! indicate test of change(3f) has begun
   call unit_check_start('change',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   call change(line, 'c/ain/AIN'     ,ier);write(*,*)'IER=',ier;call unit_check('change',ier.eq.4,'without trailing slash')
   call change(line, 'c/ The r/R/'   ,ier);write(*,*)'IER=',ier;call unit_check('change',ier.eq.1,'with trailing slash')
   call change(line, 'c/ /'          ,ier);write(*,*)'IER=',ier;call unit_check('change',ier.ge.7,'no new string') ! remove spaces
   call change(line, 'c//PREFIX:'    ,ier);write(*,*)'IER=',ier;call unit_check('change',ier.eq.1,'null new string')
   call change(line, 'c/XYZ/xxxxxx:' ,ier);write(*,*)'IER=',ier;call unit_check('change',ier.eq.0,'no matching old string')
   write(*,*)'IER=',ier,' LINE='//trim(line)
   call unit_check('change','PREFIX:RAINinSpAINfallsmAINlyontheplAIN.' .eq. line,'check cumulative changes')
   !do
   !   read(*,'(a)')direc
   !   call change(line,direc,ier)
   !   write(*,*)'IER=',ier,' LINE='//trim(line)
   !enddo
   call unit_check_good('change') ! indicate test of change(3f) passed
end program testit
