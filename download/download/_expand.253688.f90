!===================================================================================================================================
program testit                                   !  test filter to expand escape sequences in input lines
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only : expand
character(len=80) :: line
   call unit_check_start('expand',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      &  -ufpp         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')
   write(*,*)' test expand(): CLEAR SCREEN ',expand('\e\d0912J').eq.char(27)//'[2J'
   call unit_check('expand',expand('\e\d0912J').eq.char(27)//'[2J')

   write(*,*)' test expand(): PLAIN TEXT'
   call unit_check('expand',expand('this is a test').eq.'this is a test')

   write(*,*)'check all decimal values'
   do i=0,127
       !write(*,*)'value ',i
       write(line,'("%d",i3.3)')i
       !write(*,*)trim(line)
       call unit_check('expand',expand(line,'%').eq.char(i))
       write(line,'("%o",o3.3)')i
       !write(*,*)trim(line)
       call unit_check('expand',expand(line,'%').eq.char(i))
       write(line,'("%x",z2.2)')i
       !write(*,*)trim(line)
       call unit_check('expand',expand(line,'%').eq.char(i))
   enddo

   write(*,*)char(8)//char(27)//char(13)//char(11)//char(7)//char(9)
   write(*,*)expand('%d008%d027%d013%d011%d007%d009','%')
   write(*,*)expand('%b%e%r%v%a%t','%')

   write(*,*)'Test decimal escape characters'
   call unit_check('expand',expand('%d008%d027%d013%d011%d007%d009','%').eq.char(8)//char(27)//char(13)//char(11)//char(7)//char(9))

   write(*,*)'Test escape characters'
   call unit_check('expand',expand('%b%e%r%v%a%t','%').eq.char(8)//char(27)//char(13)//char(11)//char(7)//char(9))

   write(*,*)'Test of expand() passed'
   call unit_check_good('expand')

end program testit
!===================================================================================================================================
