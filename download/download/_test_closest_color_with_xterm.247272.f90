program test_closest_color_with_xterm
use m_color, only : hue, closest_color_name, color_name2rgb
use m_debug, only : unit_check
implicit none
character(len=20) :: closestname
character(len=20) :: rgbname
real              :: r,g,b
integer           :: i,j,k
integer           :: ios
   do i=0,100,25
      do j=0,100,25
         do k=0,100,25
            r=i; g=j; b=k
            call closest_color_name(r,g,b,closestname)
            write(rgbname,'("rgb:",Z2.2,"/",Z2.2,"/",Z2.2)')int([r,g,b]*2.55)
            ! change the background color on an xterm(1) terminal emulator
            write(*,'(a)',advance='no')char(27)//']11;'//trim(closestname)//char(7)
            write(*,'(a)',advance='no')char(27)//']10;'//trim(rgbname)//char(7)
            write(*,'(a)')'########################################################################################################'
            write(*,*)'COMPARE ',closestname, rgbname
            read(*,*,iostat=ios)
         enddo
      enddo
   enddo
end program test_closest_color_with_xterm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
