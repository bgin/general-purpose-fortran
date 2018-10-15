!@(#) basic test program for a driver. 
!  if we can draw a line and do hardware text we are almost there!
!
        program ftrivial

      use M_vogle
      use ISO_C_BINDING

        integer(kind=c_int),parameter :: BLACK = 0, GREEN = 2
        character(len=50) ::  device

!
!  read in device name
!
        print*,'Enter output device:'
        read(*,'(a)')device

        call vinit(device)
!
!  set font to hardware text large 
!
        call font('large')              
!
!  set current color to black 
!
        call color(BLACK)
!
!  clear to current color
!
        call clear
!
!  we want to draw in green 
!
        call color(GREEN)
!
!  draw a horizontal line at y = 0 
!
        call move2(-1.0, 0.0)
        call draw2(1.0, 0.0)
!
!  pause for some input 
!
        idum=getkey()
!
!  draw a line along x = 0 
!
        call move2(0.0, 0.0)
        call draw2(0.0, 1.0)
!
!  move to the middle of the screen 
!
        call move2(0.0, 0.0)
!
!  draw 'Hello' starting at the origin 
!
        call drawstr('Hello')
!
!  pause again 
!
        idum=getkey()
!
!  set screen back to original state 
!
        call vexit
        end
