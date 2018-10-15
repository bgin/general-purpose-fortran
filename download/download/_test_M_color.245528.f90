!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program test_M_color
use m_color, only : hue, closest_color_name, color_name2rgb
use m_debug, only : unit_check, unit_check_good
implicit none
real              :: c, m, y
real              :: r, g, b
real              ::    i, q
integer           :: status

   write(*,*)'rgb <==> hls'
   !                NAME      RGB(0-255)    HLS(0-100)
   call check_name('hls','red',     [100,  0,  0],[  0, 50,100])
   call check_name('hls','orange',  [100, 65,  0],[ 39, 50,100])
   call check_name('hls','yellow',  [100,100,  0],[ 60, 50,100])
   call check_name('hls','green',   [  0,100,  0],[120, 50,100])
   call check_name('hls','cyan',    [  0,100,100],[180, 50,100])
   call check_name('hls','blue',    [  0,  0,100],[240, 50,100])
   call check_name('hls','magenta', [100,  0,100],[300, 50,100])
   call check_name('hls','black',   [  0,  0,  0],[  0,  0,  0])
   call check_name('hls','white',   [100,100,100],[  0,100,  0])

   write(*,*)'rgb <==> cmy'
   !-----------------------------------------------
   ! Color   (C,M,Y)       (  R,  G,  B)  Hex
   ! Black   (100,100,100) (  0,  0,  0)  #000000
   ! White   (  0,  0,  0) (100,100,100)  #FFFFFF
   ! Red     (  0,100,100) (100,  0,  0)  #FF0000
   ! Green   (100,  0,100) (  0,100,  0)  #00FF00
   ! Blue    (100,100,  0) (  0,  0,100)  #0000FF
   ! Yellow  (  0,  0,100) (100,100,  0)  #FFFF00
   ! Cyan    (100,  0,  0) (  0,100,100)  #00FFFF
   ! Magenta (  0,100,  0) (100,  0,100)  #FF00FF
   !-----------------------------------------------
   call hue('cmy',100.0,  0.0,  0.0,'rgb',r,g,b,status); call unit_check('hue',all([r.eq.  0.0,g.eq.100.0,b.eq.100.0 ]),'cyan')
   call hue('cmy',  0.0,100.0,  0.0,'rgb',r,g,b,status); call unit_check('hue',all([r.eq.100.0,g.eq.  0.0,b.eq.100.0 ]),'magenta')
   call hue('cmy',  0.0,  0.0,100.0,'rgb',r,g,b,status); call unit_check('hue',all([r.eq.100.0,g.eq.100.0,b.eq.  0.0 ]),'yellow')

   call hue('rgb',100.0,  0.0,100.0,'cmy',c,m,y,status); call unit_check('hue',all([c.eq.  0.0,m.eq.100.0,y.eq.  0.0 ]),'magenta')
   call hue('rgb',  0.0,100.0,100.0,'cmy',c,m,y,status); call unit_check('hue',all([c.eq.100.0,m.eq.  0.0,y.eq.  0.0 ]),'cyan')
   call hue('rgb',100.0,100.0,  0.0,'cmy',c,m,y,status); call unit_check('hue',all([c.eq.  0.0,m.eq.  0.0,y.eq.100.0 ]),'yellow')

   write(*,*)'rgb <==> hsv'

   call check_name('hsv','black',    [0,0,0],       [0,0,0])
   call check_name('hsv','gray50',   [50,50,50],    [0,0,50])
   call check_name('hsv','silver',   [75,75,75],    [0,0,75])
   call check_name('hsv','white',    [100,100,100], [0,0,100])
   call check_name('hsv','red4',     [55,0,0],      [0,100,55])
   call check_name('hsv','red',      [100,0,0],     [0,100,100])
   call check_name('hsv','olive',    [50,50,0],     [60,100,50])
   call check_name('hsv','yellow',   [100,100,0],   [60,100,100])
   call check_name('hsv','green',    [0,100,0],     [120,100,100])
!  call check_name('hsv','lime',     [0,100,0],     [120,100,100])
   call check_name('hsv','teal',     [0,50,50],     [180,100,50])
   call check_name('hsv','cyan',     [0,100,100],   [180,100,100])
   call check_name('hsv','navy',     [0,0,50],      [240,100,50])
   call check_name('hsv','blue',     [0,0,100],     [240,100,100])
   call check_name('hsv','purple',   [63,13,94],    [277,87,94])
   call check_name('hsv','magenta4', [55,0,55],     [300,100,55])
   call check_name('hsv','magenta',  [100,0,100],   [300,100,100])
   call check_name('hsv','maroon',   [69,19,38],    [338,73,69])
!---------------------------------------------------

   write(*,*)'rgb <==> yiq'
   call hue('rgb',100.0,  0.0,  0.0,'yiq',y,i,q,status) ! rgb 100   0 100 yiq 41.29853  27.43087  52.29881
   write(*,*)y,i,q
   !call unit_check('hue',all([y.eq.  0.0,i.eq.100.0,q.eq.100.0 ]))

   call hue('rgb',  0.0,100.0,  0.0,'yiq',y,i,q,status) ! rgb   0 100   0 yiq 58.70147 -27.43087 -52.29881
   write(*,*)y,i,q
   !call unit_check('hue',all([y.eq.  0.0,i.eq.100.0,q.eq.100.0 ]))

   call hue('yiq',  0.0,100.0,  0.0,'rgb',y,i,q,status) ! yiq 41.29853  27.43087  52.29881 rgb 100   0 100
   write(*,*)r,g,b
   !call unit_check('hue',all([r.eq.  0.0,g.eq.100.0,b.eq.100.0 ]))

   call hue('yiq',  0.0,100.0,  0.0,'rgb',y,i,q,status) ! yiq 58.70147 -27.43087 -52.29881 rgb   0 100   0
   write(*,*)r,g,b
   !call unit_check('hue',all([r.eq.  0.0,g.eq.100.0,b.eq.100.0 ]))

   write(*,*)'approve routines'
   call unit_check_good('hue'            )
   call unit_check_good('color_name2rgb' )
   call unit_check_good('closest_color'  )

   contains
subroutine check_name(modelout,name,rgb,other)
! given a colorname look up RGB values, compare to expected values, convert to MODELOUT, check
implicit none
character(len=*)   :: name
integer,intent(in) :: rgb(3), other(3)
real               :: r,g,b
real               :: r1,g1,b1
real               :: r2,g2,b2
real               :: val1,val2,val3
character(len=*)   :: modelout
character(len=20)  :: echoname
character(len=20)  :: closestname
integer            :: status
   write(*,*)repeat('=',80)
   write(*,*)'NAME ============>',trim(name)
   ! given a color name look up RGB values in range 0-100
   call color_name2rgb(name,r,g,b,echoname)
   ! make sure echoed name does not equal unknown
   write(*,*)'ECHOED NAME =====>',trim(echoname)
   call unit_check('color_name2rgb',echoname.ne.'Unknown')
   ! check the RGB values against expected values.
   write(*,*)'EXPECTED RGB ====>',rgb
   write(*,*)'RETURNED RGB ====>',int([r+0.5,g+0.5,b+0.5])
   call unit_check('color_name2rgb', all(int([r+0.5,g+0.5,b+0.5]) .eq. rgb ))
   ! see if get name back
   call closest_color_name(r,g,b,closestname)
   write(*,*)'CLOSEST NAME ====>',closestname
   if(closestname.eq.name)then
      call unit_check('color_name2rgb',closestname.eq.name)
   else
      ! did not get back name put in; but maybe alternate alias
      ! if values the same assume OK
      call color_name2rgb(closestname,r1,g1,b1,echoname)
      call color_name2rgb(name,r2,g2,b2,echoname)
      call unit_check('color_name2rgb', &
      & int(r1+0.5) .eq.  int(r2+0.5)   &
      & .and.                           &
      & int(g1+0.5) .eq.  int(g2+0.5)   &
      & .and.                           &
      & int(b1+0.5) .eq.  int(b2+0.5)   &
      &)
      write(*,*)'NAMES HAVE (ALMOST) SAME VALUES SO OK ',name, closestname
   endif
   ! convert RGB values to MODELOUT values
   call hue('rgb',r,g,b,modelout,val1,val2,val3,status)
   write(*,*)'EXPECTED '//modelout//' ====>',other
   write(*,*)'RETURNED '//modelout//' ====>',int([val1+0.5,val2+0.5,val3+0.5])
   call unit_check('color_name2rgb', all(int([val1+0.5,val2+0.5,val3+0.5]) .eq. other ))
   write(*,*)'STATUS ==========>',status
   call unit_check('color_name2rgb', status .eq. 0 )
end subroutine check_name
end program test_M_color
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
