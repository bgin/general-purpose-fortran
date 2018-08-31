!>
!!##NAME
!!    M_DRAWPLUS(3f) - [M_DRAWPLUS] Additional routines using the M_DRAW graphics library
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!
!!    M_DRAWPLUS(3f) is a collection of higher level graphic routines that call the
!!    base M_DRAW(3f) graphics routines.
!!
!!##LIBRARY FUNCTION DESCRIPTIONS
!!
!!    DEVICE ROUTINES
!!
!!    ROUTINES FOR SETTING UP WINDOWS
!!
!!    CLIPPING ROUTINES
!!
!!    COLOR ROUTINES
!!
!!    INTERACTIVE ROUTINES
!!
!!    FLUSHING
!!
!!    PROJECTION AND VIEWPORT ROUTINES
!!
!!        subroutine biggest_ortho2 (left, right, bottom, top)
!!
!!    MATRIX STACK ROUTINES
!!
!!        subroutine pop
!!
!!        subroutine push
!!
!!    VIEWPOINT ROUTINES
!!
!!    MOVE ROUTINES
!!
!!    LINESTYLE ROUTINES
!!
!!    DRAW ROUTINES
!!
!!        subroutine polyline2(arrx(:),arry(:))
!!
!!        subroutine arrowhead(x1,y1,x2,y2,size,idraw)
!!
!!    ARCS AND CIRCLES
!!
!!        subroutine uarc
!!
!!    CURVE ROUTINES
!!
!!        subroutine uconic(x,y,p,e,theta1,theta2,orientation)
!!
!!        subroutine spirograph(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!!
!!        subroutine smoot, ismoo,ismoo1,ismoo2,ismoo3,perin
!!
!!    RECTANGLES AND GENERAL POLYGON ROUTINES
!!
!!    TEXT ROUTINES
!!
!!    TRANSFORMATIONS ROUTINES
!!
!!    PATCH ROUTINES
!!
!!    POINT ROUTINES
!!
!!    OBJECT ROUTINES
!!
!!        subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
!!
!!    DOUBLE BUFFERING
!!
!!    POSITION ROUTINES
!!
!!    HIGH LEVEL ROUTINES
!!
!!        subroutine barcode
!!
!!        subroutine call_draw
!!
!!        subroutine draw_interpret
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
module M_drawplus
use ISO_C_BINDING
use M_draw
use M_journal, only : journal

implicit none
private

public  :: page
public  :: biggest_ortho2
public  :: invokeobj
public  :: spirograph
public  :: call_draw
public  :: draw_interpret
public  :: arrowhead

public  :: barcode
public  :: pop
public  :: push
public  :: uconic
public  :: uarc
public  :: polyline2
integer :: ismoo,ismoo1,ismoo2,ismoo3
real    :: perin
public  :: smoot, ismoo,ismoo1,ismoo2,ismoo3,perin

private :: arc2

interface polyline2
   module procedure :: polyline2_i, polyline2_r
end interface polyline2

interface page
   module procedure :: biggest_ortho2
   module procedure :: page_rri
end interface page

!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    arrowhead(3f) - [M_drawplus] Draw arrow head (for text boxes and line markers)
!!
!!##SYNOPSIS
!!
!!   subroutine arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
!!
!!    real,intent(in)    :: xpoint,ypoint
!!    real,intent(in)    :: xstart,ystart
!!    real,intent(in)    :: size
!!    integer,intent(in) :: idraw
!!
!!##DESCRIPTION
!!    given line segment
!!
!!      START --> POINT
!!
!!    draw an arrow head of overall length SIZE measured along the line segment.
!!    The arrow head is 2/3 SIZE wide and the indent is 1/3 SIZE.
!!
!!    if IDRAW is 0, draw line from x3 to START too X3 and leave current
!!    position at POINT.
!!
!!                   o START
!!                   |
!!                   |
!!                   |
!!                   |
!!                   |  1/3*size
!!                   |<------->
!!                   |
!!                   |
!!      P1  o .      |      . o P2   ---------
!!           \  .    |   .   /         ^
!!            \   .  | .    /          |
!!             \     o P3  / -------   |
!!              \         /     ^     SIZE
!!               \       /      |      |
!!                \     /    2/3*SIZE  |
!!                 \   /        V      V
!!                  \ /      -----------------
!!                   o POINT
!===================================================================================================================================
subroutine arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
use M_draw
implicit none

character(len=*),parameter::ident="@(#)M_drawplus::arrowhead(3f): Draw arrow head (for text boxes and line markers)"

real,intent(in)    :: xpoint,ypoint
real,intent(in)    :: xstart,ystart
real,intent(in)    :: size
integer,intent(in) :: idraw

   real            :: xdel,ydel
   real            :: hyp,hyp2,hyp3
   real            :: ang
   real            :: adder
   real            :: x2,y2,x3,y3,x4,y4

   xdel=xpoint-xstart
   ydel=ypoint-ystart
   hyp=sqrt(xdel**2+ydel**2)                 ! length from START to POINT
   hyp2=hyp-size*2.0/3.0                     ! length from START to P3
   hyp3=sqrt((hyp-size)**2+(size/3.0)**2)    ! length from START to P1

   ang=atan2(ydel,xdel)                      ! angle of line START-->POINT
   adder=atan2(size/3.0,hyp-size)            ! angle P1-->START-->P3

   x2=xstart+hyp3*cos(ang+adder)             ! calculate P2 from START
   y2=ystart+hyp3*sin(ang+adder)

   x3=xstart+hyp2*cos(ang)                   ! calculate P3 from START
   y3=ystart+hyp2*sin(ang)

   x4=xstart+hyp3*cos(ang-adder)             ! calculate P4 from START
   y4=ystart+hyp3*sin(ang-adder)

   call pushattributes()
      call polyfill(.true.)                  ! fill arrowhead
      call makepoly()
         call move2(xpoint,ypoint)
         call draw2(x2,y2)
         call draw2(x3,y3)
         call draw2(x4,y4)
         call draw2(xpoint,ypoint)
      call closepoly()
      call polyfill(.false.)                 ! ASSUME ORIGINALLY OFF!
   call popattributes()

   if(idraw.eq.0)then      ! draw line from back of arrowhead (P3) to START
      call move2(x3,y3)
      call draw2(xstart,ystart)
      call move2(xpoint,ypoint)
   endif
end subroutine arrowhead
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       page_rri(3fp) - [M_drawplus] - new page
!!
!!##SYNOPSIS
!!
!!   subroutine polyline2(xsize,ysize,icolor)
!!
!!    integer,intent(in)          :: xsize
!!    integer,intent(in)          :: ysize
!!    integer,intent(in)          :: icolor
!!
!!##DESCRIPTION
!!    Given a horizontal size and vertical size and color set to window
!!    to the rectangle defined by the corner points <0.0,0.0> and <xsize,ysize>
!!    and set the viewport to the largest viewport on the display with the same
!!    aspect ratio and start a new page with the specified background color if
!!    background color is supported on the device.
!!
!!##OPTIONS
!!    XSIZE    X size of requested window
!!    YSIZE    Y size of requested window
!!    ICOLOR   Color to set background.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_page_rri
!!      use M_draw
!!      use M_drawplus, only : polyline2, page
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(300,300)
!!      call vinit(' ')
!!      call page(8.5,11.0,3)
!!      call color(2)
!!      call linewidth(100)
!!      call circle(8.5/0.0,11.0/0.0,8.5/2.0)
!!      call color(1)
!!      call polyline2([0.0,0,0,8.5,11.0]
!!      call polyline2([8.5,0,0,0.0,11.0]
!!      ipaws=getkey()
!!      call vexit()
!!      end
!!      program demo_page_rri
!===================================================================================================================================
subroutine page_rri(xsize,ysize,icolor)
real,intent(in)             :: xsize
real,intent(in)             :: ysize
integer                     :: icolor
   call biggest_ortho2(0.0,xsize,0.0,ysize)
   call pushattributes()
      call color(icolor)
      call clear()
   call popattributes()
end subroutine page_rri
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       polyline2(3f) - [M_drawplus] - draw an unclosed polyline in the XY plane
!!
!!##SYNOPSIS
!!
!!   subroutine polyline2(arrx,arry)
!!
!!    integer,intent(in)          :: arrx(:)
!!    integer,intent(in),optional :: arry(:)
!!
!!##DESCRIPTION
!!    Given either a single array composed of pairs <x(i),y(i)> of values
!!    defining points or an X and Y array move to first point and draw to
!!    remaining points using current line style.
!!
!!##OPTIONS
!!    ARRX    If ARRY is present, an array of X values
!!    ARRY    An optional array of Y values
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_polyline2
!!      use M_draw
!!      use M_drawplus, only : polyline2
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(300,300)
!!      call vinit(' ')
!!      call ortho2(-2.0,2.0,-2.0,2.0)
!!      call color(2)
!!      call linewidth(100)
!!      call polyline2([-0.5,-0.5, -0.5,+0.5, +0.5,+0.5, +0.5,-0.5])
!!      call color(4)
!!      call polyline2( [-1,-1,+1,+1,-1] , &  ! X values
!!                 & [-1,+1,+1,-1,-1] )    ! Y values
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_polyline2
!===================================================================================================================================
subroutine polyline2_i(arrx,arry)
integer,intent(in)          :: arrx(:)
integer,intent(in),optional :: arry(:)
integer                     :: i
integer                     :: isizex
integer                     :: isizey
integer                     :: ipairs
! assuming nice data in x,y pairs
if(present(arry))then    ! two arrays means X array and Y array
   isizex=size(arrx)
   isizey=size(arry)
   ipairs=min(isizex,isizey)
   if(ipairs.gt.0)then
      call move2(real(arrx(1)),real(arry(1)))
   endif
   do i=2,ipairs
      call draw2(real(arrx(i)),real(arry(i)))
   enddo
else                      ! one array means array is <x1,y1>, <x2,y2>, <x3,y3>, ...
   isizex=size(arrx)
   isizey=0
   ipairs=isizex/2
   if(ipairs.gt.0)then
      call move2(real(arrx(1)),real(arrx(2)))
   endif
   do i=3,ipairs*2,2
      call draw2(real(arrx(i)),real(arrx(i+1)))
   enddo
endif

end subroutine polyline2_i
!===================================================================================================================================
subroutine polyline2_r(arrx,arry)
real,intent(in)          :: arrx(:)
real,intent(in),optional :: arry(:)
integer                  :: i
integer                  :: isizex
integer                  :: isizey
integer                  :: ipairs
! assuming nice data in x,y pairs
if(present(arry))then    ! two arrays means X array and Y array
   isizex=size(arrx)
   isizey=size(arry)
   ipairs=min(isizex,isizey)
   if(ipairs.gt.0)then
      call move2(arrx(1),arry(1))
   endif
   do i=2,ipairs
      call draw2(arrx(i),arry(i))
   enddo
else                      ! one array means array is <x1,y1>, <x2,y2>, <x3,y3>, ...
   isizex=size(arrx)
   isizey=0
   ipairs=isizex/2
   if(ipairs.gt.0)then
      call move2(arrx(1),arrx(2))
   endif
   do i=3,ipairs*2,2
      call draw2(arrx(i),arrx(i+1))
   enddo
endif

end subroutine polyline2_r
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    invokeobj(3f) - [M_DRAWPLUS] invoke object with specified transformations
!!
!!##SYNOPSIS
!!
!!       subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
!!       real,intent(in)    ::  xt,yt,zt
!!       real,intent(in)    ::  xs,ys,zs
!!       real,intent(in)    ::  xr,yr,zr
!!       integer,intent(in) :: iobject
!!
!!##DESCRIPTION
!!    save and restore the coordinate system while invoking an object with
!!    specified translation, rotation, and scaling.
!!
!!##OPTIONS
!!    xt,yt,zt    linear transforms
!!    xs,ys,zs    scaling
!!    xr,yr,zr    rotation in degrees
!!    iobject     object to invoke
!===================================================================================================================================
subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
character(len=*),parameter::ident="@(#)invokeobj(3f) invoke object with specified transformation applied and then restored"

real,intent(in)    :: xt,yt,zt  ! linear transforms
real,intent(in)    :: xs,ys,zs  ! scaling
real,intent(in)    :: xr,yr,zr  ! rotation
integer,intent(in) :: iobject

   call pushmatrix()
   call translate(xt,yt,zt)
   call scale(xs,ys,zs)
   call rotate(xr,'x')
   call rotate(yr,'y')
   call rotate(zr,'z')
   call callobj(iobject)
   call popmatrix()

end subroutine invokeobj
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    pop(3f) - [M_DRAWPLUS] call popviewport(), popmatrix(), popattributes()
!!##SYNOPSIS
!!
!!    subroutine pop()
!!##DESCRIPTION
!!    call popviewport(), popmatrix(), popattributes()
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine pop()
character(len=*),parameter::ident="@(#)pop(3f): call popviewport(), popmatrix(), popattributes()"
   call popviewport()
   call popmatrix()
   call popattributes()
end subroutine pop
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    push(3f) - [M_DRAWPLUS] call pushviewport(), pushmatrix(), pushattributes()
!!##SYNOPSIS
!!
!!     subroutine push()
!!##DESCRIPTION
!!    call pushattributes(), pushmatrix(), pushviewport()
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine push()
character(len=*),parameter::ident="@(#)push(3f): call pushattributes(), pushmatrix(), pushviewport()"
   call pushattributes()
   call pushmatrix()
   call pushviewport()
end subroutine push
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!     uarc(3f) - [M_DRAWPLUS] create circular arc, leaving CP at end of arc
!!
!!##SYNOPSIS
!!
!!    subroutine uarc(x,y,angle)
!!
!!     real,intent(in) :: x
!!     real,intent(in) :: y
!!     real,intent(in) :: angle
!!
!!##DESCRIPTION
!!     given center point and angle in degrees, draw circular arc from
!!     current point with counterclockwise angles positive.
!===================================================================================================================================
subroutine uarc(x,y,angle)
!-----------------------------------------------------------------------------------------------------------------------------------
! given center point and angle in degrees, go from current point
! with counterclockwise angles positive.
!-----------------------------------------------------------------------------------------------------------------------------------
use M_draw
character(len=*),parameter::ident="@(#)create circular arc, leaving CP at end of arc"
real,intent(in) :: x
real,intent(in) :: y
real,intent(in) :: angle
!-----------------------------------------------------------------------------------------------------------------------------------
real :: xpass, ypass  ! copies of x and y
real :: ang1
real :: ang2
real :: d2r
integer :: nsegs
real :: radius
real :: xnow
real :: ynow

   ! let compiler know I am not changing input values
   xpass=x
   ypass=y

   call getgp2(xnow,ynow)
   radius=sqrt((xnow-xpass)**2+(ynow-ypass)**2)
   ang1=atan2(ynow-ypass,xnow-xpass)*180.0/3.14159265359
   ang2=ang1+angle
!
!!      reduce angles to range of 0 to 360
!!!     ang1=amod(amod(ang1,360.0)+360.0,360.0)
!!      ang2=amod(amod(ang2,360.0)+360.0,360.0)
!
   call arc2(xpass,ypass,radius,ang1,ang2)
!
!!      update current position to end of arc
!!!     rang2=ang2/180.0*3.14159265359
!!!!    xend=xpass+radius*cos(rang2)
!!!     yend=ypass+radius*sin(rang2)
!!      call move2(xend,yend)
!
      end subroutine uarc
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    arc2(3f) - [MDASHPLUS] draw an arc
!!
!!##SYNOPSIS
!!
!!    subroutine arc2(x, y, radius, startang, endang)
!!
!!     real,intent(in) :: x
!!     real,intent(in) :: y
!!     real,intent(in) :: radius
!!     real,intent(in) :: startang
!!     real,intent(in) :: endang
!!
!!##DESCRIPTION
!!    M_DASH arc(3f) routine does a move so it cannot be included in a
!!    polygon very easily . This routine is called by M_DASH version of
!!    uarc(3f)
!===================================================================================================================================
subroutine arc2(x, y, radius, startang, endang)
use M_draw
character(len=*),parameter::ident="@(#)Like M_DASH arc routine without move at end so can be in a polygon"

real,intent(in) :: x
real,intent(in) :: y
real,intent(in) :: radius
real,intent(in) :: startang
real,intent(in) :: endang
!----------------------------------------------------------------------------------------------------------------------------------!
real     :: cx, cy, dx, dy
real     :: deltang, cosine, sine, angle
integer  :: i
integer  :: numsegs
real     :: ang1
real     :: ang2
real     :: d2r
integer  :: nsegs
real     :: xnow
real     :: ynow
   D2R=3.14159265359/180.0
   angle = startang * D2R
   nsegs=100
   numsegs = abs(endang - startang) / 360.0 * nsegs + 0.5
   deltang = (endang - startang) * D2R / numsegs
   cosine = cos(deltang)
   sine = sin(deltang)
!  calculates initial point on arc
   cx = x + radius * cos(angle)
   cy = y + radius * sin(angle)
!  assume you are already at this point so can be used in polygons
!  call move2(cx, cy)
   do i=1,numsegs
      dx = cx - x
      dy = cy - y
      cx = x + dx * cosine - dy * sine
      cy = y + dx * sine + dy * cosine
      call draw2(cx, cy)
   enddo
end subroutine arc2
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    biggest_ortho2(3f) - [M_DRAWPLUS] set window into largest viewport available
!!
!!##SYNOPSIS
!!
!!       subroutine biggest_ortho2(xsmall,xlarge,ysmall,ylarge)
!!       real, intent=(in) :: xsmall
!!       real, intent=(in) :: xlarge
!!       real, intent=(in) :: ysmall
!!       real, intent=(in) :: ylarge
!!
!!##DESCRIPTION
!!    Given a window size, and assuming a one-to-one correspondence of window
!!    units (ie. an "x-unit" is as long as a "y-unit"), find the largest area
!!    on the display surface that has the same aspect ratio, and set the
!!    viewport to it.
!!
!!    assumes that the screen rasters are square.
!===================================================================================================================================
subroutine biggest_ortho2(xsmall,xlarge,ysmall,ylarge)
!----------------------------------------------------------------------------------------------------------------------------------!
character(len=*),parameter::ident="@(#)biggest_ortho2(3f) given a window size, find and set to largest accommodating viewport"

real,intent(in)  :: xsmall
real,intent(in)  :: xlarge
real,intent(in)  :: ysmall
real,intent(in)  :: ylarge
real             :: rps
real             :: spr
real             :: tryx
real             :: tryy
real             :: vhigh
real             :: vwide
real             :: xdelta
real             :: xmax
real             :: xmin
real             :: xsplit
real             :: ydelta
real             :: ymax
real             :: ymin
real             :: ysplit
!----------------------------------------------------------------------------------------------------------------------------------!
      call getdisplaysize(vwide,vhigh) !get screen size in terms of raster units
!----------------------------------------------------------------------------------------------------------------------------------!
!     the default viewport is in "screen units", and goes from -1,-1 to 1,1
!     all new viewports are defined in terms of this original viewport, which
!     is 2 units wide and 2 units tall.
!----------------------------------------------------------------------------------------------------------------------------------!
      rps=min(vwide,vhigh)/2.0         ! number of rasters per screen unit
      spr=2.0/min(vwide,vhigh)         ! number of screen units per raster
      tryx=vwide                       ! make as wide as display as a trial fit
      if(xlarge-xsmall.ne.0.0)then
         tryy=vwide*(ylarge-ysmall)/(xlarge-xsmall) ! calculate required height
      else                             ! ERROR: do something desperate
         call journal('*biggest_ortho2* window has a zero X dimension')
         tryy=vhigh
      endif
      if(tryy.gt.vhigh)then ! if required height too great, fit with y maximized
         tryy=vhigh
         if(ylarge-ysmall.ne.0.0)then
            tryx=vhigh*(xlarge-xsmall)/(ylarge-ysmall)
         else                          ! ERROR: do something desperate
            call journal('*biggest_ortho2* window has a zero Y dimension')
            tryx=vwide
         endif
      endif
!----------------------------------------------------------------------------------------------------------------------------------!
!   tryx and tryy are now the required viewport in raster units. The raster
!   units now need converted to screen units to be used in viewport procedure
!
!   some explanation of physical viewport units is required:
!   assuming maximizing the required aspect ratio in the available drawing area,
!   and that the original viewport "origin" 0,0 stays in it's original position,
!   and that the original -1,1,-1,1 viewport is the largest square that can fit
!   on the display, bottom left justified.
!   the screen coordinate system is a right-handed Cartesian coordinate system
!   with positive x to the viewer's right, positive y up.
!
!   at this point,
!    vwide=width in rasters of entire display
!    vhigh=height in rasters of entire display
!   assuming a square raster
!     tryx is desired width in rasters
!     tryy is desired height in rasters
!----------------------------------------------------------------------------------------------------------------------------------!
      xdelta=tryx-2.0*rps  ! need this many more rasters in x direction from 1,1
      ydelta=tryy-2.0*rps  ! need this many more rasters in y direction from 1,1
      ! to center (to left bottom justify, make xsplit and ysplit 0)
      xsplit=(vwide-tryx)/2.0
      ysplit=(vhigh-tryy)/2.0
      xmax=1+xdelta*spr+xsplit*spr
      ymax=1+ydelta*spr+ysplit*spr
      xmin=-1+xsplit*spr
      ymin=-1+ysplit*spr
!----------------------------------------------------------------------------------------------------------------------------------!
!      write(*,*)'max. display area is', vwide, ' by ',vhigh,' rasters'
!      write(*,*)'shape is ',xsmall,xlarge,ysmall,ylarge
!      write(*,*)'attempting to get a viewport of ',tryx,' by ',tryy
!      write(*,*)'needed more rasters, ',xdelta,' by ',ydelta
!      write(*,*)'resulted in viewport ',-1,xmax,-1,ymax
!----------------------------------------------------------------------------------------------------------------------------------!
      if(xmin.ne.xmax.and.ymin.ne.ymax)then
         call viewport(xmin,xmax,ymin,ymax)
      else
         call journal('*biggest_ortho2* window has zero dimension,no viewport set')
      endif
!     to prevent clipping lines that are right on edge of window fudge a bit
      !bugx=.001*(xlarge-xsmall)
      !bugy=.001*(ylarge-ysmall)
      !xsmall1=xsmall-bugx
      !xlarge1=xlarge+bugx
      !ysmall1=ysmall-bugy
      !ylarge1=ylarge+bugy
      !call ortho2(xsmall1,xlarge1,ysmall1,ylarge1)
      if(xsmall.ne.xlarge.and.ysmall.ne.ylarge)then
         call ortho2(xsmall,xlarge,ysmall,ylarge)
      else    ! ERROR: do something desperate
         call journal('*biggest_ortho2* window has zero dimension, no window set')
      endif
end subroutine biggest_ortho2
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    uconic(3f) - [M_DRAWPLUS] general conic sections
!!##SYNOPSIS
!!
!!    uconic(x,y,p,e,theta1,theta2,orientation)
!!
!!     real, intent=(in) :: x
!!     real, intent=(in) :: y
!!     real, intent=(in) :: p
!!     real, intent=(in) :: e
!!     real, intent=(in) :: theta1
!!     real, intent=(in) :: theta2
!!     real, intent=(in) :: orientation
!!
!!##DESCRIPTION
!!
!!    UCONIC() allows the user to draw a generalized conic section having
!!    a given focus, directrix, eccentricity, and starting and ending sweep
!!    positions. The conic sections that may be drawn are
!!
!!       CALL UCONIC (X,Y,P,E,THETA1,THETA2,ORIENTATION)
!!
!!    (X,Y) are used to specify the coordinates of the focus of the conic
!!    section; P is the distance from the focus to the directrix; E is the
!!    eccentricity, THETA1 and THETA2 represent the initial and final angles
!!    through which the conic section is to be drawn. Parameters E and P
!!    affect the generation of the conic section in the following manner:
!!
!!##OPTIONS
!!         x  x-coordinate of the focus of the conic section.
!!         y  y-coordinate of the focus of the conic section.
!!
!!         p  Distance from the focus to the directrix.
!!
!!              o P.GT.0 Defines the position of the focus to be to the right of (or
!!                below) the directrix.
!!              o P.LT.0 Indicates that the focus is positioned to the left of (or
!!                above) the directrix.
!!
!!         e  eccentricity.
!!
!!              o E=0 Circular arc with a center at (X,Y) at radius P/2. The arc
!!                will subtend the angular range defined by THETA1 and THETA2.
!!              o 0.LT.ABS(E).LT.1 ELLIPSE.
!!              o ABS(E)=1 PARABOLA.
!!              o ABS(E).GT.1 HYPERBOLA
!!              o E.GT.0 Indicates that the major axis of the conic section is to
!!                be oriented parallel to the X-axis.
!!              o E.LT.0 Specifies that the major axis is to be oriented along
!!                the Y-axis.
!!
!!         theta1    Initial sweep position in current angular units.
!!
!!         theta2    Terminal sweep position in current angular units.
!!
!!         orientation  angle of conic axis in degrees
!!
!!         COMMENTS
!!         The basis for uconic is the generalized conic equation
!!
!!            R=(E*P)/(1-E*COS(THETA))
!!
!!         By suitably modifying the values of the parameters P and E, all types
!!         of conic sections can be created. The following describes the effects
!!         induced by different values of P of E.
!!
!!           P.GT.0       the focus is to the right (below the directrix).
!!
!!           P.LT.0       the focus is to the left (above the directrix).
!!
!!           P = 0        the conic will be a point at x,y.
!!
!!           E = 0        the conic is a circular arc with center at x,y
!!                        and radius subtending the angular range from the
!!                        theta1 to theta2.
!!
!!           0.LT.ABS
!!           (E).LT.1     the conic is an ellipse.
!!
!!           ABS(E)=1     the conic is a parabola.
!!
!!           ABS(E).GT.1  the conic is a hyperbola.
!!
!!           E.GT.0       the conic is oriented along the x-axis
!!
!!           E.LT.0       the conic is oriented along the y-axis
!!
!!
!!         Conic sections may be rotated to any angle by defining a suitable user
!!         coordinate system.
!!
!!         The circle is a degenerate case not fully handled by the generalized
!!         conic equation. For completeness a circle with arbitrarily assigned
!!         radius of will be generated when E has a value of zero.
!!
!!         If an angle of orientation of the conic is specified, then the conic
!!         section will be oriented as specified around the point of intersection
!!         of the directrix and the semi-major axis.
!!
!!         In three dimensional applications, the conic section drawn by uconic
!!         will lie in the current xy plane.
!!
!!##AUTHOR
!!    Heavily based on a GCS routine written by Major Gabriel:
!===================================================================================================================================
subroutine uconic(x,y,p,e,th1,th2,rangle)
!    procedure draws all or part of a generalized conic section having a
!    given focus, directorix, and eccentricity and an angular span from
!    th1 to th2
!
!    basis for uconic is general conic equation  r=ep/1-e*cos(theta)
!    plotting in uconic is done in a cartesian coordinate system
!    computations are performed in degrees
!
!    procedures called:  move2, draw2, uarc, translate, d2r
!
!    calling sequence:
!       call uconic(x,y,p,e,th1,th2)
!    where
!    x,y are the coordinates of the focus of the conic section
!    p is the distance from the focus to the directorix
!             p>0       focus to the right(below) the directorix
!             p<0       focus to the left(above) the directorix
!             p=0       the conic will be a point at x,y
!    e is the eccentricity
!             e=0       the conic is a circular arc with center at x,y and
!                       radius p/2 subtending the angular range from th1 to th2
!             0<|e|<1   conic is an ellipse
!             |e|=1     conic is a parabola
!             |e|>1     conic is a hyperbola
!             e>0       conic is oriented along the x-axis
!             e<0       conic is oriented along the y-axis
!   th1      initial angular location from which the conic is to start
!   th2      final angular location where the conic will end
!   rangle   rotation angle
!
!    local variables
!        ep   the product of the eccentricity(e) and the distance from the focus
!             to the directorix
!        g    the denominator in the general conic equation
!        r    the radius
!        deg  the number of three degree line segments required to draw figure
!        n    the total number of three degree segments
!        d    the fractional part of three degree segment to complete the figure
!        an   the angular span of the circular arc
!-----------------------------------------------------------------------------------------------------------------------------------
use M_draw
use ISO_C_BINDING
use m_units, only: d2r
character(len=*),parameter::ident="@(#)m_draw:uconic(3f) general conic sections"
real :: an
real :: angdel
real :: anginc
real :: e
real :: ep
real :: finer
real :: g
integer :: i
integer :: itune
integer :: j
integer :: ltune
integer :: n
real :: p
real :: r
real :: rangle
real :: ro
real :: s
real :: t
real :: th1
real :: th2
real :: theta
real :: theta1
real :: theta2
real :: thetal
real :: x
real :: xx
real :: y
real :: yy
!-----------------------------------------------------------------------------------------------------------------------------------
! if distance between focus and directorix is zero plot a point
   if(p.eq.0.)then
      call point2(x,y)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   theta2=th2
   theta1 = th1
   thetal=theta2                             ! save last location
   anginc=3.                                 ! set initial arc segment angle
   an=theta2-theta1                          ! determine the angular span of the arc
   finer=abs(2.*p*e)                         ! compute fine tuning threshhold
   call translate(x,y,0.0)                   ! create uconic coordinate system
   n=(theta2-theta1)/anginc
   anginc=(theta2-theta1)/float(n)           ! update angle increment to leave small angle at end

!  if the eccentricity is equal to zero plot a circle ( or circular arc )
88 continue
   if(e.eq.0.)then                           ! circle or circular arc section
      r = p
!     determine where the initial point will be and move to that point
      theta2=theta1+rangle
      xx=r*cos(d2r(theta2))
      yy=r*sin(d2r(theta2))
      call move2(xx,yy)
!     since the window has been modified the user"s x,y coordinate is the (0.,0.,) coordinate in the new window.
      call uarc(0.,0.,an)                    ! draw the arc or circle with the center at (0.,0.,) for an angular span of an.
      call translate(-x,-y,0.0)              ! restore the coordinate system
      return
   elseif(e.lt.0.) then                     ! determine where the initial point will be and move to that point
      g = -1.0 + e * sin(theta1 * .01745329)
   else
      g =  1.0 - e * cos(theta1 * .01745329)
   endif

   ep=e*p
   if(g.ne.0.)then
      r = ep / g
!     save the previous value of !r!
      ro=r
      theta2=theta1+rangle
      xx=r*cos(d2r(theta2))
      yy=r*sin(d2r(theta2))
      call move2(xx,yy)
   else
!     correct to avoid starting at infinity
      n = n - 1
      theta1=theta1+anginc
      go to 88
   endif

!     construct the conic(hyperbola,parabola, or ellipse)
   do i=1,n
!     prepare fine tuning loop
      angdel=anginc
      itune=1

!     branch if fine tuning not required
      if(abs(r).gt.finer)then           ! set loop for fine tuning
         itune=min1(10.,abs(r)/finer+1.)
         angdel=anginc/float(itune)
      endif

!     start fine tuning loop
      do ltune=1,itune
!        save previous values of r and theta2
         s=r
         t=theta2
!        add anginc degrees to the previous value of theta in determining the next segment to be drawn
         theta=theta1+anginc*float(i-1)+angdel*float(ltune)

!        determine the eccentricity so the appropriate denominator for the general conic equation and axis orientation can be used
         if(e.lt.0.) then
            g = -1.0 + e * sin(theta * .01745329)
         else
            g =  1.0 - e * cos(theta * .01745329)
         endif

         if(g.eq.0.) then
            if(abs(e).gt.1.0) then
               j=2
            endif
         elseif(g.eq.2.)then
            theta2=theta+rangle
            xx=r*cos(d2r(theta2))
            yy=r*sin(d2r(theta2))
            call move2(xx,yy)
            j=1
         else
!           solve the conic equation for !r!
            r = ep / g
!           solve for the polar coordinate theta2=theta+ the angle of rotation
            theta2=theta+rangle
!           eliminate the asymptotes
            if(r*ro.gt.0.)then
!              draw the segment using !r! and theta2 as the polar coordinates
               xx=r*cos(d2r(theta2))
               yy=r*sin(d2r(theta2))
               call draw2(xx,yy)
            else
               xx=r*cos(d2r(theta2))
               yy=r*sin(d2r(theta2))
               call move2(xx,yy)
            endif
         endif
         ro=r
      enddo
   enddo

!  construct the last segment of the conic
!  if !d! is equal to zero , back-up to the last segment drawn since a single point cannot have a terminator
   if(theta.eq.thetal)then
         xx=s*cos(d2r(t))
         yy=s*sin(d2r(t))
         call move2(xx,yy)
   endif
!  determine the eccentricity
   theta=thetal

   if(e.lt.0.) then
         g = -1.0 + e * sin(theta * .01745329)
   else
         g = 1.0 - e * cos(theta * .01745329)
   endif
   if(g.ne.0.) then
         r = ep / g
         theta2=theta+rangle
         xx=r*cos(d2r(theta2))
         yy=r*sin(d2r(theta2))
         call draw2(xx,yy)
   endif
   call translate(-x,-y,0.0) ! restore the coordinate system
end subroutine uconic
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! @(#) Fortran MODULE interface to M_DRAW C routines using Fortran ISO_C_BINDING interface
!             3. Interoperating with C
!
! Any entity involved in interoperating with C must be such that equivalent
! declarations of it may be made in the two languages.
!
! Enforced within the Fortran program by requiring all such entities to
! be interoperable.
!
! We will explain in turn what this requires for types, variables,
! and procedures.
!
! They are all requirements on the syntax so that the compiler knows at
! compile time whether an entity is interoperable.
!
!      3.1 Interoperability of intrinsic types
!
! Intrinsic module ISO_C_BINDING contains named constants holding kind
! type parameter values.  For example:
!
!    C_INT                       int
!    C_SHORT                     short int
!    C_LONG                      long int
!    C_FLOAT                     float
!    C_DOUBLE                    double
!    C_LONG_DOUBLE               long double
!    C_FLOAT_COMPLEX             float _Complex
!    C_BOOL                      _Bool
!    C_CHAR                      char
!
! Lack of support is indicated with a negative value.
!
!       3.2 Interoperability of derived types
!
! For a derived type to be interoperable, it must be given the BIND
! attribute explicitly:
!
!        TYPE, BIND(C) :: MYTYPE
!         :
!        END TYPE MYTYPE
!
! Each component must have interoperable type and type parameters, must
! not be a pointer, and must not be allocatable. This allows Fortran and
! C types to correspond.
!
!         3.3 Interoperability of variables
!
! A scalar Fortran variable is interoperable if it is of interoperable
! type and type parameters, and is neither a pointer nor allocatable.
!
! An array Fortran variable is interoperable if it is of interoperable
! type and type parameters, and is of explicit shape or assumed size. It
! interoperates with a C array of the same type, type parameters and shape,
! but with reversal of subscripts.
!
! For example, a Fortran array declared as
!          INTEGER :: A(18, 3:7, *)
! is interoperable with a C array declared as
!          int b[][5][18]
!
!
!       3.4 Interoperability with C pointers
!
! For interoperating with C pointers (addresses), the module contains a
! derived type C_PTR that is interoperable with any C pointer type and a
! named constant C_NULL_PTR.
!
! The module also contains the procedures:
!
!    C_LOC(X) returns the C address of X.
!
!    C_ASSOCIATED (C_PTR1[, C_PTR2]) is an
!   inquiry function that is like ASSOCIATED.
!
!   C_F_POINTER (CPTR, FPTR [, SHAPE])) is a
!   subroutine that constructs a Fortran pointer
!   from a scalar of type C_PTR.
!
!        3.5 Interoperability of procedures
!
! A new attribute, VALUE, has been introduced for scalar dummy
! arguments. Does copy-in without copy-out.
!
! A Fortran procedure is interoperable if it has an explicit interface
! and is declared with the BIND attribute:
!
!  FUNCTION FUNC(I, J, K, L, M), BIND(C)
!
! All the dummy arguments must be interoperable.  For a function, the
! result must be scalar and interoperable.
!
!                 3.6 Binding labels
!
! The Fortran procedure has a `binding label',
! which has global scope and is the name by which
! it is known to the C processor.
! By default, it is the lower-case version of the
! Fortran name.
! An alternative binding label may be specified:
!   FUNCTION FUNC(I, J, K, L, M) BIND(C, NAME='C_Func')
!
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! hide logicals from C
! trim and append null to intent(in) character strings
subroutine barcode(XCORNER_IN,YCORNER_IN,XSMALL_IN,XLARGE_IN,YSIZE_IN,STRING)
implicit none
! void barcode( float xcorner, float ycorner, float xsmall, float xlarge, float ysize, char *string);
   interface
      subroutine barcode_F(XCORNER,YCORNER,XSMALL,XLARGE,YSIZE,STRING) bind(C,NAME='barcode')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: XCORNER
         real(KIND=C_FLOAT),intent(in),value :: YCORNER
         real(KIND=C_FLOAT),intent(in),value :: XSMALL
         real(KIND=C_FLOAT),intent(in),value :: XLARGE
         real(KIND=C_FLOAT),intent(in),value :: YSIZE
         character(KIND=C_CHAR) :: STRING(*)
      end subroutine barcode_F
   end interface

   real,intent(in) :: XCORNER_IN
   real,intent(in) :: YCORNER_IN
   real,intent(in) :: XSMALL_IN
   real,intent(in) :: XLARGE_IN
   real,intent(in) :: YSIZE_IN

   real(KIND=C_FLOAT) :: XCORNER
   real(KIND=C_FLOAT) :: YCORNER
   real(KIND=C_FLOAT) :: XSMALL
   real(KIND=C_FLOAT) :: XLARGE
   real(KIND=C_FLOAT) :: YSIZE

   character*(*) STRING

   XCORNER=XCORNER_IN
   YCORNER=YCORNER_IN
   XSMALL=XSMALL_IN
   XLARGE=XLARGE_IN
   YSIZE=YSIZE_IN

   !write(*,*)'*barcode* ',xcorner,ycorner,xsmall,xlarge,ysize,string
   call barcode_F(XCORNER,YCORNER,XSMALL,XLARGE,YSIZE,STRING(:len_trim(STRING))//achar(0))
end subroutine barcode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'    draw_interpret(3f) - [M_drawplus] call interpreter for testing M_draw(3fm) routines',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    subroutine draw_interpret(array,delimiters)                                 ',&
'    character(len=*) :: array(*)                                                ',&
'    character(len=*) :: delimiters                                              ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'    takes an array of strings and treats them as requests for simple calls      ',&
'    to the call_draw(3f) routine. This allows for creating simple               ',&
'    graphics and for testing the M_draw(3fm) module.                            ',&
'                                                                                ',&
'    The M_calc(3fm) module is used to evaluate number parameters. The           ',&
'    "set" directive is added to allow easily declaring variables for the        ',&
'    calculator.                                                                 ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    ARRAY       character array containing strings representing basic           ',&
'                M_draw(3fm) procedures.                                         ',&
'    DELIMITERS  character(s) used to separate commands in the array.            ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'    Sample program                                                              ',&
'                                                                                ',&
'     program demo_draw_interpret                                                ',&
'     use M_drawplus, only : call_draw                                           ',&
'                                                                                ',&
'     ! $FILTER variable -varname DRAW_CMDS                                      ',&
'                                                                                ',&
'     DRAW_CMDS=[ CHARACTER(LEN=128) ::                                         &',&
'     ''set N=11; prefsize 600 200; vinit ;page -15 15 -5 5                    '',&',&
'     ''textsize .3 .3; linewidth 150/3; color 0; clear                        '',&',&
'     ''color  1;  spirograph  -10  0  N  1  N  5  1000  0  0  0               '',&',&
'     ''polyhatch 1; hatchang 45.0 ; hatchpitch 0.3 # turn on polygon hatching '',&',&
'     ''color  2;  spirograph   10  0  N  1  N  5  1000  0  0  2               '',&',&
'     ''                                                                       '',&',&
'     ''vflush; getkey ; vexit                                                 '',&',&
'     '''']                                                                      ',&
'                                                                                ',&
'     ! $FILTER END                                                              ',&
'                                                                                ',&
'     call draw_interpret(DRAW_CMDS,delimiters='';'')                            ',&
'     end program demo_draw_interpret                                            ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'    call_draw(3f), M_draw(3fm), M_drawplus(3fm)                                 ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     draw_interpret(3f) - [M_drawplus] call interpreter for testing M_draw(3fm) routines
!!
!!##SYNOPSIS
!!
!!     subroutine draw_interpret(array,delimiters)
!!     character(len=*) :: array(*)
!!     character(len=*) :: delimiters
!!
!!##DESCRIPTION
!!     takes an array of strings and treats them as requests for simple calls
!!     to the call_draw(3f) routine. This allows for creating simple
!!     graphics and for testing the M_draw(3fm) module.
!!
!!     The M_calc(3fm) module is used to evaluate number parameters. The
!!     "set" directive is added to allow easily declaring variables for the
!!     calculator.
!!
!!##OPTIONS
!!     ARRAY       character array containing strings representing basic
!!                 M_draw(3fm) procedures.
!!     DELIMITERS  character(s) used to separate commands in the array.
!!
!!##EXAMPLES
!!
!!     Sample program
!!
!!      program demo_draw_interpret
!!      use M_drawplus, only : call_draw
!!
!!      ! $FILTER variable -varname DRAW_CMDS
!!
!!      DRAW_CMDS=[ CHARACTER(LEN=128) ::                                         &
!!      'set N=11; prefsize 600 200; vinit ;page -15 15 -5 5                    ',&
!!      'textsize .3 .3; linewidth 150/3; color 0; clear                        ',&
!!      'color  1;  spirograph  -10  0  N  1  N  5  1000  0  0  0               ',&
!!      'polyhatch 1; hatchang 45.0 ; hatchpitch 0.3 # turn on polygon hatching ',&
!!      'color  2;  spirograph   10  0  N  1  N  5  1000  0  0  2               ',&
!!      '                                                                       ',&
!!      'vflush; getkey ; vexit                                                 ',&
!!      '']
!!
!!      ! $FILTER END
!!
!!      call draw_interpret(DRAW_CMDS,delimiters=';')
!!      end program demo_draw_interpret
!!
!!##SEE ALSO
!!     call_draw(3f), M_draw(3fm), M_drawplus(3fm)
!===================================================================================================================================
subroutine draw_interpret(array,delimiters)
use M_strings, only: split
implicit none
character(len=:),allocatable    :: array(:)
character(len=1024),allocatable :: cmds(:) ! output array of tokens
logical                         :: found
integer                         :: iend
integer                         :: i,j
character(len=*)                :: delimiters
!-----------------------------------------------------------------------------------------------------------------------------------
   do j=1,size(array)
      call split(array(j),cmds,delimiters=trim(delimiters))
      do i=1,size(cmds)
         cmds(i)=adjustl(cmds(i))
         iend=scan(cmds(i),' #')-1
         if(iend.le.0)iend=len_trim(cmds(i))
         if(iend.ne.0)then
            if(cmds(i)(:1).eq.'#') cycle
            cmds(i)=trim(cmds(i))//' '
            call call_draw(cmds(i)(:iend),cmds(i)(iend+1:),found)
            if(.not.found)then
               write(*,'(*(a))')'ERROR: ',trim(cmds(i)(:iend)),' [',trim(cmds(i)(iend+1:)),']',' not found'
            endif
         endif
      enddo
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine draw_interpret
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!     call_draw(3f) - [M_drawplus] Given a string reprsenting a M_draw procedure and parameters  call the routine
!!##SYNOPSIS
!!
!!   subroutine call_draw(verb,parameters,found)
!!
!!    character(len=*),intent(in)  :: verb
!!    character(len=*),intent(in)  :: parameters
!!    logical,intent(out)          :: found
!!
!!##DESCRIPTION
!!    Used to allow input files to directly call arbitrary low-level graphics procedures.
!!    This is a simple interpreter for M_graph(3fm) routines.
!!
!!##OPTIONS
!!    verb         name of M_draw(3fm) routine to call
!!    parameters   string representing options to pass to
!!                 the routine specified by the verb.
!!                 Numeric values are evaluated using the
!!                 M_calc(3fm) module to allow expressions.
!!##RETURNED
!!    found        returns .TRUE. if the verb was found,
!!                 otherwise .FALSE.
!!
!!##EXAMPLE
!!
!!    Simple Example
!!
!!        program demo_call_draw
!!        use M_drawplus, only : call_draw
!!        use M_io, only : read_line
!!        implicit none
!!        character(len=:),allocatable :: line
!!        logical                      :: found
!!        integer                      :: iend
!!           INFINITE: do while (read_line(line)==0)
!!              line=adjustl(line)
!!              iend=scan(line,' #;')-1
!!              if(iend.le.0)iend=len_trim(line)
!!              if(iend.ne.0)then
!!                 line=line//' '
!!                 call call_draw(line(:iend),line(iend+1:),found)
!!                 if(.not.found)then
!!                    write(*,*)'ERROR: ',line(:iend),'['line(iend+1):']',' not found'
!!                 endif
!!              endif
!!           enddo INFINITE
!!        end program demo_call_draw
!!
!!    Sample
!!
!!        demo_call_draw <<EOF
!!        prefsize 400 400
!!        vinit X11
!!        circleprecision 100
!!        color 1
!!        circle 0 0 A=1.0
!!        color 2
!!        circle 0 0 A=A-.1
!!        color 3
!!        circle 0 0 A=A-.1
!!        color 4
!!        circle 0 0 A=A-.1
!!        color 5
!!        circle 0 0 A=A-.1
!!        color 6
!!        circle 0 0 A=A-.1
!!        color 7
!!        getkey LET
!!        vexit
!!        EOF
!===================================================================================================================================
subroutine call_draw(verb,parameters,found)
use iso_fortran_env
use M_calculator,      only : stuffa, stuff, iclen_calc
use M_calculator_plus, only : snum0, inum0, rnum0, strgar2
use M_debug,           only : debug, io_debug
use M_strings,         only : delim
use M_draw
!!implicit real   (a-h,o-z)
implicit none

character(len=*),parameter :: ident="@(#)juvog(3f): parse most M_draw(3fm) routines positionally "

! parse most M_draw(3fm) routines positionally
! simplistic, does not handle quoted parameters directly, just parses on space
!
character(len=*),intent(in)  :: parameters
character(len=*),intent(in)  :: verb
logical,intent(out)          :: found

character(len=255)           :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
character(len=1)             :: ch
character(len=1024)          :: ctemp1
integer,parameter            :: IHM=10
character(len=255)           :: array(IHM)
character(len=iclen_calc)    :: temp1
integer                      :: i
integer,parameter            :: isize=2000
integer                      :: il(IHM) ,icount, ilast
integer                      :: is(IHM), ie(IHM)
real                         :: numbrs(isize)
real                         :: points(3,1000)
real                         :: points2(2,1000)
integer                      :: idum
integer                      :: ierr
integer                      :: inums
integer                      :: ival
integer                      :: n2
real                         :: rval
real                         :: value0
real                         :: value1
real                         :: value2
real                         :: value3
real                         :: value4
!-----------------------------------------------------------------------------------------------------------------------------------
   is=0
   ie=0
   found=.true.
   array=' '
   call delim(parameters,array,int(IHM),icount,is,ie,ilast,' ')
   il=ie-is+1 ! lengths of character strings, except 1 if empty string


   if(debug)then
      write(io_debug,*)'*juifvg* PARAMETERS=',trim(parameters)
      write(io_debug,*)'*juifvg* ICOUNT=',icount
      write(io_debug,*)'*juifvg* IL=',IL
      do i=1,icount
         write(io_debug,'(a,i0,a,a,/)')'*juifvg* ARRAY=',i,' ',trim(array(i))
         write(io_debug,'(a,i0,a,a,/)')'*juifvg* IS:IE=',i,' ',trim(parameters(is(i):ie(i)))
      enddo
   endif

   x1=  array(01)(:il(01))
   x2=  array(02)(:il(02))
   x3=  array(03)(:il(03))
   x4=  array(04)(:il(04))
   x5=  array(05)(:il(05))
   x6=  array(06)(:il(06))
   x7=  array(07)(:il(07))
   x8=  array(08)(:il(08))
   x9=  array(09)(:il(09))
   x10= array(10)(:il(10))

!-----------------------------------------------------------------------------------------------------------------------------------
   select case(verb)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('arc')              ; call arc(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5))
   case('backface')         ; call backface(iflogic(snum0(x1)))
   case('backfacedir')      ; call backfacedir(iflogic(snum0(x1)))
   case('bottomjustify')    ; call bottomjustify()
   case('boxfit')           ; call boxfit(rnum0(x1),rnum0(x2),inum0(x3))
   case('callobj')          ; call callobj(inum0(x1))
   case('centertext')       ; call centertext(iflogic(snum0(x1)))
   case('circle')           ; call circle(rnum0(x1),rnum0(x2),rnum0(x3))
   case('circleprecision')  ; call circleprecision(inum0(x1))
   case('clear')            ; call clear()
   case('clipping')         ; call clipping(iflogic(snum0(x1)))
   case('closeobj')         ; call closeobj()
   case('closepoly')        ; call closepoly()
   case('color')            ; call color(inum0(x1))
   case('dashcode')         ; call dashcode(rnum0(x1))
   case('delobj')           ; call delobj(inum0(x1))
   case('draw')             ; call draw(rnum0(x1),rnum0(x2),rnum0(x3))
   case('expandviewport')   ; call expandviewport()
   case('fixedwidth')       ; call fixedwidth(iflogic(snum0(x1)))
   case('font')             ; call font(snum0(x1))
   case('frontbuffer')      ; call frontbuffer()
   case('hatchang')         ; call hatchang(rnum0(x1))
   case('hatchpitch')       ; call hatchpitch(rnum0(x1))
   case('leftjustify')      ; call leftjustify()
   case('linestyle')        ; call linestyle(x1)
   case('linewidth')        ; call linewidth(inum0(x1))
   case('loadobj')          ; call loadobj(inum0(x1),snum0(x2))
   case('lookat')           ; call lookat(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),rnum0(x6),rnum0(x7))
   case('makeobj')          ; call makeobj(inum0(x1))
   case('makepoly')         ; call makepoly()
   case('mapcolor')         ; call mapcolor(inum0(x1),inum0(x2),inum0(x3),inum0(x4))
   case('move')             ; call move(rnum0(x1),rnum0(x2),rnum0(x3))
   case('ortho')            ; call ortho(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),rnum0(x6))
   case('ortho2')           ; call ortho2(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('perspective')      ; call perspective(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('point')            ; call point(rnum0(x1),rnum0(x2),rnum0(x3))
   case('point2')           ; call point2(rnum0(x1),rnum0(x2))
   case('polarview')        ; call polarview(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('polyfill')         ; call polyfill(iflogic(snum0(x1)))
   case('polyhatch')        ; call polyhatch(iflogic(snum0(x1)))
   case('popattributes')    ; call popattributes()
   case('popdev')           ; call popdev()
   case('popmatrix')        ; call popmatrix()
   case('popviewport')      ; call popviewport()
   case('prefposition')     ; call prefposition(inum0(x1),inum0(x2))
   case('prefsize')         ; call prefsize(inum0(x1),inum0(x2))
   case('printattribs')     ; call printattribs(snum0(x1))
   case('printvdevice')     ; call printvdevice(snum0(x1))
   case('pushattributes')   ; call pushattributes()
   case('pushdev')          ; call pushdev(x1)
   case('pushmatrix')       ; call pushmatrix()
   case('pushviewport')     ; call pushviewport()
   case('rdraw')            ; call rdraw(rnum0(x1),rnum0(x2),rnum0(x3))
   case('rdraw2')           ; call rdraw2(rnum0(x1),rnum0(x2))
   case('rect')             ; call rect(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('rightjustify')     ; call rightjustify()
   case('rmove')            ; call rmove(rnum0(x1),rnum0(x2),rnum0(x3))
   case('rmove2')           ; call rmove2(rnum0(x1),rnum0(x2))
   case('rsdraw2')          ; call rsdraw2(rnum0(x1),rnum0(x2))
   case('rsmove2')          ; call rsmove2(rnum0(x1),rnum0(x2))
   case('saveobj')          ; call saveobj(inum0(x1),snum0(x2))
   case('scale')            ; call scale(rnum0(x1),rnum0(x2),rnum0(x3))
   case('sdraw2')           ; call sdraw2(rnum0(x1),rnum0(x2))
   case('sector')           ; call sector(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5))
   case('smove2')           ; call smove2(rnum0(x1),rnum0(x2))
   case('swapbuffers')      ; call swapbuffers()
   case('textang')          ; call textang(rnum0(x1))
   case('textjustify')      ; !  call textjustify()
   case('textsize')         ; call textsize(rnum0(x1),rnum0(x2))
   case('topjustify')       ; call topjustify()
   case('translate')        ; call translate(rnum0(x1),rnum0(x2),rnum0(x3))
   case('unexpandviewport') ; call unexpandviewport()
   case('up')               ; call up(rnum0(x1),rnum0(x2),rnum0(x3))
   case('vexit')            ; call vexit()
   case('vflush')           ; call vflush()
   case('viewport')         ; call viewport(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('vinit')            ; call vinit(snum0(x1))
   case('vnewdev')          ; call vnewdev(snum0(x1))
   case('voutput')          ; call voutput(snum0(x1))
   case('vsetflush')        ; call vsetflush(iflogic(snum0(x1)))
   case('window')           ; call window(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),rnum0(x6))
   case('xcentertext')      ; call xcentertext()
   case('ycentertext')      ; call ycentertext()
   case('patchbasis')       ; write(*,*)'*juvogl* patchbasis not implemented'
   case('patchprecision')   ; write(*,*)'*juvogl* patchprecision not implemented'
   case('patchcurves')      ; write(*,*)'*juvogl* patchcurves not implemented'
   case('rpatch')           ; write(*,*)'*juvogl* rpatch not implemented'
   case('patch')            ; write(*,*)'*juvogl* patch not implemented'
   case('curvebasis')
   case('curveprecision')
   case('rcurve')
   case('curve')
   case('curven')
   ! m_drawplus
   case('invokeobj')        ; call invokeobj(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),  &
                                            & rnum0(x6),rnum0(x7),rnum0(x8),rnum0(x9),inum0(x10) )
   case('biggest_ortho2')   ; call biggest_ortho2(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('page')             ; call biggest_ortho2(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('pop')              ; call pop()
   case('push')             ; call push()
   case('spirograph')       ; call spirograph(rnum0(x1),rnum0(x2), &             ! xcenter, ycenter
                                            & rnum0(x3),rnum0(x4),rnum0(x5), &   ! sunr0, planet0, offset0
                                            & rnum0(x6), &                       ! radius
                                            & inum0(x7), &                       ! ilines
                                            & rnum0(x8), &                       ! ang
                                            & rnum0(x9), &                       ! angs
                                            & inum0(x10) )                       ! ifill
!-----------------------------------------------------------------------------------------------------------------------------------
   case('vgetdev')
     call vgetdev(ctemp1)
     if(x1.ne.' ')then
        call stuffa(x1,ctemp1,idum,'')
     else
        call stuffa('$VFUNCTION',ctemp1,idum,'')
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getdepth')
     value1=getdepth()
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getkey')
     value1=getkey()
     if(icount.ge.1)then             ! if no variable specified for return, simply pause
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('checkkey')
     value1=checkkey()
     if(icount.ge.1)then             ! if variable specified for return store returned ordinal
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getstring')
     value1=getstring(inum0(x1),ctemp1)
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
     if(icount.ge.2)then
        call stuffa(x2,ctemp1,idum,'')
     else
        call stuffa('$VFUNCTION',ctemp1,idum,'')
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('set')
     do i=1,icount
        value0=rnum0(array(i))
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case('locator')
     value0=locator(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     if(icount.ge.3)then
        call stuff(x3,value0,'')
     endif
     call stuff('VFUNCTION',value0,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('slocator')
     value0=slocator(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     if(icount.ge.3)then
        call stuff(x3,value0,'')
     endif
     call stuff('VFUNCTION',value0,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getviewport')
     call getviewport(value1,value2,value3,value4)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     call stuff(x3,value3,'')
     call stuff(x4,value4,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getaspect')
     value1 =  getaspect()
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getfactors')
     call getfactors(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getdisplaysize')
     call getdisplaysize(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('move2') ! move2 x1 y1 x2 y2 ...xn yn ---> move2(x1,x2);draw2(x2,y2);draw2(x3,y3);....draw2(xn,yn)
     !call move2(rnum0(x1),rnum0(x2))
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     call move2(numbrs(1),numbrs(2))
     do i=3,inums,2     ! sloppy assumes icount divisible by two, big enough.
        call draw2(numbrs(i),numbrs(i+1))
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case('draw2') ! draw2 x1 y1 x2 y2 ...xn yn ---> draw2(x1,x2);draw2(x2,y2);draw2(x3,y3);....draw2(xn,yn)
     !call draw2(rnum0(x1),rnum0(x2))
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     do i=1,inums,2     ! sloppy assumes icount divisible by two, big enough
        call draw2(numbrs(i),numbrs(i+1))
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case('poly2') ! poly2 followed by points that define polygon
!    poly2 x1 y1 x2 y2 ...xn yn ---> poly2(n,x(),y())
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     n2=0
     do i=1,inums,2     ! sloppy assumes icount divisible by two, big enough.; use new features of fortran to resize
        n2=n2+1
        points2(1,n2)=numbrs(i)
        points2(2,n2)=numbrs(i+1)
     enddo
     call poly2(n2,points2)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('poly') ! poly followed by points that define polygon
!    poly x1 y1 z1 x2 y2 z2 ...xn yn zn ---> poly(n,x(),y(),z())
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     n2=0
     do i=1,inums,3     ! sloppy assumes icount divisible by three, big enough.
        n2=n2+1
        points(1,n2)=numbrs(i)
        points(2,n2)=numbrs(i+1)
        points(3,n2)=numbrs(i+2)
     enddo
     call poly(n2,points)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('numchars') ! wants a parameter, unlike actual function
     value1=real(numchars())
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getcharsize')
     if(icount.eq.2)then ! if icount equal 2 character was a blank
        call getcharsize(' ',value1,value2)
        call stuff(x1,value1,'')
        call stuff(x2,value2,'')
     else
        ch(1:1)=snum0(x1)
        call getcharsize(ch,value2,value3)
        call stuff(x2,value2,'')
        call stuff(x3,value3,'')
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getfontsize')
     call getfontsize(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('drawchar')
     if(icount.le.0)then
        call drawchar(' ')
     else
        ch(1:1)=snum0(x1)
        call drawchar(ch)
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('drawstr')
     if(x1(1:1).eq.'$'.or.x1(1:1).eq.'"')then
        call drawstr( trim(snum0(parameters)))
     else
        call drawstr(parameters)
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('strlength')
     temp1=trim(parameters)
     if(temp1(1:1).eq.'$'.or.temp1(1:1).eq.'"')then
        temp1=snum0(temp1)
     endif
     value1=strlength(temp1)
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('boxtext')
     temp1=trim(parameters(is(5):))
     if(temp1(1:1).eq.'$'.or.temp1(1:1).eq.'"')then ! if a string expression
        temp1=snum0(temp1)
     endif
     call boxtext(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),trim(temp1))
!-----------------------------------------------------------------------------------------------------------------------------------
   case('rotate')
     temp1=snum0(x2)
     if(temp1(1:1).eq.' ')temp1='z'
     ! parameter must be a single character for ISO_C_BINDING, at least with gfortran(1)
     ch(1:1)=snum0(x2)
     call rotate(rnum0(x1),ch)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('genobj')
     value1 = genobj()
     if(icount.ge.1)then
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getopenobj')
     value1 = getopenobj()
     if(icount.ge.1)then
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('isobj')
     if(isobj(inum0(x1)))then
        value1=0
     else
        value1=1
     endif
     if(icount.ge.2)then               ! if extra option assume it is variable name to store into
        call stuff(x2,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('backbuffer')
     value1=backbuffer()
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getgp')
     call getgp(value1,value2,value3)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     call stuff(x3,value3,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getgp2')
     call getgp2(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('sgetgp2')
     call sgetgp2(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('textslant')
     rval=rnum0(x1)
     call textslant(rval)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('textweight')
     ival=inum0(x1)
     call textweight(ival)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getgpt')
     call getgpt(value1,value2,value3,value4)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     call stuff(x3,value3,'')
     call stuff(x4,value4,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getfontdec')
     value1 = getfontdec()
     if(icount.ge.1)then
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case default
      found=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
end subroutine call_draw
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
logical function iflogic(string)
use M_calculator_plus, only : inum0
use M_strings, only         : lower
character(len=*),parameter :: ident="@(#)iflogic(3f): evaluate string in calculator and return false if value is zero"
character(len=*)           :: string
   select case(lower(string))
   case('.true.','t','.t.','true')
      iflogic=.true.
   case('.false.','f','.f.','false')
      iflogic=.false.
   case default
   if(inum0(string).eq.0)then    ! evaluate string and test if result is zero
      iflogic=.false.            ! result was zero, so return .false.
   else
      iflogic=.true.             ! result was not zero, so return .true.
   endif
   end select
end function iflogic
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!! spirograph(3f) - [M_drawplus] draw a hypotrochoid
!!
!!##SYNOPSIS
!!
!!   subroutine spirograph(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!!
!!    real,intent(in)    :: xcenter, ycenter
!!    real,intent(in)    :: sunr0,planet0,offset0
!!    real,intent(in)    :: radius
!!    integer,intent(in) :: ilines
!!    real,intent(in)    :: ang
!!    real,intent(in)    :: angs
!!    integer,intent(in) :: ifill
!!
!!##DESCRIPTION
!!    Draw a hypotrochoid generated by a fixed point on a circle rolling inside a
!!    fixed circle. It has the parametric equations
!!
!!       x = (R+r)costheta-(r+rho)cos((R+r)/rtheta)
!!       y = (R+r)sintheta-(r+rho)sin((R+r)/rtheta)
!!
!!    where R is the radius of the fixed circle, r is the radius of the rotating
!!    circle, and rho is the offset of the edge of the rotating circle. The figure
!!    closes only if R, r, and rho are rational. The equations can also be written
!!
!!##OPTIONS
!!    xcenter,ycenter     center of curve
!!    sunr0               radii of sun, planet, and planet offset
!!    planet0             radii of sun, planet, and planet offset
!!    offset0             radii of sun, planet, and planet offset
!!    radius              radius to fit the shape to (no fit if radius is 0)
!!    ilines              number of points to sample along curve
!!    ang                 angle to rotate the shape by, to orientate it.
!!    angs                angle to start sampling points at; ccw is +; 0 is East
!!    ifill               1 make a filled polygon, 2 make a hatched polygon
!!
!!##EXAMPLE
!!
!===================================================================================================================================
!===================================================================================================================================
subroutine spirograph(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!
!     Make shapes for use as markers using hypocycloidal curves.
!     Huge variety of shapes can be generated using this routine.
!===================================================================================================================================
use M_draw
character(len=*),parameter :: ident="@(#)spirographc(3f):draw hypocycloidal curves"
real,parameter :: PI= 3.14159265358979323846264338327950288419716939937510
real,intent(in)    :: xcenter, ycenter      ! center of curve
real,intent(in)    :: sunr0,planet0,offset0 ! radii of sun, planet, and planet offset
real,intent(in)    :: radius                ! radius to fit the shape to (no fit if radius is 0)
integer,intent(in) :: ilines                ! number of points to sample along curve
real,intent(in)    :: ang                   ! angle to rotate the shape by, to orientate it.
real,intent(in)    :: angs                  ! angle to start sampling points at; ccw is +; 0 is East
integer,intent(in) :: ifill                 ! 1 make a filled polygon, 2 make a hatched polygon
real    :: ang1
real    :: con1
real    :: con2
real    :: factor
integer :: i10
real    :: offset
real    :: planet
real    :: r
real    :: sunr
real    :: u
real    :: xpoin
real    :: xpoin1
real    :: ypoin
real    :: ypoin1

   sunr=sunr0
   offset=offset0
   planet=planet0

   if(ilines.eq.0.0) return
   if(planet.eq.0.0) return
   if(sunr.eq.0.0)   return

   if(radius.ne.0.and.sunr-planet+offset.ne.0)then
      factor=radius/(sunr-planet+offset)
      sunr=factor*sunr
      planet=factor*planet
      offset=factor*offset
   endif

   u=0.0+ang
   con1=PI*2.*(sunr/planet)/real(ilines)
   con2=(1.0-planet/sunr)*u
   xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
   ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)

   ang1=atan2(ypoin1,xpoin1)+angs
   r=sqrt(xpoin1**2+ypoin1**2)
   xpoin1=r*cos(ang1)+xcenter
   ypoin1=r*sin(ang1)+ycenter

   !call push()

   select case(ifill)
   case(0)
   case(1)
      call polyfill(.true.)
      call makepoly()
   case(2)
      call polyhatch(.true.)
      call makepoly()
   case(3:)
      call makepoly()
   case default
   end select

   call move2(xpoin1,ypoin1)
   do i10=1,ilines
      u=con1*i10+ang
      con2=(1.0-planet/sunr)*u
      if(con2.ge.2**24) con2=amod(con2,PI)
      xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
      ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)

      ang1=atan2(ypoin,xpoin)+angs
      r=sqrt(xpoin**2+ypoin**2)
      xpoin=r*cos(ang1)+xcenter
      ypoin=r*sin(ang1)+ycenter

      call draw2(xpoin,ypoin)
   enddo

   if(ifill.gt.0)then
     call closepoly()
     call polyfill(.false.)
   endif

   !call pop()

end subroutine spirograph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine smoot(xn,yn,ic)
!! public :: ismoo,ismoo1,ismoo2,ismoo3,perin
!@(#) draw smooth curve thru set up points using spline-fitting technique
!
! 4.4  subroutine smoot
!      ----------------
! general description:
!
! smoot is a fortran procedure which draws a smooth curve through a
! set of data points.  it accomplishes this by using a modified
! spline-fitting technique.  the procedure receives a single
! coordinate pair on each call and accumulates the points until it has
! received a sufficient number to compute a pair of cubic parametric
! equations for a smooth curve.  this accumulation method requires the
! user to specify an initial and a terminal call to the procedure.
!
! the smoot procedure operates in either of two modes:  smooth mode
! and plot mode.
!
! calling sequence:
!
!      call smoot (xpage,ypage,ipen)
!
!      xpage,ypage     are the coordinates, in inches, of a single point through
!                      which the pen moves.
!
!      ipen            determines the mode and action of the smoot procedure.
!
! detailed description:
!
! the first call to smoot must use an ipen value of 0 or -1 to put
! smoot in the smooth mode.
!
! if ipen = 0, xpage,ypage define the initial point (p(1)) on the
! curve.  the smoothing function ends at the last point (p(n)).  an
! open curve is produced.
!
! if ipen = -1, xpage,ypage are used to define the initial point (p(1))
! on the curve.  the smoothing function continues from the last point
! (p(n)) back to the initial point (p(1)).  a closed curve is produced.
!
! smooth mode:
!
! when smoot is in the smooth mode, ipen performs the following
! functions:
!
!  ipen = -2       xpage,ypage are used to define points p(2), p(3),...,
!                  p(n-1), and a smoothed curve is drawn through the points
!                  on the curve.
!
!  ipen = -3       xpage,ypage are used to define points p(2), p(3),
!                  ...,p(n-1), and the pen, in the up position, is moved
!                  through these points.  the smoothing function is
!                  maintained.
!
!  ipen = 2 or 3   the call is treated as a normal call plot_in
!                  (xpage,ypage,ipen), and the point is not considered a
!                  point on the curve.  the point of departure from the
!                  curve is the next-to-last point received by smoot, not
!                  the last point.
!
! when the next call to smoot with ipen = -2 or -3 is received, the pen
! is repositioned to the point where it left the smooth curve.  the
! smooth curve is then continued as though the calls with ipen = 2 or 3
! had not occurred.
!
! ipen <=(-24) is used for the terminal call while smoot is in the
! smooth mode.  xpage,ypage represent p(n).  the curve is finished, and
! the procedure returns to the plot mode.
!
!  plot mode:
!
! smoot is in the plot mode after receiving a terminal call.  when in
! plot mode, ipen = +-2 or +-3, the call is treated as a normal call
! plot (xpage,ypage,ipen).
!
! comments:
!
! when smoot is called while it is in the smooth mode, the pen is not
! moved until three points on an open curve or four points on a closed
! curve have been received.  for subsequent calls to smoot, the actual
! pen position is the next-to-last point received.
!
! calls to other plotting procedures may be intermixed with calls to
! smoot.  point-of-departure restrictions are the same as noted in the
! smooth mode description above.
!
! the first call to smoot must be with ipen = 0 or -1.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
!...procedure     smoot     v068a     07/30/69  product number 99013
!...copyright 1968 california computer products
!........10 char   plot      bcd       inches
!        the smooth routine simulates the plot routine with a new plot
!     mode (drawing a smooth curve to the new point).  the smooth mode
!     is initialized with the units digit of ic = 0 (for an open curve)
!     or = 1 (for a closed curve).
!                     the value of ic for smoothing is the negative of
!     the pen values for plotting.  there is, therefore, no re-origining
!     while smoothing.  using positive values for ic while smoothing
!     will be treated as a call to plot.  to end the curve and return to
!     the plot mode let ic be less than -23.
! earlier version of this procedure was
!     subroutine smooth (xn,yn,ic)
use M_draw
real :: xn
real :: yn
integer :: ic
integer :: ismoo
integer :: ismoo1
integer :: ismoo2
integer :: ismoo3
real :: perin
real :: ax
real :: ay
real :: bx
real :: by
real :: d
real :: d1
real :: d2
real :: d3
real :: dv
integer :: i
integer :: ipc
integer :: irep
integer :: isw
integer :: jsw
integer :: kc
integer :: lc
integer :: mc
integer :: n
integer :: nc
real :: pxn
real :: pyn
real :: sx1
real :: sx2
real :: sx3
real :: sy1
real :: sy2
real :: sy3
real :: t
real :: uux1
real :: uux2
real :: uuy1
real :: uuy2
real :: ux1
real :: ux2
real :: uy1
real :: uy2
real :: vx2
real :: vx3
real :: vy2
real :: vy3
real :: x
real :: x1
real :: x2
real :: x3
real :: y
real :: y1
real :: y2
real :: y3


      save
      data nc/0/,ipc/0/
      kc=ic-(ic/10)*10  ! kc is last digit of ic, of same sign as ic
      lc=nc-ipc
      pxn=xn
      pyn=yn
      irep=0
      if(kc)1,4,14
    1 continue
      if(kc+1)2,5,4
    2 continue
      if(ipc)3,3,14
    3 continue
      if(ic+24)10,10,17
    4 continue
      isw=-1
      goto 6
    5 continue
      isw=1
    6 continue
      jsw=-1
      nc=-kc/10*10
      x3=pxn
      y3=pyn
      mc=nc+3
    9 continue
      ipc=kc
      return
   10 continue
      if(ipc+1)11,13,13
   11 continue
      if(isw-1)12,15,14
   12 continue
      if(isw+1)14,16,14
   13 continue
      kc=nc+2
      ipc=1
      call plot_in (x3,y3,mc)
   14 continue
      call plot_in (pxn,pyn,kc)
      return
   15 continue
      irep=2
   16 continue
      irep=irep+1
      kc=1
   17 continue
      if(iabs(jsw)-1)14,18,14
   18 continue
       x1=x2
       y1=y2
       x2=x3
       y2=y3
       x3=pxn
       y3=pyn
      if((ipc+1).ge.0)then
         vx3=x3-x2
         vy3=y3-y2
         d3 = vx3*vx3+vy3*vy3
         sx1=x2
         sx2=x3
         sy1=y2
         sy2=y3
         goto 40
      endif
      if(jsw.eq.0)then
         goto 14
      elseif(jsw.lt.0)then
         if(isw.eq.0)then
            goto 14
         elseif(isw.lt.0)then
            vx2=x3-x2
            vy2=y3-y2
            call reflx (vx3,vy3,vx2,vy2)
            d2=vx2*vx2+vy2*vy2
            goto 26
         else
            goto 24
         endif
      endif
   23 continue
      jsw=1
   24 continue
      vx2=vx3
      vy2=vy3
      vx3=x3-x2
      vy3=y3-y2
   25 continue
      d2=d3
      ux1=ux2
      uy1=uy2
   26 continue
      d3=vx3*vx3+vy3*vy3
      ux2=d2*vx3+d3*vx2
      uy2=d2*vy3+d3*vy2
      dv = 1.0/sqrt(ux2*ux2+uy2*uy2+0.000001)
      ux2=dv*ux2
        uy2=dv*uy2
      if((isw-jsw).le.0)then
        if(jsw)23,14,28
      else
        jsw=1
        sx3=x3
        sy3=y3
        goto 40
      endif
   28 continue
      t=0.
      call getgp2(x,y)  ! d is current scaling factor (call where(x,y,d)
      d=1.0/100.0 ! playing with this to see if controls number of points
      if(abs(x1-x)-0.01*d.ge.0 .or. abs(y1-y)-0.01*d.ge.0)then
         call plot_in (x1,y1,mc)
      endif
      if(ipc+3.ne.0)then
         d=abs (ux1*vx2+uy1*vy2)
         d1=d
         uux1=d*ux1
         uuy1=d*uy1
         d=abs (ux2*vx2+uy2*vy2)
         uux2=d*ux2
         uuy2=d*uy2
         d=d+d1
         ax=uux2+uux1-vx2-vx2
         bx=vx2-uux1-ax
         ay=uuy2+uuy1-vy2-vy2
         by=vy2-uuy1-ay
!        perin is number of points to interpolate per unit distance between
!        point1 and point2
         n=int(perin*d+1.0)
         d=1.0/float(n)
         do 33 i=1,n
            t=t+d
            x=((ax*t+bx)*t+uux1)*t+x1
            y=((ay*t+by)*t+uuy1)*t+y1
            call plot_in (x,y,lc)
   33    continue
      endif
   40 continue
      if(irep.le.0)goto 9
      irep=irep-1
      if(isw.lt.0)then
         call reflx (vx3,vy3,vx2,vy2)
         x=vx3
         y=vy3
         vx3=vx2
         vy3=vy2
         vx2=x
         vy2=y
         x1=x2
         y1=y2
         goto 25
      elseif(isw.gt.0)then
         pxn=sx1
         pyn=sy1
         sx1=sx2
         sy1=sy2
         sx2=sx3
         sy2=sy3
         goto 18
      else         ! isw.eq.0
         goto 14
      endif
      return
      contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine reflx(vx1,vy1,vx2,vy2)
!@(#) internal routine called by SMOOT
implicit none
real,intent(in)  :: vx1
real,intent(in)  :: vy1
real,intent(out) :: vx2
real,intent(out) :: vy2
real ps
real ds
real ss
real temp
   ps=vy1*vy1
   ds=vx1*vx1
   ss = ds+ps+0.00001
   ds=ds-ps
   ps=2.0*vx1*vy1
   temp=(ps*vy2+vx2*ds)/ss
   vy2=(ps*vx2-vy2*ds)/ss
   vx2=temp
end subroutine reflx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine plot_in(x,y,lc)
use M_draw
!@(#) internal routine called by SMOOT
real,intent(in) :: x
real,intent(in) :: y
integer,intent(in) :: lc
   if(abs(lc).eq.3.and.ismoo1.ne.1)call move2(x,y)
   if(abs(lc).eq.3.and.ismoo1.eq.1)call draw2(x,y)
   if(abs(lc).eq.2)call draw2(x,y)
end subroutine plot_in
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end subroutine smoot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
end module M_drawplus
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
