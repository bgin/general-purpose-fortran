module M_drawinc
!>
!!##NAME
!!       M_drawinc(3f) - [M_drawinc] various routines based on the M_draw(3fm) module included into a module
!!
!!##SYNOPSIS
!!
!!       Use M_drawinc, only : illusion
!!
!!##DESCRIPTION
!!       Various routines that are based on the graphics module M_draw(3fm)
!!       have been collected into a module to provide default interfaces.
!===================================================================================================================================
private
public illusion
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   illusion(3f) - [M_drawinc:banner] draw a banner page with short labels at the compass points
!!
!!##SYNOPSIS
!!
!!   subroutine illusion(top,bottom,left,right)
!!
!!    character(len=*),intent(in) :: top
!!    character(len=*),intent(in) :: bottom
!!    character(len=*),intent(in) :: left
!!    character(len=*),intent(in) :: right
!!
!!##DESCRIPTION
!!    Draw a simple geometric illusion with short labels at the four compass points as a banner page
!!
!!##OPTIONS
!!    TOP      short top banner label
!!    BOTTOM   short bottom banner label
!!    LEFT     short left banner label
!!    RIGHT    short right banner label
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program testit
!!    use M_draw,      only : vinit, color, clear, backbuffer, getkey, vexit
!!    use M_drawplus,  only : page
!!    use M_drawinc,   only : illusion
!!    call vinit(' ')
!!    call page(0.0,4800.0,0.0,4800.0)
!!    call color(6)
!!    call clear()
!!    call color(5)
!!    idum=backbuffer()
!!    call illusion('TOP','BOTTOM','LEFT','RIGHT')
!!    idum=getkey()
!!    call vexit()
!!    end program testit
!===================================================================================================================================
subroutine illusion(top,bottom,left,right) !@(#) draw a simple geometric illusion
use M_draw
use M_xyplot, only : xy_obj12345
implicit none

character(len=*),intent(in) :: top
character(len=*),intent(in) :: bottom
character(len=*),intent(in) :: left
character(len=*),intent(in) :: right

integer,save                :: iarx(9,16)  ! fill color, number of points
integer,save                :: iary(9,16)  ! x,y values for making polygons
integer                     :: i10, i20, i30
integer                     :: i
real                        :: x1, y1
real                        :: x, y
real                        :: smid
real                        :: smax
real                        :: smin
real                        :: xx
!-----------------------------------------------------------------------------------------------------------------------------------
data(iarx(i,1), iary(i,1),i=1,9)/  1, 9, 1950,1350, 4350,1350, 4350,3150, 3450,3150, 3450,2850, 4050,2850, 4050,1650, 1950,1650/
data(iarx(i,2), iary(i,2),i=1,5)/  1, 5, 450,1650,  750,1350,  1350,1350, 1350,1650/
data(iarx(i,3), iary(i,3),i=1,5)/  1, 5, 750,3150,  1050,2850, 2850,2850, 2850,3150/
data(iarx(i,4), iary(i,4),i=1,5)/  1, 5, 750,3150,  750,1950,  1050,1950, 1050,2850/
data(iarx(i,5), iary(i,5),i=1,5)/  2, 5, 3750,1950, 4050,1650, 4050,2850, 3750,2850/
data(iarx(i,6), iary(i,6),i=1,5)/  2, 5, 1950,1950, 1950,1650, 4050,1650, 3750,1950/
data(iarx(i,7), iary(i,7),i=1,9)/  2, 9, 1350,1650, 1350,1950, 750,1950,  750,3150,  2850,3150, 2850,3450, 450,3450,  450,1650/
data(iarx(i,8), iary(i,8),i=1,5)/  2, 5, 4350,3150, 4050,3450, 3450,3450, 3450,3150/
data(iarx(i,9), iary(i,9),i=1,9)/  3, 9, 3450,1950, 3450,4350, 1650,4350, 1650,3450, 1950,3450, 1950,4050, 3150,4050, 3150,1950/
data(iarx(i,10),iary(i,10),i=1,5)/ 3, 5, 3150,450,  3450,750,  3450,1350, 3150,1350/
data(iarx(i,11),iary(i,11),i=1,5)/ 3, 5, 1650,750,  1950,1050, 1950,2850, 1650,2850/
data(iarx(i,12),iary(i,12),i=1,5)/ 3, 5, 1650,750,  2850,750,  2850,1050, 1950,1050/
data(iarx(i,13),iary(i,13),i=1,5)/ 4, 5, 2850,1950, 3150,1950, 3150,4050, 2850,3750/
data(iarx(i,14),iary(i,14),i=1,9)/ 4, 9, 3150,1350, 2850,1350, 2850,750,  1650,750,  1650,2850, 1350,2850, 1350,450,  3150,450/
data(iarx(i,15),iary(i,15),i=1,5)/ 4, 5, 2850,3750, 3150,4050, 1950,4050, 1950,3750/
data(iarx(i,16),iary(i,16),i=1,5)/ 4, 5, 1650,4350, 1350,4050, 1350,3450, 1650,3450/
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_obj12345('before')   ! begin M_DRAW object 12345

   call linewidth(30)
   do i10=1,16               ! draw each polygon
      call polyfill(.true.)
      call color(iarx(1,i10))
      do i30=1,2             ! make filled polygon on first pass, outlined on second
         call makepoly()
         x1=real(iarx(2,i10))
         y1=real(iary(2,i10))
         call move2(x1,y1)
         do i20=3,iary(1,i10)
            x=real(iarx(i20,i10))
            y=real(iary(i20,i10))
            call draw2(x,y)
         enddo
         call draw2(x1,y1)
         call closepoly()
         call color(7)
         call polyfill(.false.)
      enddo
   enddo

   call linewidth(75)
   call textsize(410.0,400.0)
   call xcentertext()
   call color(7)
   smid=(450.0+4350.0)/2.0
   smax=4350.0
   smin=450.0
   xx=130.0

   call textang(0.0)
   call move2(smid+xx,smax)
   call drawstr(top)

   call textang(180.0)
   call move2(smid-xx,smin)
   call drawstr(bottom)

   call textang(90.0)
   call move2(smin,smid+xx)
   call drawstr(left)

   call textang(270.0)
   call move2(smax,smid-xx)
   call drawstr(right)

   call textang(0.0)
   call xy_obj12345('after')

end subroutine illusion
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_drawinc
