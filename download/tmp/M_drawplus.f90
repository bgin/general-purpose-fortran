!>
!!##NAME
!!    M_DRAWPLUS(3f) - [M_DRAW] Additional routines using the M_DRAW graphics library
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
!!    subroutine biggest_ortho2 (left, right, bottom, top)
!!
!!    MATRIX STACK ROUTINES
!!
!!    VIEWPOINT ROUTINES
!!
!!    MOVE ROUTINES
!!
!!    LINESTYLE ROUTINES
!!
!!    DRAW ROUTINES
!!
!!    ARCS AND CIRCLES
!!
!!    CURVE ROUTINES
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
!!    subroutine jucall(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
!!
!!    DOUBLE BUFFERING
!!
!!    POSITION ROUTINES
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
module M_drawplus
use ISO_C_BINDING
use M_draw
implicit none
private
public :: biggest_ortho2
public :: barcode
public :: jucall
public :: pop
public :: push
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!>
!!##NAME
!!    jucall(3f) - [M_DRAW:] invoke object with specified transformations
!!
!!##SYNOPSIS
!!
!!       subroutine jucall(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
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
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
subroutine jucall(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
character(len=*),parameter::ident="@(#)jucall(3f) invoke object with specified transformation"

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

end subroutine jucall
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    pop(3f) - [M_DRAW:] call popviewport(), popmatrix(), popattributes()
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
!!    push(3f) - [M_DRAW:] call pushviewport(), pushmatrix(), pushattributes()
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
!!    biggest_ortho2(3f) - [M_DRAW] set window into largest viewport available assuming 1-to-1 correspondence of x and y coordinates
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
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
subroutine biggest_ortho2(xsmall,xlarge,ysmall,ylarge)
!----------------------------------------------------------------------------------------------------------------------------------!
use M_journal, only : journal

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
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
end module M_drawplus
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
