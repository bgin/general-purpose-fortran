
!     good program to exercise color tables, and look at differences
!     when actual output device has a color table that is dynamic,
!     or only has a small color table (a frame in this program takes
!     at least 1200 colors to produce accurately).
!
!===============================================================================
!     SORT-OF BUG:
!     in current version of vogle color tables do not exist across
!      vexit/vinit boundaries;
!     so colors need remapped each time a new driver is called
!===============================================================================
!     BUG:
!     at least one verison of X11 driver runs out of color entries
!===============================================================================
!     DRIVER DIFFERENCES
!     if you do not have enough colors in table to display all of a page,
!     different devices will produce different results.
!     PostScript really just has a single pen and everything is written to the
!     file as made
!     so it will have all the colors in it.
!     P6 compiled up with 256 colors uses those colors at the end so everything
!     will look wrong
!     except the boxes drawn with the last 256 pens defined.
!     Should I wrap around when pen number requests are bad or ignore
!     so FIRST 256 entries would be right?
!     P6 compiled up with TRUECOLOR mode works
!===============================================================================
!     P6 driver needs to put each page on a different output device without
!     having to reinitialize
!===============================================================================
!     P6 driver gets errors in filling polygons when I close the polygon myself.
!     Leaves out one raster on Tru64 box.
!===============================================================================
!     problems getting vinit to prompt for driver on Tru64 when no parameter
!     supplied or set to blank
!===============================================================================
program huedisk
      use M_vogle
      real lightness, lightstep
      integer ii
      call voutput('look.hue')
      ! might want to set this for PPM drivers or X11, but too small for PostScript
      ! call prefsize(800,800)
      call vinit(' ')
      !DIGITAL memory fault call vinit()
      !DIGITAL does not prompt call vinit(' ')
      call color(0)
      call clear()
      call color(7)
      call biggest_ortho2(-110./2.,85./2.,-110./2.,110./2.)
      call polyfill(.true.)
      call vflush()
      call vsetflush(.false.)
      lightness=50
      call wheel(lightness)

      lightness=100.0
      lightstep=-5
      do ii=1,19
         call wheel(lightness)
         LIGHTNESS=LIGHTNESS+LIGHTSTEP
      enddo
      call vsetflush(.true.)
      call vexit()
end program huedisk
!=======================================================================--------
subroutine wheel(lightness) ! draw an entire wheel
      use M_vogle
      real lightness

      character(len=40) ::  line
      real hue_val 
      integer ii
      integer idum
      call textang(0.0)
      call color(7)
      call textsize(5.0,6.0)
      call font('times.r')
      call fixedwidth(.false.)
      call move2(0.0,103.0/2.0)
      call centertext(.true.)
      call linewidth(30)
      call drawstr('COLOR WHEEL')
      call linewidth(0)
      call textsize( 2.5,2.5)
      call font('futura.l')
      call move2(0.0,90.0/2.0)
      write(line,'("lightness=",f6.2)')lightness
      call linewidth(30)
      call drawstr(line)
      call linewidth(0)
      call fixedwidth(.true.)
      call textsize(1.5,1.5)
      hue_val=0
      do ii=120, 1,-1
         call slice(hue_val,lightness)
      enddo
      call centertext(.false.)
      idum=getkey()
      call color(0)
      call clear()
      call color(7)
      call vflush()
end subroutine wheel
!=======================================================================--------
subroutine slice(hue_val,lightness0) ! draw a slice
      use m_units, only: d2r
      use m_color, only: hue
      use M_vogle
      integer buffer
      real hue_val, ang_inc
      real lightness,lightness0

      character(len=40) ::  line
      real step
      real X1, X2, X3, X4
      real Y1, Y2, Y3, Y4

      integer maxcolors, current_color
      integer ir, ig, ib
      real r,g,b
      real saturation

      integer status
      integer icount
      real angle1, angle2
      real radius1, radius2, radius3, radius4

      integer,save  ::  color_count=0

      lightness=lightness0

      buffer=8
      ANG_INC=360.0/120
      angle1=hue_val-ANG_INC/2
      angle2=angle1+ANG_INC
      saturation=100
      radius1=32
      radius3=radius1+4
      radius4=radius1+7
      ! draw tic from wheel to start of angle label
      call color(7)
      call linewidth(40)
      call move2( radius1*cos(d2r(hue_val)), radius1*sin(d2r(hue_val)))
      call draw2( radius3*cos(d2r(hue_val)), radius3*sin(d2r(hue_val)))
      ! draw degree label at tic
      call textang(hue_val)
      call move2(radius4*cos(d2r(hue_val)),radius4*sin(d2r(hue_val)))
      write(line,'(f5.0)')hue_val
      call linewidth(20)
      call drawstr(line)
      call linewidth(0)
      step=radius1/(10)
      radius2=radius1-step
      ! draw a chunk in a slice
      MAXCOLORS=(2**(getdepth()-1))-buffer
      do icount=10+1,1,-1
         CURRENT_COLOR=MOD(color_count,MAXCOLORS)+buffer  ! add buffer to leave base colors alone
         color_count=color_count+1
         ! fancy mapcolor
         call hue("hls",hue_val,lightness,saturation,"rgb",r,g,b,status)
         ir=int(r*255.0/100.0+0.50)
         ig=int(g*255.0/100.0+0.50)
         ib=int(b*255.0/100.0+0.50)
!         write(*,*)'===== set color ',current_color,hue_val,lightness,saturation,ir,ig,ib
         call mapcolor(CURRENT_COLOR,ir,ig,ib)
         call color(CURRENT_COLOR)

         X1=cos(d2r(angle1))*radius2
         Y1=sin(d2r(angle1))*radius2
         X2=cos(d2r(angle1))*radius1
         Y2=sin(d2r(angle1))*radius1
         X3=cos(d2r(angle2))*radius2
         Y3=sin(d2r(angle2))*radius2
         X4=cos(d2r(angle2))*radius1
         Y4=sin(d2r(angle2))*radius1

         call makepoly()
         call move2(X1,Y1)
         call draw2(X2,Y2)
         call draw2(X4,Y4)
         call draw2(X3,Y3)
         ! should you close it yourself or let VOGLE close it?
         ! get errors if close it yourself, which you should not
         !call draw2(X1,Y1)
         call closepoly()

         saturation=saturation-100.0/10
         radius1=radius2
         radius2=radius1-step
      enddo
      hue_val=hue_val+ANG_INC
end subroutine slice
