!>
!!##NAME
!!    M_DRAW(3f) - [M_DRAW] The M_DRAW graphics library
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!
!!    M_DRAW is a portable public-domain device-independent graphics library
!!    intended for being called from Fortran that is based on VOGLE (from
!!    the The University of Melbourne) that is loosely based on the Silicon
!!    Graphics Iris GL library. It was also partly inspired by the DIGS
!!    library developed at the U.S. Naval Academy under the guidance of
!!    Prof David Rogers.
!!
!!    Many output devices are available:
!!
!!      * FrameMaker MIF 3.0 (Maker Interchange File) driver.
!!      * Adobe PDF driver.
!!      * HTML5 Canvas driver.
!!      * SVG driver.
!!      * A PCL5/HPGL2 driver that supports prefsize() calls.
!!      * Monochrome PBM (Poskazner bitmap P1 and P4 formats) and X11
!!        bitmap driver.
!!      * Color PBM (Poskazner pixmap P3 and P6 formats). If you have the
!!        pbmplus package you can use it to make M_DRAW appear to write
!!        any format pbmplus writes (assuming your system supports the
!!        popen(3c) function).
!!      * A clear-text CGM (Computer Graphics Metafile) driver.
!!      * A different (color) PostScript driver.
!!      * A driver for Microsoft VML (Vector Markup Language)
!!
!!    M_DRAW is intended to produce simple graphics composed of line drawings and
!!    polygon fills in two and three dimensions. It handles circles, curves,
!!    arcs, patches, polygons, and software text in a device independent
!!    fashion. Simple hidden line removal is also available via polygon
!!    backfacing. Access to hardware text and double buffering of drawings
!!    depends on the driver.
!!
!!    M_DRAW is callable from C, FORTRAN 77, Fortran 90+ , and Pascal on most
!!    platforms.
!!
!!    M_DRAW is portable. It has been used on Cray UNICOS, SGI IRIX64 and IRIX,
!!    IBM AIX, NeXT, Digital ULTRIX and Digital Unix, Sun SunOS and Solaris,
!!    Linux, Compaq Tru64 UNIX, HP HP-UX, and CygWin. It has been run on
!!    additional platforms including most PCs.
!!
!!    The original source's ownership statement
!!
!!       This software is public domain and may be used for any purpose commercial
!!       or otherwise. It is offered without any guarantee as to its suitability
!!       for any purpose or as to the sanity of its writers. The authors do ask
!!       that the source is passed on to anyone that requests a copy, and that
!!       people who get copies don't go round claiming they wrote it. Use at your
!!       own risk.
!!
!!
!!##LIBRARY FUNCTION DESCRIPTIONS
!!
!!    DEVICE ROUTINES
!!    vinit(device)                    Initialise device
!!    vexit()                          Reset window/terminal (must be last routine called)
!!    voutput(path)                    Redirect output from *next* vinit to file
!!    vnewdev(device)                  Reinitialize to use new device without changing
!!    vgetdev(device)                  Get name of current device
!!    pushdev(device)                  push current device onto a stack
!!    popdev(device)                   pop device from stack created by pushdev.
!!    getdepth()                       Return number of bit planes (color planes)
!!
!!    ROUTINES FOR SETTING UP WINDOWS
!!    prefposition(x, y)               Specify preferred position of window
!!    prefsize(width, height)          Specify preferred width and height of window
!!
!!    Some devices are basically window oriented - like sunview and X11. You
!!    can give M_DRAW some information on the window that it will use with these
!!    routines. These can make your code very device independent. Both routines
!!    take arguments which are in device space. (0, 0) is the top left hand
!!    corner in device space. To have any effect these routines must be called
!!    before vinit. For the X11 device, an entry may be made in your .Xdefaults
!!    file or loaded in with the xrdb(1) command:
!!
!!       xrdb <<\end_of_file
!!       ! X11 Windows fonts to use for "small" and "large" fonts
!!       draw*smallfont: fixed
!!       draw*largefont: 9x15
!!       ! title on decoration bar for the window
!!       draw*title: My M_DRAW program
!!       ! window geometry and position,
!!       ! overridden by prefsize(3c) and prefposition(3c)
!!       draw.Geometry: =500x500-10+20
!!       end_of_file
!!
!!    (where you specify your geometry as you please).
!!
!!    CLIPPING ROUTINES
!!    clipping(onoff)                  Turn clipping on or off
!!
!!    COLOR ROUTINES
!!    clear()                          Clears screen to current color
!!    color(col)                       Set current color
!!    mapcolor(indx, red, green, blue) Set color map index
!!
!!    INTERACTIVE ROUTINES
!!    getkey()                         Return ASCII ordinal of next key typed
!!    checkkey()                       Returns zero if no key is pressed or ASCII ordinal
!!    getstring(bcol, string)          Read in a string, echoing it in current font
!!    locator(xaddr, yaddr)            Find out where cursor is
!!    slocator(xaddr, yaddr)           Find out where cursor is in screen coordinates
!!
!!    FLUSHING
!!    vsetflush(yesno)                 Set global flushing status
!!    vflush()                         Call device flush or syncronisation routine
!!
!!    On some devices (particularly X11) considerable speedups in display
!!    can be achieved by not flushing each graphics primitive call to the
!!    actual display until necessary. VOGL automatically delays flushing in
!!    the following cases:
!!
!!      * Within a callobj() call.
!!      * Within curves and patches.
!!      * Within Hershey software text.
!!      * When double buffering (the flush is only done within swapbuffers).
!!
!!    There are two user routines that can be used to control flushing.
!!
!!    VIEWPORT ROUTINES
!!    viewport(left, right, bottom, top)     Specify which part of screen to draw in
!!    pushviewport()                         Save current viewport
!!    popviewport()                          Retrieve last viewport
!!    getviewport(left, right, bottom,top)   Returns limits of current viewport in screen coordinates
!!    expandviewport()                       use the entire device viewport
!!    unexpandviewport()                     undo expandviewport(3f)
!!
!!
!!    Viewpoint routines alter the current transformation matrix.
!!
!!    GETTING THE ASPECT DETAILS
!!    getaspect()                      Returns the ratio height over width of the display device.
!!    getfactors(wfact, hfact)         Returns width over min(width of device, height of device) and height over min(width of
!!                                     device, height of device).
!!    getdisplaysize(w, h)             Returns width and height of device in device units
!!
!!
!!    Often the screen is not perfectly square and it would be nice to use
!!    the extra space without having to turn clipping off. The following
!!    routines are provided to get the values needed to adjust the calls
!!    to viewport, etc as needed.
!!
!!    ATTRIBUTE STACK ROUTINES
!!    pushattributes()                 Save the current attributes on the attribute stack.
!!    popattributes()                  Restore attributes to what they were at last pushattributes().
!!
!!
!!    The attribute stack contains details such as current color, filling,
!!    hatching, centered, fixedwidth, text height, text width, and the
!!    current font. If you need to prevent object calls from changing these,
!!    use pushattributes before the call and popattributes after.
!!
!!    PROJECTION ROUTINES
!!    ortho(left, right, bottom, top,near,far)    Define x,y,z clipping planes.
!!    ortho2(left, right, bottom, top)            Define x and y clipping planes.
!!    perspective(fov, aspect, near, far)         Specify perspective viewing pyramid
!!    window(left, right, bot, top, near,far)     Specify a perspective viewing pyramid
!!
!!
!!    All the projection routines define a new transformation matrix, and
!!    consequently the world units. Parallel projections are defined by
!!    ortho or ortho2. Perspective projections can be defined by perspective
!!    and window.
!!
!!    MATRIX STACK ROUTINES
!!    pushmatrix()                     Save the current transformation matrix on the matrix stack.
!!    popmatrix()                      Reinstall the last matrix pushed
!!
!!    VIEWPOINT ROUTINES
!!    polarview(dist, azim, inc, twist)      Specify the viewer's position in polar coordinates
!!    up(x, y, z)                            Specify the world up.
!!    lookat(vx, vy, vz, px, py, pz,twist)   Specify the viewer's position
!!
!!    MOVE ROUTINES
!!    move(x, y, z)                    Move current graphics position to (x, y, z)
!!    rmove(deltax, deltay, deltaz)    Relative move
!!    move2(x, y)                      Move graphics position to point (x, y)
!!    rmove2(deltax, deltay)           Relative move in world units.
!!    smove2(x, y)                     Move current graphics position in screen coordinates (-1.0 to 1.0).
!!    rsmove2(deltax, deltay)          Relative move in screen units (-1.0 to 1.0).
!!
!!    LINESTYLE ROUTINES
!!    linewidth()                      set line width in rasters
!!    dashcode()                       set dash pattern length
!!    linestyle()                      set the line dash pattern
!!
!!    Linestyles are specified by giving a nominal length of a single
!!    dash and a character string consisting of 1's and 0's (zeros) that
!!    specify when to draw a dash and when not to draw a dash. Linestyles
!!    will follow curves and "go around" corners. If a linestyle is set or
!!    reset, the accumulated information as to where on a curve (or line)
!!    a dash is to be draw is also reset.
!!
!!    For EXAMPLE, with a nominal view of -1 to 1, setting the dash length
!!    to 0.5, and the linestyle to '11010' would draw a line(or curve) with
!!    a 1.0 unit solid part, followed by a 0.5 unit blank part followed by
!!    a 0.5 unit solid part followed by a 0.5 unit blank part. The linestyle
!!    would then repeat itself.
!!
!!    The dash sizes are affected by the current viewport/transformation
!!    scaling factors, meaning that in perspective, the dashes look smaller
!!    the farther away they are.
!!
!!    DRAW ROUTINES
!!    draw(x, y, z)                    Draw from current graphics position to (x, y, z)
!!    rdraw(deltax, deltay, deltaz)    Relative draw
!!    draw2(x, y)                      Draw from current graphics position to point (x, y)
!!    rdraw2(deltax,deltay)            Relative draw
!!    sdraw2(x, y)                     Draw in screen coordinates (-1.0 to 1.0).
!!    rsdraw2(deltax, deltay)          Relative draw in screen units (-1.0 to 1.0).
!!
!!    ARCS AND CIRCLES
!!    circleprecision(nsegs)                  Set number of line segments in a circle. Default is 32.
!!    arc(x, y, radius, startang, endang)     Draw an arc in world units.
!!    sector(x, y, radius, startang,endang)   Draw a sector. Note: sectors are polygons.
!!    circle(x, y, radius)                    Draw a circle. Note: circles are polygons.
!!
!!    When creating arcs and sectors note that angles are
!!    measured in degrees; where zero(0) is the positive X axis in a
!!    right-handed Cartesian coordinate system and positive angles sweep
!!    counterclockwise. If filling sectors or circles (As described in the
!!    section on polygons) hatch pitch is measured in world coordinates
!!    and is initially set to 0.1. The intial hatch angle is zero(0).
!!
!!    CURVE ROUTINES
!!    curvebasis(basis)           Define a basis matrix for a curve.
!!    curveprecision(nsegs)       Define number of line segments used to draw a curve.
!!    rcurve(geom)                Draw a rational curve.
!!    curve(geom)                 Draw a curve.
!!    curven(n, geom)             Draw n - 3 overlapping curve segments. Note: n must be at least 4.
!!
!!    RECTANGLES AND GENERAL POLYGON ROUTINES
!!    rect(x1, y1, x2, y2)        Draw a rectangle.
!!    polyfill(onoff)             Set the polygon fill flag
!!    polyhatch(onoff)            Set the polygon hatch flag
!!    hatchang(angle)             Set the angle of the hatch lines.
!!    hatchpitch(pitch)           Set the distance between hatch lines.
!!    poly2(n, points)            Construct an (x, y) polygon from an array of points
!!    poly(n, points)             Construct a polygon from an array of points
!!    makepoly()                  opens polygon constructed by a series of move-draws and closed by closepoly
!!    closepoly()                 Terminates a polygon opened by makepoly.
!!    backface(onoff)             Turns on culling of backfacing polygons.
!!    backfacedir(clockwise)      Sets backfacing direction to clockwise or anti-clockwise
!!
!!    A polygon is composed of a number of coplanar line segments connected
!!    end to end to form a closed shape.
!!
!!    In M_DRAW curves are estimated by a series of line segments, and thus
!!    may be included easily into polygons.
!!
!!    Regular    A polygon with all sides and interior angles the same. Regular
!!               polygons are always convex. See Regular Polygons
!!    Irregular  Each side may a different length, each angle may be a different
!!               measure. The opposite of a regular polygon. See Irregular Polygons
!!    Convex     All interior angles less than 180 ,and all vertices 'point
!!               outwards' away from the interior. The opposite of concave. Regular
!!               polygons are always convex. See Convex Polygons
!!    Concave    One or more interior angles greater than 180 . Some vertices
!!               push 'inwards' towards the interior of the polygon. The opposite
!!               of convex.
!!    Self-intersecting or Crossed  A polygon where one or more sides crosses back over another side,
!!                                  creating multiple smaller polygons. Most of the properties and
!!                                  theorems concerning polygons do not apply to this shape. It is
!!                                  best considered as several separate polygons. A polygon that in
!!                                  not self-intersecting in this way is called a simple polygon.
!!
!!
!!    TEXT ROUTINES
!!    font(fontname)                   Set the current font
!!    numchars()                       Return number of characters in the current SOFTWARE font.
!!    textsize(width, height)          Set maximum size of a character in the current SOFTWARE font.
!!    textang(ang)                     Set the SOFTWARE text angle.
!!    fixedwidth(onoff)                Turns fixedwidth mode on or off for SOFTWARE fonts.
!!    centertext(onoff)                Turns centertext mode on or off for SOFTWARE fonts.
!!    getcharsize(c, width, height)    Get the width and height of a character.
!!    getfontdec()                     Return size of maximum font descender
!!    getfontsize(width, height)       Get maximum width and height of a character in a font.
!!    drawchar(c)                      Draw the character c and update current position.
!!    drawstr(str)                     Draw the text in string at the current position.
!!    strlength(str)                   Return the length of the string s
!!    boxtext(x, y, l, h, s)           Draw the SOFTWARE string s so that it fits in the imaginary box
!!    boxfit(x, y, l, h, s)            resize the SOFTWARE text size so it fits in a box
!!    textjustify(val)                 general text justification (C only)
!!    leftjustify()                    left justify text
!!    rightjustify()                   right justify text
!!    xcentertext()                    center text in the X direction
!!    topjustify()                     top justify text
!!    bottomjustify()                  bottom justify text
!!    ycentertext()                    center text in the Y direction
!!    textslant()                      Defines the obliqueness of the fonts.
!!    textweight()                     Defines the weight of the fonts.
!!
!!    M_DRAW supports hardware and software fonts. The software fonts are based
!!    on the character set digitized by Dr Allen V. Hershey while working at
!!    the U. S. National Bureau of Standards. Exactly what hardware fonts are
!!    supported depends on the device, but it is guaranteed that the names
!!    "large" and "small" will result in something readable. For X11 displays
!!    the default large and small fonts used by the program can be overridden
!!    by placing the following defaults in the ~/.Xdefaults file:
!!
!!      draw.smallfont: X11-font-name
!!      draw.largefont: X11-font-name
!!
!!    It is noted here that text is always assumed to be drawn parallel to the
!!    (x, y) plane, using whatever the current z coordinate is. The following
!!    software fonts are supported:
!!
!!       astrology       cursive         cyrillic        futura.l
!!       futura.m        gothic.eng      gothic.ger      gothic.ita
!!       greek           markers         math.low        math.upp
!!       meteorology     music           script          symbolic
!!       times.g         times.i         times.ib        times.r
!!       times.rb        japanese
!!
!!    A markers font "markers" is also provided for doing markers - you need
!!    to have centertext mode on for this to give sensible results when placing
!!    the markers.
!!
!!    If the environment variable "M_DRAW_FONTPATH" is set M_DRAW looks for the software
!!    fonts in the directory given by this value.
!!
!!    the default font is futura.l
!!
!!    TRANSFORMATION ROUTINES
!!    translate(x, y, z)          Set up a translation.
!!    scale(x, y, z)              Set up scaling factors in x, y, and z axis.
!!    rotate(angle, axis)         Set up a rotation in axis axis where axis is one of 'x','y', or 'z'.
!!
!!    All transformations are cumulative, so if you rotate something and
!!    then do a translate you are translating relative to the rotated
!!    axes. If you need to preserve the current transformation matrix use
!!    pushmatrix(), do the drawing, and then call popmatrix() to get back
!!    where you were before.
!!
!!    When doing transformations, ensure your objects remain in the viewing
!!    volume or they will be clipped. See routines such as ortho(3) for
!!    more information.
!!
!!    PATCH ROUTINES
!!    patchbasis(tbasis, ubasis)  Define the t and u basis matrices of a patch.
!!    patchprecision(tseg, useg)  Set minimum number of line segments making up curves in a patch.
!!    patchcurves(nt, nu)         Set the number of curves making up a patch.
!!    rpatch(gx, gy, gz, gw)      Draws a rational patch in the current basis, according to the geometry matrices gx, gy, gz, and gw.
!!    patch(gx, gy, gz)           Draws a patch in the current basis, according to the geometry matrices gx, gy, and gz.
!!
!!    POINT ROUTINES
!!    point(x, y, z)              Draw a point at x, y, z
!!    point2(x, y)                Draw a point at x, y.
!!
!!    points are drawn with the current color and linewidth. Points are
!!    currently device-specific and may appear as circles, squares, or
!!    not at all; as they are generated by a zero-length vector using the
!!    hardware line style.
!!
!!    OBJECT ROUTINES
!!    makeobj(n)                  Commence the object number n.
!!    closeobj()                  Close the current object.
!!    genobj()                    Returns a unique object identifier.
!!    getopenobj()                Return the number of the current object.
!!    callobj(n)                  Draw object number n.
!!    isobj(n)                    Returns non-zero if there is an object of number n.
!!    delobj(n)                   Delete the object number n.
!!    loadobj(n, filename)        Load the object in the file filename as object number n.
!!    saveobj(n, filename)        Save object number n into file filename. Does NOT save objects called inside object n.
!!
!!    Objects are graphical entities created by the drawing routines called
!!    between makeobj and closeobj. Objects may be called from within other
!!    objects. When an object is created most of the calculations required
!!    by the drawing routines called within it are done up to where the
!!    calculations involve the current transformation matrix. So if you need to
!!    draw the same thing several times on the screen but in different places
!!    it is faster to use objects than to call the appropriate drawing routines
!!    each time. Objects also have the advantage of being saveable to a file,
!!    from where they can be reloaded for later reuse. Routines which draw
!!    or move in screen coordinates, or change device, cannot be included
!!    in objects.
!!
!!    DOUBLE BUFFERING
!!    backbuffer()                Draw in the backbuffer. Returns -1 if the device is not up to it.
!!    frontbuffer()               Draw in the front buffer. This will always work.
!!    swapbuffers()               Swap the front and back buffers.
!!
!!    Where possible M_DRAW allows for front and back buffers to enable
!!    things like animation and smooth updating of the screen. The routine
!!    backbuffer is used to initialise double buffering.
!!
!!    POSITION ROUTINES
!!    getgp(x, y, z)              Gets the current graphics position
!!    getgpt(x, y, z, w)          Gets the current transformed graphics position in world coords.
!!    getgp2(x, y)                Gets the current graphics position
!!    sgetgp2(x, y)               Gets the current screen graphics position in screen coords (-1 to 1)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_DRAW
!!
!!    use M_DRAW
!!    use M_units,    only : cosd, sind
!!    implicit none
!!
!!       integer  :: i
!!       integer  :: j
!!       integer  :: icolor
!!       integer  :: ipaws
!!
!!       ! initialize image
!!       call prefsize(400,400)  ! set size before starting
!!       call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
!!       call color(0)
!!       call clear()            ! clear to color 0
!!       call color(7)
!!
!!       ! map area of virtual world to specified device area
!!       ! notice Y-axis for viewport is zero at TOP
!!          ! viewport(left, right, bottom, top)
!!       call viewport(0.0,  400.0,  400.0, 0.0)
!!       ! define the virtual world area we want to work in
!!           !ortho2(left, right, bottom,   top)
!!       call ortho2(0.0,  400.0,    0.0, 400.0)
!!       ! the drawing routines use these world units
!!
!!       ! draw polar grids
!!       call linewidth(100)
!!       call color(14)
!!       call target(200.0,200.0,200.0)
!!
!!       call linewidth(75)
!!       call color(0)
!!       call target(100.0,200.0,50.0)
!!
!!       ! draw some lines
!!       call color(1)
!!       call linewidth(200)
!!       call line(1.0,1.0,400.0,400.0)
!!
!!       call color(4)
!!       call line(350.0,200.0,350.0,300.0)
!!
!!       ! print some text
!!       call color(1)
!!       !call hershey(x,y,height,itext,theta,ntext)
!!       call linewidth(125)
!!       call color(7)
!!       call linewidth(25)
!!       call linewidth(100)
!!
!!       call linewidth(50)
!!
!!       ipaws=getkey()
!!       call vexit()
!!
!!    contains
!!
!!       subroutine target(xc,yc,rc)
!!       use M_units,    only : cosd, sind
!!       real     :: xc,yc,rc
!!       integer  :: i
!!       real     :: x,y
!!          do i=0,360,10
!!             x=rc*cosd(i)
!!             y=rc*sind(i)
!!             call line(xc,yc,xc+x,yc+y)
!!          enddo
!!          do i=1,int(rc),10
!!             call circle(xc,yc,real(i))
!!          enddo
!!       end subroutine target
!!    end program demo_M_DRAW
!!
!!##BUGS
!!
!!    Polygon hatching will give unexpected results unless the polygon is
!!    initially defined in the X-Y plane.
!!
!!    Double buffering isn't supported on all devices.
!!
!!    We don't recommend the use of the smove/sdraw routines.
!!
!!    The yobbarays may be turned on or they may be turned off.
!!
!!    When creating an object, current position and text size are not
!!    actually changed so almost any query routine to get position or font
!!    size or whatever will not work properly.
!===================================================================================================================================
!>
!!##NAME
!!    prefposition(3f) - [M_DRAW:WINDOW_SETUP] Specify preferred position of window
!!
!!##SYNOPSIS
!!
!!    subroutine prefposition(x, y)
!!    integer,intent(in) ::  x, y
!!
!!##DESCRIPTION
!!    Specify the preferred position of the window opened by the *next* vinit.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefsize
!!      use M_DRAW, only    : prefsize, vinit, ortho2, clear, getkey, prefposition
!!      use M_DRAW, only    : move2, draw2, vexit, color
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(60,40)
!!      call prefposition(100,100)
!!
!!      call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(0)
!!      call clear()
!!      call color(1)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!      call vexit()
!!
!!      end program demo_prefsize
!===================================================================================================================================
!>
!!##NAME
!!    prefsize(3f) - [M_DRAW:WINDOW_SETUP] Specify preferred width and height of window
!!
!!##SYNOPSIS
!!
!!         subroutine prefsize(width, height)
!!         integer width, height
!!##DESCRIPTION
!!
!!    Specify the preferred width and height of the window opened by the
!!    *next* vinit.
!===================================================================================================================================
!>
!!##NAME
!!    prefsize(3f) - [M_DRAW] specify size of output device in physical device units
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine prefsize(width, height)
!!    integer width, height
!!
!!##DESCRIPTION
!!    Specify the preferred width and height of the device output surface
!!    opened by the *next* vinit(3f).
!!
!!##OPTIONS
!!    WIDTH   width of device to create when vinit(3f) is called
!!    HEIGHT  height of device to create when vinit(3f) is called
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefsize
!!      use M_DRAW, only: prefsize, vinit, ortho2, clear, getkey
!!      use M_DRAW, only: move2, draw2, vexit, color
!!      implicit none
!!      integer :: ipaws
!!         ! make first file with one size
!!         call prefsize(60*2,40*2)
!!         call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!         call picture()
!!         ipaws=getkey()
!!         call vexit()
!!
!!         ! make second file with another size
!!         call prefsize(60*3,40*3)
!!         call vinit(' ')
!!         call picture()
!!         ipaws=getkey()
!!         call vexit()
!!      contains
!!      subroutine picture
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call color(0)
!!         call clear()
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!      end subroutine picture
!!      end program demo_prefsize
!===================================================================================================================================
!>
!!##NAME
!!    vinit(3f) - [M_DRAW:DEVICE] Initialise device
!!
!!##SYNOPSIS
!!
!!          subroutine vinit(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!! Initialise the device.
!!
!!
!!  Note 1 :- Currently available devices are:
!!
!!        INTERACTIVE DEVICES:
!!        --------------------
!!        PC   - native MSW PC driver; only tested from CygWin
!!        X11  - X11 windows (Black background)
!!        x11  - X11 windows (White background)
!!        tek  - tektronix 4010 and compatibles
!!        xtek - X11 xterm Tektronix 4010 emulator
!!
!!        PRINTERS and PLOTTERS:
!!        ----------------------
!!           PostScript:
!!              [p]psm or [p]postscript - monochrome PostScript
!!              [p]psg - grayscale PostScript
!!              [p]psc - color PostScript
!!           HPGL:
!!              hpgl - HP Graphics language and compatibles
!!              [p]hpgl2 - HPGL level 2 (obeys prefsize calls)
!!           PCL:
!!              [p]pclland  - monochrome PCL5 (obeys prefsize calls)
!!              [p]pclport  - monochrome PCL5 (obeys prefsize calls)
!!              pcl5land (color PCL5 landscape)
!!              pcl5port (color PCL5 portrait)
!!
!!        PIXMAPS (color)  and BITMAPS (monochrome)
!!        -----------------------------------------
!!        char   - An ASCII file that can be displayed to most
!!                 xterm(1) terminal emulators that support
!!                 color
!!        p1/pbm - Poskanzer (pbmplus/netplus) portable
!!                 ASCII bitmap file
!!        p3/ppm - Poskanzer portable ASCII pixmap file
!!        p4     - Poskanzer portable binary bitmap file
!!        p6     - Poskanzer portable binary pixmap file
!!        xbm    - X11 bitmap file
!!        bm     - bitmap format for atobm(1)
!!
!!        METAFILES
!!        ---------
!!        PRODUCT INPUT FILES:
!!        mif      - FrameMaker Interchange Format 3.0 (MIF) files
!!                   (16 colors)
!!        mif4     - FrameMaker Interchange Format 4.0 (MIF) files
!!                   (user-definable colors, but breaks a MIF rule)
!!        xfig     - X11 xfig(1) figure utility
!!
!!        METAFILES WITH POST_PROCESSORS/CONVERTERS:
!!        cgmt     - a clear-text CGM (Computer Graphics Metatfile)
!!        gnu      - GNU plot(1) metafile
!!        pdf      - Adobe Public Document Format
!!        unixplot - Unix plot(1) metafile
!!
!!        BROWSER FILES:
!!        canvas   - HTML5 CANVAS graphics element file
!!        svg      - Scalable Vector Graphics
!!        usemap   - HTML image map
!!        vml      - Microsoft Vector Markup Language
!!
!!        FILES:
!!        vog      - M_DRAW low level call record (debug)
!!
!!        OTHER:
!!        ------
!!        fti   - SGI vector-based icons
!!        null  - no output
!!
!!      :- Drivers I've dropped but code is there for
!!      ---------------------------------------------
!!
!!        grwin (minGW GRwin PC interface)
!!        decX11 - the decstation window manager
!!        dxy - roland DXY plotter language
!!        sun - Sun workstations running sunview
!!        next - NeXTStep and other NeXT platforms
!!        apollo - Apollo workstations
!!
!!      :- Drivers I've dropped but are in the original distribution
!!      ------------------------------------------------------------
!!
!!        hercules - IBM PC hercules graphics card
!!        cga - IBM PC cga graphics card
!!        ega - IBM PC ega graphics card
!!        vga - IBM PC vga graphics card
!!        sigma - IBM PC sigma graphics card.
!!        mswin - IBM PC Microsoft Windows.
!!
!!     Note 2 :- If device is a NULL or a null string the value
!!          of the environment variable "M_DRAW_DEVICE" is taken as the
!!          device type to be opened. The format of the variable is
!!
!!             M_DRAW_DEVICE [ xsize [ ysize [ xoffset [ yoffset ] ] ]
!!
!!          That is, if present xsize and ysize will be used
!!          in a call to prefsize(3c), and xoffset and yoffset will
!!          be used in a call to preposition(3c).
!!
!!     Note 3 :- after vinit() it is wise to explicitly clear the screen.
!!     e.g.: in C
!!          color(BLACK);
!!          clear();
!!
!!     or    in Fortran
!!          call color(BLACK)
!!          call clear
!!
!!     or    in Pascal
!!          Color(BLACK);
!!          Clear;
!!
!!     Note 4 :  Sun, X11, decX11, apollo, hercules, cga and ega support
!!               double buffering.
!!
!!
!!##NOTES:
!!
!! gnu
!!
!!    The GNU plotutils package includes a program called plot(1) that
!!    can read in the gnu metafile and render images on an X11 display, PNG
!!    (Portable Network Graphics) format, portable anymap format (PBM/PGM/PPM),
!!    a pseudo-GIF format that does not use LZW encoding, the new XML-based
!!    Scalable Vector Graphics format, the format used by Adobe Illustrator,
!!    Postscript or Encapsulated Postscript (EPS) that can be edited with
!!    idraw(1), CGM format (by default, confirming to the WebCGM profile),
!!    the format used by the xfig(1) drawing editor, the Hewlett-Packard PCL 5
!!    printer language, the Hewlett-Packard Graphics Language, ReGIS graphics
!!    format (which can be displayed by the dxterm(1) terminal emulator or
!!    by a VT330 or VT340 terminal), Tektronix format (which can be displayed
!!    by the xterm(1) terminal emulator), and device-independent GNU metafile
!!    format itself.
!!
!! pdf
!!
!!    Popular PDF readers are the Adobe PDF viewer, which is often callable from
!!    Web browsers; the GhostScript-based gv(1) utility; or the xpdf program.
!!
!!        The xpdf(1) software , related utilities ( pdftops(1), pdftotext(1),
!!        pdfinfo(1), pdffonts(1), pdftoppm(1), pdfimages(1), xpdfrc (5))
!!        and documentation are copyright 1996-2004 Glyph & Cog, LLC. at
!!        http://www.foolabs.com/xpdf/
!!
!!    The GhostScript-based tools can convert PDF files to PostScript as
!!    well as view the files.
!!
!! cgmt
!!
!!    The ralcgm(1) and gplot(1) packages are two very complete CGM viewers.
!!
!!    ppm,pbm (and p1,p3,p4,p6)
!!
!!      * p1/pbm - Poskanzer (pbmplus/netplus) portable ASCII bitmap file
!!      * p3/ppm - Poskanzer portable ASCII pixmap file
!!      * p4 - Poskanzer portable binary bitmap file
!!      * p6 - Poskanzer portable binary pixmap file
!!
!!    The NetPBM package is available for almost every platform and lets
!!    you convert the Poskanzer portable pixmap (PPM) files to just about
!!    any pixmap or bitmap format, including PNG, JPEG, GIF/PseudoGIF, BPM,
!!    ..... Other popular pixmap products such as ImageMagick, gv, ... can
!!    read PPM files, convert them, and often edit them.
!!
!!##HTML
!!
!!    The vml, canvas, svg, and usemap drivers are primarily used to generate
!!    graphics for inclusion in HTML documents. Browsers such as Opera, Safari,
!!    Foxfire, and Chrome can easily incorporate graphics generated using the
!!    SVG (Scalable Vector Graphics) format or the HTML5 CANVAS element.
!!
!! usemap
!!
!!    This driver writes out the edges of any polygon in a format that can be
!!    used with an HTML image map; if the same sizes are used a plot generated
!!    with the ppm driver; you will have clickable regions in your pixmap when
!!    converted to a GIF image.
!!
!!    If the polygons overlap you need to reverse the order of the polygon
!!    definitions in the output file. The numeric field in the<AREA> titles
!!    should help.
!!
!! vml
!!
!!    The VML format can be read in by any MicroSoft Office 2000+ product and
!!    MicroSoft's web browser Internet Explorer. If the plots contain more than
!!    about 9766 vectors MicroSoft Word starts choking (still true in 2005),
!!    but otherwise this is a very nice way to generate input for MicroSoft
!!    products.
!!
!!    I generally use this on a machine running MicroSoft Windows by installing
!!    CygWin with the X11 options (and ralcgm, the GhostScript software,
!!    the GNU plotutils packages and netpbm).
!!
!! xfig
!!
!!    The xfig(1) command can be used to edit graphics generated with the
!!    M_DRAW graphics library; and to convert the xfig(1)-format file to
!!    many other output formats. If you are generating pixmaps with the PPM
!!    driver and want to use them as image maps in your HTML documents the
!!    usemap driver can be used.
!!
!!    If you have xfig(1) installed, you will find that calling fig2dev(1)
!!    allows you to generate many output formats from a single file,
!!    including LaTex and encapsulated PostScript.
!!
!!    xfig(1) is an X11 Windows application that can be used to interactively
!!    edit figures. The HELP utility of xfig(1) provides a description of the
!!    xfig(1) file format (as well as a user guide and many other documents).
!!
!!    Unfortunately, the manual indicates the user defined colors must
!!    be defined before any other Fig objects. By default, 16 colors are
!!    defined. If undefined colors are used they are assigned a dash pattern
!!    or a fill pattern to help distinguish them. Use of hardware dash and
!!    M_DRAW software dash could get confusing.
!!
!!    Also, in the current driver version all lines are drawn as a series
!!    of move-draw vectors, which can make the files relatively very large.
!!
!!    multiple pages appear to only work with the PostScript and PDF drivers
!!    of xfig(1); and even then pages must be all positive numbers from left
!!    to right and top to bottom, printing all pages in a rectangular area.
!!
!!    Alternatively, could use depth to keep up to 999 pages separate
!!
!!##EXAMPLE
!!
!!       program demo_vinit
!!       use M_draw
!!       use ISO_C_BINDING
!!       integer(kind=c_int),parameter :: BLACK = 0, GREEN = 2
!!       character(len=50) ::  device
!!       ! read in device name
!!       print*,'Enter output device:'
!!       read(*,'(a)')device
!!       call vinit(device)
!!       ! set font to hardware text large
!!       call font('large')
!!       ! set current color to black
!!       call color(BLACK)
!!       ! clear to current color
!!       call clear
!!       ! we want to draw in green
!!       call color(GREEN)
!!       ! draw a horizontal line at y = 0
!!       call move2(-1.0, 0.0)
!!       call draw2(1.0, 0.0)
!!       ! pause for some input
!!       idum=getkey()
!!       ! draw a line along x = 0
!!       call move2(0.0, 0.0)
!!       call draw2(0.0, 1.0)
!!       ! move to the middle of the screen
!!       call move2(0.0, 0.0)
!!       ! draw 'Hello' starting at the origin
!!       call drawstr('Hello')
!!       ! pause again
!!       idum=getkey()
!!       !  set screen back to original state
!!       call vexit
!!       end program demo_vinit
!===================================================================================================================================
!>
!!##NAME
!!    vexit(3f) - [M_DRAW:DEVICE] Reset window/terminal (must be last routine called) and exit graphics mode
!!
!!##SYNOPSIS
!!
!!          subroutine vexit()
!!
!!##DESCRIPTION
!!
!!    Reset the window/terminal (must be the last M_DRAW routine called)
!!    and terminate graphics mode.  Required before calling vinit(3f)
!!    again if vinit(3f) is called more than once.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vexit
!!      use M_DRAW, only: prefsize, vexit, ortho2, clear, getkey
!!      use M_DRAW, only: move2, draw2, color, vinit
!!      integer :: ipaws
!!
!!      call prefsize(60,40)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(0)
!!      call clear()
!!      call color(1)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!
!!      call vexit()
!!
!!      end program demo_vexit
!===================================================================================================================================
!>
!!##NAME
!!    voutput(3f) - [M_DRAW:DEVICE] Redirect output from *next* vinit to file
!!
!!##SYNOPSIS
!!
!!          subroutine voutput(path)
!!          character*(*) path
!!##DESCRIPTION
!!
!!       Redirect output from *next* vinit() to file given by path. This routine
!!       only applies to device drivers that write to stdout e.g. PostScript and
!!       hpgl.
!!
!!       The special file names are
!!
!!         * - is standard output
!!         * + is standard error
!!         * |command will create a pipe to "command"
!!
!!       If the open of the file fails, an attempt is made to append to file
!!       "VOUTPUT". If this fails, standard output is used.
!!
!!       When vinit() is called if voutput() has not been called then the
!!       environment variable VOUTPUT is checked and if it is defined and not a
!!       null string then voutput() is called with the VOUTPUT variable's value.
!!
!!       A common use of the |command option is to automatically call programs
!!       that convert PPM files to other common pixmap formats or converts the GNU
!!       metafile to other formats (typically via the GNU plotutils plot program).
!===================================================================================================================================
!>
!!##NAME
!!    vnewdev(3f) - [M_DRAW:DEVICE] Reinitialize to use new device without changing
!!##SYNOPSIS
!!
!!          subroutine vnewdev(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!!    Reinitialize M_DRAW to use a new device without changing attributes,
!!    viewport etc. (eg. window and viewport specifications)
!===================================================================================================================================
!>
!!##NAME
!!    vgetdev(3f) - [M_DRAW:DEVICE] Get name of current device
!!##SYNOPSIS
!!
!!          subroutine vgetdev(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!!    Gets the name of the current M_DRAW device. The C version of the routine
!!    also returns a pointer to it's argument.
!===================================================================================================================================
!>
!!##NAME
!!    getdepth(3f) - [M_DRAW:DEVICE] Return number of bit planes (color planes)
!!
!!##SYNOPSIS
!!
!!          integer function  getdepth()
!!
!!##DESCRIPTION
!!    Returns the number of bit planes (or color planes) for a particular
!!    device. The number of colors displayable by the device is then
!!    2**(nplanes); ie. if nplanes=1,then there are two colors (black and
!!    white).
!===================================================================================================================================
!>
!!##NAME
!!    pushdev(3f) - [M_DRAW:DEVICE] push current device onto a stack
!!
!!##SYNOPSIS
!!
!!          subroutine pushdev(device)
!!          character *(*) device
!!
!!##DESCRIPTION
!!    Initialize a new device without changing attributes, viewport etc,
!!    but save the previously initialised device on a stack.
!!
!!    Note, this is intended to completely change the device, it will not  work
!!    if you pushdev() the same device that you are already running. (This will
!!    be fixed at a later date).
!===================================================================================================================================
!>
!!##NAME
!!    popdev(3f) - [M_DRAW:DEVICE] pop device from stack created by pushdev.
!!
!!##SYNOPSIS
!!
!!           subroutine popdev()
!!
!!##DESCRIPTION
!!
!!    Pops a device off the device stack and reinstates the previously pushed
!!    device.
!===================================================================================================================================
!>
!!##NAME
!!    move(3f) - [M_DRAW:MOVE] Move current graphics position to (x, y, z)
!!
!!##SYNOPSIS
!!
!!          subroutine move(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Move current graphics position to (x, y, z). (x, y, z) is a point in
!!    world coordinates.
!===================================================================================================================================
!>
!!##NAME
!!    rmove(3f) - [M_DRAW:MOVE] Relative move
!!
!!##SYNOPSIS
!!
!!          subroutine rmove(deltax, deltay, deltaz)
!!          real deltax, deltay, deltaz
!!
!!##DESCRIPTION
!!
!!    Relative move. deltax, deltay, and deltaz are offsets in world units.
!===================================================================================================================================
!>
!!##NAME
!!    move2(3f) - [M_DRAW:MOVE] Move graphics position to point (x, y)
!!
!!##SYNOPSIS
!!
!!          subroutine move2(x, y)
!!          real,intent(in) ::  x, y
!!
!!##DESCRIPTION
!!
!!    Update current position.
!!    Move graphics position to point (x, y). (x, y) is a point in world
!!    coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_move2
!!      use M_DRAW, only : prefsize, vinit, ortho2, clear, getkey
!!      use M_DRAW, only : move2, draw2, vexit
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(60,40)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(0)
!!      call clear()
!!      call color(7)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_move2
!===================================================================================================================================
!>
!!##NAME
!!    rmove2(3f) - [M_DRAW:MOVE] Relative move in world units.
!!
!!##SYNOPSIS
!!
!!     subroutine rmove2(deltax, deltay)
!!     real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Update current position.
!!    Relative move2. deltax and deltay are offsets in world units.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rmove2
!!      use M_DRAW, only: prefsize, vinit, ortho2, clear, getkey
!!      use M_DRAW, only: move2, rmove2, rdraw2, vexit
!!      use M_DRAW, only: linewidth
!!      call prefsize(500,500)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-110.0,110.0,-110.0,110.0)
!!      call move2(-100.0,-100.0)
!!      call linewidth(70)
!!      do i=1,20
!!         call rmove2(10.0, 0.0)
!!         call rdraw2( 0.0,10.0)
!!      enddo
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_rmove2
!===================================================================================================================================
!>
!!##NAME
!!    smove2(3f) - [M_DRAW:MOVE] Move current graphics position in screen coordinates (-1.0 to 1.0).
!!
!!##SYNOPSIS
!!
!!          subroutine smove2(x, y)
!!          real x, y
!!
!!##DESCRIPTION
!!    Move current graphics position in screen coordinates (-1.0 to 1.0).
!===================================================================================================================================
!>
!!##NAME
!!    rsmove2(3f) - [M_DRAW:MOVE] Relative move in screen units (-1.0 to 1.0).
!!
!!##SYNOPSIS
!!
!!          subroutine rsmove2(deltax, deltay)
!!          real deltax, deltay
!!
!!##DESCRIPTION
!!
!!    Relative smove2. deltax, and deltay are offsets in screen units
!!    (-1.0 to 1.0).
!===================================================================================================================================
!>
!!##NAME
!!    draw(3f) - [M_DRAW:DRAW] Draw from current graphics position to (x, y, z)
!!
!!##SYNOPSIS
!!
!!          subroutine draw(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Draw from current graphics position to (x, y, z). (x, y, z) is a
!!    point in world coordinates.
!===================================================================================================================================
!>
!!##NAME
!!    rdraw(3f) - [M_DRAW:DRAW] Relative draw
!!
!!##SYNOPSIS
!!
!!     subroutine rdraw(deltax, deltay, deltaz)
!!     real deltax, deltay, deltaz
!!
!!##DESCRIPTION
!!    Relative draw. deltax, deltay, and deltaz are offsets in world units.
!===================================================================================================================================
!>
!!##NAME
!!    draw2(3f) - [M_DRAW:DRAW] Draw from current graphics position to given point (x, y)
!!
!!##SYNOPSIS
!!
!!     subroutine draw2(x, y)
!!     real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_draw2
!!    use M_DRAW,    only : prefsize, vinit, ortho2, clear, getkey
!!    use M_DRAW,    only : move2, draw2, vexit, color,linewidth
!!    use M_units,   only : d2r, polar_to_cartesian
!!    !
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=A+B*theta
!!    ! Changing the parameter A will turn the spiral,
!!    ! while B controls the distance between successive turnings.
!!    !
!!       implicit none
!!       integer        :: i
!!       real           :: x,y,radius,theta
!!       real,parameter :: rotate=0.0, gap=2.0
!!       integer        :: ipaws
!!
!!       call prefsize(400,400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!       call color(5)
!!       call clear()
!!       call move2(0.0,0.0)
!!       call color(0)
!!       call linewidth(40)
!!       do i=0,360*10,5
!!          theta=d2r(i)
!!          ! equation in polar coordinates
!!          radius=rotate+gap*theta
!!          ! convert polar coordinates to cartesian
!!          call polar_to_cartesian(radius,theta,x,y)
!!          ! draw from current position to end of next segment
!!          call draw2(x,y)
!!       enddo
!!       ipaws=getkey()
!!       ! exit graphics mode
!!       call vexit()
!!    end program demo_draw2
!===================================================================================================================================
!>
!!##NAME
!!    rdraw2(3f) - [M_DRAW:DRAW] Relative draw from current position to given point
!!
!!##SYNOPSIS
!!
!!          subroutine rdraw2(deltax, deltay)
!!          real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Relative draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    deltax and deltay are offsets in world units.
!!
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rdraw2
!!      use M_DRAW, only: vinit, prefsize, ortho2,linewidth,getkey
!!      use M_DRAW, only: clear, move2, rdraw2, vexit,color
!!      integer :: ipaws
!!
!!      call prefsize(200,200)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-55.0, 55.0, -55.0,  55.0)
!!      call linewidth(400)
!!      call color(7)
!!      call clear()
!!
!!      call color(1)
!!      call move2(-50.0,0.0)
!!      call square(50.0)
!!
!!      call linewidth(200)
!!      call color(2)
!!      call move2(  0.0,-50.0)
!!      call square(50.0)
!!
!!      ipaws=getkey()
!!      call vexit()
!!
!!      contains
!!
!!      subroutine square(side)
!!      call rdraw2( side,   0.0)
!!      call rdraw2(  0.0,  side)
!!      call rdraw2(-side,   0.0)
!!      call rdraw2(  0.0, -side)
!!      end subroutine square
!!
!!      end program demo_rdraw2
!===================================================================================================================================
!>
!!##NAME
!!    sdraw2(3f) - [M_DRAW:DRAW] Draw in screen coordinates (-1.0 to 1.0).
!!
!!##SYNOPSIS
!!
!!          subroutine sdraw2(x, y)
!!          real x, y
!!##DESCRIPTION
!!    Draw in screen coordinates (-1.0 to 1.0).
!===================================================================================================================================
!>
!!##NAME
!!    rsdraw2(3f) - [M_DRAW:DRAW] Relative draw in screen units (-1.0 to 1.0).
!!
!!##SYNOPSIS
!!
!!          subroutine rsdraw2(deltax, deltay)
!!          real deltax, deltay
!!
!!##DESCRIPTION
!!    Relative sdraw2. delatx and deltay are in screen units (-1.0 to 1.0).
!===================================================================================================================================
!>
!!##NAME
!!    rect(3f) - [M_DRAW:POLYGONS] Draw a rectangle given two corners
!!
!!##SYNOPSIS
!!
!!       subroutine rect(x1, y1, x2, y2)
!!       real,intent(in) :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw rectangle given two opposite corners.
!!
!!    Note: rectangles are regarded as polygons, so if
!!    polyfill or polyhatch has been called with .TRUE., the rectangle will
!!    be filled or hatched accordingly.
!!
!!##OPTIONS
!!   Given
!!
!!       x1,y1 ############ x2,y1
!!             #          #
!!             #          #
!!             #          #
!!       x1,y2 ############ x2,y2
!!
!!    X1,Y1  coordinates of a corner of the rectangle
!!    X2,Y2  coordinates of corner point opposite first point
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rect
!!    use M_draw
!!    implicit none
!!    integer :: i, ipaws
!!    real    :: r
!!
!!    !! set up graphics area
!!    call prefsize(400,400)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
!!
!!    !! make sure display is ready, which can be a problem with X11
!!    call vflush()
!!    ipaws=getkey()
!!
!!    !! draw some filled rectangles
!!    call polyfill(.true.)
!!    do i=95,5,-10
!!       call color(i/10)
!!       r=real(i)
!!       call rect( -1.0*r, -1.0*r, 1.0*r, 1.0*r )
!!    enddo
!!
!!    !! draw some rectangles
!!    call polyfill(.false.)
!!    call linewidth(50)
!!    call color(7)
!!    do i=5,95,5
!!       r=real(i)
!!       call rect( -1.0*r, -1.0*r, 1.0*r, 1.0*r )
!!    enddo
!!
!!    !! pause
!!    call vflush()
!!    ipaws=getkey()
!!
!!    !! wrap up graphics
!!    call vexit()
!!
!!    end program demo_rect
!===================================================================================================================================
!>
!!##NAME
!!      polyfill(3f) - [M_DRAW:POLYGONS] Set the polygon fill flag
!!
!!##SYNOPSIS
!!
!!       subroutine polyfill(onoff)
!!       logical onoff
!!
!!##DESCRIPTION
!!    Set the polygon fill flag. This will always turn off hatching. A non-zero
!!    integer or LOGICAL .true. turns polyfill on.
!===================================================================================================================================
!>
!!##NAME
!!    polyhatch(3f) - [M_DRAW:POLYGONS] Set the polygon hatch flag
!!##SYNOPSIS
!!
!!     subroutine polyhatch(onoff)
!!     logical onoff
!!##DESCRIPTION
!!
!!    Set the polygon hatch flag. This will always turn off fill. A non-zero
!!    integer or LOGICAL .true. turns polyhatch on. Note that hatched polygons
!!    must initially be defined parallel to the X-Y plane.
!===================================================================================================================================

!>
!!##NAME
!!    hatchang(3f) - [M_DRAW:POLYGONS] Set the angle of the hatch lines.
!!##SYNOPSIS
!!
!!       subroutine hatchang(angle)
!!       real,intent(in) ::  angle
!!
!!##DESCRIPTION
!!
!!    Set the angle of the hatch lines. The angle is in degrees. Zero degrees
!!    is on the negative X axis. Positive values are counterclockwise. The
!!    value is 0 at program initialization. The last value set is retained
!!    even if hatching is not active or is turned on and off.
!===================================================================================================================================
!>
!!##NAME
!!    hatchpitch(3f) - [M_DRAW:POLYGONS] Set the distance between hatch lines.
!!
!!##SYNOPSIS
!!
!!       subroutine hatchpitch(pitch)
!!       real,intent(in) ::  pitch
!!
!!##DESCRIPTION
!!    Set the distance between hatch lines. The distance is measured in
!!    window units (as opposed to viewport or device units).
!===================================================================================================================================
!>
!!##NAME
!!    poly2(3f) - [M_DRAW:POLYGONS] Construct an (x, y) polygon from an array of points
!!##SYNOPSIS
!!
!!       subroutine poly2(n, points)
!!       integer,intent(in) :: n
!!       real,intent(in)    :: points(2, n)
!!
!!##DESCRIPTION
!!    Construct an (x, y) polygon from an array of points provided by the user.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_poly2
!!    use M_DRAW
!!    integer :: i,j
!!    real    :: xx,yy
!!       call prefsize(512,512)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(0.0,256.0,0.0,256.0)
!!       call linewidth(1)
!!       ! step thru a series of rectangular cells
!!       icolor=0
!!       xx=0.0
!!       do i=1,16
!!          yy=0.0
!!          do j=1,16
!!             yy=yy+16.0
!!             icolor=icolor+1
!!             call setcolor(icolor,xx,yy)
!!          enddo
!!          xx=xx+16.0
!!       enddo
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!
!!    subroutine setcolor(iset,xx,yy)
!!    use M_strings, only : v2s
!!    use M_color,  only : color_name2rgb
!!    integer,intent(in) :: iset
!!    real,intent(in)    :: xx,yy
!!    character(len=80)  :: echoname
!!    real    :: points(2,100)
!!    if(iset.gt.255)return
!!    ! determine coordinates of next square
!!    points(1:2,1)=[xx,      yy      ]
!!    points(1:2,2)=[xx,      yy+16.0 ]
!!    points(1:2,3)=[xx+16.0, yy+16.0 ]
!!    points(1:2,4)=[xx+16.0, yy      ]
!!    points(1:2,5)=[xx,      yy      ]
!!    ! get some nice RGB values to try from named colors known by M_color module
!!    call color_name2rgb(v2s(icolor),red,green,blue,echoname)
!!    if(echoname.eq.'Unknown') return
!!    ! set a color number to the new RGB values
!!    write(*,*)icolor, nint(red*2.55), nint(green*2.55), nint(blue*2.55),trim(echoname)
!!    call mapcolor(icolor, nint(red*2.55), nint(green*2.55), nint(blue*2.55))
!!    ! set to the new color
!!    call color(icolor)
!!    ! fill the rectangle in that color
!!    call poly2(5,points)
!!    end subroutine setcolor
!!
!!    end program demo_poly2
!===================================================================================================================================
!>
!!##NAME
!!    poly(3f) - [M_DRAW:POLYGONS] Construct a polygon from an array of points
!!
!!##SYNOPSIS
!!
!!       subroutine poly(n, points)
!!       integer,intent(in) :: n
!!       real,intent(in)    :: points(3, n)
!!##DESCRIPTION
!!
!!    Construct a polygon from an array of points provided by the user.
!===================================================================================================================================
!>
!!##NAME
!!    makepoly(3f) - [M_DRAW:POLYGONS] opens polygon constructed by a series of move-draws and closed by closepoly
!!
!!##SYNOPSIS
!!
!!       subroutine makepoly()
!!
!!##DESCRIPTION
!!    makepoly(3f)  opens up a polygon which will then be constructed by a series
!!    of move-draws and closed by a closepoly.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_makepoly
!!    use :: M_DRAW
!!    implicit none
!!    integer,parameter :: wide=640, tall=640
!!    integer :: rows, xoff, yoff, box_sz
!!    integer :: i20, i30, ncols, nrows, ilines
!!    real    :: bottom, left, sun_radius, planet_radius, planet_offset
!!    character(len=40) :: filename
!!    integer :: movie(300,0:wide-1,0:tall-1)
!!    integer :: ipaws
!!       call prefsize(wide,tall)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(0.0, real(wide), 0.0, real(tall) )
!!       ! call linewidth(3) ! really slows down pbm driver because all lines are polygons
!!       call color(7)
!!       call clear()
!!       call color(0)
!!       rows=1
!!       box_sz=MIN(wide,tall)/rows       ! size of biggest box to use and get specified number of rows
!!       nrows = tall/box_sz              ! number of rows of objects to draw
!!       ncols = wide/box_sz              ! number of columns of objects to draw
!!       xoff = (wide - ncols * box_sz)/2 ! initial x offset to begin row at to center drawings
!!       yoff = (tall - nrows * box_sz)/2 ! initial x offset to begin column at to center drawings
!!       sun_radius = 148
!!       planet_radius = 1
!!       do ilines = 1, 300
!!          do i20 = 1, ncols
!!             left = (i20-1)*box_sz+xoff
!!             do i30 = 1, nrows
!!                bottom = (i30-1)*box_sz+yoff
!!                call color(0)
!!             call makepoly()
!!                call rect(left,bottom,left+box_sz,bottom+box_sz)
!!             call closepoly()
!!                planet_offset= sun_radius
!!                   call color(mod(ilines,15)+1)
!!                   call hypoc(left + box_sz/2.0, bottom + box_sz/2.0, &
!!                & sun_radius, planet_radius, planet_offset, &
!!                & box_sz/2.0, ilines,  &
!!                & 0.0, 0.0, 1)
!!             enddo
!!          enddo
!!          ipaws=getkey()
!!       enddo
!!       call vexit()
!!    contains
!!    !
!!    !  Make shapes using hypocycloidal curves.
!!    !
!!    subroutine hypoc(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!!    use M_DRAW
!!    implicit none
!!    real,parameter     :: PI= 3.14159265358979323846264338327950288419716939937510
!!    real,intent(in)    :: xcenter, ycenter      ! center of curve
!!    real,intent(in)    :: sunr0,planet0,offset0 ! radii of sun, planet, and planet offset
!!    real,intent(in)    :: radius                ! radius to fit the shape to (no fit if radius is 0)
!!    integer,intent(in) :: ilines                ! number of points to sample along curve
!!    real,intent(in)    :: ang                   ! angle to rotate the shape by, to orientate it.
!!    real,intent(in)    :: angs                  ! angle to start sampling points at; ccw is +; 0 is East
!!    integer,intent(in) :: ifill                 ! 1 make a filled polygon, 2 make a hatched polygon
!!    integer            :: i10
!!    real               :: ang1, con1, con2, factor
!!    real               :: offset, planet, r, sunr, u
!!    real               :: xpoin, xpoin1, ypoin, ypoin1
!!       sunr=sunr0
!!       offset=offset0
!!       planet=planet0
!!       if(ilines.eq.0.0) return
!!       if(planet.eq.0.0) return
!!       if(sunr.eq.0.0)   return
!!       if(radius.ne.0.and.sunr-planet+offset.ne.0)then
!!          factor=radius/(sunr-planet+offset)
!!          sunr=factor*sunr
!!          planet=factor*planet
!!          offset=factor*offset
!!       endif
!!       u=0.0+ang
!!       con1=PI*2.*(sunr/planet)/real(ilines)
!!       con2=(1.0-planet/sunr)*u
!!       xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!       ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!       ang1=atan2(ypoin1,xpoin1)+angs
!!       r=sqrt(xpoin1**2+ypoin1**2)
!!       xpoin1=r*cos(ang1)+xcenter
!!       ypoin1=r*sin(ang1)+ycenter
!!       select case(ifill)
!!       case(:0)
!!       case(1:)
!!          call makepoly()
!!       end select
!!       call move2(xpoin1,ypoin1)
!!       do i10=1,ilines
!!          u=con1*i10+ang
!!          con2=(1.0-planet/sunr)*u
!!          if(con2.ge.2**24) con2=amod(con2,PI)
!!          xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!          ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!          ang1=atan2(ypoin,xpoin)+angs
!!          r=sqrt(xpoin**2+ypoin**2)
!!          xpoin=r*cos(ang1)+xcenter
!!          ypoin=r*sin(ang1)+ycenter
!!          call draw2(xpoin,ypoin)
!!       enddo
!!       call draw2(xpoin1,ypoin1)
!!       if(ifill.gt.0)then
!!         call closepoly()
!!       endif
!!    end subroutine hypoc
!!    end program demo_makepoly
!===================================================================================================================================
!>
!!##NAME
!!    closepoly(3f) - [M_DRAW:POLYGONS] Terminates a polygon opened by makepoly(3f)
!!
!!##SYNOPSIS
!!
!!       subroutine closepoly()
!!
!!##DESCRIPTION
!!
!!    Terminates a polygon opened by makepoly(3f).
!===================================================================================================================================
!>
!!##NAME
!!    backface(3f) - [M_DRAW:POLYGONS] Turns on culling of backfacing polygons.
!!
!!##SYNOPSIS
!!
!!       subroutine backface(onoff)
!!       logical onoff
!!
!!##DESCRIPTION
!!    Turns on culling of backfacing polygons. A polygon is backfacing if
!!    it's orientation in *screen* coords is clockwise, unless a call
!!    to backfacedir is made.
!===================================================================================================================================
!>
!!##NAME
!!    backfacedir(3f) - [M_DRAW:POLYGONS] Sets backfacing direction to clockwise or anti-clockwise
!!
!!##SYNOPSIS
!!
!!       subroutine backfacedir(clockwise)
!!       integer,intent(in) ::  clockwise
!!
!!##DESCRIPTION
!!    Sets the backfacing direction to clockwise or anti-clockwise depending
!!    on whether clockwise is 1 or 0. 1 = clockwise (in screen coords)
!!    0 = anticlockwise.
!!
!===================================================================================================================================
!>
!!##NAME
!!     circleprecision(3f) - [M_DRAW:ARCS] Set number of line segments used to approximate a circle
!!
!!##SYNOPSIS
!!
!!         subroutine circleprecision(nsegs)
!!         integer,intent(in)   :: nsegs
!!
!!    Set the number of line segments making up a circle. Default is currently
!!    32. The number of segments in an arc or sector is calculated from the
!!    variable "nsegs" according to the span of the arc or sector.
!!
!!
!!##OPTIONS
!!    NSEGS   number of line segments making up a circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circleprecision
!!    use M_DRAW
!!    use M_strings,  only : v2s
!!    real    :: b=0.5
!!    real    :: y1,y2,ym,x1,x2
!!    real    :: width=50.0/8.0,width2
!!    integer,parameter :: ivals(*)=[3,5,7,10,20,30,60,100]
!!    integer :: i
!!    integer :: ipaws
!!       !! set up long bar as plotting area
!!       call prefsize(1000,200)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize( 2.5/2.0, 3.0/2.0)
!!       call font('DUPLEX')
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call color(2)
!!       y1=-5
!!       y2=5
!!       ym=0
!!       x1=-25+.05*width
!!       ! draw colored rectangle and a circle and label center of circle repeat
!!       width2=width*0.95
!!       do i=1,size(ivals)
!!          x2=x1+width2
!!          call move2((x1+x2)/2.0,ym)
!!          call circleprecision(ivals(i))
!!          call drawstr((v2s(ivals(i))))     ! convert number to string and draw it
!!          call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!          x1=x1+width
!!       enddo
!!       ipaws=getkey()
!!       call vexit()
!!    end program demo_circleprecision
!!
!!##IMAGE
!!    circles are drawn with various circle precision values.
!===================================================================================================================================
!>
!!##NAME
!!     arc(3f) - [M_DRAW:ARCS] Draw an arc in world units.
!!##SYNOPSIS
!!
!!         subroutine arc(x, y, radius, startang, endang)
!!         real,intent(in) :: x
!!         real,intent(in) :: y
!!         real,intent(in) :: radius
!!         real,intent(in) :: startang
!!         real,intent(in) :: endang
!!##DESCRIPTION
!!
!!    Draw an arc. x, y, and radius are values in world units
!!    using  current line width and color
!!
!!    Angles are in degrees, positive measured counterclockwise from the
!!    +X axis. The current position after the arc is drawn is at the end
!!    of the arc.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!    STARTANG   Start angle
!!    ENDANG     End angle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_arc
!!    use M_DRAW
!!    integer  :: transparent=0
!!    call prefsize(600,240)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(0.0,60.0,0.0,24.0)
!!    call linewidth(400)
!!    call color(1)
!!    call arc(16.0,12.0,12.0,90.0,270.0)
!!    call color(2)
!!    call arc(44.0,12.0,12.0,-90.0,90.0)
!!    ipaws=getkey()
!!    call vexit()
!!    end program demo_arc
!!
!!##IMAGE
!!    The arcs are drawn; followed by a draw to the arc center (using
!!    "draw2(X,Y)").
!===================================================================================================================================
!>
!!##NAME
!!    sector(3f) - [M_DRAW:ARCS] Draw a sector. Note: sectors are polygons.
!!
!!##SYNOPSIS
!!
!!         subroutine sector(x, y, radius, startang, endang)
!!         REAL x, y, radius, startang, endang
!!
!!##DESCRIPTION
!!
!!    Draw a sector. x, y, and radius are values in world units. Note: sectors
!!    are regarded as polygons, so if polyfill or polyhatch has been called
!!    with 1, the sectors will be filled or hatched accordingly.
!===================================================================================================================================
!>
!!##NAME
!!    circle(3f) - [M_DRAW:ARCS] Draw a circle.
!!
!!##SYNOPSIS
!!
!!         subroutine circle(x, y, radius)
!!         real,intent(in) :: x
!!         real,intent(in) :: y
!!         real,intent(in) :: radius
!!
!!##DESCRIPTION
!!
!! Draw a circle. x, y, and radius are values in world units.
!!
!! Draw a circle using current line width and color
!!
!! Note: circles
!! are regarded as polygons, so if polyfill or polyhatch has been called
!! with 1, the circle will be filled or hatched accordingly.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circle
!!    use M_DRAW
!!    integer :: ipaws
!!    !! set up drawing surface
!!    call prefsize(400,400)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
!!    call color(3)
!!    call clear()
!!    call color(4)
!!    call linewidth(200)
!!    !! draw some circles
!!    call circle(0.0, 0.0, 90.0)
!!    call color(1)
!!    call circle(0.0, 0.0, 40.0)
!!    call color(2)
!!    call circle(-25.0, 25.0, 20.0)
!!    call circle(-25.0,-25.0, 20.0)
!!    call circle( 25.0, 25.0, 20.0)
!!    call circle( 25.0,-25.0, 20.0)
!!
!!    ipaws=getkey()
!!    !! exit graphics mode
!!    call vexit()
!!    end program demo_circle
!!
!!##IMAGE
!!    circles are drawn with polygon fill and hatch fill options. Multiple
!!    calls were used to create the filled, crosshatched and outlined circle.
!===================================================================================================================================
!>
!!##NAME
!!    point(3f) - [M_DRAW:POINT] Draw a point at x, y, z
!!##SYNOPSIS
!!
!!         subroutine point(x, y, z)
!!         real,intent(in) ::  x, y, z
!!##DESCRIPTION
!!    Draw a point at x, y, z
!===================================================================================================================================
!>
!!##NAME
!!    point2(3f) - [M_DRAW:POINT] Draw a point at x, y.
!!
!!##SYNOPSIS
!!
!!         subroutine point2(x, y)
!!         real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw a point at x, y. Points are device-dependent and may not appear
!!    at all. Generally points are drawn with the current color as a circle
!!    with a diameter equal to the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point2
!!    use :: M_DRAW
!!    implicit none
!!    integer :: i
!!    integer :: ipaws
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call color(5)
!!    do i=1,20
!!       call linewidth(50*i)
!!       call point2(real(i*25),real(i*25))
!!    enddo
!!    ipaws=getkey()
!!    call vexit()
!!    end program demo_point2
!===================================================================================================================================
!>
!!##NAME
!!    curvebasis(3f) - [M_DRAW:CURVE] Define a basis matrix for a curve.
!!##SYNOPSIS
!!
!!          subroutine curvebasis(basis)
!!          real,intent(in) ::  basis(4,4)
!!##DESCRIPTION
!!    Define a basis matrix for a curve.
!===================================================================================================================================
!>
!!##NAME
!!    curveprecision(3f) - [M_DRAW:CURVE] Define number of line segments used to draw a curve.
!!##SYNOPSIS
!!
!!          subroutine curveprecision(nsegs)
!!          integer nsegs
!!##DESCRIPTION
!!    Define the number of line segments used to draw a curve.
!===================================================================================================================================
!>
!!##NAME
!!    rcurve(3f) - [M_DRAW:CURVE] Draw a rational curve.
!!
!!##SYNOPSIS
!!
!!          subroutine rcurve(geom)
!!          real geom(4,4)
!!
!!##DESCRIPTION
!!    Draw a rational curve.
!===================================================================================================================================
!>
!!##NAME
!!    curve(3f) - [M_DRAW:CURVE] Draw a curve.
!!
!!##SYNOPSIS
!!
!!          subroutine curve(geom)
!!          real geom(3,4)
!!
!!##DESCRIPTION
!!    Draw a curve.
!===================================================================================================================================
!>
!!##NAME
!!    curven(3f) - [M_DRAW:CURVE] Draw n-3 overlapping curve segments. Note: n must be at least 4.
!!
!!##SYNOPSIS
!!
!!          subroutine curven(n, geom)
!!          integer n
!!          real geom(3,n)
!!
!!##DESCRIPTION
!!    Draw n-3 overlapping curve segments. Note: n must be at least 4.
!===================================================================================================================================
!>
!!
!!##NAME
!!    font(3f) - [M_DRAW:TEXT] Set the current font by name
!!
!!##SYNOPSIS
!!
!!         subroutine font(fontname)
!!         character(len=*),intent(in) :: fontname
!!
!!##DESCRIPTION
!!    Set the current font. Allowed names are
!!
!!       o futura.l  SIMPLEX
!!       o futura.m  DUPLEX
!!       o times.r   COMPLEX
!!       o times.i   ITALIC
!!
!!    WHEN ASK FOR NON-EXISTENT FONT, PROGRAM STOPS
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_font
!!    use :: M_DRAW
!!    real    :: left
!!    real    :: baseline=80.0
!!    integer :: icolor=1
!!    integer :: ipaws
!!       !! set up drawing surface
!!       call prefsize(400, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call viewport(0.0, 400.0, 400.0, 0.0)
!!       call ortho2(-100.0, 100.0, -100.0, 100.0)
!!       call color(7)
!!       call clear()
!!       call textsize(10.0, 10.0)
!!       !! place a vertical line along the edge
!!       call color(1)
!!       call move2(-90.0, -90.0)
!!       call draw2(-90.0, 90.0)
!!       !! make a centered title at top a bit bolder and bigger
!!       call xcentertext()
!!       call textsize(13.0, 13.0)
!!       call linewidth(90)
!!       left=0
!!       call nextline('Font Samples')
!!       !! print the font samples
!!       left=-90
!!       call linewidth(0)
!!       call textsize(10.0, 10.0)
!!       call centertext(.false.)
!!       icolor=icolor-1
!!       call nextline('DEFAULT (ie. futura.l)')
!!       icolor=icolor-1
!!       call nextline('now call font(3f) ...')
!!       call nextline('SIMPLEX, or futura.l')
!!       call nextline('COMPLEX, or times.r')
!!       call nextline('ITALIC, or times.i')
!!       call nextline('DUPLEX, or futura.m')
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    !! reduce some duplicate code; very specific to this EXAMPLE
!!       integer :: iend
!!       iend=index(string,',')  ! if comma, assume font name found
!!       if(iend.ne.0)call font(string(:iend-1)) ! change font
!!       icolor=icolor+1         ! set pen color
!!       call color(icolor)
!!       baseline=baseline-20    ! move down before drawing line
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!    end subroutine nextline
!!
!!    end program demo_font
!===================================================================================================================================
!>
!!##NAME
!!    numchars(3f) - [M_DRAW:TEXT] Return number of characters in the current SOFTWARE font.
!!
!!##SYNOPSIS
!!
!!         integer function numchars
!!
!!##DESCRIPTION
!!    Return the number of characters in the current font. Applicable only to
!!    software fonts.
!===================================================================================================================================
!>
!!##NAME
!!    textsize(3f) - [M_DRAW:TEXT] Set text size of a character in the current SOFTWARE font in world units.
!!##SYNOPSIS
!!
!!         subroutine textsize(width, height)
!!         real,intent(in) :: width
!!         real,intent(in) :: height
!!
!!##DESCRIPTION
!!
!! Set the maximum size of a character in the current font. Width and height
!! are values in world units. This applies to software text, but may not apply
!! to hardware fonts depending upon the output device. This must
!! be done after the font being scaled is loaded. To keep text of different
!! sizes aligned along the same baseline not that you typically need to
!! subtract the decender height from the Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textsize
!!    use M_DRAW
!!    implicit none
!!    integer :: i,ii
!!    integer :: ipaws
!!       !! set up long bar as plotting area
!!       call prefsize(900,150)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-30.0, 30.0, -5.0, 5.0)
!!       call font('DUPLEX')
!!
!!       call move2(-23.0,-4.5)
!!       call color(7)
!!       call textsize(2.0,2.0)
!!       call move2(-27.5,-3.0)
!!       call draw2( 27.5,-3.0)
!!       call move2(-27.5,-3.0)
!!
!!       do i=1,7
!!          ii=nint((i*20)*0.30)
!!          call linewidth(nint(ii*2.35))
!!          call textsize(real(i),real(i))
!!          call color(5)
!!          call drawstr('aA')
!!       enddo
!!
!!       ipaws=getkey()
!!
!!       call vexit()
!!
!!    end program demo_textsize
!===================================================================================================================================
!>
!!##NAME
!!    textang(3f) - [M_DRAW:TEXT] Set the SOFTWARE text angle.
!!##SYNOPSIS
!!
!!         subroutine textang(ang)
!!         real,intent(in) :: ang
!!
!!##DESCRIPTION
!!    Set the text angle. This angles strings and chars. This routine only
!!    affects software text. Angle is in degrees
!!
!!##OPTIONS
!!    ANG   The angle in degrees to draw text with when using drawstr(3f).
!!          Angles are measured counterclockwise with zero degrees at the horizontal
!!          line to the right of the original.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textang
!!    use :: M_DRAW
!!    use :: M_units, only : cosd, sind
!!
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(7.0,7.0)
!!    call linewidth(20)
!!
!!    do i=1,30
!!       !! draw radial lines
!!       call color(1)
!!       call move2(0.0,0.0)
!!       call draw2(100.0*cosd(i*12),100.0*sind(i*12))
!!       !! draw rotated text
!!       call color(7)
!!       call move2(30.0*cosd(i*12),30.0*sind(i*12))
!!       call textang(i*12.0)
!!       call drawstr('angled text')
!!    enddo
!!
!!    ipaws=getkey()
!!
!!    call vexit()
!!
!!    end program demo_textang
!===================================================================================================================================
!>
!!##NAME
!!    fixedwidth(3f) - [M_DRAW:TEXT] Turns fixedwidth mode on or off for SOFTWARE fonts.
!!##SYNOPSIS
!!
!!         subroutine fixedwidth(onoff)
!!         logical onoff
!!##DESCRIPTION
!!
!!    Turns fixedwidth text on or off. Non-zero (.true.) causes all text to
!!    be printed with a fixed width for each character. Otherwise, the text
!!    is spaced proportionally, where each character has a unique width less
!!    than or equal to the current fixed font width. This routine only affects
!!    software text.
!!
!!    The default at program initialization is fixedwidth(.false.)
!===================================================================================================================================
!>
!!##NAME
!!    centertext(3f) - [M_DRAW:TEXT] Turns centertext mode on or off for SOFTWARE fonts.
!!
!!##SYNOPSIS
!!
!!         subroutine centertext(onoff)
!!         logical,intent(in) :: onoff
!!
!!##DESCRIPTION
!!
!!    Turns centertext text on or off. Non-zero (.true.) is on. This centers
!!    strings and chars. This routine only affects software text.
!!
!!##OPTIONS
!!    ONOFF  set centering mode on or off
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_centertext
!!    use :: M_DRAW
!!    use :: M_units, only : cosd, sind
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(8.0,8.0)
!!    call linewidth(30)
!!
!!    x1=-150
!!    y1=-150
!!    do j=1,4
!!       select case(j)
!!       case(1);  call  xcentertext();        x1=-150;  y1=-150;  r=100
!!       case(2);  call  ycentertext();        x1=+150;  y1=-150;  r= 30
!!       case(3);  call  centertext(.true.);   x1=-150;  y1=+150;  r=100
!!       case(4);  call  centertext(.false.);  x1=+150;  y1=+150;  r= 30
!!       end select
!!       !! draw radial lines
!!       call color(1)
!!       do i=1,80
!!          call move2(x1,y1)
!!          call draw2(x1+150.0*cosd(i*12), y1+150.0*sind(i*12))
!!       enddo
!!
!!       !! draw rotated text
!!       call color(2)
!!       do i=1,30
!!          ang=i*12.0
!!          xx=x1+r*cosd(ang)
!!          yy=y1+r*sind(ang)
!!          call move2(xx,yy)
!!          call textang(ang)
!!          call color(7)
!!          call drawstr('This is angled text')
!!          call color(1)
!!       enddo
!!    enddo
!!
!!    ipaws=getkey()
!!
!!    call vexit()
!!
!!    end program demo_centertext
!===================================================================================================================================
!>
!!##NAME
!!    getcharsize(3f) - [M_DRAW:TEXT] Get the width and height of a character.
!!
!!##SYNOPSIS
!!
!!         subroutine getcharsize(c, width, height)
!!         character*1 c
!!         real width, height
!!
!!##DESCRIPTION
!!    Get the width and height of a character. At the moment the height
!!    returned is always that of the difference between the maximum descender
!!    and ascender.
!===================================================================================================================================
!>
!!##NAME
!!    getfontdec(3f) - [M_DRAW:TEXT] Return size of maximum font descender
!!
!!##SYNOPSIS
!!
!!         real function getfontdec
!!##DESCRIPTION
!!    Get the descender size of a character in a font.
!===================================================================================================================================
!>
!!##NAME
!!    getfontsize(3f) - [M_DRAW:TEXT] Get maximum width and height of a character in a font.
!!
!!##SYNOPSIS
!!
!!         subroutine getfontsize(width, height)
!!         real width, height
!!
!!##DESCRIPTION
!!
!!    Get the maximum width and height of a character in a font.
!===================================================================================================================================
!>
!!##NAME
!!    drawchar(3f) - [M_DRAW:TEXT] Draw the character c and update current position.
!!
!!##SYNOPSIS
!!
!!         subroutine drawchar(ch)
!!         character(len=1),intent(in) :: ch
!!
!!##DESCRIPTION
!!
!!    Draw the character c at the current position. The current graphics
!!    position represents the bottom left hand corner of the character space.
!!
!!    Uses current line color and thickness and text justification mode.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_drawchar
!!    use M_DRAW
!!    integer,parameter :: isize=600
!!    integer  :: ipaws
!!    !! set up environment
!!    call prefsize(isize,isize)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(150.0,150.0)
!!    call centertext(.true.)
!!
!!    do i=33,124
!!       !! draw reference circle and crosshairs
!!       call linewidth(100)
!!       call color(0)
!!       call clear()
!!       call color(4)
!!       call circle(0.0,0.0,75.0)
!!       call move2(-75.0,0.0)
!!       call draw2(75.0,0.0)
!!       call move2(0.0,-75.0)
!!       call draw2(0.0,75.0)
!!       call color(7)
!!       call linewidth(200)
!!       call textang(3.0*i)
!!       call move2(0.0,0.0)
!!       call drawchar(char(i))
!!       ipaws=getkey()
!!    enddo
!!    call vexit()
!!    end program demo_drawchar
!===================================================================================================================================
!>
!!##NAME
!!    drawstr(3f) - [M_DRAW:TEXT] Draw the text in string at the current position.
!!
!!##SYNOPSIS
!!
!!         subroutine drawstr(str)
!!         character(len=*),intent(in) :: str
!!
!!##DESCRIPTION
!!    Draw a text string at the current position. Uses current line color
!!    and thickness and text centering mode.
!===================================================================================================================================
!>
!!##NAME
!!    strlength(3f) - [M_DRAW:TEXT] return length of string
!!
!!##SYNOPSIS
!!
!!
!!       real function strlength(string)
!!       character(len=*),intent(in)    :: string
!!
!!##DESCRIPTION
!!    Return the length of the string s in world units.
!!
!!##RETURNS
!!    STRLENGTH  length of string using current font size
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_strlength
!!    use :: M_draw
!!    real    :: left
!!    real    :: baseline
!!    integer :: icolor=0
!!    real    :: texth=10.0
!!       !! set up drawing surface
!!       call prefsize(800, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call viewport(0.0, 800.0, 400.0, 0.0)
!!       call ortho2(-100.0, 300.0, -100.0, 100.0)
!!       call color(7)
!!       call clear()
!!       call linewidth(30)
!!       call textsize(texth, texth)
!!       call xcentertext()
!!       call color(1)
!!
!!       baseline=85.0
!!       call move2(0.0,baseline)
!!       call drawstr('If I Can Stop One Heart')
!!       baseline= baseline-texth*1.20
!!       call move2(0.0,baseline)
!!       call drawstr('by Emily Dickinson')
!!       call centertext(.false.)
!!
!!       texth=8.5
!!       baseline=baseline-texth*1.50
!!       call textsize(texth, texth)
!!       left=-90.0
!!
!!       call nextline('If I can stop one heart from breaking,')
!!       call nextline('I shall not live in vain;')
!!       call nextline('If I can ease one life the aching,')
!!       call nextline('Or cool one pain,')
!!       call nextline('Or help one fainting robin')
!!       call nextline('Unto his nest again,')
!!       call nextline('I shall not live in vain.')
!!
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    real :: xx
!!    !! reduce some duplicate code; very specific to this EXAMPLE
!!       call color(icolor)
!!       baseline=baseline-texth*1.5    ! move down before drawing line
!!       call makepoly()
!!       xx=strlength(string)
!!       call rect(left,baseline-texth*0.3,left+xx,baseline+texth)
!!       call closepoly()
!!       call color(7)
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!       icolor=icolor+1         ! set pen color
!!    end subroutine nextline
!!
!!    end program demo_strlength
!===================================================================================================================================
!>
!!##NAME
!!    boxtext(3f) - [M_DRAW:TEXT] Draw the SOFTWARE string s so that it fits in the imaginary box
!!
!!##SYNOPSIS
!!
!!         subroutine boxtext(x, y, l, h, s)
!!         real x, y, l, h, s
!!
!!##DESCRIPTION
!!
!!    Draw the string s so that it fits in the imaginary box defined with
!!    bottom left hand corner at (x, y), length l, and height h. This only
!!    applies to software text.
!===================================================================================================================================
!>
!!##NAME
!!    boxfit(3f) - [M_DRAW:TEXT] resize the SOFTWARE text size so it fits in a box
!!
!!##SYNOPSIS
!!
!!         subroutine boxfit(l, h, nchars)
!!         real l, h
!!         integer nchars
!!
!!##DESCRIPTION
!!    Set scale for text so that a string of the biggest characters in
!!    the font will fit in a box l by h. l and h are real values in world
!!    dimensions. This only applies to software text.
!===================================================================================================================================
!>
!!##NAME
!!    textjustify(3f) - [M_DRAW:TEXT] general text justification (C only)
!!
!!##SYNOPSIS
!!
!!        subroutine textjustify(val)
!!        character(kind=c_short) :: ival
!!        character(kind=c_char)  :: val
!!
!!##DESCRIPTION
!!    General (direct) control of text justification. The value of val is made
!!    up of the logical OR of the following predefined constants in draw.h
!!    (FOR C and Fortran only). V_LEFT, V_RIGHT, V_XCENTERED, V_TOP, V_BOTTOM,
!!    V_YCENTERED. Centering takes priority, as does RIGHT and TOP justification
!!    (if you were silly enough to set it to V_LEFT|V_RIGHT for EXAMPLE that
!!    is). A value of 0 (zero) (in all languages) resets the textjustification
!!    to the default.
!!
!!        ! from Fortran, use IANY() to OR the array of options, and CHAR()
!!        ! to convert the integer result to a C_CHAR type. KIND C_CHAR is
!!        ! defined by loading a the intrinsic module for C bindings ("USE ISO_C_BINDING").
!!        ival=iany([V_XCENTERED,V_YCENTERED])
!!        val=char(ival)
!!        call textjustify(val)
!===================================================================================================================================
!>
!!##NAME
!!    leftjustify(3f) - [M_DRAW:TEXT] left justify text
!!
!!##SYNOPSIS
!!
!!         subroutine leftjustify()
!!
!!##DESCRIPTION
!!    Left justifies text. The text string will begin at the current position
!!    and extend to the notional right. Right justification and X centering
!!    are turned off.
!===================================================================================================================================
!>
!!##NAME
!!    rightjustify(3f) - [M_DRAW:TEXT] right justify text
!!
!!##SYNOPSIS
!!
!!        subroutine rightjustify
!!
!!##DESCRIPTION
!!
!!    Right justifies text. The text string will begin at a point to the
!!    notional left of the current position and finish at the current
!!    position. Left justification and X centering are turned off.
!===================================================================================================================================
!>
!!##NAME
!!    xcentertext(3f) - [M_DRAW:TEXT] set text centering mode on in X direction
!!
!!##SYNOPSIS
!!
!!        subroutine xcentertext()
!!
!!##DESCRIPTION
!!    Set text centering mode on in X direction. Y justification is
!!    turned off.
!!
!!    Centers text in the X direction. The text string will begin at a
!!    point to the notional left of the current position and finish at a
!!    point to the right of the current position. Left justification and
!!    Right justification are turned off.
!!
!!##EXAMPLE
!!
!===================================================================================================================================
!>
!!##NAME
!!    topjustify(3f) - [M_DRAW:TEXT] top justify text
!!
!!##SYNOPSIS
!!
!!         subroutine topjustify
!!
!!##DESCRIPTION
!!    Top justifies text. The text string will be drawn with its upper edge
!!    aligned with the current Y position. Bottom justification and Y centering
!!    are turned off.
!===================================================================================================================================
!>
!!##NAME
!!    bottomjustify(3f) - [M_DRAW:TEXT] bottom justify text
!!
!!##SYNOPSIS
!!
!!        subroutine bottomjustify
!!
!!##DESCRIPTION
!!
!!    Bottom justifies text. The text string will be drawn with it's lower
!!    edge aligned with the current Y position. Top justification and Y
!!    centering are turned off.
!===================================================================================================================================
!>
!!##NAME
!!    ycentertext(3f) - [M_DRAW:TEXT] center text in the Y direction
!!
!!##SYNOPSIS
!!
!!        subroutine ycentertext()
!!
!!##DESCRIPTION
!!    Centers text in the Y direction. The text string will so that it's
!!    center line is aligned with the current y position. Top justification
!!    and Bottom justification are turned off.
!===================================================================================================================================
!>
!!##NAME
!!    textslant(3f) - [M_DRAW:TEXT] Defines the obliqueness of the fonts.
!!
!!##SYNOPSIS
!!
!!        subroutine textslant(var)
!!        real var
!!
!!##DESCRIPTION
!!
!!    Defines the obliqueness of the fonts. This is a simplistic method that
!!    allows you to generate italicized versions of the software fonts. The x-
!!    values of the software font coordinates after the current textsize()
!!    values are applied are multiplied by (1+val).
!!
!!    Note that this means the same value tilts the characters less the taller
!!    the characters are relative to their width.
!!
!!    Generally, practical values are generally between -1 and 1 times the
!===================================================================================================================================
!>
!!##NAME
!!    textweight(3f) - [M_DRAW:TEXT] Defines the weight of the fonts.
!!
!!##SYNOPSIS
!!
!!        subroutine textweight(ival)
!!        integer ival
!!##DESCRIPTION
!!
!!    Defines the weight of the fonts. Currently, the predefined constants
!!    in C and Fortran are V_NORMAL and V_BOLD; which correspond to 0 and
!!    1. This is not the same as using linethickess to change the appearance
!!    of a software font. The font is redrawn multiple times with a slight
!!    offset to create the bold appearance.
!===================================================================================================================================
!>
!!##NAME
!!    linewidth(3f) - [M_DRAW:LINESTYLE] set line width in rasters
!!
!!##SYNOPSIS
!!
!!         subroutine linewidth(iwidth)
!!         integer iwidth
!!##DESCRIPTION
!!
!!    Set the current line width in units of 1/10,000 of the X size of the
!!    display surface
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_linewidth
!!    use M_DRAW,    only : prefsize, vinit, ortho2, clear, getkey
!!    use M_DRAW,    only : move2, draw2, vexit, color, linewidth
!!    use M_units,    only : d2r, polar_to_cartesian
!!    implicit none
!!    integer :: i
!!    integer :: ipaws
!!    real    :: x,y,r,a,b,theta
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=a+b*theta
!!    ! Changing the parameter A will turn the spiral,
!!    ! while b controls the distance between successive turnings.
!!       call prefsize(401,401)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!       call clear()
!!       call move2(0.0,0.0)
!!       call color(2)
!!       a=0.0
!!       b=2.0
!!       do i=0,360*10,5
!!          theta=d2r(i)
!!          r=a+b*theta
!!          call polar_to_cartesian(r,theta,x,y)
!!          call linewidth(i/5/3)
!!          call draw2(x,y)
!!       enddo
!!       ipaws=getkey()
!!       call vexit()
!!    end program demo_linewidth
!===================================================================================================================================
!>
!!##NAME
!!    dashcode(3f) - [M_DRAW:LINESTYLE] set dash pattern length
!!
!!##SYNOPSIS
!!
!!         subroutine dashcode(dashlen)
!!         real dashlen
!!
!!##DESCRIPTION
!!
!!    Set the current dash length (in world units) to be dashlen.
!!
!!##IMAGE
!!
!!    The sample graphic shows a line segment being drawn using the same
!!    linestyle except the dashcode is being changed. Note that the dashcode
!!    is in world units,
!===================================================================================================================================
!>
!!##NAME
!!    linestyle(3f) - [M_DRAW:LINESTYLE] set the line dash pattern
!!
!!##SYNOPSIS
!!
!!         subroutine linestyle(style)
!!         character *(*) style
!!##DESCRIPTION
!!
!!    Set the current linestyle to style. Linestyles are specified by giving a
!!    nominal length of a single dash and a character string consisting of 1's
!!    and 0's (zeros) that specify when to draw a dash and when not to draw
!!    a dash. "1" is for a dash , "0" is for a gap. Linestyles will follow
!!    curves and "go around" corners.
!!
!!    To reset to a solid line style, enter a linestyle of " ". If a linestyle
!!    is set or reset, the accumulated information as to where on a curve
!!    (or line) a dash is to be draw is also reset.
!===================================================================================================================================
!>
!!##NAME
!!    clear(3f) - [M_DRAW:COLOR] Clears screen to current color
!!
!!##SYNOPSIS
!!
!!         subroutine clear()
!!
!!##DESCRIPTION
!!
!!    Clears the screen to the current color.
!!
!!##EXAMPLE
!!
!!      program demo_clear
!!      use M_DRAW, only    : prefsize, vinit, ortho2, clear, getkey
!!      use M_DRAW, only    : vexit, color, circle, polyfill
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(300,300)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      ipaws=getkey()
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!
!!      call color(0)               ! set current  color
!!      call clear()                ! clear background to current color
!!      call color(1)               ! set color to draw with
!!      call circle(0.0,0.0,50.0)
!!      ipaws=getkey()
!!
!!      call color(2)               ! make a second page
!!      call clear()
!!      call polyfill(.true.)
!!      call color(3)
!!      call circle(0.0,0.0,50.0)
!!      ipaws=getkey()
!!
!!      call vexit()
!!
!!      end program demo_clear
!===================================================================================================================================
!>
!!##NAME
!!    color(3f) - [M_DRAW:COLOR] Set current color
!!
!!##SYNOPSIS
!!
!!         subroutine color(col)
!!         integer,intent(in) :: col
!!
!!##DESCRIPTION
!!
!! Set the current color. The standard colors are as follows:
!!
!!       black  =  0  red      =  1  green  =  2  yellow  =  3
!!       blue   =  4  magenta  =  5  cyan   =  6  white   =  7
!!
!!
!!##OPTION
!!     COL  A color number from 0 to 255. To define additional
!!          colors see mapcolor(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_color
!!     use M_DRAW
!!     use M_strings,  only : v2s
!!     real    :: b=0.5
!!     real    :: y1,y2,ym,x1,x2
!!     real    :: width=50.0/8.0,width2
!!     integer :: i
!!     integer :: ipaws
!!        !! set up long bar as plotting area
!!        call prefsize(1000,200)
!!        call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!        call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!        call textsize( 3.5, 4.0)
!!        call font('DUPLEX')
!!        call centertext(.true.)
!!        call linewidth(90)
!!        y1=-5
!!        y2=5
!!        ym=0
!!        x1=-25+.05*width
!!        ! draw colored rectangle and a circle and label center of circle
!!        ! and repeat from colors 0 to 7.
!!        width2=width*0.95
!!        do i=0,7
!!           call color(i)
!!           x2=x1+width2
!!           call makepoly()
!!           call rect(x1,y1,x2,y2)
!!           call closepoly()
!!           call color(i+1)
!!           call move2((x1+x2)/2.0,ym)
!!           call drawstr((v2s(i)))     ! convert number to string and draw it
!!           call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!           x1=x1+width
!!        enddo
!!        ipaws=getkey()
!!        call vexit()
!!     end program demo_color
!===================================================================================================================================
!>
!!##NAME
!!    mapcolor(3f) - [M_DRAW:COLOR] set a color index using RGB values
!!
!!##SYNOPSIS
!!
!!         subroutine mapcolor(indx, red, green, blue)
!!         integer,intent(in) :: indx, red, green, blue
!!
!!##DESCRIPTION
!!
!!    Set the color map index indx to the color represented by (red, green,
!!    blue). If the device has no color map this call does nothing.
!!
!!    rgb values are in the range of 0 to 255.
!!
!!##OPTIONS
!!    INDX    color index number, in range 0 to 255
!!    RED     red component of color being defined, in range 0 to 255
!!    GREEN   green component of color being defined, in range 0 to 255
!!    BLUE    blue component of color being defined, in range 0 to 255
!!
!!##EXAMPLE
!!
!!  Color wheel EXAMPLE:
!!
!!    !     good program to exercise color tables, and look at differences
!!    !     when actual output device has a color table that is dynamic,
!!    !     or only has a small color table (a frame in this program takes
!!    !     at least SLICES*RINGS colors to produce accurately).
!!    !
!!    program demo_mapcolor
!!    use M_DRAW
!!    use M_DRAWPLUS, only : biggest_ortho2
!!    use m_color, only : hue
!!    use M_units, only : cosd, sind
!!    implicit none
!!       character(len=4096)  :: filename
!!       real                 :: lightstep
!!       integer              :: ii,iframe
!!       integer,parameter    :: SLICES=30
!!       integer,parameter    :: RINGS=  8
!!       real                 :: LIGHTNESS
!!       integer,parameter    :: BOX=1200
!!       integer              :: ipaws
!!       call prefsize(BOX,BOX)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call color(0)
!!       call clear()
!!       call color(7)
!!       call biggest_ortho2(-110./2.,85./2.,-110./2.,110./2.)
!!       LIGHTNESS=100.0
!!       lightstep=-5
!!       do ii=1,19
!!          iframe=ii
!!          call color(0)
!!          call clear()
!!          call color(7)
!!          call wheel()
!!          LIGHTNESS=LIGHTNESS+LIGHTSTEP
!!          ipaws=getkey()
!!       enddo
!!       call vexit()
!!    contains
!!    !=======================================================================--------
!!    subroutine wheel() ! draw an entire wheel
!!       character(len=40) :: inline
!!       real              :: hue_val
!!       integer           :: ii
!!       call textang(0.0)
!!       call color(7)
!!       call textsize(5.0,6.0)
!!       call font('times.r')
!!       call move2(0.0,103.0/2.0)
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call drawstr('COLOR WHEEL')
!!       call linewidth(0)
!!       call textsize( 2.5,2.5)
!!       call font('futura.l')
!!       call move2(0.0,90.0/2.0)
!!       write(inline,'("lightness=",f6.2)')LIGHTNESS
!!       call linewidth(30)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       call textsize(1.5,1.5)
!!       hue_val=0
!!       do ii=SLICES, 1,-1
!!          call slice(hue_val)
!!       enddo
!!       call centertext(.false.)
!!    end subroutine wheel
!!    !=======================================================================--------
!!    subroutine slice(hue_val) ! draw a slice
!!       integer           :: buffer
!!       real              :: hue_val, ang_inc
!!       character(len=40) :: inline
!!       real              :: step
!!       real              :: X1, X2, X3, X4
!!       real              :: Y1, Y2, Y3, Y4
!!       !
!!       integer           :: maxcolors, current_color
!!       integer           :: ir, ig, ib
!!       real              :: r,g,b
!!       real              :: saturation
!!       !
!!       integer           :: status
!!       integer           :: icount
!!       real              :: angle1, angle2
!!       real              :: radius1, radius2, radius3, radius4
!!       !
!!       integer,save      :: color_count=0
!!       !
!!       buffer=8
!!       ANG_INC=360.0/SLICES
!!       angle1=hue_val-ANG_INC/2
!!       angle2=angle1+ANG_INC
!!       saturation=100
!!       radius1=32
!!       radius3=radius1+4
!!       radius4=radius1+7
!!       ! draw tic from wheel to start of angle label
!!       call color(7)
!!       call linewidth(40)
!!       call move2( radius1*cosd(hue_val), radius1*sind(hue_val) )
!!       call draw2( radius3*cosd(hue_val), radius3*sind(hue_val) )
!!       ! draw degree label at tic
!!       call textang(hue_val)
!!       call move2( radius4*cosd(hue_val), radius4*sind(hue_val) )
!!       write(inline,'(i0)')nint(hue_val)
!!       call linewidth(20)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       step=radius1/(RINGS)
!!       radius2=radius1-step
!!       ! draw a chunk in a slice
!!       MAXCOLORS=(256)-buffer
!!       do icount=RINGS+1,2,-1
!!          CURRENT_COLOR=MOD(color_count,MAXCOLORS)+buffer  ! add buffer to leave base colors alone
!!          color_count=color_count+1
!!          ! fancy mapcolor
!!          call hue("hls",hue_val,LIGHTNESS,saturation,"rgb",r,g,b,status)
!!          ir=int(r*255.0/100.0+0.50)
!!          ig=int(g*255.0/100.0+0.50)
!!          ib=int(b*255.0/100.0+0.50)
!!          call mapcolor(CURRENT_COLOR,ir,ig,ib)
!!          call color(CURRENT_COLOR)
!!          !
!!          X1=cosd(angle1)*radius2
!!          Y1=sind(angle1)*radius2
!!          X2=cosd(angle1)*radius1
!!          Y2=sind(angle1)*radius1
!!          !
!!          X3=cosd(angle2)*radius2
!!          Y3=sind(angle2)*radius2
!!          X4=cosd(angle2)*radius1
!!          Y4=sind(angle2)*radius1
!!          !
!!          call makepoly()
!!          call move2(X1,Y1)
!!          call draw2(X2,Y2)
!!          call draw2(X4,Y4)
!!          call draw2(X3,Y3)
!!          call closepoly()
!!          !
!!          saturation=saturation-100.0/RINGS
!!          radius1=radius2
!!          radius2=radius1-step
!!       enddo
!!       hue_val=hue_val+ANG_INC
!!    end subroutine slice
!!    end program demo_mapcolor
!!
!!##IMAGE
!===================================================================================================================================
!>
!!##NAME
!!    clipping(3f) - [M_DRAW:CLIPPING] Turn clipping on or off
!!
!!##SYNOPSIS
!!
!!          subroutine clipping(onoff)
!!
!!          logical onoff
!!##DESCRIPTION
!!
!! Turn clipping on or off. Non-zero is considered on. Note: on some devices
!! turning clipping off may not be a good idea.
!===================================================================================================================================
!>
!!##NAME
!!    getkey(3f) - [M_DRAW:INTERACTIVE] Return ASCII ordinal of next key typed
!!
!!##SYNOPSIS
!!
!!          integer function getkey
!!
!!##DESCRIPTION
!!
!!    Return the ASCII ordinal of the next key typed at the keyboard. If the
!!    device has no keyboard getkey returns -1.
!===================================================================================================================================
!>
!!##NAME
!!    checkkey(3f) - [M_DRAW:INTERACTIVE] Returns zero if no key is pressed or ASCII ordinal
!!
!!##SYNOPSIS
!!
!!          integer function checkkey()
!!
!!##DESCRIPTION
!!
!!    Returns zero if no key is pressed or the ASCII ordinal of the key
!!    that was pressed.
!===================================================================================================================================
!>
!!##NAME
!!    getstring(3f) - [M_DRAW:INTERACTIVE] Read in a string, echoing it in current font
!!
!!##SYNOPSIS
!!
!!          integer function getstring(bcol, string)
!!          integer bcol
!!          character *(*) string
!!##DESCRIPTION
!!
!!    Read in a string, echoing it in the current font, using the current
!!    color and the current transformation. bcol is the background color which
!!    is used for erasing characters after a backspace or a delete key is
!!    received. Getstring interprets the Backspace key (ASCII 8) and the Del
!!    key (ASCII 127) as erasing characters. An EOT (ASCII 4) or a Carriage
!!    return (ASCII 13) will terminate input. Getstring returns the number
!!    of characters read. Getstring does not check for overflow in the input
!!    buffer string
!===================================================================================================================================
!>
!!##NAME
!!    locator(3f) - [M_DRAW:INTERACTIVE] Find out where cursor is
!!
!!##SYNOPSIS
!!
!!          integer function locator(xaddr, yaddr)
!!          real xaddr, yaddr
!!##DESCRIPTION
!!
!!    Find out where the cursor is. xaddr and yaddr are set to the current
!!    location in world coordinates. The function returns a bit pattern
!!    which indicates which buttons are being held down eg. if mouse buttons
!!    1 and 3 are down locator returns binary 101 (decimal 7). The function
!!    returns -1 if the device has no locator capability. Note: if you have
!!    been doing a lot of 3-D transformations xaddr and yaddr may not make a
!!    lot of sense. In this case use slocator.
!===================================================================================================================================
!>
!!##NAME
!!    slocator(3f) - [M_DRAW:INTERACTIVE] Find out where cursor is in screen coordinates
!!
!!##SYNOPSIS
!!
!!          integer function slocator(xaddr, yaddr)
!!          real xaddr, yaddr
!!##DESCRIPTION
!!
!!    Find out where the cursor is. xaddr and yaddr are set to the current
!!    location in screen coordinates. The return value of the function is
!!    set up in the same way as with locator. If the device has no locator
!!    device slocator returns -1.
!===================================================================================================================================
!>
!!##NAME
!!    vsetflush(3f) - [M_DRAW:FLUSHING] Set global flushing status
!!
!!##SYNOPSIS
!!
!!          subroutine vsetflush(yesno)
!!          logical yesno
!!
!!##DESCRIPTION
!!    Set global flushing status. If yesno = 0 (.false.) then don't do any
!!    flushing (except in swapbuffers(), or vflush()). If yesno = 1
!!    (.true.) then do the flushing as described above.
!===================================================================================================================================
!>
!!##NAME
!!    vflush(3f) - [M_DRAW:FLUSHING] Call device flush or syncronisation routine
!!
!!##SYNOPSIS
!!
!!          subroutine vflush
!!
!!##DESCRIPTION
!!
!!    Call the device flush or syncronisation routine. This forces a flush.
!===================================================================================================================================
!>
!!##NAME
!!    viewport(3f) - [M_DRAW:VIEWPORT] Specify which part of screen to draw in
!!
!!##SYNOPSIS
!!
!!          subroutine viewport(left, right, bottom, top)
!!          real,intent(in) :: left, right, bottom, top
!!##DESCRIPTION
!!    Specify which part of the screen to draw in. Left, right, bottom,
!!    and top are real values in screen coordinates (0:n,0:m).
!!
!!    If a device has been declared to be 600 x 400
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!===================================================================================================================================
!>
!!##NAME
!!    pushviewport(3f) - [M_DRAW:VIEWPORT] Save current viewport
!!
!!##SYNOPSIS
!!
!!          subroutine pushviewport
!!##DESCRIPTION
!!
!!    Save current viewport.
!===================================================================================================================================
!>
!!##NAME
!!    popviewport(3f) - [M_DRAW:VIEWPORT] Retrieve last viewport
!!
!!##SYNOPSIS
!!
!!          subroutine popviewport
!!
!!##DESCRIPTION
!!
!!    Retrieve last viewport.
!===================================================================================================================================
!>
!!##NAME
!!    getviewport(3f) - [M_DRAW:VIEWPORT] Returns limits of current viewport in screen coordinates
!!
!!##SYNOPSIS
!!
!!          subroutine getviewport(left, right, bottom, top)
!!          real,intent(out)    :: left
!!          real,intent(out)    :: right
!!          real,intent(out)    :: bottom
!!          real,intent(out)    :: top
!!
!!##DESCRIPTION
!!
!! Returns the left, right, bottom and top limits of the current viewport
!! in screen coordinates (-1.0 to 1.0).
!!
!!    If a device has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##OPTIONS
!!    LEFT     value for left side
!!    RIGHT    value for right side
!!    BOTTOM   value for bottom side
!!    TOP      value for top side
!===================================================================================================================================
!>
!!##NAME
!!    expandviewport(3f) - [M_DRAW:VIEWPORT] use the entire device viewport
!!
!!##SYNOPSIS
!!
!!         subroutine expandviewport
!!
!!##DESCRIPTION
!!
!!    When M_DRAW does viewport calculations, it will normally begin by using
!!    the largest square it can fit onto the actual display device. This
!!    call says to use the whole device... however you must then take
!!    into account any distortion that will occur due to the non-square
!!    mapping. Thus, a viewport of (-1.0, 1.0, -1.0, 1.0) will map into
!!    the whole display device.
!===================================================================================================================================
!>
!!##NAME
!!    unexpandviewport(3f) - [M_DRAW:VIEWPORT] undo expandviewport(3f)
!!
!!##SYNOPSIS
!!
!!         subroutine unexpandviewport
!!
!!##DESCRIPTION
!!
!!    Does the reverse of expandviewport. Basically, it returns M_DRAW to
!!    using the largest square of the device for it's viewport calculations.
!===================================================================================================================================
!>
!!##NAME
!!    getaspect(3f) - [M_DRAW:ASPECT] Returns the ratio height over width of the display device.
!!
!!##SYNOPSIS
!!
!!          real function getaspect()
!!
!!##DESCRIPTION
!!    Returns the ratio height over width of the display device.
!===================================================================================================================================
!>
!!##NAME
!!    getfactors(3f) - [M_DRAW:ASPECT] Returns width over min(width of device, height of device) and height over min(width of device, height of device).
!!
!!##SYNOPSIS
!!
!!          subroutine getfactors(w, h)
!!          real w, h
!!##DESCRIPTION
!!    Returns wfact as the width over min(width of device, height of device)
!!    and hfact as the height over min(width of device, height of
!!    device).
!===================================================================================================================================
!>
!!##NAME
!!    getdisplaysize(3f) - [M_DRAW:ASPECT] Returns width and height of device in device units
!!
!!##SYNOPSIS
!!
!!          subroutine getdisplaysize(w, h)
!!          real,intent(in) :: w, h
!!
!!##DESCRIPTION
!!    Returns the width and height of the device in device units in w and h
!!    respectively.
!===================================================================================================================================
!>
!!##NAME
!!    pushattributes(3f) - [M_DRAW:ATTRIBUTE_STACK] Save the current attributes on the attribute stack.
!!
!!
!!##SYNOPSIS
!!
!!          subroutine pushattributes
!!
!!##DESCRIPTION
!!
!!    Save the current attributes on the attribute stack.
!===================================================================================================================================
!>
!!##NAME
!!    popattributes(3f) - [M_DRAW:ATTRIBUTE_STACK] Restore attributes to what they were at last pushattributes().
!!
!!##SYNOPSIS
!!
!!          subroutine popattributes
!!
!!##DESCRIPTION
!!
!!    Restore the attributes to what they were at the last pushattributes().
!===================================================================================================================================
!>
!!##NAME
!!    ortho(3f) - [M_DRAW:PROJECTION] Define x,y,z clipping planes.
!!
!!##SYNOPSIS
!!
!!          subroutine ortho(left, right, bottom, top, near_d, far_d)
!!          real left, right, bottom, top, near_d, far_d
!!##DESCRIPTION
!!
!!    Define x (left, right), y (bottom, top), and z (near, far) clipping
!!    planes. The near and far clipping planes are actually specified
!!    as distances along the line of sight. These distances can also be
!!    negative. The actual location of the clipping planes is z = -near_d
!!    and z = -far_d.
!===================================================================================================================================
!>
!!##NAME
!!    ortho2(3f) - [M_DRAW:PROJECTION] define the area of the virtual world coordinates to map to the viewport
!!
!!##SYNOPSIS
!!
!!          subroutine ortho2(left, right, bottom, top)
!!          real,intent(in) :: left, right, bottom, top
!!##DESCRIPTION
!!
!!    Defines the section of the virtual world coordinates to map to
!!    the viewport. That is, Define x (left, right), and y (bottom, top)
!!    clipping planes.
!!
!!    All the projection routines define a new transformation
!!    matrix, and consequently the world units. Parallel projections are
!!    defined by ortho2.
!===================================================================================================================================
!>
!!##NAME
!!    perspective(3f) - [M_DRAW:PROJECTION] Specify perspective viewing pyramid
!!
!!##SYNOPSIS
!!
!!          subroutine perspective(fov, aspect, near, far)
!!          real fov, aspect, near, far
!!##DESCRIPTION
!!    Specify a perspective viewing pyramid in world coordinates by giving
!!    a field of view, aspect ratio and the distance from the eye of the
!!    near and far clipping plane.
!===================================================================================================================================
!>
!!##NAME
!!    window(3f) - [M_DRAW:PROJECTION] Specify a perspective viewing pyramid
!!
!!##SYNOPSIS
!!
!!          subroutine window(left, right, bot, top, near, far)
!!          real left, right, bot, top, near, far
!!##DESCRIPTION
!!
!!    Specify a perspective viewing pyramid in world coordinates by giving
!!    the rectangle closest to the eye (ie. at the near clipping plane)
!!    and the distances to the near and far clipping planes.
!===================================================================================================================================
!>
!!##NAME
!!    pushmatrix(3f) - [M_DRAW:MATRIX_STACK] Save the current transformation matrix on the matrix stack.
!!
!!##SYNOPSIS
!!
!!          subroutine pushmatrix
!!
!!##DESCRIPTION
!!
!!    Save the current transformation matrix on the matrix stack.
!===================================================================================================================================
!>
!!##NAME
!!    popmatrix(3f) - [M_DRAW:MATRIX_STACK] Reinstall the last matrix pushed
!!
!!##SYNOPSIS
!!
!!          subroutine popmatrix
!!
!!##DESCRIPTION
!!
!!    Retrieve the last matrix pushed and make it the current transformation matrix.
!===================================================================================================================================
!>
!!##NAME
!!    polarview(3f) - [M_DRAW:VIEWPORT] Specify the viewer's position in polar coordinates
!!
!!##SYNOPSIS
!!
!!          subroutine polarview(dist, azim, inc, twist)
!!          real dist, azim, inc, twist
!!##DESCRIPTION
!!
!! Specify the viewer's position in polar coordinates by giving the distance
!! from the viewpoint to the world origin, the azimuthal angle in the x-y
!! plane, measured from the y-axis, the incidence angle in the y-z plane,
!! measured from the z-axis, and the twist angle about the line of sight.
!===================================================================================================================================
!>
!!##NAME
!!    up(3f) - [M_DRAW:VIEWPORT] Specify the world up.
!!
!!##SYNOPSIS
!!
!!          subroutine up(x, y, z)
!!          real x, y, z
!!
!!##DESCRIPTION
!! Specify the world up. This can be used to prevent lookat's sometimes
!! annoying habit of turning everything upside down due to the line of
!! sight crossing the appropriate axis.
!===================================================================================================================================
!>
!!##NAME
!!    lookat(3f) - [M_DRAW:VIEWPORT] Specify the viewer's position
!!
!!##SYNOPSIS
!!
!!          subroutine lookat(vx, vy, vz, px, py, pz, twist)
!!          real vx, vy, vz, px, py, pz, twist
!!
!!##DESCRIPTION
!!    Specify the viewer's position by giving a viewpoint and a reference
!!    point in world coordinates. A twist about the line of sight may also
!!    be given.
!===================================================================================================================================
!>
!!##NAME
!!    translate(3f) - [M_DRAW:TRANSFORMATION] Set up a translation.
!!
!!##SYNOPSIS
!!
!!       subroutine translate(x, y, z)
!!       real x, y, z
!!
!!##DESCRIPTION
!!
!!    Set up a translation.
!===================================================================================================================================
!>
!!##NAME
!!    scale(3f) - [M_DRAW:TRANSFORMATION] Set up scaling factors in x, y, and z axis.
!!
!!##SYNOPSIS
!!
!!          subroutine scale(x, y, z)
!!
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Set up scaling factors in x, y, and z axis.
!===================================================================================================================================
!>
!!##NAME
!!    rotate(3f) - [M_DRAW:TRANSFORMATION] Set up a rotation in axis axis where axis is one of 'x','y', or 'z'.
!!
!!##SYNOPSIS
!!
!!          subroutine rotate(angle, axis)
!!          real angle
!!          character axis
!!##DESCRIPTION
!!
!!    Set up a rotation in axis axis. Where axis is one of 'x', 'y', or 'z'.
!===================================================================================================================================
!>
!!##NAME
!!    patchbasis(3f) - [M_DRAW:PATCH] Define the t and u basis matrices of a patch.
!!
!!##SYNOPSIS
!!
!!          subroutine patchbasis(tbasis, ubasis)
!!          real tbasis(4, 4), ubasis(4, 4)
!!##DESCRIPTION
!!
!!    Define the t and u basis matrices of a patch.
!===================================================================================================================================
!>
!!##NAME
!!    patchprecision(3f) - [M_DRAW:PATCH] Set minimum number of line segments making up curves in a patch.
!!
!!##SYNOPSIS
!!
!!          subroutine patchprecision(tseg, useg)
!!          integer tseg, useg
!!
!!##DESCRIPTION
!!    Set the minimum number of line segments making up curves in a patch.
!===================================================================================================================================
!>
!!##NAME
!!    patchcurves(3f) - [M_DRAW:PATCH] Set the number of curves making up a patch.
!!
!!##SYNOPSIS
!!
!!          subroutine patchcurves(nt, nu)
!!          integer nt, nu
!!
!!##DESCRIPTION
!!
!!    Set the number of curves making up a patch.
!===================================================================================================================================
!>
!!##NAME
!!    rpatch(3f) - [M_DRAW:PATCH] Draws a rational patch in the current basis, according to the geometry matrices gx, gy, gz, and gw.
!!
!!##SYNOPSIS
!!
!!          subroutine rpatch(gx, gy, gz, gw)
!!          real  gx(4,4), gy(4,4), gz(4,4), gw(4,4)
!!##DESCRIPTION
!!
!!    Draws a rational patch in the current basis, according to the geometry
!!    matrices gx, gy, gz, and gw.
!===================================================================================================================================
!>
!!##NAME
!!    patch(3f) - [M_DRAW:PATCH] Draws a patch in the current basis, according to the geometry matrices gx, gy, and gz.
!!
!!##SYNOPSIS
!!
!!          subroutine patch(gx, gy, gz)
!!          real  gx(4,4), gy(4,4), gz(4,4)
!!##DESCRIPTION
!!
!!    Draws a patch in the current basis, according to the geometry matrices
!!    gx, gy, and gz.
!!
!===================================================================================================================================
!>
!!##NAME
!!    makeobj(3f) - [M_DRAW:OBJECT] Commence the object number n.
!!##SYNOPSIS
!!
!!         subroutine makeobj(n)
!!         integer n
!!
!!##DESCRIPTION
!!    Commence the object number n.
!===================================================================================================================================
!>
!!##NAME
!!    closeobj(3f) - [M_DRAW:OBJECT] Close the current object.
!!
!!##SYNOPSIS
!!
!!         subroutine closeobj()
!!##DESCRIPTION
!!
!!    Close the current object.
!===================================================================================================================================
!>
!!##NAME
!!    genobj(3f) - [M_DRAW:OBJECT] Returns a unique object identifier.
!!
!!##SYNOPSIS
!!
!!         integer function genobj()
!!##DESCRIPTION
!!    Returns a unique object identifier.
!===================================================================================================================================
!>
!!##NAME
!!    getopenobj(3f) - [M_DRAW:OBJECT] Return the number of the current object.
!!
!!##SYNOPSIS
!!
!!         integer function getopenobj()
!!
!!##DESCRIPTION
!!    Return the number of the current object.
!===================================================================================================================================
!>
!!##NAME
!!    callobj(3f) - [M_DRAW:OBJECT] Draw object number n.
!!
!!##SYNOPSIS
!!
!!         subroutine callobj(n)
!!         integer n
!!##DESCRIPTION
!!    Draw object number n.
!===================================================================================================================================
!>
!!##NAME
!!    isobj(3f) - [M_DRAW:OBJECT] Returns non-zero if there is an object of number n.
!!
!!##SYNOPSIS
!!
!!         logical function isobj(n)
!!         integer n
!!##DESCRIPTION
!!
!!    Returns non-zero if there is an object of number n.
!===================================================================================================================================
!>
!!##NAME
!!    delobj(3f) - [M_DRAW:OBJECT] Delete the object number n.
!!
!!##SYNOPSIS
!!
!!         subroutine delobj(n)
!!         integer n
!!##DESCRIPTION
!!    Delete the object number n.
!===================================================================================================================================
!>
!!##NAME
!!    loadobj(3f) - [M_DRAW:OBJECT] Load the object in the file filename as object number n.
!!
!!##SYNOPSIS
!!
!!         subroutine loadobj(n, filename)
!!         integer n
!!         character*(*) filename
!!##DESCRIPTION
!!    Load the object in the file "filename" as object number n.
!===================================================================================================================================
!>
!!##NAME
!!    saveobj(3f) - [M_DRAW:OBJECT] Save object number n into file filename. Does NOT save objects called inside object n.
!!
!!##SYNOPSIS
!!
!!         saveobj(n, filename)
!!         integer   n
!!         character*(*) filename
!!##DESCRIPTION
!!    Save the object number n into the file filename. This call does not save
!!    objects called inside object n.
!===================================================================================================================================
!>
!!##NAME
!!    backbuffer(3f) - [M_DRAW:DOUBLE_BUFFERING] Draw in the backbuffer. Returns -1 if the device is not up to it.
!!
!!##SYNOPSIS
!!
!!          integer function backbuffer()
!!
!!##DESCRIPTION
!!    Make M_DRAW draw in the backbuffer. Returns -1 if the device is not up to it.
!===================================================================================================================================
!>
!!##NAME
!!    frontbuffer(3f) - [M_DRAW:DOUBLE_BUFFERING] Draw in the front buffer. This will always work.
!!
!!##SYNOPSIS
!!
!!          subroutine frontbuffer()
!!
!!##DESCRIPTION
!!
!!    Make M_DRAW draw in the front buffer. This will always work.
!===================================================================================================================================
!>
!!##NAME
!!    swapbuffers(3f) - [M_DRAW:DOUBLE_BUFFERING] Swap the front and back buffers.
!!
!!##SYNOPSIS
!!
!!          subroutine swapbuffers()
!!
!!##DESCRIPTION
!!    Swap the front and back buffers.
!===================================================================================================================================
!>
!!##NAME
!!    getgp(3f) - [M_DRAW:POSITION] Gets the current graphics position
!!
!!##SYNOPSIS
!!
!!          subroutine getgp(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Gets the current graphics position in world coords.
!===================================================================================================================================
!>
!!##NAME
!!    getgpt(3f) - [M_DRAW:POSITION] Gets the current transformed graphics position in world coords.
!!
!!##SYNOPSIS
!!
!!          subroutine getgpt(x, y, z, w)
!!          real x, y, z, w
!!##DESCRIPTION
!!
!!    Gets the current transformed graphics position in world coords.
!===================================================================================================================================
!>
!!##NAME
!!    getgp2(3f) - [M_DRAW:POSITION] Gets the current graphics position in world coordinates
!!##SYNOPSIS
!!
!!          subroutine getgp2(x, y)
!!          real,intent(out) :: x,y
!!##DESCRIPTION
!!
!!    Gets the current graphics position in world coords.
!!
!!##RETURNS
!!    X  X coordinate of current position
!!    Y  Y coordinate of current position
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!      program demo_getgp2
!!      use M_DRAW
!!      implicit none
!!      real :: X,Y
!!      call prefsize(20,20)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!      call move2(0.0,0.0)
!!      call draw2(96.5,98.333)
!!
!!      call getgp2(X,Y)
!!      write(*,*)'CURRENT POSITION (X,Y)=',X,Y
!!
!!      call vexit()
!!      end program demo_getgp2
!!
!!   Results
!!
!!    CURRENT POSITION (X,Y)=   96.5000000       98.3330002
!===================================================================================================================================
!>
!!##NAME
!!    sgetgp2(3f) - [M_DRAW:POSITION] Gets the current screen graphics position in screen coords (-1 to 1)
!!
!!##SYNOPSIS
!!
!!          subroutine sgetgp2(x, y)
!!          real x, y
!!
!!##DESCRIPTION
!!    Gets the current screen graphics position in screen coords (-1 to 1)
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
module M_draw
! hide logicals from C
! trim and append null to intent(in) character strings
   use ISO_C_BINDING
   implicit none
   private
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: V_XCENTERED=     1_C_SHORT
integer(kind=c_short),public,parameter :: V_YCENTERED=     2_C_SHORT
integer(kind=c_short),public,parameter :: V_LEFT=          4_C_SHORT  ! The default
integer(kind=c_short),public,parameter :: V_RIGHT=         8_C_SHORT
integer(kind=c_short),public,parameter :: V_TOP=          16_C_SHORT
integer(kind=c_short),public,parameter :: V_BOTTOM=       32_C_SHORT ! The default
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: V_NORMAL=        0_C_SHORT ! The default
integer(kind=c_short),public,parameter :: V_BOLD=          1_C_SHORT
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: V_THICK=         1_C_SHORT
integer(kind=c_short),public,parameter :: V_THIN=          0_C_SHORT ! The default
!-------------------------------------------------------------------------------
integer(kind=c_int),public,parameter   :: V_BLACK    =  0_C_INT
integer(kind=c_int),public,parameter   :: V_RED      =  1_C_INT
integer(kind=c_int),public,parameter   :: V_GREEN    =  2_C_INT
integer(kind=c_int),public,parameter   :: V_YELLOW   =  3_C_INT
integer(kind=c_int),public,parameter   :: V_BLUE     =  4_C_INT
integer(kind=c_int),public,parameter   :: V_MAGENTA  =  5_C_INT
integer(kind=c_int),public,parameter   :: V_CYAN     =  6_C_INT
integer(kind=c_int),public,parameter   :: V_WHITE    =  7_C_INT
!-------------------------------------------------------------------------------
public MATRIX
type, bind(C) :: MATRIX
   real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
end type MATRIX
!-------------------------------------------------------------------------------
 public :: arc
 public :: arcprecision
 public :: backbuffer
 public :: backface
 public :: backfacedir
 public :: bottomjustify
 public :: boxfit
 public :: boxtext
 public :: CalcW2Vcoeffs
 public :: callobj
 public :: centertext
 public :: checkkey
 public :: circle
 public :: circleprecision
 public :: clear
 public :: clipping
 public :: closeobj
 public :: closepoly
 public :: color
 public :: curve
 public :: curvebasis
 public :: curven
 public :: curveprecision
 public :: dashcode
 public :: delobj
 public :: draw
 public :: draw2
 public :: drawchar
 public :: drawstr
 public :: expandviewport
 public :: fixedwidth
 public :: font
 public :: frontbuffer
 public :: genobj
 public :: getaspect
 public :: getcharsize
 public :: getdepth
 public :: getdisplaysize
 public :: getfactors
 public :: getfontasc
 public :: getfontdec
 public :: getfontheight
 public :: getfontsize
 public :: getfontwidth
 public :: getgp
 public :: getgp2
 public :: getgpt
 public :: getkey
 public :: getmatrix
 public :: getopenobj
 public :: getplanes
 public :: getprefposandsize
 public :: getstring
 public :: getviewport
 public :: hatchang
 public :: hatchpitch
 public :: isobj
 public :: isobj_F
 public :: leftjustify
 public :: linestyle
 public :: linewidth
 public :: loadmatrix
 public :: loadobj
 public :: locator
 public :: lookat
 public :: makeobj
 public :: makepoly
 public :: mapcolor
 public :: move
 public :: move2
 public :: multmatrix
 public :: numchars
 public :: ortho
 public :: ortho2
 public :: patch
 public :: patchbasis
 public :: patchcurves
 public :: patchprecision
 public :: pdraw
 public :: perspective
 public :: pmove
 public :: point
 public :: point2
 public :: polarview
 public :: poly
 public :: poly2
 public :: polyfill
 public :: polyhatch
 public :: popattributes
 public :: popdev
 public :: popmatrix
 public :: popviewport
 public :: prefposition
 public :: prefsize
 public :: printattribs
 public :: printvdevice
 public :: pushattributes
 public :: pushdev
 public :: pushmatrix
 public :: pushviewport
 public :: rcurve
 public :: rdraw
 public :: rdraw2
 public :: rect
 public :: rightjustify
 public :: rmove
 public :: rmove2
 public :: rotate
 public :: rsdraw2
 public :: rsmove2
 public :: saveobj
 public :: scale
 public :: sdraw2
 public :: sector
 public :: sgetgp2
 public :: slocator
 public :: smove2
 public :: spoint2
 public :: srect
 public :: strlength
 public :: strlength_F
 public :: swapbuffers
 public :: textang
 public :: textjustify
 public :: textsize
 public :: textslant
 public :: textweight
 public :: topjustify
 public :: translate
 public :: unexpandviewport
 public :: up
 public :: vexit
 public :: vflush
 public :: vgetdev
 public :: viewport
 public :: vinit
 public :: vnewdev
 public :: voutput
 public :: vsetflush
 public :: VtoWxy
 public :: window
 public :: xcentertext
 public :: ycentertext
 public :: yobbarays
!public :: verror
! integer,parameter :: C_BOOL = SELECTED_INT_KIND(1) ! _Bool ! integer*1
!-------------------------------------------------------------------------------
! ==========  function definitions
!-------------------------------------------------------------------------------
! ==========  arc routines
!-------------------------------------------------------------------------------
! extern void arcprecision(int noseg);
   interface
      subroutine arcprecision(N) bind(C,NAME='draw_arcprecision')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine arcprecision
   end interface
!-------------------------------------------------------------------------------
! extern void arc(float x,float y, float radius, float startang, float endang);
   interface
      subroutine arc(X,Y,RADIUS,STARTANG,ENDANG) bind(C,NAME='draw_arc')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: RADIUS
         real(KIND=C_FLOAT),intent(in),value :: STARTANG
         real(KIND=C_FLOAT),intent(in),value :: ENDANG
      end subroutine arc
   end interface
!-------------------------------------------------------------------------------
! extern void circle(float x,float y,float radius);
   interface
      subroutine circle(X,Y,RADIUS) bind(C,NAME='draw_circle')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: RADIUS
      end subroutine circle
   end interface
!-------------------------------------------------------------------------------
! extern void circleprecision(int noseg);
   interface
      subroutine circleprecision(N) bind(C,NAME='draw_circleprecision')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine circleprecision
   end interface
!-------------------------------------------------------------------------------
! extern void sector(float x,float y,float radius,float startang,float endang);
   interface
      subroutine sector(X,Y,RADIUS,STARTANG,ENDANG) bind(C,NAME='draw_sector')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: RADIUS
         real(KIND=C_FLOAT),intent(in),value :: STARTANG
         real(KIND=C_FLOAT),intent(in),value :: ENDANG
      end subroutine sector
   end interface
!-------------------------------------------------------------------------------
! ==========  attr routines
!-------------------------------------------------------------------------------
! extern void popattributes(void);
   interface
      subroutine popattributes() bind(C,NAME='draw_popattributes')
         use ISO_C_BINDING
         implicit none
      end subroutine popattributes
   end interface
!-------------------------------------------------------------------------------
! extern void pushattributes(void);
   interface
      subroutine pushattributes() bind(C,NAME='draw_pushattributes')
         use ISO_C_BINDING
         implicit none
      end subroutine pushattributes
   end interface
!-------------------------------------------------------------------------------
! extern void printattribs(char *s);
   interface
      subroutine printattribs_F(S) bind(C,NAME='draw_printattribs')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR) :: S(*)
      end subroutine printattribs_F
   end interface
!-------------------------------------------------------------------------------
! extern void printvdevice(char *s);
   interface
      subroutine printvdevice_F(S) bind(C,NAME='draw_printvdevice')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR) :: S(*)
      end subroutine printvdevice_F
   end interface
!-------------------------------------------------------------------------------
! ==========  curve routines
!-------------------------------------------------------------------------------
! extern void curve(float geom[4][3]);
   interface
      subroutine curve(GEOM) bind(C,NAME='draw_curve')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),dimension(3,4) :: GEOM
      end subroutine curve
   end interface
!-------------------------------------------------------------------------------
! extern void rcurve_F(Matrix geom);
   interface
      subroutine rcurve_F(GEOM) bind(C,NAME='draw_rcurve')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOM
         type(MATRIX),intent(in) :: GEOM
      end subroutine rcurve_F
   end interface
!-------------------------------------------------------------------------------
! extern void curven(int n, float geom[][3]);
   interface
      subroutine curven(N,GEOM) bind(C,NAME='draw_curven')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         real(KIND=C_FLOAT),intent(in),dimension(3,*) :: GEOM
      end subroutine curven
   end interface
!-------------------------------------------------------------------------------
! extern void drcurve(int n, Matrix r);
!-------------------------------------------------------------------------------
! extern void curvebasis_F(Matrix basis);
   interface
      subroutine curvebasis_F(BASIS) bind(C,NAME='draw_curvebasis')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(in),dimension(4,4) :: BASIS
         type(MATRIX),intent(in) :: BASIS
      end subroutine curvebasis_F
   end interface
!-------------------------------------------------------------------------------
! extern void curveprecision(int nsegments);
   interface
      subroutine curveprecision(NSEGMENTS) bind(C,NAME='draw_curveprecision')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: NSEGMENTS
      end subroutine curveprecision
   end interface
!-------------------------------------------------------------------------------
! ==========  draw routines
!-------------------------------------------------------------------------------
! extern void draw(float x,float y, float z);
   interface
      subroutine draw(X,Y,Z) bind(C,NAME='draw_draw')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine draw
   end interface
!-------------------------------------------------------------------------------
! extern void draw2(float x,float y);
   interface
      subroutine draw2(X,Y) bind(C,NAME='draw_draw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine draw2
   end interface
!-------------------------------------------------------------------------------
! extern void rdraw(float dx,float dy,float dz);
   interface
      subroutine rdraw(X,Y,Z) bind(C,NAME='draw_rdraw')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine rdraw
   end interface
!-------------------------------------------------------------------------------
! extern void rdraw2(float dx, float dy);
   interface
      subroutine rdraw2(X,Y) bind(C,NAME='draw_rdraw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rdraw2
   end interface
!-------------------------------------------------------------------------------
! extern void sdraw2(float xs, float ys);
   interface
      subroutine sdraw2(X,Y) bind(C,NAME='draw_sdraw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine sdraw2
   end interface
!-------------------------------------------------------------------------------
! extern void rsdraw2(float dxs, float dys);
   interface
      subroutine rsdraw2(X,Y) bind(C,NAME='draw_rsdraw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rsdraw2
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void dashcode(float d);
   interface
      subroutine dashcode(D) bind(C,NAME='draw_dashcode')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_float),intent(in),value :: D
      end subroutine dashcode
   end interface
!-------------------------------------------------------------------------------
! extern void dashline(Vector p0, Vector p1);
!-------------------------------------------------------------------------------
! extern void linestyle(char *l);
   interface
      subroutine linestyle_F(L) bind(C,NAME='draw_linestyle')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: L(*)
      end subroutine linestyle_F
   end interface
!-------------------------------------------------------------------------------
! extern void linewidth(int w);
   interface
      subroutine linewidth(W) bind(C,NAME='draw_linewidth')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: W
      end subroutine linewidth
   end interface
!-------------------------------------------------------------------------------
! ==========  device routines
!-------------------------------------------------------------------------------
! extern void clear(void);
   interface
      subroutine clear() bind(C,NAME='draw_clear')
         use ISO_C_BINDING
         implicit none
      end subroutine clear
   end interface
!-------------------------------------------------------------------------------
! extern void color(int i);
   interface
      subroutine color(N) bind(C,NAME='draw_color')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine color
   end interface
!-------------------------------------------------------------------------------
! extern int      getkey(void);
   interface
      function getkey() bind(C,NAME='draw_getkey')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getkey
      end function getkey
   end interface
!-------------------------------------------------------------------------------
! extern int      getdepth(void);
   interface
      function getdepth() bind(C,NAME='draw_getdepth')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getdepth
      end function getdepth
   end interface
!-------------------------------------------------------------------------------
! extern int      checkkey(void);
   interface
      function checkkey() bind(C,NAME='draw_checkkey')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: checkkey
      end function checkkey
   end interface
!-------------------------------------------------------------------------------
! extern int      getplanes(void);
   interface
      function getplanes() bind(C,NAME='draw_getplanes')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getplanes
      end function getplanes
   end interface
!-------------------------------------------------------------------------------
! extern int      locator(float *wx,float *wy);
   interface
      function locator(WX,WY) bind(C,NAME='draw_locator')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: locator
         real(KIND=C_FLOAT),intent(out) :: WX
         real(KIND=C_FLOAT),intent(out) :: WY
      end function locator
   end interface
!-------------------------------------------------------------------------------
! extern int      slocator(float *wx,float *wy);
   interface
      function slocator(WX,WY) bind(C,NAME='draw_slocator')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: SLOCATOR
         real(KIND=C_FLOAT),intent(out) :: WX
         real(KIND=C_FLOAT),intent(out) :: WY
      end function slocator
   end interface
!-------------------------------------------------------------------------------
! extern void mapcolor(int i, short r, short g, short b);
   interface
      subroutine mapcolor(I,R,G,B) bind(C,NAME='draw_mapcolor')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: I
         integer(KIND=C_INT),intent(in),value :: R
         integer(KIND=C_INT),intent(in),value :: G
         integer(KIND=C_INT),intent(in),value :: B
      end subroutine mapcolor
   end interface
!-------------------------------------------------------------------------------
! extern void vinit(char *device);
   interface
      subroutine vinit_F(DEVICE) bind(C,NAME='draw_vinit')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: DEVICE(*)
      end subroutine vinit_F
   end interface
!-------------------------------------------------------------------------------
! extern void vexit(void);
   interface
      subroutine vexit() bind(C,NAME='draw_vexit')
         use ISO_C_BINDING
         implicit none
      end subroutine vexit
   end interface
!-------------------------------------------------------------------------------
! extern void voutput(char *path);
   interface
      subroutine voutput_F(PATH) bind(C,NAME='draw_voutput')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: PATH(*)
      end subroutine voutput_F
   end interface
!-------------------------------------------------------------------------------
! extern void vnewdev(char *device);
   interface
      subroutine vnewdev_F(DEVICE) bind(C,NAME='draw_vnewdev')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: DEVICE(*)
      end subroutine vnewdev_F
   end interface
!-------------------------------------------------------------------------------
! extern char     *vgetdev(char *buf);
   interface
      subroutine vgetdev_F(BUF) bind(C,NAME='draw_vgetdev')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(out) :: BUF(*)
      end subroutine vgetdev_F
   end interface
!-------------------------------------------------------------------------------
! extern void pushdev(char *device);
   interface
      subroutine pushdev_F(DEVICE) bind(C,NAME='draw_pushdev')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: DEVICE(*)
      end subroutine pushdev_F
   end interface
!-------------------------------------------------------------------------------
! extern void popdev(void);
   interface
      subroutine popdev() bind(C,NAME='draw_popdev')
         use ISO_C_BINDING
         implicit none
      end subroutine popdev
   end interface
!-------------------------------------------------------------------------------
! ==========  move routines
!-------------------------------------------------------------------------------
! extern void move(float x, float y, float z);
   interface
      subroutine move(X,Y,Z) bind(C,NAME='draw_move')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine move
   end interface
!-------------------------------------------------------------------------------
! extern void move2(float x, float y);
   interface
      subroutine move2(X,Y) bind(C,NAME='draw_move2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine move2
   end interface
!-------------------------------------------------------------------------------
! extern void rmove(float dx, float dy, float dz);
   interface
      subroutine rmove(X,Y,Z) bind(C,NAME='draw_rmove')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine rmove
   end interface
!-------------------------------------------------------------------------------
! extern void rmove2(float dx, float dy);
   interface
      subroutine rmove2(X,Y) bind(C,NAME='draw_rmove2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rmove2
   end interface
!-------------------------------------------------------------------------------
! extern void smove2(float xs, float ys);
   interface
      subroutine smove2(X,Y) bind(C,NAME='draw_smove2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine smove2
   end interface
!-------------------------------------------------------------------------------
! extern void rsmove2(float dxs, float dys);
   interface
      subroutine rsmove2(X,Y) bind(C,NAME='draw_rsmove2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rsmove2
   end interface
!-------------------------------------------------------------------------------
! ==========  object routines
!-------------------------------------------------------------------------------
! extern int    isobj(int n);
   interface
      function isobj_F(N) bind(C,NAME='draw_isobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: ISOBJ_F
         integer(KIND=C_INT),intent(in),value :: N
      end function isobj_F
   end interface
!-------------------------------------------------------------------------------
! extern int    genobj(void);
   interface
      function genobj() bind(C,NAME='draw_genobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: genobj
      end function genobj
   end interface
!-------------------------------------------------------------------------------
! extern void delobj(int n);
   interface
      subroutine delobj(N) bind(C,NAME='draw_delobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine delobj
   end interface
!-------------------------------------------------------------------------------
! extern void makeobj(int n);
   interface
      subroutine makeobj(N) bind(C,NAME='draw_makeobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine makeobj
   end interface
!-------------------------------------------------------------------------------
! extern void loadobj(int n, char *file);
   interface
      subroutine loadobj_F(N,FILE) bind(C,NAME='draw_loadobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         character(KIND=C_CHAR),intent(in) :: FILE(*)
      end subroutine loadobj_F
   end interface
!-------------------------------------------------------------------------------
! extern void saveobj(int n, char *file);
   interface
      subroutine saveobj_F(N,FILE) bind(C,NAME='draw_saveobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         character(KIND=C_CHAR),intent(in) :: FILE(*)
      end subroutine saveobj_F
   end interface
!-------------------------------------------------------------------------------
! extern void callobj(int n);
   interface
      subroutine callobj(N) bind(C,NAME='draw_callobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine callobj
   end interface
!-------------------------------------------------------------------------------
! extern void closeobj(void);
   interface
      subroutine closeobj() bind(C,NAME='draw_closeobj')
         use ISO_C_BINDING
         implicit none
      end subroutine closeobj
   end interface
!-------------------------------------------------------------------------------
! extern int    getopenobj(void);
   interface
      function getopenobj() bind(C,NAME='draw_getopenobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getopenobj
      end function getopenobj
   end interface
!-------------------------------------------------------------------------------
! ==========  patch routines.
!-------------------------------------------------------------------------------
! extern void patch(Matrix geomx, Matrix geomy, Matrix geomz);
   interface
      subroutine patch_F(GEOMX,GEOMY,GEOMZ) bind(C,NAME='draw_patch')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         type(MATRIX),intent(inout) :: GEOMX
         type(MATRIX),intent(inout) :: GEOMY
         type(MATRIX),intent(inout) :: GEOMZ
      end subroutine patch_F
   end interface
!-------------------------------------------------------------------------------
! extern void rpatch(Matrix geomx, Matrix geomy, Matrix geomz, Matrix geomw);
   interface
      subroutine rpatch_F(GEOMX,GEOMY,GEOMZ) bind(C,NAME='draw_rpatch')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         type(MATRIX),intent(inout) :: GEOMX
         type(MATRIX),intent(inout) :: GEOMY
         type(MATRIX),intent(inout) :: GEOMZ
      end subroutine rpatch_F
   end interface
!-------------------------------------------------------------------------------
! extern void drpatch(Tensor R, int ntcurves, int nucurves, int ntsegs, int nusegs, int ntiter, int nuiter);
!-------------------------------------------------------------------------------
! extern void patchbasis(Matrix tb, Matrix ub) ;
   interface
      subroutine patchbasis_F(TB,UB) bind(C,NAME='draw_patchbasis')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         type(MATRIX),intent(inout) :: TB
         type(MATRIX),intent(inout) :: UB
      end subroutine patchbasis_F
   end interface
!-------------------------------------------------------------------------------
! extern void patchcurves(int nt, int nu);
   interface
      subroutine patchcurves(NT,NU) bind(C,NAME='draw_patchcurves')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: NT
         integer(KIND=C_INT),intent(in),value :: NU
      end subroutine patchcurves
   end interface
!-------------------------------------------------------------------------------
! extern void patchprecision(int tseg, int useg);
   interface
      subroutine patchprecision(TSEG,USEG) bind(C,NAME='draw_patchprecision')
         use ISO_C_BINDING
         implicit none
         INTEGER(KIND=C_INT),intent(in),value :: TSEG
         INTEGER(KIND=C_INT),intent(in),value :: USEG
      end subroutine patchprecision
   end interface
!-------------------------------------------------------------------------------
! extern void transformtensor(Tensor S, Matrix m);
!-------------------------------------------------------------------------------
! ==========  point routines
!-------------------------------------------------------------------------------
! extern void point(float x, float y, float z);
   interface
      subroutine point(X,Y,Z) bind(C,NAME='draw_point')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine point
   end interface
!-------------------------------------------------------------------------------
! extern void point2(float x, float y);
   interface
      subroutine point2(X,Y) bind(C,NAME='draw_point2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine point2
   end interface
!-------------------------------------------------------------------------------
! extern void spoint2(float xs, float ys);
   interface
      subroutine spoint2(X,Y) bind(C,NAME='draw_spoint2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine spoint2
   end interface
!-------------------------------------------------------------------------------
! ==========  polygon routines.
!-------------------------------------------------------------------------------
! extern void poly(int n, float dp[][3]);
   interface
      subroutine poly(N,DP) bind(C,NAME='draw_poly')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         real(KIND=C_FLOAT),intent(in),dimension(3,*) :: DP
      end subroutine poly
   end interface
!-------------------------------------------------------------------------------
! extern void poly2(int n, float dp[][2]);
   interface
      subroutine poly2(N,DP) bind(C,NAME='draw_poly2')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         real(KIND=C_FLOAT),intent(in),dimension(2,*) :: DP
      end subroutine poly2
   end interface
!-------------------------------------------------------------------------------
! extern void hatchang(float a);
   interface
      subroutine hatchang(A) bind(C,NAME='draw_hatchang')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: A
      end subroutine hatchang
   end interface
!-------------------------------------------------------------------------------
! extern void makepoly(void);
   interface
      subroutine makepoly() bind(C,NAME='draw_makepoly')
         use ISO_C_BINDING
         implicit none
      end subroutine makepoly
   end interface
!-------------------------------------------------------------------------------
! extern void polyfill(int onoff);
   interface
      subroutine polyfill_F(ONOFF) bind(C,NAME='draw_polyfill')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine polyfill_F
   end interface
!-------------------------------------------------------------------------------
! extern void closepoly(void);
   interface
      subroutine closepoly() bind(C,NAME='draw_closepoly')
         use ISO_C_BINDING
         implicit none
      end subroutine closepoly
   end interface
!-------------------------------------------------------------------------------
! extern void polyhatch(int onoff);
   interface
      subroutine polyhatch_F(ONOFF) bind(C,NAME='draw_polyhatch')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine polyhatch_F
   end interface
!-------------------------------------------------------------------------------
! extern void hatchpitch(float a);
   interface
      subroutine hatchpitch(A) bind(C,NAME='draw_hatchpitch')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: A
      end subroutine hatchpitch
   end interface
!-------------------------------------------------------------------------------
! extern void backfacedir(int cdir);
   interface
      subroutine backfacedir_F(cdir) bind(C,NAME='draw_backfacedir')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: cdir
      end subroutine backfacedir_F
   end interface
!-------------------------------------------------------------------------------
! extern void backface(int onoff);
   interface
      subroutine backface_F(ONOFF) bind(C,NAME='draw_backface')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine backface_F
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void pmove(float x, float y, float z);
   interface
      subroutine pmove(X,Y,Z) bind(C,NAME='draw_pmove')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine pmove
   end interface
!-------------------------------------------------------------------------------
! extern void pdraw(float x, float y, float z);
   interface
      subroutine pdraw(X,Y,Z) bind(C,NAME='draw_pdraw')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine pdraw
   end interface
!-------------------------------------------------------------------------------
! ==========  rectangle routine
!-------------------------------------------------------------------------------
! extern void rect(float x1, float y1, float x2, float y2);
   interface
      subroutine rect(X,Y,X2,Y2) bind(C,NAME='draw_rect')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: X2
         real(KIND=C_FLOAT),intent(in),value :: Y2
      end subroutine rect
   end interface
!-------------------------------------------------------------------------------
! extern void srect(float x1, float y1, float x2, float y2);
   interface
      subroutine srect(X,Y,X2,Y2) bind(C,NAME='draw_srect')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: X2
         real(KIND=C_FLOAT),intent(in),value :: Y2
      end subroutine srect
   end interface
!-------------------------------------------------------------------------------
! ==========  text routines
!-------------------------------------------------------------------------------
! extern float strlength(char *s);
   interface
      function strlength_F(S) bind(C,NAME='draw_strlength')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: strlength_F
         character(KIND=C_CHAR),intent(in) :: S(*)
      end function strlength_F
   end interface
!-------------------------------------------------------------------------------
! extern void boxtext(float x, float y, float l, float h, char *s);
   interface
      subroutine boxtext_F(X,Y,L,H,S) bind(C,NAME='draw_boxtext')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: L
         real(KIND=C_FLOAT),intent(in),value :: H
         character(KIND=C_CHAR,len=1),intent(in) :: S(*)
      end subroutine boxtext_F
   end interface
!-------------------------------------------------------------------------------
! extern void boxfit(float l, float h, int nchars);
   interface
      subroutine boxfit(L,H,NCHARS) bind(C,NAME='draw_boxfit')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: L
         real(KIND=C_FLOAT),intent(in),value :: H
         integer(KIND=C_INT),intent(in),value   :: NCHARS
      end subroutine boxfit
   end interface
!-------------------------------------------------------------------------------
! extern int numchars(void);
   interface
      function numchars() bind(C,NAME='draw_numchars')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: numchars
      end function numchars
   end interface
!-------------------------------------------------------------------------------
! extern void getcharsize(char c, float *width, float *height);
   interface
      subroutine getcharsize(C,WIDTH,HEIGHT) bind(C,NAME='draw_getcharsize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: WIDTH
         real(KIND=C_FLOAT),intent(out) :: HEIGHT
         character(KIND=C_CHAR),intent(in),value :: C
      end subroutine getcharsize
   end interface
!-------------------------------------------------------------------------------
! extern void drawchar(int c);
!  there are currently issues in some ISO_C_BINDING implementations about a
!  single character being equivalent to an integer in C and pass by value
!  and such; might have to replace drawchar with a call to drawstr
   interface
      subroutine drawchar_F(C) bind(C,NAME='draw_drawchar')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in),value :: C
      end subroutine drawchar_F
   end interface
!-------------------------------------------------------------------------------
! extern void textsize(float width, float height);
   interface
      subroutine textsize(WIDTH,HEIGHT) bind(C,NAME='draw_textsize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: WIDTH
         real(KIND=C_FLOAT),intent(in),value :: HEIGHT
      end subroutine textsize
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontwidth(void);
   interface
      function getfontwidth() bind(C,NAME='draw_getfontwidth')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontwidth
      end function getfontwidth
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontheight(void);
   interface
      function getfontheight() bind(C,NAME='draw_getfontheight')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontheight
      end function getfontheight
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontdec(void);
   interface
      function getfontdec() bind(C,NAME='draw_getfontdec')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontdec
      end function getfontdec
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontasc(void);
   interface
      function getfontasc() bind(C,NAME='draw_getfontasc')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontasc
      end function getfontasc
   end interface
!-------------------------------------------------------------------------------
! extern void getfontsize(float *cw, float *ch);
   interface
      subroutine getfontsize(CW,CH) bind(C,NAME='draw_getfontsize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: CW
         real(KIND=C_FLOAT),intent(out) :: CH
      end subroutine getfontsize
   end interface
!-------------------------------------------------------------------------------
! extern void drawstr(char *string);
   interface
      subroutine drawstr_F(STRING) bind(C,NAME='draw_drawstr')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: STRING(*)
      end subroutine drawstr_F
   end interface
!-------------------------------------------------------------------------------
! extern void centertext(int onoff);
   interface
      subroutine centertext_F(ONOFF) bind(C,NAME='draw_centertext')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine centertext_F
   end interface
!-------------------------------------------------------------------------------
! extern void fixedwidth(int onoff);
   interface
      subroutine fixedwidth_F(ONOFF) bind(C,NAME='draw_fixedwidth')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine fixedwidth_F
   end interface
!-------------------------------------------------------------------------------
! extern void textang(float ang);
   interface
      subroutine textang(ANG) bind(C,NAME='draw_textang')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: ANG
      end subroutine textang
   end interface
!-------------------------------------------------------------------------------
! extern void font(char *name);
   interface
      subroutine font_F(NAME) bind(C,NAME='draw_font')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: NAME(*)
      end subroutine font_F
   end interface
!-------------------------------------------------------------------------------
! extern int    getstring(int bcol, char *s);
   interface
      function getstring(BCOL,S) bind(C,NAME='draw_getstring')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT):: GETSTRING
         integer(KIND=C_INT),intent(in),value :: BCOL
         character(KIND=C_CHAR) :: S(*)
      end function getstring
   end interface
!-------------------------------------------------------------------------------
! ==========  transformation routines
!-------------------------------------------------------------------------------
! extern void scale(float x, float y, float z);
   interface
      subroutine scale(X,Y,Z) bind(C,NAME='draw_scale')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine scale
   end interface
!-------------------------------------------------------------------------------
! extern void translate(float x, float y, float z);
   interface
      subroutine translate(X,Y,Z) bind(C,NAME='draw_translate')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine translate
   end interface
!-------------------------------------------------------------------------------
! extern void rotate(float r,char axis);
   interface
      subroutine rotate(R,AXIS) bind(C,NAME='draw_rotate')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: R
         character(KIND=C_CHAR),intent(in),value :: AXIS
      end subroutine rotate
   end interface
!-------------------------------------------------------------------------------
! ==========  window definition routines
!-------------------------------------------------------------------------------
! extern void ortho(float left,float right,float bottom,float top,float hither,float yon);
   interface
      subroutine ortho(LEFT,RIGHT,BOTTOM,TOP,HITHER,YON) bind(C,NAME='draw_ortho')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: LEFT
         real(KIND=C_FLOAT),intent(in),value :: RIGHT
         real(KIND=C_FLOAT),intent(in),value :: BOTTOM
         real(KIND=C_FLOAT),intent(in),value :: TOP
         real(KIND=C_FLOAT),intent(in),value :: HITHER
         real(KIND=C_FLOAT),intent(in),value :: YON
      end subroutine ortho
   end interface
!-------------------------------------------------------------------------------
! extern void ortho2(float left,float  right,float  bottom,float  top);
   interface
      subroutine ortho2(LEFT,RIGHT,BOTTOM,TOP) bind(C,NAME='draw_ortho2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: LEFT
         real(KIND=C_FLOAT),intent(in),value :: RIGHT
         real(KIND=C_FLOAT),intent(in),value :: BOTTOM
         real(KIND=C_FLOAT),intent(in),value :: TOP
      end subroutine ortho2
   end interface
!-------------------------------------------------------------------------------
! extern void lookat(float vx,float vy,float vz,float px,float py,float pz,float twist);
   interface
      subroutine lookat(VX,VY,VZ,PX,PY,PZ,TWIST) bind(C,NAME='draw_lookat')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: VX
         real(KIND=C_FLOAT),intent(in),value :: VY
         real(KIND=C_FLOAT),intent(in),value :: VZ
         real(KIND=C_FLOAT),intent(in),value :: PX
         real(KIND=C_FLOAT),intent(in),value :: PY
         real(KIND=C_FLOAT),intent(in),value :: PZ
         real(KIND=C_FLOAT),intent(in),value :: TWIST
      end subroutine lookat
   end interface
!-------------------------------------------------------------------------------
! extern void window(float left,float right,float bottom,float top,float hither,float yon);
   interface
      subroutine window(LEFT,RIGHT,BOTTOM,TOP,HITHER,YON) bind(C,NAME='draw_window')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: LEFT
         real(KIND=C_FLOAT),intent(in),value :: RIGHT
         real(KIND=C_FLOAT),intent(in),value :: BOTTOM
         real(KIND=C_FLOAT),intent(in),value :: TOP
         real(KIND=C_FLOAT),intent(in),value :: HITHER
         real(KIND=C_FLOAT),intent(in),value :: YON
      end subroutine window
   end interface
!-------------------------------------------------------------------------------
! extern void polarview(float dist, float azim, float inc, float twist) ;
   interface
      subroutine polarview(DIST,AZIM,INC,TWIST) bind(C,NAME='draw_polarview')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: DIST
         real(KIND=C_FLOAT),intent(in),value :: AZIM
         real(KIND=C_FLOAT),intent(in),value :: INC
         real(KIND=C_FLOAT),intent(in),value :: TWIST
      end subroutine polarview
   end interface
!-------------------------------------------------------------------------------
! extern void perspective(float fov, float aspect, float hither, float yon) ;
   interface
      subroutine perspective(FOV,ASPECT,HITHER,YON) bind(C,NAME='draw_perspective')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: FOV
         real(KIND=C_FLOAT),intent(in),value :: ASPECT
         real(KIND=C_FLOAT),intent(in),value :: HITHER
         real(KIND=C_FLOAT),intent(in),value :: YON
      end subroutine perspective
   end interface
!-------------------------------------------------------------------------------
! extern void up(float x, float y, float z) ;
   interface
      subroutine up(X,Y,Z) bind(C,NAME='draw_up')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine up
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for manipulating the viewport
!-------------------------------------------------------------------------------
! extern void getviewport(float *xlow, float *xhigh, float *ylow, float *yhigh);
   interface
      subroutine getviewport(XLOW,XHIGH,YLOW,YHIGH) bind(C,NAME='draw_getviewport')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: XLOW
         real(KIND=C_FLOAT),intent(out) :: XHIGH
         real(KIND=C_FLOAT),intent(out) :: YLOW
         real(KIND=C_FLOAT),intent(out) :: YHIGH
      end subroutine getviewport
   end interface
!-------------------------------------------------------------------------------
! extern void viewport(float xlow, float xhigh, float ylow, float yhigh);
   interface
      subroutine viewport(XLOW,XHIGH,YLOW,YHIGH) bind(C,NAME='draw_viewport')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: XLOW
         real(KIND=C_FLOAT),intent(in),value :: XHIGH
         real(KIND=C_FLOAT),intent(in),value :: YLOW
         real(KIND=C_FLOAT),intent(in),value :: YHIGH
      end subroutine viewport
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void popviewport(void);
   interface
      subroutine popviewport() bind(C,NAME='draw_popviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine popviewport
   end interface
!-------------------------------------------------------------------------------
! extern void pushviewport(void);
   interface
      subroutine pushviewport() bind(C,NAME='draw_pushviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine pushviewport
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for retrieving the graphics position
!-------------------------------------------------------------------------------
! extern void getgp(float *x,float *y,float *z);
   interface
      subroutine getgp(X,Y,Z) bind(C,NAME='draw_getgp')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out):: X
         real(KIND=C_FLOAT),intent(out) :: Y
         real(KIND=C_FLOAT),intent(out) :: Z
      end subroutine getgp
   end interface
!-------------------------------------------------------------------------------
! extern void getgpt(float *x,float *y,float *z, float *w);
   interface
      subroutine getgpt(X,Y,Z,W) bind(C,NAME='draw_getgpt')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
         real(KIND=C_FLOAT),intent(out) :: Z
         real(KIND=C_FLOAT),intent(out) :: W
      end subroutine getgpt
   end interface
!-------------------------------------------------------------------------------
! extern void getgp2(float *x,float *y);
   interface
      subroutine getgp2(X,Y) bind(C,NAME='draw_getgp2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine getgp2
   end interface
!-------------------------------------------------------------------------------
! extern void sgetgp2(float *xs,float *ys);
   interface
      subroutine sgetgp2(X,Y) bind(C,NAME='draw_sgetgp2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine sgetgp2
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for retrieving the aspect details of the device
!-------------------------------------------------------------------------------
! extern float    getaspect(void);
   interface
      function getaspect() bind(C,NAME='draw_getaspect')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getaspect
      end function getaspect
   end interface
!-------------------------------------------------------------------------------
! extern void getfactors(float *x,float *y);
   interface
      subroutine getfactors(X,Y) bind(C,NAME='draw_getfactors')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine getfactors
   end interface
!-------------------------------------------------------------------------------
! extern void getdisplaysize(float *x,float *y);
   interface
      subroutine getdisplaysize(X,Y) bind(C,NAME='draw_getdisplaysize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine getdisplaysize
   end interface
!-------------------------------------------------------------------------------
! extern void expandviewport(void);
   interface
      subroutine expandviewport() bind(C,NAME='draw_expandviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine expandviewport
   end interface
!-------------------------------------------------------------------------------
! extern void unexpandviewport(void);
   interface
      subroutine unexpandviewport() bind(C,NAME='draw_unexpandviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine unexpandviewport
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for handling the buffering
!-------------------------------------------------------------------------------
! extern int      backbuffer(void);
   interface
      function backbuffer() bind(C,NAME='draw_backbuffer')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: backbuffer
      end function backbuffer
   end interface
!-------------------------------------------------------------------------------
! extern void frontbuffer(void);
   interface
      subroutine frontbuffer() bind(C,NAME='draw_frontbuffer')
         use ISO_C_BINDING
         implicit none
      end subroutine frontbuffer
   end interface
!-------------------------------------------------------------------------------
! extern int      swapbuffers(void);
   interface
      subroutine swapbuffers() bind(C,NAME='draw_swapbuffers')
         use ISO_C_BINDING
         implicit none
      end subroutine swapbuffers
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for window sizing and positioning
!-------------------------------------------------------------------------------
! void prefposition(int x, int y);
   interface
      subroutine prefposition(X,Y) bind(C,NAME='draw_prefposition')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: X
         integer(KIND=C_INT),intent(in),value :: Y
      end subroutine prefposition
   end interface
!-------------------------------------------------------------------------------
! void prefsize(int x, int y);
   interface
      subroutine prefsize(X,Y) bind(C,NAME='draw_prefsize')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: X
         integer(KIND=C_INT),intent(in),value :: Y
      end subroutine prefsize
   end interface
!-------------------------------------------------------------------------------
! void getprefposandsize(int *x, int *y, int *xs, int *ys);
   interface
      subroutine getprefposandsize(X,Y,XS,YS) bind(C,NAME='draw_getprefposandsize')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(out) :: X
         integer(KIND=C_INT),intent(out) :: Y
         integer(KIND=C_INT),intent(out) :: XS
         integer(KIND=C_INT),intent(out) :: YS
      end subroutine getprefposandsize
   end interface
!-------------------------------------------------------------------------------
! ==========  Misc control routines
!-------------------------------------------------------------------------------
! extern void clipping(int onoff);
   interface
      subroutine clipping_F(ONOFF) bind(C,NAME='draw_clipping')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine clipping_F
   end interface
!-------------------------------------------------------------------------------
! extern void vsetflush(int yn);
   interface
      subroutine vsetflush_F(YN) bind(C,NAME='draw_vsetflush')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: YN
      end subroutine vsetflush_F
   end interface
!-------------------------------------------------------------------------------
! extern void vflush(void);
   interface
      subroutine vflush() bind(C,NAME='draw_vflush')
         use ISO_C_BINDING
         implicit none
      end subroutine vflush
   end interface
!-------------------------------------------------------------------------------
! extern void clip(Vector p0, Vector p1);
!-------------------------------------------------------------------------------
! extern void quickclip(Vector p0, Vector p1);
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void yobbarays(int onoff);
   interface
      subroutine yobbarays(ONOFF) bind(C,NAME='draw_yobbarays')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine yobbarays
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void xcentertext(void);
   interface
      subroutine xcentertext() bind(C,NAME='draw_xcentertext')
         use ISO_C_BINDING
         implicit none
      end subroutine xcentertext
   end interface
!-------------------------------------------------------------------------------
! extern void ycentertext(void);
   interface
      subroutine ycentertext() bind(C,NAME='draw_ycentertext')
         use ISO_C_BINDING
         implicit none
      end subroutine ycentertext
   end interface
!-------------------------------------------------------------------------------
! extern void topjustify(void);
   interface
      subroutine topjustify() bind(C,NAME='draw_topjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine topjustify
   end interface
!-------------------------------------------------------------------------------
! extern void bottomjustify(void);
   interface
      subroutine bottomjustify() bind(C,NAME='draw_bottomjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine bottomjustify
   end interface
!-------------------------------------------------------------------------------
! extern void leftjustify(void);
   interface
      subroutine leftjustify() bind(C,NAME='draw_leftjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine leftjustify
   end interface
!-------------------------------------------------------------------------------
! extern void rightjustify(void);
   interface
      subroutine rightjustify() bind(C,NAME='draw_rightjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine rightjustify
   end interface
!-------------------------------------------------------------------------------
! extern void textjustify(unsigned val);
! extern void textjustify(char val);
   interface
      subroutine textjustify(VAL) bind(C,NAME='draw_textjustify')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in),value :: VAL
      end subroutine textjustify
   end interface
!-------------------------------------------------------------------------------
! extern void textslant(float val);
   interface
      subroutine textslant(VAL) bind(C,NAME='draw_textslant')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: VAL
      end subroutine textslant
   end interface
!-------------------------------------------------------------------------------
! extern void textweight(int val);
   interface
      subroutine textweight(VAL) bind(C,NAME='draw_textweight')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: VAL
      end subroutine textweight
   end interface
!-------------------------------------------------------------------------------
! ==========  matrix stack routines
!-------------------------------------------------------------------------------
! extern void popmatrix(void);
   interface
      subroutine popmatrix() bind(C,NAME='draw_popmatrix')
         use ISO_C_BINDING
         implicit none
      end subroutine popmatrix
   end interface
!-------------------------------------------------------------------------------
! extern void pushmatrix(void);
   interface
      subroutine pushmatrix() bind(C,NAME='draw_pushmatrix')
         use ISO_C_BINDING
         implicit none
      end subroutine pushmatrix
   end interface
!-------------------------------------------------------------------------------
! ==========  matrix stack routines
!-------------------------------------------------------------------------------
! extern void getmatrix_F(Matrix m);
   interface
      subroutine getmatrix_F(M) bind(C,NAME='draw_getmatrix')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(out),dimension(4,4) :: M
         type(MATRIX),intent(out) :: M
      end subroutine getmatrix_F
   end interface
!-------------------------------------------------------------------------------
! extern void loadmatrix_F(Matrix mat);
   interface
      subroutine loadmatrix_F(M) bind(C,NAME='draw_loadmatrix')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(in),dimension(4,4) :: M
         type(MATRIX),intent(in) :: M
      end subroutine loadmatrix_F
   end interface
!-------------------------------------------------------------------------------
! extern void multmatrix_F(Matrix mat);
   interface
      subroutine multmatrix_F(M) bind(C,NAME='draw_multmatrix')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: M
         type(MATRIX),intent(inout) :: M
      end subroutine multmatrix_F
   end interface
!-------------------------------------------------------------------------------
! extern void printmat(char *s, Matrix m);
!-------------------------------------------------------------------------------
! extern void printvect(char *s, Vector v);
!-------------------------------------------------------------------------------
! ==========  object routines
!-------------------------------------------------------------------------------
! extern Token  *newtokens(int num);
!-------------------------------------------------------------------------------
! extern void polyobj(int n, Token dp[]);
!-------------------------------------------------------------------------------
! ==========  tensor routines
!-------------------------------------------------------------------------------
! extern void premulttensor(Tensor c, Matrix a,Tensor  b) ;
!-------------------------------------------------------------------------------
! extern void multtensor(Tensor c, Matrix a,Tensor  b) ;
!-------------------------------------------------------------------------------
! extern void copytensor(Tensor b, Tensor a) ;
!-------------------------------------------------------------------------------
! extern void copytensortrans(Tensor b,Tensor  a) ;
!-------------------------------------------------------------------------------
! ==========  mapping routines
!-------------------------------------------------------------------------------
! extern int WtoVx( float p[]);
!-------------------------------------------------------------------------------
! extern int WtoVy( float p[]);
!-------------------------------------------------------------------------------
! extern void VtoWxy( float xs, float ys, float  *xw, float *yw);
   interface
      subroutine VtoWxy(XS,YS,XW,YW) bind(C,NAME='draw_VtoWxy')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: XS
         real(KIND=C_FLOAT),intent(in),value :: YS
         real(KIND=C_FLOAT),intent(out)      :: XW
         real(KIND=C_FLOAT),intent(out)      :: YW
      end subroutine VtoWxy
   end interface
!-------------------------------------------------------------------------------
! extern void CalcW2Vcoeffs(void);
   interface
      subroutine CalcW2Vcoeffs() bind(C,NAME='draw_CalcW2Vcoeffs')
         use ISO_C_BINDING
         implicit none
      end subroutine CalcW2Vcoeffs
   end interface
!-------------------------------------------------------------------------------
! ==========  general matrix and vector routines
!-------------------------------------------------------------------------------
! extern void mult4x4(register Matrix a, register Matrix b, register Matrix c);
!-------------------------------------------------------------------------------
! extern void copymatrix(register Matrix a, register Matrix b);
!-------------------------------------------------------------------------------
! extern void identmatrix(Matrix a);
!-------------------------------------------------------------------------------
! extern void copyvector(register Vector a, register Vector b);
!-------------------------------------------------------------------------------
! extern void premultvector(Vector v, Vector a, Matrix b);
!-------------------------------------------------------------------------------
! extern void copytranspose(register Matrix a, register Matrix b);
!-------------------------------------------------------------------------------
! extern void multvector(Vector v, Vector a, Matrix b);
!-------------------------------------------------------------------------------
! ==========  other internal routines
!-------------------------------------------------------------------------------
! extern void verror(char *str);
   interface
      subroutine verror_F(STR) bind(C,NAME='draw_verror')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: STR(*)
      end subroutine verror_F
   end interface
!-------------------------------------------------------------------------------
! extern int    hershfont(char *fontname);
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!-------------------------------------------------------------------------------
!!call polyfill(.false.)
 subroutine polyfill(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF.eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call polyfill_F(IONOFF)
end subroutine polyfill
!-------------------------------------------------------------------------------
!!call backface(.false.)
 subroutine backface(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call backface_F(IONOFF)
end subroutine backface
!-------------------------------------------------------------------------------
!!call backfacedir(.false.)
 subroutine backfacedir(CDIR)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)   ::  CDIR
   integer(KIND=C_INT)  ::  ICDIR
   if(CDIR .eqv. .true.)then
      ICDIR=1 ! CLOCKWISE (IN SCREEN COORDS)
   else
      ICDIR=0 ! ANTICLOCKWISE
   endif
   call backfacedir_F(ICDIR)
end subroutine backfacedir
!-------------------------------------------------------------------------------
!!call centertext(.false.)
 subroutine centertext(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call centertext_F(IONOFF)
end subroutine centertext
!-------------------------------------------------------------------------------
!!call clipping(.false.)
 subroutine clipping(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call clipping_F(IONOFF)
end subroutine clipping
!-------------------------------------------------------------------------------
!!call fixedwidth(.false.)
 subroutine fixedwidth(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call fixedwidth_F(IONOFF)
end subroutine fixedwidth
!-------------------------------------------------------------------------------
!!call polyhatch(.false.)
 subroutine polyhatch(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call polyhatch_F(IONOFF)
end subroutine polyhatch
!-------------------------------------------------------------------------------
!!vsetflush(.false.)
 subroutine vsetflush(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  ::  ONOFF
   integer(KIND=C_INT) ::  IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call vsetflush_F(IONOFF)
end subroutine vsetflush
!-------------------------------------------------------------------------------
 function isobj(N)
   use ISO_C_BINDING
   implicit none
   logical :: isobj
   integer(KIND=C_INT),intent(in) :: N
   if(isobj_F(N) == 0 )then
      isobj=.false.
   else
      isobj=.true.
   endif
end function isobj
!-------------------------------------------------------------------------------
 subroutine font(NAME)
   implicit none
   character(len=*),intent(in) ::  NAME
   call font_F(s2c(NAME))
 end subroutine font
!-------------------------------------------------------------------------------
 subroutine linestyle(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call linestyle_F(s2c(NAME))
 end subroutine linestyle
!-------------------------------------------------------------------------------
 subroutine printvdevice(S)
   implicit none
   character(len=*),intent(in) :: S
   call printvdevice_F(s2c(S))
 end subroutine printvdevice
!-------------------------------------------------------------------------------
 subroutine printattribs(S)
   implicit none
   character(len=*),intent(in) :: S
   call printattribs_F(s2c(S))
 end subroutine printattribs
!-------------------------------------------------------------------------------
 subroutine vinit(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call vinit_F(s2c(NAME))
 end subroutine vinit
!-------------------------------------------------------------------------------
 subroutine voutput(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call voutput_F(s2c(NAME))
 end subroutine voutput
!-------------------------------------------------------------------------------
 subroutine vnewdev(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call vnewdev_F(s2c(NAME))
 end subroutine vnewdev
!-------------------------------------------------------------------------------
 subroutine pushdev(NAME)
   implicit none
   character(len=*),intent(in) ::  NAME
   call pushdev_F(s2c(NAME))
 end subroutine pushdev
!-------------------------------------------------------------------------------
 subroutine vgetdev(NAME)
   use ISO_C_BINDING
   implicit none
   character(len=*)            :: NAME
   integer(KIND=C_INT)         :: i
   NAME=' '
   i=len(NAME)
   NAME(i:i)=char(0)
   call vgetdev_F(NAME)
   do i=1,len(NAME)  ! convert nulls to spaces in returned value
      if(NAME(i:i).eq.char(0))then
         NAME(i:i)=' '
      endif
   enddo
end subroutine vgetdev
!-------------------------------------------------------------------------------
 subroutine loadobj(N,FILE)
   use ISO_C_BINDING
   implicit none
   character(len=*),intent(in) :: FILE
   integer(KIND=C_INT)         :: N
   call loadobj_F(N,s2c(FILE))
 end subroutine loadobj
!-------------------------------------------------------------------------------
 subroutine saveobj(N,FILE)
   use ISO_C_BINDING
   implicit none
   character(len=*),intent(in)  ::  FILE
   integer(KIND=C_INT) :: N
   call saveobj_F(N,s2c(FILE))
 end subroutine saveobj
!-------------------------------------------------------------------------------
 function strlength(NAME)
   use ISO_C_BINDING
   implicit none
   character(len=*),intent(in) ::  NAME
   real(KIND=C_FLOAT) :: strlength
   strlength=strlength_F(s2c(NAME))
 end function strlength
!-------------------------------------------------------------------------------
 subroutine boxtext(X,Y,L,H,S)
   use ISO_C_BINDING
   implicit none
   real(KIND=C_FLOAT),intent(in) :: X
   real(KIND=C_FLOAT),intent(in) :: Y
   real(KIND=C_FLOAT),intent(in) :: L
   real(KIND=C_FLOAT),intent(in) :: H
   character(len=*),intent(in)   :: S
   call boxtext_F(X,Y,L,H,s2c(S))
end subroutine boxtext
!-------------------------------------------------------------------------------
 subroutine drawstr(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call drawstr_F(s2c(NAME))
 end subroutine drawstr
!-------------------------------------------------------------------------------
 subroutine drawchar(NAME)
! issues between int and character and character string
! make it easier to call drawstr()
! maybe when ISO_C_BINDING has been around longer this can be eliminated
! and change drawstr_F back to drawstr
   implicit none
   character(len=1),intent(in) :: NAME
   call drawstr_F(s2c(NAME))
 end subroutine drawchar
!-------------------------------------------------------------------------------
!subroutine verror(STR)
!   implicit none
!   character(len=*),intent(in) ::  STR
!   call verror_F(s2c(STR))
! end subroutine verror
!-------------------------------------------------------------------------------
! so fortran 77 does not have to have TYPE matrix
!-------------------------------------------------------------------------------
 subroutine rcurve(GEOM)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOM
    type(MATRIX) :: GEOM_matrix
    GEOM_matrix%array=geom
    call rcurve_F(geom_matrix)
 end subroutine rcurve
!-------------------------------------------------------------------------------
 subroutine curvebasis(BASIS)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: BASIS
    type(MATRIX) :: basis_matrix
    basis_matrix%array=basis
    call curvebasis_F(basis_matrix)
 end subroutine curvebasis
!-------------------------------------------------------------------------------
 subroutine patch(GEOMX,GEOMY,GEOMZ)
    use ISO_C_BINDING
    implicit none
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOMX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOMY
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOMZ
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    type(MATRIX) :: GEOMX_matrix
    type(MATRIX) :: GEOMY_matrix
    type(MATRIX) :: GEOMZ_matrix
    geomx_matrix%array=geomx
    geomy_matrix%array=geomy
    geomz_matrix%array=geomz
    call patch_F(geomx_matrix,geomy_matrix,geomz_matrix)
 end subroutine patch
!-------------------------------------------------------------------------------
  subroutine patchbasis(TB,UB)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: TB
    real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: UB
    type(MATRIX) :: TB_matrix
    type(MATRIX) :: UB_matrix
    tb_matrix%array=tb
    ub_matrix%array=ub
    call patchbasis_F(tb_matrix,ub_matrix)
 end subroutine patchbasis
!-------------------------------------------------------------------------------
 subroutine getmatrix(M)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(out),dimension(4,4) :: M
    type(MATRIX) :: M_matrix
    call getmatrix_F(m_matrix)
    M=M_matrix%array
 end subroutine getmatrix
!-------------------------------------------------------------------------------
  subroutine loadmatrix(M)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: M
    type(MATRIX) :: M_matrix
    M_matrix%array=M
    call loadmatrix_F(m_matrix)
 end subroutine loadmatrix
!-------------------------------------------------------------------------------
  subroutine multmatrix(M)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: M
    type(MATRIX) :: M_matrix
    M_matrix%array=M
    call multmatrix_F(m_matrix)
 end subroutine multmatrix
!-------------------------------------------------------------------------------
pure function s2c(string)  RESULT (array)
character(len=*),parameter      :: ident="@(#)s2c(3f):copy string(1:Clen(string)) to char array with null terminator"
character(len=*),intent(in)     :: string
   character(kind=C_CHAR,len=1) :: array(len_trim(string)+1)
   integer                      :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!-------------------------------------------------------------------------------
end module M_draw
!-------------------------------------------------------------------------------
! drawchar -- int versus char
! textjustify -- UNSIGNED?
! pdraw and pmove in sunfort? spoint2? srect? getfontwidth? getfontheight?  getprefposandsize?
! getplanes? drawchar?
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
