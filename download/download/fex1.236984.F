c
c       A very simple test program for vopl.
c
c       This one draws a graph of y = sin(x) 0 <= x <= 2*pi
c
        program test1
      use M_vogle
        parameter (pi = 3.14159265358979)
        parameter (n = 300)

        real x(n), y(n)

        character device*30
c
c       Get VOGLE device
c
        write(*,'(a)',advance='no') 'Enter VOGLE device: '
        read(*,'(a)') device

c
c       Generate the points
c
        t = 0.0
        dt = 2 * pi / n

        do 10 i = 1, n
                x(i) = t
                y(i) = sin(t)
                t = t + dt
10      continue

c
c       Adjust the scaling according to x and y arrays
c
        call vp_adjustscale(x, n, 'x')
        call vp_adjustscale(y, n, 'y')
c
c       As we are now about to do some graphics we initialise VOGLE
c       and clear to BLACK
c
        call vinit(device)
        call color(0)
        call clear
c
c       Now set the color to GREEN
c
        call color(2)

c
c       Draw the default set of axes (in GREEN)
c
        call vp_drawaxes2
c
c       Set color to RED
c
        call color(1)
c
c       Draw the Graph
c
        call vp_plot2(x, y, n)
c
c       Wait around a bit
c
        idum=getkey()
c
c       Now draw a little one in the top right hand corner
c       by reseting the VOGLE viewport.
c
        call viewport(0.0, 1.0, 0.0, 1.0)
c
c       Draw it again, but do the plot first (in BLUE) then the axes
c       (in YELLOW)
c
        call color(4)
        call vp_plot2(x, y, n)
        call color(3)
        call vp_drawaxes2
c
c       Hang around again
c
        idum=getkey()
c
c       Bugger off...
c

        call vexit
        end
