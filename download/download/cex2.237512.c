#include <stdio.h>
#include <math.h>
#include "vogle.h"

#define	PI	3.14159265358979
#define	N	300

float	x[N], y[N];

/*  	Another very simple test program for vopl.
 *
 * 	This one also draws a graph of y = sin(x) 0 <= x <= 2 * PI
 *	but shows some of the axis and range setting stuff
 */
main() {
	int	i;
	char	device[30];
	float	t, dt;

	printf("Enter VOGLE device: "); gets(device); /* Get VOGLE device */

/* Generate the points */
	t = 0.0;
	dt = 2 * PI / N;

	for (i = 0; i != N; i++) {
		x[i] = t;
		y[i] = sin(t);
		t = t + dt;
	}

	vp_range(0.0, 10.0, 'x');                  /* Set the X-scaling to be absolute 0 - 10 (ie no auto-scaling) */
	vp_adjustscale(y, N, 'y');                 /* Autoscale the Y-axis */
	vp_axistitle("This one's for you", 'x');   /* Anyone for some axis titles? */
	vp_axistitle("This one's for me", 'y');
/*
 *	As we are now about to do some graphics we initialise VOGLE and clear to BLACK
 */
	vogle_vinit(device);
        vogle_linewidth(30);
	vogle_color(0);
	vogle_clear();
	vogle_color(2);            /* Now set the color to GREEN */
	vp_drawaxes2();      /* Draw the default set of axes (in GREEN) */
	vogle_color(1);            /* Set color to RED */
	vp_plot2(x, y, N);   /* Draw the Graph */
	vogle_getkey();            /* Wait around a bit */
/*
 *	Now draw a little one in the top right hand corner
 *	by resetting the VOGLE viewport.
 */
	vogle_viewport(0.0, 1.0, 0.0, 1.0);
        /* Draw it again, but do the plot first (in BLUE) then the axes (in YELLOW) */
	vogle_color(4);
	vp_plot2(x, y, N);
	vogle_color(3);
	vp_drawaxes2();
	vogle_getkey();            /* Hang around again */
	vogle_color(0);            /* Clear it all away */
	vogle_clear();
	vogle_viewport(-1.0, 1.0, -0.5, 0.5); /* Reset the viewport to be a "long skinny one" */
/*
 *	Autoscale the X-axis again by first setting a ridicuous scale with
 *	vp_range that vp_adjustscale will change.
 */
	vp_range(1000.0, -1000.0, 'x');
	vp_adjustscale(x, N, 'x');
/*
 *	Change the X-axis title
 */
	vp_axistitle("Blark Bonk Bloot", 'x');
/*
 *	And draw it all again...
 */
	vogle_color(5);
	vp_drawaxes2();
	vogle_color(6);
	vp_plot2(x, y, N);
/*
 *	Hang around again
 */
	vogle_getkey();
/*
 *	Bugger off...
 */

	vogle_vexit();
}
