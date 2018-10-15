#include <stdio.h>
#include <math.h>
#include "vogle.h"

#define	N	10

/*
 *	Another simple test program for vopl.
 *
 * 	This one tries to show the various "fit" options
 */
main()
{
	char		device[30];
	static float	x[N] = {
			1.0, 2.0, 3.0, 6.0,
			17.0, 19.0, 23.0, 45.0,
			50.0, 56.0
	};
	static float	y[N] = {
			1.0, 3.0, 5.0, 9.0,
			17.0, 45.0, 23.0, 99.0,
			50.0, 20.0
	};

/*
 *	Get VOGLE device
 */
	printf("Enter VOGLE device: ");
	gets(device);

/*
 *	First we'll do a linear least square fit.
 */
	vp_fit(2);
	vp_degree(1);
/*
 *	Adjust the scaling according to x and y arrays
 */
	vp_adjustscale(x, N, 'x');
	vp_adjustscale(y, N, 'y');
/*
 *	Give it a title
 */
	vp_graphtitle("Linear Least square fit");
/*
 *	As we are now about to do some graphics we initialise VOGLE
 *	and clear to BLACK
 */
	vogle_vinit(device);
	vogle_color(0);
	vogle_clear();
        vogle_linewidth(350);
/*
 *	Draw the title in CYAN
 */
	vogle_color(6);
	vp_drawtitle();
/*
 *	Now set the color to GREEN
 */
	vogle_color(2);

/*
 *	Draw the default set of axes (in GREEN)
 */
	vp_drawaxes2();
/*
 *	Set color to RED
 */
	vogle_color(1);
/*
 *	Change to the "markers" font and set the current vp_marker string
 */
	vogle_font("markers");
	vp_marker("a");
/*
 *	Draw the Graph
 */
	vp_plot2(x, y, N);
/*
 *	Wait around a bit
 */
	vogle_getkey();
/*
 *	Now we'll do a second order fit.
 */
	vp_degree(2);
	vp_graphtitle("Second order least square fit");

	vogle_color(0);
	vogle_clear();

	vogle_color(7);
	vp_plot2(x, y, N);
/*
 *	Change back to the "text" type font to draw the title and axes
 */
	vogle_font("futura.m");

	vogle_color(3);
	vp_drawaxes2();

	vogle_color(6);
	vp_drawtitle();
/*
 * 	Wait a bit
 */
	vogle_getkey();
/*
 *	Now do a Cubic spline fit (cardinal spline for this one)
 */
	vp_fit(3);
	
	vogle_color(0);
	vogle_clear();

	vogle_color(5);
	vp_drawaxes2();

	vp_graphtitle("Cardinal Cubic Spline Fit");
	vogle_color(6);
	vp_drawtitle();

/*
 *	Note, we haven't changed to the Marker font here
 */
	vp_plot2(x, y, N);

	vogle_getkey();

	vogle_vexit();
}
