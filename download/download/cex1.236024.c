#include <stdio.h>
#include <math.h>
#include "vogle.h"

#define PI	3.14159265358979
#define	N	300

float	x[N], y[N];

/*	A very simple test program for vopl.
 * 	This one draws a graph of y = sin(x) 0 <= x <= 2 * PI
 */
main() {
	float	t, dt;
	char	device[30];
	int	i;

/* Generate the points */
	t = 0.0;
	dt = 2 * PI / N;

	for (i = 0; i != N; i++) {
		x[i] = t;
		y[i] = sin(t);
		t = t + dt;
	}

/* Adjust the scaling according to x and y arrays */
	vp_adjustscale(x, N, 'x');
	vp_adjustscale(y, N, 'y');


	printf("Enter VOGLE device: "); /* Get VOGLE device */
	gets(device);
/* As we are now about to do some graphics we initialise VOGLE and clear to BLACK */
	vogle_vinit(device);
	vogle_color(0);
	vogle_clear();
	vogle_color(2);            /* Now set the color to GREEN */
        vogle_linewidth(30);
	vp_drawaxes2();      /* Draw the default set of axes (in GREEN) */
	vogle_color(1);            /* Set color to RED */
        vogle_linewidth(40);
	vp_plot2(x, y, N);   /* Draw the Graph */
	vogle_getkey();            /* Wait around a bit */
/*
 *	Now draw a little one in the top right hand corner
 *	by resetting the VOGLE viewport.
 */
	fprintf(stderr,"viewport 1\n");
	vogle_viewport(0.0, 1.0, 0.0, 1.0);
	fprintf(stderr,"viewport 2\n");
/*
 *	Draw it again, but do the plot first (in BLUE) then the axes
 *	(in YELLOW)
 */
	vogle_color(4);
        vogle_linewidth(50);
	vp_plot2(x, y, N);
	vogle_color(3);
        vogle_linewidth(10);
	vp_drawaxes2();
	vogle_getkey(); 	     /* Hang around again */
	vogle_vexit();             /* bugger off */
}
