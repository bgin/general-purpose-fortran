#include "vogle.h"
#include <stdio.h>
#include <math.h>
extern void vpage (float xsmall, float xlarge, float ysmall,
			    float ylarge);
extern void jucolor (char *modei, float clr1i, float clr2i, float clr3i,
		     char *modeo, float *clr1o, float *clr2o, float *clr3o,
		     int *status);
#define TRUE 1
#define FALSE 0

/* WRONG - LOOK AT LATER
#define R2D(x) ((x)*180.0/acos(-1.0))
#define D2R(x) ((x)*acos(-1.0)/180.0)
*/

int MAXCOLORS = 64;
int SLICES = 120;
int TIMES = 10;

float top = 110.0 / 2.0;
float center = 0.0;
float ANG_INC;
float LIGHTNESS = 50;
float LIGHTSTEP = -5;
float ANGLE;
int CURRENT_COLOR;

/*#==================================================================*/
void slice () {
   /* draw a slice */
   char line[40];
   float angle1 = ANGLE - ANG_INC / 2;
   float angle2 = angle1 + ANG_INC;
   float saturation = 100;
   float radius1 = 32;
   float radius2;
   float step;
   float X1, X2, X3, X4;
   float Y1, Y2, Y3, Y4;
   float r, g, b;
   int ir, ig, ib;
   int status;
   int icount;
   extern float d2r ();
   extern float r2d ();
   vogle_color (7);

   vogle_move2 (radius1 * cos (d2r (ANGLE)), radius1 * sin (d2r (ANGLE)));
   vogle_draw2 ((radius1 + 4) * cos (d2r (ANGLE)),
		(radius1 + 4) * sin (d2r (ANGLE)));
   vogle_textang (ANGLE);
   vogle_move2 ((radius1 + 4) * cos (d2r (ANGLE)),
		(radius1 + 4) * sin (d2r (ANGLE)));
   sprintf (line, "%.1f", ANGLE);
   vogle_drawstr (line);
   step = radius1 / (TIMES + 1);
   radius2 = radius1 - step;
   /* draw a chunk in a slice */
   for (icount = TIMES + 1; icount; icount--) {
      CURRENT_COLOR = ((CURRENT_COLOR + 1) % MAXCOLORS);
      /* fancy mapcolor */
      jucolor ("hls", ANGLE, LIGHTNESS, saturation, "rgb", &r, &g, &b,
	       &status);
      ir = r * 255.0 / 100.0;
      ig = g * 255.0 / 100.0;
      ib = b * 255.0 / 100.0;

      vogle_mapcolor (CURRENT_COLOR + 8, ir, ig, ib);
      vogle_color (CURRENT_COLOR + 8);

      X1 = cos (d2r (angle1)) * radius2;
      Y1 = sin (d2r (angle1)) * radius2;
      X2 = cos (d2r (angle1)) * radius1;
      Y2 = sin (d2r (angle1)) * radius1;
      X3 = cos (d2r (angle2)) * radius2;
      Y3 = sin (d2r (angle2)) * radius2;
      X4 = cos (d2r (angle2)) * radius1;
      Y4 = sin (d2r (angle2)) * radius1;

      vogle_makepoly ();
      vogle_move2 (X1, Y1);
      vogle_draw2 (X2, Y2);
      vogle_draw2 (X4, Y4);
      vogle_draw2 (X3, Y3);
      vogle_draw2 (X1, Y1);
      vogle_closepoly ();

      saturation = saturation - 100.0 / TIMES;
      radius1 = radius2;
      radius2 = radius1 - step;

   }
   ANGLE = ANGLE + ANG_INC;
}

/*#==================================================================*/
void wheel () {
   /* draw an entire wheel */
   int ii;
   char line[40];
   ANGLE = 0;
   CURRENT_COLOR = 0;
   vogle_textang (0.0);
   vogle_color (1);
   vogle_textsize (5.0, 6.0);
   vogle_font ("times.r");
   vogle_fixedwidth (FALSE);
   vogle_move2 (0.0, 103.0 / 2.0);
   vogle_centertext (TRUE);
   vogle_drawstr ("COLOR WHEEL");
   vogle_textsize (2.5, 2.5);
   vogle_font ("futura.l");
   vogle_move2 (0.0, 90.0 / 2.0);
   sprintf (line, "LIGHTNESS=%f", LIGHTNESS);
   vogle_drawstr (line);
   vogle_fixedwidth (TRUE);
   vogle_centertext (FALSE);
   vogle_textsize (1.5, 1.5);
   for (ii = SLICES; ii > 0; ii--) {
      ;
      slice (ii);
   };
   vogle_getkey ();
   vogle_color (0);
   vogle_clear ();
   vogle_color (7);
   vogle_vflush ();
}

/*#==================================================================*/
int main (int argc, char *argv[]) {
   float d2r ();
   int ii;
   float col;
   ANG_INC = (360.0 / SLICES);
   /*prefsize(800,800); */
   /*vinit("X11"); */
   vogle_voutput ("hue.3.out");
   vogle_vinit ("");
   vogle_color (0);
   vogle_clear ();
   vogle_color (7);
   col = vogle_getdepth ();
   printf ("DEPTH=%f\n", col);
   MAXCOLORS = pow (2.0, col - 1);
   printf ("MAXCOLORS=%d\n", MAXCOLORS);
   vpage (-110. / 2., 85. / 2., -110. / 2., 110. / 2.);
   vogle_polyfill (TRUE);
   vogle_vflush ();
   vogle_vsetflush (FALSE);
   vogle_vsetflush (TRUE);
   for (ii = 1; ii < 2; ii++) {
      wheel ();
      LIGHTNESS = LIGHTNESS + LIGHTSTEP;
   }
}
/*#==================================================================*/
