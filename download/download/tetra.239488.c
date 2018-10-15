static char ident[] = "@(#) Demonstrate a rotating translating tetrahedron.";

#include <stdio.h>
#include <stdlib.h>
#ifdef TC
extern double sin(), cos();
#else
#include <math.h>
#endif
#include "vogle.h"

#define  TETRAHEDRON 1
#define  NSIDES 3
#define  NFACES 4
#define  NPNTS 4

float pnts[NPNTS][3] = {
   {-0.5, 0.866, -0.667},
   {-0.5, -0.866, -0.667},
   { 1.0, 0.0, -0.667},
   { 0.0, 0.0, 1.334}
};

int   faces[NFACES][NSIDES] = {
   {2, 1, 0},
   {0, 1, 3},
   {1, 2, 3},
   {2, 0, 3}
};

int   colface[NFACES] = {
      GREEN,
      YELLOW,
      CYAN,
      MAGENTA
};
/*
 * maketheobject
 *
 * generate a tetrahedron as a series of move draws
 */
void maketheobject()
{
   int   i, j;
   float x, y, z;

   vogle_makeobj(TETRAHEDRON);

      for (i = 0; i < NFACES; i++) {
         vogle_color(colface[i]);
         vogle_makepoly();
          x = pnts[faces[i][0]][0];
          y = pnts[faces[i][0]][1];
          z = pnts[faces[i][0]][2];
          vogle_move(x, y, z);
          for (j = 1; j < NSIDES; j++) {
           x = pnts[faces[i][j]][0];
           y = pnts[faces[i][j]][1];
           z = pnts[faces[i][j]][2];
           vogle_draw(x, y, z);
          }
         vogle_closepoly();
      }

   vogle_closeobj();
}

int main( int argc, char **argv ){
   char  c, dev[20];
   int   i;
   float rotval = 0.0, drotval = 5.0, zeye = 5.0;
   float R = 1.6, tx = 0.0, tz = R;
   int   do_backface;
   int   do_backfacedir;
   int   do_fill;
   char  device[10], *p;

   vogle_prefsize(400, 400);

   fprintf(stderr,"Enter output device: ");
   fgets(device,20,stdin);

   vogle_vinit(device);          /* set up device */

   vogle_font("small");

   do_fill = 1;
   do_backface = 1;
   do_backfacedir = 1;

   /*
    * Make the tetrahedral object
    */
   maketheobject();

   /*
    * If there is a command line arg to use filled polygons
    */
   vogle_polyfill(do_fill);
   vogle_backface(do_backface);
   vogle_backfacedir(do_backfacedir);

   /*
    * set up a perspective projection with a field of view of
    * 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
    * and the far clipping plane at 1000.0.
    */
   vogle_perspective(40.0, 1.0, 0.001, 15.0);
   vogle_lookat(0.0, 0.0, zeye, 0.0, 0.0, 0.0, 0.0);

   /*
    * Setup drawing into the backbuffer....
    */

   if (vogle_backbuffer() < 0) {
      vogle_vexit();
      fprintf(stderr, "device '%s' can't support doublebuffering\n",vogle_vgetdev(dev));
      exit(1);
   }

   do {
      for (rotval = 0.0; rotval < 360.0; rotval += drotval) {
         vogle_color(BLACK);
         vogle_clear();

         /*
          * Rotate the whole scene...(this accumulates - hence
          * drotval)
          */
         vogle_rotate(drotval * 0.1, 'x');
         vogle_rotate(drotval * 0.1, 'z');

         vogle_color(RED);
         vogle_pushmatrix();
          vogle_polyfill(0);
          vogle_rotate(90.0, 'x');
          vogle_circle(0.0, 0.0, R);
          vogle_polyfill(do_fill);
         vogle_popmatrix();

         vogle_color(BLUE);
         vogle_move(0.0, 0.0, 0.0);
         vogle_draw(tx, 0.0, tz);
         
         /*
          * Remember! The order of the transformations is
          * the reverse of what is specified here in between
          * the pushmatrix and the popmatrix. These ones don't
          * accumulate because of the push and pop.
          */
         vogle_pushmatrix();
          vogle_translate(tx, 0.0, tz);
          vogle_rotate(rotval, 'x');
          vogle_rotate(rotval, 'y');
          vogle_rotate(rotval, 'z');
          vogle_scale(0.4, 0.4, 0.4);
          vogle_callobj(TETRAHEDRON);
         vogle_popmatrix();

         tz = R * cos((double)(rotval * 3.1415926535 / 180));
         tx = R * sin((double)(rotval * 3.1415926535 / 180));

         vogle_swapbuffers();

         c = vogle_checkkey();

         if (c == 'b') {  
          do_backface = !do_backface;
          vogle_backface(do_backface);
         } else if (c == 'f') {
          do_fill = !do_fill;
          vogle_polyfill(do_fill);
         } else if (c == 'd') {
          do_backfacedir = !do_backfacedir;
          vogle_backfacedir(do_backfacedir);
         } else if (c != 0) {
          vogle_vexit();
          exit(0);
         }
      }

   } while (1);
}

