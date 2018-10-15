#include <stdlib.h>
#include <stdio.h>
#include <math.h>
/* -------------------------------------------------------------------------------------------------------------------------------*/
#include "vogle.h"

#define  BEERGLASS 1
static char ident[] = "@(#) generate a beer glass";
/* -------------------------------------------------------------------------------------------------------------------------------*/
#define  SIZE 1.5
#define HEAD 0.2
#define TOPRAD 0.4
#define BOTRAD 0.3
/* -------------------------------------------------------------------------------------------------------------------------------*/
void makebeer(){
   int   i, j;
   float x, y, z;
   float a;

   vogle_makeobj(BEERGLASS);

   vogle_color(WHITE);
   vogle_pushmatrix();
      vogle_rotate(90.0, 'x');
      vogle_translate(0.0, 0.0, -SIZE / 2.0);
      vogle_circle(0.0, 0.0, TOPRAD);
      vogle_translate(0.0, 0.0, -HEAD);
      vogle_circle(0.0, 0.0, TOPRAD);
   vogle_popmatrix();

   for (i = 0; i < 180; i += 30)
   {
      vogle_pushmatrix();
         vogle_rotate((float) i, 'y');
         vogle_color(YELLOW);
         vogle_move(TOPRAD, SIZE / 2.0, 0.0);
         vogle_draw(BOTRAD, -SIZE / 2.0, 0.0);
         vogle_draw(-BOTRAD, -SIZE / 2.0, 0.0);
         vogle_draw(-TOPRAD, SIZE / 2.0, 0.0);
         vogle_color(WHITE);
         vogle_draw(-TOPRAD, SIZE / 2.0 + HEAD, 0.0);
         vogle_draw(TOPRAD, SIZE / 2.0 + HEAD, 0.0);
         vogle_draw(TOPRAD, SIZE / 2.0, 0.0);
         vogle_draw(-TOPRAD, SIZE / 2.0, 0.0);
      vogle_popmatrix();
   }
   vogle_pushmatrix();
      vogle_color(YELLOW);
      vogle_rotate(90.0, 'x');
      vogle_translate(0.0, 0.0, SIZE / 2.0);
      for (i = 0; i < 6; i++)
      {
         vogle_circle(0.0, 0.0,
                BOTRAD + (TOPRAD - BOTRAD) * (float) i / 6.0);
         vogle_translate(0.0, 0.0, -SIZE / 6.0);
      }
   vogle_popmatrix();

   vogle_closeobj();
}
/* -------------------------------------------------------------------------------------------------------------------------------*/
int main( int argc, char *argv[] ){
   char  c, dev[20];
   int   i;
   float rotval = 0.0, drotval = 10.0, zeye = 5.0;
   float R = 1.6, tx = 0.0, tz = R;
   char  device[10], *p;

   vogle_prefsize(400, 400);

   fprintf(stderr,"Enter output device: ");
   fgets(device,20,stdin);

   vogle_vinit(device);          /* set up device */

   vogle_font("futura.l");

   makebeer();

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
      fprintf(stderr, "device '%s' can't support doublebuffering\n",
      vogle_vgetdev(dev));
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

         vogle_color(GREEN);
         vogle_move(0.0, 0.0, 0.0);
         vogle_boxtext(-0.9 * R, -0.1 * R, 1.8 * R, 0.2 * R, "Don't spill it!");
         
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
          vogle_callobj(BEERGLASS);
         vogle_popmatrix();

         tz = R * cos((double)(rotval * 3.1415926535 / 180));
         tx = R * sin((double)(rotval * 3.1415926535 / 180));

         vogle_swapbuffers();

         c = vogle_checkkey();

         if (c != 0) {
          vogle_vexit();
          exit(0);
         }
      }

   } while (1);
}
/* -------------------------------------------------------------------------------------------------------------------------------*/
