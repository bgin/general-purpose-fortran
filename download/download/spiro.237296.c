#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "vogle.h"

#define MAX(x,y)  ((x) > (y) ? (x) : (y))
#define MIN(x,y)  ((x) < (y) ? (x) : (y))
#define ABS(x)   ((x) < 0 ? -(x) : (x))
#define PI  3.14159265358979323844

/*============================================================================*/
int main(){
   char device[10];

   int wide=640, tall=640, rows, xoff, yoff, box_sz;

   long int i20, i30, ncols, nrows, ilines;

   float  bottom, left, sun_radius, planet_radius, planet_offset;

   extern void juhypc();
   extern void vogle_vexit();

   vogle_prefsize(wide,tall);

   fprintf(stderr,"Enter output device: ");
   gets(device);
   vogle_vinit(device);
   vogle_ortho2(0.0, (float)wide, 0.0, (float)tall);

   /*vogle_linewidth(3);*/ /* really slows down pbm driver because all lines are polygons */
   /*vogle_linewidth(1); */
   vogle_polyfill(1);
   vogle_vsetflush(0);
   vogle_color(BLACK);
   vogle_clear();
   vogle_color(WHITE);

   rows=5;
   box_sz=MIN(wide,tall)/rows;       /* size of biggest box to use and get specified number of rows */
   nrows = (int)tall/box_sz;         /* number of rows of objects to draw */
   ncols = (int)wide/box_sz;         /* number of columns of objects to draw */
   xoff = (wide - ncols * box_sz)/2; /* initial x offset to begin row at to center drawings */
   yoff = (tall - nrows * box_sz)/2; /* initial x offset to begin column at to center drawings */

   sun_radius = 148;
   planet_radius = 1;

   for ( ilines = 1; ilines <= 300; ilines++ ){
      for( i20 = 1; i20 <= ncols; i20++ ){
         left = (i20-1)*box_sz+xoff;
         for( i30 = 1; i30 <= nrows; i30++ ){
            bottom = (i30-1)*box_sz+yoff;
            vogle_color(BLACK);
            vogle_rect(left,bottom,left+box_sz,bottom+box_sz);
            vogle_color(WHITE);
            planet_offset= sun_radius;
            juhypc(left + box_sz/2.0,bottom + box_sz/2.0,sun_radius,planet_radius,planet_offset,box_sz/2.0,ilines);
            vogle_vflush();
            vogle_getkey();
         }
      }
      vogle_vflush();
      vogle_getkey();
   }
   vogle_vflush();
   vogle_getkey();
   vogle_vexit();
}
/*============================================================================*/
void juhypc(xcenter, ycenter, sun_radius, planet_radius, planet_offset, radius, ilines)
float xcenter, ycenter, sun_radius, planet_radius, planet_offset, radius;
long int ilines;
{
   long int i;
   float a, b, c, con1, con2, factor, rlines, u ;

   float   polyxy[5000][2];

   fprintf(stderr,"CENTER = %f %f\n",xcenter,ycenter);
   fprintf(stderr,"CONFIGURATION RADIUS = %f %f %f\n",sun_radius,planet_radius,planet_offset);
   fprintf(stderr,"FIT RADIUS = %f \n",radius);
   fprintf(stderr,"LINES = %ld\n",ilines);
   c = sun_radius;
   b = planet_offset;
   a = planet_radius;
   rlines = ilines;
   factor = radius/(c - a + b);
   c = factor*c;
   a = factor*a;
   b = factor*b;
   ilines = rlines;
   con1 = PI*2.*(c/a)/rlines;
   u = 0;
   con2 = (1 - a/c)*u;
   polyxy[0][0] = (c - a)*cos( a*u/c ) + b*cos( con2 ) + xcenter;
   polyxy[0][1] = (c - a)*sin( a*u/c ) - b*sin( con2 ) + ycenter;

   for ( i = 1; i <= ilines; i++ )
   {
      u = con1*i;
      con2 = (1 - a/c)*u;
      if( con2 >=  16777216. )
      {
         con2 = fmod( con2, PI );
      }
      polyxy[i][0] = (c - a)*cos( a*u/c ) + b*cos( con2 ) + xcenter;
      polyxy[i][1] = (c - a)*sin( a*u/c ) - b*sin( con2 ) + ycenter;
   }
   if (ilines >= 0){ 
   	vogle_poly2(ilines,polyxy);
   }
   return;
}
