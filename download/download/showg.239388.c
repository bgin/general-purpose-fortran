#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vogle.h"

static char ident[] = "@(#) this program will read geometry files in the OFF format.";

#define     GEOM  1
#define     TOPLEFT  2
#define     TOPRIGHT 3
#define     BOTTOMLEFT 4
#define     BOTTOMRIGHT 5
#define     MIN(x, y) ((x) < (y) ? (x) : (y))
#define     MAX(x, y) ((x) > (y) ? (x) : (y))

typedef struct v {
   float x, y, z;
} vertex;

static   char title[128] = "G'day there mate";

FILE  *fp;


void read_err(s)
   char *s;
{
   vogle_vexit();
   fprintf(stderr,"Error reading input file near %s\n", s);
   exit(1);
}

/*
 * makeobject
 *
 * read in and set up set up the object
 */
void makeobject()
{
   float    fmax, fmin;
   int      i, j, verts, connections, edges, npoints, pointno;
   vertex    *vlist;

   vogle_makeobj(GEOM);

      if (fscanf(fp, "%d %d %d\n", &verts, &connections, &edges) != 3)
         read_err("nverts npoly nedges");

      vlist = (vertex *)malloc(sizeof(vertex) * verts);

      fmax = -1.0e20;
      fmin = 1.0e20;

      for (i = 0; i != verts; i++) {
         if(fscanf(fp,"%f %f %f\n", &vlist[i].x, &vlist[i].y, &vlist[i].z) != 3)
            read_err("coordinate information");

         fmin = MIN(fmin, vlist[i].x);
         fmin = MIN(fmin, vlist[i].y);
         fmin = MIN(fmin, vlist[i].z);

         fmax = MAX(fmax, vlist[i].x);
         fmax = MAX(fmax, vlist[i].y);
         fmax = MAX(fmax, vlist[i].z);
      }

      /* 
       * Scale the object.
       */
      fmax = fmax - fmin;
      vogle_scale(6.0 / fmax, 6.0 / fmax, 6.0 / fmax);

      for (i = 0; i != connections; i++) {
         if (fscanf(fp, "%d", &npoints) != 1)
            read_err("connectivity 1");

         vogle_makepoly();
            if (fscanf(fp, "%d", &pointno) != 1)
             read_err("connectivity 2");

            pointno--;
            vogle_move(vlist[pointno].x, vlist[pointno].y, vlist[pointno].z);
            for (j = 1; j != npoints; j++) {
             if (fscanf(fp, "%d", &pointno) != 1)
              read_err("connectivity 3");

             pointno--;
             vogle_draw(vlist[pointno].x, vlist[pointno].y, vlist[pointno].z);
            }
         vogle_closepoly();
      }

   vogle_closeobj();
}

/*
 * side
 *
 * define a face for the cube
 */
void side(){ 
   vogle_pushmatrix();
      vogle_translate(0.0, 0.0, 1.0);
      vogle_rect(-1.0, -1.0, 1.0, 1.0);
   vogle_popmatrix();
}

/*
 * makecube
 *
 * set up a cube
 */
void makecube()
{

   vogle_makeobj(GEOM);

      /*
       * The border around the cube
       */
      vogle_rect(-5.0, -5.0, 10.0, 10.0);

      /*
       * Make the cube from 4 squares
       */
      vogle_pushmatrix();
         side();
         vogle_rotate(90.0, 'x');
         side();
         vogle_rotate(90.0, 'x');
         side();
         vogle_rotate(90.0, 'x');
         side();
      vogle_popmatrix();

      vogle_move2(-4.5, -4.5);
      vogle_drawstr(title);
   vogle_closeobj();
}



/*
 * this program will read geometry files in the OFF format.
 * Demonstrate just how much you can put in an object
 */
int main( int argc, char *argv[] ){
   int   i, backface_on = 0, direction = 0;
   char  *filename = (char *)NULL;

   if (argc < 2) {
      fprintf(stderr,"Usage: %s [-b] [-a] filename\n", argv[0]);
      exit(1);
   }

   for (i = 1; i < argc; i++) {
      if (strcmp(argv[i], "-b") == 0)
         backface_on = 1;
      if (strcmp(argv[i], "-a") == 0)
         direction = 1;
      else
         filename = argv[i];
   }
      
   /*
    * Uses the environment variable VDEVICE to find device....
    */
   vogle_vinit(0);

   vogle_backface(backface_on);
   vogle_backfacedir(direction);

   vogle_font("times.r");
   vogle_textsize(0.3, 0.6);

   if (filename) {
      if ((fp = fopen(filename, "r")) == (FILE *)NULL) {
         vogle_vexit();
         fprintf(stderr, "Couldn't open %s for reading\n", filename);
         exit(1);
      }
      makeobject();
   } else 
      makecube();

   vogle_color(BLACK);
   vogle_clear();

   /*
    * set up an object which draws in the top left of the screen.
    */
   vogle_makeobj(TOPLEFT);
      vogle_viewport(-1.0, 0.0, 0.0, 1.0);
      vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

      vogle_color(RED);

      vogle_rect(-5.0, -5.0, 5.0, 5.0);
      vogle_move2(-4.8, -4.8);
      vogle_color(WHITE);
      vogle_drawstr(title);

      vogle_perspective(50.0, 1.0, 0.1, 1000.0);
      vogle_lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0);

      vogle_color(RED);
      vogle_callobj(GEOM);

   vogle_closeobj();

   /*
    * now set up one which draws in the top right of the screen
    */
   vogle_makeobj(TOPRIGHT);
      vogle_viewport(0.0, 1.0, 0.0, 1.0);
      vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

      vogle_color(GREEN);

      vogle_rect(-5.0, -5.0, 5.0, 5.0);
      vogle_move2(-4.8, -4.8);
      vogle_color(WHITE);
      vogle_drawstr(title);

      vogle_window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0);
      vogle_lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 90.0);

      vogle_color(GREEN);
      vogle_callobj(GEOM);

      vogle_color(RED);

   vogle_closeobj();

   /*
    * try the bottom left
    */
   vogle_makeobj(BOTTOMLEFT);
      vogle_viewport(-1.0, 0.0, -1.0, 0.0);
      vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

      vogle_color(MAGENTA);

      vogle_rect(-5.0, -5.0, 5.0, 5.0);
      vogle_move2(-4.8, -4.8);
      vogle_color(WHITE);
      vogle_drawstr(title);

      vogle_perspective(40.0, 1.0, 0.1, 1000.0);
      vogle_polarview(15.0, 30.0, 30.0, 30.0);

      vogle_color(MAGENTA);
      vogle_callobj(GEOM);

      vogle_color(YELLOW);

   vogle_closeobj();

   /*
    * and the bottom right
    */
   vogle_makeobj(BOTTOMRIGHT);
      vogle_viewport(0.0, 1.0, -1.0, 0.0);
      vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

      vogle_color(CYAN);

      vogle_rect(-5.0, -5.0, 5.0, 5.0);
      vogle_move2(-4.8, -4.8);
      vogle_color(WHITE);
      vogle_drawstr(title);

      vogle_window(-6.0, 6.0, -6.0, 6.0, -6.0, 6.0);
      vogle_polarview(18.0, -38.0, -19.0, 18.0);

      vogle_color(CYAN);
      vogle_callobj(GEOM);

      vogle_color(BLUE);

   vogle_closeobj();

   /*
    * now draw them
    */
   vogle_callobj(TOPLEFT);
   vogle_callobj(TOPRIGHT);
   vogle_callobj(BOTTOMLEFT);
   vogle_callobj(BOTTOMRIGHT);

   vogle_getkey();

   vogle_vexit();
}
