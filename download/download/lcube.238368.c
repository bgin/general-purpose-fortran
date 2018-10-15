static char ident[] = "@(#)draw cube and sample and track mouse ";
#include <stdio.h>
#include <stdlib.h>
#include <vogle.h>

#define  CUBE_SIZE 200.0
#define  TRANS  25.0
#define  SCAL   0.1
#define FACE    1
#define FILLED  2
#define OUTLINE  3
void makecube(obj)
   int   obj;
{
   vogle_makeobj(obj);
      if (obj == OUTLINE) {
         vogle_pushattributes();
         vogle_polyfill(0);
         vogle_polyhatch(0);
         vogle_color(BLACK);
      }

      vogle_pushmatrix();
         vogle_translate(0.0, 0.0, CUBE_SIZE);
         if (obj == FILLED)
            vogle_color(RED);

         vogle_callobj(FACE);
      vogle_popmatrix();

      vogle_pushmatrix();
         vogle_translate(CUBE_SIZE, 0.0, 0.0);
         vogle_rotate(90.0, 'y');
         if (obj == FILLED)
            vogle_color(GREEN);

         vogle_callobj(FACE);
      vogle_popmatrix();

      vogle_pushmatrix();
         vogle_translate(0.0, 0.0, -CUBE_SIZE);
         vogle_rotate(180.0, 'y');
         if (obj == FILLED)
            vogle_color(BLUE);

         vogle_callobj(FACE);
      vogle_popmatrix();

      vogle_pushmatrix();
         vogle_translate(-CUBE_SIZE, 0.0, 0.0);
         vogle_rotate(-90.0, 'y');
         if (obj == FILLED)
            vogle_color(CYAN);

         vogle_callobj(FACE);
      vogle_popmatrix();

      vogle_pushmatrix();
         vogle_translate(0.0, CUBE_SIZE, 0.0);
         vogle_rotate(-90.0, 'x');
         if (obj == FILLED)
            vogle_color(MAGENTA);

         vogle_callobj(FACE);
      vogle_popmatrix();

      vogle_pushmatrix();
         vogle_translate(0.0, -CUBE_SIZE, 0.0);
         vogle_rotate(90.0, 'x');
         if (obj == FILLED)
            vogle_color(YELLOW);

         vogle_callobj(FACE);
      vogle_popmatrix();

      if (obj == OUTLINE)
         vogle_popattributes();

   vogle_closeobj();
}
/*
 * draw cube and sample and track mouse
 */

int main( int argc, char *argv[] ){
        char    device[10], *p;
   float x, y, tdir = TRANS;
   float scal = 1.0 + SCAL;
   int   but, nplanes;
   int   back, fill, hatch, i, n;

   fprintf(stderr,"Enter output device: ");
   fgets(device,20,stdin);


   vogle_prefposition(50, 50);
   vogle_prefsize(500, 500);

   vogle_vinit(device);

   vogle_window(-800.0, 800.0, -800.0, 800.0, -800.0, 800.0);
   vogle_lookat(0.0, 0.0, 1500.0, 0.0, 0.0, 0.0, 0.0);

   /*
    * Start with a very ordinary filled cube like the old demo..
    */
   vogle_polyhatch(0);
   vogle_hatchang(45.0);
   vogle_hatchpitch(40.0);
   vogle_polyfill(1);

   fill = 1;
   hatch = 0;
   back = 1;

   vogle_makeobj(FACE);  /* hatched or filled polygon */
      vogle_makepoly();
         vogle_rect(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
      vogle_closepoly();
   vogle_closeobj();

   makecube(FILLED);

   if ((nplanes = vogle_getdepth()) == 1)
      makecube(OUTLINE);

   vogle_backface(1);

   if (vogle_backbuffer() < 0) {
      vogle_vexit();
      fprintf(stderr, "lcube: device doesn't support double buffering.\n");
      exit(0);
   }

   while((but = vogle_slocator(&x, &y)) != 44) {
      vogle_pushmatrix();
         vogle_rotate(100.0 * x, 'y');
         vogle_rotate(100.0 * y, 'x');
         vogle_color(BLACK);
         vogle_clear();
         vogle_callobj(FILLED); /* The filled or hatched one */
         if (nplanes == 1 && (fill || hatch))
            vogle_callobj(OUTLINE); /* The outline */

      vogle_popmatrix();
      vogle_swapbuffers();

      switch (but = vogle_checkkey()) {
      case 'p':
         vogle_voutput("lcube.ps");
         vogle_vnewdev("postscript");
         vogle_pushmatrix();
            vogle_rotate(100.0 * x, 'y');
            vogle_rotate(100.0 * y, 'x');
            vogle_color(BLACK);
            vogle_clear();
            vogle_callobj(FILLED);
            if (nplanes == 1 && (fill || hatch))
               vogle_callobj(OUTLINE);
         vogle_popmatrix();
         vogle_vnewdev(device);
         (void)vogle_backbuffer();
         break;
      case 'f': /* Toggle filling */
         fill = !fill;
         hatch = 0;
         vogle_polyfill(fill);
         break;
      case 'h': /* Toggle hatching */
         hatch = !hatch;
         fill = 0;
         vogle_polyhatch(hatch);
         break;
      case 'b':  /* Toggle backfacing */
         back = !back;
         vogle_backface(back);
         break;
      case 's':
         vogle_scale(scal, scal, scal);
         break;
      case 'x':
         vogle_translate(tdir, 0.0, 0.0);
         break;
      case 'y':
         vogle_translate(0.0, tdir, 0.0);
         break;
      case 'z':
         vogle_translate(0.0, 0.0, tdir);
         break;
      case '-':
         tdir = -tdir;

         if (scal < 1.0)
            scal = 1.0 + SCAL;
         else
            scal = 1.0 - SCAL;

         break;
      case '+':
         tdir = TRANS;
         break;
      case 27: /* ESC */
      case 'q':
         vogle_vexit();
         exit(0);
      default:
         ;
      }
   }

   vogle_vexit();
}
