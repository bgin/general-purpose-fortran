static char ident[] = "@(#) draw spinning cube";

#include <stdio.h>
#include <stdlib.h>
#include <vogle.h>

float carray[][3] = { -1.0,  -1.0,   1.0, /* front */
            1.0,  -1.0,   1.0,
            1.0,   1.0,   1.0,
           -1.0,   1.0,   1.0,
           -1.0,  -1.0,  -1.0, /* rear */
            1.0,  -1.0,  -1.0,
            1.0,   1.0,  -1.0,
           -1.0,   1.0,  -1.0
      };

int   nplanes;

/*
 *  drawcube: draw the cube, setting colours if available
 */
void drawcube() {
   if (nplanes > 1)
      vogle_color(RED);

   vogle_makepoly(); /* Front */
      vogle_move(carray[0][0], carray[0][1], carray[0][2]);
      vogle_draw(carray[1][0], carray[1][1], carray[1][2]);
      vogle_draw(carray[2][0], carray[2][1], carray[2][2]);
      vogle_draw(carray[3][0], carray[3][1], carray[3][2]);
      vogle_draw(carray[0][0], carray[0][1], carray[0][2]);
   vogle_closepoly();

   if (nplanes > 1)
      vogle_color(GREEN);

   vogle_makepoly(); /* Back */
      vogle_move(carray[5][0], carray[5][1], carray[5][2]);
      vogle_draw(carray[4][0], carray[4][1], carray[4][2]);
      vogle_draw(carray[7][0], carray[7][1], carray[7][2]);
      vogle_draw(carray[6][0], carray[6][1], carray[6][2]);
      vogle_draw(carray[5][0], carray[5][1], carray[5][2]);
   vogle_closepoly();

   if (nplanes > 1)
      vogle_color(YELLOW);

   vogle_makepoly(); /* Right side */
      vogle_move(carray[1][0], carray[1][1], carray[1][2]);
      vogle_draw(carray[5][0], carray[5][1], carray[5][2]);
      vogle_draw(carray[6][0], carray[6][1], carray[6][2]);
      vogle_draw(carray[2][0], carray[2][1], carray[2][2]);
      vogle_draw(carray[1][0], carray[1][1], carray[1][2]);
   vogle_closepoly();

   if (nplanes > 1)
      vogle_color(BLUE);

   vogle_makepoly(); /* Left side */
      vogle_move(carray[0][0], carray[0][1], carray[0][2]);
      vogle_draw(carray[3][0], carray[3][1], carray[3][2]);
      vogle_draw(carray[7][0], carray[7][1], carray[7][2]);
      vogle_draw(carray[4][0], carray[4][1], carray[4][2]);
      vogle_draw(carray[0][0], carray[0][1], carray[0][2]);
   vogle_closepoly();

   if (nplanes > 1)
      vogle_color(MAGENTA);

   vogle_makepoly(); /* Top */
      vogle_move(carray[2][0], carray[2][1], carray[2][2]);
      vogle_draw(carray[6][0], carray[6][1], carray[6][2]);
      vogle_draw(carray[7][0], carray[7][1], carray[7][2]);
      vogle_draw(carray[3][0], carray[3][1], carray[3][2]);
      vogle_draw(carray[2][0], carray[2][1], carray[2][2]);
   vogle_closepoly();

   if (nplanes > 1)
      vogle_color(CYAN);

   vogle_makepoly(); /* Bottom */
      vogle_move(carray[0][0], carray[0][1], carray[0][2]);
      vogle_draw(carray[4][0], carray[4][1], carray[4][2]);
      vogle_draw(carray[5][0], carray[5][1], carray[5][2]);
      vogle_draw(carray[1][0], carray[1][1], carray[1][2]);
      vogle_draw(carray[0][0], carray[0][1], carray[0][2]);
   vogle_closepoly();
}


int main( int argc, char *argv[] ){
   char  c, device[10], *p;
   float r, t, dr = 10.0, dt = 0.2;
   int   fill, back, backdir, nkey;

   fprintf(stderr,"Enter output device: ");
   fgets(device,20,stdin);

   vogle_prefsize(300, 300);

   vogle_vinit(device);

   nplanes = vogle_getdepth();

   fill = 0;
   back = 1;
   backdir = 1;

   vogle_polyfill(fill);

   vogle_color(BLACK);
   vogle_clear();

   vogle_window(-1.5, 1.5, -1.5, 1.5, 9.0, -5.0);
   vogle_lookat(0.0, 0.0, 12.0, 0.0, 0.0, 0.0, 0.0);

   vogle_backface(back);
   vogle_backfacedir(backdir);

   if (vogle_backbuffer() < 0) {
      vogle_vexit();
      fprintf(stderr, "cube: device doesn't support double buffering.\n");
      exit(0);
   }

   t = 0.0;
   c = '\0';

   do {
      for (r = 0.0; r < 360.0; r += dr) {
         vogle_color(BLACK);
         vogle_clear();
         vogle_pushmatrix();
            vogle_translate(0.0, 0.0, t);
            vogle_rotate(r, 'y');
            vogle_rotate(r, 'z');
            vogle_rotate(r, 'x');
            vogle_color(WHITE);
            drawcube();
            if (nplanes == 1 && argc > 1) {
               vogle_polyfill(0);
               vogle_color(0);
               drawcube();
               vogle_polyfill(fill);
            }

         vogle_popmatrix();

         if (c == 'p') {
            vogle_vnewdev(device);
            c = '\0';
         }

         t += dt;
         if (t > 3.0 || t < -18.0)
            dt = -dt;

         vogle_swapbuffers();

         if (c = vogle_checkkey()) {
            if (c == 'p') {
               vogle_voutput("cube.ps");
               vogle_vnewdev("postscript");
            } else if (c == 'f') {
               fill = !fill;
               vogle_polyfill(fill);
            } else if (c == 'b') {
               back = !back;
               vogle_backface(back);
            } else if (c == 'd') {
               backdir = !backdir;
               vogle_backfacedir(backdir);
            } else {
               vogle_vexit();
               exit(0);
            }
         }
      }
   } while(1);

   /* vexit(); */
}
