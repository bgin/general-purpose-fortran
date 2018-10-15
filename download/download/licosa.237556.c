
static char ident[] = "@(#) draw regular solid and track mouse ";

#include <stdio.h>
#include <stdlib.h>
#include <vogle.h>

#define  TRANS    0.06

static float   xyz[12][3] = {
   {0.000000, 0.000000, 1.0},
   {0.809017, -0.587785, 0.500000},
   {0.809017, 0.587785, 0.500000},
   {-0.309017, 0.951057, 0.500000},
   {-1.000000, 0.000000, 0.500000},
   {-0.309017, -0.951057, 0.500000},
   {1.000000, 0.000000, -0.500000},
   {0.309017, 0.951057, -0.500000},
   {-0.809017, 0.587785, -0.500000},
   {-0.809017, -0.587785, -0.500000},
   {0.309017, -0.951057, -0.500000},
   {0.000000, 0.000000, -1.0}
};
static int  ncon[20][3] = {
   {1, 2, 3},
   {1, 3, 4},
   {1, 4, 5},
   {1, 5, 6},
   {1, 6, 2},
   {2, 7, 3},
   {3, 8, 4},
   {4, 9, 5},
   {5, 10, 6},
   {6, 11, 2},
   {7, 8, 3},
   {8, 9, 4},
   {9, 10, 5},
   {10, 11, 6},
   {11, 7, 2},
   {7, 12, 8},
   {8, 12, 9},
   {9, 12, 10},
   {10, 12, 11},
   {11, 12, 7}
};

void drawshape(fill)
   int   fill;
{
   int   i;

   vogle_polyfill(fill);
   if (!fill)
      vogle_color(BLACK);

   for (i = 0; i < 20; i++) {
      if (fill)
         vogle_color(i + 1);

      vogle_makepoly();
   vogle_move(xyz[ncon[i][0]-1][0], xyz[ncon[i][0]-1][1], xyz[ncon[i][0]-1][2]);
   vogle_draw(xyz[ncon[i][1]-1][0], xyz[ncon[i][1]-1][1], xyz[ncon[i][1]-1][2]);
   vogle_draw(xyz[ncon[i][2]-1][0], xyz[ncon[i][2]-1][1], xyz[ncon[i][2]-1][2]);
      vogle_closepoly();
   }
}

int main( int argc, char *argv[] ){
        char    device[10], *p;
   float x, y, tdir = TRANS;
   int   but, nplanes;
   int   i, n;
   char  buf[10][128];

   fprintf(stderr,"Enter output device: ");
   fgets(device,20,stdin);

   vogle_prefposition(50, 50);
   vogle_prefsize(300, 300);

   vogle_vinit(device);
   nplanes = vogle_getdepth();

   vogle_window(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   vogle_lookat(0.0, 0.0, 2.1, 0.0, 0.0, 0.0, 0.0);

   vogle_textsize(0.15, 0.3);

   vogle_backface(1);

   /*
    * Green color ramp...
    */
   for (i = 1; i <= 20; i++)
      vogle_mapcolor(i, 20, 20 + i * 10 , 20);

   if (vogle_backbuffer() < 0) {
      vogle_vexit();
      fprintf(stderr, "licosa: device doesn't support double buffering.\n");
      exit(0);
   }

   while((but = vogle_slocator(&x, &y)) != 44) {
      vogle_pushmatrix();
         vogle_rotate(120.0 * x, 'y');
         vogle_rotate(120.0 * y, 'x');
         vogle_color(BLACK);
         vogle_clear();
         vogle_color(20);
         vogle_move(0.0, 0.0, 0.0);
         vogle_drawstr("Hello, world");
         drawshape(1);
         if (nplanes == 1)
            drawshape(0);
      vogle_popmatrix();
      vogle_swapbuffers();

      switch (but = vogle_checkkey()) {
      case 'p':
         vogle_voutput("licosa.ps");
         vogle_vnewdev("postscript");
         vogle_pushmatrix();
            vogle_rotate(100.0 * x, 'y');
            vogle_rotate(100.0 * y, 'x');
            vogle_color(BLACK);
            vogle_clear();
            drawshape(1);
            if (nplanes == 1)
               drawshape(0);
         vogle_popmatrix();
         vogle_vnewdev(device);
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
