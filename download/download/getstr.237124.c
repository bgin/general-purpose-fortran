#include "vogle.h"
#include <stdio.h>
static char ident[] = "@(#) Test the getting of strings in graphics mode";

int main (int argc, char *argv[]) {
   char device[10], *p;
   float cw, ch;
   int i, n;
   char buf[10][128];

   fprintf (stderr, "Enter output device: ");
   fgets (device, 20, stdin);

   vogle_vinit (device);

   if (argc > 1)
      vogle_font (argv[1]);

   vogle_clipping (0);

   vogle_window (-1.0, 1.0, -1.0, 1.0, 1.0, -1.0);
   vogle_lookat (0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0);

   vogle_textsize (0.1, 0.25);


   vogle_rotate (30.0, 'x');
   vogle_rotate (30.0, 'z');
   vogle_rotate (60.0, 'y');

   vogle_color (BLACK);
   vogle_clear ();
   vogle_color (YELLOW);

   vogle_rect (-0.5, -0.5, 0.5, 0.5);
   vogle_move2 (-0.5, 0.0);

   vogle_color (GREEN);

   n = 0;
   while ((i = vogle_getstring (BLACK, buf[n]) && n < 10)) {
      printf("%d %d %s \n ",i,n,buf[n]);
      n++;
   }

   vogle_vexit ();

   for (i = 0; i < n; i++){
      printf ("Line %d was: %s\n", i + 1, buf[i]);
   }

}
