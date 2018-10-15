#include <stdlib.h>
#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) the basic test program for a driver";

/*
 * if we can draw a line and do hardware text we are almost there!
 */
int main( int argc, char *argv[] ){
   char device[100];

   fprintf(stderr, "Enter output device: ");   /* read in device name */
   fgets(device,20,stdin);

   vogle_vinit(device);

   if (argc == 2)
      vogle_font(argv[1]);  /* set font to argument */
   else
      vogle_font("large");  /* set font to hardware text large */

   vogle_color(BLACK);   /* we want to clear in black */
   vogle_clear();   /* clear to current color */

   vogle_color(GREEN);   /* set current color to green */

   vogle_move2(-1.0f, 0.0f);  /* draw a horizontal line at y = 0 */
   vogle_draw2(1.0f, 0.0f);

   vogle_getkey();   /* pause for some input */

   vogle_move2(0.0f, 0.0f);  /* draw a line along x = 0 */
   vogle_draw2(0.0f, 1.0f);

   vogle_move2(0.0f, 0.0f);  /* move to the middle of the screen */
   vogle_drawstr("Hello");  /* draw "Hello" starting at the origin */

   vogle_getkey();   /* pause again */

   vogle_vexit();   /* set screen back to original state */
}
