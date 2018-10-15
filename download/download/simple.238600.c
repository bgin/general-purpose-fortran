#include <stdlib.h>
#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) show basic line drawing, text and (if applicable) color";

/*
 *
 * As none of the projection routines have been called we
 * move and draw in the initial coordinate system -1.0 to 1.0.
 */
int main(ac, av)
   int   ac;
   char  **av;
{
   char  device[10], *p;
   float cw, ch;

   fprintf(stderr,"Enter output device: ");
   fgets(device,20,stdin);

   vogle_prefsize(300, 300);
   vogle_prefposition(100, 100);
   vogle_vinit(device);  /* set up device */

   if (ac == 2)
      vogle_font(av[1]); /* change font to the argument */

   vogle_color(BLACK);  /* set current color */
   vogle_clear();  /* clear screen to current color */

   vogle_color(GREEN);
         /* 2 d move to start where we want drawstr to start */
   vogle_move2(-0.9f, 0.9f);

   /* draw string in current color */
   vogle_drawstr("A Simple Example 0 1 2 3 4 5 6 7 8 9"); 

   /*
    * the next four lines draw the x 
    */
   vogle_move2(0.0f,  0.0f);
   vogle_draw2(0.76f, 0.76f);
   vogle_move2(0.0f,  0.76f);
   vogle_draw2(0.76f, 0.0f);

   vogle_move2(0.0f,0.5f);
   vogle_drawstr("x done");
   vogle_drawstr("next sentence");

   vogle_move2(0.0f,0.1f);
/* for (p = "hello world"; *p != (void)NULL; p++) */
   for (p = "hello world"; *p != (char)NULL; p++) 
      vogle_drawchar(*p);  /* draw the string one char at a time */

   /*
    * the next five lines draw the square
    */
   vogle_move2(0.f,0.f);
   vogle_draw2(.76f,0.f);
   vogle_draw2(.76f,.76f);
   vogle_draw2(0.f,.76f);
   vogle_draw2(0.f,0.f);

   vogle_getkey();  /* wait for some input */

   vogle_vexit();  /* set the screen back to its original state */
}
