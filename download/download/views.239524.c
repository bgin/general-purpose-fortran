#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) various combinations of viewing and projection transformations";

void drawtetra(){ /* generate a tetrahedron as a series of move draws */
        
        vogle_move(-0.5f,  0.866f, -0.5f);
        vogle_draw(-0.5f, -0.866f, -0.5f);
        vogle_draw( 1.0f,  0.0f,   -0.5f);
        vogle_draw(-0.5f,  0.866f, -0.5f);
        vogle_draw( 0.0f,  0.0f,    1.5f);
        vogle_draw(-0.5f, -0.866f, -0.5f);
        vogle_move( 1.0f,  0.0f,   -0.5f);
        vogle_draw( 0.0f,  0.0f,    1.5f);
        
        /* 
         * Label the vertices.
         */
        vogle_color(WHITE);
        vogle_textsize(0.3f, 0.5f);             /* set the text size */
        vogle_move(-0.5f,  0.866f, -0.5f);
        vogle_drawchar('a');
        vogle_move(-0.5f, -0.866f, -0.5f);
        vogle_drawchar('b');
        vogle_move( 1.0f,  0.0f,   -0.5f);
        vogle_drawchar('c');
        vogle_move( 0.0f,  0.0f,    1.5f);
        vogle_drawchar('d');
}

/*
 * various combinations of viewing and projection transformations
 */
int main( int argc, char *argv[] ){
        char device[20];

        fprintf(stderr,"Enter device name: ");
        fgets(device,20,stdin);
        vogle_vinit(device);

        vogle_color(BLACK);
        vogle_clear();

        /*
         * we want to draw just within the boundaries of the screen
         */
        vogle_viewport(-0.9f, 0.9f, -0.9f, 0.9f);


        vogle_ortho2(-5.0f, 5.0f, -5.0f, 5.0f);   /* set the world size */

        vogle_color(RED);
        vogle_rect(-5.0f, -5.0f, 5.0f, 5.0f);     /* draw a boundary frame */

        /*
         * set up a perspective projection with a field of view of
         * 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
         * and the far clipping plane at 1000.0.
         */
        vogle_perspective(40.0f, 1.0f, 0.1f, 1000.0f);

        /*
         * we want the drawing to be done with our eye point at (5.0, 8.0, 5.0)
         * looking towards (0.0, 0.0, 0.0). The last parameter gives a twist
         * in degrees around the line of sight, in this case zero.
         */
        vogle_lookat(5.0f, 8.0f, 5.0f, 0.0f, 0.0f, 0.0f, 0.0f);

        drawtetra();

        vogle_move2(-4.5f, -4.5f);
        vogle_textsize(0.6f, 0.9f);             /* set the text size */
        vogle_drawstr("perspective/lookat");

        vogle_getkey();

        /*
         * window can also be used to give a perspective projection. Its
         * arguments are 6 clipping planes, left, right, bottom, top, near,
         * and far.
         */
        vogle_window(-5.0f, 5.0f, -5.0f, 5.0f, -5.0f, 5.0f);
        /*
         * as window replaces the current transformation matrix we must
         * specify our viewpoint again.
         */
        vogle_lookat(5.0f, 8.0f, 5.0f, 0.0f, 0.0f, 0.0f, 0.0f);

        vogle_color(BLACK);
        vogle_clear();

        vogle_color(GREEN);
        vogle_rect(-5.0f, -5.0f, 5.0f, 5.0f);

        drawtetra();

        vogle_move2(-4.5f,-4.5f);
        vogle_textsize(0.6f, 0.9f);             /* set the text size */
        vogle_drawstr("window/lookat");

        vogle_getkey();

        /*
         * set up our original perspective projection again.
         */
        vogle_perspective(40.0f, 1.0f, 0.1f, 1000.0f);
        /*
         * polarview also specifies our viewpoint, but, unlike lookat, in polar
         * coordinates. Its arguments are the distance from the world origin, an
         * azimuthal angle in the x-y plane measured from the y axis, an 
         * incidence angle in the y-z plane measured from the z axis, and a
         * twist around the line of sight.
         */
        vogle_polarview(15.0f, 30.0f, 30.0f, 30.0f);

        vogle_color(BLACK);
        vogle_clear();

        vogle_color(MAGENTA);
        vogle_rect(-5.0f, -5.0f, 5.0f, 5.0f);

        drawtetra();

        vogle_move2(-4.5f,-4.5f);
        vogle_textsize(0.6f, 0.9f);             /* set the text size */
        vogle_drawstr("perspective/polarview");

        vogle_getkey();

        /*
         * once more with window for comparison
         */
        vogle_window(-4.0f, 4.0f, -4.0f, 4.0f, -4.0f, 4.0f);
        vogle_polarview(6.0f, 20.0f, -30.0f, 70.0f);

        vogle_color(BLACK);
        vogle_clear();

        vogle_color(YELLOW);
        vogle_rect(-5.0f, -5.0f, 5.0f, 5.0f);

        drawtetra();

        vogle_move2(-4.5f,-4.5f);
        vogle_textsize(0.6f, 0.9f);             /* set the text size */
        vogle_drawstr("window/polarview");

        vogle_getkey();

        vogle_vexit();
}
