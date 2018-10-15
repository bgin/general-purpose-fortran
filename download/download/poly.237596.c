#include <stdlib.h>
#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) An array of points for a polygon";

static float    parray[][3] = {
        {-8.0, -8.0, 0.0},
        {-5.0, -8.0, 0.0},
        {-5.0, -5.0, 0.0},
        {-8.0, -5.0, 0.0}
};

/*
 * drawpoly
 *
 *      draw some polygons
 */
void drawpoly(){
        vogle_color(YELLOW);

        /*
         * Draw a polygon using poly, parray is our array of
         * points and 4 is the number of points in it.
         */
        vogle_poly(4, parray);

        vogle_color(GREEN);

        /*
         * Draw a 5 sided figure by using move, draw and closepoly.
         */
        vogle_makepoly();
                vogle_move(0.0, 0.0, 0.0);
                vogle_draw(3.0, 0.0, 0.0);
                vogle_draw(3.0, 4.0, 0.0);
                vogle_draw(-1.0, 5.0, 0.0);
                vogle_draw(-2.0, 2.0, 0.0);
        vogle_closepoly();

        vogle_color(MAGENTA);

        /*
         * draw a sector representing a 1/4 circle
         */
        vogle_sector(1.5, -7.0, 3.0, 0.0, 90.0);

        vogle_getkey();
}

/*
 * Using polygons, hatching, and filling.
 */
int main( int argc, char *argv[] ){
        char device[10];

        fprintf(stderr,"Enter output device: ");
        fgets(device,20,stdin);

        vogle_vinit(device);

        vogle_color(BLACK);           /* clear to bleck */
        vogle_clear();

        /*
         * world coordinates are now in the range -10 to 10
         * in x, y, and z. Note that positive z is towards us.
         */
        vogle_ortho(-10.0, 10.0, -10.0, 10.0, 10.0, -10.0);

        vogle_color(YELLOW);

        /*
         * write out the string "Polygon from poly()" in the
         * starting at (-8.0, -4.0) and scaled to be 4.0 units long,
         * 0.5 units high.
         */
        vogle_boxtext(-8.0, -4.0, 4.0, 0.5, "Polygon from poly()");

        vogle_color(GREEN);

        /*
         * write out a scaled string starting at (0.0, 6.0)
         */
        vogle_boxtext(0.0, 6.0, 4.0, 0.5, "Polygon from move()/ draw()");

        vogle_color(MAGENTA);

        /*
         * write out a scaled string starting at (0.0, 6.0)
         */
        vogle_boxtext(3.5, -3.5, 1.9, 0.5, "Sector");

        drawpoly();             /* draw some polygons */

        /*
         * turn on polygon hatching
         */
        vogle_polyhatch(1);
        vogle_hatchang(45.0);
        vogle_hatchpitch(0.3);

        /*
         *  Rotate 20 degrees around x and 30 around y
         */
        vogle_rotate(20.0, 'x');
        vogle_rotate(30.0, 'y');

        drawpoly();             /* draw some polygons wth hatching */

        /*
         * turn on polygon filling - this automatically turns off hatching
         */
        vogle_polyfill(1);

        /*
         *  Do another set of rotations.
         */
        vogle_rotate(20.0, 'x');
        vogle_rotate(30.0, 'y');

        drawpoly();             /* draw some polygons with filling */

        vogle_vexit();
}
