#include <stdio.h>
#include "vogle.h"
static char ident[] = "@(#) using curves ";

/*
 * curve basis types
 */
Matrix  bezier = {
        {-1.0,  3.0,    -3.0,   1.0},
        {3.0,   -6.0,   3.0,    0.0},
        {-3.0,  3.0,    0.0,    0.0},
        {1.0,   0.0,    0.0,    0.0} 
};

Matrix  cardinal = {
        {-0.5,  1.5,    -1.5,   0.5},
        {1.0,   -2.5,   2.0,    -0.5},
        {-0.5,  0.0,    0.5,    0.0},
        {0.0,   1.0,    0.0,    0.0}
};

Matrix  bspline = {
        {-1.0 / 6.0,    3.0 / 6.0,      -3.0 / 6.0,     1.0 / 6.0},
        {3.0 / 6.0,     -6.0 / 6.0,     3.0 / 6.0,      0.0},
        {-3.0 / 6.0,    0.0,            3.0 / 6.0,      0.0},
        {1.0 / 6.0,     4.0 / 6.0,      1.0 / 6.0,      0.0}    
};

/*
 *      Geometry matrix to demonstrate basic spline segments
 */
float   geom1[4][3] = {
        { -180.0, 10.0, 0.0 },
        { -100.0, 110.0, 0.0 },
        { -100.0, -90.0, 0.0 },
        { 0.0, 50.0, 0.0 }
};

/*
 *      Geometry matrix to demonstrate overlapping control points to
 *      produce continuous (Well, except for the bezier ones) curves
 *      from spline segments
 */
float   geom2[6][3] = {
        { 200.0, 480.0, 0.0 },
        { 380.0, 180.0, 0.0 },
        { 250.0, 430.0, 0.0 },
        { 100.0, 130.0, 0.0 },
        { 50.0,  280.0, 0.0 },
        { 150.0, 380.0, 0.0 }
};

/*
 *  using curves
 */
int main( int argc, char *argv[] ){
        char    dev[20];
        int     i;

        fprintf(stderr,"Enter device: ");

        fgets(dev,20,stdin);
        vogle_vinit(dev);

        vogle_ortho2(-200.0, 400.0, -100.0, 500.0);

        vogle_color(BLACK);
        vogle_clear();

        vogle_color(YELLOW);

        vogle_textsize(10.0, 10.0);

        /*
         * label the control points in geom1
         */
        for (i = 0; i < 4; i++) {
                vogle_move2(geom1[i][0], geom1[i][1]);
                sprintf(dev, "%d", i);
                vogle_drawstr(dev);
        }
                                                                 
        /*
         * label the control points in geom2
         */
        for (i = 0; i < 6; i++) {
                vogle_move2(geom2[i][0], geom2[i][1]);
                sprintf(dev, "%d", i);
                vogle_drawstr(dev);
        }

        /*
         * scale the current font so that 30 of the largest characters
         * in the current font will fit in a region 300 world units wide,
         * 20 high.
         */
        vogle_boxfit(300.0, 20.0, 30);

        /*
         * set the number of line segments appearing in each curve to 20
         */
        vogle_curveprecision(20);

        /*
         * copy the bezier basis matrix into the curve basis matrix.
         */
        vogle_curvebasis(bezier);

        vogle_color(RED);

        /*
         * draw a curve using the current basis matrix (bezier in this case)
         * and the control points in geom1
         */
        vogle_curve(geom1);

        vogle_move2(70.0, 60.0);
        vogle_drawstr("Bezier Curve Segment");

        vogle_move2(-190.0, 450.0);
        vogle_drawstr("Three overlapping Bezier Curves");

        /*
         * curven draws overlapping curve segments according to geom2, the
         * number of curve segments drawn is three less than the number of
         * points passed, assuming there are a least four points in the
         * geometry matrix (in this case geom2). This call will draw 3
         * overlapping curve segments in the current basis matrix - still
         * bezier.
         */
        vogle_curven(6, geom2);

        vogle_getkey();

        /*
         * load in the cardinal basis matrix
         */
        vogle_curvebasis(cardinal);

        vogle_color(MAGENTA);

        vogle_move2(70.0, 10.0);
        vogle_drawstr("Cardinal Curve Segment");

        /*
         * plot out a curve segment using the cardinal basis matrix
         */
        vogle_curve(geom1);

        vogle_move2(-190.0, 400.0);
        vogle_drawstr("Three overlapping Cardinal Curves");

        /*
         * now draw a bunch of them again.
         */
        vogle_curven(6, geom2);

        vogle_getkey();

        /*
         * change the basis matrix again
         */
        vogle_curvebasis(bspline);

        vogle_color(GREEN);

        vogle_move2(70.0, -40.0);
        vogle_drawstr("Bspline Curve Segment");

        /*
         * now draw our curve segment in the new basis...
         */
        vogle_curve(geom1);

        vogle_move2(-190.0, 350.0);
        vogle_drawstr("Three overlapping Bspline Curves");

        /*
         * ...and do some overlapping ones
         */
        vogle_curven(6, geom2);

        vogle_getkey();

        vogle_vexit();
}
