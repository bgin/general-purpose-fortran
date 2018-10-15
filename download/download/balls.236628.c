#include <stdlib.h>
#include <stdio.h>
#include "vogle.h"

#include <math.h>

#define RADIUS 10.0
#define SPHERE  1

/*
 *  makesphere: make a sphere object
 */
void makesphere(){
        float   i, r, z, a;

        vogle_makeobj(SPHERE);

                /*
                 * create the latitudinal rings
                 */
                for (i = 0; i < 180; i += 20) {
                        vogle_pushmatrix();
                                vogle_rotate(i, 'y');
                                vogle_circle(0.0, 0.0, RADIUS);
                        vogle_popmatrix();
                }
                
                /*
                 * create the longitudinal rings
                 */
                vogle_pushmatrix();
                        vogle_rotate(90.0, 'x');
                        for (a = -90.0; a < 90.0; a += 20.0) {
                                r = RADIUS * cos((double)a * PI / 180.0);
                                z = RADIUS * sin((double)a * PI / 180.0);
                                vogle_pushmatrix();
                                        vogle_translate(0.0, 0.0, -z);
                                        vogle_circle(0.0, 0.0, r);
                                vogle_popmatrix();    
                        }
                vogle_popmatrix();

        vogle_closeobj();
}

/*
 * balls.c: a demonstration of objects
 */
int main( int argc, char *argv[] ){
        char ident[] = "@(#)a demonstration of objects";

        char    dev[20];

        fprintf(stderr,"Enter device: ");
        fgets(dev,20,stdin);

        vogle_vinit(dev);

        vogle_vsetflush(0);

        /*
         * set up our viewing transformation
         */
        vogle_perspective(90.0, 1.0, 0.001, 500.0);
        vogle_lookat(13.0, 13.0, 8.0, 0.0, 0.0, 0.0, 0.0);

        vogle_color(BLACK);
        vogle_clear();

        /*
         * Call a routine to make the sphere object
         */
        makesphere();

        /*
         * Now draw the sphere object scaled down. We use the pushmatrix
         * and the popmatrix to preserve the transformation matrix so
         * that only this sphere is drawn scaled.
         */
        vogle_color(CYAN);

        vogle_pushmatrix();
                vogle_scale(0.5, 0.5, 0.5);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        /*
         * now we draw the same sphere translated, with a different
         * scale and color.
         */

        vogle_color(WHITE);

        vogle_pushmatrix();
                vogle_translate(0.0, -1.4 * RADIUS, 1.4 * RADIUS);
                vogle_scale(0.3, 0.3, 0.3);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        /*
         * and maybe a few more times....
         */


        vogle_color(RED);

        vogle_pushmatrix();
                vogle_translate(0.0, RADIUS, 0.7 * RADIUS);
                vogle_scale(0.2, 0.2, 0.2);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        vogle_color(GREEN);

        vogle_pushmatrix();
                vogle_translate(0.0, 1.5 * RADIUS, -RADIUS);
                vogle_scale(0.15, 0.15, 0.15);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        vogle_color(YELLOW);

        vogle_pushmatrix();
                vogle_translate(0.0, -RADIUS, -RADIUS);
                vogle_scale(0.12, 0.12, 0.12);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        vogle_color(BLUE);

        vogle_pushmatrix();
                vogle_translate(0.0, -2.0*RADIUS, -RADIUS);
                vogle_scale(0.3, 0.3, 0.3);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        vogle_font("times.rb");
        vogle_ortho2(0.0, 1.0, 0.0, 1.0);
        vogle_centertext(1);
        vogle_textsize(0.08, 0.15);
        vogle_move2(0.8, 0.5);
        vogle_textang(-90.0);
        vogle_drawstr("I'm very ordinary!");

        vogle_getkey();

        vogle_vexit();
}
