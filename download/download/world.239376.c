#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "vogle.h"
#include <math.h>

static char ident[] = "@(#) show perspective, objects, software text";

#define RADIUS  10.0
#define SPHERE  1


/* showroundtext: draw string str wrapped around a circle in 3d */
void showroundtext(char *str){
        float   i, inc;

        inc = 360.0 / (float)strlen(str);

        for (i = 0; i < 360.0; i += inc) {
                vogle_pushmatrix();
                        /*
                         * find the spot on the edge of the sphere
                         * by making it (0, 0, 0) in world coordinates
                         */
                        vogle_rotate(i, 'y');
                        vogle_translate(0.0, 0.0, RADIUS);

                        vogle_move(0.0, 0.0, 0.0);

                        vogle_drawchar(*str++);
                vogle_popmatrix();
        }
}

/* makesphere: create the sphere object */
void makesphere(){
        float   i, r, z, a;

        vogle_makeobj(SPHERE);

                for (i = 0; i < 180; i += 20) {
                        vogle_pushmatrix();
                                vogle_rotate(i, 'y');
                                vogle_circle(0.0, 0.0, RADIUS);
                        vogle_popmatrix();
                }
                
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
 * show perspective, objects, software text
 *
 * most of the things in this program have been done before but it has
 * a certain novelty value.
 */
int main( int argc, char *argv[] ){
        char    dev[20];
        int     i;
        float   r, z, a;

        fprintf(stderr,"Enter device: ");
        fgets(dev,20,stdin);

        vogle_vinit(dev);
        vogle_vsetflush(0);
        vdevice.clipoff = 1;

        vogle_font("futura.m");

        vogle_perspective(80.0, 1.0, 0.001, 50.0);
        vogle_lookat(13.0, 13.0, 8.0, 0.0, 0.0, 0.0, 0.0);

        vogle_color(BLACK);
        vogle_clear();

        makesphere();

        /* draw the main one in cyan */
        vogle_color(CYAN);

        vogle_callobj(SPHERE);

        /* draw a smaller one outside the main one in white */
        vogle_color(WHITE);

        vogle_pushmatrix();
                vogle_translate(0.0, -1.4 * RADIUS, 1.4 * RADIUS);
                vogle_scale(0.3, 0.3, 0.3);
                vogle_callobj(SPHERE);
        vogle_popmatrix();

        /* scale the text */
        vogle_boxfit(2.0 * PI * RADIUS, 0.25 * RADIUS, 31);

        /* now write the text in rings around the main sphere */
        vogle_color(GREEN);
        showroundtext("Around the world in eighty days ");

        vogle_color(BLUE);
        /*
         * note: that software text is rotated here as
         * anything else would be whether you use textang
         * or rotate depends on what you are trying to do.
         * Experience is the best teacher here.
         */
        vogle_rotate(90.0, 'x');
        showroundtext("Around the world in eighty days ");

        vogle_color(RED);
        vogle_rotate(90.0, 'z');
        showroundtext("Around the world in eighty days ");

        vogle_getkey();

        vogle_vexit();
}
