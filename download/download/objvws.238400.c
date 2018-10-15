#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) Demonstrate just how much you can put in an object ";

#define         CUBE            1
#define         TOPLEFT         2
#define         TOPRIGHT        3
#define         BOTTOMLEFT      4
#define         BOTTOMRIGHT     5

/*
 * side
 *
 *      define a face for the cube
 */
void side()
{
        vogle_pushmatrix();
                vogle_translate(0.0, 0.0, 1.0);
                vogle_rect(-1.0, -1.0, 1.0, 1.0);
        vogle_popmatrix();
}

/*
 * makecube
 *
 *      set up a cube
 */
void makecube()
{

        vogle_makeobj(CUBE);

                /*
                 * The border around the cube
                 */
                vogle_rect(-5.0, -5.0, 10.0, 10.0);

                /*
                 * Make the cube from 4 squares
                 */
                vogle_pushmatrix();
                        side();
                        vogle_rotate(90.0, 'x');
                        side();
                        vogle_rotate(90.0, 'x');
                        side();
                        vogle_rotate(90.0, 'x');
                        side();
                vogle_popmatrix();

        vogle_closeobj();
}


/*
 * Demonstrate just how much you can put in an object
 */
int main( int argc, char *argv[] ){
        char device[20];

        fprintf(stderr,"Enter device name: ");
        fgets(device,20,stdin);
        vogle_vinit(device);
        vogle_pushviewport();

        vogle_textsize(0.5, 0.9);
        vogle_font("futura.m");

        vogle_color(BLACK);
        vogle_clear();

        makecube();

        /*
         * set up an object which draws in the top left of the screen.
         */
        vogle_makeobj(TOPLEFT);
                vogle_viewport(-1.0, 0.0, 0.0, 1.0);
                vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

                vogle_color(RED);

                vogle_rect(-5.0, -5.0, 5.0, 5.0);

                vogle_perspective(40.0, 1.0, 0.1, 1000.0);
                vogle_lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0);

                vogle_callobj(CUBE);

                vogle_color(GREEN);

                vogle_move2(-4.5, -4.5);
                vogle_drawstr("perspective/lookat");
        vogle_closeobj();

        /*
         * now set up one which draws in the top right of the screen
         */
        vogle_makeobj(TOPRIGHT);
                vogle_viewport(0.0, 1.0, 0.0, 1.0);
                vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

                vogle_color(GREEN);

                vogle_rect(-5.0, -5.0, 5.0, 5.0);

                vogle_window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0);
                vogle_lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0);

                vogle_callobj(CUBE);

                vogle_color(RED);

                vogle_move2(-4.5, -4.5);
                vogle_drawstr("window/lookat");
        vogle_closeobj();

        /*
         * try the bottom left
         */
        vogle_makeobj(BOTTOMLEFT);
                vogle_viewport(-1.0, 0.0, -1.0, 0.0);
                vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

                vogle_color(MAGENTA);

                vogle_rect(-5.0, -5.0, 5.0, 5.0);

                vogle_perspective(40.0, 1.0, 0.1, 1000.0);
                vogle_polarview(15.0, 30.0, 30.0, 30.0);

                vogle_callobj(CUBE);

                vogle_color(YELLOW);

                vogle_move2(-4.5, -4.5);
                vogle_drawstr("perspective/polarview");
        vogle_closeobj();

        /*
         * and the bottom right
         */
        vogle_makeobj(BOTTOMRIGHT);
                vogle_viewport(0.0, 1.0, -1.0, 0.0);
                vogle_ortho2(-5.0, 5.0, -5.0, 5.0);

                vogle_color(CYAN);

                vogle_rect(-5.0, -5.0, 5.0, 5.0);

                vogle_window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0);
                vogle_polarview(8.0, -18.0, -3.0, 18.0);

                vogle_callobj(CUBE);

                vogle_color(BLUE);

                vogle_move2(-4.5, -4.5);
                vogle_drawstr("window/polarview");
        vogle_closeobj();

        /*
         * now draw them
         */
        vogle_callobj(TOPLEFT);
        vogle_callobj(TOPRIGHT);
        vogle_callobj(BOTTOMLEFT);
        vogle_callobj(BOTTOMRIGHT);

        vogle_getkey();

        vogle_vexit();
}
