#include <stdio.h>
#include <stdlib.h>
#include "vogle.h"

static char ident[] = "@(#)a routine to demostrate using locator ";

int main( int argc, char *argv[] ){
        char    dev[50], name[50];
        int     i, bt, curpnt, act, nchars;
        float   x, y, sx, sy;

        fprintf(stderr,"Enter device name: ");
        fgets(dev,20,stdin);

        vogle_prefsize(512, 512);
        vogle_vinit(dev);

        vogle_color(BLACK);
        vogle_clear();

        vogle_color(BLUE);

        /*
         * draw some axes
         */
        vogle_move2(0.0, 1.0);
        vogle_draw2(0.0, -1.0);

        vogle_move2(1.0, 0.0);
        vogle_draw2(-1.0, 0.0);

        vogle_color(GREEN);

        act = 0;
        curpnt = 0;
        /*
         * locator returns whether a mouse button has been
         * pressed or not. In a device such as the tektronix
         * where you have to wait for a keypress to get the
         * position of the crosshairs locator returns 0
         * automatically on every second call. A return value
         * of 2 indicates the second mouse button has been pressed.
         * A return value of 1 indicates the first mouse button has
         * been pressed. We wait for the locator to return zero so
         * that we know the mouse button has been released.
         */
        while((bt = vogle_locator(&x, &y)) != 2) {
                if (bt == -1) {
                        vogle_vexit();
                        printf("No locator device found\n");
                        exit(0);
                } else if (bt == 0) {
                        act = 1;
                } else if (act) {
                        act = 0;
                        if (bt == 1) {
                                if (curpnt) {
                                        vogle_move2(sx, sy);
                                        vogle_draw2(x, y);
                                        curpnt = 0;
                                } else
                                        curpnt = 1;
                                sx = x;
                                sy = y;
                        }
                }
        }

        vogle_vexit();

}
