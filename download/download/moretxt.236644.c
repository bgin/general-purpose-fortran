#include <stdlib.h>
#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) draw a grid in the middle of the screen ";

void drawgrid(){
        float   x;
        int     i;

        vogle_color(GREEN);

        vogle_rect(0.1, 0.4, 0.9, 0.6);

        x = 0.2;
        for (i = 0; i < 8; i++) {
                vogle_move2(x, 0.4);
                vogle_draw2(x, 0.6);
                x += 0.1;
        }
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);

        vogle_color(YELLOW);
}

/*
 * demonstrate some more features of text
 */
int main( int argc, char *argv[] ){
        char    dev[20];
        int     i;
        float   x;
        
        fprintf(stderr, "Enter device: ");
        fgets(dev,20,stdin); 
        vogle_vinit(dev);

        vogle_color(BLACK);
        vogle_clear();

        if (argc == 2)
                vogle_font(argv[1]);

        vogle_ortho2(0.0, 1.0, 0.0, 1.0);

        drawgrid();

        /*
         * show some scaled text on the grid (In the bottom part)
         */
        vogle_boxtext(0.1, 0.4, 0.8, 0.1, "{This is Some text] | $");

        vogle_getkey();

        vogle_color(BLACK);
        vogle_clear();

        drawgrid();

        /*
         * centertext causes text to be centered around the current graphics
         * position this is especially useful if you want your text to come
         * out centered on a line, or a character to be centered on a point
         * in a graph. A non-zero argument turns centertext on.
         *
         * show a string centered on the center line
         */
        vogle_centertext(1);

        vogle_boxtext(0.5, 0.5, 0.8, 0.1, "{This is Some Centered text] | $");

        /*
         * turn centertext off. We use an argument with the value zero.
         */
        vogle_centertext(0);

        vogle_getkey();

        vogle_color(BLACK);
        vogle_clear();

        /*
         * rotate the grid so that it is the same angle as the text after
         * textang for text ang.
         */
        vogle_pushmatrix();
                vogle_translate(0.5, 0.5, 0.0);
                vogle_rotate(90.0, 'z');
                vogle_translate(-0.5, -0.5, 0.0);

                drawgrid();
        vogle_popmatrix();

        /*
         * turn on centered text again
         */
        vogle_centertext(1);

        /*
         * set the angle to 90.
         */
        vogle_textang(90.0);

        /*
         * draw the string
         */
        vogle_boxtext(0.5, 0.5, 0.8, 0.1, "{This is Some Rotated Centered text] | $");

        /*
         * turn off center text
         */
        vogle_centertext(0);

        /*
         * set text angle back to 90
         */
        vogle_textang(0.0);

        vogle_getkey();

        vogle_color(BLACK);
        vogle_clear();

        drawgrid();

        /*
         * as all the software fonts are proportionally spaced we use
         * the fixedwidth call to make each character take the same amount
         * of horizontal space. As with centertext this is done by passing
         * fixedwidth a non-zero argument.
         */
        vogle_fixedwidth(1);

        vogle_boxtext(0.1, 0.5, 0.8, 0.1, "{This is Some Fixedwidth text] | $");

        vogle_getkey();

        vogle_color(BLACK);
        vogle_clear();

        drawgrid();

        /*
         * now try centered and fixewidth at the same time
         */
        vogle_centertext(1);

        vogle_move2(0.5, 0.5);
        vogle_drawstr("{This is Some Cent.Fixedwidth text] | $");

        vogle_centertext(0);
        
        vogle_getkey();
        vogle_color(BLACK);
        vogle_clear();

        drawgrid();

        /*
         * scale the text so tha a character is the size of a box in
         * the grid.
         */
        vogle_boxfit(0.8, 0.1, 8);

        /*
         * draw the two strings fixedwidth (it is still turned on)
         */
        vogle_move2(0.1, 0.4);
        vogle_drawstr("ABCDefgh");

        vogle_move2(0.1, 0.5);
        vogle_drawstr("IJKLmnop");

        vogle_getkey();

        vogle_vexit();
}
