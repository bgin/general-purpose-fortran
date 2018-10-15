#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) demostrate still more features of text";

void drawstuff()
{
        vogle_color(BLACK);
        vogle_polyfill(1);    /* So rect clears a bit for us */
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_color(GREEN);
        vogle_move2(0.5, 0.5);
        vogle_leftjustify();
        vogle_drawstr("This is Left Justified text");

        vogle_getkey();

        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_color(YELLOW);
        vogle_move2(0.5, 0.5);
        vogle_centertext(1);
        vogle_drawstr("This is Centered text");
        vogle_centertext(0);

        vogle_getkey();

        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_color(MAGENTA);
        vogle_move2(0.5, 0.5);
        vogle_rightjustify();
        vogle_drawstr("This is Right Justified text");
        vogle_textjustify(0);

        vogle_getkey();
}

void drawstuff2(ang)
        float   ang;
{
        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_textang(ang);

        vogle_color(GREEN);
        vogle_move2(0.5, 0.5);
        vogle_leftjustify();
        vogle_drawchar('B');

        vogle_textang(0.0);
        vogle_textjustify(0);
        vogle_boxtext(0.1, 0.1, 0.4, 0.02, "The 'B' should be leftjustified");
        
        vogle_getkey();

        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_textang(ang);
        vogle_color(YELLOW);
        vogle_move2(0.5, 0.5);
        vogle_centertext(1);
        vogle_drawchar('B');
        vogle_centertext(0);

        vogle_textang(0.0);
        vogle_textjustify(0);
        vogle_boxtext(0.1, 0.1, 0.4, 0.02, "The 'B' should be centered");

        vogle_getkey();

        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_textang(ang);
        vogle_color(MAGENTA);
        vogle_move2(0.5, 0.5);
        vogle_rightjustify();
        vogle_drawchar('B');

        vogle_textang(0.0);
        vogle_textjustify(0);
        vogle_boxtext(0.1, 0.1, 0.4, 0.02, "The 'B' should be rightjustified");

        vogle_getkey();

        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_textang(ang);
        vogle_color(MAGENTA);
        vogle_move2(0.5, 0.5);
        vogle_topjustify();
        vogle_drawchar('B');

        vogle_textang(0.0);
        vogle_textjustify(0);
        vogle_boxtext(0.1, 0.1, 0.4, 0.02, "The 'B' should be topjustified");

        vogle_getkey();

        vogle_color(BLACK);
        vogle_rect(0.1, 0.1, 0.9, 0.9);
        vogle_color(WHITE);
        vogle_move2(0.1, 0.5);
        vogle_draw2(0.9, 0.5);
        vogle_move2(0.5, 0.1);
        vogle_draw2(0.5, 0.9);

        vogle_textang(ang);
        vogle_color(MAGENTA);
        vogle_move2(0.5, 0.5);
        vogle_topjustify();
        vogle_rightjustify();
        vogle_drawchar('B');

        vogle_textang(0.0);
        vogle_textjustify(0);
        vogle_boxtext(0.1, 0.1, 0.4, 0.02, "The 'B' should be right/topjustified");

        vogle_textjustify(0);

        vogle_getkey();

}

/*
 * demonstrate still more features of text
 */
int main( int argc, char *argv[] ){
        char    dev[20];

        printf("Enter device: ");
        fgets(dev,20,stdin);
        vogle_vinit(dev);

        if (argc == 2)
                vogle_font(argv[1]);
        else
                vogle_font("futura.l");

        vogle_textsize(0.03, 0.04);

        vogle_ortho2(0.0, 1.0, 0.0, 1.0);

        vogle_color(RED);
        vogle_clear();

        drawstuff();

        /* Now do it all with the text rotated .... */

        vogle_textang(45.0);
        drawstuff();

        vogle_textang(160.0);
        drawstuff();

        vogle_textang(270.0);
        drawstuff();

        /* Now with a single character */

        vogle_textjustify(0);

        drawstuff2(0.0);

        drawstuff2(90.0);

        drawstuff2(160.0);

        drawstuff2(270.0);

        vogle_vexit();
}

