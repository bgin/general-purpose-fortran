#include <stdio.h>
#include "vogle.h"

static char ident[] = "@(#) A program showing basic line styles.";

extern double   sin(), cos();

void drawbox(s)
        float   s;
{
        vogle_pushmatrix();

        vogle_rotate(30.0, 'x');
        vogle_rotate(60.0, 'y');
        vogle_translate(-0.7, -1.2, 0.0);
        vogle_scale(s, s, s);

        vogle_move(0.0, 0.0, 0.0);

        vogle_draw(1.0, 0.0, 0.0);
        vogle_draw(1.0, 1.0, 0.0);
        vogle_draw(0.0, 1.0, 0.0);
        vogle_draw(0.0, 0.0, 0.0);

        vogle_draw(0.0, 0.0, -1.0);
        vogle_draw(1.0, 0.0, -1.0);
        vogle_draw(1.0, 1.0, -1.0);
        vogle_draw(0.0, 1.0, -1.0);
        vogle_draw(0.0, 0.0, -1.0);

        vogle_move(0.0, 1.0, -1.0);
        vogle_draw(0.0, 1.0, 0.0);

        vogle_move(1.0, 1.0, 0.0);
        vogle_draw(1.0, 1.0, -1.0);

        vogle_move(1.0, 0.0, 0.0);
        vogle_draw(1.0, 0.0, -1.0);

        vogle_popmatrix();
}

#define RAD 0.5
#define AMP 0.03

void drawsine(s)
        float   s;
{
        float   a, x, y, z;
        int     i;

        vogle_pushmatrix();

        vogle_translate(RAD + 0.2, -0.5, 0.0);
        vogle_scale(s, s, s);

        vogle_move(RAD, 0.0, 0.0);
        for (a = 0.0; a <= 2 * 3.1415926; a += 0.02) {
                x = RAD * cos(a);
                z = RAD * sin(a);
                y = AMP * sin(a * 6.0);

                vogle_draw(x, y, z);
        }

        vogle_popmatrix();
}


void drawscene()
{
        vogle_color(BLACK);           /* set current color */
        vogle_clear();                /* clear screen to current color */

        vogle_color(GREEN);
        vogle_dashcode(0.03);
                        /* 2 d move to start where we want drawstr to start */

        vogle_xcentertext();
        vogle_move2(-0.45, 0.9);
        vogle_drawstr("Linestyle: \"10\"");
        vogle_move2(-0.45, 0.7);
        vogle_drawstr("Linestyle: \"110\"");
        vogle_move2(-0.45, 0.5);
        vogle_drawstr("Linestyle: \"111010\"");
        vogle_move2(-0.45, 0.3);
        vogle_drawstr("Linestyle: \"0001\"");

        vogle_linestyle("10");
        vogle_move2(-0.9, 0.9);
        vogle_draw2( 0.0, 0.9);
        vogle_circle(0.6, 0.6, 0.4);  /* Should be pi * 0.2 /0.03 dashes ~= 21 */

        drawbox(0.9);
        drawsine(0.9);

        vogle_color(RED);
        vogle_linestyle("110");
        vogle_move2(-0.9, 0.7);
        vogle_draw2( 0.0, 0.7);
        vogle_circle(0.6, 0.6, 0.3);
        drawbox(0.7);
        drawsine(0.7);

        vogle_color(CYAN);
        vogle_linestyle("111010");
        vogle_move2(-0.9, 0.5);
        vogle_draw2( 0.0, 0.5);
        vogle_circle(0.6, 0.6, 0.2);
        drawbox(0.5);
        drawsine(0.5);

        vogle_color(YELLOW);
        vogle_linestyle("0001");
        vogle_move2(-0.9, 0.3);
        vogle_draw2( 0.0, 0.3);
        vogle_circle(0.6, 0.6, 0.1);
        drawbox(0.3);
        drawsine(0.3);

        vogle_getkey();               /* wait for some input */

}
/*
 * A program showing basic line styles.
 */
int main( int argc, char *argv[] ){
        char    device[20];

        fprintf(stderr,"Enter output device: ");
        fgets(device,20,stdin);

        vogle_vinit(device);          /* set up device */
        vogle_vsetflush(0);
        vogle_linewidth(V_THICK);
        vogle_up(0.0, 1.0, 0.0);
        vogle_perspective(90.0, 1.0, 0.3, 3.0);
        vogle_translate(0.0, 0.0, -1.3);

        drawscene();
        vogle_rotate(-30.0, 'y');
        vogle_rotate(-30.0, 'x');
        vogle_linestyle("");
        drawscene();

        vogle_vexit();                /* set the screen back to its original state */
}
