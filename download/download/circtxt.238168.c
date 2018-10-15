
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vogle.h"
#include <math.h>

#define pi 3.1415926535

char *fonts[] = {
        "astrology",
        "cursive",
        "futura.l",
        "futura.m",
        "gothic.eng",
        "gothic.ger",
        "gothic.ita",
        "greek",
        "japanese",
        "markers",
        "math.low",
        "math.upp",
        "meteorology",
        "music",
        "cyrillic",
        "script",
        "symbolic",
        "times.g",
        "times.ib",
        "times.i",
        "times.r",
        "times.rb"
};
/*
 * ShowCircularText
 *
 *      show a ring of text
 */
void ShowCircularText(r, str)
        double  r;
        char    *str;
{
        double  i, inc, x, y;
        double  a;

        inc = 360.0 / (double)strlen(str);

        for (i = 0; i < 360.0; i += inc) {
                /*
                 * calculate the next drawing position
                 */
                x = r * cos(i * pi / 180.0);
                y = r * sin(i * pi / 180.0);
                vogle_move2(x, y);
                /*
                 * calculate angle for next character
                 */
                a = (90 + i);
                /*
                 * set the orientation of the next character
                 */
                vogle_textang(a);
                /*
                 * draw the character
                 */
                vogle_drawchar(*str++);
        }
}

/*
 *  display all the hershey fonts and demonstrate textang
 */
int main( int argc, char *argv[] ){
        char ident[] = "@(#) display all Hershey fonts using textang";

        char    buf[50];
        char    *str1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
        char    *str2 = "abcdefghijklmnopqrstuvwxyz" ;
        char    *str3 = "1234567890+-=!@#$%^&*(){}[]" ;
        char    *str4 = "<>,./?~`\\|_BONK,blark" ;
        int     i;

        fprintf(stderr,"Enter device name: ");
        gets(buf);

        vogle_vinit(buf);

        vogle_color(BLACK);
        vogle_clear();

        vogle_ortho2(-14.0, 14.0, -14.0, 14.0);       /* define the world space */

        vogle_vsetflush(0);

        for(i = 0; i < 22; i++) {

                /*
                 * textang is used to specify the orientation of text. As
                 * we want the title to come out straight we make sure it is
                 * zero each time we go through this loop.
                 */
                vogle_textang(0.0);

                /*
                 * do the title
                 */
                vogle_color(YELLOW);
                vogle_font("futura.m");
                sprintf(buf, "This is hershey font %s", fonts[i]);
                vogle_boxtext(-11.0, 12.0, 20.0, 1.0, buf);

                /*
                 * draw a box around the title
                 */
                vogle_rect(-11.0, 12.0, 9.0, 13.0);

                vogle_color(GREEN);

                vogle_font(fonts[i]);         /* grab a font from the table */

                vogle_textsize(1.5, 1.5);             /* show the outer ring */
                ShowCircularText(11.0, str1);

                vogle_textsize(1.3, 1.3);             /* show the second ring */
                ShowCircularText(8.5, str2);

                vogle_textsize(1.1, 1.1);             /* show the third ring */
                ShowCircularText(7.0, str3);

                vogle_textsize(0.9, 0.9);             /* show the inside ring */
                ShowCircularText(5.0, str4);

                if (vogle_getkey() == 'q') {
                        vogle_vexit();
                        exit(0);
                }

                vogle_color(BLACK);
                vogle_clear();
        }

        vogle_vexit();
}

