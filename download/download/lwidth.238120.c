#include <stdio.h>
#include <math.h>
#include "vogle.h"

static char ident[] = "@(#) line thickness tests ";

static void tcircle(x, y) /* Draw a circle on thick line segment end point */
int x, y;
{
        /* there are more efficient ways to do this */
        /* circle precision */
        const int nsegs= 15;
        float cx, cy, dx, dy, angle, cosine, sine ;
        /* array to place circle points on */
        float cxras[nsegs][2];
        int i;

        angle = 2.0 * PI / nsegs;
        cosine = cos((double)angle);
        sine = sin((double)angle);

        /* first point on circle */
        cxras[0][0] = cx =  x + 15/2.0;
        cxras[0][1] = cy = y;
        for (i = 1; i < nsegs; i++) {
                dx = cx - x; 
                dy = cy - y;
                cxras[i][0] = ( cx = x + dx * cosine - dy * sine) ;
                cxras[i][1] = ( cy = y + dx * sine   + dy * cosine) ;
        }
        vogle_poly2(nsegs,cxras);
}
void lines(step,radius)
float step, radius;
{
        float x,y, angle, rads;
        angle=0.0;
        vogle_color(BLACK);   /* clear to black */
        vogle_clear();
        vogle_color(WHITE);
        for (angle=0.0; angle < 360.0; angle += step ){
                vogle_move2(0.0,0.0);
                rads=angle/180.0*3.1415926535897932384;
                x=radius*cos(rads);
                y=radius*sin(rads);
                vogle_draw2(x,y);
        }

        tcircle(0,0);
        vogle_getkey();

}

int main( int argc, char *argv[] ){ /* line thickness tests */
        char device[10];

        fprintf(stderr,"Enter output device: ");
        fgets(device,20,stdin);

        vogle_vinit(device);


/*
 * world coordinates are now in the range -10 to 10
 * in x, y, and z. Note that positive z is towards us.
 */
        vogle_ortho(-10.0, 10.0, -10.0, 10.0, 10.0, -10.0);

        lines(5.0,10.0);
        vogle_linewidth(30);
        lines(5.0,10.0);
        vogle_linewidth(100);
        lines(5.0,10.0);
        vogle_ortho(-20.0, 20.0, -20.0, 20.0, 20.0, -20.0);
        vogle_linewidth(100);
        lines(5.0,10.0);

        vogle_linewidth(200);
        vogle_color(BLACK);   /* clear to black */
        vogle_clear();
        vogle_color(WHITE);
        vogle_move2(-15.0,-15.0);
        vogle_draw2(-10.0, 15.0);
        vogle_draw2( -5.0,-10.0);
        vogle_draw2(  0.0, 10.0);
        vogle_draw2( 10.0,-15.0);
        vogle_draw2( 10.0, 15.0);
        vogle_draw2( 15.0,-15.0);
        vogle_getkey();

        vogle_vexit();
}
