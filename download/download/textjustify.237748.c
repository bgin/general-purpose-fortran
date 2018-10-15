#include "vogle.h"
#include <stdio.h>
seejustify(char string[], short just, float x, float y){
 float width, height;
 vogle_color(1);
 /*
 fprintf(stdout,"%s %d\n",string,just);
 */
 vogle_move2(x-1,y); vogle_draw2(x+1,y); vogle_move2( x,y-1); vogle_draw2( x,y+1);
 vogle_circle(x,y,5.0);
 vogle_textjustify((char)just);
 vogle_color(2); vogle_move2(x,y); vogle_drawstr(string);
 vogle_color(7);
 vogle_rmove2(-vogle_strlength(string),0.0);
 vogle_rdraw2(vogle_strlength(string),0.0);
 vogle_getfontsize(&width, &height);
 vogle_rmove2(0.0,height);
 vogle_rdraw2(-vogle_strlength(string),0.0);
}
main(){
char ident[] = "@(#)demonstration of textjustify";
char dev[20];
float x1=-20, x2=20, y1=-20, y2=20;
/*
 vogle_prefsize((int)(x2-x1)*15,(int)(y2-y1)*15);
 vogle_vinit("X11");
*/

 fprintf(stderr,"Enter device: ");
 fgets(dev,20,stdin);
 vogle_vinit(dev);

 vogle_ortho2(x1,x2,y1,y2);
 vogle_clear();
 vogle_getkey();
 vogle_linewidth(20); vogle_textsize(0.9*1.0, 1.4*1.0); vogle_font("times.rb");
 vogle_color(2);
 seejustify( "right|top",           V_RIGHT | V_TOP,           -10.0, -10.0 );
 seejustify( "right|ycentered",     V_RIGHT | V_YCENTERED,     -10.0,   0.0 );
 seejustify( "right|bottom",        V_RIGHT | V_BOTTOM,        -10.0, +10.0 );
 seejustify( "xcentered|top",       V_XCENTERED | V_TOP,         0.0, -10.0 );
 seejustify( "xcentered|ycentered", V_XCENTERED | V_YCENTERED,   0.0,   0.0 );
 seejustify( "xcentered|bottom",    V_XCENTERED | V_BOTTOM,      0.0, +10.0 );
 seejustify( "left|top",            V_LEFT | V_TOP,            +10.0, -10.0 );
 seejustify( "left|ycentered",      V_LEFT | V_YCENTERED,      +10.0,   0.0 );
 seejustify( "left|bottom",         V_LEFT | V_BOTTOM,         +10.0, +10.0 );
 vogle_vflush();
 vogle_getkey();
 vogle_vexit();
};
