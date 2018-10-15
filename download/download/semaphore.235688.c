#include <stdio.h>
#include <string.h>
#include <math.h>
#include "vogle.h"
extern void vpage(float xsmall,float xlarge,float ysmall,float ylarge);

void semaphores(int sema[26]){
   /* # semaphores as objects */
#define WHITE 0
#define BLUE 6
#define BLACK 7
#define RED 1
#define GREEN 2
#define YELLOW 3
#define OUTLINE BLACK
#define BACKGROUND GREEN
#define TEXT BLACK

#define TRUE 1
#define FALSE 0
#define Box() vogle_color(OUTLINE);vogle_move2(0.0,0.0);vogle_draw2(0.0,1.0);vogle_draw2(1.0,1.0);vogle_draw2(1.0,0.0);vogle_draw2(0.0,0.0);
/* ################################################################################ */
   float W;
   float CORNER;
   float BOXX=0.0;
   float BOXY=0.0;
   int ii;
   /* ################################################################################ */
#define    ALPHA      (sema[ 0])
#define    BRAVO      (sema[ 1])
#define    CHARLIE    (sema[ 2])
#define    DELTA      (sema[ 3])
#define    ECHO       (sema[ 4])
#define    FOXTROT    (sema[ 5])
#define    GOLF       (sema[ 6])
#define    HOTEL      (sema[ 7])
#define    INDIA      (sema[ 8])
#define    JULIET     (sema[ 9])
#define    KILO       (sema[10])
#define    LIMA       (sema[11])
#define    MIKE       (sema[12])
#define    NOVEMBER   (sema[13])
#define    OSCAR      (sema[14])
#define    PAPA       (sema[15])
#define    QUEBEC     (sema[16])
#define    ROMEO      (sema[17])
#define    SIERRA     (sema[18])
#define    TANGO      (sema[19])
#define    UNIFORM    (sema[20])
#define    VICTOR     (sema[21])
#define    WHISKEY    (sema[22])
#define    XRAY       (sema[23])
#define    YANKEE     (sema[24])
#define    ZULU       (sema[25])
#define XX   for (ii=0;ii<26;ii++){ fprintf(stdout," %d  %d \n",ii,sema[ii]); }


   /* ################################################################################ */
   for (ii=0;ii<26;ii++){
   sema[ii]=-9999;
   }
   vogle_polyfill(TRUE);
   vogle_color(-1); /* line thickness */
   /* ################################################################################ */
   ALPHA=vogle_genobj();
   vogle_makeobj(ALPHA);
      vogle_color(WHITE);
      vogle_makepoly();
         vogle_color(WHITE);
         vogle_rect(0.0,0,00.5,1.0);
      vogle_closepoly();
      vogle_color(BLUE);
      vogle_makepoly();
         vogle_move2( 00.5,  0.0);
         vogle_draw2( 00.5,  1.0);
         vogle_draw2( 1.0,  1.0);
         vogle_draw2( 1.0-.25, 00.5);
         vogle_draw2( 1.0,  0.0);
         vogle_draw2( 00.5,   0.0);
      vogle_closepoly();
      vogle_color( OUTLINE);
      vogle_move2( 0.0,  1.0);
      vogle_draw2( 1.0,  1.0);
      vogle_draw2( 1-.25, 0.5);
      vogle_draw2( 1.0,  0.0);
      vogle_draw2( 0.0,  0.0);
      vogle_draw2( 0.0,  1.0);
   vogle_closeobj();
   /* ################################################################################ */
   BRAVO=vogle_genobj();
   vogle_makeobj(BRAVO);
      vogle_color(RED);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_draw2( 0.0, 1.0);
         vogle_draw2( 1.0, 1.0);
         vogle_draw2( 1-.25, 0.5);
         vogle_draw2( 1.0, 0.0);
         vogle_draw2( 0.0, 0.0);
      vogle_closepoly();
      vogle_color( OUTLINE);
      vogle_move2( 0.0, 0.0);
      vogle_draw2( 0.0, 1.0);
      vogle_draw2( 1.0, 1.0);
      vogle_draw2( 1-.25, 0.5);
      vogle_draw2( 1.0, 0.0);
      vogle_draw2( 0.0, 0.0);
   vogle_closeobj();
   /* ################################################################################ */
   CHARLIE=vogle_genobj();
   vogle_makeobj(CHARLIE);
      vogle_color(BLUE);
      vogle_makepoly();
         vogle_rect(0.0, 0.0, 1.0, 0.25);
      vogle_closepoly();
      vogle_makepoly();
         vogle_rect(0.0, 0.75, 1.0, 1.0);
      vogle_closepoly();
      vogle_color( WHITE);
      vogle_makepoly();
         vogle_rect(0.0, 0.25, 1.0, 0.25+0.50/3.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_rect(0.0, 0.25+2*0.50/3.0, 1.0, 0.75);
      vogle_closepoly();
      vogle_color( RED);
      vogle_makepoly();
         vogle_rect(0.0, 0.25+0.50/3.0, 1.0, 0.25+2*.50/3.0);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   DELTA=vogle_genobj();
   vogle_makeobj(DELTA);
      vogle_color(YELLOW);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  1.0, 0.25);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2(0.0, 0.75);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX,BOXY, 1.0, 1.0);
      vogle_closepoly();
      vogle_color(BLUE);
      vogle_makepoly();
         vogle_move2(0.0, 0.25);
         vogle_getgp2( &BOXX, &BOXY);
         vogle_rect( BOXX, BOXY, 1.0, 0.75);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## ECHO; */
   ECHO=vogle_genobj();
   vogle_makeobj(ECHO);
      vogle_color( RED);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  1.0, 0.50);
      vogle_closepoly();
      vogle_color( BLUE);
      vogle_makepoly();
         vogle_move2( 0.0, 00.5);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  1.0, 1.0);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## FOXTROT; */
   FOXTROT=vogle_genobj();
   vogle_makeobj(FOXTROT);
      vogle_color( WHITE);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_draw2( 0.0, 00.5);
         vogle_draw2( 00.5, 0.0);
         vogle_draw2( 0.0, 0.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 1.0, 0.0);
         vogle_draw2( 00.5, 0.0);
         vogle_draw2( 1.0, 00.5);
         vogle_draw2( 1.0, 0.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 1.0, 1.0);
         vogle_draw2( 1.0, 00.5);
         vogle_draw2( 00.5, 1.0);
         vogle_draw2( 1.0, 1.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 0.0, 1.0);
         vogle_draw2( 00.5, 1.0);
         vogle_draw2( 0.0, 00.5);
         vogle_draw2( 0.0, 1.0);
      vogle_closepoly();
      vogle_color( RED);
      vogle_makepoly();
         vogle_move2( 00.5, 0.0);
         vogle_draw2( 1.0, 00.5);
         vogle_draw2( 00.5, 1.0);
         vogle_draw2( 0.0, 00.5);
         vogle_draw2( 00.5, 0.0);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## GOLF; */
   GOLF=vogle_genobj();
      vogle_makeobj(GOLF);
      vogle_color( YELLOW);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  1.0/6, 1.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 2.0/6, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  3.0/6, 1.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 4.0/6, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  5.0/6, 1.0);
      vogle_closepoly();
      vogle_color( BLUE);
      vogle_makepoly();
         vogle_move2( 1.0/6, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  2.0/6, 1.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 3.0/6, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  4.0/6, 1.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 5.0/6, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  6.0/6, 1.0);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## HOTEL; */
   HOTEL=vogle_genobj();
   vogle_makeobj(HOTEL);
      vogle_color( WHITE);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  0.5, 1.0);
      vogle_closepoly();
      vogle_color( RED);
      vogle_makepoly();
         vogle_move2( 0.5, 0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  1.0, 1.0);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## INDIA; */
   INDIA=vogle_genobj();
   vogle_makeobj(INDIA);
      vogle_color( YELLOW);
      vogle_makepoly();
         vogle_rect( 0.0, 0.0,  1.0, 1.0);
      vogle_closepoly();
      vogle_color( RED);
      vogle_makepoly();
         vogle_circle( 0.5, 0.5, 0.25/2.0);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## JULIET; */
   JULIET=vogle_genobj();
   vogle_makeobj(JULIET);
   vogle_color( BLUE);
   vogle_makepoly();
   vogle_move2( 0.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 1.0/3);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 2.0/3);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 1.0);
   vogle_closepoly();
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_move2( 0.0, 1.0/3);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 2.0/3);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## KILO; */
   KILO=vogle_genobj();
   vogle_makeobj(KILO);
   vogle_color( YELLOW);
   vogle_makepoly();
   vogle_rect( 0.0, 0,  0.5, 1.0);
   vogle_closepoly();
   vogle_color( BLUE);
   vogle_makepoly();
   vogle_rect( 0.5, 0.0, 1.0, 1.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## LIMA; */
   LIMA=vogle_genobj();
   vogle_makeobj(LIMA);
   vogle_color( BLACK);
   vogle_makepoly();
   vogle_move2( 0.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  00.5, 0.5);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 00.5, 0.5);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 1.0);
   vogle_closepoly();
   vogle_color( YELLOW);
   vogle_makepoly();
   vogle_move2( 1.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  00.5, 0.5);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  00.5, 0.5);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## MIKE; */
   W=1.0/6.0;
   CORNER=(W)*(1.0/sqrt(2.0));
   MIKE=vogle_genobj();
   vogle_makeobj(MIKE);
      vogle_color( WHITE);
      vogle_makepoly();
         vogle_move2( 0.0, 0.0);
         vogle_draw2( CORNER, 0.0);
         vogle_draw2( 00.5, 0.5-CORNER);
         vogle_draw2( 1-CORNER, 0.0);
         vogle_draw2( 1.0, 0.0);
         vogle_draw2( 1.0, CORNER);
         vogle_draw2( 00.5+CORNER, 0.5);
         vogle_draw2( 1.0, 1-CORNER);
         vogle_draw2( 1.0, 1.0);
         vogle_draw2( 1-CORNER, 1.0);
         vogle_draw2( 00.5, 0.5+CORNER);
         vogle_draw2( 0+CORNER, 1.0);
         vogle_draw2( 0.0, 1.0);
         vogle_draw2( 0.0, 1-CORNER);
         vogle_draw2( 00.5-CORNER, 0.5);
         vogle_draw2( 0.0, CORNER);
         vogle_draw2( 0.0,0.0);
      vogle_closepoly();
      vogle_color( BLUE);
      vogle_makepoly();
         vogle_move2( 0.0, CORNER);
         vogle_draw2( 00.5-CORNER, 0.5);
         vogle_draw2( 0.0, 1-CORNER);
         vogle_draw2( 0.0, CORNER);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( CORNER, 0.0);
         vogle_draw2( 1-CORNER, 0.0);
         vogle_draw2( 00.5, 0.5-CORNER);
         vogle_draw2( CORNER, 0.0);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 1.0, CORNER);
         vogle_draw2( 00.5+CORNER, 0.5);
         vogle_draw2( 1.0, 1-CORNER);
         vogle_draw2( 1.0, CORNER);
      vogle_closepoly();
      vogle_makepoly();
         vogle_move2( 00.5, 0.5+CORNER);
         vogle_draw2( CORNER, 1.0);
         vogle_draw2( 1-CORNER, 1.0);
         vogle_draw2( 00.5, 0.5+CORNER);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## NOVEMBER; */
   NOVEMBER=vogle_genobj();
   vogle_makeobj(NOVEMBER);
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_move2( 0.0/4, 0.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0/4, 1.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0/4, 2.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0/4, 3.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0/4, 1.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  2.0/4, 2.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0/4, 3.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  2.0/4, 4.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 2.0/4, 0.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  3.0/4, 1.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 2.0/4, 2.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  3.0/4, 3.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 3.0/4, 1.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  4.0/4, 2.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 3.0/4, 3.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  4.0/4, 4.0/4);
   vogle_closepoly();
   vogle_color( BLUE);
   vogle_makepoly();
   vogle_move2( 0.0/4, 1.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0/4, 2.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0/4, 3.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0/4, 4.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0/4, 0.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  2.0/4, 1.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0/4, 2.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  2.0/4, 3.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 2.0/4, 1.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  3.0/4, 2.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 2.0/4, 3.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  3.0/4, 4.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 3.0/4, 0.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  4.0/4, 1.0/4);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 3.0/4, 2.0/4);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  4.0/4, 3.0/4);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## OSCAR; */
   OSCAR=vogle_genobj();
   vogle_makeobj(OSCAR);
   vogle_color( YELLOW);
   vogle_makepoly();
   vogle_move2( 0.0, 0.0);
   vogle_draw2( 1.0, 0.0);
   vogle_draw2( 0.0, 1.0);
   vogle_draw2( 0.0, 0.0);
   vogle_closepoly();
   vogle_color( RED);
   vogle_makepoly();
   vogle_move2( 1.0, 0.0);
   vogle_draw2( 1.0, 1.0);
   vogle_draw2( 0.0, 1.0);
   vogle_draw2( 1.0, 0.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## PAPA; */
   vogle_move2( 0.0, 0.0);
   PAPA=vogle_genobj();
   vogle_makeobj(PAPA);
   vogle_color(  BLUE);
   vogle_makepoly();
   vogle_rect( 0.0, 0.0,  1.0, 1.0);
   vogle_closepoly();
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_rect( 1.0/3, 1.0/3,  2.0/3, 2.0/3);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## QUEBEC; */
   QUEBEC=vogle_genobj();
   vogle_makeobj(QUEBEC);
   vogle_color( YELLOW);
   vogle_makepoly();
   vogle_move2( 0.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 1.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## ROMEO; */
   W=1.0/12;
   ROMEO=vogle_genobj();
   vogle_makeobj(ROMEO);
   vogle_color( RED);
   vogle_makepoly();
   vogle_move2( 0.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5-W, 0.5-W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5+W, 0.5-W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5-W, 0.5+W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0, 1.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5+W, 0.5+W);
   vogle_closepoly();
   vogle_color( YELLOW);
   vogle_makepoly();
   vogle_move2( 00.5-W, 0.0);
   vogle_draw2( 00.5+W, 0.0);
   vogle_draw2( 00.5+W, 0.5-W);
   vogle_draw2( 1.0,   00.5-W);
   vogle_draw2( 1.0,   00.5+W);
   vogle_draw2( 00.5+W, 0.5+W);
   vogle_draw2( 00.5+W, 1.0);
   vogle_draw2( 00.5-W, 1.0);
   vogle_draw2( 00.5-W, 0.5+W);
   vogle_draw2( 0.0,   00.5+W);
   vogle_draw2( 0.0,   00.5-W);
   vogle_draw2( 00.5-W, 0.5-W);
   vogle_draw2( 00.5-W, 0.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## SIERRA; */
   W=3.0/8.0;
   vogle_move2( 0.0,0.0);
   SIERRA=vogle_genobj();
   vogle_makeobj(SIERRA);
      vogle_color(WHITE);
      vogle_makepoly();
         vogle_move2(0.0,0.0);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect(BOXX, BOXY, 1.0, 1.0);
      vogle_closepoly();
      vogle_color( BLUE);
      vogle_makepoly();
         vogle_move2( W, W);
         vogle_getgp2(&BOXX, &BOXY);
         vogle_rect( BOXX, BOXY,  1.0-W, 1.0-W);
      vogle_closepoly();
      Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## TANGO; */
   vogle_move2( 0.0,0.0);
   TANGO=vogle_genobj();
   vogle_makeobj(TANGO);
   vogle_color( RED);
   vogle_makepoly();
   vogle_move2( 0.0/3,0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0/3, 1.0);
   vogle_closepoly();
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_move2( 1.0/3,0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  2.0/3, 1.0);
   vogle_closepoly();
   vogle_color( BLUE);
   vogle_makepoly();
   vogle_move2( 2.0/3,0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  3.0/3, 1.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## UNIFORM; */
   UNIFORM=vogle_genobj();
   vogle_makeobj(UNIFORM);
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_move2( 0.0,0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5, 0.5);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.5, 0.5);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 1.0);
   vogle_closepoly();
   vogle_color( RED);
   vogle_makepoly();
   vogle_move2( 0.5, 0.5);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.0, 1.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.5, 0.5);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  1.0, 0.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## VICTOR; */
   W=1.0/6;
   CORNER=(W)*(1.0/sqrt(2.0));
   vogle_move2( 0.0,0.0);
   VICTOR=vogle_genobj();
   vogle_makeobj(VICTOR);
   vogle_color( RED);
   vogle_makepoly();
   vogle_move2( 0.0,0.0);
   vogle_draw2( CORNER, 0.0);
   vogle_draw2( 0.5, 0.5-CORNER);
   vogle_draw2( 1-CORNER, 0.0);
   vogle_draw2( 1.0,0.0);
   vogle_draw2( 1.0, CORNER);
   vogle_draw2( 0.5+CORNER, 0.5);
   vogle_draw2( 1.0, 1-CORNER);
   vogle_draw2( 1.0, 1.0);
   vogle_draw2( 1-CORNER, 1.0);
   vogle_draw2( 0.5, 0.5+CORNER);
   vogle_draw2( 0+CORNER, 1.0);
   vogle_draw2( 0.0, 1.0);
   vogle_draw2( 0.0, 1-CORNER);
   vogle_draw2( 0.5-CORNER, 0.5);
   vogle_draw2( 0.0, CORNER);
   vogle_draw2( 0.0,0.0);
   vogle_closepoly();
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_move2( 0.0, CORNER);
   vogle_draw2( 0.5-CORNER, 0.5);
   vogle_draw2( 0.0, 1-CORNER);
   vogle_draw2( 0.0, CORNER);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( CORNER, 0.0);
   vogle_draw2( 1-CORNER, 0.0);
   vogle_draw2( 0.5, 0.5-CORNER);
   vogle_draw2( CORNER, 0.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0, CORNER);
   vogle_draw2( 0.5+CORNER, 0.5);
   vogle_draw2( 1.0, 1-CORNER);
   vogle_draw2( 1.0, CORNER);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.5, 0.5+CORNER);
   vogle_draw2( CORNER, 1.0);
   vogle_draw2( 1-CORNER, 1.0);
   vogle_draw2( 0.5, 0.5+CORNER);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## WHISKEY; */
   WHISKEY=vogle_genobj();
   vogle_makeobj(WHISKEY);
   vogle_color( BLUE);
   vogle_makepoly();
   vogle_rect( 0.0, 0,  1.0, 1.0);
   vogle_closepoly();
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_rect( 1.0/6, 1.0/6,  5.0/6, 5.0/6);
   vogle_closepoly();
   vogle_color( RED);
   vogle_makepoly();
   vogle_rect( 2.0/6, 2.0/6,  4.0/6, 4.0/6);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## XRAY; */
   W=1.0/10;
   XRAY=vogle_genobj();
   vogle_makeobj(XRAY);
   vogle_color( WHITE);
   vogle_makepoly();
   vogle_move2( 0.0,0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5-W, 0.5-W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0, 0.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5+W, 0.5-W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5-W, 0.5+W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1.0, 1.0);
   vogle_getgp2(&BOXX, &BOXY);
   vogle_rect( BOXX, BOXY,  0.5+W, 0.5+W);
   vogle_closepoly();
   vogle_color( BLUE);
   vogle_makepoly();
   vogle_move2( 0.5-W,0.0);
   vogle_draw2( 0.5+W,0.0);
   vogle_draw2( 0.5+W, 0.5-W);
   vogle_draw2( 1.0, 0.5-W);
   vogle_draw2( 1.0, 0.5+W);
   vogle_draw2( 0.5+W, 0.5+W);
   vogle_draw2( 0.5+W, 1.0);
   vogle_draw2( 0.5-W, 1.0);
   vogle_draw2( 0.5-W, 0.5+W);
   vogle_draw2( 0.0, 0.5+W);
   vogle_draw2( 0.0, 0.5-W);
   vogle_draw2( 0.5-W, 0.5-W);
   vogle_draw2( 0.5-W, 0.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## YANKEE; */
   W=1.0/5;
   YANKEE=vogle_genobj();
   vogle_makeobj(YANKEE);
   vogle_color( RED);
   vogle_makepoly();
   vogle_move2( 1-5*W, 0.0);
   vogle_draw2( 1-4*W, 0.0);
   vogle_draw2( 1.0, 4*W);
   vogle_draw2( 1.0, 5*W);
   vogle_draw2( 1-5*W, 0.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1-3*W, 0.0);
   vogle_draw2( 1-2*W, 0.0);
   vogle_draw2( 1.0, 2*W);
   vogle_draw2( 1.0, 3*W);
   vogle_draw2( 1-3*W, 0.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1-1*W, 0.0);
   vogle_draw2( 1-0*W, 0.0);
   vogle_draw2( 1.0, 0*W);
   vogle_draw2( 1.0, 1*W);
   vogle_draw2( 1-1*W, 0.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1-4*W);
   vogle_draw2( 0.0, 1-3*W);
   vogle_draw2( 3*W, 1.0);
   vogle_draw2( 4*W, 1.0);
   vogle_draw2( 0.0, 1-4*W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1-2*W);
   vogle_draw2( 0.0, 1-1*W);
   vogle_draw2( 1*W, 1.0);
   vogle_draw2( 2*W, 1.0);
   vogle_draw2( 0.0, 1-2*W);
   vogle_closepoly();
   vogle_color( YELLOW);
   vogle_makepoly();
   vogle_move2( 1-4*W, 0.0);
   vogle_draw2( 1-3*W, 0.0);
   vogle_draw2( 1.0, 3*W);
   vogle_draw2( 1.0, 4*W);
   vogle_draw2( 1-4*W, 0.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 1-2*W, 0.0);
   vogle_draw2( 1-1*W, 0.0);
   vogle_draw2( 1.0, 1*W);
   vogle_draw2( 1.0, 2*W);
   vogle_draw2( 1-2*W, 0.0);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1-5*W);
   vogle_draw2( 0.0, 1-4*W);
   vogle_draw2( 4*W, 1.0);
   vogle_draw2( 5*W, 1.0);
   vogle_draw2( 0.0, 1-5*W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1-3*W);
   vogle_draw2( 0.0, 1-2*W);
   vogle_draw2( 2*W, 1.0);
   vogle_draw2( 3*W, 1.0);
   vogle_draw2( 0.0, 1-3*W);
   vogle_closepoly();
   vogle_makepoly();
   vogle_move2( 0.0, 1-1*W);
   vogle_draw2( 0.0, 1-0*W);
   vogle_draw2( 0*W, 1.0);
   vogle_draw2( 1*W, 1.0);
   vogle_draw2( 0.0, 1-1*W);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
   /* ## ZULU; */
   ZULU=vogle_genobj();
   vogle_makeobj(ZULU);
   vogle_color( RED);
   vogle_makepoly();
      vogle_move2( 0.0,0.0);
      vogle_draw2( 00.5, 0.5);
      vogle_draw2( 1.0, 0.0);
      vogle_draw2( 0.0,0.0);
   vogle_closepoly();
   vogle_color( BLACK);
   vogle_makepoly();
      vogle_move2( 0.0,0.0);
      vogle_draw2( 0.0, 1.0);
      vogle_draw2( 00.5, 0.5);
      vogle_draw2( 0.0,0.0);
   vogle_closepoly();
   vogle_color( BLUE);
   vogle_makepoly();
      vogle_move2( 1.0, 0.0);
      vogle_draw2( 00.5, 0.5);
      vogle_draw2( 1.0, 1.0);
      vogle_draw2( 1.0, 0.0);
   vogle_closepoly();
   vogle_color( YELLOW);
   vogle_makepoly();
      vogle_move2( 0.0, 1.0);
      vogle_draw2( 00.5, 0.5);
      vogle_draw2( 1.0, 1.0);
      vogle_draw2( 0.0, 1.0);
   vogle_closepoly();
   Box();
   vogle_closeobj();
   /* ################################################################################; */
}
void eachsem(int sema[26]){
   float CPX;
   float CPY;
   int i;
   int ii;
   for(i=0;i< 26; i++){
      vogle_color(BACKGROUND);
      vogle_clear();
      vogle_callobj(sema[i]);
      vogle_vflush();
      vogle_getkey();
   }
}
void allsem(int sema[26]) {
static char alf[26][10];
   float W, W2, SCR;
   float A;
   float B;
   float CPX;
   float CPY;
   int i,j;
   int NAME;
   vogle_color(-2); /* line thickness */
   W=(10.0-7.0)/8.0;
   W2=(10.0-2.0*W)/4.0;
   SCR=(W2-1.0)/12.0;
   strncpy(&alf[ 0][0],"ALPHA    ",10);
   strncpy(&alf[ 1][0],"BRAVO    ",10);
   strncpy(&alf[ 2][0],"CHARLIE  ",10);
   strncpy(&alf[ 3][0],"DELTA    ",10);
   strncpy(&alf[ 4][0],"ECHO     ",10);
   strncpy(&alf[ 5][0],"FOXTROT  ",10);
   strncpy(&alf[ 6][0],"GOLF     ",10);
   strncpy(&alf[ 7][0],"HOTEL    ",10);
   strncpy(&alf[ 8][0],"INDIA    ",10);
   strncpy(&alf[ 9][0],"JULIET   ",10);
   strncpy(&alf[10][0],"KILO     ",10);
   strncpy(&alf[11][0],"LIMA     ",10);
   strncpy(&alf[12][0],"MIKE     ",10);
   strncpy(&alf[13][0],"NOVEMBER ",10);
   strncpy(&alf[14][0],"OSCAR    ",10);
   strncpy(&alf[15][0],"PAPA     ",10);
   strncpy(&alf[16][0],"QUEBEC   ",10);
   strncpy(&alf[17][0],"ROMEO    ",10);
   strncpy(&alf[18][0],"SIERRA   ",10);
   strncpy(&alf[19][0],"TANGO    ",10);
   strncpy(&alf[20][0],"UNIFORM  ",10);
   strncpy(&alf[21][0],"VICTOR   ",10);
   strncpy(&alf[22][0],"WHISKEY  ",10);
   strncpy(&alf[23][0],"XRAY     ",10);
   strncpy(&alf[24][0],"YANKEE   ",10);
   strncpy(&alf[25][0],"ZULU     ",10);
   vpage(0.0,10.0,0.0,10.0);
   vogle_textsize (SCR*2, 7.0/5.0*SCR*2);
   NAME=0;

   for(i=0; i <= 3; i++){
      A= W+i*W2;
      for(j=6; j >= 0; j--){
         B=W+(1+W)*j;
         if( NAME < 26 ){
            vogle_move2(A,B);
            vogle_pushmatrix();
            vogle_getgp2(&CPX,&CPY);
            vogle_translate(CPX,CPY,0.0);
            vogle_callobj(sema[NAME]);
            vogle_popmatrix();
            vogle_move2(A+1.1,B+0.5);
            vogle_color(TEXT);
            vogle_drawstr(&alf[NAME][0]);
            fprintf(stdout,"%s\n",&alf[NAME][0]);
         }
         NAME++;
         vogle_vflush();
      }
   }
   fprintf(stdout,"done\n");
   vogle_move2( 0.1, 0.05);
   vogle_drawstr("SEMAPHORE John S. Urban, 19940927");
   vogle_getkey();
}
int main( int argc, char *argv[] ){
   float CPX;
   float CPY;
   int i;
   int ii;
   int sema[26];
   vogle_voutput("semaphore.out");
   /* prefsize(1000,1000); */
   vogle_vinit("");
   vogle_polyfill(TRUE);
   /* vpage( -0.25, 1.25, -0.25, 1.25); */
   vpage( -0.05, 1.05, -0.05, 1.05);
   vogle_color(-1); /* line thickness */
   vogle_move2(0.0,0.0);
   vogle_getgp2(&CPX,&CPY);
   vogle_translate(CPX,CPY,0.0);
   semaphores(sema);
   eachsem(sema);
   vogle_color(BACKGROUND);
   vogle_clear();
   vogle_color(TEXT);
   allsem(sema);
   vogle_vexit();
}
