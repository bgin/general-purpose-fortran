#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "draw.h"
static float atx;
static float aty;

static float littlebar=8.0;        /* these numbers are arbitrary,*/
                                   /* as long as the ratio*/
static float bigbar=20.0;          /* stays between 2:1 and 3:1 */

static float ysize=75.0;
/*------------------------------------------------------------------------------*/
void SetBarcodeSize(float narrow,float wide, float tall){
   littlebar=narrow;
   bigbar=wide;
   ysize=tall;
}
/*------------------------------------------------------------------------------*/
void SetBarcodeStart(float xstart,float ystart){
   atx=xstart;
   aty=ystart;
}
/*------------------------------------------------------------------------------*/
/*
   @(#) 39 bar code
   given an ASCII string, do what's necessary. tack on delimiter '*'s
*/
void PrintBarcode(char *string)
{
        int x;
        void PutBars(char c);

        PutBars('*');   /* starting delimiter */
        for(x=0;string[x];x++){
                PutBars(string[x]);     /* do each char */
        }
        PutBars('*');   /* ending delimiter */
}
/*------------------------------------------------------------------------------*/
/*
   low-level M_DRAW graphics commands to draw filled rectangles
*/
void DoRectangle(float atx,float xsize,float aty,float ysize){
        draw_pushattributes();
           draw_polyfill(1); /* true*/
            draw_rect(atx,aty,atx+xsize,aty+ysize);
           draw_polyfill(0); /* false*/
           /*fprintf(stdout,"%f %f %f %f\n",atx,aty,atx+xsize,aty+ysize);*/
        draw_popattributes();
}
/*------------------------------------------------------------------------------*/
/*
   given the thick/thin map of 9 0's and 1's, order up some rectangles
   of the appropriate sizes and positions
*/
void BarChar(char *mapstring){
        int x;

        float barsize;

        for(x=0;x < 9;x++){
                if(mapstring[x]=='0'){
                        /* thin one */
                        barsize=littlebar;
                } else {
                        /* thick one */
                        barsize=bigbar;
                }
                if(x%2){
                        /* gap - do nothing */
                } else {
                        /* black bar - draw it */
                        DoRectangle(atx,barsize,aty,ysize);
                }
                atx += barsize; /* move over that much */
        }
        atx += littlebar;       /* gap between each bar code character */
}
/*------------------------------------------------------------------------------*/
/*
   determine the actual thick/thin pattern for the submitted character
*/
void PutBars(char c){
        /*fprintf(stdout,"=================== %c\n",c);*/
        switch(c){
                case '0': BarChar("000110100");break;
                case '1': BarChar("100100001");break;
                case '2': BarChar("001100001");break;
                case '3': BarChar("101100000");break;
                case '4': BarChar("000110001");break;
                case '5': BarChar("100110000");break;
                case '6': BarChar("001110000");break;
                case '7': BarChar("000100101");break;
                case '8': BarChar("100100100");break;
                case '9': BarChar("001100100");break;
                case '-': BarChar("010000101");break;
                case '.': BarChar("110000100");break;
                case ' ': BarChar("011000100");break;
                case '+': BarChar("010001010");break;
                case '%': BarChar("000101010");break;
                case '$': BarChar("010101000");break;
                case '/': BarChar("010100010");break;
                case 'A':
                case 'a': BarChar("100001001");break;
                case 'B':
                case 'b': BarChar("001001001");break;
                case 'C':
                case 'c': BarChar("101001000");break;
                case 'D':
                case 'd': BarChar("000011001");break;
                case 'E':
                case 'e': BarChar("100011000");break;
                case 'F':
                case 'f': BarChar("001011000");break;
                case 'G':
                case 'g': BarChar("000001101");break;
                case 'H':
                case 'h': BarChar("100001100");break;
                case 'I':
                case 'i': BarChar("001001100");break;
                case 'J':
                case 'j': BarChar("000011100");break;
                case 'K':
                case 'k': BarChar("100000011");break;
                case 'L':
                case 'l': BarChar("001000011");break;
                case 'M':
                case 'm': BarChar("101000010");break;
                case 'N':
                case 'n': BarChar("000010011");break;
                case 'O':
                case 'o': BarChar("100010010");break;
                case 'P':
                case 'p': BarChar("001010010");break;
                case 'Q':
                case 'q': BarChar("000000111");break;
                case 'R':
                case 'r': BarChar("100000110");break;
                case 'S':
                case 's': BarChar("001000110");break;
                case 'T':
                case 't': BarChar("000010110");break;
                case 'U':
                case 'u': BarChar("110000001");break;
                case 'V':
                case 'v': BarChar("011000001");break;
                case 'W':
                case 'w': BarChar("111000000");break;
                case 'X':
                case 'x': BarChar("010010001");break;
                case 'Y':
                case 'y': BarChar("110010000");break;
                case 'Z':
                case 'z': BarChar("011010000");break;

                /* an asterisk and everything else! */
                default:  BarChar("010010100");break;
        }
}
/*------------------------------------------------------------------------------*/
void barcode(
      float xcorner,
      float ycorner,
      float xsmall,
      float xlarge,
      float ysize,
      char *string
   ){
   SetBarcodeSize(xsmall,xlarge,ysize);
   SetBarcodeStart(xcorner,ycorner);
   PrintBarcode(string);
}
/*------------------------------------------------------------------------------*/

void barcode_sun_f(
      float *xcorner,
      float *ycorner,
      float *xsmall,
      float *xlarge,
      float *ysize,
      char *s,
      int len
   ){
        char            buf[BUFSIZ];
        register char   *p;

        strncpy(buf, s, len);
        buf[len] = 0;

        for (p = &buf[len - 1]; *p == ' '; p--)
                ;

        *++p = 0;

        barcode(*xcorner,*ycorner,*xsmall,*xlarge,*ysize,buf);
}
/*------------------------------------------------------------------------------*/
