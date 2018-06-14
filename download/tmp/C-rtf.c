#ident "@(#)M_DRAW:driver/rtf.c - M_DRAW driver for RTF (Microsoft Rich Text Format) files"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Mar 2008"
/*
 ===============================================================================
 Low level interface to RTF

 Based on RTF specifications
   Word2003RTFSpec.rtf
   Word2007RTFSpec9.rtf
 found at
   http://www.microsoft.com

 Quite a bit of the RTF specification needs clarification. Testing against
 the MicroSoft program "wordview".

 Recollecting  connected line segments into polylines because RTF is too
 verbose to do otherwise.

 Use negative color values to specify line thickness in raster units.
 ===============================================================================
 NEED TO DO YET:

 Should do grouping based on pen attribute change and page but this is
 complicated because apparently a count of the number of objects in the
 group is needed when the group begins, which would complicate the code.

 Need  decent  support of hardware  fonts  (size,  angle,  font ...).

 Should  support  center, left, right and top, bottom middle  justify
 flags for hardware text too.

 Can line weight be specified as scaled to local coordinate system size?
 This would allow thickness to scale with a rescaled plot.

 See if can reduce file size. RTF documentation is not very clear.

 A polyline of zero length does not print as a point; need to make into a point
or make sure zero-length vectors are changed to have length
 ===============================================================================
*/
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <time.h>
#ifndef MINGW
#include <pwd.h>
#include <sys/utsname.h>
#endif

#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include "draw.h"

extern FILE     *_voutfile();

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))
#define FLIPY(y)        ((vdevice.sizeSy)-(y))

#define FALSE 0
#define TRUE  1

#define Inches2twips = 1440 /* Conversion factor */

#define RTFXSIZE 12240  /* total drawing area size in x direction */
#define RTFYSIZE 15840  /* total drawing area size in y direction */

static int      RTF_first_time = 1, drawn = 0, pslstx = -1, pslsty = -1;/* last (x, y) drawn */
static int      MIN_LINE_X= 0, MAX_LINE_X= 0 ,MIN_LINE_Y= 0, MAX_LINE_Y = 0;
extern  FILE    *fp;

int OLDX;
int OLDY;
int RTF_MOVED=0;
int rtf_count=0;

#define CMAPSIZE 256
struct rgb_color {
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};
static struct rgb_color RTF_carr[CMAPSIZE];

/******************************************************************************/
/* Create line of unknown length for storing a polyline in */
static int  GATHER_COUNT=0;
#define     GATHERLINE_INC 65536
char      *pGATHERLINE;
int         GATHERLINE_CURRENT_SIZE = 0;
/******************************************************************************/
static int      lineopen = FALSE; /* PolyLine not open */
static int      shapeopen = FALSE; /* shape not open */
static int      curcol = 0; /* Current pen color (black) */
static int      curwid = 1; /* Current pen width */
static int      curpat  = 0; /* Current fill pattern*/
static int      pgroup=1; /* groupid reserved for the entire page */
extern float hardwidth[128]; /* array to store hardware character widths */
char fontstyle[256];
/******************************************************************************/
/* make a line from (x,y) to (x2,y2) */
void rtfline(int ix1,int iy1,int ix2,int iy2){
        int inw_x, inw_y;
        int iw, ih;
        /* Get the page coordinates of northwest corner */
        inw_x = (ix1 < ix2) ? ix1 : ix2;
        inw_y = (iy1 < iy2) ? iy1 : iy2;
        ix1 -= inw_x;
        ix2 -= inw_x;
        iy1 -= inw_y;
        iy2 -= inw_y;
        /* So that (x1,y1) and (x2,y2) are relative to the NW corner */
        iw = abs(ix1 - ix2); /* horizontal distance */
        ih = abs(iy1 - iy2); /* vertical distance */
        rtf_count++;
        fprintf(fp,"\n{\\comment RTFLINE}");
        fprintf (fp, "{\\*\\do\\dobxpage\\dobypage\\dodhgt%d",rtf_count);

        /* <dpsimple> ----> <dpsimpledpk><dphead><dpprops> */
           /* <dpsimpledpk> */
              fprintf (fp, "\\dpline");
              fprintf (fp, "\\dpptx%d\\dppty%d", ix1,FLIPY(iy1));
              fprintf (fp, "\\dpptx%d\\dppty%d", ix2,FLIPY(iy2));
           /* </dpsimpledpk> */

           /* <dphead> */
              /* POSITION AND SIZE */
              /* dpxN X-offset of the drawing primitive from it's anchor */
              /* dpyN Y-offset of the drawing primitive from it's anchor */
              fprintf (fp, "\\dpx%d\\dpy%d ", inw_x, inw_y);

              /* dpxsizeN X-size of the drawing primitive */
              /* dpysizeN Y-size of the drawing primitive */
              fprintf (fp, "\\dpxsize%d\\dpysize%d", iw, ih);
           /* </dphead> */

           /* <dpprops> */
              /* PROPERTIES */
              /* line thickness */
              fprintf (fp, "\\dplinew%d ",15);

              /* RGB of line color */
              fprintf (fp, "\\dplinecor%d\\dplinecog%d\\dplinecob%d", 0,0,0);
           /* </dpprops> */
        /* </dpsimple> */

        fprintf (fp, "}\n");
}
/******************************************************************************/
static int RTF_header(){
   time_t tod;
   struct tm *thetime;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;
   time(&tod);
   thetime = localtime(&tod);
/*----------------------------------------------------------------------------*/
#ifndef MINGW
   un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
   uname(un);
/* Start off the RTF file.  Page units are in twips */
   fprintf( fp, "{\\rtf1\\ansi\n");
   fprintf( fp, "\\deff0{\\fonttbl {\\f0 \\froman Times New Roman;}}\n");
   fprintf( fp, "\\paperw%d \\paperh%d ", RTFXSIZE, RTFYSIZE);
   fprintf( fp, "\\deflang1033\\plain\\f0\\fs20\n");
   fprintf( fp, "\n");

   fprintf(fp,"{\\*\\generator @(#)M_DRAW RTF Driver Version 1.0.0, October  2007; John S. Urban}\n");

   if ((username = getlogin()) == NULL ){
      pw = getpwuid(getuid());
      username = pw->pw_name;
   }
   fprintf(fp,"{\\info\n");
   fprintf(fp,"{\\comment ---------------------------------------------------------------------}\n");
   fprintf(fp,"{\\title %s}\n","M_DRAW RTF plots");
   fprintf(fp,"{\\author %s}\n",username);
   fprintf(fp,"{\\doccomm on OS=%.*s NETWORK_NAME=%.*s RELEASE=%.*s VERSION=%.*s MACHINE=%.*s}\n",
       (int)sizeof(un->sysname),  un->sysname,
       (int)sizeof(un->nodename), un->nodename,
       (int)sizeof(un->release),  un->release,
       (int)sizeof(un->version),  un->version,
       (int)sizeof(un->machine),  un->machine);
#endif
/*----------------------------------------------------------------------------*/
    fprintf(fp,"{\\creatim\\yr%d\\mo%d\\dy%d\\hr%d\\min%d\\sec%d}\n",
      thetime->tm_year+1900,
      thetime->tm_mon,
      thetime->tm_mday,
      thetime->tm_hour,
      thetime->tm_min,
      thetime->tm_sec
    );
    fprintf(fp,"}\n"); /* end info section */

    fprintf(fp,"{\\pard\n");
    /*
    rtfline(0,0,RTFXSIZE,RTFYSIZE);
    rtfline(RTFXSIZE,0,0,RTFYSIZE);
    */
    return(0);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
int RTF_mapcolor(int i, int r, int g, int b) {
   if (i >= CMAPSIZE || i < 0 ){
      return(-1);
   }
   RTF_carr[i].red = (unsigned short)(r);
   RTF_carr[i].green = (unsigned short)(g);
   RTF_carr[i].blue = (unsigned short)(b);
   return(0);
}
/******************************************************************************/
/* RTF_init set up the environment. Returns 1 on success. */
static int RTF_init(void) {
   int prefx, prefy, prefxs, prefys;
   int i;
   fp = _voutfile();
   rtf_count=0;

   if (!RTF_first_time) return(1);

   RTF_header();

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ){
      vdevice.sizeSy = prefys;
      vdevice.sizeSx = prefxs;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
   }else{
      vdevice.sizeSy = RTFYSIZE;  /* size in resolution rasters */
      vdevice.sizeSx = RTFXSIZE;  /* size in resolution rasters */
      vdevice.sizeX = vdevice.sizeY = MIN(RTFXSIZE,RTFYSIZE); /* current viewport to use */
   }

   /*
   fprintf(fp,"<p><a name=\"Page%d\"></a>\n", pgroup);
   */
   vdevice.depth = 8;
   for (i = 0; i < CMAPSIZE; i++) /* set up the basic colors */
   {
      RTF_carr[i].red=255;
      RTF_carr[i].green=255;
      RTF_carr[i].blue=255;
   }

   RTF_mapcolor(0, 255, 255, 255);
   RTF_mapcolor(1, 255, 0, 0);
   RTF_mapcolor(2, 0, 255, 0);
   RTF_mapcolor(3, 255, 255, 0);
   RTF_mapcolor(4, 0, 0, 255);
   RTF_mapcolor(5, 255, 0, 255);
   RTF_mapcolor(6, 0, 255, 255);
   RTF_mapcolor(7, 0, 0, 0);

   RTF_mapcolor( 8, 155, 0, 0);
   RTF_mapcolor( 9, 0, 155, 0);
   RTF_mapcolor(10, 155, 255, 255);
   RTF_mapcolor(11, 155, 155, 0);
   RTF_mapcolor(12, 0, 0, 155);
   RTF_mapcolor(13, 155, 0, 155);
   RTF_mapcolor(14, 0, 155, 155);
   RTF_mapcolor(15, 100, 100, 100);
   lineopen = FALSE; /* Polyline not open */
   shapeopen = FALSE; /* Polyline not open */
   curcol=0;
   curwid=1;
   curpat=0;
   drawn = 0;
   return (1);
   /*      Set other line drawing parameters */
   /*      Move                              */
   /*      Set a default font height         */
}
/******************************************************************************/
static int closeline(void){
   if(lineopen){
      if(RTF_MOVED == 0 ){
         /* draw small line to make a point */
         //fprintf(fp,"{\\comment CLOSELINE point RTFLINE}");
         //rtfline(OLDX-1,OLDY-1,OLDX+1,OLDY+1);
      }
      /* end curve */
      fprintf (fp, "\\dppolycount%d\n", GATHER_COUNT);
      fprintf (fp, "%s\n", pGATHERLINE);
      fprintf (fp, "\\dpx0\\dpy0\\dpxsize%d\\dpysize%d", RTFXSIZE, RTFYSIZE);
      fprintf (fp, "\\dplinew%d",curwid); /* line width */
      fprintf (fp, "\\dplinecor%d\\dplinecog%d\\dplinecob%d",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
      fprintf(fp, "}\n");
      //fprintf(fp, "{\\comment CLOSELINE}\n");
      lineopen = FALSE; /* Polyline not open */
      GATHER_COUNT = 0;
      //free it
      free (pGATHERLINE);
   }
   return (0);
}
/******************************************************************************/
static int closeshape(void){
   if(shapeopen){
      fprintf(fp, "\n"); /* end curve */
      shapeopen = FALSE; /* Polyline not open */
      GATHER_COUNT = 0;
   }
   return (0);
}
/******************************************************************************/
static int openline(void){
   if(!lineopen){
      //fprintf(fp,"{\\comment OPENLINE}\n"); /* start curve */
      pGATHERLINE = malloc (GATHERLINE_INC);
      GATHERLINE_CURRENT_SIZE=GATHERLINE_INC;
      GATHER_COUNT=0;
      rtf_count++;
      fprintf(fp,"{\\*\\do\\dobxpage\\dobypage\\dodhgt%d\\dppolyline",rtf_count);
      MIN_LINE_X= pslstx, MAX_LINE_X = pslstx ,MIN_LINE_Y= pslsty, MAX_LINE_Y = pslsty;
      lineopen = TRUE; /* Polyline open */
   }
   return (0);
}
/******************************************************************************/
static int openshape(void){
   if(!shapeopen){
      shapeopen = TRUE; /* shape open */
   }
   return (0);
}
/******************************************************************************/
/* RTF_exit do a flush and close the output file if necessary.  */
static int RTF_exit(void) {
   closeline(); /* close Polyline line if it open */
   closeshape(); /* close shape line if it open */
   fprintf(fp,"\\par}\n");
   fprintf(fp,"\n}\n"); /* End of Document */
   drawn = 0;
   GATHER_COUNT = 0;

   if (fp != stdout && fp != stderr ){
                fflush(fp);
                if(vdevice.writestoprocess == 2){
                   pclose(fp);
                }else{
                   fclose(fp);
                }
   }
   return (0);
}
/******************************************************************************/
static int RTF_draw(int x, int y) {
/* RTF_draw draw to an x, y point.  */
/* Note: (0, 0) is defined as the top left of the window in RTF.  */
   static char linefeed[2] = {' ','\n'};
   char point[100];
   int i;

   if (pslstx != vdevice.cpVx || pslsty != vdevice.cpVy ){
      closeline(); /* close line if required */
      openshape(); /* start shape if required */
      openline();  /* start line */
      /* start gathering points into line of relatively arbitrary length */
      snprintf (pGATHERLINE,GATHERLINE_CURRENT_SIZE,"\\dpptx%d\\dppty%d", vdevice.cpVx, FLIPY(vdevice.cpVy));
      OLDX=vdevice.cpVx;
      OLDY=FLIPY(vdevice.cpVy);
      RTF_MOVED=0;

      GATHER_COUNT = 1;
   }
   openshape();              /* start shape if required */
   openline();               /* start line if required */
   if(GATHER_COUNT == 0){
      fprintf(fp,"{\\comment RTF_draw to RTFLINE}");
      rtfline(x,y,x+1,y+1);  /* unclear what happens to zero-length vector */

      OLDX=x;
      OLDY=FLIPY(y);
      RTF_MOVED=0;

   }else{

      if(OLDX!=x || OLDY!=FLIPY(y))RTF_MOVED++;

      OLDX=x;
      OLDY=FLIPY(y);

      /* gather point */
      snprintf(point,100,"%c\\dpptx%d\\dppty%d", linefeed[( (GATHER_COUNT % 6) /5)], x ,FLIPY(y));
      //if i reached maximize size then realloc size
      i=strlen(pGATHERLINE);
      if (i +strnlen(point,100) <= GATHERLINE_CURRENT_SIZE ) {
         GATHERLINE_CURRENT_SIZE = i + GATHERLINE_INC;
         pGATHERLINE = realloc (pGATHERLINE, GATHERLINE_CURRENT_SIZE);
      }
      strncat(pGATHERLINE,point,GATHERLINE_CURRENT_SIZE);
   }

   GATHER_COUNT++;
   pslstx = x;
   pslsty = y;
   drawn = 1;
   return (0);
}
/******************************************************************************/
/* RTF_clear flush the current page without resetting the graphics state */
static int RTF_clear(void) {
   closeline();                      /* close line if required */
   closeshape();                     /* close shape if required */
   if (drawn){
     pgroup++;                       /* increment page id */
    fprintf(fp,"\\page\\par}\n");    /* Page Clear, End of Page Group */
    fprintf(fp,"{\\pard\n");
   }
   drawn = 0;
   GATHER_COUNT = 0;
   return(0);
}
/******************************************************************************/
/* RTF_color change the color of the pen
 *      kludged so negative value sets raster line width
 *      if exceed allowable number of colors maybe pick a line style
 *      or something like a gradient fill style for fun
 */
static int RTF_color(int col) {
   closeline(); /* close line if required */
   closeshape(); /* close shape if required */
   if ( col < 0 )
   {
      curwid = abs(col);
   } else {
      curpat = col/CMAPSIZE;
      curcol = col % CMAPSIZE;
   }
   return(0);
}
/******************************************************************************/
/*
 *      value sets raster linewidth
 */
static int RTF_setlw(int width) {
   closeline(); /* close line if required */
   closeshape(); /* close shape if required */
   if ( width >= 0 ) {
      curwid = width;
   }
   return(0);
}
/******************************************************************************/
/* load in small or large - could be improved. Radically KLUDGED; made SoftText extern  */
static int RTF_font(char *fontname) {
   int i;
   float rat;
   /* assuming vdevice.hwidth is the desired width of the reference character,
    * this is a list of percentages of the other character widths.
    */
   static int helvetica_w[128] = {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55,
      34,34,58,58,58,61,97,
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61,
      33,28,33,58,55,28,
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50,
      39,28,39,58,0
   };

   vdevice.attr->a.softtext = SOFTHARDWARE;
   /* textsize will be obeyed after the font is set
    * maybe should read current software size and convert virtual
    * to device instead of resetting each time */

      if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
         vdevice.hwidth=11.0;
         vdevice.hheight=11.0;
      }

   if (strcmp(fontname, "small") == 0) {
      rat=0.55; /* Kludge Factor */

      for (i = 0; i < 128; i++){
         hardwidth[i]=1.00 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
         }
      strcpy(fontstyle,"Font-weight:bold; font-family:'Times New Roman'");
      strcpy(fontstyle,"font-family:'Times New Roman'");
   } else if (strcmp(fontname, "large") == 0) {
      rat=1.00; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=((float)helvetica_w[i])/100.0*rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
      }
      strcpy(fontstyle,"font-weight:bold; font-family:'Arial Black', Arial, sans-serif");
      strcpy(fontstyle,"font-family:'Arial Black', Arial, sans-serif");
   } else{
      strcpy(fontstyle,"font-weight:bold; font-family:'Arial Black', Arial, sans-serif");
      strcpy(fontstyle,"font-family:'Arial Black', Arial, sans-serif");
      return(0);
   }

   return(1);
}
/******************************************************************************/
/*  print the given string, escaped as good RTF*/
/*  actually escapes much more than is necessary*/
void esc(char *s ){
   int ch;
   while ((ch = *s++)){
      switch(ch) {
      case 9:
         fputs("\\tab ", fp);
         break;
      case 12:
         fputs("\\page",fp);
         break;

      case '\\':
         fputs("\\\\",fp);
         break;

      case '{':
         fputs("\\{",fp);
         break;

      case '}':
         fputs("\\}",fp);
         break;

      case '\n':
         fputs("\n\\line ",fp);
         break;

      default:
         /* ignore control characters instead? */
         if (ch < ' ' || ch >= 128) {
            fprintf(fp,"\\'%02x",ch);
         }else{
            fputc(ch,fp);
         }
      }
   }
   fprintf(fp,"\n");
   return;
}
/******************************************************************************/
/* output a character string using current character size and rotation angle. */
static int RTF_string(char *s) {
   float   slen;
   float   sheight;
   int uneven;
   int x;
   int y;
   int ijust;
   int First_Line_Height=100;

   closeline(); /* close line if required */
   closeshape(); /* close shape if required */

   if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
      fprintf(stderr,"*RTF_string* ERROR: ZERO SIZE CHARACTERS\n");
      vdevice.hwidth=11.0;
      vdevice.hheight=11.0;
   }
   slen=draw_strlength(s)*vdevice.hwidth;
   sheight=vdevice.hheight;

   /* There is a bug where if the line is horizontal, the text prints with zero character height */
        uneven=0;
        if( FLIPY(vdevice.cpVy) == FLIPY(vdevice.cpVy+slen*vdevice.attr->a.textsin) ){
                uneven=1;
        }else{
                uneven=0;
        }
        /* RTF aligns thru center of text */
        ijust=(int)(0.50*sheight);

        /* The line the text will be placed along */
        x=(int)(vdevice.cpVx+slen*vdevice.attr->a.textcos);
        y=(int)(uneven+vdevice.cpVy+slen*vdevice.attr->a.textsin);

       //fprintf(fp,"<v:line from=\"%d,%d\" to=\"%d,%d\"\n", vdevice.cpVx, FLIPY(vdevice.cpVy+ijust), x, FLIPY(y+ijust));

      /* character outline stroke color */

      /* character fill color */
      fprintf(fp,"//rgb(%d,%d,%d)\n",
          RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);

      //fprintf(fp,"style=%s;",fontstyle);
      //fprintf(fp,"fontsize:%d\"/>\n",(int)sheight);
/* -------------------------------------------------------------------------- */
/*
   Write the contents of string into this exact-positioned
   paragraph, feeding each one through esc(line).
*/

        fprintf(fp,"\n{\\comment RTF_string}");
        fprintf(fp,"{\\pard \\pvpg\\phpg");

        /* corner of */
        fprintf(fp,"\\posx%d \\posy%d", OLDX, OLDY-First_Line_Height) ;
        fprintf(fp,"\\absw%d \\absh-%d", OLDX,OLDY + First_Line_Height);
        fprintf(fp,"\\f0\\fs20"); /*  select font from font table and font size */
        esc(s);
        /*
        fprintf(fp,"\n\\line");
        */
        fprintf(fp,"{\\par\n"); /*  close paragraph */
        esc(s);
   drawn = 1;
   pslstx = x;
   pslsty = y;
   closeshape(); /* close shape if required */
   return(0);
}
/******************************************************************************/
/* RTF_char output a character */
int RTF_char(char c){
   char  s[2];
   s[0] = c;
   s[1]='\0';
   RTF_string(s);
   return(0);
}
/******************************************************************************/
/* make a polygon */
int RTF_fill_A(int n, int x[],int y[]){
   int i;
   int ix1, iy1;
   int inw_x, inw_y;
   int ise_x, ise_y;
   int iw, ih;
   static char linefeed[2] = {' ','\n'};

   closeline(); /* close line if required */
   closeshape(); /* close line if required */
   /* Get the page coordinates of northwest corner and southeast corner*/
   inw_x = x[0];
   inw_y = y[0];
   ise_x = x[0];
   ise_y = y[0];
   for (i=1;i <n;i++){
      inw_x = MIN(inw_x,x[i]);
      inw_y = MIN(inw_y,y[i]);
      ise_x = MAX(ise_x,x[i]);
      ise_y = MAX(ise_y,y[i]);
   }
   /* So that (x[i],y[i]) are relative to the NW corner */

   rtf_count++;
   fprintf (fp, "{\\*\\do\\dobxpage\\dobypage\\dodhgt%d",rtf_count);
   fprintf (fp, "\\dppolygon\\dppolycount%d\n",n);

   for (i=0;i <n;i++){
      ix1 = x[i]-inw_x;
      iy1 = y[i]-inw_y;
      fprintf(fp, "\\dpptx%d\\dppty%d%c", ix1,FLIPY(iy1),linefeed[(i % 8/7)]);
   }
   fprintf (fp, "\n\\dpx%d\\dpy%d", inw_x, FLIPY(y[0]-inw_y));

   iw = abs(ise_x - inw_x); /* horizontal distance */
   ih = abs(inw_y - ise_y); /* vertical distance */
   fprintf (fp, "\\dpxsize%d\\dpysize%d\n", iw, ih);

   /* RGB of background color */
   fprintf (fp, "\\dpfillbgcr%d\\dpfillbgcg%d\\dpfillbgcb%d\n",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
   /* RGB of foreground color */
   fprintf (fp, "\\dpfillfgcr%d\\dpfillfgcg%d\\dpfillfgcb%d\n",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
   /* line width and RGB of outline color */
   fprintf (fp, "\\dplinew1\\dplinecor%d\\dplinecog%d\\dplinecob%d\n",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
   /* fill pattern */
   fprintf (fp, "\\dpfillpat%d\n}\n", 1);

   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];

   pslstx = pslsty = -1;           /* fill destroys current path */
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* make a polygon based to corner of page */
int RTF_fill(int n, int x[],int y[]){
   int i;
   static char linefeed[2] = {' ','\n'};

   closeline(); /* close line if required */
   closeshape(); /* close line if required */
   rtf_count++;
   fprintf (fp, "{\\*\\do\\dobxpage\\dobypage\\dodhgt%d",rtf_count);
   fprintf (fp, "\\dppolygon\\dppolycount%d\n",n);

   for (i=0;i <n;i++){
      fprintf(fp, "\\dpptx%d\\dppty%d%c", x[i],FLIPY(y[i]),linefeed[(i % 8/7)]);
   }
   fprintf (fp, "\n\\dpx0\\dpy0\\dpxsize%d\\dpysize%d\n", RTFXSIZE, RTFYSIZE);

   /* RGB of background color */
   fprintf (fp, "\\dpfillbgcr%d\\dpfillbgcg%d\\dpfillbgcb%d",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
   /* RGB of foreground color */
   fprintf (fp, "\\dpfillfgcr%d\\dpfillfgcg%d\\dpfillfgcb%d",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
   /* line width and RGB of outline color */
   fprintf (fp, "\\dplinew1\\dplinecor%d\\dplinecog%d\\dplinecob%d\n",
       RTF_carr[curcol].red, RTF_carr[curcol].green, RTF_carr[curcol].blue);
   /* fill pattern */
   fprintf (fp, "\\dpfillpat%d\n}\n", 1);

   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];

   pslstx = pslsty = -1;           /* fill destroys current path */
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry rtfdev = {
   "rtf",        /* name of device */
   "large",      /* name of large font */
   "small",      /* name of small font */
   noop,         /* Set drawing in back buffer */
   RTF_char,     /* Draw a hardware character */
   noop,         /* Check if a key was hit */
   RTF_clear,    /* Clear the screen to current color */
   RTF_color,    /* Set current color */
   RTF_draw,     /* Draw a line */
   RTF_exit,     /* Exit graphics */
   RTF_fill,     /* Fill a polygon */
   RTF_font,     /* Set hardware font */
   noop,         /* Set drawing in front buffer */
   noop,         /* Wait for and get the next key hit */
   RTF_init,     /* Initialize the device */
   noop2,        /* Get mouse/cross hair position */
   RTF_mapcolor, /* Set color indices */
   RTF_setlw,    /* Set line width */
   RTF_string,   /* Draw a hardware string */
   noop,         /* Swap front and back buffers */
   noop          /* Syncronize the display */
};
/******************************************************************************/
/* _RTF_devcpy copy the rtf device into vdevice.dev.  */
int _RTF_devcpy() {
   vdevice.dev = rtfdev;
   return(0);
}
/******************************************************************************/
/*
 *
{\rtf1\ansi (0,0), (1000,1000), (2000,500), (3000,2000) connected below:\par
{\*\do\dobxmargin\dobypara\dodhgt0\dppolyline\dppolycount4
\dpptx0\dppty0
\dpptx1000\dppty1000
\dpptx2000\dppty500
\dpptx3000\dppty2000
\dpx0\dpy0\dpxsize3000\dpysize2000
}
}
 *
 */
/******************************************************************************/
