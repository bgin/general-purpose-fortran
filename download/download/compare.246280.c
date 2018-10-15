#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <stdlib.h>
/*
    This program compares 2 similar text files for mismatches.  Call as

        COMPARE FileA FileB   (redirection of output okay)

Pascal History.

     *  Written and Copyright by James F. Miner, University of Minnesota, 1977
        for CDC mainframe.

     *  Modified Willett Kempton, Mich State Univ., 1984.  Added options for
        ANSI output and CPM support.

     *  Modified Kim Kokkonen, TurboPower Software, 1986.
        Minimize code for Turbo Pascal version 3.0


C History.

     *  Converted to C language (Microsoft 3.0 and higher) Gary Hill,
        Westinghouse Electric Corp., November, 1987 and added options

     *  Converted to C language (DG C 3.01) Gary Hill,
        Westinghouse Electric Corp., November, 1987.

*/

/* ***************************** contants ****************************** */

#define DLineLength 256         /* maximum significant line length */
#define DLineLengthR (DLineLength+1)    /* max for allocate and reads */
#define MinRead 40              /* try for multiple reads =1 saves heap */
#define Prespace 7              /* spaces before text on reportfile lines */

/* ************************* macros ***************************** */

#define Nil ((Line*)NULL)
#define Endstream(x) ((x.cursor==Nil) && x.endfile)
#define writeoneline(n,l,p) printf("%c%5d|%s\n",n,l,p->image)

/* ************************** new data types ***************************** */

typedef struct sLine {          /* Line definition */
    struct sLine *nextline;
    int  length;
    char image[DLineLengthR];
} Line;

typedef struct {                /* Stream definition */
    char name[80];              /* name of file */
    FILE *fileS;                /* stream */
    Line *cursor,*head,*tail;
    int  cursorlineno, headlineno, taillineno;
    int  endfile;               /* EOF flag */
} Stream;

typedef struct {
    long flen;                  /* length of file in bytes */
    int cyear,cmonth,cday;      /* creation file date */
    int chour,cminute,csecond;  /* creation file time */
    int myear,mmonth,mday;      /* modify     file date */
    int mhour,mminute,msecond;  /* modify     file time */
} Fdata;

/* ********************* global data allocation ************************** */

Stream  A,B;                    /* 2 streams to be compared */
Fdata   Adat,Bdat;              /* file statistics of files to compare */
int     endfile=0;              /* an EOF occurred somewhere */
Line    *freelines=Nil;         /* pointer to chain of available lines */
int     same=1;                 /* boolean flag for lines the same */
int     linestoolong=0;         /* number of lines not completely checked */

int     LineLength=DLineLength; /* maximum significant line length */
int     LineLengthR;            /* maximum line length or read */
int     MinLinesForMatch=4;     /* number of consecutive equivalent
                                   lines to end mismatch */
int     MarkUnequalColumns=5;   /* unequal lines are paired, and cols
                                   marked, if mismatch < this length */
int     xtabs=0;                /* expand tabs flag */
int     wsp=0;                  /* white space equivalence flag */
int     body=0,bodyp=1;         /* body expansion flag */

/* *************************  code  ***************************** */

char *strupr(cptr)      /* converts a string to upper case and returns string */
    char *cptr;
{
    char *ptr;
    for(ptr=cptr; *ptr; ptr++) if(islower(*ptr)) *ptr = _toupper(*ptr);
    return(cptr);
}

long filelength(fp)     /* returns the length of a file given its packet */
    FILE *fp;
{
    long  curlen,endlen;
    curlen = ftell(fp);         /* get current position */
    endlen = fseek(fp,0L,2);    /* go to the end */
    if(!endlen)endlen=ftell(fp);/* get position at end of file */
    fseek(fp,curlen,0);         /* get back to original position */
    return(endlen);             /* return the end position */
}

char *fgets_xt(cptr,csize,fp)   /* fgets with expanded tabs */
    char *cptr; int csize; FILE *fp;
{
    int i; char c,*ptr;  int blank=0;
    csize--;
    for(i=0,ptr=cptr; i<csize; i++) {
        switch( c=fgetc(fp) ) {
            case '\n': case '\f': case '\r':
                *(ptr++)=c; *ptr=0; return(cptr);
            case '\t':
                if( wsp ) {
                    if( blank ) i--; else {*(ptr++)=' ';blank=1;}
                } else {
                    do {*(ptr++)=' '; i++;} while( i&7 && i<csize ); i--;
                    if(i==csize) {*ptr=0;return(cptr);}
                }
                break;
            case ' ':
                if( wsp ) {
                    if( blank ) i--; else {*(ptr++)=' ';blank=1;}
                    break;
                }
            default:
                if( feof(fp) ) return(NULL);
                *(ptr++) = c; blank=0;
        }
    }
    *ptr=0;
    return(cptr);
}

int comparelines()      /* compares the current lines of streams and returns */
{                       /* match condition.  EOF match if on both */
    if(A.cursor==Nil || B.cursor==Nil) return(Endstream(A) && Endstream(B));
    if( A.cursor->length != B.cursor->length ) return(0);
    return(!strcmp(A.cursor->image,B.cursor->image));
}

void Mark(x)            /* causes beginning of stream to be positioned before */
    Stream *x;          /* current stream cursor.  buffers get reclaimed,     */
{                       /* line counters reset, etc.                          */
    Line *ptr;
    if( x->head != Nil ) {
        while( x->head != x->cursor ) {
            ptr = x->head->nextline;
            x->head->nextline = freelines;
            freelines = x->head;
            x->head = ptr;
        }
        x->headlineno = x->cursorlineno;
        if( x->cursor == Nil ) {
            x->tail = Nil;
            x->taillineno = x->cursorlineno;
        }
    }
}

void readline(x)        /* reads line(s) from the specified stream */
    Stream *x;
{
    Line *newline;
    int  morereads=MinRead;
    char *cptr;

    while( !x->endfile && morereads > 0 ) {

        /* get space for the lines as appropriate */
        newline = freelines;
        if( newline != Nil )    /* use space available */
            freelines = freelines->nextline;
        else {                  /* allocate */
            if( (newline=(Line*)malloc(sizeof(Line))) == Nil )
                {printf("Out of memory.  Cannot continue.\n"); exit(2);}
        }
        newline->nextline = Nil;

        /* read in a line for the stream */
        if( ((xtabs||wsp) ? fgets_xt(newline->image,LineLengthR,x->fileS) :
          fgets(newline->image,LineLengthR,x->fileS)) == NULL ) {
            x->endfile=1;                       /* set stream EOF flag */
            newline->nextline = freelines;
            freelines = newline;
            return;
        }

        /* trim off the trailing whitespace - i.e. \n,\t,blanks */
        if((cptr=strpbrk(newline->image,"\n\f\r")) == NULL ) {  /* find del */
            linestoolong++;             /* bump number of long lines */
            cptr=newline->image+LineLength;     /* point to last byte of line */
            while(strchr("\n\f\r",fgetc(x->fileS))==NULL && !feof(x->fileS));
        }
        *cptr = 0;                              /* set end of string */
        while( cptr > newline->image &&         /* while not at beginning & */
          strchr(" \t\n",*(cptr-1)) != NULL)    /* prev char is white */
                *(--cptr) = 0;                  /* prev char nul and current */

        /* set the stats about the current newline */
        newline->length = cptr - newline->image;        /* set the length */
        if( x->tail == Nil ) {                  /* if first read of stream */
            x->head = newline;
            x->headlineno = x->taillineno = 1;
        } else {                                /* if not first read of strm.*/
            x->tail->nextline = newline;        /* set nline from current tail*/
            x->taillineno++;                    /* bump tail line number */
        }
        x->tail = newline;                      /* set new tail pointer */
        morereads--;                            /* decrement lines to read */
    }           /* get more reads */
}

void movecursor(x)      /* cursor for stream x is moved forward one line,    */
    Stream *x;          /* reading from x if req'rd and bumping line count.  */
{
    if( x->cursor != Nil ) {
        if( x->cursor == x->tail ) readline(x);
        x->cursor = x->cursor->nextline;
        if( x->cursor == Nil ) endfile = 1;
        x->cursorlineno++;
    } else {
        if( !x->endfile ) {
            readline(x);
            x->cursor = x->head;
            x->cursorlineno = x->headlineno;
        } else {
            endfile = 1;
        }
    }
}

int backtrack(x)        /* causes the current position of stream x to become */
    Stream *x;          /* that of the last mark operation.  i.e. the current */
{                       /* line when the stream was marked last becomes the */
    int rval;           /* new cursor.  return is the number of lines from */
                        /* the new cursor to the old cursor inclusive. */
    rval = x->cursorlineno - x->headlineno + 1;
    x->cursor = x->head;
    x->cursorlineno = x->headlineno;
    endfile = (Endstream(A) || Endstream(B));
    return(rval);
}

void checkfullmatch(x,y,match)  /* from the current poistions in x and y,    */
    Stream *x,*y; int *match;   /* which match, make sure that the next      */
{                               /* MinLinesForMatch-1 also match, or set     */
    int n;                      /* match to false. */
    Line *savexcur,*saveycur;
    int  savexline,saveyline;

    savexcur = x->cursor;               saveycur = y->cursor;
    savexline= x->cursorlineno;         saveyline= y->cursorlineno;

    *match = comparelines();
    n = MinLinesForMatch - 1;

    while( *match && n-- ) {
        movecursor(x);
        movecursor(y);
        *match = comparelines();
    }
    x->cursor = savexcur;               y->cursor = saveycur;
    x->cursorlineno = savexline;        y->cursorlineno = saveyline;
}

void search(x,y,match)          /* look one line ahead on stream y and search */
    Stream *x,*y; int *match;   /* for that line backtracking on stream x */
{
    int count;                  /* number of lines backtracked on x */

    movecursor(y);
    count = backtrack(x) - 1;
    checkfullmatch(x,y,match);
    while( count && !(*match) ) {
        movecursor(x);
        count--;
        checkfullmatch(x,y,match);
    }
}

void writetext(x)
    Stream *x;
{
    Line *p,*q;         int  lineno;
    char ac;

    ac = (x == &A) ? 'a' : 'b';
    p = x->head;  q = x->cursor;  lineno = x->headlineno;
    while( p != Nil  &&   p != q ) {
        writeoneline(ac, lineno, p);
        p = p->nextline;
        lineno++;
    }
    if( p == Nil ) printf(" *** EOF ***");
}

void writepairs(pa,pb, la,lb)   /* writes from head to cursor like writetext */
    Line *pa,*pb;               /* but writes from both files at once, */
    int  la,lb;                 /* comparing columns within lines, and marks */
{                               /* unequal columns */
    int col,maxcol;
    char *ai,*bi;               /* pointers to the images */

    do {
        writeoneline('a', la, pa);
        writeoneline('b', lb, pb);

        maxcol= (pa->length > pb->length) ? pa->length : pb->length;
        ai = pa->image;
        bi = pb->image;
        for(col=0;col<Prespace;col++) putchar(' ');
        for(col=0;col<maxcol;col++) {
            if( *ai != *bi )
                putchar('^');
            else
                (*ai == '\t') ? putchar('\t') : putchar(' ');
            if( *ai ) ai++;             /* bump if non-null */
            if( *bi ) bi++;             /* bump if non-null */
        }
        putchar('\n');
        pa = pa->nextline; la++;
        pb = pb->nextline; lb++;
    } while( pa != A.cursor && pa != Nil);
}

void writelineno(x)
    Stream *x;
{
    int f,l;
    printf("file %s, line",x->name);
    f = x->headlineno;
    l = x->cursorlineno - 1;
    if( f == l )
        printf(" %d",f);
    else
        printf("s %d-%d",f,l);
    if( x->cursor == Nil ) printf(" (before EOF)");
}

void printextratext(x,y)
    Stream *x,*y;
{
    printf("extra text:  on file %s, ",x->name);
    if( y->head == Nil )
        printf("before EOF on file %s\n",y->name);
    else
        printf("between line %d and %d of file %s\n",
                y->headlineno-1, y->headlineno, y->name);
    putchar('\n');
    writetext(x);
    if( body ) putchar('\n');
}

void printmismatch()
{
    int ea,eb;

    /* output some kind of header */
    putchar('\n');
    for(ea=0 ; ea < Prespace; ea++) putchar(' ');
    for(ea=78; ea > Prespace; ea--) putchar('*'); putchar('\n');

    ea = (A.head == A.cursor);          /* determine if cursor is head  */
    eb = (B.head == B.cursor);          /* i.e. empty queue */

    if( ea || eb ) {                    /* if 1 file is done */
        if( ea )
            printextratext(&B,&A);
        else
            printextratext(&A,&B);
        return;
    }

    /* mismatch location banner */
    printf("Mismatch: "); writelineno(&A); printf(" not equal to ");
    writelineno(&B); printf(":\n\n");

    /* the mismatch itself */
    if( MarkUnequalColumns >= (ea=A.cursorlineno-A.headlineno) &&
                        ea == B.cursorlineno-B.headlineno ) {
        writepairs(A.head,B.head,A.headlineno,B.headlineno);
        return;
    }

    writetext(&A);
    for(ea=0 ; ea < Prespace; ea++) putchar(' ');
    for(ea=60; ea > Prespace; ea--) putchar('='); putchar('\n');
    writetext(&B);
    if( body ) putchar('\n');
}

void findmismatch(match)
    int *match;
{
    do {
        if( body ) {
            if(!bodyp)
                printf("      |%s\n",A.head->image);
            else
                bodyp = 0;
        }
        movecursor(&A); movecursor(&B);
        Mark(&A); Mark(&B);
        *match=comparelines();
    } while(!endfile && *match);
}

void findmatch(match)
    int *match;
{
    int advanceb=1;             /* toggle one-line lookahead between streams */

    do {
        if( advanceb = (endfile) ? Endstream(A) : !advanceb )
            search(&A,&B,match);
        else
            search(&B,&A,match);
    } while( !(*match) );
    printmismatch();
    if(body && A.cursor != Nil) bodyp = printf("      |%s\n",A.cursor->image);
}

void comparefiles()
{
    int match=1;                /* match flag -- assume initial match */
    do {
        if( match )
            findmismatch(&match);
        else {
            same = 0;
            findmatch(&match);
        }
    } while( !endfile || !match );
    if( body && match && A.cursor != Nil ) {
        A.cursor = A.cursor->nextline;
        while( A.cursor != Nil ) {
            printf("      |%s\n",A.cursor->image);
            A.cursor = A.cursor->nextline;
        }
    }
    return;
}

initstream( x, fname )
    Stream *x;  char *fname;
{
    strcpy(x->name,fname);
    if( (x->fileS = fopen(x->name,"r")) == NULL )
        { printf("Unable to open File: \"%s\"\n",x->name); return(1); }
    if( filelength( x->fileS ) == 0L )      /* dg version removes fileno */
        { printf("File has 0 length: \"%s\"\n",x->name); return(1); }
    x->cursor = x->head = x->tail = Nil;
    x->cursorlineno = x->headlineno = x->taillineno = 0;
    x->endfile = 0;
    return(0);
}

int fxstat( x, xdat )
    Stream *x;  Fdata *xdat;
{      
    struct stat buf; struct tm *xt; time_t *cl;
    fstat(x->fileS,&buf);
    cl= &(buf.st_ctime);
    xt=localtime(cl);
    xdat->cyear=xt->tm_year;
    xdat->cmonth =xt->tm_mon+1;
    xdat->cday   =xt->tm_mday;
    xdat->chour  =xt->tm_hour;
    xdat->cminute=xt->tm_min;
    xdat->csecond=xt->tm_sec;
    cl= &(buf.st_mtime);
    xt=localtime(cl);
    xdat->myear=xt->tm_year;
    xdat->mmonth =xt->tm_mon+1;
    xdat->mday   =xt->tm_mday;
    xdat->mhour  =xt->tm_hour;
    xdat->mminute=xt->tm_min;
    xdat->msecond=xt->tm_sec;
    xdat->flen=(long)buf.st_size;
    return(0);
}

main( argc, argv )
    int argc;  char *argv[];
{
    int error_flag,badarg=0,filea=0,fileb=0;
    int iarg,swval;

    /* determine the switches and files */
    for(iarg=1; iarg<argc; iarg++ ) {
        strupr(argv[iarg]);
        if( *argv[iarg] == '-' ) {
            if(!strncmp(argv[iarg],"-LC=",4)) {
                swval = atoi(argv[iarg]+4);
                if(swval < 1 || swval > DLineLength) {
                    fprintf(stderr,"Value of -LC= not between 1 and %d\n",
                        DLineLength);
                    badarg++;
                } else {
                    LineLength = swval;
                }
            }
            if(!strncmp(argv[iarg],"-MS=",4)) {
                swval = atoi(argv[iarg]+4);
                if(swval < 1 ) {
                    fprintf(stderr,"Value of -ML= must greater than 1\n");
                    badarg++;
                } else {
                    MinLinesForMatch = swval;
                }
            }
            if(!strncmp(argv[iarg],"-MC=",4)) {
                swval = atoi(argv[iarg]+4);
                if(swval < 0 ) {
                    fprintf(stderr,"Value of -MC= must greater than 2\n");
                    badarg++;
                } else {
                    MarkUnequalColumns = swval;
                }
            }
            if(!strcmp(argv[iarg],"-XT")) xtabs=1;
            if(!strcmp(argv[iarg],"-WS")) wsp=1;
            if(!strcmp(argv[iarg],"-BODY")) body=1;
            continue;
        }
        if(!filea)
            filea = iarg;
        else
            if(!fileb)
                fileb = iarg;
            else
                badarg++;
    }

    /* check for proper arguments */
    if( badarg || !filea || !fileb ) {
        if(badarg) fprintf(stderr,"Bad or invalid arguments/switches\n\n\n");
        fprintf(stderr,"COMPARE FileA FileB [-LC=n] [-MS=n] [-MC=n] [-BODY] [-XT] [-WS]\n\n");
        fprintf(stderr,"   FileA and FileB are the pathnames to the files\n");
        fprintf(stderr,"   to be compared.  The resulting output may be\n");
        fprintf(stderr,"   redirected.\n\n");
        fprintf(stderr,"-LC=n  optional switch to set the Last Column\n");
        fprintf(stderr,"       to be considered significant in the compare.\n");
        fprintf(stderr,"       (Default and max allowed = %d)\n",DLineLength);
        fprintf(stderr,"-MS=n  optional switch to set the Match Size or\n");
        fprintf(stderr,"       number of matching consecutive lines to end\n");
        fprintf(stderr,"       a mismatch (Default = 4 and must be > 1).\n");
        fprintf(stderr,"-MC=n  optional switch to set limit on difference\n");
        fprintf(stderr,"       marking. (Default = 5 and must be >= 0).\n");
        fprintf(stderr,"-BODY  optional switch to include printout of the\n");
        fprintf(stderr,"       lines common to files as well as differences.\n");
        fprintf(stderr,"-XT    optional switch to expand tabs on normal\n");
        fprintf(stderr,"       stops, making tabs and blanks equivalent.\n");
        fprintf(stderr,"-WS    optional switch to expand all consecutive\n");
        fprintf(stderr,"       whitespace to a single blank.\n");
        exit(1);
    }

    /* bring in filenames and initialize the file streams. */
    error_flag  = initstream( &A, argv[filea] );
    error_flag += initstream( &B, argv[fileb] );
    if( error_flag ) {printf("Comparison aborted.\7\n"); exit(2);}

    error_flag  = fxstat( &A, &Adat );
    error_flag += fxstat( &B, &Bdat );
    if( error_flag ) {
        printf("Error obtaining file statistics.\n");
        printf("Comparison aborted.\7\n");
        exit(2);
    }

    /* set remaining global constants */
    LineLengthR = LineLength + 1;

    /* display the selected options */
    printf("FileA: %s\n",A.name);
    printf("       Created : %2.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d\n",
        Adat.cmonth,Adat.cday,(Adat.cyear%100), Adat.chour,Adat.cminute,
        Adat.csecond);
    printf("       Modified: %2.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d\n",
        Adat.mmonth,Adat.mday,(Adat.myear%100), Adat.mhour,Adat.mminute,
        Adat.msecond);
    printf("       Length  : %d\n",Adat.flen);

    printf("FileB: %s\n",B.name);
    printf("       Created : %2.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d\n",
        Bdat.cmonth,Bdat.cday,(Bdat.cyear%100), Bdat.chour,Bdat.cminute,
        Bdat.csecond);
    printf("       Modified: %2.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d\n",
        Bdat.mmonth,Bdat.mday,(Bdat.myear%100), Bdat.mhour,Bdat.mminute,
        Bdat.msecond);
    printf("       Length  : %d\n",Bdat.flen);

    printf("Compare options:\n");
    printf("  Last Column (to  compare): %d\n",LineLength);
    printf("  Match Size  (to  rematch): %d\n",MinLinesForMatch);
    printf("  Mark Column (limit value): %d\n",MarkUnequalColumns);
    printf("  Body expansion           : %s\n",(body)?"ON":"OFF");
    printf("  Tab expansion            : %s\n",(xtabs)?"ON":"OFF");
    printf("  White space equivalence  : %s\n",(wsp)?"ON":"OFF");
    putchar('\n');              /* skip line after options */

    comparefiles();

    /* output the summary information */
    if( same ) {
        printf("%d lines read, no differences.\n",A.cursorlineno-1);
        error_flag = 0;
    } else {
        printf("\nFiles are different.\n");
        error_flag = 1;
    }
    if( linestoolong ) {
        printf("\n%d lines were too long.  The lines were\n",linestoolong);
        printf("not compared past the %dth character.\n",LineLength);
        error_flag = 2;
    }
    exit(error_flag);
}
