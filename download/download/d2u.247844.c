/***********************************************************************/
/* d2u: convert printable ascii files between Unix and DOS conventions */

/* $Id: d2u.c 1.2 2002/10/17 11:10:56Z Dan Release $ */
/* **********************************************************************
*  Copyright 2001 Purple Sage Computing Solutions, Inc.
*  All Rights Reserved

*   This program is free software; you can redistribute it and/or
*   modify it under the terms of the GNU General Public
*   License as published by the Free Software Foundation; either
*   version 2 of the License, or (at your option) any later version.

*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*   General Public License for more details.

*   You should have received a copy of the GNU General Public
*   License along with this program; if not, write to the Free
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

* To report bugs, suggest enhancements, etc. to the Authors,
* Contact:
*    Purple Sage Computing Solutions, Inc.
*                               send email to dnagle@erols.com
*                                   or fax to 703 471 0684 (USA)
*                                  or mail to 12142 Purple Sage Ct.
*                                             Reston, VA 20194-5621 USA
* **********************************************************************/
/* DOS convention is that an end of line is CR-LF. */
/* Unix convention is that an end of line is newline. */

/* usage: d2u [-d|-u] [-z] [-v] input output */

/* -d convert Unix file to DOS */
/* -u (default) convert DOS file to Unix */
/* -z guarantee last character of DOS file is ^Z,
      guarantee last character of Unix file is not ^Z
      otherwise, ^Z in input is copied or not as-is */
/* -v verbose mode reports character and line counts */
/***********************************************************************/
/* header files */
#include        <stdio.h>
#include        <string.h>
/***********************************************************************/
/* constants */
#define CZ      26              /* DOS eof (cntl-Z) */
#define NL      (int) '\n'      /* Unix end of line */
#define CR      (int) '\r'      /* DOS end of line */
#define LF      (int) '\n'

#define TODOS   1       /* Unix to DOS */
#define TOUNIX  2       /* DOS to Unix */

#define CHKCZ   1       /* process explicit DOS eof */
#define ASISCZ  2       /* process not explicit DOS eof */

#define QUIET   0       /* report no character counts */
#define VERBOSE 1       /* report character counts */

#define MATCH   0       /* special return value from strcmp() */
/***********************************************************************/
/* global variables */
/*---------------------------------------------------------------------*/
                                        /* input & output files */
FILE    *input;                         /* input file */
FILE    *output;                        /* output file */
                                        /* flags indicating command line options */
int processing_direction = TOUNIX;      /* which filter */
int process_cntl_z = ASISCZ;            /* process ^Z or not */
int verbose = QUIET;                    /* print report or not */
                                        /* character and line counts */
int chs_rd = 0;                         /* count chars read */
int chs_pr = 0;                         /* count chars written */
int lns_dn = 0;                         /* count lines */
/***********************************************************************/
                                        /* prototypes */
void dos_to_unix( void);                /* CR-LF to newline */
void unix_to_dos( void);                /* newline to CR-LF */
/***********************************************************************/
                                                         /* procedures */
/*---------------------------------------------------------------------*/
                                /* dos_to_unix() copy CR-LF to newline */
void dos_to_unix(){                                /* CR-LF to newline */
        int c;                               /* character to be copied */
        int prev_c;                            /* look ahead character */
                                           /* start prev_c, c pipeline */
        if( (prev_c = fgetc(input)) == EOF ){             /* check eof */
                fprintf( stderr, "Empty input file\n");
                return;                             /* quit if no work */
        }

        chs_rd++;                                       /* count chars */

        /* copy character by character */
        while( (c = fgetc(input)) != EOF ){             /* read to eof */
                chs_rd++;                               /* count chars */

                /* check for a CR-LF sequence */
                if( (prev_c == CR) && (c == LF) ){       /* found CR-LF */
                        fputc( NL, output);            /* write newline */
                        chs_pr++;                        /* count chars */
                        lns_dn++;                        /* count lines */

                        if( (c = fgetc(input)) == EOF ){ /* reload pipeline */
                                prev_c = c;                  /* set flag */
                                break;                   /* quit at eof */
                        }

                        chs_rd++;
                }else{                                     /* any other */
                        fputc( (char) prev_c, output);    /* write char */
                        chs_pr++;                        /* count chars */
                }
                prev_c = c;                           /* cycle pipeline */
        }

        /* cntl-Z as-is */
        if( process_cntl_z == ASISCZ ){        /* write last character */
                if( prev_c != EOF ){
                        fputc( (char) prev_c, output);     /* write it */
                        chs_pr++;                          /* count it */
                }
        }else if( process_cntl_z == CHKCZ ){            /* guarantee no ^Z */
                if( (prev_c != EOF) && (prev_c != CZ) ){ /* char is not ^Z */
                        fputc( (char) prev_c, output);     /* write it */
                        chs_pr++;                          /* count it */
                }
        }
        return;         /* done dos_to_unix() */
}
/*---------------------------------------------------------------------*/
                                /* unix_to_dos() copy newline to CR-LF */
void unix_to_dos(){                                /* newline to CR-LF */
        int c;                               /* character to be copied */
        int prev_c;
                                        /* copy character by character */
        while( (c = fgetc(input)) != EOF ){             /* read to eof */
                chs_rd++;                               /* count chars */
                if( c == NL ){                           /* if newline */
                        fputc( CR, output);                /* write CR */
                        chs_pr++;                        /* count char */
                        fputc( LF, output);                /* write LF */
                        chs_pr++;                        /* count char */
                        lns_dn++;                        /* count line */
                }else{                               /* any other char */
                        fputc( (char) c, output);        /* write char */
                        chs_pr++;                        /* count char */
                }
                prev_c = c;                   /* check to guarantee ^Z */
        }
                                            /* complain if input empty */
        if( chs_rd == 0 ){                         /* nothing was read */
                fprintf( stderr, "Empty input file\n");
                return;
        }
                                               /* check last character */
        if( process_cntl_z == CHKCZ ){                 /* guarantee ^Z */
                if( prev_c != CZ ){             /* last char is not ^Z */
                        fputc( (char) CZ, output);       /* write char */
                        chs_pr++;                        /* count char */
                }
        }
        return;         /* done unix_to_dos() */
}
/*---------------------------------------------------------------------*/
    /* read command line, process file and optionally print statistics */
int main( int argc, char *argv[]){
                                                              /* local */
        int optind = 1;                     /* start with first option */
                                                          /* begin d2u */
        while( optind < (argc - 2)){                /* process options */

                /* Unix to DOS */
                if( strcmp( argv[ optind], "-d") == MATCH ){ /* opt == -d */
                        processing_direction = TODOS;      /* set flag */
                }

                /* DOS to Unix */
                if( strcmp( argv[ optind], "-u") == MATCH ){    /* opt == -u */
                        processing_direction = TOUNIX;          /* set flag */
                }

                /* process cntl-Z */
                if( strcmp( argv[ optind], "-z") == MATCH ){    /* opt == -z */
                        process_cntl_z = CHKCZ;                 /* set flag */
                }

                /* report character & line counts */
                if( strcmp( argv[ optind], "-v") == MATCH ){    /* opt == -v */
                        verbose = VERBOSE;                      /* set flag */
                }

                /* next option */
                optind++;
        }                                /* process options */

        /* open input file */
        if ((input = fopen( argv[ argc - 2], "rb")) == NULL){
                fprintf( stderr,
                "Cannot open input file: %s\n", argv[ argc - 2]);
                return 1;
        }

        /* open output file */
        if ((output = fopen( argv[ argc - 1], "wb")) == NULL){
                fprintf( stderr,
                "Cannot open output file: %s\n", argv[ argc - 1]);
                return 1;
        }

        /* process DOS file to Unix */
        if( processing_direction == TOUNIX ){
                if( verbose == VERBOSE ){
                        fprintf( stderr, "dos to unix\n");
                }

                dos_to_unix();          /* DOS to Unix */
        }else if( processing_direction == TODOS ){ /* process Unix file to DOS */
                if( verbose == VERBOSE ){
                        fprintf( stderr, "unix to dos\n");
                }

                unix_to_dos();          /* Unix to DOS */
        }

        /* if verbose, report counts */

        if( verbose == VERBOSE ){
                fprintf( stderr, "read: %d, written: %d, lines: %d\n",
                chs_rd, chs_pr, lns_dn);
        }

        /* end of d2u */
        return 0;       /* success */
}
/*---------------------------------------------------------------------*/
/* $Id: d2u.c 1.2 2002/10/17 11:10:56Z Dan Release $ */
/***********************************************************************/
