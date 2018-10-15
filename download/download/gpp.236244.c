#include "vopl.h"
#include "vogle.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef PC
#define rindex	strrchr
#else
#include <strings.h>
#endif
/*
--------------------------------------------------------------------------------
Calls:

_centertext  _clear  _clipping  _color  _drawchar
_drawstr     _font   _getkey    _move2  _strlength
_textsize    _vexit  _viewport  _vinit

_vp_adjustscale  _vp_annotate       _vp_axistitle  _vp_degree      _vp_drawaxes2
_vp_drawtitle    _vp_endslopes      _vp_fit        _vp_graphtitle  _vp_gridspacing
_vp_marker       _vp_markerspacing  _vp_newm1      _vp_plot2       _vp_range
_vp_scaling

--------------------------------------------------------------------------------
Calls:
*/


#define MAXPNTS	1048576

#ifdef PC
#define DEFAULTDEV "hercules"
#else
#define DEFAULTDEV "x11"
#endif

#define	VERSION	"678-983-132773-1.1"

/*
 * graph types
 */
#define	X		1
#define	Y		2
#define	Z		4

/*
 * single graph structure
 */
typedef struct gr {
	int		type;
	char		*legend;
	int		npnts;
	float		*x, *y, *z;
	struct gr	*nxt;
} graph;

extern graph	*gp;

extern char	device[], fontname[];

extern char	*xlabel, *ylabel, *zlabel;

extern int	ngraphs, do_legend, uxscale, uyscale, uzscale;

extern FILE	*infile;

extern	float	wholescale;

graph	*gp;

int	ngraphs = 0,
	do_legend = 0,
	points_only = 0,
	uxscale = 0,
	uyscale = 0,
	uzscale = 0;

char	*xlabel,
	*ylabel,
	*zlabel,
	*title,
	fontname[100],			/* name of the font we use */
	device[20];			/* name of the device we use */

static	char	*myname;		/* Me */

FILE	*infile;			/* current input file */

float	wholescale = 1.0;

/*
 * usage
 *
 *	Print a usage message
 */
void usage(){
	fprintf(stderr, "\nUsage: %s [-X] [-Y] [-x <min> <max>] [-y <min> <max>] \n", myname);
	fprintf(stderr, "       [-S|P|G|l<n>|-s <s0> <sn>]\n");
	fprintf(stderr, "       [-r<fact>] [-m<n>] [-g<n>] [-p<n>] [-L] [-n]\n");
	fprintf(stderr, "       [-d<device>] [-f<fontname>] [-|<filename>]\n\n");

	fprintf(stderr, "Where:\n");
	fprintf(stderr, "-X, -Y 	specifies log scaling of x and y axes\n");
	fprintf(stderr, "-x <min> <man> specifies optional min and max  of x axis\n");
	fprintf(stderr, "-y <min> <man> specifies optional min and max  of y axis\n");
	fprintf(stderr, "-S 		uses a cardinal spline fit\n");
	fprintf(stderr, "-P 		   power equation fit\n");
	fprintf(stderr, "-G 		   saturated growth fit\n");
	fprintf(stderr, "-l <n>            least squares fit of degree <n>\n");
	fprintf(stderr, "-s <s0> <sn> 	   'clamped' spline fit with endslopes s0 and sn\n");
	fprintf(stderr, "-r <fact>	reduces (enlarges) the plot by fact\n");
	fprintf(stderr, "-m <n>		places markers at every <n> data points\n");
	fprintf(stderr, "-g <n>		places a grid spaced every <n> data points over the plot\n");
	fprintf(stderr, "-p <n>		draws markers only at each <n>th point (ie. no lines)\n");
	fprintf(stderr, "-L 		draws a legend (if any are provided)\n");
	fprintf(stderr, "-n 		means don't draw any axis anotation\n");
	fprintf(stderr, "-d <device> 	uses VOGLE device <device>\n");
	fprintf(stderr, "-f <fontname> 	uses VOGLE font <fontname>\n");
	fprintf(stderr, "-              uses standard input for input\n");
	fprintf(stderr, "<filename>          filename for input\n");
	fprintf(stderr, "-v            	print the version number\n");
	
	exit(1);
}

/*
 * getone
 *
 *	Returns one floating point number form the argument list.
 */
float
getone(c, arg)
	char c, *arg;
{
	float	a;

	if (sscanf(arg, "%f", &a) != 1) {
		fprintf(stderr,"-%c option expects one number\n", c);
		usage();
	}
	return (a);
}

/*
 * gettwo
 *
 *	grabs two floating point numbers from the argument list
 */
static void
gettwo(argv, c, a, b)
	char	**argv, c;
	float	*a, *b;
{	
	if (*(argv + 2) != (char *)NULL) {
		if (sscanf(*(++argv), "%f", a) != 1) {
			fprintf(stderr, "-%c option expects two numbers\n", c);
			usage();
		}
		if (sscanf(*(++argv), "%f", b) != 1) {
			fprintf(stderr, "-%c option expects two numbers\n", c);
			usage();
		}
	} else {
		fprintf(stderr, "-%c option expects two numbers\n", c);
		usage();
	}
}

/*
 * doargs
 *
 * 	Interpret command line args.
 */
void
doargs(argc, argv)
	int	argc;
	char	**argv;
{
	float	a, b;
	char	c;
	int	i, gotinfile = 0;

	device[0] = '\0';
	fontname[0] = '\0';

#ifdef PC
	if ((myname = rindex(argv[0],'\\')) == NULL)
#else
	if ((myname = rindex(argv[0],'/')) == NULL)
#endif
		myname = argv[0];
	else
		*myname++;

#ifdef PC
	*rindex(myname,'.') = '\0';
#endif

	while (argc > 1) 
		if (*(*++argv) == '-')
			switch(*(*argv+1)) {
			case 'x':
			case 'y':
			case 'z':
				c = *(*argv+1);
				gettwo(argv, c, &a, &b);
				argc -= 3;
				argv += 2;
				vp_range(a, b, c);
				if (c == 'x')
					uxscale = 1;
				else if (c == 'y')
					uyscale = 1;
				else
					uzscale = 1;
				break;
			case 'X': 
				vp_scaling(1, 'x');
				argc--;
				break;
			case 'Y': 
				vp_scaling(1, 'y');
				argc--;
				break;
			case 's':
				vp_fit(CUBIC_SPLINE);
				gettwo(argv, 's', &a, &b);
				vp_endslopes(a, b);
				argc -= 3;
				argv += 2;
				break;
			case 'r':
				if (*(*argv + 2)) {
					a = getone('r', *argv + 2);
					argc--;
				} else {
					a = getone('r', *(++argv));
					argc -= 2;
				}
				/*graphscale(a);*/
				wholescale = a;
				break;
			case 'l':
				vp_fit(LEAST_SQUARE);
				if (*(*argv + 2) != 0)
					vp_degree(atoi(*argv + 2));
				argc--;
				break;
			case 'S':
				vp_fit(CUBIC_SPLINE);
				argc--;
				break;
			case 'P':
				vp_fit(POWER_EQN);
				argc--;
				break;
			case 'G':
				vp_fit(SGR_FIT);
				argc--;
				break;
			case 'm':
				if ((i = atoi(*argv + 2)) <= 0)
					i = 1;

				vp_markerspacing(i);
				argc--;
				break;
			case 'p':
				if ((i = atoi(*argv + 2)) <= 0)
					i = 1;

				vp_markerspacing(i);
				vp_fit(NO_LINES);
				argc--;
				break;
			case 'n':
				vp_annotate("", 'x');
				vp_annotate("", 'y');
				vp_annotate("", 'z');
				argc--;
				break;
			case 'v':
				/* Print the version of gpp */
				printf("This is version %s of gpp\n", VERSION);
				exit(0);
			case 'g':
				if ((i = atoi(*argv + 2)) <= 0)
					i = 1;

				vp_gridspacing(i, 'x');
				vp_gridspacing(i, 'y');
				vp_gridspacing(i, 'z');
				argc--;
				break;
			case 'L':
				do_legend = 1;
				argc--;
				break;
			case 'f' :
				if (*(*argv+2)) {
					strcpy(fontname, *argv + 2);
					argc--;
				} else {
					strcpy(fontname, *(++argv));
					argc -= 2;
				}
				break;
			case 'd':
				if (*(*argv + 2)) {
					strcpy(device,*argv + 2);
					argc--;
				} else {
					strcpy(device,*(++argv));
					argc -= 2;
				}
				break;
			case 0:			/* single - read stdin */
				gotinfile = 1;
				infile = stdin;
				argc--;
				break;
			default:
				fprintf(stderr, "Unknown flag: %s\n",*argv);
				usage();
				exit(-1);
			}
		else {   	/* the input file */
			if ((infile = fopen(*argv,"r")) == NULL) {
				fprintf(stderr, "Can't open: %s\n",*argv);
				usage();
				exit(-1);
			} else {
				gotinfile = 1;
				argc--;
			}
		}

	if (!gotinfile) 
		usage();

	if (!device[0]) 
		strcpy(device, DEFAULTDEV);
}


char	*fmttable[] = {
	"",
	"%f ",
	"%f ",
	"%f %f ",
	"%f ",
	"%f %f ",
	"%f %f ",
	"%f %f %f "
};

extern	float	*vp_newm1();

/* get_line -- reads in a file from file in.  */
char *get_line( FILE *in) {
	char	*p, line[BUFSIZ]; 
	int	c;

	p = line;

	while ((c = getc(in)) != '\n') {
		if (feof(in))
			return((char *)NULL);
		*p++ = c;
	}

	*p = 0;

	p = (char *)malloc(p - line + 1);
	strcpy(p, line);

	return(p);
}

/*
 * decode
 *
 *	converts string type into its X, Y, and Z code.
 */
int
decode(type)
	char	*type;
{
	int	itype;
	char	*sp;

	itype = 0;

	for (sp = type; *sp; sp++)
		switch (*sp) {
		case 'x':
			itype |= X;
			break;
		case 'y':
			itype |= Y;
			break;
		case 'z':
			itype |= Z;
			break;
		default:
			fprintf(stderr, "gpp: bad axes/type descriptor\n");
			exit(1);
		}

	return(itype);
}

/*
 * readgraphs
 *
 *	read in the graphs
 */
void
readgraphs()
{
	int		type;
	graph		*p, *lp;
	char		stype[4], *fmt, *title;
	int		atype, c;
	float		x, y, z, *xaxis, *yaxis, *zaxis;

	ngraphs = 0;

	/*  First we read the title */

	if ((title = get_line(infile)) == NULL) {
		fprintf(stderr,"gpp: EOF when title was expected.\n");
		exit(1);
	}

	vp_graphtitle(title);

	/* next the type */

	if (fscanf(infile, "type %s", stype) != 1) {
		fprintf(stderr,"gpp: EOF when type expected.\n");
		exit(1);
	}

	getc(infile);		/* vp_skip single newline */

	type = decode(stype);

	if ((type & X) && (xlabel = get_line(infile)) == NULL) {
		fprintf(stderr,"gpp: EOF when x-axis label expected.\n");
		exit(1);
	}
	if ((type & Y) && (ylabel = get_line(infile)) == NULL) {
		fprintf(stderr,"gpp: EOF when y-axis label was expected.\n");
		exit(1);
	}
	if ((type & Z) && (zlabel = get_line(infile)) == NULL) {
		fprintf(stderr,"gpp: EOF when z-axis label was expected.\n");
		exit(1);
	}

	/*  Now we should be reading coordinates */

	xaxis = (float *)NULL;
	yaxis = (float *)NULL;
	zaxis = (float *)NULL;

	p = gp = (graph *)malloc(sizeof(graph));
	if (gp == (graph *)NULL) {
		fprintf(stderr, "gpp: malloc returns NULL.\n");
		exit(1);
	}

	if ((gp->legend = get_line(infile)) == NULL) {
		fprintf(stderr,"gpp: EOF when legend was expected.\n");
		exit(1);
	}

	while (!feof(infile)) {
		if (fscanf(infile, "axes %s ", stype) != 1) {
			fprintf(stderr,"EOF when axes expected.\n");
			exit(1);
		}

		p->x = xaxis;
		p->y = yaxis;
		p->z = zaxis;

		atype = decode(stype);

		fmt = fmttable[atype];

		switch (atype) {
		case X:
			xaxis = p->x = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &x) != 0) {
				p->x[p->npnts++] = x;
			}
			break;
		case Y:
			yaxis = p->y = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &y) != 0) {
				p->y[p->npnts++] = y;
			}
			break;
		case Z:
			zaxis = p->z = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &z) != 0) {
				p->z[p->npnts++] = z;
			}
			break;
		case X | Y:
			xaxis = p->x = vp_newm1(MAXPNTS);
			yaxis = p->y = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &x, &y) != 0) {
				p->x[p->npnts] = x;
				p->y[p->npnts++] = y;
			}
			break;
		case X | Z:
			xaxis = p->x = vp_newm1(MAXPNTS);
			zaxis = p->z = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &x, &z) != 0) {
				p->x[p->npnts] = x;
				p->z[p->npnts++] = z;
			}
			break;
		case Y | Z:
			yaxis = p->y = vp_newm1(MAXPNTS);
			zaxis = p->z = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &y, &z) != 0) {
				p->y[p->npnts] = y;
				p->z[p->npnts++] = z;
			}
			break;
		case X | Y | Z:
			xaxis = p->x = vp_newm1(MAXPNTS);
			yaxis = p->y = vp_newm1(MAXPNTS);
			zaxis = p->z = vp_newm1(MAXPNTS);
			p->npnts = 0;
			while (fscanf(infile, fmt, &x, &y, &z) != 0) {
				p->x[p->npnts] = x;
				p->y[p->npnts++] = y;
			}
			break;
		default:
			fprintf(stderr, "gpp: readgraphs - internal error.\n");
			exit(1);
		}
 
		if (fscanf(infile, "plot %s", stype) == 1) {
			p->type = decode(stype);
			if ((p->nxt = (graph *)malloc(sizeof(graph))) == (graph *)NULL) {
				fprintf(stderr, "readgraphs: malloc returns NULL\n");
				exit(1);
			}
			lp = p;
			p = p->nxt;

			/*
			 * vp_skip newline after plot command
			 */
			while ((c = getc(infile)) != '\n' && !feof(infile))
				;

			p->legend = get_line(infile);

			ngraphs++;

			/*
			 * vp_skip extra characters before next axis keyword
			 */
			while ((c = getc(infile)) != 'a' && !feof(infile))
				;

			if (!feof(infile))
				ungetc(c, infile);
		}
	}

	/* finish off the last one */
	lp->nxt = NULL;
}


extern	float	strlength();

/*
 * vp_drawlegend
 *
 *	Draw a legend for gpp
 */
void vp_drawlegend() {
	float	ydiff, x, y;
	char	buf[80];
	int	i = 0;
	graph	*p;

	ydiff = 2 * TEXTHEIGHT;
	y = YMAX;
	x =  XMAX + 3 * TEXTWIDTH;

	vogle_clipping(0);
	vogle_centertext(1);

	vogle_textsize(TEXTWIDTH * 1.2, TEXTHEIGHT * 1.2);

	/*
 	 * First draw all the marker symbols
	 */
	for (i = 0; i < ngraphs; i++) {
		vogle_color((i % 7) + 1);
		vogle_move2(x, y);
		vogle_drawchar('a' + i);
		y -= ydiff;
	}

	/*
	 * Now fill in the legends
	 */

	if (fontname[0])
		vogle_font(fontname);
	else
		vogle_font("futura.l");

	vogle_textsize(TEXTWIDTH * 1.2, TEXTHEIGHT * 1.2);

	y = YMAX;
	i = 0;
 
	for (p = gp; p != (graph *)NULL; p = p->nxt) {
		vogle_color((i++ % 7) + 1);
		if (strlen(p->legend) != 0) {
			buf[0] = buf[1] = buf[3] = ' ';
			buf[2] = '-';
			buf[4] = '\0';
			strcat(buf, p->legend);
			vogle_move2(x + vogle_strlength(buf) / 2, y);
			vogle_drawstr(buf);
			y -= ydiff;
		}
	}

	vogle_centertext(0);
	vogle_clipping(1);
}
/*
 *   gpp
 *  
 *   Graph plotting programme. 
 *
 */

/*
 * main driver
 */
main(argc, argv)
	int	argc;
	char	**argv;
{
	int	i;
	char	cm[2];
	graph	*p;

	doargs(argc, argv);

	vogle_vinit(device);
	vogle_linewidth(30);

	vogle_viewport(-wholescale, wholescale, -wholescale, wholescale);

	vogle_color(BLACK);
	vogle_clear();

	vogle_color(WHITE);

	if (fontname[0])
		vogle_font(fontname);

	readgraphs();
	
	if (!uxscale || !uyscale /* || !uzscale */) 
		for (p = gp; p != (graph *)NULL; p = p->nxt) {
			if (!uxscale)
				vp_adjustscale(p->x, p->npnts, 'x');

			if (!uyscale)
				vp_adjustscale(p->y, p->npnts, 'y');

			/*if (!uzscale)
				vp_adjustscale(p->z, p->npnts, 'z');*/
		}

	vp_axistitle(xlabel, 'x');
	vp_axistitle(ylabel, 'y');
	/*vp_axistitle(zlabel, 'z');*/

	vp_drawaxes2();
	vp_drawtitle();

	i = 0;
	cm[1] = '\0';

	vogle_font("markers");

	for (p = gp; p != (graph *)NULL; p = p->nxt) {
		vogle_color((i % 7) + 1);
		cm[0] = 'a' + (i++);
		vp_marker(cm);
		vp_plot2(p->x, p->y, p->npnts);
	}

	if (do_legend)
		vp_drawlegend();


	vogle_getkey();

	vogle_vexit();
}
