
/******************************************************************************/
/*
 * @(#) Txedith - Help Utility for Txedit
 */
/*
	Help utility for unix xedit.  unix xedit is a look-alike of
        Xedit - Extended Text Editor Control Data Corporation.
	See source for Txedit.c for complete description.
	
	Put together by R.K.Wonders - Westinghouse Corporate Computer Services

*/
/*  #define msdos 1   for microsoft c   */
/*  #define SUN 1 for SUN  or use cc Txedit.c -DSUN on compile */
#ifdef cyber
#include <stdio_h>
#include <string_h>
#include <ctype_h>
#else
#ifdef apollo
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "/sys/ins/base.ins.c"
#include "/sys/ins/error.ins.c"
#else
/*   default includes  */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#endif
#endif
  

/* ***************************** contants ****************************** */

#define MaxString 256                   /* maximum significant line length */
#define MaxStringR (MaxString+1)        /* max for allocate and reads */
#define MaxCommandWidth MaxString       /* set max command width*/
#define max_cmd_name_length 8

enum string_option {no_strings, optional_string, one_string, two_strings};
enum  command_processors  {CMD_print, CMD_locate, CMD_next, CMD_stop,
      CMD_bottom, CMD_delimit, CMD_change, CMD_insert, CMD_insertb, CMD_add,
      CMD_delete, CMD_replace, CMD_z_command, CMD_changes, CMD_copy, CMD_cut,
      CMD_copy_delete, CMD_read, CMD_modify, CMD_yqmodify, CMD_qmodify,
      CMD_where, CMD_help, CMD_file, CMD_quit, CMD_top, CMD_jump, CMD_dup,
      CMD_brief, CMD_restore, CMD_wmargin, CMD_rmargin, CMD_lmargin,
      CMD_verify, CMD_locaten, CMD_system, CMD_exit, CMD_qcut, CMD_nobells,
      CMD_deftab, CMD_tabs, CMD_expand, CMD_listab, CMD_xedit, CMD_scan_mode,
      CMD_veto, CMD_lveto, CMD_paste, CMD_truncate, CMD_lcopy, CMD_error};
enum window_accesss {no_window, anchored, windowed};
enum input_mode {read_commands, read_data, read_special, read_undefined};
#define no_error 0
#define non_fatal 1
#define fatal 2
#define TRUE 1
#define FALSE 0

/* ************************** new data types ***************************** */

typedef struct {                /* Stream definition */
    char name[80];              /* name of file */
    FILE *fileS;                /* stream */
    int  filelen;               /* filelen */
    int  endfile;               /* EOF flag */
} Stream;

typedef struct {                        /* command_descriptor */
    char long_form[max_cmd_name_length + 1];  /* name + nul */
    char short_form[max_cmd_name_length + 1]; /* name + nul */
    enum string_option string_op;                      /* string_option*/
    int  count;                          /*   boolean */
    int  file_name;                      /*   boolean */
    int  special_parameters ;            /*   boolean */
    enum command_processors processor;   /*   command_processors */
} command_descriptor;

typedef struct {
  long skip_count;
  long iteration_count;
  int start_at_top;
  int input_from_command_line;
  int verify_command;
  enum window_accesss window_access;
  enum command_processors processor;   /*   command_processors */
} command_actions;

typedef struct{
  int left;
  int right;
} margin;

/* ********************* global data allocation ************************** */
Stream  I,O;
char    cdate[]="V910823";
int     LineLength=MaxString;   /* maximum significant line length */
int     LineLengthR;            /* maximum line length or read */
int     end_of_input = 0;       /* flag end of input encountered*/
char    currentline[MaxStringR];  /* current line string*/
char    command_string[MaxCommandWidth];
int     si,end_of_line;
char    cc[MaxCommandWidth],c;

command_descriptor command_table[] = {
   {"add     ", "a       ", no_strings, TRUE, FALSE, FALSE, CMD_add},
   {"bottom  ", "b       ", no_strings, FALSE, FALSE, FALSE, CMD_bottom},
   {"brief   ", "br      ", no_strings, FALSE, FALSE, TRUE, CMD_brief},
   {"change  ", "c       ", two_strings, TRUE, FALSE, FALSE, CMD_change},
   {"changes ", "cs      ", two_strings, TRUE, FALSE, FALSE, CMD_changes},
   {"copy    ", "copy    ", optional_string, TRUE, TRUE, FALSE, CMD_copy},
   {"copyd   ", "copyd   ", optional_string,TRUE,TRUE,FALSE, CMD_copy_delete},
   {"cut     ", "cut     ", no_strings, FALSE, FALSE, FALSE, CMD_cut},
   {"deftab  ", "dt      ", no_strings, FALSE, FALSE, TRUE, CMD_deftab},
   {"delete  ", "d       ", optional_string, TRUE, FALSE, FALSE, CMD_delete},
   {"delimit ", "del     ", no_strings, FALSE, FALSE, TRUE, CMD_delimit},
   {"duplicat", "dup     ", no_strings, TRUE, FALSE, FALSE, CMD_dup},
   {"edit    ", "edit    ", no_strings, FALSE, TRUE, FALSE, CMD_xedit},
   {"end     ", "e       ", no_strings, FALSE, TRUE, FALSE, CMD_quit},
   {"exit    ", "exit    ", no_strings, FALSE, FALSE, FALSE, CMD_exit},
   {"expand  ", "expand  ", no_strings, TRUE, FALSE, FALSE, CMD_expand},
   {"file    ", "f       ", no_strings, FALSE, TRUE, FALSE, CMD_file},
   {"help    ", "h       ", no_strings, FALSE, FALSE, FALSE, CMD_help},
   {"insert  ", "i       ", no_strings, TRUE, FALSE, FALSE, CMD_insert},
   {"insertb ", "ib      ", no_strings, TRUE, FALSE, FALSE, CMD_insertb},
   {"jump    ", "j       ", no_strings, TRUE, FALSE, FALSE, CMD_jump},
   {"lcopy   ", "lcopy   ", optional_string, TRUE, TRUE, FALSE, CMD_lcopy},
   {"lineveto", "lveto   ", no_strings, FALSE, FALSE, TRUE, CMD_lveto},
   {"listab  ", "lt      ", no_strings, FALSE, FALSE, FALSE, CMD_listab},
   {"lmargin ", "lm      ", no_strings, FALSE, FALSE, TRUE, CMD_lmargin},
   {"locate  ", "l       ", one_string, TRUE, FALSE, FALSE, CMD_locate},
   {"locaten ", "ln      ", no_strings, TRUE, FALSE, FALSE, CMD_locaten},
   {"modify  ", "m       ", no_strings, TRUE, FALSE, FALSE, CMD_modify},
   {"next    ", "n       ", no_strings, TRUE, FALSE, FALSE, CMD_next},
   {"nobells ", "nb      ", no_strings, FALSE, FALSE, FALSE, CMD_nobells},
   {"paste   ", "paste   ", no_strings, FALSE, FALSE, FALSE, CMD_paste},
   {"print   ", "p       ", no_strings, TRUE, FALSE, FALSE, CMD_print},
   {"qcut    ", "qcut    ", no_strings, FALSE, FALSE, FALSE, CMD_qcut},
   {"qinsert ", "qi      ", no_strings, TRUE, FALSE, FALSE, CMD_insert},
   {"qinsertb", "qib     ", no_strings, TRUE, FALSE, FALSE, CMD_insertb},
   {"qmod    ", "qm      ", no_strings, TRUE, FALSE, FALSE, CMD_qmodify},
   {"qprint  ", "qp      ", no_strings, TRUE, FALSE, FALSE, CMD_print},
   {"quit    ", "q       ", no_strings, FALSE, TRUE, FALSE, CMD_quit},
   {"read    ", "read    ", no_strings, FALSE, TRUE, FALSE, CMD_read},
   {"readonly", "scan    ", no_strings, FALSE, FALSE, TRUE, CMD_scan_mode},
   {"replace ", "r       ", no_strings, TRUE, FALSE, FALSE, CMD_replace},
   {"restore ", "restore ", no_strings, FALSE, FALSE, FALSE, CMD_restore},
   {"rmargin ", "rm      ", no_strings, FALSE, FALSE, TRUE, CMD_rmargin},
   {"system  ", "system  ", no_strings, FALSE, FALSE, TRUE, CMD_system},
   {"stop    ", "stop    ", no_strings, FALSE, FALSE, FALSE, CMD_stop},
   {"tabs    ", "tab     ", no_strings, FALSE, FALSE, TRUE, CMD_tabs},
   {"top     ", "t       ", no_strings, FALSE, FALSE, FALSE, CMD_top},
   {"truncate", "trunc   ", no_strings, TRUE, FALSE, FALSE, CMD_truncate},
   {"verify  ", "v       ", no_strings, FALSE, FALSE, TRUE, CMD_verify},
   {"veto    ", "veto    ", no_strings, FALSE, FALSE, TRUE, CMD_veto},
   {"where   ", "w       ", no_strings, FALSE, FALSE, FALSE, CMD_where},
   {"wmargin ", "wm      ", no_strings, FALSE, FALSE, TRUE, CMD_wmargin},
   {"xedit   ", "xedit   ", no_strings, FALSE, TRUE, FALSE, CMD_xedit},
   {"yqmod   ", "yqm     ", no_strings, TRUE, FALSE, FALSE, CMD_yqmodify},
   {"y       ", "y       ", no_strings, FALSE, FALSE, TRUE, CMD_z_command},
   {"z       ", "z       ", no_strings, FALSE, FALSE, TRUE, CMD_z_command},
   {"error   ", "error   ", no_strings, FALSE, FALSE, FALSE, CMD_error}
};
#define c_table_length  sizeof(command_table) / sizeof(command_table[0])
int command_table_length=c_table_length;

char letters[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
char delimiters[] = "[]<>$%_\'()=;:!\".?/-+#&^~`|@,\\";
int  error_in_command;
int  error_level;
int  terminate_edit;
int  end_of_file;
command_actions standard_actions = {0, 0, FALSE, TRUE,
                                      TRUE, no_window, CMD_error};
command_actions default_actions;
command_actions current_command;
char command_name[max_cmd_name_length + 1];  /* name + nul */
char command_text[MaxStringR];
char input_line[MaxStringR];
char delimiter;
/* *************************  code  ***************************** */
char *strupr(cptr)   /* converts a string to upper case and returns string */
    char *cptr;
{
    char *ptr;
    for(ptr=cptr; *ptr; ptr++) if(islower(*ptr)) *ptr = toupper(*ptr);
    return(cptr);
}

char *strlower(cptr)  /* converts a string to lower case and returns string */
    char *cptr;
{
    char *ptr;
    for(ptr=cptr; *ptr; ptr++) if(isupper(*ptr)) *ptr = tolower(*ptr);
    return(cptr);
}
int pause()
{
	printf("	Enter to continue ");
	getchar();
}

void copysubstr(source,start,count,result)

       char source[],result[]; int start,count;
/*  copy substring of source to new string result
    start is first char position- starting from ZERO
    count is number of characters to copy */
{
        int i;
        i = 0;
        result[0] = '\0';

        if(start >= 0 && strlen(source) > start)
           for(i=0;i<count && source[start+i] != '\0';i++)
             result[i] = source[start+i];

        result[i] = '\0';
}

int c_in_string(c,string)
     char c; char string[MaxStringR];
/*   returns the position of c in string starting FROM 1
     return 0 if not found  */
{
     int i;
     for( i = 0; i < strlen(string); i++){
       if(c == string[i]) return(i+1);
     }
     return(0);
}

int get_next (il,s,delimiter)
      char il[],s[],delimiter;
{
        int i,j;
        char temp_string[MaxStringR];

        i = strlen(il);
        if(il[i-1] != delimiter){
         il[i] = delimiter;
         il[i+1] = '\0';
        }

        i = 0;
        while(il[i] != delimiter){
          i++;
        }
        if(i>0)
         copysubstr(il,0,i,s);
        else
         s[0] = '\0';

        if(i < strlen(il)){
          j = strlen(il) - i;
          copysubstr(il,i+1,j,temp_string);
          strcpy(il,temp_string);
        }
        else
          il[0] = '\0';
}

void error(s,error_level)
  char s[]; int error_level;
{
      if(error_in_command < error_level){
        fprintf(O.fileS,"%s\n",s);
        error_in_command = error_level;
      }
}



int read_next (s,im)
     char s[]; enum input_mode im;
{

      if(!current_command.input_from_command_line && im == read_data){
        read_command_line (s, im);
      }
      else{
        if(strlen(input_line) > 0)
          get_next (input_line, s, delimiter);
        else if(im == read_data)
          read_command_line (s, im);
        else {
          read_command_line (input_line, im);
          get_next (input_line, s, delimiter);
        }
      }
}


read_command_line( string, im )
    char string[]; enum input_mode im;
{
    if(im == read_commands){
      fprintf(O.fileS,"\nXedit-Help: Enter command or 'EXIT': ");
    }
    if(I.fileS != NULL){
     if(fgets(string,MaxStringR,I.fileS)==NULL) end_of_input = 1;
    }
    else end_of_input=1;
    if(!end_of_input) string[strlen(string)-1]='\0';
}

int next_char()
{
   c = cc[si];
   if(c=='\0') end_of_line=TRUE;
   if(si > strlen(cc))
      end_of_line = TRUE;
   else
      si++;
   return;
}

int skip_spaces()
{
        char comma;
        comma = ',';
        cc[strlen(cc)] = '\0';
        while((c == ' ') || (c == comma)){
          if(c == ',') comma = ' ';
          next_char();
        };
}


void show_help()
{
           fprintf(O.fileS," Xedit Commands and Abbreviations\n\n");
           fprintf(O.fileS,"  Add(A)         ");
           fprintf(O.fileS,"  Jump(J)        ");
           fprintf(O.fileS,"  Read           ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Bottom(B)      ");
           fprintf(O.fileS,"  Lcopy          ");
           fprintf(O.fileS,"  Readonly(SCAN) ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Brief(BR)      ");
           fprintf(O.fileS,"  Lineveto(LVETO)");
           fprintf(O.fileS,"  Replace(R)     ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Change(C)      ");
           fprintf(O.fileS,"  Listab(LT)     ");
           fprintf(O.fileS,"  Restore        ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Changes(CS)    ");
           fprintf(O.fileS,"  Lmargin(LM)    ");
           fprintf(O.fileS,"  Rmargin(RM)    ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Copy           ");
           fprintf(O.fileS,"  Locate(L)      ");
           fprintf(O.fileS,"  Stop           ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Copyd          ");
           fprintf(O.fileS,"  Locaten(LN)    ");
           fprintf(O.fileS,"  System         ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Cut            ");
           fprintf(O.fileS,"  Modify(M)      ");
           fprintf(O.fileS,"  Tabs(TAB)      ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Deftab(DT)     ");
           fprintf(O.fileS,"  Next(N)        ");
           fprintf(O.fileS,"  Top(T)         ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Delete(D)      ");
           fprintf(O.fileS,"  Nobells(NB)    ");
           fprintf(O.fileS,"  Truncate(TRUNC)");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Delimit(DEL)   ");
           fprintf(O.fileS,"  Paste          ");
           fprintf(O.fileS,"  Verify(V)      ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Duplicat(DUP)  ");
           fprintf(O.fileS,"  Print(P)       ");
           fprintf(O.fileS,"  Veto           ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  End(E)         ");
           fprintf(O.fileS,"  Qcut           ");
           fprintf(O.fileS,"  Where(W)       ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Expand         ");
           fprintf(O.fileS,"  Qinsert(QI)    ");
           fprintf(O.fileS,"  Wmargin(WM)    ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  File(F)        ");
           fprintf(O.fileS,"  Qinsertb(QIB)  ");
           fprintf(O.fileS,"  Xedit(EDIT)    ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Help(H)        ");
           fprintf(O.fileS,"  Qmod(QM)       ");
           fprintf(O.fileS,"  Y              ");
           fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Insert(I)      ");
           fprintf(O.fileS,"  Qprint(QP)     ");
           fprintf(O.fileS,"  Yqmod(YQM)     ");
             fprintf(O.fileS,"\n");
           fprintf(O.fileS,"  Insertb(IB)    ");
           fprintf(O.fileS,"  Quit           ");
           fprintf(O.fileS,"  Z              ");
             fprintf(O.fileS,"\n");
}



void process_command()
{
      long i,j,ii,jj;
      int found, start, length;
      char newtext[MaxStringR];

          printf("\n");
          switch(current_command.processor){
          case CMD_add :
printf("ADD #      [A]  add\n");
printf("=====\n");
printf("ADD- takes the next line of input and appends it to # lines\n");
printf("starting at the current pointer position.  If the length of the new\n");
printf("lines exceed 256 characters, an informative message is issued and the\n");
printf("line is truncated to that length.\n");
            break;
          case CMD_bottom :
printf("BOTTOM    [B]  bottom\n");
printf("======\n");
printf("BOTTOM- moves the pointer to the last line in the file.\n");
            break;
          case CMD_change :
printf("CHANGE /STRING1/STRING3/ #    [C]  change\n");
printf("==========================\n");
printf("CHANGE- changes all occurances of STRING1 to STRING3 in the first #\n");
printf("lines (starting at the pointer) where STRING1 occurs.  STRING1 and\n");
printf("STRING2 are arbitrary strings of characters and may be of different\n");
printf("length.  If the last delimiter of the change command is omitted, a\n");
printf("delimiter is assumed after the last nonblank character of the command\n");
printf("line and a warning message is issued. If the changed line becomes\n");
printf("longer than 256 characters, it is truncated and an informative message\n");
printf("is issued.\n");
printf("\n");
printf("Other forms of the CHANGE command are-\n");
printf("\n");
printf("CHANGE /STRING1//#    [C]\n");
printf("==========================\n");
printf("CHANGE-  identical to the regular CHANGE,  except that STRING1 is\n");
printf("removed completely.\n");
	pause();
printf("CHANGE //STRING3/ #    [C]\n");
printf("===================\n");
printf("CHANGE- changes the null string to STRING3.  By definition,  XEDIT\n");
printf("assumes the presence of a null string immediately before the first\n");
printf("character which is available for searching. Normally this character is\n");
printf("the first character of the line but can be changed via the 'WMARGIN'\n");
printf("and the use of either the 'A' or 'W' postfix characters. For example-\n");
printf("\n");
printf("     ?? WMARGIN 10,10\n");
printf("     ?? PRINT\n");
printf("     THIS IS AN EXAMPLE.\n");
printf("     ?? C//NOTE- /\n");
printf("     NOTE- THIS IS AN EXAMPLE.\n");
printf("     ?? CW//(INSERT)/\n");
printf("     NOTE- THI(INSERT)S IS AN EXAMPLE.\n");
printf("\n");
printf("CHANGE /STRING1...STRING2/STRING3/ # [C]\n");
printf("====================================\n");
printf("CHANGE-  similar to the regular CHANGE command except that all\n");
printf("characters between (and including) STRING1 and STRING2 are changed to\n");
printf("STRING3.\n");
            break;
          case CMD_changes :
printf("CHANGES /STRING1/STRING3/ #    [CS]  changes\n");
printf("===========================\n");
printf("CHANGES- performs much like the regular CHANGE command except that the\n");
printf("# field takes on a different meaning. In the regular CHANGE command,\n");
printf("all occurances of STRING1 which occur in # lines are changed;  In the\n");
printf("changes command, the first # occurances of STRING1 are changed.\n");
printf("\n");
printf("All forms available under the regular CHANGE command are available\n");
printf("under the CHANGES command.\n");
            break;
          case CMD_copy :
printf("COPY FNAME #  copy\n");
printf("============\n");
printf("COPY- copies # lines from the edited file to file 'FNAME'.  The\n");
printf("pointer is positioned at the last line copied. In verify mode, each\n");
printf("line copied is printed. The first copy onto a file causes it to be\n");
printf("rewound before copying begins. Consecutive copies onto the same file\n");
printf("add information to that file. Note, XEDIT must be able to write on\n");
printf("file 'FNAME'.\n");
printf("\n");
printf("The alternate forms of the command are-\n");
printf("COPY FNAME /STRING/ #\n");
printf("COPY FNAME /STRING1...STRING2/ #\n");
printf("COPY FNAME /STRING1---STRING2/ #\n");
printf("COPY FNAME /---STRING2/ #\n");
printf("These forms of the command allow string searches to be\n");
printf("included in the termination requirement. If a string field is used,\n");
printf("XEDIT will copy all the lines from the current pointer position to the\n");
printf("#th occurance of the string match or to the 'END-OF-FILE',  whichever\n");
printf("occurs first. (For information regarding the string searches, see the\n");
printf("'LOCATE' command.) \n");
            break;
          case CMD_copy_delete :
printf("COPYD FNAME #  copyd\n");
printf("=============\n");
printf("COPYD- copies # lines to file 'FNAME' while deleting the copied\n");
printf("lines. The pointer is positioned after the last line deleted. For more\n");
printf("information regarding this command, see the 'COPY' command.\n");
            break;
          case CMD_cut :
printf("CUT  cut \n");
printf("====\n");
printf("CUT- splits the current line into 2 separate lines and advances the\n");
printf("pointer to the newly created second line.  The line is 'cut' at the\n");
printf("right margin. (See RMARGIN).\n");
printf(" (See QCUT , it prompts for the 'cut' column.\n");
            break;
          case CMD_qcut :
printf("QCUT  qcut \n");
printf("====\n");
printf("QCUT- splits the current line into 2 separate lines and advances the\n");
printf("pointer to the newly created second line.   Prior to the 'cut', col-\n");
printf("umn numbers are printed out, and user input requested.   The 'cut' is\n");
printf("made at the first occurance of the up-arrow character (^) supplied  by\n");
printf("input. If the current line length is less than  the  up-arrow location\n");
printf("or no up-arrow appears, nothing is done.\n");
            break;
          case CMD_delete :
printf("DELETE #    [D]  delete\n");
printf("========\n");
printf("DELETE- deletes # lines starting with the current line. The pointer is\n");
printf("positioned after the last line deleted. If the user is in VERIFY mode,\n");
printf("the deleted lines are printed.\n");
printf("\n");
printf("Additionally, one can specify various string options. If a string is\n");
printf("specified,  XEDIT will delete # lines which match the string\n");
printf("specifications.  Information regarding string searches is detailed\n");
printf("under the 'LOCATE' command. The additional forms are-\n");
printf("\n");
printf("DELETE /STRING/ #    [D]\n");
printf("DELETE /STRING1...STRING2/ #    [D]\n");
printf("DELETE /STRING1---STRING2/ #    [D]\n");
printf("DELETE /---STRING2/ #    [D]\n");
            break;
          case CMD_dup:
printf("DUPLICAT #     [DUP]  duplicat\n");
printf("===========\n");
printf("DUPLICAT- duplicates the current line # times, and advances the  pointer\n");
printf("to the last duplicate line.\n");
            break;
          case CMD_delimit:
printf("DELIMIT c    [DEL]  delimit\n");
printf("=========\n");
printf("DELIMIT- sets the character 'c' as the delimiter to be used to separate\n");
printf("lines of input.  'c' may be any character except space, comma, or\n");
printf("alphabetics. If no argument is specified, the delimiter character is\n");
printf("cleared. For example-\n");
printf("\n");
printf("    ?? DEL ;       (define ; as the delimiter character)\n");
printf("                   (insert the next 7 input lines)\n");
printf("    ?? +A;J(10);+I3;LINE1;LINE2;LINE3;XQ\n");
printf("\n");
printf("If the delimit command is used from within a 'Z' command, the\n");
printf("effect of the delimit command is suppressed until the Z command\n");
printf("sequence is exited.  In other words, the initial delimiter on the Z\n");
printf("command remains in force throughout the execution of the Z command\n");
printf("sequence.\n");
	  break;
          case CMD_brief:
printf("BRIEF    [BR]  brief\n");
printf("=====\n");
printf("BRIEF- puts the editor into BRIEF mode.  In BRIEF mode, automatic\n");
printf("verification of commands is suppressed. Other forms of this command\n");
printf("are-\n");
printf("\n");
printf("  BRIEF+   turns on BRIEF mode  (same as BRIEF)\n");
printf("  BRIEF-   turns off BRIEF mode (same as VERIFY)\n");
	  break;
          case CMD_verify:
printf("VERIFY    [V]  verify\n");
printf("======\n");
printf("VERIFY- puts the editor into 'VERIFY' mode. In VERIFY mode all lines\n");
printf("operated upon will be typed out.  The editor is\n");
printf("initially in VERIFY mode. Other forms of this command are-\n");
printf("\n");
printf("   VERIFY+  sets the VERIFY flag (same as VERIFY)\n");
printf("   VERIFY-  clears the VERIFY flag (same as BRIEF)\n");
	  break;
          case CMD_rmargin:
printf("RMARGIN #    [RM]  rmargin\n");
printf("=========\n");
printf("RMARGIN-   sets the right margin setting for use by the\n");
printf("'TRUNCATE',and 'CUT' commands. The right margin must be between\n");
printf("10 and 256 characters, and initially is 256.\n");
	  break;
          case CMD_lmargin:
printf("LMARGIN #    [LM]  lmargin\n");
printf("=========\n");
printf("LMARGIN-   sets the left margin setting for use by window commands.\n");
printf("(WMARGIN sets both the left and right margins.)\n");
	  break;
          case CMD_lveto:
printf("LINEVETO  [LVETO]  lineveto\n");
printf("========\n");
printf("LINEVETO- can be used to force XEDIT to require verification of changes\n");
printf("made to each line.  (line item veto).  Flag is checked by the CHANGE, DELETE\n");
printf("commands.  LINEVETO + turns the flag on. LINEVETO - turns it off.\n");
printf("The default is off.  When on, before a line is changed or deleted, the\n");
printf("line is displayed with the prompt CHANGE?/DELETE?.  One can enter\n");
printf("Y(es), N(o), or '-'.  '-' tells xedit to make the present change and\n");
printf("toggle the LINEVETO flag to off.  (i.e. don't ask for verification).\n");
	  break;
          case CMD_veto:
printf("VETO  veto\n");
printf("====\n");
printf("VETO- can be used to force XEDIT to require verification of changes\n");
printf("before implementing them. VETO is a mode in XEDIT and can be set by\n");
printf("using VETO+  or reset by using VETO-  commands.  VETO by itself is the\n");
printf("same as VETO+.   The default is - (off).  When VETO mode is\n");
printf("+ (on), the the user will be prompted by:\n");
printf("                 IMPLEMENT?\n");
printf("whenever the file is about to be changed.  If the user replies Y / YES\n");
printf("then the changes will be made. Anything else results in the equivalent\n");
printf("of the RESTORE command (see RESTORE command for details).  Commands\n");
printf("which force alteration of the file (e.g. - STOP, FILE, END, QUIT)  are\n");
printf("not affected by VETO.\n");
	  break;
          case CMD_wmargin:
printf("WMARGIN # #    [WM]  wmargin\n");
printf("============\n");
printf("WMARGIN -setsthe left and right window margins for use by the 'A' and\n");
printf("'W' postfix characters. The margins must be arrange such that\n");
printf("\n");
printf("               1 <= left margin <= right margin <= 256\n");
printf("\n");
printf("For the use of the 'A' and 'W' postfix characters, see the 'LOCATE'\n");
printf("command.\n");
	  break;
          case CMD_deftab:
printf("DEFTAB c    [DT]  deftab\n");
printf("========\n");
printf("DEFTAB- defines 'c' as a tab character. Whenever a 'c' is encountered in a\n");
printf("'REPLACE', 'INSERT', 'INSERTB' data line or  while  in  'INPUT'  mode,\n");
printf("tabbing  will  occur. The amount of tabbing is controlled by tab stops\n");
printf("(see 'TAB' command).\n");
	  break;
          case CMD_tabs:
printf("TABS # # # # # # # #    [TAB]  tab\n");
printf("====================\n");
printf("TABS - defines the values given as tab stops. The tab values must be\n");
printf("in increasing value, and must be between 1 and 256.  This command\n");
printf("without any parameters displays all tab stops. Any tab stop not defined\n");
printf("has no effect of the tabbing, and any tab character given in the input\n");
printf("line which occurs past the last tab stop will be retained within the\n");
printf("line.  The default tab stops are 10, 20, and 30 . See the\n");
printf("'DEFTAB' command for instructions on setting the tab character.\n");
	  break;
          case CMD_scan_mode :
printf("READONLY    [SCAN]  readonly\n");
printf("========\n");
printf("READONLY- causes the editor to be put into read only mode.  This speeds \n");
printf("up searches.  No mods can be made while in readonly mode.\n");
printf("The file is positioned to line 1 after this command.\n");
            break;
          case CMD_expand :
printf("EXPAND  #  expand\n");
printf("======\n");
printf("EXPAND- expands the next # lines based on the current tab character.\n");
            break;
          case CMD_help :
            show_help();
            break;
          case CMD_jump :
printf("JUMP  #       [J]  jump\n");
printf("=======\n");
printf("JUMP-  moves the line pointer to the absolute line number specified\n");
printf("by the # parameter.  If # is greater than the number of lines in the\n");
printf("file, then the pointer is moved to the first line of the file.\n");
            break;
          case CMD_file :
printf("FILE FNAME         [F]  file\n");
printf("===============\n");
printf("FILE- places a copy of the edited file on the file named FNAME.\n");
printf("The pointer is positioned  at the top of the file upon completion.\n");
            break;
          case CMD_insert :
printf("INSERT #    [I]  insert\n");
printf("========\n");
printf("INSERT- inserts the next # lines of input after the current line.  The\n");
printf("pointer is positioned at the last line inserted. The # field can be\n");
printf("overridden by entering a null line (i.e. a carriage return by itself).\n");
printf("\n");
printf("QINSERT #     [QI]  qinsert\n");
printf("=========\n");
printf("QINSERT - performs the same function as the insert command, but column\n");
printf("numbers are displayed prior to user input.\n");
            break;
          case CMD_insertb :
printf("INSERTB #    [IB]  insertb\n");
printf("=========\n");
printf("INSERTB- inserts # lines of input before the current pointer position.\n");
printf("This command does not alter the pointer position. Also, the # field\n");
printf("can be overridden by entering a null carriage return.\n");
printf("\n");
printf("QINSERTB #     [QIB] qinsertb\n");
printf("==========\n");
printf("QINSERTB - performs the same function as the insertb command, but column\n");
printf("numbers are displayed prior to user input.\n");
            break;
          case CMD_listab :
printf("LISTAB    [LT]  listab\n");
printf("======\n");
printf("LISTAB- lists the current tab character and tab stops.\n");
            break;
          case CMD_lcopy :
printf("LCOPY FNAME /string/ #    [LCOPY]  lcopy\n");
printf("============\n");
printf("LCOPY- locates and copies # lines from the edited file to file 'FNAME'.\n");
printf("pointer is positioned at the last line copied. In verify mode, each\n");
printf("line copied is printed. The first copy onto a file causes it to be\n");
printf("rewound before copying begins. Consecutive copies onto the same file\n");
printf("add information to that file. Note, XEDIT must be able to write on\n");
printf("file 'FNAME'.\n");
            break;
          case CMD_locate :
printf("LOCATE /STRING/ #    [L]  locate\n");
printf("=================\n");
printf("LOCATE- starting at the current line, perform a string search for the\n");
printf("#th line containing the string specified. In VERIFY mode, each line\n");
printf("containing the delimited string is printed.  If no terminating\n");
printf("delimiter is found,  a delimiter is assumed after the last nonblank\n");
printf("character of the command line and an informative message is provided.\n");
printf("\n");
printf("If the # field is 0 (which is different than blank), then XEDIT will\n");
printf("not advance the pointer in order to locate the string(s). Additionally\n");
printf("(if the string is not found), XEDIT will ignore any remaining commands\n");
printf("on that particular command line (e.g.  a Z command or delimited\n");
printf("command sequence). This feature applies to all commands involving\n");
printf("string searches.\n");
printf("\n");
	pause();
printf("\n");
printf("Furthermore, the scope of all string searches can be limited by the\n");
printf("use of the 'A' and 'W' postfix characters. If any string search\n");
printf("command is so modified, XEDIT will limit the columns it searches\n");
printf("according to the postfix (see the WMARGIN command for information on\n");
printf("setting the windows). The postfix characters perform as follows-\n");
printf("\n");
printf("the 'A' postfix causes XEDIT to require that the first character of\n");
printf("STRING1  reside within the window. Thus, the following commands will\n");
printf("cause XEDIT to locate a line which begins with the string 'DOCUMENT'.\n");
printf("\n");
printf("          ?? WM,1,1\n");
printf("          ?? LA/DOCUMENT/\n");
printf("\n");
	pause();
printf("\n");
printf("The 'W' postfix causes XEDIT to require that all characters of both\n");
printf("strings  (where there are two) to reside within the window. Caution-\n");
printf("specifying a string which has more characters than the window will\n");
printf("never locate the desired string.\n");
printf("\n");
printf("Other forms of the LOCATE command are-\n");
printf("\n");
printf("LOCATE /STRING1...STRING2/ #    [L]\n");
printf("============================\n");
printf("LOCATE- locates the #th line containing STRING1 which is followed by\n");
printf("STRING2 separated by any number of intermediate characters. In all\n");
printf("other ways this command is the same as regular locate.\n");
printf("\n");
printf("LOCATE /STRING1---STRING2/ #    [L]\n");
printf("============================\n");
printf("LOCATE- locates the #th line containing STRING1 which is not followed\n");
printf("by STRING2.  In all other respects, this command is identical to the\n");
printf("regular LOCATE.\n");
printf("\n");
	pause();
printf("\n");
printf("LOCATE /---STRING2/ #    [L]\n");
printf("=====================\n");
printf("LOCATE-   locates the #th line which does not contain STRING2.\n");
printf("In all other respects, this command is identical to the\n");
printf("regular LOCATE.\n");
printf("\n");
            break;
          case CMD_locaten :
printf("LOCATEN #    [LN]  locaten\n");
printf("=========\n");
printf("LOCATEN- starting at the current line, perform a string search for the\n");
printf("#th line containing the last used locate search pattern.(Locate Next)\n");
            break;
          case CMD_modify :
printf("MODIFY    [M]  modify\n");
printf("======\n");
printf("MODIFY- modifies the line currently pointed at. The modify directives\n");
printf("are as follows-  these are entered as input followin the echoed line\n");
printf("\n");
printf(" Directive                       Explanation\n");
printf(" ---------                       ------------\n");
printf(" ^string#   causes the string of characters between the ^ and the\n");
printf("            next # to be inserted before the characters pointed to\n");
printf("            by the ^.  An ^ or & within the string is treated as a\n");
printf("            regular character.  If the closing # is not specified,\n");
printf("            XEDIT inserts the remainder of the line as if a # was\n");
printf("            specified after the last nonblank character.\n");
printf("            There are two exceptions.  The combination ^# causes a\n");
printf("            # to be inserted before the character pointed to by the\n");
printf("            ^, and an ^ as the last character of the directives\n");
printf("            causes a blank to be inserted.\n");
printf("\n");
	pause();
printf("\n");
printf(" Directive                       Explanation\n");
printf(" ---------                       ------------\n");
printf(" #          (when not the first # after an ^) causes the character\n");
printf("            above it to be deleted.\n");
printf("\n");
printf(" &          replaces the character above it with a space.\n");
printf("\n");
printf(" !          truncates the rest of the line from this point.\n");
printf("\n");
printf(" (space)    a space below a character leaves it unchanged.\n");
printf("\n");
printf(" any other character replaces the character above it.\n");
printf("\n");
printf("                 EXAMPLE\n");
printf(" print original line    ?? PRINT\n");
printf("                        10 THIS STRING TO BE MORTIFD\n");
printf(" issue modify command   ?? MODIFY\n");
printf(" XEDIT prints the line    10 THIS STRING TO BE MORTIFD\n");
printf(" the directives line-   ?        ^ IS THE#       D#  ^IE\n");
printf(" verification           10 THIS IS THE STRING TO BE MODIFIED\n");
            break;
          case CMD_next :
printf("NEXT #      [N]  next\n");
printf("======\n");
printf("NEXT- advances the pointer # lines from the present position.\n");
printf("\n");
printf("NEXT -#    [N]  next\n");
printf("=======\n");
printf("NEXT- moves the pointer back # lines or to the top of the file\n");
printf("(whichever is closer).  If # is omitted or equal to zero, no move is\n");
printf("performed. Note- backward move is considerably slower than forward.\n");
            break;
          case CMD_nobells :
printf("NOBELLS  [NB]  nobells\n");
printf("=======\n");
printf("NOBELLS- turn off the ringing of the bell on errors.\n");
            break;
          case CMD_paste :
printf("PASTE  paste\n");
printf("======\n");
printf("PASTE- deletes the current line, but inserts the same text before the\n");
printf("text of the following line.  If the 'W' postfix is appended, then the\n");
printf("lines are pasted with an intervening blank.  No operations are \n");
printf("performed on the last line of a file.\n");
            break;
          case CMD_print :
printf("PRINT #    [P]   print\n");
printf("=======\n");
printf("PRINT- prints # lines starting at the current pointer position.  The\n");
printf("pointer is left positioned at the last line printed.  If the 'W' post-\n");
printf("fix is appended,  only columns within the current window are displayed\n");
printf("(see the WMARGIN command for information on setting the windows).\n");
printf("\n");
printf("QPRINT #   [QP]  qprint\n");
printf("========\n");
printf("QPRINT- same as print above except prints column numbers before first line.\n");
            break;
          case CMD_qmodify :
printf("QMOD #    [QM]  qmod\n");
printf("======\n");
printf("QMOD- prints out column numbers and the first line to be modified,\n");
printf("takes modify directives from the next line of input, and uses them\n");
printf("to modify # lines starting at the pointer position. The pointer is not\n");
printf("moved if the directives line is blank.  (See MODIFY for directives)\n");
            break;
          case CMD_yqmodify :
printf("YQMOD #    [YQM]  yqmod\n");
printf("======\n");
printf("YQMOD- takes a list of modify directives from the next line of input, and uses them\n");
printf("to modify # lines starting at the pointer position. The pointer is not\n");
printf("moved if the directives line is blank.  (See MODIFY for directives)\n");
            break;
          case CMD_exit :
            terminate_edit = TRUE;
            break;
          case CMD_quit :
printf("END FNAME     [E] end        QUIT FNAME  [QUIT] quit\n");
printf("==============\n");
printf("END or QUIT- ends the editing and places  the  edited  file  with  all  the\n");
printf("corrections  on  file  FNAME .\n");
printf("\n");
printf("If  the  file  name  is  omitted,  'EDITFIL' from the XEDIT command is\n");
printf("assumed.\n");
            break;
          case CMD_read :
printf("READ FNAME1  read\n");
printf("=========================\n");
printf("READ- reads the file FNAME1 into the edited file\n");
printf("after the current pointer position. The file is read starting with\n");
printf("the first line, continuing until end of file is reached.\n");
printf("After the read, the pointer is positioned at the last line read.\n");  
            break;
          case CMD_replace :
printf("REPLACE #    [R]  replace\n");
printf("=========\n");
printf("REPLACE- starting at the current pointer position, replaces # lines of\n");
printf("the edited file with the next # lines of input. The # field can be\n");
printf("overridden by entering a null carriage return.  The pointer is left\n");
printf("positioned at the last line replaced.\n");
            break;
          case CMD_stop :
printf("STOP  stop\n");
printf("====\n");
printf("STOP- aborts the editor without writing corrections anywhere.  This\n");
printf("method of exiting is considerably faster than the END and QUIT\n");
printf("commands.\n");
            break;
          case CMD_system :
printf("SYSTEM  [SYSTEM]  system\n");
printf("====\n");
printf("SYSTEM- escapes to the system. \n");
            break;
          case CMD_top :
printf("TOP    [T]  top\n");
printf("===\n");
printf("TOP- moves the pointer to the top of the file.\n");
            break;
          case CMD_truncate :
printf("TRUNCATE #    [TRUNC]  truncate\n");
printf("==========\n");
printf("TRUNCATE- truncates the next # lines to 'RMARGIN' number of characters\n");
printf("starting  with the current line.  If the 'W' postfix is appended, col-\n");
printf("umns outside of the current window are truncated.  (see the WMARGIN\n");
printf("command for information on setting windows).\n");
            break;
          case CMD_restore:
printf("RESTORE    [REST]  restore\n");
printf("=======\n");
printf("RESTORE- removes all modifications made to the file since the pointer\n");
printf("was last at the top of the file. The pointer is moved to the top of\n");
printf("the file for the following reasons-\n");
printf("  1. the TOP command is encountered.\n");
printf("  2. the ^ (up arrow) prefix character is encountered.\n");
printf("  3. a next- command is encountered.\n");
printf("  4. a jump command requiring reverse motion is encountered.\n");
printf("\n");
printf("See the VETO command.\n");
            break;
          case CMD_where :
printf("WHERE    [W]  where\n");
printf("=====\n");
printf("WHERE- prints the current line number.\n");
            break;
          case CMD_xedit :
printf("XEDIT FNAME         [XEDIT]  xedit\n");
printf("===============\n");
printf("XEDIT- begins a new xedit session with FNAME.  If mods were made\n");
printf("to the current file, you will be prompted for saving it.\n");
            break;
          case CMD_z_command :
printf("Z /COMMAND1/COMMAND2/ ... /COMMANDN  y\n");
printf("===================================\n");
printf("Z- takes the specified list of commands and/or data and enters it\n");
printf("into the Z command buffer and then executes that buffer. The normal\n");
printf("rules regarding command entry apply to the commands contained in the\n");
printf("Z list and if any command violates the rules, or is unrecognized,\n");
printf("the remaining commands are ignored and XEDIT returns to the main input\n");
printf("file for the next command.  Specifically, this allows commands and\n");
printf("data to be intermixed within the Z list and allows all features\n");
printf("available via the delimit command to apply to the Z command list.\n");
            break;
          case CMD_error :
            error ("Illegal command", fatal);
          default:
            fprintf(O.fileS,"Command = %s\n",command_string);
          }
}

void decode_command(command_text)
     char command_text[MaxStringR];
{
    int ci,i,ni;

      strcpy(cc,command_text);
      si = 0;
      cc[strlen(cc)] = '\0';
      next_char();
      end_of_line = FALSE;
      error_in_command = no_error;
      current_command = default_actions;

      skip_spaces();
      ni = 0;
      strcpy(command_name,"        ");
/*  max_cmd_name_length spaces ^^     */
      while((c_in_string(c,letters)) && (ni < max_cmd_name_length)){
        command_name[ni++] = c;
        next_char();
      }
      strlower(command_name);

      if(ni >= 2){
        if(command_name[ni - 1] == 'w'){
          command_name[ni - 1] = ' ';
          current_command.window_access = windowed;
        }
        else if(command_name[ni - 1] == 'a'){
          command_name[ni - 1] = ' ';
          current_command.window_access = anchored;
        }
      }
      ci = 0;
      while(strcmp(command_name,command_table[ci].long_form) &&
             strcmp(command_name,command_table[ci].short_form)  &&
             ci < command_table_length-1){
        ci++;
      }

      current_command.processor = command_table[ci].processor;

      process_command();

}

/*  ***************** main function *************************** */

main (argc, argv)
    int argc;  char *argv[];
{
    int error_flag=0,badarg=0,filea=0;
    int iarg,swval,create=0,i,argIO=0,argR=0;

    strcpy(input_line,"H;");

    /* determine the switches and files */
    for(iarg=1; iarg<argc; iarg++ ) {
        if( *argv[iarg] == '-' ) {
            strupr(argv[iarg]);
            if(!strcmp(argv[iarg],"-A")){
	       strcpy(input_line,"h;A;B;BR;C;CS;copy;copyd;cut;");
	       strcat(input_line,"dt;d;del;dup;e;expand;f;i;");
	       strcat(input_line,"ib;j;lcopy;lveto;lt;lm;l;ln;m;");
	       strcat(input_line,"n;paste;p;qcut;qi;qib;qm;qp;quit;");
	       strcat(input_line,"read;scan;r;restore;rm;stop;system;");
	       strcat(input_line,"tab;t;trunc;v;veto;w;wm;xedit;y;yqm;z;exit");
	    }
	    else{
	      printf("Usage: Txedith [-a] [\"cmd1;cmd2;...cmdn\"]\n");
	      printf("        [-a] -> prints entire 'help' manual to stdout\n");
	      printf("        [cmd1..]  -> xedit commands to inquire about\n");
	      exit(1);
	    }
            continue;
        } 
        strcpy(input_line,argv[iarg]);
    }

    /* set remaining global constants */
    LineLengthR = LineLength + 1;
    terminate_edit = FALSE;
    default_actions = standard_actions;
    delimiter = ';';

    I.fileS=stdin;
    strcpy(I.name,"stdin");
    O.fileS=stdout;
    strcpy(O.name,"stdout");

    /* display the selected options */
    fprintf(O.fileS,"Xedit-Help (%s)\n",cdate);

    while( end_of_input == 0 && !terminate_edit ){
      read_next(command_string,read_commands);
      decode_command(command_string);
    }
    exit(0);
}
