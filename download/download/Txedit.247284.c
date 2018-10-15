/******************************************************************************/
/*
 * @(#) Txedit - Extended Text Editor
 */

/*
        Control Data Corporation
        Xedit - Extended Text Editor
        This version of xedit is a look-alike of the above.
        Some modules were extracted from the NOSVE Cybil version of xedit,
        and converted to C.  Other modules were written to support features
        that were not in the NOSVE version, but in the NOS version.
        Most of the (W) Gray Hill extentions were added, as well as some
        new features written by (W) R.K. Wonders.

        Put together by R.K.Wonders - Westinghouse Corporate Computer Services

        V900517 - change *modify* to handle tab char (change to spaces)
        V900609 - fix copy_string to handle case where command delimeter
                  is in the string.    c/1;/2;/*   del=;
        V901128 - fix problem with upper-case 'X' as prefix.  was ignored.
        V910613 - fix problem with call to strings. wm,17,18;lw/  /* caused
                  a core dump
        V910614 - Add filter capability with dialog.
        V910617 - Add system and y command. Add trace file.
        V910823 - Add nobells(nb) command
        V930419 - Add -DXEDITTMP to cc command to allow use of environment
                  variable to control where scratch files are written, per
                  request of Eleanor Iwinski, NATD. Tested on the following:
                  unicos: cc -DXEDITTMP xedit2.c
                  SunOS:  cc -DXEDITTMP -DSUN xedit2.c
                  HP-UX:  cc -DXEDITTMP -DSUN xedit2.c
                  - John S. Urban

*/
/*  #define msdos 1   for microsoft c   */
/*  #define cyber 1   for CDC NOSVE   */
/*  #define SUN 1 for SUN  or use cc xedit.c -DSUN on compile */
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
#define infinity 99999999L
#define tab_settings 30
#define max_cmd_name_length 8

enum string_option {no_strings, optional_string, one_string, two_strings};
enum  command_processors  {CMD_print, CMD_locate, CMD_next, CMD_stop,
      CMD_bottom, CMD_delimit, CMD_change, CMD_insert, CMD_insertb, CMD_add,
      CMD_delete, CMD_replace, CMD_z_command, CMD_changes, CMD_copy, CMD_cut,
      CMD_copy_delete, CMD_read, CMD_modify, CMD_yqmodify, CMD_qmodify,
      CMD_where, CMD_help, CMD_file, CMD_quit, CMD_top, CMD_jump, CMD_dup,
      CMD_brief, CMD_restore, CMD_wmargin, CMD_rmargin, CMD_lmargin,
      CMD_verify, CMD_locaten, CMD_system, CMD_nobells,
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
char    cdate[]="V910823";
Stream  A,B,C,D,I,O,T;                /* 7 streams  */
Stream  *CurStream;             /* current read stream*/
Stream  *CurWork;               /* current work stream*/
int     DIALOG=1;               /* flag for interactive dialog */
int     debug;
int     initial_open;           /* flag for initial open of editfile */
int     LineLength=MaxString;   /* maximum significant line length */
int     LineLengthR;            /* maximum line length or read */
char    edit_file[80];
char    current_file[80];
int     veto_mode = 0;          /* flag to allow veto of changes */
int     veto_line = 0;          /* flag to allow line item veto */
int     scan_mode = 0;          /* flag to force read_only commands */
int     modified = 0;           /* flag modification made */
int     mods_made = 0;          /* flag modifications made */
int     end_of_input = 0;       /* flag end of input encountered*/
int     bell_mode = 7;          /* bell character */
char    currentline[MaxStringR];  /* current line string*/
char    command_string[MaxCommandWidth];
char    prev_command[MaxCommandWidth];
int     si,repeat_y,end_of_line;
char    cc[MaxCommandWidth],c;
long    left_window,right_window;
int     dashdashdash;          /* flag used in locate_next,=1 if --- */
int     ldashdashdash;          /* flag used in locate_next,=1 if --- */

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
   {"expand  ", "expand  ", no_strings, TRUE, FALSE, FALSE, CMD_expand},
   {"file    ", "f       ", no_strings, FALSE, TRUE, FALSE, CMD_file},
   {"help    ", "h       ", no_strings, FALSE, TRUE, FALSE, CMD_help},
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
   {"qcut    ", "qcut    ", no_strings, FALSE, FALSE, FALSE, CMD_cut},
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
   {"yqmod   ", "yqm     ", no_strings, TRUE, FALSE, FALSE, CMD_yqmodify},
   {"y       ", "y       ", no_strings, FALSE, FALSE, TRUE, CMD_z_command},
   {"z       ", "z       ", no_strings, FALSE, FALSE, TRUE, CMD_z_command},
   {"error   ", "error   ", no_strings, FALSE, FALSE, FALSE, CMD_error}
};
#define c_table_length  sizeof(command_table) / sizeof(command_table[0])
int command_table_length=c_table_length;

margin margins;
char letters[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
char delimiters[] = "[]<>$%_\'()=;:!\".?/-+#&^~`|@,\\";
char numbers[] = "0123456789";
char prefixes[] = "/xX+^";
int  z_option;
int  error_in_command;
int  error_level;
int  terminate_edit;
int  terminal_interrupt;
int  end_of_file;
int  line_number;
command_actions standard_actions = {0, 0, FALSE, TRUE,
                                      TRUE, no_window, CMD_error};
command_actions default_actions;
command_actions current_command;
char command_name[max_cmd_name_length + 1];  /* name + nul */
char string1[MaxStringR];
char e_string[MaxStringR];
char lstring1[MaxStringR];
char le_string[MaxStringR];
char string2[MaxStringR];
int s1es_len;
int tabs[tab_settings];
int last_tab_setting;
char tab_char;
char z_command_text[MaxStringR];
char y_command_text[MaxStringR];
char previous_command_text[MaxStringR];
char command_text[MaxStringR];
char input_line[MaxStringR];
char delimiter;
char z_delimiter;
/* ************************ prototypes  ************************* */
#ifdef SUN    /*  also for cyber---   ifdef cyber removed  */
void advance();
#else
void advance(long);
#endif
void verify_line();
#ifndef msdos
#ifdef XEDITTMP
FILE *xtmpfile();
#else
FILE *tmpfile();
#endif
#endif
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
 /*  char *ptr;
     ptr = strchr(string,c);
     if(ptr == NULL) return(FALSE);
     return(TRUE);
*/
}

int yes_or_no(prompt)
   char prompt[];
/*  returns 1 if Y(y) or 0 if N(n) is entered  */
{
     char line_in[5];

     while(TRUE){
      if(DIALOG) fprintf(O.fileS,"%s",prompt);
      fgets(line_in,5,I.fileS);
      if(!strncmp(strupr(line_in),"Y",1)) return(1);
      if(!strncmp(strupr(line_in),"N",1)) return(0);
     }
}

int line_veto(prompt)
   char prompt[];
/*   return 1 if do change  0 if change vetoed */
/*   - does change and turns off veto */
/*   A vetos change and aborts command */
{
     char line_in[5];

     if(!veto_line) return(1);
     while(TRUE){
      if(DIALOG) fprintf(O.fileS,"%s",prompt);
      fgets(line_in,5,I.fileS);
      if(!strncmp(strupr(line_in),"Y",1)) return(1);
      if(!strncmp(strupr(line_in),"N",1)) return(0);
      if(!strncmp(strupr(line_in),"-",1)) {
        veto_line = FALSE;
        return(1);
      }
     }
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

int string_in_string(source_string,search_string)
        char source_string[],search_string[];
/*   returns the position of search_string in string starting FROM 1
     return 0 if not found  */
{
      int i,max,search_length;
      char *ptr;
      search_length = strlen(search_string);
      max = strlen(source_string) - search_length + 1;
      if(max < 1) return(0);
      ptr = &source_string[0];
      for(i=0;i <= max;i++){
        if(strncmp(ptr,search_string,search_length)==NULL)
          return(i+1);
        ptr++;
      }
      return(0);
}

int replace_string (s1,s2,start,count)
       char s1[],s2[]; int start; long *count;
/*      replace s1 with s2 in currentline
        start is the position of first occurrance of s1 */
{
      int i, starting_position, col, slen;
      char *cptr,*nptr,new_string[MaxStringR];

      starting_position = start + left_window - 1 ;
      col = 0;
      new_string[0] = '\0';
      cptr = &currentline[0];
      nptr = &new_string[0];
      slen = strlen(s1);
      if(s1es_len > 0 ) slen = s1es_len;

      do{
        for(i=0; i < starting_position-1; i++){
            *nptr++ = *cptr++;
            col++;
        }
        for(i=0;i<strlen(s2);i++)
            *nptr++ = s2[i];
        cptr = cptr + slen;
        col += slen;
    /*  *count--;  */
        *count = *count - 1L;
        if(strlen(s1) > 0 ){
         starting_position = string_in_string(cptr,s1);
         col += starting_position;
         if(current_command.window_access == windowed ||
            current_command.window_access == anchored){
          if(col+slen > right_window) starting_position = 0;
         }
        }
        else
         starting_position = 0;
      }while(starting_position && (*count > 0));
      for(cptr;cptr<= &currentline[strlen(currentline)];cptr++)
        *nptr++ = *cptr;
      if(veto_line && DIALOG)
        fprintf(O.fileS,"%s",new_string);
      if(line_veto(" Change?")){
       strcpy(currentline,new_string);
       modified = TRUE;
      }
      if(!veto_line) verify_line();
      return(1);
}

int check_end_of_file()
{
      if(end_of_file){
        y_command_text[0] = '\0';
        if(DIALOG) fprintf(O.fileS,"%c--eoi/top--\n",bell_mode);
        goto_top();
      }
}

void display_occurrences (j)
      int j;
{
      if(!current_command.verify_command &&
          current_command.iteration_count == infinity ) {
        y_command_text[0] = '\0';
        if(DIALOG) fprintf(O.fileS,"%c--eoi/top-- / occurrences = %d\n",
                bell_mode,current_command.iteration_count - j);
        goto_top();
      }
}

void verify_line()
{
      if(current_command.verify_command && !end_of_file )
        fprintf(O.fileS,"%s",currentline);
}

int expand_tabs(old_line,n_line)
    char old_line[],n_line[];
{

      int sp,np,j,i;

      n_line[0] = '\0';
      if(tab_char == '\0' || last_tab_setting == 0) return(0);
      np = 0;
      j = 1;

      for(sp=0;sp < strlen(old_line);sp++){
       if(old_line[sp] == tab_char){
          while(tabs[j]-1 <= np && j < last_tab_setting){
            j++;
          }
          for(i=np; i < (tabs[j]-1);i++){
            n_line[np++] = ' ';
          }
       }
       else n_line[np++] = old_line[sp];
      }
      n_line[np] = '\0';
      return(1);
}

int expand_lines()
{
        long i;
        char new_line[MaxStringR];

        i = current_command.iteration_count;
        while(!end_of_file){
          if(expand_tabs(currentline,new_line)){
            strcpy(currentline,new_line);
          }
          modified = TRUE;
          verify_line();
          i--;
          if(i < 1) return(1);
          advance (1L);
        }
}

void do_prefix_actions()
{
      if(current_command.start_at_top) goto_top();
      advance(current_command.skip_count);
      if(current_command.window_access == no_window){
        left_window = 1;
        right_window = infinity;
      }
      else{
        left_window = margins.left;
        right_window = margins.right;
        if(current_command.window_access == anchored)
          right_window = right_window + strlen(string1);
      }
}

int search_this_line()
/*  locates string1 and/or e_string in currentline
    return position if found */
{
      int i,position,maxright,position1;
      char temp_string[MaxStringR];

      if((strlen(string1) == 0)){
        return(0);
      }

      s1es_len = 0;
      if(right_window < strlen(currentline))
        maxright = right_window;
      else
        maxright = strlen(currentline);
      if(left_window <= maxright){
        i = maxright - left_window ;
        position = left_window - 1L;
        copysubstr(currentline,position,i,temp_string);
        position = string_in_string(temp_string,string1);
        position1 = position;
        if(position){
          if(strlen(e_string) > 0){
            position = position + strlen(string1) + left_window -2L;
            i = maxright - position + 1;
            copysubstr(currentline,position,i,temp_string);
            position = string_in_string(temp_string,e_string);
            if(position){
             s1es_len = strlen(string1) + position + strlen(e_string) - 1L;
             return(position1);
            }
          }
          else return(position);
        }
      }
      return(0);
}


int locate_next()
/*  locates string1 and/or e_string in currentline
    return position if found */
{
      int i,position,maxright,position1;
      char temp_string[MaxStringR];

      if((strlen(string1) == 0 && !dashdashdash) || end_of_file){
        return(!end_of_file);
      }
      s1es_len=0;
      while(!end_of_file){
        if(right_window < strlen(currentline))
          maxright = right_window;
        else
          maxright = strlen(currentline);
        if(left_window <= maxright){
          i = maxright - left_window + 1 ;
/*        if(current_command.window_access == anchored)
           i += strlen(string1)-1;  */
          position = left_window - 1L;
          copysubstr(currentline,position,i,temp_string);
          position = string_in_string(temp_string,string1);
          position1 = position;
           if(position || (strlen(string1)==0 && dashdashdash)){
             if(strlen(e_string) > 0){
               position = position + strlen(string1) + left_window -2L;
               i = maxright - position + 1;
               copysubstr(currentline,position,i,temp_string);
               position = string_in_string(temp_string,e_string);
               if(position){
                 if(!dashdashdash){
                    s1es_len = strlen(string1) +
                               position + strlen(e_string) -1L;
                    return(position1);
                 }
               }
               else
                 if(dashdashdash) return(1);
             }
             else return(position);
           }
          }
          else {
           if(dashdashdash){
             position = string_in_string(temp_string,e_string);
             if(position == 0 ) return(1);
           }
          }
        if(current_command.iteration_count == 0) return(0);
        advance (1L);
        if(end_of_file) return(0);
      }
}


void error(s,error_level)
  char s[]; int error_level;
{
      if(error_in_command < error_level && DIALOG){
        fprintf(O.fileS,"%s\n",s);
        error_in_command = error_level;
      }
}


void readline(x)        /* reads line from the specified stream */
    Stream *x;
{
    if( !x->endfile ) {

        /* read in a line for the stream */
        if(fgets(currentline,LineLengthR,x->fileS) == NULL ) {
            x->endfile=1;                       /* set stream EOF flag */
            end_of_file = TRUE;
            return;
        }

        /* set the stats about the current newline */

    }
    else end_of_file = TRUE;
    if(x->filelen > 0 && ftell(x->fileS) > x->filelen){
      end_of_file=TRUE;
    }
}

void writeline(x ,textline )     /* writes a line to the specified stream */
    Stream *x; char *textline;
{
        if(textline[0]!='\0'){
         fputs(textline,x->fileS);
        }
            return;
}

void read_file()
{
     char old_line[MaxStringR];

     if(strlen(D.name) > 0 )
      fclose(D.fileS);
     if(!initstream(&D, current_file, "r")){
       rewind(D.fileS);
       end_of_file = FALSE;
       while(!D.endfile){
         strcpy(old_line,currentline);
         readline(&D);
         if(!D.endfile){
           if(line_number>0) writeline(CurWork, old_line);
           modified = TRUE;
           line_number++;
         }else{
           strcpy(currentline,old_line);
         }
       }
       end_of_file = FALSE;
       if(strlen(D.name) > 0 )
        fclose(D.fileS);
       D.name[0] = '\0';
     }
     else{
        fprintf(O.fileS," Error opening %s\n",D.name);
        error (" file cannot be read.", fatal);
     }
}

void insert_lines(count)
     long count;
{
      char new_line[MaxStringR],old_line[MaxStringR];
      long i;

      i=count;
      while(i > 0){
       read_next(new_line,read_data);
       if(strlen(new_line) == 0 ) return;
       if(line_number > 0 ) writeline(CurWork, currentline);
       strcat(new_line, "\n");
       if(!expand_tabs(new_line,currentline)){
         strcpy(currentline,new_line);
       }
       modified = TRUE;
       line_number++;
       end_of_file = FALSE;
       i--;
      }
      return;
}

void insertb_lines(count)
     long count;
{
      char newtext[MaxStringR],new_line[MaxStringR];
      long i;

      i=count;
      while(i > 0){
       read_next(newtext,read_data);
       if(strlen(newtext) == 0 ) return;
       strcat(newtext, "\n");
       if(expand_tabs(newtext,new_line)){
         strcpy(newtext,new_line);
       }
       writeline(CurWork, newtext);
       modified = TRUE;
       line_number++;
       i--;
      }
      return;
}

int modify_line(mods)
   char mods[];
{
        char c,new_line[MaxStringR],*nptr,*cptr,temp_line[MaxStringR];
        int mlp,clp,cs,ms,i;
        char mod_line[MaxStringR];

        strcpy(mod_line, mods);
        ms = strlen(mods)-1;
        cs = strlen(currentline);
        strcat(mod_line,"#");
        for(i=0;i<MaxStringR;i++){
          if(currentline[i] == '\n' || currentline[i] == '\0' )
            break;
          else
            temp_line[i] = currentline[i];
        }
        for(i=i;i<MaxStringR;i++)
          temp_line[i] = ' ';
        temp_line[i] = '\0';
        clp = 0;
        mlp = 0;
        new_line[0] = '\0';

        if(cs == 0)
          new_line[0] = ' ';
        else
          strcpy(new_line,temp_line);

        while(clp <= ms){
          c = mod_line[clp++];
          if(c == ' ')
            mlp++;
          else if(c == '&')
            new_line[mlp++] = ' ';
          else if(c == '!')
            new_line[mlp++] = '\0';
          else if(c == '#'){
            if(clp <= cs){
              cptr = &temp_line[clp];
              for(nptr = &new_line[mlp];
                  cptr <= &temp_line[cs];nptr++) *nptr = *cptr++;
            }
          }
          else if(c == '^'){
            i = clp - 1;
            c = mod_line[clp++];
            while(c != '#'){
              new_line[mlp++] = c;
              c = mod_line[clp++];
            }
            if(i <= cs){
              cptr = &temp_line[i];
              for(nptr = &new_line[mlp];
                  cptr <= &temp_line[cs];nptr++) *nptr = *cptr++;
            }
            mlp = mlp + clp - i;
          }
          else{
            new_line[mlp++] = c;
            }
        }
        mlp--;
        if(ms < cs)
          mlp = mlp + cs - ms;
        while((mlp > 0) && (new_line[mlp] == ' ')){
          mlp--;
        }
        if(new_line[mlp] != '\n') new_line[++mlp] = '\n';
        new_line[++mlp] = '\0';
        strcpy(currentline,new_line);
        modified = TRUE;
}


void replace_lines(count)
      long count;
{
      char newtext[MaxStringR];
      long i;

      i=count;
      do{
       read_next(newtext,read_data);
       if(strlen(newtext) == 0 ) return;
       strcat(newtext, "\n");
       if(!expand_tabs(newtext,currentline)){
         strcpy(currentline,newtext);
       }
       modified = TRUE;
       if(--i>0){
         advance(1L);
         if(end_of_file) return;
       }
      }while(i>0);
      return;
}

void add_lines(count)
      long count;
{
      char newtext[MaxStringR];
      long i;

      read_next(newtext,read_data);
      if(strlen(newtext) == 0 ) return;
      strcat(newtext, "\n");

      i=count;
      do{
       currentline[strlen(currentline)-1] = '\0';
       strcat(currentline,newtext);
       modified = TRUE;
       if(--i>0){
         advance(1L);
         if(end_of_file) return;
       }
      }while(i>0);
      return;
}


void print_many_lines(count)
   long count;
{
      long i;
      int length;
      char temp_string[MaxStringR];

      i = count;
      length = right_window - left_window + 1;
      if(length < 1) length = infinity;

      while(i-- > 1){
        if(current_command.window_access == windowed){
          copysubstr(currentline,left_window-1,length,temp_string);
          fprintf(O.fileS,"%s\n",temp_string);
        }
        else
          fprintf(O.fileS,"%s",currentline);
        advance (1L);
        if(end_of_file) return;
      }
      if(current_command.window_access == windowed){
        copysubstr(currentline,left_window-1,length,temp_string);
        fprintf(O.fileS,"%s\n",temp_string);
      }
      else
        fprintf(O.fileS,"%s",currentline);
}


void advance(count)
    long count;
/* write current line to work file, read from x and bump line count. */
{
    long i;
    char temp_string[MaxStringR];

    i = count;
    while(!end_of_file && (i > 0) ){


/*     read from current stream file */
        if( !end_of_file ) {
            temp_string[0] = '\0';
            strcpy(temp_string,currentline);   /* save old line */
            readline(CurStream);
            if(!end_of_file) {
              line_number++;
/*            write out current line to work file */
              if(line_number > 1 && !scan_mode)
                 writeline(CurWork, temp_string);
            }
            else
             strcpy(currentline,temp_string);
        i--;
      }
    }
    if(i != 0) {
      if(CurStream->endfile) end_of_file = TRUE;
    }
}

int rewrite_file(file_name)   /* rewrite current work file to file_name   */
      char file_name[];
{

        goto_top();   /* read of first line done */
        if(strcmp(A.name,"stdout")){
          if(strlen(A.name) > 0 ) fclose(A.fileS);
          if(initstream(&A, file_name, "w")){
               fprintf(O.fileS," Error opening %s\n",A.name);
               A.name[0] = '\0';
               return(0);
          }
          rewind(A.fileS);
        }
        while(!end_of_file) {
         writeline(&A,currentline);
         readline(CurStream);
        }
        if(strcmp(A.name,"stdout")) fclose(A.fileS);
        A.name[0] = '\0';
        return(1);
}

int copy_to_file (delete_line, count)
      int delete_line; long count;
{
        int found; long i;

      if(!strcmp(D.name,current_file)){
        if(strlen(D.name) > 0 ) fclose(D.fileS);
        if(initstream(&D, current_file, "a+")){
               fprintf(O.fileS," Error opening %s\n",D.name);
               D.name[0] = '\0';
               error (" file cannot be written.", fatal);
               return(0);
        }
      }
      else{
        if(strlen(D.name) > 0 ) fclose(D.fileS);
        if(initstream(&D, current_file, "w")){
               fprintf(O.fileS," Error opening %s\n",D.name);
               D.name[0] = '\0';
               error (" cannot be written.", fatal);
               return(0);
        }
      }

      i = count;
      if(strlen(string1)+strlen(e_string)> 0) i=infinity;
      while(!end_of_file && i > 0){
        if(line_number > 0) writeline(&D,currentline);
        verify_line();
        i--;
        found = search_this_line();
        if(found) i=0;
        if(delete_line)
          if(line_veto(" Delete?")){
           currentline[0] = '\0';
           modified = TRUE;
          }
        if(i>0) advance(1L);
      }
      fclose(D.fileS);
      D.name[0] = '\0';
}


void next_lines()
{
       advance(1L);
       if( !CurStream->endfile ) {
           if(DIALOG) fprintf(O.fileS,"%c--eoi/top--\n",bell_mode);
           goto_top();
       }
}

int  goto_top()
{
    int position;
    char temp[80];

    if( modified && veto_mode){
       if(!yes_or_no(" \nImplement? ")) modified = FALSE;
    }
    if( modified || initial_open) {
        while(!end_of_file) {
          advance(1L);
        }
        writeline(CurWork, currentline);
        CurWork->filelen = ftell(CurWork->fileS);

        CurStream = CurWork;
        if(initial_open && !strcmp(A.name,"stdin")){
          A.fileS = stdout;
          strcpy(A.name,"stdout");
        }
        initial_open = 0;
        if(CurWork == &B ){
             CurWork = &C;
        }
        else {
             CurWork = &B;
        }
        modified = FALSE;
        mods_made = TRUE;
    }
    rewind(CurStream->fileS);
    rewind(CurWork->fileS);
    if(CurStream->filelen > 0 || initial_open ){
     CurStream->endfile = FALSE;
     end_of_file = FALSE;
    }else{
     CurStream->endfile = TRUE;
     end_of_file = TRUE;
    }
    CurWork->endfile = 0;
    CurWork->filelen = 0;
    line_number = 0;
    advance(1L);
}

int initstream( x, fname, mode)
    Stream *x;  char *fname; char *mode;
{
    strcpy(x->name,fname);
    if( !strcmp(mode , "temp"))
    {
#ifndef msdos
#ifdef XEDITTMP
     x->fileS = xtmpfile(x->name);
#else
     x->fileS = tmpfile();
#endif
#else
     x->fileS = fopen(x->name,"w+");
#endif
    }
    else
     x->fileS = fopen(x->name,mode);

     if( (x->fileS == NULL) && DIALOG ){
         fprintf(O.fileS,"Unable to open File: \"%s\"\n",x->name); return(1);
     }
    x->endfile = 0;
    x->filelen = 0;
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

int read_next (s,im)
     char s[]; enum input_mode im;
{

      if(!current_command.input_from_command_line && im == read_data){
        read_command_line (s, im);
      }
      else if(strlen(y_command_text) > 0 ){
        get_next (y_command_text, s, z_delimiter);
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
      if(T.fileS != NULL) fprintf(T.fileS,"%s\n",s);
}


read_command_line( string, im )
    char string[]; enum input_mode im;
{
    if(im == read_commands){
     if(DIALOG) fprintf(O.fileS,"??");
    }
    else{
     if(DIALOG) fprintf(O.fileS,"?");
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

long decode_number()
{
    char tempstring[MaxString]; int tindex;
    long i;

        tindex = 0;
        skip_spaces();
        i = 1;
        if(end_of_line) return(i);
        if(isdigit(c)){
          i = 0;
          cc[strlen(cc)] = '\0';
          while(isdigit(c)){
            tempstring[tindex++] = c;
            next_char();
          };
          tempstring[tindex] = ' ';
          i = atol(tempstring);
        }
        else if(c == '-'){
          next_char();
          i = decode_number();
          i = - i;
        }
        else if(c == '*'){
          next_char();
          i = infinity;
        }
        else
          error ("Invalid count", fatal);
        return(i);
}

int copy_string(s,string_delimiter)
    char *s, string_delimiter;
{
        char *sptr;

        sptr = s;
        next_char();
        while( c != string_delimiter){
          if(end_of_line){
           if((strlen(input_line) || strlen(y_command_text))){
             if(strlen(input_line))
               *sptr++ = delimiter;
             else
               *sptr++ = z_delimiter;
             end_of_line=0;
             read_next(cc,read_commands);
             si=0;
           }
           else break;
          }
          else{
           *sptr++ = c;
          }
          next_char();
        };
        *sptr = '\0';
}

int cut_line()
{
      char newtext[MaxStringR];
      long i,j;
            if(command_name[0] == 'q'){
            if(DIALOG){
             fprintf(O.fileS,
                     " 0        1         2         3         4       ");
             fprintf(O.fileS,"  5         6         7     \n");
             fprintf(O.fileS,
                     " 12345678901234567890123456789012345678901234567");
             fprintf(O.fileS,"8901234567890123456789012345\n");
             fprintf(O.fileS," ");
             fprintf(O.fileS,"%s",currentline);
            }
             read_next(newtext,read_data);
             if(strlen(newtext) == 0 ) return(0);
             i = c_in_string('^',newtext);
            }
            else{
             i = margins.right;
            }
            if(i < 1 || i >= strlen(currentline)) return(0);
            copysubstr(currentline,0,i,newtext);
            strcat(newtext,"\n");
            writeline(CurWork, newtext);
            modified = TRUE;
            line_number++;
            copysubstr(currentline,i,strlen(currentline)-i,newtext);
            strcpy(currentline,newtext);
            verify_line();
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

int strings(string_action)
    enum string_option string_action;
{
        int i,count,max;
        char string_delimiter;

        string1[0] = '\0';
        e_string[0] = '\0';
        string2[0] = '\0';
        dashdashdash = 0;   /*  initialize to no dashes */
        if(string_action == no_strings) return;
        skip_spaces();
        if((c_in_string(c,delimiters)) && (!end_of_line)){
          string_delimiter = c;
          copy_string(string1,string_delimiter);
          if(string_action == two_strings){
            if(end_of_line)
              error("Two strings required", fatal);
            else
              copy_string(string2,string_delimiter);
          }
          else if(end_of_line)
                 error("Missing string delimiter", non_fatal);
          next_char();
        }
        else if(one_string == string_action)
          error("Missing required string", fatal);

        max = strlen(string1)-3;
        for(i=0;i<max;i++){
          if((string1[i]=='.' && string1[i+1]=='.' && string1[i+2]=='.') ||
             (string1[i]=='-' && string1[i+1]=='-' && string1[i+2]=='-')){
            if(string1[i] == '-') dashdashdash = 1;
            count = strlen(string1) - i - 3;
            copysubstr(string1,i+3,count,e_string);
            if(i > 0)
              string1[i] = '\0';  /*  truncate to just string1 */
            else{
              if(!dashdashdash){
               strcpy(string1,e_string);
               e_string[0] = '\0';
              }
              else string1[0] = '\0';
            }
            return;
          }
        }
        e_string[0]= '\0';
}

void filename()
{
        char s[80];
        int l;

        skip_spaces();
        if(end_of_line){
          strcpy(current_file,edit_file);
          return;
        }
        cc[strlen(cc)] = '\0';
        l = 0;
        while((c != ' ') && (c != ',') && !end_of_line ){
          s[l++] = c;
          next_char();
        }
        s[l] = '\0';
        strcpy(current_file,s);
}

void special_processing()
{
      int i;
        skip_spaces();
        switch( current_command.processor) {
        case CMD_brief :
          if(end_of_line) c='+';
          if(c_in_string(c,"+-")){
            default_actions.verify_command = (c == '-');
            next_char();
          }else
            error ("Syntax error", fatal);
          break;
        case CMD_verify :
          if(end_of_line) c='+';
          if(c_in_string(c,"+-")){
            default_actions.verify_command = (c == '+');
            next_char();
          }else
            error ("Syntax error", fatal);
          break;
        case CMD_lveto :
          if(end_of_line) c='+';
          if(c_in_string(c,"+-")){
            veto_line = (c == '+');
            next_char();
          }else
            error ("Syntax error", fatal);
          break;
        case CMD_veto :
          if(end_of_line) c='+';
          if(c_in_string(c,"+-")){
            veto_mode = (c == '+');
            next_char();
          }else
            error ("Syntax error", fatal);
          break;
        case CMD_deftab :
          if(end_of_line)
            tab_char = '\0';
          else{
            tab_char = c;
            next_char();
          }
          break;
        case CMD_delimit :
          if(end_of_line)
            delimiter = ';';
          else if(c_in_string(c,delimiters)){
            delimiter = c;
            next_char();
          }
          else
            error ("Bad delimiter", fatal);
          break;
        case CMD_rmargin :
        case CMD_lmargin :
        case CMD_wmargin :
          if(!end_of_line){
           if(current_command.processor != CMD_rmargin){
             i=decode_number();
             margins.left = i;
           }
           if(current_command.processor != CMD_lmargin ){
             i= decode_number();
             if(i < margins.left)
               error (" Right Margin < Left Margin", fatal);
             else
               margins.right = i;
           }
          }
          else
            fprintf(O.fileS,
                       " Left margin = %d, Right margin = %d\n",margins.left,
                       margins.right);
          break;
        case CMD_scan_mode :
          goto_top();
          if(end_of_line) c='+';
          if(c_in_string(c,"+-")){
            scan_mode = (c == '+');
            next_char();
          }else
            error ("Syntax error", fatal);
          break;
        case CMD_tabs :
          i = 1;
          last_tab_setting = i - 1;
          while(c_in_string(c,numbers)){
            tabs[i]= decode_number();
            if(i > 1){
              if(tabs[last_tab_setting] >= tabs[i]){
                tabs[i] = infinity;
                error ("Invalid Tab Setting", fatal);
                return;
              }
            }
            last_tab_setting = i++;
            skip_spaces();
          }
          tabs[i] = infinity;
          break;
        case CMD_system :
          string1[0] = c;
          i=1;
          while(!end_of_line){
           next_char();
           if(!end_of_line) string1[i++] = c;
          }
          if(strlen(input_line)){
           string1[i++] = delimiter;
           string1[i] = '\0';
           strcat(string1,input_line);
           input_line[0] = '\0';
          }
          else string1[i] = '\0';
          break;
        case CMD_z_command :
          end_of_line = TRUE;
          if(strlen(cc) > si){
            z_delimiter = c;
            copysubstr(cc,si,strlen(cc)-si+1,z_command_text);
          }else
            z_command_text[0] = '\0';
        }
}

int valid_scan_mode()
/*  return 1 if valid current_command valid during scan_mode  */
{
          switch(current_command.processor){
          case CMD_bottom :
          case CMD_copy :
          case CMD_quit :
          case CMD_jump :
          case CMD_lcopy :
          case CMD_locate :
          case CMD_locaten :
          case CMD_next :
          case CMD_nobells :
          case CMD_print :
          case CMD_help :
          case CMD_stop :
          case CMD_top :
          case CMD_where :
          case CMD_delimit:
          case CMD_brief:
          case CMD_verify:
          case CMD_rmargin:
          case CMD_lmargin:
          case CMD_lveto:
          case CMD_scan_mode:
          case CMD_veto:
          case CMD_wmargin:
          case CMD_deftab:
          case CMD_tabs:
           return(1);
          default:
           return(0);
        }
}

void process_command()
{
      long i,j,ii,jj;
      int found, start, length;
      char newtext[MaxStringR];

        do_prefix_actions();
/*      if(!end_of_file || (current_command.processor IN
              commands_valid_on_empty_file) AND (line_number = 0) THEN
*/
          if(scan_mode && !valid_scan_mode()){
            fprintf(O.fileS," Command not valid in Read Only mode\n");
            current_command.processor = CMD_error;
          }

          switch(current_command.processor){
          case CMD_add :
            add_lines(current_command.iteration_count);
            break;
          case CMD_bottom :
            advance(infinity);
            end_of_file = FALSE;
            verify_line();
            break;
          case CMD_change :
            j = infinity;
            i = current_command.iteration_count;
            found = locate_next();
            while(found){
              replace_string (string1,string2,found,&j);
              i--;
              if(i < 1) break;
              advance(1L);
              found =locate_next();
            }
            display_occurrences(i);
            break;
          case CMD_changes :
            i = current_command.iteration_count;
            found = locate_next();
            while(found){
              replace_string (string1,string2,found,&i);
              if(i < 1) break;
              advance (1L);
              found = locate_next();
            }
            display_occurrences (i);
            break;
          case CMD_copy :
            copy_to_file ( FALSE, current_command.iteration_count);
            break;
          case CMD_copy_delete :
            copy_to_file ( TRUE, current_command.iteration_count);
            break;
          case CMD_cut :
            cut_line();
            break;
          case CMD_delete :
            i = current_command.iteration_count;
            found =locate_next();
            while(found){
              verify_line();
              i--;
              if(line_veto(" Delete?")){
               currentline[0] = '\0';
               line_number--;
               modified = TRUE;
              }
              advance(1L);
              if(i<1) break;
              if(end_of_file)
               found = FALSE;
              else
               found = locate_next();
            }
            display_occurrences (i);
            break;
          case CMD_dup:
            i = current_command.iteration_count;
            if(strlen(currentline) <= 0 )  break;
            while(i-- > 0){
             writeline(CurWork, currentline);
             line_number++;
             modified = TRUE;
            }
            verify_line();
            break;
          case CMD_delimit:
          case CMD_brief:
          case CMD_verify:
          case CMD_rmargin:
          case CMD_lmargin:
          case CMD_lveto:
          case CMD_veto:
          case CMD_wmargin:
          case CMD_deftab:
          case CMD_tabs:
          case CMD_scan_mode :
            break;
          case CMD_expand :
            expand_lines();
            break;
          case CMD_help :
            strcpy(newtext,"Txedith ");
            if(strcmp(edit_file,current_file))
              strcat(newtext,current_file);
            else{
              show_help();
              strcat(newtext," \"\"");
            }
            i = system(newtext);
/*            if(i != 0 ) show_help();*/
            break;
          case CMD_jump :
            if(current_command.iteration_count > 0){
             i = current_command.iteration_count - line_number;
             if(i >= 0){
                 advance(i);
              } else{
                 goto_top();
                 advance(current_command.iteration_count - 1);
              }
              verify_line();
            }
            break;
          case CMD_file :
            i = 0;
            if(!mods_made && !modified){
             modified = TRUE;
             i = 1;
            }
            if(rewrite_file (current_file )){
             if(current_command.verify_command && DIALOG && !scan_mode)
              fprintf(O.fileS,"--- %s written ---\n",A.name);
            }
            if(i==1){
             mods_made = FALSE;
             modified = FALSE;
            }
            goto_top();
            break;
          case CMD_insert :
            if(command_name[0] == 'q' && DIALOG){
             fprintf(O.fileS,
                     " 0        1         2         3         4       ");
             fprintf(O.fileS,"  5         6         7     \n");
             fprintf(O.fileS,
                     " 12345678901234567890123456789012345678901234567");
             fprintf(O.fileS,"8901234567890123456789012345\n");
            }
            insert_lines(current_command.iteration_count);
            break;
          case CMD_insertb :
            if(command_name[0] == 'q' && DIALOG){
             fprintf(O.fileS,
                     " 0        1         2         3         4       ");
             fprintf(O.fileS,"  5         6         7     \n");
             fprintf(O.fileS,
                     " 12345678901234567890123456789012345678901234567");
             fprintf(O.fileS,"8901234567890123456789012345\n");
            }
            insertb_lines(current_command.iteration_count);
            break;
          case CMD_listab :
            if(last_tab_setting > 0 ){
               fprintf(O.fileS," Tab character = ");
               putchar(tab_char);
               fprintf(O.fileS,"  Tab settings = ");
               for(i=1;i<last_tab_setting;i++)
                 fprintf(O.fileS," %d",tabs[i]);
               fprintf(O.fileS," %d\n",tabs[i]);
            }
            else fprintf(O.fileS," No Tabs Set\n");
            break;
          case CMD_lcopy :
            i = current_command.iteration_count;
            found = locate_next();
            if(i == 0 && !found){
             y_command_text[0] = '\0';
             break;
            }
            while(found){
              copy_to_file ( FALSE, 1L);
              verify_line();
              i--;
              if(i < 1) break;
              advance (1L);
              if(end_of_file)
               found = FALSE;
              else
               found = locate_next();
            }
            display_occurrences (i);
            break;
          case CMD_locate :
            ldashdashdash = dashdashdash;
            strcpy(lstring1,string1);
            strcpy(le_string,e_string);
            i = current_command.iteration_count;
            found = locate_next();
            if(i == 0 && !found){
             y_command_text[0] = '\0';
             break;
            }
            while(found){
              verify_line();
              i--;
              if(i < 1) break;
              advance (1L);
              if(end_of_file)
               found = FALSE;
              else
               found = locate_next();
            }
            display_occurrences (i);
            break;
          case CMD_locaten :
            strcpy(string1,lstring1);
            strcpy(e_string,le_string);
            dashdashdash = ldashdashdash;
            i = current_command.iteration_count;
            advance (1L);
            if(!end_of_file) found = locate_next();
            if(i == 0 && !found){
             y_command_text[0] = '\0';
             break;
            }
            while(found){
              verify_line();
              i--;
              if(i < 1) break;
              advance (1L);
              if(end_of_file)
               found = FALSE;
              else
               found = locate_next();
            }
            display_occurrences (i);
            break;
          case CMD_modify :
            strcpy(string1,"\t");
            strcpy(string2,"        ");
            ii=string_in_string(currentline,string1);
            jj = infinity;
            if(ii)replace_string(string1,string2,ii,&jj);
            if(DIALOG) {
              fprintf(O.fileS,
                      " 0        1         2         3         4       ");
              fprintf(O.fileS,"  5         6         7     \n");
              fprintf(O.fileS,
                      " 12345678901234567890123456789012345678901234567");
              fprintf(O.fileS,"8901234567890123456789012345\n");
              fprintf(O.fileS," ");
              fprintf(O.fileS,"%s",currentline);
            }
            read_next(newtext,read_data);
            if(strlen(newtext) == 0 ) break;
            modify_line(newtext);
            verify_line();
            break;
          case CMD_next :
            if(current_command.iteration_count >= 0){
             advance (current_command.iteration_count);
            } else{
             i = line_number + current_command.iteration_count - 1;
             goto_top();
             advance(i);
            }
            verify_line();
            break;
          case CMD_nobells :
            bell_mode=0;
            break;
          case CMD_paste :
            strcpy(string2,currentline);
            string1[0] = '\0';
            currentline[0] = '\0';
            advance(1L);
            if(!end_of_file){
              j = 1;
              string2[strlen(string2)-1] = '\0';   /* remove newlinechar */
              replace_string (string1,string2,1,&j);
              line_number--;
            }
            else
              writeline(CurWork, string2);
            break;
          case CMD_print :
            if(command_name[0] == 'q' && DIALOG){
             fprintf(O.fileS,"0        1         2         3         4       ");
             fprintf(O.fileS,"  5         6         7     \n");
             fprintf(O.fileS,"12345678901234567890123456789012345678901234567");
             fprintf(O.fileS,"8901234567890123456789012345\n");
            }
            print_many_lines (current_command.iteration_count);
            break;
          case CMD_qmodify :
            i = current_command.iteration_count;
            strcpy(string1,"\t");
            strcpy(string2,"        ");
            jj = infinity;
            if(( ii=string_in_string(currentline,string1)) > 0 )
                replace_string(string1,string2,ii,&jj);
            if(DIALOG) {
              fprintf(O.fileS,
                      " 0        1         2         3         4       ");
              fprintf(O.fileS,"  5         6         7     \n");
              fprintf(O.fileS,
                      " 12345678901234567890123456789012345678901234567");
              fprintf(O.fileS,"8901234567890123456789012345\n");
              fprintf(O.fileS," ");
              fprintf(O.fileS,"%s",currentline);
            }
            read_next(newtext,read_data);
            if(strlen(newtext) == 0 ) break;
            while(i-- > 0){
             modify_line(newtext);
             verify_line();
             advance (1L);
             if(end_of_file) break;
             strcpy(string1,"\t");
             strcpy(string2,"        ");
             jj = infinity;
             if(( ii=string_in_string(currentline,string1)) > 0 )
                replace_string(string1,string2,ii,&jj);
            }
            break;
          case CMD_quit :
            goto_top();
            if(mods_made){
             rewrite_file(current_file );
             goto_top();
             if(current_command.verify_command && DIALOG && !scan_mode)
              fprintf(O.fileS,"--- %s rewritten ---\n",current_file);
            }
            if(error_in_command != fatal) terminate_edit = TRUE;
            break;
          case CMD_read :
            read_file();
            break;
          case CMD_replace :
            replace_lines(current_command.iteration_count);
            break;
          case CMD_stop :
            if(DIALOG) fprintf(O.fileS,"\n aborted \n");
            terminate_edit = TRUE;
            break;
          case CMD_top :
            goto_top();
            break;
          case CMD_truncate :
            i = current_command.iteration_count;
            if(current_command.window_access == windowed){
              length = right_window - left_window + 1;
              start = left_window-1;
            }
            else{
              length = margins.right;
              start = 0;
            }
            while(i-- > 0){
             strcpy(newtext, currentline);
             if(start < strlen(newtext)){
                copysubstr(newtext,start,length,currentline );
                strcat(currentline,"\n");
                modified = TRUE;
             }
             verify_line();
             if(i <= 0) break;
             advance (1L);
             if(end_of_file) break;
            }
            break;
          case CMD_restore:
            modified = FALSE;
            goto_top();
            break;
          case CMD_system :
            system(string1);
            break;
          case CMD_where :
            fprintf(O.fileS," Line = %d\n", line_number);
            break;
          case CMD_xedit :
            goto_top();
            currentline[0] = '\0';
            if(mods_made && !scan_mode){
             fprintf(O.fileS,"\n Save changes to %s ? ",edit_file);
             if(yes_or_no(" (Y or N)-")){
                 rewrite_file(edit_file );
                 if(current_command.verify_command && DIALOG )
                    fprintf(O.fileS,"--- %s rewritten ---\n",edit_file);
             }
            }

            initial_open = 1;
            strcpy(edit_file,current_file);
            if( initstream( &A, edit_file ,"r" )){
                fprintf(O.fileS," Creating File\n");
                A.endfile = 1;
                modified = TRUE;
                end_of_file = TRUE;
            }
            mods_made  = FALSE;
            CurStream = &A;
            CurWork = &B;
            B.filelen = 0;
            B.endfile = 0;
            goto_top();
            break;
          case CMD_yqmodify :
            i = current_command.iteration_count;
            strcpy(string1,"\t");
            strcpy(string2,"        ");
            jj = infinity;
            if(( ii=string_in_string(currentline,string1)) > 0 )
                replace_string(string1,string2,ii,&jj);
            read_next(newtext,read_data);
            if(strlen(newtext) == 0 ) break;
            while(i-- > 0){
             modify_line(newtext);
             verify_line();
             advance (1L);
             if(end_of_file) break;
             strcpy(string1,"\t");
             strcpy(string2,"        ");
             jj= infinity;
             if(( ii=string_in_string(currentline,string1)) > 0 )
                replace_string(string1,string2,ii,&jj);
            }
            break;
          case CMD_z_command :
            strcpy(y_command_text,z_command_text);
            break;
          case CMD_error :
            error ("Illegal command", fatal);
          default:
            fprintf(O.fileS,"Command = %s\n",command_string);
          }
         check_end_of_file();
         if(line_number == 0 && DIALOG)
            fprintf(O.fileS,"Warning: Empty File\n");
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
      while(c_in_string(c,prefixes)){
        if(c == '/')
          current_command.skip_count++;
        else if(c == '^'){
          current_command.start_at_top = TRUE;
          current_command.skip_count = 0;
        }
        else if(c == '+')
          current_command.input_from_command_line = !current_command.
                input_from_command_line;
        else if(c == 'x' || c == 'X')
          current_command.verify_command = !current_command.verify_command;
        next_char();
      }

      skip_spaces();
      if((c == '-') || (c == '.')) {
        if(c == '-') repeat_y = TRUE;
        next_char();
        i= decode_number();
        current_command.skip_count++;
        do_prefix_actions();
        if(!end_of_file){
          if(repeat_y)
            strcpy(y_command_text,z_command_text);
          else
            decode_command(previous_command_text);
        }
        else
          check_end_of_file();
        return;
      }

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

      if(command_table[ci].file_name) filename();

      strings(command_table[ci].string_op);
      if((dashdashdash && current_command.processor == CMD_change) ||
         (dashdashdash && current_command.processor == CMD_changes)){
        current_command.processor = CMD_error;
      }

      if(command_table[ci].count)
        current_command.iteration_count= decode_number();

      if(command_table[ci].special_parameters)
        special_processing();

      skip_spaces();
      if(!end_of_line || (error_in_command == fatal))
        current_command.processor = CMD_error;

      process_command();
      strcpy(previous_command_text,cc);

}

/*  ***************** main function *************************** */

main (argc, argv)
    int argc;  char *argv[];
{
    int error_flag=0,badarg=0,filea=0;
    int iarg,swval,create=0,i,argIO=0,argR=0;

    I.fileS=stdin;
    strcpy(I.name,"stdin");
    O.fileS=stdout;
    strcpy(O.name,"stdout");

    strcpy(input_line,"T;");
    /* determine the switches and files */
    for(iarg=1; iarg<argc; iarg++ ) {
        if( *argv[iarg] == '-' ) {
            strupr(argv[iarg]);
            if(!strcmp(argv[iarg],"-C")){
              create=1;
            }
            else if(!strcmp(argv[iarg],"-H")){
              show_help();
              exit(1);
            }
            else if(!strcmp(argv[iarg],"-R")){
              argR=1;
            }
            else if(!strcmp(argv[iarg],"-I")){
              if(initstream( &I, argv[++iarg] ,"r" )){
               fprintf(stderr," Error opening %s\n",I.name);
               exit(1);
              }
              DIALOG = 0;
            }
            else if(!strcmp(argv[iarg],"-O")){
              if(initstream( &O, argv[++iarg] ,"w" )){
               fprintf(stderr," Error opening %s\n",O.name);
               exit(1);
              }
            }
            else if(!strcmp(argv[iarg],"-T")){
              if(initstream( &T, argv[++iarg] ,"w" )){
               fprintf(stderr," Error opening %s\n",T.name);
               exit(1);
              }
            }
            else if(!strcmp(argv[iarg],"-IO")){
#ifndef msdos
              if(initstream( &I, "/dev/tty" ,"r+" )){
#else
              if(initstream( &I, "con" ,"r+" )){
#endif
               fprintf(stderr," Error opening %s\n",I.name);
               exit(1);
              }
              O.fileS = stderr;
              strcpy(O.name,"stderr");
/*            O.fileS = I.fileS;*/
/*            strcpy(O.name,I.name);*/
              DIALOG=1;
              argIO=1;
            }
            else badarg++;
            continue;
        }
        if(!filea)
            filea = iarg;
        else
            strcat(input_line,argv[iarg]);
    }
    if(!strcmp(input_line,"T;")) input_line[0] = '\0';

    /* check for proper arguments */
    if( badarg || !filea  ) {
        if(badarg) fprintf(stderr,"Bad or invalid arguments/switches\n\n\n");
        fprintf(stderr,
   "XEDIT file_name [-c] [-h] [-r] [-i ifile] [-o ofile] [-io] [-t trace]\n\n");
        fprintf(stderr,
   "   file_name = file to edit ('stdin' implies filter mode)\n");
        fprintf(stderr,"   -c  force creation of file_name \n");
        fprintf(stderr,"   -h  (help)-list commands \n");
        fprintf(stderr,"   -r  read only mode \n");
        fprintf(stderr,"   -i ifile  input file name (default='stdin')\n");
        fprintf(stderr,"   -o ofile  output file name (default='stdout')\n");
        fprintf(stderr,"   -io  allow interaction during filter mode\n");
        fprintf(stderr,"   -t trace  write commands to trace file\n");
        exit(1);
    }

    /* initialize the file streams. */
    modified = FALSE;
    initial_open = 1;
    strcpy(edit_file,argv[filea]);
    if(!strcmp(edit_file,"stdin")){
       A.fileS=stdin;
       strcpy(A.name,"stdin");
       /*  assume user has piped xedit */
       if(!strcmp(I.name,"stdin")){
         I.fileS=NULL;
         strcpy(I.name,"NULL");
       }
       if(!strcmp(O.name,"stdout")){
         O.fileS=stderr;
         strcpy(O.name,"stderr");
       }
       if(!argIO) DIALOG = 0;
    }
    else{
      if(create)
         error_flag  = initstream( &A, edit_file ,"w" );
      else{
         error_flag  = initstream( &A, edit_file ,"r" );
         if(error_flag){
           if(DIALOG) fprintf(O.fileS," Creating File\n");
           A.endfile = 1;
           modified = TRUE;
           end_of_file = TRUE;
           A.name[0] = '\0';
         }
      }
    }

    error_flag  = initstream( &B, "ZQUXVZXY" ,"temp" );
    error_flag  = initstream( &C, "ZQUXVZXZ" ,"temp" );
    if( error_flag ){
     fprintf(O.fileS,"Xedit aborted.%c\n",bell_mode);
     exit(2);
    }

    /* set remaining global constants */
    LineLengthR = LineLength + 1;
    CurStream = &A;
    CurWork = &B;
    terminate_edit = FALSE;
    default_actions = standard_actions;
    delimiter = ';';
    for(i=1;i>5;i++) tabs[i] = i*10;
    last_tab_setting = 5;

    /* display the selected options */
    if(DIALOG) fprintf(O.fileS,"Xedit (%s)\n",cdate);

    goto_top();
    if(argR) scan_mode = 1;


    while( end_of_input == 0 && !terminate_edit ){
      read_next(command_string,read_commands);
      if(strlen(command_string) == 0 )
        strcpy(command_string,"I*");
/*      if(T.fileS != NULL) fprintf(T.fileS,"%s\n",command_string);*/
      decode_command(command_string);
    }
    if(!terminate_edit){
       goto_top();
       rewrite_file(current_file );
    }
    if(strlen(A.name) > 0 )
     fclose(A.fileS);
    if(strlen(B.name) > 0 )
     fclose(B.fileS);
#ifdef XEDITTMP
     /*printf("B UNLINKED name is %s \n",B.name);*/
     unlink(B.name);
#endif
#ifdef XEDITTMP
    if(strlen(C.name) > 0 )
     fclose(C.fileS);
     /*printf("C UNLINKED name is %s \n",C.name);*/
     unlink(C.name);
#endif
    if(strlen(T.name) > 0 )
     fclose(T.fileS);
#ifdef msdos
    unlink("ZQUXVZXY");
    unlink("ZQUXVZXZ");
#endif
    exit(0);
}
#ifdef XEDITTMP
/*
============================================================================
Added so user can specify the directory that is used for temporary files
by using the environment variable XEDIT_TMP
JSU 04/19/93
============================================================================
*/
FILE *
xtmpfile(xedit_tmp2)
char *xedit_tmp2;
{
   char *xedit_tmp;
   FILE *tmpfile();
   FILE *p;
   char *getenv();
   int putenv();
   char sput[1024];
   strcpy(sput,"TMPDIR=");
   if((xedit_tmp=getenv("XEDIT_TMP")) == NULL)
   {
      p = tmpfile();
      return(p);
   }
   else
   {
      strcat(sput,xedit_tmp);
      putenv(sput);
      xedit_tmp = tempnam(xedit_tmp,"XeDiT");
      if((p = fopen(xedit_tmp,"w+")) == NULL)
      {
         return NULL;
      }
      else
      {
         strcpy(xedit_tmp2,xedit_tmp);
         /*printf("returned  file name is %s \n",xedit_tmp2);*/
         return p;
      }
   }
}
#endif 
