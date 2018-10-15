/* -------------------------------------------------------------------------- */
#include <stdio.h>
#include <stdlib.h>
main(){
   char string[20];
   int ii;
   ii=20;
   my_uname("m",string,&ii);
   fprintf (stderr, "C TEST PROGRAM:my_uname:AFTER:%s\n", string);
   exit(0);
}
