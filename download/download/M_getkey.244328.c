#include <stdlib.h>
#include <stdio.h>
/* read keys in hot (raw I/O) mode until letter q is hit */
main () {
   char keyvalue;
   keyvalue = 0;
   fprintf (stdout, "press keys ('q' to quit)\n");
   fflush (stdout);
   fflush (stdin);
   while (keyvalue != 'q') {
      keyvalue = Fgetkey ();
      if(keyvalue != '\0'){
         fprintf (stdout, "C:KEY=%c %d\n", keyvalue, keyvalue);
      }else{
         usleep(5);
         fflush (stdout);
      }
   }
}
