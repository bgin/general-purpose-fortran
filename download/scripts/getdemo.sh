#!/bin/bash
# @(#) extract Fortran demo program from specially formatted man pages
# man pages that have a sample program in the form
#    program demo_*
#    end program demo_*
# have the section extracted into a file. Used to help verify man(1) pages
################################################################################
GETDEMO(){
TOPIC=$1
SECTION=$2
SECTION=${SECTION/(/}
SECTION=${SECTION/)/}
   FILE=${TOPIC}_${SECTION}_.F90
   man -s $SECTION $TOPIC|
      col -b|
      expand|
      sed -n -e '\%^ *program  *demo_%,\%^ *end  *program  *demo_%{p}' >$FILE
  
   if [ -s "$FILE" ]
   then
      :
      #ccall $FILE
      #ccall $FILE && (${TOPIC}_ </dev/null)
   else
      #rm $FILE
      echo "WARNING: demo case for $TOPIC not found"
   fi
}
################################################################################
export M_DRAW_DEVICE=p6
export M_DRAW_OUTPUT="|ppmtogif > $TOPIC.gif"
export MANPATH=`pwd`/tmp/man
mkdir -p tmp/PROGRAMS/DEMOS
cd tmp/PROGRAMS/DEMOS
man -k .|cprint 1 2 -delimiters '()' |while read NAME SECTION
do
 case "$SECTION" in
 2*|3*) GETDEMO $NAME $SECTION ;;
 esac
done
################################################################################
exit
################################################################################
