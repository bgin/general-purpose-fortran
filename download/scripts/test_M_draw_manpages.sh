#!/bin/sh
#set -x
# @(#)Strip FORTRAN90 from specially-formatted man(1) pages and run demo programs
banner.sh test_M_draw_manpages.sh
#
# use to test man pages that follow special format and do not require interactive
# input from the terminal
#
# assume "program demo_*" to "end program demo_*" delimits the code
################################################################################
RUNTHEM(){
export TOPIC
for TOPIC in $*
do

 echo "topic $TOPIC" 1>&2

 rm -f /tmp/xxx_.F90
 CMD=$(which xxx_ 2>/dev/null)
 [ "$CMD" != '' ] && rm $CMD

 mank -s 3m_draw $TOPIC|
    col -b|
    expand|
    sed -n -e '\%^ *program  *demo_%,\%^ *end  *program  *demo_%{p}' >/tmp/xxx_.F90

 if [ -s /tmp/xxx_.F90 ]
 then
    cat /tmp/xxx_.F90
    export VOUTPUT="|ppmtogif >../doc/images/N_$TOPIC.3.3m_draw.gif"
    ccall /tmp/xxx_.F90 && (xxx_ </dev/null && rm /tmp/xxx_.F90)
 else
    echo "WARNING: demo case for $TOPIC not found"
 fi

done
}
################################################################################
export M_DRAW_DEVICE=p6

RUNTHEM $(mank -s 3m_draw -k M_draw|cprint 1)
################################################################################
exit
################################################################################
