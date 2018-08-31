#!/bin/bash
####################################################################################################################################
banner.sh getsource.sh
GETSOURCE(){
#@(#) expand source archive file from my target location
(cd tmp || exit
   ar xv $HOME/lib/`sr`/GPF.a
   ## rename C source  that confuses makemake(1) because there is a NAME.f90 too
   ls *.c|grep -v '^C-'|xargs -iXX mv XX C-XX
   cp $HOME/LIBRARY/libGPF/draw/inc/draw.h .
)
}
GETSOURCE
####################################################################################################################################
exit
####################################################################################################################################
