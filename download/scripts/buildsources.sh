#!/bin/bash
getsource.sh            # expand source archive file from my target location
supplemental_source.sh  # create user-supplied procedures "c" and "juown1" required for the calculator module
getprograms.sh          # get some programs to add to the procedures
makemakefile.sh         # create makefile
cp    /home/urbanjs/V600/LIBRARY/libGPF/EXE/DRAW/EXAMPLES/*.f90    tmp/PROGRAMS/M_DRAW_EXAMPLES/
cp    /home/urbanjs/V600/LIBRARY/libGPF/EXE/CALCOMP/EXAMPLES/*.f90 tmp/PROGRAMS/CALCOMP_EXAMPLES/
cp    /home/urbanjs/V600/LIBRARY/libGPF/EXE/NCURSES/EXAMPLES/*.f90 tmp/PROGRAMS/NCURSES_EXAMPLES/
cp -r /home/urbanjs/V600/LIBRARY/libGPF/EXE/HASHKEYS/test_vectors  tmp/PROGRAMS/
