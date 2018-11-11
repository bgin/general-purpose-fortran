#!/bin/sh
####################################################################################################################################
#(@)# process doc files and convert to man(1) pages, html documents, ...
#set -x
####################################################################################################################################
#   Assuming most users would simply like the .f90 files, and not have
#   to install ufpp, what, goodbad, ccall, html2f90, html2man, and the
#   other components of the programming environment take the source archive
#   created by make.shell for the libGPF.a library and add a test program
#   and the "c" and "juown1" procedures required by the calculator module
#   and place them in a scratch directory and use makemake(1) to make a
#   simple make(1) file. Then put all that back into an archive file and
#   compress it for easy download from browsers.
####################################################################################################################################
#   Tools to help create man(1) pages
#    html2man  -- GNU commands take --help switch and then convert this to a man(1) page
#    txt2man   -- perl script converts text files following specified markdown rules into a man(1) page
#    manserver --
####################################################################################################################################
(
exec 2>&1
date
DIRNAME=`dirname $0`
cd $DIRNAME
export DIRNAME=`pwd`
export PUTMAN=$DIRNAME/tmp/man
export PUTHTML=$DIRNAME/tmp/html
export MANPATH=$DIRNAME/tmp/man
PATH=$PATH:$DIRNAME/scripts
source $DIRNAME/scripts/functions.sh
####################################################################################################################################
#  Unexpected
#  When changed NAME line in man(1) pages to format
#   M_modulename::topic - description
#  The mandb output looks to have been made from
#   filename (section) - description
#  As M_modulename:: does not show in a
#    man -k .
#  but
#    man -k M_modulename
#  shows the correct sections so the original data is in the database.
#  could name files M_modulename_topic but then "K" would not work in vim(1), for example.
#  apparently will have to use
#    topic - [M_modulename] description
####################################################################################################################################
#
#    XX                                      X
#   X                               X
#   X                               X
#  XXXX   XX  XX  XX XX    XXXXX   XXXX    XXX     XXXXX  XX XX    XXXXX
#   X      X   X   XX  X  X     X   X        X    X     X  XX  X  X     X
#   X      X   X   X   X  X         X        X    X     X  X   X   XXX
#   X      X   X   X   X  X         X        X    X     X  X   X      XX
#   X      X  XX   X   X  X     X   X  X     X    X     X  X   X  X     X
#  XXXX     XX XX XXX XXX  XXXXX     XX    XXXXX   XXXXX  XXX XXX  XXXXX
#
####################################################################################################################################
GETSECTION(){
# function to get section number from a document file
#echo "GETSECTION $NAME $SUFFIX" 1>&2
NAME=$1
SUFFIX=$2
   SHORTNAME=$(BASENAME $NAME $SUFFIX)
   SECTION=${SHORTNAME/*./}
   SECTION=${SECTION:-'7l'}
   SECTION_NUMBER=$(echo "$SECTION"|sed -e 's/[^0-9]*//g')
   SECTION_NUMBER=${SECTION_NUMBER:-'7'}
   SECTION_NAME=${SECTION_NUMBER}$(echo "$SECTION"|sed -e 's/^[0-9]*//')
   echo "SECTION=$SECTION SECTION_NUMBER=$SECTION_NUMBER SECTION_NAME=$SECTION_NAME"
}
####################################################################################################################################
# convert *.man files to make man(1) pages in tmp/man and HTML versions of man(1) pages in tmp/html
txt2man -h >doc/txt2man.1.man
# Order files will be processed
# *.htm   assumed to look like a man(1) page convert to flat txt and then run thru txt2man like a *.man file
# *.man   text files that look like a man(1) page that are to be run thru txt2man to make a *roff file.
# *.txt   if a line starts with a "." these are assumed to be *roff file files and go straight to  man page
# *.txt   if no line starts with a "." surround with .nf and .fi to make a primitive man(1) page
# *.htm   assumed to need a header and footer make into an HTML document and OVERWRITE file made from man(1) page
# *.html  copied as-is only to HTML directory, OVERWRITING anything files made by previous steps
# *.md
####################################################################################################################################
# make sure directories exist for man pages and convert files to man(1) pages
MAKEMANDIR(){
   pwd
   cd doc && (
      rm -rfv tmp
      mkdir -p tmp images
   )
#------------------------------------------------------------------------------#
echo 'Take *.htm pages and convert them to text *.man pages in tmp/'
mkdir -p ../tmp/html
for NAME in ${*:-*.[1-8]*.htm}
do
   NEWNAME=$(BASENAME $NAME .htm)
   echo "processing .htm file $NAME to tmp/$NEWNAME.man"
   (
   header.sh
   cat $NAME
   footer.sh
   ) | html2txt $NAME > tmp/$NEWNAME.man
done
#------------------------------------------------------------------------------#
echo 'Convert *.man pages to *roff files and install as man pages and html pages'
(cd $DIRNAME/doc;     man2man.sh)
(cd $DIRNAME/doc/tmp; man2man.sh)
#------------------------------------------------------------------------------#
echo 'Take *.[1-8]*.txt man pages and make into as-is man(1) pages and HTML pages'
for NAME in ${*:-*.[1-8]*.txt}
do
   export NAMEMAN=$(BASENAME $NAME .txt)
   export NAMEHTML=$NAMEMAN.html
   (
      # assume if a line starts with "." that already *roff
      if grep -q '^\.' $NAME
      then
              echo "copy man(1) page $NAME" 1>&2
         cat $NAME
      else
         echo 'make flat text $NAME into basic man page' 1>&2
         # assume flat text
         echo '.nf'
         cat $NAME|sed -e 's@\\@\\\\@g'
         echo '.fi'
      fi
   ) >tmp/$NAME
   echo "install $NAME as man page" 1>&2
   ccall tmp/$NAME
   rm -f tmp/$NAME
   echo "convert man page $NAME to html" 1>&2
   manserver $NAME >$PUTHTML/$NAMEHTML
done
#------------------------------------------------------------------------------#
echo 'Take *.htm pages and copy into tmp/html adding a header and footer'
echo 'done last to override what was made from man(1) pages'
mkdir -p ../tmp/html
for NAME in ${*:-*.[1-8]*.htm}
do
   NEWNAME=$(BASENAME $NAME .htm)
   echo "processing .htm file $NAME to $NEWNAME.html"
   (
   header.sh
   cat $NAME
   footer.sh
   ) > $PUTHTML/$NEWNAME.html
done
#------------------------------------------------------------------------------#
echo 'Take *.html pages and copy into $PUTHTML as-is'
echo 'Take *.html pages and make man(1) page if one has not been made of same name'
for NAME in ${*:-*.[1-8]*.html}
do
   echo "$NAME to $PUTHTML"
   cp $NAME $PUTHTML
   GETSECTION $NAME .html
   # if no man page of this name exists make a plain text one, assuming not suitable for formatting (or it would be a .htm page)
   # If it does not have a NAME line at the top it will cause problems with
   # manb(1). If it does not a ".nf" line in it after that, it will be formatted, which may or may not work.
   if [ ! -r "$PUTMAN/man$SECTION_NUMBER/$SHORTNAME.*" ]
   then
      html2txt $NAME >tmp/$SHORTNAME.txt
      ccall tmp/$SHORTNAME.txt
   fi
done
}
####################################################################################################################################
BLANKOUT(){
rm -rf   tmp/*             # ensure scratch directory is empty
mkdir -p tmp/PROGRAMS      # make sure scratch directories exist
mkdir -p tmp/PROGRAMS/M_DRAW_EXAMPLES
mkdir -p tmp/PROGRAMS/CALCOMP_EXAMPLES
mkdir -p tmp/PROGRAMS/NCURSES_EXAMPLES
mkdir -p tmp/html
for NUMBER in 1 2 3 4 5 6 7 8
do
   mkdir -p tmp/man/man$NUMBER
   mkdir -p tmp/man/man${NUMBER}f
   #break
   #continue
done
}
####################################################################################################################################
#
#                                                     XX
#                                                      X
#                                                      X
#  XXXXX   XXXXX  XXX X   XXX X    XXXX   XX XX    XXXXX   XXXXX
# X     X X     X  X X X   X X X       X   XX  X  X    X  X     X
# X       X     X  X X X   X X X   XXXXX   X   X  X    X   XXX
# X       X     X  X X X   X X X  X    X   X   X  X    X      XX
# X     X X     X  X X X   X X X  X    X   X   X  X    X  X     X
#  XXXXX   XXXXX  XX X XX XX X XX  XXXX X XXX XXX  XXXXXX  XXXXX
#
####################################################################################################################################
cat <<EOF
================================================================================
DIRNAME ....... $DIRNAME
PUTMAN ........ $PUTMAN
================================================================================
EOF
####################################################################################################################################
#TEST# cd doc; export PUTHTML=../tmp/html PUTMAN=../tmp/man NAME=minefield SECTION_NUMBER=1 ;MAN2MAN 1m_draw
####################################################################################################################################
BLANKOUT                # make empty tmp directory, removing previous output files
buildsources.sh         # get all the code source files except the demo files from the man(1) pages
echo 'create documents in tmp/doc, tmp/man, and tmp/html'
(MAKEMANDIR)
####################################################################################################################################
build.apropos
mank.sh           # make html index pages of the man pages
####################################################################################################################################
#echo 'copy permanent document repository to tmp area to create tar file from'
#cp -r -p doc tmp/
####################################################################################################################################
buildscripts.sh # put a few scripts into the tar file that might be of interest
####################################################################################################################################
cp /home/urbanjs/V600/LIBRARY/libGPF/draw/inc/draw.h tmp/
mkdir tmp/data/ tmp/fonts/
cp /home/urbanjs/V600/LIBRARY/libGPF/draw/hershey/data/hersh.oc tmp/data/
cp /home/urbanjs/V600/LIBRARY/libGPF/draw/hershey/data/hersh.or tmp/data/
cp -r -p /home/urbanjs/V600/LIBRARY/libGPF/draw/hershey/fonts/*.hmp tmp/fonts/
cp /home/urbanjs/V600/LIBRARY/libGPF/draw/hershey/hershey.sh tmp/
####################################################################################################################################
# extract test programs for M_pixel module and run them to test man(1) pages
# and generate GIF images for HTML versions of the man page, which man2html(1)
# always added to the end of a man(1) page so HTML documents include documents,
# which have not been able to reliably get from utilities converting markup to
# man(1) pages, info(1) pages  and HTML.
(
   cd $DIRNAME
   cd doc/images/
   banner.sh 'test pixel'
   env DISPLAY= test_M_pixel_manpages.sh
)
#----------------------------------------------------------------------------------------------------------------------------------#
# M_draw GIF images and manpage tests
env DISPLAY= test_M_draw_manpages.sh
#----------------------------------------------------------------------------------------------------------------------------------#
# M_calcomp GIF images and manpage tests
(
cd ../draw/calcomp/
ufpp UFPP_TEST -i M_calcomp.FF -system -verbose
)
#----------------------------------------------------------------------------------------------------------------------------------#
:  copy pixmap files needed to documentation
(
cd $DIRNAME
mkdir -p tmp/html/images
cp -p doc/images/* tmp/html/images/
cp -p ../html/icons/* tmp/html/images/
)
#----------------------------------------------------------------------------------------------------------------------------------#
(
cd $DIRNAME
mkdir -p tmp/html/
cp -r -p ../html/StyleSheets tmp/html/
)
####################################################################################################################################
buildbooks.sh # combine man pages into books
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'now that all procedure descriptions are in place make main index page download.html'
make_index.sh
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'build index of source file metadata using what(1) command'
what_source.sh
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'build index of program file metadata using what(1) command'
what_program.sh
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'extract demo programs from man(1) pages'
getdemo.sh
####################################################################################################################################
echo 'create tar file for downloading'
(cd tmp;tar cvfz ../GPF.tgz *)
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'build all the programs'
makeall.sh
#----------------------------------------------------------------------------------------------------------------------------------#
(doxygen.sh)
#----------------------------------------------------------------------------------------------------------------------------------#
date
)|tee LOG.setup_download.txt
####################################################################################################################################
exit
####################################################################################################################################
