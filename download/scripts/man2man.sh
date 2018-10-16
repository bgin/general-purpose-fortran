####################################################################################################################################
MAN2MAN(){

#echo 'MAN2MAN: take a *.man file and make man(1) page using txt2man(1) in tmp/man and HTML version of man(1) page in tmp/html'
SECTION=$1
#set -x -v
: NAME             $NAME
: SECTION          $SECTION
: SECTION_NUMBER   $SECTION_NUMBER
: PUTMAN           $PUTMAN
: PUTHTML          $PUTHTML

# convert *.man file to roff and then to html

# convert to roff and install and convert to html and install

echo "MAN2MAN: $NAME.$SECTION.man ==> $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION.gz"
echo "MAN2MAN: $NAME.$SECTION.man ==> $PUTHTML/$NAME.$SECTION.html"
export AUX_FILENAME="$NAME.$SECTION"
txt2man -s $SECTION_NAME -t "$NAME" $NAME.$SECTION.man|
   tee $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION|
   man2html >$PUTHTML/$NAME.$SECTION.html

gzip --force $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION

chmod a=r,u+w $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION.gz

}
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
####################################################################################################################################
MAN_TO_ROFF(){
(
for NAME in ${*:-*.[1-8]*.man}
do
   echo $NAME
   GETSECTION $NAME .man
   NAME=$(BASENAME $SHORTNAME .$SECTION)
   MAN2MAN $SECTION
done
)
}
####################################################################################################################################
DIRNAME=${DIRNAME:-$(dirname $0)/..}
export  PUTMAN=${PUTMAN:-$DIRNAME/tmp/man}
export PUTHTML=${PUTHTML:-$DIRNAME/tmp/html}

MAN_TO_ROFF $*
####################################################################################################################################
exit
####################################################################################################################################
