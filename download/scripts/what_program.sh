#!/bin/bash
HERE=$(dirname $0)
HERE=$(cd $HERE;pwd)
export PATH=$HERE:$PATH
(
   header.sh
   cd tmp/PROGRAMS
   what $(find . -path './*/*' -prune -o -type f -printf '../PROGRAMS/%P ') -html|
   sed -n -e '\%^ *<td>%,\%^ *</td>%{
s/^/ZZZZ:/
s/ZZZZ:.*DESCRIPTION *://
/ZZZZ:/d
s/^/<td>/
s/$/<\/td>/
#\%^ *DESCRIPTION *:.*%{!d;p;n}
}' -e p
   footer.sh
) > tmp/html/programs.html
exit
