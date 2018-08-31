#!/bin/bash
HERE=$(dirname $0)
HERE=$(cd $HERE;pwd)
export PATH=$HERE:$PATH
(
   cd tmp
   header.sh
   # set LC_ALL=C to make sure sort(1) does not sort case-insensitive without it being specified
   # if FILES is longer than the argument length of what(1) this will need redone
   FILES=$(find . -path './*/*' -prune -o -type f -printf '%P\n'|
      grep -v source.html |
      egrep '.*\.f.*|.*\.c' |
      env LC_ALL=C sort --ignore-case -t . -k 2r,2r -k 1,1 |
      xargs )
      what $FILES -html |
      grep -v '(3fp)'
   footer.sh
) >tmp/source.html
