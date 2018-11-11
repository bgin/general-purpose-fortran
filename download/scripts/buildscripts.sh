#!/bin/bash
echo 'put a few scripts into the tar file that might be of interest'
mkdir -p tmp/scripts/
for NAME in   \
   txt2man    \
   man2html   \
   manserver  \
   goodbad    \
   mank       \
   makemake   \
   inpath     \
   h          \
   manvi      \
   $NULL
do
   cp `which $NAME`  tmp/scripts/
done
cp -r $(dirname $(which manserver) )/manserver_dir/ tmp/scripts/
cp /home/urbanjs/.twm/scripts_vi/vimrc tmp/
cp /home/urbanjs/.twm/scripts_vi/exrc tmp/
