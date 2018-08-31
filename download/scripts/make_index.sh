#!/bin/bash
#@(#) make HTML page that is an index to the tar file contents
#set -x
banner.sh make_index

source $(dirname $0)/functions.sh

DOWNLOADHTML(){
(
header.sh
cat <<\EOF
<h1>Download Page</h1>
<h2>(for Fortran General Purpose Fortran Collection)</h2>

<p>
   There is one file to download for the libGPF library -
   <a href="../../GPF.tgz" style="background-color: yellow"
      title="
      It is easiest to pull all the GPF (General Purpose Fortran) sources
      code at one time in an archive file  because many of the modules use
      other GPF modules; and many included programs use multiple modules
      and routines.
      "
   > GPF.tgz </a>.
   This includes
<ul>


   <li>  <a href="man3.html"> manpages for the <em>procedures</em> </a> as *roff and HTML </li>

   <li>  selected <a href="programs.html"> example programs</a>
         <br/> Some of the most useful programs for developers are
       <ul>
       <li><a href="what_overview.7.html">An overview of what(1)</a>, 
           and <a href="what.1.html">what(1)</a> -- extract SCCS-style metadata from a file </li>
       <li><a href="ufpp_overview.7.html">An overview of ufpp(1)</a>,
           and <a href="ufpp.1.html">ufpp(1)</a> -- a source code preprocessor</li>
       <li><a href="numdiff.1.html"> numdiff(1)</a> -- Compare numeric differences in a file </li>
       <li><a href="makeout.1.html"> makeout(1)</a> -- Generate initial make(1) files for a directory</li>
       </ul>
   </li>
   <li>  <a href="man1.html"> manpages for the <em>example programs</em> </a> as *roff and HTML </li>

   <li><a href="../source.html" title="
      The what(1) command was run on the expanded source directory to produce
      a listing that shows what procedures are in which files: The what(1)
      command used is a customized version (included in this collection)
      that can generate simple HTML documents.
      "
      >the GPF(General Purpose Fortran) sources</a>
   </li>

   <li>a <a href="../Makefile">make(1) file</a> to build the source </li>

   <li> A start at manpages for the <a href="man3i.html"> <em>Fortran intrinsics</em></a> as *roff and HTML </li>

   <li> additional HTML documentation </li>

</ul>

<p>
   My own <a href="../../../FAQ/html/faq.html">FAQ on</a> Fortran is also available, with links to other Fortran resources.
</p>

</p>

</p>
   Assuming a recent version of gfortran is on your system you can
   download the file and place it in an empty directory and enter
</p>

<pre>
   tar xvfz GPF.tgz

   # if you are not on a platform with X11, ncurses, readline, and
   # sqlite you might have to remove some of the files from the 
   # Makefile, and change the compiler and loader options if not
   # using gfortran(1) and gcc(1). It is usually easiest to try
   # it and then handle any failures individually ...

   make

   # you might want to edit hershey.sh to select where hershey
   # font files will be installed (default is /usr/share/hershey), 
   # and then enter ...

   bash hershey.sh
</pre>

<p>
   This will compile the GPF procedures and sample utility programs.
</p>

<p>
   For other programming environments you will need to change the lines
   in the Makefile that define the compiler command. Many Fortran 2003
   features are required. Recently, I have only tested with GNU Fortran
   (GCC) 6.4.0 .
</p>

<hr>
<h3> MAN  pages:</h3>

<p>
   The man(1) pages are particularly useful when working with the
   code files when used with editors such as vim(1) and emacs(1).
   Several editors can now look up and display the man(1) pages for a
   routine from inside the editors. Place your cursor over a function
   name in the example programs and enter "K" in vim(1) to access the
   man(1) page for a procedure, for example.  Typically, you set the
   MANPATH environment variable to include the full pathname to the man/
   directory first. Something like
   <pre>
      export MANPATH=/home/$USER/GPF/man:$MANPATH::
   </pre>
   (assuming you installed the source in /home/$USER/GPF)
   will work, depending on what shell you are using.
</p>

<hr>
<h3> HTML pages:</h3>

<p>
   HTML documents are the most complete reference, as they include
   some higher-level overviews of the larger module files that are not
   currently included in the man(1) pages. Otherwise, the HTML documents
   are for the most part generated from the man(1) pages.
</p>

<p>
   Special HTML files
   use javascript to append groups of the HTML documents into a
   single manual.
</p>

EOF
################################################################################
(
   cd tmp/html
   # make sure sort(1) does not sort case-insensitive
   find . -type f -name 'BOOK_*.html' |
      env LC_ALL=C /usr/bin/sort|
      while read NAME
      do
         SHORTNAME=$(BASENAME $NAME .html)
         echo "<a href=\\\"$NAME\\\"> $SHORTNAME </a>|;"
       done |xargs|xargs -n 5 -d ';'|sed -e 's/|$//'|table2html -asis -delimiters '|'
)
################################################################################
cat <<\EOF

EOF
################################################################################
# make a table listing other files
echo '<h3> Alphabetical listing of HTML documents: </h3>'
(
cd tmp/html
find . -type f -name '*.html'|sort --ignore-case|
while read NAME
do
   SHORTNAME=$(BASENAME $NAME .html)
   case "$NAME" in  # there are some names used already that do not need listed
   download);;
   INDEX);;
   download);;
   man_?k);;
   *)
      echo "<a href=\\\"$NAME\\\"> $SHORTNAME </a>|;"
   ;;
   esac
done |xargs|xargs -n 5 -d ';'|sed -e 's/|$//'|table2html -asis -delimiters '|'
)
################################################################################
SKIP(){
# make a table listing .3.html files
echo '<h3> Procedures: </h3>'
(
cd tmp/html
find . -type f -name '*.3.html'|
while read NAME
do
   SHORTNAME=$(BASENAME $NAME .html)
   echo "<a href=\\\"$NAME\\\"> $SHORTNAME </a>|;"
done |xargs|xargs -n 5 -d ';'|sed -e 's/|$//'|table2html -asis -delimiters '|'
)
}
################################################################################
SKIP(){
# make a table listing .1.html files
echo '<h3> Commands: </h3>'
(
cd tmp/html
find . -type f -name '*.1.html'|
while read NAME
do
   SHORTNAME=$(BASENAME $NAME .html)
   echo "<a href=\\\"$NAME\\\"> $SHORTNAME </a>|;"
done |xargs|xargs -n 7 -d ';'|sed -e 's/|$//'|table2html -asis -delimiters '|'
)
}
################################################################################
cat <<\EOF
<h3>Footnotes:</h3>

<p>
   Most of the code is maintained using a custom programming
   environment. The original source files are mostly ufpp(1) input files
   and/or HTML documents.  To eliminate the need to install this custom
   environment all the source files have been expanded to standard
   Fortran and C source.
</p>

<p>
   To simplify building the resulting collection of code a make(1)
   file has been automatically generated using makeout(1) to compile
   the sources.
</p>

<p>
   Using a variety of utilities (primarily ufpp(1) and txt2man(1) )
   the help text is generally part of the source file and automatically
   converted to comments, flat text, HTML and man(1) pages.
</p>

<p>
   For those interested (particulary since ufpp(1) is one of the
   utilities included in this collection) most of the source is kept
   as ufpp(1) files.  ufpp(1) can write text blocks to various files.
   This allows basic documentation to easily be maintained with the
   source.  These blocks are then converted either into comments or help
   functions in the code by ufpp(1), but also optionally written to a
   common directory where they can be further processed, if required
   (they might already be HTML, which generally does not need further
   processing).  For simple routines the documentation is often written
   to look like a flat text version of a man(1) page; which has proven
   to be a good standard for documenting procedures, which many people
   are familiar with, and integrates into Unix/Linux environments and
   tools. The txt2man(1) utility is used to convert the flat files
   to *roff files; other files are run thru markdown(1) or LaTex(1)
   or other utilities as appropriate, based on the suffix of the files.
<p>

</p>
   The utility groff(1) was originally used to convert the man(1)
   pages to HTML and PDF but it has a bug as of this writing where it
   removes significant spaces when generating HTML; so the man(1) pages
   are now formatted using manserver.pl. Also see rman(1), html2man(1),
   and roff2html(1).  It sounds complicated but is totally automated
   just using a few bash(1) scripts to glue it together, and provides
   for an easy way to maintain documentation and source in the same file.
</p>

<p>
   <div class='property'> LINK: <a class='category_link' href='../../../GPF.html'>GPF home</a></div>
</p>

<div class="byline">
  Revised on Thu, Jul 21, 2016 11:27:17 PM
  by
  <a class="existingWikiWord" href="JSU.xhtml">JSU</a>
</div>
EOF
footer.sh
################################################################################
) 
}

(DOWNLOADHTML) > tmp/html/download.html
