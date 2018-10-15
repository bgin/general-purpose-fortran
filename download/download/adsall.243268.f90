program simple
! @(#) convert spreadsheet dump to USH data file
! this program takes a file of tab-delimited fields and uses them
! to create a binary input file for the USH plotting program.
! The file is a dump of a database, the format of which was
! determined by description from Mike Mankowski and inspection
! 19940805 John S. Urban
parameter(ibin=55)
!#-----------------------------------------------------------------------
integer icode
character(len=80)   :: title, title2
character(len=8)    :: varid
integer node,subnode,tertiary
integer units
character(len=20)   ::  alpha, tab*1, tab2*1
integer time
integer extra(5)
real array(400)
character(len=4000) ::  huge
character(len=11)   :: a(400),b(400),c(400)
character(len=2)    :: end
common/abc/a,b,c
   end=' '
   end(1:1)=char(92)
   tab=char(9)
   tab2=char(11)
!#-----------------------------------------------------------------------
!#    open output files.  ibin is a binary file in default USH format
   open(unit=ibin,file='binary.ush',access='sequential',form='unformatted',status='unknown')
!#-----------------------------------------------------------------------
!#     The first line of any USH file is an ASCII title line 80
!#     characters long. It will be the default title for plots drawn
!#     from this file's data (You may override this title on the plots).
!#
!#     More than one title line may be specified if all previous
!#     title lines end with a backslash '\'. Blank title lines
!#     should not be used; and currently not guaranteeing more
!#     than five printable title lines will be supported.
!#
!#     If the first character of a title line is a pound character
!#     it will not be printed, but will show in a dirf command. This
!#     allows comment information to be stored in the data file.
!#
   do 11 i11=1,2
      read(*,'(A)')huge
      call delim(huge,title,1,icount,ilenx,tab)
      ilen=len_trim(title)+1
      read(*,'(A)')huge
      call delim(huge,title2,1,icount,ilenx,tab)
      ilen2=len_trim(title2)+1
      title2(ilen2:ilen2)=end(i11:i11)
      title=title(:ilen)//title2(:ilen2)
      write(ibin)title
      write(*,*)title(:ilen+ilen2)
11 continue

!      Facility Name:  OSU
!      Test Number:   U0001
!      Date:   06/01/94
!      Time:   01:34 PM
!#-----------------------------------------------------------------------
   read(*,'(A)')huge
   call delim(huge,b,400,icount,ilenx,tab)
   write(*,*)'number of fields=',icount
   read(*,'(A)')huge
   call delim(huge,a,400,icount,ilenx,tab)
   write(*,*)'number of fields=',icount
   read(*,'(A)')huge
   call delim(huge,c,400,icount,ilenx,tab)
   write(*,*)'number of fields=',icount
!Channels 200 - 3    200     202     203     204     205     210     211
!Time    IntervalDP-611  FMM-205 FMM-701 FMM-802 FMM-901 LDP-602 LDP-606
!        (s)     "H2O    GPM     GPM     GPM     GPM     "H2O    "H2O    "


!#-----------------------------------------------------------------------
!#     The next section of the file is called the "header" section. This
!#     consists of a single line for each set of x or y values that
!#     specifies such things as
!#
!#       o  the units of the data (e.g. FT/SEC or Degrees Celsius).
!#       o  user-definable "categories" that let the user quickly find
!#          and select his curves for plotting and/or manipulating.
!#       o  information used to build the default legend for plots
!#          using legend labels.
!#
!#     The header elements are called:
!#
!# icode
!#   An arbitrary integer used only for categorizing data so that it
!#   may be easily listed or extracted by group. Several users use
!#   it as a numeric synonym for the "varid" field; as the built-in
!#   calculator handles only numbers. Others just number their curves
!#   here so they can more easily identify their origin if they extract
!#   them from one file and put them in another.
!#
!# varid
!#   A label of 1 to 8 characters that is the first part of the default
!#   legend and can be used in such commands as GET to extract or
!#   list groups of curves with a single command.
!#
!# node,         Arbitrary integer categories, useful in commands such as
!# subnode,      get and used in the default legend label.
!# tertiary node
!#
!# units
!#   An integer that is a shorthand for specifying the units that data
!#   represents. The subroutine "units" maps the number to specific
!#   unit strings that are used as the default axis labels.
!#   Use the UNITS command to see the list or see the appendix for a list
!#
!# alpha field
!#   A string of 1 to 20 characters used as the last part of the
!#   default label string.
!#
!#  time pointer
!#    A "pointer" that matches "y" values to default "x" values.
!#    the "x" values being pointed to are often time values.
!#    A value of 0 identifies the curve as a set of "x" values.
!#    The "y" values are matched to the "x" values by putting the
!#    numeric position of the "x" values in this field. For example,
!#    say records 3 and 4 describe velocity data in FT/S, and that
!#    record 5 is the shared matching "x" values for that data in
!#    SEC. The header records might contain data something like this:
!#
!#                                           time pointer --*
!# 3)   2FT/S        0    0    0  384 OBJECT ONE             5 0 0 0 0 0
!# 4)   2FT/S        0    0    0  384 OBJECT TWO             5 0 0 0 0 0
!# 5)   1TT          1    0    0  355                        0 0 0 0 0 0
!#
!* Extra integers
!*  There are five extra integers following the time record
!*  that are intended for internal use by the USH program.  Currently,
!*  field one has been  designated as containing the number of points in a
!*  curve,  but this is only  useful to people  mixing  curves in a single
!*  file that have a  different  number of points.  It should be left zero
!*  if all curves have the same number of points.
!*
!* Although  this file format is intended  to be very  stable,  alternate
!* forms may also become  supported.  This format was  generated  to meet
!* the specific  needs of the original  developers  and procurers of this
!* utility.  Others  can  be  easily  added,  but there  are  obvious
!* advantages to using only one or two formats.
!*
!* Note that the  "arbitrary"  code  categories  have  more  specific
!* meanings by  convention  in USH.  The  HEADER  command  also  allows
!* changing the values of extracted curves.
!*
!* The header section ends with a header line that has a "varid" of "END".
!*
!* Several  fields  are used to  build  the  default  legend  labels.  In
!* summary, a legend label is composed of:
!*
!*   varid  node subnode tertiary_node alpha
!*
!*   An example header section in ASCII:
!*
!#                  sub  tert.
!#icode varid  node node node units        alpha       time extra ------------->
!#*****--------*****-----*****-----********************-----*****-----*****-----*****
!#    1TT          1    0    0  355                        0    0    0    0    0    0
!#    7U          13    0    0  384NODE  13   ELEV10.50    1    0    0    0    0    0
!#    0END         0    0    0    0                        0    0    0    0    0    0
!#
!#    to be discussed:
!#    how about a few real fields for user and/or USH use?
!*    Like a user-preferred min and max value to draw the curve with?
!#    put in an option so only alpha field is used for default legend.
!#-----------------------------------------------------------------------
   read(*,'(A)')huge
   ijunk=0
   call str2arr(huge,400,array,inums,tab2,tab,ierr,ijunk) ! read a string into an array NOT USING CALCULATOR
   write(*,*)'number of columns is ',inums-1
!#-----------------------------------------------------------------------
!#    WRITE A TIME RECORD (a set of x values to be potentially shared)
!#-----------------------------------------------------------------------
   icode=1
   varid=a(1)
   node=0
   subnode=0
   tertiary=0
   units=355
!#-----------------------------------------------------------------------
   alpha=b(1)//c(1)
!#-----------------------------------------------------------------------
   time=0
!#-----------------------------------------------------------------------
   do 5 i5=1,5,1
      extra(i5)=0
5  continue
!#-----------------------------------------------------------------------
   write(ibin)icode,varid,node,subnode,tertiary,units,alpha,time,extra
!#-----------------------------------------------------------------------
!# WRITE CURVE HEADERS that use the above time curve for x values
!#-----------------------------------------------------------------------
   icode=2
   time=1             ! x values are one curve 1

   do 20 i20=2,inums-1
!#-----------------------------------------------------------------------
      varid=a(i20)
      if(varid(1:7).eq.'DAS-CTD')then
         write(*,*)'fixing DAS Names'
         varid='DASCTD'//a(i20)(8:9)
      elseif(varid(1:5).eq.'SC-TH')then
         write(*,*)'fixing SC-TH instrument names'
         varid='SCTH'//a(i20)(7:9)//a(i20)(11:11)
      elseif(varid(1:4).eq.'HPS-')then
         write(*,*)'fixing HPS instrument names'
         varid='HPS'//a(i20)(5:9)
      endif
!#-----------------------------------------------------------------------
      call findloc(varid,index,alpha) ! look up a nice alpha based on the varid string
      if(alpha.eq.' ')then
         write(*,*)'did not find varid=',varid
         alpha=b(i20)//c(i20)
         if(c(i20).eq.'"""H2O"')then
            write(*,*)'fixing H2O labels'
            alpha=b(i20)//'"H2O'
         endif
      endif
!#-----------------------------------------------------------------------
      node=i20
!#-----------------------------------------------------------------------
      if(c(i20).eq.'GPM')then
         units=0402
      elseif(c(i20).eq.'"H2O')then
         units=0190
      elseif(c(i20).eq.'"""H2O"')then
         units=0190
      elseif(c(i20).eq.'psig')then
         units=0260
      elseif(c(i20).eq.'psid')then
         units=0261
      elseif(c(i20).eq.'scfm')then
         units=0463
      elseif(c(i20).eq.'F')then
         units=0338
      elseif(c(i20).eq.'psia')then
         units=0259
      elseif(c(i20).eq.'kW')then
         units=0255
      elseif(c(i20).eq.'lbm')then
         units=0209
      elseif(c(i20).eq.'BTU/hr-ft^2')then
         units=0144
      elseif(c(i20).eq.'Deg F')then
         units=0338
      elseif(c(i20).eq.'Temp')then
         units=0338
      elseif(c(i20).eq.'dT')then
         units=0341
      else
         write(*,*)'unknown units for [',c(i20),']'
         units=0
      endif
!#-----------------------------------------------------------------------
!        (s)     "H2O    GPM     GPM     GPM     GPM     "H2O    "H2O    "
      write(ibin)icode,varid,node,subnode,tertiary,units,alpha,time,extra
20 continue
!#-----------------------------------------------------------------------
!#    TERMINATE THE HEADER SECTION
!#-----------------------------------------------------------------------
   varid='END'
   write(ibin)icode,varid,node,subnode,tertiary,units,alpha,time,extra
!#-----------------------------------------------------------------------
!#    WRITE OUT THE X,Y VALUES.  The curves share the same x values.
!#-----------------------------------------------------------------------
   icounts=1
1  write(ibin) (array(ii),ii=2,inums)
   read(*,'(a)',end=999)huge
   call str2arr(huge,400,array,inums2,tab2,tab,ierr,icounts) ! read a string into an array NOT USING CALCULATOR
   write(*,*)'icounts,inums2=',icounts,inums2
   if(inums2.ne.inums)then
      write(*,*)inums2,inums,'bad=',huge
      goto 888
   endif
!      read(*,101,end=999,err=888)(array(ii),ii=1,inums)
!101   format(8x,400(f7.0,1x))
   icounts=icounts+1
   goto 1
888 write(*,*)'error where icounts=',icounts
999 continue
   write(*,*)'number of points=',icounts
!#-----------------------------------------------------------------------
   close(ibin)
end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine str2arr(line,iread,numbrs,inums,delims,delimc,ierr,icc) ! read a string into an array NOT USING CALCULATOR
!     1989 John S. Urban
!     line=input string
!     iread=maximum number of values to try to read into numbrs
!     numbrs=real array to be filled with values
!     inums=number of values read (before error occurs if one does)
!     ierr==0 if no error, column number error string starts at
! given a line of structure , string , string , string process each
! string and store into an array. delimc and delims are only legal
! delimiters. no checking for more than can fit in numbrs.
! quits if encounters any errors in read.
   real numbrs(iread)
   character(len=*) :: line
   character(len=1) :: delims,delimc
   integer iend,iend1,iend2,inums,ilen
!x    parameter (delims=' ',delimc=',')
   ierr=0
   inums=0
   do 20 i20=len(line),1,-1
      ilen=i20
      if(line(i20:i20).ne.delims.and.line(i20:i20).ne.delimc.and.line(i20:i20).ne.char(0).and.line(i20:i20).ne.' ')goto 30
20 continue
!     command was totally composed of delimiters
   write(*,*)'*str2arr* blank line passed as a list of numbers'
   return
30 continue
!     there is at least one non-blank sub-string
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
   icol=1
   do 10 i10=1,iread,1
      iwhere=i10
200   if(line(icol:icol).ne.delims.and.line(icol:icol).ne.delimc)then
         istart=icol
         iend1=index(line(istart:ilen),delimc)
         iend2=index(line(istart:ilen),delims)
         iend=min(iend1,iend2)
         if(iend.eq.0)iend=max(iend1,iend2)
         if(iend.le.0)then
            call a2r(line(istart:ilen),rval,ier,iwhere,icc)
            if(ier.eq.0)then
               numbrs(i10)=rval
               inums=inums+1
            else
               ierr=istart
            endif
            return
         else
            iend=iend+istart-2
            call a2r(line(istart:iend),rval,ier,iwhere,icc)
            if(ier.eq.0)then
               numbrs(i10)=rval
               inums=inums+1
            else
               ierr=istart
               return
            endif
         endif
         icol=iend+2
      else
         icol=icol+1
         goto 200
      endif
!     last character in line was a delimiter, so only a null string is left
!     should not happen because only look out to last non-delimiter
      if(icol.gt.ilen)return
10 continue
!     error >>>>> more than iread numbers were in the line.
end subroutine str2arr
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine a2r(chars,valu,ierr,iwh,icc) ! returns a real value from a numeric character string NOT USING CALCULATOR
!     1989 John S. Urban
!
!     returns a real value from a numeric character string.
!
!  o  works with any g-format input, including integer, real, and
!     exponential.
!
!     if an error occurs in the read, iostat is returned in ierr and
!     value is set to zero.  if no error occurs, ierr=0.
!
   character(len=*)  :: chars, frmt*13
   character(len=11) :: a(400),b(400),c(400)
   common/abc/a,b,c
   ierr=0
   if(chars(1:5).eq.'000-')then
      write(*,*)'found a funny =',chars
!        chars(1:4)='    '
   elseif(index(chars,':').ne.0)then
      write(*,*)'assuming this is a time=',chars
      valu=0
      return
   elseif(chars.eq.'RAWHUGE')then
      write(*,*)'varid=',a(iwh)
      write(*,*)'set RAWHUGE in curve=',iwh,',subscript=',icc,' to 1'
      valu=1.0
      return
   elseif(chars.eq.'  RAWHUGE')then
      write(*,*)'varid=',a(iwh)
      write(*,*)'set RAWHUGE in curve=',iwh,',subscript=',icc,' to 1'
      valu=1.0
      return
   elseif(chars.eq.'HUGE+')then
      write(*,*)'varid=',a(iwh)
      write(*,*)'set HUGE+ in curve ',iwh,',subscript=',icc,' to 1'
      valu=1.0
      return
   elseif(chars.eq.'HUGE-')then
      write(*,*)'varid=',a(iwh)
      write(*,*)'set HUGE- in curve ',iwh,',subscript=',icc,' to 1'
      valu=1.0
      return
   endif
   write(frmt,101)len(chars)
101 format( '(bn,g',i5,'.0)')
   read(chars,fmt=frmt,iostat=ierr,err=999)valu
   return
999 valu=0.0
   write(*,*)'*a2r* - cannot produce number from this string'
   write(*,*)'[',chars,']'
   return
end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine delim(line,array,n,icount,ilen,dlim)
!     given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!     also count number of elements of array initialized, and return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more than n elements were found).
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
   character(len=4000) ::  line, dlim*1
   character array(n)*(*)
   integer icount,ilen
   icount=0
   ilen=len(line)
   if(ilen.eq.0)return !     command was totally blank
   ilen=len_trim(line)
   if(ilen.eq.0)then
      ilen=1
   endif
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
   idels=1
   icol=1
   do 100 iarray=1,n,1
200   if(line(icol:icol).ne.dlim)then
         idels=0
         istart=icol
         iend=index(line(istart:ilen),dlim)
         if(iend.le.0)then
            array(iarray)=line(istart:ilen)
            icount=iarray
            return
         else
            iend=iend+istart-2
            array(iarray)=line(istart:iend)
         endif
         icol=iend+2
      else
!=======================================================================
! if you want blank fields (all delimiter fields) ignored remove this section
         if(idels.ge.0)then
            idels=1
            array(iarray)=' '
            icount=iarray
            icol=icol+1
            goto 300
         else
            idels=1
         endif
!=======================================================================
         icol=icol+1
         goto 200
      endif
!     last character in line was a delimiter, so no text left (should not happen where blank=delimiter)
300   continue
      if(icol.gt.ilen)then
         icount=iarray
         return
      endif
100 continue
!     more than n elements
   icount=n
   return
end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine findloc(varnam,index,varout)
!
!     assuming an alphabetized array of character strings, find the location (index) where that name can be found, unless it is not
!     found -- in which case report where it should be placed as a negative index number.  it is assumed all variable names are
!     lexically greater than a blank string.
!
!     if it is found, return the number extracted from the right side of the string
!
!     finds the index assigned to a specific variable name.  assumes that the user index array is sorted in descending order
!     (highest at top).  if varnam is not found; return line number it should be placed at ; with a negative sign.
!
   use M_sort, only : sort_shell
   implicit real   (a-h,o-z)
   parameter(ic=772)                                                ! the number of commands
   parameter(ileft=8)
   character(len=28) :: ixn(ic), varnam*(*), varout*(*)
   integer maxtry
   save ixn ,ncall, imax
   data ncall/0/
!=======================================================================
   if(ncall.eq.0)then
      ncall=1
      ixn(  1)='DP-111  DP Upper Core Plate '
      ixn(  2)='DP-114  DP Uppr Supprt Plate'
      ixn(  3)='DP-121  DP btwn DVI-1 & CL-1'
      ixn(  4)='DP-122  DP btwn DVI-2 & CL-1'
      ixn(  5)='DP-123  DP btwn DVI-1 & CL-3'
      ixn(  6)='DP-124  DP btwn DVI-2 & CL-4'
      ixn(  7)='DP-125  HL-1 entrance losses'
      ixn(  8)='DP-126  HL-2 entrance losses'
      ixn(  9)='DP-128  DVI-1 entrnce losses'
      ixn( 10)='DP-129  DVI-2 entrnce losses'
      ixn( 11)='DP-130  Upper Head DP       '
      ixn( 12)='DP-201  CL-1 DP             '
      ixn( 13)='DP-202  RCP-2 DP            '
      ixn( 14)='DP-203  RCP-1 DP            '
      ixn( 15)='DP-204  CL-2 DP             '
      ixn( 16)='DP-205  RCP-3 DP            '
      ixn( 17)='DP-206  RCP-4 DP            '
      ixn( 18)='DP-207  CL-3 DP             '
      ixn( 19)='DP-208  CL-4 DP             '
      ixn( 20)='DP-209  HL-1 DP             '
      ixn( 21)='DP-210  HL-2 DP             '
      ixn( 22)='DP-211  SG-1 Shrt Tube Loss '
      ixn( 23)='DP-212  SG-2 Lng Tbe Ext Los'
      ixn( 24)='DP-213  SG-1 Lng Tbe Ext Los'
      ixn( 25)='DP-214  SG-2 Lng Tbe Ent Los'
      ixn( 26)='DP-215  HL Break DP         '
      ixn( 27)='DP-216  HL Break DP         '
      ixn( 28)='DP-401  ACC-1 Injection DP  '
      ixn( 29)='DP-402  ACC-2 Injection DP  '
      ixn( 30)='DP-501  CMT-1 Injection DP  '
      ixn( 31)='DP-502  CMT-2 Injection DP  '
      ixn( 32)='DP-503  CMT-1 Balnce Line DP'
      ixn( 33)='DP-504  CMT-2 Balnce Line DP'
      ixn( 34)='DP-601  ADS4-1 Separ Ent DP '
      ixn( 35)='DP-602  ADS4-2 Separ Ent DP '
      ixn( 36)='DP-611  PZR Surge Line DP   '
      ixn( 37)='DP-701  IRWST/DVI-1 Injec DP'
      ixn( 38)='DP-702  IRWST/DVI-2 Injec DP'
      ixn( 39)='DP-905  Break Separ Ent DP  '
      ixn( 40)='FDP-604 ADS-2 Flow          '
      ixn( 41)='FDP-605 ADS-1 Flow          '
      ixn( 42)='FDP-606 ADS-3 Flow          '
      ixn( 43)='FMM-001 SG-01 Feed Flow     '
      ixn( 44)='FMM-002 SG-02 Feed Flow     '
      ixn( 45)='FMM-201 CL-1 Loop Flow      '
      ixn( 46)='FMM-202 CL-2 Loop Flow      '
      ixn( 47)='FMM-203 CL-3 Loop Flow      '
      ixn( 48)='FMM-204 CL-4 Loop Flow      '
      ixn( 49)='FMM-205 DVI-1 Flow          '
      ixn( 50)='FMM-206 DVI-2 Flow          '
      ixn( 51)='FMM-401 ACC-1 Injection Flow'
      ixn( 52)='FMM-402 ACC-2 Injection Flow'
      ixn( 53)='FMM-501 CMT-1 Injection Flow'
      ixn( 54)='FMM-502 CMT-2 CL Bal Lin Flw'
      ixn( 55)='FMM-503 CMT-1 CL Bal Lin Flw'
      ixn( 56)='FMM-504 CMT-2 Injection Flow'
      ixn( 57)='FMM-601 ADS1-3 Loop Seal Flw'
      ixn( 58)='FMM-602 ADS4-2 Loop Seal Flw'
      ixn( 59)='FMM-603 ADS4-1 Loop Seal Flw'
      ixn( 60)='FMM-701 IRWST/DVI-1 Inj Flow'
      ixn( 61)='FMM-702 IRWST/DVI-2 Inj Flow'
      ixn( 62)='FMM-703 IRWST Overflow      '
      ixn( 63)='FMM-801 CVSP Discharge Flow '
      ixn( 64)='FMM-802 PRHR Inlet Flow     '
      ixn( 65)='FMM-803 RNSP to DVI-2 Flow  '
      ixn( 66)='FMM-804 PRHR Outlet Flow    '
      ixn( 67)='FMM-805 RNSP Discharge Flow '
      ixn( 68)='FMM-901 Pri Smp/DVI1 Inj Flw'
      ixn( 69)='FMM-902 Pri Smp/DVI2 Inj Flw'
      ixn( 70)='FMM-903 CR to Primary Sump  '
      ixn( 71)='FMM-905 Brk Sepa Lp Seal Flw'
      ixn( 72)='FVM-001 SG-1 Steam Flow     '
      ixn( 73)='FVM-002 SG-2 Steam Flow     '
      ixn( 74)='FVM-003 Stm Hdr Totl Stm Flw'
      ixn( 75)='FVM-004 City H2O/DI Sys Flw '
      ixn( 76)='FVM-601 ADS1-3 Sep Stm Flow '
      ixn( 77)='FVM-602 ADS4-2 Sep Stm Flow '
      ixn( 78)='FVM-603 ADS4-1 Sep Stm Flow '
      ixn( 79)='FVM-701 IRWST Steam Exhaust '
      ixn( 80)='FVM-901 BAMS HDR 6" Stm Flw '
      ixn( 81)='FVM-902 BAMS HDR 10" Stm Flw'
      ixn( 82)='FVM-903 Pri Sump Stm Exhaust'
      ixn( 83)='FVM-905 Brk Sep 6" Steam Flw'
      ixn( 84)='FVM-906 Brk Sep 8" Steam Flw'
      ixn( 85)='HFM-101 Ht Ls Up RPV Lvl I-I'
      ixn( 86)='HFM-102 Ht Ls Up RPV Lvl P-P'
      ixn( 87)='HFM-103 Ht Ls Lw RPV Mid Spc'
      ixn( 88)='HFM-104 Ht Ls Up RPV Lvl K-K'
      ixn( 89)='HFM-105 Heat Loss Top of RPV'
      ixn( 90)='HFM-106 Heat Loss Bot of RPV'
      ixn( 91)='HFM-107 Ht Ls CL2 flng @ RPV'
      ixn( 92)='HFM-108 Ht Ls CL4 flng @ RPV'
      ixn( 93)='HFM-109 Ht Ls HL2 flng @ RPV'
      ixn( 94)='HFM-110 Ht Ls DVI2 flg @ RPV'
      ixn( 95)='HFM-111 Ht Ls CL1 flng @ RPV'
      ixn( 96)='HFM-112 Ht Ls CL3 flng @ RPV'
      ixn( 97)='HFM-113 Ht Ls HL1 flng @ RPV'
      ixn( 98)='HFM-114 Ht Ls DVI1 flg @ RPV'
      ixn( 99)='HFM-201 Ht Loss CL1 Upstream'
      ixn(100)='HFM-202 Ht Loss CL2 Upstream'
      ixn(101)='HFM-203 Ht Loss CL3 Upstream'
      ixn(102)='HFM-204 Ht Loss CL4 Upstream'
      ixn(103)='HFM-205 Heat Losses from HL1'
      ixn(104)='HFM-206 Heat Losses from HL2'
      ixn(105)='HFM-301 Ht Ls SG1 bot HL Sid'
      ixn(106)='HFM-302 Ht Ls SG2 bot HL Sid'
      ixn(107)='HFM-303 Ht Ls SG1 Lwr Dwncmr'
      ixn(108)='HFM-304 Ht Ls SG2 Lwr Dwncmr'
      ixn(109)='HFM-305 Ht Ls SG1 Upr Dwncmr'
      ixn(110)='HFM-306 Ht Ls SG2 Upr Dwncmr'
      ixn(111)='HFM-401 Heat Loss from ACC-1'
      ixn(112)='HFM-402 Heat Loss from ACC-2'
      ixn(113)='HFM-501 Heat Loss CMT-1 Bot '
      ixn(114)='HFM-502 Heat Loss CMT-2 Bot '
      ixn(115)='HFM-503 Ht Ls CMT-1 50% Lev '
      ixn(116)='HFM-504 Ht Ls CMT-2 50% Lev '
      ixn(117)='HFM-505 Heat Loss CMT-1 Top '
      ixn(118)='HFM-506 Heat Loss CMT-2 Top '
      ixn(119)='HFM-507 Ht Ls CMT-1/CL-3 Bal'
      ixn(120)='HFM-510 Ht Ls CMT-2/CL-1 Bal'
      ixn(121)='HFM-601 Ht Ls ADS4-1 Upstrm '
      ixn(122)='HFM-602 Ht Ls Lwr PZR Htr Rg'
      ixn(123)='HFM-603 Ht Ls PZR Surge Line'
      ixn(124)='HFM-604 Heat Loss PZR bottom'
      ixn(125)='HFM-605 Heat Loss upper PZR '
      ixn(126)='HFM-606 Ht Ls ADS1-3 Cmn Lin'
      ixn(127)='HFM-607 Heat Loss PZR Top   '
      ixn(128)='HFM-608 Ht Ls ADS4-2 Upstrm '
      ixn(129)='HFM-701 Heat Loss Lowr IRWST'
      ixn(130)='HFM-702 Heat Loss Uppr IRWST'
      ixn(131)='HFM-703 Heat Loss Top IRWST '
      ixn(132)='HFM-801 Ht Los PRHR HX inlet'
      ixn(133)='HFM-802 Ht Ls PRHR HX outlet'
      ixn(134)='HFM-901 Heat Loss Prmry Sump'
      ixn(135)='HFM-902 Heat Loss Scnd Sump '
      ixn(136)='HFM-903 Ht Ls Pri Sump/DVI-1'
      ixn(137)='HFM-904 Ht Ls Pri Sump/DVI-2'
      ixn(138)='HPS201-1CL-1 Ht Trnsfr Coeff'
      ixn(139)='HPS201-2CL-1 Htr dT-Fld Temp'
      ixn(140)='HPS201-3CL-1 Fluid Temp     '
      ixn(141)='HPS202-1CL-2 Ht Trnsfr Coeff'
      ixn(142)='HPS202-2CL-2 Htr dT-Fld Temp'
      ixn(143)='HPS202-3CL-2 Fluid Temp     '
      ixn(144)='HPS203-1CL-3 Heat Trnsfr Coe'
      ixn(145)='HPS203-2CL-3 Htr dT-Fld Temp'
      ixn(146)='HPS203-3CL-3 Fluid Temp     '
      ixn(147)='HPS204-1CL-4 Ht Trnsfr Coeff'
      ixn(148)='HPS204-2CL-4 Htr dT-Fld Temp'
      ixn(149)='HPS204-3CL-4 Fluid Temp     '
      ixn(150)='HPS205-1HL-1 Ht Trnsfr Coeff'
      ixn(151)='HPS205-2HL-1 Htr dT-Fld Temp'
      ixn(152)='HPS205-3HL-1 Fluid Temp     '
      ixn(153)='HPS206-1HL-2 Ht Trnsfr Coeff'
      ixn(154)='HPS206-2HL-2 Htr dT-Fld Temp'
      ixn(155)='HPS206-3HL-2 Fluid Temp     '
      ixn(156)='HPS509-1CMT-1 Ht Trnsfr Coef'
      ixn(157)='HPS509-2CMT-1 Htr dT-Fld Tmp'
      ixn(158)='HPS509-3CMT-1 Fluid Temp    '
      ixn(159)='HPS512-1CMT-2 Ht Trnsfr Coef'
      ixn(160)='HPS512-2CMT-2 Htr dT-Fld Tmp'
      ixn(161)='HPS512-3CMT-2 Fluid Temp    '
      ixn(162)='HPS604-1PZR Srge Ht Trn Coef'
      ixn(163)='HPS604-2PZR Srge Ht dT-Fld T'
      ixn(164)='HPS604-3PZR Surge Fluid Temp'
      ixn(165)='HPS606-1ADS1-3 Cmn Inlt Coef'
      ixn(166)='HPS606-2ADS1-3 Cmn Inlt dT  '
      ixn(167)='HPS606-3ADS1-3 Cmn Inlt Fl-T'
      ixn(168)='HPS607-1Upr PZR Srge Ht Coef'
      ixn(169)='HPS607-2Upr PZR Srge Htr dT '
      ixn(170)='HPS607-3Upr PZR Srge Fld Tmp'
      ixn(171)='HPS801-1PRHR HX Ht Trns Coef'
      ixn(172)='HPS801-2PRHR HX Htr dT      '
      ixn(173)='HPS801-3PRHR HX Fluid Temp  '
      ixn(174)='KW-101  Group 1 Power       '
      ixn(175)='KW-102  Group 2 Power       '
      ixn(176)='KW-103  Group 1 Power       '
      ixn(177)='KW-104  Group 2 Power       '
      ixn(178)='KW-601  PZR Power           '
      ixn(179)='LCT-701 IRWST Load Cell     '
      ixn(180)='LCT-901 Prime Sump Load Cell'
      ixn(181)='LCT-902 Scnd Sump Load Cell '
      ixn(182)='LDP-001 FST Level           '
      ixn(183)='LDP-101 CL/Bypss Noz H2O Lvl'
      ixn(184)='LDP-102 CL/Bypss Noz H2O Lvl'
      ixn(185)='LDP-103 DVI/CL H2O Lvl (270)'
      ixn(186)='LDP-104 DVI/CL H2O Lvl (180)'
      ixn(187)='LDP-105 Upr Core Plt/DVI Lvl'
      ixn(188)='LDP-106 Bot Cor/Lwr Plt Lvl '
      ixn(189)='LDP-107 Bot Cor/Lwr Plt Lvl '
      ixn(190)='LDP-108 Bot Cor/Lwr Plt Lvl '
      ixn(191)='LDP-109 Lwr Cor Plt/Spcr Lvl'
      ixn(192)='LDP-110 Mid Spc/Upr Spcr Lvl'
      ixn(193)='LDP-112 Upr Cor Plt/DVI Lvl '
      ixn(194)='LDP-113 DVI/Bot Upr Sprt Plt'
      ixn(195)='LDP-115 Top Upr Sprt Plt/RPV'
      ixn(196)='LDP-116 Bot RPV/Bot Byp Hole'
      ixn(197)='LDP-117 Upr Cor Spcr/DVI Lvl'
      ixn(198)='LDP-118 Lw Cr Plt/Up Cor Plt'
      ixn(199)='LDP-119 Lw Cr Plt/Up Spc Grd'
      ixn(200)='LDP-127 Rctr Wide Range Lvl '
      ixn(201)='LDP-138 Up Cr Spc/Bt Sup Plt'
      ixn(202)='LDP-139 Top Lw Cr Plt/Up Spc'
      ixn(203)='LDP-140 Bt Rctr /Bt Flw Hole'
      ixn(204)='LDP-201 CL-1 Water Level    '
      ixn(205)='LDP-202 CL-2 Water Level    '
      ixn(206)='LDP-203 CL-3 Water Level    '
      ixn(207)='LDP-204 CL-4 Water Level    '
      ixn(208)='LDP-205 HL-1 Water Level    '
      ixn(209)='LDP-206 HL-2 Water Level    '
      ixn(210)='LDP-207 SG1/HL1 Elbw Level  '
      ixn(211)='LDP-208 SG2/HL2 Elbw Level  '
      ixn(212)='LDP-209 SG1 HL Plenum Level '
      ixn(213)='LDP-210 SG2 CL4 Plenum Level'
      ixn(214)='LDP-211 SG1 CL3 Plenum Level'
      ixn(215)='LDP-212 SG2 CL2 Plenum Level'
      ixn(216)='LDP-213 SG1 CL1 Plenum Level'
      ixn(217)='LDP-214 SG2 HL Plenum Level '
      ixn(218)='LDP-215 SG1 Long Tube HL Lvl'
      ixn(219)='LDP-216 SG2 Shrt Tube HL Lvl'
      ixn(220)='LDP-217 SG1 Shrt Tube HL Lvl'
      ixn(221)='LDP-218 SG2 Long Tube HL Lvl'
      ixn(222)='LDP-219 SG1 Long Tube CL Lvl'
      ixn(223)='LDP-220 SG2 Short Tube CL Lv'
      ixn(224)='LDP-221 SG1 Short Tube CL Lv'
      ixn(225)='LDP-222 SG2 Long Tube CL Lvl'
      ixn(226)='LDP-301 SG1 WR Level        '
      ixn(227)='LDP-302 SG2 WR Level        '
      ixn(228)='LDP-303 SG1 NR Level        '
      ixn(229)='LDP-304 SG2 NR Level        '
      ixn(230)='LDP-401 ACC-01 Level        '
      ixn(231)='LDP-402 ACC-02 Level        '
      ixn(232)='LDP-501 CMT-1 NR Lvl (Bottm)'
      ixn(233)='LDP-502 CMT-2 WR Level      '
      ixn(234)='LDP-503 CMT-1 NR Lvl (Middl)'
      ixn(235)='LDP-504 CMT-2 NR Lvl (Bottm)'
      ixn(236)='LDP-505 CMT-1 NR Level (Top)'
      ixn(237)='LDP-506 CMT-2 NR Lvl (Middl)'
      ixn(238)='LDP-507 CMT-1 WR Level      '
      ixn(239)='LDP-508 CMT-2 NR Level (Top)'
      ixn(240)='LDP-509 CL3/CMT1 Bal Lin Lvl'
      ixn(241)='LDP-510 CL1/CMT2 Bal Lin Lvl'
      ixn(242)='LDP-601 PZR WR Level        '
      ixn(243)='LDP-602 PZR Surge Line Level'
      ixn(244)='LDP-605 PZR Up Srge Pipe Lvl'
      ixn(245)='LDP-606 Surge Pipe Lvl @PZR '
      ixn(246)='LDP-607 PZR Mid Srg Pipe Lvl'
      ixn(247)='LDP-608 PZR Low Srg Pipe Lvl'
      ixn(248)='LDP-609 Surge Pipe Lvl @HL-2'
      ixn(249)='LDP-610 ADS1-3 Separator Lvl'
      ixn(250)='LDP-611 ADS4-1 Separator Lvl'
      ixn(251)='LDP-612 ADS4-2 Separator Lvl'
      ixn(252)='LDP-701 IRWST Level         '
      ixn(253)='LDP-801 PRHR HX Inl Head Lvl'
      ixn(254)='LDP-802 PRHR HX WR Level    '
      ixn(255)='LDP-901 Primary Sump Level  '
      ixn(256)='LDP-902 Secondary Sump Level'
      ixn(257)='LDP-903 CRT Level           '
      ixn(258)='LDP-905 Break Separator Lvl '
      ixn(259)='PT-001  MFP Dischrg Pressure'
      ixn(260)='PT-002  MS Header Pressure  '
      ixn(261)='PT-003  Lab Room Barometer  '
      ixn(262)='PT-101  CL1 Press @Rctr Flng'
      ixn(263)='PT-102  CL2 Press @Rctr Flng'
      ixn(264)='PT-103  CL3 Press @Rctr Flng'
      ixn(265)='PT-104  CL4 Press @Rctr Flng'
      ixn(266)='PT-107  Rctr Upr Head Press '
      ixn(267)='PT-108  Bottm Reactor Press '
      ixn(268)='PT-109  DVI1 Pres @Rctr Flng'
      ixn(269)='PT-110  DVI2 Pres @Rctr Flng'
      ixn(270)='PT-111  Reactor Annular Pres'
      ixn(271)='PT-112  Rctr Annular Pr @Bot'
      ixn(272)='PT-113  Rctr Blw Mid Spc Grd'
      ixn(273)='PT-201  SG1 Long Tube Press '
      ixn(274)='PT-202  HL2 Press @SG-2 Flng'
      ixn(275)='PT-203  CL Brk Pr @Brk Valve'
      ixn(276)='PT-204  SG2 Long Tube Press '
      ixn(277)='PT-205  HL1 Press @SG-1 Flng'
      ixn(278)='PT-206  HL Brk Pr @Brk Valve'
      ixn(279)='PT-301  SG-01 Pressure      '
      ixn(280)='PT-302  SG-02 Pressure      '
      ixn(281)='PT-401  ACC-01 Pressure     '
      ixn(282)='PT-402  ACC-02 Pressure     '
      ixn(283)='PT-501  CMT-01 Pressure     '
      ixn(284)='PT-502  CMT-02 Pressure     '
      ixn(285)='PT-602  PZR NR Pressure     '
      ixn(286)='PT-603  PZR WR Pressure     '
      ixn(287)='PT-604  PZR WR Pressure     '
      ixn(288)='PT-605  ADS1-3 Separtr Press'
      ixn(289)='PT-606  IRWST Sparger Press '
      ixn(290)='PT-610  ADS4-2 Separtr Press'
      ixn(291)='PT-611  ADS4-1 Separtr Press'
      ixn(292)='PT-701  IRWST Pressure      '
      ixn(293)='PT-801  CVSP Discharge Press'
      ixn(294)='PT-802  RNSP Discharge Press'
      ixn(295)='PT-901  Primary Sump Press  '
      ixn(296)='PT-902  BAMS Header Pressure'
      ixn(297)='PT-905  Break Separatr Press'
      ixn(298)='SC-101  TF101/CL3 Signl Cond'
      ixn(299)='SC-102  TF102/CL4 Signl Cond'
      ixn(300)='SC-105  TF105/CL1 Signl Cond'
      ixn(301)='SC-106  TF106/CL2 Signl Cond'
      ixn(302)='SC-140  TF140/HL2 Signl Cond'
      ixn(303)='SC-141  TF141/HL1 Signl Cond'
      ixn(304)='SC-205  TF205/HL1 Signl Cond'
      ixn(305)='SC-206  TF206/HL2 Signl Cond'
      ixn(306)='SC-301  TF301/SG1 Signl Cond'
      ixn(307)='SC-310  TF310/SG2 Signl Cond'
      ixn(308)='SC-608  TF608/PZR Signl Cond'
      ixn(309)='SC-715  TF715/IRWST Sgnl Con'
      ixn(310)='SC-902  TF902/Sec Smp Sg Con'
      ixn(311)='SC-903  TF-903/Pri Smp Sg Co'
      ixn(312)='SC-914  TF914/CRT Signl Cond'
      ixn(313)='SC-917  TF917/BAMS Sgnl Cond'
      ixn(314)='SCTH1013TH1013/Grp1 Sgnl Con'
      ixn(315)='SCTH1024TH1024/Grp1 Sgnl Con'
      ixn(316)='SCTH1033TH1033/Grp1 Sgnl Con'
      ixn(317)='SCTH1044TH1044/Grp1 Sgnl Con'
      ixn(318)='SCTH3043TH3043/Grp2 Sgnl Con'
      ixn(319)='SCTH3143TH3143/Grp2 Sgnl Con'
      ixn(320)='TF-001  RCP-1 Seal H2O Temp '
      ixn(321)='TF-002  RCP-2 Seal H2O Temp '
      ixn(322)='TF-003  RCP-3 Seal H2O Temp '
      ixn(323)='TF-004  RCP-4 Seal H2O Temp '
      ixn(324)='TF-005  Lab Ambient Temp Low'
      ixn(325)='TF-006  Lab Ambient Temp Mid'
      ixn(326)='TF-007  Lab Ambent Temp High'
      ixn(327)='TF-101  CL3/Reactor Flng Top'
      ixn(328)='TF-102  CL4/Reactor Flng Top'
      ixn(329)='TF-103  CL3/Reactor Flng Bot'
      ixn(330)='TF-104  CL4/Reactor Flng Bot'
      ixn(331)='TF-105  CL1/Reactor Flng Top'
      ixn(332)='TF-106  CL2/Reactor Flng Top'
      ixn(333)='TF-107  CL1/Reactor Flng Bot'
      ixn(334)='TF-108  CL2/Reactor Flng Bot'
      ixn(335)='TF-113  DVI1/Reactr Flng Top'
      ixn(336)='TF-114  DVI2/Reactr Flng Bot'
      ixn(337)='TF-115  DVI1/Reactr Flng Bot'
      ixn(338)='TF-116  DVI2/Reactr Flng Top'
      ixn(339)='TF-118  Lwr Rx Vssl Layr Y-Y'
      ixn(340)='TF-120  Top of Reactor      '
      ixn(341)='TF-126  Lwr Rx Vssl Layr A-A'
      ixn(342)='TF-127  Lwr Rx Vssl Layr A-A'
      ixn(343)='TF-128  Lwr Rx Vssl Layr C-C'
      ixn(344)='TF-129  Lwr Rx Vssl Layr C-C'
      ixn(345)='TF-130  Lwr Rx Vssl Layr G-G'
      ixn(346)='TF-131  Lwr Rx Vssl Layr G-G'
      ixn(347)='TF-132  Upr Rx Vssl Layr F-F'
      ixn(348)='TF-133  Upr Rx Vssl Layr F-F'
      ixn(349)='TF-134  Upr Rx Vssl Layr E-E'
      ixn(350)='TF-135  Upr Rx Vssl Layr E-E'
      ixn(351)='TF-140  HL2/Reactor Flng Top'
      ixn(352)='TF-141  HL1/Reactor Flng Top'
      ixn(353)='TF-142  HL2/Reactor Flng Bot'
      ixn(354)='TF-143  HL1/Reactor Flng Bot'
      ixn(355)='TF-147  Upr Rx Vssl Layr I-I'
      ixn(356)='TF-148  Upr Rx Vssl Layr I-I'
      ixn(357)='TF-149  Upr Rx Vssl Layr H-H'
      ixn(358)='TF-150  Upr Rx Vssl Layr H-H'
      ixn(359)='TF-151  Upr Rx Vssl Layr E-E'
      ixn(360)='TF-152  Upr Rx Vssl Layr E-E'
      ixn(361)='TF-153  Upr Rx Vssl Layr F-F'
      ixn(362)='TF-154  Upr Rx Vssl Layr F-F'
      ixn(363)='TF-155  Lwr Rx Vssl Layr G-G'
      ixn(364)='TF-156  Lwr Rx Vssl Layr G-G'
      ixn(365)='TF-157  Lwr Rx Vssl Layr C-C'
      ixn(366)='TF-158  Lwr Rx Vssl Layr C-C'
      ixn(367)='TF-162  Lwr Rx Vssl Layr A-A'
      ixn(368)='TF-163  Lwr Rx Vssl Layr A-A'
      ixn(369)='TF-164  Upr Rx Vssl Layr H-H'
      ixn(370)='TF-165  Upr Rx Vssl Layr H-H'
      ixn(371)='TF-166  Upr Rx Vssl Layr I-I'
      ixn(372)='TF-167  Upr Rx Vssl Layr I-I'
      ixn(373)='TF-168  Upr Rx Vssl Layr K-K'
      ixn(374)='TF-169  Upr Rx Vssl Layr M-M'
      ixn(375)='TF-170  Upr Rx Vssl Layr M-M'
      ixn(376)='TF-171  Top of Reactor Down '
      ixn(377)='TF-172  Lwr Rx Vsl Lyr AA-AA'
      ixn(378)='TF-173  Lwr Rx Vsl Lyr AA-AA'
      ixn(379)='TF-201  CL-1 @ RCP-1 Inlet  '
      ixn(380)='TF-202  CL-2 @ RCP-2 Inlet  '
      ixn(381)='TF-203  CL-3 @ RCP-3 Inlet  '
      ixn(382)='TF-204  CL-4 @ RCP-4 Inlet  '
      ixn(383)='TF-205  HL-1 @ SG-1 Outlet  '
      ixn(384)='TF-206  HL-2 @ SG-2 Outlet  '
      ixn(385)='TF-207  SG1 Shrt Tub Mid Otl'
      ixn(386)='TF-208  SG2 Shrt Tub Mid Otl'
      ixn(387)='TF-209  SG1 Shrt Tub Mid Inl'
      ixn(388)='TF-210  SG2 Shrt Tub Mid Inl'
      ixn(389)='TF-211  SG1 Long Tub Mid Otl'
      ixn(390)='TF-212  SG2 Long Tub Mid Otl'
      ixn(391)='TF-213  SG1 Long Tub Mid Inl'
      ixn(392)='TF-214  SG2 Long Tub Mid Inl'
      ixn(393)='TF-215  SG-01 Short Tube Top'
      ixn(394)='TF-216  SG-02 Short Tube Top'
      ixn(395)='TF-217  SG-01 Long Tube Top '
      ixn(396)='TF-218  SG-02 Long Tube Top '
      ixn(397)='TF-301  SG-01 Steam Header  '
      ixn(398)='TF-305  SG-01 Dwncmr HL Side'
      ixn(399)='TF-306  SG-02 Dwncmr HL Side'
      ixn(400)='TF-307  SG-01 Dwncmr CL Side'
      ixn(401)='TF-308  SG-02 Dwncmr CL Side'
      ixn(402)='TF-310  SG-02 Steam Header  '
      ixn(403)='TF-311  SG-01 Feed Header   '
      ixn(404)='TF-312  SG-02 Feed Header   '
      ixn(405)='TF-401  ACC-01 Outlet       '
      ixn(406)='TF-402  ACC-02 Outlet       '
      ixn(407)='TF-403  ACC-1 Tank Temp @Top'
      ixn(408)='TF-404  ACC-2 Tank Temp @Top'
      ixn(409)='TF-405  ACC-1 Injection Line'
      ixn(410)='TF-406  ACC-2 Injection Line'
      ixn(411)='TF-501  CMT-1 Long T/C Rod  '
      ixn(412)='TF-502  CMT-2 Injection Line'
      ixn(413)='TF-503  CMT1 @1/2 Lwr Hd Hgt'
      ixn(414)='TF-504  CMT-2 Long T/C Rod  '
      ixn(415)='TF-505  CMT1 @20% Vol Height'
      ixn(416)='TF-506  CMT2 @1/2 Lwr Hd Hgt'
      ixn(417)='TF-507  CMT-1 Long T/C Rod  '
      ixn(418)='TF-508  CMT2 @20% Vol Height'
      ixn(419)='TF-509  CMT-1 Long T/C Rod  '
      ixn(420)='TF-510  CMT-2 Long T/C Rod  '
      ixn(421)='TF-511  CMT1 @50% Vol Height'
      ixn(422)='TF-512  CMT-2 Long T/C Rod  '
      ixn(423)='TF-513  CMT-1 Long T/C Rod  '
      ixn(424)='TF-514  CMT2 @50% Vol Height'
      ixn(425)='TF-515  CMT-1 Long T/C Rod  '
      ixn(426)='TF-516  CMT-2 Long T/C Rod  '
      ixn(427)='TF-517  CMT1 @75% Vol Height'
      ixn(428)='TF-518  CMT-2 Long T/C Rod  '
      ixn(429)='TF-519  CMT-1 Long T/C Rod  '
      ixn(430)='TF-520  CMT2 @75% Vol Height'
      ixn(431)='TF-521  CMT1 @75% Vol Height'
      ixn(432)='TF-522  CMT-2 Long T/C Rod  '
      ixn(433)='TF-523  CMT-1 Long T/C Rod  '
      ixn(434)='TF-524  CMT2 @75% Vol Height'
      ixn(435)='TF-525  CMT1 @1/2 Upr Hd Hgt'
      ixn(436)='TF-526  CMT-2 Long T/C Rod  '
      ixn(437)='TF-527  CMT-1 Long T/C Rod  '
      ixn(438)='TF-528  CMT2 @1/2 Upr Hd Hgt'
      ixn(439)='TF-529  CMT-1 Long T/C Rod  '
      ixn(440)='TF-530  CMT-2 Long T/C Rod  '
      ixn(441)='TF-531  CMT-1 Balance Line  '
      ixn(442)='TF-532  CMT-2 Long T/C Rod  '
      ixn(443)='TF-533  CMT1/CL Balance Line'
      ixn(444)='TF-535  CMT-1 Injection Line'
      ixn(445)='TF-536  CMT2/CL Balance Line'
      ixn(446)='TF-537  CMT1 @20% Vol Height'
      ixn(447)='TF-538  CMT2 @20% Vol Height'
      ixn(448)='TF-539  CMT1 @50% Vol Height'
      ixn(449)='TF-540  CMT2 @50% Vol Height'
      ixn(450)='TF-541  CMT1 @60% Vol Height'
      ixn(451)='TF-542  CMT2 @60% Vol Height'
      ixn(452)='TF-543  CMT1 @75% Vol Height'
      ixn(453)='TF-544  CMT2 @75% Vol Height'
      ixn(454)='TF-546  CMT-02 Balance Line '
      ixn(455)='TF-547  CMT-1 Long T/C Rod  '
      ixn(456)='TF-548  CMT-2 Long T/C Rod  '
      ixn(457)='TF-549  CMT-1 Discharge Line'
      ixn(458)='TF-550  CMT-2 Discharge Line'
      ixn(459)='TF-551  CMT-1 Short T/C Rod '
      ixn(460)='TF-552  CMT-2 Short T/C Rod '
      ixn(461)='TF-553  CMT-1 Short T/C Rod '
      ixn(462)='TF-554  CMT-2 Short T/C Rod '
      ixn(463)='TF-555  CMT-1 Short T/C Rod '
      ixn(464)='TF-556  CMT-2 Short T/C Rod '
      ixn(465)='TF-557  CMT-1 Short T/C Rod '
      ixn(466)='TF-558  CMT-2 Short T/C Rod '
      ixn(467)='TF-559  CMT-1 Short T/C Rod '
      ixn(468)='TF-560  CMT-2 Short T/C Rod '
      ixn(469)='TF-561  CMT-1 Short T/C Rod '
      ixn(470)='TF-562  CMT-2 Short T/C Rod '
      ixn(471)='TF-563  CMT-1 Short T/C Rod '
      ixn(472)='TF-564  CMT-2 Short T/C Rod '
      ixn(473)='TF-601  PZR Surge @PZR Inlet'
      ixn(474)='TF-602  ADS1-3 Common Line  '
      ixn(475)='TF-603  PZR Surge Line @HL-2'
      ixn(476)='TF-605  PZR Water Space     '
      ixn(477)='TF-608  PZR H2O Spc @Htr Bnd'
      ixn(478)='TF-609  ADS4-1 Line Temp    '
      ixn(479)='TF-610  ADS4-2 Line Temp    '
      ixn(480)='TF-614  PZR Steam Vent Line '
      ixn(481)='TF-615  ADS1-3 Cmmn Line PZR'
      ixn(482)='TF-616  ADS1-3 Sprtr Lp Seal'
      ixn(483)='TF-617  ADS1-3 Sprtr Stm Out'
      ixn(484)='TF-618  ADS4-2 Lp Seal Temp '
      ixn(485)='TF-619  ADS4-1 Lp Seal Temp '
      ixn(486)='TF-620  ADS4-2 Inlet HL-2   '
      ixn(487)='TF-621  ADS4-1 Inlet HL-1   '
      ixn(488)='TF-622  ADS4-2 Sprtr Stm Out'
      ixn(489)='TF-623  ADS4-1 Sprtr Stm Out'
      ixn(490)='TF-701  IRWST/PRHR T/C Rod  '
      ixn(491)='TF-702  IRWST/PRHR T/C Rod  '
      ixn(492)='TF-703  IRWST/PRHR T/C Rod  '
      ixn(493)='TF-704  IRWST/PRHR T/C Rod  '
      ixn(494)='TF-705  IRWST/PRHR T/C Rod  '
      ixn(495)='TF-706  IRWST/PRHR T/C Rod  '
      ixn(496)='TF-707  IRWST/PRHR T/C Rod  '
      ixn(497)='TF-708  IRWST/PRHR T/C Rod  '
      ixn(498)='TF-709  IRWST/PRHR T/C Rod  '
      ixn(499)='TF-710  IRWST/PRHR T/C Rod  '
      ixn(500)='TF-711  IRWST/PRHR T/C Rod  '
      ixn(501)='TF-712  IRWST/PRHR T/C Rod  '
      ixn(502)='TF-713  IRWST Discharge DVI1'
      ixn(503)='TF-714  IRWST Discharge DVI2'
      ixn(504)='TF-715  IRWST Spargr T/C Rod'
      ixn(505)='TF-716  IRWST Spargr T/C Rod'
      ixn(506)='TF-717  IRWST Spargr T/C Rod'
      ixn(507)='TF-718  IRWST Spargr T/C Rod'
      ixn(508)='TF-719  IRWST Sparger Outlet'
      ixn(509)='TF-720  IRWST/DVI-2 Inj Line'
      ixn(510)='TF-721  IRWST/DVI-1 Inj Line'
      ixn(511)='TF-722  IRWST Steam Exhaust '
      ixn(512)='TF-723  IRWST/Pr Sump Ovrflw'
      ixn(513)='TF-801  CVSP Dischrg Hdr Tmp'
      ixn(514)='TF-802  RNSP Dischrg Hdr Tmp'
      ixn(515)='TF-803  PRHR HX Inlet       '
      ixn(516)='TF-804  PRHR HX Outlet      '
      ixn(517)='TF-805  PRHR HX Lng Tube Out'
      ixn(518)='TF-806  PRHR HX Shrt Tub Out'
      ixn(519)='TF-808  PRHR HX Shrt Tub Cnt'
      ixn(520)='TF-809  PRHR HX Lng Tube Cnt'
      ixn(521)='TF-810  PRHR HX Shrt Tub Inl'
      ixn(522)='TF-811  PRHR HX Lng Tube Inl'
      ixn(523)='TF-812  PRHR HX Outl Hd Temp'
      ixn(524)='TF-813  RNSP Dischrg to DVI1'
      ixn(525)='TF-814  RNSP Dischrg to DVI2'
      ixn(526)='TF-901  Pri Smp Inlt/Fill Ln'
      ixn(527)='TF-902  Scnd Smp Inlt/Fill  '
      ixn(528)='TF-903  Pri Smp Tmp @Inj Lin'
      ixn(529)='TF-904  Pri Smp/DVI2 Inj Lin'
      ixn(530)='TF-905  Pr Smp Tmp @Smp Xovr'
      ixn(531)='TF-906  Pr Smp Exhst to BAMS'
      ixn(532)='TF-907  Prim Sump Temp @Top '
      ixn(533)='TF-908  Brk Sprtr Inlt Temp '
      ixn(534)='TF-909  Pri Smp/DVI1 Inj Lin'
      ixn(535)='TF-910  CRP Dschrg to Pr Smp'
      ixn(536)='TF-911  CRP Dischrg to IRWST'
      ixn(537)='TF-912  Brk Sep Lp Seal Temp'
      ixn(538)='TF-913  Brk Sep Stm Outl Tem'
      ixn(539)='TF-914  CRP Suction Header  '
      ixn(540)='TF-915  Break Seprtr 6" Line'
      ixn(541)='TF-916  BAMS Header 10" Line'
      ixn(542)='TF-917  BAMS Header 6" Line '
      ixn(543)='TF-918  Break Seprtr 8" Line'
      ixn(544)='TFM-101 Thermocouple RX TE  '
      ixn(545)='TFM-102 Thermocouple RX TE  '
      ixn(546)='TFM-103 Thermocouple RX TE  '
      ixn(547)='TFM-104 Thermocouple RX TE  '
      ixn(548)='TFM-105 Thermocouple RX TE  '
      ixn(549)='TFM-106 Thermocouple RX TE  '
      ixn(550)='TFM-107 Thermocouple RX TE  '
      ixn(551)='TFM-108 Thermocouple RX TE  '
      ixn(552)='TFM-109 Thermocouple RX TE  '
      ixn(553)='TFM-110 Thermocouple RX TE  '
      ixn(554)='TFM-111 Thermocouple RX TE  '
      ixn(555)='TFM-112 Thermocouple RX TE  '
      ixn(556)='TFM-113 Thermocouple RX TE  '
      ixn(557)='TFM-114 Thermocouple RX TE  '
      ixn(558)='TFM-201 Thermocouple CL1    '
      ixn(559)='TFM-202 Thermocouple CL2    '
      ixn(560)='TFM-203 Thermocouple CL3    '
      ixn(561)='TFM-204 Thermocouple CL4    '
      ixn(562)='TFM-205 Thermocouple HL1    '
      ixn(563)='TFM-206 Thermocouple HL2    '
      ixn(564)='TFM-301 Thermocouple SG1    '
      ixn(565)='TFM-302 Thermocouple SG2    '
      ixn(566)='TFM-303 Thermocouple SG1    '
      ixn(567)='TFM-304 Thermocouple SG2    '
      ixn(568)='TFM-305 Thermocouple SG1    '
      ixn(569)='TFM-306 Thermocouple SG2    '
      ixn(570)='TFM-401 Thermocouple ACC1   '
      ixn(571)='TFM-402 Thermocouple ACC2   '
      ixn(572)='TFM-501 Thermocouple CMT1   '
      ixn(573)='TFM-502 Thermocouple CMT2   '
      ixn(574)='TFM-503 Thermocouple CMT1   '
      ixn(575)='TFM-504 Thermocouple CMT2   '
      ixn(576)='TFM-505 Thermocouple CMT1   '
      ixn(577)='TFM-506 Thermocouple CMT2   '
      ixn(578)='TFM-507 Thrmocpl CMT1/CL Bal'
      ixn(579)='TFM-510 Thrmocpl CMT2/CL Bal'
      ixn(580)='TFM-601 Thermocouple ADS04  '
      ixn(581)='TFM-602 Thermocouple PZR    '
      ixn(582)='TFM-603 Thrmcpl PZR Srg Line'
      ixn(583)='TFM-604 Thermocouple PZR    '
      ixn(584)='TFM-605 Thermocouple PZR    '
      ixn(585)='TFM-606 Thermocouple ADS1-3 '
      ixn(586)='TFM-607 Thermocouple PZR    '
      ixn(587)='TFM-608 Thermocouple ADS04  '
      ixn(588)='TFM-701 Thermocouple IRWST  '
      ixn(589)='TFM-702 Thermocouple IRWST  '
      ixn(590)='TFM-703 Thermocouple IRWST  '
      ixn(591)='TFM-801 Thermocouple PRHR HX'
      ixn(592)='TFM-802 Thermocouple PRHR HX'
      ixn(593)='TFM-901 Thermocple Pri Sump '
      ixn(594)='TFM-902 Thermocple Scnd Sump'
      ixn(595)='TFM-903 Thermocoupl Sump DVI'
      ixn(596)='TFM-904 Thermocoupl Sump DVI'
      ixn(597)='TH-101-1Rod B1-101 @28.13"  '
      ixn(598)='TH-101-2Rod B1-101 @ 34.13" '
      ixn(599)='TH-101-3Rod B1-101 @ 40.13" '
      ixn(600)='TH-101-4Rod B1-101 @ 46.13" '
      ixn(601)='TH-102-1Rod C1-102 @ 16.13" '
      ixn(602)='TH-102-2Rod C1-102 @ 22.13" '
      ixn(603)='TH-102-3Rod C1-102 @ 34.13" '
      ixn(604)='TH-102-4Rod C1-102 @ 40.13" '
      ixn(605)='TH-103-1Rod B1-103 @ 28.13" '
      ixn(606)='TH-103-2Rod B1-103 @ 34.13" '
      ixn(607)='TH-103-3Rod B1-103 @ 40.13" '
      ixn(608)='TH-103-4Rod B1-103 @ 46.13" '
      ixn(609)='TH-104-1Rod C1-104 @ 16.13" '
      ixn(610)='TH-104-2Rod C1-104 @ 22.13" '
      ixn(611)='TH-104-3Rod C1-104 @ 34.13" '
      ixn(612)='TH-104-4Rod C1-104 @ 40.13" '
      ixn(613)='TH-302-1Rod C2-302 @ 16.13" '
      ixn(614)='TH-302-2Rod C2-302 @ 22.13" '
      ixn(615)='TH-302-3Rod C2-302 @ 34.13" '
      ixn(616)='TH-302-4Rod C2-302 @ 40.13" '
      ixn(617)='TH-304-1Rod B2-304 @ 28.13" '
      ixn(618)='TH-304-2Rod B2-304 @ 34.13" '
      ixn(619)='TH-304-3Rod B2-304 @ 40.13" '
      ixn(620)='TH-304-4Rod B2-304 @ 46.13" '
      ixn(621)='TH-307-1Rod C2-304 @ 16.13" '
      ixn(622)='TH-307-2Rod C2-304 @ 22.13" '
      ixn(623)='TH-307-3Rod C2-304 @ 34.13" '
      ixn(624)='TH-307-4Rod C2-304 @ 40.13" '
      ixn(625)='TH-309-1Rod B2-309 @ 28.13" '
      ixn(626)='TH-309-2Rod B2-309 @ 34.13" '
      ixn(627)='TH-309-3Rod B2-309 @ 40.13" '
      ixn(628)='TH-309-4Rod B2-309 @ 46.13" '
      ixn(629)='TH-312-1Rod C2-312 @ 16.13" '
      ixn(630)='TH-312-2Rod C2-312 @ 22.13" '
      ixn(631)='TH-312-3Rod C2-312 @ 34.13" '
      ixn(632)='TH-312-4Rod C2-312 @ 40.13" '
      ixn(633)='TH-314-1Rod B2-314 @ 28.13" '
      ixn(634)='TH-314-2Rod B2-314 @ 34.13" '
      ixn(635)='TH-314-3Rod B2-314 @ 40.13" '
      ixn(636)='TH-317-1Rod C2-317 @ 16.13" '
      ixn(637)='TH-317-2Rod C2-317 @ 22.13" '
      ixn(638)='TH-317-3Rod C2-317 @ 34.13" '
      ixn(639)='TH-317-4Rod C2-317 @ 40.13" '
      ixn(640)='TH-319-1Rod B2-319 @ 28.13" '
      ixn(641)='TH-319-2Rod B2-319 @ 34.13" '
      ixn(642)='TH-319-3Rod B2-319 @ 40.13" '
      ixn(643)='TH-319-4Rod B2-319 @ 46.13" '
      ixn(644)='TH-501-1Rod B2-501 @ 28.13" '
      ixn(645)='TH-501-2Rod B2-501 @ 34.13" '
      ixn(646)='TH-501-3Rod B2-501 @ 40.13" '
      ixn(647)='TH-501-4Rod B2-501 @ 46.13" '
      ixn(648)='TH-503-1Rod B2-503 @ 28.13" '
      ixn(649)='TH-503-2Rod B2-503 @ 34.13" '
      ixn(650)='TH-503-3Rod B2-503 @ 40.13" '
      ixn(651)='TH-503-4Rod B2-503 @ 46.13" '
      ixn(652)='TH-505-1Rod B2-505 @ 28.13" '
      ixn(653)='TH-505-2Rod B2-505 @ 34.13" '
      ixn(654)='TH-505-3Rod B2-505 @ 40.13" '
      ixn(655)='TH-505-4Rod B2-505 @ 46.13" '
      ixn(656)='TH-507-1Rod B2-507 @ 28.13" '
      ixn(657)='TH-507-2Rod B2-507 @ 34.13" '
      ixn(658)='TH-507-3Rod B2-507 @ 40.13" '
      ixn(659)='TH-507-4Rod B2-507 @ 46.13" '
      ixn(660)='TH-601  PZR Heater Rod 1    '
      ixn(661)='TH-602  PZR Heater Rod 2    '
      ixn(662)='TH-603  PZR Heater Rod 3    '
      ixn(663)='TR-001-1Rod D-001 @ 10.50"  '
      ixn(664)='TR-001-2Rod D-001 @ 19.13"  '
      ixn(665)='TR-001-3Rod D-001 @ 25.13"  '
      ixn(666)='TR-001-4Rod D-001 @ 31.13"  '
      ixn(667)='TR-001-5Rod D-001 @ 37.13"  '
      ixn(668)='TR-001-6Rod D-001 @ 43.13"  '
      ixn(669)='TR-001-7Rod D-001 @ 49.13"  '
      ixn(670)='TR-001-8Rod D-001 @ 51.13"  '
      ixn(671)='TR-303-1Rod D-303 @ 10.51"  '
      ixn(672)='TR-303-2Rod D-303 @ 19.13"  '
      ixn(673)='TR-303-3Rod D-303 @ 25.13"  '
      ixn(674)='TR-303-4Rod D-303 @ 31.13"  '
      ixn(675)='TR-303-5Rod D-303 @ 37.13"  '
      ixn(676)='TR-303-6Rod D-303 @ 43.13"  '
      ixn(677)='TR-303-7Rod D-303 @ 49.13"  '
      ixn(678)='TR-303-8Rod D-303 @ 51.13"  '
      ixn(679)='TR-308-1Rod E-308 @ 22.13"  '
      ixn(680)='TR-308-2Rod E-308 @ 34.13"  '
      ixn(681)='TR-308-3Rod E-308 @ 46.13"  '
      ixn(682)='TR-313-1Rod D-313 @ 10.50"  '
      ixn(683)='TR-313-2Rod D-313 @ 19.13"  '
      ixn(684)='TR-313-3Rod D-313 @ 25.13"  '
      ixn(685)='TR-313-4Rod D-313 @ 31.13"  '
      ixn(686)='TR-313-5Rod D-313 @ 37.13"  '
      ixn(687)='TR-313-6Rod D-313 @ 43.13"  '
      ixn(688)='TR-313-7Rod D-313 @ 49.13"  '
      ixn(689)='TR-313-8Rod D-313 @ 51.13"  '
      ixn(690)='TR-318-1Rod F-318 @ 28.13"  '
      ixn(691)='TR-318-2Rod F-318 @ 40.13"  '
      ixn(692)='TR-318-3Rod F-318 @ 51.86"  '
      ixn(693)='TW-201  SG1 Shrt Tub Bot Otl'
      ixn(694)='TW-202  SG2 Shrt Tub Bot Otl'
      ixn(695)='TW-203  SG1 Shrt Tub Bot Inl'
      ixn(696)='TW-204  SG2 Shrt Tub Bot Inl'
      ixn(697)='TW-205  SG1 Lng Tube Bot Otl'
      ixn(698)='TW-206  SG2 Lng Tube Bot Otl'
      ixn(699)='TW-208  SG2 Lng Tube Bot Inl'
      ixn(700)='TW-209  SG1 Shrt Tub Top Otl'
      ixn(701)='TW-210  SG2 Shrt Tub Top Otl'
      ixn(702)='TW-216  SG2 Lng Tube Top Inl'
      ixn(703)='TW-501  CMT1 @2/3 Lwr Hd Hgt'
      ixn(704)='TW-502  CMT2 @2/3 Lwr Hd Hgt'
      ixn(705)='TW-503  CMT1 @2/3 Lwr Ttl Hd'
      ixn(706)='TW-504  CMT2 @2/3 Lwr Ttl Hd'
      ixn(707)='TW-505  CMT1 @20% Vol Height'
      ixn(708)='TW-506  CMT2 @20% Vol Height'
      ixn(709)='TW-507  CMT1 @20% Vol Height'
      ixn(710)='TW-508  CMT2 @20% Vol Height'
      ixn(711)='TW-509  CMT1 @20% Vol Height'
      ixn(712)='TW-510  CMT2 @20% Vol Height'
      ixn(713)='TW-511  CMT1 @20% Vol Height'
      ixn(714)='TW-512  CMT2 @20% Vol Height'
      ixn(715)='TW-513  CMT1 @50% Vol Height'
      ixn(716)='TW-514  CMT2 @50% Vol Height'
      ixn(717)='TW-515  CMT1 @50% Vol Height'
      ixn(718)='TW-516  CMT2 @50% Vol Height'
      ixn(719)='TW-517  CMT1 @50% Vol Height'
      ixn(720)='TW-518  CMT2 @50% Vol Height'
      ixn(721)='TW-519  CMT1 @50% Vol Height'
      ixn(722)='TW-520  CMT2 @50% Vol Height'
      ixn(723)='TW-521  CMT1 @50% Vol Height'
      ixn(724)='TW-522  CMT2 @50% Vol Height'
      ixn(725)='TW-523  CMT1 @50% Vol Height'
      ixn(726)='TW-524  CMT2 @50% Vol Height'
      ixn(727)='TW-525  CMT1 @50% Vol Height'
      ixn(728)='TW-526  CMT2 @50% Vol Height'
      ixn(729)='TW-527  CMT1 @60% Vol Height'
      ixn(730)='TW-528  CMT2 @60% Vol Height'
      ixn(731)='TW-529  CMT1 @60% Vol Height'
      ixn(732)='TW-530  CMT2 @60% Vol Height'
      ixn(733)='TW-531  CMT1 @75% Vol Height'
      ixn(734)='TW-532  CMT2 @75% Vol Height'
      ixn(735)='TW-533  CMT1 @75% Vol Height'
      ixn(736)='TW-534  CMT2 @75% Vol Height'
      ixn(737)='TW-535  CMT1 @75% Vol Height'
      ixn(738)='TW-536  CMT2 @75% Vol Height'
      ixn(739)='TW-537  CMT1 @75% Vol Height'
      ixn(740)='TW-538  CMT2 @75% Vol Height'
      ixn(741)='TW-539  CMT1 @Upr Ttl Hd Hgt'
      ixn(742)='TW-540  CMT2 @Upr Ttl Hd Hgt'
      ixn(743)='TW-541  CMT1 @Upr Ttl Hd Hgt'
      ixn(744)='TW-542  CMT2 @Upr Ttl Hd Hgt'
      ixn(745)='TW-543  CMT1 @2/3 Upr Hd Hgt'
      ixn(746)='TW-544  CMT2 @2/3 Upr Hd Hgt'
      ixn(747)='TW-545  CMT1 @2/3 Upr Hd Hgt'
      ixn(748)='TW-546  CMT2 @2/3 Upr Hd Hgt'
      ixn(749)='TW-547  CMT1 @1/3 Upr Hd Hgt'
      ixn(750)='TW-548  CMT2 @1/3 Upr Hd Hgt'
      ixn(751)='TW-549  CMT1 @1/3 Upr Hd Hgt'
      ixn(752)='TW-550  CMT2 @1/3 Upr Hd Hgt'
      ixn(753)='TW-551  CMT1 @1/3 Upr Hd Hgt'
      ixn(754)='TW-552  CMT2 @1/3 Upr Hd Hgt'
      ixn(755)='TW-553  CMT1 @1/3 Upr Hd Hgt'
      ixn(756)='TW-554  CMT2 @1/3 Upr Hd Hgt'
      ixn(757)='TW-555  CMT1 @1/3 Upr Hd Hgt'
      ixn(758)='TW-556  CMT2 @1/3 Upr Hd Hgt'
      ixn(759)='TW-601  ADS1-3 Sep Wall Temp'
      ixn(760)='TW-602  ADS4-2 Sep Wall Temp'
      ixn(761)='TW-603  ADS4-1 Sep Wall Temp'
      ixn(762)='TW-801  PRHR HX Lng Tub Outl'
      ixn(763)='TW-802  PRHR HX Shrt Tub Otl'
      ixn(764)='TW-803  PRHR Lng Tub Lwr Mid'
      ixn(765)='TW-804  PRHR Shrt Tub Lw Mid'
      ixn(766)='TW-805  PRHR Shrt Tub Up Mid'
      ixn(767)='TW-806  PRHR Lng Tub Upr Mid'
      ixn(768)='TW-807  PRHR HX Shrt Tub Inl'
      ixn(769)='TW-808  PRHR HX Lng Tube Inl'
      ixn(770)='TW-905  Break Sep Wall Temp '
      ixn(771)='AAAAAAAA                    '
      ixn(772)='zzzzzzzz                    '
      call sort_shell(ixn,order='d',startcol=1,endcol=ileft)
   endif
!=======================================================================
   varout=' '
   imin=1
   imax=ic
   maxtry=int(log(float(imax))/log(2.0)+1.0)
   index=(imax+1)/2
   do 10 i10=1,maxtry
      if( varnam.eq.ixn(index)(1:ileft) )then                               ! found a match
         varout=ixn(index)(ileft+1:)
         return
      else if(varnam.gt.ixn(index)(1:ileft))then
         imax=index-1
      else
         imin=index+1
      endif
      if(imin.gt.imax)then
         index=-imin
         if(iabs(index).gt.imax)then
!            write(*,*)'error 03 in findloc'
            return
         endif
         return
      endif
      index=(imax+imin)/2
      if(index.gt.imax.or.index.le.0)then
!         write(*,*)'error 01 in findloc'
         return
      endif
10 continue
!      write(*,*)'error 02 in findloc'
   return
end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
