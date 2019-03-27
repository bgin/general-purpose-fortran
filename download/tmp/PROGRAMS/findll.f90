subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       findll(1f) - [FILE FILTER] find long lines                               ',&
'SYNOPSIS                                                                        ',&
'       findll [FILENAMES] [-l LENGTH] [-wrap] | [-help| -version]               ',&
'DESCRIPTION                                                                     ',&
'       find lines in files over a specified length and print                    ',&
'       them; or wrap each input line to fit in specified width.                 ',&
'                                                                                ',&
'       Non-printable characters are not treated specially (eg. a                ',&
'       tab character is treated as a single character).                         ',&
'OPTIONS                                                                         ',&
'       FILENAMES  the files to scan for long lines                              ',&
'       -l NUMBER  maximum line length of lines to ignore.                       ',&
'                  The default is 132.                                           ',&
'       --wrap     instead of locating and displaying long                       ',&
'                  lines, fold the lines at the specified                        ',&
'                  line length                                                   ',&
'                                                                                ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'EXAMPLES                                                                        ',&
'       Sample commands:                                                         ',&
'                                                                                ',&
'        $ findll <filename                                                      ',&
'                                                                                ',&
'        # show lines over 72 characters in length                               ',&
'        $ findll *.f *.F -l 72                                                  ',&
'        # show length of all lines on stdin                                     ',&
'        $ findll -l -1                                                          ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        findll(1f) - [FILE FILTER] find long lines
!!##SYNOPSIS
!!
!!        findll [FILENAMES] [-l LENGTH] [-wrap] | [-help| -version]
!!##DESCRIPTION
!!        find lines in files over a specified length and print
!!        them; or wrap each input line to fit in specified width.
!!
!!        Non-printable characters are not treated specially (eg. a
!!        tab character is treated as a single character).
!!##OPTIONS
!!        FILENAMES  the files to scan for long lines
!!        -l NUMBER  maximum line length of lines to ignore.
!!                   The default is 132.
!!        --wrap     instead of locating and displaying long
!!                   lines, fold the lines at the specified
!!                   line length
!!
!!        --help     display this help and exit
!!        --version  output version information and exit
!!##EXAMPLES
!!
!!        Sample commands:
!!
!!         $ findll <filename
!!
!!         # show lines over 72 characters in length
!!         $ findll *.f *.F -l 72
!!         # show length of all lines on stdin
!!         $ findll -l -1
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        findll(1f)>',&
'@(#)DESCRIPTION:    find long lines>',&
'@(#)VERSION:        23.1 20160618>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Mon, Mar 25th, 2019 12:00:42 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program findll
use M_kracken, only : kracken, sgets, lget, iget
use M_io, only      : read_line
use M_strings, only : v2s, switch
use ISO_FORTRAN_ENV, only: error_unit ! compiler_options,compiler_version,input_unit,output_unit
implicit none

character(len=*),parameter::ident_1="@(#)findll(1f): find long lines"

character(len=:),allocatable :: line
character(len=:),allocatable :: filenames(:)
character(len=1024)          :: msg
integer                      :: i
integer                      :: ilen
integer                      :: ilines
integer                      :: ios
integer                      :: ilength
integer                      :: isize
logical                      :: wrap
character(len=:),allocatable :: fmt

   call kracken('findll',' -oo -l 132 -wrap .f.              &
                        &             -help .f. -version .f. ')   ! define and crack command line arguments
   call help_usage(lget('findll_help'))                           ! process -help switch
   call help_version(lget('findll_version'))                      ! process -version switch
   wrap=lget('findll_wrap')                                       ! test if -wrap    switch is present on command line
   ilength=max(0,iget('findll_l'))
   filenames=sgets('findll_oo')
   isize=size(filenames)                                          ! number of words in default list

   fmt='('//v2s(max(1,ilength))//'a1)'                            ! make a format that writes array at specified length

   SELECT_DATA: select case(isize)
   case(0)                                                        ! no filenames, read data from stdin
      ilines=0
      STDIN: do while (read_line(line)==0)                        ! read lines of arbitrary length
         ilines=ilines+1
         ilen=len_trim(line)
         if(wrap)then                                             ! if wrapping lines
            if(ilen.gt.ilength)then
               write(*,fmt)switch(line)                           ! if line needs wrapper write it as an array of chars
            else
               write(*,'(a)')line                                 ! line does not need wrapped
            endif
         elseif(ilen.gt.ilength)then                              ! found long line to show
            write(*,'(i0,":",i0,":",a)')ilines,ilen,line
         endif
      enddo STDIN
   case default                                                   ! a list of filenames is present
      FILES: do i=1,isize                                         ! step through files
         ilines=0                                                 ! number of lines successfully read from this file
         open(unit=10,file=filenames(i),status='old',&
         & action='read',access='sequential',&
         & iostat=ios,iomsg=msg)
         if(ios.ne.0)then                                         ! if file could not be open proceed to next file
            write(ERROR_UNIT,'(a)')trim(msg)
            cycle FILES
         endif

         FILE: do while (read_line(line,lun=10)==0)
            ilines=ilines+1
            ilen=len_trim(line)
            if(wrap)then
               if(ilen.gt.ilength)then
                  write(*,fmt)switch(line)
               else
                  write(*,'(a)')line
               endif
            elseif(ilen.gt.ilength)then
               write(*,'(a,":",i0,":",i0,":",a)')&
               & trim(filenames(i)),ilines,ilen,line
            endif
         enddo FILE

         close(unit=10,iostat=ios)
      enddo FILES
   end select SELECT_DATA

end program findll
! status="scratch|new|old|unknown|replace"
! form="unformatted|formatted"
! ACTION='read|write'
! ACCESS='stream|sequential|direct',recl=READLENGTH,position=FILE_POSITION)
