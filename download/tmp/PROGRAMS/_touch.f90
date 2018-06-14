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
'       _touch(1f) - [FUNIX] change file access timestamps or create null file   ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       touch FILE... [--help|--version|--verbose]                               ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'       Make sure specified filenames exist (by creating them as empty           ',&
'       files) and change file access time to current time.                      ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _touch(1f) - [FUNIX] change file access timestamps or create null file
!!
!!##SYNOPSIS
!!
!!        touch FILE... [--help|--version|--verbose]
!!
!!##DESCRIPTION
!!
!!        Make sure specified filenames exist (by creating them as empty
!!        files) and change file access time to current time.
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
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        _touch(1)>',&
'@(#)DESCRIPTION:    change file access timestamp to current time, creating file is necessary>',&
'@(#)VERSION:        1.0, 20180217>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Mon, Jun 4th, 2018 8:59:13 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program touch
use M_kracken, only : kracken,lget,sgets
implicit none
character(len=4096),allocatable :: filenames(:)
integer                         :: i
integer                         :: ios
integer                         :: lun
logical                         :: verbose
character(len=4096)             :: errmsg
logical                         :: ex,od
character(len=1)                :: char

! define command arguments, default values and crack command line
call kracken('touch','-version .f. -help .f. -verbose .f.')
call help_usage(lget('touch_help'))                ! if -help option is present, display help text and exit
call help_version(lget('touch_version'))           ! if -version option is present, display version text and exit
verbose=lget('touch_verbose')
filenames=sgets('touch_oo')

do i=1,size(filenames)
   ! ex, od always become defined unless an error condition occurs.
   inquire(file=filenames(i), exist=ex, opened=od, iostat=ios)
   open(file=filenames(i),newunit=lun,iostat=ios,iomsg=errmsg)
   if(ios.ne.0)then
      write(*,*)'*touch* ERROR on '//trim(filenames(i))//':'//trim(errmsg)
   elseif(verbose)then
      if(.not.ex)then
         write(*,'(a)')trim(filenames(i))//' created'
      else
         read(lun,'(a)',iostat=ios,iomsg=errmsg)char ! make access attempt or date not updating
         write(*,*)'CHAR=',char
         if(ios.ne.0)then
            write(*,*)'*touch* ERROR on '//trim(filenames(i))//':'//trim(errmsg)
         else
            write(*,'(a)')trim(filenames(i))//' updated'
         endif
      endif
   else
      read(lun,'(a)',iostat=ios,iomsg=errmsg)char ! make access attempt or date not updating
      write(*,*)'CHAR=',char
   endif
   close(unit=lun,iostat=ios)
enddo
end program touch
