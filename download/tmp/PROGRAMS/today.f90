!>
!!##NAME
!!        today(1f) - [TIME] output current time for uses such as file suffixes.
!!##SYNOPSIS
!!
!!        today [format]
!!##DESCRIPTION
!!        Outputs the current date using the specified format. Typically used
!!        to generate a string to be used in building filenames containing
!!        date information.
!!##OPTIONS
!!        format   any allowable format for the fmtdate(3) routine. Enter
!!                 "-" to get a list on stdout. defaults to "YMD".
!!##EXAMPLE
!!
!!        Sample commands:
!!
!!         cp myfile myfile.`today`
!!         find . -ls > MANIFEST.`today epoch`
!!         mkdir `today YMDhms`
!!         today -                              # show formatting options
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        today(1f)>',&
'@(#)DESCRIPTION:    output current time for uses such as file suffixes.>',&
'@(#)VERSION:        1.0, 2009>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Sat, Oct 21st, 2017 8:46:14 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program today
use M_time, only : now, fmtdate_usage
implicit none

character(len=*),parameter::ident="@(#)today(1f): output current time for uses such as file suffixes."

character(len=:),allocatable :: arguments
character(len=:),allocatable :: options
integer                      :: arguments_length
integer                      :: i
   call get_command(length=arguments_length)              ! get command line length
   allocate(character(len=arguments_length) :: arguments) ! allocate string big enough to hold command line
   call get_command(command=arguments)                    ! get command line as a string
   arguments=adjustl(arguments)                           ! JIC:: trim leading spaces just in case

   i=index(arguments,' ')                                 ! remove command verb from command line assuming verb name exists
   if(i.eq.0)then                                         ! if options are blank set a default
      options='YMD'
   else
      options=arguments(i+1:)
   endif

   if(options.eq.'-')then                                 ! special option to list date format documentation
      call fmtdate_usage()                                ! see all formatting options
   else
      write(*,'(a)')now(options)                          ! display current date using format from command line
   endif

   deallocate(arguments)                                  ! JIC:: releasing string

end program today
