module M_args
use M_journal, only : journal
private

public :: get_command_arguments_stack
public :: get_command_arguments_string
public :: get_namelist

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   logical                  :: boolean
end type option

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_command_arguments_stack(3f) - [M_args] return a character array containing all the command line argument
!!##SYNOPSIS
!!
!!    function get_command_arguments(stack) result (args)
!!
!!     character(len=:),allocatable :: args(:)
!!
!!##DESCRIPTION
!!    Return a character array containing all the command arguments.
!!    For cases where it is difficult to process the command arguments
!!    one at a time, this function returns an array of the command line
!!    arguments
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_get_command_arguments_stack
!!    use M_args,    only : get_command_arguments_stack
!!    implicit none
!!    character(len=:),allocatable :: myargs(:)
!!    integer                      :: i
!!    myargs=get_command_arguments_stack()
!!    write(*,'(i0,t10,a)')(i,myargs(i),i=1,size(myargs))
!!    write(*,*)'longest argument is ',len(myargs)
!!    write(*,*)'number of arguments is ',size(myargs)
!!    end program demo_get_command_arguments_stack
!===================================================================================================================================
function get_command_arguments_stack() result(args)
character(len=:),allocatable :: args(:)
integer :: ilength, ilongest, iargs, istatus, i
ilength=0
ilongest=0
iargs=command_argument_count()
   GET_LONGEST: do i=1,iargs                                                ! look at all arguments
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! stop program on error
         call journal('sc','*get_command_arguments_stack* error obtaining argument ',i)
         exit GET_LONGEST
      elseif(ilength.gt.0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
   allocate(character(len=ilongest) :: args(iargs))
   args=''
   GET_ARGS: do i=1,command_argument_count()                                             ! copy array of arguments
      call get_command_argument(number=i,value=args(i),length=ilength,status=istatus)    ! get next argument
      if(istatus /= 0) then                                                              ! stop program on error
         call journal('sc','*get_command_arguments_stack* error obtaining argument ',i)
         exit GET_ARGS
      endif
   enddo GET_ARGS
end function get_command_arguments_stack
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          get_command_arguments_string(3f) - [ARGUMENTS:M_args] return all command arguments as an allocated strin
!!
!!##SYNOPSIS
!!
!!   subroutine get_command_arguments_string(string,istatus)
!!
!!    character(len=:),allocatable,intent(out) :: string
!!    integer,intent(out)                      :: istatus
!!##DESCRIPTION
!!
!!##RETURNS
!!    STRING  composed of all command arguments concatenated into a string
!!    ISTATUS status (non-zero means error)
!!
!!##EXAMPLE
!!
!!
!!   Sample usage
!!
!!    program demo_get_command_arguments_string
!!    use M_journal, only : journal
!!    use M_args, only : get_command_arguments_string
!!    implicit none
!!    integer :: ier
!!    character(len=:),allocatable :: cmd
!!    call get_command_arguments_string(cmd,ier)
!!    write(*,*)'CMD=',trim(cmd)
!!    write(*,*)'LEN(CMD)=',len(cmd)
!!    write(*,*)'IER=',ier
!!    end program demo_get_command_arguments_string
!!##SEE ALSO
!!    M_kracken, kracken
!!
!!    dget,dgets,iget,igets,lget,lgets,rget,rgets,sget,sgets,retrev
!!
!!    parse,dissect,store,setprompts,show
!===================================================================================================================================
subroutine get_command_arguments_string(string,istatus)

character(len=*),parameter::ident_1="&
&@(#)M_kracken::get_command_arguments_string(3f): return all command arguments as an allocated string"

!  try to guess original quoting and reintroduce quotes
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable,intent(out) :: string            ! string of all arguments to create
integer,intent(out)                      :: istatus           ! status (non-zero means error)
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: max_string_len             ! allowed length of output string
   integer                      :: i                          ! loop count
   character(len=:),allocatable :: value                      ! store individual arguments one at a time
   integer                      :: ilength                    ! length of individual arguments
   character(len=1024)          :: deallocate_error_message
   integer                      :: deallocate_status
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_command(LENGTH=max_string_len, STATUS=istatus)
   if(istatus > 0)then
     STOP "*get_command_arguments_string* error: could not retrieve command line"
   elseif (max_string_len == 0) then
     STOP "*get_command_arguments_string* error: could not determine command length"
   endif
   max_string_len=max_string_len+2*command_argument_count()   ! leave room for adding double quotes to each argument
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(character(len=max_string_len) :: value)           ! no single argument should be longer than entire command length
   istatus=0                                                  ! initialize returned error code
   string=""                                                  ! initialize returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
   APPEND_ARGS: do i=1,command_argument_count()               ! append any arguments together
      call get_command_argument(i,value,ilength,istatus)      ! get next argument
      if(istatus /= 0) then                                   ! stop program on error
         call journal('sc','*get_command_arguments_string* error obtaining argument ',i)
         exit APPEND_ARGS
      elseif(ilength.gt.0)then
         !---------------------
         ! BEGIN GUESS AT RE-QUOTING STRING
         ! if argument contains a space and does not contain a double-quote and is short enough to have double quotes added
         ! assume this argument was quoted but that the shell stripped the quotes and add double quotes. This is an optional
         ! behavior and assumes an operating system that strips the quotes from quoted strings on the command line. If the
         ! operating system is smarter than that remove this section
         if(index(value(:ilength),' ').ne.0.and.index(value(:ilength),'"').eq.0)then
            if((ilength+2).le.len(value))then
               string=string//' "'//value(:ilength)//'"'
            endif
         ! END GUESS AT RE-QUOTING STRING
         !---------------------
         else
            string=string//' '//value(:ilength) ! append strings together
         endif
      endif
   enddo APPEND_ARGS
!-----------------------------------------------------------------------------------------------------------------------------------
   deallocate(value,stat=deallocate_status,errmsg=deallocate_error_message) ! should be automatically removed in newer compilers
   if(deallocate_status.ne.0)then
      call journal('*get_command_arguments_string *'//trim(deallocate_error_message))
   endif
end subroutine get_command_arguments_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_namelist(3f) - [ARGUMENTS:M_args] NAMELIST-based command line argument parsin
!!
!!##SYNOPSIS
!!
!!   subroutine get_namelist(string)
!!
!!    character(len=:),allocatable,intent(out) :: string
!!##DESCRIPTION
!!
!!    NAMELIST can be used to create a unprecedentedly simple method of
!!    passing keyword-value pairs via the command line. the following example
!!    program simply needs an initialized variable added to the NAMELIST
!!    and it automatically is available as a command line argument. Hard
!!    to imagine it getting much simpler.
!!
!!    You can call the example program with syntax like:
!!
!!       testit r=200e3 i=200
!!       testit K=33333,J=22222,I=11111
!!
!!    Note that if you pass in strings you probably will have to use nested
!!    quotes or escape your quote characters. How you do that can vary with
!!    what shell and OS you are running in.
!!
!!       # just quote the entire argument list with single quotes ...
!!       testit 'c="my character string" S=10,T=20.30,R=3e-2'
!!
!!       or nest the quotes ...
!!       testit c='"string"' S=20.30
!!
!!       or escape the quotes ...
!!       testit c=\"string\"
!!
!!    As you will see, there is no need to convert from strings to numeric
!!    values in the source code. Even arrays and user-defined types can be
!!    used, complex values can be input ... just define the variable and
!!    add it to the NAMELIST definition.
!!
!!    And if you want to use a config file instead of command line arguments
!!    just create a NAMELIST input file and read it.
!!
!!##RETURNS
!!    STRING  composed of all command arguments concatenated into a string
!!            prepared for reading as a NAMELIST.
!!
!!##EXAMPLE
!!
!!   Sample usage
!!
!!    program demo_get_namelist
!!    implicit none
!!    character(len=255)           :: message ! use for I/O error messages
!!    character(len=:),allocatable :: string  ! stores command line argument
!!    integer                      :: ios
!!
!!    ! declare and initialize a namelist
!!    integer    :: i=1, j=2, k=3
!!    real       :: s=111.1, t=222.2, r=333.3
!!    real       :: arr(3)=[10.0,20.0,30.0]
!!    character(len=255) :: c=' '
!!    ! just add a variable here and it is a new parameter !!
!!    namelist /cmd/ i,j,k,s,t,r,c,arr
!!
!!       ! return command line arguments as NAMELIST input
!!       string=get_namelist()
!!       ! internal read of namelist
!!       read(string,nml=cmd,iostat=ios,iomsg=message)
!!       if(ios.ne.0)then
!!          write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!          write(*,*)'OPTIONS:'
!!          write(*,nml=cmd)
!!          stop 1
!!       endif
!!       ! all done cracking the command line
!!
!!       ! use the values in your program. For example ...
!!       sum=i+j+k
!!       write(*,*)'sum=',sum
!!    end program demo_get_namelist
!===================================================================================================================================
function get_namelist() result (string)

character(len=*),parameter::ident_2="@(#)M_kracken::get_namelist(3f): return all command arguments as a NAMELIST(3f) string"

character(len=:),allocatable :: string                     ! stores command line argument
integer :: command_line_length
   call get_command(length=command_line_length)            ! get length needed to hold command
   allocate(character(len=command_line_length) :: string)
   call get_command(string)
   ! trim off command name and get command line arguments
   string=adjustl(string)//' '                             ! assuming command verb does not have spaces in it
   string=string(index(string,' '):)
   string="&cmd "//string//" /"                            ! add namelist prefix and terminator
   end function get_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
