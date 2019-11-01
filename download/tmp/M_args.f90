module M_args
use M_journal, only : journal
use M_list,          only : insert, locate, replace, remove
use iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>OUTPUT_UNIT    ! access computing environment
use M_strings,       only : isupper, lower, quote, upper
private
!===================================================================================================================================
public  :: get_command_arguments_stack
public  :: get_command_arguments_string
public  :: longest_command_argument
public  :: get_namelist
public  :: print_dictionary
public debug
public unnamed

public :: get_command_arguments_as_raw_namelist

private :: namelist_to_dictionary
private :: prototype_and_cmd_args_to_nlist
private :: prototype_to_dictionary
private :: update
private :: get
private :: wipe_dictionary

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
end type option
!===================================================================================================================================
character(len=:),allocatable   :: keywords(:)
character(len=:),allocatable   :: values(:)
integer,allocatable            :: counts(:)
logical,allocatable            :: present_in(:)

logical                        :: keyword_single=.true.
character(len=:),allocatable   :: passed_in
character(len=:),allocatable   :: namelist_name

character(len=:),allocatable   :: unnamed(:)
logical                        :: debug=.false.
logical                        :: return_all

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function get_command_arguments_stack() result(args)
character(len=:),allocatable :: args(:)
integer :: ilength, ilongest, iargs, istatus, i
ilength=0
ilongest=1 ! get an error if try to get string of zero length in gfortran 7.0.4 so set to 1 instead of 0
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
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine get_command_arguments_string(string,istatus)

character(len=*),parameter::ident_1="&
&@(#)M_args::get_command_arguments_string(3f): return all command arguments as an allocated string"

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
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function get_namelist(definition,all) result (readme)

character(len=*),parameter::ident_2="@(#)M_args::get_namelist(3f): return all command arguments as a NAMELIST(3f) string to read"

character(len=*),intent(in),optional :: definition
logical,intent(in),optional          :: all
character(len=:),allocatable         :: hold               ! stores command line argument
character(len=:),allocatable         :: readme             ! stores command line argument
integer                              :: ibig

   if(allocated(unnamed))then
       deallocate(unnamed)
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) :: unnamed(0))
   if(present(all))then
      return_all=all
   else
      return_all=.false.
   endif
   if(present(definition))then
      if(definition.eq.'')then
         readme=get_command_arguments_as_raw_namelist()
      else
         call wipe_dictionary()
         hold=adjustl(definition)
         if(hold(1:1).eq.'&')then                          ! definition is assumed to be a NAMELIST string
            call namelist_to_dictionary(hold)
            present_in=.false.
            call prototype_and_cmd_args_to_nlist(' ',readme)
         else                                              ! definition is assumed to be a prototype of the command
            call prototype_and_cmd_args_to_nlist(hold,readme)
         endif
      endif
   else                                                    ! assume should read command line as a raw string in NAMELIST format
      readme=get_command_arguments_as_raw_namelist()
   endif

   if(.not.allocated(unnamed))then
       allocate(character(len=0) :: unnamed(0))
   endif

end function get_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function get_command_arguments_as_raw_namelist() result (string)

character(len=*),parameter::ident_3="&
&@(#)M_args::get_command_arguments_as_raw_namelist(3f): return all command arguments as a NAMELIST(3f) string"

character(len=:),allocatable :: string                     ! stores command line argument
character(len=:),allocatable :: string_bug                 ! bug in gfortran 7.4.0 where string in LHS and RHS causes problems
integer :: command_line_length
   call get_command(length=command_line_length)            ! get length needed to hold command
   allocate(character(len=command_line_length) :: string)
   call get_command(string)
   ! trim off command name and get command line arguments
   string_bug=adjustl(string)//' '                         ! assuming command verb does not have spaces in it
   string=string_bug(index(string_bug,' '):)
   string="&ARGS "//string//" /"                            ! add namelist prefix and terminator
   end function get_command_arguments_as_raw_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine prototype_to_dictionary(string)
implicit none

character(len=*),parameter::ident_4="@(#)M_args::prototype_to_dictionary(3f): parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy   ! working copy of string
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
character(len=3)                  :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! character to left of CURRNT
character(len=1)                  :: forwrd  ! character to right of CURRNT
integer,dimension(2)              :: ipnt
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: itype
integer                           :: ifwd
integer                           :: ibegin
integer                           :: iend

   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=string//'  '

   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in parameter name
   ipnt(1)=1           ! pointer to position in parameter value
   itype=1             ! itype=1 for value, itype=2 for variable

   delmt="off"
   prev=" "

   keyword_single=.true.
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)

      if((currnt=="-".and.prev==" ".and.delmt == "off".and.index("0123456789.",forwrd) == 0).or.ipoint > islen)then
         ! beginning of a parameter name
         if(forwrd.eq.'-')then                      ! change --var to -var so "long" syntax is supported
            dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead
            keyword_single=.false.
         else
            keyword_single=.true.
         endif
         if(ipnt(1)-1 >= 1)then
            ibegin=1
            iend=len_trim(value(:ipnt(1)-1))
            do
               if(iend  ==  0)then                  ! len_trim returned 0, parameter value is blank
                  iend=ibegin
                  exit
               elseif(value(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit
               endif
            enddo
            if(keyword.ne.' ')then
               call update(keyword,value)         ! store name and its value
            else
               write(stderr,*)'*prototype_to_dictionary* warning: ignoring blank keyword ',trim(value)
            endif
         else
            if(keyword.ne.' ')then
               call update(keyword,'F')           ! store name and null value
            else
               if(debug)then
                  write(stderr,*)'*prototype_to_dictionary* warning: blank keyword, and ignoring blank value',trim(value)
               endif
            endif
         endif
         itype=2                               ! change to filling a variable name
         value=""                              ! clear value for this variable
         keyword=""                            ! clear variable name
         ipnt(1)=1                             ! restart variable value
         ipnt(2)=1                             ! restart variable name

      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  2)then
            ! switch from building a keyword string to building a value string
            itype=1
            ! beginning of a delimited parameter value
         elseif(currnt  ==  """".and.itype  ==  1)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype.eq.1)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
               delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
               delmt="off"
            else
               delmt="on"
            endif
            if(prev /= """")then  ! leave quotes where found them
               if(itype.eq.1)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current parameter name or parameter value
            if(itype.eq.1)then
               value=value//currnt
            else
               keyword=keyword//currnt
            endif
            ipnt(itype)=ipnt(itype)+1
         endif

      endif

      prev=currnt
      if(ipoint <= islen)then
         cycle
      endif
      exit
   enddo

end subroutine prototype_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place
integer                               :: ilen
character(len=:),allocatable          :: val_local
   if(debug)then
      if(present(val))then
         write(stderr,*)'*update* DEBUG: KEY=',key,' VAL=',val
      else
         write(stderr,*)'*update* DEBUG: KEY=',key
      endif
   endif
   if(present(val))then
      val_local=val
      ilen=len_trim(val_local)
      call locate(keywords,key,place)                   ! find where string is or should be
      if(place.lt.1)then                                ! if string was not found insert it
         call insert(keywords,key,iabs(place))
         call insert(values,val_local,iabs(place))
         call insert(counts,ilen,iabs(place))
         call insert(present_in,.true.,iabs(place))
      else
         call replace(values,val_local,place)
         call replace(counts,ilen,place)
         call replace(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate(keywords,key,place)
      if(place.gt.0)then
         call remove(keywords,place)
         call remove(values,place)
         call remove(counts,place)
         call remove(present_in,place)
      endif
   endif
   if(debug)then
      if(present(val))then
         write(stderr,*)'*update* DEBUG: KEY=',key,'PLACE=',place,' VAL=',val, &
                &size(keywords),size(values),size(counts),size(present_in)
      else
         write(stderr,*)'*update* DEBUG: KEY=',key,'PLACE=',place,size(keywords),size(values),size(counts),size(present_in)
      endif
      write(stderr,*)present_in
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
   if(allocated(present_in))deallocate(present_in)
   allocate(present_in(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine prototype_and_cmd_args_to_nlist(prototype,nml)
implicit none

character(len=*),parameter::ident_5="&
&@(#)M_args::prototype_and_cmd_args_to_nlist: create dictionary from prototype (if not null) and update from command line arguments"

character(len=*)             :: prototype
character(len=:),allocatable :: nml
integer                      :: i
integer                      :: ibig
   if(debug)then
      write(stderr,*)'*prototype_and_cmd_args_to_nlist* DEBUG: prototype=',trim(prototype)
   endif

   passed_in=prototype ! make global copy for printing

   if(allocated(unnamed))deallocate(unnamed)
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) ::unnamed(0))

   if(prototype.ne.'')then
      call prototype_to_dictionary(prototype)  ! build dictionary from prototype
      namelist_name='&ARGS'
      present_in=.false.  ! reset all values to false
   endif

   if(debug)then                            ! look at some of the values as strings or numbers
      call print_dictionary('DICTIONARY FROM PROTOTYPE')
   endif

   call cmd_args_to_dictionary(check=.true.)

   call dictionary_to_namelist(nml)

   ! show array
   if(debug)then
      call print_dictionary('DICTIONARY FROM COMMAND LINE:')
   endif

end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary(check)
! convert command line arguments to dictionary entries
! reading the namelist output will trap unknown option names so do not really need to trap them here
logical,intent(in),optional  :: check
logical                      :: check_local
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i
integer                      :: ilength, istatus, imax
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
   if(present(check))then
      check_local=check
   else
      check_local=.false.
   endif
   nomore=.false.
   pointer=0
   lastkeyword=' '
   keyword_single=.true.
   GET_ARGS: do i=1, command_argument_count()                                                        ! insert and replace entries
      call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
      if(istatus /= 0) then                                                                          ! stop program on error
         write(stderr,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength
         exit GET_ARGS
      else
         if(allocated(current_argument))deallocate(current_argument)
         ilength=max(ilength,1)
         allocate(character(len=ilength) :: current_argument)
         call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
         if(istatus /= 0) then                                                                       ! stop program on error
            write(stderr,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
               &'status=',istatus,&
               &'length=',ilength,&
               &'target length=',len(current_argument)
            exit GET_ARGS
          endif
      endif

      if(current_argument.eq.'-')then  ! sort of
         current_argument='"stdin"'
      endif
      if(current_argument.eq.'--')then ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         cycle
      endif
      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '
      if(.not.nomore.and.current_argument_padded(1:2).eq.'--'.and.index("0123456789.",dummy(3:3)).eq.0)then ! beginning of long word
         keyword_single=.false.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(keywords,current_argument_padded(3:),pointer)
         if(pointer.le.0.and.check_local)then
            call print_dictionary('UNKNOWN LONG KEYWORD: '//current_argument)
            stop 1
         endif
         lastkeyword=trim(current_argument_padded(3:))
      elseif(.not.nomore.and.current_argument_padded(1:1).eq.'-'.and.index("0123456789.",dummy(2:2)).eq.0)then  ! short word
         keyword_single=.true.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(keywords,current_argument_padded(2:),pointer)
         if(pointer.le.0.and.check_local)then
            call print_dictionary('UNKNOWN SHORT KEYWORD: '//current_argument)
            stop 2
         endif
         lastkeyword=trim(current_argument_padded(2:))
      elseif(pointer.eq.0)then                                                                           ! unnamed arguments
         imax=max(len(unnamed),len(current_argument))
         unnamed=[character(len=imax) :: unnamed,current_argument]
      else
         if(debug)then
            write(stderr,*)'POINTER=',pointer,' KEYWORD=',keywords(pointer),' VALUE=',current_argument,' LENGTH=',ilength
         endif
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1).eq.'"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then  ! assume boolean parameter
            if(current_argument.ne.' ')then
               imax=max(len(unnamed),len(current_argument))
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
            current_argument='T'
         endif
         call update(keywords(pointer),current_argument)
         pointer=0
         lastkeyword=''
      endif
   enddo GET_ARGS
   if(lastkeyword.ne.'')then
      call ifnull()
   endif


contains
subroutine ifnull()
   oldvalue=get(lastkeyword)//' '
   if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then
      call update(lastkeyword,'T')
   elseif(oldvalue(1:1).eq.'"')then
      call update(lastkeyword,'" "')
   else
      call update(lastkeyword,' ')
   endif
end subroutine ifnull

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dictionary_to_namelist(nml)
character(len=:),allocatable,intent(out) :: nml
integer :: i
character(len=:),allocatable :: newkeyword
   ! build namelist string
   nml=namelist_name//' '
   do i=1,size(keywords)
      if(isupper(keywords(i)(1:1)))then
         newkeyword=trim(lower(keywords(i)))//'_'
      else
         newkeyword=trim(keywords(i))
      endif
      if(return_all.or.present_in(i))then
         nml=nml//newkeyword//'='//trim(values(i))//' '
      endif
   enddo
   nml=nml//' /'
   if(debug)then
      write(stderr,'(a)')'NAMELIST:'
      write(stderr,'(a)')nml
      if(size(unnamed).gt.0)then
         write(stderr,'(a)')'UNNAMED'
         write(stderr,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
end subroutine dictionary_to_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine print_dictionary(header)
character(len=*),intent(in),optional :: header
integer          :: i
   if(present(header))then
      if(header.ne.'')then
         write(stderr,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords).gt.0)then
         write(stderr,'(*(a,t21,a,t30,a))')'KEYWORD','PRESENT','VALUE'
         write(stderr,'(*(a,t21,l0,t30,"[",a,"]",/))')(trim(keywords(i)),present_in(i),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(stderr,'(a)')'UNNAMED'
         write(stderr,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function longest_command_argument() result(ilongest)
integer :: i
integer :: ilength
integer :: istatus
integer :: ilongest
   ilength=0
   ilongest=0
   GET_LONGEST: do i=1,command_argument_count()                             ! loop throught command line arguments to find longest
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! stop program on error
         write(stderr,*)'*prototype_and_cmd_args_to_nlist* error obtaining length for argument ',i
         exit GET_LONGEST
      elseif(ilength.gt.0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine namelist_to_dictionary(string)
implicit none

character(len=*),parameter::ident_6="@(#)M_args::namelist_to_dictionary(3f): parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy       ! working copy of string
character(len=:),allocatable      :: dummy_bug   ! bug in gfortran 7.4.0 where if dummy is on LHS and used in RHS get wrong result
character(len=:),allocatable      :: keyword_value
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
logical                           :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! current character being processed
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: istart
integer                           :: iend
integer                           :: ileft
integer                           :: icut
integer                           :: i
integer                           :: iback1,iback2
   if(debug)then
      write(stderr,*)'*namelist_to_dictionary* INPUT=',trim(string)
   endif
   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   islen=islen-1                                        ! by definition last character in NAMELIST output is /
   dummy=trim(adjustl(string(:islen)))
   ! strip off namelist group name
   ileft=index(dummy,'&')
   dummy_bug=adjustl(dummy(ileft+1:))
   ileft=index(dummy_bug,' ')
   if(ileft.eq.0)then
      ileft=len(dummy_bug)
   endif
   namelist_name=upper('&'//dummy_bug(:ileft-1))
   dummy=adjustl(dummy_bug(ileft:))

   islen=len(dummy)
   dummy=dummy//'    '
   if(debug)then
      write(stderr,*)'*namelist_to_dictionary* NAMELIST_NAME=['//namelist_name//']'
      write(stderr,*)'*namelist_to_dictionary* DUMMY=['//dummy//']'
   endif


   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   delmt=.false.       ! whether in a character string or not
   prev=" "
   istart=1
   do ipoint=1,islen
      currnt=dummy(ipoint:ipoint)             ! store current character into currnt
      if(currnt=="=".and..not.delmt)then ! end of a parameter name
         keyword_value=''
         iend=0
         do i=ipoint-1,1,-1
            if(dummy(i:i).eq.' ')cycle
            ! found non-space
            iback1=index(dummy(:i),' ',back=.true.)
            iback2=index(dummy(:i),',',back=.true.)
            iend=max(iback1,iback2)
            exit
         enddo
         if(iend.ne.0)then
            call splitit()
         endif
         istart=iend+1
      elseif(currnt  ==  """")then
         if(prev  ==  """")then               ! second of a double quote, put quote in
            delmt=.not.delmt
         elseif(delmt)then
            delmt=.false.
         else
            delmt=.true.
         endif
      endif
      prev=currnt
      if(ipoint.ge.islen)then
         iend=ipoint
         call splitit()
      endif
   enddo
   if(debug)then
      call print_dictionary('NAMELIST TO DICTIONARY')
   endif
contains

subroutine splitit()
integer :: ilast
keyword_value=dummy(istart:iend)
! split keyword_value on first = and convert values to lowercase except for LETTER_ convert to uppercase LETTER and
! remove trailing , as NAMELIST output being read should not contain null values as everything in a namelist needs
! to be allocated (at least in this version of Fortran?).
   icut=index(keyword_value,'=')
   if(icut.eq.0)then
      write(stderr,*)'*splitit* INTERNAL ERROR: KEYWORD_VALUE=['//keyword_value//']'
   else
      if(debug)then
         write(stderr,*)'*splitit* KEYWORD_VALUE=['//keyword_value//']',icut
      endif
      keyword=adjustl(trim(lower(keyword_value(:icut-1))))
      if(len(keyword).eq.2)then
         if(keyword(2:2).eq.'_')then
            keyword=upper(keyword(1:1))
         endif
      endif
      if(icut.eq.len(keyword_value))then
         value=''
      else
         value=trim(adjustl(keyword_value(icut+1:)))
         ilast=len(value)
         if(ilast.eq.0)then
            value=''
         else
            if(value(ilast:ilast).eq.',')then
               value=trim(value(:ilast-1))
            endif
         endif
      endif
      call update(keyword,value)
   endif
end subroutine splitit

end subroutine namelist_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_suite_M_args()
implicit none
!! setup
   call test___copy_m_args_Option()
   call test___final_m_args_Option()
   call test_get_command_arguments_as_raw_namelist()
   call test_get_command_arguments_stack()
   call test_get_command_arguments_string()
   call test_get_namelist()
   call test_longest_command_argument()
   call test_print_dictionary()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___copy_m_args_Option()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('__copy_m_args_Option',msg='')
   !!call unit_check('__copy_m_args_Option', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('__copy_m_args_Option',msg='')
end subroutine test___copy_m_args_Option
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test___final_m_args_Option()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('__final_m_args_Option',msg='')
   !!call unit_check('__final_m_args_Option', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('__final_m_args_Option',msg='')
end subroutine test___final_m_args_Option
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_arguments_as_raw_namelist()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('get_command_arguments_as_raw_namelist',msg='')
   !!call unit_check('get_command_arguments_as_raw_namelist', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('get_command_arguments_as_raw_namelist',msg='')
end subroutine test_get_command_arguments_as_raw_namelist
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_arguments_stack()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('get_command_arguments_stack',msg='')
   !!call unit_check('get_command_arguments_stack', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('get_command_arguments_stack',msg='')
end subroutine test_get_command_arguments_stack
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_arguments_string()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('get_command_arguments_string',msg='')
   !!call unit_check('get_command_arguments_string', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('get_command_arguments_string',msg='')
end subroutine test_get_command_arguments_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_namelist()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('get_namelist',msg='')
   !!call unit_check('get_namelist', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('get_namelist',msg='')
end subroutine test_get_namelist
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_longest_command_argument()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('longest_command_argument',msg='')
   !!call unit_check('longest_command_argument', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('longest_command_argument',msg='')
end subroutine test_longest_command_argument
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_dictionary()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('print_dictionary',msg='')
   !!call unit_check('print_dictionary', 0.eq.0, msg=msg('checking',100))
   call unit_check_done('print_dictionary',msg='')
end subroutine test_print_dictionary
!===================================================================================================================================
end subroutine test_suite_M_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
