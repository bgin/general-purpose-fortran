!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_journal
use iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
implicit none
private

!>
!!##NAME
!!      journal(3f) - [M_journal] provides public message routine, no paging or graphic mode chang
!!##SYNOPSIS
!!
!!
!!    subroutine journal([where,]message,[VALUE(s)])
!!
!!     character(len=*),intent(in) :: where
!!     character(len=*),intent(in) :: msg
!!     character(len=*)|real|integer|doubleprecision|complex :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!
!!   WRITE MESSAGES
!!
!!       call journal(where,message,[VALUE])
!!
!!    or a shortcut for "call journal('sc',message)"
!!
!!       call journal(message)
!!
!!   OPEN OR CLOSE TRAIL FILE
!!
!!    open a trail file, or close trail if filename is blank
!!
!!       call journal('O',[trailfile_name|''])
!!
!!   SET OUTPUT TIME PREFIX
!!
!!    set the NOW(3f) function display format for timestamps
!!
!!       call journal('%',time_stamp_prefix_specification)
!!
!!   MODES
!!
!!    Turn on/off writing DEBUG messages to trail file
!!
!!       call journal([.true.|.false.],'debug')
!!
!!   ASSIGN STDOUT TO AN ALTERNATE FILE
!!    change stdout to iunit and open filename; or close unit and go back to stdout if filename=''
!!
!!       call journal(iunit,filename)
!!
!!    change stdout to iunit to use a file already open
!!
!!       call journal(iunit)
!!
!!##DESCRIPTION
!!
!!    If a user procedure is used for outputting messages instead of calling
!!    WRITE(3f) it is easy to provide control of when messages are printed
!!    (ie. a "verbose" mode, or "quite" mode), creating files to replace
!!    program execution, duplicating output, ...
!!
!!##OPTIONS
!!   WHERE  indicates where messages are written. A combination of the
!!          following characters can be used...
!!
!!      Usually one of these to write to the standard output files ...
!!
!!      S   write to stdout or iounit set with journal(unit) or
!!          journal(unit,filename).
!!      E   write to stderr
!!
!!      And one of these to write to trail file (ignore if no trail file
!!      defined) ...
!!
!!      C   write to trail file as a comment (if file is open)
!!          Writing output "as a comment" means it is preceded by a pound(#)
!!          character.
!!      T   write to trail file (if file is open)
!!
!!      Usually used by itself
!!
!!      D   write to trail file as a comment with DEBUG: prefix in front
!!          of message (if file is open) if debug mode is on
!!
!!      Modifier for S|E|C|T|D specifiers
!!
!!      +   subsequent files are written to with advance='no'. Position is
!!          important. '+sc' does an advance='no' on both files, 's+c'
!!          only does the advance='no' for the trail file.
!!
!!      Mode changing options used by themselves:
!!
!!      >   turn off debug messages
!!      <   turn on debug messages
!!      O   open trail file using value of "message" parameter or close
!!          trail file if no filename or a blank filename.
!!      A   Auxiliary programs that also want to write to the current log file
!!          (a2b, z2a, ...) call this routine to see if there is a trail file
!!          being generated and then add to it so that a program like ush(1f)
!!          can call the auxiliary programs and still just generate one log file,
!!          but if the auxiliary program is used as a stand-alone program no trail
!!          is generated.
!!      %   set prefix to run thru now(3f) to generate time prefix strings
!!
!!   MESSAGE   message to write to stdout, stderr, and the trail file.
!!   FILENAME  when WHERE="O" to turn the trail file on or off, the "message"
!!             field becomes the trail filename to open. If blank, writing
!!             to the trail file is turned off.
!!   TFORMAT   when WHERE="%" the message is treated as a time format
!!             specification as described under now(3f).
!!   VALUE     a numeric or character value to optionally be appended to the message.
!!             Up to nine values are allowed. The WHERE field is required if values are added.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_journal
!!    use M_journal, only : journal
!!
!!    !! BASIC USAGE
!!    call journal('write to standard output as-is, and trail file as a comment')
!!
!!    ! since we have not opened a trail file yet, only stdout will display output
!!    call journal('sc','write to standard output as-is, and trail file as a comment')
!!    call journal('c','ignored, as trail file is not open')
!!
!!    ! now open trail file "trail"
!!    call journal('o','trail')
!!    call journal('sc','same thing, with ')
!!    ! only write to trail file if open
!!    call journal('c','not ignored, as trail file is open. Written with # suffix')
!!    call journal('t','not ignored, as trail file is open. Written as-is')
!!    ! turn off trail file
!!    call journal('o')
!!
!!    end program demo_journal
!===================================================================================================================================
public journal

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,c,[g1-g9]) ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
   module procedure change_model              ! journal(i,c)             ! first is not a string then string
end interface journal

public test_suite_M_journal

character(len=*),parameter::ident_1="&
&@(#)M_journal::journal(3fg): provides public message routine, no paging or graphic mode change"

! global variables

!integer,save,private       :: stdin=INPUT_UNIT
integer,save,private       :: stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

character(len=*),parameter::ident_2="@(#)M_journal::where_write_message(3fp): basic message routine used for journal files"

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
!
!  writes error messages and general information text to stdout and the trace file
!     where=*C* write to trail file as a comment (if file is open)
!     where=*D* write to trail file as a comment with DEBUG: prefix in front of message (if file is open and debug mode on)
!     where=*E* write to stderr
!     where=*S* write to stdout or iounit set with journal(unit) or journal(unit,filename)
!     where=*T* write to trail file (if file is open)
!     where=*+* subsequent writes for this call are written with advance='no'

!     where=> turn off debug messages (change mode), which are ones with WHERE='D'
!     where=< turn on debug messages  (change mode), which are ones with WHERE='D'

!     where=O open trail file "msg" or close trail file if blank filename is given
!     where=% set prefix to run thru now(3f) to generate time prefix strings, blank turns off time prefix
!     where=N open new file and assign stdout to the file unless file name is blank; then revert to stdout being original stdout.
!
!  the trail file messages are preceded by a pound character (#) by default so they can easily be interpreted as comments
!  if the trace file is subsequently used as input data for a program
!
   logical,save                       :: trailopen=.false.
   integer,save                       :: itrail
   character,save                     :: comment='#'
   integer                            :: i
   integer                            :: ios
   integer                            :: times             ! number of times written to stdout
   character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

   character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
   character(len=:),allocatable       :: prefix            ! the prefix string to add to output
   logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
   character(len=4096)                :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
   interface
      function now_ex(format)
         character(len=*),intent(in),optional :: format
         character(len=:),allocatable         :: now_ex
      end function now_ex
   end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(prefix_it)then
      prefix=now_ex(prefix_template)
   else
      prefix=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         !!elseif(times.eq.0)then
         !!   write(stdout,'(a)',advance=adv)prefix//trim(msg)
         !!   times=times+1
         endif
      case('S','s')
         write(stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg.eq.'')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for stdout
         if(msg.ne.' '.and.msg.ne.'#N#'.and.msg.ne.'"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=trim(msg),iostat=ios)
            if(ios.eq.0)then
               stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg.eq.' ')then
            close(unit=last_int,iostat=ios)
            stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times.eq.0)then
            !! write(stdout,'(2a)',advance=adv)prefix,trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(3a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times.eq.0)then
               write(stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios.ne.0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=trim(msg),form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=trim(msg),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()

character(len=*),parameter::ident_3="@(#)M_journal::flush_trail(3fp): flush trail file"

call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)

character(len=*),parameter::ident_4="@(#)M_journal::set_stdout_lun(3fp): change I/O logical unit value for standard writes"

integer,intent(in)                   :: iounit
   stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine change_model(value,mode)

character(len=*),parameter::ident_5="@(#)M_journal::change_model(3fp): change integer journal(3f) modes"

logical,intent(in)          :: value
character(len=*),intent(in) :: mode

select case(mode)
case('debug','DEBUG')
   debug=value
case default
   call where_write_message('sc','*journal* unknown logical mode '//trim(mode))
end select

end subroutine change_model
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    where_write_message_all(3f) - [M_journal] converts any standard scalar type to a string
!!##SYNOPSIS
!!
!!   subroutine  where_write_message_all(where,message,g1,g2g3,g4,g5,g6,g7,g8,g9,nospace)
!!
!!     character(len=*),intent(in)   :: where
!!     character(len=*),intent(in)   :: message
!!     class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     logical,intent(in),optional   :: nospace
!!
!!##DESCRIPTION
!!    where_write_message_all(3f) builds and writes a space-separated string from up to nine scalar values
!!
!!##OPTIONS
!!
!!    where    string designating where to write message
!!    message  initial message string
!!    g[1-9]   optional values to print the value of after the message. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!    nospace  if nospace=.true., then no spaces are added between values
!!##RETURNS
!!    where_write_message_all   description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wm_all
!!    use M_journal, only : where_write_message_all
!!    implicit none
!!    end program program demo_wm_all
!===================================================================================================================================
subroutine where_write_message_all(where,message, &
                & generic1, generic2, generic3,   &
                & generic4, generic5, generic6,   &
                & generic7, generic8, generic9,   &
                & nospace)
implicit none

character(len=*),parameter::ident_6="&
&@(#)M_debug::where_write_message_all(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
character(len=*),intent(in)   :: message
class(*),intent(in),optional  :: generic1, generic2, generic3
class(*),intent(in),optional  :: generic4, generic5, generic6
class(*),intent(in),optional  :: generic7, generic8 ,generic9
logical,intent(in),optional   :: nospace
   character(len=4096)        :: line
   integer                    :: istart
   integer                    :: increment
   if(present(nospace))then
      if(nospace)then
         increment=1
      else
         increment=2
      endif
   else
      increment=2
   endif

   istart=1
   line=trim(message)
   istart=len_trim(message)+increment
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   call where_write_message(where,trim(line))
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      !type is (real(kind=real256));     write(line(istart:),'(1pg0)') generic
      !type is (real);                   write(line(istart:),'(1pg0)') generic
      !type is (doubleprecision);        write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(1l)') generic
      type is (character(len=*));       write(line(istart:),'(a)') generic
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
end subroutine print_generic
!===================================================================================================================================
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

character(len=*),parameter::ident_7="@(#)M_journal::write_message_only(3fp): calls JOURNAL('sc',message)"

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_journal()
implicit none
!! setup
   call test_change_model()
   call test_flush_trail()
   call test_set_stdout_lun()
   call test_where_write_message_all()
   call test_write_message_only()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_change_model()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('change_model',msg='')
   !!call unit_check('change_model', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('change_model',msg='')
end subroutine test_change_model
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_flush_trail()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('flush_trail',msg='')
   call journal()
   !!call unit_check('flush_trail', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('flush_trail',msg='')
end subroutine test_flush_trail
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_set_stdout_lun()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('set_stdout_lun',msg='')
   !!call unit_check('set_stdout_lun', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('set_stdout_lun',msg='')
end subroutine test_set_stdout_lun
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_where_write_message_all()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('where_write_message_all',msg='')
   !!call unit_check('where_write_message_all', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('where_write_message_all',msg='')
end subroutine test_where_write_message_all
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_write_message_only()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
implicit none
   call unit_check_start('write_message_only',msg='')
   !!call unit_check('write_message_only', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('write_message_only',msg='')
end subroutine test_write_message_only
!===================================================================================================================================
end subroutine test_suite_M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
