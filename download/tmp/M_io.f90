!===================================================================================================================================
MODULE M_io
implicit none
private
public uniq
public print_inquire
public notopen
public slurp
public swallow
public dirname
public splitpath
public isdir
public get_tmp
public scratch
public read_line
public read_all
public read_table
public rd
public test_suite_M_io

character(len=*),parameter::ident_1="@(#)M_io::read_table(3f): read file containing a table of numeric values"

interface read_table
   module procedure read_table_real, read_table_doubleprecision
end interface

character(len=*),parameter::ident_2="@(#)M_io::rd(3f): ask for string or number from standard input with user-definable prompt"
interface rd
   module procedure rd_character
   module procedure rd_integer
   module procedure rd_real
   module procedure rd_doubleprecision
end interface
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      uniq(3f) - [M_io] append a number to the end of filename to make a unique name if name exists
!!##SYNOPSIS
!!
!!      Usage
!!
!!       character(len=:),allocatable function uniq(name,istart,verbose,create)
!!       character(len=*),intent(in) :: name
!!       integer,intent(in),optional :: istart
!!       logical,intent(in),optional :: verbose
!!       logical,intent(in),optional :: create
!!
!!##DESCRIPTION
!!    Given a filename test if it is in use or exists. If it is, or if it
!!    ends in a period add a four-digit number to the end of the name and
!!    test if the new name exists. If necessary, increment the number and
!!    try again up to the value 9999999. By default an empty file is created
!!    if an unused name is found.
!!
!!    o relatively non-generic;
!!    o does not try to detect io errors
!!
!!##OPTIONS
!!    name     base input name used to create output filename
!!             If name ends in "." a numeric suffix is always added.
!!    istart   number to start with as a suffix. Default is 1. Must be a
!!             positive integer less than 9999999.
!!    verbose  writes extra messages to stdout. Defaults to .false.
!!    create   create file if new name is successfully found. Defaults
!!             to .true. .
!!
!!##RETURNS
!!    uniq     A unique filename that is the same as the NAME input parameter
!!             except with a number appended at the end if needed. If could
!!             not find a unique name a blank is returned.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_uniq
!!       use M_io, only : uniq
!!       implicit none
!!       character(len=4096) :: myname
!!       integer             :: i
!!          myname=uniq('does_not_exist')
!!          open(unit=10,file='does_exist')
!!          write(*,*)'name stays the same ',trim(myname)
!!          myname=uniq('does_exist')
!!          write(*,*)'name has suffix added ',trim(myname)
!!          do i=1,10
!!             myname=uniq('does_exist')
!!             write(*,*) 'FILENAME:',trim(myname)
!!             open(unit=20+i,file=myname)
!!          enddo
!!       end program demo_uniq
!!
!!    Expected output
!!
!!     name stays the same does_not_exist
!!     name has suffix added does_exist0001
!!     FILENAME:does_exist0002
!!     FILENAME:does_exist0003
!!     FILENAME:does_exist0004
!!     FILENAME:does_exist0005
!!     FILENAME:does_exist0006
!!     FILENAME:does_exist0007
!!     FILENAME:does_exist0008
!!     FILENAME:does_exist0009
!!     FILENAME:does_exist0010
!!     FILENAME:does_exist0011
!!
!!##AUTHOR
!!    John S. Urban, 1993
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
function uniq(name,istart,verbose,create)
use M_journal, only : journal
implicit none

character(len=*),parameter::ident_3="&
&@(#)M_io::uniq(3f): append a number to the end of filename to make a unique name if name exists"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: name
character(len=:),allocatable :: uniq
integer,intent(in),optional  :: istart
logical,intent(in),optional  :: verbose
logical,intent(in),optional  :: create
!-----------------------------------------------------------------------------------------------------------------------------------
logical                     :: around
integer,save                :: icount=1           ! counter to generate suffix from
character(len=4096),save    :: lastname=' '       ! name called with last time the routine was called
integer                     :: ilen
integer                     :: itimes
integer                     :: iscr
integer                     :: ios
logical                     :: verbose_local
logical                     :: create_local
!-----------------------------------------------------------------------------------------------------------------------------------
   uniq=trim(name)                                   ! the input name will be returned if it passes all the tests
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                          ! if a different input name than last time called reset icount
      lastname=name                                  ! a new name to keep for subsequent calls
      icount=1                                       ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(create))then
      create_local=create
   else
      create_local=.true.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(istart))then
      icount=istart                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                               ! find last non-blank character in file name
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.ne.0)then                                 ! a blank input name so name will just be a suffix
      if(name(ilen:ilen).ne.'.')then                 ! always append a number to a file ending in .
         inquire(file=name(:ilen),exist=around)      ! check filename as-is
         if(.not.around)then                         ! file name does not exist, can use it as-is
            uniq=trim(name)
            if(create_local)then
               open(newunit=iscr,file=uniq,iostat=ios,status='new')
               close(unit=iscr,iostat=ios)
            endif
            return
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   itimes=0                                           ! count number of times tried to get a uniq name
   deallocate(uniq)
   allocate(character(len=ilen+8) :: uniq)            ! make it useable with an internal WRITE(3f) with room for a numeric suffix
   uniq(:)=name
   INFINITE: do                                       ! top of loop trying for a unique name
      if(itimes.ge.9999999)then                       ! if too many tries to be reasonable give up
         call journal('sc','*uniq* unable to find a unique filename. Too many tries')
         uniq=''
         return
      endif
      if(icount.gt.9999999) icount=1                  ! reset ICOUNT when it hits arbitrary maximum value
      if(icount.le.9999)then
         write(uniq(ilen+1:),'(i4.4)')icount          ! create name by adding a numeric string to end
      else
         write(uniq(ilen+1:),'(i7.7)')icount          ! create name by adding a numeric string to end
      endif
      icount=icount+1                                 ! increment counter used to come up with suffix
      inquire(file=uniq,exist=around)                 ! see if this filename already exists
      if(.not.around)then                             ! found an unused name
         if(verbose_local)then
            call journal('c',trim('*uniq* name='//trim(uniq))) ! write out message reporting name used
         endif
         if(create_local)then
            open(newunit=iscr,file=uniq,iostat=ios,status='new')
            close(unit=iscr,iostat=ios)
         endif
         uniq=trim(uniq)
         return                                       ! return successfully
      endif
      itimes=itimes+1                                 ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_inquire(3f) - [M_io] Do INQUIRE on file by name/number and print results
!!
!!##SYNOPSIS
!!
!!   Definition:
!!
!!    subroutine print_inquire(iunit,name)
!!    integer,intent(in)          :: iunit
!!    character(len=*),intent(in) :: name
!!
!!##DESCRIPTION
!!    Given either a Fortran file-unit-number or filename, call the INQUIRE(3f)
!!    intrinsic and print typical status information.
!!
!!##OPTIONS
!!    iunit  if >=0 then query by number and ignore filename
!!    name   if IUNIT < 0 then query by this filename
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_print_inquire
!!       use M_io, only : print_inquire
!!
!!       call print_inquire(5,'')
!!
!!       call print_inquire(19,'')
!!
!!       open(unit=20)
!!       call print_inquire(20,'')
!!
!!       open(unit=21,status='scratch')
!!       call print_inquire(21,'')
!!
!!       open(unit=22,file='junko')
!!       write(22,*)'WRITE TO JUNKO'
!!       close(unit=22)
!!       call print_inquire(22,'')
!!       call print_inquire(-1,'junko')
!!
!!       end program demo_print_inquire
!!
!!   Expected output:
!!
!!    ====================================================
!!    *print_inquire* checking file: /dev/pty1
!!    *print_inquire* file exists
!!    *print_inquire* using unit number  5
!!    *print_inquire* access type SEQUENTIAL,FORMATTED
!!    ====================================================
!!    *print_inquire* unit number is not open ,unit= 19
!!    ====================================================
!!    *print_inquire* checking file: fort.20
!!    *print_inquire* file exists
!!    *print_inquire* using unit number  20
!!    *print_inquire* access type SEQUENTIAL,FORMATTED
!!    ====================================================
!!    *print_inquire* unit number is not named ,unit= 21
!!    ====================================================
!!    *print_inquire* unit number is not open ,unit= 22
!!    ====================================================
!!    *print_inquire* checking file: junko
!!    *print_inquire* file exists
!!    *print_inquire* file is not open
!===================================================================================================================================
subroutine print_inquire(iunit,name) ! Version: JSU-1997-12-31
use M_journal, only : journal

character(len=*),parameter::ident_4="@(#)M_io::print_inquire(3f): Do INQUIRE on file by name/number and print results"

integer,intent(in)          :: iunit         ! if iunit >= 0 then query by unit number, else by name
character(len=*),intent(in) :: name
      character(len=1024)   :: nm
      integer               :: iend,inum,ircl,inext,ios
      logical               :: ex,od,nmd
      character(len=20)     :: a1,a2,a3,a4,a5,a6

      ! ex, od, and nmd always become defined unless an error condition occurs.
      call journal('sc','====================================================')
!-----------------------------------------------------------------------------------------------------------------------------------
      if(iunit.ge.0)then ! query by number
         inquire(iunit,opened=od,iostat=ios,err=999,named=nmd,exist=ex)
         if(.not.ex)then
            call journal('sc','*print_inquire* unit number is not valid ,unit=',iunit)
            return
         elseif(.not.od)then
            call journal('sc','*print_inquire* unit number is not open ,unit=',iunit)
            return
         elseif(.not.nmd)then
            call journal('sc','*print_inquire* unit number is not named ,unit=',iunit)
            return
         endif
         ! unit is connected and name and exists so get name
         inquire(iunit,iostat=ios,err=999,name=nm) ! set nm to filename
         iend=len_trim(nm)
         ! go on, pretending you queried by name
      else ! query by name
         nm=name
         iend=len_trim(nm(:len(name)))
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(iend.eq.0)then
         call journal('sc','*print_inquire* blank filename')
         inquire(unit=iunit,     &
     &   exist=ex,               &
     &   opened=od,              &
     &   number=inum,            &
     &   direct=a1,              &
     &   sequential=a2,          &
     &   access=a3,              &
     &   formatted=a4,           &
     &   unformatted=a5,         &
     &   recl=ircl,              &
     &   nextrec=inext,          &
     &   form=a6,                &
     &   iostat=ios,err=999)
      else
         call journal('sc','*print_inquire* checking file:'//trim(nm(1:iend)) )
         inquire(file=nm(:iend), &
     &   exist=ex,               &
     &   opened=od,              &
     &   number=inum,            &
     &   direct=a1,              &
     &   sequential=a2,          &
     &   access=a3,              &
     &   formatted=a4,           &
     &   unformatted=a5,         &
     &   recl=ircl,              &
     &   nextrec=inext,          &
     &   form=a6,                &
     &   iostat=ios,err=999)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(.not.ex)then
         call journal('sc','*print_inquire* file does not exist')
         return
      else
         call journal('sc','*print_inquire* file exists')
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(.not.od)then
         call journal('sc','*print_inquire* file is not open')
         return
      else
         call journal('sc','*print_inquire* using unit number ',inum)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!      call journal('sc','can access be sequential? '//a2)
!      call journal('sc','can access be direct? '//a1)
!      call journal('sc','is formatted I/O allowed? '//a4)
!      call journal('sc','is unformatted I/O allowed? '//a5)
!     DIRECT/SEQUENTIAL, FORMATTED/UNFORMATTED
      call journal('sc','*print_inquire* access type '//trim(a3)//','//trim(a6))
      if(a3.eq.'DIRECT')then
         call journal('sc','*print_inquire* using record length of RECL=',ircl)
         call journal('sc','*print_inquire* next record to sequentially read=',inext)
      endif
      return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
      call journal('sc','*print_inquire* bad inquire')
!     If an error condition occurs during execution of an INQUIRE  statement,
!     all of the inquiry identifiers except ios become undefined.
      call journal('sc','*print_inquire* inquire call failed,iostat=',ios)
end subroutine print_inquire
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    read_table(3f) - [M_io] read file containing a table of numeric values
!!
!!##SYNOPSIS
!!
!!   subroutine read_table(filename,array,ierr)
!!
!!    character(len=*),intent(in)             :: filename
!!
!!    doubleprecision,allocatable,intent(out) :: array(:,:)
!!    ! or
!!    real           ,allocatable,intent(out) :: array(:,:)
!!
!!    integer,intent(out)                     :: ierr
!!
!!##DESCRIPTION
!!     Read a table from a file that is assumed to be columns of
!!     space-delimited numbers, with each row containing the same
!!     numer of values
!!
!!##OPTIONS
!!       filename   filename to read
!!       array      array to create
!!       ierr       zero if no error occurred
!!##EXAMPLES
!!
!!    Sample program, assuming the input file "inputfile" exists:
!!
!!     program demo_read_table
!!     use M_io, only : read_table
!!     doubleprecision,allocatable :: array(:,:)
!!
!!     ! create test file
!!     open(file='inputfile',unit=10)
!!     write(10,'(a)') '1 10  45'
!!     write(10,'(a)') '10 10  45'
!!     write(10,'(a)') '  2 20  15'
!!     write(10,'(a)') ' 20.345 20  15'
!!     write(10,'(a)') '  3 30.111   0'
!!     write(10,'(a)') '30 30e3   0'
!!     write(10,'(a)') '  4 300.444e-1 -10'
!!     write(10,'(a)') '40 30.5555d0 -10'
!!     write(10,'(a)') '  4 300.444E-1 -10'
!!     write(10,'(a)') '40 30.5555D0 -10'
!!     close(unit=10)
!!
!!     ! read file as a table
!!     call read_table('inputfile',array,ierr)
!!
!!     ! print values
!!     write(*,*)'size=',size(array)
!!     write(*,*)'size=',size(array,dim=1)
!!     write(*,*)'size=',size(array,dim=2)
!!     do i=1,size(array,dim=1)
!!        write(*,*)array(i,:)
!!     enddo
!!
!!     ! remove sample file
!!     open(file='inputfile',unit=10)
!!     close(unit=10,status='delete')
!!
!!     end program demo_read_table
!!
!!   Results:
!!
!!     size=          30
!!     size=          10
!!     size=           3
!!       1.0000000000000000        10.000000000000000        45.000000000000000
!!       10.000000000000000        10.000000000000000        45.000000000000000
!!       2.0000000000000000        20.000000000000000        15.000000000000000
!!       20.344999999999999        20.000000000000000        15.000000000000000
!!       3.0000000000000000        30.111000000000001        0.0000000000000000
!!       30.000000000000000        30000.000000000000        0.0000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!===================================================================================================================================
subroutine read_table_doubleprecision(filename,array,ierr)
use M_strings, only : s2vs
implicit none

character(len=*),intent(in)             :: FILENAME
doubleprecision,allocatable,intent(out) :: array(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table_doubleprecision* failed to load file '//FILENAME
       ierr=-1
    else
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          line(i:i)=text(i)
       enddo
       icols=size(s2vs(line))
       allocate(array(irows,icols))

       array=0.0d0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             line(k:k)=text(i)
          enddo
          istart=i+1
          array(j,:)=s2vs(line)
       enddo

       deallocate(text)  ! release memory
    endif

end subroutine read_table_doubleprecision
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_table_real(filename,array,ierr)
use M_strings, only : s2vs
implicit none

character(len=*),intent(in)             :: FILENAME
real,allocatable,intent(out) :: array(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table_real* failed to load file '//FILENAME
       ierr=-1
    else
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          line(i:i)=text(i)
       enddo
       icols=size(s2vs(line))
       allocate(array(irows,icols))

       array=0.0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             line(k:k)=text(i)
          enddo
          istart=i+1
          array(j,:)=s2vs(line)
       enddo

       deallocate(text)  ! release memory
    endif

end subroutine read_table_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    swallow(3f) - [M_io] read a file into a character array line by line
!!##SYNOPSIS
!!
!!   subroutine swallow(filename,pageout)
!!
!!    character(len=*),intent(in) :: filename
!!    character(len=1),allocatable,intent(out) :: pageout(:)
!!##DESCRIPTION
!!    Read an entire file into memory as a character array, one character variable per line.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory, or LUN (Fortran Logical Unit Number)
!!       pageout    array of characters to hold file
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_swallow
!!    use M_io,      only : swallow
!!    use M_strings, only : notabs
!!    implicit none
!!    character(len=4096)          :: FILENAME   ! file to read
!!    character(len=:),allocatable :: pageout(:) ! array to hold file in memory
!!    integer                      :: longest, lines, i, ilen
!!    character(len=:),allocatable :: line
!!       ! get a filename
!!       call get_command_argument(1, FILENAME)
!!       ! allocate character array and copy file into it
!!       call swallow(FILENAME,pageout)
!!       if(.not.allocated(pageout))then
!!          write(*,*)'*demo_swallow* failed to load file '//FILENAME
!!       else
!!          ! write file from last line to first line
!!          longest=len(pageout)
!!          lines=size(pageout)
!!          allocate(character(len=longest)::line)
!!          write(*,*)'number of lines is ',lines
!!          write(*,*)'and length of lines is ',longest
!!          write(*,'(a)')repeat('%',longest+2)
!!          do i=lines,1,-1
!!             call notabs(pageout(i),line,ilen)
!!             write(*,'("%",a,"%")')line
!!          enddo
!!          write(*,'(a)')repeat('%',longest+2)
!!          deallocate(pageout)  ! release memory
!!       endif
!!    end program demo_swallow
!!
!!   Given
!!
!!    first line
!!    second line
!!    third line
!!
!!   Expected output
!!
!!     number of lines is 3
!!     and length of lines is 11
!!    %%%%%%%%%%%%%
!!    %third line %
!!    %second line%
!!    %first line %
!!    %%%%%%%%%%%%%
!===================================================================================================================================
subroutine swallow(FILENAME,pageout)
implicit none
class(*),intent(in)                      :: FILENAME   ! file to read
character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory

   call slurp(FILENAME,text) ! allocate character array and copy file into it

   if(.not.allocated(text))then
      select type(FILENAME)
       type is (character(len=*)); write(*,*)'*swallow* failed to load file '//FILENAME
       type is (integer);          write(*,'(a,i0)')'*swallow* failed to load file unit ',FILENAME
      end select
   else  ! convert array of characters to array of lines
      pageout=page(text)
      deallocate(text)     ! release memory
   endif

contains
function page(array)  result (table)

character(len=*),parameter::ident_5="@(#)M_strings::page(3fp): function to copy char array to page of text"

character(len=1),intent(in)  :: array(:)
character(len=:),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
!!character(len=1),parameter   :: nl=new_line('A')
character(len=1),parameter   :: nl=char(10)
   lines=0
   linelength=0
   length=0
   sz=size(array)
   do i=1,sz
      if(array(i).eq.nl)then
         linelength=max(linelength,length)
         lines=lines+1
         length=0
      else
         length=length+1
      endif
   enddo
   if(sz.gt.0)then
      if(array(sz).ne.nl)then
         lines=lines+1
      endif
   endif

   allocate(character(len=linelength) :: table(lines))
   table=' '

   linecount=1
   position=1
   do i=1,sz
      if(array(i).eq.nl)then
         linecount=linecount+1
         position=1
      else
         table(linecount)(position:position)=array(i)
         position=position+1
      endif
   enddo
end function page
end subroutine swallow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    SLURP(3f) - [M_io] read a file into a character array
!!##SYNOPSIS
!!
!!   subroutine slurp(filename,text)
!!
!!    character(len=*),intent(in) :: filename
!!     or
!!    integer,intent(in)          :: filenumber
!!
!!    character(len=1),allocatable,intent(out) :: text(:)
!!    integer,intent(out),optional :: length
!!    integer,intent(out),optional :: lines
!!##DESCRIPTION
!!    Read an entire file into memory as a stream, retaining line end
!!    terminators.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory or LUN (Fortran Logical Unit Number)
!!       text       array of characters to hold file
!!       length     length of longest line read(Optional).
!!       lines      number of lines read(Optional).
!!
!!##EXAMPLES
!!
!!    Sample program, which  creates test input file "inputfile":
!!
!!     program demo_slurp
!!     use M_io, only      : slurp
!!     implicit none
!!     character(len=1),allocatable :: text(:) ! array to hold file in memory
!!     character(len=*),parameter :: FILENAME='inputfile' ! file to read
!!
!!     ! create test file
!!     open(file=FILENAME,unit=10)
!!     write(10,'(a)') new_line('A')//'esrever lliw'
!!     write(10,'(a)') 'margorp elpmas eht taht'
!!     write(10,'(a)') 'elif elpmas a si sihT'
!!     close(unit=10)
!!
!!     call slurp(FILENAME,text) ! allocate character array and copy file into it
!!
!!     if(.not.allocated(text))then
!!        write(*,*)'*rever* failed to load file '//FILENAME
!!     else
!!        ! write file reversed to stdout
!!        write(*,'(*(a:))',advance='no')text(size(text):1:-1)
!!        deallocate(text)  ! release memory
!!     endif
!!
!!     end program demo_slurp
!!
!!    Expected output:
!!
!!     >This is a sample file
!!     >that the sample program
!!     >will reverse
!===================================================================================================================================
subroutine slurp(filename,text,length,lines)
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none

character(len=*),parameter::ident_6="@(#)M_io::slurp(3f): allocate text array and read file filename into it"

class(*),intent(in)                      :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer,intent(out),optional             :: length      ! length of longest line
integer,intent(out),optional             :: lines       ! number of lines
!-----------------------------------------------------------------------------------------------------------------------------------
integer :: nchars=0             ! holds size of file
integer :: igetunit             ! use newunit=igetunit in f08
integer :: ios=0                ! used for I/O error status
integer :: length_local
integer :: lines_local
integer :: i
integer :: icount
character(len=256)  :: message
character(len=4096) :: local_filename
!-----------------------------------------------------------------------------------------------------------------------------------
   length_local=0
   lines_local=0
   igetunit=notopen(10,99)         ! find unused file unit number (hopefully)
   if(igetunit.lt.0)then
      call stderr_local('*slurp* could not find unused file unit number')
      return
   endif
!-------------------------------------------
   message=''
      select type(FILENAME)
       type is (character(len=*))
          open(unit=igetunit, file=trim(filename), action="read", iomsg=message,&
           form="unformatted", access="stream",status='old',iostat=ios)
          local_filename=filename
       type is (integer)
          rewind(unit=filename,iostat=ios,iomsg=message)
          write(local_filename,'("unit ",i0)')filename
      end select
!-------------------------------------------
   if(ios.eq.0)then  ! if file was successfully opened
!-------------------------------------------
      inquire(unit=igetunit, size=nchars)
!-------------------------------------------
      if(nchars.le.0)then
         call stderr_local( '*slurp* empty file '//trim(local_filename) )
         return
      endif
      ! read file into text array
      !
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios) text      ! load input file -> text array
      if(ios.ne.0)then
         call stderr_local( '*slurp* bad read of '//trim(local_filename) )
      endif
   else
      call stderr_local('*slurp* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif

   close(iostat=ios,unit=igetunit)            ! close if opened successfully or not

   if(present(lines).or.present(length))then  ! get length of longest line and number of lines
      icount=0
      do i=1,nchars
         if(text(i).eq.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
            icount=0
         endif
         icount=icount+1
      enddo
      if(nchars.ne.0)then
         if(text(nchars).ne.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
         endif
      endif
      if(present(lines))lines=lines_local
      if(present(length))length=length_local
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr_local(message)
use iso_fortran_env, only : error_unit
character(len=*) :: message
   write(error_unit,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine slurp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use
!!##SYNOPSIS
!!
!!    Usage
!!
!!       integer function notopen(start,end,err)
!!       integer,optional,intent(in)  :: start
!!       integer,optional,intent(in)  :: end
!!       integer,optional,intent(out) :: err
!!##DESCRIPTION
!!    A free FORTRAN unit number is needed to OPEN a file. NOTOPEN() returns
!!    a FORTRAN unit number from START to END not currently associated with
!!    an I/O unit. START and END are expected to be positive integers where
!!    END .ge. START.
!!
!!    If NOTOPEN() returns -1, then no free FORTRAN unit could be found in
!!    the specified range.
!!
!!    Otherwise, NOTOPEN() returns an integer representing a free FORTRAN
!!    logical unit number. Note that NOTOPEN() assumes the following unit
!!    numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module
!!
!!       ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT
!!
!!    are special, and will never return those values.
!!
!!##OPTIONS
!!       start  optional logical unit number to start scan at, defaults to 10.
!!       end    optional logical unit number to stop scan at, defaults to 99.
!!       err    optional error flag returned. ERR will be non-zero if no errors.
!!              If not present and an error occurs the program will stop instead
!!              of returning.
!!
!!##NOTES
!!
!!    Why are the default START and END limits from 10 to 99? the Fortran 77
!!    standard did not specify a specific limit on the upper range limit, but
!!    the LUN range of 1 to 99 was almost always supported in conventional
!!    programming environments. Additionally, units in the range 0-10 have
!!    often been the units used for pre-assigned files. Occasionally 100,
!!    101 and 102 are reserved (for files such as standard input, standard
!!    output, standard error, ...). Therefore, the defaults for START and
!!    END were selected to be 10 and 99. And most programs do not need
!!    more than 90 files simultaneously open, so the defaults work well in
!!    practice with many versions/vintages of Fortran.
!!
!!    Note that an environment may impose a limit on the number of
!!    simultaneously open files (which some compilers work around).
!!
!!    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead
!!    of an open unit locator.
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_notopen ! test the NOTOPEN(3f) function
!!     use m_io, only: notopen
!!     implicit none
!!     integer :: ii, ierr, igot
!!
!!     write(*,*)'check for preassigned files from unit 0 to unit 1000'
!!     write(*,*)'(5 and 6 always return -1)'
!!
!!     do ii=0,1000
!!        if(notopen(ii,ii,ierr) .ne. ii)then
!!           write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
!!        endif
!!     enddo
!!
!!     ! open all files from UNIT=10 to UNIT=30 so have used units
!!     do ii=10,30,1
!!       open(unit=ii,status="scratch")
!!     enddo
!!     ! close UNIT=25
!!     close(25)
!!
!!     ! find open file in range 10 to 30
!!     write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)
!!
!!     close(18)
!!     do ii=10,32
!!       igot=notopen(ii,ii,ierr)
!!       write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
!!     enddo
!!
!!     end program demo_notopen
!!
!!    Expected output(can vary with each programming environment):
!!
!!       check for preassigned files from unit 0 to unit 1000
!!       (5 and 6 always return -1)
!!       INUSE:    0    -1
!!       INUSE:    5    -1
!!       INUSE:    6    -1
!!       Should get 25 for this .. 25
!!       For  unit  10  I  got  -1  with  ERR=  -1
!!       For  unit  11  I  got  -1  with  ERR=  -1
!!       For  unit  12  I  got  -1  with  ERR=  -1
!!       For  unit  13  I  got  -1  with  ERR=  -1
!!       For  unit  14  I  got  -1  with  ERR=  -1
!!       For  unit  15  I  got  -1  with  ERR=  -1
!!       For  unit  16  I  got  -1  with  ERR=  -1
!!       For  unit  17  I  got  -1  with  ERR=  -1
!!       For  unit  18  I  got  18  with  ERR=   0
!!       For  unit  19  I  got  -1  with  ERR=  -1
!!       For  unit  20  I  got  -1  with  ERR=  -1
!!       For  unit  21  I  got  -1  with  ERR=  -1
!!       For  unit  22  I  got  -1  with  ERR=  -1
!!       For  unit  23  I  got  -1  with  ERR=  -1
!!       For  unit  24  I  got  -1  with  ERR=  -1
!!       For  unit  25  I  got  25  with  ERR=   0
!!       For  unit  26  I  got  -1  with  ERR=  -1
!!       For  unit  27  I  got  -1  with  ERR=  -1
!!       For  unit  28  I  got  -1  with  ERR=  -1
!!       For  unit  29  I  got  -1  with  ERR=  -1
!!       For  unit  30  I  got  -1  with  ERR=  -1
!!       For  unit  31  I  got  31  with  ERR=   0
!!       For  unit  32  I  got  32  with  ERR=   0
!!
!!##AUTHORS
!!       John S. Urban
!===================================================================================================================================
integer function notopen(start,end,err)
!-----------------------------------------------------------------------------------------------------------------------------------
! AUTHOR: John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
! access computing environment
use iso_fortran_env, only : error_unit,input_unit,output_unit
implicit none

character(len=*),parameter::ident_7="@(#)M_io::notopen(3f): find free FORTRAN unit number to OPEN() a file"

integer,optional,intent(in)    :: start                           ! unit number to start looking at
integer,optional,intent(in)    :: end                             ! last unit number to look at
integer,optional,intent(out)   :: err                             ! error flag returned
integer                        :: istart
integer                        :: iend
integer                        :: ierr

integer         :: i10                                            ! counter from start to end
integer         :: ios                                            ! iostatus from INQUIRE
logical         :: lopen                                          ! returned from INQUIRE
logical         :: lexist                                         ! returned from INQUIRE
!-----------------------------------------------------------------------------------------------------------------------------------
   !! IEND=MERGE( END, 99, PRESENT(END)) do not use merge, as TSOURCE must be evaluated before the call
   if(present(start))then; istart=start; else; istart=10; endif
   if(present(end  ))then; iend  =end  ; else; iend  =99; endif
   ierr=0
   notopen=(-1)                                                   ! result if no units are available
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=istart,iend                                             ! check units over selected range
      select case (i10)                                           ! always skip these predefined units
      case(error_unit,input_unit,output_unit)
          cycle
      end select
      inquire( unit=i10, opened=lopen, exist=lexist, iostat=ios )
      if( ios == 0 )then                                          ! no error on inquire
         if(.not. lopen .and. lexist)then                         ! if unit number not in use, return it
            notopen = i10
            exit                                                  ! only need to find one, so return
         endif
      else
         write(error_unit,*)'*notopen*:error on unit ',i10,'=',ios
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if (notopen .lt. 0 )then                                       ! no valid unit was found in given range
      ierr=-1
   else                                                           ! valid value being returned
      ierr=0
   endif
   if(present(err))then                                           ! if error flag is present set it
      err=ierr
   elseif(ierr.ne.0)then                                          ! if error occurred and error flag not present stop program
      stop 1
   endif
end function notopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dirname(3f) - [M_io] strip last component from filename
!!
!!##SYNOPSIS
!!
!!    function dirname(FILENAME) result (DIRECTORY)
!!
!!      character(len=*),intent(in)  :: FILENAME
!!      character(len=:),allocatable :: DIRECTORY
!!
!!##DESCRIPTION
!!    Output FILENAME with its last non-slash component and trailing slashes removed.
!!    if FILENAME contains no '/' character, output '.' (meaning the current directory).
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not contain
!!    trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to remove the last leaf from
!!
!!##RETURNS
!!      DIRECTORY  directory name for pathname
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dirname
!!    use M_io, only : dirname
!!    implicit none
!!    character(len=:),allocatable :: filename
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1 , command_argument_count()
!!       call get_command_argument (i , length=filename_length)
!!       allocate(character(len=filename_length) :: filename)
!!       call get_command_argument (i , value=filename)
!!       write(*,'(a)')dirname(filename)
!!       deallocate(filename)
!!    enddo
!!    end program demo_dirname
!!
!!   Sample program executions:
!!
!!      demo_dirname /usr/bin/          -> "/usr"
!!      demo_dirname dir1/str dir2/str  -> "dir1" followed by "dir2"
!!      demo_dirname stdio.h            -> "."
!!
!!##SEE ALSO
!!    dirname(3c), basename(3c), readlink(3c), realpath(3c)
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        dirname(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!! LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.
!!                 There is NO WARRANTY, to the extent permitted by law.
!===================================================================================================================================
function dirname(filename) result (directory)
implicit none

character(len=*),parameter::ident_8="@(#)M_io::dirname(3f): strip last component from filename"

character(len=*),intent(in)      :: filename
character(len=:),allocatable     :: directory
integer                          :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   directory=trim(filename)
   call removetail()                         ! trim trailing slashes even if duplicates
   iend=index(directory,'/',back=.true.)     ! find last slash if any
   if(iend.eq.0)then                         ! filename is a leaf
      directory='.'                          ! special case
   else
      directory=directory(:iend-1)           ! remove leaf
      call removetail()                      ! trim off trailing slashes in case duplicates
   endif
   directory=trim(directory)                 ! clean up return value
contains
   subroutine removetail()              ! replace trailing slashes with spaces even if duplicates
   integer :: right
   do right=len(directory),1,-1
      if(directory(right:right).eq.'/'.or.directory(right:right).eq.' ')then
         directory(right:right)=' '
      else
         exit
      endif
   enddo
   end subroutine removetail

end function dirname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     splitpath(3f) - [M_io] split a Unix pathname into components
!!
!!##SYNOPSIS
!!
!!   splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out),optional :: dir
!!    character(len=maxlen),intent(out),optional :: name
!!    character(len=maxlen),intent(out),optional :: basename
!!    character(len=maxlen),intent(out),optional :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash separates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of the
!!    filename components; it merely parses a string.
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not
!!    contain trailing spaces.
!!
!!##OPTIONS
!!    path      Path to be broken into components. It is assumed
!!
!!              o Forward slashes (/) separate pathname components.
!!              o the name '.' means "current directory"
!!              o the name '..' means "up one directory"
!!              o a pathname ending in a slash is a directory name
!!              o a slash starting the pathname represents the root
!!                directory.
!!              o trailing spaces are insignificant.
!!
!!    Using these rules helps to reduce incorrect parsing, but the
!!    routine is only intended for simple parsing of names of the form
!!    "[dir/]name[.extension].
!!
!!##RESULTS
!!    dir       Path of directories, including the trailing slash.
!!    name      Name of file leaf or, if no file is specified in path,
!!              name of the lowest directory.
!!    basename  NAME with any extension removed
!!    ext       File name extension, if any, including the leading period (.).
!!
!!    The path parameter can be a complete or partial file specification. The
!!    special name "." is assumed to mean the current directory, and the
!!    special name ".." is assumed to mean one directory above the current
!!    directory.
!!
!!##EXAMPLE
!!
!!   program demo_splitpath
!!
!!    use m_io, only : splitpath
!!    implicit none
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),parameter   :: file(*)=[&
!!       & 'dirs/name.ext  ', &
!!       & 'xx/IO/zz/NN.FF ', &
!!       & 'xx/IO/zz/NN    ', &
!!       & '/xx/IO/zz/NN   ', &
!!       & '/xx/IO/zz/     ', &
!!       & '/xx/IO/zz.A/   ', &
!!       & '/xx/IO/zz/.    ', &
!!       & '               ', &
!!       & './             ', &
!!       & '/              ', &
!!       & '/..            ', &
!!       & './..           ', &
!!       & 'name.          ', &
!!       & '.name          ', &
!!       & '.name.         ', &
!!       & '.              ', &
!!       & '..             ', &
!!       & '...            ']
!!
!!    character(len=maxlen)  :: dir
!!    character(len=maxlen)  :: name
!!    character(len=maxlen)  :: basename
!!    character(len=maxlen)  :: ext
!!    integer                :: i
!!    integer                :: longest
!!    longest=maxval(len_trim(file)) ! find longest filename
!!
!!    do i=1,size(file)
!!       call splitpath(file(i), dir, name, basename, ext)
!!       write(*,'(*("| ",a:))')  &
!!       & file(i)(:longest),     &
!!       & dir(:longest),         &
!!       & name(:longest),        &
!!       & basename(:longest),    &
!!       & ext(:longest)
!!    enddo
!!   end program demo_splitpath
!!
!!   Output
!!
!!    | dirs/name.ext | dirs          | name.ext      | name          | .ext
!!    | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
!!    | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
!!    | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
!!    | /xx/IO/zz/    | /xx/IO/zz     |               |               |
!!    | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
!!    | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
!!    |               | .             |               |               |
!!    | ./            | .             |               |               |
!!    | /             | /             |               |               |
!!    | /..           | /             |               |               |
!!    | ./..          | ./..          |               |               |
!!    | name.         |               | name.         | name          | .
!!    | .name         |               | .name         | .name         |
!!    | .name.        |               | .name.        | .name         | .
!!    | .             | .             |               |               |
!!    | ..            |               |               |               |
!!    | ...           |               | ...           | ..            | .
!===================================================================================================================================
subroutine splitpath(path,dir,name,basename,ext)
use M_strings, only : split
implicit none

character(len=*),parameter::ident_9="@(#)M_io::splitpath(3f): split Unix pathname into components (dir,name,basename,extension)"

!===================================================================================================================================
character(len=*),intent(in)           :: path
character(len=*),intent(out),optional :: dir
character(len=*),intent(out),optional :: name
character(len=*),intent(out),optional :: basename
character(len=*),intent(out),optional :: ext
integer,parameter                     :: maxlen=4096
character(len=maxlen)                 :: dir_local
character(len=maxlen)                 :: name_local
character(len=maxlen)                 :: basename_local
character(len=maxlen)                 :: ext_local
character(len=len(path)+1)            :: path_local
integer                               :: where
integer                               :: i
integer                               :: iend
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir_local=''
   name_local=''
   basename_local=''
   ext_local=''
   iend=len_trim(path_local)
   LOCAL : block
!===================================================================================================================================
   if(iend.eq.0)then                         ! blank input path
      dir_local='.'
      exit LOCAL
   endif
!===================================================================================================================================
   if(path_local(iend:iend).eq.'/')then      ! assume entire name is a directory if it ends in a slash
      if(iend.gt.1)then
         dir_local=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir_local=path_local
      endif
      exit LOCAL
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i).eq.'/')then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend.eq.0)then                         ! path composed entirely of slashes.
      dir_local='/'
      exit LOCAL
   endif
!===================================================================================================================================
   where=INDEX(path_local,'/',BACK=.true.)   ! find any right-most slash in remaining non-null name_local after trimming trailing slashes
   if(where.le.0)then                        ! no slash in path so everything left is name_local
      name_local=path_local(:iend)                 ! this is name_local unless '.' or '..'
   else                                      ! last slash found
      dir_local=path_local(:where-1)               ! split into directory
      name_local=path_local(where+1:iend)          ! this is name_local unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name_local(1:3))                   ! special cases where name_local is a relative directory name_local '.' or '..'
   case('.  ')
      dir_local=path_local
      name_local=''
   case('.. ')
      if(dir_local.eq.'')then
         if(path_local(1:1).eq.'/')then
            dir_local='/'
         endif
      else
         dir_local=path_local
      endif
      name_local=''
   case default
   end select
!===================================================================================================================================
   if(name_local.eq.'.')then
      name_local=''
   endif
!===================================================================================================================================
   iend=len_trim(name_local)
   where=INDEX(name_local,'.',BACK=.true.)         ! find any extension
   if(where.gt.0.and.where.ne.1)then         ! only consider a non-blank extension name_local
      ext_local=name_local(where:)
      basename_local=name_local(:where-1)
   else
      basename_local=name_local
   endif
!===================================================================================================================================
   endblock LOCAL
   if(present(dir))dir=dir_local
   if(present(name))name=name_local
   if(present(basename))basename=basename_local
   if(present(ext))ext=ext_local
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isdir(3f) - [M_io] checks if argument is a directory path
!!##SYNTAX
!!
!!   logical function isdir(path)
!!
!!    character(len=*),intent(in) :: path
!!    logical                     :: isdir
!!
!!##DESCRIPTION
!!
!!    isdir(3f) checks if path is a path to a directory on Unix-compatible file systems
!!
!!##OPTIONS
!!
!!    path    a character string representing a directory pathname.
!!            Trailing spaces are ignored.
!!
!!##RETURNS
!!    isdir   TRUE if the path is currently a directory
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_isdir
!!    Use M_io, only : isdir
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a directory? ', isdir(names(i))
!!    enddo
!!    end program demo_isdir
!!
!!   Results:
!!
!!    is /tmp a directory?  T
!!    is /tmp/NOTTHERE a directory?  F
!!    is /usr/local a directory?  T
!!    is . a directory?  T
!!    is PROBABLY_NOT a directory?  F
!===================================================================================================================================
function isdir(dirname)
implicit none

character(len=*),parameter::ident_10="@(#)M_io::isdir(3f): determine if DIRNAME is a directory name ON UNIX-COMPATIBLE file systems"

logical                     :: isdir
character(len=*),intent(in) :: dirname

! a trick to be sure DIRNAME is a dir on systems supporting Unix-compatible pathnames
if(dirname.ne.'')then
   inquire( file=trim(dirname)//"/.", exist=isdir )
else
   isdir=.false.
endif

end function isdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_all(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit
!!
!!##SYNTAX
!!   function read_all(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,intent(out)                      :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##OPTIONS
!!
!!    LINE   line read
!!    LUN    optional LUN (Fortran logical I/O unit) number. Defaults
!!           to stdin.
!!##RETURNS
!!
!!    IER    zero unless an error occurred. If not zero, LINE returns the
!!           I/O error message.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_read_all
!!    use M_io, only : read_all
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (read_all(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program demo_read_all
!===================================================================================================================================
function read_all(line,lun) result(ier)
use iso_fortran_env, only : INPUT_UNIT
implicit none

character(len=*),parameter::ident_11="&
&@(#)M_io::read_all(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier
character(len=4096)                      :: message

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=buflen)                    :: buffer
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=INPUT_UNIT
   endif

   INFINITE: do                                                      ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for files
                                                                     ! other than stdin so system line limit is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)            ! append what was read to result
      if(is_iostat_eor(ier))then                                     ! if hit EOR reading is complete unless backslash ends the line
         ier=0                                                       ! hitting end of record is not an error for this routine
         exit INFINITE                                               ! end of reading line
     elseif(ier.ne.0)then                                            ! end of file or error
        line=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   line=line_local                                                   ! trim line
end function read_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_line(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit cleaning up input line
!!
!!##SYNTAX
!!   function read_line(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in)                       :: lun
!!    integer,intent(out)                      :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    o Append lines that end in a backslash with next line
!!    o Expand tabs
!!    o Replace unprintable characters with spaces
!!    o Remove trailing carriage return characters and white space
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_read_line
!!    use M_io, only : read_line
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (read_line(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program demo_read_line
!===================================================================================================================================
function read_line(line,lun) result(ier)
use iso_fortran_env, only : INPUT_UNIT
use M_strings,only : notabs
implicit none

character(len=*),parameter::ident_12="&
&@(#)M_io::read_line(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
integer                                  :: biggest
character(len=buflen)                    :: buffer
integer                                  :: last
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=INPUT_UNIT
   endif

   INFINITE: do                                                           ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize) buffer ! read next buffer (might use stream I/O for files
                                                                          ! other than stdin so system line limit is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)   ! append what was read to result
      if(is_iostat_eor(ier))then                            ! if hit EOR reading is complete unless backslash ends the line
         last=len(line_local)
         if(last.ne.0)then
            if(line_local(last:last).eq.'\')then            ! if line ends in backslash it is assumed a continued line
               line_local=line_local(:last-1)               ! remove backslash
               cycle INFINITE                               ! continue on and read next line and append to result
            endif
         endif
         ier=0                                              ! hitting end of record is not an error for this routine
         exit INFINITE                                      ! end of reading line
     elseif(ier.ne.0)then                                   ! end of file or error
        exit INFINITE
     endif
   enddo INFINITE
   biggest=8*len(line_local)                                ! worst case is raw line is all tab characters
   allocate(character(len=biggest) :: line)
   call notabs(line_local,line,last)                        ! expand tabs, trim carriage returns, remove unprintable characters
   line=trim(line(:last))                                   ! trim line
end function read_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      get_tmp(3f) - [M_io] Return the name of the scratch directory
!!##SYNOPSIS
!!
!!     function get_tmp() result(tname)
!!
!!      character(len=:),allocatable :: tname
!!##DESCRIPTION
!!
!!    Return the name of the scratch directory set by the most common environment variables used to designate a scratch directory.
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1] to use to specify a temporary directory for scratch space.
!!    If $TMPDIR is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If nothing is set "/tmp/" is returned. The
!!    returned value always ends in "/". No test is made that the directory exists or is writable.
!!
!!##EXAMPLE
!!
!!
!!   Sample:
!!
!!     program demo_get_tmp
!!     use M_io, only : get_tmp, uniq
!!     implicit none
!!     character(len=:),allocatable :: answer
!!        answer=get_tmp()
!!        write(*,*)'result is ',answer
!!        answer=get_tmp()//uniq('_scratch',create=.false.)
!!        write(*,*)'the file ',answer,' was a good scratch file name, at least a moment ago'
!!     end program demo_get_tmp
!!
!!   Sample Results:
!!
!!     result is /cygdrive/c/Users/JSU/AppData/Local/Temp/
!===================================================================================================================================
function get_tmp() result(tname)

character(len=*),parameter::ident_13="@(#)M_io::get_tmp(3f): Return the name of the scratch directory"

character(len=:),allocatable :: tname
integer                      :: lngth
character(len=10),parameter  :: names(4)=["TMPDIR    ","TEMP      ","TEMPDIR   ","TMP       "]
integer                      :: i
   tname=''
   do i=1,size(names)
      call get_environment_variable(name=names(i), length=lngth)
      if(lngth.ne.0)then
         deallocate(tname)
         allocate(character(len=lngth) :: tname)
         call get_environment_variable(name=names(i), value=tname)
         exit
      endif
   enddo
   if(lngth.eq.0)then
      tname='/tmp'
      lngth=len_trim(tname)
   endif
   if(scan(tname(lngth:lngth),'/\').eq.0)then
      tname=tname//'/'
   endif
end function get_tmp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      scratch(3f) - [M_io] Return the name of a scratch file
!!##SYNOPSIS
!!
!!     function scratch(prefix) result(tname)
!!
!!      character(len=:),allocatable         :: tname
!!      character(len=*),intent(in),optional :: prefix
!!##DESCRIPTION
!!
!!    Fortran supports non-retainable scratch files via OPEN(STATUS='SCRATCH',... .
!!    There are circumstances where a file with a unique name is required
!!    instead. Specifying the pathname of a file can be required for performance
!!    reasons, file space limitations, or to support the ability for other
!!    processes or subprocesses to access the file.
!!
!!    SCRATCH(3f) Return the name of a scratch file in the scratch directory set
!!    by the most common environment variables used to designate a scratch
!!    directory.
!!
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1] used to
!!    specify a temporary directory for scratch space. If $TMPDIR is not set,
!!    $TEMP, $TEMPDIR, and $TMP are examined in that order. If nothing is set
!!    "/tmp/" is used.
!!
!!##OPTIONS
!!    prefix  an optional prefix for the leaf of the filename. A suffix created
!!            by genuuid(3) is used to make the name unique.
!!
!!            The default prefix is the basename of the program that called the
!!            procedure (the name trimmed of anything from the right-most period
!!            in the name to the end of the name)..
!!##EXAMPLE
!!
!!
!!   Sample:
!!
!!     program demo_scratch
!!     use M_io, only : scratch
!!     implicit none
!!     write(*,*)'find good scratch file name candidates; one should test if writable'
!!     write(*,*)scratch('JUNK:')
!!     write(*,*)scratch('')
!!     write(*,*)scratch()
!!     end program demo_scratch
!!   Results:
!!
!!     find good scratch file name candidates; one should test if writable
!!     /cygdrive/c/Users/JSU/AppData/Local/Temp/JUNK:8462159a-2ca8-4961-7ff1-2ff4f9ebaca4
!!     /cygdrive/c/Users/JSU/AppData/Local/Temp/f7585e37-8557-4f25-777d-29abb6ffb981
!!     /cygdrive/c/Users/JSU/AppData/Local/Temp/demo_scratch-ec470965-42be-4ba6-4193-0f25cf2fa26c
!===================================================================================================================================
function scratch(prefix) result(tname)
use M_uuid, only : generate_uuid

character(len=*),parameter::ident_14="@(#)M_io::scratch(3f): Return the name of a scratch file"

character(len=*),intent(in),optional :: prefix
character(len=:),allocatable :: tname

integer,parameter     :: maxlen=4096
character(len=maxlen) :: path
character(len=maxlen) :: bname

if(present(prefix))then
   tname=get_tmp()//trim(prefix)//generate_uuid()
else
   call get_command_argument(number=0,value=path)
   call splitpath(path,basename=bname)
   tname=get_tmp()//trim(bname)//'-'//generate_uuid()
endif
end function scratch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! rd(3f) - [M_io] ask for string from standard input with user-definable prompt
!!##SYNOPSIS
!!
!!   function rd(prompt,default) result(strout)
!!
!!    character(len=*),intent(in)              :: prompt
!!
!!    character(len=*),intent(in)              :: default
!!          or
!!    integer,intent(in)                       :: default
!!          or
!!    real,intent(in)                          :: default
!!          or
!!    doubleprecision,intent(in)               :: default
!!
!!    character(len=:),allocatable,intent(out) :: strout
!!
!!##DESCRIPTION
!!    Ask for string or value from standard input with user-definable prompt up
!!    to 20 times.
!!
!!    Do not use the function in an I/O statement as not all versions of Fortran
!!    support this form of recursion. Numeric values may be input in standard
!!    INTEGER, REAL, and DOUBLEPRECISION formats or as whole numbers in base 2 to
!!    36 in the format BASE#VALUE.
!!
!!##OPTIONS
!!    prompt    Prompt string; displayed on same line as input is read from
!!    default   default answer on carriage-return. The type of the default determines the
!!              type of the output.
!!##RETURNS
!!    strout    returned string or value.
!!              If an end-of-file or system error is encountered the string "EOF" is returned, or
!!              a "Nan" numeric value.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rd
!!    use M_io, only : rd
!!    character(len=:),allocatable :: mystring
!!    doubleprecision              :: d
!!    real                         :: r
!!    integer                      :: i
!!
!!    INFINITE: do
!!       mystring=rd('Enter string or "STOP":',default='Today')
!!       if(mystring.eq.'STOP')stop
!!       i=rd('Enter integer:',default=huge(0))
!!       r=rd('Enter real:',default=huge(0.0))
!!       d=rd('Enter double:',default=huge(0.0d0))
!!
!!       write(*,*)'I=', i, 'R=', r, 'D=',d,  'MYSTRING=', mystring
!!    enddo INFINITE
!!
!!    end program demo_rd
!===================================================================================================================================
function rd_character(prompt,default) result(strout)
! 1995 John S. Urban
!
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit !!, stdout=>output_unit, stderr=>error_unit
use M_journal,                    only : journal
implicit none

character(len=*),parameter::ident_15="@(#)M_io::rd_character(3fp): ask for string from standard input with user-definable prompt"

character(len=*),intent(in)  :: prompt
character(len=*),intent(in)  :: default
character(len=:),allocatable :: strout

integer                      :: len_default
integer                      :: igot
integer                      :: ierr
integer                      :: icount
!===================================================================================================================================
   len_default=len(prompt)
!===================================================================================================================================
   do icount=1,20                                                  ! prevent infinite loop on error or end-of-file
      if(len_default.gt.0)write(*,'(a,'' '')',advance='no')prompt  ! write prompt
      ierr=read_all(strout,stdin)                                  ! get back string
      igot=len(strout)
      if(ierr.ne.0)then
         strout='EOF'
         cycle
      elseif(igot.eq.0.and.len_default.gt.0)then
         strout=default
         exit
      elseif(igot.le.0)then
         call journal('*rd* blank string not allowed')
         cycle
      else
         exit
      endif
   enddo
end function rd_character
!===================================================================================================================================
function rd_doubleprecision(prompt,default) result(dvalue)
use M_strings, only : s2v, isnumber, decodebase
implicit none

character(len=*),parameter::ident_16="&
&@(#)M_io::rd_doubleprecision(3fp): ask for number from standard input with user-definable prompt"

doubleprecision              :: dvalue
integer                      :: ivalue
character(len=*),intent(in)  :: prompt
doubleprecision,intent(in)   :: default
character(len=:),allocatable :: strout
character(len=:),allocatable :: message
integer                      :: itest
integer                      :: i

do i=1,20 ! twenty tries max
   strout=rd_character(prompt,'NaN')

   ! 1 for an integer [-+]NNNNN
   ! 2 for a whole number [-+]NNNNN.
   ! 3 for a real value [-+]NNNNN.MMMM
   ! 4 for a exponential value [-+]NNNNN.MMMM[-+]LLLL [-+]NNNNN.MMMM[ed][-+]LLLL
   ! values less than 1 represent an error
   if(strout.eq.'NaN')then
      dvalue=default
      exit
   elseif(index(strout,'#').ne.0)then
      if( decodebase(strout,0,ivalue))then
         dvalue=ivalue
         exit
      else
         write(*,*)'ERROR> could not convert ',strout
      endif
   else
      itest=isnumber(strout,message)
      if(itest.gt.0)then
         dvalue=s2v(strout)
         exit
      else
         write(*,*)' ERROR> for ',strout,' ',itest,':',trim(message)
         cycle
      endif
   endif
enddo
end function rd_doubleprecision
!===================================================================================================================================
function rd_real(prompt,default) result(rvalue)
implicit none

character(len=*),parameter::ident_17="@(#)M_io::rd_real(3fp): ask for number from standard input with user-definable prompt"

real                         :: rvalue
character(len=*),intent(in)  :: prompt
real,intent(in)              :: default
   rvalue=real(rd_doubleprecision(prompt,dble(default)))
end function rd_real
!===================================================================================================================================
function rd_integer(prompt,default) result(ivalue)
implicit none

character(len=*),parameter::ident_18="@(#)M_io::rd_integer(3fp): ask for number from standard input with user-definable prompt"

integer                      :: ivalue
character(len=*),intent(in)  :: prompt
integer,intent(in)           :: default
   ivalue=int(rd_doubleprecision(prompt,dble(default)))
end function rd_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_io()

!! setup
   call test_dirname()
   call test_get_tmp()
   call test_isdir()
   call test_notopen()
   call test_print_inquire()
   call test_rd()
   call test_read_all()
   call test_read_line()
   call test_read_table()
   call test_slurp()
   call test_splitpath()
   call test_uniq()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dirname()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
call unit_check_start('dirname',msg='')
call unit_check('dirname',  dirname('/usr/bin/') .eq. '/usr', msg=msg('/usr/bin ==>',dirname('/usr/bin'))  )
call unit_check('dirname',  dirname('dir1/str/') .eq. 'dir1', msg=msg('dir1/str ==>',dirname('dir1/str/')) )
call unit_check('dirname',  dirname('stdio.h')   .eq. '.',    msg=msg('/stdio.h ==>',dirname('stdio.h'))  )
call unit_check_done('dirname',msg='')
end subroutine test_dirname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_tmp()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('get_tmp',msg='')
   !!call unit_check('get_tmp', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('get_tmp',msg='')
end subroutine test_get_tmp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isdir()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('isdir',msg='')
   !!call unit_check('isdir', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('isdir',msg='')
end subroutine test_isdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notopen()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
integer :: i, ierr, ierr2

   call unit_check_start('notopen',msg='')
   call unit_check_msg('notopen','check for preassigned files from unit 0 to unit 1000')
   call unit_check_msg('notopen','assume 5 and 6 always return -1')

   do i=0,1000
      if(notopen(i,i,ierr) .ne. i)then
         call unit_check_msg('notopen','INUSE:',i,ierr, notopen(i,i,ierr2) )
      endif
   enddo
   call unit_check('notopen', notopen(5,6)           .eq. -1 ,msg=msg('preassigned'))

   do i=10,30,1
     open(unit=i,status="scratch")
   enddo

   close(25)
   close(28)
   call unit_check('notopen', notopen(10,30)           .eq. 25 ,msg=msg(''))
   call unit_check('notopen', notopen()                .eq. 25 ,msg=msg(''))
   call unit_check('notopen', notopen(start=12,end=30) .eq. 25 ,msg=msg(''))
   call unit_check('notopen', notopen(26)              .eq. 28 ,msg=msg(''))
   call unit_check('notopen', notopen(26,99)           .eq. 28 ,msg=msg(''))

   call unit_check_done('notopen',msg='')

end subroutine test_notopen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_inquire()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('print_inquire',msg='')
   !!call unit_check('print_inquire', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('print_inquire',msg='')
end subroutine test_print_inquire
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rd()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('rd',msg='')
   !!call unit_check('rd_character', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('rd',msg='')
end subroutine test_rd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_all()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('read_all',msg='')
   !!call unit_check('read_all', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('read_all',msg='')
end subroutine test_read_all
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_line()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('read_line',msg='')
   !!call unit_check('read_line', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('read_line',msg='')
end subroutine test_read_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_table()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('read_table',msg='')
   !!call unit_check('read_table', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('read_table',msg='')
end subroutine test_read_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_slurp()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('slurp',msg='')
   !!call unit_check('slurp', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('slurp',msg='')
end subroutine test_slurp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splitpath()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
integer,parameter      :: maxlen=4096
character(len=maxlen)  :: dir
character(len=maxlen)  :: name
character(len=maxlen)  :: basename
character(len=maxlen)  :: ext
   call unit_check_start('splitpath',msg='')
   call splitpath('/usr/local/bin/test.exe', dir, name, basename, ext)
   call unit_check('splitpath', dir=='/usr/local/bin/', msg=msg('directory'))
   call unit_check('splitpath', name=='test.exe', msg=msg('name'))
   call unit_check('splitpath', basename=='test', msg=msg('basename'))
   call unit_check('splitpath', ext=='ext', msg=msg('ext'))
   call unit_check_done('splitpath',msg='')
   call splitpath('/usr/local/bin/test.exe', dir=dir)
   call unit_check('splitpath', dir=='/usr/local/bin/', msg=msg('directory'))
   call splitpath('/usr/local/bin/test.exe', name=name)
   call unit_check('splitpath', name=='test.exe', msg=msg('name'))
   call splitpath('/usr/local/bin/test.exe', ext=ext)
   call unit_check('splitpath', ext=='test.exe', msg=msg('ext'))
   call splitpath('/usr/local/bin/test', basename=basename)
   call unit_check('splitpath', basename=='exe', msg=msg('basename'))
end subroutine test_splitpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniq()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('uniq',msg='')
   !!call unit_check('uniq', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('uniq',msg='')
end subroutine test_uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end subroutine test_suite_M_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
