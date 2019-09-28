module M_hybrid
private
public fetch             ! ftch2
public strgar3
public errc
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    strgar3(3f) - [M_hybrid] fetch a value from language dictionary but process via calculator too
!!
!!##SYNOPSIS
!!
!!   subroutine strgar3(line,iread,default,numbrs,inums,delims,delimc,ierr)
!!
!!    character(len=*),intent(in)  :: line
!!    integer,intent(in)           :: iread
!!    real,intent(in)              :: default
!!    real,intent(out)             :: numbrs(iread,4)
!!    integer,intent(out)          :: inums
!!    character(len=1),intent(in)  :: delims
!!    character(len=1),intent(in)  :: delimc
!!    integer,intent(out)          :: ierr
!!
!!##DESCRIPTION
!! This is a special-purpose clone of strgar2 that expects each string to return
!! two values with a : character splitting the values. If the second string does
!! not exist, the current file value is unchanged. For example, the string
!!
!!     "10:20 30 40 50:60" with a default of -1 would return
!!     +--+--+--+--+
!!     |10|30|40|50| curve numbers (or curve ranges)
!!     +--+--+--+--+
!!     |20|-1|-1|60| file numbers
!!     +--+--+--+--+
!!     |-1|-1|-1|-1| top error curve numbers possibly set by e()
!!     +--+--+--+--+
!!     |-1|-1|-1|-1| bottom error curve numbers possibly set by e()
!!     +--+--+--+--+
!!
!!
!! The number on the left of the colon is assumed to exist. This if for the
!! purpose of letting USH use the syntax curvenumber:filename and has no other
!! purpose. It is not really part of the calculator.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!       program demo_strgar3
!!       use M_hybrid,     only : strgar3
!!       use M_calculator, only : juown1
!!       character(len=90) :: string
!!       real              :: values(10,4)
!!       rdum1=rnum0('ownmode(1)') ! allow user-define procedure calls from juown1
!!       do
!!          values(:,:)=-123
!!          write(*,*)'*strgar3* Enter string like 10:1 20 30 40:50'
!!          read(*,'(a)',iostat=ios)string
!!          if(ios.ne.0)stop
!!          call strgar3(string,10,-1.0,values,inums,' ',' ',ierr)
!!          write(*,*)'inums=',inums
!!          write(*,*)'ierr=',ierr
!!          write(*,*)'values(:,1)=',values(:inums,1)
!!          write(*,*)'values(:,2)=',values(:inums,2)
!!          write(*,*)'values(:,3)=',values(:inums,3)
!!          write(*,*)'values(:,4)=',values(:inums,4)
!!       enddo
!!       end
!!
!!       subroutine juown1(func,iflen,args,iargstp,n,x,y,fval,ctmp,ier) ! extend functions available to the calculator routine
!!       ! if the function owncode(1) is called this subroutine can be accessed to do user-written functions.
!!       use M_journal, only : journal
!!       use m_calculator, only : x, y
!!       integer,parameter :: dp=kind(0.0d0)
!!       character(len=*)  :: func
!!       integer           :: iflen
!!       real(kind=dp)     :: args(100)
!!       integer           :: iargstp(100)
!!       integer           :: n
!!       real(kind=dp)     :: fval
!!       character(len=*)  :: ctmp
!!       integer           :: ier
!!       integer           :: i10
!!       character(len=80) :: temp1
!!       fval=0
!!       select case (func)
!!       case('e')
!!          fval=errc(args(1),args(2),args(3))
!!       case default
!!          fval=errc(args(1),args(2),args(3))
!!          call journal('sc', '*juown1* unknown function')
!!          temp1='function name is ........'//func(1:iflen) ! some machines cannot concatenate a string being passed as an argumen
!!          call journal('sc',temp1)
!!          call journal('sc','function name length is..',iflen)
!!          call journal('sc','number of arguments .....',n)
!!          write(*,*)(args(i10),i10=1,n,1)
!!       end select
!!       end subroutine juown1
!!       !end program demo_strgar3
!===================================================================================================================================
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine strgar3(line,iread,default,numbrs,inums,delims,delimc,ierr)
!  read a string into an array USING CALCULATOR and passing "
!  1989 John S. Urban
!  given a line of structure , string , string , string process each
!  string and store into an array. delimc and delims are only legal
!  delimiters. no checking for more than can fit in numbrs.
!  quits if encounters any errors in read.

   use M_journal,    only : journal
   use M_calculator, only : iclen_calc
   use M_calculator, only : jucalcx
   implicit none
   character(len=*),parameter :: ident="@(#)strgar3(3f):read a string into an array USING CALCULATOR passing double-quoted strings"
!===================================================================================================================================
   character(len=*),intent(in)  :: line              ! input string
   integer,intent(in)           :: iread             ! maximum number of values to try to read into numbrs
   real,intent(in)              :: default
   real,intent(out)             :: numbrs(iread,4)   ! real array to be filled with values
                                                     ! curve, file, top err, bottom err
   integer,intent(out)          :: inums             ! number of values read (before error occurs if one does)
   character(len=1),intent(in)  :: delims
   character(len=1),intent(in)  :: delimc
   integer,intent(out)          :: ierr              ! ierr==0 if no error, else column number error string starts at
!===================================================================================================================================
   character(len=1)             :: ch
   integer                      :: i10,i20           ! loop counters
   integer                      :: iend
   integer                      :: iend1
   integer                      :: ier               ! error flag returned by jucalcx(3f) to be examined
   integer                      :: ierrcurves(3)
   integer                      :: ii
   integer                      :: ilen
   integer                      :: ilendm            ! dummy parameter for use on jucalcx(3f) calls
   integer,parameter            :: inoerrcurves=0
   integer                      :: instring
   integer                      :: iprev
   integer                      :: istart
   integer                      :: istart2
   integer                      :: istarto
   integer                      :: itwasd
   character(len=iclen_calc)    :: outlin
   doubleprecision              :: dval
   doubleprecision              :: dval2
   common /errq/ierrcurves
   save /errq/
!===================================================================================================================================
   ierr=0                                                            ! return error flag (value defines where error occurs)
   ier=0                                                             ! error flag returned by jucalcx(3f) calls
   inums=0                                                           ! initialize number of values read
!===================================================================================================================================
   if(delims.eq.'"'.or.delimc.eq.'"')then                            ! MAKING THE ASSUMPTION THAT " IS NOT A DELIMITER
      call journal('sc','*strgar3* bad delimiter " requested')
      return
   endif
!===================================================================================================================================
   ilen=0                                                            ! find position of last non-delimiter
   do i20=len(line),1,-1                                             ! start at right and work back till find a non-delimiter
      if(line(i20:i20).ne.delims.and.line(i20:i20).ne.delimc)then    ! exit loop when a non-delimiter is found
         ilen=i20
         exit
      endif
   enddo
   if(ilen.eq.0)then                                                 !  command was totally composed of delimiters
!     call journal('sc','*strgar3* blank line passed as a list of numbers')
      return
   endif
!===================================================================================================================================
   numbrs(:,3)=inoerrcurves    ! set list of curve numbers for error bar top to special value meaning no error bar
   numbrs(:,4)=inoerrcurves    ! set list of curve numbers for error bar bottom to special value meaning no error bar
!===================================================================================================================================
!  there is at least one non-delimiter sub-string.  ilen is now the column position of the last non-blank character
   iprev=-1
   itwasd=0                    ! flag when previous character was a delimiter not in a quoted region
   instring=1                  ! flag that not inside a quoted string
   istart=1
   istarto=1
   iend=1
!  find next non-delimiter
   do i10=1,ilen+1
      if(i10.eq.ilen+1)then    ! finishing string
         ch=delims
         if(instring.eq.0)then ! if unmatched quote encountered put the unclaimed characters into one last parameter
            istart=istarto
            goto 999
         endif
      else                     ! regular string
         ch=line(i10:i10)
      endif
      if(ch.eq.'"'.and.instring.eq.1)then         ! starting quote
         istarto=istart
         if(iprev.ne.i10-1.and.itwasd.eq.0)then
            istart=i10                            ! start new string
            iend=i10-1                            ! in case this string is not ended, do not ignore what went before
         endif
         instring=0                               ! flag that in a string
      elseif(ch.eq.'"')then                       ! closing quote or internal quote
         instring=1
         iprev=i10
      elseif(instring.eq.1.and.(ch.eq.delims.or.ch.eq.delimc))then ! delimiter not in a quoted string , last char is always delims
         iend=i10-1
         if(iend-istart.ge.0)then
            if(line(istart:iend).eq.'*')then
               dval=-99999.0d0
            else
               CALL COMMON()
            endif
            if(ier.eq.0)then ! returned number
              inums=inums+1
              if(inums.gt.iread)then
                 call journal('sc','*strgar3* max parameters allowed is ',iread)
                 return
              endif
              numbrs(inums,1)=dval
              numbrs(inums,2)=dval2
              numbrs(inums,3)=ierrcurves(2)
              numbrs(inums,4)=ierrcurves(3)
              !----------------------------------
            elseif(ier.eq.2)then
              !call journal('sc','*strgar3* could not turn string into number')
              !call journal('sc',line(istart:iend)
            else
              call journal('sc','*strgar3* error is ',ier)
              ierr=istart
              return  ! keep going to others or not?
            endif
         endif
         istart=i10+1    ! start new string
         itwasd=0
      else
         iend=i10
         itwasd=1
      endif
   enddo
   return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
         if(iend-istart.ge.0)then   ! process what is left over when an unmatched parenthesis is encountered
            if(line(istart:iend).eq.'*')then
               dval=-99999.0d0
            else
               CALL COMMON()
            endif
            if(ier.eq.0)then
              inums=inums+1
              if(inums.gt.iread)then
                 call journal('sc','*strgar3* max parameters allowed is ',iread)
                 return
              endif
              numbrs(inums,1)=dval
              numbrs(inums,2)=dval2
              numbrs(inums,3)=ierrcurves(2)
              numbrs(inums,4)=ierrcurves(3)
              !--------------------
            elseif(ier.eq.2)then
              !call journal('sc','*strgar3* could not turn string into number')
              !call journal('sc',line(istart:iend)
            else
              call journal('sc','*strgar3* error is ',ier)
              ierr=istart
              return  ! keep going to others or not?
            endif
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
         subroutine COMMON()
               ii=index(line(istart:iend),':')
               dval2=default                                                      ! second value is default
               ierrcurves(:)=inoerrcurves                                         ! clear values that might be set by e() function
               if(ii.eq.0)then                                                    ! if no colon
                  call jucalcx(line(istart:iend),dval,outlin,ier,ilendm)          ! curve number or e(top,curve,bottom)
               else                                                               ! there is a colon
                  iend1=istart+ii-2                                               ! find end of first string
                  if(iend1-istart+1.le.0)then                                     ! if no first number
                     dval=0.0d0                                                   ! ERROR
                     call journal('sc','*strgar3* missing curve number')
                  else                                                            ! get first value from string up to colon
                     call jucalcx(line(istart:iend1),dval,outlin,ier,ilendm)      ! get curve number or e(top,curve,bottom)
                  endif
                                                                                  ! get second number after colon (file number)
                  istart2=istart+ii
                  if(iend.ge.istart2)then
                     call jucalcx(line(istart2:iend),dval2,outlin,ier,ilendm)     ! get file number
                  else                                                            ! nothing after the colon
                     dval2=default                                                ! file number is set to default
                  endif
               endif
end subroutine COMMON
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine strgar3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function errc(curve,top,bottom)
implicit none
character(len=*),parameter  :: ident="@(#)errc(3f): function for specifying error curve numbers"
integer,parameter           :: dp=kind(0.0d0)
   real(kind=dp),intent(in) ::  curve, top, bottom
   integer                  :: ierrcurves(3)
   common /errq/ierrcurves
   save /errq/
   ierrcurves(1)=int(curve)
   ierrcurves(2)=int(top)
   ierrcurves(3)=int(bottom)
   errc=curve
end function errc
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    fetch(3f) - [M_hybrid] call sget(3f) (and calculator if fetched string starts with $ or double-quote
!!
!!##SYNOPSIS
!!
!!   function fetch(dicname)
!!
!!    character(len=:),allocatable  :: fetch
!!    character, intent=(in)        :: dicname*(*)
!!
!!##DESCRIPTION
!!    fetch() is a special version of sget(3f) that integrates the
!!    Calculator library with the Language library.
!!
!!    o sget(3f) is called to look up the value of the specified name in the
!!      language dictionary.
!!
!!    o If the returned value is a string expression (ie. starts with a $ or "
!!      character) then evaluate the expression using the calculator library
!!      and return the calculated value.
!!
!!    o If the name does not appear in the dictionary a blank string is
!!      returned.
!!
!!    If the evaluation works return the expanded expression's value
!!    else return a blank string (could return the expression itself).
!!
!!##OPTIONS
!!    DICNAME is the name of a variable stored in the language dictionary.
!!
!!##DEPENDENCIES
!!       o sget
!!       o jucalcx
!!
!!##EXAMPLES
!!
!!
!!   Sample program
!!
!!       program T_fetch
!!       use M_kracken, only : store
!!       use M_hybrid,  only : fetch
!!       call store('VERB_KEYWORD','This is a value','define',ierr)
!!       write(*,*)'VALUE is ',fetch('VERB_KEYWORD')
!!       end program T_fetch
!!
!!##SEE ALSO
!!     see sget(3f).
!!##REFERENCES
!!    + NONE.
!!##AUTHOR
!!    + John S. Urban, 1994
!===================================================================================================================================
function fetch(dicname)
! 1994 John S. Urban
use M_kracken,         only : sget
use M_calculator,      only : iclen_calc
use M_calculator,      only : jucalcx
use M_journal,         only : journal
implicit none

character(len=*),parameter::ident_1="&
&@(#)M_xyplot::fetch(3f): call sget(3f) (and calculator if fetched string starts with $ or double-quote)"

character(len=:),allocatable :: fetch
character(len=*),intent(in)  :: dicname
character(len=iclen_calc)    :: outlin0
doubleprecision              :: dvalue2
integer                      :: ierr
integer                      :: ilen
!-----------------------------------------------------------------------------------------------------------------------------------
   fetch=sget(dicname)                                         ! get string value from dictionary
   if(len(fetch).eq.0)then
      call journal('sc','*fetch* name [',trim(dicname),'] not found')
      fetch=' '
   else
      fetch=fetch//' '                                         ! make sure at least one character long for substring tests
      if(fetch(1:1).eq.'$'.or.fetch(1:1).eq.'"')then           ! get string value from calculator
         ierr=0
         call jucalcx(fetch,dvalue2,outlin0,ierr,ilen)         ! convert a calculator expression into a numeric value
         if(ierr.eq.2) then                                    ! if a string was successfully returned set fetch to it
            fetch=outlin0
         else
            fetch=' '                                          ! somewhat arbitrary as to leave the expression or return a blank
         endif
      endif
      fetch=trim(fetch)
   endif
end function fetch
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_hybrid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
