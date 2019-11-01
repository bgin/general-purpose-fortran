          !program demo_M_calculator
          program show_M_calculator

             !     line mode calculator that calls jucalc
             !
             use M_calculator, only: jucalc,iclen_calc
             use M_calculator, only : rnum0
             implicit none
             integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
             character(len=iclen_calc) :: event, line
             character(len=iclen_calc) :: outlin
             integer                   :: ios
             integer                   :: ierr
             real(kind=k_dbl)          :: rvalue
             character(len=80)         :: string
             INFINITE: do
                read(*,'(a)',iostat=ios)line
                if(ios.ne.0)exit INFINITE
                call jucalc(line,outlin,event,rvalue,ierr)
                ! line   -- input expression
                ! outlin -- result as a string
                ! event  --
                ! rvalue -- result as a numeric value
                ! ierr   -- return status
                !
                ! several different meaning to the status flag ...
                select case(ierr)
                case(0)  ! a numeric value was returned without error
                  write(6,'(a,a,a)')trim(outlin),' = ',trim(line)
                case(2)  ! a string value was returned without error
                  write(6,'(a)')trim(event)
                case(1)  ! a request for a message has been returned
                         ! (from DUMP or FUNC)
                  write(6,'(a,a)')'message===>',trim(event)
                case(-1) ! an error has occurred
                  write(6,'(a,a)')'error===>',trim(event)
                case default ! this should not occur
                  write(6,'(a)')'warning===> unexpected ierr value from jucalc'
                end select
             enddo INFINITE

             string='A=sind(30)'
             rvalue=rnum0(string,ierr)
             if(ierr.eq.0)then
                write(*,*) rvalue
             else
                write(*,*) 'error evaluating '//trim(string)
             endif
             rvalue=rnum0('A',ierr)
             write(*,*) rvalue

             end program show_M_calculator
             !#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
             subroutine juown1(func,iflen,args,iargstp,n,fval,ctmp,ier)
             ! extend functions available to the calculator routine
             !
             !  if the function ownmode(1) is called this subroutine
             !  will be accessed to do user-written functions.
             !
             !  func(iend-1)=procedure name.  func should not be changed.
             !  iflen=length of procedure name.
             !  args=array of 100 elements containing procedure arguments.
             !  iargstp=type of argument(1=value,2=position of string value)
             !  n=integer number of parameters
             !  x=array of 55555 x values
             !  y=array of 55555 y values
             !  fval=value to replace function call
             !  ctmp=string to return when returning a string value
             !  ier=returned error flag value.
             !      set to -1 if an error occurs.
             !      set to  0 if a number is returned
             !      set to  2 if a string is returned
             !
             use M_calculator, only: x,y,values,values_len,iclen_calc
             ! values: the values of string variables
             ! values_len: the lengths of the string variable values
             character(len=*)          :: func

             character(len=*)          :: ctmp
             character(len=iclen_calc) :: temp1
             integer iflen ,n, ier, iargstp(100)
             integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
             real(kind=k_dbl)  :: args(100)
             real(kind=k_dbl)  :: fval
             integer           :: iwhich
                fval=0.0d0
             !-----------------------------------------------------------------------
                write(*,*)'*juown1* unknown function ', func(1:iflen)
                write(*,*)'function name length is..',iflen
                write(*,*)'number of arguments .....',n
                do i10=1,n
                   if(iargstp(i10).eq.0)then
                      write(*,*)i10,' VALUE=',args(i10)
                   elseif(iargstp(i10).eq.2)then
                      iwhich=int(args(i10))
                      ilen=values_len(iwhich)
                      write(*,*)i10,' STRING='//values(iwhich)(:ilen)
                   else
                      write(*,*)'unknown parameter type is ',iargstp(i10)
                   endif
                enddo
             end subroutine juown1
             !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
             real function c(fval,n)
             implicit none
             !  a built-in calculator function called c must be satisfied.
             !  write whatever you want here as a function
             integer, parameter          :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
             real(kind=k_dbl),intent(in) :: fval
             integer,intent(in)          :: n
                c=0.0
             end function c
             !end program demo_M_calculator
