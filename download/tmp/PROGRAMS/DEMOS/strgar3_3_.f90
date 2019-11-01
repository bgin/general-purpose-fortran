             program demo_strgar3
             use M_hybrid,     only : strgar3
             character(len=90) :: string
             real              :: values(10,4)
             rdum1=rnum0('ownmode(1)') ! allow user-define procedure calls from juown1
             do
                values(:,:)=-123
                write(*,*)'*strgar3* Enter string like 10:1 20 30 40:50'
                read(*,'(a)',iostat=ios)string
                if(ios.ne.0)stop
                call strgar3(string,10,-1.0,values,inums,' ',' ',ierr)
                write(*,*)'inums=',inums
                write(*,*)'ierr=',ierr
                write(*,*)'values(:,1)=',values(:inums,1)
                write(*,*)'values(:,2)=',values(:inums,2)
                write(*,*)'values(:,3)=',values(:inums,3)
                write(*,*)'values(:,4)=',values(:inums,4)
             enddo
             end

             subroutine juown1(func,iflen,args,iargstp,n,x,y,fval,ctmp,ier) ! extend functions available to the calculator routine
             ! if the function owncode(1) is called this subroutine can be accessed to do user-written functions.
             use M_journal, only : journal
             use m_calculator, only : x, y
             integer,parameter :: dp=kind(0.0d0)
             character(len=*)  :: func
             integer           :: iflen
             real(kind=dp)     :: args(100)
             integer           :: iargstp(100)
             integer           :: n
             real(kind=dp)     :: fval
             character(len=*)  :: ctmp
             integer           :: ier
             integer           :: i10
             character(len=80) :: temp1
             fval=0
             select case (func)
             case('e')
                fval=errc(args(1),args(2),args(3))
             case default
                fval=errc(args(1),args(2),args(3))
                call journal('sc', '*juown1* unknown function')
                temp1='function name is ........'//func(1:iflen) ! some machines cannot concatenate a string being passed as an argument
                call journal('sc',temp1)
                call journal('sc','function name length is..',iflen)
                call journal('sc','number of arguments .....',n)
                write(*,*)(args(i10),i10=1,n,1)
             end select
             end subroutine juown1
             !end program demo_strgar3
