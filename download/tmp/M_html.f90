!>
!!##NAME
!!    M_html(3fm) - [M_html] a module of routines to help write output as HTML documents
!!
!!##SYNOPSIS
!!
!!    use M_html, only : h_hopen, h_close
!!    use M_html, only : h_array
!!
!!##DESCRIPTION
!!     o m_array(lun,array)   Open HTML file and create simple header
!!     o m_open(lun)          Write array as table
!!     o m_close(lun)         Close HTML file
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
MODULE M_html
private
public h_open
public h_close
public h_array
public test_suite_M_html
CONTAINS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    h_array(3f) - [M_html] print a numeric array as an HTML table
!!
!!##SYNOPSIS
!!
!!   subroutine h_array(iounit,array)
!!
!!    integer,intent(in) :: iounit
!!    real,intent(in)    :: array(:,:)
!!
!!##DESCRIPTION
!!    Write an array as an HTML table
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_h_array
!!    use M_html
!!    implicit none
!!    real    :: arr(10,20)=0.0
!!    integer :: io=20
!!    integer :: i,j
!!    do i=1,10
!!       do j=1,20
!!          arr(i,j)=(i-1)*20+j
!!       enddo
!!    enddo
!!    call h_open(io,'table.html')
!!    call h_array(io,arr)
!!    call h_close(io)
!!
!!    end program demo_h_array
!===================================================================================================================================
subroutine h_array(iounit,array)
use M_journal, only : journal
implicit none

character(len=*),parameter::ident_1="@(#)M_html::h_array(3f):write table from array"

integer,intent(in) :: iounit
real,intent(in)    :: array(:,:)
integer            :: i
integer            :: j
   write(iounit,'(a)')'<table border="1">'
   do i=1,size(array,dim=1)
      write(iounit,*)'<tr>'
      do j=1,size(array,dim=2)
         write(iounit,*)'<td>',array(i,j),'</td>'
      enddo
      write(iounit,'(a)')'</tr>'
   enddo
   write(iounit,'(a)')'</table>'
end subroutine h_array
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    h_close(3f) - [M_html] close an HTML file
!!##SYNOPSIS
!!
!!   subroutine h_close(iounit)
!!
!!    integer,intent(in) :: iounit
!!
!!##DESCRIPTION
!!    Open an HTML file
!!
!!##OPTIONS
!!    lun       The unit number to close
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_h_close
!!    use M_html
!!    implicit none
!!    real    :: arr(10,20)=0.0
!!    integer :: io=20
!!    integer :: i,j
!!    do i=1,10
!!       do j=1,20
!!          arr(i,j)=(i-1)*20+j
!!       enddo
!!    enddo
!!    call h_open(io,'table.html')
!!    call h_array(io,arr)
!!    call h_close(io)
!!
!!    end program demo_h_close
!===================================================================================================================================
subroutine h_close(iounit)
use M_journal, only : journal
implicit none

character(len=*),parameter::ident_2="@(#)M_html::h_close(3f):close HTML file"

integer,intent(in) :: iounit
   write(iounit,*)'</body>'
   write(iounit,*)'</html>'
   close(unit=iounit)
end subroutine h_close
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    h_open(3f) - [M_html] close an HTML file
!!##SYNOPSIS
!!
!!   subroutine h_open(iounit)
!!
!!    integer,intent(in) :: iounit
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!    Open  an HTML output file
!!
!!##OPTIONS
!!    lun       The unit number to open
!!    filename  Name of the file to open
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_h_open
!!    use M_html
!!    implicit none
!!    real    :: arr(10,20)=0.0
!!    integer :: io=20
!!    integer :: i,j
!!    do i=1,10
!!       do j=1,20
!!          arr(i,j)=(i-1)*20+j
!!       enddo
!!    enddo
!!    call h_open(io,'table.html')
!!    call h_array(io,arr)
!!    call h_close(io)
!!
!!    end program demo_h_open
!===================================================================================================================================
subroutine h_open(iounit,filename)
use M_journal, only : journal
implicit none

character(len=*),parameter::ident_3="@(#)M_html::h_open(3f):open HTML file"

character(len=*),intent(in) :: filename
integer,intent(in)          :: iounit
end subroutine h_open
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_suite_M_html()

!! setup
   call test_h_array()
   call test_h_close()
   call test_h_open()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_array()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('h_array',msg='')
   !!call unit_check('h_array', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('h_array',msg='')
end subroutine test_h_array
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_close()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('h_close',msg='')
   !!call unit_check('h_close', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('h_close',msg='')
end subroutine test_h_close
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_open()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('h_open',msg='')
   !!call unit_check('h_open', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('h_open',msg='')
end subroutine test_h_open
!===================================================================================================================================
end subroutine test_suite_M_html
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE M_html
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
