!-----------------------------------------------------------------------------------------------------------------------------------
program test_substitute
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only : substitute
implicit none
character(len=:),allocatable    :: targetline   ! input line to be changed
character(len=:),allocatable    :: old          ! old substring to replace
character(len=:),allocatable    :: new          ! new substring
integer                         :: ml           ! ml sets the left  margin
integer                         :: mr           ! mr sets the right margin
integer                         :: ier          ! error code. if ier = -1 bad directive, >= 0then ier changes made
!-----------------------------------------------------------------------------------------------------------------------------------
   targetline='This an that and any other '
   old='an'
   new='##'
   ml=1
   mr=len(targetline)
   write(*,*)'ORIGINAL: '//targetline
   call substitute(targetline,old,new,ier,ml,mr) !Globally substitute one substring for another in string
   write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ier.ne.3)then
      write(*,*)ier,targetline
      call unit_check_bad('substitute')
      stop 1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(targetline.ne.'This ## that ##d ##y other ')then
      call unit_check_bad('substitute')
      stop 2
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   targetline='This and that, This and that,                               '
   write(*,*)'ORIGINAL: '//targetline

   old=''
   new='BEGINNING: '
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline

   old='This'
   new='THIS'
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline

   old='that'
   new='LONGER STRING'
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline

   old='LONGER STRING'
   new=''
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline

   if ( targetline .ne. 'BEGINNING: THIS and , THIS and ,')then
      call unit_check_bad('substitute')
      stop 3
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_good('substitute')
!-----------------------------------------------------------------------------------------------------------------------------------
end program test_substitute
!-----------------------------------------------------------------------------------------------------------------------------------
