!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program testit
!(LICENSE:MIT)
use M_draw
use M_drawplus, only : page
implicit none
real :: CPX
real :: CPY
integer :: sig(26)
integer :: ORDINAL
integer :: i

integer :: background=D_GREEN
integer :: text=D_BLACK
integer :: outline=D_BLACK

character(len=1) :: let

   call voutput("signal_flags.out")
   call prefsize(500,500)
   call vinit("")
   call polyfill(.true.)
   call page( -0.05, 1.05, -0.05, 1.05)
   call color(-1)                        ! line thickness
   call move2(0.0,0.0)
   call getgp2(CPX,CPY)
   call translate(CPX,CPY,0.0)
   do i=1,26
      let=char(i+ichar('A')-1)
      write(*,*) i,1000+i,let
      call makeobj(1000+i)
      call maritime_signal(let)
      call closeobj()
      sig(i)=1000+i
   enddo
   call each_signal(sig)
   call color(BACKGROUND)
   call clear()
   call color(TEXT)
   call all_signals(sig)
   call vexit()
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine each_signal(sig)
integer :: sig(26)
   integer :: i
   do i=1,26
      call color(BACKGROUND)
      call clear()
      call callobj(sig(i))
      call vflush()
      ordinal=getkey()
   enddo
end subroutine each_signal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine all_signals(sig)
   integer :: sig(26)
   character(len=10) :: alf(26)
   real :: W, W2, SCR
   real :: A
   real :: B
   real :: CPX
   real :: CPY
   integer :: i,j
   integer :: NAME
   call color(-2);  ! line thickness
   W=(10.0-7.0)/8.0;
   W2=(10.0-2.0*W)/4.0;
   SCR=(W2-1.0)/12.0;
   alf(01)="ALPHA    "
   alf(02)="BRAVO    "
   alf(03)="CHARLIE  "
   alf(04)="DELTA    "
   alf(05)="ECHO     "
   alf(06)="FOXTROT  "
   alf(07)="GOLF     "
   alf(08)="HOTEL    "
   alf(09)="INDIA    "
   alf(10)="JULIET   "
   alf(11)="KILO     "
   alf(12)="LIMA     "
   alf(13)="MIKE     "
   alf(14)="NOVEMBER "
   alf(15)="OSCAR    "
   alf(16)="PAPA     "
   alf(17)="QUEBEC   "
   alf(18)="ROMEO    "
   alf(19)="SIERRA   "
   alf(20)="TANGO    "
   alf(21)="UNIFORM  "
   alf(22)="VICTOR   "
   alf(23)="WHISKEY  "
   alf(24)="XRAY     "
   alf(25)="YANKEE   "
   alf(26)="ZULU     "
   call page(0.0,10.0,0.0,10.0)
   call textsize(SCR*2, 7.0/5.0*SCR*2)
   NAME=1

   do i=0,3
      A= W+i*W2
      do j=6,0,-1
         B=W+(1+W)*j
         if( NAME <= 26 )then
            call move2(A,B)
            call pushmatrix()
            call getgp2(CPX,CPY)
            call translate(CPX,CPY,0.0)
            call callobj(sig(NAME))
            call popmatrix()
            call move2(A+1.1,B+0.5)
            call color(TEXT)
            call drawstr(alf(NAME))
            write(*,*)alf(NAME)
         endif
         NAME=NAME+1
         call vflush()
      enddo
   enddo
   write(*,*)'done'
   call move2( 0.1, 0.05)
   call drawstr("International Code of Signals and NATO Phonetic Alphabet,  John S. Urban, 19940927")
   ordinal=getkey()
end subroutine all_signals
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! ######################################################################========
subroutine box_outline(OUTLINE)
   integer :: OUTLINE
   call color(OUTLINE)
   call move2(0.0,0.0)
   call draw2(0.0,1.0)
   call draw2(1.0,1.0)
   call draw2(1.0,0.0)
   call draw2(0.0,0.0)
   return
end subroutine box_outline
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! ######################################################################========
subroutine maritime_signal(char)
   character(len=1),intent(in) :: char
   real :: W
   real :: CORNER
   real :: BOXX
   real :: BOXY
! ######################################################################========
   BOXX=0.0
   BOXY=0.0
! ######################################################################========
   select case(char)
! ######################################################################========
    case('A') ! ALPHA
      call color( D_WHITE)
      call makepoly()
      call rect(0.0,0.0,0.5,1.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 00.5,  0.0)
      call draw2( 00.5,  1.0)
      call draw2( 1.0,  1.0)
      call draw2( 1.0-.25, 00.5)
      call draw2( 1.0,  0.0)
      call draw2( 00.5,   0.0)
      call closepoly()
      call color( OUTLINE)
      call move2( 0.0,  1.0)
      call draw2( 1.0,  1.0)
      call draw2( 1-.25, 0.5)
      call draw2( 1.0,  0.0)
      call draw2( 0.0,  0.0)
      call draw2( 0.0,  1.0)
      ! ################################################################################
    case('B') ! BRAVO
      call color( D_RED)
      call makepoly()
      call move2( 0.0, 0.0)
      call draw2( 0.0, 1.0)
      call draw2( 1.0, 1.0)
      call draw2( 1-.25, 0.5)
      call draw2( 1.0, 0.0)
      call draw2( 0.0, 0.0)
      call closepoly()
      call color( OUTLINE)
      call move2( 0.0, 0.0)
      call draw2( 0.0, 1.0)
      call draw2( 1.0, 1.0)
      call draw2( 1-.25, 0.5)
      call draw2( 1.0, 0.0)
      call draw2( 0.0, 0.0)
      ! ################################################################################
      case('C') ! CHARLIE
      call color( D_BLUE)
      call makepoly()
      call rect(0.0, 0.0, 1.0, 0.25)
      call closepoly()
      call makepoly()
      call rect(0.0, 0.75, 1.0, 1.0)
      call closepoly()
      call color( D_WHITE)
      call makepoly()
      call rect(0.0, 0.25, 1.0, 0.25+0.50/3.0)
      call closepoly()
      call makepoly()
      call rect(0.0, 0.25+2*0.50/3.0, 1.0, 0.75)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call rect(0.0, 0.25+0.50/3.0, 1.0, 0.25+2*.50/3.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('D') !     DELTA
      call color( D_YELLOW)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 0.25)
      call closepoly()
      call makepoly()
      call move2(0.0, 0.75)
      call getgp2(BOXX, BOXY)
      call rect( BOXX,BOXY, 1.0, 1.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2(0.0, 0.25)
      call getgp2( BOXX, BOXY)
      call rect( BOXX, BOXY, 1.0, 0.75)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('E') !     ECHO
      call color( D_RED)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 0.50)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 0.0, 00.5)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('F') !     FOXTROT
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0, 0.0)
      call draw2( 0.0, 00.5)
      call draw2( 00.5, 0.0)
      call draw2( 0.0, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1.0, 0.0)
      call draw2( 00.5, 0.0)
      call draw2( 1.0, 00.5)
      call draw2( 1.0, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1.0, 1.0)
      call draw2( 1.0, 00.5)
      call draw2( 00.5, 1.0)
      call draw2( 1.0, 1.0)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1.0)
      call draw2( 00.5, 1.0)
      call draw2( 0.0, 00.5)
      call draw2( 0.0, 1.0)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call move2( 00.5, 0.0)
      call draw2( 1.0, 00.5)
      call draw2( 00.5, 1.0)
      call draw2( 0.0, 00.5)
      call draw2( 00.5, 0.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('G') !     GOLF
      call color( D_YELLOW)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0/6, 1.0)
      call closepoly()
      call makepoly()
      call move2( 2.0/6, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  3.0/6, 1.0)
      call closepoly()
      call makepoly()
      call move2( 4.0/6, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  5.0/6, 1.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 1.0/6, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  2.0/6, 1.0)
      call closepoly()
      call makepoly()
      call move2( 3.0/6, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  4.0/6, 1.0)
      call closepoly()
      call makepoly()
      call move2( 5.0/6, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  6.0/6, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('H') !     HOTEL
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5, 1.0)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call move2( 0.5, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('I') !     INDIA
      call color( D_YELLOW)
      call makepoly()
      call rect( 0.0, 0.0,  1.0, 1.0)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call circle( 0.5, 0.5, 0.25/2.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('J') !     JULIET
      call color( D_BLUE)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0/3)
      call closepoly()
      call makepoly()
      call move2( 0.0, 2.0/3)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0)
      call closepoly()
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0, 1.0/3)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 2.0/3)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('K') !     KILO
      call color( D_YELLOW)
      call makepoly()
      call rect( 0.0, 0.0,  0.5, 1.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call rect( 0.5, 0.0, 1.0, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('L') !     LIMA
      call color( D_BLACK)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  00.5, 0.5)
      call closepoly()
      call makepoly()
      call move2( 00.5, 0.5)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0)
      call closepoly()
      call color( D_YELLOW)
      call makepoly()
      call move2( 1.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  00.5, 0.5)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  00.5, 0.5)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('M') !     MIKE
      W=1.0/6.0
      CORNER=(W)*(1.0/sqrt(2.0))
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0, 0.0)
      call draw2( CORNER, 0.0)
      call draw2( 00.5, 0.5-CORNER)
      call draw2( 1-CORNER, 0.0)
      call draw2( 1.0, 0.0)
      call draw2( 1.0, CORNER)
      call draw2( 00.5+CORNER, 0.5)
      call draw2( 1.0, 1-CORNER)
      call draw2( 1.0, 1.0)
      call draw2( 1-CORNER, 1.0)
      call draw2( 00.5, 0.5+CORNER)
      call draw2( 0+CORNER, 1.0)
      call draw2( 0.0, 1.0)
      call draw2( 0.0, 1-CORNER)
      call draw2( 00.5-CORNER, 0.5)
      call draw2( 0.0, CORNER)
      call draw2( 0.0,0.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 0.0, CORNER)
      call draw2( 00.5-CORNER, 0.5)
      call draw2( 0.0, 1-CORNER)
      call draw2( 0.0, CORNER)
      call closepoly()
      call makepoly()
      call move2( CORNER, 0.0)
      call draw2( 1-CORNER, 0.0)
      call draw2( 00.5, 0.5-CORNER)
      call draw2( CORNER, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1.0, CORNER)
      call draw2( 00.5+CORNER, 0.5)
      call draw2( 1.0, 1-CORNER)
      call draw2( 1.0, CORNER)
      call closepoly()
      call makepoly()
      call move2( 00.5, 0.5+CORNER)
      call draw2( CORNER, 1.0)
      call draw2( 1-CORNER, 1.0)
      call draw2( 00.5, 0.5+CORNER)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('N') !     NOVEMBER
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0/4, 0.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0/4, 1.0/4)
      call closepoly()
      call makepoly()
      call move2( 0.0/4, 2.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0/4, 3.0/4)
      call closepoly()
      call makepoly()
      call move2( 1.0/4, 1.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  2.0/4, 2.0/4)
      call closepoly()
      call makepoly()
      call move2( 1.0/4, 3.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  2.0/4, 4.0/4)
      call closepoly()
      call makepoly()
      call move2( 2.0/4, 0.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  3.0/4, 1.0/4)
      call closepoly()
      call makepoly()
      call move2( 2.0/4, 2.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  3.0/4, 3.0/4)
      call closepoly()
      call makepoly()
      call move2( 3.0/4, 1.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  4.0/4, 2.0/4)
      call closepoly()
      call makepoly()
      call move2( 3.0/4, 3.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  4.0/4, 4.0/4)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 0.0/4, 1.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0/4, 2.0/4)
      call closepoly()
      call makepoly()
      call move2( 0.0/4, 3.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0/4, 4.0/4)
      call closepoly()
      call makepoly()
      call move2( 1.0/4, 0.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  2.0/4, 1.0/4)
      call closepoly()
      call makepoly()
      call move2( 1.0/4, 2.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  2.0/4, 3.0/4)
      call closepoly()
      call makepoly()
      call move2( 2.0/4, 1.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  3.0/4, 2.0/4)
      call closepoly()
      call makepoly()
      call move2( 2.0/4, 3.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  3.0/4, 4.0/4)
      call closepoly()
      call makepoly()
      call move2( 3.0/4, 0.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  4.0/4, 1.0/4)
      call closepoly()
      call makepoly()
      call move2( 3.0/4, 2.0/4)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  4.0/4, 3.0/4)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('O') !     OSCAR
      call color( D_YELLOW)
      call makepoly()
      call move2( 0.0, 0.0)
      call draw2( 1.0, 0.0)
      call draw2( 0.0, 1.0)
      call draw2( 0.0, 0.0)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call move2( 1.0, 0.0)
      call draw2( 1.0, 1.0)
      call draw2( 0.0, 1.0)
      call draw2( 1.0, 0.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('P') !     PAPA
      call move2( 0.0, 0.0)
      call color( D_BLUE)
      call makepoly()
      call rect( 0.0, 0.0,  1.0, 1.0)
      call closepoly()
      call color( D_WHITE)
      call makepoly()
      call rect( 1.0/3, 1.0/3,  2.0/3, 2.0/3)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('Q') !  QUEBEC
      call color( D_YELLOW)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('R') !     ROMEO
      W=1.0/12
      call color( D_RED)
      call makepoly()
      call move2( 0.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5-W, 0.5-W)
      call closepoly()
      call makepoly()
      call move2( 1.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5+W, 0.5-W)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5-W, 0.5+W)
      call closepoly()
      call makepoly()
      call move2( 1.0, 1.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5+W, 0.5+W)
      call closepoly()
      call color( D_YELLOW)
      call makepoly()
      call move2( 00.5-W, 0.0)
      call draw2( 00.5+W, 0.0)
      call draw2( 00.5+W, 0.5-W)
      call draw2( 1.0,   00.5-W)
      call draw2( 1.0,   00.5+W)
      call draw2( 00.5+W, 0.5+W)
      call draw2( 00.5+W, 1.0)
      call draw2( 00.5-W, 1.0)
      call draw2( 00.5-W, 0.5+W)
      call draw2( 0.0,   00.5+W)
      call draw2( 0.0,   00.5-W)
      call draw2( 00.5-W, 0.5-W)
      call draw2( 00.5-W, 0.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('S') !     SIERRA
      W=3.0/8.0
      call move2( 0.0,0.0)
      call color( D_WHITE)
      call makepoly()
      call move2(0.0,0.0)
      call getgp2(BOXX, BOXY)
      call rect(BOXX, BOXY, 1.0, 1.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( W, W)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0-W, 1.0-W)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('T') !     TANGO
      call move2( 0.0,0.0)
      call color( D_RED)
      call makepoly()
      call move2( 0.0/3,0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0/3, 1.0)
      call closepoly()
      call color( D_WHITE)
      call makepoly()
      call move2( 1.0/3,0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  2.0/3, 1.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 2.0/3,0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  3.0/3, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('U') !     UNIFORM
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0,0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5, 0.5)
      call closepoly()
      call makepoly()
      call move2( 0.5, 0.5)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 1.0)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call move2( 0.5, 0.5)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.0, 1.0)
      call closepoly()
      call makepoly()
      call move2( 0.5, 0.5)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  1.0, 0.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('V') !     VICTOR
      W=1.0/6
      CORNER=(W)*(1.0/sqrt(2.0))
      call move2( 0.0,0.0)
      call color( D_RED)
      call makepoly()
      call move2( 0.0,0.0)
      call draw2( CORNER, 0.0)
      call draw2( 0.5, 0.5-CORNER)
      call draw2( 1-CORNER, 0.0)
      call draw2( 1.0,0.0)
      call draw2( 1.0, CORNER)
      call draw2( 0.5+CORNER, 0.5)
      call draw2( 1.0, 1-CORNER)
      call draw2( 1.0, 1.0)
      call draw2( 1-CORNER, 1.0)
      call draw2( 0.5, 0.5+CORNER)
      call draw2( 0+CORNER, 1.0)
      call draw2( 0.0, 1.0)
      call draw2( 0.0, 1-CORNER)
      call draw2( 0.5-CORNER, 0.5)
      call draw2( 0.0, CORNER)
      call draw2( 0.0,0.0)
      call closepoly()
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0, CORNER)
      call draw2( 0.5-CORNER, 0.5)
      call draw2( 0.0, 1-CORNER)
      call draw2( 0.0, CORNER)
      call closepoly()
      call makepoly()
      call move2( CORNER, 0.0)
      call draw2( 1-CORNER, 0.0)
      call draw2( 0.5, 0.5-CORNER)
      call draw2( CORNER, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1.0, CORNER)
      call draw2( 0.5+CORNER, 0.5)
      call draw2( 1.0, 1-CORNER)
      call draw2( 1.0, CORNER)
      call closepoly()
      call makepoly()
      call move2( 0.5, 0.5+CORNER)
      call draw2( CORNER, 1.0)
      call draw2( 1-CORNER, 1.0)
      call draw2( 0.5, 0.5+CORNER)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('W') !     WHISKEY
      call color( D_BLUE)
      call makepoly()
      call rect( 0.0, 0.0,  1.0, 1.0)
      call closepoly()
      call color( D_WHITE)
      call makepoly()
      call rect( 1.0/6, 1.0/6,  5.0/6, 5.0/6)
      call closepoly()
      call color( D_RED)
      call makepoly()
      call rect( 2.0/6, 2.0/6,  4.0/6, 4.0/6)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('X') !     XRAY
      W=1.0/10
      call color( D_WHITE)
      call makepoly()
      call move2( 0.0,0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5-W, 0.5-W)
      call closepoly()
      call makepoly()
      call move2( 1.0, 0.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5+W, 0.5-W)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5-W, 0.5+W)
      call closepoly()
      call makepoly()
      call move2( 1.0, 1.0)
      call getgp2(BOXX, BOXY)
      call rect( BOXX, BOXY,  0.5+W, 0.5+W)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 0.5-W,0.0)
      call draw2( 0.5+W,0.0)
      call draw2( 0.5+W, 0.5-W)
      call draw2( 1.0, 0.5-W)
      call draw2( 1.0, 0.5+W)
      call draw2( 0.5+W, 0.5+W)
      call draw2( 0.5+W, 1.0)
      call draw2( 0.5-W, 1.0)
      call draw2( 0.5-W, 0.5+W)
      call draw2( 0.0, 0.5+W)
      call draw2( 0.0, 0.5-W)
      call draw2( 0.5-W, 0.5-W)
      call draw2( 0.5-W, 0.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('Y') !     YANKEE
      W=1.0/5
      call color( D_RED)
      call makepoly()
      call move2( 1-5*W, 0.0)
      call draw2( 1-4*W, 0.0)
      call draw2( 1.0, 4*W)
      call draw2( 1.0, 5*W)
      call draw2( 1-5*W, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1-3*W, 0.0)
      call draw2( 1-2*W, 0.0)
      call draw2( 1.0, 2*W)
      call draw2( 1.0, 3*W)
      call draw2( 1-3*W, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1-1*W, 0.0)
      call draw2( 1-0*W, 0.0)
      call draw2( 1.0, 0*W)
      call draw2( 1.0, 1*W)
      call draw2( 1-1*W, 0.0)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1-4*W)
      call draw2( 0.0, 1-3*W)
      call draw2( 3*W, 1.0)
      call draw2( 4*W, 1.0)
      call draw2( 0.0, 1-4*W)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1-2*W)
      call draw2( 0.0, 1-1*W)
      call draw2( 1*W, 1.0)
      call draw2( 2*W, 1.0)
      call draw2( 0.0, 1-2*W)
      call closepoly()
      call color( D_YELLOW)
      call makepoly()
      call move2( 1-4*W, 0.0)
      call draw2( 1-3*W, 0.0)
      call draw2( 1.0, 3*W)
      call draw2( 1.0, 4*W)
      call draw2( 1-4*W, 0.0)
      call closepoly()
      call makepoly()
      call move2( 1-2*W, 0.0)
      call draw2( 1-1*W, 0.0)
      call draw2( 1.0, 1*W)
      call draw2( 1.0, 2*W)
      call draw2( 1-2*W, 0.0)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1-5*W)
      call draw2( 0.0, 1-4*W)
      call draw2( 4*W, 1.0)
      call draw2( 5*W, 1.0)
      call draw2( 0.0, 1-5*W)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1-3*W)
      call draw2( 0.0, 1-2*W)
      call draw2( 2*W, 1.0)
      call draw2( 3*W, 1.0)
      call draw2( 0.0, 1-3*W)
      call closepoly()
      call makepoly()
      call move2( 0.0, 1-1*W)
      call draw2( 0.0, 1-0*W)
      call draw2( 0*W, 1.0)
      call draw2( 1*W, 1.0)
      call draw2( 0.0, 1-1*W)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################
      case('Z') !     ZULU
      call color( D_RED)
      call makepoly()
      call move2( 0.0,0.0)
      call draw2( 00.5, 0.5)
      call draw2( 1.0, 0.0)
      call draw2( 0.0,0.0)
      call closepoly()
      call color( D_BLACK)
      call makepoly()
      call move2( 0.0,0.0)
      call draw2( 0.0, 1.0)
      call draw2( 00.5, 0.5)
      call draw2( 0.0,0.0)
      call closepoly()
      call color( D_BLUE)
      call makepoly()
      call move2( 1.0, 0.0)
      call draw2( 00.5, 0.5)
      call draw2( 1.0, 1.0)
      call draw2( 1.0, 0.0)
      call closepoly()
      call color( D_YELLOW)
      call makepoly()
      call move2( 0.0, 1.0)
      call draw2( 00.5, 0.5)
      call draw2( 1.0, 1.0)
      call draw2( 0.0, 1.0)
      call closepoly()
      call box_outline(OUTLINE)
      ! ################################################################################;
   end select
end subroutine maritime_signal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end program testit
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
