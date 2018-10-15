!@(#) display sample page of a font
!#define 1
! ksh -c "PREFIX='-DTESTPRG90" ccall seefont.html'
      program showchars
      use M_draw
      call prefsize(1000,800)
      call vinit(' ')
      call color(-2)                   ! set line thickness
      write(*,*)'call with cursive'
      call seefont('cursive')
      write(*,*)'call with bad font name'
      call seefont('not_a_font_name')
      write(*,*)'blank should show all fonts'
      call seefont(' ')
      call vexit()
      stop
      end program showchars
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
subroutine seefont(fontin) ! draw a display of a font
! blank name: show sample of all fonts, then details on each one.
! known font name: show chart on just that font
! unknown name: show sample of all fonts and quit
! when pausing, use q in graphics area to q(uit),n(ext),p(revious)
! numeric string: show font by number
use M_journal, only : journal
use m_calculator,      only: stuff
use m_calculator_plus, only: dnum0
use M_draw
use M_drawplus, only : pop, push, biggest_ortho2
character(len=*),intent(in)   :: fontin
   character(len=80)  ::  line
   parameter(ifontnames=33)
   character(len=20)  ::  fontname(0:ifontnames)
   doubleprecision rval8
   integer BGCOLOR   ! background color
   integer FCOLOR    ! font color
   integer NCOLOR    ! number color
   integer LCOLOR    ! little letter color
   integer BCOLOR    ! box color

   fontname( 1)='astrology'
   fontname( 2)='cursive'
   fontname( 3)='cyrillic'
   fontname( 4)='futura.l'
   fontname( 5)='futura.m'
   fontname( 6)='gothic.eng'
   fontname( 7)='gothic.ger'
   fontname( 8)='gothic.ita'
   fontname( 9)='greek'
   fontname(10)='markers'
   fontname(11)='math.low'
   fontname(12)='math.upp'
   fontname(13)='meteorology'
   fontname(14)='music'
   fontname(15)='script'
   fontname(16)='symbolic'
   fontname(17)='times.g'
   fontname(18)='times.i'
   fontname(19)='times.ib'
   fontname(20)='times.r'
   fontname(21)='times.rb'
   fontname(22)='japanese'
   fontname(23)='small'
   fontname(24)='large'
   fontname(25)='orall_aa'
   fontname(26)='orall_ab'
   fontname(27)='orall_ac'
   fontname(28)='orall_ad'
   fontname(29)='orall_ae'
   fontname(30)='orall_af'
   fontname(31)='orall_ag'
   fontname(32)='orall_ah'
   fontname(33)='orall_ai'

   idum=backbuffer()
   !write(*,*)'bg, f, n, l, b'
   !read(*,*)bgcolor,fcolor,ncolor,lcolor,bcolor
   BGCOLOR=0
   FCOLOR=7
   NCOLOR=1
   LCOLOR=4
   BCOLOR=2
   iordinal=-1 ! initialize for when no page displayed
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
!  calculate ISTART and IEND and IDELTA (flag if to show one font)
   istart=-1
   iend=-1
   if(fontin.eq. ' ')then  ! if string is blank, show all fonts
      istart=1
      iend=ifontnames
   else                    ! look up string as a font name.
      ! display all font names as text(and see if input string matches one)
      do i20=0,ifontnames
         if(fontin.eq.fontname(i20))then ! if fontname matches
            istart=i20
            iend=i20
            exit
         endif
      enddo
      if(istart.lt.0)then ! no match to fontnames, so try string as a number
         rval8=dnum0(fontin)
         if(rval8.le.0.or.rval8.gt.ifontnames)then
            call journal('*seefont* unknown font name/number')
            do i90=1,ifontnames   ! list all font names
               line=fontname(i90)//'is font '
               call journal('sc',line(:len_trim(line)),i90)
            enddo
            return
         else ! got a number in range
            istart=int(rval8+0.5)
            istart=min(ifontnames,istart)
            istart=max(istart,0)
            iend=istart
         endif
      endif
   endif
!  if istart is -1 or 0 no match, else a specific font was asked for
   idelta=iend-istart
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
   do i80=1,ifontnames   ! list all font names
      line=fontname(i80)//'is font '
      call journal('sc',line(:len_trim(line)),i80)
   enddo
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
     icount=0  ! clear X11 key buffer on X11 on Linux (anybody else need this?)
100  continue  ! flush key presses in case someone has been clicking around
     idum=checkkey()
     icount=icount+1
     if(idum.gt.0.and.icount.lt.100)then
        !call journal('sc','*seefont* flushing ',idum)
        goto 100
     endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
! draw sample page with a line of each font
   if(idelta.gt.0)then  ! doing all fonts
     idum=backbuffer()
     call makeobj(12345)
     call push()          ! save graphics environment
     call polyfill(.false.)
     call color(BGCOLOR)
     call clear()                   ! clear display area to background color
     icount=1
     irows=ifontnames
     icols=irows
     ! set window and viewport so each box 10 units on a side
     call biggest_ortho2((-icols)*5.0,icols*5.0,-irows*5.0,irows*5.0)
     call color(FCOLOR)
     !call move2((-icols)*5.0,(-irows)*5.0)
     !call draw2(( icols)*5.0,( irows)*5.0)
     !call move2((-icols)*5.0,( irows)*5.0)
     !call draw2(( icols)*5.0,(-irows)*5.0)
     call centertext(.false.)
     call fixedwidth(.false.)
     tsize=4.3*2.8*0.85
     step=1.88
     call textsize(tsize,tsize*4.0/3.0)
     rleft=(-icols)*5.0+4.0
     y=irows*5.0+tsize*.5
     do i35=1,ifontnames
        call font('futura.l')
        tdec=getfontdec()
        y=y-step*tsize
        call move2(rleft,y)
        call rmove2(0.0,tdec+tsize/2.0)
        write(line,'(i3,'')'')')i35
        call drawstr(line(1:4))
        call drawstr(fontname(i35))
        call move2(rleft+8.3*tsize,y)
        call font(fontname(i35))      ! select text font for numbers
        tdec=getfontdec()
        call rmove2(0.0,tdec+tsize/2.0)
        call drawstr('@ABCZabcz012')
        if(y.lt.(-irows)*5.0+tsize*step.or.i35.eq.ifontnames)then
           y=irows*5.0+tsize*.5
           rleft=rleft+(2*icols*5.0)/2.0
        endif
     enddo

     call pop()  ! restore graphics environment
     call closeobj()
     call callobj(12345)
     call swapbuffers()
     call vflush()
     call journal('*seefont* enter q(uit) or n(ext) in graphic area')
     iordinal=getkey()
     if(iordinal.eq.113)then
        call stuff('PLTOBJECT',12345.0d0,'')
        return
     endif
     ! instructions for multi-font display
     call journal('*seefont* #=========================#')
     call journal('*seefont* | In graphics area press: |')
     call journal('*seefont* |    n for next font      |')
     call journal('*seefont* |    p for previous font  |')
     call journal('*seefont* |    q to quit            |')
     call journal('*seefont* #=========================#')

   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
!  do font-specific pages in detail
   irows=10
   icols=10
   icount=0   ! no infinite loops
60 continue   ! come here with new istart value if 'p' for previous
   do 10 i10=istart,iend
      idum=backbuffer()
      call makeobj(12345)
      icount=icount+1

      call push()          ! save graphics environment
      call polyfill(.false.)
      call color(BGCOLOR)            ! background color
      call clear()                   ! clear display area to background color

      ! lay out window with boxes are 10x10, with room above for 10x100 title
      ! The window value 0,0 is in the middle of the box area

      ! set window and viewport so each box 10 units on a side
                 ! xsmall,      xlarge,   ysmall,        ylarge
      call biggest_ortho2((-icols)*5.0,icols*5.0,-irows*5.0-9.0,irows*5.0+10.0)
      call color(FCOLOR)
               ! x1,          y1,             x2,        y2
      call rect((-icols)*5.0, -irows*5.0-9.0, icols*5.0, irows*5.0+10.0)

      call centertext(.false.)       ! all text should be centered
      tsize=8.0

      y=irows*5.0+10.0-tsize
      call move2((-icols)*5.0+4.0,y)

      call font('times.rb')          ! select text font for numbers
      call textsize(tsize,tsize)
      ilen=len_trim(fontname(i10))
      write(line,'(a)')fontname(i10)(1:ilen)
      call drawstr(line)

      call textsize(tsize/2.0,tsize/2.0)
      call drawstr('(')
      write(line,'(i3)')i10
      if(i10.lt.10)then
         call drawstr(line(3:3))
      else
         call drawstr(line(2:3))
      endif
      call drawstr(')')
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
!     draw the boxed letters
      ibox=33
      do i30=1,irows
         do i40=1,icols
            xmin=(i40-1)*10.0-icols*5.0
            xmax=xmin+10.0
            ymax=irows*5.0-(i30-1)*10.0
            ymin=ymax-10.0
            call color(BCOLOR)
            call rect(xmin,ymin,xmax,ymax)

            call color(FCOLOR)
            call centertext(.true.)    ! all text should be centered
            call textsize(5.5,5.5)
            call font(fontname(i10))   ! select text font for numbers
            write(line,'(a1)')char(ibox)
            call move2(xmin+5.0,ymin+5.0)
            call drawstr(line)

            call centertext(.false.)   ! all text should be centered
            call font('futura.m')      ! select text font for numbers
            call textsize(2.0,2.5)

            call color(NCOLOR)
            write(line,'(i3)')ibox
            call move2(xmin,ymin)
            call drawstr(line)

            call color(LCOLOR)
            write(line,'(a1)')char(ibox)
            back=strlength(line)
            call move2(xmax-back*2,ymin)
            call drawstr(line)

            ibox=ibox+1
            if(ibox.ge.128)goto 50
         enddo
      enddo
50    continue
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
      call pop()           ! restore graphics environment
      call closeobj()
      call callobj(12345)
      call swapbuffers()
      call vflush()
      if(icount.gt.200) goto 999  ! been in here too long, assume a loop in a batch job
      if(idelta.le.0)goto 999  ! originally single font requested
      iordinal=getkey()
      if(iordinal.eq.113)then     ! quit on "q" key
         goto 999
      elseif(iordinal.eq.110)then ! next on "n" key but at end
      elseif(iordinal.eq.112)then ! back to previous on "p" key
         istart=i10-1
         if(istart.le.0)istart=ifontnames
         iend=ifontnames
         goto 60
      endif
10 continue

   if(iordinal.le.-1)goto 999  ! not an interactive graphics device so end
   istart=1
   iend=ifontnames
   go to 60 ! keep going until a q is entered or hit end, then quit
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
999   continue
   call stuff('PLTOBJECT',12345.0d0,'')
   end subroutine seefont
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC--------
