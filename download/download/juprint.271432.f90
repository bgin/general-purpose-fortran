!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
!#define 1
! ksh -c "PREFIX='-DTESTPRG90" ccall juprint.HTML'
!     something very wrong with textsizes with font changes and such
!     font size after font change will not have the effect expected for some fonts
!     fontsize is set based on character size of box. Some fonts have descenders
!     and ascenders larger than a capital A or lower-case g. So they will look
!     smaller when displayed with the same text size. But if the text size is
!     set first, then a query will show that the text size is relative to the
!     original size, keeping the text the same size.

!     THIS ROUTINE IS CURRENTLY VERY SLOW

      program testit
      use M_draw
      use M_drawplus, only : biggest_ortho2
      use M_time
      character(len=256) ::  string
      call prefsize(1000,800)
      call prefposition(0,0)
      call vinit('X11')
      call centertext(.false.)
      call biggest_ortho2(-100.0,100.0,0.0,110.0)
      tsize=5.0
      tsizex=5.0*.75
      call font('futura.l')
      call textsize(tsizex,tsize)
      do
         call color(7)
         call clear()
         call color(1)
         call centertext(.true.)
         call move2(50.0,50.0)
         call drawstr('A')
         call move2(50.0,50.0)
         call drawstr('a')
         call centertext(.false.)
         call circle(50.0,50.0,2.5)

         yy=95.0
         xx=-5.0
         write(*,*)'Enter centering mode (1=centered,2=right justified)'
         read(*,*,end=999,err=999)icen
         do i10=1,15
            read(*,'(a)',end=999,err=999)string
            yy=yy-tsize
            call juprint(xx,yy,string,icen)
            call move2(50.0,yy)
            call jucp2(string,len_trim(string))
            call color(2)
            call move2(50.0,yy)
            call draw2(xx+100.0,yy)
            call vflush
         enddo
      enddo

999   continue
      call system_sleep(4)
      call vexit()
      end program testit
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! at this point, assuming text is not at an angle and is left justified,
! not centered
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine juprint(x,y,line,icenter)
!@(#) print string LINE at position x,y with embedded directives
      use M_draw
!      icenter=1    centermode
!      icenter=2    clear centering data, print right justified
      character(len=*) :: line
      call pushattributes()
      call centertext(.false.)
      call clipping(.false.)
      call justr(0.0,0.0,line(:len(line)),icenter,xs,xl,ys,yl) ! clear centering data or get centering data
      call justr(x,y,line(:len(line)),0,xs,xl,ys,yl)           ! actually print the string
      call popattributes()
      end subroutine juprint
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine justrlen(line,xlen,ylen,ydown,yup)
!@(#) query string size with embedded directives
      character(len=*) :: line
      ilen=len(line)
      call justr(0.0,0.0,line(:ilen),1,xs,xl,ys,yl)  ! set size and centering data
      call justr(0.0,0.0,line(:ilen),2,xs,xl,ys,yl)  ! clear centering data
      xlen=xl-xs
      ylen=yl-ys
      ydown=-ys
      yup=yl
      end subroutine justrlen
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine justr(x,y,l0,itype,xmin,xmax,ymin,ymax)
!@(#) print string l at position x,y with embedded directives
      use M_draw
      use M_drawplus, only : pop, push
      use M_journal,         only : journal
      use M_calculator_plus, only : inum0
!     string is assumed to be [string]\directive\string\directive\string\directive\...
!     does not allow for escaped backslash in string or null string between directives
!     environment is RESET after each string (MAY CHANGE THIS)
!     itype=0    draw the string with centering or size data set (must be second call)
!     itype=1    getting string size and centering shift
!     itype=2    blank out centering values
      character(len=*)    :: l0
      character(len=2000) :: line
      character(len=256)  :: directive
      logical             :: instring
      parameter(ilens=40) ! max number of lines from a single string
      real xmins(ilens),xmaxs(ilens) ! store each string centering information
      character(len=20)   :: lastfont
      save xmins,xmaxs,shift
      logical debug
      save debug
      data debug/.false./
      data xmins/40*0.0/
      data xmaxs/40*0.0/
      data shift/0.0/

      !write(*,*)'itype and string=',itype,l0
      line(1:2000)=' '
      line=l0
      if(itype.eq.2)goto 999            ! just blank out centering values

      imax=len(l0)                      ! determine length of input string
      if(imax.le.0)goto 999             ! null strings might cause problems
      ichars=len_trim(line(:imax))         ! number of characters in string
      if(ichars.le.0)goto 999           ! if string is all blanks ignore it (causes current position to not be updated)
      xnow=x                            ! initial x print position
      ynow=y                            ! initial y print position
      call move2(xnow,ynow)

      xmin=xnow                         ! min X position for this line of string
      xmax=xnow                         ! max X position for this line of string
      !write(*,*)'xmax 1 =',xmax
      ymin=y
      ymax=y

      k=1                               ! count of number of extra lines
      istart=1                          ! where current string/directive starts
      iend=0                            ! where current string/directive ends
      ichange=0                         ! were attributes changed by directives 0=no 1=yes
      instring=.true.
      call getfontsize(width,height)    !
      if(width.eq.0.0.or.height.eq.0.0)then
         write(*,*)'*justr* A) bad font size (W,H)=',width,height
         call printattribs('justr A')
      endif
      call textsize(width,height)    !
      width1=width                      ! use to restore the values at end (M_DASH does not reset this properly with pop/push?)
      height1=height
      ione=0 ! not zero print P=ichar and return to directive processing
             ! negative, spliced a string back into options

      do i10=1,ichars+1
        if(itype.eq.1)then
           xmins(k)=x
        endif
!-----------------------------------------------------------------------
        if(line(i10:i10).eq.char(92).or.i10.eq.ichars+1)then  ! found beginning or end of font directives
1000      continue
          if(instring)then               ! if was in a string now in a directive so print string and start directive
            instring=.false.
            if(iend-istart.ge.0.or.ione.ne.0)then    ! if string is not null print it
               if(itype.eq.0)then ! draw the string
                  call rmove2(-((xmaxs(k)-xmins(k))/2.0),0.0)
                  if(ione.gt.0)then
                     call jucp2(char(ione),1)  ! draw the special character from P=
                  elseif(ione.lt.0)then        ! splicing a $string back in
                  else
                     call jucp2(line(istart:iend),iend-istart+1)  !draw the string
                  endif
                  call rmove2(((xmaxs(k)-xmins(k))/2.0),0.0)
               elseif(itype.eq.1)then   ! just building string to get the size
                  xmins(k)=min(xmins(k),xnow)
                  !write(*,*)'building size ione=',ione,xmax
                  if(ione.eq.0)then
                     call move2(xnow+ustrlen2(line(istart:iend),iend-istart+1),ynow)
                  elseif(ione.lt.0)then
                  else
                     call move2(xnow+ustrlen2(char(ione),1),ynow)
                  endif
               else
                  call journal('sc','*justr* bad itype',itype)
                  goto 999
               endif
               call getgp2(xnow,ynow)
               xmax=max(xnow,xmax)
               !write(*,*)'xmax 2 =',xmax
               if(itype.eq.1)then    ! building length
                  xmaxs(k)=max(xmaxs(k),xnow)
               endif
               call getfontsize(width2,height2)

               !tdec=getfontdec()+height2/2.0  ! descender value
               call pushattributes()
                  call getjufont(lastfont)
                  call font(lastfont)
                  call getfontsize(widthf,heightf)
                  tdec=-0.22*heightf  ! descender value
               call popattributes()
               call textsize(width2,height2)
               xmin=min(xnow,xmin)
               xmax=max(xnow,xmax)
               !write(*,*)'xmax 3 =',xmax
               ymin=min(ynow-tdec-height2/2.0,ymin)
               ymax=max(ynow-tdec+height2/2.0,ymax)
            endif
          else                           ! end of a directive
            instring=.true.
            if(iend-istart.ge.0)then
              if(ichange.eq.0)then
                ichange=1
                call push()           ! save current environment in case change it
              endif
              if(ione.eq.0)directive=line(istart:iend)
              ione=0
              call fontchng(xnow,ynow,width,height,directive,x,y,k,itype,ione,debug)
              call move2(xnow,ynow)
              xmin=min(xnow,xmin)
              xmax=max(xnow,xmax)
              !write(*,*)'xmax 4 =',xmax
              ymin=min(ynow,ymin)
              ymax=max(ynow,ymax)
            endif
          endif
          if(ione.ne.0)then
             goto 1000
          else
             istart=i10+1                ! beginning of next string or directive
          endif
        else                           ! not a delimiter
          iend=i10                    ! this might be the end of the last directive or string
        endif
!-----------------------------------------------------------------------
      enddo
      if(ichange.ne.0)call pop()        ! so things changed here do not effect other calls
      if(debug)then
            write(*,*)'BOX:',xmin,xmax,ymin,ymax,shift
            ! draw box around text area
            call pushattributes()
            call move2(xmin-shift,ymin)
            call draw2(xmax-shift,ymin)
            call draw2(xmax-shift,ymax)
            call draw2(xmin-shift,ymax)
            call draw2(xmin-shift,ymin)
            call color(1)
            call move2(xmin-shift,y)
            call draw2(xmax-shift,y)
            call color(7)
            call popattributes()
      endif
      if(itype.eq.0)then
      else if(itype.eq.1)then
         shift=(xmax-xmin)/2.0
      endif

      call move2(xnow,ynow)             ! does pop() loose current position?
      call textsize(width1,height1)
999   continue
         if(itype.eq.0.or.itype.eq.2)then        ! do not keep length information
            do i40=1,ilens
                  xmins(i40)=0.0
                  xmaxs(i40)=0.0
            enddo
            shift=0.0
         endif
      end subroutine justr
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine fontchng(xnow,ynow,width,height,string,x0,y0,ilines,ii,ione,debug)
!     does not check if STRING is longer than LINE
!     if using this feature, the variables v,h,f,s,c,w,n,a are reserved names
      use M_calculator, only: igetvalue, stuff, rgetvalue, iclen_calc
      use M_calculator_plus, only : inum0,rnum0,jucalcx, strgar2
      use M_journal, only : journal
      use M_strings, only : delim
      use M_draw
      character(len=*)           :: string  ! THIS MUST BE ABLE TO BE CHANGED AND LENGTHENED
      parameter(ifontnames=33)
      character fonts(ifontnames)*11
      parameter(ixn=25)
      !real rcurv(ixn)
      parameter(NN=20)
      character(len=80)          :: array(NN),cname
      character(len=iclen_calc)  :: fetched
      character(len=len(string)) :: chold
      logical                    :: debug
      integer                    :: ibegin(NN), ITERM(NN)
      doubleprecision            :: dvalue
      data fonts/                                                       &
     & 'astrology', 'cursive', 'cyrillic', 'futura.l', 'futura.m',      &
     &'gothic.eng', 'gothic.ger', 'gothic.ita',                         &
     &'greek', 'markers', 'math.low', 'math.upp',                       &
     &'meteorology', 'music', 'script', 'symbolic',                     &
     &'times.g', 'times.i', 'times.ib', 'times.r', 'times.rb',          &
     &'japanese', 'small', 'large',                                     &
     &'orall_aa', 'orall_ab', 'orall_ac',                               &
     &'orall_ad', 'orall_ae', 'orall_af',                               &
     &'orall_ag', 'orall_ah', 'orall_ai'/
!-----------------------------------------------------------------------
!     M_DRAW BUG
      call getfontsize(widthT,heightT)
      if(widthT.le.0.or.heightT.le.0)then
         call textsize(width,height)
      endif
!-----------------------------------------------------------------------
!     cache variable names
      XV =rgetvalue('v')
      XH =rgetvalue('h')
      XF =rgetvalue('f')
      XS =rgetvalue('s')
      XC =rgetvalue('c')
      XW =rgetvalue('w')
      XN =rgetvalue('n')
      XSX=rgetvalue('sx')
      XSY=rgetvalue('sy')
      XVV=rgetvalue('V')
      XHH=rgetvalue('H')
      XSS=rgetvalue('S')
      XA =rgetvalue('a')
      XB =rgetvalue('b')
      XBB=rgetvalue('B')
      XX =rgetvalue('x')
      XY =rgetvalue('y')
      XP =rgetvalue('P')
      XD =rgetvalue('D')
      XI =rgetvalue('i')

      call getgp2(XX,YY)
      call stuff('CX',XX,'')
      call stuff('CY',YY,'')
!-----------------------------------------------------------------------
      if(string.ne.' ')then
         call delim(string,array,NN,icount,ibegin,iterm,ilen,' ') ! parse string into array
         cname(1:1)='$'
         do i30=1,icount
            call zqjreset()
            if(index(array(i30),'=').ne.0)then   ! if an = assume a default numeric expression
               rdum=rnum0(array(i30))  ! evaluate numeric expression
            else  ! string names without a $ character
               cname(2:)=array(i30)
               call jucalcx(cname,dvalue,fetched,ierr,iline2)
               if(ierr.ne.2)then
                  call journal('*juprint* error in string variable')
               elseif(ierr.eq.2.and.dvalue.ge.1.0d0)then
                  !call strgar2(fetched,ixn,rcurv,inums,' ',ierr) ! evaluate expanded math functions
                  chold(:)=' '
                  chold=string(iterm(i30)+1:)
                  string(:)=fetched
                  istart=len_trim(string)
                  istart=istart+1
                  string(istart:)=chold
                  ione=-1 ! flag to just redo the directive
                  goto 40
               endif
            endif
            call doescape(ii,fonts,width,height,xnow,ynow,ilines,x0,y0,ione)
            if(ione.ne.0)then
               string(:iterm(i30))=' ' ! blank out already processed directives
               goto 40
            endif
         enddo
      endif
40    continue
!-----------------------------------------------------------------------
      XDnew=rgetvalue('D')
      if(XDnew.eq.0)then   ! do not change unless D=1 or D=0
         debug=.true.
      elseif(XDnew.eq.1)then
         debug=.false.
      endif
!-----------------------------------------------------------------------
!     restore variable names
      call stuff('v',XV,'')
      call stuff('h',XH,'')
      call stuff('f',XF,'')
      call stuff('s',XS,'')
      call stuff('c',XC,'')
      call stuff('w',XW,'')
      call stuff('n',XN,'')
      call stuff('sx',XSX,'')
      call stuff('sy',XSY,'')
      call stuff('P',XP,'')
      call stuff('V',XVV,'')
      call stuff('H',XHH,'')
      call stuff('S',XSS,'')
      call stuff('a',XA,'')
      call stuff('b',XB,'')
      call stuff('B',XBB,'')
      call stuff('x',XX,'')
      call stuff('y',XY,'')
      call stuff('D',XD,'')
      call stuff('i',XI,'')
!-----------------------------------------------------------------------
      end subroutine fontchng
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine zqjreset()
      use m_calculator, only : stuff
      call stuff('v',-999.0d0,'')
      call stuff('h',-999.0d0,'')
      call stuff('f',-999.0d0,'')
      call stuff('s',-999.0d0,'')
      call stuff('c',-999.0d0,'')
      call stuff('w',-999.0d0,'')
      call stuff('n',-999.0d0,'')
      call stuff('sx',-999.0d0,'')
      call stuff('sy',-999.0d0,'')
      call stuff('P',-999.0d0,'')
      call stuff('V',-999.0d0,'')
      call stuff('H',-999.0d0,'')
      call stuff('S',-999.0d0,'')
      call stuff('a',-999.0d0,'')
      call stuff('b',-999.0d0,'')
      call stuff('B',-999.0d0,'')
      call stuff('x',-999.0d0,'')
      call stuff('y',-999.0d0,'')
      call stuff('Fx',1234.5678d0,'')
      call stuff('Fy',1234.5678d0,'')
      call stuff('Fcen',-999.0d0,'')
      call stuff('Ffix',-999.0d0,'')
      call stuff('D',-999.0d0,'')
      call stuff('i',-999.0d0,'')
      end subroutine zqjreset
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine zqjresetSLOW()
      use M_calculator_plus, only : strgar2
      character(len=200)  ::  line
      parameter(ixn=25)
      real rcurv(ixn)

       line(:)=' '
!                    123456789 123456789 123456789 123456789 1234567890
      line(001:050)='v=-999 h=-999 f=-999 s=-999 c=-999 w=-999'
      line(051:100)=' n=-999 sx=-999 sy=-999 V=-999 H=-999 S=-999'
      line(101:150)=' a=-999 b=-999 B=-999 x=-999 y=-999 P=-999 D=-999'
      line(151:200)=' Fx=1234.5678 Fy=1234.5678 Fcen=-999 Ffix=-9999'
      call strgar2(line,ixn,rcurv,inums,' ',ierr) ! evaluate math functions
      end subroutine zqjresetSLOW
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
! too many calls to calculator. Do one pass at end like earlier version?
      subroutine doescape(ii,fonts,width,height,xnow,ynow,ilines,x0,y0,ione)
      use m_calculator, only: igetvalue, rgetvalue ! igetvalue faster than inum0?
      use M_journal, only : journal
      use M_draw
      use M_calculator_plus, only : inum0,rnum0
      !parameter(ifontnames=37)
      parameter(ifontnames=33)
      character fonts(ifontnames)*11
      character lastfont*20
      ! igetvalue faster than inum0?
!-----------------------------------------------------------------------
!     color selection
      if(ii.eq.0)then    ! M_DRAW BUG CHANGES COLOR IN PUSH/POP
         ic=int(igetvalue('c')) ! ic=inum0('c')
         if(ic.ne.-999)then
            call color(ic)
         endif
      endif
!-----------------------------------------------------------------------
!     font selection by number
      if=int(igetvalue('f')) !  if=inum0('f')
      if(if.ge.1.and.if.le.ifontnames)then
         call font(fonts(if)) ! call selected font
      elseif(if.eq.0)then
         call getjufont(lastfont)
         call font(lastfont)
      elseif(if.ne.-999)then
         do i10=1,ifontnames
            call journal('sc',fonts(i10),i10)
         enddo
      endif
!-----------------------------------------------------------------------
!     line width for software characters
      iw=int(igetvalue('w')) !  iw=inum0('w')
      if(iw.ne.-999)call rasters(iw)
!-----------------------------------------------------------------------
!     vertical movement from current position in scale of original character height
      v=rgetvalue('V') !  v=rnum0('V')
      if(v.ne.-999)then
         ynow=ynow+height*v
      endif

!     vertical movement from current position in scale of current character height
      v=rgetvalue('v') !  v=rnum0('v')
      if(v.ne.-999)then
         call getfontsize(cwidth,cheight)
         ynow=ynow+cheight*v
      endif
!-----------------------------------------------------------------------
!     horizontal movement from current position in scale of original character width
      h=rgetvalue('H') !  h=rnum0('H')
      if(h.ne.-999)then
         xnow=xnow+width*h
      endif

!     horizontal movement from current position in scale of current character width
      h=rgetvalue('h') !  h=rnum0('h')
      if(h.ne.-999)then
         call getfontsize(cwidth,cheight)
         xnow=xnow+cwidth*h
      endif
!-----------------------------------------------------------------------
!     font size
      s=rgetvalue('S') !  s=rnum0('S')
      if(s.ne.-999)then
         call textsize(width*s,height*s)
      endif

      s=rgetvalue('s') !  s=rnum0('s')
      if(s.ne.-999)then
         call getfontsize(cwidth,cheight)
         call textsize(cwidth*s,cheight*s)
      endif
!-----------------------------------------------------------------------
      sx=rgetvalue('sx') !  sx=rnum0('sx')
      sy=rgetvalue('sy') !  sy=rnum0('sy')
      if(sx.ne.-999.or.sy.ne.-999)then
         if(sx.eq.-999) sx=1.0
         if(sy.eq.-999) sy=1.0
         call textsize(width*sx,height*sy)
      endif
!-----------------------------------------------------------------------
      ixp=int(igetvalue('P')) !  x=rnum0('x')
      if(ixp.ne.-999)then
           ione=ixp
      endif
!-----------------------------------------------------------------------
      x=rgetvalue('x') !  x=rnum0('x')
      if(x.ne.-999)then
         xnow=x
      endif
!-----------------------------------------------------------------------
      y=rgetvalue('y') !  y=rnum0('y')
      if(y.ne.-999)then
         ynow=y
      endif
!-----------------------------------------------------------------------
      a=rgetvalue('a') !  a=rnum0('a')
      if(a.ne.-999)then
         call textang(a)
      endif
!-----------------------------------------------------------------------
      ri=rgetvalue('i') !  ri=rnum0('i')
      if(ri.ne.-999)then
         call textslant(ri)
      endif
!-----------------------------------------------------------------------
      b=rgetvalue('b') !  b=rnum0('b')
      if(b.ne.-999)then
         call getfontsize(cwidth,cheight)
         ynow=y0+b*cheight
      endif
!-----------------------------------------------------------------------
      B=rgetvalue('B') !  B=rnum0('B')
      if(B.ne.-999)then
         ynow=y0+B*height
      endif
!-----------------------------------------------------------------------
!     line feed
      rn=rgetvalue('n') !  rn=rnum0('n')
      if(rn.ne.-999)then
         call getfontsize(cwidth,cheight)
         xnow=x0
         !ynow=ynow-y0-(ilines+rn)*cheight
         ynow=ynow-rn*cheight
         ilines=ilines+1
      endif
!-----------------------------------------------------------------------
      ifixed=igetvalue('Ffix') !  ifixed=inum0('Ffix')
      if(ifixed.eq.0)then
         call fixedwidth(.false.)
      elseif(ifixed.eq.1)then
         call fixedwidth(.true.)
      endif
!-----------------------------------------------------------------------
      icen=igetvalue('Fcen') !  icen=inum0('Fcen')
      if(icen.eq.0)then
         call centertext(.false.)
      elseif(icen.eq.1)then
         call centertext(.true.)
      endif
!-----------------------------------------------------------------------
      fx=rgetvalue('Fx') !  fx=rnum0('Fx')
      if(fx.ne.1234.5678)then
         xnow=fx
      endif
!-----------------------------------------------------------------------
      fy=rgetvalue('Fy') !  fy=rnum0('Fy')
      if(fy.ne.1234.5678)then
         ynow=fy
      endif
!-----------------------------------------------------------------------
      icen=igetvalue('Fcen') !  icen=inum0('Fcen')
      if(icen.eq.0)then
         call centertext(.false.)
      elseif(icen.eq.1)then
         call centertext(.true.)
      endif
!-----------------------------------------------------------------------
      end subroutine doescape
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
      subroutine jucp2(line,ilen)
!@(#) plot string line at current position
      use M_draw
!     For some packages and machines it is best to put the string into
!     a scratch variable and put a null character at the end
!     IGNORES TRAINING BLANKS
!     USES CURRENT TEXT JUSTIFICATION MODES

!     M_DRAW fonts use the x value for the bottom of the descender for letters like g,
!     so move the character down about 22% to keep it level if change size so
!     THE CURRENT Y-VALUE IS THE HEIGHT OF THE BASE OF LETTERS WITHOUT DESCENDERS
!     need to make a lower level option in M_DASH for this

      character(len=*)   :: line
      parameter(imx=257)
      character(len=imx) :: line2  ! scratch variable for template and workstation bugs
      imax=len(line)               ! determine length of input string
                                   ! COULD WARN IF LENGTH OF INPUT TOO BIG TO STORE IN BUFFER
      if(imax.le.0)return          ! null strings might cause problems
      ! DO NOT TRIM TRAILING WHITE-SPACE
      ! ichars=len_trim(line(:imax))    ! number of characters in string
      ichars=ilen
      if(ichars.le.0)return        ! if string is all blanks ignore it (causes current position to not be updated)
      ichars=min(ichars,imx-1)
      line2=line(:ichars)          ! transfer a copy of the string to the buffer
      ix=ichars+1                  ! fixes hp/sun bug
      line2(ix:ix)= char(0)        ! put a null character at end of string
      call getgp2(xnow,ynow)

      call getfontsize(cwidth,cheight)   ! NOT GETTING EXPECTED SIZE IF DID A FONT CHANGE
      if(cwidth.eq.0.0.or.cheight.eq.0.0)then
         write(*,*)'1 bad font size=',cwidth,cheight
         call printattribs('size1')
       endif

      call pushattributes()
         call font('futura.l')
         call getfontsize(cwidth2,cheight2)   ! NOT GETTING EXPECTED SIZE IF DID A FONT CHANGE
         if(cwidth2.eq.0.0.or.cheight2.eq.0.0)then
            write(*,*)'2 bad font size=',cwidth2,cheight2
            call printattribs('size2')
          endif
      call popattributes()

      call getfontsize(cwidth3,cheight3)   ! NOT GETTING EXPECTED SIZE IF DID A FONT CHANGE
      if(cwidth3.eq.0.0.or.cheight3.eq.0.0)then
         write(*,*)'3 bad font size=',cwidth3,cheight3
         call printattribs('size3')
      endif
      call textsize(cwidth,cheight)      ! DOES NOT HAVE EXPECTED CHANGE
      fudge=-cheight/2.0+.28*cheight2
      call move2(xnow,ynow+fudge)! account for descender in font
      call clipping(.false.)
      call drawstr(line2(:ix))     ! output string with null at end
      call clipping(.true.)
      call getgp2(xnow,ynow)
      call move2(xnow,ynow-fudge)! account for descender in font
      end subroutine jucp2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
