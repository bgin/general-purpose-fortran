! cannot get strlength to work properly on SunOS from Fortran
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!@(#) Make STK tape silo barcode stickers
      program sticky
      use M_vogle
      integer,parameter    :: iset=256
      character(len=iset)  :: alpha
      character(len=20)    :: dev
      character(len=5)     :: bar(iset)
      character(len=5)     :: space(iset)
      character(len=8)     :: anum
      character(len=6)     :: prefix
      character(len=1)     :: string
      character(len=80)    :: fgetvar
      character(len=80)    :: fname
      character(len=6)     :: style
      character(len=80)    :: oname
      integer              :: ihue(iset),ilight(iset),isat(iset)
      integer,save         :: iread(8) =(/ 1,5,6,7,2,3,4,8 /)
      icount=0                ! initialize number of characters in character set
!=======================================================================
!     read in character set data
      fname=fgetvar('CODE39','/usr/local/bin/code39') ! read environment variable CODE39 for a filename
      write(*,*)' using code39 file ',fname
      open(unit=39,file=fname,status='old')
      do i=1,256
         read(39,'(a1,1x,a5,1x,a5,3(1x,i3))',end=2) alpha(i:i),bar(i),space(i),ihue(i),ilight(i),isat(i)
         icount=i
      enddo
2     continue
      write(*,*)'defined ',icount,' characters'
      if(icount.gt.0)then
         write(*,'(a)')alpha(:icount)
      else
         write(*,*)'no letters'
         stop
      endif
!=======================================================================
!     sticker is 3 inches long, 8 barcode characters, using 39 code
!     3*2+9*1+1 (3 double wides + 9 single wides + 1 single wide space at end)=13 stripes per letter)
!     tall is total height of a sticker
      tall=3.0
      gap=tall/8.0/13.0   ! height of a single-wide stripe
!=======================================================================
      style=fgetvar('BARCODE','EDP') ! read environment variable CODE39 for a filename
      write(*,*)' BARCODE [WRIGHT|EDP] = ',style
      if(style.eq.'EDP')then
         konstant=0           ! start at top of sticker when making bars
      else
        konstant=tall         ! start at bottom of sticker when making bars
        gap=-gap              ! switch sign so move up instead of down while making bars
        do i80=1,8
           iread(i80)=i80     ! change order volser letters printed to 1,2,3,4,5,6,7,8
        enddo
      endif
!=======================================================================
!     a lot of empirical adjustments in these values
      actual=(9+5.0/16+0.9/32.0+1.0/44.0)/25.0
!     letter width
      wlet=7.0/32.0
!     bar width
      barlet=5.0/32.0*.95
      barlet=actual-wlet
      step=tall/6.0       ! height of a letter box
      over=barlet+wlet    ! how far to move right to make next ticket
!=======================================================================
      barlet=barlet-2.5/64.0 !make a slight gap
!=======================================================================
!     overall sheet size
      yy=7.5
      xx=10.0
      top=yy-1.0/3.0-3.0/32.0+1.0/16.0-1.0/32.0      ! top of first row
      rleft0=xx/32.0+1.0/64.0                        ! left edge of first sticker
      push=tall+1.0/8.0
!     how many fit across page
      itimes=25
!=======================================================================
      fname=fgetvar('BRUCE','NO') ! read environment variable CODE39 for a filename
      if(fname(1:3).eq.'YES')then
         itimes=13
         barlet=barlet+over
         over=over*2.0
      endif
!=======================================================================
      write(*,*)'Manual Adjustments'
      uleft=ureadv('hor. adjustment in inches,  + to right')
      udown=ureadv('vert. adjustment in inches, + is up')
      urot= ureadv('rotation angle in degrees,  + is ccw')
      top=top+udown
      rleft0=rleft0+uleft
!=======================================================================
!     read optional prefix
123   continue
      write(*,*)'please enter optional 1-5 char. alphameric prefix'
      write(*,*)'(use a # to represent blanks)'
      read(*,'(a)',err=123)prefix
      iprefix=len_trim(prefix)
      do 90 i90=1,6
      if(prefix(i90:i90).eq.'#')prefix(i90:i90)=' '
90    continue
      if(iprefix.eq.6)then
         number=0
         number2=50000
         ! read strings instead of making them with prefix and numbers
      else
!        read first number for this sheet
         write(*,*)'enter starting and ending numbers'
         read(*,*)number,number2
         if(number2.lt.number)then
            write(*,*)'second number must be greater than first'
            stop
         elseif(6-iprefix-int(log10(real(number2*10))).lt.0)then
            write(*,*)'only six (6) characters are allowed'
            write(*,*)'string {',prefix,'} and ',number2
            write(*,*)'will be longer than this'
            stop
         endif
      endif
!=======================================================================
      dev=fgetvar('VDEVICE',' ') ! read environment variable for a device
      if(dev.eq.' ')then
         write(*,'(a)',advance='no') ' enter device: ' ! get device
         read(*,'(a)') dev
      endif
!     as we are now about to do graphics initialize graphics and clear
      if(dev(1:3).eq.'X11')then
         iwidth=800
         iheight=600
         call prefsize(iwidth, iheight) ! Specify the preferred width and height  of  the  window opened by the *next* vinit.
      elseif(dev(1:3).eq.'psc')then
         iwidth=3000
         iheight=2250
         ixpos=150
         iypos=200
         call prefsize(iwidth, iheight) ! Specify the preferred width and height  of  the  window opened by the *next* vinit.
         call prefposition(ixpos,iypos)
      endif
      oname=fgetvar('ONAME','printme') ! output file name
      call voutput(oname)
      call vinit(dev)
      call juclr()
      idepth=getdepth()
      WRITE(*,*)'NUMBER OF COLORS IS ',2**(IDEPTH-1)
      call biggest_ortho2(0.0,10.0,0.0,7.5)
      call rotate(urot, 'z')
      call fixedwidth(.true.)
      call centertext(.true.) ! center text
      call vsetflush(.false.)
      call font('futura.l')
      call polyfill(.true.)
      wlet2=wlet*1.4
      call textsize(wlet2,wlet2) ! set size after selecting font
!=======================================================================
!c     adjust for skewing occuring in the printer in tray 2
!      rleft0=rleft0+1.0/16.0
!      x=0
!      y=0
!      z=0
!      sx=1
!      sy=1
!      sz=1
!      rx=0
!      ry=0
!      rise=1.0/16.0
!      run=9.25
!      ang=atan(rise/run)
!      rz=ang*180/3.1416
!      call u3csys(x,y,z,sx,sy,sz,rx,ry,rz)
!=======================================================================
!c     adjust for skewing occuring in the printer in tray 3
       rleft0=rleft0-3.0/64.0+3.00/64.0-1.0/64.0
       top=top+1.0/32.0
!=======================================================================
1     continue
      call color(7)
      call rasters(1)
      do 60 i60=1,2
      rleft1=rleft0
      do 50 i50=1,itimes
      if(iprefix.eq.6)then
         if(style.eq.'EDP')then
             anum='$      $'
         else
             anum='*      *'
         endif
         read(*,'(a)',end=999)anum(2:7)
      else
         if(style.eq.'EDP')then
             write(anum,202)number
202          format('$',i6.6,'$')
         else
             write(anum,303)number
303          format('*',i6.6,'*')
         endif
         if(iprefix.ne.0)then
         anum(2:iprefix+1)=prefix
         endif
      endif
      number=number+1
      pos=top-konstant ! start at bottom instead of top if making WRIGHT instead of EDP
      rleft=rleft1+wlet
      right=rleft+barlet
      do ii=1,8
         iii=iread(ii)
         indx=index(alpha,anum(iii:iii))
         if(indx.eq.0)then
            write(*,*)'error -unknown character can not be drawn'
            stop
         endif
         call rasters(1)
         do i30=1,5
            call move2(rleft,pos)
            if(bar(indx)(i30:i30).eq.'1')then
             down=2
            else
             down=1
            endif
            pos=pos-down*gap
            call makepoly()
            call jurect(right,pos)
            call closepoly()
            if(space(indx)(i30:i30).eq.'1')then
             down=2
            else
             down=1
            endif
            pos=pos-down*gap
         enddo
      enddo
!=======================================================================
!     place the colored text boxes and the text out
      pos=top
      do i40=2,7
         colr=8+i40
         indx=index(alpha,anum(i40:i40))
         rhue=ihue(indx)
         rlight=ilight(indx)
         rsat=isat(indx)
         call jumapc('hls',colr,rhue,rlight,rsat)
         icolor=colr
         call color(icolor)
         call move2(rleft1,pos)
         pos2=pos-step
         call makepoly()
         call jurect(rleft,pos2)
         call closepoly()
         call color(7)
         call rasters(3)
         call move2(rleft1,pos)
         call jurect(rleft,pos2)
         string(1:1)=alpha(indx:indx)
   
         call color(7)
         call rasters(7)
         call jprint((rleft1+rleft)/2.0,(pos+pos2)/2.0,string)
   
   !      call color(0)
   !      call rasters(2)
   !      call jprint((rleft1+rleft)/2.0,(pos+pos2)/2.0,string)
   
         call rasters(1)
         pos=pos-step
      enddo
      call color(7)
      rleft1=rleft1+over
!      if(number.gt.number2)goto 999
50    continue
      top=top-push  ! move down to do second row
60    continue
      if(number.le.number2)then
        call jupaus()
        call juclr()
        top=top+push+push
        goto 1
      endif
999   continue
      call vexit()
      end program sticky
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine jurect(x2,y2)
      use M_vogle
      call getgp2(x1,y1)
      call draw2(x2,y1)
      call draw2(x2,y2)
      call draw2(x1,y2)
      call draw2(x1,y1)
      return
      end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*(*) function fgetvar(varname,default)
!      this function returns the value of an enviromental variable
!      or a default value if the enviromental variable is not defined.
!        varname = enviromental variable name, blank or null terminated.
!                  string must be left justified (no leading white space).
!                  a character variable of 1 to 128 characters.
!        default = default value for returned value if varname is not an
!                  enviromental variable name, blank terminated.
!                  a character variable of 1 to 128 characters.
!        getvar  = enviromental variable value returned, blank terminated.
!                  a character variable of 1 to 128 characters.
!     to set an enviromental variable:
!       (Bourne shell): $export varname;varname='value for varname'
!       (C shell):      %setenv varname='value for varname'
      character*(*) varname,default
      iend=len_trim(varname) ! find last non-blank character in varname
      if(iend.eq.0)then
         write(*,*)'*getvar* blank variable name'
         fgetvar=default
         return
      endif
      call get_environment_variable(varname,fgetvar)
      if(fgetvar.eq.' ')fgetvar=default
      return
      end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real function ureadv(string)
      character*(*) string
1     write(*,*)string
      read(*,*,end=1,err=1,iostat=ios)value
      ureadv=value
      return
      end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine jprint(x,y,l)
!     print string l at position x,y
      use M_vogle
      character*(*) l
      character*132 l2  ! scratch variable for template and workstation bugs
      imax=len(l)
      if(imax.le.0)return
      ichars=len_trim(l(:imax))            ! number of characters in string
      if(ichars.le.0)return
      l2=l(:ichars)
      ix=ichars+1                       ! fixes hp/sun bug
      call move2(x,y)
      l2(ix:ix)= char(0)
      call drawstr(l2(:ix))
      return
      end
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine jupaus()
!     pause until a character is entered from graphics input.
!----------------------------------------------------------------------
      use M_vogle
!----------------------------------------------------------------------
      call vflush()             ! flush graphics buffers
      write(*,*)char(07)        ! send bell character
!     if standard  input is not coming from a terminal then you may want to do
!     a pause  from the  graphic  display  so that the  user can  still  pause
!     interactively  this would typically  occur if commands are coming from a
!     unix here document
      idum=getkey()           ! wait till a keypress is read in graphics window
      end subroutine jupaus
!#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine jumapc(model,ci,v1,v2,v3)
!     1989 john s. urban
!     ci      color index
!     model=rgb, hls, hsv
!----------------------------------------------------------------------
      use m_color, only : hue
      use M_vogle
!----------------------------------------------------------------------
      character*(*) model
      real vals(3)
      vals(1)=v1
      vals(2)=v2
      vals(3)=v3
!     WOULD BE NICE IF CHECK FOR IF OUT OF RANGE ABOVE MAXIMUM
      if(ci.lt.0.)then
         write(*,*) '*color* color index out of range'
         return
      endif
      if(model.ne.'rgb')then
         ix=2
!        bring hue angle into range of 0 to 360 degrees for template routine
         if(vals(1).gt.360.0)then
            vals(1)=mod(vals(1),360.0)
         elseif(vals(1).lt.0)then
            itemp=abs(int(vals(1))/360)+1
            vals(1)=vals(1)+itemp*360.0
         endif
      else
         ix=1
      endif
      do 20 i20=ix,3
      if(vals(i20).gt.100.0)then
         vals(i20)=100.0
         write(*,*) '*color* color value too high, set to 100'
      endif
20    continue
      do 10 i10=1,3
      if(vals(i10).lt.0)then
         vals(i10)=0.0
         write(*,*) '*color* negative color value set to zero'
      endif
10    continue
      imax=getdepth()**2-1
      if(ci.lt.0.or.ci.gt.imax)then
         write(*,*) '*color* color index out of range'
         write(*,*)'number of allowable colors is ',imax
         write(*,*)'color index is ',ci
         return
      endif
      call hue(model,vals(1),vals(2),vals(3),'rgb',vr,vg,vb,ist)
      ired=int(vr*2.55)
      igreen=int(vg*2.55)
      iblue=int(vb*2.55)
      call mapcolor(int(ci),ired,igreen,iblue)
      end subroutine jumapc
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
