      PROGRAM PARS
      ! read in ADI files from some CAD/CAM packages
      use M_vogle
      CHARACTER*80 LINE, ARRAY(20)
      INTEGER IBEGIN(20),ITERM(20)
      REAL VALUE(20),COLORS(8)                                
!     colors should be settable, arbitrarily set here
      DATA COLORS/7.,1.,2.,3.,4.,5.,6.,7./
      ifirst=0
      ICOUNT=0
      open(unit=90,file='west1')
1     READ(90,'(A)',END=999)LINE
!     ICOUNT=NUMBER OF STRINGS RETURNED
      CALL PARSE(LINE,ARRAY,ICOUNT,IBEGIN,ITERM,ILEN)
      IF(ICOUNT.NE.0)THEN
      DO 110 I=1,ICOUNT
         CALL RNUM0(ARRAY(I),VALUE(I),IFLAG)
         IF(IFLAG.ne.0)THEN
         WRITE(*,*)' error - non-numeric value found'
         ENDIF
110      CONTINUE
      ELSE
         WRITE(*,*)' BLANK LINE ENCOUNTERED'
         GO TO 1
      ENDIF
      IWHERE=INT(VALUE(1)+0.5)
!X      WRITE(*,*)' IWHERE=',IWHERE
!X      WRITE(*,*)LINE
      goto(10,20,30,40,50,60,70,80,90)IWHERE
      WRITE(*,*)' ILLEGAL INITIAL VALUE =',VALUE(1)
      GO TO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
10    CONTINUE
      if(ifirst.eq.0)then
         CALL vinit('')
         RES=1000
         RMAX=65535
         RMAXX=65.0
         RMAXY=65.
         MINX=RMAX+1
         MAXX=-1
         MINY=RMAX+1
         MAXY=-1
         CALL biggest_ortho2(0.0,RMAXX,0.0,RMAXY)
         !CALL UOUTLN
      else
         !CALL UERASE
         !CALL UOUTLN
      endif
         ifirst=ifirst+1
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
20    CONTINUE
!     end plot
      !CALL UPAUSE
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
30    CONTINUE
!     move
      CALL move2(VALUE(2)/RES,VALUE(3)/RES)
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
40    CONTINUE
!     draw
      CALL draw2(VALUE(2)/RES,VALUE(3)/RES)
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
50    CONTINUE
!     new pen
!     actual command to select a pen. The second value selects a
!     pen number
      IPEN=MOD(INT(VALUE(2)+0.5),7)+1
!x    CALL UPSET('COLOR',COLORS(IPEN))
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
60    CONTINUE
!     select speed
!     not applicable
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
70    CONTINUE
!     set line style
!     code selects a plotter line font. It is a universal
!     convention within AutoCAD that zero selects a continuous
!     (solid) line. The meaning of the other fonts is up to
!     the driver.
      if(value(2).eq.0)then
        !call uset('LNULL')
      else
        !call uset('DNULL')
      endif
      GOTO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
80    CONTINUE
!     pen change
!     not applicable
!!!!!!THIS SHOULD NOT BE THERE
      ICOUNT=ICOUNT+1
      !call ubell
      !call upause
      IPEN=MOD(INT(ICOUNT),7)+1
!x    CALL UPSET('COLOR',COLORS(IPEN))
      GO TO 1
90    CONTINUE
      WRITE(*,*)'ABORT PLOT will never be seen (pg 86)'
      WRITE(*,*)LINE
      WRITE(*,*)(VALUE(II),II=1,ICOUNT)
      GO TO 1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
999   CONTINUE
      CALL vexit
      END 
      SUBROUTINE PARSE(LINE,ARRAY,ICOUNT,IBEGIN,ITERM,ILEN) 
      CHARACTER*(*) LINE, ARRAY(20)*80, DELIM*1
      INTEGER ICOUNT, IBEGIN(20),ITERM(20),ILEN
      PARAMETER (DELIM=',')
      ICOUNT=0
      ILEN=LEN_TRIM(LINE)
      IF(ILEN.EQ.0)RETURN
      ICOL=1
      DO 100 IARRAY=1,20,1
200   IF(LINE(ICOL:ICOL).NE.DELIM)THEN
        ISTART=ICOL 
        IBEGIN(IARRAY)=ICOL
        IEND=INDEX(LINE(ISTART:ILEN),DELIM)
         IF(IEND.LE.0)THEN
           ITERM(IARRAY)=ILEN 
           ARRAY(IARRAY)=LINE(ISTART:ILEN)
           ICOUNT=IARRAY
           RETURN
         ELSE
           IEND=IEND+ISTART-2 
           ITERM(IARRAY)=IEND 
           ARRAY(IARRAY)=LINE(ISTART:IEND)
         ENDIF
        ICOL=IEND+2 
      ELSE
        ICOL=ICOL+1 
        GO TO 200
      ENDIF
      IF(ICOL.GT.ILEN)THEN
        ICOUNT=IARRAY
        RETURN
      ENDIF
100   CONTINUE
      ICOUNT=20
      RETURN
      END 
      SUBROUTINE RNUM0(CHARS,VALUE,IFLAG)
      CHARACTER*(*) CHARS, FRMT*13
      WRITE(FRMT,101)LEN(CHARS)
101   FORMAT( '(BN,G',I5,'.0)' )
      IFLAG=0
      IERR=0
      READ(CHARS,FMT=FRMT,IOSTAT=IERR,ERR=999)VALUE
      RETURN
999   VALUE=0.0
      IFLAG=1
      RETURN
      END 
