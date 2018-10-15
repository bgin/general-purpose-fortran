      PROGRAM SYMBOLS
! *** LAST REVISED ON 20-FEB-1995 18:43:14.56
! *** SOURCE FILE: [libDL.SOURCES.FORTRAN.EXAMPLES]SYMBOLS.FOR
!
!     A SIMPLE PROGRAM TO PLOT A SAMPLE OF EACH PLOTTING SYMBOL
!     FROM THE ROUTINES DL_SYMS, AND DL_SYMSS
!
!     THIS VERSION OF SYMBOLS IS FOR COMPILERS THAT DON'T ESCAPE !
      CHARACTER(len=2)  :: T
      CHARACTER(len=65) :: STRING
      DATA STRING/'\2A\0=\7R\6a\^2\[\]\_\6n\[\]\]\]\=\U\6n=1\@\]\]\O\1K'/
!
      CALL DL_INIT(0,9.0,9.0,1.0,1.0,1.0)       !INTIALIZE PLOT PACKAGE
!
!     PLOT "DL_SYMS" CHARACTER FONTS
!
      AL=DL_SYMS(0.0,2.5,.2,'SYMS Character Fonts',90.0,20,-1)
      CALL dl_translate(1.2,-.5)
      CALL DL_SCALE(0.8)
      DO 43 I=1,32
         X1=(I-1)*.25
         AL=DL_SYMS(X1,0.,.20,CHAR(I-1),0.,1,-1)
         CALL DL_NUMBER(X1,-0.5,.15,FLOAT(I-1),90.,0.0,-1)
43    CONTINUE
!
      AL=DL_SYMS(-1.2,.5,.2,STRING,90.,99,-1)
      call dl_symbol(-1.2,1.5,.12,STRING,90.,60,-1)
      DO 100 I=1,9
         T=CHAR(92)//CHAR(I+47)
         call dl_symbol(-.35,0.3+(I-1)*.9,.15,'Font',90.,4,-1)
         call dl_symbol(999.,999.,.15,CHAR(I+47),90.,1,-1)
         AL=DL_SYMS(0.,0.3+(I-1)*.9,.23,T,0.,2,-1)     ! CHANGE FONT AND LOCATION
         AL=DL_SYMS(999.,999.,.23,'@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_',0.,33,-1)
         AL=DL_SYMS(0.,.6+(I-1)*.9,.23,T,0.,2,-1)      ! CHANGE LOCATION
         AL=DL_SYMS(999.,999.,.23,'`abcdefghijklmnopqrstuvwxyz{|}~',0.,32,-1)
         AL=DL_SYMS(0.,.9+(I-1)*.9,.23,T,0.,2,-1)      ! CHANGE LOCATION
         AL=DL_SYMS(999.,999.,.23,' !"#$%&''()*+,-./0123456789:;<=>?',0.,32,-1)
100   CONTINUE
      CALL DL_PAUSE()  ! flush and pause
      CALL DL_CLEAR
!
!     NOW PLOT "SYMSS" CHARACTER FONTS
!
      call dl_symbol(0.,3.,.2,'SYMSS Characters',90.,16,-1)
      DO 44 I=1,32
         X1=(I-1)*.25
         AL=DL_SYMSS(X1,0.,.25,CHAR(I-1),0.,1,-1,.02,0,0,0,0,0)
         CALL DL_NUMBER(X1,-.6,.18,FLOAT(I-1),90.,0.0,-1)
44    CONTINUE
      AL=DL_SYMSS(-1.3,.5,.2,STRING,0.,99,-1,.02,0,0,0,0,0)
      call dl_symbol(0.9,.5,.2,STRING,0.,60,-1)
      DO 110 I=1,9
         T=CHAR(92)//CHAR(I+47)
         call dl_symbol(-.35,I*.9,.15,'Font ',90.,5,-1)
         call dl_symbol(999.,999.,.15,CHAR(I+47),90.,1,-1)
         AL=DL_SYMSS(0.,I*.9,.23,T,0.,2,-1,.02,0,0,0,0,0)! CHANGE FONT,LOCATION
         AL=DL_SYMSS(999.,999.,.23,'@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_',0.,32,-1,.02,0,0,0,0,0)
         AL=DL_SYMSS(0.,.3+I*.9,.23,T,0.,2,-1,.02,0,0,0,0,0)! LOCATION
         AL=DL_SYMSS(999.,999.,.23,'`abcdefghijklmnopqrstuvwxyz{||}~',0.,33,-1,.02,0,0,0,0,0)
         AL=DL_SYMSS(0.,.6+I*.9,.23,T,0.,2,-1,.02,0,0,0,0,0)! LOCATION
         AL=DL_SYMSS(999.,999.,.23,' !"#$%&''()*+,-./0123456789:;<=>?',0.,32,-1,.02,0,0,0,0,0)
110   CONTINUE
      CALL DL_PAUSE()
      CALL DL_EXIT
      STOP
      END
