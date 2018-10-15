      PROGRAM LINETYPE
! *** LAST REVISED ON 22-JUN-1990 08:33:36.89
! *** SOURCE FILE: [DL.GRAPHICS.libDL]LINETYPE.FOR
!
!     PROGRAM TO DEMONSTRATE THE AVAILABLE LINE TYPES AND COLORS
!
      CALL DL_INIT(0,12.0,12.0,0.2,0.3,1.0)
      do 200 idash=0,1000,100
!
!        PLOT A PAGE TITLE
         CALL DL_NEWPEN(-1)
!
         CALL DL_SYMBOL(.15,.1,.15, '(Type,Width,Color) Available Line Types' ,90.,39,-1)
!
!        PLOT A SAMPLE OF EACH LINE TYPE, WIDTH, AND COLOR
!
         ICNT=0
         DO IWIDTH=1,5
            DO ITYPE=1,9
               DO ICOLOR=1,7
      !
10                X=(ICNT/35)*1.7+.21
                  Y=MOD(ICNT,35)*.2
                  IF (X.GT.10.0) THEN             ! NEW PAGE
                      CALL DL_PAUSE()
                      CALL DL_CLEAR
                      ICNT=0
                      GOTO 10
                  ENDIF
                  ICNT=ICNT+1
      !
      !           SET TO SOLID LINE OF WIDTH 1
      !
                  CALL DL_NEWPEN(-1)
                  CALL DL_COLOR(2)
                  CALL DL_NUMBER(X,Y,.15,REAL(ITYPE),0.,0.0,-1)
                  CALL DL_SYMBOL(999.,999.,.15,',',0.,1,-1)
                  CALL DL_NUMBER(999.,999.,.15,REAL(IWIDTH),0.,0.0,-1)
                  CALL DL_SYMBOL(999.,999.,.15,',',0.,1,-1)
                  CALL DL_NUMBER(999.,999.,.15,REAL(ICOLOR),0.,0.0,-1)
                  CALL DL_COLOR(ICOLOR)
                  CALL DL_NEWPEN(IDASH+ITYPE+10*IWIDTH)
                  X=X+0.8
                  CALL DL_MOVE(X,Y)
                  CALL DL_DRAW(X+.75,Y)
                  CALL DL_MOVE(X,Y)
               enddo
            enddo
         enddo
         CALL DL_PAUSE
         CALL DL_CLEAR
200   CONTINUE
      CALL DL_PAUSE()
      CALL DL_EXIT
      STOP
      END
