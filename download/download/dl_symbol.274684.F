#ifdef TESTPRG
      PROGRAM TSYMBOL
!
!     A SIMPLE PROGRAM TO PLOT A SAMPLE OF EACH PLOTTING SYMBOL
!     FROM THE ROUTINE SYMBOL
!
      CALL dl_init(0,8.0,8.0,1.0,1.0,1.0)  !INTIALIZE PLOT PACKAGE
      X1=0.5
      Y=1.0
!
!     PLOT "SYMBOL" CHARACTER FONT
!
      CALL DL_SYMBOL(0.,3.,.2,'DL_SYMBOL Characters',90.,17,-1)
      DO 25 ISYM=0,23
         DO 26 J=0,6
            Y=FLOAT(J)*1.1
            K=J*23+ISYM
            IF (K.GT.134) GOTO 26
            CALL DL_NUMBER(X1+.01,Y,.18,FLOAT(K),90.,0.0,-1)
            CALL DL_SYMBOL(X1,Y+.65,.2,CHAR(K),90.,1,-1)
26       CONTINUE
         X1=X1+.24
25    CONTINUE
      call dl_pause()
      CALL dl_exit()    ! TERMINATE PLOT PACKAGE
      STOP
      END
#endif
