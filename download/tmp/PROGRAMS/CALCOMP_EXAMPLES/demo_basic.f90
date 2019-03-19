PROGRAM QA1
   USE M_calcomp
   CHARACTER * 10 ICHR1
   CHARACTER * 14 ICHR2,ICHR3
   CHARACTER * 5  ICHR4,ICHR6
   CHARACTER * 4  ICHR5,ICHR7
   CHARACTER * 29 ICHR8
   CHARACTER * 32 ICHR9,ICHR10,ICHR11,ICHR12
   CHARACTER * 1 IBCD
   ICHR1='WIDTH (FT)'
   ICHR2='THICKNESS (IN)'
   ICHR3='PRESSURE (PSI)'
   ICHR4='THK= '
   ICHR5=' IN.'
   ICHR6='WTH= '
   ICHR7=' FT.'
   ICHR8='CRITICAL BUCKLING PRESSURE OF'
   ICHR9='HYPERBOLIC PARABOLOID SHELLS FOR'
   ICHR10='FIXED WIDTH VS VARYING THICKNESS'
   ICHR11='FIXED THICKNESS VS VARYING WIDTH'
   ICHR12='PREPARED ON A CALCOMP PLOTTER'
   CALL PLOTS(0.0,24.0,0.0,12.0)
! ESTABLISH AN ORIGIN SO NEGATIVE VALUES UP TO -0.5 MAY BE USED
   CALL PLOT(0.5,0.5,-3)
! PLOT X-AXIS FOR WIDTH
   X=0.0
   DO I=1,10
      CALL PLOT(X,0.0,3)
      X=X+1.0
      CALL PLOT(X,0.0,2)
      CALL PLOT(X,-.1,2)
      CALL NUMBER(X,-0.25,0.1,5.0*X,0.0,-1)
   ENDDO
   CALL SYMBOL(4.0,-0.40,0.12,IBCD,1,0.0,-1)
   CALL SYMBOL(4.2,-0.45,0.14,ICHR1,INTEQ,0.0,10)
   CALL PLOT (0.0,0.5,-3)
! PLOT X-AXIS FOR THICKNESS
   X=0.0
   DO I=1,5
      CALL PLOT(X,0.0,3)
      X=X+1.0
      CALL PLOT(X,0.0,2)
      CALL PLOT(X,-.1,2)
      CALL PLOT(X,0.0,2)
      X=X+1.0
      CALL PLOT(X,0.0,2)
      CALL PLOT(X,-.1,2)
      CALL NUMBER(X,-0.25,0.1,X,0.0,-1)
   enddo
   CALL SYMBOL(3.7,-0.40,0.12,IBCD,7,0.0,-1)
   CALL SYMBOL(4.0,-0.45,0.14,ICHR2,INTEQ,0.0,14)
! PLOT Y-AXIS
   Y=0.0
   DO I=1,9
      CALL PLOT(0.0,Y,3)
      Y=Y+1.0
      CALL PLOT(0.0,Y,2)
      CALL PLOT(-.1,Y,2)
      CALL NUMBER(-.15,Y-.2,0.1,1000.*Y,90.0,0)
   enddo
   CALL SYMBOL(-0.30,3.5,0.14,ICHR3,INTEQ,90.0,14)
   THICK=3.0
   WDTH=25.0
   DO I=1,3
      TSQR=THICK*THICK
      WSQR=WDTH*WDTH
      PSI=100.99*TSQR
      CALL SYMBOL(0.6,PSI/1000.0,0.1,ICHR4,INTEQ,0.0,5)
      CALL NUMBER(999.0,999.0,0.10,THICK,0.0,0)
      CALL SYMBOL(999.0,999.0,0.10,ICHR5,INTEQ,0.0,4)
      CALL SYMBOL( 2.0 ,999.0,0.12,IBCD,1,0.0,-1)
      DO J=10,50
         WX=real(J)
         PSI=10099.0*TSQR/(WX*WX)
         CALL PLOT(WX/5.0,PSI/1000.0,2)
      enddo
      PSI=10099.0*81.0/WSQR
      CALL SYMBOL(9.2,PSI/1000.0,0.1,ICHR6,INTEQ,0.0,5)
      CALL NUMBER(999.0,999.0,0.10,WDTH,0.0,0)
      CALL SYMBOL(999.0,999.0,0.10,ICHR7,INTEQ,0.0,4)
      CALL SYMBOL( 9.0 ,999.0,0.12,IBCD,7,0.0,-1)
      DO J=5,50
         TX=real(J)
         TX=(50.0-TX)/5.0
         PSI=10099.0*TX*TX/WSQR
         CALL PLOT(TX,PSI/1000.0,2)
      enddo
      THICK=THICK+3.0
      WDTH=WDTH-5.0
   enddo
   CALL SYMBOL(3.3,8.5,.14,ICHR8,INTEQ,0.,29)
   CALL SYMBOL(3.1,8.2,.14,ICHR9,INTEQ,0.,32)
   CALL SYMBOL(3.1,7.9,.14,ICHR10,INTEQ,0.,32)
   CALL SYMBOL(3.1,7.6,.14,ICHR11,INTEQ,0.,32)
   CALL SYMBOL(3.3,7.0,.14,ICHR12,INTEQ,0.,29)
   CALL PLOT(0.0,0.0,999)
   STOP
END PROGRAM QA1
