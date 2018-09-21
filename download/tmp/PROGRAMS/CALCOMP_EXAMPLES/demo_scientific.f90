PROGRAM QA5
   USE M_calcomp
   DIMENSION XAR(10), YAR(10)
   DIMENSION R(19),ANGLE(19)
   CHARACTER*50 IBCD
100 FORMAT (A40,A2)
200 FORMAT(2F4.2)
!
!
   KIN = 50
   call make_c_qa5()
   open(unit=kin,file='qa5.dat')

   CALL PLOTS(0.0,8.5,0.0,11.0)
   CALL PLOT(0.0,-11.0,3)
!     CALL PLOT(0.0,-10.5,-3)
   CALL NFRAME()
! DRAW FRAME
   CALL PLOT(7.0,0.0,2)
   CALL PLOT(7.0,9.0,2)
   CALL PLOT(0.0,9.0,2)
   CALL PLOT(0.0,0.0,2)
! COMMENTS ARE INSERTED
   READ (KIN ,100)IBCD
   CALL SYMBOL (0.7,8.5,0.14,IBCD,INTEQ,0.0,40)
   READ (KIN ,100)IBCD
   CALL SYMBOL (0.7,4.25,0.14,IBCD,INTEQ,0.0,23)
   READ (KIN ,100)IBCD
   CALL SYMBOL (0.7,8.25,0.14,IBCD,INTEQ,0.0,23)
! TWO PAIRS OF AXES ARE DRAWN
   READ (KIN ,100)IBCD
   CALL AXIS  (1.0,4.75,IBCD,-1,5.0,0.0,0.0,1.0)
   CALL AXIS  (1.0,4.75,IBCD, 1,3.0,90.0,0.0,1.0)
   CALL AXIS  (1.0,0.75,IBCD,-1,5.0,0.0,0.0,1.0)
   CALL AXIS  (1.0,0.75,IBCD, 1,3.0,90.0,0.0,1.0)
! CURVX IS DRAWN
   CALL PLOT(1.0,4.75,-3)
   CALL CURVX (0.1,5.0,2.40,0.0,0.75,2.0,-0.525,3.0,0.075,4.0)
   CALL PLOT(-1.0,-4.75,-3)
! CURVY IS DRAWN
   CALL PLOT(1.0,0.75,-3)
   CALL CURVY (0.1,3.0,9.0,1.26,-6.0,2.52,1.0,3.78,0.0,0.0)
   CALL PLOT(-1.0,-0.75,-3)
! EQUATIONS ARE DRAWN
   READ (KIN ,100)IBCD
   CALL SYMBOL (3.0,7.75,0.09,IBCD,INTEQ,0.0,35)
   READ (KIN ,100)IBCD
   CALL SYMBOL (3.0,3.90,0.09,IBCD,INTEQ,0.0,27)
!     CALL PLOT(11.0,0.0,-3)
   CALL NFRAME()
!     DRAW FRAME
   CALL PLOT(7.0,0.0,2)
   CALL PLOT(7.0,9.0,2)
   CALL PLOT(0.0,9.0,2)
   CALL PLOT(0.0,0.0,2)
!     READ AXIS TITLES
!     DRAW AXIS
   READ (KIN ,100)IBCD
   CALL AXIS  (0.75,0.75,IBCD,-12,5.0,0.0,5.0,1.0)
   READ (KIN ,100)IBCD
   CALL AXIS  (0.75,0.75,IBCD, 9,7.0,90.0,0.0,100.0)
!     DRAW COMMENTS
   READ (KIN ,100)IBCD
   CALL SYMBOL (0.7,8.25,0.14,IBCD,INTEQ,0.0,34)
   CALL PLOT(5.0,7.8,3)
   CALL PLOT(5.1,7.8,2)
   READ (KIN ,100)IBCD
   CALL SYMBOL (5.2,7.80,0.09,IBCD,INTEQ,0.0, 6)
   INTEQ = 1
   CALL SYMBOL (5.0,7.60,0.10,IBCD,INTEQ,0.0,-1)
   INTEQ=999
   READ (KIN ,100)IBCD
   CALL SYMBOL (5.2,7.60,0.09,IBCD,INTEQ,0.0, 5)
! SMOOTHING
   CALL SMOOT (0.75,3.75,0)
   CALL SMOOT (1.75,2.5,-2)
   CALL SMOOT (2.25,5.75,-2)
   CALL SMOOT (2.75,7.0,-2)
   CALL SMOOT (3.25,7.25,-2)
   CALL SMOOT (4.25,6.75,-2)
   CALL SMOOT (4.75,3.75,-2)
   CALL SMOOT (5.75,4.75,-24)
! FLINE IS USED
   READ (KIN ,200)(XAR  (I),YAR  (I),I=1,8)
   XAR  (9)=0.0
   XAR  (10)=1.0
   YAR  (9)=0.0
   YAR  (10)=1.0
   CALL PLOT(0.75,3.25,3)
   CALL FLINE(XAR  ,YAR  ,-8,1,1,1)
!     CALL PLOT(11.0,0.0,-3)
   CALL NFRAME()
! DRAW FRAME
   CALL PLOT(7.0,0.0,2)
   CALL PLOT(7.0,9.0,2)
   CALL PLOT(0.0,9.0,2)
   CALL PLOT(0.0,0.0,2)
! DRAW COMMENTS
   READ (KIN ,100)IBCD
   CALL SYMBOL (0.7,8.25,0.14,IBCD,INTEQ,0.0,42)
   READ (KIN ,100)IBCD
   CALL SYMBOL (0.7,3.80,0.14,IBCD,INTEQ,0.0,22)
! AXIS IS DRAWN
   READ (KIN ,100)IBCD
   CALL AXIS  (1.0,4.75,IBCD,-8,5.0,0.0,0.0,25.0)
   READ (KIN ,40)(XAR(I),YAR(I),I=1,6)
40 FORMAT(F4.2,F4.0)
   CALL SCALG (YAR,3.0,6,1)
   READ (KIN ,100)IBCD
   CALL LGAXS (1.0,4.75,IBCD,11,3.0,90.0,YAR(7),YAR(8))
   CALL SCALE(XAR,5.0,6,1)
   CALL PLOT(1.0,4.75,-3)
   CALL LGLIN (XAR,YAR,6,1,0,1,1)
   CALL PLOT(-1.0,-4.75,-3)
! POLAR SUBROUTINE IS USED
   X=0.0
   DO 90 K=1,19
      THETA=X*0.0174533
      R(K)=2.0*(1.0-COS(THETA))
      ANGLE(K)=THETA
90 X=X+10.0
   CALL PLOT(5.0,0.75,-3)
   CALL POLAR(R,ANGLE,19,1,0,1,0.0,1.0)
   ANGL =30.0
   A=1.0
   DO  95 I=1,5
      THETA=ANGL *0.0174533
      XA=COS(THETA)
      YA=SIN(THETA)
      CALL PLOT(XA,YA,3)
      XB=1.1*XA
      YB=1.1*YA
      CALL PLOT(XB,YB,2)
      XC=XB+0.05*XA
      YC=YB+0.05*YA
      IF(I-3)50,50,60
60    A=1.5
50    BETA=1.570797-THETA
      XD=XC-0.105*A*COS(BETA)
      YD=YC+0.105*A*SIN(BETA)
      BANG=270.0+ANGL
      CALL NUMBER(XD,YD,0.105,ANGL ,BANG,-1)
95 ANGL =ANGL +30.0
   XX=0.0
   DO 400 I=1,19
      ANGLE(I) = XX*0.0174533
      R (I) = 1.0
400 XX=XX+10.0
   CALL POLAR (R,ANGLE,19,1,0,1,0.0,1.0)
   CALL PLOT(-5.0,-0.75,-3)
! AXIS IS DRAWN
   READ (KIN ,100)IBCD
   CALL AXIS  (1.0,0.75,IBCD,-1,4.0,0.0,4.0,-1.0)
   CALL AXIS  (5.0,0.75,IBCD,-1,1.0,0.0,0.0,1.0)
   READ (KIN ,100)IBCD
   CALL SYMBOL (3.75,3.5,0.09,IBCD,INTEQ,0.0,23)
   CALL PLOT(11.0,0.0,999)
   STOP
END PROGRAM QA5
subroutine make_c_qa5()
   integer,parameter :: io=40
   open(unit=io,file='qa5.dat')
   write(io,'(a)')'SAMPLE OF SCIENTIFIC SUBROUTINES PACKAGE'
   write(io,'(a)')'USING CURVY SUBROUTINE'
   write(io,'(a)')'USING CURVX SUBROUTINE'
   write(io,'(a)')''
   write(io,'(a)')'Y=0.075X**4-0.525X**3+0.75X**2+2.40'
   write(io,'(a)')'X=Y**3.78-6Y**2.52+9Y**1.26'
   write(io,'(a)')'SERVICE TIME'
   write(io,'(a)')'FREQUENCY'
   write(io,'(a)')'USING FLINE AND SMOOT SUBROUTINES'
   write(io,'(a)')'SMOOT'
   write(io,'(a)')'FLINE'
   write(io,'(a)')' 075 325'
   write(io,'(a)')' 175 200'
   write(io,'(a)')' 225 525'
   write(io,'(a)')' 275 650'
   write(io,'(a)')' 325 675'
   write(io,'(a)')' 425 625'
   write(io,'(a)')' 475 325'
   write(io,'(a)')' 575 425'
   write(io,'(a)')'USING SCALG, LGAXS, AND LGLIN SUBROUTINES'
   write(io,'(a)')'USING POLAR SUBROUTINE'
   write(io,'(a)')'ALTITUDE'
   write(io,'(a)')' 100 250'
   write(io,'(a)')' 200 110'
   write(io,'(a)')' 300 500'
   write(io,'(a)')' 400 900'
   write(io,'(a)')' 500 200'
   write(io,'(a)')' 600 140'
   write(io,'(a)')'TEMPERATURE'
   write(io,'(a)')''
   write(io,'(a)')'RADIUS=2*(1-COS(ANGLE))'
   close(unit=io)
end subroutine make_c_qa5