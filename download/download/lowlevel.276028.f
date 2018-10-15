      PROGRAM QA3
C     PLOT THREE GRAPHS ILLUSTRATING SCALE, AXIS, AND LINE
C     CALIFORNIA COMPUTER PRODUCTS SAMPLE PROGRAM NUMBER 1      V097
      CHARACTER * 19 ICHR7
      CHARACTER * 21 ICHR6
      CHARACTER * 17 ICHR8
      ICHR6='CAR MODEL AGE (YEARS)'
      ICHR7='CAR VALUE (DOLLARS)'
      ICHR8='AVERAGE CAR VALUE'
C-----------------------------------------------------------------------
C     INITIALIZE GRAPHICS
      CALL DL_INIT(0, 10.0,10.0,0.75,0.75, 1.0)
      call dl_width(2)
C-----------------------------------------------------------------------
C     PLOT CAR VALUE CHART WITHOUT USING SCALE,AXIS,OR LINE
      X=1.0
C     PLOT X-AXIS
      DO 130 I=1,7
        CALL DL_PLOT(X-1.0,0.0,3)
        CALL DL_PLOT(X   , 0.0,2)
        CALL DL_PLOT(X   ,-0.1,2)
                   !    X     Y     H  F A   E   I
        CALL DL_NUMBER(X-.02,-0.25,0.1,X,0.0,0.0,-1)
 130    X=X+1.0

      !              X    Y     H    S    A  N
      CALL DL_SYMBOL(2.0,-0.5,0.14,ICHR6,0.0,21,-1)
C     PLOT Y-AXIS
      VALUE=1000.0
      DO 140  I=1,6
        Y=0.0015*VALUE
        CALL DL_PLOT(0.0,Y-1.5,3)
        CALL DL_PLOT(0.0,Y-.75,2)
        CALL DL_PLOT(-.1,Y-.75,2)
        CALL DL_PLOT(0.0,Y-.75,2)
        CALL DL_PLOT(0.0,Y    ,2)
        CALL DL_PLOT(-.1,Y    ,2)
        CALL DL_NUMBER(-0.7,Y,0.14,VALUE,0.0,0.0,-1)
 140    VALUE=VALUE+1000.0
      CALL DL_SYMBOL(-0.8,3.1,0.14,ICHR7,90.0,19,-1)
C     PLOT CURVES
      CALL DL_NEWPEN(2)
      CALL DL_COLOR(2)
      DO 150  I=2000,6000,500
        VALUE=I
        AGE=0.0
        CALL DL_PLOT(AGE,0.0015*VALUE,3)
        DO 150  J=1,84
          VALUE=VALUE*0.972
          AGE=AGE+0.08333
 150      CALL DL_PLOT(AGE,0.0015*VALUE,2)
      CALL DL_NEWPEN(0)
      CALL DL_WIDTH(3)
      call dl_color(5)
      CALL DL_SYMBOL(3.0,6.0,0.21,ICHR8,0.0,17,-1)
C-----------------------------------------------------------------------
      CALL DL_PAUSE()
      CALL DL_PLOT(0.0,0.0,999)
C-----------------------------------------------------------------------
      STOP
      END
