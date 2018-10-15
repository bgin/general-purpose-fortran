        PROGRAM ILLUS
C *** LAST REVISED ON 20-APR-1993 15:14:33.25
C *** SOURCE FILE: [LONGD.GRAPHICS]ILLUS.FOR
C
C       CREATED: DGL 4-APR-1985
C       MODIFIED: DGL 28-JAN-1991
C       + ADDED IMAGE CAPABILITY AND CHANGED SCALING
C       MODIFIED: DGL 20-APR-1993
C       + SET UP FOR ADOBE ILLUSTRATOR INPUT
C
C       THIS PROGRAM CONVERTS THE LONGLIB METAFILE
C       PRODUCED BY THE LONGLIB GRAPHICS LIBRARY
C       TO THE POSTSCRIPT LANGUAGE.
C
C       USES INTEGER*2 IN REGET (MAY BE CHANGED TO INTEGER IF DONE
C       IN THE MAIN LONGLIB FILE)
C
        CHARACTER(len=80) :: NAME=' '
C
C       VAX DEPENDENT ROUTINE TO GET COMMAND LINE FILE NAME
C       (CAN BE COMMENTED OUT)
C
C       IERR=LIB$GET_FOREIGN(NAME,,IFLAG)
C
C       DEFAULT NAME
C
        if (name.eq.' ') then
        NAME='for003.dat'       ! DEFAULT FILE NAME
        endif
        CALL LPLOT(NAME)
        STOP
        END
C
        SUBROUTINE LPLOT(NAME)
C
C       MAIN ROUTINE TO CONVERT PRINTER GRAPHICS FILE TO POSTSCRIPT
C
C       NAME    (CHARACTER)     FILE NAME
C
        CHARACTER*(*) NAME
C
C       DEFINITIONS FOR LINE TYPES
C
        CHARACTER*80 LINE
        CHARACTER*13 LINETYPE(10)
        DATA LINETYPE(1)/'[]'/,LINETYPE(2)/'[2]'/,LINETYPE(3)/'[4 4]'/
        DATA LINETYPE(4)/'[4 7]'/,LINETYPE(5)/'[2 2]'/
        DATA LINETYPE(6)/'[2 2 2 2 4]'/,LINETYPE(7)/'[6 2 3 2 4]'/
        DATA LINETYPE(8)/'[1 1]'/,LINETYPE(9)/'[1 1 4 4]'/
        DATA LINETYPE(10)/'[1 1 2 2 3 3]'/
C
        INTEGER PENUP
        REAL IX,IY,IXOLD,IYOLD
C
C       INITIALIZE VARIABLES
C
        LW=-1           ! LINE WIDTH (INVALID TO FORCE NEW WIDTH)
        LT=-1           ! LINE TYPE (INVALID TO FORCE NEW TYPE)
        LC=-1           ! LINE COLOR (INVALID TO FORCE NEW TYPE)
C
        AMX=288.0
        AMY=288.0
        MP=999
        IXOLD=0
        IYOLD=0
C
C       PENUP IS PEN CONTROL FLAG (0=NO SEGMENT, 2=PEN UP, 3=PEN DOWN)
C
        PENUP=0
        JPCNT=0
C
C       OPEN INPUT FILE PRODUCED BY LONGLIB GRAPHICS LIBRARY
C
        OPEN(UNIT=2,FILE=NAME,FORM='UNFORMATTED',STATUS='OLD')
C
C       OPEN OUTPUT FILE
C
        OPEN(UNIT=1,FILE='out.lis',FORM='FORMATTED',STATUS='NEW')
C
C       SEND INITIALIZATION CODES TO OUTPUT FILE
C
        write(*,*) 'ADOBE ILLUSTRATOR POSTSCRIPT CONVERTER'
        OPEN(UNIT=3,FILE='ADIPS.HEADER',FORM='FORMATTED',
     $          STATUS='OLD',ERR=310)
910     READ (3,200,END=300) LINE
200     FORMAT(A)
        WRITE(1,200) LINE
        GOTO 910
300     CONTINUE
        CLOSE(3)
310     jps = 1
C
C       TOP OF LONGLIB META-FILE READ LOOP
C
1000    CONTINUE
C
C       READ INITIAL COMMAND CODE AND PARAMETERS FROM LONGLIB META-FILE
C
        CALL REGET(M1,M2,M3,MP,2)
C
C       CHECK END OF FILE
C
        IF (M3.EQ.11) GOTO 110
        SX=M1
        SY=M2
        IF (M1.EQ.0.OR.M2.EQ.0) GOTO 110
  1     CONTINUE
C
C       READ COMMAND CODE AND PARAMETERS FROM LONGLIB META-FILE
C
        CALL REGET(M1,M2,M3,MP,2)
C
C       COORDINATE TRANSFORM
C
  2     Y1=M1/SX
        X1=M2/SY
        I3=M3
        IOLD=-1
C
C       CHECK END OF FILE
C
        IF (M3.GT.999) GOTO 999
C
C       EXECUTE META-FILE COMMAND
C
        GOTO (10,20,20,10,10,10,10,10,20,100,110) I3
10      GOTO 1
C
C       INTERPRET COMMAND
C
20      CONTINUE
C
C       "CALL dl_PLOT(X1,Y1,I3)"
C
C       CONVERT TO INTEGER 0.25/72'S INCH
C
        IX=Y1*72.0
        IY=780.0-X1*72.0
        IF (IY.LT.0) IY=0
        IF (I3.EQ.3) THEN
C
C       PEN UP MOVE
C
                IF (PENUP.EQ.2) THEN
                        KX=IDIGITS(IFIX(0.9999+IXOLD))
                        KY=IDIGITS(IFIX(0.9999+IYOLD))
                        WRITE (1,601) IXOLD,IYOLD
601                     FORMAT(g20.13,X,g20.13,' l'/'S')
                ENDIF
                PENUP=3
                JPCNT=0
        ELSE
C
C       PEN DOWN MOVE
C                               ! PEN DOWN MOVE
                IF (PENUP.EQ.3) THEN
                        KX=IDIGITS(IFIX(0.9999+IX))
                        KY=IDIGITS(IFIX(0.9999+IY))
                        WRITE (1,600) IXOLD,IYOLD
600                     FORMAT(g20.13,X,g20.13,' m')
602                     FORMAT(g20.13,X,g20.13,' l'/'S')
                        JPCNT=0
                ENDIF
                PENUP=2
                KX=IDIGITS(IFIX(0.9999+IX))
                KY=IDIGITS(IFIX(0.9999+IY))
C
C       OUTPUT LINE CONTINUATION.  IF # POINTS EXCEEDS 256, OUTPUT
C       ALSO, DON'T OUTPUT REDUNDANT POINTS
C
                IF (JPCNT.GT.256) THEN
                   WRITE (1,602) IX,IY
                   WRITE (1,600) IX,IY
                   JPCNT=0
                ELSE
                   IF (JPCNT.EQ.0.OR.(IX.NE.IXOLD.OR.IY.NE.IYOLD)) THEN
                      WRITE (1,603) IX,IY
603                   FORMAT(g20.13,X,g20.13,' l')
                      JPCNT=JPCNT+1
                   ENDIF
                ENDIF
        ENDIF
        IXOLD=IX
        IYOLD=IY
        GOTO 1
C
100     CONTINUE
C
C       "CALL DL_CLEAR"
C
        IF (PENUP.EQ.2) THEN
                KX=IDIGITS(IFIX(0.9999+IXOLD))
                KY=IDIGITS(IFIX(0.9999+IYOLD))
                WRITE (1,603) IXOLD,IYOLD
                PENUP=0
                JPCNT=0
        ENDIF
        JPS = JPS+1
        WRITE (1,402) JPS,JPS
402     FORMAT('%% dl_clear ',I2,X,I2)
        GOTO 1
C
C       SPECIAL COMMANDS
C
999     CONTINUE
        JPCNT=0
        IF (I3.GT.1999) GOTO 1999
        GOTO (1000,1001,1002,1003) I3-999
        GOTO 1
C
C       NEW PEN/LINE TYPE
C
1001    CONTINUE
        IF (M1.LT.0) M1=0
        IF (M1.GT.9) M1=MOD(M1,10)
        IF (M1.NE.LT) THEN
                IF (PENUP.EQ.2) THEN
                        KX=IDIGITS(IFIX(0.9999+IXOLD))
                        KY=IDIGITS(IFIX(0.9999+IYOLD))
                        WRITE (1,601) IXOLD,IYOLD
                        PENUP=0
                ENDIF
                WRITE (1,403) LINETYPE(M1+1)
                LT=M1
        ENDIF
403     FORMAT(A13,'0 d')
        GOTO 1
C
C       LINE COLOR
C
1002    CONTINUE
c       LW=M1
C
C       INCLUDE PEN COLOR AS GRAY SCALE (0-16) BLACK=16, WHITE=0
C
        IF (M2.NE.LC) THEN
C
C       "CALL dl_color(M2)"
C
C               WRITE (1,407) FLOAT(M2-1)/16.
C 407           FORMAT(X,F4.2,' setgray')
        ENDIF
        LC=M2
        GOTO 1
C
C       LINEWIDTH
C
1003    CONTINUE
C
C       "CALL DL_NEWPEN(I3)"
C
        IF (M1.LE.0) M1=1
C
C       NOTE: THE LINEWIDTH IS GRAPHICS DEVICE DEPENDENT AND MAY VARY
C       DEPENDING ON LINE ORIENTATION
C
        IF (M1.NE.LW) THEN
                IF (PENUP.EQ.2) THEN
                        KX=IDIGITS(IFIX(0.9999+IXOLD))
                        KY=IDIGITS(IFIX(0.9999+IYOLD))
                        WRITE (1,601) IXOLD,IYOLD
                        PENUP=0
                ENDIF
C
C       NEW LINE WIDTH
C
                WRITE (1,404) M1
404             FORMAT(I1,' w')
                LW=M1
        ENDIF
        GOTO 1
C
C       IMAGE MODE
C
1999    CONTINUE
C
C       CHECK EACH OPTION
C
        IF (I3.EQ.2000) THEN
                Y1=M1/SX
                X1=M2/SY
                AMX=X1*288
                AMY=Y1*288
        ENDIF
        IF (I3.EQ.2001) THEN
                INX=M1
                IF (INX.LT.1) INX=1
                INY=M2
                IF (INY.LT.1) INY=1
                NUM=INX*INY
                IY=IYOLD-NINT(AMY)
C
C       READ IMAGE DATA AND CONVERT TO HEXIDECIMAL
C
                IF (MOD(NUM,2).EQ.1) NUM=NUM+1
                NUM=NUM/2
                IC=0
                DO 2002 I=1,NUM
                        CALL REGET(M1,M2,M3,MP,2)
                        IF (M3.NE.2002) GOTO 2
                        CALL HEXIT(M1,M2,IC)
2002            CONTINUE
                IF (IC.GT.0) CALL HEXIT(M1,M2,-IC)
        ENDIF
        GOTO 1
C
C       CLOSE INPUT FILE
C
110     CLOSE(2)
C
        IF (PENUP.EQ.2) THEN
                KX=IDIGITS(IFIX(0.9999+IXOLD))
                KY=IDIGITS(IFIX(0.9999+IYOLD))
                WRITE (1,601) IXOLD,IYOLD
                PENUP=0
        ENDIF
C
C
        OPEN(UNIT=3,FILE='ADIPS.TRAILER',FORM='FORMATTED',
     $          STATUS='OLD',ERR=510)
!     $         STATUS='OLD',READONLY,ERR=510)
410     READ (3,200,END=500) LINE
        WRITE(1,200) LINE
        GOTO 410
500     CONTINUE
        CLOSE(3)
C
C       CLOSE OUTPUT FILE
C
510     CLOSE(1)
        END
C
        SUBROUTINE REGET(M1,M2,M3,MP,ILU)
C
C       READ DATA FROM LONGLIB META-FILE PRINTER DATA FILE
C
        INTEGER*2 M(128)
        MP=MP+3
        IF (MP.GT.128) THEN
                READ (ILU,ERR=99) M
                MP=3
        ENDIF
        M3=M(MP)
        M2=M(MP-1)
        M1=M(MP-2)
        IF (M3.EQ.999) GOTO 99
        RETURN
99      M3=11
        RETURN
        END
C
C
        INTEGER FUNCTION IDIGITS(I)
        N=1
        IF (I.GT.9) N=N+1
        IF (I.GT.99) N=N+1
        IF (I.GT.999) N=N+1
        IF (I.GT.9999) N=N+1
        IDIGITS=N
        RETURN
        END
C
C
        SUBROUTINE HEXIT(I,J,IC)
        INTEGER C(38)
        IF (IC.EQ.0) ICNT=1
        IF (IC.LT.0) THEN               ! DUMP OUTPUT BUFFER
                IF (ICNT.GT.1) WRITE (1,10) (C(I),I=1,ICNT-1)
                ICNT=1
                GOTO 100
        ENDIF
        IC=1
        C(ICNT)=I
        IF (C(ICNT).LT.0) C(ICNT)=0
        IF (C(ICNT).GT.255) C(ICNT)=255
        ICNT=ICNT+1
        C(ICNT)=J
        IF (C(ICNT).LT.0) C(ICNT)=0
        IF (C(ICNT).GT.255) C(ICNT)=255
        ICNT=ICNT+1
        IF (ICNT.GE.38) THEN
                WRITE (1,10) (C(I),I=1,38)
                ICNT=1
        ENDIF
10      FORMAT(X,38Z2.2)
100     RETURN
        END
