#ifdef TESTPRG
      PROGRAM CONVEX       ! test dl_triangc()
!--------------------------+----------------------------------------------------------------------------
      PARAMETER(N=12349)   !  N     (I): number of pointS
                           !            < 0 : IE,IBE,ITE arrays not used
                           !            > 0 : IE,IBE,ITE arrays used (normal)
      PARAMETER(NZZ=3*N)   !  NZZ   (I): dimension of NT array (>3*N)
      PARAMETER(NI=N)      !  NI    (I): dimension of I,J arrays (NI>=N)
      PARAMETER(NZ=3*N)    !  NZ    (I): dimension of IE,IBE,ITE array (>3*N)
      REAL X(N),Y(N)       !  X,Y   (R): arrays of X,Y POINTS
      INTEGER I(NI),J(NI)  !  I,J   (I): working arrays (dimensioned I(NI),J(NI))
!--------------------------+----------------------------------------------------------------------------
      INTEGER NT(NZZ,3)    !  NT    (I): array of indicies of triangulated points NT(NZZ,3)
                           !             corner 1 of triangle K = (X,Y) = (X(NT(K,1),Y(NT(K,1)))
                           !             corner 2 of triangle K = (X,Y) = (X(NT(K,2),Y(NT(K,2)))
                           !             corner 3 of triangle K = (X,Y) = (X(NT(K,3),Y(NT(K,3)))
                           !
                           !  NOTE: these arrays only used if N>0
      INTEGER IE(NZ,2)     !  IE    (I): array of indicies of each triangle edge IE(NZ,2)
      INTEGER IBE(NZ)      !  IBE   (I): edge flag array dimensioned IBE(NZ)
                           !             = 0 for interior edge
                           !             = 1 if ie is a boundary edge
      INTEGER ITE(NZ,4)    !  ITE   (I): array of indicies of the neighbor edges of
                           !             each triangle, dimensioned ITE(NZ,4)
      INTEGER M            !  M     (I): number of triangles stored in NT
      INTEGER L            !  L     (I): number of edges in IE,IBE,ITE
!--------------------------+----------------------------------------------------------------------------
      DOUBLE PRECISION DRAND
      EXTERNAL DRAND
      REAL XT(8),YT(8),SCR(24)
!-----------------------------------------------------------------------........-------------------------
                  !   xmax ymax xoff yoff zoom
      CALL DL_INIT( 0,12.0,12.0,0.2 , 0.3, 1.0)  ! create a drawing surface
      CALL SYSTEM('XREFRESH')
      CALL DL_COLOR(3)
      CALL DL_CLEAR()
      IDUM=101  ! seed for random numbers
      WRITE(*,*)'ENTER NUMBER OF POINTS (2<N<12350) TO TRIANGULATE'
      READ(*,*)IN
      IPAUSE=1  ! 1 = just draw one
      IPAUSE=0  ! 0 = step thru
!-----------------------------------------------------------------------........-------------------------
1     CONTINUE
      CALL DL_NEWPEN(-1)
      CALL DL_COLOR(4)
!-----------------------------------------------------------------------........-------------------------
      ! fill  points with random numbers and draw a small circle at each point
      IDUM2=DRAND(IDUM)
      DO 10 I10=1,ABS(IN)
         X(I10)=DRAND(IDUM)*10.0+0.5
         Y(I10)=DRAND(IDUM)*10.0+0.5
         CALL DL_ARC(X(I10),Y(I10),0.0,360.0,0.20,0.20)
         CALL DL_FLUSH()
10    CONTINUE
      IF(IPAUSE.EQ.0)CALL DL_PAUSE()
      CALL DL_FLUSH()
!-----------------------------------------------------------------------........-------------------------
      CALL DL_TRIANGC(X,Y,IN,NT,NZZ,M,I,J,NI,L,NZ,IE,IBE,ITE)
!-----------------------------------------------------------------------........-------------------------
      ! draw all the triangles
      CALL DL_COLOR(2)
      DO 20 I20=1,M
         CALL DL_MOVE(X(NT(I20,1)),Y(NT(I20,1))) ! corner 1 of triangle K = (X(NT(K,1),Y(NT(K,1)))
         CALL DL_DRAW(X(NT(I20,2)),Y(NT(I20,2))) ! corner 2 of triangle K = (X(NT(K,2),Y(NT(K,2)))
         CALL DL_DRAW(X(NT(I20,3)),Y(NT(I20,3))) ! corner 3 of triangle K = (X(NT(K,3),Y(NT(K,3)))
         CALL DL_DRAW(X(NT(I20,1)),Y(NT(I20,1))) ! corner 1 of triangle K = (X(NT(K,1),Y(NT(K,1)))
         CALL DL_FLUSH()
         IF(IPAUSE.EQ.0)CALL DL_PAUSE()
20    CONTINUE
!-----------------------------------------------------------------------........-------------------------
      IDASH=2
      IWIDTH=5
      CALL DL_NEWPEN(IDASH+10*IWIDTH)
!-----------------------------------------------------------------------........-------------------------
      IF(IN.LT.0)GOTO 90  ! if IE,IBE,ITE are not defined skip this
!-----------------------------------------------------------------------........-------------------------
      ! draw each edge (in different style if it defines the boundary)
      WRITE(*,*)'NUMBER OF EDGES=',L
      DO 40 I40=1,L   ! draw each edge found
         IF(IBE(I40).EQ.0)THEN  ! not a boundary
            CALL DL_COLOR(4)
            CALL DL_WIDTH(1)
            CALL DL_MOVE(X(IE(I40,1)),Y(IE(I40,1)))
            CALL DL_DRAW(X(IE(I40,2)),Y(IE(I40,2)))
         ELSE                   ! boundary point
            CALL DL_COLOR(6)
            CALL DL_WIDTH(5)
            CALL DL_MOVE(X(IE(I40,1)),Y(IE(I40,1)))
            CALL DL_DRAW(X(IE(I40,2)),Y(IE(I40,2)))
         ENDIF
        CALL DL_FLUSH()
         IF(IPAUSE.EQ.0)CALL DL_PAUSE()
40    CONTINUE
!-----------------------------------------------------------------------........-------------------------
      CALL DL_COLOR(0)
      CALL DL_WIDTH(2)
!-----------------------------------------------------------------------........-------------------------
      ! draw adjacent points for each edge
      ANG=0.0
      DO 50 I50=1,L
         CALL DL_NEWPEN(-1)
         CALL DL_WIDTH(3)
         CALL DL_COLOR(0)
         ! draw a base edge
         CALL DL_MOVE(X(IE(I50,1)),Y(IE(I50,1)))
         CALL DL_DRAW(X(IE(I50,2)),Y(IE(I50,2)))
         ! draw the neighboring edge points
         ! that is, the edges of the other triangles that
         ! have the first edge as a side.
         II=1
         IF(IBE(I50).EQ.0)THEN ! interior edges have 4
            IEDGES=4
         ELSE
            IEDGES=2
         ENDIF
         DO 60 I60=1,IEDGES
            I1=ITE(I50,I60)
            XT(II)=X(IE(I1,1))
            XT(II+1)=X(IE(I1,2))
            YT(II)=Y(IE(I1,1))
            YT(II+1)=Y(IE(I1,2))
            IF(I60.GT.3)THEN
               CALL DL_COLOR(1)
            ELSE
               CALL DL_COLOR(2)
            ENDIF
            CALL DL_MOVE( XT(II), YT(II))
            CALL DL_DRAW( XT(II+1), YT(II+1))
            II=II+2
60       CONTINUE
         CALL DL_COLOR(6)
         CALL DL_WIDTH(1)
         CALL DL_SHADE(XT,YT,4,1,2,0.15,ANG,SCR,1,0.0,1.0,0.0,1.0)
         IF(IBE(I50).EQ.0) THEN ! interior points have four
            CALL DL_COLOR(3)
         !              x        y n i l    d   t   w ma xm  dx  ym  dy
         CALL DL_SHADE(XT(5),YT(5),4,1,2,0.15,ANG,SCR,1,0.0,1.0,0.0,1.0)
         ENDIF

         CALL DL_FLUSH()
         IF(IPAUSE.EQ.0)CALL DL_PAUSE()
         ANG=ANG+10.0
50    CONTINUE
!-----------------------------------------------------------------------........-------------------------
90    CONTINUE
      WRITE(*,*)'0 = EXIT, 1=ANOTHER'
      READ(*,*,ERR=1,END=999)II
      IF(II.NE.1)THEN
         GOTO 999
      ELSE
        CALL DL_FLUSH()
        IF(IPAUSE.EQ.0)CALL DL_PAUSE()
        CALL DL_CLEAR()
        CALL DL_FLUSH()
        GOTO 1
      ENDIF
999   CONTINUE
      CALL DL_EXIT()
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DRAND(IY)
      INTEGER IY
!     DRAND IS A UNIFORM RANDOM NUMBER GENERATOR BASED  ON  THEORY  AND
!     SUGGESTIONS  GIVEN  IN  D.E. KNUTH (1969),  VOL  2.  THE INTEGER  IY
!     SHOULD BE INITIALIZED TO AN ARBITRARY INTEGER PRIOR TO THE FIRST
!     CALL TO DRAND. THE CALLING PROGRAM SHOULD NOT ALTER THE VALUE OF
!     IY BETWEEN  SUBSEQUENT CALLS TO DRAND.  VALUES OF DRAND WILL BE
!     RETURNED IN THE INTERVAL (0,1).
      INTEGER IA,IC,ITWO,M2,M,MIC
      DOUBLE PRECISION HALFM,S
      DOUBLE PRECISION DATAN,DSQRT
      DATA M2/0/,ITWO/2/
      SAVE M2, ITWO,IA,IC,MIC,S
!-----------------------------------------------------------------------
      IF (M2 .NE. 0) GOTO 20
!  IF FIRST ENTRY, COMPUTE MACHINE INTEGER WORD LENGTH
      M = 1
   10 CONTINUE
      M2 = M
      M = ITWO*M2
      IF (M .GT. M2) GOTO 10
      HALFM = M2
!  COMPUTE MULTIPLIER AND INCREMENT FOR LINEAR CONGRUENTIAL METHOD
      IA = 8*IDINT(HALFM*DATAN(1.D0)/8.D0) + 5
      IC = 2*IDINT(HALFM*(0.5D0-DSQRT(3.D0)/6.D0)) + 1
      MIC = (M2 - IC) + M2
!  S IS THE SCALE FACTOR FOR CONVERTING TO FLOATING POINT
      S = 0.5D0/HALFM
!-----------------------------------------------------------------------
!  COMPUTE NEXT RANDOM NUMBER
   20 CONTINUE
      IY = IY*IA
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHICH DO NOT ALLOW
!  INTEGER OVERFLOW ON ADDITION
      IF (IY .GT. MIC) IY = (IY - M2) - M2
      IY = IY + IC
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE THE
!  WORD LENGTH FOR ADDITION IS GREATER THAN FOR MULTIPLICATION
      IF (IY/2 .GT. M2) IY = (IY - M2) - M2
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE INTEGER
!  OVERFLOW AFFECTS THE SIGN BIT
      IF (IY .LT. 0) IY = (IY + M2) + M2
      DRAND = DFLOAT(IY)*S
      RETURN
      END
#endif
