!===================================================================================================================================
module M_bessel
use M_journal, only : journal
private
public bes
public besi
public besj
public besj0
public besj1
public besk
public besy
public besy0
public test_suite_M_bessel
contains
!===================================================================================================================================
      SUBROUTINE BES(X,NO,KODE,RSLT1,RSLT2,T1,T2,IERR)                      ! BES.2     
!@(#) M_bessel::bes(3f):calculate Bessel functions J(X), Y(X), I(X), K(X) for real arguments and integer orders
!                                                                           ! BES.3     
!     SANDIA MATHEMATICAL PROGRAM LIBRARY                                   ! BES.4     
!     APPLIED MATHEMATICS DIVISION 2642                                     ! BES.5     
!     SANDIA LABORATORIES                                                   ! BES.6     
!     P. O. BOX 5800                                                        ! BES.7     
!     ALBUQUERQUE, NEW MEXICO  87115                                        ! BES.8     
!     CONTROL DATA 6600 VERSION 5.1, 10 DECEMBER 1973                       ! BES.9     
!                                                                           ! BES.10    
!     WRITTEN BY RONALD D. HALBGEWACHS, JULY, 1968.                         ! BES.11    
!     MODIFIED BY RDH FOR INCREASED ACCURACY, MAY 8,1973.                   ! BES.12    
!                                                                           ! BES.13    
!     WARD IMPLEMENTATION          S. J. ORBON   4/15/74                    ! BES.14    
!                                                                           ! BES.15    
!                THIS ROUTINE UTILIZES A DOUBLE PRECISION ARITHMETIC        ! BES.16    
!                SCHEME IN ORDER TO MAXIMIZE THE ACCURACY OBTAINABLE.       ! BES.17    
!                IF LOWER ACCURACY IS ACCEPTABLE, OR IF  HIGHER SPEED       ! BES.18    
!                IS IMPORTANT,THEN EITHER REMOVE THE DOUBLE PRECISION       ! BES.19    
!                VARIABLES FROM THIS ROUTINE, OR USE ONE OF THE ROUTINES    ! BES.20    
!                IN THE IBM SSP                                             ! BES.21    
!                                                                           ! BES.22    
!     ABSTRACT                                                              ! BES.23    
!                                                                           ! BES.24    
!             THIS ROUTINE CALCULATES THE BESSEL FUNCTIONS J(X),Y(X),       ! BES.25    
!             I(X), OR K(X) FOR REAL ARGUMENTS AND INTEGER ORDERS.          ! BES.26    
!             BACKWARDS RECURRENCE TECHNIQUES ARE USED FOR THE J(X)         ! BES.27    
!             AND I(X) FUNCTIONS EXCEPT FOR VERY SMALL ARGUMENTS,           ! BES.28    
!             WHERE DOUBLE PRECISION SERIES EVALUATION IS USED.             ! BES.29    
!             FORWARD RECURRENCE IS USED FOR THE Y(X) AND K(X)              ! BES.30    
!             FUNCTIONS WITH DOUBLE PRECISION CHEBYSHEV APPROXIMATIONS      ! BES.31    
!             USED FOR FUNCTION INITIALIZATION.  ACCURACY IS BETWEEN        ! BES.32    
!             THIRTEEN AND FOURTEEN SIGNIFICANT FIGURES.                    ! BES.33    
!             FOR SPECIFIC FUNCTIONS J0, J1, Y0, Y1, SEE BESJ0,             ! BES.34    
!             BESJ1, BESY0, AND BESY1.                                      ! BES.35    
!                                                                           ! BES.36    
!     DESCRIPTION OF PARAMETERS                                             ! BES.37    
!                                                                           ! BES.38    
!             X     = INPUT,REAL ARGUMENT OF THE BESSEL FUNCTION.           ! BES.39    
!                     THE ARGUMENT MAY BE POSITIVE,ZERO, OR NEGATIVE        ! BES.40    
!                     (NEG. ARG. FOR Y(X) OR K(X) PRODUCES ERROR            ! BES.41    
!                     MESSAGE SINCE RESULTS MAY BE COMPLEX.)                ! BES.42    
!                     RESTRICTION ON RANGE IS                               ! BES.43    
!                        FOR J(X), -1100.0 .LE. X .LE. 1100.0               ! BES.44    
!                        FOR Y(X), 0.0 .LE. X .LE. 1100.0                   ! BES.45    
!                        FOR I(X), -600.0 .LE. X .LE. 600.0                 ! BES.46    
!                        FOR K(X), 0.0 .LE. X .LE. 600.0                    ! BES.47    
!                                                                           ! BES.48    
!             NO    = INPUT,INTEGER ORDER OF FUNCTION DESIRED FOR A         ! BES.49    
!                     SINGLE VALUE TO BE RETURNED, OR THE MAXIMUM ORDER     ! BES.50    
!                     DESIRED (+ OR -) IF AN ARRAY OF VALUES IS TO BE       ! BES.51    
!                     RETURNED.                                             ! BES.52    
!                     LET XX = ABS(X).  THEN BOUNDS ON ORDERS ARE           ! BES.53    
!                                                                           ! BES.54    
!                     1. FOR 0.0 .LE. XX .LE. 0.025,                        ! BES.55    
!                          THE ABSOLUTE VALUE OF MAXIMUM ORDER,ANO,         ! BES.56    
!                          AND ARGUMENT SUPPLIED (ABS(X)) MUST              ! BES.57    
!                          SATISFY THE RELATION                             ! BES.58    
!                                                                           ! BES.59    
!                          log(GAMMA(ANO))-ANO*log(XX/2.0)   
!                          + log(XX)/2.0  .LE.  679.0  
!                                                                           ! BES.62    
!                          FOR A GIVEN ARGUMENT AND AN ORDER GREATER        ! BES.63    
!                          THAN THAT ALLOWED BY THE ABOVE RELATION          ! BES.64    
!                                                                           ! BES.65    
!                          JN(X) = 0.0, N.NE.0, AND =1.0 FOR N=0            ! BES.66    
!                          YN(X) = -INF                                     ! BES.67    
!                          IN(X) = 0.0, N.NE.0, AND =1.0 FOR N=0            ! BES.68    
!                          KN(X) = INF                                      ! BES.69    
!                     2. FOR 0.025 .LT. XX .LE. 0.20,                       ! BES.70    
!                          ABS(NO) .LE. INT(140.0*XX + 83.0)                ! BES.71    
!                     3. FOR 0.20 .LT. XX .LE. 1.0,                         ! BES.72    
!                         ABS(NO) .LE. INT(42.0*XX + 102.0)                 ! BES.73    
!                     4. FOR 1.0 .LT. XX .LE. 20.0,                         ! BES.74    
!                          ABS(NO) .LE. INT(0.02*XX**3 - 0.86*XX**2 +       ! BES.75    
!                                           17.15*XX + 124.0)               ! BES.76    
!                     5. FOR 20.0 .LT. XX .LE. 100.0,                       ! BES.77    
!                          ABS(NO) .LE. INT(2.75*XX + 228.0)                ! BES.78    
!                     6. FOR 100.0 .LT. XX .LE. 400.0,                      ! BES.79    
!                          ABS(NO) .LE. INT(1.67*XX + 336.0)                ! BES.80    
!                     7. FOR 400.0 .LT. XX .LE. 1100.0,                     ! BES.81    
!                          ABS(NO) .LE. INT(1.33*XX + 470.0)                ! BES.82    
!                                                                           ! BES.83    
!             KODE  = INPUT,INTEGER INDICATOR FOR THE PARTICULAR            ! BES.84    
!                     FUNCTION TO BE COMPUTED.                              ! BES.85    
!                                                                           ! BES.86    
!                       KODE = 10 -- FUNCTION J(X) ONLY                     ! BES.87    
!                            = 11 --          Y(X) ONLY                     ! BES.88    
!                            = 12 --          J(X) AND Y(X)                 ! BES.89    
!                                                                           ! BES.90    
!                            = 20 --          I(X) ONLY                     ! BES.91    
!                            = 21 --          K(X) ONLY                     ! BES.92    
!                            = 22 --          I(X) AND K(X)                 ! BES.93    
!                                                                           ! BES.94    
!             RSLT1 = OUTPUT,CONTAINS THE REAL FUNCTION VALUE FOR J(X)      ! BES.95    
!                     OR I(X) CORRESPONDING TO THE ORDER AND ARGUMENT       ! BES.96    
!                     SUPPLIED, DEPENDING ON THE KODE VALUE. THIS           ! BES.97    
!                     PARAMETER WOULD CONTAIN THE RESULT IF ONLY ONE        ! BES.98    
!                     FUNCTION VALUE IS TO BE RETURNED.                     ! BES.99    
!                                                                           ! BES.100   
!             RSLT2 = OUTPUT,CONTAINS THE REAL FUNCTION VALUE FOR Y(X)      ! BES.101   
!                     OR K(X) IN A MANNER SIMILAR TO RSLT1.                 ! BES.102   
!                                                                           ! BES.103   
!             T1    = OUTPUT,A WORK AREA WHICH WILL CONTAIN THE ARRAY OF    ! BES.104   
!                     REAL FUNCTION VALUES FOR J(X) OR I(X) OF ORDERS       ! BES.105   
!                     ZERO THROUGH NO, DEPENDING ON KODE.                   ! BES.106   
!                     T1 MUST BE DIMENSIONED IN THE CALLING PROGRAM AND     ! BES.107   
!                     MUST CONTAIN AT LEAST M CELLS OF STORAGE, WHERE       ! BES.108   
!                                                                           ! BES.109   
!                         M = MAX(ABS(NO),INT(2*ABS(X))) + 51               ! BES.110   
!                                                                           ! BES.111   
!                     IN USING THE ARRAY, T1(1) = FUNCTION OF ORDER 0,      ! BES.112   
!                     --- T1(NO+1) = FUNCTION OF ORDER NO.                  ! BES.113   
!                                                                           ! BES.114   
!             T2    = OUTPUT,SIMILAR TO T1 FOR THE FUNCTIONS Y(X) OR        ! BES.115   
!                     K(X). AN EXCEPTION IS THAT IF ONLY J(X) OR I(X)       ! BES.116   
!                     ARE CALLED, THEN T2 NEEDS NO DIMENSION IN THE         ! BES.117   
!                     CALLING PROGRAM, BUT THE PARAMETER MUST STILL         ! BES.118   
!                     APPEAR IN THE CALLING SEQUENCE.  OTHERWISE, T2        ! BES.119   
!                     MUST BE DIMENSIONED AT LEAST M.                       ! BES.120   
!                                                                           ! BES.121   
!             IERR  = OUTPUT,ERROR FLAG FOR THE CONDITIONS                  ! BES.122   
!                                                                           ! BES.123   
!                      -- NORMAL CODE                                       ! BES.124   
!                         =0, NORMAL - NO ERRORS                            ! BES.125   
!                      -- ABNORMAL CODES                                    ! BES.126   
!                         =1, ARGUMENT OUT OF RANGE                         ! BES.127   
!                         =2, ORDER TOO LARGE FOR ARGUMENT SUPPLIED         ! BES.128   
!                         =3, ARGUMENT TOO LARGE FOR I(X) AND K(X)          ! BES.129   
!                         =4, NEGATIVE ARGUMENTS FOR Y(X) OR K(X)           ! BES.130   
!                         =5, INCORRECT PARAMETER KODE                      ! BES.131   
!                                                                           ! BES.132   
!     BS IS DOCUMENTED FOR THE ORIGINAL VERSION IN SC-M-69-336             ! BES.133   
!                                                                           ! BES.134   
      implicit doubleprecision (a-h, o-z)
!      DOUBLE PRECISION DX,DXX,DX2,TEMP,DCHK,PI,EULER,A0,A,B,C               ! BES.135   
!      DOUBLE PRECISION SUMJIN,DXORD,DXK                                     ! BES.136   
      doubleprecision DX,DXX,DX2,TEMP,DCHK,PI,EULER,A0,A,B,C            
      doubleprecision  SUMJIN,DXORD,DXK                                 
      DIMENSION  T1(*),T2(*),A(18),B(21),C(19)                              ! BES.137   
      DATA  PI/3.1415926535897932384626434D00/,                            &! BES.138   
     &      EULER/0.57721566490153286060651209D00/                          ! BES.139   
      DATA (A(I),I=1,18)/           0.15999999999999999996D+02,            &! BES.140   
     & 0.96000000000000000392D+02,  0.20859259259259257733D+03,            &! BES.141   
     & 0.23703703703703736105D+03,  0.16626725925925503356D+03,            &! BES.142   
     & 0.79290469135839064596D+02,  0.27400431054739774751D+02,            &! BES.143   
     & 0.71803471197186985165D+01,  0.14763245818980230758D+01,            &! BES.144   
     & 0.24456169711179137024D+00,  0.33342447857340252160D-01,            &! BES.145   
     & 0.380697152755597312D-02,    0.36933105872797696D-03,               &! BES.146   
     & 0.30849206583296D-04,        0.222445483065344D-05,                 &! BES.147   
     & 0.14811194720256D-06,        0.635655159808D-08,                    &! BES.148   
     & 0.68719476736D-09/                                                   ! BES.149   
      DATA (B(I),I=1,21)/           0.98813927043864915927D+00,            &! BES.150   
     & -.11277407316570291310D-01,  0.5340716774420596D-03,                &! BES.151   
     & -.435456758226194D-04,       0.488456084594416D-05,                 &! BES.152   
     & -.68181429589264D-06,        0.11199290865952D-06,                  &! BES.153   
     & -.2089895303616D-07,         0.4325898624D-08,                      &! BES.154   
     & -.97628537856D-09,           0.23715879424D-09,                     &! BES.155   
     & -.6140542976D-10,            0.1680852992D-10,                      &! BES.156   
     & -.481501184D-11,             0.144621568D-11,                       &! BES.157   
     & -.47808512D-12,              0.1572864D-12,                         &! BES.158   
     & -.31457280D-13,              0.9175040D-14,                         &! BES.159   
     & -.1310720D-13,               0.524288D-14/                           ! BES.160   
      DATA (C(I),I=1,19)/          -0.73804295108687506715D-01,            &! BES.161   
     & 0.11366785079620443739D+02, -0.65838973034256501712D+02,            &! BES.162   
     & 0.14119145750221817396D+03, -0.15929975325701922684D+03,            &! BES.163   
     & 0.11122328958866232246D+03, -0.52866443153661476803D+02,            &! BES.164   
     & 0.18223597971689250243D+02, -0.47661469297599122637D+01,            &! BES.165   
     & 0.97840283604837466112D+00, -0.16191400580768858112D+00,            &! BES.166   
     & 0.2212712874183229440D-01,  -0.2606907391286968320D-02,             &! BES.167   
     & 0.316831265267384320D-03,   -0.6102072906743808D-04,                &! BES.168   
     & 0.1658373309202432D-04,     -0.3439710458347520D-05,                &! BES.169   
     & 0.338099825541120D-06,      -0.343597383680D-09/                     ! BES.170   
      IERR=0                                                                ! BES.171   
!      bigval=1.0d322
      bigval=1.0d307
      IF (KODE.LT.10) GO TO 600                                             ! BES.172   
      SIGN = 1.0                                                            ! BES.173   
      KO = IABS(NO) + 1                                                     ! BES.174   
      XSQFR = 0.25*X*X                                                      ! BES.175   
!     ---------                                                             ! BES.176   
!     INITIAL CHECK OF ORDER-ARGUMENT RANGE TO DETERMINE IF ORDER IS OUT    ! BES.177   
!     OF RANGE FOR THE GIVEN ARGUMENT.                                      ! BES.178   
!                                                                           ! BES.179   
!     ---------                                                             ! BES.180   
      IF (X) 3,27,3                                                         ! BES.181   
    3 XCHK = ABS(X)                                                         ! BES.182   
      IF (XCHK-1100.0) 4,18,608                                             ! BES.183   
    4 IF (NO.EQ.0) GO TO 25                                                 ! BES.184   
      IF (XCHK - 0.025) 5,5,6                                               ! BES.185   
    5 ANO = IABS(NO)                                                        ! BES.186   
      AX = ANO*log(2.0*ANO/XCHK) + 0.5*log(2.0*real(PI)/ANO) - ANO + 1.0/(12.0*ANO) + log(XCHK)/2.0                            
      RSLT1 = AX                                                            ! BES.189   
      IF (AX-679.0) 25,25,606                                               ! BES.190   
    6 IF (XCHK-0.20) 7,7,8                                                  ! BES.191   
    7 LARGOR =  int(140.0*XCHK + 83.0)                                      ! BES.192   
      GO TO 24                                                              ! BES.193   
    8 IF (XCHK-1.0) 9,9,10                                                  ! BES.194   
    9 LARGOR =  int(42.0*XCHK + 102.0)                                      ! BES.195   
      GO TO 24                                                              ! BES.196   
   10 IF (XCHK-20.0) 11,11,12                                               ! BES.197   
   11 LARGOR =  int(((0.02*XCHK-0.86)*XCHK+17.15)*XCHK+124.0)               ! BES.198   
      GO TO 24                                                              ! BES.199   
   12 IF (XCHK-100.0) 13,13,14                                              ! BES.200   
   13 LARGOR =  int(2.75*XCHK + 228.0)                                      ! BES.201   
      GO TO 24                                                              ! BES.202   
   14 IF (XCHK-400.0) 16,16,18                                              ! BES.203   
   16 LARGOR =  int(1.67*XCHK + 336.0)                                      ! BES.204   
      GO TO 24                                                              ! BES.205   
   18 LARGOR =  int(1.33*XCHK + 470.0)                                      ! BES.206   
   24 IF (IABS(NO)-LARGOR) 25,25,606                                        ! BES.207   
   25 XX=X                                                                  ! BES.208   
!     ---------                                                             ! BES.209   
!     DETERMINE WHICH SET OF FUNCTIONS IS TO BE CALCULATED.                 ! BES.210   
!     ---------                                                             ! BES.211   
   27 MASK1 = KODE/10                                                       ! BES.212   
      IF (MASK1-2) 30,31,600                                                ! BES.213   
   30 MASK2 = KODE-10                                                       ! BES.214   
      GO TO 32                                                              ! BES.215   
   31 MASK2 = KODE-20                                                       ! BES.216   
   32 IF (MASK2-2) 34,36,600                                                ! BES.217   
   34 IF (MASK2) 600,37,42                                                  ! BES.218   
!     ---------                                                             ! BES.219   
!     CHECK FUNCTIONS J(X) AND I(X) FOR ZERO ARGUMENT.                      ! BES.220   
!     ---------                                                             ! BES.221   
   36 IF (X) 604,38,59                                                      ! BES.222   
   37 IF (X) 58,38,59                                                       ! BES.223   
   38 IF (NO) 54,40,54                                                      ! BES.224   
   40 T1(1) = 1.0                                                           ! BES.225   
      RSLT1 = 1.0                                                           ! BES.226   
      IF (MASK2.EQ.0) RETURN                                                ! BES.227   
!     ---------                                                             ! BES.228   
!     CHECK FUNCTIONS Y(X) AND K(X) FOR ZERO ARGUMENT.                      ! BES.229   
!     The functions y0, y1, and yn have logarithmic  singularities
!     at the origin, so they treat zero and negative arguments the
!     way log does.  Such  arguments  are unexceptional for j0, j1, and jn.
!     ---------                                                             ! BES.230   
   42 IF (X) 604,44,59                                                      ! BES.231   
   44 IF (MASK1.EQ.2) GO TO 50                                              ! BES.232   
      DO 48 IK=1,KO                                                         ! BES.233   
   48 T2(IK) = -bigval
      RSLT2  = -bigval
      RETURN                                                                ! BES.236   
   50 DO 52 IK=1,KO                                                         ! BES.237   
   52 T2(IK) = bigval
      RSLT2  = bigval
      RETURN                                                                ! BES.240   
!     ---------                                                             ! BES.241   
!     FILL OUT ARRAY FOR J(X) OR I(X) WHEN (NO.NE.0).                       ! BES.242   
!     ---------                                                             ! BES.243   
   54 DO 56 IK=2,KO                                                         ! BES.244   
   56 T1(IK) = 0.0                                                          ! BES.245   
      RSLT1 = 0.0                                                           ! BES.246   
      T1(1) = 1.0                                                           ! BES.247   
      IF (MASK2.EQ.0) RETURN                                                ! BES.248   
      GO TO 44                                                              ! BES.249   
   58 X = ABS(X)                                                            ! BES.250   
   59 MO = IABS(NO)                                                         ! BES.251   
      IMO = MO                                                              ! BES.252   
      IF (X-1.0) 60,71,71                                                   ! BES.253   
   60 IF (MASK1.EQ.2) GO TO 175                                             ! BES.254   
!     ---------                                                             ! BES.255   
!     USE SERIES TO DETERMINE J(N) AND J(N-1) WHEN ARGUMENT IS SMALL,       ! BES.256   
!     THEN USE RECURRENCE TO DETERMINE REMAINING FUNCTION VALUES.           ! BES.257   
!     ---------                                                             ! BES.258   
      XORD = MO                                                             ! BES.259   
      DXORD = XORD                                                          ! BES.260   
      IF (MO.GT.1) GO TO 61                                                 ! BES.261   
      DX2 = 1.0D00                                                          ! BES.262   
      A0 = 1.0D00                                                           ! BES.263   
      ILOOP = 1                                                             ! BES.264   
      GO TO 63                                                              ! BES.265   
   61 A0 = 1.0D00                                                           ! BES.266   
      IEND = MO-1                                                           ! BES.267   
      DO 62 IK=1,IEND                                                       ! BES.268   
      XK = IK                                                               ! BES.269   
      DXK = XK                                                              ! BES.270   
      A0 = A0*DXK                                                           ! BES.271   
   62 CONTINUE                                                              ! BES.272   
      DX2 = 1.0D00/(A0*DXORD)                                               ! BES.273   
      A0 = 1.0D00/A0                                                        ! BES.274   
      ILOOP = 1                                                             ! BES.275   
   63 SUMJIN = DX2                                                          ! BES.276   
      DX = X                                                                ! BES.277   
      DXX = 0.25D00*DX*DX                                                   ! BES.278   
      DO 66 IK=1,200                                                        ! BES.279   
      XK = IK                                                               ! BES.280   
      DXK = XK                                                              ! BES.281   
      TEMP = -DX2*DXX/(DXK*(DXORD+DXK))                                     ! BES.282   
      SUMJIN = SUMJIN + TEMP                                                ! BES.283   
      IF (SUMJIN) 64,65,64                                                  ! BES.284   
   64 DCHK = abs(TEMP/SUMJIN)    
      IF (DCHK - 1.0D-20) 67,65,65                                          ! BES.286   
   65 DX2 = TEMP                                                            ! BES.287   
   66 CONTINUE                                                              ! BES.288   
   67 IF (ILOOP.GT.1) GO TO 68                                              ! BES.289   
      T1(KO) = real(SUMJIN*(0.5D00*DX)**MO)                                 ! BES.290   
      IF (MO.EQ.0) GO TO 83                                                 ! BES.291   
      ILOOP = 2                                                             ! BES.292   
      DX2 = A0                                                              ! BES.293   
      XORD = MO-1                                                           ! BES.294   
      DXORD = XORD                                                          ! BES.295   
      GO TO 63                                                              ! BES.296   
   68 T1(KO-1) = real(SUMJIN*(0.5D00*DX)**(MO-1))                           ! BES.297   
      IF (KO.LE.2) GO TO 83                                                 ! BES.298   
      IEND = KO-2                                                           ! BES.299   
      DO 69 IK=1,IEND                                                       ! BES.300   
      NK = KO-IK                                                            ! BES.301   
      XNKM1 = NK-1                                                          ! BES.302   
      T1(NK-1) = 2.0*XNKM1 *T1(NK)/X - T1(NK+1)                             ! BES.303   
   69 CONTINUE                                                              ! BES.304   
      GO TO 83                                                              ! BES.305   
   71 IF (MASK2.EQ.0) GO TO 74                                              ! BES.306   
!     ---------                                                             ! BES.307   
!     DETERMINE STARTING LOCATION OF RECURRENCE IF Y(X) OR K(X)             ! BES.308   
!     ARE TO BE FOUND.                                                      ! BES.309   
!     ---------                                                             ! BES.310   
      JO = 2* int(X)                                                        ! BES.311   
      IF (IMO - JO) 72,73,73                                                ! BES.312   
   72 IMO = JO                                                              ! BES.313   
   73 IMO = IMO + 51                                                        ! BES.314   
      GO TO 78                                                              ! BES.315   
!     ---------                                                             ! BES.316   
!     DETERMINE STARTING LOCATION FOR RECURRENCE OF J(X).                   ! BES.317   
!     ---------                                                             ! BES.318   
   74 JO = 2* int(X)                                                        ! BES.319   
      IF (IMO - JO) 75,76,76                                                ! BES.320   
   75 IMO = JO                                                              ! BES.321   
   76 IMO = IMO + 51                                                        ! BES.322   
!     ---------                                                             ! BES.323   
!     INITIALIZE VALUES FOR J(X) AND Y(X)                                   ! BES.324   
!     ---------                                                             ! BES.325   
   78 T1(IMO) = 0.0                                                         ! BES.326   
      T1(IMO-1) = 1.0d-200                                                  ! BES.327   
      IF (MASK1.EQ.2) GO TO 151                                             ! BES.328   
      F = 2*(IMO-1)                                                         ! BES.329   
      IMO = IMO - 3                                                         ! BES.330   
      I2 = IMO                                                              ! BES.331   
   79 F = F - 2.0                                                           ! BES.332   
!     ---------                                                             ! BES.333   
!     RECURRENCE USED FOR FUNCTION VALUES.                                  ! BES.334   
!     VARIABLE SUM IS USED TO DETERMINE ADJUSTMENT FACTOR                   ! BES.335   
!     ON RECURRED VALUES.                                                   ! BES.336   
!     ---------                                                             ! BES.337   
      T1(I2+1) = F/X*T1(I2+2) - T1(I2+3)                                    ! BES.338   
      IF (I2) 80,81,80                                                      ! BES.339   
   80 I2 = I2-1                                                             ! BES.340   
      GO TO 79                                                              ! BES.341   
   81 SUM = T1(1)                                                           ! BES.342   
      DO 82 J=3,IMO,2                                                       ! BES.343   
   82 SUM = SUM + 2.0*T1(J)                                                 ! BES.344   
      F = 1.0/SUM                                                           ! BES.345   
   83 IF (NO) 86,84,84                                                      ! BES.346   
   84 IF (XX) 90,32,92                                                      ! BES.347   
   86 IF (XX) 92,32,90                                                      ! BES.348   
   90 SIGN = -SIGN                                                          ! BES.349   
   92 IF (MASK2.EQ.0) GO TO 93                                              ! BES.350   
      GO TO 300                                                             ! BES.351   
   93 IF (X - 1.0) 96,94,94                                                 ! BES.352   
   94 DO 95 J=1,KO                                                          ! BES.353   
   95 T1(J) = T1(J)*F                                                       ! BES.354   
   96 IF (MO.EQ.0) GO TO 98                                                 ! BES.355   
      DO 97 J=2,KO,2                                                        ! BES.356   
   97 T1(J) = T1(J)*SIGN                                                    ! BES.357   
   98 RSLT1 = T1(KO)                                                        ! BES.358   
      X = XX                                                                ! BES.359   
      RETURN                                                                ! BES.360   
!     ---------                                                             ! BES.361   
!     INITIALIZE STARTING VALUES FOR I(X) AND K(X) RECURRENCE.              ! BES.362   
!     ---------                                                             ! BES.363   
  151 IF (X-600.0) 152,152,602                                              ! BES.364   
  152 F = 2*(IMO-1) - 2                                                     ! BES.365   
      IMO = IMO - 3                                                         ! BES.366   
      I2 = IMO                                                              ! BES.367   
  153 T1(I2+1) = F/X*T1(I2+2) + T1(I2+3)                                    ! BES.368   
      IF (I2) 154,155,154                                                   ! BES.369   
  154 I2 = I2-1                                                             ! BES.370   
      F=F-2.                                                                ! BES.371   
      GO TO 153                                                             ! BES.372   
  155 SUM = T1(1)                                                           ! BES.373   
      DO 170 J=2,IMO                                                        ! BES.374   
  170 SUM = SUM + 2.0*T1(J)                                                 ! BES.375   
      F = 1.0/SUM*EXP(X)                                                    ! BES.376   
      IF (XX) 171,32,172                                                    ! BES.377   
  171 SIGN = -SIGN                                                          ! BES.378   
  172 DO 173 J=1,KO,2                                                       ! BES.379   
      T1(J) = T1(J)*F                                                       ! BES.380   
  173 T1(J+1) = T1(J+1)*F*SIGN                                              ! BES.381   
      RSLT1 = T1(KO)                                                        ! BES.382   
      IF (MASK2.NE.0) GO TO 400                                             ! BES.383   
      X = XX                                                                ! BES.384   
      RETURN                                                                ! BES.385   
  175 XORD = MO                                                             ! BES.386   
      DXORD = XORD                                                          ! BES.387   
      IF (MO.GT.1) GO TO 177                                                ! BES.388   
      DX2 = 1.0D00                                                          ! BES.389   
      A0 = 1.0D00                                                           ! BES.390   
      ILOOP = 1                                                             ! BES.391   
      GO TO 180                                                             ! BES.392   
  177 A0 = 1.0D00                                                           ! BES.393   
      IEND = MO - 1                                                         ! BES.394   
      DO 178 IK=1,IEND                                                      ! BES.395   
      XK = IK                                                               ! BES.396   
      DXK = XK                                                              ! BES.397   
      A0 = A0*DXK                                                           ! BES.398   
  178 CONTINUE                                                              ! BES.399   
      DX2 = 1.0D00/(A0*DXORD)                                               ! BES.400   
      A0 = 1.0D00/A0                                                        ! BES.401   
      ILOOP = 1                                                             ! BES.402   
  180 SUMJIN = DX2                                                          ! BES.403   
      DX = X                                                                ! BES.404   
      DXX = 0.25D00*DX*DX                                                   ! BES.405   
      DO 182 IK=1,200                                                       ! BES.406   
      XK = IK                                                               ! BES.407   
      DXK = XK                                                              ! BES.408   
      TEMP = DX2*DXX/(DXK*(DXORD+DXK))                                      ! BES.409   
      SUMJIN = SUMJIN + TEMP                                                ! BES.410   
      DCHK = abs(TEMP/SUMJIN)     
      IF (DCHK - 1.0D-20) 184,181,181                                       ! BES.412   
  181 DX2 = TEMP                                                            ! BES.413   
  182 CONTINUE                                                              ! BES.414   
  184 IF (ILOOP.GT.1) GO TO 185                                             ! BES.415   
      T1(KO) = real(SUMJIN*(0.5D00*DX)**MO)                                 ! BES.416   
      IF (MO.EQ.0) GO TO 188                                                ! BES.417   
      ILOOP = 2                                                             ! BES.418   
      DX2 = A0                                                              ! BES.419   
      XORD = MO-1                                                           ! BES.420   
      DXORD = XORD                                                          ! BES.421   
      GO TO 180                                                             ! BES.422   
  185 T1(KO-1) = real(SUMJIN*(0.5D00*DX)**(MO-1))                           ! BES.423   
      IF (KO.LE.2) GO TO 188                                                ! BES.424   
      IEND = KO-2                                                           ! BES.425   
      DO 187 IK=1,IEND                                                      ! BES.426   
      NK = KO-IK                                                            ! BES.427   
      XNKM1 = NK-1                                                          ! BES.428   
      T1(NK-1) = 2.0*XNKM1 *T1(NK)/X + T1(NK+1)                             ! BES.429   
  187 CONTINUE                                                              ! BES.430   
  188 IF (XX) 189,32,190                                                    ! BES.431   
  189 SIGN = -SIGN                                                          ! BES.432   
  190 IF (MO.EQ.0) GO TO 194                                                ! BES.433   
      DO 192 J=2,KO,2                                                       ! BES.434   
  192 T1(J) = T1(J)*SIGN                                                    ! BES.435   
  194 RSLT1 = T1(KO)                                                        ! BES.436   
      IF (MASK2.NE.0) GO TO 400                                             ! BES.437   
      X = XX                                                                ! BES.438   
      RETURN                                                                ! BES.439   
!     ---------                                                             ! BES.440   
!     EVALUATE Y0 AND Y1 TO START RECURRENCE.                               ! BES.441   
!     ---------                                                             ! BES.442   
  300 IF (X-1.0) 3001,301,301                                               ! BES.443   
 3001 DX = X                                                                ! BES.444   
      DXX = DX*DX/64.0D00                                                   ! BES.445   
      DX2 = 1.0D00                                                          ! BES.446   
      TEMP = C(1)                                                           ! BES.447   
      DO 3010 J=2,19                                                        ! BES.448   
      DX2 = DX2*DXX                                                         ! BES.449   
      A0 = C(J)*DX2                                                         ! BES.450   
      TEMP = TEMP + A0                                                      ! BES.451   
      IF (TEMP) 3005,3010,3005                                              ! BES.452   
 3005 DCHK = abs(A0/TEMP)   
      IF (DCHK - 1.0D-20) 3015,3010,3010                                    ! BES.454   
 3010 CONTINUE                                                              ! BES.455   
 3015 A0 = (2.0D00/PI)*LOG(DX)  
      T2(1) = A0*T1(1) + TEMP                                               ! BES.457   
      GO TO 321                                                             ! BES.458   
  301 DO 302 J=1,IMO                                                        ! BES.459   
  302 T1(J) = T1(J)*F                                                       ! BES.460   
      SUMJ1 = 0.0                                                           ! BES.461   
      SUMJ2 = 0.0                                                           ! BES.462   
      IF (IMO.LE.80) GO TO 305                                              ! BES.463   
      IF (KO - JO) 303,304,304                                              ! BES.464   
  303 KEND = JO/2                                                           ! BES.465   
      GO TO 306                                                             ! BES.466   
  304 KEND = KO/2                                                           ! BES.467   
      GO TO 306                                                             ! BES.468   
  305 KEND = IMO/2                                                          ! BES.469   
  306 DO 307 N=1,KEND,2                                                     ! BES.470   
      XN = N                                                                ! BES.471   
  307 SUMJ1 = SUMJ1 + T1(2*N+1)/XN                                          ! BES.472   
      DO 308 N=2,KEND,2                                                     ! BES.473   
      XN = N                                                                ! BES.474   
  308 SUMJ2 = SUMJ2 + T1(2*N+1)/XN                                          ! BES.475   
      SUMJN = 2.0*(SUMJ2-SUMJ1)                                             ! BES.476   
      T2(1) = 2.0/PI*(T1(1)*(log(X/2.0) + EULER) - SUMJN)   
  321 IF (MO.GT.0) GO TO 309                                                ! BES.478   
      RSLT1 = T1(1)                                                         ! BES.479   
      RSLT2 = T2(1)                                                         ! BES.480   
      X = XX                                                                ! BES.481   
      RETURN                                                                ! BES.482   
  309 T2(2) = (T1(2)*T2(1) - 2.0/(PI*X))/T1(1)                              ! BES.483   
      IF (MO.EQ.1) GO TO 311                                                ! BES.484   
      NORD = KO-1                                                           ! BES.485   
      DO 310 N=2,NORD                                                       ! BES.486   
      XN = N-1                                                              ! BES.487   
      T2(N+1) = (2.0*XN)/X*T2(N) - T2(N-1)                                  ! BES.488   
  310 CONTINUE                                                              ! BES.489   
  311 DO 312 J=2,KO,2                                                       ! BES.490   
  312 T2(J) = T2(J)*SIGN                                                    ! BES.491   
      RSLT2 = T2(KO)                                                        ! BES.492   
      IF (MASK2.EQ.1) GO TO 315                                             ! BES.493   
      DO 313 J=2,KO,2                                                       ! BES.494   
  313 T1(J) = T1(J)*SIGN                                                    ! BES.495   
      RSLT1 = T1(KO)                                                        ! BES.496   
  315 X = XX                                                                ! BES.497   
      RETURN                                                                ! BES.498   
  400 DX = X                                                                ! BES.499   
      DX2 = 1.0D00                                                          ! BES.500   
      IF (X-6.0)  410,410,440                                               ! BES.501   
  410 DXX = DX*DX/64.0D00                                                   ! BES.502   
      TEMP = 0.0D00                                                         ! BES.503   
      DO 420 J=1,18                                                         ! BES.504   
      DX2 = DX2*DXX                                                         ! BES.505   
      A0 = A(J)*DX2                                                         ! BES.506   
      TEMP = TEMP + A0                                                      ! BES.507   
      DCHK = A0/TEMP                                                        ! BES.508   
      IF (DCHK - 1.0D-20) 430,420,420                                       ! BES.509   
  420 CONTINUE                                                              ! BES.510   
  430 A0 = -(EULER + LOG(0.5D00*DX))   
      T2(1) = A0*T1(1) + TEMP                                               ! BES.512   
      IF (NO.EQ.0) GO TO 490                                                ! BES.513   
      GO TO 480                                                             ! BES.514   
  440 DXX = 10.0D00/DX - 1.0D00                                             ! BES.515   
      TEMP = B(1)                                                           ! BES.516   
      DO 460 J=2,21                                                         ! BES.517   
      DX2 = DX2*DXX                                                         ! BES.518   
      A0 = B(J)*DX2                                                         ! BES.519   
      TEMP = TEMP + A0                                                      ! BES.520   
      IF (TEMP) 450,460,450                                                 ! BES.521   
  450 DCHK = abs(A0/TEMP)  
      IF (DCHK - 1.0D-20) 470,460,460                                       ! BES.523   
  460 CONTINUE                                                              ! BES.524   
  470 T2(1) = SQRT(PI/(2.0*DX))*exp(-DX)*TEMP 
      IF (NO.EQ.0) GO TO 490                                                ! BES.526   
  480 T2(2) = (1.0/X - T2(1)*T1(2))/T1(1)                                   ! BES.527   
      IF (KO.LE.2) GO TO 490                                                ! BES.528   
      NORD = KO-1                                                           ! BES.529   
      DO 485 N=2,NORD                                                       ! BES.530   
      XN = N-1                                                              ! BES.531   
      T2(N+1) = (2.0*XN)/X *T2(N) + T2(N-1)                                 ! BES.532   
  485 CONTINUE                                                              ! BES.533   
  490 RSLT2 = T2(KO)                                                        ! BES.534   
      X = XX                                                                ! BES.535   
      RETURN                                                                ! BES.536   
  600 IERR=5                                                                ! BES.537   
      RETURN                                                                ! BES.538   
  602 X=XX                                                                  ! BES.539   
      IERR=3                                                                ! BES.540   
      RETURN                                                                ! BES.541   
  604 IERR=4                                                                ! BES.542   
      RETURN                                                                ! BES.543   
  606 IERR=2                                                                ! BES.544   
      RETURN                                                                ! BES.545   
  608 IERR=1                                                                ! BES.546   
      RETURN                                                                ! BES.547   
      END                                                                   ! BES.548   
!===================================================================================================================================
      SUBROUTINE BESI(X,N, BI,IER)                                         ! BESI.2     
!                                                                          ! BESI.3     
!     ..................................................................   ! BESI.4     
!                                                                          ! BESI.5     
!        SUBROUTINE BESI                                                   ! BESI.6     
!                                                                          ! BESI.7     
!        PURPOSE                                                           ! BESI.8     
!@(#) M_bessel::besi(3f):compute the I Bessel function for a given argument and order 
!                                                                          ! BESI.10    
!        USAGE                                                             ! BESI.11    
!           CALL BESI(X,N,BI,IER)                                          ! BESI.12    
!                                                                          ! BESI.13    
!        DESCRIPTION OF PARAMETERS                                         ! BESI.14    
!           X  -THE ARGUMENT OF THE I BESSEL FUNCTION DESIRED              ! BESI.15    
!           N  -THE ORDER OF THE I BESSEL FUNCTION DESIRED                 ! BESI.16    
!           BI -THE RESULTANT I BESSEL FUNCTION                            ! BESI.17    
!           IER-RESULTANT ERROR CODE WHERE                                 ! BESI.18    
!              IER=0 NO ERROR                                              ! BESI.19    
!              IER=1 N IS NEGATIVE                                         ! BESI.20    
!              IER=2 X IS NEGATIVE                                         ! BESI.21    
!              IER=3 UNDERFLOW, BI .LT. 1.E-69, BI SET TO 0.0              ! BESI.22    
!              IER=4 OVERFLOW, X .GT. 170 WHERE X .GT. N                   ! BESI.23    
!                                                                          ! BESI.24    
!        REMARKS                                                           ! BESI.25    
!           N AND X MUST BE .GE. ZERO                                      ! BESI.26    
!                                                                          ! BESI.27    
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                     ! BESI.28    
!           NONE                                                           ! BESI.29    
!                                                                          ! BESI.30    
!        METHOD                                                            ! BESI.31    
!           COMPUTES I BESSEL FUNCTION USING SERIES OR ASYMPTOTIC          ! BESI.32    
!           APPROXIMATION DEPENDING ON RANGE OF ARGUMENTS.                 ! BESI.33    
!                                                                          ! BESI.34    
!     ..................................................................   ! BESI.35    
!                                                                          ! BESI.36    
!                                                                          ! BESI.37    
!     CHECK FOR ERRORS IN N AND X AND EXIT IF ANY ARE PRESENT              ! BESI.38    
!                                                                          ! BESI.39    
   implicit doubleprecision (a-h, o-z)
      IER=0                                                                ! BESI.40    
      BI=1.0                                                               ! BESI.41    
      IF(N)150,15,10                                                       ! BESI.42    
   10 IF(X)160,20,20                                                       ! BESI.43    
   15 IF(X)160,17,20                                                       ! BESI.44    
   17 RETURN                                                               ! BESI.45    
!                                                                          ! BESI.46    
!     DEFINE TOLERANCE                                                     ! BESI.47    
!                                                                          ! BESI.48    
   20 TOL=1.E-6                                                            ! BESI.49    
!                                                                          ! BESI.50    
!     IF ARGUMENT GT 12 AND GT N, USE ASYMPTOTIC FORM                      ! BESI.51    
!                                                                          ! BESI.52    
      IF(X-12.)40,40,30                                                    ! BESI.53    
   30 IF(X-FLOAT(N))40,40,110                                              ! BESI.54    
!                                                                          ! BESI.55    
!     COMPUTE FIRST TERM OF SERIES AND SET INITIAL VALUE OF THE SUM        ! BESI.56    
!                                                                          ! BESI.57    
   40 XX=X/2.                                                              ! BESI.58    
      TERM=1.0                                                             ! BESI.59    
      IF(N) 70,70,55                                                       ! BESI.60    
   55 DO 60 I=1,N                                                          ! BESI.61    
      FI=I                                                                 ! BESI.62    
      IF(ABS(TERM)-1.0d-68)56,60,60 
   56 IER=3                                                                ! BESI.64    
      BI=0.0                                                               ! BESI.65    
      RETURN                                                               ! BESI.66    
   60 TERM=TERM*XX/FI                                                      ! BESI.67    
   70 BI=TERM                                                              ! BESI.68    
      XX=XX*XX                                                             ! BESI.69    
!                                                                          ! BESI.70    
!     COMPUTE TERMS, STOPPING WHEN ABS(TERM) LE ABS(SUM OF TERMS)          ! BESI.71    
!     TIMES TOLERANCE                                                      ! BESI.72    
!                                                                          ! BESI.73    
      DO 90 K=1,1000                                                       ! BESI.74    
      IF(ABS(TERM)-ABS(BI*TOL))100,100,80                                  ! BESI.75    
   80 FK=K*(N+K)                                                           ! BESI.76    
      TERM=TERM*(XX/FK)                                                    ! BESI.77    
   90 BI=BI+TERM                                                           ! BESI.78    
!                                                                          ! BESI.79    
!     RETURN BI AS ANSWER                                                  ! BESI.80    
!                                                                          ! BESI.81    
  100 RETURN                                                               ! BESI.82    
!                                                                          ! BESI.83    
!     X GT 12 AND X GT N, SO USE ASYMPTOTIC APPROXIMATION                  ! BESI.84    
!                                                                          ! BESI.85    
  110 FN=4*N*N                                                             ! BESI.86    
      IF(X-170.0)115,111,111                                               ! BESI.87    
  111 IER=4                                                                ! BESI.88    
      RETURN                                                               ! BESI.89    
  115 XX=1./(8.*X)                                                         ! BESI.90    
      TERM=1.                                                              ! BESI.91    
      BI=1.                                                                ! BESI.92    
      DO 130 K=1,30                                                        ! BESI.93    
      IF(ABS(TERM)-ABS(TOL*BI))140,140,120                                 ! BESI.94    
  120 FK=(2*K-1)**2                                                        ! BESI.95    
      TERM=TERM*XX*(FK-FN)/FLOAT(K)                                        ! BESI.96    
  130 BI=BI+TERM                                                           ! BESI.97    
!                                                                          ! BESI.98    
!     SIGNIFICANCE LOST AFTER 30 TERMS, TRY SERIES                         ! BESI.99    
!                                                                          ! BESI.100   
      GO TO 40                                                             ! BESI.101   
  140 PI=3.141592653                                                       ! BESI.102   
      BI=BI*EXP(X)/SQRT(2.*PI*X)                                           ! BESI.103   
      GO TO 100                                                            ! BESI.104   
  150 IER=1                                                                ! BESI.105   
      GO TO 100                                                            ! BESI.106   
  160 IER=2                                                                ! BESI.107   
      GO TO 100                                                            ! BESI.108   
      END                                                                  ! BESI.109   
!===================================================================================================================================
      SUBROUTINE BESJ(X,N,BJ,D,IER)                                        ! BESJ.2     
!                                                                          ! BESJ.3     
!     ..................................................................   ! BESJ.4     
!                                                                          ! BESJ.5     
!        SUBROUTINE BESJ                                                   ! BESJ.6     
!                                                                          ! BESJ.7     
!        PURPOSE                                                           ! BESJ.8     
!@(#) M_bessel::besj(3f):compute the J Bessel function for a given argument and order
!                                                                          ! BESJ.10    
!        USAGE                                                             ! BESJ.11    
!           CALL BESJ(X,N,BJ,D,IER)                                        ! BESJ.12    
!                                                                          ! BESJ.13    
!        DESCRIPTION OF PARAMETERS                                         ! BESJ.14    
!           X  -THE ARGUMENT OF THE J BESSEL FUNCTION DESIRED              ! BESJ.15    
!           N  -THE ORDER OF THE J BESSEL FUNCTION DESIRED                 ! BESJ.16    
!           BJ -THE RESULTANT J BESSEL FUNCTION                            ! BESJ.17    
!           D  -REQUIRED ACCURACY                                          ! BESJ.18    
!           IER-RESULTANT ERROR CODE WHERE                                 ! BESJ.19    
!              IER=0  NO ERROR                                             ! BESJ.20    
!              IER=1  N IS NEGATIVE                                        ! BESJ.21    
!              IER=2  X IS NEGATIVE OR ZERO                                ! BESJ.22    
!              IER=3  REQUIRED ACCURACY NOT OBTAINED                       ! BESJ.23    
!              IER=4  RANGE OF N COMPARED TO X NOT CORRECT (SEE REMARKS)   ! BESJ.24    
!                                                                          ! BESJ.25    
!        REMARKS                                                           ! BESJ.26    
!           N MUST BE GREATER THAN OR EQUAL TO ZERO, BUT IT MUST BE        ! BESJ.27    
!           LESS THAN                                                      ! BESJ.28    
!              20+10*X-X** 2/3   FOR X LESS THAN OR EQUAL TO 15            ! BESJ.29    
!              90+X/2           FOR X GREATER THAN 15                      ! BESJ.30    
!                                                                          ! BESJ.31    
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                     ! BESJ.32    
!           NONE                                                           ! BESJ.33    
!                                                                          ! BESJ.34    
!        METHOD                                                            ! BESJ.35    
!           RECURRENCE RELATION TECHNIQUE DESCRIBED BY H. GOLDSTEIN AND    ! BESJ.36    
!           R.M. THALER,"RECURRENCE TECHNIQUES FOR THE CALCULATION OF      ! BESJ.37    
!           BESSEL FUNCTIONS",M.T.A.C.,V.13,PP.102-108 AND I.A. STEGUN     ! BESJ.38    
!           AND M. ABRAMOWITZ,"GENERATION OF BESSEL FUNCTIONS ON HIGH      ! BESJ.39    
!           SPEED COMPUTERS",M.T.A.C.,V.11,1957,PP.255-257                 ! BESJ.40    
!                                                                          ! BESJ.41    
!     ..................................................................   ! BESJ.42    
!                                                                          ! BESJ.43    
!                                                                          ! BESJ.44    
   implicit doubleprecision (a-h, o-z)
      BJ=.0                                                                ! BESJ.45    
      IF(N)10,20,20                                                        ! BESJ.46    
   10 IER=1                                                                ! BESJ.47    
      RETURN                                                               ! BESJ.48    
   20 IF(X)30,30,31                                                        ! BESJ.49    
   30 IER=2                                                                ! BESJ.50    
      RETURN                                                               ! BESJ.51    
   31 IF(X-15.)32,32,34                                                    ! BESJ.52    
   32 NTEST=20.+10.*X-X** 2/3                                              ! BESJ.53    
      GO TO 36                                                             ! BESJ.54    
   34 NTEST=90.+X/2.                                                       ! BESJ.55    
   36 IF(N-NTEST)40,38,38                                                  ! BESJ.56    
   38 IER=4                                                                ! BESJ.57    
      RETURN                                                               ! BESJ.58    
   40 IER=0                                                                ! BESJ.59    
      N1=N+1                                                               ! BESJ.60    
      BPREV=.0                                                             ! BESJ.61    
!                                                                          ! BESJ.62    
!     COMPUTE STARTING VALUE OF M                                          ! BESJ.63    
!                                                                          ! BESJ.64    
      IF(X-5.)50,60,60                                                     ! BESJ.65    
   50 MA=X+6.                                                              ! BESJ.66    
      GO TO 70                                                             ! BESJ.67    
   60 MA=1.4*X+60./X                                                       ! BESJ.68    
   70 MB=N+int(X)/4+2   
      MZERO=MAX0(MA,MB)                                                    ! BESJ.70    
!                                                                          ! BESJ.71    
!     SET UPPER LIMIT OF M                                                 ! BESJ.72    
!                                                                          ! BESJ.73    
      MMAX=NTEST                                                           ! BESJ.74    
      DO 190 M=MZERO,MMAX,3                                                ! BESJ.75    
!                                                                          ! BESJ.76    
!     SET F(M),F(M-1)                                                      ! BESJ.77    
!                                                                          ! BESJ.78    
      FM1=1.0E-28                                                          ! BESJ.79    
      FM=.0                                                                ! BESJ.80    
      ALPHA=.0                                                             ! BESJ.81    
      IF(M-(M/2)*2)120,110,120                                             ! BESJ.82    
  110 JT=-1                                                                ! BESJ.83    
      GO TO 130                                                            ! BESJ.84    
  120 JT=1                                                                 ! BESJ.85    
  130 M2=M-2                                                               ! BESJ.86    
      DO 160 K=1,M2                                                        ! BESJ.87    
      MK=M-K                                                               ! BESJ.88    
      BMK=2.*FLOAT(MK)*FM1/X-FM                                            ! BESJ.89    
      FM=FM1                                                               ! BESJ.90    
      FM1=BMK                                                              ! BESJ.91    
      IF(MK-N-1)150,140,150                                                ! BESJ.92    
  140 BJ=BMK                                                               ! BESJ.93    
  150 JT=-JT                                                               ! BESJ.94    
      S=1+JT                                                               ! BESJ.95    
  160 ALPHA=ALPHA+BMK*S                                                    ! BESJ.96    
      BMK=2.*FM1/X-FM                                                      ! BESJ.97    
      IF(N)180,170,180                                                     ! BESJ.98    
  170 BJ=BMK                                                               ! BESJ.99    
  180 ALPHA=ALPHA+BMK                                                      ! BESJ.100   
      BJ=BJ/ALPHA                                                          ! BESJ.101   
      IF(ABS(BJ-BPREV)-ABS(D*BJ))200,200,190                               ! BESJ.102   
  190 BPREV=BJ                                                             ! BESJ.103   
      IER=3                                                                ! BESJ.104   
  200 RETURN                                                               ! BESJ.105   
      END                                                                  ! BESJ.106   
!===================================================================================================================================
      FUNCTION BESJ0(XX)                                                  ! BESJ0.2
!                                                                         ! BESJ0.3
!     SANDIA MATHEMATICAL PROGRAM LIBRARY                                 ! BESJ0.4
!     APPLIED MATHEMATICS DIVISION 2642                                   ! BESJ0.5
!     SANDIA LABORATORIES                                                 ! BESJ0.6
!     P. O. BOX 5800                                                      ! BESJ0.7
!     ALBUQUERQUE, NEW MEXICO  87115                                      ! BESJ0.8
!     CONTROL DATA 6600 VERSION 5.1, 10 DECEMBER 1973                     ! BESJ0.9
!                                                                         ! BESJ0.10
!         WRITTEN BY    RONALD D. HALBGEWACHS, OCTOBER 1,1971.            ! BESJ0.11
!                                                                         ! BESJ0.12
!     ARD IMPLEMENTATION           S. J. ORBON       4/16/74              ! BESJ0.13
!                                                                         ! BESJ0.14
!     ABSTRACT                                                            ! BESJ0.15
!                                                                         ! BESJ0.16
!@(#) M_bessel::besj0(3f):calculates the Bessel function J(X) of order zero.
!         SERIES EVALUATION IS USED FOR SMALL ARGUMENTS, RECURRENCE       ! BESJ0.18
!         TECHNIQUES ARE USED FOR MIDRANGE, AND HANKEL-S ASYMPTOTIC       ! BESJ0.19
!         EXPANSION IS USED FOR LARGE ARGUMENTS.                          ! BESJ0.20
!         ACCURACY IS BETWEEN THIRTEEN AND FOURTEEN CORRECT SIGNIFICANT   ! BESJ0.21
!         FIGURES EXCEPT FOR LARGE ARGUMENTS WHERE THE ACCURACY           ! BESJ0.22
!         EVENTUALLY FALLS TO EIGHT FIGURES.                              ! BESJ0.23
!                                                                         ! BESJ0.24
!     DESCRIPTION OF ARGUMENT                                             ! BESJ0.25
!                                                                         ! BESJ0.26
!         X MAY BE ANY REAL ARGUMENT.                                     ! BESJ0.27
!                                                                         ! BESJ0.28
      implicit doubleprecision (a-h, o-z)
      DIMENSION T1(101)                                                   ! BESJ0.29
      DIMENSION T2(101) ! for bug in Intel 11.1.046 compiler Sun Aug 23 15:27:46 EDT 2009
      DATA TWOVPI/0.63661977236758/,PIOV4/0.78539816339745/               ! BESJ0.30
      DATA P1/4.5/,P2/4.59375E02/,P3/1.500778125E05/                      ! BESJ0.31
      DATA Q1/37.5/,Q2/7.441875E03/,Q3/3.623307187E06/                    ! BESJ0.32
      X = ABS(XX)                                                         ! BESJ0.33
      IF (X - 25.0) 20,20,40                                              ! BESJ0.34
   20 CALL BES(XX,0,10,BESJ0,R2,T1,T2,IERR)                               ! BESJ0.35
      RETURN                                                              ! BESJ0.36
   40 CHI = X - PIOV4                                                     ! BESJ0.37
      FACTOR = SQRT(TWOVPI/X)                                             ! BESJ0.38
      EIGHTX = 0.125/X                                                    ! BESJ0.39
      EXSQ = EIGHTX*EIGHTX                                                ! BESJ0.40
      EXFOUR = EXSQ*EXSQ                                                  ! BESJ0.41
      EXSIX = EXFOUR*EXSQ                                                 ! BESJ0.42
      P = 1.0 - P1*EXSQ + P2*EXFOUR - P3*EXSIX                            ! BESJ0.43
      Q = EIGHTX*(-1.0 + Q1*EXSQ - Q2*EXFOUR + Q3*EXSIX)                  ! BESJ0.44
      BESJ0 = FACTOR*(P*COS(CHI) - Q*SIN(CHI))                            ! BESJ0.45
      RETURN                                                              ! BESJ0.46
      END                                                                 ! BESJ0.47
!===================================================================================================================================
      FUNCTION BESJ1(XX)                                                  ! BESJ1.2
!                                                                         ! BESJ1.3
!     SANDIA MATHEMATICAL PROGRAM LIBRARY                                 ! BESJ1.4
!     APPLIED MATHEMATICS DIVISION 2642                                   ! BESJ1.5
!     SANDIA LABORATORIES                                                 ! BESJ1.6
!     P. O. BOX 5800                                                      ! BESJ1.7
!     ALBUQUERQUE, NEW MEXICO  87115                                      ! BESJ1.8
!     CONTROL DATA 6600 VERSION 5.1, 10 DECEMBER 1973                     ! BESJ1.9
!                                                                         ! BESJ1.10
!         WRITTEN BY    RONALD D. HALBGEWACHS, OCTOBER 1,1971.            ! BESJ1.11
!                                                                         ! BESJ1.12
!     ARD IMPLEMENTATION           S. J. ORBON       4/16/74              ! BESJ1.13
!                                                                         ! BESJ1.14
!     ABSTRACT                                                            ! BESJ1.15
!                                                                         ! BESJ1.16
!@(#) M_bessel::besj1(3f): calculates the Bessel function J(X) of order one. 
!         SERIES EVALUATION IS USED FOR SMALL ARGUMENTS, RECURRENCE       ! BESJ1.18
!         TECHNIQUES ARE USED FOR MIDRANGE, AND HANKEL-S ASYMPTOTIC       ! BESJ1.19
!         EXPANSION IS USED FOR LARGE ARGUMENTS.                          ! BESJ1.20
!         ACCURACY IS BETWEEN THIRTEEN AND FOURTEEN CORRECT SIGNIFICANT   ! BESJ1.21
!         FIGURES EXCEPT FOR LARGE ARGUMENTS WHERE THE ACCURACY           ! BESJ1.22
!         EVENTUALLY FALLS TO EIGHT FIGURES.                              ! BESJ1.23
!                                                                         ! BESJ1.24
!     DESCRIPTION OF ARGUMENT                                             ! BESJ1.25
!                                                                         ! BESJ1.26
!         X MAY BE ANY REAL ARGUMENT                                      ! BESJ1.27
!                                                                         ! BESJ1.28
      implicit doubleprecision (a-h, o-z)
      DIMENSION T1(101)                                                   ! BESJ1.29
      DIMENSION T2(101) ! for Intel compiler bug 11.1.046 Sun Aug 23 2009
      DATA  PI/3.1415926535898/,TWOVPI/0.63661977236758/                  ! BESJ1.30
      DATA P1/7.5/,P2/5.90625E02/,P3/1.773646875E05/                      ! BESJ1.31
      DATA Q1/5.25E01/,Q2/9.095625E03/,Q3/4.180739062E06/                 ! BESJ1.32
      X = ABS(XX)                                                         ! BESJ1.33
      IF (X - 25.0) 20,20,40                                              ! BESJ1.34
   20 CALL BES(XX,1,10,BESJ1,R2,T1,T2,IERR)                               ! BESJ1.35
      RETURN                                                              ! BESJ1.36
   40 CHI = X - 0.75*PI                                                   ! BESJ1.37
      FACTOR = SQRT(TWOVPI/X)                                             ! BESJ1.38
      EIGHTX = 0.125/X                                                    ! BESJ1.39
      EXSQ = EIGHTX*EIGHTX                                                ! BESJ1.40
      EXFOUR = EXSQ*EXSQ                                                  ! BESJ1.41
      EXSIX = EXFOUR*EXSQ                                                 ! BESJ1.42
      P = 1.0 + P1*EXSQ - P2*EXFOUR + P3*EXSIX                            ! BESJ1.43
      Q = EIGHTX*(3.0 - Q1*EXSQ + Q2*EXFOUR - Q3*EXSIX)                   ! BESJ1.44
      BESJ1 = FACTOR*(P*COS(CHI) - Q*SIN(CHI))                            ! BESJ1.45
      IF (XX) 60,80,80                                                    ! BESJ1.46
   60 BESJ1 = -BESJ1                                                      ! BESJ1.47
   80 RETURN                                                              ! BESJ1.48
      END                                                                 ! BESJ1.49
!===================================================================================================================================
      SUBROUTINE BESK(X,N,BK,IER)                                          ! BESK.2     
!                                                                          ! BESK.3     
!     ..................................................................   ! BESK.4     
!                                                                          ! BESK.5     
!        SUBROUTINE BESK                                                   ! BESK.6     
!                                                                          ! BESK.7     
!@(#) M_bessel::besk(3f):compute the K Bessel function for a given argument and order 
!                                                                          ! BESK.9     
!        USAGE                                                             ! BESK.10    
!           CALL BESK(X,N,BK,IER)                                          ! BESK.11    
!                                                                          ! BESK.12    
!        DESCRIPTION OF PARAMETERS                                         ! BESK.13    
!           X  -THE ARGUMENT OF THE K BESSEL FUNCTION DESIRED              ! BESK.14    
!           N  -THE ORDER OF THE K BESSEL FUNCTION DESIRED                 ! BESK.15    
!           BK -THE RESULTANT K BESSEL FUNCTION                            ! BESK.16    
!           IER-RESULTANT ERROR CODE WHERE                                 ! BESK.17    
!              IER=0  NO ERROR                                             ! BESK.18    
!              IER=1  N IS NEGATIVE                                        ! BESK.19    
!              IER=2  X IS ZERO OR NEGATIVE                                ! BESK.20    
!              IER=3  X .GT. 170, MACHINE RANGE EXCEEDED                   ! BESK.21    
!              IER=4  BK .GT. 10**70                                       ! BESK.22    
!                                                                          ! BESK.23    
!        REMARKS                                                           ! BESK.24    
!           N MUST BE GREATER THAN OR EQUAL TO ZERO                        ! BESK.25    
!                                                                          ! BESK.26    
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                     ! BESK.27    
!           NONE                                                           ! BESK.28    
!                                                                          ! BESK.29    
!        METHOD                                                            ! BESK.30    
!           COMPUTES ZERO ORDER AND FIRST ORDER BESSEL FUNCTIONS USING     ! BESK.31    
!           SERIES APPROXIMATIONS AND THEN COMPUTES N TH ORDER FUNCTION    ! BESK.32    
!           USING RECURRENCE RELATION.                                     ! BESK.33    
!           RECURRENCE RELATION AND POLYNOMIAL APPROXIMATION TECHNIQUE     ! BESK.34    
!           AS DESCRIBED BY A.J.M.HITCHCOCK,"POLYNOMIAL APPROXIMATIONS     ! BESK.35    
!           TO BESSEL FUNCTIONS OF ORDER ZERO AND ONE AND TO RELATED       ! BESK.36    
!           FUNCTIONS", M.T.A.C., V.11,1957,PP.86-88, AND G.N. WATSON,     ! BESK.37    
!           "A TREATISE ON THE THEORY OF BESSEL FUNCTIONS", CAMBRIDGE      ! BESK.38    
!           UNIVERSITY PRESS, 1958, P. 62                                  ! BESK.39    
!                                                                          ! BESK.40    
!     ..................................................................   ! BESK.41    
!                                                                          ! BESK.42    
      implicit doubleprecision (a-h, o-z)
      DIMENSION T(12)                                                      ! BESK.43    
      BK=.0                                                                ! BESK.44    
      IF(N)10,11,11                                                        ! BESK.45    
   10 IER=1                                                                ! BESK.46    
      RETURN                                                               ! BESK.47    
   11 IF(X)12,12,20                                                        ! BESK.48    
   12 IER=2                                                                ! BESK.49    
      RETURN                                                               ! BESK.50    
   20 IF(X-170.0)22,22,21                                                  ! BESK.51    
   21 IER=3                                                                ! BESK.52    
      RETURN                                                               ! BESK.53    
   22 IER=0                                                                ! BESK.54    
      IF(X-1.)36,36,25                                                     ! BESK.55    
   25 A=EXP(-X)                                                            ! BESK.56    
      B=1./X                                                               ! BESK.57    
      C=SQRT(B)                                                            ! BESK.58    
      T(1)=B                                                               ! BESK.59    
      DO 26 L=2,12                                                         ! BESK.60    
   26 T(L)=T(L-1)*B                                                        ! BESK.61    
      IF(N-1)27,29,27                                                      ! BESK.62    
!                                                                          ! BESK.63    
!     COMPUTE KO USING POLYNOMIAL APPROXIMATION                            ! BESK.64    
!                                                                          ! BESK.65    
   27 G0=A*(1.25331414-.15666418*T(1)+.088111278*T(2)-.091390954*T(3)    & ! BESK.66    
     &+.13445962*T(4)-.22998503*T(5)+.37924097*T(6)-.52472773*T(7)       & ! BESK.67    
     &+.55753684*T(8)-.42626329*T(9)+.21845181*T(10)-.066809767*T(11)    & ! BESK.68    
     &+.009189383*T(12))*C                                                 ! BESK.69    
      IF(N)20,28,29                                                        ! BESK.70    
   28 BK=G0                                                                ! BESK.71    
      RETURN                                                               ! BESK.72    
!                                                                          ! BESK.73    
!     COMPUTE K1 USING POLYNOMIAL APPROXIMATION                            ! BESK.74    
!                                                                          ! BESK.75    
   29 G1=A*(1.2533141+.46999270*T(1)-.14685830*T(2)+.12804266*T(3)       & ! BESK.76    
     &-.17364316*T(4)+.28476181*T(5)-.45943421*T(6)+.62833807*T(7)       & ! BESK.77    
     &-.66322954*T(8)+.50502386*T(9)-.25813038*T(10)+.078800012*T(11)    & ! BESK.78    
     &-.010824177*T(12))*C                                                 ! BESK.79    
      IF(N-1)20,30,31                                                      ! BESK.80    
   30 BK=G1                                                                ! BESK.81    
      RETURN                                                               ! BESK.82    
!                                                                          ! BESK.83    
!     FROM KO,K1 COMPUTE KN USING RECURRENCE RELATION                      ! BESK.84    
!                                                                          ! BESK.85    
   31 DO 35 J=2,N                                                          ! BESK.86    
      GJ=2.*(FLOAT(J)-1.)*G1/X+G0                                          ! BESK.87    
      IF(GJ-1.0d70)33,33,32  
   32 IER=4                                                                ! BESK.89    
      GO TO 34                                                             ! BESK.90    
   33 G0=G1                                                                ! BESK.91    
   35 G1=GJ                                                                ! BESK.92    
   34 BK=GJ                                                                ! BESK.93    
      RETURN                                                               ! BESK.94    
   36 B=X/2.                                                               ! BESK.95    
      A=.57721566+log(B)
      C=B*B                                                                ! BESK.97    
      IF(N-1)37,43,37                                                      ! BESK.98    
!                                                                          ! BESK.99    
!     COMPUTE KO USING SERIES EXPANSION                                    ! BESK.100   
!                                                                          ! BESK.101   
   37 G0=-A                                                                ! BESK.102   
      X2J=1.                                                               ! BESK.103   
      FACT=1.                                                              ! BESK.104   
      HJ=.0                                                                ! BESK.105   
      DO 40 J=1,6                                                          ! BESK.106   
      RJ=1./FLOAT(J)                                                       ! BESK.107   
      X2J=X2J*C                                                            ! BESK.108   
      FACT=FACT*RJ*RJ                                                      ! BESK.109   
      HJ=HJ+RJ                                                             ! BESK.110   
   40 G0=G0+X2J*FACT*(HJ-A)                                                ! BESK.111   
      IF(N)43,42,43                                                        ! BESK.112   
   42 BK=G0                                                                ! BESK.113   
      RETURN                                                               ! BESK.114   
!                                                                          ! BESK.115   
!     COMPUTE K1 USING SERIES EXPANSION                                    ! BESK.116   
!                                                                          ! BESK.117   
   43 X2J=B                                                                ! BESK.118   
      FACT=1.                                                              ! BESK.119   
      HJ=1.                                                                ! BESK.120   
      G1=1./X+X2J*(.5+A-HJ)                                                ! BESK.121   
      DO 50 J=2,8                                                          ! BESK.122   
      X2J=X2J*C                                                            ! BESK.123   
      RJ=1./FLOAT(J)                                                       ! BESK.124   
      FACT=FACT*RJ*RJ                                                      ! BESK.125   
      HJ=HJ+RJ                                                             ! BESK.126   
   50 G1=G1+X2J*FACT*(.5+(A-HJ)*FLOAT(J))                                  ! BESK.127   
      IF(N-1)31,52,31                                                      ! BESK.128   
   52 BK=G1                                                                ! BESK.129   
      RETURN                                                               ! BESK.130   
      END                                                                  ! BESK.131   
!===================================================================================================================================
      SUBROUTINE BESY(X,N,BY,IER)                                          ! BESY.2     
!                                                                          ! BESY.3     
!     ..................................................................   ! BESY.4     
!                                                                          ! BESY.5     
!        SUBROUTINE BESY                                                   ! BESY.6     
!                                                                          ! BESY.7     
!        PURPOSE                                                           ! BESY.8     
!@(#) M_bessel::besy(3f):compute the Y Bessel function for a given argument and order
!                                                                          ! BESY.10    
!        USAGE                                                             ! BESY.11    
!           CALL BESY(X,N,BY,IER)                                          ! BESY.12    
!                                                                          ! BESY.13    
!        DESCRIPTION OF PARAMETERS                                         ! BESY.14    
!           X  -THE ARGUMENT OF THE Y BESSEL FUNCTION DESIRED              ! BESY.15    
!           N  -THE ORDER OF THE Y BESSEL FUNCTION DESIRED                 ! BESY.16    
!           BY -THE RESULTANT Y BESSEL FUNCTION                            ! BESY.17    
!           IER-RESULTANT ERROR CODE WHERE                                 ! BESY.18    
!              IER=0  NO ERROR                                             ! BESY.19    
!              IER=1  N IS NEGATIVE                                        ! BESY.20    
!              IER=2  X IS NEGATIVE OR ZERO                                ! BESY.21    
!              IER=3  BY HAS EXCEEDED MAGNITUDE OF 10**70                  ! BESY.22    
!                                                                          ! BESY.23    
!        REMARKS                                                           ! BESY.24    
!           VERY SMALL VALUES OF X MAY CAUSE THE RANGE OF THE LIBRARY      ! BESY.25    
!           FUNCTION log TO BE EXCEEDED  
!           X MUST BE GREATER THAN ZERO                                    ! BESY.27    
!           N MUST BE GREATER THAN OR EQUAL TO ZERO                        ! BESY.28    
!                                                                          ! BESY.29    
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                     ! BESY.30    
!           NONE                                                           ! BESY.31    
!                                                                          ! BESY.32    
!        METHOD                                                            ! BESY.33    
!           RECURRENCE RELATION AND POLYNOMIAL APPROXIMATION TECHNIQUE     ! BESY.34    
!           AS DESCRIBED BY A.J.M.HITCHCOCK,"POLYNOMIAL APPROXIMATIONS     ! BESY.35    
!           TO BESSEL FUNCTIONS OF ORDER ZERO AND ONE AND TO RELATED       ! BESY.36    
!           FUNCTIONS", M.T.A.C., V.11,1957,PP.86-88, AND G.N. WATSON,     ! BESY.37    
!           "A TREATISE ON THE THEORY OF BESSEL FUNCTIONS", CAMBRIDGE      ! BESY.38    
!           UNIVERSITY PRESS, 1958, P. 62                                  ! BESY.39    
!                                                                          ! BESY.40    
!     ..................................................................   ! BESY.41    
!                                                                          ! BESY.42    
!                                                                          ! BESY.43    
!     CHECK FOR ERRORS IN N AND X                                          ! BESY.44    
!                                                                          ! BESY.45    
      implicit doubleprecision (a-h, o-z)
      IF(N)180,10,10                                                       ! BESY.46    
   10 IER=0                                                                ! BESY.47    
      IF(X)190,190,20                                                      ! BESY.48    
   20 PI=3.141592653                                                       ! BESY.49    
!                                                                          ! BESY.50    
!     BRANCH IF X LESS THAN OR EQUAL 4                                     ! BESY.51    
!                                                                          ! BESY.52    
      IF(X-4.)40,40,30                                                     ! BESY.53    
!                                                                          ! BESY.54    
!       COMPUTE Y0 AND Y1 FOR X GREATER THAN 4                             ! BESY.55    
!                                                                          ! BESY.56    
   30 T=4./X                                                               ! BESY.57    
      P0=.3989422793                                                       ! BESY.58    
      Q0=-.0124669441                                                      ! BESY.59    
      P1=.3989422819                                                       ! BESY.60    
      Q1=.0374008364                                                       ! BESY.61    
      A=T*T                                                                ! BESY.62    
      B=A                                                                  ! BESY.63    
      P0=P0-.0017530620*A                                                  ! BESY.64    
      Q0=Q0+.0004564324*A                                                  ! BESY.65    
      P1=P1+.0029218256*A                                                  ! BESY.66    
      Q1=Q1-.00063904*A                                                    ! BESY.67    
      A=A*A                                                                ! BESY.68    
      P0=P0+.00017343*A                                                    ! BESY.69    
      Q0=Q0-.0000869791*A                                                  ! BESY.70    
      P1=P1-.000223203*A                                                   ! BESY.71    
      Q1=Q1+.0001064741*A                                                  ! BESY.72    
      A=A*B                                                                ! BESY.73    
      P0=P0-.0000487613*A                                                  ! BESY.74    
      Q0=Q0+.0000342468*A                                                  ! BESY.75    
      P1=P1+.0000580759*A                                                  ! BESY.76    
      Q1=Q1-.0000398708*A                                                  ! BESY.77    
      A=A*B                                                                ! BESY.78    
      P0=P0+.0000173565*A                                                  ! BESY.79    
      Q0=Q0-.0000142078*A                                                  ! BESY.80    
      P1=P1-.000020092*A                                                   ! BESY.81    
      Q1=Q1+.00001622*A                                                    ! BESY.82    
      A=A*B                                                                ! BESY.83    
      P0=P0-.0000037043*A                                                  ! BESY.84    
      Q0=Q0+.0000032312*A                                                  ! BESY.85    
      P1=P1+.0000042414*A                                                  ! BESY.86    
      Q1=Q1-.0000036594*A                                                  ! BESY.87    
      A=SQRT(2.*PI)                                                        ! BESY.88    
      B=4.*A                                                               ! BESY.89    
      P0=A*P0                                                              ! BESY.90    
      Q0=B*Q0/X                                                            ! BESY.91    
      P1=A*P1                                                              ! BESY.92    
      Q1=B*Q1/X                                                            ! BESY.93    
      A=X-PI/4.                                                            ! BESY.94    
      B=SQRT(2./(PI*X))                                                    ! BESY.95    
      Y0=B*(P0*SIN(A)+Q0*COS(A))                                           ! BESY.96    
      Y1=B*(-P1*COS(A)+Q1*SIN(A))                                          ! BESY.97    
      GO TO 90                                                             ! BESY.98    
!                                                                          ! BESY.99    
!       COMPUTE Y0 AND Y1 FOR X LESS THAN OR EQUAL TO 4                    ! BESY.100   
!                                                                          ! BESY.101   
   40 XX=X/2.                                                              ! BESY.102   
      X2=XX*XX                                                             ! BESY.103   
      T=log(XX)+.5772156649   
      SUM=0.                                                               ! BESY.105   
      TERM=T                                                               ! BESY.106   
      Y0=T                                                                 ! BESY.107   
      DO 70 L=1,15                                                         ! BESY.108   
      IF(L-1)50,60,50                                                      ! BESY.109   
   50 SUM=SUM+1./FLOAT(L-1)                                                ! BESY.110   
   60 FL=L                                                                 ! BESY.111   
      TS=T-SUM                                                             ! BESY.112   
      TERM=(TERM*(-X2)/FL**2)*(1.-1./(FL*TS))                              ! BESY.113   
   70 Y0=Y0+TERM                                                           ! BESY.114   
      TERM = XX*(T-.5)                                                     ! BESY.115   
      SUM=0.                                                               ! BESY.116   
      Y1=TERM                                                              ! BESY.117   
      DO 80 L=2,16                                                         ! BESY.118   
      SUM=SUM+1./FLOAT(L-1)                                                ! BESY.119   
      FL=L                                                                 ! BESY.120   
      FL1=FL-1.                                                            ! BESY.121   
      TS=T-SUM                                                             ! BESY.122   
      TERM=(TERM*(-X2)/(FL1*FL))*((TS-.5/FL)/(TS+.5/FL1))                  ! BESY.123   
   80 Y1=Y1+TERM                                                           ! BESY.124   
      PI2=2./PI                                                            ! BESY.125   
      Y0=PI2*Y0                                                            ! BESY.126   
      Y1=-PI2/X+PI2*Y1                                                     ! BESY.127   
!                                                                          ! BESY.128   
!     CHECK IF ONLY Y0 OR Y1 IS DESIRED                                    ! BESY.129   
!                                                                          ! BESY.130   
   90 IF(N-1)100,100,130                                                   ! BESY.131   
!                                                                          ! BESY.132   
!     RETURN EITHER Y0 OR Y1 AS REQUIRED                                   ! BESY.133   
!                                                                          ! BESY.134   
  100 IF(N)110,120,110                                                     ! BESY.135   
  110 BY=Y1                                                                ! BESY.136   
      GO TO 170                                                            ! BESY.137   
  120 BY=Y0                                                                ! BESY.138   
      GO TO 170                                                            ! BESY.139   
!                                                                          ! BESY.140   
!    PERFORM RECURRENCE OPERATIONS TO FIND YN(X)                           ! BESY.141   
!                                                                          ! BESY.142   
  130 YA=Y0                                                                ! BESY.143   
      YB=Y1                                                                ! BESY.144   
      K=1                                                                  ! BESY.145   
  140 T=FLOAT(2*K)/X                                                       ! BESY.146   
      YC=T*YB-YA                                                           ! BESY.147   
      IF(ABS(YC)-1.0d70)145,145,141 
  141 IER=3                                                                ! BESY.149   
      RETURN                                                               ! BESY.150   
  145 K=K+1                                                                ! BESY.151   
      IF(K-N)150,160,150                                                   ! BESY.152   
  150 YA=YB                                                                ! BESY.153   
      YB=YC                                                                ! BESY.154   
      GO TO 140                                                            ! BESY.155   
  160 BY=YC                                                                ! BESY.156   
  170 RETURN                                                               ! BESY.157   
  180 IER=1                                                                ! BESY.158   
      RETURN                                                               ! BESY.159   
  190 IER=2                                                                ! BESY.160   
      RETURN                                                               ! BESY.161   
      END                                                                  ! BESY.162   
!===================================================================================================================================
      FUNCTION BESY0(X)                                                   ! BESY0.2
!                                                                         ! BESY0.3
!     SANDIA MATHEMATICAL PROGRAM LIBRARY                                 ! BESY0.4
!     APPLIED MATHEMATICS DIVISION 2642                                   ! BESY0.5
!     SANDIA LABORATORIES                                                 ! BESY0.6
!     P. O. BOX 5800                                                      ! BESY0.7
!     ALBUQUERQUE, NEW MEXICO  87115                                      ! BESY0.8
!     CONTROL DATA 6600 VERSION 5.1, 10 DECEMBER 1973                     ! BESY0.9
!                                                                         ! BESY0.10
!         WRITTEN BY    RONALD D. HALBGEWACHS, OCTOBER 1,1971.            ! BESY0.11
!                                                                         ! BESY0.12
!     ARD IMPLEMENTATION           S. J. ORBON       4/16/74              ! BESY0.13
!     REPLACED ARITHMETIC IF WITH IF/ELSE/ENDIF,...  J. S. Urban
!                                                                         ! BESY0.14
!     ABSTRACT                                                            ! BESY0.15
!                                                                         ! BESY0.16
!@(#) M_bessel::besy0(3f): calculates the Bessel function Y(X) of order zero.
!         SERIES EVALUATION IS USED FOR SMALL ARGUMENTS, RECURRENCE       ! BESY0.18
!         TECHNIQUES ARE USED FOR MIDRANGE, AND HANKEL-S ASYMPTOTIC       ! BESY0.19
!         EXPANSION IS USED FOR LARGE ARGUMENTS.                          ! BESY0.20
!         ACCURACY IS BETWEEN THIRTEEN AND FOURTEEN CORRECT SIGNIFICANT   ! BESY0.21
!         FIGURES EXCEPT FOR LARGE ARGUMENTS WHERE THE ACCURACY           ! BESY0.22
!         EVENTUALLY FALLS TO EIGHT FIGURES.                              ! BESY0.23
!                                                                         ! BESY0.24
!     DESCRIPTION OF ARGUMENT                                             ! BESY0.25
!                                                                         ! BESY0.26
!         X MAY BE ANY NONNEGATIVE REAL ARGUMENT.                         ! BESY0.27
!                                                                         ! BESY0.28
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit real(kind=kind(0.0d0)) (a-h, o-z)
      integer,parameter :: dp=kind(0.0d0)
      DIMENSION T1(101),T2(101)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     DATA  TWOVPI/0.63661977236758/, PIOV4/0.78539816339745/
!     DATA  P1/4.5/, P2/4.59375E02/, P3/1.500778125E05/
!     DATA  Q1/37.5/, Q2/7.441875E03/, Q3/3.623307187E06/
      real(kind=dp),parameter :: TWOVPI=  0.63661977236758
      real(kind=dp),parameter :: PIOV4 =  0.78539816339745
      real(kind=dp),parameter :: P1    =  4.5d0
      real(kind=dp),parameter :: P2    =  4.59375d02
      real(kind=dp),parameter :: P3    =  1.500778125d05
      real(kind=dp),parameter :: Q1    = 37.5d0
      real(kind=dp),parameter :: Q2    =  7.441875d03
      real(kind=dp),parameter :: Q3    =  3.623307187d06
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(x.lt.0)then
         besy0=-10.0d-32   ! bad value assigned so compiler does not complain unassigned
         call journal('*besy0* ERROR: negative input value')
      elseif ((x - 25.0_dp) .lt. 0_dp)then
         CALL BES(X,0,11,R1,BESY0,T1,T2,IERR)
      else
         CHI    = X - PIOV4
         FACTOR = SQRT( TWOVPI / X )
         EIGHTX = 0.125_dp / X
         EXSQ   = EIGHTX * EIGHTX
         EXFOUR = EXSQ * EXSQ
         EXSIX  = EXFOUR * EXSQ
         P      = 1.0_dp - P1*EXSQ + P2*EXFOUR - P3*EXSIX
         Q      = EIGHTX * ( -1.0_dp + Q1*EXSQ - Q2*EXFOUR + Q3*EXSIX )
         BESY0  = FACTOR * ( P*SIN(CHI) + Q*COS(CHI) )
      endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      END FUNCTION BESY0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_bessel()
implicit none
!! setup
   call test_bes()
   call test_besi()
   call test_besj()
   call test_besj0()
   call test_besj1()
   call test_besk()
   call test_besy()
   call test_besy0()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bes()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('bes',msg='')
   !!call unit_check('bes', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('bes',msg='')
end subroutine test_bes
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besi()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besi',msg='')
   !!call unit_check('besi', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besi',msg='')
end subroutine test_besi
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besj',msg='')
   !!call unit_check('besj', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besj',msg='')
end subroutine test_besj
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj0()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besj0',msg='')
   !!call unit_check('besj0', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besj0',msg='')
end subroutine test_besj0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besj1',msg='')
   !!call unit_check('besj1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besj1',msg='')
end subroutine test_besj1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besk()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besk',msg='')
   !!call unit_check('besk', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besk',msg='')
end subroutine test_besk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besy()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besy',msg='')
   !!call unit_check('besy', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besy',msg='')
end subroutine test_besy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besy0()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('besy0',msg='')
   !!call unit_check('besy0', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('besy0',msg='')
end subroutine test_besy0
!===================================================================================================================================
end subroutine test_suite_M_bessel
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_bessel
!===================================================================================================================================
