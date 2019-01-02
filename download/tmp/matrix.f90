! MATLAB stands for MATrix LABoratory.  It is a FORTRAN package developed by
! Argonne National Laboratories for in-house use.  It provides comprehensive
! vector and tensor operations in a package which may be programmed, either
! through a macro language or through execution of script files.
!
! Matlab is reentrant and recursive.  Functions supported include (but are not
! by any means limited to) sin, cos, tan, arcfunctions, upper triangular,
! lower triangular, determinants, matrix multiplication, identity, Hilbert
! matrices, eigenvalues and eigenvectors, matrix roots and products, inversion
! and so on and so forth.
!
! The file available on the bulletin board as Matlab.arc contains an Amiga-ized
! executable copy of MATLAB and the online help file, as well as this intro.
!
! If you want the source code (over 300K) and a manual, or if your bulletin
! board only has this message and not the package, send $5.00 and a 3.5"
! disk to:
!
!                            Jim Locker
!                            4443 N. Hyland Ave.
!                            Dayton, Oh 45424
!
! The package is public domain, but of course postage and reproduction cost
! money.  Believe me, this package is a bargain at the price.  Please feel free
! to distribute the package.
!
! The source was taken off a VAX 11/780. It ran without modification (except the
! file handler and some minor error handling) on an Amiga 1000 using ABSoft
! Fortran v2.2.  It will run in 512K environment.  I have seen it on IBM
! mainframes and IBM PCs.
!===============================================================================
! Some minimal changes: John S. Urban

! Converted to do most I/O via JUN() so can be used with my codes more
! easily, and write a trail file at all times. Disabled DIARY().

! Added minimal command editor something like CDC Xedit modif and c directives

! Made call to system shell where available with SH command

! Made case-sensitive

! Made it take directives from string on routine call

! Allow longer filenames

! Partly converted program away from use of HOLLERITH towards use of ADE
! or maybe even character variables (enough to be able to use GNU g95
! compiler, anyway). Might have to change the way I make a letter
! "hollerith" on non little-endian non-32bit platforms.

! Allow additional comment indicator (# in column 1) so can read back in
! trail files made by sample JUN() procedure
!
! g77 --no-backslash matrix.f
! g95 matrix.f
!===============================================================================

!===============================================================================
! ********************** A Sample Session ***********************************
!
! For this session the <> character is the MATLAB prompt.
!
!  <> a=<1 2 3;5 4 6;7 8 9>            <---  you enter this
!  A     =                             <---  MATLAB response
!      1.    2.    3.
!      5.    4.    6.
!      7.    8.    9.
!  <> b=<5;6;7>                        <--- you enter this
!  B     =                             <--- MATLAB response
!      5.
!      6.
!      7.
!  <> a*b             <--- you enter "multiply a and b"
!  ANS   =            <--- MATLAB response
!     38.
!     91.
!    146.
!  <> b*a             <---you enter "multiply b and a"
!     /--ERROR                         <--- MATLAB response
!  INCOMPATIBLE FOR MULTIPLICATION
!  <> det(a)         <--- Take the determinant of a
!  ANS   =           <---MATLAB response
!     18.
!  <> exit           <--- you quit MATLAB
!  total flops        34
!  ADIOS
!-----------------------------------------------------------------------
      SUBROUTINE ML_88(INIT,STRING0)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!     INIT    =  0 FOR ORDINARY FIRST ENTRY WITH READS FROM STDIN
!             = -1 NEGATIVE FOR SILENT INITIALIZATION (IGNORES STRING0)
!             =  1 POSITIVE FOR SUBSEQUENT ENTRIES
!             =  2 SUBSEQUENT ENTRY , RETURN AFTER DOING STRING0
!
!     STRING0 = STRING OF INITIAL COMMANDS
!
      PARAMETER (IALF=78)
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER ALFA(IALF),ALFB(IALF),ALFL,CASE
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      CHARACTER STRING0*(*)
      CHARACTER STRINGQ*1024
      COMMON /ZSTRINGC/STRINGQ
      COMMON /ZSTRINGI/ISTRINGQ,INITQ

!
      DOUBLE PRECISION S,T
      INTEGER EPS(4),FLOPS(4),EYE(4),RAND(4)
!
!     CHARACTER SET
!            0       10       20       30       40       50
!
!     0      0        A        K        U   COLON  :  LESS   <
!     1      1        B        L        V   PLUS   +  GREAT  >
!     2      2        C        M        W   MINUS  -
!     3      3        D        N        X   STAR   *
!     4      4        E        O        Y   SLASH  /
!     5      5        F        P        Z   BSLASH !     6      6        G        Q  BLANK     EQUAL  =
!     7      7        H        R  LPAREN (  DOT    .
!     8      8        I        S  RPAREN )  COMMA  ,
!     9      9        J        T  SEMI   ;  QUOTE  '
!----------------------------------------------------------------
      CHARACTER CH_A*(IALF),CH_B*(IALF)
      COMMON /ML_CHARSZ/ CH_A, CH_B
      SAVE /ML_CHARSZ/
      EXTERNAL ML_CHARS
!----------------------------------------------------------------
      SAVE EPS, EYE, FLOPS, RAND
      DATA EPS/14,25,28,36/,FLOPS/15,21,24,25/
      DATA EYE/14,34,14,36/,RAND/27,10,23,13/
!----------------------------------------------------------------
      STRINGQ=STRING0
      ISTRING0=LEN(STRING0)
      ISTRINGQ=len_trim(STRING0(1:ISTRING0))
      IF(ISTRINGQ.LE.0.AND.INIT.EQ.2)THEN
         STRINGQ='return'
         ISTRINGQ=6
      ENDIF
      INITQ=INIT
!----------------------------------------------------------------
      IF (INIT .NE. 0 .and. INIT .ne. -1) GOTO 90  ! already initialized
      RTE = 5 ! unit number for terminal input
      CALL ML_FILES(RTE,BUF)
      RIO = RTE  ! current file to read commands from
      WTE = 6    ! unit number for terminal  output
      CALL ML_FILES(WTE,BUF)
!
      IF (INIT .GE. 0) then   ! initializing verbose
         call journal('  < M A T L A B >')
         call journal(' Version of 05/25/82')
      endif
!
!     HIO = UNIT NUMBER FOR HELP FILE
      HIO = 11
      CALL ML_FILES(HIO,BUF)  ! open HELP FILE
!
!     RANDOM NUMBER SEED
      RAN(1) = 0
!
!     INITIAL LINE LIMIT
      LCT(2) = 25
!
      ALFL = IALF
      CASE = 0
!     CASE = 1 for file names in lower case
!------------------------------------------------------------------------
      call str2buf(ch_a,alfa,alfl) ! convert string to hollerith
      call str2buf(ch_b,alfb,alfl) ! convert string to hollerith
!------------------------------------------------------------------------
!
      VSIZE = BIGMEM
      LSIZE = 48
      PSIZE = 32
      BOT = LSIZE-3
      CALL ML_WSET(5,0.0D0,0.0D0,STKR(VSIZE-4),STKI(VSIZE-4),1)
      CALL ML_PUTID(IDSTK(1,LSIZE-3),EPS)
      LSTK(LSIZE-3) = VSIZE-4
      MSTK(LSIZE-3) = 1
      NSTK(LSIZE-3) = 1
      S = 1.0D0
   30 S = S/2.0D0
      T = 1.0D0 + S
      IF (T .GT. 1.0D0) GOTO 30
      STKR(VSIZE-4) = 2.0D0*S
      CALL ML_PUTID(IDSTK(1,LSIZE-2),FLOPS)
      LSTK(LSIZE-2) = VSIZE-3
      MSTK(LSIZE-2) = 1
      NSTK(LSIZE-2) = 2
      CALL ML_PUTID(IDSTK(1,LSIZE-1), EYE)
      LSTK(LSIZE-1) = VSIZE-1
      MSTK(LSIZE-1) = -1
      NSTK(LSIZE-1) = -1
      STKR(VSIZE-1) = 1.0D0
      CALL ML_PUTID(IDSTK(1,LSIZE), RAND)
      LSTK(LSIZE) = VSIZE
      MSTK(LSIZE) = 1
      NSTK(LSIZE) = 1
      FMT = 1
      FLP(1) = 0
      FLP(2) = 0
      DDT = 0
      RAN(2) = 0
      PTZ = 0
      PT = PTZ
      ERR = 0
      IF (INIT .EQ. -1) RETURN
!
   90 CONTINUE
      CALL ML_PARSE(INIT)
      IF (FUN .EQ. 1)  CALL ML_MATFN1()
      IF (FUN .EQ. 2)  CALL ML_MATFN2()
      IF (FUN .EQ. 3)  CALL ML_MATFN3()
      IF (FUN .EQ. 4)  CALL ML_MATFN4()
      IF (FUN .EQ. 5)  CALL ML_MATFN5()
      IF (FUN .EQ. 6)  CALL ML_MATFN6()
      IF (FUN .EQ. 21) CALL ML_MATFN1()
      IF (FUN .NE. 99) GOTO 90
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_PARSE(INIT)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)

      character*80 mline
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      LOGICAL ML_EQID
      PARAMETER (IALF=78)
      INTEGER SEMI,EQUAL,EOL,ID(4),EXCNT,LPAREN,RPAREN,COLON,PTS,ALFL
      INTEGER BLANK,COMMA,LESS,GREAT,NAME,ANS(4),ENND(4),ELSE(4),P,R
      save BLANK,SEMI,EQUAL,EOL,COMMA,COLON
      save LPAREN,RPAREN,LESS,GREAT,NAME,ALFL
      save ANS,ENND,ELSE
      DATA BLANK/36/,SEMI/39/,EQUAL/46/,EOL/99/,COMMA/48/,COLON/40/
      DATA LPAREN/37/,RPAREN/38/,LESS/50/,GREAT/51/,NAME/1/,ALFL/IALF/
      DATA ANS/10,23,28,36/,ENND/14,23,13,36/,ELSE/14,21,28,14/
!
   01 CONTINUE
      R = 0
      IF (ERR .GT. 0) PTZ = 0
      IF (ERR.LE.0 .AND. PT.GT.PTZ) R = RSTK(PT)
      IF (DDT .EQ. 1) THEN
         WRITE(MLINE,'('' PARSE'',4I4)') PT,R,PTZ,ERR
         CALL JOURNAL(MLINE)
      ENDIF
      IF (R.EQ.15) GOTO 93
      IF (R.EQ.16 .OR. R.EQ.17) GOTO 94
      SYM = EOL
      TOP = 0
      IF (RIO .NE. RTE) CALL ML_FILES(-RIO,BUF)
      RIO = RTE
      LCT(3) = 0
      LCT(4) = 2
      LPT(1) = 1
   10 CONTINUE
      IF (SYM.EQ.EOL.AND.MOD(LCT(4)/2,2).EQ.1) CALL ML_PROMPT(LCT(4)/4)
      IF (SYM .EQ. EOL) CALL ML_GETLIN()
      ERR = 0
      PT = PTZ
   15 CONTINUE
      EXCNT = 0
      IF (DDT .EQ. 1) THEN
         MLINE='STATE'
         CALL APPNUM(REAL(PT),MLINE,ILEN,IERR)
         CALL APPNUM(REAL(TOP),MLINE,ILEN,IERR)
         call journal(mline)
      ENDIF
      LHS = 1
      CALL ML_PUTID(ID,ANS)
      CALL ML_GETSYM
      IF (SYM.EQ.COLON .AND. CHRA.EQ.EOL) DDT = 1-DDT
      IF (SYM .EQ. COLON) CALL ML_GETSYM
      IF (SYM.EQ.SEMI .OR. SYM.EQ.COMMA .OR. SYM.EQ.EOL) GOTO 80
      IF (SYM .EQ. NAME) GOTO 20
      IF (SYM .EQ. LESS) GOTO 40
      IF (SYM .EQ. GREAT) GOTO 45
      GOTO 50
!.......................................................................
!     LHS BEGINS WITH NAME
   20 CONTINUE
      CALL ML_COMAND(SYN)
      IF (ERR .GT. 0) GOTO 01
      IF (FUN .EQ. 99) GOTO 95
      IF (FIN .EQ. -15) GOTO 80
      IF (FIN .LT. 0) GOTO 91
      IF (FIN .GT. 0) GOTO 70
!     IF NAME IS A FUNCTION, MUST BE RHS
      RHS = 0
      CALL ML_FUNS(SYN)
      IF (FIN .NE. 0) GOTO 50
!     PEEK ONE CHARACTER AHEAD
      IF (CHRA.EQ.SEMI .OR. CHRA.EQ.COMMA .OR. CHRA.EQ.EOL) CALL ML_PUTID(ID,SYN)
      IF (CHRA .EQ. EQUAL) GOTO 25
      IF (CHRA .EQ. LPAREN) GOTO 30
      GOTO 50
!.......................................................................
!     LHS IS SIMPLE VARIABLE
   25 CONTINUE
      CALL ML_PUTID(ID,SYN)
      CALL ML_GETSYM
      CALL ML_GETSYM
      GOTO 50
!.......................................................................
!     LHS IS NAME(...)
   30 CONTINUE
      LPT(5) = LPT(4)
      CALL ML_PUTID(ID,SYN)
      CALL ML_GETSYM
   32 CONTINUE
      CALL ML_GETSYM
      EXCNT = EXCNT+1
      PT = PT+1
      CALL ML_PUTID(IDS(1,PT), ID)
      PSTK(PT) = EXCNT
      RSTK(PT) = 1
!     *CALL* EXPR
      GOTO 92
!.......................................................................
   35 CONTINUE
      CALL ML_PUTID(ID,IDS(1,PT))
      EXCNT = PSTK(PT)
      PT = PT-1
      IF (SYM .EQ. COMMA) GOTO 32
      IF (SYM .NE. RPAREN) CALL ML_ERROR(3)
      IF (ERR .GT. 0) GOTO 01
      IF (ERR .GT. 0) RETURN
      IF (SYM .EQ. RPAREN) CALL ML_GETSYM
      IF (SYM .EQ. EQUAL) GOTO 50
!     LHS IS REALLY RHS, FORGET SCAN JUST DONE
      TOP = TOP - EXCNT
      LPT(4) = LPT(5)
      CHRA = LPAREN
      SYM = NAME
      CALL ML_PUTID(SYN,ID)
      CALL ML_PUTID(ID,ANS)
      EXCNT = 0
      GOTO 50
!.......................................................................
!     MULTIPLE LHS
   40 CONTINUE
      LPT(5) = LPT(4)
      PTS = PT
      CALL ML_GETSYM
   41 CONTINUE
      IF (SYM .NE. NAME) GOTO 43
      CALL ML_PUTID(ID,SYN)
      CALL ML_GETSYM
      IF (SYM .EQ. GREAT) GOTO 42
      IF (SYM .EQ. COMMA) CALL ML_GETSYM
      PT = PT+1
      LHS = LHS+1
      PSTK(PT) = 0
      CALL ML_PUTID(IDS(1,PT),ID)
      GOTO 41
   42 CONTINUE
      CALL ML_GETSYM
      IF (SYM .EQ. EQUAL) GOTO 50
   43 CONTINUE
      LPT(4) = LPT(5)
      PT = PTS
      LHS = 1
      SYM = LESS
      CHRA = LPT(4)-1
      CHRA = LIN(CHRA)
      CALL ML_PUTID(ID,ANS)
      GOTO 50
!.......................................................................
!     MACRO STRING
   45 CONTINUE
      CALL ML_GETSYM
      IF (DDT .EQ. 1) THEN
         MLINE='MACRO'
         CALL APPNUM(REAL(PT),MLINE,ILEN,IERR)
         CALL APPNUM(REAL(TOP),MLINE,ILEN,IERR)
      ENDIF
      IF (SYM.EQ.LESS .AND. CHRA.EQ.EOL) CALL ML_ERROR(28)
      IF (ERR .GT. 0) GOTO 01
      PT = PT+1
      RSTK(PT) = 20
!     *CALL* EXPR
      GOTO 92
!.......................................................................
   46 CONTINUE
      PT = PT-1
      IF (SYM.NE.LESS .AND. SYM.NE.EOL) CALL ML_ERROR(37)
      IF (ERR .GT. 0) GOTO 01
      IF (SYM .EQ. LESS) CALL ML_GETSYM
      K = LPT(6)
      LIN(K+1) = LPT(1)
      LIN(K+2) = LPT(2)
      LIN(K+3) = LPT(6)
      LPT(1) = K + 4
!     TRANSFER STACK TO INPUT LINE
      K = LPT(1)
      L = LSTK(TOP)
      N = MSTK(TOP)*NSTK(TOP)
      DO 48 J = 1, N
         LS = L + J-1
         LIN(K) = IDINT(STKR(LS))
         IF (LIN(K).LT.0 .OR. LIN(K).GE.ALFL) CALL ML_ERROR(37)
         IF (ERR .GT. 0) RETURN
         IF (K.LT.1024) K = K+1
         IF (K.EQ.1024) then
            WRITE(mline,47) K
   47       FORMAT(1X,'INPUT BUFFER LIMIT IS ',I4,' CHARACTERS.')
            call journal(mline)
          endif
   48 CONTINUE
      TOP = TOP-1
      LIN(K) = EOL
      LPT(6) = K
      LPT(4) = LPT(1)
      LPT(3) = 0
      LPT(2) = 0
      LCT(1) = 0
      CHRA = BLANK
      PT = PT+1
      PSTK(PT) = LPT(1)
      RSTK(PT) = 21
!     *CALL* PARSE
      GOTO 15
!.......................................................................
   49 CONTINUE
      PT = PT-1
      IF (DDT .EQ. 1)then
         WRITE(mline,'('' MACEND '',2I4)') PT,TOP
         call journal(mline)
      endif
      K = LPT(1) - 4
      LPT(1) = LIN(K+1)
      LPT(4) = LIN(K+2)
      LPT(6) = LIN(K+3)
      CHRA = BLANK
      CALL ML_GETSYM
      GOTO 80
!.......................................................................
!     LHS FINISHED, START RHS
   50 CONTINUE
      IF (SYM .EQ. EQUAL) CALL ML_GETSYM
      PT = PT+1
      CALL ML_PUTID(IDS(1,PT),ID)
      PSTK(PT) = EXCNT
      RSTK(PT) = 2
!     *CALL* EXPR
      GOTO 92
   55 CONTINUE
      IF (SYM.EQ.SEMI .OR. SYM.EQ.COMMA .OR. SYM.EQ.EOL) GOTO 60
      IF (SYM.EQ.NAME .AND. ML_EQID(SYN,ELSE)) GOTO 60
      IF (SYM.EQ.NAME .AND. ML_EQID(SYN,ENND)) GOTO 60
      CALL ML_ERROR(40)
      IF (ERR .GT. 0) GOTO 01
!
!     STORE RESULTS
   60 CONTINUE
      RHS = PSTK(PT)
      CALL ML_STACKP(IDS(1,PT))
      IF (ERR .GT. 0) GOTO 01
      PT = PT-1
      LHS = LHS-1
      IF (LHS .GT. 0) GOTO 60
      GOTO 70
!.......................................................................
!
!     UPDATE AND POSSIBLY PRINT OPERATION COUNTS
   70 CONTINUE
      K = FLP(1)
      IF (K .NE. 0) STKR(VSIZE-3) = DFLOAT(K)
      STKR(VSIZE-2) = STKR(VSIZE-2) + DFLOAT(K)
      FLP(1) = 0
      IF (.NOT.(CHRA.EQ.COMMA .OR. (SYM.EQ.COMMA .AND. CHRA.EQ.EOL)))GOTO 80
      CALL ML_GETSYM
      I5 = 10**5
   71 CONTINUE
      IF (K .EQ. 0) call journal('   no flops')
      IF (K .EQ. 1) call journal('    1 flop')
      IF (1.LT.K .AND. K.LT.100000)THEN
         WRITE(mline,'(1X,I5,'' flops'')') K
         call journal(mline)
      ENDIF
      IF (100000 .LE. K)then
         WRITE(mline,'(1x,i9,'' flops'')') K
         call journal(mline)
      endif
      GOTO 80
!.......................................................................
!
!     FINISH STATEMENT
   80 CONTINUE
      FIN = 0
      P = 0
      R = 0
      IF (PT .GT. 0) P = PSTK(PT)
      IF (PT .GT. 0) R = RSTK(PT)
      IF (DDT .EQ. 1)then
         WRITE(mline,'('' FINISH'',5I4)') PT,PTZ,P,R,LPT(1)
         call journal(mline)
      endif
      IF (SYM.EQ.COMMA .OR. SYM.EQ.SEMI) GOTO 15
      IF (R.EQ.21 .AND. P.EQ.LPT(1)) GOTO 49
      IF (PT .GT. PTZ) GOTO 91
      GOTO 10
!.......................................................................
!
!     SIMULATE RECURSION
   91 CONTINUE
      CALL ML_CLAUSE
      IF (ERR .GT. 0) GOTO 01
      IF (PT .LE. PTZ) GOTO 15
      R = RSTK(PT)
      IF (R .EQ. 21) GOTO 49
      GOTO (99,99,92,92,92,99,99,99,99,99,99,99,15,15,99,99,99,99,99),R
!
   92 CONTINUE
      CALL ML_EXPR
      IF (ERR .GT. 0) GOTO 01
      R = RSTK(PT)
      GOTO (35,55,91,91,91,93,93,99,99,94,94,99,99,99,99,99,99,94,94,46),R
!
   93 CONTINUE
      CALL ML_TERM
      IF (ERR .GT. 0) GOTO 01
      R = RSTK(PT)
      GOTO (99,99,99,99,99,92,92,94,94,99,99,99,99,99,95,99,99,99,99),R
!
   94 CONTINUE
      CALL ML_FACTOR()
      IF (ERR .GT. 0) GOTO 01
      R = RSTK(PT)
      GOTO (99,99,99,99,99,99,99,93,93,92,92,94,99,99,99,95,95,92,92),R
!.......................................................................
!
!     CALL ML_MATFNS BY RETURNING TO MATLAB
   95 CONTINUE
      if(TOP.LT.1)then
         !call journal('*ml_parse* stack emptied',top)
      else
         IF (FIN.GT.0 .AND. MSTK(TOP).LT.0) CALL ML_ERROR(14)
      endif
      IF (ERR .GT. 0) GOTO 01
      RETURN
!.......................................................................
!
   99 CONTINUE
      CALL ML_ERROR(22)
      GOTO 01
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_CLAUSE
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      character*256 mline
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ

      INTEGER FOR(4),WHILE(4),IFF(4),ELSE(4),ENND(4),DO(4),THENN(4)
      INTEGER SEMI,EQUAL,EOL,BLANK,R
      INTEGER OP,COMMA,LESS,GREAT,NAME
      LOGICAL ML_EQID
      DOUBLE PRECISION E1,E2
      SAVE SEMI,EQUAL,EOL,BLANK
      SAVE COMMA,LESS,GREAT,NAME
      SAVE FOR,WHILE,IFF
      SAVE ELSE,ENND
      SAVE DO,THENN
      DATA SEMI/39/,EQUAL/46/,EOL/99/,BLANK/36/
      DATA COMMA/48/,LESS/50/,GREAT/51/,NAME/1/
      DATA FOR/15,24,27,36/,WHILE/32,17,18,21/,IFF/18,15,36,36/
      DATA ELSE/14,21,28,14/,ENND/14,23,13,36/
      DATA DO/13,24,36,36/,THENN/29,17,14,23/
      R = -FIN-10
      FIN = 0
      IF (DDT .EQ. 1)then
         WRITE(mline,'('' CLAUSE '',3I4)') PT,RSTK(PT),R
         call journal(mline)
      endif
      IF (R.LT.1 .OR. R.GT.6) GOTO 01
      GOTO (02,30,30,80,99,90),R
   01 R = RSTK(PT)
      GOTO (99,99,05,40,45,99,99,99,99,99,99,99,15,55,99,99,99),R
      call journal('*ml_clause* -- internal error')
      goto 99
!.......................................................................
!     FOR
!
   02 CALL ML_GETSYM
      IF (SYM .NE. NAME) CALL ML_ERROR(34) ! improper for clause
      IF (ERR .GT. 0) RETURN
      PT = PT+2
      CALL ML_PUTID(IDS(1,PT),SYN)
      CALL ML_GETSYM
      IF (SYM .NE. EQUAL) CALL ML_ERROR(34) ! improper for clause
      IF (ERR .GT. 0) RETURN
      CALL ML_GETSYM
      RSTK(PT) = 3
!     *CALL* EXPR
      RETURN
   05 PSTK(PT-1) = 0
      PSTK(PT) = LPT(4) - 1
      IF (ML_EQID(SYN,DO)) SYM = SEMI
      IF (SYM .EQ. COMMA) SYM = SEMI
      IF (SYM .NE. SEMI) CALL ML_ERROR(34) ! improper for clause
      IF (ERR .GT. 0) RETURN
   10 J = PSTK(PT-1)
      LPT(4) = PSTK(PT)
      SYM = SEMI
      CHRA = BLANK
      J = J+1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      LJ = L+(J-1)*M
      L2 = L + M*N
      IF (M .NE. -3) GOTO 12
      LJ = L+3
      L2 = LJ
      STKR(LJ) = STKR(L) + DFLOAT(J-1)*STKR(L+1)
      STKI(LJ) = 0.0
      IF (STKR(L+1).GT.0.0D0 .AND. STKR(LJ).GT.STKR(L+2)) GOTO 20
      IF (STKR(L+1).LT.0.0D0 .AND. STKR(LJ).LT.STKR(L+2)) GOTO 20
      M = 1
      N = J
   12 IF (J .GT. N) GOTO 20
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18) ! too many names
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L2
      MSTK(TOP) = M
      NSTK(TOP) = 1
      ERR = L2+M - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)   ! too much memory required
      IF (ERR .GT. 0) RETURN
      CALL ML_WCOPY(M,STKR(LJ),STKI(LJ),1,STKR(L2),STKI(L2),1)
      RHS = 0
      CALL ML_STACKP(IDS(1,PT))
      IF (ERR .GT. 0) RETURN
      PSTK(PT-1) = J
      PSTK(PT) = LPT(4)
      RSTK(PT) = 13
!     *CALL* PARSE
      RETURN
   15 GOTO 10
   20 MSTK(TOP) = 0
      NSTK(TOP) = 0
      RHS = 0
      CALL ML_STACKP(IDS(1,PT))
      IF (ERR .GT. 0) RETURN
      PT = PT-2
      GOTO 80
!.......................................................................
!
!     WHILE OR IF
!
   30 PT = PT+1
      CALL ML_PUTID(IDS(1,PT),SYN)
      PSTK(PT) = LPT(4)-1
   35 LPT(4) = PSTK(PT)
      CHRA = BLANK
      CALL ML_GETSYM
      RSTK(PT) = 4
!     *CALL* EXPR
      RETURN
   40 IF (SYM.NE.EQUAL .AND. SYM.NE.LESS .AND. SYM.NE.GREAT)CALL ML_ERROR(35)    ! improper WHILE or IF clause
      IF (ERR .GT. 0) RETURN
      OP = SYM
      CALL ML_GETSYM
      IF (SYM.EQ.EQUAL .OR. SYM.EQ.GREAT) OP = OP + SYM
      IF (OP .GT. GREAT) CALL ML_GETSYM
      PSTK(PT) = 256*PSTK(PT) + OP
      RSTK(PT) = 5
!     *CALL* EXPR
      RETURN
   45 OP = MOD(PSTK(PT),256)
      PSTK(PT) = PSTK(PT)/256
      L = LSTK(TOP-1)
      E1 = STKR(L)
      L = LSTK(TOP)
      E2 = STKR(L)
      TOP = TOP - 2
      IF (ML_EQID(SYN,DO) .OR. ML_EQID(SYN,THENN)) SYM = SEMI
      IF (SYM .EQ. COMMA) SYM = SEMI
      IF (SYM .NE. SEMI) CALL ML_ERROR(35) ! improper WHILE or IF clause
      IF (ERR .GT. 0) RETURN
      IF (OP.EQ.EQUAL         .AND. E1.EQ.E2) GOTO 50
      IF (OP.EQ.LESS          .AND. E1.LT.E2) GOTO 50
      IF (OP.EQ.GREAT         .AND. E1.GT.E2) GOTO 50
      IF (OP.EQ.(LESS+EQUAL)  .AND. E1.LE.E2) GOTO 50
      IF (OP.EQ.(GREAT+EQUAL) .AND. E1.GE.E2) GOTO 50
      IF (OP.EQ.(LESS+GREAT)  .AND. E1.NE.E2) GOTO 50
      PT = PT-1
      GOTO 80
   50 RSTK(PT) = 14
!     *CALL* PARSE
      RETURN
   55 IF (ML_EQID(IDS(1,PT),WHILE)) GOTO 35
      PT = PT-1
      IF (ML_EQID(SYN,ELSE)) GOTO 80
      RETURN
!.......................................................................
!
!     SEARCH FOR MATCHING END OR ELSE
   80 KOUNT = 0
      CALL ML_GETSYM
   82 IF (SYM .EQ. EOL) RETURN
      IF (SYM .NE. NAME) GOTO 83
      IF (ML_EQID(SYN,ENND) .AND. KOUNT.EQ.0) RETURN
      IF (ML_EQID(SYN,ELSE) .AND. KOUNT.EQ.0) RETURN
      IF (ML_EQID(SYN,ENND) .OR. ML_EQID(SYN,ELSE))KOUNT = KOUNT-1
      IF (ML_EQID(SYN,FOR) .OR. ML_EQID(SYN,WHILE).OR.ML_EQID(SYN,IFF)) KOUNT = KOUNT+1
   83 CALL ML_GETSYM
      GOTO 82
!.......................................................................
!
!     EXIT FROM LOOP
   90 IF (DDT .EQ. 1)then
         WRITE(mline,'('' EXIT '',10I4)') (RSTK(I),I=1,PT)
         call journal(mline)
      endif
      IF (RSTK(PT) .EQ. 14) PT = PT-1
      IF (PT .LE. PTZ) RETURN
      IF (RSTK(PT) .EQ. 14) PT = PT-1
      IF (PT-1 .LE. PTZ) RETURN
      IF (RSTK(PT) .EQ. 13) TOP = TOP-1
      IF (RSTK(PT) .EQ. 13) PT = PT-2
      GOTO 80
!.......................................................................
!
   99 CALL ML_ERROR(22)    ! recursion difficulties
      IF (ERR .GT. 0) RETURN
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_COMAND(ID)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      INTEGER ID(4)
      character*256 mline

      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ

      PARAMETER (IALF=78)
      INTEGER ALFA(IALF),ALFB(IALF),ALFL,CASE
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE

      INTEGER CMD(4,17),CMDL,A,D,E,Z,LRECL,CH,BLANK,NAME,DOT,H(4)
      INTEGER SEMI,COMMA,EOL
      LOGICAL ML_EQID
      INTEGER A2, Z2
      PARAMETER(A2=52,Z2=77)
      SAVE CMDL,A,D,E,Z,EOL,SEMI,COMMA,BLANK,NAME,DOT
      DOUBLE PRECISION ML_URAND
      EXTERNAL ML_URAND
      parameter(linelen=255)
      save LRECL
      DATA CMDL/17/,A/10/,D/13/,E/14/,Z/35/,EOL/99/,SEMI/39/,COMMA/48/
      DATA BLANK/36/,NAME/1/,DOT/47/
!.......................................................................
!                         1         2         3         4         5
!       COUNT   01234567890123456789012345678901234567890123456789012
!               0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ();:+-*/\=.,''<>
!
!       CLEAR ELSE  END   EXIT
!       FOR   HELP  IF    LONG
!       RETUR SEMI
!       SHORT WHAT  WHILE
!       WHO   SH    LALA  SHELL
      DATA CMD/                                                       &
     &  12,21,14,10, 14,21,28,14, 14,23,13,36, 14,33,18,29,           &
     &  15,24,27,36, 17,14,21,25, 18,15,36,36, 21,24,23,16,           &
     &  27,14,29,30, 28,14,22,18,                                     &
     &  28,17,24,27, 32,17,10,29, 32,17,18,21,                        &
     &  32,17,24,36, 28,17,36,36, 21,10,21,10, 28,17,14,21/
      DATA LRECL/LINELEN/
!
      IF (DDT .EQ. 1)call journal('COMAND')
      FUN = 0
      DO 10 K = 1, CMDL
        IF (ML_EQID(ID,CMD(1,K))) GOTO 20
   10 CONTINUE
      FIN = 0
      RETURN
!
   20 CONTINUE
      IF (CHRA.EQ.COMMA .OR. CHRA.EQ.SEMI .OR. CHRA.EQ.EOL) GOTO 22
      IF ((CHRA.LE.Z.OR.(CHRA.GE.A2.AND.CHRA.LE.Z2)) .OR. K.EQ.6)GOTO 22  ! if alphanumeric or K=6
      CALL ML_ERROR(16) ! improper command
      RETURN
!
   22 CONTINUE
      FIN = 1
      GOTO (25,36,38,40,30,80,34,52,44,55,50,65,32,60,70,46,48),K
!.......................................................................
!     CLEAR
   25 CONTINUE
      IF ((CHRA.GE.A.AND.CHRA.LE.Z).or.(CHRA.GE.A2.AND.CHRA.LE.Z2)) GOTO 26 ! alphameric character
      BOT = LSIZE-3
      GOTO 98
   26 CONTINUE
      CALL ML_GETSYM
      TOP = TOP+1
      MSTK(TOP) = 0
      NSTK(TOP) = 0
      RHS = 0
      CALL ML_STACKP(SYN)
      IF (ERR .GT. 0) RETURN
      FIN = 1
      GOTO 98
!.......................................................................
!     FOR, WHILE, IF, ELSE, END
   30 FIN = -11
      GOTO 99
   32 FIN = -12
      GOTO 99
   34 FIN = -13
      GOTO 99
   36 FIN = -14
      GOTO 99
   38 FIN = -15
      GOTO 99
!.......................................................................
!     EXIT
   40 CONTINUE
      IF (PT .GT. PTZ) FIN = -16
      IF (PT .GT. PTZ) GOTO 98
      K = IDINT(STKR(VSIZE-2))
      call journal('sc',' total flops ',k)

      ! for serendipity's sake
      ii=ML_URAND(RAN(1))*9
      if(ii.le.1)then
         call journal(' adios')
       elseif(ii.eq.2)then
         call journal(' adieu')
       elseif(ii.eq.3)then
         call journal(' arrivederci')
       elseif(ii.eq.4)then
         call journal(' au revior')
       elseif(ii.eq.5)then
         call journal(' so long')
       elseif(ii.eq.6)then
         call journal(' sayonara')
       elseif(ii.eq.7)then
         call journal(' auf wiedersehen')
       else
         call journal(' cheerio')
       endif

      FUN = 99
      GOTO 98
!.......................................................................
!     RETURN
   44 CONTINUE
      K = LPT(1) - 7
      IF (K .LE. 0) FUN = 99
      IF (K .LE. 0) GOTO 98
      CALL ML_FILES(-RIO,BUF)
      LPT(1) = LIN(K+1)
      LPT(4) = LIN(K+2)
      LPT(6) = LIN(K+3)
      PTZ = LIN(K+4)
      RIO = LIN(K+5)
      LCT(4) = LIN(K+6)
      CHRA = BLANK
      SYM = COMMA
      GOTO 99
!.......................................................................
!     LALA
   46 CONTINUE
      call journal('QUIT SINGING AND GET BACK TO WORK.')
      GOTO 98
!.......................................................................
!     FOO
   48 CONTINUE
      call journal(' Your place or mine?')
      GOTO 98
!.......................................................................
!     SHORT, LONG
   50 CONTINUE
      FMT = 1
      GOTO 54
   52 CONTINUE
      FMT = 2
   54 IF (CHRA.EQ.E .OR. CHRA.EQ.D) FMT = FMT+2
      IF (CHRA .EQ. Z) FMT = 5
      IF (CHRA.EQ.E .OR. CHRA.EQ.D .OR. CHRA.EQ.Z) CALL ML_GETSYM()
      GOTO 98
!.......................................................................
!     SEMI
   55 LCT(3) = 1 - LCT(3)
      GOTO 98
!.......................................................................
!     WHO
   60 CONTINUE
      call journal(' Your current variables are...')
      CALL ML_PRNTID(IDSTK(1,BOT),LSIZE-BOT+1)
      L = VSIZE-LSTK(BOT)+1
      WRITE(mline,161) L,VSIZE
  161 FORMAT(1X,'using ',I7,' out of ',I7,' elements.')
      call journal(mline)
      GOTO 98
!.......................................................................
!     WHAT
   65 CONTINUE
      call journal('The functions and commands are...')
      H(1) = 0
      CALL ML_FUNS(H)
      CALL ML_PRNTID(CMD,CMDL-2)
      GOTO 98
!.......................................................................
!     SH
   70 CONTINUE                     ! call system shell interactively or passing command
      IF (CHRA .eq. EOL )then      ! if next character on stack is end-of-line call interactive shell
          call execute_command_line('/bin/sh',cmdstat=istat) ! call shell interactively
      else                         ! there were characters after SH on the line
          call buf2str(mline,buf(4),lrecl) ! pass ENTIRE line
          call execute_command_line(MLINE(:len_trim(mline)),cmdstat=istat)           ! call system shell
          CALL ML_GETLIN()         ! start a new line because gave all of this one to shell
          if(istat.ne.0)then
             CALL JOURNAL('sc','*SH* RETURN=',ISTAT)
          endif
      endif
      GOTO 98
!.......................................................................
!     HELP
   80 CONTINUE
      IF (CHRA .EQ. EOL) THEN
         call journal('Type "help" followed by ...')
         call journal(' INTRO   (To get started)')
         call journal(' NEWS    (recent revisions)')
         H(1) = 0
         CALL ML_FUNS(H)
         CALL ML_PRNTID(CMD,CMDL-2)
         J = BLANK+2
         call journal(' ans   EDIT  FILE  FUN   MACRO')
         !-------------------------------------------------
         ! write ALFA(J) to ALFA(A2-1) one string at a time
81       CONTINUE
         jj=j+16
         jj=min(jj,a2-1)
         WRITE(MLINE,'(1X,17(A1,1X))')(CHAR(ALFA(I)),I=J,A2-1)
         call journal(mline)
         IF(jj.lt.A2-1)goto 81
         !-------------------------------------------------
         GOTO 98
      ENDIF
!
      CALL ML_GETSYM()
      IF (SYM .NE. NAME) THEN
         IF (SYM .EQ. 0) SYM = DOT
         H(1) = ALFA(SYM+1)
         H(2) = ALFA(BLANK+1)
         H(3) = ALFA(BLANK+1)
         H(4) = ALFA(BLANK+1)
      ELSE
         DO 83 I = 1, 4
           CH = SYN(I)
           H(I) = ALFA(CH+1)
   83    CONTINUE
      ENDIF

84    CONTINUE
      IF(HIO .NE. 0) THEN
         READ(HIO,'(a)',END=89) mline   ! read line from help file
         call str2buf(mline,buf,lrecl)  ! convert string to ADE array
         DO 85 I = 1, 4                 ! look for match of 4 chars of topic in first 4 chars
            IF (H(I) .NE. BUF(I)) GOTO 84
   85    CONTINUE
         call journal(' ')
   86    CONTINUE
         !-------------------------------------------------
         ! find last non-blank character in line
         K = LRECL + 1
   87    CONTINUE
         K = K - 1
         IF(K.LE.0) THEN  ! blank line
            K=1
         ELSEIF (BUF(K) .EQ. ALFA(BLANK+1)) THEN
            GOTO 87
         ENDIF
         !-------------------------------------------------
         call buf2str(mline,buf,k)
         call journal(mline(1:k))
         READ(HIO,'(a)') mline
         call str2buf(mline,buf,lrecl)
         IF (BUF(1) .EQ. ALFA(BLANK+1)) GOTO 86
         CALL ML_FILES(-HIO,BUF)
         GOTO 98
      ENDIF
!
   89 CONTINUE
      call buf2str(mline,h,4)
      mline=' SORRY, NO HELP ON '//mline(1:4)
      call journal(mline)
      CALL ML_FILES(-HIO,BUF)
      GOTO 98
!.......................................................................
   98 CONTINUE
      CALL ML_GETSYM
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_EXPR
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)

      CHARACTER MLINE*80
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER OP,R,BLANK,SIGN,PLUS,MINUS,NAME,COLON,EYE(4)
      SAVE COLON,BLANK,PLUS,MINUS,NAME,EYE
      DATA COLON/40/,BLANK/36/,PLUS/41/,MINUS/42/,NAME/1/
      DATA EYE/14,34,14,36/
      IF (DDT .EQ. 1) THEN
         WRITE(MLINE,'('' EXPR '',2I4)') PT,RSTK(PT)
         CALL JOURNAL(MLINE)
      ENDIF
      R = RSTK(PT)
      GOTO (01,01,01,01,01,05,25,99,99,01,01,99,99,99,99,99,99,01,01,01),R
   01 CONTINUE
      IF (SYM .EQ. COLON) CALL ML_PUTID(SYN,EYE)
      IF (SYM .EQ. COLON) SYM = NAME
      KOUNT = 1
   02 CONTINUE
      SIGN = PLUS
      IF (SYM .EQ. MINUS) SIGN = MINUS
      IF (SYM.EQ.PLUS .OR. SYM.EQ.MINUS) CALL ML_GETSYM
      PT = PT+1
      IF (PT .GT. PSIZE-1) CALL ML_ERROR(26) ! too complicated (stack overflow)
      IF (ERR .GT. 0) RETURN
      PSTK(PT) = SIGN + 256*KOUNT
      RSTK(PT) = 6
!     *CALL* TERM
      RETURN
   05 CONTINUE
      SIGN = MOD(PSTK(PT),256)
      KOUNT = PSTK(PT)/256
      PT = PT-1
      IF (SIGN .EQ. MINUS) CALL ML_STACK1(MINUS)
      IF (ERR .GT. 0) RETURN
   10 CONTINUE
      IF (SYM.EQ.PLUS .OR. SYM.EQ.MINUS) GOTO 20
      GOTO 50
   20 CONTINUE
      IF (RSTK(PT) .NE. 10) GOTO 21
!     BLANK IS DELIMITER INSIDE ANGLE BRACKETS
      LS = LPT(3) - 2
      IF (LIN(LS) .EQ. BLANK) GOTO 50
   21 CONTINUE
      OP = SYM
      CALL ML_GETSYM
      PT = PT+1
      PSTK(PT) = OP + 256*KOUNT
      RSTK(PT) = 7
!     *CALL* TERM
      RETURN
   25 CONTINUE
      OP = MOD(PSTK(PT),256)
      KOUNT = PSTK(PT)/256
      PT = PT-1
      CALL ML_STACK2(OP)
      IF (ERR .GT. 0) RETURN
      GOTO 10
   50 CONTINUE
      IF (SYM .NE. COLON) GOTO 60
      CALL ML_GETSYM
      KOUNT = KOUNT+1
      GOTO 02
   60 CONTINUE
      IF (KOUNT .GT. 3) CALL ML_ERROR(33)  ! too many colons
      IF (ERR .GT. 0) RETURN
      RHS = KOUNT
      IF (KOUNT .GT. 1) CALL ML_STACK2(COLON)
      IF (ERR .GT. 0) RETURN
      RETURN
   99 CONTINUE
      CALL ML_ERROR(22)     ! recursion difficulties
      IF (ERR .GT. 0) RETURN
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_FACTOR()
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)

      character*80 mline
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      PARAMETER(IALF=78)
      INTEGER SEMI,EOL,BLANK,R,ID(4),EXCNT,LPAREN,RPAREN
      INTEGER STAR,DSTAR,COMMA,LESS,GREAT,QUOTE,NUM,NAME
      SAVE DSTAR,SEMI,EOL,BLANK
      SAVE STAR,COMMA,LPAREN,RPAREN
      SAVE LESS,GREAT,QUOTE,NUM,NAME
      DATA DSTAR/54/,SEMI/39/,EOL/99/,BLANK/36/
      DATA STAR/43/,COMMA/48/,LPAREN/37/,RPAREN/38/
      DATA LESS/50/,GREAT/51/,QUOTE/49/,NUM/0/,NAME/1/
      IF (DDT .EQ. 1) then
         WRITE(mline,'('' FACTOR '',3I4)') PT,RSTK(PT),SYM
         call journal(mline)
      endif
      R = RSTK(PT)
      GOTO (99,99,99,99,99,99,99,01,01,25,45,65,99,99,99,55,75,32,37),R
   01 CONTINUE
      IF (SYM.EQ.NUM .OR. SYM.EQ.QUOTE .OR.  SYM.EQ.LESS) GOTO 10
      IF (SYM .EQ. GREAT) GOTO 30
      EXCNT = 0
      IF (SYM .EQ. NAME) GOTO 40
      ID(1) = BLANK
      IF (SYM .EQ. LPAREN) GOTO 42
      CALL ML_ERROR(2)
      IF (ERR .GT. 0) RETURN
!
!     PUT SOMETHING ON THE STACK
   10 CONTINUE
      L = 1
      IF (TOP .GT. 0) L = LSTK(TOP) + MSTK(TOP)*NSTK(TOP)
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L
      IF (SYM .EQ. QUOTE) GOTO 15
      IF (SYM .EQ. LESS) GOTO 20
!
!     SINGLE NUMBER, GETSYM STORED IT IN STKI
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      STKR(L) = STKI(VSIZE)
      STKI(L) = 0.0D0
      CALL ML_GETSYM
      GOTO 60
!
!     STRING
   15 CONTINUE
      N = 0
      LPT(4) = LPT(3)
      CALL ML_GETCH()  ! get next character
   16 CONTINUE
      IF (CHRA .EQ. QUOTE) GOTO 18
   17 CONTINUE
      LN = L+N
      IF (CHRA .EQ. EOL) CALL ML_ERROR(31)
      IF (ERR .GT. 0) RETURN
      STKR(LN) = DFLOAT(CHRA)
      STKI(LN) = 0.0D0
      N = N+1
      CALL ML_GETCH()  ! get next character
      GOTO 16
   18 CONTINUE
      CALL ML_GETCH()  ! get next character
      IF (CHRA .EQ. QUOTE) GOTO 17
      IF (N .LE. 0) CALL ML_ERROR(31)
      IF (ERR .GT. 0) RETURN
      MSTK(TOP) = 1
      NSTK(TOP) = N
      CALL ML_GETSYM
      GOTO 60
!
!     EXPLICIT MATRIX
   20 CONTINUE
      MSTK(TOP) = 0
      NSTK(TOP) = 0
   21 CONTINUE
      TOP = TOP + 1
      LSTK(TOP) = LSTK(TOP-1) + MSTK(TOP-1)*NSTK(TOP-1)
      MSTK(TOP) = 0
      NSTK(TOP) = 0
      CALL ML_GETSYM
   22 CONTINUE
      IF (SYM.EQ.SEMI .OR. SYM.EQ.GREAT .OR. SYM.EQ.EOL) GOTO 27
      IF (SYM .EQ. COMMA) CALL ML_GETSYM
      PT = PT+1
      RSTK(PT) = 10
!     *CALL* EXPR
      RETURN
   25 CONTINUE
      PT = PT-1
      TOP = TOP - 1
      IF (MSTK(TOP) .EQ. 0) MSTK(TOP) = MSTK(TOP+1)
      IF (MSTK(TOP) .NE. MSTK(TOP+1)) CALL ML_ERROR(5)
      IF (ERR .GT. 0) RETURN
      NSTK(TOP) = NSTK(TOP) + NSTK(TOP+1)
      GOTO 22
   27 CONTINUE
      IF (SYM.EQ.SEMI .AND. CHRA.EQ.EOL) CALL ML_GETSYM
      CALL ML_STACK1(QUOTE)
      IF (ERR .GT. 0) RETURN
      TOP = TOP - 1
      IF (MSTK(TOP) .EQ. 0) MSTK(TOP) = MSTK(TOP+1)
      IF (MSTK(TOP).NE.MSTK(TOP+1).AND.MSTK(TOP+1).GT.0)CALL ML_ERROR(6)
      IF (ERR .GT. 0) RETURN
      NSTK(TOP) = NSTK(TOP) + NSTK(TOP+1)
      IF (SYM .EQ. EOL) CALL ML_GETLIN()
      IF (SYM .NE. GREAT) GOTO 21
      CALL ML_STACK1(QUOTE)
      IF (ERR .GT. 0) RETURN
      CALL ML_GETSYM
      GOTO 60
!
!     MACRO STRING
   30 CONTINUE
      CALL ML_GETSYM
      IF (SYM.EQ.LESS .AND. CHRA.EQ.EOL) CALL ML_ERROR(28)
      IF (ERR .GT. 0) RETURN
      PT = PT+1
      RSTK(PT) = 18
!     *CALL* EXPR
      RETURN
   32 CONTINUE
      PT = PT-1
      IF (SYM.NE.LESS .AND. SYM.NE.EOL) CALL ML_ERROR(37)
      IF (ERR .GT. 0) RETURN
      IF (SYM .EQ. LESS) CALL ML_GETSYM
      K = LPT(6)
      LIN(K+1) = LPT(1)
      LIN(K+2) = LPT(2)
      LIN(K+3) = LPT(6)
      LPT(1) = K + 4
!     TRANSFER STACK TO INPUT LINE
      K = LPT(1)
      L = LSTK(TOP)
      N = MSTK(TOP)*NSTK(TOP)
      DO 34 J = 1, N
        LS = L + J-1
        LIN(K) = IDINT(STKR(LS))
        IF (LIN(K).LT.0 .OR. LIN(K).GE.IALF) CALL ML_ERROR(37)
        IF (ERR .GT. 0) RETURN
        IF (K.LT.1024) K = K+1
        IF (K.EQ.1024)call journal('sc','INPUT BUFFER CHAR LIMIT EXCEEDED=',K)
   34 CONTINUE
      TOP = TOP-1
      LIN(K) = EOL
      LPT(6) = K
      LPT(4) = LPT(1)
      LPT(3) = 0
      LPT(2) = 0
      LCT(1) = 0
      CHRA = BLANK
      CALL ML_GETSYM
      PT = PT+1
      RSTK(PT) = 19
!     *CALL* EXPR
      RETURN
   37 CONTINUE
      PT = PT-1
      K = LPT(1) - 4
      LPT(1) = LIN(K+1)
      LPT(4) = LIN(K+2)
      LPT(6) = LIN(K+3)
      CHRA = BLANK
      CALL ML_GETSYM
      GOTO 60
!
!     FUNCTION OR MATRIX ELEMENT
   40 CONTINUE
      CALL ML_PUTID(ID,SYN)
      CALL ML_GETSYM
      IF (SYM .EQ. LPAREN) GOTO 42
      RHS = 0
      CALL ML_FUNS(ID)
      IF (FIN .NE. 0) CALL ML_ERROR(25)
      IF (ERR .GT. 0) RETURN
      CALL ML_STACKG(ID)
      IF (ERR .GT. 0) RETURN
      IF (FIN .EQ. 7) GOTO 50
      IF (FIN .EQ. 0) CALL ML_PUTID(IDS(1,PT+1),ID)
      IF (FIN .EQ. 0) CALL ML_ERROR(4)
      IF (ERR .GT. 0) RETURN
      GOTO 60
!
   42 CONTINUE
      CALL ML_GETSYM
      EXCNT = EXCNT+1
      PT = PT+1
      PSTK(PT) = EXCNT
      CALL ML_PUTID(IDS(1,PT),ID)
      RSTK(PT) = 11
!     *CALL* EXPR
      RETURN
!.......................................................................
   45 CONTINUE
      CALL ML_PUTID(ID,IDS(1,PT))
      EXCNT = PSTK(PT)
      PT = PT-1
      IF (SYM .EQ. COMMA) GOTO 42
      IF (SYM .NE. RPAREN) CALL ML_ERROR(3)
      IF (ERR .GT. 0) RETURN
      IF (SYM .EQ. RPAREN) CALL ML_GETSYM
      IF (ID(1) .EQ. BLANK) GOTO 60
      RHS = EXCNT
      CALL ML_STACKG(ID)
      IF (ERR .GT. 0) RETURN
      IF (FIN .EQ. 0) CALL ML_FUNS(ID)
      IF (FIN .EQ. 0) CALL ML_ERROR(4)
      IF (ERR .GT. 0) RETURN
!
!     EVALUATE MATRIX FUNCTION
   50 CONTINUE
      PT = PT+1
      RSTK(PT) = 16
!     *CALL* MATFN
      RETURN
   55 CONTINUE
      PT = PT-1
      GOTO 60
!
!     CHECK FOR QUOTE (TRANSPOSE) AND ** (POWER)
   60 CONTINUE
      IF (SYM .NE. QUOTE) GOTO 62
         I = LPT(3) - 2
         IF (LIN(I) .EQ. BLANK) GOTO 90
         CALL ML_STACK1(QUOTE)
         IF (ERR .GT. 0) RETURN
         CALL ML_GETSYM
   62 CONTINUE
      IF (SYM.NE.STAR .OR. CHRA.NE.STAR) GOTO 90
      CALL ML_GETSYM
      CALL ML_GETSYM
      PT = PT+1
      RSTK(PT) = 12
!     *CALL* FACTOR
      GOTO 01
   65 CONTINUE
      PT = PT-1
      CALL ML_STACK2(DSTAR)
      IF (ERR .GT. 0) RETURN
      IF (FUN .NE. 2) GOTO 90
!     MATRIX POWER, USE EIGENVECTORS
      PT = PT+1
      RSTK(PT) = 17
!     *CALL* MATFN
      RETURN
   75 CONTINUE
      PT = PT-1
   90 CONTINUE
      RETURN
   99 CONTINUE
      CALL ML_ERROR(22)
      IF (ERR .GT. 0) RETURN
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_FILES(LUNIT,INAME)
      use M_journal, only : journal
      INTEGER LUNIT
!     LUNIT = LOGICAL UNIT NUMBER
      ! if LUNIT is zero, return
      ! if LUNIT = standard input, return
      ! if LUNIT = standard output, return
      ! if LUNIT is 11, open the help file
      ! if LUNIT is -11 and HIO .ne. 0 , rewind the help file
      ! if LUNIT is positive, open the unit to file name INAME
      ! if LUNIT is negative, close the unit number
!     INAME = FILE NAME, 1 CHARACTER PER WORD
      ! how to know length of iname?
      INTEGER INAME(256)
      character*1024 NAME
      character*1024 temp1
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
!  Amiga dependent stuff squeezes the NAME from one CHAR per word to one per byte
!.......................................................................
      IF (DDT .EQ. 1) then
         call journal('sc','*MLFILES* LUNIT=', LUNIT)
         name(1:10)='*MLFILES* INAME='
         call buf2str(name(11:),iname,256)
         call journal(name)
      endif
!.......................................................................
      FE=0
      IF (LUNIT .EQ. 0) goto 999    ! error catcher
      if (LUNIT .eq. 5) goto 999  ! if unit is standard input return
      if (LUNIT .eq. 6) goto 999  ! if unit is standard output return
!.......................................................................
! HELP FILE
      if (LUNIT .eq. 11) then
         temp1=' '
         call get_environment_variable('ML88_HELP',temp1) ! get default name for helpfile
         if(temp1(:).eq.' ')temp1='matlab88_help.txt'
         itemp1=max(1,len_trim(temp1))
         OPEN(11,FILE=temp1(:itemp1),STATUS='OLD',ERR=14) ! open help file
         !call journal('HELP is available')
         goto 999
!        HELP FILE NOT FOUND
   14    CONTINUE
         call journal('HELP IS NOT AVAILABLE ON FILE ...')
         call journal(temp1(1:itemp1))
         HIO = 0
         goto 999
      elseif (LUNIT .eq. -11 .AND. HIO .NE. 0) then
         rewind (11,ERR=999)
         goto 999
      elseif (LUNIT .lt. 0) then     ! if LUNIT is negative, close the unit
         flush(unit=-LUNIT,iostat=ios)
         close(unit=-LUNIT,ERR=999)
         goto 999
      end if
!.......................................................................
!  ALL OTHER FILES
      call buf2str(name,iname,256)
      itemp=len_trim(name)
      !call journal('filename='//NAME(:itemp)
      OPEN(UNIT=LUNIT,FILE=NAME(:itemp),STATUS='UNKNOWN',ERR=98) ! open a file
      goto 999
!.......................................................................
! GENERAL FILE OPEN FAILURE
   98 CONTINUE
      call journal('*ml_files* OPEN FILE FAILED')
      call journal(NAME(1:len_trim(NAME)))
      FE=1
      ! SET THE I/O TO TERMINAL I/O
      RIO=RTE ! set current file to read input lines from to RTE
      goto 999
!.......................................................................
999   CONTINUE
      RETURN
!.......................................................................
      END
!.......................................................................
      SUBROUTINE ML_FUNS(ID)
       integer BIGMEM
       parameter(BIGMEM=200005)
!     SCAN FUNCTION LIST
!
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN
      INTEGER ID(4)
      LOGICAL ML_EQID
      INTEGER FUNL,FUNN(4,57),FUNP(57)
      save funl, funn, funp
      DATA FUNL/57/
!
!    1  ABS   ATAN  BASE  CHAR
!    2  CHOL  CHOP  COND  CONJ
!    3  COS   DET   DIAG  DIAR
!    4  DISP  EIG   EPS   EXEC
!    5  EXP   EYE   FLOP  HESS
!    6  HILB  IMAG  INV   KRON
!    7  LINE  LOAD  LOG   LU
!    8  MAGIC NORM  ONES  ORTH
!    9  PINV  PLOT  POLY  PRINT
!    $  PROD  QR    RAND  RANK
!    1  RAT   RCOND REAL  ROOT
!    2  ROUND RREF  SAVE  SCHUR
!    3  SIN   SIZE  SQRT  SUM
!    4  SVD   TRIL  TRIU  USER
!    5  DEBUG
!
      DATA FUNN/                                             &
     &  10,11,28,36, 10,29,10,23, 11,10,28,14, 12,17,10,27,  &
     &  12,17,24,21, 12,17,24,25, 12,24,23,13, 12,24,23,19,  &
     &  12,24,28,36, 13,14,29,36, 13,18,10,16, 13,18,10,27,  &
     &  13,18,28,25, 14,18,16,36, 14,25,28,36, 14,33,14,12,  &
     &  14,33,25,36, 14,34,14,36, 15,21,24,25, 17,14,28,28,  &
     &  17,18,21,11, 18,22,10,16, 18,23,31,36, 20,27,24,23,  &
     &  21,18,23,14, 21,24,10,13, 21,24,16,36, 21,30,36,36,  &
     &  22,10,16,18, 23,24,27,22, 24,23,14,28, 24,27,29,17,  &
     &  25,18,23,31, 25,21,24,29, 25,24,21,34, 25,27,18,23,  &
     &  25,27,24,13, 26,27,36,36, 27,10,23,13, 27,10,23,20,  &
     &  27,10,29,36, 27,12,24,23, 27,14,10,21, 27,24,24,29,  &
     &  27,24,30,23, 27,27,14,15, 28,10,31,14, 28,12,17,30,  &
     &  28,18,23,36, 28,18,35,14, 28,26,27,29, 28,30,22,36,  &
     &  28,31,13,36, 29,27,18,21, 29,27,18,30, 30,28,14,27,  &
     &  13,14,11,30/
!
      DATA FUNP/                                             &
     &  221,203,507,509, 106,609,303,225, 202,102,602,505,   &
     &  506,211,000,501, 204,606,000,213, 105,224,101,611,   &
     &  508,503,206,104, 601,304,608,402, 302,510,214,504,   &
     &  604,401,607,305, 511,103,223,215, 222,107,502,212,   &
     &  201,610,205,603, 301,614,615,605, 512/
!
      IF (ID(1).EQ.0) CALL ML_PRNTID(FUNN,FUNL-1)
      IF (ID(1).EQ.0) RETURN
!
      DO 10 K = 1, FUNL
         IF (ML_EQID(ID,FUNN(1,K))) GOTO 20
   10 CONTINUE
      FIN = 0
      RETURN
!
   20 FIN = MOD(FUNP(K),100)
      FUN = FUNP(K)/100
      IF (RHS.EQ.0 .AND. FUNP(K).EQ.606) FIN = 0
      IF (RHS.EQ.0 .AND. FUNP(K).EQ.607) FIN = 0
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_GETCH() ! get next character from input line
!     GET NEXT CHARACTER

      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER EOL
      PARAMETER(EOL=99)
      L = LPT(4)
      CHRA = LIN(L)
      IF (CHRA .NE. EOL) LPT(4) = L + 1
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_GETLIN() ! get a new input line
      use M_journal, only : journal
      character*1024 mline
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      PARAMETER (IALF=78)
      INTEGER ALFA(IALF),ALFB(IALF),ALFL,CASE
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE

      CHARACTER STRINGQ*1024
      COMMON /ZSTRINGC/STRINGQ
      COMMON /ZSTRINGI/ISTRINGQ,INITQ

      INTEGER LRECL,EOL,SLASH,BSLASH,DOT,BLANK,RETU(4)
      parameter(linelen=255)
      SAVE  EOL, DOT, BLANK, RETU, SLASH, BSLASH, LRECL
      DATA EOL/99/,DOT/47/,BLANK/36/,RETU/27,14,29,30/
      DATA SLASH/44/,BSLASH/45/,LRECL/LINELEN/
!.......................................................................
   10 CONTINUE
      L = LPT(1)
!.......................................................................
   11 CONTINUE
      DO 12 J = 1, LRECL            ! blank out buffer before reading it
         BUF(J) = ALFA(BLANK+1)
   12 CONTINUE
!.......................................................................
      N = LRECL+1
!     get line of input
      IF(ISTRINGQ.GT.0)THEN                  ! read from string instead of file
         call str2buf(stringq,buf,lrecl)     ! read input line from ML_88 call string
         call journal('c',stringq)           ! write as a comment
         IF(INITQ.EQ.2)THEN                  ! terminate after processing STRINGQ
            ISTRINGQ=6
            STRINGQ='return'
         ELSE                                ! go to normal matlab mode after processing STRINGQ
            ISTRINGQ=0
         ENDIF
      ELSE
         mline(:)=' '
         READ(RIO,'(A)',END=50,ERR=15) mline ! read input line from file
         if(RIO.eq.5)then
            call journal('t',mline)          ! reading from standard input, so copy to trail file
         else
            call journal('c',mline)          ! reading from an exec() command, so write as a comment
         endif
         if(mline(1:1).eq.'#')goto 11        ! ignore lines with a # in column 1
         call str2buf(mline,buf,lrecl)       ! read input line from file
      ENDIF
!.......................................................................
   15 CONTINUE
      N = N-1
      IF(N.LT.1)THEN
         N=1
      else IF (BUF(N) .EQ. ALFA(BLANK+1))then
         GOTO 15 ! trim off trailing spaces
      endif
!.......................................................................
      IF (MOD(LCT(4),2) .EQ. 1) then
              call buf2str(mline,buf,n) ! convert ADE buffer to character
              call journal('s',mline) ! just to standard output
      endif
!.......................................................................
      DO 40 J = 1, N
         DO 20 K = 1, ALFL  ! make sure this letter is in set of matlab characters and get it's matlab number
           IF (BUF(J).EQ.ALFA(K) .OR. BUF(J).EQ.ALFB(K)) GOTO 30
   20    CONTINUE
         call journal('sc','UNKNOWN CHARACTER AT COLUMN ',J) ! this is not a known character
         K = EOL+1
!         CALL ML_XCHAR(BUF(J),K)  ! handle special characters
         IF (K .GT. EOL) GOTO 10   ! UNKNOWN CHARACTER , K NOT CHANGED. get new line
         IF (K .EQ. EOL) GOTO 45
         IF (K .EQ. -1) L = L-1
         IF (K .LE. 0) GOTO 40
!
   30    CONTINUE
         K = K-1   ! K is index into ALF*, should be in range 0 to 51
         IF (K.EQ.SLASH .AND. BUF(J+1).EQ.BUF(J)) GOTO 45  ! if // rest is comment
         IF (K.EQ.DOT .AND. BUF(J+1).EQ.BUF(J)) GOTO 11    ! if .. line continuation
         IF (K.EQ.BSLASH .AND. N.EQ.1) GOTO 60             ! if \ in column 1
         LIN(L) = K
         IF (L.LT.1024) L = L+1
         IF (L.EQ.1024) call journal('sc','input buffer limit exceeded=',L)
   40 CONTINUE
!.......................................................................
   45 CONTINUE      ! line is ready, reset line pointers
      LIN(L) = EOL
      LPT(6) = L
      LPT(4) = LPT(1)
      LPT(3) = 0
      LPT(2) = 0
      LCT(1) = 0
      CALL ML_GETCH() ! load first character onto CHRA
      RETURN
!.......................................................................
   50 CONTINUE ! hit end of file
      CALL ML_PUTID(LIN(L),RETU) ! store RETU onto LIN(L) to simulate RETURN command
      L = L + 4
      GOTO 45
!.......................................................................
   60 CONTINUE
      N = LPT(6) - LPT(1)
      DO 61 I = 1, N
         J = L+I-1
         K = LIN(J)
         BUF(I) = ALFA(K+1)
         IF (CASE.EQ.1 .AND. K.LT.36) BUF(I) = ALFB(K+1)
   61 CONTINUE
      CALL ML_EDIT(BUF,N)
      N = N + 1
      GOTO 15
!.......................................................................
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_XCHAR(BUF,K)
      use M_journal, only : journal
      INTEGER BUF(*),K
      character*80 mline
!     SYSTEM DEPENDENT ROUTINE TO HANDLE SPECIAL CHARACTERS
      INTEGER BACK,MASK
      save back, mask
      DATA BACK/Z'20202008'/,MASK/Z'000000FF'/
!
      IF (BUF(1) .EQ. BACK) K = -1
      !L = BUF(1) .AND. MASK
      !IF(BUF(1).LT.30.OR.BUF(1).GT.70)
      L = IAND(BUF(1),MASK)
      IF (K .NE. -1) WRITE(MLINE,10) CHAR(BUF(1)),L
   10 FORMAT(1X,''',A1,'' = ',Z2,' hex is not a MATLAB character.')
      call journal(mline)
      RETURN
      END
!-----------------------------------------------------------------------
!      SUBROUTINE XCHAR(BUF,K)
!      INTEGER BUF(1),K
!C
!C     SYSTEM DEPENDENT ROUTINE TO HANDLE SPECIAL CHARACTERS: AMIGA
!C
!      INTEGER BACK,MASK
!      DATA BACK/Z'20202008'/,MASK/Z'000000FF'/
!C
!      IF (BUF(1) .EQ. BACK) K = -1
!      L = BUF(1) .AND. MASK
!      IF (K .NE. -1) WRITE(6,10) BUF(1),L
!   10 FORMAT(1X,1H',A1,4H' = ,Z2,' hex is not a MATLAB character.')
!      RETURN
!      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_GETSYM ! get a symbol
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!     GET A SYMBOL
      character mline*80
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN
      PARAMETER (IALF=78)
      INTEGER ALFA(IALF),ALFB(IALF),ALFL,CASE
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE

      INTEGER A2, Z2
      PARAMETER(A2=52,Z2=77)
      DOUBLE PRECISION SYV,S,ML_FLOP
      INTEGER BLANK,Z,DOT,D,E,PLUS,MINUS,NAME,NUM,SIGN,CHCNT,EOL
      INTEGER STAR,SLASH,BSLASH,SS
      SAVE BLANK,Z,DOT,D,E,EOL,PLUS,MINUS,NAME,NUM,STAR,SLASH,BSLASH
      DATA BLANK/36/,Z/35/,DOT/47/,D/13/,E/14/,EOL/99/,PLUS/41/
      DATA MINUS/42/,NAME/1/,NUM/0/,STAR/43/,SLASH/44/,BSLASH/45/
!.......................................................................
   10 CONTINUE
      IF (CHRA .NE. BLANK) GOTO 20
      CALL ML_GETCH() ! get next character
      GOTO 10
!.......................................................................
   20 CONTINUE
      LPT(2) = LPT(3)
      LPT(3) = LPT(4)
      IF (CHRA .LE. 9) GOTO 50  ! numeric character (0-9)
      IF (CHRA .LE. Z.OR.(CHRA.GE.A2.AND.CHRA.LE.Z2)) GOTO 30
      ! ALPHAMERIC (A-Z OR A-Z)
!.......................................................................
!     SPECIAL CHARACTER
      SS = SYM
      SYM = CHRA
      CALL ML_GETCH() ! get next character
      IF (SYM .NE. DOT) GOTO 90
!
!     IS DOT PART OF NUMBER OR OPERATOR
      SYV = 0.0D0
      IF (CHRA .LE. 9) GOTO 55  ! a number character
      IF (CHRA.EQ.STAR.OR.CHRA.EQ.SLASH.OR.CHRA.EQ.BSLASH) GOTO 90
      IF (SS.EQ.STAR .OR. SS.EQ.SLASH .OR. SS.EQ.BSLASH) GOTO 90
      GOTO 55
!.......................................................................
!     NAME
   30 CONTINUE
      SYM = NAME
      SYN(1) = CHRA
      CHCNT = 1
   40 CONTINUE
      CALL ML_GETCH() ! get next character
      CHCNT = CHCNT+1
      IF (CHRA .GE. A2.and.CHRA.lE.Z2) GOTO  44! alternate case letter
      IF (CHRA .GT. Z) GOTO 45  ! a control character not alphanumeric and not special like EOL
44    CONTINUE
      IF (CHCNT .LE. 4) SYN(CHCNT) = CHRA
      GOTO 40
   45 CONTINUE
      IF (CHCNT .GT. 4) GOTO 47
      DO 46 I = CHCNT, 4
         SYN(I) = BLANK
   46 CONTINUE
   47 CONTINUE
      GOTO 90
!.......................................................................
!     NUMBER
   50 CONTINUE
      CALL ML_GETVAL(SYV)
      IF (CHRA .NE. DOT) GOTO 60
      CALL ML_GETCH() ! get next character
   55 CONTINUE
      CHCNT = LPT(4)
      CALL ML_GETVAL(S)
      CHCNT = LPT(4) - CHCNT
      IF (CHRA .EQ. EOL) CHCNT = CHCNT+1
      SYV = SYV + S/10.0D0**CHCNT
   60 CONTINUE
      IF (CHRA.NE.D .AND. CHRA.NE.E) GOTO 70
      CALL ML_GETCH() ! get next character
      SIGN = CHRA
      IF (SIGN.EQ.MINUS .OR. SIGN.EQ.PLUS) CALL ML_GETCH() ! get next character
      CALL ML_GETVAL(S)
      IF (SIGN .NE. MINUS) SYV = SYV*10.0D0**S
      IF (SIGN .EQ. MINUS) SYV = SYV/10.0D0**S
   70 CONTINUE
      STKI(VSIZE) = ML_FLOP(SYV)
      SYM = NUM
!
   90 CONTINUE
      IF (CHRA .NE. BLANK) GOTO 99
      CALL ML_GETCH() ! get next character
      GOTO 90
   99 CONTINUE
      IF (DDT .NE. 1) RETURN
      IF (SYM.GT.NAME .AND. SYM.LT.ALFL) THEN
              call journal(CHAR(ALFA(SYM+1)))
      endif
      IF (SYM .GE. ALFL) call journal('EOL')
      IF (SYM .EQ. NAME) CALL ML_PRNTID(SYN,1)
      IF (SYM .EQ. NUM) THEN
         WRITE(MLINE,'(1X,G8.2)') SYV
         CALL journal(MLINE)
      ENDIF
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_GETVAL(S)
      DOUBLE PRECISION S
!     FORM NUMERICAL VALUE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN
      S = 0.0D0
   10 CONTINUE
         IF (CHRA .GT. 9) RETURN
         S = 10.0D0*S + CHRA
         CALL ML_GETCH() ! get next character
      GOTO 10
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_PUTID(X,Y)
!     STORE A NAME
      INTEGER X(4),Y(4)
      DO 10 I = 1, 4
   10 X(I) = Y(I)
      RETURN
      END
!-----------------------------------------------------------------------
      LOGICAL FUNCTION ML_EQID(X,Y)
!     CHECK FOR EQUALITY OF TWO NAMES
      INTEGER X(4),Y(4)
      ML_EQID = .TRUE.
      DO 10 I = 1, 4
         ML_EQID = ML_EQID .AND. (X(I).EQ.Y(I))
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
      SUBROUTINE ML_PLOT(LPLOT,X,Y,N,P,K)
      DOUBLE PRECISION X(N),Y(N),P(*)
      CHARACTER BUF*79
!
!     PLOT X VS. Y ON LPLOT
!     IF K IS NONZERO, THEN P(1),...,P(K) ARE EXTRA PARAMETERS
!     BUF IS WORK SPACE
!
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX,DY,DX,Y1,Y0
!
      INTEGER H,W
      parameter(H=20,W=79)
!     H = HEIGHT, W = WIDTH
!
      IF (K .GT. 0) WRITE(LPLOT,01) (P(I), I=1,K)
   01 FORMAT('Extra parameters',1000(f5.1,/))
      XMIN = X(1)
      XMAX = X(1)
      YMIN = Y(1)
      YMAX = Y(1)
      DO 10 I = 1, N
         XMIN = DMIN1(XMIN,X(I))
         XMAX = DMAX1(XMAX,X(I))
         YMIN = DMIN1(YMIN,Y(I))
         YMAX = DMAX1(YMAX,Y(I))
   10 CONTINUE
      DX = XMAX - XMIN
      IF (DX .EQ. 0.0D0) DX = 1.0D0
      DY = YMAX - YMIN
      WRITE(LPLOT,'(80X)')
      DO 40 L = 1, H
         BUF(:)=' '  ! blank out the line
         Y1 = YMIN + (H-L+1)*DY/H
         Y0 = YMIN + (H-L)*DY/H
         JMAX = 1
         DO 30 I = 1, N
            IF (Y(I) .GT. Y1) GOTO 30
            IF (L.NE.H .AND. Y(I).LE.Y0) GOTO 30
            J = 1 + (W-1)*(X(I) - XMIN)/DX
            BUF(J:J) = '*'
            JMAX = MAX0(JMAX,J)
   30    CONTINUE
         WRITE(LPLOT,'(1X,A)') BUF(1:JMAX)
   40 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_PRINT(ID,K)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!     PRIMARY OUTPUT ROUTINE

      character*81 mline
      character*80 form
      character ls_char*1
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      PARAMETER (IALF=78)
      INTEGER ALFA(IALF),ALFB(IALF)
      INTEGER ALFL,CASE
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER ID(4),K
      DOUBLE PRECISION S,TR,TI,PR(12),PI(12),ML_ROUND
      INTEGER FNO(11),FNL(11),SIG(12),PLUS,MINUS,TYP,F
      save fno, fnl
      DATA PLUS/41/,MINUS/42/
!     FORMAT NUMBERS AND LENGTHS
      DATA FNO /11,12,21,22,23,24,31,32,33,34,-1/
      DATA FNL /12, 6, 8, 4, 6, 3, 4, 2, 3, 1, 1/
!     FMT   1       2       3       4       5
!         SHORT   LONG   SHORT E  LONG E    Z
!     TYP   1       2       3
!         INTEGER  REAL   COMPLEX
!.......................................................................
      IF (LCT(1) .LT. 0) GOTO 99
!.......................................................................
      L = LSTK(K)
      M = MSTK(K)
      N = NSTK(K)
      MN = M*N
      TYP = 1
      S = 0.0D0
      DO 10 I = 1, MN
        LS = L+I-1
        TR = STKR(LS)
        TI = STKI(LS)
        S = DMAX1(S,DABS(TR),DABS(TI))
        IF (ML_ROUND(TR) .NE. TR) TYP = MAX0(2,TYP)
        IF (TI .NE. 0.0D0) TYP = 3
   10 CONTINUE
      IF (S .NE. 0.0D0) S = DLOG10(S)
      KS = IDINT(S)
      IF (-2 .LE. KS .AND. KS .LE. 1) KS = 0
      IF (KS .EQ. 2 .AND. FMT .EQ. 1 .AND. TYP .EQ. 2) KS = 0

      F=0                          ! initialize to bad value
      IF (TYP .EQ. 1 )THEN         ! if output type is integer
         IF( KS .LE. 2 )THEN
            F = 1
         ELSE
            F = 2
         ENDIF
      ENDIF
      IF (TYP .EQ. 1 .AND. KS .GT. 9) TYP = 2  !change type from integer to real

      IF (TYP .EQ. 2) F = FMT + 2   ! if type is real
      IF (TYP .EQ. 3) F = FMT + 6   ! if type is complex
      if(f.eq.0)then
         call journal('*ml_print* internal error - bad type')
         goto 99
      endif

      IF (MN.EQ.1 .AND. KS.NE.0 .AND. FMT.LT.3 .AND. TYP.NE.1) F = F+2

      IF (FMT .EQ. 5) F = 11

      JINC = FNL(F)
      F = FNO(F)

      S = 1.0D0
      IF (F.EQ.21 .OR. F.EQ.22 .OR. F.EQ.31 .OR. F.EQ.32) S = 10.0D0**KS
      LS = ((N-1)/JINC+1)*M + 2
!.......................................................................
      IF (LCT(1) + LS .LE. LCT(2)) GOTO 20
         LCT(1) = 0

         WRITE(mline,43) LS
   43    FORMAT(' AT LEAST ',I5,' MORE LINES.','  ENTER BLANK LINE TO CONTINUE OUTPUT.')
         call journal(mline)

         READ(RTE,'(a1)',END=19) LS_CHAR  ! read response to pause from standard input
         IF (LS_CHAR .EQ. ' ') GOTO 20         ! if blank or a return display the values
         LCT(1) = -1
         GOTO 99
   19    CONTINUE
         CALL ML_FILES(-RTE,BUF)
   20 CONTINUE
!.......................................................................
      call journal(' ')
      CALL ML_PRNTID(ID,-1)
      LCT(1) = LCT(1)+2
      IF (S .NE. 1.0D0)then
         WRITE(mline,'(''  '',1PD9.1,2H *)') S
         if(wte.eq.6)then
            call journal(mline)
         else
            write(wte,'(a)')mline(1:80)
         endif
      endif
      DO 80 J1 = 1, N, JINC
        J2 = MIN0(N, J1+JINC-1)
        IF (N .GT. JINC)then
           WRITE(mline,'(''     COLUMNS'',I6,'' THRU'',I6)') J1,J2
           if(wte.eq.6)then
              call journal(mline)
           else
              write(wte,'(a)')mline(1:80)
           endif
        endif
        DO 70 I = 1, M
          JM = J2-J1+1
          DO 60 J = 1, JM
             LS = L+I-1+(J+J1-2)*M
             PR(J) = STKR(LS)/S
             PI(J) = DABS(STKI(LS)/S)
             SIG(J) = ALFA(PLUS+1)
             IF (STKI(LS) .LT. 0.0D0) SIG(J) = ALFA(MINUS+1)
   60     CONTINUE
          goto(11,12)F-10
          goto(21,22,23,24)F-20
          goto(31,32,33,34)F-30
          IF (F .EQ. -1) THEN
             CALL ML_FORMZ(WTE,STKR(LS),STKI(LS))
             goto 71
          endif
          call journal('*internal error*')
          goto 99
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11        CONTINUE
          FORM='(1X,12F6.0)'  ! integer
          ISTEP=12
          goto 716
12       CONTINUE
          FORM='(1X,6F12.0)'  ! integer
          ISTEP=6
          goto 716

716       CONTINUE
          J3=1
7161      CONTINUE
          WRITE(mline,FORM)(PR(J),J=J3,MIN(J3+ISTEP-1,JM))
          if(wte.eq.6)then
             call journal(mline)
          else
             write(wte,'(a)')mline(1:80)
          endif
          J3=J3+ISTEP
          if(J3.le.JM)goto 7161
          GOTO 71
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
21        CONTINUE
          FORM='(1X,F9.4,7F10.4)'  ! 8 numbers
          ISTEP=8
          goto 714
22        CONTINUE
          FORM='(1X,F19.15,3F20.15)'  ! 4 numbers
          ISTEP=4
          goto 714
23        CONTINUE
          FORM='(1X,1P6D13.4)'   ! 6 numbers
          ISTEP=6
          goto 714
24        CONTINUE
          FORM='(1X,1P3D24.15)'  ! 3 numbers
          ISTEP=3
          GOTO 714

714       CONTINUE
          J3=1
7141      CONTINUE
          WRITE(mline,FORM)(PR(J),J=J3,MIN(J3+ISTEP,JM))
          if(wte.eq.6)then
             call journal(mline)
          else
             write(wte,'(a)')mline(1:80)
          endif
          J3=J3+ISTEP
          if(J3.le.JM)goto 7141
          goto 71
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
31        CONTINUE
          FORM='(1X,4(F9.4,1X,A1,F7.4,''i''))'  ! 4x3
          ISTEP=12
          goto 718
32        CONTINUE
          FORM='(1X,F19.15,A1,F18.15,''i'',F20.15,A1,F18.15,''i'')'  ! 6
          ISTEP=6
          goto 718
33        CONTINUE
          FORM='(1X,3(1PD13.4,1X,A1,1PD10.4,''i''))'  ! 9
          ISTEP=9
          goto 718
34        CONTINUE
          FORM='(1X,1PD24.15,1X,A1,1PD21.15,''i'')'  ! 3
          ISTEP=3

718       CONTINUE
          J3=1
7181      CONTINUE
          WRITE(mline,form)(PR(J),SIG(J),PI(J),J=J3,MIN(J3+ISTEP-1,JM))
          if(wte.eq.6)then
             call journal(mline)
          else
             write(wte,'(a)')mline(1:80)
          endif
          J3=J3+ISTEP
          if(J3.le.JM)goto 7181
          goto 71
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
71        CONTINUE
          LCT(1) = LCT(1)+1
   70   CONTINUE
   80 CONTINUE
      GOTO 99
!.......................................................................
   99 CONTINUE
      if(wte.ne.6)flush(unit=wte,iostat=ios)
!
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_FORMZ(LUNIT,X,Y)
      use M_journal, only : journal
      DOUBLE PRECISION X,Y
!     SYSTEM DEPENDENT ROUTINE TO PRINT WITH Z FORMAT
      character*36 mline
      IF (Y .NE. 0.0D0) then
         WRITE(mline,10) X,Y
      else
         WRITE(mline,10) X
      endif
      call journal(mline)
   10 FORMAT(2Z18)
      END SUBROUTINE ML_FORMZ
!-----------------------------------------------------------------------
      SUBROUTINE ML_PRNTID(ID,ARGCNT)   ! print variable names
      use M_journal, only : journal
!
!@(#) PRINT TABLE OF VARIABLE ID NAMES (UP TO) EIGHT PER LINE
!     ID     IS ARRAY OF 4-CHARACTER IDS TO PRINT
!     ARGCNT IS NUMBER OF IDS TO PRINT
!            IF = -1, PRINT ONE ID WITH AN "  =" SUFFIX
!
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/

      INTEGER ID(4,*),ARGCNT

      CHARACTER*80 MLINE  ! scratch space for building line to print
      INTEGER BUF(256)    ! scratch buffer for building up line

      PARAMETER (IALF=78)
      INTEGER ALFA_Q(IALF),alfb(ialf),alfl,case
      COMMON /ALFS/ ALFA_Q,alfb,alfl,case

      INTEGER ADE_BLANK
      INTEGER ADE_EQUAL
      PARAMETER (ADE_BLANK=32)
      PARAMETER (ADE_EQUAL=61)

      J1 = 1                              ! which ID to start the line with
   10 CONTINUE                            ! start a line
      BUF(1)=ADE_BLANK                    ! put a space at beginning of line
      L = 2                               ! pointer into output line being built
      DO 16 J = J1,MIN0(J1+7,IABS(ARGCNT))! copy up to eight names into buffer
         DO 15 I = 1, 4                   !    copy one name into buffer
            K = ID(I,J)+1                 ! this is the kth letter of the set
            BUF(L) = ALFA_Q(K)
            L = L+1                       ! increment pointer into output
   15    CONTINUE
         BUF(L+0)=ADE_BLANK               ! put two space between names
         BUF(L+1)=ADE_BLANK
         L=L+2
   16 CONTINUE
      IF (ARGCNT .EQ. -1) then     ! special flag to print one word and  =
         BUF(L) = ADE_EQUAL        ! put value for equal sign into buffer
      ELSE
         L=L-3                     ! was prepared for another ID with two blanks
      ENDIF
      !-----------------------------------------------
      CALL BUF2STR(MLINE,BUF,L) ! write BUF(1:L) line to a character variable
      if(wte.eq.6)then
         CALL journal(MLINE)               ! print the line
      else
         write(wte,'(a)')mline(1:80)     ! print the line
      endif
      !-----------------------------------------------
      J1 = J1+8                          ! prepare to get up to eight more IDs
      IF (J1 .LE. IABS(ARGCNT)) GOTO 10  ! if not done do another line
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_PROMPT(PAUSE) ! issue interactive prompt with optional pause
      use M_journal, only : journal
      INTEGER PAUSE
!
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      CHARACTER*1 DUMMY

      ! paranoid checks
      if(wte.le.0)then
              call journal('*ml_prompt* internal error: wte <= 0')
              goto 999
      elseif(rte.lt.0)then
              call journal('*ml_prompt* internal error: rte <= 0')
              goto 999
      endif

      ! write prompt using non-ANSI format that stays on current line
      if(wte.eq.6)then
          WRITE(WTE,'(''<>'')',advance='no')   ! write prompt to interactive input
      endif
      IF (PAUSE .EQ. 1) READ(RTE,'(A1)') DUMMY
999   CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_EDIT(BUF,N)
      use M_journal, only : journal
      use M_strings, only : change, modif
      INTEGER BUF(N)
!.......................................................................
!     CALLED AFTER INPUT OF A SINGLE BACKSLASH
!     BUF CONTAINS PREVIOUS INPUT LINE, ONE CHAR PER WORD
!     ENTER LOCAL EDITOR IF AVAILABLE
!     OTHERWISE CREATE MINIMAL EDITOR WITH MODIF() AND CHANGE()
!     BUF IS THE INPUT STRING AS HOLLERITH
!     N IS NUMBER OF CHARACTERS IN BUF ON INPUT AND OUTPUT
!.......................................................................
      PARAMETER(ICH=256)
      CHARACTER*(ICH) LINE
      CHARACTER*(ICH) COMMAND
      IN=MAX(0,MIN(ICH,N))
      IF(IN.EQ.0)RETURN
!.......................................................................
!     CONVERT HOLLERITH TO CHARACTER
      LINE(:)=' '
      call buf2str(line,buf,in)
!.......................................................................
      INFINITE: do
         WRITE(*,'(A1,A)')'>',LINE(1:IN)
         READ(*,'(a)')COMMAND  ! read editing directive
         ILAST=len_trim(COMMAND)
         IF(ILAST.EQ.0)THEN                         ! blank command line; return and execute
            EXIT INFINITE
         ELSEIF(ILAST.EQ.1.AND.COMMAND(1:1).EQ.'.')THEN
            LINE=' '
            N=0
            EXIT INFINITE
         ELSEIF(COMMAND(1:1).EQ.' '.OR.COMMAND(1:1).EQ.'m')THEN                 ! modify the string
            CALL MODIF(LINE,COMMAND(2:))
         ELSEIF(COMMAND(1:1).EQ.'c'.OR.COMMAND(1:1).EQ.'s')THEN                 ! change old string to new
            CALL CHANGE(LINE,COMMAND(1:),IER)                                   ! xedit-like change command
         ELSEIF(COMMAND(1:1).EQ.'?'.OR.COMMAND(1:1).EQ.'h')THEN                 ! display help
          CALL JOURNAL('#-------------------------------------------------#')
          CALL JOURNAL('|EDIT BUFFER:                                     |')
          CALL JOURNAL('|c/oldstring/newstring/  # change/substitute      |')
          CALL JOURNAL('| mod_string # Modify (replace, delete, insert)   |')
          CALL JOURNAL('|              #         deletes                  |')
          CALL JOURNAL('|                        blank leaves as-is       |')
          CALL JOURNAL('|              &         replaces with a blank    |')
          CALL JOURNAL('|              ^STRING#  inserts a string         |')
          CALL JOURNAL('|              Any other replaces character       |')
          CALL JOURNAL('|-------------------------------------------------|')
          CALL JOURNAL('|RETURN TO NORMAL COMMAND MODE:                   |')
          CALL JOURNAL('|         # return and execute command in buffer  |')
          CALL JOURNAL('|.        # return a blank line                   |')
          CALL JOURNAL('|-------------------------------------------------|')
          CALL JOURNAL('|HELP:                                            |')
          CALL JOURNAL('|h|?      # display this help text                |')
          CALL JOURNAL('#-------------------------------------------------#')
         ENDIF
         IN=len_trim(LINE)
      enddo INFINITE
!.......................................................................
!     CONVERT CHARACTER BACK TO HOLLERITH
      N=len_trim(LINE)
      call str2buf(line,buf,N)     ! char to hollerith
      if(N.lt.0)then
          call journal('*ML_EDIT* CONVERT ERROR')
      endif
      END SUBROUTINE ML_EDIT
!-----------------------------------------------------------------------
      SUBROUTINE ML_ERROR(N)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      INTEGER N

      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ

      PARAMETER (IALF=78)
      INTEGER ALFA(IALF),ALFB(IALF),ALFL,CASE
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE

      parameter(linelen=255)
      CHARACTER*(LINELEN) BLH
      CHARACTER*(LINELEN) MSG
      character*(LINELEN+20) mline
      BLH(1:LINELEN)='        '
!
      K = LPT(2) - LPT(1)
      IF (K .LT. 1) K = 1
   98 CONTINUE

      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40),N
!
      call journal('sc','*ml_error* unknown error code =',n)
      GOTO 888
    1 MSG='IMPROPER MULTIPLE ASSIGNMENT'
      GOTO 888
    2 MSG='IMPROPER FACTOR'
      GOTO 888
    3 MSG='EXPECT RIGHT PARENTHESIS'
      GOTO 888
!.......................................................................
    4 DO 9400 I9400 = 1, 4
         KK = IDS(I9400,PT+1)
         BUF(I9400) = ALFA(KK+1)
9400  CONTINUE
      call buf2str(msg,buf,4)
      msg='UNDEFINED VARIABLE: '//msg(1:4)
      GOTO 888
!.......................................................................
    5 MSG='COLUMN LENGTHS DO NOT MATCH'
      GOTO 888
    6 MSG='ROW LENGTHS DO NOT MATCH'
      GOTO 888
    7 MSG='TEXT TOO LONG'
      GOTO 888
    8 MSG='Incompatible for ADDITION'
      GOTO 888
    9 MSG='Incompatible for SUBTRACTION'
      GOTO 888
   10 MSG='Incompatible for MULTIPLICATION'
       GOTO 888
   11 MSG='Incompatible for RIGHT DIVISION'
      GOTO 888
   12 MSG='Incompatible for LEFT DIVISION'
      GOTO 888
   13 MSG='Improper assignment to PERMANENT VARIABLE'
      GOTO 888
   14 MSG='EYE-dentity undefined by CONTEXT'
      GOTO 888
   15 MSG='IMPROPER ASSIGNMENT TO SUBMATRIX'
      GOTO 888
   16 MSG='IMPROPER COMMAND'
      GOTO 888
!.......................................................................
   17 LB = VSIZE - LSTK(BOT) + 1
      LT = ERR + LSTK(BOT)
      call journal(' TOO MUCH MEMORY REQUIRED')
      WRITE(MLINE,'(1X,I7,'' VARIABLES,'',I7,'' TEMPORARIES,'',I7,'' AVAILABLE.'')') LB,LT,VSIZE
      call journal(MLINE)
      GOTO 99
!.......................................................................
   18 MSG='TOO MANY NAMES'
      GOTO 888
   19 MSG='MATRIX IS SINGULAR TO WORKING PRECISION'
      GOTO 888
   20 MSG='MATRIX MUST BE SQUARE'
      GOTO 888
   21 MSG='SUBSCRIPT OUT OF RANGE'
      GOTO 888
!.......................................................................
   22 WRITE(MLINE,122) (RSTK(I),I=1,PT)
  122 FORMAT(1X,'RECURSION DIFFICULTIES',10I4)
      call journal(mline)
      GOTO 99
!.......................................................................
   23 MSG='ONLY 1, 2 OR INF NORM OF MATRIX'
      GOTO 888
   24 MSG='NO CONVERGENCE'
      GOTO 888
   25 MSG='CAN NOT USE FUNCTION NAME AS VARIABLE'
      GOTO 888
   26 MSG='TOO COMPLICATED (STACK OVERFLOW)'
      GOTO 888
   27 MSG='DIVISION BY ZERO IS A NO-NO'
      GOTO 888
   28 MSG='EMPTY MACRO'
      GOTO 888
   29 MSG='NOT POSITIVE DEFINITE'
      GOTO 888
   30 MSG='IMPROPER EXPONENT'
      GOTO 888
   31 MSG='IMPROPER STRING'
      GOTO 888
   32 MSG='SINGULARITY OF LOG OR ATAN'
      GOTO 888
   33 MSG='TOO MANY COLONS'
      GOTO 888
   34 MSG='IMPROPER FOR CLAUSE'
      GOTO 888
   35 MSG='IMPROPER WHILE OR IF CLAUSE'
      GOTO 888
   36 MSG='ARGUMENT OUT OF RANGE'
      GOTO 888
   37 MSG='IMPROPER MACRO'
      GOTO 888
   38 MSG='IMPROPER FILE NAME'
      GOTO 888
   39 MSG='INCORRECT NUMBER OF ARGUMENTS'
      GOTO 888
   40 MSG='EXPECT STATEMENT TERMINATOR'
      GOTO 888
!.......................................................................
888   CONTINUE
      IEND=MAX(1,len_trim(MSG))

      IF(K+IEND.lt.LEN(MLINE))then
         WRITE(MLINE,'(1X,A,''/^--ERROR:'',A)') BLH(1:K),MSG(1:IEND)
         call journal(MLINE)
      else
         WRITE(MLINE,'(1X,A,''/^--ERROR:'')') BLH(1:K)
         call journal(MLINE)
         call journal(MSG)
      endif
      GOTO 99
!.......................................................................
   99 CONTINUE
      ERR = N
      END
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
      SUBROUTINE ML_MATFN1
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!
!     EVALUATE FUNCTIONS INVOLVING GAUSSIAN ELIMINATION
!
      character mline*80
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      DOUBLE PRECISION DTR(2),DTI(2),SR,SI,RCOND,T,T0,T1
      DOUBLE PRECISION ML_FLOP,EPS,ML_WASUM
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN1* ', FIN)
!
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (FIN .EQ. -1) GOTO 10
      IF (FIN .EQ. -2) GOTO 20
      GOTO (30,40,50,60,70,80,85),FIN
!.......................................................................
!
!     MATRIX RIGHT DIVISION, A/A2
   10 L2 = LSTK(TOP+1)
      M2 = MSTK(TOP+1)
      N2 = NSTK(TOP+1)
      IF (M2 .NE. N2) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      IF (M*N .EQ. 1) GOTO 16
      IF (N .NE. N2) CALL ML_ERROR(11)
      IF (ERR .GT. 0) RETURN
      L3 = L2 + M2*N2
      ERR = L3+N2 - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WGECO(STKR(L2),STKI(L2),M2,N2,BUF,RCOND,STKR(L3),STKI(L3))
      IF (RCOND .EQ. 0.0D0) CALL ML_ERROR(19)
      IF (ERR .GT. 0) RETURN
      T = ML_FLOP(1.0D0 + RCOND)
      IF (T.EQ.1.0D0 .AND. FUN.NE.21)then
         call journal('WARNING:')
         call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      IF (T.EQ.1.0D0 .AND. FUN.EQ.21)then
         call journal('WARNING')
         call journal('EIGENVECTORS ARE BADLY CONDITIONED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      DO 15 I = 1, M
         DO 13 J = 1, N
            LS = L+I-1+(J-1)*M
            LL = L3+J-1
            STKR(LL) = STKR(LS)
            STKI(LL) = -STKI(LS)
   13    CONTINUE
         CALL ML_WGESL(STKR(L2),STKI(L2),M2,N2,BUF,STKR(L3),STKI(L3),1)
         DO 14 J = 1, N
            LL = L+I-1+(J-1)*M
            LS = L3+J-1
            STKR(LL) = STKR(LS)
            STKI(LL) = -STKI(LS)
   14    CONTINUE
   15 CONTINUE
      IF (FUN .NE. 21) GOTO 99
!
!     CHECK FOR IMAGINARY ROUNDOFF IN MATRIX FUNCTIONS
      SR = ML_WASUM(N*N,STKR(L),STKR(L),1)
      SI = ML_WASUM(N*N,STKI(L),STKI(L),1)
      EPS = STKR(VSIZE-4)
      T = EPS*SR
      IF (DDT .EQ. 18)then
         WRITE(WTE,'('' SR,SI,EPS,T'',1P4D13.4)') SR,SI,EPS,T ! debug 18
      endif
      IF (SI .LE. EPS*SR) CALL ML_RSET(N*N,0.0D0,STKI(L),1)
      GOTO 99
!
   16 SR = STKR(L)
      SI = STKI(L)
      N = N2
      M = N
      MSTK(TOP) = N
      NSTK(TOP) = N
      CALL ML_WCOPY(N*N,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      GOTO 30
!.......................................................................
!
!     MATRIX LEFT DIVISION A BACKSLASH A2
   20 L2 = LSTK(TOP+1)
      M2 = MSTK(TOP+1)
      N2 = NSTK(TOP+1)
      IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      IF (M2*N2 .EQ. 1) GOTO 26
      L3 = L2 + M2*N2
      ERR = L3+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WGECO(STKR(L),STKI(L),M,N,BUF,RCOND,STKR(L3),STKI(L3))
      IF (RCOND .EQ. 0.0D0) CALL ML_ERROR(19)
      IF (ERR .GT. 0) RETURN
      T = ML_FLOP(1.0D0 + RCOND)
      IF (T .EQ. 1.0D0) then
         call journal('WARNING:')
         call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      IF (M2 .NE. N) CALL ML_ERROR(12)
      IF (ERR .GT. 0) RETURN
      DO 23 J = 1, N2
         LJ = L2+(J-1)*M2
         CALL ML_WGESL(STKR(L),STKI(L),M,N,BUF,STKR(LJ),STKI(LJ),0)
   23 CONTINUE
      NSTK(TOP) = N2
      CALL ML_WCOPY(M2*N2,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      GOTO 99
   26 SR = STKR(L2)
      SI = STKI(L2)
      GOTO 30
!.......................................................................
!     INV
!
   30 IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      IF (DDT .EQ. 17) GOTO 32
      DO 31 J = 1, N
      DO 31 I = 1, N
        LS = L+I-1+(J-1)*N
        T0 = STKR(LS)
        T1 = ML_FLOP(1.0D0/(DFLOAT(I+J-1)))
        IF (T0 .NE. T1) GOTO 32
   31 CONTINUE
      GOTO 72
   32 L3 = L + N*N
      ERR = L3+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WGECO(STKR(L),STKI(L),M,N,BUF,RCOND,STKR(L3),STKI(L3))
      IF (RCOND .EQ. 0.0D0) CALL ML_ERROR(19)
      IF (ERR .GT. 0) RETURN
      T = ML_FLOP(1.0D0 + RCOND)
      IF (T .EQ. 1.0D0) then
         call journal('WARNING:')
         call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      CALL ML_WGEDI(STKR(L),STKI(L),M,N,BUF,DTR,DTI,STKR(L3),STKI(L3),1)
      IF (FIN .LT. 0) CALL ML_WSCAL(N*N,SR,SI,STKR(L),STKI(L),1)
      GOTO 99
!.......................................................................
!     DET
!
   40 IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      CALL ML_WGEFA(STKR(L),STKI(L),M,N,BUF,INFO)
      CALL ML_WGEDI(STKR(L),STKI(L),M,N,BUF,DTR,DTI,SR,SI,10)
      K = IDINT(DTR(2))
      KA = IABS(K)+2
      T = 1.0D0
      DO 41 I = 1, KA
         T = T/10.0D0
         IF (T .EQ. 0.0D0) GOTO 42
   41 CONTINUE
      STKR(L) = DTR(1)*10.D0**K
      STKI(L) = DTI(1)*10.D0**K
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      GOTO 99
   42 CONTINUE
      IF (DTI(1) .EQ. 0.0D0)then
              WRITE(mline,43) DTR(1),K
   43 FORMAT(' DET =  ',F7.4,7H * 10**,I4)
              call journal(mline)
      else
              WRITE(mline,44) DTR(1),DTI(1),K
   44 FORMAT(' DET =  ',F7.4,' + ',F7.4,' i ',7H * 10**,I4)
              call journal(mline)
      endif
      STKR(L) = DTR(1)
      STKI(L) = DTI(1)
      STKR(L+1) = DTR(2)
      STKI(L+1) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 2
      GOTO 99
!.......................................................................
!     RCOND
!
   50 IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      L3 = L + N*N
      ERR = L3+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WGECO(STKR(L),STKI(L),M,N,BUF,RCOND,STKR(L3),STKI(L3))
      STKR(L) = RCOND
      STKI(L) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      IF (LHS .EQ. 1) GOTO 99
      L = L + 1
      CALL ML_WCOPY(N,STKR(L3),STKI(L3),1,STKR(L),STKI(L),1)
      TOP = TOP + 1
      LSTK(TOP) = L
      MSTK(TOP) = N
      NSTK(TOP) = 1
      GOTO 99
!.......................................................................
!     LU
!
   60 IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      CALL ML_WGEFA(STKR(L),STKI(L),M,N,BUF,INFO)
      IF (LHS .NE. 2) GOTO 99
      NN = N*N
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L + NN
      MSTK(TOP) = N
      NSTK(TOP) = N
      ERR = L+NN+NN - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      DO 64 KB = 1, N
        K = N+1-KB
        DO 61 I = 1, N
          LL = L+I-1+(K-1)*N
          LU = LL + NN
          IF (I .LE. K) STKR(LU) = STKR(LL)
          IF (I .LE. K) STKI(LU) = STKI(LL)
          IF (I .GT. K) STKR(LU) = 0.0D0
          IF (I .GT. K) STKI(LU) = 0.0D0
          IF (I .LT. K) STKR(LL) = 0.0D0
          IF (I .LT. K) STKI(LL) = 0.0D0
          IF (I .EQ. K) STKR(LL) = 1.0D0
          IF (I .EQ. K) STKI(LL) = 0.0D0
          IF (I .GT. K) STKR(LL) = -STKR(LL)
          IF (I .GT. K) STKI(LL) = -STKI(LL)
   61   CONTINUE
        I = BUF(K)
        IF (I .EQ. K) GOTO 64
        LI = L+I-1+(K-1)*N
        LK = L+K-1+(K-1)*N
        CALL ML_WSWAP(N-K+1,STKR(LI),STKI(LI),N,STKR(LK),STKI(LK),N)
   64 CONTINUE
      GOTO 99
!.......................................................................
!     HILBERT
   70 N = IDINT(STKR(L))
      MSTK(TOP) = N
      NSTK(TOP) = N
   72 CALL ML_HILBER(STKR(L),N,N)
      CALL ML_RSET(N*N,0.0D0,STKI(L),1)
      IF (FIN .LT. 0) CALL ML_WSCAL(N*N,SR,SI,STKR(L),STKI(L),1)
      GOTO 99
!.......................................................................
!
!     CHOLESKY
   80 IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      CALL ML_WPOFA(STKR(L),STKI(L),M,N,ERR)
      IF (ERR .NE. 0) CALL ML_ERROR(29)
      IF (ERR .GT. 0) RETURN
      DO 81 J = 1, N
        LL = L+J+(J-1)*M
        CALL ML_WSET(M-J,0.0D0,0.0D0,STKR(LL),STKI(LL),1)
   81 CONTINUE
      GOTO 99
!.......................................................................
!
!     RREF
   85 IF (RHS .LT. 2) GOTO 86
        TOP = TOP-1
        L = LSTK(TOP)
        IF (MSTK(TOP) .NE. M) CALL ML_ERROR(5)
        IF (ERR .GT. 0) RETURN
        N = N + NSTK(TOP)
   86 CALL ML_RREF(STKR(L),STKI(L),M,M,N,STKR(VSIZE-4))
      NSTK(TOP) = N
      GOTO 99
!.......................................................................
!
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_MATFN2
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!
!     EVALUATE ELEMENTARY FUNCTIONS AND FUNCTIONS INVOLVING
!     EIGENVALUES AND EIGENVECTORS
!

      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      DOUBLE PRECISION ML_PYTHAG,ML_ROUND,TR,TI,SR,SI,POWR,POWI,ML_FLOP
      LOGICAL HERM,SCHUR,VECT,HESS
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN2* ', FIN)
!
!     FUNCTIONS/FIN
!     **   SIN  COS ATAN  EXP  SQRT LOG
!      0    1    2    3    4    5    6
!    EIG  SCHU HESS POLY ROOT
!     11   12   13   14   15
!    ABS  ROUN REAL IMAG CONJ
!     21   22   23   24   25
      IF (FIN .NE. 0) GOTO 05
         L = LSTK(TOP+1)
         POWR = STKR(L)
         POWI = STKI(L)
   05 L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (FIN .GE. 11 .AND. FIN .LE. 13) GOTO 10
      IF (FIN .EQ. 14 .AND. (M.EQ.1 .OR. N.EQ.1)) GOTO 50
      IF (FIN .EQ. 14) GOTO 10
      IF (FIN .EQ. 15) GOTO 60
      IF (FIN .GT. 20) GOTO 40
      IF (M .EQ. 1 .OR. N .EQ. 1) GOTO 40
!
!     EIGENVALUES AND VECTORS
   10 IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      SCHUR = FIN .EQ. 12
      HESS = FIN .EQ. 13
      VECT = LHS.EQ.2 .OR. FIN.LT.10
      NN = N*N
      L2 = L + NN
      LD = L2 + NN
      LE = LD + N
      LW = LE + N
      ERR = LW+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WCOPY(NN,STKR(L),STKI(L),1,STKR(L2),STKI(L2),1)
!
!     CHECK IF HERMITIAN
      HERM=.FALSE.
      DO 15 J = 1, N
      DO 15 I = 1, J
         LS = L+I-1+(J-1)*N
         LL = L+(I-1)*N+J-1
         HERM = STKR(LL).EQ.STKR(LS) .AND. STKI(LL).EQ.-STKI(LS)
         IF (.NOT. HERM) GOTO 30
   15 CONTINUE
!
!     HERMITIAN EIGENVALUE PROBLEM
      CALL ML_WSET(NN,0.0D0,0.0D0,STKR(L),STKI(L),1)
      CALL ML_WSET(N,1.0D0,0.0D0,STKR(L),STKI(L),N+1)
      CALL ML_WSET(N,0.0D0,0.0D0,STKI(LD),STKI(LE),1)
      JOB = 0
      IF (VECT) JOB = 1
      CALL ML_HTRIDI(N,N,STKR(L2),STKI(L2),STKR(LD),STKR(LE),STKR(LE),STKR(LW))
      IF(.NOT.HESS)CALL ML_IMTQL2(N,N,STKR(LD),STKR(LE),STKR(L),ERR,JOB)
      IF (ERR .GT. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
      IF (JOB .NE. 0) CALL ML_HTRIBK(N,N,STKR(L2),STKI(L2),STKR(LW),N,STKR(L),STKI(L))
      GOTO 31
!
!     NON-HERMITIAN EIGENVALUE PROBLEM
   30 CALL ML_CORTH(N,N,1,N,STKR(L2),STKI(L2),STKR(LW),STKI(LW))
      IF (.NOT.VECT .AND. HESS) GOTO 31
      JOB = 0
      IF (VECT) JOB = 2
      IF (VECT .AND. SCHUR) JOB = 1
      IF (HESS) JOB = 3
      CALL ML_COMQR3(N,N,1,N,STKR(LW),STKI(LW),STKR(L2),STKI(L2), STKR(LD),STKI(LD),STKR(L),STKI(L),ERR,JOB)
      IF (ERR .GT. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
!
!     VECTORS
   31 IF (.NOT.VECT) GOTO 34
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L2
      MSTK(TOP) = N
      NSTK(TOP) = N
!
!     DIAGONAL OF VALUES OR CANONICAL FORMS
   34 IF (.NOT.VECT .AND. .NOT.SCHUR .AND. .NOT.HESS) GOTO 37
      DO 36 J = 1, N
         LJ = L2+(J-1)*N
         IF (SCHUR .AND. (.NOT.HERM)) LJ = LJ+J
         IF (HESS .AND. (.NOT.HERM)) LJ = LJ+J+1
         LL = L2+J*N-LJ
         CALL ML_WSET(LL,0.0D0,0.0D0,STKR(LJ),STKI(LJ),1)
   36 CONTINUE
      IF (.NOT.HESS .OR. HERM) CALL ML_WCOPY(N,STKR(LD),STKI(LD),1,STKR(L2),STKI(L2),N+1)
      LL = L2+1
      IF (HESS .AND. HERM)CALL ML_WCOPY(N-1,STKR(LE+1),STKI(LE+1),1,STKR(LL),STKI(LL),N+1)
      LL = L2+N
      IF (HESS .AND. HERM)CALL ML_WCOPY(N-1,STKR(LE+1),STKI(LE+1),1,STKR(LL),STKI(LL),N+1)
      IF (FIN .LT. 10) GOTO 42
      IF (VECT .OR. .NOT.(SCHUR.OR.HESS)) GOTO 99
      CALL ML_WCOPY(NN,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      GOTO 99
!
!     VECTOR OF EIGENVALUES
   37 IF (FIN .EQ. 14) GOTO 52
      CALL ML_WCOPY(N,STKR(LD),STKI(LD),1,STKR(L),STKI(L),1)
      NSTK(TOP) = 1
      GOTO 99
!
!     ELEMENTARY FUNCTIONS
!     FOR MATRICES.. X,D = EIG(A), FUN(A) = X*FUN(D)/X
   40 INC = 1
      N = M*N
      L2 = L
      GOTO 44
   42 INC = N+1
   44 DO 46 J = 1, N
        LS = L2+(J-1)*INC
        SR = STKR(LS)
        SI = STKI(LS)
        TI = 0.0D0
        IF (FIN .NE. 0) GOTO 45
          CALL ML_WLOG(SR,SI,SR,SI)
          CALL ML_WMUL(SR,SI,POWR,POWI,SR,SI)
          TR = DEXP(SR)*DCOS(SI)
          TI = DEXP(SR)*DSIN(SI)
   45   IF (FIN .EQ. 1) TR = DSIN(SR)*DCOSH(SI)
        IF (FIN .EQ. 1) TI = DCOS(SR)*DSINH(SI)
        IF (FIN .EQ. 2) TR = DCOS(SR)*DCOSH(SI)
        IF (FIN .EQ. 2) TI = (-DSIN(SR))*DSINH(SI)
        IF (FIN .EQ. 3) CALL ML_WATAN(SR,SI,TR,TI)
        IF (FIN .EQ. 4) TR = DEXP(SR)*DCOS(SI)
        IF (FIN .EQ. 4) TI = DEXP(SR)*DSIN(SI)
        IF (FIN .EQ. 5) CALL ML_WSQRT(SR,SI,TR,TI)
        IF (FIN .EQ. 6) CALL ML_WLOG(SR,SI,TR,TI)
        IF (FIN .EQ. 21) TR = ML_PYTHAG(SR,SI)
        IF (FIN .EQ. 22) TR = ML_ROUND(SR)
        IF (FIN .EQ. 23) TR = SR
        IF (FIN .EQ. 24) TR = SI
        IF (FIN .EQ. 25) TR = SR
        IF (FIN .EQ. 25) TI = -SI
        IF (ERR .GT. 0) RETURN
        STKR(LS) = ML_FLOP(TR)
        STKI(LS) = 0.0D0
        IF (TI .NE. 0.0D0) STKI(LS) = ML_FLOP(TI)
   46 CONTINUE
      IF (INC .EQ. 1) GOTO 99
      DO 48 J = 1, N
        LS = L2+(J-1)*INC
        SR = STKR(LS)
        SI = STKI(LS)
        LS = L+(J-1)*N
        LL = L2+(J-1)*N
        CALL ML_WCOPY(N,STKR(LS),STKI(LS),1,STKR(LL),STKI(LL),1)
        CALL ML_WSCAL(N,SR,SI,STKR(LS),STKI(LS),1)
   48 CONTINUE
!     SIGNAL MATFN1 TO DIVIDE BY EIGENVECTORS
      FUN = 21
      FIN = -1
      TOP = TOP-1
      GOTO 99
!
!     POLY
!     FORM POLYNOMIAL WITH GIVEN VECTOR AS ROOTS
   50 N = MAX0(M,N)
      LD = L+N+1
      CALL ML_WCOPY(N,STKR(L),STKI(L),1,STKR(LD),STKI(LD),1)
!
!     FORM CHARACTERISTIC POLYNOMIAL
   52 CALL ML_WSET(N+1,0.0D0,0.0D0,STKR(L),STKI(L),1)
      STKR(L) = 1.0D0
      DO 56 J = 1, N
         CALL ML_WAXPY(J,-STKR(LD),-STKI(LD),STKR(L),STKI(L),-1, STKR(L+1),STKI(L+1),-1)
         LD = LD+1
   56 CONTINUE
      MSTK(TOP) = N+1
      NSTK(TOP) = 1
      GOTO 99
!
!     ROOTS
   60 LL = L+M*N
      STKR(LL) = -1.0D0
      STKI(LL) = 0.0D0
      K = -1
   61 K = K+1
      L1 = L+K
      IF (DABS(STKR(L1))+DABS(STKI(L1)) .EQ. 0.0D0) GOTO 61
      N = MAX0(M*N - K-1, 0)
      IF (N .LE. 0) GOTO 65
      L2 = L1+N+1
      LW = L2+N*N
      ERR = LW+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WSET(N*N+N,0.0D0,0.0D0,STKR(L2),STKI(L2),1)
      DO 64 J = 1, N
         LL = L2+J+(J-1)*N
         STKR(LL) = 1.0D0
         LS = L1+J
         LL = L2+(J-1)*N
         CALL ML_WDIV(-STKR(LS),-STKI(LS),STKR(L1),STKI(L1), STKR(LL),STKI(LL))
         IF (ERR .GT. 0) RETURN
   64 CONTINUE
      CALL ML_COMQR3(N,N,1,N,STKR(LW),STKI(LW),STKR(L2),STKI(L2),STKR(L),STKI(L),TR,TI,ERR,0)
      IF (ERR .GT. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
   65 MSTK(TOP) = N
      NSTK(TOP) = 1
      GOTO 99
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_MATFN3
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!
!     EVALUATE FUNCTIONS INVOLVING SINGULAR VALUE DECOMPOSITION
!

      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      LOGICAL FRO,INF
      DOUBLE PRECISION P,S,T,TOL,EPS
      DOUBLE PRECISION ML_FLOP
      DOUBLE PRECISION ML_WDOTCR,ML_WDOTCI,ML_PYTHAG,ML_WNRM2,ML_WASUM
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN3* ', FIN)
!
      IF (FIN.EQ.1 .AND. RHS.EQ.2) TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      MN = M*N
      GOTO (50,70,10,30,70), FIN
!
!     COND
!
   10 LD = L + M*N
      L1 = LD + MIN0(M+1,N)
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WSVDC(STKR(L),STKI(L),M,M,N,STKR(LD),STKI(LD),STKR(L1),STKI(L1),T,T,1,T,T,1,STKR(L2),STKI(L2),0,ERR)
      IF (ERR .NE. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
      S = STKR(LD)
      LD = LD + MIN0(M,N) - 1
      T = STKR(LD)
      IF (T .EQ. 0.0D0) GOTO 13
      STKR(L) = ML_FLOP(S/T)
      STKI(L) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      GOTO 99
   13 CONTINUE
      call journal(' CONDITION IS INFINITE')
      MSTK(TOP) = 0
      GOTO 99
!
!     NORM
!
   30 P = 2.0D0
      INF = .FALSE.
      IF (RHS .NE. 2) GOTO 31
      FRO = IDINT(STKR(L)).EQ.15 .AND. MN.GT.1
      INF = IDINT(STKR(L)).EQ.18 .AND. MN.GT.1
      IF (.NOT. FRO) P = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      MN = M*N
      IF (FRO) M = MN
      IF (FRO) N = 1
   31 IF (M .GT. 1 .AND. N .GT. 1) GOTO 40
      IF (P .EQ. 1.0D0) GOTO 36
      IF (P .EQ. 2.0D0) GOTO 38
      I = ML_IWAMAX(MN,STKR(L),STKI(L),1) + L - 1
      S = DABS(STKR(I)) + DABS(STKI(I))
      IF (INF .OR. S .EQ. 0.0D0) GOTO 49
      T = 0.0D0
      DO 33 I = 1, MN
         LS = L+I-1
         T = ML_FLOP(T + (ML_PYTHAG(STKR(LS),STKI(LS))/S)**P)
   33 CONTINUE
      IF (P .NE. 0.0D0) P = 1.0D0/P
      S = ML_FLOP(S*T**P)
      GOTO 49
   36 S = ML_WASUM(MN,STKR(L),STKI(L),1)
      GOTO 49
   38 S = ML_WNRM2(MN,STKR(L),STKI(L),1)
      GOTO 49
!
!     MATRIX NORM
!
   40 IF (INF) GOTO 43
      IF (P .EQ. 1.0D0) GOTO 46
      IF (P .NE. 2.0D0) CALL ML_ERROR(23)
      IF (ERR .GT. 0) RETURN
      LD = L + M*N
      L1 = LD + MIN0(M+1,N)
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WSVDC(STKR(L),STKI(L),M,M,N,STKR(LD),STKI(LD),STKR(L1),STKI(L1),T,T,1,T,T,1,STKR(L2),STKI(L2),0,ERR)
      IF (ERR .NE. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
      S = STKR(LD)
      GOTO 49
   43 S = 0.0D0
      DO 45 I = 1, M
         LI = L+I-1
         T = ML_WASUM(N,STKR(LI),STKI(LI),M)
         S = DMAX1(S,T)
   45 CONTINUE
      GOTO 49
   46 S = 0.0D0
      DO 48 J = 1, N
         LJ = L+(J-1)*M
         T = ML_WASUM(M,STKR(LJ),STKI(LJ),1)
         S = DMAX1(S,T)
   48 CONTINUE
      GOTO 49
   49 STKR(L) = S
      STKI(L) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      GOTO 99
!
!     SVD
!
   50 IF (LHS .NE. 3) GOTO 52
      K = M
      IF (RHS .EQ. 2) K = MIN0(M,N)
      LU = L + M*N
      LD = LU + M*K
      LV = LD + K*N
      L1 = LV + N*N
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      JOB = 11
      IF (RHS .EQ. 2) JOB = 21
      CALL ML_WSVDC(STKR(L),STKI(L),M,M,N,STKR(LD),STKI(LD),STKR(L1),STKI(L1),STKR(LU),STKI(LU),M,STKR(LV),STKI(LV), &
     &        N,STKR(L2),STKI(L2),JOB,ERR)
      DO 51 JB = 1, N
      DO 51 I = 1, K
        J = N+1-JB
        LL = LD+I-1+(J-1)*K
        IF (I.NE.J) STKR(LL) = 0.0D0
        STKI(LL) = 0.0D0
        LS = LD+I-1
        IF (I.EQ.J) STKR(LL) = STKR(LS)
        LS = L1+I-1
        IF (ERR.NE.0 .AND. I.EQ.J-1) STKR(LL) = STKR(LS)
   51 CONTINUE
      IF (ERR .NE. 0) CALL ML_ERROR(24)
      ERR = 0
      CALL ML_WCOPY(M*K+K*N+N*N,STKR(LU),STKI(LU),1,STKR(L),STKI(L),1)
      MSTK(TOP) = M
      NSTK(TOP) = K
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L + M*K
      MSTK(TOP) = K
      NSTK(TOP) = N
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L + M*K + K*N
      MSTK(TOP) = N
      NSTK(TOP) = N
      GOTO 99
!
   52 LD = L + M*N
      L1 = LD + MIN0(M+1,N)
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WSVDC(STKR(L),STKI(L),M,M,N,STKR(LD),STKI(LD),STKR(L1),STKI(L1),T,T,1,T,T,1,STKR(L2),STKI(L2),0,ERR)
      IF (ERR .NE. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
      K = MIN0(M,N)
      CALL ML_WCOPY(K,STKR(LD),STKI(LD),1,STKR(L),STKI(L),1)
      MSTK(TOP) = K
      NSTK(TOP) = 1
      GOTO 99
!
!     PINV AND RANK
!
   70 TOL = -1.0D0
      IF (RHS .NE. 2) GOTO 71
      TOL = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
   71 LU = L + M*N
      LD = LU + M*M
      IF (FIN .EQ. 5) LD = L + M*N
      LV = LD + M*N
      L1 = LV + N*N
      IF (FIN .EQ. 5) L1 = LD + N
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      IF (FIN .EQ. 2) JOB = 11
      IF (FIN .EQ. 5) JOB = 0
      CALL ML_WSVDC(STKR(L),STKI(L),M,M,N,STKR(LD),STKI(LD),STKR(L1),STKI(L1),STKR(LU),STKI(LU),M,STKR(LV),STKI(LV), &
     &        N,STKR(L2),STKI(L2),JOB,ERR)
      IF (ERR .NE. 0) CALL ML_ERROR(24)
      IF (ERR .GT. 0) RETURN
      EPS = STKR(VSIZE-4)
      IF (TOL .LT. 0.0D0) TOL = ML_FLOP(DFLOAT(MAX0(M,N))*EPS*STKR(LD))
      MN = MIN0(M,N)
      K = 0
      DO 72 J = 1, MN
        LS = LD+J-1
        S = STKR(LS)
        IF (S .LE. TOL) GOTO 73
        K = J
        LL = LV+(J-1)*N
        IF (FIN .EQ. 2) CALL ML_WRSCAL(N,1.0D0/S,STKR(LL),STKI(LL),1)
   72 CONTINUE
   73 IF (FIN .EQ. 5) GOTO 78
      DO 76 J = 1, M
      DO 76 I = 1, N
        LL = L+I-1+(J-1)*N
        L1 = LV+I-1
        L2 = LU+J-1
        STKR(LL) = ML_WDOTCR(K,STKR(L2),STKI(L2),M,STKR(L1),STKI(L1),N)
        STKI(LL) = ML_WDOTCI(K,STKR(L2),STKI(L2),M,STKR(L1),STKI(L1),N)
   76 CONTINUE
      MSTK(TOP) = N
      NSTK(TOP) = M
      GOTO 99
   78 STKR(L) = DFLOAT(K)
      STKI(L) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      GOTO 99
!
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_MATFN4
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!
!     EVALUATE FUNCTIONS INVOLVING QR DECOMPOSITION (LEAST SQUARES)
!

      character mline*81
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      DOUBLE PRECISION T,TOL,EPS,ML_FLOP
      INTEGER QUOTE
      save quote
      DATA QUOTE/49/
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN4* ', FIN)
!
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (FIN .EQ. -1) GOTO 10
      IF (FIN .EQ. -2) GOTO 20
      GOTO 40
!
!     RECTANGULAR MATRIX RIGHT DIVISION, A/A2
   10 L2 = LSTK(TOP+1)
      M2 = MSTK(TOP+1)
      N2 = NSTK(TOP+1)
      TOP = TOP + 1
      IF (N.GT.1 .AND. N.NE.N2) CALL ML_ERROR(11)
      IF (ERR .GT. 0) RETURN
      CALL ML_STACK1(QUOTE)
      IF (ERR .GT. 0) RETURN
      LL = L2+M2*N2
      CALL ML_WCOPY(M*N,STKR(L),STKI(L),1,STKR(LL),STKI(LL),1)
      CALL ML_WCOPY(M*N+M2*N2,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      LSTK(TOP) = L+M2*N2
      MSTK(TOP) = M
      NSTK(TOP) = N
      CALL ML_STACK1(QUOTE)
      IF (ERR .GT. 0) RETURN
      TOP = TOP - 1
      M = N2
      N = M2
      GOTO 20
!
!     RECTANGULAR MATRIX LEFT DIVISION A BACKSLASH A2
!
   20 L2 = LSTK(TOP+1)
      M2 = MSTK(TOP+1)
      N2 = NSTK(TOP+1)
      IF (M2*N2 .GT. 1) GOTO 21
        M2 = M
        N2 = M
        ERR = L2+M*M - LSTK(BOT)
        IF (ERR .GT. 0) CALL ML_ERROR(17)
        IF (ERR .GT. 0) RETURN
        CALL ML_WSET(M*M-1,0.0D0,0.0D0,STKR(L2+1),STKI(L2+1),1)
        CALL ML_WCOPY(M,STKR(L2),STKI(L2),0,STKR(L2),STKI(L2),M+1)
   21 IF (M2 .NE. M) CALL ML_ERROR(12)
      IF (ERR .GT. 0) RETURN
      L3 = L2 + MAX0(M,N)*N2
      L4 = L3 + N
      ERR = L4 + N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      IF (M .GT. N) GOTO 23
      DO 22 JB = 1, N2
        J = N+1-JB
        LS = L2 + (J-1)*M
        LL = L2 + (J-1)*N
        CALL ML_WCOPY(M,STKR(LS),STKI(LS),-1,STKR(LL),STKI(LL),-1)
   22 CONTINUE
   23 DO 24 J = 1, N
        BUF(J) = 0
   24 CONTINUE
      CALL ML_WQRDC(STKR(L),STKI(L),M,M,N,STKR(L4),STKI(L4),BUF,STKR(L3),STKI(L3),1)
      K = 0
      EPS = STKR(VSIZE-4)
      T = DABS(STKR(L))+DABS(STKI(L))
      TOL = ML_FLOP(DFLOAT(MAX0(M,N))*EPS*T)
      MN = MIN0(M,N)
      DO 27 J = 1, MN
        LS = L+J-1+(J-1)*M
        T = DABS(STKR(LS)) + DABS(STKI(LS))
        IF (T .GT. TOL) K = J
   27 CONTINUE
      IF (K .LT. MN) then
         WRITE(mline,'('' RANK DEFICIENT,  RANK ='',I4,'',  TOL ='',1PD13.4)') K,TOL
         call journal(mline)
      endif
      MN = MAX0(M,N)
      DO 29 J = 1, N2
        LS = L2+(J-1)*MN
        CALL ML_WQRSL(STKR(L),STKI(L),M,M,K,STKR(L4),STKI(L4),STKR(LS),STKI(LS),T,T,STKR(LS),STKI(LS),STKR(LS), &
       & STKI(LS),T,T,T,T,100,INFO)
        LL = LS+K
        CALL ML_WSET(N-K,0.0D0,0.0D0,STKR(LL),STKI(LL),1)
   29 CONTINUE
      DO 31 J = 1, N
        BUF(J) = -BUF(J)
   31 CONTINUE
      DO 35 J = 1, N
        IF (BUF(J) .GT. 0) GOTO 35
        K = -BUF(J)
        BUF(J) = K
   33   CONTINUE
          IF (K .EQ. J) GOTO 34
          LS = L2+J-1
          LL = L2+K-1
          CALL ML_WSWAP(N2,STKR(LS),STKI(LS),MN,STKR(LL),STKI(LL),MN)
          BUF(K) = -BUF(K)
          K = BUF(K)
          GOTO 33
   34   CONTINUE
   35 CONTINUE
      DO 36 J = 1, N2
        LS = L2+(J-1)*MN
        LL = L+(J-1)*N
        CALL ML_WCOPY(N,STKR(LS),STKI(LS),1,STKR(LL),STKI(LL),1)
   36 CONTINUE
      MSTK(TOP) = N
      NSTK(TOP) = N2
      IF (FIN .EQ. -1) CALL ML_STACK1(QUOTE)
      IF (ERR .GT. 0) RETURN
      GOTO 99
!
!     QR
!
   40 MM = MAX0(M,N)
      LS = L + MM*MM
      IF (LHS.EQ.1 .AND. FIN.EQ.1) LS = L
      LE = LS + M*N
      L4 = LE + MM
      ERR = L4+MM - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      IF (LS.NE.L) CALL ML_WCOPY(M*N,STKR(L),STKI(L),1,STKR(LS),STKI(LS),1)
      JOB = 1
      IF (LHS.LT.3) JOB = 0
      DO 42 J = 1, N
        BUF(J) = 0
   42 CONTINUE
      CALL ML_WQRDC(STKR(LS),STKI(LS),M,M,N,STKR(L4),STKI(L4),BUF,STKR(LE),STKI(LE),JOB)
      IF (LHS.EQ.1 .AND. FIN.EQ.1) GOTO 99
      CALL ML_WSET(M*M,0.0D0,0.0D0,STKR(L),STKI(L),1)
      CALL ML_WSET(M,1.0D0,0.0D0,STKR(L),STKI(L),M+1)
      DO 43 J = 1, M
        LL = L+(J-1)*M
        CALL ML_WQRSL(STKR(LS),STKI(LS),M,M,N,STKR(L4),STKI(L4),   &
     &             STKR(LL),STKI(LL),STKR(LL),STKI(LL),T,T,        &
     &             T,T,T,T,T,T,10000,INFO)
   43 CONTINUE
      IF (FIN .EQ. 2) GOTO 99
      NSTK(TOP) = M
      DO 45 J = 1, N
        LL = LS+J+(J-1)*M
        CALL ML_WSET(M-J,0.0D0,0.0D0,STKR(LL),STKI(LL),1)
   45 CONTINUE
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = LS
      MSTK(TOP) = M
      NSTK(TOP) = N
      IF (LHS .EQ. 2) GOTO 99
      CALL ML_WSET(N*N,0.0D0,0.0D0,STKR(LE),STKI(LE),1)
      DO 47 J = 1, N
        LL = LE+BUF(J)-1+(J-1)*N
        STKR(LL) = 1.0D0
   47 CONTINUE
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = LE
      MSTK(TOP) = N
      NSTK(TOP) = N
      GOTO 99
!
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_MATFN5()   ! file handling and other I/O
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      character mline*(256)
      character ch_char*1
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      PARAMETER (IALF=78)
      INTEGER ALFA(IALF),ALFB(IALF),ALFL,CASE
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /ALFS/ ALFA,ALFB,ALFL,CASE
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER EOL,CH,BLANK,FLAG,TOP2,PLUS,MINUS,QUOTE,SEMI,LRAT,MRAT
      INTEGER ID(4)
      DOUBLE PRECISION EPS,B,S,T,ML_FLOP,ML_WASUM,TDUM(2)
      LOGICAL TEXT
      SAVE EOL,BLANK,PLUS,MINUS,QUOTE,SEMI,LRAT,MRAT
      save flag
      DATA EOL/99/,BLANK/36/,PLUS/41/,MINUS/42/,QUOTE/49/,SEMI/39/
      DATA LRAT/5/,MRAT/100/
      data flag/0/
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN5* ',FIN)
!     FUNCTIONS/FIN
!     EXEC SAVE LOAD PRIN DIAR DISP BASE LINE CHAR PLOT RAT  DEBU
!      1    2    3    4    5    6    7    8    9   10   11   12
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (FIN .GT. 5) GOTO 15
!
!     CONVERT FILE NAME
      MN = M*N
      FLAG = 3
      IF (SYM .EQ. SEMI) FLAG = 0
      IF (RHS .GE. 2) THEN
         FLAG = IDINT(STKR(L))
         TOP2 = TOP
         TOP = TOP-1
         L = LSTK(TOP)
         MN = MSTK(TOP)*NSTK(TOP)
      ENDIF
      LUN = -1
      IF (MN.EQ.1 .AND. STKR(L).LT.10.0D0) LUN = IDINT(STKR(L))
      IF (LUN .LT. 0) THEN
          DO 14 J = 1, 32
             LS = L+J-1
             IF (J .LE. MN) CH = IDINT(STKR(LS))
             IF (J .GT. MN) CH = BLANK
             IF (CH.LT.0 .OR. CH.GE.ALFL) CALL ML_ERROR(38)
             IF (ERR .GT. 0) RETURN
             IF (CASE .EQ. 0) BUF(J) = ALFA(CH+1)
             IF (CASE .EQ. 1) BUF(J) = ALFB(CH+1)
   14     CONTINUE
      ENDIF
!.......................................................................
   15 CONTINUE
      GOTO (20,30,35,25,27,60,65,70,50,80,40,95),FIN
!.......................................................................
!     EXEC
   20 CONTINUE
      IF (LUN .EQ. 0) THEN
!     EXEC(0)
         RIO = RTE
         ERR = 99
      else
         K = LPT(6)
         LIN(K+1) = LPT(1)
         LIN(K+2) = LPT(3)
         LIN(K+3) = LPT(6)
         LIN(K+4) = PTZ
         LIN(K+5) = RIO
         LIN(K+6) = LCT(4)
         LPT(1) = K + 7
         LCT(4) = FLAG
         PTZ = PT - 4
         IF (RIO .EQ. RTE) RIO = 12
         RIO = RIO + 1
         IF (LUN .GT. 0) RIO = LUN
         IF (LUN .LT. 0) CALL ML_FILES(RIO,BUF)
         IF (FLAG .GE. 4)call journal(' PAUSE MODE. ENTER BLANK LINES.')
         SYM = EOL
         MSTK(TOP) = 0
      endif
      GOTO 99
!.......................................................................
!     PRINT
   25 CONTINUE
      K = WTE
      WTE = LUN
      IF (LUN .LT. 0) WTE = 7
      IF (LUN .LT. 0) CALL ML_FILES(WTE,BUF)

      L = LCT(2)
      LCT(2) = 9999
      IF (RHS .GT. 1) CALL ML_PRINT(SYN,TOP2)
      LCT(2) = L

      WTE = K
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     DIARY
   27 CONTINUE
      call journal('*DIARY COMMAND DOES NOTHING')
      !WIO = LUN
      !CALL ML_FILES(WIO,BUF)
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     SAVE
   30 CONTINUE
      IF (LUN .LT. 0) LUNIT = 1
      IF (LUN .LT. 0) CALL ML_FILES(LUNIT,BUF)
      IF (LUN .GT. 0) LUNIT = LUN
      K = LSIZE-4
      IF (K .LT. BOT) K = LSIZE
      IF (RHS .EQ. 2) K = TOP2
      IF (RHS .EQ. 2) CALL ML_PUTID(IDSTK(1,K),SYN)
   32 CONTINUE
      L = LSTK(K)
      M = MSTK(K)
      N = NSTK(K)
      DO 34 I = 1, 4
         J = IDSTK(I,K)+1
         BUF(I) = ALFA(J)
   34 CONTINUE
      IMG = 0
      IF (ML_WASUM(M*N,STKI(L),STKI(L),1) .NE. 0.0D0) IMG = 1
      IF(FE .EQ. 0)CALL ML_SAVLOD(LUNIT,BUF,M,N,IMG,0,STKR(L),STKI(L))
      K = K-1
      IF (K .GE. BOT) GOTO 32
      CALL ML_FILES(-LUNIT,BUF)
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     LOAD
   35 CONTINUE
      IF (LUN .LT. 0) LUNIT = 2
      IF (LUN .LT. 0) CALL ML_FILES(LUNIT,BUF) ! open the unit
      IF (LUN .GT. 0) LUNIT = LUN
   36 CONTINUE
      JOB = LSTK(BOT) - L
      IF(FE .EQ. 0)CALL ML_SAVLOD(LUNIT,ID,MSTK(TOP),NSTK(TOP),IMG,JOB,STKR(L),STKI(L))
      MN = MSTK(TOP)*NSTK(TOP)
      IF (MN .EQ. 0) GOTO 39
      IF (IMG .EQ. 0) CALL ML_RSET(MN,0.0D0,STKI(L),1)
      DO 38 I = 1, 4
         J = 0
   37    CONTINUE
         J = J+1
         IF (ID(I).NE.ALFA(J) .AND. J.LE.BLANK) GOTO 37
         ID(I) = J-1
   38 CONTINUE
      SYM = SEMI
      RHS = 0
      CALL ML_STACKP(ID)
      TOP = TOP + 1
      GOTO 36
   39 CONTINUE
      CALL ML_FILES(-LUNIT,BUF) ! close unit
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     RAT
   40 CONTINUE
      IF (RHS .EQ. 2) GOTO 44
      MN = M*N
      L2 = L
      IF (LHS .EQ. 2) L2 = L + MN
      LW = L2 + MN
      ERR = LW + LRAT - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      IF (LHS .EQ. 2) TOP = TOP + 1
      LSTK(TOP) = L2
      MSTK(TOP) = M
      NSTK(TOP) = N
      CALL ML_RSET(LHS*MN,0.0D0,STKI(L),1)
      DO 42 I = 1, MN
         CALL ML_RAT(STKR(L),LRAT,MRAT,S,T,STKR(LW))
         STKR(L) = S
         STKR(L2) = T
         IF (LHS .EQ. 1) STKR(L) = ML_FLOP(S/T)
         L = L + 1
         L2 = L2 + 1
   42 CONTINUE
      GOTO 99
   44 MRAT = IDINT(STKR(L))
      LRAT = IDINT(STKR(L-1))
      TOP = TOP - 1
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     CHAR
   50 CONTINUE
      K = IABS(IDINT(STKR(L)))
      IF (M*N.NE.1 .OR. K.GE.ALFL) CALL ML_ERROR(36)
      IF (ERR .GT. 0) RETURN
      CH = ALFA(K+1)
      IF (STKR(L) .LT. 0.0D0) CH = ALFB(K+1)
      WRITE(mline,'('' REPLACE CHARACTER '',A1)') CHAR(CH)
      call journal(mline)
      READ(RTE,'(A1)') CH_CHAR
      call str2buf(ch_char,ch,1)
      IF (STKR(L) .GE. 0.0D0) ALFA(K+1) = CH
      IF (STKR(L) .LT. 0.0D0) ALFB(K+1) = CH
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!
!     DISP
   60 CONTINUE
      call journal(' ')
      IF (RHS .EQ. 2) GOTO 65
      MN = M*N
      TEXT = .TRUE.
      DO 62 I = 1, MN
        LS = L+I-1
        CH = IDINT(STKR(LS))
        TEXT = TEXT .AND. (CH.GE.0) .AND. (CH.LT.ALFL)
        TEXT = TEXT .AND. (DFLOAT(CH).EQ.STKR(LS))
   62 CONTINUE
      DO 64 I = 1, M
      DO 63 J = 1, N
        LS = L+I-1+(J-1)*M
        IF (STKR(LS) .EQ. 0.0D0) CH = BLANK
        IF (STKR(LS) .GT. 0.0D0) CH = PLUS
        IF (STKR(LS) .LT. 0.0D0) CH = MINUS
        IF (TEXT) CH = IDINT(STKR(LS))
        BUF(J) = ALFA(CH+1)
   63 CONTINUE
      call buf2str(mline,buf,n)
      call journal(mline)
   64 CONTINUE
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!
!     BASE
   65 CONTINUE
      IF (RHS .NE. 2) CALL ML_ERROR(39)
      IF (STKR(L) .LE. 1.0D0) CALL ML_ERROR(36)
      IF (ERR .GT. 0) RETURN
      B = STKR(L)
      L2 = L
      TOP = TOP-1
      RHS = 1
      L = LSTK(TOP)
      M = MSTK(TOP)*NSTK(TOP)
      EPS = STKR(VSIZE-4)
      DO 66 I = 1, M
         LS = L2+(I-1)*N
         LL = L+I-1
         CALL ML_BASE(STKR(LL),B,EPS,STKR(LS),N)
   66 CONTINUE
      CALL ML_RSET(M*N,0.0D0,STKI(L2),1)
      CALL ML_WCOPY(M*N,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      MSTK(TOP) = N
      NSTK(TOP) = M
      CALL ML_STACK1(QUOTE)
      IF (FIN .EQ. 6) GOTO 60
      GOTO 99
!.......................................................................
!     LINES
   70 CONTINUE
      LCT(2) = IDINT(STKR(L))
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     PLOT
   80 CONTINUE
      IF (RHS .GE. 2) GOTO 82
      N = M*N
      DO 81 I = 1, N
         LL = L+I-1
         STKI(LL) = DFLOAT(I)
   81 CONTINUE
      CALL ML_PLOT(WTE,STKI(L),STKR(L),N,TDUM,0)
      MSTK(TOP) = 0
      GOTO 99
   82 IF (RHS .EQ. 2) K = 0
      IF (RHS .EQ. 3) K = M*N
      IF (RHS .GT. 3) K = RHS - 2
      TOP = TOP - (RHS - 1)
      N = MSTK(TOP)*NSTK(TOP)
      IF (MSTK(TOP+1)*NSTK(TOP+1) .NE. N) CALL ML_ERROR(5)
      IF (ERR .GT. 0) RETURN
      LX = LSTK(TOP)
      LY = LSTK(TOP+1)
      IF (RHS .GT. 3) L = LSTK(TOP+2)
      CALL ML_PLOT(WTE,STKR(LX),STKR(LY),N,STKR(L),K)
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!     DEBUG
   95 CONTINUE
      DDT = IDINT(STKR(L))
      call journal('sc',' DEBUG ',DDT)
      MSTK(TOP) = 0
      GOTO 99
!.......................................................................
!
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_MATFN6
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
!
!     EVALUATE UTILITY FUNCTIONS
!
      character*80 mline
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER SEMI,ID(4),UNIFOR(4),NORMAL(4),SEED(4)
      DOUBLE PRECISION EPS0,EPS,S,SR,SI,T
      DOUBLE PRECISION ML_FLOP,ML_URAND
      LOGICAL ML_EQID
      save semi,unifor,normal,seed
      DATA SEMI/39/
      DATA UNIFOR/30,23,18,15/,NORMAL/23,24,27,22/,SEED/28,14,14,13/
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN6* ',FIN)
!     FUNCTIONS/FIN
!     MAGI DIAG SUM  PROD USER EYE  RAND ONES CHOP SIZE KRON  TRIL TRIU
!       1    2    3    4    5    6    7    8    9   10  11-13  14   15
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      GOTO (75,80,65,67,70,90,90,90,60,77,50,50,50,80,80),FIN
!
!     KRONECKER PRODUCT
   50 IF (RHS .NE. 2) CALL ML_ERROR(39)
      IF (ERR .GT. 0) RETURN
      TOP = TOP - 1
      L = LSTK(TOP)
      MA = MSTK(TOP)
      NA = NSTK(TOP)
      LA = L + MAX0(M*N*MA*NA,M*N+MA*NA)
      LB = LA + MA*NA
      ERR = LB + M*N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
!     MOVE A AND B ABOVE RESULT
      CALL ML_WCOPY(MA*NA+M*N,STKR(L),STKI(L),1,STKR(LA),STKI(LA),1)
      DO 54 JA = 1, NA
        DO 53 J = 1, N
          LJ = LB + (J-1)*M
          DO 52 IA = 1, MA
!           GET J-TH COLUMN OF B
            CALL ML_WCOPY(M,STKR(LJ),STKI(LJ),1,STKR(L),STKI(L),1)
!           ADDRESS OF A(IA,JA)
            LS = LA + IA-1 + (JA-1)*MA
            DO 51 I = 1, M
!             A(IA,JA) OP B(I,J)
              IF (FIN .EQ. 11) CALL ML_WMUL(STKR(LS),STKI(LS),STKR(L),STKI(L),STKR(L),STKI(L))
              IF (FIN .EQ. 12) CALL ML_WDIV(STKR(LS),STKI(LS),STKR(L),STKI(L),STKR(L),STKI(L))
              IF (FIN .EQ. 13) CALL ML_WDIV(STKR(L),STKI(L),STKR(LS),STKI(LS),STKR(L),STKI(L))
              IF (ERR .GT. 0) RETURN
              L = L + 1
   51       CONTINUE
   52     CONTINUE
   53   CONTINUE
   54 CONTINUE
      MSTK(TOP) = M*MA
      NSTK(TOP) = N*NA
      GOTO 99
!
!     CHOP
   60 EPS0 = 1.0D0
   61 EPS0 = EPS0/2.0D0
      T = ML_FLOP(1.0D0 + EPS0)
      IF (T .GT. 1.0D0) GOTO 61
      EPS0 = 2.0D0*EPS0
      FLP(2) = IDINT(STKR(L))
      IF (SYM .NE. SEMI) then
         WRITE(mline,'(''CHOP '',I2,'' PLACES.'')') FLP(2)
         call journal(mline)
      endif
      EPS = 1.0D0
   63 EPS = EPS/2.0D0
      T = ML_FLOP(1.0D0 + EPS)
      IF (T .GT. 1.0D0) GOTO 63
      EPS = 2.0D0*EPS
      T = STKR(VSIZE-4)
      IF (T.LT.EPS .OR. T.EQ.EPS0) STKR(VSIZE-4) = EPS
      MSTK(TOP) = 0
      GOTO 99
!
!     SUM
   65 SR = 0.0D0
      SI = 0.0D0
      MN = M*N
      DO 66 I = 1, MN
         LS = L+I-1
         SR = ML_FLOP(SR+STKR(LS))
         SI = ML_FLOP(SI+STKI(LS))
   66 CONTINUE
      GOTO 69
!
!     PROD
   67 SR = 1.0D0
      SI = 0.0D0
      MN = M*N
      DO 68 I = 1, MN
         LS = L+I-1
         CALL ML_WMUL(STKR(LS),STKI(LS),SR,SI,SR,SI)
   68 CONTINUE
   69 STKR(L) = SR
      STKI(L) = SI
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      GOTO 99
!
!     USER
   70 S = 0.0D0
      T = 0.0D0
      IF (RHS .LT. 2) GOTO 72
      IF (RHS .LT. 3) GOTO 71
      T = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
   71 S = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
   72 CALL ML_USER(STKR(L),M,N,S,T)
      CALL ML_RSET(M*N,0.0D0,STKI(L),1)
      MSTK(TOP) = M
      NSTK(TOP) = N
      GOTO 99
!
!     MAGIC
   75 N = MAX0(IDINT(STKR(L)),0)
      IF (N .EQ. 2) N = 0
      IF (N .GT. 0) CALL ML_MAGIC(STKR(L),N,N)
      CALL ML_RSET(N*N,0.0D0,STKI(L),1)
      MSTK(TOP) = N
      NSTK(TOP) = N
      GOTO 99
!
!     SIZE
   77 STKR(L) = M
      STKR(L+1) = N
      STKI(L) = 0.0D0
      STKI(L+1) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 2
      IF (LHS .EQ. 1) GOTO 99
      NSTK(TOP) = 1
      TOP = TOP + 1
      LSTK(TOP) = L+1
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      GOTO 99
!
!     DIAG, TRIU, TRIL
   80 K = 0
      IF (RHS .NE. 2) GOTO 81
         K = IDINT(STKR(L))
         TOP = TOP-1
         L = LSTK(TOP)
         M = MSTK(TOP)
         N = NSTK(TOP)
   81 IF (FIN .GE. 14) GOTO 85
      IF (M .EQ. 1 .OR. N .EQ. 1) GOTO 83
      IF (K.GE.0) MN=MIN0(M,N-K)
      IF (K.LT.0) MN=MIN0(M+K,N)
      MSTK(TOP) = MAX0(MN,0)
      NSTK(TOP) = 1
      IF (MN .LE. 0) GOTO 99
      DO 82 I = 1, MN
         IF (K.GE.0) LS = L+(I-1)+(I+K-1)*M
         IF (K.LT.0) LS = L+(I-K-1)+(I-1)*M
         LL = L+I-1
         STKR(LL) = STKR(LS)
         STKI(LL) = STKI(LS)
   82 CONTINUE
      GOTO 99
   83 N = MAX0(M,N)+IABS(K)
      ERR = L+N*N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      MSTK(TOP) = N
      NSTK(TOP) = N
      DO 84 JB = 1, N
      DO 84 IB = 1, N
         J = N+1-JB
         I = N+1-IB
         SR = 0.0D0
         SI = 0.0D0
         IF (K.GE.0) LS = L+I-1
         IF (K.LT.0) LS = L+J-1
         LL = L+I-1+(J-1)*N
         IF (J-I .EQ. K) SR = STKR(LS)
         IF (J-I .EQ. K) SI = STKI(LS)
         STKR(LL) = SR
         STKI(LL) = SI
   84 CONTINUE
      GOTO 99
!
!     TRIL, TRIU
   85 DO 87 J = 1, N
         LD = L + J - K - 1 + (J-1)*M
         IF (FIN .EQ. 14) LL = J - K - 1
         IF (FIN .EQ. 14) LS = LD - LL
         IF (FIN .EQ. 15) LL = M - J + K
         IF (FIN .EQ. 15) LS = LD + 1
         IF (LL .GT. 0) CALL ML_WSET(LL,0.0D0,0.0D0,STKR(LS),STKI(LS),1)
   87 CONTINUE
      GOTO 99
!
!     EYE, RAND, ONES
   90 IF (M.GT.1 .OR. RHS.EQ.0) GOTO 94
      IF (RHS .NE. 2) GOTO 91
        NN = IDINT(STKR(L))
        TOP = TOP-1
        L = LSTK(TOP)
        N = NSTK(TOP)
   91 IF (FIN.NE.7 .OR. N.LT.4) GOTO 93
      DO 92 I = 1, 4
        LS = L+I-1
        ID(I) = IDINT(STKR(LS))
   92 CONTINUE
      IF (ML_EQID(ID,UNIFOR).OR.ML_EQID(ID,NORMAL)) GOTO 97
      IF (ML_EQID(ID,SEED)) GOTO 98
   93 IF (N .GT. 1) GOTO 94
      M = MAX0(IDINT(STKR(L)),0)
      IF (RHS .EQ. 2) N = MAX0(NN,0)
      IF (RHS .NE. 2) N = M
      ERR = L+M*N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      MSTK(TOP) = M
      NSTK(TOP) = N
      IF (M*N .EQ. 0) GOTO 99
   94 DO 96 J = 1, N
      DO 96 I = 1, M
        LL = L+I-1+(J-1)*M
        STKR(LL) = 0.0D0
        STKI(LL) = 0.0D0
        IF(I.EQ.J .OR. FIN.EQ.8) STKR(LL) = 1.0D0
        IF(FIN.EQ.7 .AND. RAN(2).EQ.0)STKR(LL)=ML_FLOP(ML_URAND(RAN(1)))
        IF(FIN.NE.7 .OR. RAN(2).EQ.0) GOTO 96
   95      SR = 2.0D0*ML_URAND(RAN(1))-1.0D0
           SI = 2.0D0*ML_URAND(RAN(1))-1.0D0
           T = SR*SR + SI*SI
           IF (T .GT. 1.0D0) GOTO 95
        STKR(LL) = ML_FLOP(SR*DSQRT((-(2.0D0*DLOG(T)))/T))
   96 CONTINUE
      GOTO 99
!
!     SWITCH UNIFORM AND NORMAL
   97 RAN(2) = ID(1) - UNIFOR(1)
      MSTK(TOP) = 0
      GOTO 99
!
!     SEED
   98 IF (RHS .EQ. 2) RAN(1) = NN
      STKR(L) = RAN(1)
      MSTK(TOP) = 1
      IF (RHS .EQ. 2) MSTK(TOP) = 0
      NSTK(TOP) = 1
      GOTO 99
!
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_RAT(X,LEN,MAXD,A,B,D)
      use M_journal, only : journal
      INTEGER LEN,MAXD
      DOUBLE PRECISION X,A,B,D(LEN)
!
!     A/B = CONTINUED FRACTION APPROXIMATION TO X
!           USING  LEN  TERMS EACH LESS THAN MAXD
!
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      DOUBLE PRECISION S,T,Z,ML_ROUND
      Z = X
      k=0  ! preset to illegal value
      if(LEN.lt.1)then
         call journal('*ml_rat* internal error -- len<1')
         return
      endif
      DO 10 I = 1, LEN
         K = I
         D(K) = ML_ROUND(Z)
         Z = Z - D(K)
         IF (DABS(Z)*DFLOAT(MAXD) .LE. 1.0D0) GOTO 20
         Z = 1.0D0/Z
   10 CONTINUE
   20 T = D(K)
      S = 1.0D0
      IF (K .LT. 2) GOTO 40
      DO 30 IB = 2, K
         I = K+1-IB
         Z = T
         T = D(I)*T + S
         S = Z
   30 CONTINUE
   40 IF (S .LT. 0.0D0) T = -T
      IF (S .LT. 0.0D0) S = -S
      IF (DDT .EQ. 27)then
              WRITE(WTE,50) X,T,S,(D(I),I=1,K) ! debug 27
   50 FORMAT(/1X,1PD23.15,0PF8.0,' /',F8.0,4X,6F5.0/(1X,45X,6F5.0))
      endif
      A = T
      B = S
      RETURN
      END
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
      SUBROUTINE ML_SAVLOD(LSAVE,ID,M,N,IMG,JOB,XREAL,XIMAG)
      INTEGER LSAVE,ID(4),M,N,IMG,JOB
      DOUBLE PRECISION XREAL(*),XIMAG(*)
      character*4 CID
!@(#) read next variable from a "save" file or write next variable to it
      ! should report I/O errors (disk full, unwritable, ....)
!
!     IMPLEMENT SAVE AND LOAD
!     LSAVE = LOGICAL UNIT NUMBER
!     ID = NAME, FORMAT 4A1
!     M, N = DIMENSIONS
!     IMG = NONZERO IF XIMAG IS NONZERO
!           RETURNED ON A LOAD
!     JOB = 0     FOR SAVE
!         = SPACE AVAILABLE FOR LOAD
!     XREAL, XIMAG = REAL AND OPTIONAL IMAGINARY PARTS
!.......................................................................
      IF (JOB .LE. 0) THEN   ! save
         call buf2str(cid,id,4) ! convert ID to a character string
         WRITE(LSAVE,101) CID,M,N,IMG
         DO 15 J = 1, N
            K = (J-1)*M+1
            L = J*M
            WRITE(LSAVE,102) (XREAL(I),I=K,L)                 ! real
            IF (IMG .NE. 0) WRITE(LSAVE,102) (XIMAG(I),I=K,L) ! imaginary
   15    CONTINUE
!.......................................................................
      ELSE                   ! load
         READ(LSAVE,101,END=30) cid,M,N,IMG
         call str2buf(cid,id,4) ! convert character string to and ID
         IF (M*N .GT. JOB) GOTO 30
         DO 25 J = 1, N
            K = (J-1)*M+1
            L = J*M
            READ(LSAVE,102,END=30) (XREAL(I),I=K,L)   ! real
            IF (IMG .NE. 0) READ(LSAVE,102,END=30) (XIMAG(I),I=K,L) !imaginary
   25    CONTINUE
      ENDIF
!.......................................................................
      RETURN
!.......................................................................
!     END OF FILE
   30 CONTINUE
      M = 0
      N = 0
      RETURN
!     SYSTEM DEPENDENT FORMATS
  101 FORMAT(A4,3I4)  ! ID, MxN dimensions of ID, IMAGINARY OR REAL FLAG
  102 FORMAT(4Z18)    ! format for data
      END
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
      SUBROUTINE ML_STACK1(OP)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      INTEGER OP
!
!     UNARY OPERATIONS
!

      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER QUOTE
      save quote
      DATA QUOTE/49/
      IF (DDT .EQ. 1) call journal('sc','ML_STACK1 ',OP)
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      MN = M*N
      IF (MN .EQ. 0) GOTO 99
      IF (OP .EQ. QUOTE) GOTO 30
!
!     UNARY MINUS
      CALL ML_WRSCAL(MN,-1.0D0,STKR(L),STKI(L),1)
      GOTO 99
!
!     TRANSPOSE
   30 LL = L + MN
      ERR = LL+MN - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WCOPY(MN,STKR(L),STKI(L),1,STKR(LL),STKI(LL),1)
      M = NSTK(TOP)
      N = MSTK(TOP)
      MSTK(TOP) = M
      NSTK(TOP) = N
      DO 50 I = 1, M
      DO 50 J = 1, N
        LS = L+MN+(J-1)+(I-1)*N
        LL = L+(I-1)+(J-1)*M
        STKR(LL) = STKR(LS)
        STKI(LL) = -STKI(LS)
   50 CONTINUE
      GOTO 99
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_STACK2(OP)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      INTEGER OP
!
!     BINARY AND TERNARY OPERATIONS
!
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN
      DOUBLE PRECISION ML_WDOTUR,ML_WDOTUI
      DOUBLE PRECISION SR,SI,E1,ST,E2,ML_FLOP
      INTEGER PLUS,MINUS,STAR,DSTAR,SLASH,BSLASH,DOT,COLON
      SAVE PLUS,MINUS,STAR,DSTAR,SLASH,BSLASH,DOT,COLON
      DATA PLUS/41/,MINUS/42/,STAR/43/,DSTAR/54/,SLASH/44/
      DATA BSLASH/45/,DOT/47/,COLON/40/
!
      IF (DDT .EQ. 1) call journal('sc',',STACK2 ',OP)
      L2 = LSTK(TOP)
      M2 = MSTK(TOP)
      N2 = NSTK(TOP)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      FUN = 0
      IF (OP .EQ. PLUS) GOTO 01
      IF (OP .EQ. MINUS) GOTO 03
      IF (OP .EQ. STAR) GOTO 05
      IF (OP .EQ. DSTAR) GOTO 30
      IF (OP .EQ. SLASH) GOTO 20
      IF (OP .EQ. BSLASH) GOTO 25
      IF (OP .EQ. COLON) GOTO 60
      IF (OP .GT. 2*DOT) GOTO 80
      IF (OP .GT. DOT) GOTO 70
!
!     ADDITION
   01 IF (M .LT. 0) GOTO 50
      IF (M2 .LT. 0) GOTO 52
      IF (M .NE. M2) CALL ML_ERROR(8)
      IF (ERR .GT. 0) RETURN
      IF (N .NE. N2) CALL ML_ERROR(8)
      IF (ERR .GT. 0) RETURN
      CALL ML_WAXPY(M*N,1.0D0,0.0D0,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      GOTO 99
!
!     SUBTRACTION
   03 IF (M .LT. 0) GOTO 54
      IF (M2 .LT. 0) GOTO 56
      IF (M .NE. M2) CALL ML_ERROR(9)
      IF (ERR .GT. 0) RETURN
      IF (N .NE. N2) CALL ML_ERROR(9)
      IF (ERR .GT. 0) RETURN
      CALL ML_WAXPY(M*N,-1.0D0,0.0D0,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      GOTO 99
!
!     MULTIPLICATION
   05 IF (M2*M2*N2 .EQ. 1) GOTO 10
      IF (M*N .EQ. 1) GOTO 11
      IF (M2*N2 .EQ. 1) GOTO 10
      IF (N .NE. M2) CALL ML_ERROR(10)
      IF (ERR .GT. 0) RETURN
      MN = M*N2
      LL = L + MN
      ERR = LL+M*N+M2*N2 - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WCOPY(M*N+M2*N2,STKR(L),STKI(L),-1,STKR(LL),STKI(LL),-1)
      DO 08 J = 1, N2
      DO 08 I = 1, M
        K1 = L + MN + (I-1)
        K2 = L2 + MN + (J-1)*M2
        K = L + (I-1) + (J-1)*M
        STKR(K) = ML_WDOTUR(N,STKR(K1),STKI(K1),M,STKR(K2),STKI(K2),1)
        STKI(K) = ML_WDOTUI(N,STKR(K1),STKI(K1),M,STKR(K2),STKI(K2),1)
   08 CONTINUE
      NSTK(TOP) = N2
      GOTO 99
!
!     MULTIPLICATION BY SCALAR
   10 SR = STKR(L2)
      SI = STKI(L2)
      L1 = L
      GOTO 13
   11 SR = STKR(L)
      SI = STKI(L)
      L1 = L+1
      MSTK(TOP) = M2
      NSTK(TOP) = N2
   13 MN = MSTK(TOP)*NSTK(TOP)
      CALL ML_WSCAL(MN,SR,SI,STKR(L1),STKI(L1),1)
      IF (L1.NE.L) CALL ML_WCOPY(MN,STKR(L1),STKI(L1),1,STKR(L),STKI(L),1)
      GOTO 99
!
!     RIGHT DIVISION
   20 IF (M2*N2 .EQ. 1) GOTO 21
      IF (M2 .EQ. N2) FUN = 1
      IF (M2 .NE. N2) FUN = 4
      FIN = -1
      RHS = 2
      GOTO 99
   21 SR = STKR(L2)
      SI = STKI(L2)
      MN = M*N
      DO 22 I = 1, MN
         LL = L+I-1
         CALL ML_WDIV(STKR(LL),STKI(LL),SR,SI,STKR(LL),STKI(LL))
         IF (ERR .GT. 0) RETURN
   22 CONTINUE
      GOTO 99
!
!     LEFT DIVISION
   25 IF (M*N .EQ. 1) GOTO 26
      IF (M .EQ. N) FUN = 1
      IF (M .NE. N) FUN = 4
      FIN = -2
      RHS = 2
      GOTO 99
   26 SR = STKR(L)
      SI = STKI(L)
      MSTK(TOP) = M2
      NSTK(TOP) = N2
      MN = M2*N2
      DO 27 I = 1, MN
         LL = L+I-1
         CALL ML_WDIV(STKR(LL+1),STKI(LL+1),SR,SI,STKR(LL),STKI(LL))
         IF (ERR .GT. 0) RETURN
   27 CONTINUE
      GOTO 99
!
!     POWER
   30 IF (M2*N2 .NE. 1) CALL ML_ERROR(30)
      IF (ERR .GT. 0) RETURN
      IF (M .NE. N) CALL ML_ERROR(20)
      IF (ERR .GT. 0) RETURN
      NEXP = IDINT(STKR(L2))
      IF (STKR(L2) .NE. DFLOAT(NEXP)) GOTO 39
      IF (STKI(L2) .NE. 0.0D0) GOTO 39
      IF (NEXP .LT. 2) GOTO 39
      MN = M*N
      ERR = L2+MN+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WCOPY(MN,STKR(L),STKI(L),1,STKR(L2),STKI(L2),1)
      L3 = L2+MN
      DO 36 KEXP = 2, NEXP
       DO 35 J = 1, N
         LS = L+(J-1)*N
         CALL ML_WCOPY(N,STKR(LS),STKI(LS),1,STKR(L3),STKI(L3),1)
         DO 34 I = 1, N
          LS = L2+I-1
          LL = L+I-1+(J-1)*N
          STKR(LL)=ML_WDOTUR(N,STKR(LS),STKI(LS),N,STKR(L3),STKI(L3),1)
          STKI(LL)=ML_WDOTUI(N,STKR(LS),STKI(LS),N,STKR(L3),STKI(L3),1)
   34    CONTINUE
   35  CONTINUE
   36 CONTINUE
      GOTO 99
!
!     NONINTEGER OR NONPOSITIVE POWER, USE EIGENVECTORS
   39 FUN = 2
      FIN = 0
      GOTO 99
!
!     ADD OR SUBTRACT SCALAR
   50 IF (M2 .NE. N2) CALL ML_ERROR(8)
      IF (ERR .GT. 0) RETURN
      M = M2
      N = N2
      MSTK(TOP) = M
      NSTK(TOP) = N
      SR = STKR(L)
      SI = STKI(L)
      CALL ML_WCOPY(M*N,STKR(L+1),STKI(L+1),1,STKR(L),STKI(L),1)
      GOTO 58
   52 IF (M .NE. N) CALL ML_ERROR(8)
      IF (ERR .GT. 0) RETURN
      SR = STKR(L2)
      SI = STKI(L2)
      GOTO 58
   54 IF (M2 .NE. N2) CALL ML_ERROR(9)
      IF (ERR .GT. 0) RETURN
      M = M2
      N = N2
      MSTK(TOP) = M
      NSTK(TOP) = N
      SR = STKR(L)
      SI = STKI(L)
      CALL ML_WCOPY(M*N,STKR(L+1),STKI(L+1),1,STKR(L),STKI(L),1)
      CALL ML_WRSCAL(M*N,-1.0D0,STKR(L),STKI(L),1)
      GOTO 58
   56 IF (M .NE. N) CALL ML_ERROR(9)
      IF (ERR .GT. 0) RETURN
      SR = -STKR(L2)
      SI = -STKI(L2)
      GOTO 58
   58 DO 59 I = 1, N
         LL = L + (I-1)*(N+1)
         STKR(LL) = ML_FLOP(STKR(LL)+SR)
         STKI(LL) = ML_FLOP(STKI(LL)+SI)
   59 CONTINUE
      GOTO 99
!
!     COLON
   60 E2 = STKR(L2)
      ST = 1.0D0
      N = 0
      IF (RHS .LT. 3) GOTO 61
      ST = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      IF (ST .EQ. 0.0D0) GOTO 63
   61 E1 = STKR(L)
!     CHECK FOR CLAUSE
      IF (RSTK(PT) .EQ. 3) GOTO 64
      ERR = L + MAX0(3,IDINT((E2-E1)/ST)) - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
   62 IF (ST .GT. 0.0D0 .AND. STKR(L) .GT. E2) GOTO 63
      IF (ST .LT. 0.0D0 .AND. STKR(L) .LT. E2) GOTO 63
        N = N+1
        L = L+1
        STKR(L) = E1 + DFLOAT(N)*ST
        STKI(L) = 0.0D0
        GOTO 62
   63 NSTK(TOP) = N
      MSTK(TOP) = 1
      IF (N .EQ. 0) MSTK(TOP) = 0
      GOTO 99
!
!     FOR CLAUSE
   64 STKR(L) = E1
      STKR(L+1) = ST
      STKR(L+2) = E2
      MSTK(TOP) = -3
      NSTK(TOP) = -1
      GOTO 99
!
!     ELEMENTWISE OPERATIONS
   70 OP = OP - DOT
      IF (M.NE.M2 .OR. N.NE.N2) CALL ML_ERROR(10)
      IF (ERR .GT. 0) RETURN
      MN = M*N
      DO 72 I = 1, MN
         J = L+I-1
         K = L2+I-1
         IF (OP .EQ. STAR)CALL ML_WMUL(STKR(J),STKI(J),STKR(K),STKI(K),STKR(J),STKI(J))
         IF (OP .EQ. SLASH)CALL ML_WDIV(STKR(J),STKI(J),STKR(K),STKI(K),STKR(J),STKI(J))
         IF (OP .EQ. BSLASH)CALL ML_WDIV(STKR(K),STKI(K),STKR(J),STKI(J),STKR(J),STKI(J))
         IF (ERR .GT. 0) RETURN
   72 CONTINUE
      GOTO 99
!
!     KRONECKER
   80 FIN = OP - 2*DOT - STAR + 11
      FUN = 6
      TOP = TOP + 1
      RHS = 2
      GOTO 99
!
   99 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_STACKG(ID)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      INTEGER ID(4)
!
!     GET VARIABLES FROM STORAGE
!

      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      LOGICAL ML_EQID
      IF (DDT .EQ. 1)then
         call journal('sc','STACKG(1)=',ID(1))
         call journal('sc','STACKG(2)=',ID(2))
         call journal('sc','STACKG(3)=',ID(3))
         call journal('sc','STACKG(4)=',ID(4))
      endif
      CALL ML_PUTID(IDSTK(1,BOT-1), ID)
      K = LSIZE+1
   10 K = K-1
      IF (.NOT.ML_EQID(IDSTK(1,K), ID)) GOTO 10
      IF (K .GE. LSIZE-1 .AND. RHS .GT. 0) GOTO 98
      IF (K .EQ. BOT-1) GOTO 98
      LK = LSTK(K)
      IF (RHS .EQ. 1) GOTO 40
      IF (RHS .EQ. 2) GOTO 60
      IF (RHS .GT. 2) CALL ML_ERROR(21)
      IF (ERR .GT. 0) RETURN
      L = 1
      IF (TOP .GT. 0) L = LSTK(TOP) + MSTK(TOP)*NSTK(TOP)
      IF (TOP+1 .GE. BOT) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
!
!     LOAD VARIABLE TO TOP OF STACK
      LSTK(TOP) = L
      MSTK(TOP) = MSTK(K)
      NSTK(TOP) = NSTK(K)
      MN = MSTK(K)*NSTK(K)
      ERR = L+MN - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
!     IF RAND, MATFN6 GENERATES RANDOM NUMBER
      IF (K .EQ. LSIZE) GOTO 97
      CALL ML_WCOPY(MN,STKR(LK),STKI(LK),1,STKR(L),STKI(L),1)
      GOTO 99
!
!     VECT(ARG)
   40 IF (MSTK(TOP) .EQ. 0) GOTO 99
      L = LSTK(TOP)
      MN = MSTK(TOP)*NSTK(TOP)
      MNK = MSTK(K)*NSTK(K)
      IF (MSTK(TOP) .LT. 0) MN = MNK
      DO 50 I = 1, MN
        LL = L+I-1
        LS = LK+I-1
        IF (MSTK(TOP) .GT. 0) LS = LK + IDINT(STKR(LL)) - 1
        IF (LS .LT. LK .OR. LS .GE. LK+MNK) CALL ML_ERROR(21)
        IF (ERR .GT. 0) RETURN
        STKR(LL) = STKR(LS)
        STKI(LL) = STKI(LS)
   50 CONTINUE
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      IF (MSTK(K) .GT. 1) MSTK(TOP) = MN
      IF (MSTK(K) .EQ. 1) NSTK(TOP) = MN
      GOTO 99
!
!     MATRIX(ARG,ARG)
   60 TOP = TOP-1
      L = LSTK(TOP)
      IF (MSTK(TOP+1) .EQ. 0) MSTK(TOP) = 0
      IF (MSTK(TOP) .EQ. 0) GOTO 99
      L2 = LSTK(TOP+1)
      M = MSTK(TOP)*NSTK(TOP)
      IF (MSTK(TOP) .LT. 0) M = MSTK(K)
      N = MSTK(TOP+1)*NSTK(TOP+1)
      IF (MSTK(TOP+1) .LT. 0) N = NSTK(K)
      L3 = L2 + N
      MK = MSTK(K)
      MNK = MSTK(K)*NSTK(K)
      DO 70 J = 1, N
      DO 70 I = 1, M
        LI = L+I-1
        IF (MSTK(TOP) .GT. 0) LI = L + IDINT(STKR(LI)) - 1
        LJ = L2+J-1
        IF (MSTK(TOP+1) .GT. 0) LJ = L2 + IDINT(STKR(LJ)) - 1
        LS = LK + LI-L + (LJ-L2)*MK
        IF (LS.LT.LK .OR. LS.GE.LK+MNK) CALL ML_ERROR(21)
        IF (ERR .GT. 0) RETURN
        LL = L3 + I-1 + (J-1)*M
        STKR(LL) = STKR(LS)
        STKI(LL) = STKI(LS)
   70 CONTINUE
      MN = M*N
      CALL ML_WCOPY(MN,STKR(L3),STKI(L3),1,STKR(L),STKI(L),1)
      MSTK(TOP) = M
      NSTK(TOP) = N
      GOTO 99
   97 FIN = 7
      FUN = 6
      RETURN
   98 FIN = 0
      RETURN
   99 FIN = -1
      FUN = 0
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_STACKP(ID)
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)
      INTEGER ID(4)
      character*100 mline
!
!     PUT VARIABLES INTO STORAGE
!
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      LOGICAL ML_EQID
      INTEGER SEMI
      SAVE SEMI
      DATA SEMI/39/
      IF (DDT .EQ. 1) THEN
          WRITE(MLINE,'('' STACKP '',4I4)') ID
          call journal(MLINE)
      ENDIF
      IF (TOP .LE. 0) CALL ML_ERROR(1)
      IF (ERR .GT. 0) RETURN
      CALL ML_FUNS(ID)
      IF (FIN .NE. 0) CALL ML_ERROR(25)
      IF (ERR .GT. 0) RETURN
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (M .GT. 0) L = LSTK(TOP)
      IF (M .LT. 0) CALL ML_ERROR(14)
      IF (ERR .GT. 0) RETURN
      IF (M .EQ. 0 .AND. N .NE. 0) GOTO 99
      MN = M*N
      LK = 0
      MK = 1
      NK = 0
      LT = 0
      MT = 0
      NT = 0
!
!     DOES VARIABLE ALREADY EXIST
      CALL ML_PUTID(IDSTK(1,BOT-1),ID)
      K = LSIZE+1
   05 K = K-1
      IF (.NOT.ML_EQID(IDSTK(1,K),ID)) GOTO 05
      IF (K .EQ. BOT-1) GOTO 30
      LK = LSTK(K)
      MK = MSTK(K)
      NK = NSTK(K)
      MNK = MK*NK
      IF (RHS .EQ. 0) GOTO 20
      IF (RHS .GT. 2) CALL ML_ERROR(15)
      IF (ERR .GT. 0) RETURN
      MT = MK
      NT = NK
      LT = L + MN
      ERR = LT + MNK - LSTK(BOT)
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      CALL ML_WCOPY(MNK,STKR(LK),STKI(LK),1,STKR(LT),STKI(LT),1)
!
!     DOES IT FIT
   20 IF (RHS.EQ.0 .AND. MN.EQ.MNK) GOTO 40
      IF (K .GE. LSIZE-3) CALL ML_ERROR(13)
      IF (ERR .GT. 0) RETURN
!
!     SHIFT STORAGE
      IF (K .EQ. BOT) GOTO 25
      LS = LSTK(BOT)
      LL = LS + MNK
      CALL ML_WCOPY(LK-LS,STKR(LS),STKI(LS),-1,STKR(LL),STKI(LL),-1)
      KM1 = K-1
      DO 24 IB = BOT, KM1
        I = BOT+KM1-IB
        CALL ML_PUTID(IDSTK(1,I+1),IDSTK(1,I))
        MSTK(I+1) = MSTK(I)
        NSTK(I+1) = NSTK(I)
        LSTK(I+1) = LSTK(I)+MNK
   24 CONTINUE
!
!     DESTROY OLD VARIABLE
   25 BOT = BOT+1
!
!     CREATE NEW VARIABLE
   30 IF (MN .EQ. 0) GOTO 99
      IF (BOT-2 .LE. TOP) CALL ML_ERROR(18)
      IF (ERR .GT. 0) RETURN
      K = BOT-1
      CALL ML_PUTID(IDSTK(1,K), ID)
      IF (RHS .EQ. 1) GOTO 50
      IF (RHS .EQ. 2) GOTO 55
!
!     STORE
   40 IF (K .LT. LSIZE) LSTK(K) = LSTK(K+1) - MN
      MSTK(K) = M
      NSTK(K) = N
      LK = LSTK(K)
      CALL ML_WCOPY(MN,STKR(L),STKI(L),-1,STKR(LK),STKI(LK),-1)
      GOTO 90
!
!     VECT(ARG)
   50 IF (MSTK(TOP-1) .LT. 0) GOTO 59
      MN1 = 1
      MN2 = 1
      L1 = 0
      L2 = 0
      IF (N.NE.1 .OR. NK.NE.1) GOTO 52
      L1 = LSTK(TOP-1)
      M1 = MSTK(TOP-1)
      MN1 = M1*NSTK(TOP-1)
      M2 = -1
      GOTO 60
   52 IF (M.NE.1 .OR. MK.NE.1) CALL ML_ERROR(15)
      IF (ERR .GT. 0) RETURN
      L2 = LSTK(TOP-1)
      M2 = MSTK(TOP-1)
      MN2 = M2*NSTK(TOP-1)
      M1 = -1
      GOTO 60
!
!     MATRIX(ARG,ARG)
   55 IF (MSTK(TOP-1).LT.0 .AND. MSTK(TOP-2).LT.0) GOTO 59
      L2 = LSTK(TOP-1)
      M2 = MSTK(TOP-1)
      MN2 = M2*NSTK(TOP-1)
      IF (M2 .LT. 0) MN2 = N
      L1 = LSTK(TOP-2)
      M1 = MSTK(TOP-2)
      MN1 = M1*NSTK(TOP-2)
      IF (M1 .LT. 0) MN1 = M
      GOTO 60
!
   59 IF (MN .NE. MNK) CALL ML_ERROR(15)
      IF (ERR .GT. 0) RETURN
      LK = LSTK(K)
      CALL ML_WCOPY(MN,STKR(L),STKI(L),-1,STKR(LK),STKI(LK),-1)
      GOTO 90
!
   60 IF (MN1.NE.M .OR. MN2.NE.N) CALL ML_ERROR(15)
      IF (ERR .GT. 0) RETURN
      LL = 1
      IF (M1 .LT. 0) GOTO 62
      DO 61 I = 1, MN1
         LS = L1+I-1
         MK = MAX0(MK,IDINT(STKR(LS)))
         LL = MIN0(LL,IDINT(STKR(LS)))
   61 CONTINUE
   62 MK = MAX0(MK,M)
      IF (M2 .LT. 0) GOTO 64
      DO 63 I = 1, MN2
         LS = L2+I-1
         NK = MAX0(NK,IDINT(STKR(LS)))
         LL = MIN0(LL,IDINT(STKR(LS)))
   63 CONTINUE
   64 NK = MAX0(NK,N)
      IF (LL .LT. 1) CALL ML_ERROR(21)
      IF (ERR .GT. 0) RETURN
      MNK = MK*NK
      LK = LSTK(K+1) - MNK
      ERR = LT + MT*NT - LK
      IF (ERR .GT. 0) CALL ML_ERROR(17)
      IF (ERR .GT. 0) RETURN
      LSTK(K) = LK
      MSTK(K) = MK
      NSTK(K) = NK
      CALL ML_WSET(MNK,0.0D0,0.0D0,STKR(LK),STKI(LK),1)
      IF (NT .LT. 1) GOTO 67
      DO 66 J = 1, NT
         LS = LT+(J-1)*MT
         LL = LK+(J-1)*MK
         CALL ML_WCOPY(MT,STKR(LS),STKI(LS),-1,STKR(LL),STKI(LL),-1)
   66 CONTINUE
   67 DO 68 J = 1, N
      DO 68 I = 1, M
        LI = L1+I-1
        IF (M1 .GT. 0) LI = L1 + IDINT(STKR(LI)) - 1
        LJ = L2+J-1
        IF (M2 .GT. 0) LJ = L2 + IDINT(STKR(LJ)) - 1
        LL = LK+LI-L1+(LJ-L2)*MK
        LS = L+I-1+(J-1)*M
        STKR(LL) = STKR(LS)
        STKI(LL) = STKI(LS)
   68 CONTINUE
      GOTO 90
!
!     PRINT IF DESIRED AND POP STACK
   90 IF (SYM.NE.SEMI .AND. LCT(3).EQ.0) CALL ML_PRINT(ID,K)
      IF (SYM.EQ.SEMI .AND. LCT(3).EQ.1) CALL ML_PRINT(ID,K)
      IF (K .EQ. BOT-1) BOT = BOT-1
   99 IF (M .NE. 0) TOP = TOP - 1 - RHS
      IF (M .EQ. 0) TOP = TOP - 1
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_TERM
      use M_journal, only : journal
       integer BIGMEM
       parameter(BIGMEM=200005)

      character mline*(256)
      DOUBLE PRECISION STKR(BIGMEM),STKI(BIGMEM)
      INTEGER IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
      INTEGER IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /VSTK/ STKR,STKI,IDSTK,LSTK,MSTK,NSTK,VSIZE,LSIZE,BOT,TOP
      COMMON /RECU/ IDS,PSTK,RSTK,PSIZE,PT,PTZ
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN

      INTEGER R,OP,BSLASH,STAR,SLASH,DOT
      save bslash,star,slash,dot
      DATA BSLASH/45/,STAR/43/,SLASH/44/,DOT/47/
      IF (DDT .EQ. 1) then
         mline='TERM '
         call appnum(real(pt),mline,ilen,ierr)
         call appnum(real(rstk(pt)),mline,ilen,ierr)
         call journal(mline)
      endif
      R = RSTK(PT)
      GOTO (99,99,99,99,99,01,01,05,25,99,99,99,99,99,35,99,99,99,99),R
   01 PT = PT+1
      RSTK(PT) = 8
!     *CALL* FACTOR
      RETURN
!.......................................................................
   05 CONTINUE
      PT = PT-1
   10 CONTINUE
      OP = 0
      IF (SYM .EQ. DOT) OP = DOT
      IF (SYM .EQ. DOT) CALL ML_GETSYM
      IF (SYM.EQ.STAR .OR. SYM.EQ.SLASH .OR. SYM.EQ.BSLASH) GOTO 20
      RETURN
!.......................................................................
   20 CONTINUE
      OP = OP + SYM
      CALL ML_GETSYM()
      IF (SYM .EQ. DOT) OP = OP + SYM
      IF (SYM .EQ. DOT) CALL ML_GETSYM()
      PT = PT+1
      PSTK(PT) = OP
      RSTK(PT) = 9
!     *CALL* FACTOR
      RETURN
!.......................................................................
   25 CONTINUE
      OP = PSTK(PT)
      PT = PT-1
      CALL ML_STACK2(OP)
      IF (ERR .GT. 0) RETURN
!     SOME BINARY OPS DONE IN MATFNS
      IF (FUN .EQ. 0) GOTO 10
      PT = PT+1
      RSTK(PT) = 15
!     *CALL* MATFN
      RETURN
!.......................................................................
   35 CONTINUE
      PT = PT-1
      GOTO 10
!.......................................................................
   99 CONTINUE
      CALL ML_ERROR(22)
      IF (ERR .GT. 0) RETURN
      RETURN
!.......................................................................
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WGECO(AR,AI,LDA,N,IPVT,RCOND,ZR,ZI)
      INTEGER LDA,N,IPVT(*)
      DOUBLE PRECISION AR(LDA,*),AI(LDA,*),ZR(*),ZI(*)
      DOUBLE PRECISION RCOND
!
!     WGECO FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION
!     AND ESTIMATES THE CONDITION OF THE MATRIX.
!
!     IF  RCOND  IS NOT NEEDED, WGEFA IS SLIGHTLY FASTER.
!     TO SOLVE  A*X = B , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  INVERSE(A)*C , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  DETERMINANT(A) , FOLLOW WGECO BY WGEDI.
!     TO COMPUTE  INVERSE(A) , FOLLOW WGECO BY WGEDI.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        RCOND   DOUBLE PRECISION
!                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
!                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
!                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
!                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
!                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
!        1.0 + RCOND .EQ. 1.0
!                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
!                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
!                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
!                UNDERFLOWS.
!
!        Z       DOUBLE-COMPLEX(N)
!                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
!                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
!                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
!                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     LINPACK WGEFA
!     BLAS WAXPY,WDOTC,ML_WASUM
!     FORTRAN DABS,DMAX1
!
!     INTERNAL VARIABLES
!
      DOUBLE PRECISION EKR,EKI,TR,TI,WKR,WKI,WKMR,WKMI
      DOUBLE PRECISION ML_WDOTCR,ML_WDOTCI
      DOUBLE PRECISION ANORM,S,ML_WASUM,SM,YNORM,ML_FLOP
      INTEGER INFO,J,K,KB,KP1,L
!
      DOUBLE PRECISION ZDUMR,ZDUMI
      DOUBLE PRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE 1-NORM OF A
!
      ANORM = 0.0D0
      DO 10 J = 1, N
         ANORM = DMAX1(ANORM,ML_WASUM(N,AR(1,J),AI(1,J),1))
   10 CONTINUE
!
!     FACTOR
!
      CALL ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
!
!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E .
!     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A .
!     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
!     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E .
!     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
!
!     SOLVE CTRANS(U)*W = E
!
      EKR = 1.0D0
      EKI = 0.0D0
      DO 20 J = 1, N
         ZR(J) = 0.0D0
         ZI(J) = 0.0D0
   20 CONTINUE
      DO 110 K = 1, N
         CALL ML_WSIGN(EKR,EKI,-ZR(K),-ZI(K),EKR,EKI)
         IF (CABS1(EKR-ZR(K),EKI-ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 40
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(EKR-ZR(K),EKI-ZI(K))
            CALL ML_WRSCAL(N,S,ZR,ZI,1)
            EKR = S*EKR
            EKI = S*EKI
   40    CONTINUE
         WKR = EKR - ZR(K)
         WKI = EKI - ZI(K)
         WKMR = -EKR - ZR(K)
         WKMI = -EKI - ZI(K)
         S = CABS1(WKR,WKI)
         SM = CABS1(WKMR,WKMI)
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 50
            CALL ML_WDIV(WKR,WKI,AR(K,K),-AI(K,K),WKR,WKI)
            CALL ML_WDIV(WKMR,WKMI,AR(K,K),-AI(K,K),WKMR,WKMI)
         GOTO 60
   50    CONTINUE
            WKR = 1.0D0
            WKI = 0.0D0
            WKMR = 1.0D0
            WKMI = 0.0D0
   60    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GOTO 100
            DO 70 J = KP1, N
               CALL ML_WMUL(WKMR,WKMI,AR(K,J),-AI(K,J),TR,TI)
               SM = ML_FLOP(SM + CABS1(ZR(J)+TR,ZI(J)+TI))
               CALL ML_WAXPY(1,WKR,WKI,AR(K,J),-AI(K,J),1,ZR(J),ZI(J),1)
               S = ML_FLOP(S + CABS1(ZR(J),ZI(J)))
   70       CONTINUE
            IF (S .GE. SM) GOTO 90
               TR = WKMR - WKR
               TI = WKMI - WKI
               WKR = WKMR
               WKI = WKMI
               DO 80 J = KP1, N
                  CALL ML_WAXPY(1,TR,TI,AR(K,J),-AI(K,J),1,ZR(J),ZI(J),1)
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
         ZR(K) = WKR
         ZI(K) = WKI
  110 CONTINUE
      S = 1.0D0/ML_WASUM(N,ZR,ZI,1)
      CALL ML_WRSCAL(N,S,ZR,ZI,1)
!
!     SOLVE CTRANS(L)*Y = W
!
      DO 140 KB = 1, N
         K = N + 1 - KB
         IF (K .GE. N) GOTO 120
            ZR(K) = ZR(K) + ML_WDOTCR(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
            ZI(K) = ZI(K) + ML_WDOTCI(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
  120    CONTINUE
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) GOTO 130
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL ML_WRSCAL(N,S,ZR,ZI,1)
  130    CONTINUE
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
  140 CONTINUE
      S = 1.0D0/ML_WASUM(N,ZR,ZI,1)
      CALL ML_WRSCAL(N,S,ZR,ZI,1)
!
      YNORM = 1.0D0
!
!     SOLVE L*V = Y
!
      DO 160 K = 1, N
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
         IF (K .LT. N) CALL ML_WAXPY(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) GOTO 150
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL ML_WRSCAL(N,S,ZR,ZI,1)
            YNORM = S*YNORM
  150    CONTINUE
  160 CONTINUE
      S = 1.0D0/ML_WASUM(N,ZR,ZI,1)
      CALL ML_WRSCAL(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
!     SOLVE  U*Z = V
!
      DO 200 KB = 1, N
         K = N + 1 - KB
         IF (CABS1(ZR(K),ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 170
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(ZR(K),ZI(K))
            CALL ML_WRSCAL(N,S,ZR,ZI,1)
            YNORM = S*YNORM
  170    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 180
            CALL ML_WDIV(ZR(K),ZI(K),AR(K,K),AI(K,K),ZR(K),ZI(K))
  180    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .NE. 0.0D0) GOTO 190
            ZR(K) = 1.0D0
            ZI(K) = 0.0D0
  190    CONTINUE
         TR = -ZR(K)
         TI = -ZI(K)
         CALL ML_WAXPY(K-1,TR,TI,AR(1,K),AI(1,K),1,ZR(1),ZI(1),1)
  200 CONTINUE
!     MAKE ZNORM = 1.0
      S = 1.0D0/ML_WASUM(N,ZR,ZI,1)
      CALL ML_WRSCAL(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLE PRECISION AR(LDA,1),AI(LDA,1)
!
!     WGEFA FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION.
!
!     WGEFA IS USUALLY CALLED BY WGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR WGECO) = (1 + 9/N)*(TIME FOR WGEFA) .
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!  CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!  INDICATE THAT WGESL OR WGEDI WILL DIVIDE BY ZERO
!  IF CALLED.  USE  RCOND  IN WGECO FOR A RELIABLE
!  INDICATION OF SINGULARITY.
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WSCAL,ML_IWAMAX
!     FORTRAN DABS
!
!     INTERNAL VARIABLES
!
      DOUBLE PRECISION TR,TI
      INTEGER ML_IWAMAX,J,K,KP1,L,NM1
!
      DOUBLE PRECISION ZDUMR,ZDUMI
      DOUBLE PRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GOTO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
!
!        FIND L = PIVOT INDEX
!
         L = ML_IWAMAX(N-K+1,AR(K,K),AI(K,K),1) + K - 1
         IPVT(K) = L
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF (CABS1(AR(L,K),AI(L,K)) .EQ. 0.0D0) GOTO 40
!
!           INTERCHANGE IF NECESSARY
!
            IF (L .EQ. K) GOTO 10
               TR = AR(L,K)
               TI = AI(L,K)
               AR(L,K) = AR(K,K)
               AI(L,K) = AI(K,K)
               AR(K,K) = TR
               AI(K,K) = TI
   10       CONTINUE
!
!           COMPUTE MULTIPLIERS
!
            CALL ML_WDIV(-1.0D0,0.0D0,AR(K,K),AI(K,K),TR,TI)
            CALL ML_WSCAL(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO 30 J = KP1, N
               TR = AR(L,J)
               TI = AI(L,J)
               IF (L .EQ. K) GOTO 20
                  AR(L,J) = AR(K,J)
                  AI(L,J) = AI(K,J)
                  AR(K,J) = TR
                  AI(K,J) = TI
   20          CONTINUE
               CALL ML_WAXPY(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,AR(K+1,J),AI(K+1,J),1)
   30       CONTINUE
         GOTO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (CABS1(AR(N,N),AI(N,N)) .EQ. 0.0D0) INFO = N
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WGESL(AR,AI,LDA,N,IPVT,BR,BI,JOB)
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION AR(LDA,*),AI(LDA,*),BR(*),BI(*)
!
!     WGESL SOLVES THE DOUBLE-COMPLEX SYSTEM
!     A * X = B  OR  CTRANS(A) * X = B
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        B       DOUBLE-COMPLEX(N)
!                THE RIGHT HAND SIDE VECTOR.
!
!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  CTRANS(A)*X = B  WHERE
!         CTRANS(A)  IS THE CONJUGATE TRANSPOSE.
!
!     ON RETURN
!
!        B       THE SOLUTION VECTOR  X .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF WGECO HAS SET RCOND .GT. 0.0
!        OR WGEFA HAS SET INFO .EQ. 0 .
!
!     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL ML_WGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GOTO ...
!           DO 10 J = 1, P
!              CALL ML_WGESL(A,LDA,N,IPVT,C(1,J),0)
!        10 CONTINUE
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WDOTC
!
!     INTERNAL VARIABLES
!
      DOUBLE PRECISION ML_WDOTCR,ML_WDOTCI,TR,TI
      INTEGER K,KB,L,NM1
!
      NM1 = N - 1
      IF (JOB .NE. 0) GOTO 50
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
         IF (NM1 .LT. 1) GOTO 30
         DO 20 K = 1, NM1
           L = IPVT(K)
           TR = BR(L)
           TI = BI(L)
           IF (L .EQ. K) GOTO 10
              BR(L) = BR(K)
              BI(L) = BI(K)
              BR(K) = TR
              BI(K) = TI
   10     CONTINUE
         CALL ML_WAXPY(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
   20    CONTINUE
   30    CONTINUE
!
!        NOW SOLVE  U*X = Y
!
         DO 40 KB = 1, N
            K = N + 1 - KB
            CALL ML_WDIV(BR(K),BI(K),AR(K,K),AI(K,K),BR(K),BI(K))
            TR = -BR(K)
            TI = -BI(K)
            CALL ML_WAXPY(K-1,TR,TI,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
   40    CONTINUE
      GOTO 100
   50 CONTINUE
!
!        JOB = NONZERO, SOLVE  CTRANS(A) * X = B
!        FIRST SOLVE  CTRANS(U)*Y = B
!
         DO 60 K = 1, N
            TR = BR(K) - ML_WDOTCR(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
            TI = BI(K) - ML_WDOTCI(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
            CALL ML_WDIV(TR,TI,AR(K,K),-AI(K,K),BR(K),BI(K))
   60    CONTINUE
!
!        NOW SOLVE CTRANS(L)*X = Y
!
         IF (NM1 .LT. 1) GOTO 90
         DO 80 KB = 1, NM1
            K = N - KB
            BR(K) = BR(K) + ML_WDOTCR(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
            BI(K) = BI(K) + ML_WDOTCI(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GOTO 70
               TR = BR(L)
               TI = BI(L)
               BR(L) = BR(K)
               BI(L) = BI(K)
               BR(K) = TR
               BI(K) = TI
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WGEDI(AR,AI,LDA,N,IPVT,DETR,DETI,WORKR,WORKI,JOB)
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION AR(LDA,*),AI(LDA,*),DETR(2),DETI(2),WORKR(*),WORKI(*)
!
!     WGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        WORK    DOUBLE-COMPLEX(N)
!                WORK VECTOR.  CONTENTS DESTROYED.
!
!        JOB     INTEGER
!                = 11   BOTH DETERMINANT AND INVERSE.
!                = 01   INVERSE ONLY.
!                = 10   DETERMINANT ONLY.
!
!     ON RETURN
!
!        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE UNCHANGED.
!
!        DET     DOUBLE-COMPLEX(2)
!                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE NOT REFERENCED.
!                DETERMINANT = DET(1) * 10.0**DET(2)
!                WITH  1.0 .LE. CABS1(DET(1) .LT. 10.0
!                OR  DET(1) .EQ. 0.0 .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
!        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
!        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
!        AND IF WGECO HAS SET RCOND .GT. 0.0 OR WGEFA HAS SET
!        INFO .EQ. 0 .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WSCAL,WSWAP
!     FORTRAN DABS,MOD
!
!     INTERNAL VARIABLES
!
      DOUBLE PRECISION TR,TI
      DOUBLE PRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
!
      DOUBLE PRECISION ZDUMR,ZDUMI
      DOUBLE PRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE DETERMINANT
!
      IF (JOB/10 .EQ. 0) GOTO 80
         DETR(1) = 1.0D0
         DETI(1) = 0.0D0
         DETR(2) = 0.0D0
         DETI(2) = 0.0D0
         TEN = 10.0D0
         DO 60 I = 1, N
           IF (IPVT(I) .EQ. I) GOTO 10
              DETR(1) = -DETR(1)
              DETI(1) = -DETI(1)
   10      CONTINUE
           CALL ML_WMUL(AR(I,I),AI(I,I),DETR(1),DETI(1),DETR(1),DETI(1))
!          ...EXIT
!       ...EXIT
           IF (CABS1(DETR(1),DETI(1)) .EQ. 0.0D0) GOTO 70
   20      IF (CABS1(DETR(1),DETI(1)) .GE. 1.0D0) GOTO 30
              DETR(1) = TEN*DETR(1)
              DETI(1) = TEN*DETI(1)
              DETR(2) = DETR(2) - 1.0D0
              DETI(2) = DETI(2) - 0.0D0
           GOTO 20
   30      CONTINUE
   40      IF (CABS1(DETR(1),DETI(1)) .LT. TEN) GOTO 50
              DETR(1) = DETR(1)/TEN
              DETI(1) = DETI(1)/TEN
              DETR(2) = DETR(2) + 1.0D0
              DETI(2) = DETI(2) + 0.0D0
           GOTO 40
   50      CONTINUE
   60    CONTINUE
   70    CONTINUE
   80 CONTINUE
!
!     COMPUTE INVERSE(U)
!
      IF (MOD(JOB,10) .EQ. 0) GOTO 160
         DO 110 K = 1, N
            CALL ML_WDIV(1.0D0,0.0D0,AR(K,K),AI(K,K),AR(K,K),AI(K,K))
            TR = -AR(K,K)
            TI = -AI(K,K)
            CALL ML_WSCAL(K-1,TR,TI,AR(1,K),AI(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GOTO 100
            DO 90 J = KP1, N
              TR = AR(K,J)
              TI = AI(K,J)
              AR(K,J) = 0.0D0
              AI(K,J) = 0.0D0
              CALL ML_WAXPY(K,TR,TI,AR(1,K),AI(1,K),1,AR(1,J),AI(1,J),1)
   90       CONTINUE
  100       CONTINUE
  110    CONTINUE
!
!        FORM INVERSE(U)*INVERSE(L)
!
         NM1 = N - 1
         IF (NM1 .LT. 1) GOTO 150
         DO 140 KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO 120 I = KP1, N
               WORKR(I) = AR(I,K)
               WORKI(I) = AI(I,K)
               AR(I,K) = 0.0D0
               AI(I,K) = 0.0D0
  120       CONTINUE
            DO 130 J = KP1, N
              TR = WORKR(J)
              TI = WORKI(J)
              CALL ML_WAXPY(N,TR,TI,AR(1,J),AI(1,J),1,AR(1,K),AI(1,K),1)
  130       CONTINUE
            L = IPVT(K)
            IF (L .NE. K)CALL ML_WSWAP(N,AR(1,K),AI(1,K),1,AR(1,L),AI(1,L),1)
  140    CONTINUE
  150    CONTINUE
  160 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WPOFA(AR,AI,LDA,N,INFO)
      DOUBLE PRECISION AR(LDA,1),AI(LDA,1)
      DOUBLE PRECISION S,TR,TI,ML_WDOTCR,ML_WDOTCI
      DO 30 J = 1, N
         INFO = J
         S = 0.0D0
         JM1 = J-1
         IF (JM1 .LT. 1) GOTO 20
         DO 10 K = 1, JM1
           TR=AR(K,J)-ML_WDOTCR(K-1,AR(1,K),AI(1,K),1,AR(1,J),AI(1,J),1)
           TI=AI(K,J)-ML_WDOTCI(K-1,AR(1,K),AI(1,K),1,AR(1,J),AI(1,J),1)
           CALL ML_WDIV(TR,TI,AR(K,K),AI(K,K),TR,TI)
           AR(K,J) = TR
           AI(K,J) = TI
           S = S + TR*TR + TI*TI
   10    CONTINUE
   20    CONTINUE
         S = AR(J,J) - S
         IF (S.LE.0.0D0 .OR. AI(J,J).NE.0.0D0) GOTO 40
         AR(J,J) = DSQRT(S)
   30 CONTINUE
      INFO = 0
   40 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_RREF(AR,AI,LDA,M,N,EPS)
      DOUBLE PRECISION AR(LDA,1),AI(LDA,1),EPS,TOL,TR,TI,ML_WASUM
      TOL = 0.0D0
      DO 10 J = 1, N
         TOL = DMAX1(TOL,ML_WASUM(M,AR(1,J),AI(1,J),1))
   10 CONTINUE
      TOL = EPS*DFLOAT(2*MAX0(M,N))*TOL
      K = 1
      L = 1
   20 IF (K.GT.M .OR. L.GT.N) RETURN
      I = ML_IWAMAX(M-K+1,AR(K,L),AI(K,L),1) + K-1
      IF (DABS(AR(I,L))+DABS(AI(I,L)) .GT. TOL) GOTO 30
         CALL ML_WSET(M-K+1,0.0D0,0.0D0,AR(K,L),AI(K,L),1)
         L = L+1
         GOTO 20
   30 CALL ML_WSWAP(N-L+1,AR(I,L),AI(I,L),LDA,AR(K,L),AI(K,L),LDA)
      CALL ML_WDIV(1.0D0,0.0D0,AR(K,L),AI(K,L),TR,TI)
      CALL ML_WSCAL(N-L+1,TR,TI,AR(K,L),AI(K,L),LDA)
      AR(K,L) = 1.0D0
      AI(K,L) = 0.0D0
      DO 40 I = 1, M
         TR = -AR(I,L)
         TI = -AI(I,L)
         IF (I .NE. K) CALL ML_WAXPY(N-L+1,TR,TI,AR(K,L),AI(K,L),LDA,AR(I,L),AI(I,L),LDA)
   40 CONTINUE
      K = K+1
      L = L+1
      GOTO 20
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_HILBER(A,LDA,N)
      DOUBLE PRECISION A(LDA,N)
!     GENERATE INVERSE HILBERT MATRIX
      DOUBLE PRECISION P,R
      P = DFLOAT(N)
      DO 20 I = 1, N
        IF (I.NE.1) P = (DFLOAT(N-I+1)*P*DFLOAT(N+I-1))/DFLOAT(I-1)**2
        R = P*P
        A(I,I) = R/DFLOAT(2*I-1)
        IF (I.EQ.N) GOTO 20
        IP1 = I+1
        DO 10 J = IP1, N
          R = (-1)*(DFLOAT(N-J+1)*R*(N+J-1))/DFLOAT(J-1)**2
          A(I,J) = R/DFLOAT(I+J-1)
          A(J,I) = A(I,J)
   10   CONTINUE
   20 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_HTRIDI(NM,N,AR,AI,D,E,E2,TAU)
!
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLE PRECISION AR(NM,N),AI(NM,N),D(N),E(N),E2(N),TAU(2,N)
      DOUBLE PRECISION F,G,H,FI,GI,HH,SI,SCALE
      DOUBLE PRECISION ML_FLOP,ML_PYTHAG
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX
!     TO A REAL SYMMETRIC TRIDIAGONAL MATRIX USING
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX HERMITIAN INPUT MATRIX.
!          ONLY THE LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION IN THEIR FULL LOWER
!          TRIANGLES.  THEIR STRICT UPPER TRIANGLES AND THE
!          DIAGONAL OF AR ARE UNALTERED.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
!          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
!
!        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
!          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
      TAU(1,N) = 1.0D0
      TAU(2,N) = 0.0D0
!
      DO 100 I = 1, N
  100 D(I) = AR(I,I)
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0D0
         SCALE = 0.0D0
         IF (L .LT. 1) GOTO 130
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = ML_FLOP(SCALE + DABS(AR(I,K)) + DABS(AI(I,K)))
!
         IF (SCALE .NE. 0.0D0) GOTO 140
         TAU(1,L) = 1.0D0
         TAU(2,L) = 0.0D0
  130    E(I) = 0.0D0
         E2(I) = 0.0D0
         GOTO 290
!
  140    DO 150 K = 1, L
            AR(I,K) = ML_FLOP(AR(I,K)/SCALE)
            AI(I,K) = ML_FLOP(AI(I,K)/SCALE)
            H = ML_FLOP(H + AR(I,K)*AR(I,K) + AI(I,K)*AI(I,K))
  150    CONTINUE
!
         E2(I) = ML_FLOP(SCALE*SCALE*H)
         G = ML_FLOP(DSQRT(H))
         E(I) = ML_FLOP(SCALE*G)
         F = ML_PYTHAG(AR(I,L),AI(I,L))
!     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T ..........
         IF (F .EQ. 0.0D0) GOTO 160
         TAU(1,L) = ML_FLOP((AI(I,L)*TAU(2,I) - AR(I,L)*TAU(1,I))/F)
         SI = ML_FLOP((AR(I,L)*TAU(2,I) + AI(I,L)*TAU(1,I))/F)
         H = ML_FLOP(H + F*G)
         G = ML_FLOP(1.0D0 + G/F)
         AR(I,L) = ML_FLOP(G*AR(I,L))
         AI(I,L) = ML_FLOP(G*AI(I,L))
         IF (L .EQ. 1) GOTO 270
         GOTO 170
  160    TAU(1,L) = -TAU(1,I)
         SI = TAU(2,I)
         AR(I,L) = G
  170    F = 0.0D0
!
         DO 240 J = 1, L
            G = 0.0D0
            GI = 0.0D0
!     .......... FORM ELEMENT OF A*U ..........
            DO 180 K = 1, J
               G = ML_FLOP(G + AR(J,K)*AR(I,K) + AI(J,K)*AI(I,K))
               GI = ML_FLOP(GI - AR(J,K)*AI(I,K) + AI(J,K)*AR(I,K))
  180       CONTINUE
!
            JP1 = J + 1
            IF (L .LT. JP1) GOTO 220
!
            DO 200 K = JP1, L
               G = ML_FLOP(G + AR(K,J)*AR(I,K) - AI(K,J)*AI(I,K))
               GI = ML_FLOP(GI - AR(K,J)*AI(I,K) - AI(K,J)*AR(I,K))
  200       CONTINUE
!     .......... FORM ELEMENT OF P ..........
  220       E(J) = ML_FLOP(G/H)
            TAU(2,J) = ML_FLOP(GI/H)
            F = ML_FLOP(F + E(J)*AR(I,J) - TAU(2,J)*AI(I,J))
  240    CONTINUE
!
         HH = ML_FLOP(F/(H + H))
!     .......... FORM REDUCED A ..........
         DO 260 J = 1, L
            F = AR(I,J)
            G = ML_FLOP(E(J) - HH*F)
            E(J) = G
            FI = -AI(I,J)
            GI = ML_FLOP(TAU(2,J) - HH*FI)
            TAU(2,J) = -GI
!
            DO 260 K = 1, J
               AR(J,K) = ML_FLOP(AR(J,K) - F*E(K) - G*AR(I,K) + FI*TAU(2,K) + GI*AI(I,K))
               AI(J,K) = ML_FLOP(AI(J,K) - F*TAU(2,K) - G*AI(I,K) - FI*E(K) - GI*AR(I,K))
  260    CONTINUE
!
  270    DO 280 K = 1, L
            AR(I,K) = ML_FLOP(SCALE*AR(I,K))
            AI(I,K) = ML_FLOP(SCALE*AI(I,K))
  280    CONTINUE
!
         TAU(2,L) = -SI
  290    HH = D(I)
         D(I) = AR(I,I)
         AR(I,I) = HH
         AI(I,I) = ML_FLOP(SCALE*DSQRT(H))
  300 CONTINUE
!
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_HTRIBK(NM,N,AR,AI,TAU,M,ZR,ZI)
!
      INTEGER I,J,K,L,M,N,NM
      DOUBLE PRECISION AR(NM,N),AI(NM,N),TAU(2,N),ZR(NM,M),ZI(NM,M)
      DOUBLE PRECISION H,S,SI,ML_FLOP
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN
!     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
!     REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRIDI.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  HTRIDI  IN THEIR
!          FULL LOWER TRIANGLES EXCEPT FOR THE DIAGONAL OF AR.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED.
!
!        ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
!          IN ITS FIRST M COLUMNS.
!
!     ON OUTPUT.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS
!          IN THEIR FIRST M COLUMNS.
!
!     NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR
!     IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
      IF (M .EQ. 0) GOTO 200
!     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC
!                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN
!                TRIDIAGONAL MATRIX. ..........
      DO 50 K = 1, N
!
         DO 50 J = 1, M
            ZI(K,J) = ML_FLOP(-(ZR(K,J)*TAU(2,K)))
            ZR(K,J) = ML_FLOP(ZR(K,J)*TAU(1,K))
   50 CONTINUE
!
      IF (N .EQ. 1) GOTO 200
!     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES ..........
      DO 140 I = 2, N
         L = I - 1
         H = AI(I,I)
         IF (H .EQ. 0.0D0) GOTO 140
!
         DO 130 J = 1, M
            S = 0.0D0
            SI = 0.0D0
!
            DO 110 K = 1, L
               S = ML_FLOP(S + AR(I,K)*ZR(K,J) - AI(I,K)*ZI(K,J))
               SI = ML_FLOP(SI + AR(I,K)*ZI(K,J) + AI(I,K)*ZR(K,J))
  110       CONTINUE
!     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ..........
            S = ML_FLOP((S/H)/H)
            SI = ML_FLOP((SI/H)/H)
!
            DO 120 K = 1, L
               ZR(K,J) = ML_FLOP(ZR(K,J) - S*AR(I,K) - SI*AI(I,K))
               ZI(K,J) = ML_FLOP(ZI(K,J) - SI*AR(I,K) + S*AI(I,K))
  120       CONTINUE
!
  130    CONTINUE
!
  140 CONTINUE
!
  200 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_IMTQL2(NM,N,D,E,Z,IERR,JOB)
!
      INTEGER I,J,K,L,M,N,II,NM,MML,IERR
      DOUBLE PRECISION D(N),E(N),Z(NM,N)
      DOUBLE PRECISION B,C,F,G,P,R,S
      DOUBLE PRECISION ML_FLOP
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,
!     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
!     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
!     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
!     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
!     FULL MATRIX TO TRIDIAGONAL FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
!          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
!
!        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
!          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
!          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
!          THE IDENTITY MATRIX.
!
!      ON OUTPUT.
!
!        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
!          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
!          UNORDERED FOR INDICES 1,2,...,IERR-1.
!
!        E HAS BEEN DESTROYED.
!
!        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
!          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
!          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
!          EIGENVALUES.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER 30 ITERATIONS.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
!
!*****
!     MODIFIED BY C. MOLER TO ELIMINATE MACHEP 11/22/78
!     MODIFIED TO ADD JOB PARAMETER 08/27/79
!*****
      IERR = 0
      IF (N .EQ. 1) GOTO 1001
!
      DO 100 I = 2, N
  100 E(I-1) = E(I)
!
      E(N) = 0.0D0
!
      DO 240 L = 1, N
         J = 0
!     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (M .EQ. N) GOTO 120
!*****
            P = ML_FLOP(DABS(D(M)) + DABS(D(M+1)))
            S = ML_FLOP(P + DABS(E(M)))
            IF (P .EQ. S) GOTO 120
!*****
  110    CONTINUE
!
  120    P = D(L)
         IF (M .EQ. L) GOTO 240
         IF (J .EQ. 30) GOTO 1000
         J = J + 1
!     .......... FORM SHIFT ..........
         G = ML_FLOP((D(L+1) - P)/(2.0D0*E(L)))
         R = ML_FLOP(DSQRT(G*G+1.0D0))
         G = ML_FLOP(D(M) - P + E(L)/(G + DSIGN(R,G)))
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M - L
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = ML_FLOP(S*E(I))
            B = ML_FLOP(C*E(I))
            IF (DABS(F) .LT. DABS(G)) GOTO 150
            C = ML_FLOP(G/F)
            R = ML_FLOP(DSQRT(C*C+1.0D0))
            E(I+1) = ML_FLOP(F*R)
            S = ML_FLOP(1.0D0/R)
            C = ML_FLOP(C*S)
            GOTO 160
  150       S = ML_FLOP(F/G)
            R = ML_FLOP(DSQRT(S*S+1.0D0))
            E(I+1) = ML_FLOP(G*R)
            C = ML_FLOP(1.0D0/R)
            S = ML_FLOP(S*C)
  160       G = ML_FLOP(D(I+1) - P)
            R = ML_FLOP((D(I) - G)*S + 2.0D0*C*B)
            P = ML_FLOP(S*R)
            D(I+1) = G + P
            G = ML_FLOP(C*R - B)
            IF (JOB .EQ. 0) GOTO 185
!     .......... FORM VECTOR ..........
            DO 180 K = 1, N
               F = Z(K,I+1)
               Z(K,I+1) = ML_FLOP(S*Z(K,I) + C*F)
               Z(K,I) = ML_FLOP(C*Z(K,I) - S*F)
  180       CONTINUE
  185       CONTINUE
!
  200    CONTINUE
!
         D(L) = ML_FLOP(D(L) - P)
         E(L) = G
         E(M) = 0.0D0
         GOTO 105
  240 CONTINUE
!     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO 300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
!
         DO 260 J = II, N
            IF (D(J) .GE. P) GOTO 260
            K = J
            P = D(J)
  260    CONTINUE
!
         IF (K .EQ. I) GOTO 300
         D(K) = D(I)
         D(I) = P
!
         IF (JOB .EQ. 0) GOTO 285
         DO 280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
  285    CONTINUE
!
  300 CONTINUE
!
      GOTO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 CONTINUE
      IERR = L
 1001 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_CORTH(NM,N,LOW,IGH,AR,AI,ORTR,ORTI)
!
      INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
      DOUBLE PRECISION AR(NM,N),AI(NM,N),ORTR(IGH),ORTI(IGH)
      DOUBLE PRECISION F,G,H,FI,FR,SCALE
      DOUBLE PRECISION ML_FLOP,ML_PYTHAG
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968)
!     BY MARTIN AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
!
!     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE
!     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
!     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL.  IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE HESSENBERG MATRIX.  INFORMATION
!          ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION
!          IS STORED IN THE REMAINING TRIANGLES UNDER THE
!          HESSENBERG MATRIX.
!
!        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE
!          TRANSFORMATIONS.  ONLY ELEMENTS LOW THROUGH IGH ARE USED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GOTO 200
!
      DO 180 M = KP1, LA
         H = 0.0D0
         ORTR(M) = 0.0D0
         ORTI(M) = 0.0D0
         SCALE = 0.0D0
!     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
         DO 90 I = M, IGH
   90    SCALE = ML_FLOP(SCALE + DABS(AR(I,M-1)) + DABS(AI(I,M-1)))
!
         IF (SCALE .EQ. 0.0D0) GOTO 180
         MP = M + IGH
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
         DO 100 II = M, IGH
            I = MP - II
            ORTR(I) = ML_FLOP(AR(I,M-1)/SCALE)
            ORTI(I) = ML_FLOP(AI(I,M-1)/SCALE)
            H = ML_FLOP(H + ORTR(I)*ORTR(I) + ORTI(I)*ORTI(I))
  100    CONTINUE
!
         G = ML_FLOP(DSQRT(H))
         F = ML_PYTHAG(ORTR(M),ORTI(M))
         IF (F .EQ. 0.0D0) GOTO 103
         H = ML_FLOP(H + F*G)
         G = ML_FLOP(G/F)
         ORTR(M) = ML_FLOP((1.0D0 + G)*ORTR(M))
         ORTI(M) = ML_FLOP((1.0D0 + G)*ORTI(M))
         GOTO 105
!
  103    ORTR(M) = G
         AR(M,M-1) = SCALE
!     .......... FORM (I-(U*UT)/H)*A ..........
  105    DO 130 J = M, N
            FR = 0.0D0
            FI = 0.0D0
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
            DO 110 II = M, IGH
               I = MP - II
               FR = ML_FLOP(FR + ORTR(I)*AR(I,J) + ORTI(I)*AI(I,J))
               FI = ML_FLOP(FI + ORTR(I)*AI(I,J) - ORTI(I)*AR(I,J))
  110       CONTINUE
!
            FR = ML_FLOP(FR/H)
            FI = ML_FLOP(FI/H)
!
            DO 120 I = M, IGH
               AR(I,J) = ML_FLOP(AR(I,J) - FR*ORTR(I) + FI*ORTI(I))
               AI(I,J) = ML_FLOP(AI(I,J) - FR*ORTI(I) - FI*ORTR(I))
  120       CONTINUE
!
  130    CONTINUE
!     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
         DO 160 I = 1, IGH
            FR = 0.0D0
            FI = 0.0D0
!     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
            DO 140 JJ = M, IGH
               J = MP - JJ
               FR = ML_FLOP(FR + ORTR(J)*AR(I,J) - ORTI(J)*AI(I,J))
               FI = ML_FLOP(FI + ORTR(J)*AI(I,J) + ORTI(J)*AR(I,J))
  140       CONTINUE
!
            FR = ML_FLOP(FR/H)
            FI = ML_FLOP(FI/H)
!
            DO 150 J = M, IGH
               AR(I,J) = ML_FLOP(AR(I,J) - FR*ORTR(J) - FI*ORTI(J))
               AI(I,J) = ML_FLOP(AI(I,J) + FR*ORTI(J) - FI*ORTR(J))
  150       CONTINUE
!
  160    CONTINUE
!
         ORTR(M) = ML_FLOP(SCALE*ORTR(M))
         ORTI(M) = ML_FLOP(SCALE*ORTI(M))
         AR(M,M-1) = ML_FLOP(-(G*AR(M,M-1)))
         AI(M,M-1) = ML_FLOP(-(G*AI(M,M-1)))
  180 CONTINUE
!
  200 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_COMQR3(NM,N,LOW,IGH,ORTR,ORTI,HR,HI,WR,WI,ZR,ZI,IERR ,JOB)
!*****
!     MODIFICATION OF EISPACK COMQR2 TO ADD JOB PARAMETER
!     JOB = 0  OUTPUT H = SCHUR TRIANGULAR FORM, Z NOT USED
!         = 1  OUTPUT H = SCHUR FORM, Z = UNITARY SIMILARITY
!         = 2  SAME AS COMQR2
!         = 3  OUTPUT H = HESSENBERG FORM, Z = UNITARY SIMILARITY
!     ALSO ELIMINATE MACHEP
!     C. MOLER, 11/22/78 AND 09/14/80
!     OVERFLOW CONTROL IN EIGENVECTOR BACKSUBSTITUTION, 3/16/82
!*****
!
      INTEGER I,J,K,L,M,N,EN,II,JJ,LL,NM,NN,IGH,IP1,ITN,ITS,LOW,LP1,ENM1,IEND,IERR
      DOUBLE PRECISION HR(NM,N),HI(NM,N),WR(N),WI(N),ZR(NM,N),ZI(NM,N),ORTR(IGH),ORTI(IGH)
      DOUBLE PRECISION SI,SR,TI,TR,XI,XR,YI,YR,ZZI,ZZR,NORM
      DOUBLE PRECISION ML_FLOP,ML_PYTHAG
!
!     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE
!     ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS
!     AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
!     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS
!     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR
!     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX
!     CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE
!     THIS GENERAL MATRIX TO HESSENBERG FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL.  IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED.
!          ONLY ELEMENTS LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS
!          OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND
!          ORTI(J) TO 0.0D0 FOR THESE ELEMENTS.
!
!        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.
!          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER
!          INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE
!          REDUCTION BY  CORTH, IF PERFORMED.  IF THE EIGENVECTORS OF
!          THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE
!          ARBITRARY.
!
!     ON OUTPUT.
!
!        ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI
!          HAVE BEEN DESTROYED.
!
!        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR
!          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,...,N.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS
!          ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF
!          THE EIGENVECTORS HAS BEEN FOUND.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER A TOTAL OF 30*N ITERATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
      IERR = 0
!*****
      IF (JOB .EQ. 0) GOTO 150
!*****
!     .......... INITIALIZE EIGENVECTOR MATRIX ..........
      DO 100 I = 1, N
!
         DO 100 J = 1, N
            ZR(I,J) = 0.0D0
            ZI(I,J) = 0.0D0
            IF (I .EQ. J) ZR(I,J) = 1.0D0
  100 CONTINUE
!     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS
!                FROM THE INFORMATION LEFT BY CORTH ..........
      IEND = IGH - LOW - 1
      IF (IEND) 180, 150, 105
!     .......... FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........
  105 DO 140 II = 1, IEND
         I = IGH - II
         IF (ORTR(I) .EQ. 0.0D0 .AND. ORTI(I) .EQ. 0.0D0) GOTO 140
         IF (HR(I,I-1) .EQ. 0.0D0 .AND. HI(I,I-1) .EQ. 0.0D0) GOTO 140
!     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
         NORM = ML_FLOP(HR(I,I-1)*ORTR(I) + HI(I,I-1)*ORTI(I))
         IP1 = I + 1
!
         DO 110 K = IP1, IGH
            ORTR(K) = HR(K,I-1)
            ORTI(K) = HI(K,I-1)
  110    CONTINUE
!
         DO 130 J = I, IGH
            SR = 0.0D0
            SI = 0.0D0
!
            DO 115 K = I, IGH
               SR = ML_FLOP(SR + ORTR(K)*ZR(K,J) + ORTI(K)*ZI(K,J))
               SI = ML_FLOP(SI + ORTR(K)*ZI(K,J) - ORTI(K)*ZR(K,J))
  115       CONTINUE
!
            SR = ML_FLOP(SR/NORM)
            SI = ML_FLOP(SI/NORM)
!
            DO 120 K = I, IGH
               ZR(K,J) = ML_FLOP(ZR(K,J) + SR*ORTR(K) - SI*ORTI(K))
               ZI(K,J) = ML_FLOP(ZI(K,J) + SR*ORTI(K) + SI*ORTR(K))
  120       CONTINUE
!
  130    CONTINUE
!
  140 CONTINUE
!*****
      IF (JOB .EQ. 3) GOTO 1001
!*****
!     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
  150 L = LOW + 1
!
      DO 170 I = L, IGH
         LL = MIN0(I+1,IGH)
         IF (HI(I,I-1) .EQ. 0.0D0) GOTO 170
         NORM = ML_PYTHAG(HR(I,I-1),HI(I,I-1))
         YR = ML_FLOP(HR(I,I-1)/NORM)
         YI = ML_FLOP(HI(I,I-1)/NORM)
         HR(I,I-1) = NORM
         HI(I,I-1) = 0.0D0
!
         DO 155 J = I, N
            SI = ML_FLOP(YR*HI(I,J) - YI*HR(I,J))
            HR(I,J) = ML_FLOP(YR*HR(I,J) + YI*HI(I,J))
            HI(I,J) = SI
  155    CONTINUE
!
         DO 160 J = 1, LL
            SI = ML_FLOP(YR*HI(J,I) + YI*HR(J,I))
            HR(J,I) = ML_FLOP(YR*HR(J,I) - YI*HI(J,I))
            HI(J,I) = SI
  160    CONTINUE
!*****
         IF (JOB .EQ. 0) GOTO 170
!*****
         DO 165 J = LOW, IGH
            SI = ML_FLOP(YR*ZI(J,I) + YI*ZR(J,I))
            ZR(J,I) = ML_FLOP(YR*ZR(J,I) - YI*ZI(J,I))
            ZI(J,I) = SI
  165    CONTINUE
!
  170 CONTINUE
!     .......... STORE ROOTS ISOLATED BY CBAL ..........
  180 DO 200 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GOTO 200
         WR(I) = HR(I,I)
         WI(I) = HI(I,I)
  200 CONTINUE
!
      EN = IGH
      TR = 0.0D0
      TI = 0.0D0
      ITN = 30*N
!     .......... SEARCH FOR NEXT EIGENVALUE ..........
  220 IF (EN .LT. LOW) GOTO 680
      ITS = 0
      ENM1 = EN - 1
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
  240 DO 260 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GOTO 300
!*****
         XR = ML_FLOP(DABS(HR(L-1,L-1)) + DABS(HI(L-1,L-1)) + DABS(HR(L,L)) +DABS(HI(L,L)))
         YR = ML_FLOP(XR + DABS(HR(L,L-1)))
         IF (XR .EQ. YR) GOTO 300
!*****
  260 CONTINUE
!     .......... FORM SHIFT ..........
  300 IF (L .EQ. EN) GOTO 660
      IF (ITN .EQ. 0) GOTO 1000
      IF (ITS .EQ. 10 .OR. ITS .EQ. 20) GOTO 320
      SR = HR(EN,EN)
      SI = HI(EN,EN)
      XR = ML_FLOP(HR(ENM1,EN)*HR(EN,ENM1))
      XI = ML_FLOP(HI(ENM1,EN)*HR(EN,ENM1))
      IF (XR .EQ. 0.0D0 .AND. XI .EQ. 0.0D0) GOTO 340
      YR = ML_FLOP((HR(ENM1,ENM1) - SR)/2.0D0)
      YI = ML_FLOP((HI(ENM1,ENM1) - SI)/2.0D0)
      CALL ML_WSQRT(YR**2-YI**2+XR,2.0D0*YR*YI+XI,ZZR,ZZI)
      IF (YR*ZZR + YI*ZZI .GE. 0.0D0) GOTO 310
      ZZR = -ZZR
      ZZI = -ZZI
  310 CALL ML_WDIV(XR,XI,YR+ZZR,YI+ZZI,ZZR,ZZI)
      SR = ML_FLOP(SR - ZZR)
      SI = ML_FLOP(SI - ZZI)
      GOTO 340
!     .......... FORM EXCEPTIONAL SHIFT ..........
  320 SR = ML_FLOP(DABS(HR(EN,ENM1)) + DABS(HR(ENM1,EN-2)))
      SI = 0.0D0
!
  340 DO 360 I = LOW, EN
         HR(I,I) = ML_FLOP(HR(I,I) - SR)
         HI(I,I) = ML_FLOP(HI(I,I) - SI)
  360 CONTINUE
!
      TR = ML_FLOP(TR + SR)
      TI = ML_FLOP(TI + SI)
      ITS = ITS + 1
      ITN = ITN - 1
!     .......... REDUCE TO TRIANGLE (ROWS) ..........
      LP1 = L + 1
!
      DO 500 I = LP1, EN
         SR = HR(I,I-1)
         HR(I,I-1) = 0.0D0
         NORM= ML_FLOP(DABS(HR(I-1,I-1)) + DABS(HI(I-1,I-1)) + DABS(SR))
         NORM= ML_FLOP(NORM*DSQRT((HR(I-1,I-1)/NORM)**2 + (HI(I-1,I-1)/NORM)**2 + (SR/NORM)**2))
         XR = ML_FLOP(HR(I-1,I-1)/NORM)
         WR(I-1) = XR
         XI = ML_FLOP(HI(I-1,I-1)/NORM)
         WI(I-1) = XI
         HR(I-1,I-1) = NORM
         HI(I-1,I-1) = 0.0D0
         HI(I,I-1) = ML_FLOP(SR/NORM)
!
         DO 490 J = I, N
            YR = HR(I-1,J)
            YI = HI(I-1,J)
            ZZR = HR(I,J)
            ZZI = HI(I,J)
            HR(I-1,J) = ML_FLOP(XR*YR + XI*YI + HI(I,I-1)*ZZR)
            HI(I-1,J) = ML_FLOP(XR*YI - XI*YR + HI(I,I-1)*ZZI)
            HR(I,J) = ML_FLOP(XR*ZZR - XI*ZZI - HI(I,I-1)*YR)
            HI(I,J) = ML_FLOP(XR*ZZI + XI*ZZR - HI(I,I-1)*YI)
  490    CONTINUE
!
  500 CONTINUE
!
      SI = HI(EN,EN)
      IF (SI .EQ. 0.0D0) GOTO 540
      NORM = ML_PYTHAG(HR(EN,EN),SI)
      SR = ML_FLOP(HR(EN,EN)/NORM)
      SI = ML_FLOP(SI/NORM)
      HR(EN,EN) = NORM
      HI(EN,EN) = 0.0D0
      IF (EN .EQ. N) GOTO 540
      IP1 = EN + 1
!
      DO 520 J = IP1, N
         YR = HR(EN,J)
         YI = HI(EN,J)
         HR(EN,J) = ML_FLOP(SR*YR + SI*YI)
         HI(EN,J) = ML_FLOP(SR*YI - SI*YR)
  520 CONTINUE
!     .......... INVERSE OPERATION (COLUMNS) ..........
  540 DO 600 J = LP1, EN
         XR = WR(J-1)
         XI = WI(J-1)
!
         DO 580 I = 1, J
            YR = HR(I,J-1)
            YI = 0.0D0
            ZZR = HR(I,J)
            ZZI = HI(I,J)
            IF (I .EQ. J) GOTO 560
            YI = HI(I,J-1)
            HI(I,J-1) = ML_FLOP(XR*YI + XI*YR + HI(J,J-1)*ZZI)
  560       HR(I,J-1) = ML_FLOP(XR*YR - XI*YI + HI(J,J-1)*ZZR)
            HR(I,J) = ML_FLOP(XR*ZZR + XI*ZZI - HI(J,J-1)*YR)
            HI(I,J) = ML_FLOP(XR*ZZI - XI*ZZR - HI(J,J-1)*YI)
  580    CONTINUE
!*****
         IF (JOB .EQ. 0) GOTO 600
!*****
         DO 590 I = LOW, IGH
            YR = ZR(I,J-1)
            YI = ZI(I,J-1)
            ZZR = ZR(I,J)
            ZZI = ZI(I,J)
            ZR(I,J-1) = ML_FLOP(XR*YR - XI*YI + HI(J,J-1)*ZZR)
            ZI(I,J-1) = ML_FLOP(XR*YI + XI*YR + HI(J,J-1)*ZZI)
            ZR(I,J) = ML_FLOP(XR*ZZR + XI*ZZI - HI(J,J-1)*YR)
            ZI(I,J) = ML_FLOP(XR*ZZI - XI*ZZR - HI(J,J-1)*YI)
  590    CONTINUE
!
  600 CONTINUE
!
      IF (SI .EQ. 0.0D0) GOTO 240
!
      DO 630 I = 1, EN
         YR = HR(I,EN)
         YI = HI(I,EN)
         HR(I,EN) = ML_FLOP(SR*YR - SI*YI)
         HI(I,EN) = ML_FLOP(SR*YI + SI*YR)
  630 CONTINUE
!*****
      IF (JOB .EQ. 0) GOTO 240
!*****
      DO 640 I = LOW, IGH
         YR = ZR(I,EN)
         YI = ZI(I,EN)
         ZR(I,EN) = ML_FLOP(SR*YR - SI*YI)
         ZI(I,EN) = ML_FLOP(SR*YI + SI*YR)
  640 CONTINUE
!
      GOTO 240
!     .......... A ROOT FOUND ..........
  660 HR(EN,EN) = ML_FLOP(HR(EN,EN) + TR)
      WR(EN) = HR(EN,EN)
      HI(EN,EN) = ML_FLOP(HI(EN,EN) + TI)
      WI(EN) = HI(EN,EN)
      EN = ENM1
      GOTO 220
!     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
!                VECTORS OF UPPER TRIANGULAR FORM ..........
!
!*****  THE FOLLOWING SECTION CHANGED FOR OVERFLOW CONTROL
!       C. MOLER, 3/16/82
!
  680 IF (JOB .NE. 2) GOTO 1001
!
      NORM = 0.0D0
      DO 720 I = 1, N
         DO 720 J = I, N
            TR = ML_FLOP(DABS(HR(I,J))) + ML_FLOP(DABS(HI(I,J)))
            IF (TR .GT. NORM) NORM = TR
  720 CONTINUE
      IF (N .EQ. 1 .OR. NORM .EQ. 0.0D0) GOTO 1001
!     .......... FOR EN=N STEP -1 UNTIL 2 DO -- ..........
      DO 800 NN = 2, N
         EN = N + 2 - NN
         XR = WR(EN)
         XI = WI(EN)
         HR(EN,EN) = 1.0D0
         HI(EN,EN) = 0.0D0
         ENM1 = EN - 1
!     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
         DO 780 II = 1, ENM1
            I = EN - II
            ZZR = 0.0D0
            ZZI = 0.0D0
            IP1 = I + 1
            DO 740 J = IP1, EN
               ZZR = ML_FLOP(ZZR + HR(I,J)*HR(J,EN) - HI(I,J)*HI(J,EN))
               ZZI = ML_FLOP(ZZI + HR(I,J)*HI(J,EN) + HI(I,J)*HR(J,EN))
  740       CONTINUE
            YR = ML_FLOP(XR - WR(I))
            YI = ML_FLOP(XI - WI(I))
            IF (YR .NE. 0.0D0 .OR. YI .NE. 0.0D0) GOTO 765
               YR = NORM
  760          YR = ML_FLOP(YR/100.0D0)
               YI = ML_FLOP(NORM + YR)
               IF (YI .NE. NORM) GOTO 760
               YI = 0.0D0
  765       CONTINUE
            CALL ML_WDIV(ZZR,ZZI,YR,YI,HR(I,EN),HI(I,EN))
            TR = ML_FLOP(DABS(HR(I,EN))) + ML_FLOP(DABS(HI(I,EN)))
            IF (TR .EQ. 0.0D0) GOTO 780
            IF (TR + 1.0D0/TR .GT. TR) GOTO 780
            DO 770 J = I, EN
               HR(J,EN) = ML_FLOP(HR(J,EN)/TR)
               HI(J,EN) = ML_FLOP(HI(J,EN)/TR)
  770       CONTINUE
  780    CONTINUE
!
  800 CONTINUE
!*****
!     .......... END BACKSUBSTITUTION ..........
      ENM1 = N - 1
!     .......... VECTORS OF ISOLATED ROOTS ..........
      DO  840 I = 1, ENM1
         IF (I .GE. LOW .AND. I .LE. IGH) GOTO 840
         IP1 = I + 1
!
         DO 820 J = IP1, N
            ZR(I,J) = HR(I,J)
            ZI(I,J) = HI(I,J)
  820    CONTINUE
!
  840 CONTINUE
!     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
!                VECTORS OF ORIGINAL FULL MATRIX.
!                FOR J=N STEP -1 UNTIL LOW+1 DO -- ..........
      DO 880 JJ = LOW, ENM1
         J = N + LOW - JJ
         M = MIN0(J,IGH)
!
         DO 880 I = LOW, IGH
            ZZR = 0.0D0
            ZZI = 0.0D0
!
            DO 860 K = LOW, M
               ZZR = ML_FLOP(ZZR + ZR(I,K)*HR(K,J) - ZI(I,K)*HI(K,J))
               ZZI = ML_FLOP(ZZI + ZR(I,K)*HI(K,J) + ZI(I,K)*HR(K,J))
  860       CONTINUE
!
            ZR(I,J) = ZZR
            ZI(I,J) = ZZI
  880 CONTINUE
!
      GOTO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = EN
 1001 RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WSVDC(XR,XI,LDX,N,P,SR,SI,ER,EI,UR,UI,LDU,VR,VI,LDV,WORKR,WORKI,JOB,INFO)
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO
      DOUBLE PRECISION XR(LDX,*),XI(LDX,*),SR(*),SI(*),ER(*),EI(*), UR(LDU,*),UI(LDU,*),VR(LDV,*),VI(LDV,*), WORKR(*),WORKI(*)
!
!
!     WSVDC IS A SUBROUTINE TO REDUCE A DOUBLE-COMPLEX NXP MATRIX X BY
!     UNITARY TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE
!     DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE
!     COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,
!     AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.
!
!     ON ENTRY
!
!         X         DOUBLE-COMPLEX(LDX,P), WHERE LDX.GE.N.
!                   X CONTAINS THE MATRIX WHOSE SINGULAR VALUE
!                   DECOMPOSITION IS TO BE COMPUTED.  X IS
!                   DESTROYED BY WSVDC.
!
!         LDX       INTEGER.
!                   LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!         N         INTEGER.
!                   N IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!         P         INTEGER.
!                   P IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!         LDU       INTEGER.
!                   LDU IS THE LEADING DIMENSION OF THE ARRAY U
!                   (SEE BELOW).
!
!         LDV       INTEGER.
!                   LDV IS THE LEADING DIMENSION OF THE ARRAY V
!                   (SEE BELOW).
!
!         WORK      DOUBLE-COMPLEX(N).
!                   WORK IS A SCRATCH ARRAY.
!
!         JOB       INTEGER.
!                   JOB CONTROLS THE COMPUTATION OF THE SINGULAR
!                   VECTORS.  IT HAS THE DECIMAL EXPANSION AB
!                   WITH THE FOLLOWING MEANING
!
!     A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR
!               VECTORS.
!     A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS
!               IN U.
!     A.GE.2    RETURNS THE FIRST MIN(N,P)
!               LEFT SINGULAR VECTORS IN U.
!     B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR
!               VECTORS.
!     B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS
!               IN V.
!
!     ON RETURN
!
!         S         DOUBLE-COMPLEX(MM), WHERE MM=MIN(N+1,P).
!                   THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE
!                   SINGULAR VALUES OF X ARRANGED IN DESCENDING
!                   ORDER OF MAGNITUDE.
!
!         E         DOUBLE-COMPLEX(P).
!                   E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE
!                   DISCUSSION OF INFO FOR EXCEPTIONS.
!
!         U         DOUBLE-COMPLEX(LDU,K), WHERE LDU.GE.N.
!                   IF JOBA.EQ.1 THEN K.EQ.N,
!                   IF JOBA.EQ.2 THEN K.EQ.MIN(N,P).
!                   U CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P
!                   OR IF JOBA.GT.2, THEN U MAY BE IDENTIFIED WITH X
!                   IN THE SUBROUTINE CALL.
!
!         V         DOUBLE-COMPLEX(LDV,P), WHERE LDV.GE.P.
!                   V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   V IS NOT REFERENCED IF JOBB.EQ.0.  IF P.LE.N,
!                   THEN V MAY BE IDENTIFIED WHTH X IN THE
!                   SUBROUTINE ML_CALL.
!
!         INFO      INTEGER.
!                   THE SINGULAR VALUES (AND THEIR CORRESPONDING
!                   SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)
!                   ARE CORRECT (HERE M=MIN(N,P)).  THUS IF
!                   INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR
!                   VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX
!                   B = CTRANS(U)*X*V IS THE BIDIAGONAL MATRIX
!                   WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE
!                   ELEMENTS OF E ON ITS SUPER-DIAGONAL (CTRANS(U)
!                   IS THE CONJUGATE-TRANSPOSE OF U).  THUS THE
!                   SINGULAR VALUES OF X AND B ARE THE SAME.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS    ML_WAXPY,ML_PYTHAG,ML_WDOTCR,ML_WDOTCI,ML_WSCAL,WSWAP,
!             ML_RROTG,ML_WNRM2
!     FORTRAN DABS,DIMAG,DMAX1
!     FORTRAN MAX0,MIN0,MOD,DSQRT
!
!     INTERNAL VARIABLES
!
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,M,MAXIT,MM,MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1
      DOUBLE PRECISION ML_PYTHAG,ML_WDOTCR,ML_WDOTCI,TR,TI,RR,RI
      DOUBLE PRECISION B,C,CS,EL,EMM1,F,G,ML_WNRM2,SCALE,SHIFT,SL,SM,SN,SMM1,T1,TEST,ZTEST,SMALL,ML_FLOP
      LOGICAL WANTU,WANTV
!
      DOUBLE PRECISION ZDUMR,ZDUMI
      DOUBLE PRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET THE MAXIMUM NUMBER OF ITERATIONS.
!
      MAXIT = 75
!
!     SMALL NUMBER, ROUGHLY MACHINE EPSILON, USED TO AVOID UNDERFLOW
!
      SMALL = 1.D0/2.D0**48
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
      WANTU = .FALSE.
      WANTV = .FALSE.
      JOBU = MOD(JOB,100)/10
      NCU = N
      IF (JOBU .GT. 1) NCU = MIN0(N,P)
      IF (JOBU .NE. 0) WANTU = .TRUE.
      IF (MOD(JOB,10) .NE. 0) WANTV = .TRUE.
!
!     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
!     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
!
      INFO = 0
      NCT = MIN0(N-1,P)
      NRT = MAX0(0,MIN0(P-2,N))
      LU = MAX0(NCT,NRT)
      IF (LU .LT. 1) GOTO 190
      DO 180 L = 1, LU
         LP1 = L + 1
         IF (L .GT. NCT) GOTO 30
!
!           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
!           PLACE THE L-TH DIAGONAL IN S(L).
!
            SR(L) = ML_WNRM2(N-L+1,XR(L,L),XI(L,L),1)
            SI(L) = 0.0D0
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 20
               IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 10
                  CALL ML_WSIGN(SR(L),SI(L),XR(L,L),XI(L,L),SR(L),SI(L))
   10          CONTINUE
               CALL ML_WDIV(1.0D0,0.0D0,SR(L),SI(L),TR,TI)
               CALL ML_WSCAL(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
               XR(L,L) = ML_FLOP(1.0D0 + XR(L,L))
   20       CONTINUE
            SR(L) = -SR(L)
            SI(L) = -SI(L)
   30    CONTINUE
         IF (P .LT. LP1) GOTO 60
         DO 50 J = LP1, P
            IF (L .GT. NCT) GOTO 40
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 40
!
!              APPLY THE TRANSFORMATION.
!
               TR= -ML_WDOTCR(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               TI= -ML_WDOTCI(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               CALL ML_WDIV(TR,TI,XR(L,L),XI(L,L),TR,TI)
               CALL ML_WAXPY(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
   40       CONTINUE
!
!           PLACE THE L-TH ROW OF X INTO  E FOR THE
!           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
!
            ER(J) = XR(L,J)
            EI(J) = -XI(L,J)
   50    CONTINUE
   60    CONTINUE
         IF (.NOT.WANTU .OR. L .GT. NCT) GOTO 80
!
!           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
!           MULTIPLICATION.
!
            DO 70 I = L, N
               UR(I,L) = XR(I,L)
               UI(I,L) = XI(I,L)
   70       CONTINUE
   80    CONTINUE
         IF (L .GT. NRT) GOTO 170
!
!           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
!           L-TH SUPER-DIAGONAL IN E(L).
!
            ER(L) = ML_WNRM2(P-L,ER(LP1),EI(LP1),1)
            EI(L) = 0.0D0
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 100
               IF (CABS1(ER(LP1),EI(LP1)) .EQ. 0.0D0) GOTO 90
                  CALL ML_WSIGN(ER(L),EI(L),ER(LP1),EI(LP1),ER(L),EI(L))
   90          CONTINUE
               CALL ML_WDIV(1.0D0,0.0D0,ER(L),EI(L),TR,TI)
               CALL ML_WSCAL(P-L,TR,TI,ER(LP1),EI(LP1),1)
               ER(LP1) = ML_FLOP(1.0D0 + ER(LP1))
  100       CONTINUE
            ER(L) = -ER(L)
            EI(L) = +EI(L)
            IF (LP1 .GT. N .OR. CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 140
!
!              APPLY THE TRANSFORMATION.
!
               DO 110 I = LP1, N
                  WORKR(I) = 0.0D0
                  WORKI(I) = 0.0D0
  110          CONTINUE
               DO 120 J = LP1, P
                  CALL ML_WAXPY(N-L,ER(J),EI(J),XR(LP1,J),XI(LP1,J),1, WORKR(LP1),WORKI(LP1),1)
  120          CONTINUE
               DO 130 J = LP1, P
                  CALL ML_WDIV(-ER(J),-EI(J),ER(LP1),EI(LP1),TR,TI)
                  CALL ML_WAXPY(N-L,TR,-TI,WORKR(LP1),WORKI(LP1),1, XR(LP1,J),XI(LP1,J),1)
  130          CONTINUE
  140       CONTINUE
            IF (.NOT.WANTV) GOTO 160
!
!              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
!              BACK MULTIPLICATION.
!
               DO 150 I = LP1, P
                  VR(I,L) = ER(I)
                  VI(I,L) = EI(I)
  150          CONTINUE
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
  190 CONTINUE
!
!     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
!
      M = MIN0(P,N+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT .GE. P) GOTO 200
         SR(NCTP1) = XR(NCTP1,NCTP1)
         SI(NCTP1) = XI(NCTP1,NCTP1)
  200 CONTINUE
      IF (N .GE. M) GOTO 210
         SR(M) = 0.0D0
         SI(M) = 0.0D0
  210 CONTINUE
      IF (NRTP1 .GE. M) GOTO 220
         ER(NRTP1) = XR(NRTP1,M)
         EI(NRTP1) = XI(NRTP1,M)
  220 CONTINUE
      ER(M) = 0.0D0
      EI(M) = 0.0D0
!
!     IF REQUIRED, GENERATE U.
!
      IF (.NOT.WANTU) GOTO 350
         IF (NCU .LT. NCTP1) GOTO 250
         DO 240 J = NCTP1, NCU
            DO 230 I = 1, N
               UR(I,J) = 0.0D0
               UI(I,J) = 0.0D0
  230       CONTINUE
            UR(J,J) = 1.0D0
            UI(J,J) = 0.0D0
  240    CONTINUE
  250    CONTINUE
         IF (NCT .LT. 1) GOTO 340
         DO 330 LL = 1, NCT
            L = NCT - LL + 1
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 300
               LP1 = L + 1
               IF (NCU .LT. LP1) GOTO 270
               DO 260 J = LP1, NCU
                  TR = -ML_WDOTCR(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  TI = -ML_WDOTCI(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  CALL ML_WDIV(TR,TI,UR(L,L),UI(L,L),TR,TI)
                  CALL ML_WAXPY(N-L+1,TR,TI,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
  260          CONTINUE
  270          CONTINUE
               CALL ML_WRSCAL(N-L+1,-1.0D0,UR(L,L),UI(L,L),1)
               UR(L,L) = ML_FLOP(1.0D0 + UR(L,L))
               LM1 = L - 1
               IF (LM1 .LT. 1) GOTO 290
               DO 280 I = 1, LM1
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
  280          CONTINUE
  290          CONTINUE
            GOTO 320
  300       CONTINUE
               DO 310 I = 1, N
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
  310          CONTINUE
               UR(L,L) = 1.0D0
               UI(L,L) = 0.0D0
  320       CONTINUE
  330    CONTINUE
  340    CONTINUE
  350 CONTINUE
!
!     IF IT IS REQUIRED, GENERATE V.
!
      IF (.NOT.WANTV) GOTO 400
         DO 390 LL = 1, P
            L = P - LL + 1
            LP1 = L + 1
            IF (L .GT. NRT) GOTO 370
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 370
               DO 360 J = LP1, P
                  TR = -ML_WDOTCR(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  TI = -ML_WDOTCI(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                CALL ML_WDIV(TR,TI,VR(LP1,L),VI(LP1,L),TR,TI)
                CALL ML_WAXPY(P-L,TR,TI,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
  360          CONTINUE
  370       CONTINUE
            DO 380 I = 1, P
               VR(I,L) = 0.0D0
               VI(I,L) = 0.0D0
  380       CONTINUE
            VR(L,L) = 1.0D0
            VI(L,L) = 0.0D0
  390    CONTINUE
  400 CONTINUE
!
!     TRANSFORM S AND E SO THAT THEY ARE REAL.
!
      DO 420 I = 1, M
            TR = ML_PYTHAG(SR(I),SI(I))
            IF (TR .EQ. 0.0D0) GOTO 405
            RR = SR(I)/TR
            RI = SI(I)/TR
            SR(I) = TR
            SI(I) = 0.0D0
            IF (I .LT. M) CALL ML_WDIV(ER(I),EI(I),RR,RI,ER(I),EI(I))
            IF (WANTU) CALL ML_WSCAL(N,RR,RI,UR(1,I),UI(1,I),1)
  405    CONTINUE
!     ...EXIT
         IF (I .EQ. M) GOTO 430
            TR = ML_PYTHAG(ER(I),EI(I))
            IF (TR .EQ. 0.0D0) GOTO 410
            CALL ML_WDIV(TR,0.0D0,ER(I),EI(I),RR,RI)
            ER(I) = TR
            EI(I) = 0.0D0
            CALL ML_WMUL(SR(I+1),SI(I+1),RR,RI,SR(I+1),SI(I+1))
            IF (WANTV) CALL ML_WSCAL(P,RR,RI,VR(1,I+1),VI(1,I+1),1)
  410    CONTINUE
  420 CONTINUE
  430 CONTINUE
!
!     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
!
      MM = M
      ITER = 0
  440 CONTINUE
!
!        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
!
!     ...EXIT
         IF (M .EQ. 0) GOTO 700
!
!        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
!        FLAG AND RETURN.
!
         IF (ITER .LT. MAXIT) GOTO 450
            INFO = M
!     ......EXIT
            GOTO 700
  450    CONTINUE
!
!        THIS SECTION OF THE PROGRAM INSPECTS FOR
!        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON
!        COMPLETION THE VARIABLE KASE IS SET AS FOLLOWS.
!
!           KASE = 1     IF SR(M) AND ER(L-1) ARE NEGLIGIBLE AND L.LT.M
!           KASE = 2     IF SR(L) IS NEGLIGIBLE AND L.LT.M
!           KASE = 3     IF ER(L-1) IS NEGLIGIBLE, L.LT.M, AND
!     SR(L), ..., SR(M) ARE NOT NEGLIGIBLE (QR STEP).
!           KASE = 4     IF ER(M-1) IS NEGLIGIBLE (CONVERGENCE).
!
         DO 470 LL = 1, M
            L = M - LL
!        ...EXIT
            IF (L .EQ. 0) GOTO 480
            TEST = ML_FLOP(DABS(SR(L)) + DABS(SR(L+1)))
            ZTEST = ML_FLOP(TEST + DABS(ER(L))/2.0D0)
            IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 460
               ER(L) = 0.0D0
!        ......EXIT
               GOTO 480
  460       CONTINUE
  470    CONTINUE
  480    CONTINUE
         IF (L .NE. M - 1) GOTO 490
            KASE = 4
         GOTO 560
  490    CONTINUE
            LP1 = L + 1
            MP1 = M + 1
            DO 510 LLS = LP1, MP1
               LS = M - LLS + LP1
!           ...EXIT
               IF (LS .EQ. L) GOTO 520
               TEST = 0.0D0
               IF (LS .NE. M) TEST = ML_FLOP(TEST + DABS(ER(LS)))
               IF (LS .NE. L + 1) TEST = ML_FLOP(TEST + DABS(ER(LS-1)))
               ZTEST = ML_FLOP(TEST + DABS(SR(LS))/2.0D0)
               IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 500
                  SR(LS) = 0.0D0
!           ......EXIT
                  GOTO 520
  500          CONTINUE
  510       CONTINUE
  520       CONTINUE
            IF (LS .NE. L) GOTO 530
               KASE = 3
            GOTO 550
  530       CONTINUE
            IF (LS .NE. M) GOTO 540
               KASE = 1
            GOTO 550
  540       CONTINUE
               KASE = 2
               L = LS
  550       CONTINUE
  560    CONTINUE
         L = L + 1
!
!        PERFORM THE TASK INDICATED BY KASE.
!
         GOTO (570, 600, 620, 650), KASE
!
!        DEFLATE NEGLIGIBLE SR(M).
!
  570    CONTINUE
            MM1 = M - 1
            F = ER(M-1)
            ER(M-1) = 0.0D0
            DO 590 KK = L, MM1
               K = MM1 - KK + L
               T1 = SR(K)
               CALL ML_RROTG(T1,F,CS,SN)
               SR(K) = T1
               IF (K .EQ. L) GOTO 580
                  F = ML_FLOP(-(SN*ER(K-1)))
                  ER(K-1) = ML_FLOP(CS*ER(K-1))
  580          CONTINUE
               IF (WANTV) CALL ML_RROT(P,VR(1,K),1,VR(1,M),1,CS,SN)
               IF (WANTV) CALL ML_RROT(P,VI(1,K),1,VI(1,M),1,CS,SN)
  590       CONTINUE
         GOTO 690
!
!        SPLIT AT NEGLIGIBLE SR(L).
!
  600    CONTINUE
            F = ER(L-1)
            ER(L-1) = 0.0D0
            DO 610 K = L, M
               T1 = SR(K)
               CALL ML_RROTG(T1,F,CS,SN)
               SR(K) = T1
               F = ML_FLOP(-(SN*ER(K)))
               ER(K) = ML_FLOP(CS*ER(K))
               IF (WANTU) CALL ML_RROT(N,UR(1,K),1,UR(1,L-1),1,CS,SN)
               IF (WANTU) CALL ML_RROT(N,UI(1,K),1,UI(1,L-1),1,CS,SN)
  610       CONTINUE
         GOTO 690
!
!        PERFORM ONE QR STEP.
!
  620    CONTINUE
!
!           CALCULATE THE SHIFT.
!
            SCALE = DMAX1(DABS(SR(M)),DABS(SR(M-1)),DABS(ER(M-1)), DABS(SR(L)),DABS(ER(L)))
            SM = SR(M)/SCALE
            SMM1 = SR(M-1)/SCALE
            EMM1 = ER(M-1)/SCALE
            SL = SR(L)/SCALE
            EL = ER(L)/SCALE
            B = ML_FLOP(((SMM1 + SM)*(SMM1 - SM) + EMM1**2)/2.0D0)
            C = ML_FLOP((SM*EMM1)**2)
            SHIFT = 0.0D0
            IF (B .EQ. 0.0D0 .AND. C .EQ. 0.0D0) GOTO 630
               SHIFT = ML_FLOP(DSQRT(B**2+C))
               IF (B .LT. 0.0D0) SHIFT = -SHIFT
               SHIFT = ML_FLOP(C/(B + SHIFT))
  630       CONTINUE
            F = ML_FLOP((SL + SM)*(SL - SM) - SHIFT)
            G = ML_FLOP(SL*EL)
!
!           CHASE ZEROS.
!
            MM1 = M - 1
            DO 640 K = L, MM1
               CALL ML_RROTG(F,G,CS,SN)
               IF (K .NE. L) ER(K-1) = F
               F = ML_FLOP(CS*SR(K) + SN*ER(K))
               ER(K) = ML_FLOP(CS*ER(K) - SN*SR(K))
               G = ML_FLOP(SN*SR(K+1))
               SR(K+1) = ML_FLOP(CS*SR(K+1))
               IF (WANTV) CALL ML_RROT(P,VR(1,K),1,VR(1,K+1),1,CS,SN)
               IF (WANTV) CALL ML_RROT(P,VI(1,K),1,VI(1,K+1),1,CS,SN)
               CALL ML_RROTG(F,G,CS,SN)
               SR(K) = F
               F = ML_FLOP(CS*ER(K) + SN*SR(K+1))
               SR(K+1) = ML_FLOP(-(SN*ER(K)) + CS*SR(K+1))
               G = ML_FLOP(SN*ER(K+1))
               ER(K+1) = ML_FLOP(CS*ER(K+1))
               IF (WANTU .AND. K .LT. N) CALL ML_RROT(N,UR(1,K),1,UR(1,K+1),1,CS,SN)
               IF (WANTU .AND. K .LT. N) CALL ML_RROT(N,UI(1,K),1,UI(1,K+1),1,CS,SN)
  640       CONTINUE
            ER(M-1) = F
            ITER = ITER + 1
         GOTO 690
!
!        CONVERGENCE
!
  650    CONTINUE
!
!           MAKE THE SINGULAR VALUE  POSITIVE
!
            IF (SR(L) .GE. 0.0D0) GOTO 660
               SR(L) = -SR(L)
             IF (WANTV) CALL ML_WRSCAL(P,-1.0D0,VR(1,L),VI(1,L),1)
  660       CONTINUE
!
!           ORDER THE SINGULAR VALUE.
!
  670       IF (L .EQ. MM) GOTO 680
!           ...EXIT
               IF (SR(L) .GE. SR(L+1)) GOTO 680
               TR = SR(L)
               SR(L) = SR(L+1)
               SR(L+1) = TR
               IF (WANTV .AND. L .LT. P)CALL ML_WSWAP(P,VR(1,L),VI(1,L),1,VR(1,L+1),VI(1,L+1),1)
               IF (WANTU .AND. L .LT. N)CALL ML_WSWAP(N,UR(1,L),UI(1,L),1,UR(1,L+1),UI(1,L+1),1)
               L = L + 1
            GOTO 670
  680       CONTINUE
            ITER = 0
            M = M - 1
  690    CONTINUE
      GOTO 440
  700 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WQRDC(XR,XI,LDX,N,P,QRAUXR,QRAUXI,JPVT,WORKR,WORKI, JOB)
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLE PRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*), WORKR(*),WORKI(*)
!
!     WQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR
!     FACTORIZATION OF AN N BY P MATRIX X.  COLUMN PIVOTING
!     BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE
!     PERFORMED AT THE USERS OPTION.
!
!     ON ENTRY
!
!        X       DOUBLE-COMPLEX(LDX,P), WHERE LDX .GE. N.
!                X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE
!                COMPUTED.
!
!        LDX     INTEGER.
!                LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N       INTEGER.
!                N IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!        P       INTEGER.
!                P IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!        JPVT    INTEGER(P).
!                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
!                OF THE PIVOT COLUMNS.  THE K-TH COLUMN X(K) OF X
!                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
!                VALUE OF JPVT(K).
!
!                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
!                   COLUMN.
!
!                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.
!
!                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.
!
!                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS
!                ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL
!                COLUMNS TO THE END.  BOTH INITIAL AND FINAL COLUMNS
!                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
!                FREE COLUMNS ARE MOVED.  AT THE K-TH STAGE OF THE
!                REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN
!                IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST
!                REDUCED NORM.  JPVT IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        WORK    DOUBLE-COMPLEX(P).
!                WORK IS A WORK ARRAY.  WORK IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        JOB     INTEGER.
!                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
!                IF JOB .EQ. 0, NO PIVOTING IS DONE.
!                IF JOB .NE. 0, PIVOTING IS DONE.
!
!     ON RETURN
!
!        X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER
!                TRIANGULAR MATRIX R OF THE QR FACTORIZATION.
!                BELOW ITS DIAGONAL X CONTAINS INFORMATION FROM
!                WHICH THE UNITARY PART OF THE DECOMPOSITION
!                CAN BE RECOVERED.  NOTE THAT IF PIVOTING HAS
!                BEEN REQUESTED, THE DECOMPOSITION IS NOT THAT
!                OF THE ORIGINAL MATRIX X BUT THAT OF X
!                WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.
!
!        QRAUX   DOUBLE-COMPLEX(P).
!                QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER
!                THE UNITARY PART OF THE DECOMPOSITION.
!
!        JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE
!                ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO
!                THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WQRDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS ML_WAXPY,ML_PYTHAG,ML_WDOTCR,ML_WDOTCI,ML_WSCAL
!     BLAS ML_WSWAP ,ML_WNRM2
!     FORTRAN DABS,DIMAG,DMAX1,MIN0
!
!     INTERNAL VARIABLES
!
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLE PRECISION MAXNRM,TT
      DOUBLE PRECISION ML_WNRM2
      DOUBLE PRECISION ML_PYTHAG,ML_WDOTCR,ML_WDOTCI,ML_FLOP
      DOUBLE PRECISION NRMXLR,NRMXLI,TR,TI
      LOGICAL NEGJ,SWAPJ
!
      DOUBLE PRECISION ZDUMR,ZDUMI
      DOUBLE PRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GOTO 60
!
!        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
!        ACCORDING TO JPVT.
!
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GOTO 10
               IF (J .NE. PL) CALL ML_WSWAP(N,XR(1,PL),XI(1,PL),1,XR(1,J),XI(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GOTO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GOTO 30
                  CALL ML_WSWAP(N,XR(1,PU),XI(1,PU),1,XR(1,J),XI(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
!
!     COMPUTE THE NORMS OF THE FREE COLUMNS.
!
      IF (PU .LT. PL) GOTO 80
      DO 70 J = PL, PU
         QRAUXR(J) = ML_WNRM2(N,XR(1,J),XI(1,J),1)
         QRAUXI(J) = 0.0D0
         WORKR(J) = QRAUXR(J)
         WORKI(J) = QRAUXI(J)
   70 CONTINUE
   80 CONTINUE
!
!     PERFORM THE HOUSEHOLDER REDUCTION OF X.
!
      LUP = MIN0(N,P)
      DO 210 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GOTO 120
!
!           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
!           INTO THE PIVOT POSITION.
!
            MAXNRM = 0.0D0
            MAXJ = L
            DO 100 J = L, PU
               IF (QRAUXR(J) .LE. MAXNRM) GOTO 90
                  MAXNRM = QRAUXR(J)
                  MAXJ = J
   90          CONTINUE
  100       CONTINUE
            IF (MAXJ .EQ. L) GOTO 110
              CALL ML_WSWAP(N,XR(1,L),XI(1,L),1,XR(1,MAXJ),XI(1,MAXJ),1)
              QRAUXR(MAXJ) = QRAUXR(L)
              QRAUXI(MAXJ) = QRAUXI(L)
              WORKR(MAXJ) = WORKR(L)
              WORKI(MAXJ) = WORKI(L)
              JP = JPVT(MAXJ)
              JPVT(MAXJ) = JPVT(L)
              JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUXR(L) = 0.0D0
         QRAUXI(L) = 0.0D0
         IF (L .EQ. N) GOTO 200
!
!           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
!
            NRMXLR = ML_WNRM2(N-L+1,XR(L,L),XI(L,L),1)
            NRMXLI = 0.0D0
            IF (CABS1(NRMXLR,NRMXLI) .EQ. 0.0D0) GOTO 190
              IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 130
              CALL ML_WSIGN(NRMXLR,NRMXLI,XR(L,L),XI(L,L),NRMXLR,NRMXLI)
  130         CONTINUE
              CALL ML_WDIV(1.0D0,0.0D0,NRMXLR,NRMXLI,TR,TI)
              CALL ML_WSCAL(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
              XR(L,L) = ML_FLOP(1.0D0 + XR(L,L))
!
!             APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
!             UPDATING THE NORMS.
!
              LP1 = L + 1
              IF (P .LT. LP1) GOTO 180
              DO 170 J = LP1, P
                  TR = -ML_WDOTCR(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  TI = -ML_WDOTCI(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  CALL ML_WDIV(TR,TI,XR(L,L),XI(L,L),TR,TI)
                  CALL ML_WAXPY(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GOTO 160
                  IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 160
                    TT=1.0D0 - (ML_PYTHAG(XR(L,J),XI(L,J))/QRAUXR(J))**2
                    TT=DMAX1(TT,0.0D0)
                    TR=ML_FLOP(TT)
                    TT=ML_FLOP(1.0D0+0.05D0*TT*(QRAUXR(J)/WORKR(J))**2)
                    IF (TT .EQ. 1.0D0) GOTO 140
                     QRAUXR(J) = QRAUXR(J)*DSQRT(TR)
                     QRAUXI(J) = QRAUXI(J)*DSQRT(TR)
                     GOTO 150
  140                CONTINUE
      QRAUXR(J) = ML_WNRM2(N-L,XR(L+1,J),XI(L+1,J),1)
      QRAUXI(J) = 0.0D0
      WORKR(J) = QRAUXR(J)
      WORKI(J) = QRAUXI(J)
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
  180          CONTINUE
!
!              SAVE THE TRANSFORMATION.
!
               QRAUXR(L) = XR(L,L)
               QRAUXI(L) = XI(L,L)
               XR(L,L) = -NRMXLR
               XI(L,L) = -NRMXLI
  190       CONTINUE
  200    CONTINUE
  210 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WQRSL(XR,XI,LDX,N,K,QRAUXR,QRAUXI,YR,YI,QYR,QYI,QTYR,QTYI,BR,BI,RSDR,RSDI,XBR,XBI,JOB,INFO)
      INTEGER LDX,N,K,JOB,INFO
      DOUBLE PRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*),YR(*),     &
     &                 YI(*),QYR(*),QYI(*),QTYR(*),QTYI(*),BR(*),BI(*),   &
     &                 RSDR(*),RSDI(*),XBR(*),XBI(*)
!
!     WQRSL APPLIES THE OUTPUT OF WQRDC TO COMPUTE COORDINATE
!     TRANSFORMATIONS, PROJECTIONS, AND LEAST SQUARES SOLUTIONS.
!     FOR K .LE. MIN(N,P), LET XK BE THE MATRIX
!
!            XK = (X(JPVT(1)),X(JPVT(2)), ... ,X(JPVT(K)))
!
!     FORMED FROM COLUMNNS JPVT(1), ... ,JPVT(K) OF THE ORIGINAL
!     N X P MATRIX X THAT WAS INPUT TO WQRDC (IF NO PIVOTING WAS
!     DONE, XK CONSISTS OF THE FIRST K COLUMNS OF X IN THEIR
!     ORIGINAL ORDER).  WQRDC PRODUCES A FACTORED UNITARY MATRIX Q
!     AND AN UPPER TRIANGULAR MATRIX R SUCH THAT
!
!              XK = Q * (R)
!    (0)
!
!     THIS INFORMATION IS CONTAINED IN CODED FORM IN THE ARRAYS
!     X AND QRAUX.
!
!     ON ENTRY
!
!        X      DOUBLE-COMPLEX(LDX,P).
!               X CONTAINS THE OUTPUT OF WQRDC.
!
!        LDX    INTEGER.
!               LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N      INTEGER.
!               N IS THE NUMBER OF ROWS OF THE MATRIX XK.  IT MUST
!               HAVE THE SAME VALUE AS N IN WQRDC.
!
!        K      INTEGER.
!               K IS THE NUMBER OF COLUMNS OF THE MATRIX XK.  K
!               MUST NNOT BE GREATER THAN MIN(N,P), WHERE P IS THE
!               SAME AS IN THE CALLING SEQUENCE TO WQRDC.
!
!        QRAUX  DOUBLE-COMPLEX(P).
!               QRAUX CONTAINS THE AUXILIARY OUTPUT FROM WQRDC.
!
!        Y      DOUBLE-COMPLEX(N)
!               Y CONTAINS AN N-VECTOR THAT IS TO BE MANIPULATED
!               BY WQRSL.
!
!        JOB    INTEGER.
!               JOB SPECIFIES WHAT IS TO BE COMPUTED.  JOB HAS
!               THE DECIMAL EXPANSION ABCDE, WITH THE FOLLOWING
!               MEANING.
!
! IF A.NE.0, COMPUTE QY.
! IF B,C,D, OR E .NE. 0, COMPUTE QTY.
! IF C.NE.0, COMPUTE B.
! IF D.NE.0, COMPUTE RSD.
! IF E.NE.0, COMPUTE XB.
!
!               NOTE THAT A REQUEST TO COMPUTE B, RSD, OR XB
!               AUTOMATICALLY TRIGGERS THE COMPUTATION OF QTY, FOR
!               WHICH AN ARRAY MUST BE PROVIDED IN THE CALLING
!               SEQUENCE.
!
!     ON RETURN
!
!        QY     DOUBLE-COMPLEX(N).
!               QY CONNTAINS Q*Y, IF ITS COMPUTATION HAS BEEN
!               REQUESTED.
!
!        QTY    DOUBLE-COMPLEX(N).
!               QTY CONTAINS CTRANS(Q)*Y, IF ITS COMPUTATION HAS
!               BEEN REQUESTED.  HERE CTRANS(Q) IS THE CONJUGATE
!               TRANSPOSE OF THE MATRIX Q.
!
!        B      DOUBLE-COMPLEX(K)
!               B CONTAINS THE SOLUTION OF THE LEAST SQUARES PROBLEM
!
! MINIMIZE NORM2(Y - XK*B),
!
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  (NOTE THAT
!               IF PIVOTING WAS REQUESTED IN WQRDC, THE J-TH
!               COMPONENT OF B WILL BE ASSOCIATED WITH COLUMN JPVT(J)
!               OF THE ORIGINAL MATRIX X THAT WAS INPUT INTO WQRDC.)
!
!        RSD    DOUBLE-COMPLEX(N).
!               RSD CONTAINS THE LEAST SQUARES RESIDUAL Y - XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  RSD IS
!               ALSO THE ORTHOGONAL PROJECTION OF Y ONTO THE
!               ORTHOGONAL COMPLEMENT OF THE COLUMN SPACE OF XK.
!
!        XB     DOUBLE-COMPLEX(N).
!               XB CONTAINS THE LEAST SQUARES APPROXIMATION XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  XB IS ALSO
!               THE ORTHOGONAL PROJECTION OF Y ONTO THE COLUMN SPACE
!               OF X.
!
!        INFO   INTEGER.
!               INFO IS ZERO UNLESS THE COMPUTATION OF B HAS
!               BEEN REQUESTED AND R IS EXACTLY SINGULAR.  IN
!               THIS CASE, INFO IS THE INDEX OF THE FIRST ZERO
!               DIAGONAL ELEMENT OF R AND B IS LEFT UNALTERED.
!
!     THE PARAMETERS QY, QTY, B, RSD, AND XB ARE NOT REFERENCED
!     IF THEIR COMPUTATION IS NOT REQUESTED AND IN THIS CASE
!     CAN BE REPLACED BY DUMMY VARIABLES IN THE CALLING PROGRAM.
!     TO SAVE STORAGE, THE USER MAY IN SOME CASES USE THE SAME
!     ARRAY FOR DIFFERENT PARAMETERS IN THE CALLING SEQUENCE.  A
!     FREQUENTLY OCCURING EXAMPLE IS WHEN ONE WISHES TO COMPUTE
!     ANY OF B, RSD, OR XB AND DOES NOT NEED Y OR QTY.  IN THIS
!     CASE ONE MAY IDENTIFY Y, QTY, AND ONE OF B, RSD, OR XB, WHILE
!     PROVIDING SEPARATE ARRAYS FOR ANYTHING ELSE THAT IS TO BE
!     COMPUTED.  THUS THE CALLING SEQUENCE
!
!          CALL ML_WQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)
!
!     WILL RESULT IN THE COMPUTATION OF B AND RSD, WITH RSD
!     OVERWRITING Y.  MORE GENERALLY, EACH ITEM IN THE FOLLOWING
!     LIST CONTAINS GROUPS OF PERMISSIBLE IDENTIFICATIONS FOR
!     A SINGLE CALLINNG SEQUENCE.
!
!          1. (Y,QTY,B) (RSD) (XB) (QY)
!
!          2. (Y,QTY,RSD) (B) (XB) (QY)
!
!          3. (Y,QTY,XB) (B) (RSD) (QY)
!
!          4. (Y,QY) (QTY,B) (RSD) (XB)
!
!          5. (Y,QY) (QTY,RSD) (B) (XB)
!
!          6. (Y,QY) (QTY,XB) (B) (RSD)
!
!     IN ANY GROUP THE VALUE RETURNED IN THE ARRAY ALLOCATED TO
!     THE GROUP CORRESPONDS TO THE LAST MEMBER OF THE GROUP.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     ML_WQRSL USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS ML_WAXPY,WCOPY,ML_WDOTCR,ML_WDOTCI
!     FORTRAN DABS,DIMAG,MIN0,MOD
!
!     INTERNAL VARIABLES
!
      INTEGER I,J,JJ,JU,KP1
      DOUBLE PRECISION ML_WDOTCR,ML_WDOTCI,TR,TI,TEMPR,TEMPI
      LOGICAL CB,CQY,CQTY,CR,CXB
!
      DOUBLE PRECISION ZDUMR,ZDUMI
      DOUBLE PRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET INFO FLAG.
!
      INFO = 0
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
      CQY = JOB/10000 .NE. 0
      CQTY = MOD(JOB,10000) .NE. 0
      CB = MOD(JOB,1000)/100 .NE. 0
      CR = MOD(JOB,100)/10 .NE. 0
      CXB = MOD(JOB,10) .NE. 0
      JU = MIN0(K,N-1)
!
!     SPECIAL ACTION WHEN N=1.
!
      IF (JU .NE. 0) GOTO 80
         IF (.NOT.CQY) GOTO 10
            QYR(1) = YR(1)
            QYI(1) = YI(1)
   10    CONTINUE
         IF (.NOT.CQTY) GOTO 20
            QTYR(1) = YR(1)
            QTYI(1) = YI(1)
   20    CONTINUE
         IF (.NOT.CXB) GOTO 30
            XBR(1) = YR(1)
            XBI(1) = YI(1)
   30    CONTINUE
         IF (.NOT.CB) GOTO 60
            IF (CABS1(XR(1,1),XI(1,1)) .NE. 0.0D0) GOTO 40
               INFO = 1
            GOTO 50
   40       CONTINUE
               CALL ML_WDIV(YR(1),YI(1),XR(1,1),XI(1,1),BR(1),BI(1))
   50       CONTINUE
   60    CONTINUE
         IF (.NOT.CR) GOTO 70
            RSDR(1) = 0.0D0
            RSDI(1) = 0.0D0
   70    CONTINUE
      GOTO 290
   80 CONTINUE
!
!        SET UP TO COMPUTE QY OR QTY.
!
         IF (CQY) CALL ML_WCOPY(N,YR,YI,1,QYR,QYI,1)
         IF (CQTY) CALL ML_WCOPY(N,YR,YI,1,QTYR,QTYI,1)
         IF (.NOT.CQY) GOTO 110
!
!           COMPUTE QY.
!
            DO 100 JJ = 1, JU
               J = JU - JJ + 1
               IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 90
                  TEMPR = XR(J,J)
                  TEMPI = XI(J,J)
                  XR(J,J) = QRAUXR(J)
                  XI(J,J) = QRAUXI(J)
                  TR=-ML_WDOTCR(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
                  TI=-ML_WDOTCI(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
                  CALL ML_WDIV(TR,TI,XR(J,J),XI(J,J),TR,TI)
                  CALL ML_WAXPY(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QYR(J), QYI(J),1)
                  XR(J,J) = TEMPR
                  XI(J,J) = TEMPI
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
         IF (.NOT.CQTY) GOTO 140
!
!           COMPUTE CTRANS(Q)*Y.
!
            DO 130 J = 1, JU
               IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 120
                  TEMPR = XR(J,J)
                  TEMPI = XI(J,J)
                  XR(J,J) = QRAUXR(J)
                  XI(J,J) = QRAUXI(J)
                  TR = -ML_WDOTCR(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
                  TI = -ML_WDOTCI(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
                  CALL ML_WDIV(TR,TI,XR(J,J),XI(J,J),TR,TI)
                  CALL ML_WAXPY(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
                  XR(J,J) = TEMPR
                  XI(J,J) = TEMPI
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
!
!        SET UP TO COMPUTE B, RSD, OR XB.
!
         IF (CB) CALL ML_WCOPY(K,QTYR,QTYI,1,BR,BI,1)
         KP1 = K + 1
         IF (CXB) CALL ML_WCOPY(K,QTYR,QTYI,1,XBR,XBI,1)
         IF (CR .AND. K .LT. N)CALL ML_WCOPY(N-K,QTYR(KP1),QTYI(KP1),1,RSDR(KP1),RSDI(KP1),1)
         IF (.NOT.CXB .OR. KP1 .GT. N) GOTO 160
            DO 150 I = KP1, N
               XBR(I) = 0.0D0
               XBI(I) = 0.0D0
  150       CONTINUE
  160    CONTINUE
         IF (.NOT.CR) GOTO 180
            DO 170 I = 1, K
               RSDR(I) = 0.0D0
               RSDI(I) = 0.0D0
  170       CONTINUE
  180    CONTINUE
         IF (.NOT.CB) GOTO 230
!
!           COMPUTE B.
!
            DO 210 JJ = 1, K
               J = K - JJ + 1
               IF (CABS1(XR(J,J),XI(J,J)) .NE. 0.0D0) GOTO 190
                  INFO = J
!                 ......EXIT
!           ......EXIT
                  GOTO 220
  190          CONTINUE
               CALL ML_WDIV(BR(J),BI(J),XR(J,J),XI(J,J),BR(J),BI(J))
               IF (J .EQ. 1) GOTO 200
                  TR = -BR(J)
                  TI = -BI(J)
                  CALL ML_WAXPY(J-1,TR,TI,XR(1,J),XI(1,J),1,BR,BI,1)
  200          CONTINUE
  210       CONTINUE
  220       CONTINUE
  230    CONTINUE
         IF (.NOT.CR .AND. .NOT.CXB) GOTO 280
!
!           COMPUTE RSD OR XB AS REQUIRED.
!
            DO 270 JJ = 1, JU
               J = JU - JJ + 1
               IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 260
                  TEMPR = XR(J,J)
                  TEMPI = XI(J,J)
                  XR(J,J) = QRAUXR(J)
                  XI(J,J) = QRAUXI(J)
                  IF (.NOT.CR) GOTO 240
                  TR = -ML_WDOTCR(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
                  TI = -ML_WDOTCI(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
                  CALL ML_WDIV(TR,TI,XR(J,J),XI(J,J),TR,TI)
                  CALL ML_WAXPY(N-J+1,TR,TI,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
  240             CONTINUE
                  IF (.NOT.CXB) GOTO 250
                   TR = -ML_WDOTCR(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
                   TI = -ML_WDOTCI(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
                   CALL ML_WDIV(TR,TI,XR(J,J),XI(J,J),TR,TI)
                   CALL ML_WAXPY(N-J+1,TR,TI,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
  250             CONTINUE
                  XR(J,J) = TEMPR
                  XI(J,J) = TEMPI
  260          CONTINUE
  270       CONTINUE
  280    CONTINUE
  290 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_MAGIC(A,LDA,N)
!
!     ALGORITHMS FOR MAGIC SQUARES TAKEN FROM
!        MATHEMATICAL RECREATIONS AND ESSAYS, 12TH ED.,
!        BY W. W. ROUSE BALL AND H. S. M. COXETER
!
      DOUBLE PRECISION A(LDA,N),T
!
      IF (MOD(N,4) .EQ. 0) GOTO 100
      IF (MOD(N,2) .EQ. 0) M = N/2
      IF (MOD(N,2) .NE. 0) M = N
!
!     ODD ORDER OR UPPER CORNER OF EVEN ORDER
!
      DO 20 J = 1,M
         DO 10 I = 1,M
            A(I,J) = 0
   10    CONTINUE
   20 CONTINUE
      I = 1
      J = (M+1)/2
      MM = M*M
      DO 40 K = 1, MM
         A(I,J) = K
         I1 = I-1
         J1 = J+1
         IF(I1.LT.1) I1 = M
         IF(J1.GT.M) J1 = 1
         IF(IDINT(A(I1,J1)).EQ.0) GOTO 30
            I1 = I+1
            J1 = J
   30    I = I1
         J = J1
   40 CONTINUE
      IF (MOD(N,2) .NE. 0) RETURN
!
!     REST OF EVEN ORDER
!
      T = M*M
      DO 60 I = 1, M
         DO 50 J = 1, M
            IM = I+M
            JM = J+M
            A(I,JM) = A(I,J) + 2*T
            A(IM,J) = A(I,J) + 3*T
            A(IM,JM) = A(I,J) + T
   50    CONTINUE
   60 CONTINUE
      M1 = (M-1)/2
      IF (M1.EQ.0) RETURN
      DO 70 J = 1, M1
         CALL ML_RSWAP(M,A(1,J),1,A(M+1,J),1)
   70 CONTINUE
      M1 = (M+1)/2
      M2 = M1 + M
      CALL ML_RSWAP(1,A(M1,1),1,A(M2,1),1)
      CALL ML_RSWAP(1,A(M1,M1),1,A(M2,M1),1)
      M1 = N+1-(M-3)/2
      IF(M1.GT.N) RETURN
      DO 80 J = M1, N
         CALL ML_RSWAP(M,A(1,J),1,A(M+1,J),1)
   80 CONTINUE
      RETURN
!
!     DOUBLE EVEN ORDER
!
  100 K = 1
      DO 120 I = 1, N
         DO 110 J = 1, N
            A(I,J) = K
            IF (MOD(I,4)/2 .EQ. MOD(J,4)/2) A(I,J) = N*N+1 - K
            K = K+1
  110    CONTINUE
  120 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_BASE(X,B,EPS,S,N)
      DOUBLE PRECISION X,B,EPS,S(*),T
!
!     STORE BASE B REPRESENTATION OF X IN S(1:N)
!
      INTEGER PLUS,MINUS,DOT,ZERO,COMMA
      save plus, minus, dot, zero, comma
      DATA PLUS/41/,MINUS/42/,DOT/47/,ZERO/0/,COMMA/48/
      L = 1
      IF (X .GE. 0.0D0) S(L) = PLUS
      IF (X .LT. 0.0D0) S(L) = MINUS
      S(L+1) = ZERO
      S(L+2) = DOT
      X = DABS(X)
      IF (X .NE. 0.0D0) THEN
         K = DLOG(X)/DLOG(B)
      ELSE
         K = 0
      ENDIF
      IF (X .GT. 1.0D0) K = K + 1
      X = X/B**K
      IF (B*X .GE. B) K = K + 1
      IF (B*X .GE. B) X = X/B
      IF (EPS .NE. 0.0D0)THEN
         M = (-1)*DLOG(EPS)/DLOG(B) + 4
      ELSE
         M = 54
      ENDIF
      DO 10 L = 4, M
      X = B*X
      J = IDINT(X)
      S(L) = DFLOAT(J)
      X = X - S(L)
   10 CONTINUE
      S(M+1) = COMMA
      IF (K .GE. 0) S(M+2) = PLUS
      IF (K .LT. 0) S(M+2) = MINUS
      T = DABS(DFLOAT(K))
      N = M + 3
      IF (T .GE. B) N = N + IDINT(DLOG(T)/DLOG(B))
      L = N
   20 J = IDINT(DMOD(T,B))
      S(L) = DFLOAT(J)
      L = L - 1
      T = T/B
      IF (L .GE. M+3) GOTO 20
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WMUL(AR,AI,BR,BI,CR,CI)
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI,T,ML_FLOP
!     C = A*B
      T = AR*BI + AI*BR
      IF (T .NE. 0.0D0) T = ML_FLOP(T)
      CR = ML_FLOP(AR*BR - AI*BI)
      CI = T
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WDIV(AR,AI,BR,BI,CR,CI)
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI
!     C = A/B
      DOUBLE PRECISION S,D,ARS,AIS,BRS,BIS,ML_FLOP
      S = DABS(BR) + DABS(BI)
      IF (S .EQ. 0.0D0) CALL ML_ERROR(27)
      IF (S .EQ. 0.0D0) RETURN
      ARS = AR/S
      AIS = AI/S
      BRS = BR/S
      BIS = BI/S
      D = BRS**2 + BIS**2
      CR = ML_FLOP((ARS*BRS + AIS*BIS)/D)
      CI = (AIS*BRS - ARS*BIS)/D
      IF (CI .NE. 0.0D0) CI = ML_FLOP(CI)
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WSIGN(XR,XI,YR,YI,ZR,ZI)
      DOUBLE PRECISION XR,XI,YR,YI,ZR,ZI,ML_PYTHAG,T
!     IF Y .NE. 0, Z = X*Y/ABS(Y)
!     IF Y .EQ. 0, Z = X
      T = ML_PYTHAG(YR,YI)
      ZR = XR
      ZI = XI
      IF (T .NE. 0.0D0) CALL ML_WMUL(YR/T,YI/T,ZR,ZI,ZR,ZI)
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WSQRT(XR,XI,YR,YI)
      DOUBLE PRECISION XR,XI,YR,YI,S,TR,TI,ML_PYTHAG,ML_FLOP
!     Y = SQRT(X) WITH YR .GE. 0.0 AND SIGN(YI) .EQ. SIGN(XI)
!
      TR = XR
      TI = XI
      S = DSQRT(0.5D0*(ML_PYTHAG(TR,TI) + DABS(TR)))
      IF (TR .GE. 0.0D0) YR = ML_FLOP(S)
      IF (TI .LT. 0.0D0) S = -S
      IF (TR .LE. 0.0D0) YI = ML_FLOP(S)
      IF (TR .LT. 0.0D0) YR = ML_FLOP(0.5D0*(TI/YI))
      IF (TR .GT. 0.0D0) YI = ML_FLOP(0.5D0*(TI/YR))
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WLOG(XR,XI,YR,YI)
      DOUBLE PRECISION XR,XI,YR,YI,T,R,ML_PYTHAG
!     Y = LOG(X)
      R = ML_PYTHAG(XR,XI)
      IF (R .EQ. 0.0D0) CALL ML_ERROR(32)
      IF (R .EQ. 0.0D0) RETURN
      T = DATAN2(XI,XR)
      IF (XI.EQ.0.0D0 .AND. XR.LT.0.0D0) T = DABS(T)
      YR = DLOG(R)
      YI = T
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WATAN(XR,XI,YR,YI)
!     Y = ATAN(X) = (I/2)*LOG((I+X)/(I-X))
      DOUBLE PRECISION XR,XI,YR,YI,TR,TI
      IF (XI .NE. 0.0D0) GOTO 10
         YR = DATAN2(XR,1.0D0)
         YI = 0.0D0
         RETURN
   10 IF (XR.NE.0.0D0 .OR. DABS(XI).NE.1.0D0) GOTO 20
         CALL ML_ERROR(32)
         RETURN
   20 CALL ML_WDIV(XR,1.0D0+XI,-XR,1.0D0-XI,TR,TI)
      CALL ML_WLOG(TR,TI,TR,TI)
      YR = -(TI/2.0D0)
      YI = TR/2.0D0
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WCOPY(N,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION XR(*),XI(*),YR(*),YI(*)
      IF (N .LE. 0) RETURN
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         YR(IY) = XR(IX)
         YI(IY) = XI(IX)
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WSET(N,XR,XI,YR,YI,INCY)
      INTEGER N,INCY
      DOUBLE PRECISION XR,XI,YR(*),YI(*)
      IY = 1
      IF (N .LE. 0 ) RETURN
      DO 10 I = 1,N
         YR(IY) = XR
         YI(IY) = XI
         IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WSWAP(N,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION XR(*),XI(*),YR(*),YI(*),T
      IF (N .LE. 0) RETURN
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         T = XR(IX)
         XR(IX) = YR(IY)
         YR(IY) = T
         T = XI(IX)
         XI(IX) = YI(IY)
         YI(IY) = T
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_RSET(N,DX,DY,INCY)
!
!     COPIES A SCALAR, DX, TO A SCALAR, DY.
      DOUBLE PRECISION DX,DY(*)
!
      IF (N.LE.0) RETURN
      IY = 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX
        IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_RSWAP(N,X,INCX,Y,INCY)
      DOUBLE PRECISION X(*),Y(*),T
      IF (N .LE. 0) RETURN
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX+1
      IF (INCY.LT.0) IY = (-N+1)*INCY+1
      DO 10 I = 1, N
         T = X(IX)
         X(IX) = Y(IY)
         Y(IY) = T
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_RROT(N,DX,INCX,DY,INCY,C,S)
!
!     APPLIES A PLANE ROTATION.
      DOUBLE PRECISION DX(*),DY(*),DTEMP,C,S,ML_FLOP
      INTEGER I,INCX,INCY,IX,IY,N
!
      IF (N.LE.0) RETURN
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = ML_FLOP(C*DX(IX) + S*DY(IY))
        DY(IY) = ML_FLOP(C*DY(IY) - S*DX(IX))
        DX(IX) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_RROTG(DA,DB,C,S)
!
!     CONSTRUCT GIVENS PLANE ROTATION.
!
      DOUBLE PRECISION DA,DB,C,S,RHO,ML_PYTHAG,ML_FLOP,R,Z
!
      RHO = DB
      IF ( DABS(DA) .GT. DABS(DB) ) RHO = DA
      C = 1.0D0
      S = 0.0D0
      Z = 1.0D0
      R = ML_FLOP(DSIGN(ML_PYTHAG(DA,DB),RHO))
      IF (R .NE. 0.0D0) C = ML_FLOP(DA/R)
      IF (R .NE. 0.0D0) S = ML_FLOP(DB/R)
      IF ( DABS(DA) .GT. DABS(DB) ) Z = S
      IF (DABS(DB) .GE. DABS(DA) .AND. C .NE. 0.0D0)Z = ML_FLOP(1.0D0/C)
      DA = R
      DB = Z
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WRSCAL(N,S,XR,XI,INCX)
      DOUBLE PRECISION S,XR(*),XI(*),ML_FLOP
      IF (N .LE. 0) RETURN
      IX = 1
      DO 10 I = 1, N
         XR(IX) = ML_FLOP(S*XR(IX))
         IF (XI(IX) .NE. 0.0D0) XI(IX) = ML_FLOP(S*XI(IX))
         IX = IX + INCX
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WSCAL(N,SR,SI,XR,XI,INCX)
      DOUBLE PRECISION SR,SI,XR(*),XI(*)
      IF (N .LE. 0) RETURN
      IX = 1
      DO 10 I = 1, N
         CALL ML_WMUL(SR,SI,XR(IX),XI(IX),XR(IX),XI(IX))
         IX = IX + INCX
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_WAXPY(N,SR,SI,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION SR,SI,XR(*),XI(*),YR(*),YI(*),ML_FLOP
      IF (N .LE. 0) RETURN
      IF (SR .EQ. 0.0D0 .AND. SI .EQ. 0.0D0) RETURN
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         YR(IY) = ML_FLOP(YR(IY) + SR*XR(IX) - SI*XI(IX))
         YI(IY) = YI(IY) + SR*XI(IX) + SI*XR(IX)
         IF (YI(IY) .NE. 0.0D0) YI(IY) = ML_FLOP(YI(IY))
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_PYTHAG(A,B)
      use M_journal, only : journal
      DOUBLE PRECISION A,B
      INTEGER DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
      COMMON /IOP/ DDT,ERR,FMT,LCT,LIN,LPT,HIO,RIO,RTE,WTE,FE
      SAVE /IOP/
      DOUBLE PRECISION P,Q,R,S,T

      P = DMAX1(DABS(A),DABS(B))
      Q = DMIN1(DABS(A),DABS(B))

      !------- DEBUG
      IF (DDT .EQ. 25) THEN
         CALL JOURNAL('sc','*ml_pythag* a) P=',real(P)) ! debug 25
         CALL JOURNAL('sc','*ml_pythag* a) Q=',real(Q)) ! debug 25
      ENDIF

      IF (Q .EQ. 0.0D0) GOTO 20

   10 CONTINUE
      R = (Q/P)**2
      T = 4.0D0 + R
      IF (T .EQ. 4.0D0) GOTO 20
      S = R/T
      P = P + 2.0D0*P*S
      Q = Q*S
      !------- DEBUG
      IF (DDT .EQ. 25) then
         CALL JOURNAL('sc','*ml_pythag* b) P=',real(P)) ! debug 25
         CALL JOURNAL('sc','*ml_pythag* b) Q=',real(Q)) ! debug 25
      ENDIF
      GOTO 10

   20 CONTINUE
      ML_PYTHAG = P
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_URAND(IY)
      INTEGER IY
!
!  ML_URAND IS A UNIFORM RANDOM NUMBER GENERATOR BASED  ON  THEORY  AND
!  SUGGESTIONS  GIVEN  IN  D.E. KNUTH (1969),  VOL  2.  THE INTEGER  IY
!  SHOULD BE INITIALIZED TO AN ARBITRARY INTEGER PRIOR TO THE FIRST
!  CALL TO ML_URAND. THE CALLING PROGRAM SHOULD NOT ALTER THE VALUE OF
!  IY BETWEEN  SUBSEQUENT CALLS TO ML_URAND.  VALUES OF ML_URAND WILL BE
!  RETURNED IN THE INTERVAL (0,1).
!
      INTEGER IA,IC,ITWO,M2,M,MIC
      DOUBLE PRECISION HALFM,S
      DOUBLE PRECISION DATAN,DSQRT
      save m2, itwo,ia,ic,mic,s
      DATA M2/0/,ITWO/2/
!-----------------------------------------------------------------------
      IF (M2 .NE. 0) GOTO 20
!
!  IF FIRST ENTRY, COMPUTE MACHINE INTEGER WORD LENGTH
!
      M = 1
   10 CONTINUE
      M2 = M
      M = ITWO*M2
      IF (M .GT. M2) GOTO 10
      HALFM = M2
!
!  COMPUTE MULTIPLIER AND INCREMENT FOR LINEAR CONGRUENTIAL METHOD
!
      IA = 8*IDINT(HALFM*DATAN(1.D0)/8.D0) + 5
      IC = 2*IDINT(HALFM*(0.5D0-DSQRT(3.D0)/6.D0)) + 1
      MIC = (M2 - IC) + M2
!
!  S IS THE SCALE FACTOR FOR CONVERTING TO FLOATING POINT
!
      S = 0.5D0/HALFM
!-----------------------------------------------------------------------
!
!  COMPUTE NEXT RANDOM NUMBER
!
   20 CONTINUE
      IY = IY*IA
!
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHICH DO NOT ALLOW
!  INTEGER OVERFLOW ON ADDITION
!
      IF (IY .GT. MIC) IY = (IY - M2) - M2
!
      IY = IY + IC
!
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE THE
!  WORD LENGTH FOR ADDITION IS GREATER THAN FOR MULTIPLICATION
!
      IF (IY/2 .GT. M2) IY = (IY - M2) - M2
!
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE INTEGER
!  OVERFLOW AFFECTS THE SIGN BIT
!
      IF (IY .LT. 0) IY = (IY + M2) + M2
      ML_URAND = DFLOAT(IY)*S
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_WNRM2(N,XR,XI,INCX)
      DOUBLE PRECISION XR(*),XI(*),ML_PYTHAG,S
!     NORM2(X)
      S = 0.0D0
      IF (N .LE. 0) GOTO 20
      IX = 1
      DO 10 I = 1, N
         S = ML_PYTHAG(S,XR(IX))
         S = ML_PYTHAG(S,XI(IX))
         IX = IX + INCX
   10 CONTINUE
   20 ML_WNRM2 = S
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_WASUM(N,XR,XI,INCX)
      DOUBLE PRECISION XR(*),XI(*),S,ML_FLOP
!     NORM1(X)
      S = 0.0D0
      IF (N .LE. 0) GOTO 20
      IX = 1
      DO 10 I = 1, N
         S = ML_FLOP(S + DABS(XR(IX)) + DABS(XI(IX)))
         IX = IX + INCX
   10 CONTINUE
   20 ML_WASUM = S
      RETURN
      END
!-----------------------------------------------------------------------
      INTEGER FUNCTION ML_IWAMAX(N,XR,XI,INCX)
      DOUBLE PRECISION XR(*),XI(*),S,P
!     INDEX OF NORMINF(X)
      K = 0
      IF (N .LE. 0) GOTO 20
      K = 1
      S = 0.0D0
      IX = 1
      DO 10 I = 1, N
         P = DABS(XR(IX)) + DABS(XI(IX))
         IF (P .GT. S) K = I
         IF (P .GT. S) S = P
         IX = IX + INCX
   10 CONTINUE
   20 ML_IWAMAX = K
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_WDOTUR(N,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION XR(*),XI(*),YR(*),YI(*),S,ML_FLOP
      S = 0.0D0
      IF (N .LE. 0) GOTO 20
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         S = ML_FLOP(S + XR(IX)*YR(IY) - XI(IX)*YI(IY))
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
   20 ML_WDOTUR = S
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_WDOTUI(N,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION XR(*),XI(*),YR(*),YI(*),S,ML_FLOP
      S = 0.0D0
      IF (N .LE. 0) GOTO 20
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         S = S + XR(IX)*YI(IY) + XI(IX)*YR(IY)
         IF (S .NE. 0.0D0) S = ML_FLOP(S)
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
   20 ML_WDOTUI = S
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_WDOTCR(N,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION XR(*),XI(*),YR(*),YI(*),S,ML_FLOP
      S = 0.0D0
      IF (N .LE. 0) GOTO 20
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         S = ML_FLOP(S + XR(IX)*YR(IY) + XI(IX)*YI(IY))
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
   20 ML_WDOTCR = S
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_WDOTCI(N,XR,XI,INCX,YR,YI,INCY)
      DOUBLE PRECISION XR(*),XI(*),YR(*),YI(*),S
      DOUBLE PRECISION ML_FLOP
      S = 0.0D0
      IF (N .LE. 0) GOTO 20
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         S = S + XR(IX)*YI(IY) - XI(IX)*YR(IY)
         IF (S .NE. 0.0D0) S = ML_FLOP(S)
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
   20 ML_WDOTCI = S
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_ROUND(X)
      DOUBLE PRECISION X,Y,Z,E,H
      save H
      DATA H/1.0D9/
      Z = DABS(X)
      Y = Z + 1.0D0
      IF (Y .EQ. Z) GOTO 40
      Y = 0.0D0
      E = H
   10 IF (E .GE. Z) GOTO 20
         E = 2.0D0*E
         GOTO 10
   20 IF (E .LE. H) GOTO 30
         IF (E .LE. Z) Y = Y + E
         IF (E .LE. Z) Z = Z - E
         E = E/2.0D0
         GOTO 20
   30 Z = IDINT(Z + 0.5D0)
      Y = Y + Z
      IF (X .LT. 0.0D0) Y = -Y
      ML_ROUND = Y
      RETURN
   40 ML_ROUND = X
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_DFLOAT(I)
!
!   THIS IS THE AMIGA FUNCTION WHICH CONVERTS INTEGERS TO DOUBLE FLOATS
!
      IMPLICIT NONE
      INTEGER I
      ML_DFLOAT = DBLE(I)
      RETURN
      END
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION ML_FLOP(X)
      DOUBLE PRECISION X
!     SYSTEM DEPENDENT FUNCTION
!     COUNT AND POSSIBLY CHOP EACH FLOATING POINT OPERATION
!     FLP(1) IS FLOP COUNTER
!     FLP(2) IS NUMBER OF PLACES TO BE CHOPPED
!
      INTEGER SYM,SYN(4),BUF(256),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
      COMMON /COM/ SYM,SYN,BUF,CHRA,FLP,FIN,FUN,LHS,RHS,RAN
!
      DOUBLE PRECISION MASK(14),XX,MM
      LOGICAL LX(2),LM(2)
      EQUIVALENCE (LX(1),XX),(LM(1),MM)
      equivalence (MASK(1),mas(1,1))
!==============================================
!      real mas(2,14)
!      save mas
!      data mas/                     &
!     & Z'ffffffff',Z'fff0ffff',     &
!     & Z'ffffffff',Z'ff00ffff',     &
!     & Z'ffffffff',Z'f000ffff',     &
!     & Z'ffffffff',Z'0000ffff',     &
!     & Z'ffffffff',Z'0000fff0',     &
!     & Z'ffffffff',Z'0000ff00',     &
!     & Z'ffffffff',Z'0000f000',     &
!     & Z'ffffffff',Z'00000000',     &
!     & Z'fff0ffff',Z'00000000',     &
!     & Z'ff00ffff',Z'00000000',     &
!     & Z'f000ffff',Z'00000000',     &
!     & Z'0000ffff',Z'00000000',     &
!     & Z'0000fff0',Z'00000000',     &
!     & Z'0000ff80',Z'00000000'/
!==============================================
      real,save :: mas(2,14)=reshape([ &
     & transfer(Z'ffffffff',0.0),transfer(Z'fff0ffff',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'ff00ffff',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'f000ffff',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'0000ffff',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'0000fff0',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'0000ff00',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'0000f000',0.0),     &
     & transfer(Z'ffffffff',0.0),transfer(Z'00000000',0.0),     &
     & transfer(Z'fff0ffff',0.0),transfer(Z'00000000',0.0),     &
     & transfer(Z'ff00ffff',0.0),transfer(Z'00000000',0.0),     &
     & transfer(Z'f000ffff',0.0),transfer(Z'00000000',0.0),     &
     & transfer(Z'0000ffff',0.0),transfer(Z'00000000',0.0),     &
     & transfer(Z'0000fff0',0.0),transfer(Z'00000000',0.0),     &
     & transfer(Z'0000ff80',0.0),transfer(Z'00000000',0.0)],shape(mas))
!==============================================
!
      FLP(1) = FLP(1) + 1
      K = FLP(2)
      ML_FLOP = X
      IF (K .LE. 0) RETURN
      ML_FLOP = 0.0D0
      IF (K .GE. 15) RETURN
      XX = X
      MM = MASK(K)
      LX(1) = LX(1) .AND. LM(1)
      LX(2) = LX(2) .AND. LM(2)
      ML_FLOP = XX
      RETURN
      END
!-----------------------------------------------------------------------
!      W   H  Y
!      87 72 89 32
! DECIMAL
! | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
! | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
! | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
! | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
! | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
! | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
! | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
! | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
! | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
! | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
! | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
! | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
! | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
! |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
! |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
! |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
! HEXADECIMAL
! | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
! | 08 bs | 09 ht | 0a nl | 0b vt | 0c np | 0d cr | 0e so | 0f si |
! | 10 dle| 11 dc1| 12 dc2| 13 dc3| 14 dc4| 15 nak| 16 syn| 17 etb|
! | 18 can| 19 em | 1a sub| 1b esc| 1c fs | 1d gs | 1e rs | 1f us |
! | 20 sp | 21  ! | 22  " | 23  # | 24  $ | 25  % | 26  & | 27  ' |
! | 28  ( | 29  ) | 2a  * | 2b  + | 2c  , | 2d  - | 2e  . | 2f  / |
! | 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7 |
! | 38  8 | 39  9 | 3a  : | 3b  ; | 3c  < | 3d  = | 3e  > | 3f  ? |
! | 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G |
! | 48  H | 49  I | 4a  J | 4b  K | 4c  L | 4d  M | 4e  N | 4f  O |
! | 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W |
! | 58  X | 59  Y | 5a  Z | 5b  [ | 5c  \ | 5d  ] | 5e  ^ | 5f  _ |
! | 60  ` | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g |
! | 68  h | 69  i | 6a  j | 6b  k | 6c  l | 6d  m | 6e  n | 6f  o |
! | 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w |
! | 78  x | 79  y | 7a  z | 7b  { | 7c  | | 7d  } | 7e  ~ | 7f del|
! OCTAL
! |000 nul|001 soh|002 stx|003 etx|004 eot|005 enq|006 ack|007 bel|
! |010 bs |011 ht |012 nl |013 vt |014 np |015 cr |016 so |017 si |
! |020 dle|021 dc1|022 dc2|023 dc3|024 dc4|025 nak|026 syn|027 etb|
! |030 can|031 em |032 sub|033 esc|034 fs |035 gs |036 rs |037 us |
! |040 sp |041  ! |042  " |043  # |044  $ |045  % |046  & |047  ' |
! |050  ( |051  ) |052  * |053  + |054  , |055  - |056  . |057  / |
! |060  0 |061  1 |062  2 |063  3 |064  4 |065  5 |066  6 |067  7 |
! |070  8 |071  9 |072  : |073  ; |074  < |075  = |076  > |077  ? |
! |100  @ |101  A |102  B |103  C |104  D |105  E |106  F |107  G |
! |110  H |111  I |112  J |113  K |114  L |115  M |116  N |117  O |
! |120  P |121  Q |122  R |123  S |124  T |125  U |126  V |127  W |
! |130  X |131  Y |132  Z |133  [ |134  \ |135  ] |136  ^ |137  _ |
! |140  ` |141  a |142  b |143  c |144  d |145  e |146  f |147  g |
! |150  h |151  i |152  j |153  k |154  l |155  m |156  n |157  o |
! |160  p |161  q |162  r |163  s |164  t |165  u |166  v |167  w |
! |170  x |171  y |172  z |173  { |174  | |175  } |176  ~ |177 del|
!-----------------------------------------------------------------------
      BLOCKDATA ML_CHARS
      PARAMETER (IALF=78)
      CHARACTER CH_A*(IALF),CH_B*(IALF)
      COMMON /ML_CHARSZ/ CH_A, CH_B
      SAVE /ML_CHARSZ/
!                0         1         2         3         4         '5
!                01234567890123456789012345678901234567890123456789'01
      DATA CH_A/'0123456789abcdefghijklmnopqrstuvwxyz ();:+-*/\=.,''<>ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
!     ALTERNATE CHARACTER SET
!                0         1         2         3         4         5
!                0123456789012345678901234567890123456789012345678901
      DATA CH_B/'0123456789abcdefghijklmnopqrstuvwxyz {};|+-*/$=@,"[]ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      END
!-----------------------------------------------------------------------
      SUBROUTINE ML_2CHARS(DINTS,M,ICOUNT,IROW,STRING)
      use M_journal, only : journal
      ! convert a row of an array of ML integers into a character variable
      ! DINTS(M,ICOUNT) is a DP array holding integer values to convert to letters
      ! ICOUNT is number of letters to convert
      ! IROW is row number to convert
      ! STRING is returned strng
      DOUBLE PRECISION DINTS(M,ICOUNT)
      CHARACTER STRING*(*)
      INTEGER ICOUNT

      EXTERNAL ML_CHARS
      PARAMETER (IALF=78)
      CHARACTER CH_A*(IALF),CH_B*(IALF)
      COMMON /ML_CHARSZ/ CH_A, CH_B
      SAVE /ML_CHARSZ/

      ITOP=LEN(STRING)
      STRING(1:ITOP)=' '
      IF(ICOUNT.GT.ITOP)THEN
         call journal('*ML_2CHARS* error: STRING TOO SHORT')
      ELSE
         ITOP=ICOUNT
      ENDIF
      DO I10=1,ITOP
         IC=DINTS(IROW,I10)
         IF(IC.GE.0.AND.IC.LE.51)THEN
            IC=IC+1
            STRING(I10:I10)=CH_A(IC:IC)
         ELSE
            call journal('sc','*ML_2CHARS* error: BAD CHARACTER NUMBER ',IC)
         ENDIF
      ENDDO
      END SUBROUTINE ML_2CHARS
!-----------------------------------------------------------------------
      subroutine str2buf(string,buf,lrecl) ! convert string to hollerith
      ! g95 compiler does not support Hollerith, KLUDGE to think about it
      integer lrecl
      integer buf(lrecl)
      character string*(*)
      do i10=1,lrecl
         buf(i10)=ichar(string(i10:i10))+538976304-48
      enddo
      end subroutine str2buf
!----------------------------------------------------------------
!      DO 20 I = 1, ALFL ! convert character to hollerith
!         ALFA(I) = ALPHA(I)
!         ALFB(I) = ALPHB(I)
!          ! g95 will not take this
!         READ(CH_A(I:I),'(A1)')ALFA(I) ! convert character to hollerith
!         READ(CH_B(I:I),'(A1)')ALFB(I) ! convert character to hollerith
!         ALFA(I)=ICHAR(CH_A(I:I))+538976304-48
!         ALFB(I)=ICHAR(CH_B(I:I))+538976304-48
!   20 CONTINUE
!----------------------------------------------------------------
!      INTEGER ALPHA(IALF),ALPHB(IALF)
!      DATA ALPHA /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,
!     $    1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI,1HJ,
!     $    1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR,1HS,1HT,
!     $    1HU,1HV,1HW,1HX,1HY,1HZ,1H ,1H(,1H),1H;,
!     $    1H:,1H+,1H-,1H*,1H/,1H\,1H=,1H.,1H,,1H',
!     $    1H<,1H>/
!     ALTERNATE CHARACTER SET
!      DATA ALPHB /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,
!     $    1Ha,1Hb,1Hc,1Hd,1He,1Hf,1Hg,1Hh,1Hi,1Hj,
!     $    1Hk,1Hl,1Hm,1Hn,1Ho,1Hp,1Hq,1Hr,1Hs,1Ht,
!     $    1Hu,1Hv,1Hw,1Hx,1Hy,1Hz,1H ,1H(,1H),1H;,
!     $    1H|,1H+,1H-,1H*,1H/,1H$,1H=,1H.,1H,,1H",
!     $    1H[,1H]/
!
!      SAVE ALPHB, ALPHA
!-----------------------------------------------------------------------
      subroutine buf2str(string,buf,lrecl) ! convert string to hollerith
      integer lrecl
      integer buf(lrecl)
      character string*(*)
      string(:)=' '
      do i10=1,lrecl
         string(i10:i10)=char(buf(i10)-538976304+48)
      enddo
      end subroutine buf2str
!.......................................................................
!.......................................................................
!=======================================================================--------
subroutine appnum(rval,string,ilen,ierr)
use M_journal, only : journal
use M_strings, only: value_to_string
implicit none
character(len=*),parameter :: ident="@(#)appnum(3f): subroutine returns a string given a prefix string and a real value"
!     Input string should have at least 20 blank characters at end
!     03/16/87 J. S. Urban
!
!-------------------------------------------------------------------------------
      real,intent(in)                :: rval   ! input value to convert to characters and append to STRING
      character(len=*)               :: string ! string to append string value of RVAL to
      integer,intent(out)            :: ilen   ! new length of STRING on output
      integer,intent(out)            :: ierr   ! flag to indicate if error occurred
!-------------------------------------------------------------------------------
      external                       :: jun
      external                       :: junr
      intrinsic                      :: len_trim
!-------------------------------------------------------------------------------
      character(len=20)              :: chars  ! scratch string to store string representation of RVAL in
      integer                        :: ilen2  ! length of string created by converting RVAL to a string
!-------------------------------------------------------------------------------
      ierr=0                                   ! initialize error flag to indicate no errors
      chars=' '                                ! initialize scratch string to all blanks
      ilen=len_trim(string(:len(string)))      ! find last non-blank character in initial input string

      call value_to_string(rval,chars,ilen2,ierr)         ! convert RVAL to a string in CHARS
      if(ilen+ilen2.gt.len(string))then
         call journal('sc','*appnum* error: input string variable too short to store output string')
         call journal('sc','*appnum* '//string,rval)
         ierr=-1
      else
         string=string(:ilen)//chars(:ilen2)   ! append CHARS to STRING
         ilen=ilen+ilen2                       ! calculate length of new string
      endif
end subroutine appnum
!=======================================================================
