      PROGRAM CHECKALL
!-----------------------------------------------------------------------------------------------------------------------------------
!     CHECKALL  is  a program that performs complete regression testing of the
!     W‐IF97 steam table routines, including all properties  as  functions  of
!     all  programmed  independent  properties,  in  all  regions;  basic  and
!     backward equations; all units provided; and all high and low limits  and
!     error  codes.   Four  input  files  are  expected  to be read, with each
!     providing the properties to which the calculated W‐IF97  properties  are
!     to be compared.
!                                               Toby Burnett, March 2008
!-----------------------------------------------------------------------------------------------------------------------------------
      implicit none

      real(kind=8) :: EEMX(9,5),EEMN(9,5),AAMX(9,5)
      real(kind=8) :: PATM

      integer :: J, I, IER, IEND, NCASE 
      integer :: NERR=0           ! count of number of errors encountered
      integer :: IP,IT,IH,IV,IS

      CHARACTER(len=78) ::  TITLE
      CHARACTER(len=6)  ::  XOUT(5)

      DATA (XOUT(I),I=1,5) / ' PTOUT',' PHOUT',' PSOUT',' PRAND','SATOUT'/

      PATM=0.0D0
      
      DO J=1,5
         DO I=1,9
            EEMX(I,J)=0.0d0
            EEMN(I,J)=0.0d0
         ENDDO
      ENDDO

      OPEN (11,FILE='SUMMARY')
      
      OPEN (2,FILE='TestSI.in',STATUS='OLD',IOSTAT=IER)
      IF (IER .NE. 0) THEN
          WRITE (*,*)  '  TestSI.in file is NOT available'
          WRITE (11,*) '  TestSI.in file is NOT available'
          call endoffile()
      ENDIF
      OPEN (3,FILE='TestSat.in',STATUS='OLD',IOSTAT=IER)
      IF (IER .NE. 0) THEN
          WRITE (*,*)  '  TestSat.in file is NOT available'
          WRITE (11,*) '  TestSat.in file is NOT available'
          call endoffile()
      ENDIF
      OPEN (4,FILE='TestUnit.in',STATUS='OLD',IOSTAT=IER)
      IF (IER .NE. 0) THEN
          WRITE (*,*)  '  TestUnit.in file is NOT available'
          WRITE (11,*) '  TestUnit.in file is NOT available'
          call endoffile()
      ENDIF
      OPEN (1,FILE='TestLim.in',STATUS='OLD',IOSTAT=IER)
      IF (IER .NE. 0) THEN
          WRITE (*,*)  '  TestLim.in file is NOT available'
          WRITE (11,*) '  TestLim.in file is NOT available'
          call endoffile()
      ENDIF

      write(*,*)  ' Output files will be named:                                 '
      write(*,*)  '      PTOUT - Properties as functions of P & T               '
      write(*,*)  '      PHOUT - Properties as functions of P & h               '
      write(*,*)  '      PSOUT - Properties as functions of P & s               '
      write(*,*)  '      PRAND - Transport properties as functions of P & T     '
      write(*,*)  '     SATOUT - Saturation properties                          '
      write(*,*)  '    SUMMARY - Max deviations (absolute value) from input file'

      ! Read title line from input file and write on output files
      READ (2,FMT='(A)') TITLE

      WRITE (11,FMT='(1X,"SUMMARY: ",A)') TITLE

      OPEN (8,FILE='PTOUT')
      WRITE (8,FMT='(1X,"PTOUT:  ",A)') TITLE
      WRITE (8,209)
      WRITE (8,201)

      OPEN (9,FILE='PHOUT')
      WRITE (9,FMT='(1X,"PHOUT:  ",A)') TITLE
      WRITE (9,209)
      WRITE (9,201)

      OPEN (10,FILE='PSOUT')
      WRITE (10,FMT='(1X,"PSOUT: ",A)') TITLE
      WRITE (10,209)
      WRITE (10,201)

      OPEN (12,FILE='PRAND')
      WRITE (12,FMT='(1X,"PRAND: ",A)') TITLE

      OPEN (13,FILE='SCRATCH')

      OPEN (14,FILE='SATOUT')
      WRITE (14,FMT='(1X,"SATOUT: ",A)') TITLE

 209  FORMAT ('    Differences are E-6 for T, h, and Terr; E-9 for s; E-6% for v, and E-3% for Cp')
 201  FORMAT ('  IR    P        T       h        v        s       Cp       dT      dh     dv,%     ds     dCp,%  T-Herr')

      WRITE (12,*) &
     &'    Vis & KVis are 1.0E-3 percent, others 1.E-6%'
      WRITE (12,'(a)') &
     &'  IR     P       T        w       Cond        Vis         KinVis      Pran No.  %dw  %dCond  %dVis %dKVis %dPrandtl'

      WRITE (11,'(a)') &
     &'  Summary of maximum abso deviations from input file'
      WRITE (11,'(a)') &
     &' Output  T-error   h-error   v-err(%)  s-error  Cp-err(%) h-err/Cp or T-err*Cp'

      !     Read conversion indices on 2nd line, skip 3rd line, then begin on input data
      READ (2,*) IP,IT,IH,IV,IS
      CALL CONVERTU(IP,IT,IH,IV,IS,0.0D0)
      READ (2,*) 
      READ (2,*) 
      
      DO NCASE=1,31
         CALL TESTPTHS(2,NCASE,EEMN,EEMX,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()
      ENDDO
      CLOSE (2)

      !     Now read and check saturation tables
      READ (3,FMT='(A)') TITLE
      READ (3,*) 
      READ (3,*) 
      WRITE (14,'(a)') ' Differences are E-6 for P & T, E-3 for h, E-6 for s, and E-3% for v & Surf Ten'
      WRITE (14,'(a)') '   Pressure    Temper    dP %   dT     dhf    dhg   dvf %  dvg %   dsf    dsg  dSurf %'


      DO NCASE=1,9
         CALL TESTSAT(3,NCASE,EEMN,EEMX,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()
      ENDDO
        
      !  Now check English units and centigrade.
      READ (4,*)
      DO NCASE=1,2
         !  First 7 lines (2 data sets) are English units; next 7 are Celsius SI 
         READ (4,*) IP,IT,IH,IV,IS
         CALL CONVERTU(IP,IT,IH,IV,IS,0.D0)
         READ (4,*)
         READ (4,*)
         CALL TESTPTHS(4,NCASE,EEMN,EEMX,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()

         !  Check saturation units
         READ (4,*)
         READ (4,*)
         CALL TESTSAT(4,NCASE,EEMN,EEMX,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()
      ENDDO        

      !  Next, check the 11 different units for Pressure (all with deg-C & SI) 
      DO I=1,6
         READ (4,*)
      ENDDO
      DO NCASE=3,13
         READ (4,*) IP,IT,IH,IV,IS
         IF (IP .GE. 6 .AND. IP .LE. 10) READ (4,*) PATM
         CALL CONVERTU(IP,IT,IH,IV,IS,PATM)
         CALL TESTPTHS(4,NCASE,EEMN,EEMX,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()
         CALL TESTSAT(4,NCASE,EEMN,EEMX,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()
      ENDDO
 
      !  Done with various test cases!  Get and write out max & min values
      DO J=1,5
         DO I=1,9
            AAMX(I,J)=DMAX1(EEMX(I,J),-EEMN(I,J))
         ENDDO
      ENDDO
      DO J=1,3
         WRITE (11,131) XOUT(J),(AAMX(I,J),I=2,7)
      ENDDO
        
      WRITE (11,*)
      WRITE (11,132)
      WRITE (11,131) XOUT(4),(AAMX(I,4),I=3,7)
      
      WRITE (11,*)
      WRITE (11,133)
      WRITE (11,131) XOUT(5),(AAMX(I,5),I=1,9)

 131  FORMAT (1X, A6, 9(1PG10.2))
 132  FORMAT (7X,'    %w-err  %Con-err  %Vis-err %Visk-err  %Pr# err')
 133  FORMAT (7X,' %Psat-err  Tsat-err    hf-err    hg-err   %vf-err   %vg-err    sf-err    sg-err  %Surf-err')

      ! Last, check high and low limits, and error codes, starting with liquid & vapor
      WRITE (8,205)
      WRITE (9,205)
      WRITE (10,205)
      WRITE (12,205)
 205  FORMAT (/ 3X,'Test of HiLo Limits & Error Codes.  Excluded from SUMMARY).'/5X,'No multipliers -- all displays are times 1.0')
     
      WRITE (8,206)
      WRITE (9,206)
      WRITE (10,206)
206   FORMAT('    IR      P        T       h        v        s       Cp       dT      dh     dv,%     ds     dCp,% ')

      WRITE (12,207)
207  FORMAT('    IR     P       T        w       Cond        Vis         KinVis       Pran No.  %dw   %dCond  %dVis  %dKVis %dPran')

      ! First 5 lines are text description of file; then unit specs; then header 

      DO I=1,5
         READ (1,*) 
      ENDDO

      READ (1,*) IP,IT,IH,IV,IS
      READ (1,*)
      CALL CONVERTU(IP,IT,IH,IV,IS,0.0D0)
      DO NCASE=1,27
         !  First 10 cases have small out-of-limit variables, remaining cases are way out
         CALL TESTLIM(1,NCASE,IEND,NERR)
         IF (IEND .EQ. 1) call endoffile()
      ENDDO
      DO NCASE=28,33
         CALL TESTSATL(1,NCASE,IEND,NERR)
      ENDDO
      IF (IEND .EQ. 1) call endoffile()

      !  Last, list number of errors
      WRITE (11,*)
      WRITE (11,*)
      IF (NERR .EQ. 0) THEN
          WRITE (11,*) ' Very Good! No errors found for any input files'
      ELSE
          WRITE (11,*) '  Oops!! Number of errors is',NERR
      ENDIF 
      END PROGRAM CHECKALL
!-----------------------------------------------------------------------------------------------------------------------------------
      subroutine endoffile()
      WRITE (*,*) ' Unexpected End-of-File encountered!  Program halted!'
      stop 1
      end subroutine endoffile
!-----------------------------------------------------------------------------------------------------------------------------------

      SUBROUTINE GETMAX(IK,P,T,H,V,S,CP,DUM1,DUM2,DUM3,PI,TI,HI,VI,SI,CPI,DUM4,DUM5,DUM6,EEMN,EEMX)
      !*** Get max and min values of deviations from input
      !  IK indicates which property set is being checked:
      !       1 - PTOUT;      2 - PHOUT;      3 - PSOUT
      !       4 - PRAND;      5 - SATOUT

      IMPLICIT REAL(kind=8) (A-H,O-Z)
      real(kind=8) :: EEMX(9,5),EEMN(9,5),EE(9)
      EQUIVALENCE (EE(1),EP),(EE(2),ET),(EE(3),EH,EW),(EE(4),EV,ECOND),(EE(5),ES,EVIS,EVF),   &
     &   (EE(6),EC,EVISK,EVG),(EE(7),ETH,EPR,ESF),(EE(8),ESG), (EE(9),ESURF)

      EP=100.0d0*(P-PI)/PI
      IF (PI .LT. 1.D-4) EP=P-PI
      ET=T-TI
      EH=H-HI
      EV=V-VI
      ES=S-SI
      EC=CP-CPI
      IF (IK .GE. 1 .AND. IK .LE. 3) THEN
            IF (DABS(VI) .GT. 1.0D-8) EV=100.0d0*EV/VI
            IF (DABS(CPI) .GT. 1.0D-8) EC=100.0d0*EC/DMAX1(DABS(CPI),DABS(CP))
            ETH=0.0d0
            IF (DABS(CPI) .GT. 1.D-8) ETH=EH/CPI
            IF (IK .EQ. 2) ETH=ET*CPI
      ENDIF    
      IF (IK .EQ. 4) THEN
            EW=100.0d0*EH/HI
            ECOND=100.0d0*EV/VI
            EVIS=100.0d0*ES/SI
            EVISK=100.0d0*EC/CPI
            EPR=100.0d0*(DUM1-DUM4)/DUM4
      ENDIF
      IF (IK .EQ. 5) THEN
            EVF=100.0d0*ES/SI
            EVG=100.0d0*EC/CPI
            ESF=DUM1-DUM4
            ESG=DUM2-DUM5
            ESURF=100.0d0*(DUM3-DUM6)/DUM6
      ENDIF
    
      DO I=1,9
         IF (EE(I) .LT. EEMN(I,IK)) EEMN(I,IK)=EE(I)
         IF (EE(I) .GT. EEMX(I,IK)) EEMX(I,IK)=EE(I)
      ENDDO
      END SUBROUTINE GETMAX
!-----------------------------------------------------------------------------------------------------------------------------------
        
        
      SUBROUTINE TESTPTHS(INFILE,NCASE,EEMN,EEMX,IEND,NERR)
      !  Read and compare input liquid or vapor properties with calc'd values
      !  as functions of (P,T), (P,h), or (P,s)
      IMPLICIT REAL(kind=8) (A-H,O-Z)

      integer,intent(inout) :: NERR   !- counter for total number of errors encountered

      real(kind=8) :: EEMX(9,5),EEMN(9,5),SPEC(9),ER(9),SPECT(9)
      CHARACTER(len=8) :: LFIL(4)
      EQUIVALENCE (ER(1),EP),(ER(2),ET,EW),(ER(3),EH,ECOND), (ER(4),EV,EVIS),(ER(5),ES,EVISK),(ER(6),EC,EPR),(ER(7),ETH)
      DATA (SPEC(I),I=1,9) / 1.0d0, 1.D-6, 10.D-6, 5.D-6, 10.D-9, 5.D-5, 1.D-6, 1.0d0, 1.0d0/
      DATA (LFIL(I),I=1,4)/ 'TestUnit','  TestSI',' TestSat',' TestLim'/

      DO I=1,9
         ER(I)=0.0d0
         SPECT(I)=10.0D-6
      ENDDO

      READ (INFILE,*,END=950) IR,PI,TI,HI,VI,SI,CPI,WI,CONDI,VISI,VISKI,PRI
      CALL HPTI(PI,TI,H,V,S,X,CP,IREG)
      P=PI
      T=TI
      CALL GETMAX(1,P,T,H,V,S,CP,DUM1,DUM2,DUM3,PI,TI,HI,VI,SI,CPI,DUM4,DUM5,DUM6,EEMN,EEMX)
      ET=0.0d0
      EH=H-HI
      EV=(V-VI)/VI
      ES=S-SI
      EC=(CP-CPI)/CPI
      WRITE (8,199) IR,IREG,PI,TI,H,V,S,CP,1.0D6*ET,EH*1.0D6,EV*1.0D8,ES*1.0D9,EC*1.0D5,1.0D6*EH/CPI
      NOGO=0
      CALL VERIFY(IR,IREG,SPEC,ER,NOGO)
      W=WPTI(PI,TI,IRX)
      CALL PRANPTI(PI,TI,CPS,COND,VIS,VISK,PR,IRY)
      CALL GETMAX(4,P,T,W,COND,VIS,VISK,PR,DUM2,DUM3,PI,TI,WI,CONDI,VISI,VISKI,PRI,DUM5,DUM6,EEMN,EEMX)
      EW=(W-WI)/WI
      ECOND=(COND-CONDI)/CONDI
      EVIS=(VIS-VISI)/VIS
      EVISK=(VISK-VISKI)/VISKI
      EPR=(PR-PRI)/PRI
      WRITE (12,198) IR,IRY,PI,TI,W,COND,VIS,VISK,PR,EW*1.0D8,ECOND*1.0D8,EVIS*1.0D5,EVISK*1.0D5,EPR*1.0D8
      CALL VERIFY(IR,IRY,SPECT,ER,NOGO)
  198 FORMAT (I2,I3,F9.4, F9.3,F9.3,3D12.5,F10.5,5F7.3)
        
      CALL TPHI(PI,HI,T,V,S,X,CP,IREG)
      P=PI
      H=HI
      CALL GETMAX(2,P,T,H,V,S,CP,DUM1,DUM2,DUM3,PI,TI,HI,VI,SI,CPI,DUM4,DUM5,DUM6,EEMN,EEMX)
      ET=T-TI
      EH=0.0d0
      EV=(V-VI)/VI
      ES=(S-SI)
      EC=(CP-CPI)/CPI
      WRITE (9,199) IR,IREG,PI,T,HI,V,S,CP,ET*1.0D6,EH*1.0D6,EV*1.0D8,ES*1.0D9,EC*1.0D5,ET*CPI*1.0D6
      SPEC(2)=1.D-6
      IF (PI .GT. 20.D0 .AND. IR .EQ. 3) SPEC(2)=3.D-6
      CALL VERIFY(IR,IREG,SPEC,ER,NOGO)

      CALL TPSI(PI,SI,T,H,V,X,CP,IREG)
      P=PI
      S=SI
      CALL GETMAX(3,P,T,H,V,S,CP,DUM1,DUM2,DUM3,PI,TI,HI,VI,SI,CPI,DUM4,DUM5,DUM6,EEMN,EEMX)
      ET=(T-TI)
      EH=(H-HI)
      EV=(V-VI)/VI
      ES=0.0d0
      EC=(CP-CPI)/CPI
      WRITE (10,199) IR,IREG,PI,T,H,V,SI,CP,ET*1.D6,EH*1.D6,EV*1.D8,ES*1.D9,EC*1.D5,1.D6*EH/CPI
  199 FORMAT (I2,I3,F9.4,F9.3,F8.2,F10.5,F8.4,F8.4,7F8.3)
      SPEC(2)=1.D-6
      IF (PI .GT. 20.D0 .AND. IR .EQ. 3) SPEC(2)=3.D-6
      CALL VERIFY(IR,IREG,SPEC,ER,NOGO)

      IF (NOGO .NE. 0) THEN
          WRITE (*,2001) LFIL(INFILE),NCASE,PI,TI,HI,SI
          WRITE (11,2001) LFIL(INFILE),NCASE,PI,TI,HI,SI
          NERR=NERR+1  ! increment total error counter
      ENDIF
2001  FORMAT (A8,' Case #',I2,' out of spec.  Input variables are: P=',F8.2,', T=',F8.2,', h=',F8.2,', s=',F8.4)
      IEND=0
      RETURN
 950  CONTINUE
      IEND=1
      END SUBROUTINE TESTPTHS
!-----------------------------------------------------------------------------------------------------------------------------------
        
      SUBROUTINE TESTSAT(INFILE,NCASE,EEMN,EEMX,IEND,NERR)        
      !  Read and compare input saturation properties with calc'd values
      !  as functions of P, with check of Psat(T)
      IMPLICIT REAL(kind=8) (A-H,O-Z)
      integer,intent(inout) :: NERR   !- counter for total number of errors encountered
      real(kind=8) :: EEMX(9,5),EEMN(9,5),SPEC(9),ER(9)
      real(kind=8),external :: surfti
      CHARACTER(len=8) :: LFIL(4)
      EQUIVALENCE (ER(1),EPS),(ER(2),ETS),(ER(3),EHF),(ER(4),EHG),(ER(5),EVF),(ER(6),EVG),(ER(7),ESF),(ER(8),ESG),(ER(9),ESURF)
      DATA (SPEC(I),I=1,9) / 2.D-6, 1.D-6, 10.D-6, 10.D-6, 5.D-6,5.D-5, 50.D-9, 50.D-9, 10.D-6/
      DATA (LFIL(I),I=1,4)/ ' TestLim','  TestSI',' TestSat','TestUnit'/

      DO I=1,9
         ER(I)=0.0d0
      ENDDO
        
      READ (INFILE,*,END=950) PI,TI,HFI,HGI,VFI,VGI,SFI,SGI,SURFI
      CALL TSATPI(PI,TSAT,HF,HG,VF,VG,SF,SG,IRT)
      ETS=TSAT-TI
      EHF=HF-HFI
      EHG=HG-HGI
      EVF=100.*(VF-VFI)/VFI
      EVG=100.*(VG-VGI)/VGI
      ESF=SF-SFI
      ESG=SG-SGI
      CALL PSATTI(TI,PSAT,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,IRP)
      SURF=SURFTI(TSAT,IREG)
      CALL GETMAX(5,PSAT,TSAT,HF,HG,VF,VG,SF,SG,SURF,PI,TI,HFI,HGI,VFI,VGI,SFI,SGI,SURFI,EEMN,EEMX)
      EPS=100.*(PSAT-PI)/PI
      IF (PI .LT. 1.D-4) EPS=PSAT-PI
      IF (INFILE .EQ. 4) EPS=0.0d0
      IF (SURFI .LT. 1.D-5) ESURF=SURF-SURFI
      WRITE (14,195) PSAT,TSAT,1.D6*EPS,1.D6*ETS,1.D3*EHF,1.D3*EHG,1.D3*EVF,1.D3*EVG,1.D6*ESF,1.D6*ESG,1.D3*ESURF
 195  FORMAT (2F11.6,9F7.3)
      IF (DABS(PI-22.06D0) .LE. 0.005D0) THEN
         DO I=3,8
            ER(I)=ER(I)/2000.0d0
         ENDDO
      ENDIF 
      NOGO=0
      CALL VERIFY(4,IRP,SPEC,ER,NOGO)
      CALL VERIFY(4,IRT,SPEC,ER,NOGO)

      IF (NOGO .NE. 0) THEN
          WRITE (*,2001) LFIL(INFILE),NCASE,PI,TI
          WRITE (11,2001) LFIL(INFILE),NCASE,PI,TI
          NERR=NERR+1  ! increment total error counter
      ENDIF
2001  FORMAT (A8,' Case #',I2,' out of spec.  Input variables are: P=',F8.2,', T=',F8.2)
      IEND=0
      RETURN
 950  CONTINUE
      IEND=1
      END SUBROUTINE TESTSAT
!-----------------------------------------------------------------------------------------------------------------------------------

      SUBROUTINE TESTLIM(INFILE,NCASE,IEND,NERR)
      IMPLICIT real(kind=8) (A-H,O-Z)

      !  Compare high and low limits and check error codes for liquid and vapor regions
      !  Inputs are: 
      integer,intent(in)    :: INFILE !- Unit to read
      integer,intent(in)    :: NCASE  !- case number
      !  Output is:
      integer,intent(out)   :: IEND   !- Set to 1 if end-of-file encountered on READ unit.
      integer,intent(inout) :: NERR   !- counter for total number of errors encountered

      real(kind=8) :: ER(9),SPEC(9)
      INTEGER IR(3)
      EQUIVALENCE (ER(1),EP),(ER(2),ET,EW),(ER(3),EH,ECOND),(ER(4),EV,EVIS),(ER(5),ES,EVISK),(ER(6),EC,EPR)
      
      DO I=1,9
         ER(I)=0.0d0
         SPEC(I)=0.02D0
      ENDDO

      READ (INFILE,*,END=950) IN,IR,PI,TI,HI,VI,SI,CPI,WI,CONDI,VISI,VISKI,PRI
      CALL HPTI(PI,TI,H,V,S,X,CP,IR1)
      ET=0.0d0
      EH=(H-HI)
      EV=(V-VI)/VI
      ES=(S-SI)
      EC=(CP-CPI)/CPI
      WRITE (8,199) IN,IR(1),IR1,PI,TI,H,V,S,CP,ET,EH,EV,ES,EC
      IF (NCASE .GT. 10) ET=0.0d0
      IF (NCASE .GT. 10) EH=0.0d0
      IF (NCASE .GT. 10) ES=0.0d0
      NOGO=0
      CALL VERIFY(IR(1),IR1,SPEC,ER,NOGO)
      W=WPTI(PI,TI,IRX)
      CALL PRANPTI(PI,TI,CPS,COND,VIS,VISK,PR,IR4)
      EW=(W-WI)/WI
      ECOND=(COND-CONDI)/CONDI
      EVIS=(VIS-VISI)/VIS
      EVISK=(VISK-VISKI)/VISKI
      EPR=(PR-PRI)/PRI
      WRITE (12,198) IN,IR(1),IR4,PI,TI,W,COND,VIS,VISK,PR,EW,ECOND,EVIS,EVISK,EPR
      IF (NCASE .GT. 10) ET=0.0d0
      IF (NCASE .GT. 10) EH=0.0d0
      IF (NCASE .GT. 10) ES=0.0d0
      CALL VERIFY(IR(1),IR4,SPEC,ER,NOGO)
  198 FORMAT (I2,2I3,F9.4, F9.3,F9.3,3D12.5,F10.5,5F7.3)
        
      CALL TPHI(PI,HI,T,V,S,X,CP,IR2)
      ET=(T-TI)
      EH=0.0d0
      EV=(V-VI)/VI
      ES=(S-SI)
      EC=(CP-CPI)/CPI
      WRITE (9,199) IN,IR(2),IR2,PI,T,HI,V,S,CP,ET,EH,EV,ES,EC
      IF (NCASE .GT. 10) ET=0.0d0
      IF (NCASE .GT. 10) EH=0.0d0
      IF (NCASE .GT. 10) ES=0.0d0
      CALL VERIFY(IR(2),IR2,SPEC,ER,NOGO)
        
      CALL TPSI(PI,SI,T,H,V,X,CP,IR3)
      ET=(T-TI)
      EH=(H-HI)
      EV=(V-VI)/VI
      ES=0.0d0
      EC=(CP-CPI)/CPI
      WRITE (10,199) IN,IR(3),IR3,PI,T,H,V,SI,CP,ET,EH,EV,ES,EC
      IF (NCASE .GT. 10) ET=0.0d0
      IF (NCASE .GT. 10) EH=0.0d0
      IF (NCASE .GT. 10) ES=0.0d0
      CALL VERIFY(IR(3),IR3,SPEC,ER,NOGO)
  199 FORMAT (I2,2I3,F9.4,F9.3,F8.2,F10.5,F8.4,F8.4,7F8.3)
      IF (NOGO .NE. 0) THEN
          WRITE (*,2001) NCASE,PI,TI,HI,SI
          WRITE (11,2001) NCASE,PI,TI,HI,SI
          NERR=NERR+1  ! increment total error counter
      ENDIF
2001  FORMAT (' TestLim Case #',I2,' out of spec.  Input variables are: P=',F8.2,', T=',F8.2,', h=',F8.2,', s=',F8.4)          
      IEND=0
      RETURN
950   CONTINUE
      IEND=1
      END SUBROUTINE TESTLIM
!-----------------------------------------------------------------------------------------------------------------------------------
      
      SUBROUTINE VERIFY(IRIN,IROUT,SPEC,ERROR,NOGO)
      IMPLICIT REAL(kind=8) (A-H,O-Z)
      ! Verify that the calculated results equal the input to within tolerance
      !  Inputs are:
      !       IRIN - Expected region
      !       IROUT - Calculated region
      !       SPEC - Maximum acceptable error for EP,ET,EH,EV,ES, EC, etc
      !       ER Errors; e.g. EP,EH,EV,ES,EC - For errors in P,T,h,v,s, and Cp
      !  Output is:  NOGO - Error flag, set to 1 if out-of-spec error
      !   (NOGO must be set to zero by calling routine prior to call)
      real(kind=8) :: SPEC(9),ERROR(9)

      IF (IRIN .NE. IROUT) NOGO=1
      IF (IRIN .NE. IROUT) WRITE (11,*) ' IRIN,IROUT=',IRIN,IROUT  
      DO I=1,9
         IF (DABS(ERROR(I)) .GT. SPEC(I)) NOGO=1
         IF (DABS(ERROR(I)) .GT. SPEC(I)) WRITE (11,*) ' I,ERROR, SPEC=',I,ERROR(I),SPEC(I)
      ENDDO
      END SUBROUTINE VERIFY
!-----------------------------------------------------------------------------------------------------------------------------------

      SUBROUTINE TESTSATL(INFILE,NCASE,IEND,NERR)        
      !  Read and compare input saturation properties with calc'd values
      !  as functions of P, with check of Psat(T)
      implicit none

      !  Inputs are: 
      integer,intent(in)    :: INFILE !- Unit to read
      integer,intent(in)    :: NCASE  !- case number
      !  Output is:
      integer,intent(out)   :: IEND   !- Set to 1 if end-of-file encountered on READ unit.
      integer,intent(inout) :: NERR   !- counter for total number of errors encountered

      real(kind=8) :: ER(9),SPEC(9)
      real(kind=8) :: EPS,ETS,EHF,EHG,EVF,EVG,ESF,ESG,ESURF
      real(kind=8) :: PI,TI,HFI,HGI,VFI,VGI,SFI,SGI,SURFI
      integer      :: IDUM,IRPE,IRTE
      real(kind=8) :: TSAT,HF,HG,VF,VG,SF,SG
      integer      :: irp
      real(kind=8) :: PSAT,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
      integer      :: IRT,IREG
      integer      :: I,NOGO
      real(kind=8) :: SURF

      real(kind=8),external :: surfti

      EQUIVALENCE (ER(1),EPS),(ER(2),ETS),(ER(3),EHF),(ER(4),EHG),(ER(5),EVF),(ER(6),EVG),(ER(7),ESF),(ER(8),ESG),(ER(9),ESURF)

      DO I=1,9
         SPEC(I)=1.0D-2
         ER(I)=0.0d0
      ENDDO

      SPEC(7)=1.0D-3
      SPEC(8)=1.0D-3
      READ (INFILE,*,END=950) IDUM,IRPE,IRTE,PI,TI,HFI,HGI,VFI,VGI,SFI,SGI,SURFI
      CALL TSATPI(PI,TSAT,HF,HG,VF,VG,SF,SG,IRP)
      ETS=TSAT-TI
      EHF=HF-HFI
      EHG=HG-HGI
      EVF=(VF-VFI)/VFI
      EVG=(VG-VGI)/VGI
      ESF=SF-SFI
      ESG=SG-SGI
      CALL PSATTI(TI,PSAT,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,IRT)
      SURF=SURFTI(TSAT,IREG)
      EPS=(PSAT-PI)/DMAX1(PI,PSAT)
      IF (PI .LT. 1.D-4) EPS=PSAT-PI
      IF (PI .LT. 1.D0) ESURF=(SURF-SURFI)/SURFI
      IF (PI .GT. 22.D0) ESURF=1.0E4*(SURF-SURFI)
      WRITE (14,195) PSAT,TSAT,EPS,ETS,EHF,EHG,EVF,EVG,ESF,ESG,ESURF
      
 195  FORMAT (2F11.6,9F7.3)
      EPS=0.0d0
      IF (NCASE .GT. 29) ETS=0 
      NOGO=0
      CALL VERIFY(IRPE,IRP,SPEC,ER,NOGO)
      CALL VERIFY(IRTE,IRT,SPEC,ER,NOGO)
      IF(NOGO .NE. 0) THEN
         WRITE (*, 2001) NCASE,PI,TI
         WRITE (11,2001) NCASE,PI,TI
         NERR=NERR+1  ! increment total error counter
      ENDIF
2001  FORMAT (' TestLim Case #',I2,' out of spec.  Input variables are: Psat=',F10.4,', Tsat=',F10.4)
      IEND=0
      RETURN
950   CONTINUE
      IEND=1
      END SUBROUTINE TESTSATL
!-----------------------------------------------------------------------------------------------------------------------------------
