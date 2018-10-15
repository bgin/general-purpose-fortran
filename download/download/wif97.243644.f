        PROGRAM WIF97
C*** Driver program to calculate water and steam properties per IAPWS-IF97 
        IMPLICIT REAL*8 (A-H,O-Z)
        
        WRITE (*,*) '  Westinghouse IAPWS-IF97 water & steam properties' 
        write (*,*)
        WRITE (*,*) ' Output file will be named WIF97.OUT'
        OPEN (8,FILE='WIF97.OUT')
        OPEN (9,FILE='WIF97.ADD')

 1     WRITE (*,*) ' Input desired units for P, T, h, v, and s'
       WRITE (*,*)'  1 - SI (MPa, K, kJ/kg, m3/kg, and kJ/kg-K'  
       WRITE (*,*)'  2 - English (psia, F, BTU/lbm, ft3/lbm, BTU/lbm-F)'
       WRITE (*,*)'   IP: 1=MPa; 2=psia; 3=bar; 4=Pa, 5=kg/cm2; 11=inHg'
       WRITE (*,*)'   IT: 1=degK; 2=degF; 3=degC'
 3     WRITE (*,*) ' IP,IT,IH,IV,IS= ??'
     $    ,' (Enter 0,0,0,0,0 or Ctrl-C to quit) '
        READ (*,*) IP,IT,IH,IV,IS
        IF (IP .EQ. 0) GO TO 999
        IF (IABS(IP-3) .GT. 2 .AND. IP .NE. 11) THEN
            WRITE (*,*) ' Only 1, 2, 3, 4, 5, OR 11 allowed for IP'
            GO TO 1
        ENDIF
        IF (IABS(IT-2) .GT. 1) THEN
            WRITE (*,*) ' Only 1, 2, or 3 allowed for IT'
            GO TO 1
        ENDIF
        IF (IH .NE. 1 .AND. IH .NE. 2) THEN 
            WRITE (*,*) ' Only 1 or 2 allowed for IH'
            GO TO 1
        ENDIF
        IF (IV .NE. 1 .AND. IV .NE. 2) THEN 
            WRITE (*,*) ' Only 1 or 2 allowed for IV'
            GO TO 1
        ENDIF
        IF (IS .NE. 1 .AND. IS .NE. 2) THEN 
            WRITE (*,*) ' Only 1 or 2 allowed for IS'
            GO TO 1
        ENDIF
        CALL CONVERTU(IP,IT,IH,IV,IS,0.0D0)
   5    WRITE (*,*)
        WRITE (*,*) ' Select one of the following;           '
        WRITE (*,*) ' 1 - Properties as a function of P & T'
        WRITE (*,*) ' 2 - Properties as a function of P & h'
        WRITE (*,*) ' 3 - Properties as a function of P & s'
        WRITE (*,*) ' 4 - Saturation properties as a function of P'
        WRITE (*,*) ' 5 - Saturation properties as a function of T  '
        WRITE (*,*) '   0 - Change units (or Ctrl-C to quit)  '
        WRITE (*,*) '       Which one?    '
        READ (*,*) IEQN
        IF (IEQN .EQ. 0) GO TO 3
        IF (IEQN .LT. 1 .OR. IEQN .GT. 5) GO TO 5

        CALL HEADER(IP,IT,IH,IV,IS,IEQN)

 10     CONTINUE
        IF (IEQN .EQ. 1) THEN
           WRITE (*,*) ' P,T=?  (enter 0,0 to change input properties) '
                READ (*,*) P,T
                IF (P .EQ. 0.0D0) GO TO 5
                CALL HPTI(P,T,H,V,S,X,CP,IREG)
                W=WPTI(P,T,IREG)
                CALL PRANPTI(P,T,CP,COND,VIS,VISK,PRANDTL,IREG)
        ENDIF
        IF (IEQN .EQ. 2) THEN
           WRITE (*,*) ' P,h=?  (enter 0,0 to change input properties) '
                READ (*,*) P,H
                IF (P .EQ. 0.0D0) GO TO 5
                CALL TPHI(P,H,T,V,S,X,CP,IREG)
        ENDIF
        IF (IEQN .EQ. 3) THEN
           WRITE (*,*) ' P,s=?  (enter 0,0 to change input properties) '
                READ (*,*) P,S
                IF (P .EQ. 0.0D0) GO TO 5
                CALL TPSI(P,S,T,H,V,X,CP,IREG)
        ENDIF
        IF (IEQN .EQ. 4) THEN
              WRITE (*,*) ' P=?  (enter 0 to change input properties) '
                READ (*,*) P
                IF (P .EQ. 0.0D0) GO TO 5
                CALL TSATPI(P,T,HF,HG,VF,VG,SF,SG,ISAT)
                SURFT=SURFTI(T,IDUM)
        ENDIF
        IF (IEQN .EQ. 5) THEN
              WRITE (*,*) ' T=?  (enter 0 to change input properties) '
                READ (*,*) T
                IF (T .EQ. 0.0D0) GO TO 5
                CALL PSATTI(T,P,HF,HG,VF,VG,SF,SG,ISAT)
                SURFT=SURFTI(T,IDUM)
        ENDIF
        
        IF (IREG .LT. 0) THEN
          WRITE (*,*) ' WARNING! Input out of range!!'
          WRITE (*,*) ' Valid range is .0006<P<100 MPa, 0<T<2000 C'
          WRITE (*,*) '     (0.089<P<14,504 psia, 32<T<3632 F)'
          WRITE (*,*) '  or T<800C (1472F) above 50 MPa (7252 psia)'
        ENDIF
        IF (ISAT .LT. 0) THEN
          WRITE (*,*) ' WARNING! Saturation input out of range!!'
          WRITE (*,*) ' Range is .0006<P<22.064 MPa, 0<T<373.946 C'
          WRITE (*,*) ' Range is 0.087<P3200.11 psia, 32<T<705.10 F'
        ENDIF
        
        IF (IEQN .EQ. 1) THEN
            CALL TSATPI(P,TSAT,HF,HG,VF,VG,SF,SG,IREGDUM)
            SUB=T-TSAT
            SUPER=T-TSAT
            IF (T .LE. TSAT) THEN
C Print -subcooling if T<Tsat ("-" sign denotes subcooling.
                WRITE (*,197) IREG,P,T,H,V,S,CP,SUB,
     $          W,COND,VIS,VISK,PRANDTL,P,T
                WRITE (8,197) IREG,P,T,H,V,S,CP,SUB,
     $          W,COND,VIS,VISK,PRANDTL,P,T
  196   FORMAT (1X,I3,F11.4,F9.3,F10.3,1PG14.7,0PF9.6,F8.4,F10.3,5X,
     $          F8.2,3(1PG12.5),0PF8.4,F9.2,F9.3)
  197   FORMAT (1X,I3,F11.4,F9.3,F10.3,1PG14.7,0PF9.6,F8.4,F10.3,1X,
     $          F8.2,3(1PG12.5),0PF8.4,F9.2,F9.3)
            ENDIF
            IF (T .GT. TSAT) THEN
C Print superheat if T>Tsat ("+" sign denotes superheat.
                WRITE (*,198) IREG,P,T,H,V,S,CP,SUPER,
     $          W,COND,VIS,VISK,PRANDTL,P,T
                WRITE (8,198) IREG,P,T,H,V,S,CP,SUPER,
     $          W,COND,VIS,VISK,PRANDTL,P,T
  195   FORMAT (1X,I3,F11.4,F9.3,F10.3,1PG14.7,0PF9.6,F8.4,'  +',F7.2,
     $          5X,F8.2,3(1PG12.5),0PF8.4,F9.2,F9.3)
  198   FORMAT (1X,I3,F11.4,F9.3,F10.3,1PG14.7,0PF9.6,F8.4,'  +',F7.2,
     $          1X,F8.2,3(1PG12.5),0PF8.4,F9.2,F9.3)
            ENDIF
        ENDIF
        
        IF (IEQN .GE. 2 .AND. IEQN .LE. 3) THEN
            CALL TSATPI(P,TSAT,HF,HG,VF,VG,SF,SG,IREGDUM)
            IF (T .LT. TSAT-1.0D-6) THEN
C Print -subcooling if T<Tsat ("-" sign denotes subcooling.
                SUB=T-TSAT
                WRITE (8,1197) IREG,P,T,H,V,S,CP,SUB
                WRITE (*,1197) IREG,P,T,H,V,S,CP,SUB
 1197       FORMAT (1X,I3,F11.4,F9.3,F10.3,G15.8,F9.6,F8.4,F10.3)
            ENDIF
            
            IF (T .GT. TSAT+1.0D-6) THEN
C Print superheat if T>Tsat ("+" sign denotes superheat.
                SUPER=T-TSAT
                WRITE (*,1198) IREG,P,T,H,V,S,CP,SUPER
 1198       FORMAT (1X,I3,F11.4,F9.3,F10.3,G15.8,F9.6,F8.4,'  +',F8.3)
            ENDIF
            
            IF (DABS(T-TSAT) .LE. 1.0D-6) THEN
                WRITE (8,199) IREG,P,T,H,V,S,CP,X
                WRITE (*,199) IREG,P,T,H,V,S,CP,X
  199       FORMAT (1X,I3,F11.4,F9.3,F10.3,G15.8,F9.6,F8.4,F9.4)
            ENDIF
        ENDIF
        
        IF (IEQN .GE. 4 .AND. IEQN .LE. 5) THEN 
                WRITE (*,200) ISAT,P,T,HF,HG,VF,VG,SF,SG,SURFT
                WRITE (8,200) ISAT,P,T,HF,HG,VF,VG,SF,SG,SURFT
        ENDIF
  200   FORMAT (1X,I3,F10.4,F9.3,2F10.3,2F11.6,2F9.5,F12.7)

        GO TO 10
 999    END
 
 
        SUBROUTINE HEADER(IP,IT,IH,IV,IS,IEQN)
C*** WIF97 WRITE formating
        IMPLICIT REAL*8 (A-H,O-Z)
        
        CHARACTER*11 LABELP(11),LABELCON(2)
        CHARACTER*10 LABELS(2),LABELVIS(2)
        CHARACTER*6 LABELT(3),LABELSUR(2),LABELX,LABELW(2)
        CHARACTER*7 LABELH(2),LABELV(2),LABELKIN(2)
        DATA (LABELP(I),I=1,11) /      '    MPa-abs', '    psia   ',
     $ '  bar-abs  ', '   Pa-abs  ', ' kg/cm2-abs', '  MPa-gage ',
     $ '    psig   ', '  bar-gage ', '  Pa-gage  ', 'kg/cm2-gage',
     $ 'in Hg (abs)'/
        DATA (LABELT(I),I=1,3) / 'Deg-K ', 'Deg-F ', 'Deg-C '/
        DATA (LABELH(I),I=1,2) / ' kJ/kg ', 'BTU/lbm'/
        DATA (LABELV(I),I=1,2) / ' m3/kg ','ft3/lbm'/
        DATA (LABELS(I),I=1,2) / '  kJ/kg-K ', 'BTU/lbm-F '/
        DATA (LABELW(I),I=1,2) / '  m/s ','ft/sec'/
        DATA (LABELCON(I),I=1,2) / ' mW/m-K    ','BTU/hr-ft-F'/
        DATA (LABELVIS(I),I=1,2) / '     Pa-s ', 'lbm/ft-sec'/
        DATA (LABELKIN(I),I=1,2) / ' m2/s ','ft2/sec'/
        DATA (LABELSUR(I),I=1,2) / '  N/m ', 'lbf/ft'/
        
        WRITE (8,*)
        IF (IEQN .EQ. 1) WRITE (8,201)
        IF (IEQN .EQ. 2) WRITE (8,202)
        IF (IEQN .EQ. 3) WRITE (8,203)
        IF (IEQN .EQ. 4) WRITE (8,204)
        IF (IEQN .EQ. 5) WRITE (8,205)
        IF (IEQN .EQ. 1) WRITE (*,201)
        IF (IEQN .EQ. 2) WRITE (*,202)
        IF (IEQN .EQ. 3) WRITE (*,203)
        IF (IEQN .EQ. 4) WRITE (*,204)
        IF (IEQN .EQ. 5) WRITE (*,205)

  201   FORMAT ('  Reg   P-input  T-input      h   '
     $  ,'      v           s         Cp   X,T-Tsat',
     $  ' SonicVel TherCond    Viscosity  KinViscosity Prandtl#',
     $  ' Pressure Temper')
  202   FORMAT ('  Reg   P-input     T      h-input'
     $  ,'      v           s         Cp    X,T-Tsat ')
  203   FORMAT ('  Reg   P-input     T         h   '
     $  ,'      v         s-input     Cp    X,T-Tsat ')
  204   FORMAT (' Reg   Pinput     T         hf        hg   ',
     $    '    vf        vg        sf       sg       Surf Tens')
  205   FORMAT (' Reg     P     Tinput       hf        hg   ',
     $    '    vf        vg        sf       sg       Surf Tens')

        LABELX='#, Deg'
        IF (IEQN .EQ. 1) WRITE (8,206)  LABELP(IP),LABELT(IT),
     $  LABELH(IH), LABELV(IV),LABELS(IS),LABELS(IS),LABELX,
     $  LABELW(IV),LABELCON(IV),LABELVIS(IV),LABELKIN(IV)
        IF (IEQN .EQ. 2) WRITE (8,207) LABELP(IP),LABELT(IT),
     $  LABELH(IH), LABELV(IV),LABELS(IS),LABELS(IS),LABELX
        IF (IEQN .EQ. 3) WRITE (8,208) LABELP(IP),LABELT(IT),
     $  LABELH(IH), LABELV(IV),LABELS(IS),LABELS(IS),LABELX
        IF (IEQN .GE. 4) WRITE (8,209) LABELP(IP),LABELT(IT),
     $  LABELH(IH),LABELH(IH),LABELV(IV),LABELV(IV),LABELS(IS),
     $  LABELS(IS),LABELSUR(IV) 
  206   FORMAT (1X,3X,A11,3X,A6,3X,A7,4X,A7,2X,2A10,1X,A6,
     $  1X,A9,1x,A9,1X,2A12,'  Fraction',A8,A9,A7)
  207   FORMAT (1X,3X,A11,3X,A6,3X,A7,4X,A7,4X,2A10,1X,A6)
  208   FORMAT (1X,3X,A11,3X,A6,3X,A7,4X,A7,4X,2A10,1X,A6)
  209   FORMAT (4X,A11,1X,A6,4X,A7,3X,A7,3X,A7,3X,A7,2X,2A10,1X,A6)
        IF (IEQN .EQ. 1) WRITE (*,206)  LABELP(IP),LABELT(IT),
     $  LABELH(IH), LABELV(IV),LABELS(IS),LABELS(IS),LABELX,
     $  LABELW(IV),LABELCON(IV),LABELVIS(IV),LABELKIN(IV)
        IF (IEQN .EQ. 2) WRITE (*,207) LABELP(IP),LABELT(IT),
     $  LABELH(IH), LABELV(IV),LABELS(IS),LABELS(IS),LABELX
        IF (IEQN .EQ. 3) WRITE (*,208) LABELP(IP),LABELT(IT),
     $  LABELH(IH), LABELV(IV),LABELS(IS),LABELS(IS),LABELX
        IF (IEQN .GE. 4) WRITE (*,209) LABELP(IP),LABELT(IT),
     $  LABELH(IH),LABELH(IH),LABELV(IV),LABELV(IV),LABELS(IS),
     $  LABELS(IS),LABELSUR(IV) 
        
        RETURN
        END
