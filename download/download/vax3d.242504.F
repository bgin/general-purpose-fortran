#ifdef TESTPRG
      PROGRAM VAX3DTESTS
!     WRITTEN BY: JSU, LAST REVISED ON 19-JUL-2005
      PARAMETER (IX=35)
      PARAMETER (IZ=45)
      DIMENSION SURFDAT(ix,iz)
      DIMENSION IC(255)
      DATA TPI/3.141592654/
!     initialize the color array
      DO 1 I=1,255 
         IC(I)=MOD(I,7)
1     CONTINUE
!
!     fill some arrays with data we can plot
      DO 3 J=1,IX
       DO 3 I=1,IZ
        SURFDAT(J,I)=COS(TPI*REAL(J-1)/12.0)*COS(TPI*REAL(I-1)/12.0)
3     CONTINUE
!
      CALL DL_INIT(0,12.5,12.5,1.5,1.5,1.0)
!
!     now plot surface of volume using slices 
      NDX=IX  
      NDZ=IZ 
      NX=IX
      NZ=IZ
      A=25.0
      B=15.0
      XH=6.0
      YH=3.8
      ZH=5.0
      IFLAG=002
      IAX=-11
      !XT='VAX3D X TITLE'
      NXT=13
      XS=-10.0
      XE=10.0
      !YT='VAX3D Y TITLE'
      NYT=13
      !ZT='Z TITLE'
      NZT=7
      ZS=-1.0
      ZE=1.0
      DM=-1.0
      DX=1.0

      WRITE(*,*)'VAX3D=',NDX,NDZ,NX,NZ
!23456789012345678901234567890123456789012345678901234567890123456789012
      CALL VAX3D(SURFDAT,NDX,NDZ,NX,NZ,A,B,XH,YH,ZH,IFLAG,IAX,          &
     & 'VAX3D X TITLE',NXT,                                             &
     & XS,XE,                                                           &
     & 'VAX3D Y TITLE',NYT,                                             &
     & 'Z TITLE',NZT,                                                   &
     & ZS,ZE,DM,DX,IC)
!
!     add a label after master routine call
      CALL DL_COLOR(1)
      CALL DL_WIDTH(1)
      CALL DL_SYMBOL(0.0,0.0,0.25,'VAX3D',0.0,6,-1)

      CALL DL_PAUSE() ! pause
      CALL DL_EXIT()  ! close up plot package
      STOP            ! all done
      END
#endif
