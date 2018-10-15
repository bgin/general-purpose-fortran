#ifdef TESTPRG
        PROGRAM WORLD
! *** LAST REVISED ON 22-JUN-1990 12:49:24.25
! *** SOURCE FILE: [DL.GRAPHICS.libDL]WORLD.FOR
!
!       AN EXAMPLE OF USING THE MAPLIB ROUTINES.
!       THIS PROGRAM WILL PLOT A 3-D MAP OF THE WORLD
!
        DIMENSION V(3)
!
!       DEFINE DEGREE TO RADIAN CONVERSION
!
        DEGRAD(X)=X*3.141592654/180.0
!
        R=6378.1778             ! RADIUS OF THE EARTH IN KM
        FLAT=3.3528132E-3       ! FLATNESS OF EARTH
        F3=0.00002              ! 3D SCALE FACTOR
        F=5.0                   ! PLOTTING SCALE FACTOR
        X=100.                  ! VIEW POINT LOCATION
        Y=100.
        Z=100.
        DS=600.                 ! PERSPECTIVE FACTOR
        RES=20.                 ! LAT/LONG GRID RESOLUTION
!
        NPTS=180./RES
        FPTS=NPTS
!
!       OPEN libDL PACKAGE WITH PROMPT FOR SCREEN DEVICE
!
        CALL DL_INIT(0,6.0,6.0,3.0,3.0,F)
!
!       INITIALIZE 3D ROUTINES
!
        CALL INIT3D(X,Y,Z,0.,0.,0.,0.,DS,F3,1)
!
!       PLOT A 2D LABEL (DOESN'T USE 3D ROUTINES)
!
        call dl_symbol(-.5,-.5,.03,'WORLD',0.,5,-1)
!
!       PLOT EARTH LAT/LONG GRID IN 3D
!
        call dl_color(3)        ! SET PEN COLOR
        CALL DL_NEWPEN(4)
        DO 310 ILAT=2,NPTS
                ALAT=DEGRAD((ILAT-1)*180./FPTS)
                ID=3
                DO 310 ILONG=1,2*NPTS+1
                        ALONG=DEGRAD((ILONG-1)*180./FPTS)
                        RAD=R*(1.-FLAT*SIN(ALAT))
                        CALL SPRECT1(V,ALONG,ALAT,RAD)
                        CALL PLOT3D(V(1),V(2),V(3),ID)
                        ID=2
310     CONTINUE
        call dl_color(1)        ! SET PEN COLOR
        CALL DL_NEWPEN(6)
        DO 320 ILONG=1,NPTS*2+1
                ALONG=DEGRAD((ILONG-1)*180./FPTS)
                ID=3
                DO 320 ILAT=1,NPTS+1
                        ALAT=DEGRAD((ILAT-1)*180./FPTS)
                        RAD=R*(1.-FLAT*SIN(ALAT))
                        CALL SPRECT1(V,ALONG,ALAT,RAD)
                        CALL PLOT3D(V(1),V(2),V(3),ID)
                        ID=2
320     CONTINUE
        CALL dl_move(0.,0.)
!
!       ADD LAND MAP
!
        call dl_color(2)        ! SET PEN COLOR
        CALL DL_NEWPEN(1)
        CALL EARTH3D(R,FLAT)
        CALL DL_PAUSE()
        CALL DL_EXIT()
        STOP
        END
#endif
