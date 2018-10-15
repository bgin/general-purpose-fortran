#ifdef TESTPRG
      PROGRAM PLOTPIE
!
!     THIS PROGRAM TESTS THE PIE CHART ROUTINE
!     NOTE: NOT ALL OPTIONS WILL BE ILLUSTRATED.
!
      parameter(nwedges=7)
      CHARACTER*13 labels(nwedges)
      character*80 title
      real values(nwedges)
      integer ishade_opts(nwedges)
      integer iwidth_opts(nwedges)
      integer icolor(255)
      real box_lll(nwedges*2)

      DATA labels(1)/'Plot Routines'/
      DATA labels(2)/'Auxliary Rou.'/
      DATA labels(3)/'MASTER Routns'/
      DATA labels(4)/'MAP Routines '/
      DATA labels(5)/'Cursor Routns'/
      DATA labels(6)/'3D Plot Rou. '/
      DATA labels(7)/'Internal Rou.'/

      DATA values/51.0,34.0,-32.0,6.0,6.0,21.0,43.0/ ! wedge values
      DATA ishade_opts/0,1,2,3,4,5,6/                ! shade option array
      DATA iwidth_opts/2,2,3,2,2,2,2/                ! line weight array
      DO 10 I10=1,255                                ! initialize color array
           icolor(I10)=I10
10    CONTINUE
!
      CALL DL_INIT(0,9.0,9.0, 1.0,1.0,1.0)   ! open plotting with a 12x12 page
!
!     FIRST EXAMPLE WILL BE A FULL PAGE PIE CHART
!     SHOWING THE RELATIVE PROPORTIONS OF ROUTINE NAMES IN libDL
!
      xcenter=3.0
      ycenter=3.0
      radius=2.0
      offset=0.25
      start_angle=-40.0
      nc_labels=13
      charsize=0.12
      box_size=0.21
      nc_title=16
      t_charsize=0.2
      shade_dis=0.25
      title='libDL Routines'

      CALL DL_PIE(xcenter,ycenter,radius,offset,                        &
     & start_angle,nwedges,values,ishade_opts,iwidth_opts,              &
     & labels,nc_labels,charsize,box_size,                              &
     & title,nc_title,t_charsize,shade_dis,                             &
     & box_lll,icolor)
!
      CALL DL_PAUSE()  ! pause till character entered in graphics area
      CALL DL_EXIT()   ! close up plot package
      STOP             ! all done
      END
#endif
