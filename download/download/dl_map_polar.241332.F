#ifdef TESTPRG
      PROGRAM VAX3DTESTS
      CALL DL_INIT(0,20.0,20.0,0.0,0.0,1.0)
      call dl_color(2)
      call dl_width(1)
      CALL DL_MAP_POLAR(5.0,5.0,5.0,0.0)
      call dl_width(2)
      call dl_arc( 5.0,5.0, 0.0,360.0, 5.0,5.0)

      call dl_color(4)
      call dl_width(1)
      CALL DL_MAP_POLAR(5.0,15.0,-5.0,0.0)
      call dl_width(2)
      call dl_arc(5.0,15.0, 0.0,360.0, 5.0,5.0)

      CALL DL_PAUSE() ! pause
      call dl_translate(10.0,10.0)
      scale=1.005
      call dl_width(1)
      do 10 i10=1,200
         call dl_clear()
         call dl_color(1)
         CALL DL_MAP_POLAR(0.0,0.0,5.0,0.0)
         call dl_arc( 0.0,0.0, 0.0,360.0, 5.0,5.0)
         call dl_scale(scale)
         call dl_translate(0.0,0.025)
10    continue
      CALL DL_PAUSE() ! pause
      CALL DL_EXIT()  ! close up plot package
      STOP            ! all done
      END
#endif
