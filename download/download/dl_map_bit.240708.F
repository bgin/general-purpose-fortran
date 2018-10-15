#ifdef TESTPRG
      PROGRAM TESTS               ! Last revised on 19-JUL-2005, JSU
      CALL DL_INIT(0,12.5,10.5,0.0,0.0,1.0) ! initialize 12.5x10.5 unit surface
      call dl_color(2)            ! select pen color
      call dl_width(2)            ! select pen width
      CALL DL_MAP_BIT(0.0,10.0,12.0,360,360,1) ! draw land map
      CALL DL_PAUSE()             ! pause
      CALL DL_EXIT()              ! close up plot package
      STOP                        ! all done
      END
#endif
