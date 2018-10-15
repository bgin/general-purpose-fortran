        program fonts
! *** LAST REVISED ON 20-FEB-1995 19:28:07.77
! *** SOURCE FILE: [LONGD]SYMTEST.FOR
!
!       this program demonstrates the fonts of the libDL DL_SYMS routine
!
!       this version is for compilers that don't interpret \ characters
!
        call dl_init(0,8.5,8.5,0.25,0.5,1.0)

      a=dl_syms(1.75,0.0,0.25,'\4SYMS \3Character Fonts',0.,24,-1)
      a=dl_syms(0.,0.5,.25, '\2A\0=\7R\6a\^2\[\]\_\6n\[\]\]\]\=\U\6n=1\@\]\]\O\1K',0.,99,-1)
      call dl_symbol(1.5,0.5,.12, '"\2A\0=\7R\6a\^2\[\]\_\6n\[\]\]\]\=\U\6n=1\@\]\]\O\1K"',0.,99,-1)
!
!              infinity    2
!           A = SIGMA alpha
!              zeta=1      zeta

      a=dl_syms(0.0,1.0,0.12,'\0Plot Symbols',0.,33,-1)
        do i=1,32
          x=(i-1)*0.12+1.75
          a=dl_syms(x,1.0,0.12,char(i-1),0.,1,-1)
        end do

      a=dl_syms(0.0,1.5,0.12,'\00 Simplex (def)',0.,33,-1)
      a=dl_syms(1.75,1.5,0.12, '\0abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,1.35,0.12, '\001234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,2.,0.12,'\01 Math Symbols ',0.,33,-1)
      a=dl_syms(1.75,2.,0.12, '\1abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,1.75,0.12, '\101234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,2.5,0.12,'\02 Simplex Italic ',0.,33,-1)
      a=dl_syms(1.75,2.5,0.12, '\2abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,2.35,0.12, '\201234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,3.,0.12,'\03 Roman',0.,33,-1)
      a=dl_syms(1.75,3.0,0.12, '\3abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,2.75,0.12, '\301234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,3.5,0.12,'\04 Roman Italic ',0.,33,-1)
      a=dl_syms(1.75,3.5,0.12, '\4abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,3.35,0.12, '\401234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,4.0,0.12,'\05 Duplex',0.,33,-1)
      a=dl_syms(1.75,4.0,0.12, '\5abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,3.75,0.12, '\501234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,4.5,0.12,'\06 Simplex Greek ',0.,33,-1)
      a=dl_syms(1.75,4.5,0.12, '\6abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,4.35,0.12, '\601234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,5.0,0.12,'\07 Complex Greek ',0.,33,-1)
      a=dl_syms(1.75,5.0,0.12, '\7abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,4.75,0.12, '\701234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      a=dl_syms(0.,5.5,0.12,'\08 Fixed Width',0.,33,-1)
      a=dl_syms(1.75,5.5,0.12, '\8abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',0.,99,-1)
      a=dl_syms(1.75,5.35,0.12, '\801234567890-=`~!_+[]{}:"|;''\\,./?<>',0.,99,-1)

      call dl_pause()
      call dl_exit()
      stop
      end
