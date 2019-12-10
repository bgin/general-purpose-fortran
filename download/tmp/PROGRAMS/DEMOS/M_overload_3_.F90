          program demo_M_overload
          use M_compare_float_numbers, only : operator(.EqualTo.)

          use M_overload, only : int          ! allow strings to be converted to integers
          use M_overload, only : real,dble    ! allow strings to be converted to floating point
          use M_overload, only : operator(==) ! use == like .eqv.
          use M_overload, only : operator(/=) ! use /= like .neqv.

          if(int('1234')               .eq.1234)                      write(*,*)'int("STRING") works '
          if(real('1234.56789')        .EqualTo.1234.56789)           write(*,*)'real("STRING") works '
          if(dble('1234.5678901234567').EqualTo.1234.5678901234567d0) write(*,*)'dble("STRING") works '

          if (.true. == .true. )  write(*,*)'== works like .eqv. for LOGICAL values'
          if (.true. /= .false. ) write(*,*)'/= works like .neqv. for LOGICAL values'

          write(*,*)int(['111','222'])+333
          write(*,*)real(['111.111','222.222'])+333.333
          write(*,*)dble(['111.111d0','222.222d0'])+333.333d0
          write(*,*)dble([character(len=10) :: '111','222.222','333.333d0'])+444.444d0

          end program demo_M_overload
