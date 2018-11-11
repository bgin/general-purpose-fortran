           program demo_anyinteger_to_128bit
           use M_anything,     only : int128
           use iso_fortran_env, only : int8, int16, int32, int64
           implicit none
              ! call same function with many scalar input types
              write(*,*)squarei(huge(0_int8)),huge(0_int8) , &
              & '16129'
              write(*,*)squarei(huge(0_int16)),huge(0_int16) , &
              & '1073676289'
              write(*,*)squarei(huge(0_int32)),huge(0_int32) , &
              & '4611686014132420609'
              write(*,*)squarei(huge(0_int64)),huge(0_int64) , &
              & '85070591730234615847396907784232501249'
              write(*,*)squarei(huge(0_int128)),huge(0_int128) , &
              & '28948022309329048855892746252171976962977213799489202546401021394546514198529'
           contains
           !
           function squarei(invalue)
           use M_anything, only : anyinteger_to_128bit, int128
           class(*),intent(in)  :: invalue
           doubleprecision      :: invalue_local
           doubleprecision      :: squarei
              invalue_local=anyinteger_to_128bit(invalue)
              squarei=invalue_local*invalue_local
           end function squarei
           !
           end program demo_anyinteger_to_128bit
