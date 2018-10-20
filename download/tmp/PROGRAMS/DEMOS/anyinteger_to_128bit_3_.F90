           program demo_anyinteger_to_128bit
           use M_anything,     only : int128
           use iso_fortran_env, only : int8, int16, int32, int64
           implicit none
              ! call same function with many scalar input types
              write(*,*)squarei(2_int8)
              write(*,*)squarei(2_int16)
              write(*,*)squarei(2_int32)
              write(*,*)squarei(2_int64)
              write(*,*)squarei(2_int128)
           contains

           function squarei(invalue)
           use M_anything, only : anyinteger_to_128bit, int128
           implicit none
           class(*),intent(in)  :: invalue
           real                 :: invalue_local
           integer(kind=int128) :: squarei
              invalue_local=anyinteger_to_128bit(invalue)
              dvalue=invalue_local*invalue_local
           end function squarei

           end program demo_anyinteger_to_128bit
