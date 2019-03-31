           program demo_anyscalar_to_double
           use M_anything,      only : real256
           use iso_fortran_env, only : int8, int16, int32, int64
           use iso_fortran_env, only : real32, real64, real128
           implicit none
              ! call same function with many scalar input types
              write(*,*)squarei(2_int8)
              write(*,*)squarei(2_int16)
              write(*,*)squarei(2_int32)
              write(*,*)squarei(2_int64)
              write(*,*)squarei(2_real32)
              write(*,*)squarei(2_real64)
              write(*,*)squarei(2_real128)
              write(*,*)squarei(2_real256)
           contains

           function squarei(invalue) result (dvalue)
           use M_anything, only : anyscalar_to_double
           class(*),intent(in)  :: invalue
           doubleprecision      :: invalue_local
           doubleprecision      :: dvalue
              invalue_local=anyscalar_to_double(invalue)
              dvalue=invalue_local*invalue_local
           end function squarei

           end program demo_anyscalar_to_double
