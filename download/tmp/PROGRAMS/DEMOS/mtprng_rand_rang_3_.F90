          program demo_mtprng_rand_range
          use M_random, only : mtprng_state, mtprng_init, mtprng_rand_rang
          use, intrinsic :: iso_fortran_env, only : int32
          implicit none
          integer(INT32) :: seed
          type(mtprng_state) :: state
            seed = nint(100*secnds(0.))
            call mtprng_init(seed, state)
            write(*,*) mtprng_rand_rang(state,20,30)
          end program demo_mtprng_rand_range
