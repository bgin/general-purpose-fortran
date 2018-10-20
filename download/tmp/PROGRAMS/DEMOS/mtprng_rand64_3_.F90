          program demo_mtprng_rand64
          use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
          use, intrinsic :: iso_fortran_env, only : int32, int64
          implicit none
          integer(INT32) :: seed
          type(mtprng_state) :: state
            seed = nint(100*secnds(0.))
            call mtprng_init(seed, state)
            write(*,*) mtprng_rand64(state)
          end program demo_mtprng_rand64
