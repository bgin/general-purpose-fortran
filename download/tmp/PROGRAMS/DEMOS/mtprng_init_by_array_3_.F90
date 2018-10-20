          program demo_mtprng_init_by_array
          use M_random, only : mtprng_state, mtprng_init_by_array
          use M_random, only : mtprng_rand64, mtprng_rand_real1
          use, intrinsic :: iso_fortran_env, only : int32, int64
          implicit none
          integer(INT32)     :: init_key(3)
          type(mtprng_state) :: state
            init_key(1) = nint(11*secnds(0.))
            init_key(2) = nint(37*secnds(0.))
            init_key(3) = nint(97*secnds(0.))
            call mtprng_init_by_array(init_key, state )
            ! returns a INT64 integer with a range in 0 .. 2^32-1
            write(*,*) mtprng_rand64(state)
            ! returns a IEEE64 real, may be used as double precision
            write(*,*) mtprng_rand_real1(state)
          end program demo_mtprng_init_by_array
