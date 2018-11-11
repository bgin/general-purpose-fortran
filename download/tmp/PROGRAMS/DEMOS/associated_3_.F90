           program demo_associated
              implicit none
              real, target  :: tgt(2) = (/1., 2./)
              real, pointer :: ptr(:)
              ptr => tgt
              if (associated(ptr)     .eqv. .false.) call abort
              if (associated(ptr,tgt) .eqv. .false.) call abort
           end program demo_associated
