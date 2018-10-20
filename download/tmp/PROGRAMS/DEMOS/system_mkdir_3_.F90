          program demo_system_mkdir
          use M_system, only : system_perror
          use M_system, only : system_mkdir
          use M_system, only : R_GRP,R_OTH,R_USR,R_WXG,R_WXO
          use M_system, only : R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
          use M_system, only : DEFFILEMODE, ACCESSPERMS
          implicit none
          integer :: ierr
          ierr=system_mkdir('_scratch',IANY([R_USR,W_USR,X_USR)])
          end program demo_system_mkdir
