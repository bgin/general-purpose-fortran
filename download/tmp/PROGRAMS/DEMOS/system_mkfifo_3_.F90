          program demo_system_mkfifo
          use M_system, only : system_mkfifo, system_perror
          use M_system, only : R_GRP,R_OTH,R_USR,R_WXG,R_WXO
          use M_system, only : R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
          use M_system, only : DEFFILEMODE, ACCESSPERMS
          implicit none
             integer :: status
             status = system_mkfifo("/home/cnd/mod_done", IANY([W_USR, R_USR, R_GRP, R_OTH]))
             if(status.ne.0)then
                call system_perror('*mkfifo* error:')
             endif
          end program demo_system_mkfifo
