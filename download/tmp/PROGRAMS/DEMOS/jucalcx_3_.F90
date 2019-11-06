           program demo_jucalcx
           use M_calculator,      only : iclen_calc
           use M_calculator, only : jucalcx
           character(len=iclen_calc) ::  outlin0
           doubleprecision :: outval
           call jucalcx('A=3.4**5    ',outval,outlin0,ierr,ilen)
           write(*,*)'value of expression is ',outval
           write(*,*)'string representation of value is ',trim(outlin0)
           write(*,*)'error flag value is ',ierr
           write(*,*)'length of expression is ',ilen
           end program demo_jucalcx
