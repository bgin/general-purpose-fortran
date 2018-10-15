program demo_M_system
use M_system, only: system_getppid, system_getpid
use M_system, only: system_rand, system_srand
use M_system, only: system_chdir
use M_system, only: system_putenv, set_environment_variable
implicit none
integer :: i
integer :: ierr

write(*,*)'PID=',system_getpid()
write(*,*)'PPID=',system_getppid()

do i=1,20
   write(*,*)system_rand()
enddo

do i=1,20
   write(*,*)system_rand()
enddo

call execute_command_line('pwd')
call system_chdir('/tmp',ierr)
call execute_command_line('pwd')
write(*,*)'*CHDIR* IERR=',ierr

call execute_command_line('env|grep GRU')
call system_putenv('GRU=this is the value',ierr)
call execute_command_line('env|grep GRU')
write(*,*)'*PUTENV* IERR=',ierr

call set_environment_variable('GRU','this is the NEW value',ierr)
write(*,*)'*SET_ENVIRONMENT_VARIABLE* IERR=',ierr
call execute_command_line('env|grep GRU')

end program demo_M_system
