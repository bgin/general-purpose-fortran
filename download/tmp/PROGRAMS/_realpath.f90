program demo_system_realpath
!(LICENSE:PD)
use M_system, only : system_realpath, system_perror
implicit none
character(len=:),allocatable :: pathi,patho
integer                      :: i
integer                      :: filename_length
   ! get pathname from command line arguments
   do i = 1, command_argument_count()
      call get_command_argument (i , length=filename_length)
      allocate(character(len=filename_length) :: pathi)
      call get_command_argument (i , value=pathi)
      patho= system_realpath(pathi)
      if(patho.eq.char(0))then
         write(*,'(a)')trim(patho)
      else
         call system_perror('*system_realpath* error for pathname '//trim(pathi)//':')
         write(*,'(a)')trim(patho)
      endif
      deallocate(pathi)
   enddo
   if(i.eq.1)then
      patho=system_realpath('.')
      write(*,'(a)')trim(patho)
   endif
end program demo_system_realpath
