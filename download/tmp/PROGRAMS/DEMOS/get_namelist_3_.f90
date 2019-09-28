          program demo_get_namelist
          implicit none
          character(len=255)           :: message ! use for I/O error messages
          character(len=:),allocatable :: string  ! stores command line argument
          integer                      :: ios

          ! declare and initialize a namelist
          integer    :: i=1, j=2, k=3
          real       :: s=111.1, t=222.2, r=333.3
          real       :: arr(3)=[10.0,20.0,30.0]
          character(len=255) :: c=' '
          ! just add a variable here and it is a new parameter !!
          namelist /cmd/ i,j,k,s,t,r,c,arr

             ! return command line arguments as NAMELIST input
             string=get_namelist()
             ! internal read of namelist
             read(string,nml=cmd,iostat=ios,iomsg=message)
             if(ios.ne.0)then
                write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
                write(*,*)'OPTIONS:'
                write(*,nml=cmd)
                stop 1
             endif
             ! all done cracking the command line

             ! use the values in your program. For example ...
             sum=i+j+k
             write(*,*)'sum=',sum
       end program demo_get_namelist
