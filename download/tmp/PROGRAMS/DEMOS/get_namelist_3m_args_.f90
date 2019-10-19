           program demo_get_namelist
           use M_args,  only : unnamed, get_namelist, print_dictionary
           implicit none
           integer            :: i, ios
           character(len=255) :: message
           ! define namelist
           real               :: x, y, z
           logical            :: help, h, version, v
           namelist /args/ x,y,z,help,h,version,v
           ! equivalence short and long version and help options
           equivalence           (help,h),(version,v)
           ! define NAMELIST string that defines all NAMELIST
           ! group variables
           character(len=:),allocatable :: cmd
              cmd='&ARGS X=1 Y=2 Z=3 HELP=F H=F VERSION=F V=F/'
              ! initialize all values in NAMELIST by reading string
              read(cmd,nml=args,iostat=ios,iomsg=message)
              if(ios.eq.0)then
                 ! reduce NAMELIST string to just values on command line
                 cmd=get_namelist(cmd)
                 ! update NAMELIST group with values from command line
                 read(cmd,nml=args,iostat=ios,iomsg=message)
              endif
              if(ios.ne.0)then
                 call print_dictionary('ERROR: '//message)
                 stop 1
              endif
              ! all done. use values in program
              write(*,nml=args)
              if(size(unnamed).gt.0)then
                 write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
              endif
           end program demo_get_namelist
             program demo_get_namelist
                use M_args,  only : unnamed, get_namelist, print_dictionary
                implicit none
                integer                      :: i
                character(len=255) :: message ! use for I/O error messages
                character(len=:),allocatable :: readme ! stores updated namelist
                integer                      :: ios

             ! declare a namelist
                real               :: x, y, z, point(3)
                character(len=80)  :: title
                logical            :: help, version, l, l_, v, h
                equivalence       (help,h),(version,v)
                namelist /args/ x,y,z,point,title,help,h,version,v,l,l_

             ! Define the prototype
             !  o All parameters must be listed with a default value
             !  o string values  must be double-quoted
             !  o numeric lists must be comma-delimited. No spaces are allowed
                character(len=*),parameter  :: cmd=' -x 1 -y 2 -z 3 &
                & --point -1,-2,-3  &
                & --title "my title" -h --help -v --version -l -L'

                ! reading in a NAMELIST definition defining the entire
                ! NAMELIST group and then (optionally) do this again but
                ! the second time only getting options that are present on
                ! command line returned.  If not done twice equivalenced
                ! variables are not correct.
                do i=1,2
                   readme=get_namelist(cmd,all=i.eq.1)
                   read(readme,nml=args,iostat=ios,iomsg=message)
                   if(ios.ne.0)then
                      write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
                      call print_dictionary('OPTIONS:')
                      stop 1
                   endif
                enddo
                ! all done cracking the command line

                ! use the values in your program.
                write(*,nml=args)
                ! the optional unnamed values on the command line are
                ! accumulated in the character array "UNNAMED"
                if(size(unnamed).gt.0)then
                   write(*,'(a)')'files:'
                   write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
                endif

             end program demo_get_namelist
