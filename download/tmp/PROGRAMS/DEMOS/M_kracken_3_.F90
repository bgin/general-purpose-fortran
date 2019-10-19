            program demo_placement
            use M_kracken, only : kracken
            ! define and crack command line arguments
            call kracken('cmd',' DEFAULT STRING -x 123 -y 456 ')
            call showstring()
            call showvalue()
            contains

            subroutine showstring()
            use M_kracken, only : sget
            character(len=:),allocatable :: string
            ! get value of string before any switch
            string=trim(sget('cmd_oo'))
            write(*,*)'string is ',string
            end subroutine showstring

            subroutine showvalue()
            use M_kracken, only : rget
            ! show values for -x and -y parameters
            x=rget('cmd_x')
            y=rget('cmd_y')
            write(*,*)' X and Y are ',x,y
            end subroutine showvalue

            end program demo_placement
            program demo_placement
            use M_kracken, only : kracken, sgets, rget, sget, lget
            implicit none
            character(len=:),allocatable :: files(:)
            character(len=:),allocatable :: title
            real :: x,y
            integer :: i
            ! define and crack command line arguments
            call kracken('cmd',' --title this is my title -x 123 -y 456 --help .false.')

            title=sget('cmd_title')
            x=rget('cmd_x')
            y=rget('cmd_y')
            write(*,*)'help=',lget('cmd_help')
            write(*,*)' title is ',title
            write(*,*)' x and y are ',x,y
            ! get value of string before any switch
            files=sgets('cmd_oo')
            if(size(files).gt.0)then
               do i=1,size(files)
                  write(*,*)i,files(i)
               enddo
            endif
            end program demo_placement
            program demo_placement
            use M_kracken, only : kracken, sget, rget, lget
            USE M_KRACKEN, ONLY : KRACKEN_METHOD, UNNAMED
            implicit none
            character(len=:),allocatable :: title
            real :: x,y
            integer :: i
            KRACKEN_METHOD='ARGS'
            ! define and crack command line arguments
            !
            ! One difference in how you specify your command prototype:
            !
            !  You MUST use the string ".false." for any switch variable;
            !  and basically the values ".true." and ".false." are
            !  reserved, regardless of case.
            !
            call kracken('cmd',' --title this is my title -x 123 -y 456 --help .false.')
            title=sget('cmd_title')
            x=rget('cmd_x')
            y=rget('cmd_y')
            write(*,*)'help=',lget('cmd_help')
            write(*,*)' title is ',title
            write(*,*)' x and y are ',x,y

            IF(SIZE(UNNAMED).GT.0)THEN
               DO I=1,SIZE(UNNAMED)
                  WRITE(*,*)I,UNNAMED(I)

   ENDDO
   ENDIF
       end program demo_placement
