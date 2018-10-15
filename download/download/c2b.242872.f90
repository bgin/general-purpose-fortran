!=======================================================================--------====================================================
program c2b                               ! @(#) convert space-delimited columns of numbers into BINARY USH sequential file
use M_journal, only : journal
use M_kracken, only : kracken,lget,sget,setprompts
use M_debug,   only : debug
use M_zebra
character(len=1024)              :: filein            ! ASCII input file composed of columns of numbers or expressions
character(len=1024)              :: fileout           ! name of binary sequential output file
character(len=20)                :: itype             ! type of input file ("numeric" or "expression")
character(len=256)               :: delim             ! delimiter characters
!=======================================================================--------====================================================
   call iftrail()                  ! if $USHTRAIL environment variable set, set trail file to it; else set trail to a scratch file
!=======================================================================--------====================================================
!   define the command option prompts
    call setprompts('c2b', '                               &
    & -oo "#N#"                                            &
    & -i input filename of ASCII columns                   &
    & -o binary output data file name                      &
    & -itype type of column file (number or expression)    &
    & -delim delimiter characters                          &
    & -help "#N#"                                          &
    & -version "#N#"                                       &
    & ')
!  define the command options and default values and apply arguments from user command line
   call kracken("c2b", ' -i -o -itype expression -help F -version F -delim "#N#" -debug F')
!=======================================================================--------====================================================
   if(lget("c2b_help"))then                                                           ! check if -help switch is present
      call journal('================================================================================')
      call journal(' c2b -i INFILE -o OUTFILE -itype num*|exp* -delim ; [-help] [-version] [-debug] ')
      call journal('                                                                                ')
      call journal(' Convert a text column input file to a standard binary USH input file.          ')
      call journal(' -itype numbers                                                                 ')
      call journal('    Assumes values are simple numbers delimited by spaces or commas             ')
      call journal(' -itype expressions                                                             ')
      call journal('    Assumes values are numeric expressions delimited by spaces or semi-colons   ')
      call journal(' -delim delimiters                                                              ')
      call journal('    optional list of delimiter characters                                       ')
      call journal('    If ; or # is specified, use value of ''"delims"''                             ')
      call journal('================================================================================')
      stop                                                                    ! exit program
   endif
!=======================================================================--------====================================================
   if(lget("c2b_version"))then                                                ! check if -version switch is present
      call journal('*c2b* version 3.0-20131215')                                ! display version number
      stop 1                                                                  ! exit program
   endif
!=======================================================================--------====================================================
   debug=lget("c2b_debug")                                                    ! check if -debug switch is present
   delim=sget("c2b_delim")                                                    ! check delimiter list
   filein=sget("c2b_i")                                                       ! get input filename from command line
   fileout=sget("c2b_o")                                                      ! try to get filename from command line arguments
   itype=sget("c2b_itype")                                                    ! get command-line value
!=======================================================================--------====================================================
!  CHECK INPUT FILE TYPE
   i_type=0                                                                   ! only allow simple numeric values by default
   select case(itype(1:3))                                                    ! test input file type value for validity
   case('num')                                                                ! if type starts with "num" set to simple numbers
      i_type=0
   case('col','exp')                                                          ! flag to allow expressions instead of simple values
      i_type=1 ! allow expressions
   case default                                                               ! no valid value was input
      call journal('*c2b* error : unknown -itype value. Allowed values are:')
      call journal('    num     -- numbers ')
      call journal('    col|exp -- columns of expressions ')
      stop 2
   end select
!=======================================================================--------====================================================
   if(filein.eq.' ')then                                                      ! check input filename
      call journal('*c2b* error : -i input_file is required')
      stop 3
   endif
!=======================================================================--------====================================================
   if(fileout.eq.' ')then
      call journal('*c2b* error : -o output_file is required')                  ! check output filename
      stop 4
   endif
!=======================================================================--------====================================================
   if(i_type.eq.0)then
      if(delim.eq."#N#")then
         delim=" ,;"
      endif
      call txt2bin(trim(filein),trim(fileout),'num',delim)  ! simple number values
   else
      if(delim.eq."#N#")then
         delim=" ;"
      endif
      call txt2bin(trim(filein),trim(fileout),'col',delim)   ! allow numeric expressions
   endif
!=======================================================================--------====================================================
   end program c2b
!=======================================================================--------====================================================
