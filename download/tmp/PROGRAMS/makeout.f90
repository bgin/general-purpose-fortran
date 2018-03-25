subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       makeout(1f) - [DEVELOPER] Generate a Makefile from the sources (C, Fortran) in the current directory',&
'SYNOPSIS                                                                        ',&
'       makeout program_files [-o [filename]] [-v][--version|--help]             ',&
'DESCRIPTION                                                                     ',&
'  If all the source for a set of programs exists in a single directory,         ',&
'  with a subdirectory containing the files with main programs, then makeout(1)  ',&
'  creates a make(1) file for the current directory and the specified programs.  ',&
'  A mixture of C and Fortran files is allowed. makeout(1) looks for             ',&
'  dependencies created by INCLUDE(7f), "#include", and USE(3f) statements       ',&
'  in their most common forms.                                                   ',&
'                                                                                ',&
'  The resulting makefile is quite simple, with few comments. The targets        ',&
'  and rules are explicit. The goal is to provide a well defined starting        ',&
'  point for your Fortran makefile. You will most certainly need to edit         ',&
'  the resultant makefile, but doing so is intended to be quite simple.          ',&
'                                                                                ',&
'  For more complex dependencies see your compiler information, as               ',&
'  several compilers have switches that will generate a very complete            ',&
'  list of dependencies.                                                         ',&
'                                                                                ',&
'       gfortran  -M        # GNU/Linux GCC, Free Software Foundation, Inc.      ',&
'       ifort     -gen-dep  # Intel Compiler Suite                               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       program_files  optional name of program files                            ',&
'                      Defaults to current directory.                            ',&
'       -o             output file. If not present, defaults to stdout.          ',&
'                      If present but no value is given "Makefile" is            ',&
'                      used.                                                     ',&
'       -v             verbose mode. Shows lines in input files that             ',&
'                      were used to create the dependencies.                     ',&
'       -mode          profile|production|debug                                  ',&
'                      If the default gfortran options are being used            ',&
'                      (ie. environment variables overide defaults)              ',&
'                      different compile and loader options are selected.        ',&
'                        profile     adds -pg option for gprof(1)                ',&
'                        production  good options for optimized performance      ',&
'                        debug       typical debug options                       ',&
'                                                                                ',&
'       --help         display command help and exit                             ',&
'       --version      output version information and exit EXAMPLES              ',&
'                                                                                ',&
'ENVIRONMENT                                                                     ',&
'   CC              defaults to "cc"                                             ',&
'   CFLAGS          defaults to "-O"                                             ',&
'                                                                                ',&
'   F90             defaults to "gfortran"                                       ',&
'   F90FLAGS        defaults to "-O"                                             ',&
'                                                                                ',&
'   FC              defaults to "f90"                                            ',&
'   FFLAGS          defaults to "-O"                                             ',&
'                                                                                ',&
'   LDFLAGS         defaults to "-s"                                             ',&
'   LIBS            defaults to "-lncurses -lsqlite3 -lreadline"                 ',&
'EXAMPLES                                                                        ',&
'    Common usage                                                                ',&
'                                                                                ',&
'        env F90=f90 makeout PROGRAMS/testit.f90 -o;make                         ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'  If your project needs exceeds the capabilities of makeout(1), see             ',&
'  the documentation for gmake(1), make(1), cmake(1), cpp(1), fpp(1),            ',&
'  automake(1), create_makefile(1), create_makefiles(1), gccmakedep(1),          ',&
'  imake(1), makedepend(1), xmkmf(1) and many resources on the WWW.              ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        makeout(1f) - [DEVELOPER] Generate a Makefile from the sources (C, Fortran) in the current directory
!!##SYNOPSIS
!!
!!        makeout program_files [-o [filename]] [-v][--version|--help]
!!##DESCRIPTION
!!   If all the source for a set of programs exists in a single directory,
!!   with a subdirectory containing the files with main programs, then makeout(1)
!!   creates a make(1) file for the current directory and the specified programs.
!!   A mixture of C and Fortran files is allowed. makeout(1) looks for
!!   dependencies created by INCLUDE(7f), "#include", and USE(3f) statements
!!   in their most common forms.
!!
!!   The resulting makefile is quite simple, with few comments. The targets
!!   and rules are explicit. The goal is to provide a well defined starting
!!   point for your Fortran makefile. You will most certainly need to edit
!!   the resultant makefile, but doing so is intended to be quite simple.
!!
!!   For more complex dependencies see your compiler information, as
!!   several compilers have switches that will generate a very complete
!!   list of dependencies.
!!
!!        gfortran  -M        # GNU/Linux GCC, Free Software Foundation, Inc.
!!        ifort     -gen-dep  # Intel Compiler Suite
!!
!!##OPTIONS
!!        program_files  optional name of program files
!!                       Defaults to current directory.
!!        -o             output file. If not present, defaults to stdout.
!!                       If present but no value is given "Makefile" is
!!                       used.
!!        -v             verbose mode. Shows lines in input files that
!!                       were used to create the dependencies.
!!        -mode          profile|production|debug
!!                       If the default gfortran options are being used
!!                       (ie. environment variables overide defaults)
!!                       different compile and loader options are selected.
!!                         profile     adds -pg option for gprof(1)
!!                         production  good options for optimized performance
!!                         debug       typical debug options
!!
!!        --help         display command help and exit
!!        --version      output version information and exit EXAMPLES
!!
!!##ENVIRONMENT
!!    CC              defaults to "cc"
!!    CFLAGS          defaults to "-O"
!!
!!    F90             defaults to "gfortran"
!!    F90FLAGS        defaults to "-O"
!!
!!    FC              defaults to "f90"
!!    FFLAGS          defaults to "-O"
!!
!!    LDFLAGS         defaults to "-s"
!!    LIBS            defaults to "-lncurses -lsqlite3 -lreadline"
!!##EXAMPLES
!!
!!     Common usage
!!
!!         env F90=f90 makeout PROGRAMS/testit.f90 -o;make
!!
!!##SEE ALSO
!!   If your project needs exceeds the capabilities of makeout(1), see
!!   the documentation for gmake(1), make(1), cmake(1), cpp(1), fpp(1),
!!   automake(1), create_makefile(1), create_makefiles(1), gccmakedep(1),
!!   imake(1), makedepend(1), xmkmf(1) and many resources on the WWW.
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        makeout(1f)>',&
'@(#)DESCRIPTION:    create Makefile for current directory>',&
'@(#)VERSION:        1.0, 2017-12-09>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Sun, Mar 4th, 2018 6:47:37 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------
program makeout
use M_io, only                         : splitpath, readline
use M_kracken, only                    : kracken, rget, lget, sgets, sget
use M_sort, only                       : sort_shell, unique
use M_strings, only                    : expand, split, lower, substitute, chomp
use M_system, only                     : system_opendir,system_readdir, system_closedir, system_stat, system_isdir
use M_time, only                       : now
use,intrinsic :: iso_c_binding, only   : c_ptr
use,intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT ! , INPUT_UNIT    ! access computing environment
implicit none

character(len=*),parameter::ident="@(#)makeout(1f): Generate a Makefile from the sources (C, Fortran) in the current directory."

character(len=:),allocatable    :: programs(:)
character(len=:),allocatable    :: fixed(:)
character(len=:),allocatable    :: directory
character(len=:),allocatable    :: sources
character(len=4096),allocatable :: sources_array(:)
character(len=4096),allocatable :: sources_array_lower(:)
character(len=:),allocatable    :: objects
character(len=:),allocatable    :: bases
type(c_ptr)                     :: dir
character(len=:),allocatable    :: filename
character(len=:),allocatable    :: outfile
character(len=:),allocatable    :: makeout_mode
logical                         :: makeout_v
integer                         :: i, io, ios
integer                         :: ierr
character(len=256)              :: message

integer,parameter               :: maxlen=4096
character(len=maxlen)           :: dirname
character(len=maxlen)           :: name
character(len=maxlen)           :: basename
character(len=maxlen)           :: ext
!----------------------------------------------------------------------------------------------------------------------------------
   sources=''                                                     ! list of files found starts with empty array
   objects=''                                                     ! list of target objects starts with empty array
   bases=''                                                       ! list of target names
!----------------------------------------------------------------------------------------------------------------------------------
   call kracken('makeout',' -help F -version F -v .F. -o "#N#" -mode ') ! define and crack command line
   call help_usage(lget('makeout_help'))
   call help_version(lget('makeout_version'))
   makeout_v=lget('makeout_v')
   makeout_mode=sget('makeout_mode')

   outfile=sget('makeout_o')                                      ! open output file if specified
   if(outfile.eq.'#N#')then
      io=OUTPUT_UNIT
   else
      if(outfile.eq.'')then
         outfile='Makefile'
      endif
      open(newunit=io,file=outfile,iostat=ios,iomsg=message,access='sequential',action='write',form='formatted',status='unknown')
      if(ios.ne.0)then
         write(ERROR_UNIT,'(a)')trim(message)
         stop 1
      endif
   endif
!----------------------------------------------------------------------------------------------------------------------------------
   write(io,'(a)')now('#@(#) Makefile started by makeout(1) year-month-day hour:minute:second') ! date comment starts make(1) file
!----------------------------------------------------------------------------------------------------------------------------------
   programs=sgets('makeout_oo')
   if(size(programs).ne.0)then
      write(io,'(/,"PROGFILES = ",*(5(a:,1x),"\",/,"        "))')(trim(programs(i)),i=1,size(programs))
   else
      write(io,'(/,"PROGFILES = ")')
   endif
   write(io,'(/,a)')'PROG = ${PROGFILES:.f90=}'
!----------------------------------------------------------------------------------------------------------------------------------
   directory='.'                                                  ! pathname of current directory
   call system_opendir(directory,dir,ierr)                        ! open directory stream to read from
   if(ierr.ne.0)stop 1
   do                                                             ! read directory
      call system_readdir(dir,filename,ierr)
      if(filename.eq.' ')exit
      filename=trim(directory)//'/'//trim(filename)
      if(system_isdir(filename))cycle                             ! skip directories
      call splitpath(filename, dirname, name, basename, ext)      ! split pathname into components
      PREFIXES: select case(ext)                                  ! look for desired file suffixes
      case('.f','.f90','.F','.F90','.c','.for','.f95','.f03','.ff','.FF','.shf')
         sources=sources//trim(name)//' '                         ! append source file names together into string
         objects=objects//trim(basename)//'.o '                   ! append source file basenames//'.o' together into string
         bases=bases//lower(trim(basename))//' '                  ! append source file basenames//'.o' together into string
      case('.h','.inc')
         sources=sources//trim(name)//' '                         ! append source file names together into string
         objects=objects//trim(basename)//'.o '                   ! append source file basenames//'.o ' together into string
         bases=bases//lower(trim(basename))//' '                  ! append source file basenames//' ' together into string
      endselect PREFIXES
   enddo
   call system_closedir(dir,ierr)                                 ! close directory stream
   if(ierr.ne.0)stop 3
!----------------------------------------------------------------------------------------------------------------------------------
   call pretty_print('SRCS',sources)
   call pretty_print('OBJS',objects)
!----------------------------------------------------------------------------------------------------------------------------------
   write(io,'(a)')repeat('# ',40)
   write(io,'(a)')'# platform-specific values you will probably change'
   write(io,'(a)')'# '
   select case(makeout_mode)
   case('debug')
      call printmakevar('LIBS','-lncurses -lsqlite3 -lreadline')
      call printmakevar('CC','cc')
      call printmakevar('CFLAGS',' &
      & -Og &
      & -Wall &
      & -pedantic &
      & -Wformat &
      & -Wunused &
      & -Wuninitialized &
      &')
      call printmakevar('FC','f90')
      call printmakevar('FFLAGS',' &
      & -std=f2008 &
      & -Og &
      & -fbounds-check &
      & -fbacktrace &
      & -finit-real=nan &
      & -fno-range-check &
      & -frecord-marker=4 &
      & -Wunreachable-code &
      & -Wunused &
      & -Wuninitialized &
      & -Wall &
      & -Wextra &
      & -fcheck=all &
      &')
      call printmakevar('F90','gfortran')
      call printmakevar('F90FLAGS',' &
      & -std=f2008 &
      & -Og &
      & -fbounds-check &
      & -fbacktrace &
      & -finit-real=nan &
      & -fno-range-check &
      & -frecord-marker=4 &
      & -Wunreachable-code &
      & -Wunused &
      & -Wuninitialized &
      & -Wall &
      & -Wextra &
      & -fcheck=all &
      &')
      call printmakevar('LDFLAGS',' -Wall ')
   case('profile')
      call printmakevar('LIBS','-lncurses -lsqlite3 -lreadline')
      call printmakevar('CC','cc')
      call printmakevar('CFLAGS','-pg')
      call printmakevar('FC','f90')
      call printmakevar('FFLAGS','-pg')
      call printmakevar('F90','gfortran')
      call printmakevar('F90FLAGS','-pg')
      call printmakevar('LDFLAGS','-pg')
   case('production')
      call printmakevar('LIBS','-lncurses -lsqlite3 -lreadline')
      call printmakevar('CC','cc')
      call printmakevar('CFLAGS','-O')
      call printmakevar('FC','f90')
      call printmakevar('FFLAGS','-O3  -march=native -Wall -fwhole-file -std=f2008')
      call printmakevar('F90','gfortran')
      call printmakevar('F90FLAGS','-O3  -march=native -Wall -fwhole-file -std=f2008')
      call printmakevar('F90','gfortran')
      call printmakevar('LDFLAGS','')
   case default
      call printmakevar('LIBS','-lncurses -lsqlite3 -lreadline')
      call printmakevar('CC','cc')
      call printmakevar('CFLAGS','-O')
      call printmakevar('FC','f90')
      call printmakevar('FFLAGS','-O')
      call printmakevar('F90','gfortran')
      call printmakevar('F90FLAGS','-O')
      call printmakevar('LDFLAGS','')
   endselect
   write(io,'(a)')repeat('# ',40)
!----------------------------------------------------------------------------------------------------------------------------------
   if(size(programs).eq.0)then
      write(io,'(a)')'#all: $(PROG)'
      write(io,'(a)')'all: $(OBJS)'
   else
      write(io,'(a)')'all: $(PROG)'
   endif
!----------------------------------------------------------------------------------------------------------------------------------
   fixed=[character(len=132) :: &
      &'                                                   ',&
      &'$(PROG): $(OBJS)                                   ',&
      &'                                                   ',&
      &'\t$(F90) $(LDFLAGS) $@.f90 -o $@ $(OBJS) $(LIBS)   ',&
      &'                                                   ',&
      &'clean:                                             ',&
      &'\trm -f $(PROG) $(OBJS) *.mod                      ',&
      &'                                                   ',&
      &'.SUFFIXES: $(SUFFIXES) .f90 .F90 .ff .FF .shf      ',&
      &'# .shf -- assumed to write Fortran code to stdout when executed  ',&
      &'# .FF -- run thru ufpp(1) with    $system directives allowed     ',&
      &'# .ff -- run thru ufpp(1) without $system directives allowed     ',&
      &'                                                   ',&
      &'.f90.o:                                            ',&
      &'\t$(F90) $(F90FLAGS) -c $<                         ',&
      &'                                                   ',&
      &'.F90.o:                                            ',&
      &'\t$(F90) $(F90FLAGS) -c $<                         ',&
      &'                                                   ',&
      &'.f95.o:                                            ',&
      &'\t$(F90) $(F90FLAGS) -c $<                         ',&
      &'                                                   ',&
      &'.F95.o:                                            ',&
      &'\t$(F90) $(F90FLAGS) -c $<                         ',&
      &'                                                   ',&
      &'.f03.o:                                            ',&
      &'\t$(F90) $(F90FLAGS) -c $<                         ',&
      &'                                                   ',&
      &'.F03.o:                                            ',&
      &'\t$(F90) $(F90FLAGS) -c $<                         ',&
      &'#=================================================================================',&
      &'# Fortran free format file known to have ufpp(1) preprocessor directives          ',&
      &'# run thru ufpp(1) preprocessor with system commands allowed, variable F90 defined',&
      &'# Assumes .F90 file does not exist previously, as it will overwrite it.           ',&
      &'.FF.o:                                                                            ',&
      &'\t@# run thru ufpp(1) preprocessor with system commands allowed                   ',&
      &'\t@[ -x $(*F).F90 ] || echo "error: $(*F).F90 exists"                             ',&
      &'\tufpp -D F90 OS=`uname -o` -verbose -system .true. -i $(<) -o $(*F).F90          ',&
      &'\t@[ -s $(*F).F90 ] || echo "error: $(*F).F90 is empty"                           ',&
      &'\t$(F90) $(F90FLAGS) -c $(*F).F90                                                 ',&
      &'# clean up scratch files                                                          ',&
      &'\t@rm -f $(*F).F90 # ${?F:.FF=.o}                                                 ',&
      &'#=================================================================================',&
      &'# Fortran free format file known to have ufpp(1) preprocessor directives          ',&
      &'# run thru ufpp(1) preprocessor with no system commands allowed, variable F90     ',&
      &'# defined. Assumes .F90 file does not exist previously, as it will overwrite it.  ',&
      &'.ff.o:                                                                            ',&
      &'\t@# run thru ufpp(1) preprocessor with system commands allowed                   ',&
      &'\t@[ -x $(*F).F90 ] || echo "error: $(*F).F90 exists"                             ',&
      &'\tufpp -D F90 OS=`uname -o` -verbose -i $(<) -o $(*F).F90                         ',&
      &'\t@[ -s $(*F).F90 ] || echo "error: $(*F).F90 is empty"                           ',&
      &'\t$(F90) $(F90FLAGS) -c $(*F).F90                                                 ',&
      &'# clean up scratch files                                                          ',&
      &'\t@rm -f $(*F).F90 # ${?F:.ff=.o}                                                 ',&
      &'#=================================================================================',&
      &'']
   do i=1,size(fixed)
      write(io,'(a)')expand(trim(fixed(i)))
   enddo
!----------------------------------------------------------------------------------------------------------------------------------
   call split(sources,sources_array,delimiters=' ',order='sequential',nulls='ignore')
   call split(bases,sources_array_lower,delimiters=' ',order='sequential',nulls='ignore')

   call find_dependencies()
!----------------------------------------------------------------------------------------------------------------------------------
contains
!----------------------------------------------------------------------------------------------------------------------------------
subroutine pretty_print(prefix,string)
!
! print a long list of words as a continued line broken into five words per line
! so the output file does not have excessively long lines in it when a lot of
! files are in the directory
!
implicit none
character(len=*),intent(in)      :: prefix
character(len=*),intent(in)      :: string
   character(len=:),allocatable  :: string_local
   character(len=:),allocatable  :: token
   integer                       :: icount
   icount=0
   write(io,'(/,a)',advance='no')trim(prefix)//' =  '    ! print the prefix string
   string_local=string
   do while ( chomp(string_local,token,delimiters=' ').ge. 0)  ! go through string one word at a time
      icount=icount+1                                    ! if five words have been written start new line
      if(icount.gt.5)then
         write(io,'(a)') ' \'
         write(io,'(a)',advance='no') '     '
         icount=0
      endif
      write(io,'(a,1x)',advance='no')trim(token)
   enddo
   write(io,'(a)')
end subroutine pretty_print
!----------------------------------------------------------------------------------------------------------------------------------
subroutine find_dependencies()
implicit none
   character(len=:),allocatable  :: depends
   character(len=:),allocatable  :: token
   integer                       :: i
   integer,parameter             :: maxlen=4096
   character(len=maxlen)         :: dirname
   character(len=maxlen)         :: name
   character(len=maxlen)         :: basename
   character(len=maxlen)         :: ext
   do i=1,size(sources_array)
      token=trim(sources_array(i))
      if(makeout_v)then
         write(io,'("# ",a)')repeat('=',78)
      endif
      depends=scanfile(token)
      call substitute(depends,token,'') ! kludge for if file contains multiple modules that reference name
      if(depends.ne.' ')then

         call splitpath(token, dirname, name, basename, ext)      ! split pathname into components
         PREFIXES: select case(ext)                               ! look for desired file suffixes
         case('.f','.f90','.F','.F90','.c','.for','.f95','.f03','.ff','.FF','.shf')
            token=trim(basename)//'.o '                           ! change source file names to object file names
         case('.h','.inc')
         case default
             write(ERROR_UNIT,'(a)')'WARNING: unexpected file suffix in filename ['//token//']'
         endselect PREFIXES
         call substitute(depends,token,'') ! kludge for if file contains multiple modules that reference name
         write(io,'(a,": ",a)')trim(token),depends
         !! strgar3.o: M_calculator.o M_calculator_plus.o M_journal.o
      endif
   enddo
end subroutine find_dependencies
!----------------------------------------------------------------------------------------------------------------------------------
function scanfile(filename) result (depends)
implicit none
character(len=*),intent(in) :: filename
   character(len=4096),allocatable :: array(:) ! output array of tokens
   character(len=:),allocatable    :: line
   character(len=:),allocatable    :: depends
   character(len=4096),allocatable :: depends_array(:)
   integer                         :: iunique
   integer                         :: i
   integer                         :: ios
   integer                         :: lun
   integer                         :: ifound
   character(len=256)              :: message
   integer,parameter               :: maxlen=4096
   character(len=maxlen)           :: dirname
   character(len=maxlen)           :: name
   character(len=maxlen)           :: basename
   character(len=maxlen)           :: ext
   depends=''
   open(newunit=lun,file=filename,iostat=ios,action='read',iomsg=message)
   if(ios.ne.0)then
      if(makeout_v)then
         write(ERROR_UNIT,'("ERROR: ",a,":",a)')trim(filename),trim(message)
      endif
      return
   endif

   INFINITE: do while (readline(line,lun)==0)
      ifound=index(line,'!')  ! remove Fortran comments
      if(ifound.ne.0)then
         line=line(:ifound-1)
      endif
      call split(line,array,delimiters=' :"'',',order='sequential',nulls='ignore')
      if(size(array).lt.2)cycle INFINITE
      select case(lower(array(1))) ! if first word is USE, or INCLUDE/#INCLUDE or MODULE
      case('include')
      case('$include')
      case('#include')
         if(array(2)(1:1).eq.'<')then     ! do not bother with system C files
            cycle INFINITE
         endif
      case('use')
         array(2)=lower(array(2))
         select case(array(2))
         case('intrinsic')  ! assuming no module is called these names
            cycle INFINITE
         case('non_intrinsic')  ! assuming no module is called these names
         if(size(array).ge.3)then
            array(2)=lower(array(3))
         endif
         endselect
         FINDFILE : block
         do i=1,size(sources_array)
            if(array(2).eq.sources_array_lower(i))then
               array(2)=sources_array(i)
               exit FINDFILE
            endif
         enddo
            if(makeout_v)then
               write(ERROR_UNIT,'(a)')'WARNING: no file found for module '//trim(array(2))
            endif
            cycle INFINITE  ! warning : file match not found
         endblock FINDFILE
         call splitpath(array(2), dirname, name, basename, ext)      ! split pathname into components
         array(2)=trim(basename)//'.o'
      case('module')
         array(2)=lower(array(2))
         cycle INFINITE
      case default
         cycle INFINITE
      endselect
      if(makeout_v)then
         write(io,'("# <<",a)')line
         !!write(io,'("# >>",*(a:,1x))')(trim(array(i)),i=1,size(array))
      endif
      depends=depends//trim(array(2))//' '

   enddo INFINITE

   call split(depends,depends_array,delimiters=' ',order='sequential',nulls='ignore')
   call sort_shell(depends_array,order='a')
   call unique(depends_array,iunique)

   depends=''
   do i=1,iunique
      depends=depends//trim(depends_array(i))//' '
   enddo

   close(unit=lun,iostat=ios) ! unconditionally close
end function scanfile
!----------------------------------------------------------------------------------------------------------------------------------
subroutine printmakevar(varname,default)
!>
!!
!!  given environment variable name and default value look in environment
!!  table for the variable and if it is set override the default, then
!!  print the value as "VARNAME = VALUE" in makefile
!===================================================================================================================================
implicit none
character(len=*),intent(in)     :: varname
character(len=*),intent(in)     :: default
   integer                      :: ilen
   integer                      :: istatus
   character(len=:),allocatable :: thevalue
   ilen=0
   call get_environment_variable (name=trim(varname),length=ilen,status=istatus)
   if(ilen.gt.0)then                                                                  ! variable is defined with a value
      allocate(character(len=ilen) :: thevalue)                                       ! make it as long as the value
      thevalue(:)=' '                                                                 ! keep current size but make it blank
      call get_environment_variable (name=trim(varname),value=thevalue)               ! get the variable value
      if(thevalue.ne.'')then
         write(io,'(a," = ",a)')trim(varname),trim(thevalue)
      else
         write(io,'(a," = ",a)')trim(varname),trim(default)
      endif
   elseif(istatus.eq.1)then                                                           ! variable does not exist so use default
      write(io,'(a," = ",a)')trim(varname),trim(default)
   else                                                                               ! variable is defined as blank
      write(io,'(a," = ")')trim(varname)
   endif
end subroutine printmakevar
end program makeout
!----------------------------------------------------------------------------------------------------------------------------------
!===============================================================================
! message
! *ufpp* CURRENT STATE
! *ufpp*    TOTAL LINES READ ...........         955
! *ufpp*    CONDITIONAL_NESTING_LEVEL...    0
! *ufpp*    G_WRITE (general processing)    T
! *ufpp*    G_LLWRITE (write input lines)   T
! *ufpp*    DATE........................ 18:47  4 Mar 2018
! *ufpp*    ARGUMENTS .................. TESTPRG90 -D CYGWIN -D CYGWIN64_GFORTRAN -D ENDCON=ENDCON -D LOADER_BUG=LOADER_BUG -I /tmp/CCALL_CYGWIN64_GFORTRAN_102676 -I /home/urbanjs/V600/LIBRARY/lib2zebra/inc -I /home/urbanjs/V600/LIBRARY/libDL/inc -I /home/urbanjs/V600/LIBRARY/libGCS/inc -I /home/urbanjs/V600/LIBRARY/libJSU/inc -I /home/urbanjs/V600/LIBRARY/libMULTI/inc -I /home/urbanjs/V600/LIBRARY/libObsolete/inc -I /home/urbanjs/V600/LIBRARY/libSTUG/inc -I /home/urbanjs/V600/LIBRARY/libUSH/inc -I /home/urbanjs/V600/LIBRARY/libcalcomp/inc -I /home/urbanjs/V600/LIBRARY/libgdi/inc -I /home/urbanjs/V600/LIBRARY/libgks2/inc -I /home/urbanjs/V600/LIBRARY/libmachine/inc -I /home/urbanjs/V600/LIBRARY/libncar/inc -I /home/urbanjs/V600/LIBRARY/libnogle/inc -I /home/urbanjs/V600/LIBRARY/libnswc/inc -I /home/urbanjs/V600/LIBRARY/librandlib/inc -I /home/urbanjs/V600/LIBRARY/libslatec/inc -I /home/urbanjs/V600/LIBRARY/libsteam67/inc -I /home/urbanjs/V600/LIBRARY/libtemplate/inc -I /home/urbanjs/V600/LIBRARY/libvg320/inc -I /home/urbanjs/V600/LIBRARY/libvogle/inc -I /home/urbanjs/V600/LIBRARY/libvopl/inc -I /home/urbanjs/V600/LIBRARY/libxyplot/inc -I ./LIBRARY/libGPF/EXE/MAKEOUT -verbose -system .true. -i ./LIBRARY/libGPF/EXE/MAKEOUT/makeout.ff -o /tmp/CCALL_CYGWIN64_GFORTRAN_102676/makeout.102676.f90 ! *ufpp* VARIABLES:
! *ufpp*    ! TESTPRG90                       !           1                    
! *ufpp*    ! CYGWIN                          !           1                    
! *ufpp*    ! CYGWIN64_GFORTRAN               !           1                    
! *ufpp*    ! ENDCON                          ! ENDCON                         
! *ufpp*    ! LOADER_BUG                      ! LOADER_BUG                     
! *ufpp* OPEN FILES:
! *ufpp*    ! ---- ! UNIT ! LINE NUMBER ! FILENAME
! *ufpp*    !    1 !   50 !         955 ! ./LIBRARY/libGPF/EXE/MAKEOUT/makeout.ff
! *ufpp* INCLUDE DIRECTORIES:
! /tmp/CCALL_CYGWIN64_GFORTRAN_102676
! /home/urbanjs/V600/LIBRARY/lib2zebra/inc
! /home/urbanjs/V600/LIBRARY/libDL/inc
! /home/urbanjs/V600/LIBRARY/libGCS/inc
! /home/urbanjs/V600/LIBRARY/libJSU/inc
! /home/urbanjs/V600/LIBRARY/libMULTI/inc
! /home/urbanjs/V600/LIBRARY/libObsolete/inc
! /home/urbanjs/V600/LIBRARY/libSTUG/inc
! /home/urbanjs/V600/LIBRARY/libUSH/inc
! /home/urbanjs/V600/LIBRARY/libcalcomp/inc
! /home/urbanjs/V600/LIBRARY/libgdi/inc
! /home/urbanjs/V600/LIBRARY/libgks2/inc
! /home/urbanjs/V600/LIBRARY/libmachine/inc
! /home/urbanjs/V600/LIBRARY/libncar/inc
! /home/urbanjs/V600/LIBRARY/libnogle/inc
! /home/urbanjs/V600/LIBRARY/libnswc/inc
! /home/urbanjs/V600/LIBRARY/librandlib/inc
! /home/urbanjs/V600/LIBRARY/libslatec/inc
! /home/urbanjs/V600/LIBRARY/libsteam67/inc
! /home/urbanjs/V600/LIBRARY/libtemplate/inc
! /home/urbanjs/V600/LIBRARY/libvg320/inc
! /home/urbanjs/V600/LIBRARY/libvogle/inc
! /home/urbanjs/V600/LIBRARY/libvopl/inc
! /home/urbanjs/V600/LIBRARY/libxyplot/inc
! ./LIBRARY/libGPF/EXE/MAKEOUT
!===============================================================================
