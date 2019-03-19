!-----------------------------------------------------------------------
subroutine jsh_v(i) ! assume long routine names are supported
use iso_fortran_env
      ! return version number in character variable version and print
      ! compile information to unit i
      if(i.ge.0)then
      write(i,'(1x,79("-"))')
      call trimit('@(#)File ................ jsh_v>')
      call trimit('@(#)Program Version ..... 0.0.0>')
      call trimit('@(#)Build Target ........ CYGWIN64_GFORTRAN>')
      call trimit('@(#)Compiler Version .... '//trim(compiler_version())//'>')
!     call trimit('@(#)Compiler Options .... '//trim(compiler_options())//'>')
      call trimit('@(#)Compile Date ........ '//&
     &'Mon Mar 18 23:21:10 EDT 2019>')
     call trimit('@(#)Compiled on node:>')
      call trimit('@(#) Nodename ........... '// &
     &'buzz>')
      call trimit('@(#) System Type ........ '// &
     &'CYGWIN_NT-10.0>')
      call trimit('@(#) O.S. Release ....... '// &
     &'2.10.0(0.325/5/3)>')
      call trimit('@(#) O.S. Version ....... ' &
     &//'2018-02-02 ' &
     &//'15:16 ' &
     &//'>')
      call trimit('@(#) Hardware Name ...... '//&
     &'x86_64>')
      write(i,'(1x,79("-"))')
      endif
      contains
         subroutine trimit(string) ! leave off metadata prefix
         character(len=*) :: string
         write(i,*)trim(string(5:len_trim(string)-1))
         end subroutine trimit
end subroutine jsh_v
!-----------------------------------------------------------------------
