!-----------------------------------------------------------------------
subroutine jsh_v(i) ! assume long routine names are supported
use iso_fortran_env
implicit none
integer,intent(in) :: i
      ! return version number in character variable version and print
      ! compile information to unit i
      if(i.ge.0)then
      write(i,'(1x,79("-"))')
      call trimit('@(#)File ................ jsh_v>')
      call trimit('@(#)Program Version ..... 0.0.0>')
      call trimit('@(#)Build Target ........ CYGWIN64_GFORTRAN>')
      call trimit('@(#)Compiler Version .... '//trim(compiler_version())//'>')
      call trimit('@(#)Compiler Options .... '//trim(compiler_options())//'>')
      call trimit('@(#)Compile Date ........ '//&
     &'Wed, Jan  1, 2020  1:42:21 AM>')
     call trimit('@(#)Compiled on node:>')
      call trimit('@(#) Nodename ........... '// &
     &'venus>')
      call trimit('@(#) System Type ........ '// &
     &'CYGWIN_NT-10.0>')
      call trimit('@(#) O.S. Release ....... '// &
     &'3.1.0(0.340/5/3)>')
      call trimit('@(#) O.S. Version ....... ' &
     &//'2019-10-10 ' &
     &//'14:06 ' &
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
