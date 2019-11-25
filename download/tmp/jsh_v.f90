












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
      call trimit('@(#)Build Target ........ Linux_gfortran>')
      call trimit('@(#)Compiler Version .... '//trim(compiler_version())//'>')
      call trimit('@(#)Compiler Options .... '//trim(compiler_options())//'>')
      call trimit('@(#)Compile Date ........ '//&
     &'Mon Nov 25 05:16:26 EST 2019>')
     call trimit('@(#)Compiled on node:>')
      call trimit('@(#) Nodename ........... '// &
     &'venus>')
      call trimit('@(#) System Type ........ '// &
     &'Linux>')
      call trimit('@(#) O.S. Release ....... '// &
     &'4.4.0-18362-Microsoft>')
      call trimit('@(#) O.S. Version ....... ' &
     &//'#476-Microsoft ' &
     &//'Fri ' &
     &//'Nov ' &
     &//'01 ' &
     &//'16:53:00 ' &
     &//'PST ' &
     &//'2019 ' &
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
