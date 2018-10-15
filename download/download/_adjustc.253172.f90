program test_adjustc
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_strings, only: adjustc
integer,parameter :: iwidth=80
character(len=iwidth) :: line
   write(*,'(8("1234567890"))')
   write(*,'(8(9x,i1))')(i,i=1,8)
   INFINITE: block
      do
         read(*,'(a)',iostat=ios)line
         if(ios.ne.0)exit INFINITE
         line=adjustc(trim(line),iwidth)
         write(*,'(a)')trim(line)
      enddo
   endblock INFINITE
end program test_adjustc
!-----------------------------------------------------------------------------------------------------------------------------------
