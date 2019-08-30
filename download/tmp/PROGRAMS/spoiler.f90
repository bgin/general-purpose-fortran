program spoiler
! A trifling program for reading punchlines that are
! encoded using the ROT-13 rotation encryption.
implicit none
character(len=256) :: line
integer            :: ios
   do
      read(*,'(a)',iostat=ios)line
      if(ios.ne.0)exit
      write(*,'(a)')rotate13(line)
   enddo
contains
function rotate13 (input)
implicit none

!@(#) M_strings::rotate13(3f): converts a character to its ROT13 equivalent, which is a trivial rotation encryption - JSU 20190827

character(len=*),intent(in) :: input
character(len=len(input))   :: rotate13
integer                     :: itemp
integer                     :: i
  rotate13=' '
  do i=1,len_trim(input)
     itemp = ichar (input(i:i))
     select case(itemp)
      case(65:77,97:109)
        itemp = itemp + 13
      case(78:90,110:122)
        itemp = itemp - 13
     end select
     rotate13(i:i) = char ( itemp )
  enddo

end function rotate13
end program spoiler
