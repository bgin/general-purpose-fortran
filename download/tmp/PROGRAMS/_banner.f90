!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program block_letters
implicit none
call main()
contains
!==================================================================================================================================!
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
'    _banner(1f) - [FUNIX] print large block letters                             ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    _banner STRING(S) -c LETTER| --help| --version                              ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'    Print strings as large block letters.                                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    STRING(S)  strings to print as large block letters                          ',&
'    -c         letter to use to create block letters                            ',&
'    --help     display this help and exit                                       ',&
'    --version  output version information and exit                              ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'    To generate a large banner enter                                            ',&
'                                                                                ',&
'       _banner HELLO                                                            ',&
'                                                                                ',&
'       >  XXX XXX XXXXXXX XXXXX   XXXXX     XXX                                 ',&
'       >   X   X   X    X   X       X      X   X                                ',&
'       >   X   X   X        X       X     X     X                               ',&
'       >   X   X   X  X     X       X     X     X                               ',&
'       >   XXXXX   XXXX     X       X     X     X                               ',&
'       >   X   X   X  X     X       X     X     X                               ',&
'       >   X   X   X        X       X     X     X                               ',&
'       >   X   X   X    X   X   X   X   X  X   X                                ',&
'       >  XXX XXX XXXXXXX XXXXXXX XXXXXXX   XXX                                 ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
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
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        _banner(1f)>',&
'@(#)DESCRIPTION:    print text in big block letters>',&
'@(#)VERSION:        3.0, 20181028>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      Copyright (c) 1984, 1996 John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Oct 30th, 2019 9:50:52 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!==================================================================================================================================!
subroutine main()
use M_kracken, only : kracken, sget, lget
use M_messages, only : signs
character(len=1) :: letter
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('banner','-c X -help .f. -version .f. ')                 ! define and parse command line
   call help_usage(lget('banner_help'))                                  ! display help and stop if -help switch is present
   call help_version(lget('banner_version'))                             ! display version and stop if -version switch is present
!-----------------------------------------------------------------------------------------------------------------------------------
   letter=sget('banner_c')
   if(letter/='X')then
      call signs(sget('banner_oo'),6,letter)
   else
      call signs(sget('banner_oo'),6)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine main
!-----------------------------------------------------------------------------------------------------------------------------------
end program block_letters
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
