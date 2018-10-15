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
'@(#)PROGRAM:        hell(1)>',&
'@(#)DESCRIPTION:    "hello world!" examples>',&
'@(#)VERSION:        1.0, 20180519>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Thu, Sep 20th, 2018 1:02:07 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
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
'   hello(1f) - [] "hello world!" examples                                       ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    hello(1f) - [] "hello world!" examples
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program hello
use ISO_FORTRAN_ENV, only : io => OUTPUT_UNIT
use M_kracken,only : kracken, lget                          ! add command-line parser module
implicit none
                                                            ! define command arguments, default values and crack command line
   call kracken('hello','-help .false. -version .false.')
   call help_usage(lget('hello_help'))                      ! if -help option is present, display help text and exit
   call help_version(lget('hello_version'))                 ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
write(io,'(a)')'program hello_world'
write(io,'(a)')'   write(*,*)''Hello World!'''
write(io,'(a)')'end program hello_world'
write(io,'(a)')'#!/bin/bash'
write(io,'(a)')'#@(#) name - description'
write(io,'(a)')'exit'
end program hello
