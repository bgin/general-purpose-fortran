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
'       _stat(1f) - [FUNIX]list file properties                                  ',&
'SYNOPSIS                                                                        ',&
'       _stat pathname|--version|--help                                          ',&
'DESCRIPTION                                                                     ',&
'       Given a pathname list properties                                         ',&
'OPTIONS                                                                         ',&
'       directory   name of pathname to display properties of.                   ',&
'       --help      display command help and exit                                ',&
'                                                                                ',&
'       --version   output version information and exit                          ',&
'EXAMPLES                                                                        ',&
' Sample command lines ...                                                       ',&
'                                                                                ',&
'        _stat /tmp                                                              ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _stat(1f) - [FUNIX]list file properties
!!##SYNOPSIS
!!
!!        _stat pathname|--version|--help
!!##DESCRIPTION
!!        Given a pathname list properties
!!##OPTIONS
!!        directory   name of pathname to display properties of.
!!        --help      display command help and exit
!!
!!        --version   output version information and exit
!!##EXAMPLES
!!
!!  Sample command lines ...
!!
!!         _stat /tmp
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
'@(#)PROGRAM:        _stat(1f)>',&
'@(#)DESCRIPTION:    list pathname properties>',&
'@(#)VERSION:        1.0, 2017-10-00>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Sun, Oct 8th, 2017 1:25:25 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------
program demo_system_stat
use M_kracken, only  : kracken, rget, lget, sgets
use M_system, only : system_stat, system_isdir
use iso_c_binding, only : c_ptr
implicit none
character(len=:),allocatable :: filenames(:)
character(len=:),allocatable :: filename
integer                      :: ierr
integer(kind=8)              :: values8(13)
integer                      :: i
!----------------------------------------------------------------------------------------------------------------------------------
   call kracken('stat','. -help F -version F')
   call help_usage(lget('stat_help'))
   call help_version(lget('stat_version'))

   filenames=sgets('stat_oo')
   do i=1,size(filenames)
      filename=trim(filenames(i))
      call system_stat(filename,values8,ierr)
      if(ierr.eq.0)then
         write(*,'("Pathname:",T30,a)')filename
         call printit()
      endif
   enddo
contains
!----------------------------------------------------------------------------------------------------------------------------------
subroutine printit
use M_system, only : system_getpwuid, system_getgrgid, system_perm
use M_time, only :   fmtdate, u2d
implicit none
   character(len=*),parameter   :: fmt_date='year-month-day hour:minute:second'

   write(*, FMT="('Residence:',                   T30)",advance='no')
   write(*, FMT="('Inode:',                       I0)",advance='no') values8(2)
   write(*, FMT="('  Device ID(hex/decimal):',    Z0,'h/',I0,'d')",advance='no') values8(1),values8(1)
   write(*, FMT="('  Device where located:',      I0)",advance='no') values8(7)
   write(*,*)

   write(*, FMT="('Size:',                        T30)",advance='no')
   write(*, FMT="('File size(bytes):',            I0)",advance='no') values8(8)
   write(*, FMT="('  No. of blocks allocated:',   I0)",advance='no') values8(13)
   write(*, FMT="('  Preferred block size(bytes):', I0)",advance='no') values8(12)
   write(*,*)

   write(*, FMT="('File mode octal/decimal/str:',  T30, o0,'o')",advance='no') values8(3)
   write(*, FMT="('/',i0,'d')",advance='no') values8(3)
   write(*,'("/",a)')system_perm(values8(3))

   write(*, FMT="('Number of links:',             T30, I0)") values8(4)
   write(*, FMT="('Owner''s uid/username:',       T30, I0,'/', A)") values8(5), system_getpwuid(values8(5))
   write(*, FMT="('Owner''s gid/group:',          T30, I0,'/', A)") values8(6), system_getgrgid(values8(6))

   write(*, FMT="('Last access time:',            T30, I0,1x, A)") values8(9), fmtdate(u2d(int(values8(9))),fmt_date)
   write(*, FMT="('Last modification time',       T30, I0,1x, A)") values8(10),fmtdate(u2d(int(values8(10))),fmt_date)
   write(*, FMT="('Last status change time:',     T30, I0,1x, A)") values8(11),fmtdate(u2d(int(values8(11))),fmt_date)

end subroutine printit
!----------------------------------------------------------------------------------------------------------------------------------
end program demo_system_stat
!----------------------------------------------------------------------------------------------------------------------------------
