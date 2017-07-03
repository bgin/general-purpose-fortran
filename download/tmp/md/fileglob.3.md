[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                              Manual Reference Pages  - fileglob (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    fileglob(3f) - [M_system] Read output of an ls(1) command from Fortran

CONTENTS

    Synopsis
    Description
    Example

SYNOPSIS

    subroutine fileglob(glob, list)


       character(len=*),intent(in)   :: glob                   ! Pattern for the filenames (like: *.txt)
       character(len=*),pointer      :: list(:)                ! Allocated list of filenames (returned), the caller must deallocate it.



DESCRIPTION

    Non-portable procedure uses the shell and the ls(1) command to expand a filename and returns a pointer to a list of expanded
    filenames.

EXAMPLE

    Read output of an ls(1) command from Fortran

       program test_fileglob  ! simple unit test
          call testit( *.* )
          call testit( /tmp/__notthere.txt )
       end program test_fileglob


       subroutine testit(string)
          use M_system, only : fileglob
          character(len=255),pointer :: list(:)
          character(len=*) :: string
          call fileglob(string, list)
          write(*,*) Files: ,size(list)
          write(*, (a) )(trim(list(i)),i=1,size(list))
          deallocate(list)
       end subroutine testit



-----------------------------------------------------------------------------------------------------------------------------------

                                                           fileglob (3)                                               July 02, 2017

Generated by manServer 1.08 from 9c18d793-9f03-45a6-90aa-2b05290dedd2 using man macros.
                                                            [fileglob]