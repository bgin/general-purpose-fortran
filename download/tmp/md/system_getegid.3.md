[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                           Manual Reference Pages  - system_getegid (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    system_getegid(3f) - [M_system]get the effective group ID (GID) of current process from Fortran by calling getegid(3c)

CONTENTS

    Synopsis
    Description
    Return Value
    Errors
    See Also
    Example

SYNOPSIS

    integer(kind=c_int) function system_getegid()

DESCRIPTION

    The getegid() function returns the effective group ID of the calling process.

RETURN VALUE

    The getegid() should always be successful and no return value is reserved to indicate an error.

ERRORS

    No errors are defined.

SEE ALSO

    getegid(), system_geteuid(), getuid(), setegid(), seteuid(), setgid(), setregid(), setreuid(), setuid()

EXAMPLE

    Get group ID from Fortran

       program demo_system_getegid
       use M_system, only: system_getegid
       implicit none
          write(*,*) GID= ,system_getegid()
       end program demo_system_getegid



-----------------------------------------------------------------------------------------------------------------------------------

                                                        system_getegid (3)                                            July 02, 2017

Generated by manServer 1.08 from 534e31a2-56c6-4239-b702-065259b2f823 using man macros.
                                                           [system_get]