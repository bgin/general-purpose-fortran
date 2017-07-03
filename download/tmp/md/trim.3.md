[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - trim (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    trim(3f) - [INTRINSIC:CHARACTER] Remove trailing blank characters of a string

CONTENTS

    Syntax
    Description
    Arguments
    Return Value
    Example
    Standard
    Class
    See Also

SYNTAX

    result = trim(string)

DESCRIPTION

    Removes trailing blank characters of a string.

ARGUMENTS

        STRING - Shall be a scalar of type CHARACTER.

RETURN VALUE

    A scalar of type CHARACTER which length is that of STRING less the number of trailing blanks.

EXAMPLE

    Sample program:

        program test_trim
          character(len=10), parameter :: s = "gfortran  "
          write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks
        end program



STANDARD

    [[Fortran 95]] and later

CLASS

    Transformational function

SEE ALSO

    [[adjustl]], [[adjustr]]

-----------------------------------------------------------------------------------------------------------------------------------

                                                             trim (3)                                                 July 02, 2017

Generated by manServer 1.08 from 2d56a1ac-7529-476e-9469-45aef678cb35 using man macros.
                                                              [trim]