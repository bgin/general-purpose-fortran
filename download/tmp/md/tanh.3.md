[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - tanh (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    tanh(3f) - [INTRINSIC:TRIGONOMETRIC] Hyperbolic tangent function

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

    x = tanh(x)

DESCRIPTION

    tanh(x) computes the hyperbolic tangent of X.

ARGUMENTS

       X - The type shall be REAL or COMPLEX.

RETURN VALUE

    The return value has same type and kind as X. If X is complex, the imaginary part of the result is in radians. If X is REAL,
    the return value lies in the range

          -1 <= tanh(x) <= 1.



EXAMPLE

    Sample program:

       program test_tanh
         real(8) :: x = 2.1_8
         x = tanh(x)
       end program test_tanh



STANDARD

    [[FORTRAN 77]] and later, for a complex argument [[Fortran 2008]] or later

CLASS

    [[Elemental procedure|Elemental function]]

SEE ALSO

    [[atanh]]

-----------------------------------------------------------------------------------------------------------------------------------

                                                             tanh (3)                                                 July 02, 2017

Generated by manServer 1.08 from 77692ef9-972b-4e98-a857-bb872864660f using man macros.
                                                              [tanh]