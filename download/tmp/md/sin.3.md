[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                 Manual Reference Pages  - sin (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    sin(3f) - [INTRINSIC:TRIGONOMETRIC] Sine function

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

    result = sin(x)

DESCRIPTION

    sin(x) computes the sine of X.

ARGUMENTS

       X - The type shall be REAL or COMPLEX.

RETURN VALUE

    The return value has same type and kind as X.

EXAMPLE

    Sample program:

        program test_sin
          real :: x = 0.0
          x = sin(x)
        end program test_sin



STANDARD

    [[FORTRAN 77]] and later

CLASS

    [[Elemental function]]

SEE ALSO

    [[asin]], [[cos]], [[tan]]

-----------------------------------------------------------------------------------------------------------------------------------

                                                              sin (3)                                                 July 02, 2017

Generated by manServer 1.08 from f5f85644-a059-4c16-acb0-c37500d0e60a using man macros.
                                                               [sin]