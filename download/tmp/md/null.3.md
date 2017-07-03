[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - null (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    null(3f) - [INTRINSIC] Function that returns an disassociated pointer

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

    ptr => null([mold])

DESCRIPTION

    Returns a disassociated pointer.

    If MOLD is present, a disassociated pointer of the same type is returned, otherwise the type is determined by context.

    In [[Fortran 95]], MOLD is optional. Please note that [[Fortran 2003]] includes cases where it is required.

ARGUMENTS

          MOLD - (Optional) shall be a pointer of any association status and of any type.

RETURN VALUE

    A disassociated pointer.

EXAMPLE

    Sample program:

        real, pointer, dimension(:) :: vec => null ()



STANDARD

    [[Fortran 95]] and later

CLASS

    Transformational function

SEE ALSO

    [[associated]]

-----------------------------------------------------------------------------------------------------------------------------------

                                                             null (3)                                                 July 02, 2017

Generated by manServer 1.08 from 5d1be59b-ad3a-4899-81a6-987ee64adc9a using man macros.
                                                              [null]