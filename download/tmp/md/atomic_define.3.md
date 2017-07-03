[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                            Manual Reference Pages  - atomic_define (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    atomic_define(3f) - [INTRINSIC] Setting a variable atomically

CONTENTS

    Syntax
    Description
    Arguments
    Example
    Standard
    Class
    See Also

SYNTAX

    call atomic_define (atom, value [, stat])

DESCRIPTION

    atomic_define(atom, value) defines the variable ATOM with the value VALUE atomically. When STAT is present and the invocation
    was successful, it is assigned the value 0. If it is present and the invocation has failed, it is assigned a positive value; in
    particular, for a coindexed ATOM, if the remote image has stopped, it is assigned the value of iso_fortran_env s
    stat_stopped_image and if the remote image has failed, the value stat_failed_image.

ARGUMENTS

             ATOM - Scalar coarray or coindexed variable of either integer type with atomic_int_kind kind or logical type with
             atomic_logical_kind kind.

             VALUE - Scalar of the same type as ATOM. If the kind is different, the value is converted to the kind of ATOM.

             STAT - (optional) Scalar default-kind integer variable.

EXAMPLE

    Sample program:

       program atomic
         use iso_fortran_env
         integer(atomic_int_kind) :: atom[*]
         call atomic_define(atom[1], this_image())
       end program atomic



STANDARD

    [[Fortran 2008]] and later; with STAT, [[TS 18508]] or later

CLASS

    Atomic subroutine

SEE ALSO

    [[atomic_ref]], [[atomic_cas]], [[iso_fortran_env]], [[atomic_add]], [[atomic_and]], [[atomic_or]], [[atomic_xor]]

-----------------------------------------------------------------------------------------------------------------------------------

                                                         atomic_define (3)                                            July 02, 2017

Generated by manServer 1.08 from eb9fec1e-6477-4bf5-82f0-b59b7f2a4a99 using man macros.
                                                           [atomic_def]