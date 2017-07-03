[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                              Manual Reference Pages  - junbuster (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    junbuster(3f) - [M_messages]call journal(3f) to print eye-catching ASCII graphic (ghostbuster)

CONTENTS

    Synopsis
    Description
    Example

SYNOPSIS



    SUBROUTINE junbuster(where)


           character(len=*),intent(in) :: where



DESCRIPTION

    Sample output:

      >                        __---__
      >                     _-       _--______
      >                __--( /     \ )XXXXXXXXXXXXX_
      >              --XXX(   O   O  )XXXXXXXXXXXXXXX-
      >             /XXX(       U     )        XXXXXXX\
      >           /XXXXX(              )--_  XXXXXXXXXXX\
      >          /XXXXX/ (      O     )   XXXXXX   \XXXXX\
      >          XXXXX/   /            XXXXXX   \__ \XXXXX----
      >          XXXXXX__/          XXXXXX         \__----  -
      >  ---___  XXX__/          XXXXXX      \__         ---
      >    --  --__/   ___/\  XXXXXX            /  ___---=
      >      -_    ___/    XXXXXX               --- XXXXXX
      >        --\/XXX\ XXXXXX                      /XXXXX
      >          \XXXXXXXXX                        /XXXXX/
      >           \XXXXXX                        _/XXXXX/
      >             \XXXXX--__/              __-- XXXX/
      >              --XXXXXXX---------------  XXXXX--
      >                 \XXXXXXXXXXXXXXXXXXXXXXXX-
      >                   --XXXXXXXXXXXXXXXXXX-





    See the description of JOURNAL() for a meaning for the I/O flag.

EXAMPLE

    Sample program:

       program seebuster
       use M_messages, only : junbuster
          call junbuster( s )
       end program seebuster



-----------------------------------------------------------------------------------------------------------------------------------

                                                           junbuster (3)                                              July 02, 2017

Generated by manServer 1.08 from 48c59033-2d49-49b6-9d8b-124047723986 using man macros.
                                                            [junbuster]