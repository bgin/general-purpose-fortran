[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - vinit (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    vinit(3f) - [M_pixel] initialize pixel graphics module

CONTENTS

    Synopsis
    Description
    Options
    Example

SYNOPSIS

    definition:


      subroutine vinit()



DESCRIPTION

    Initialize the pixel graphics module. The pixel array P_pixel and the colormap P_ColorMap are directly accessible after the
    call to allow display or printing

OPTIONS

EXAMPLE

    Sample program:

         program demo_vinit
         use M_pixel, only    : prefsize, vinit, ortho2, clear
         use M_pixel, only    : move2, draw2, vexit, color
         use M_pixel, only    : P_pixel, P_colormap
         use M_writegif, only : writegif
         call prefsize(60,40)
         call vinit()
         call ortho2(-300.0,300.0,-200.0,200.0)
         call clear(0)
         call color(1)
         call move2(-300.0,-200.0)
         call draw2(300.0,200.0)
         call move2(300.0,-200.0)
         call draw2(-300.0,200.0)
         call writegif( vinit.3.gif ,P_pixel,P_colormap)
         call vexit()
         end program demo_vinit

-----------------------------------------------------------------------------------------------------------------------------------

                                                             vinit (3)                                                July 02, 2017

Generated by manServer 1.08 from 3b4a3ae8-942c-4cec-97bf-589f0f88b865 using man macros.
                                                              [vinit]