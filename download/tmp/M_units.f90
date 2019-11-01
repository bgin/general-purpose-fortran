!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_units
use M_anything,only : anyscalar_to_real, anyscalar_to_double
implicit none                        ! require all variables to be declared
private
!  common trigonometric functions using degrees instead of radians for units
      public sind
      public cosd
      public tand
      public asind
      public acosd
      public atand
      public atan2d
!  normalize angles
      public norm_angle_rad
      public norm_angle_360
!  convert between degrees and radians
      public d2r
      public r2d
!  distance
      public feet_to_meters
      public meters_to_feet
!  weight
      public pounds_to_kilograms
!  convert between Celsius and Fahrenheit
      public f2c
      public c2f
!  convert between coordinate systems
      public cartesian_to_spherical
      public spherical_to_cartesian
      public cartesian_to_polar
      public polar_to_cartesian
!  tables
      public symbol2atomnum ! return atomic number given element symbol name
      public atomnum2symbol ! return element symbol given atomic number

      public test_suite_M_units
!===================================================================================================================================
      public is_even
      public is_nan
      public inf
      interface inf
         module procedure inf32, inf64, inf128
      end interface inf

      public nan
      interface nan
         module procedure nan32, nan64, nan128
      end interface nan
!===================================================================================================================================
!  constants

doubleprecision, parameter, private :: eighth_circle_rad_d  = atan(1.0d0)            ! pi/4
doubleprecision, parameter, private :: quarter_circle_rad_d = 2*eighth_circle_rad_d  ! pi/2
doubleprecision, parameter, private :: half_circle_rad_d    = 4*eighth_circle_rad_d  ! pi
doubleprecision, parameter, private :: circle_rad_d         = 8*eighth_circle_rad_d  ! 2pi

real, parameter, private :: eighth_circle_rad_r  = atan(1.0)              ! pi/4
real, parameter, private :: quarter_circle_rad_r = 2*eighth_circle_rad_r  ! pi/2
real, parameter, private :: half_circle_rad_r    = 4*eighth_circle_rad_r  ! pi
real, parameter, private :: circle_rad_r         = 8*eighth_circle_rad_r  ! 2pi

!===================================================================================================================================
integer, public, parameter :: DP = selected_real_kind(15)
real(kind=DP), public, parameter ::              &
!---------------------!------------------------------------------------------------
                      ! velocity of light in a vacuum
   c__m_per_sec       = 2.99792458d+8,                                            & ! m/sec
   c__ft_per_sec      = 9.83571056d+8,                                            & ! ft/sec
!---------------------!------------------------------------------------------------
                      ! "e" is the base of the natural logarithm system.
                      ! "e" was named in honor of Euler, but is known as Napier's constant.
   e                  = 2.71828182845904523536028747135266249775724709369995d+00, &
!---------------------!------------------------------------------------------------
   euler              = 0.577215664901532860606512090082402431042d+00,            &
!---------------------!------------------------------------------------------------
                      ! The Euler-Mascheroni constant is often denoted by a lower-case Gamma.  Gamma is defined as
                      ! Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
   gamma              = 0.577215664901532860606512090082402431042d+00,            &
!---------------------!------------------------------------------------------------
   pi                 = 3.14159265358979323846264338327950288419716939937510d0,   &
!---------------------!------------------------------------------------------------
                      ! for two values A+B is to A as A is to B
   Golden_Ratio       = 1.6180339887498948482045868_DP,                           &
!---------------------!------------------------------------------------------------
   Deg_Per_Rad        = 57.2957795130823208767981548_DP,                          &
   Rad_Per_Deg        = 0.01745329251994329576923691_DP,                          &
   degrees_to_radians = PI / 180.0D+00,                                           &
!---------------------!------------------------------------------------------------
   end=99999    ! END OF CONSTANTS
!===================================================================================================================================
      interface norm_angle_rad                                  ! a Generic Interface in a module with PRIVATE specific procedures
         module procedure norm_angle_rad_real, norm_angle_rad_double
      end interface

      interface norm_angle_360                                  ! a Generic Interface in a module with PRIVATE specific procedures
         module procedure norm_angle_360_real, norm_angle_360_double
         module procedure norm_angle_360_integer
      end interface

      interface r2d
         module procedure r2d_d, r2d_r, r2d_i
      end interface
      interface d2r
         module procedure d2r_d, d2r_r, d2r_i
      end interface

contains
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function c2f(celsius)
character(len=*),parameter::ident_1="@(#)M_units::c2f(3f): Convert celsius to fahrenheit"
class(*),intent(in)           :: celsius        ! celsius value to convert to fahrenheit
   real                       :: celsius_local
   celsius_local=anyscalar_to_real(celsius)
   c2f=(celsius_local+40.0)*9.0/5.0 - 40.0         ! do the conversion
end function c2f
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function f2c(fahrenheit)
character(len=*),parameter::ident_2="@(#)M_units::f2c(3f): Convert fahrenheit to celsius"
class(*),intent(in)           :: fahrenheit     ! input fahrenheit to convert to celsius
   real                       :: fahrenheit_local
   fahrenheit_local=anyscalar_to_real(fahrenheit)
   f2c=(fahrenheit_local+40.0)*5.0/9.0 - 40.0      ! do the conversion
end function f2c
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function r2d_i(iradians)

character(len=*),parameter::ident_3="@(#)M_units::r2d_i(3f): Convert radians to degrees"

doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
integer,intent(in)           :: iradians        ! input radians to convert to degrees
   r2d_i=dble(iradians)/DEGREE ! do the conversion
end function r2d_i
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function r2d_d(radians)

character(len=*),parameter::ident_4="@(#)M_units::r2d_d(3f): Convert radians to degrees"

doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
doubleprecision,intent(in)           :: radians        ! input radians to convert to degrees
   r2d_d=radians / DEGREE ! do the conversion
end function r2d_d
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function r2d_r(radians)

character(len=*),parameter::ident_5="@(#)M_units::r2d_r(3f): Convert radians to degrees"

doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
real,intent(in)           :: radians        ! input radians to convert to degrees
   r2d_r=radians / DEGREE ! do the conversion
end function r2d_r
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function d2r_r(degrees)

character(len=*),parameter::ident_6="@(#)M_units::d2r_r(3f): Convert degrees to radians"

doubleprecision,parameter :: RADIAN=57.2957795131d0 ! degrees
real,intent(in)           :: degrees                ! input degrees to convert to radians
   d2r_r=dble(degrees)/RADIAN                       ! do the unit conversion
end function d2r_r
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function d2r_d(degrees)

character(len=*),parameter::ident_7="@(#)M_units::d2r_d(3f): Convert degrees to radians"

doubleprecision,parameter :: RADIAN=57.2957795131d0 ! degrees
doubleprecision,intent(in) :: degrees               ! input degrees to convert to radians
   d2r_d=degrees/RADIAN                             ! do the unit conversion
end function d2r_d
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function d2r_i(idegrees)

character(len=*),parameter::ident_8="@(#)M_units::d2r_i(3f): Convert degrees to radians"

doubleprecision,parameter :: RADIAN=57.2957795131d0 ! degrees
integer,intent(in) :: idegrees                      ! input degrees to convert to radians
   d2r_i=dble(idegrees)/RADIAN                      ! do the unit conversion
end function d2r_i
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function sind(angle_in_degrees)

character(len=*),parameter::ident_9="@(#)M_units::sind(3f): sin(3f) with degrees as input instead of radians"

class(*),intent(in)           :: angle_in_degrees
real                       :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_real(angle_in_degrees)
   sind=sin(angle_in_degrees_local*degrees_to_radians)
end function sind
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function cosd(angle_in_degrees)

character(len=*),parameter::ident_10="@(#)M_units::cosd(3f): cos(3f) with degrees as input instead of radians"

class(*),intent(in)           :: angle_in_degrees
real                       :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_real(angle_in_degrees)
   cosd=cos(angle_in_degrees_local*degrees_to_radians)
end function cosd
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function tand(angle_in_degrees)

character(len=*),parameter::ident_11="@(#)M_units::tand(3f): tan(3f) with degrees as input instead of radians"

class(*),intent(in)           :: angle_in_degrees
real                       :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_real(angle_in_degrees)
   tand=tan(angle_in_degrees_local*degrees_to_radians)
end function tand
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function asind(x)

character(len=*),parameter::ident_12="@(#)M_units::asind(3f): asin(3f) with degrees as output instead of radians"

class(*),intent(in)           :: x
real                          :: x_local
   x_local=anyscalar_to_real(x)
   asind=asin(x_local)/degrees_to_radians
end function asind
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function acosd(x)

character(len=*),parameter::ident_13="@(#)M_units::acosd(3f): calculate arc-cos of angle in degrees"

class(*),intent(in)           :: x
real                          :: x_local
   x_local=anyscalar_to_real(x)
   acosd=acos(x_local)/degrees_to_radians
end function acosd
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function atand(x)

character(len=*),parameter::ident_14="@(#)M_units::atand(3f): result is arc-tangent of angle in degrees"

class(*),intent(in)           :: x
real                          :: x_local
   x_local=anyscalar_to_real(x)
   atand=atan(x_local)/degrees_to_radians
end function atand
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function atan2d(x,y)

character(len=*),parameter::ident_15="@(#)M_units::atan2d(3f): calculate arc-tangent of angle in degrees"

class(*),intent(in)           :: x
class(*),intent(in)           :: y
real                       :: x_local
real                       :: y_local
   x_local=anyscalar_to_real(x)
   y_local=anyscalar_to_real(y)
   atan2d=atan2(x_local,y_local)/degrees_to_radians
end function atan2d
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental function feet_to_meters(feet)
character(len=*),parameter::ident_16="@(#)M_units::feet_to_meters(3f): converts a measurement in feet to meters"
class(*),intent(in)           :: feet                           ! the input length in feet.
doubleprecision               :: feet_to_meters                 ! OUTPUT, the corresponding length in meters.
doubleprecision               :: feet_local
   feet_local=anyscalar_to_double(feet)
   !!feet_to_meters = 0.0254 * 12.0 * feet_local
   feet_to_meters = 0.3048d0 * feet_local

end function feet_to_meters
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental function meters_to_feet(meters)
character(len=*),parameter::ident_17="@(#)M_units::meters_to_feet(3f): converts a measurement in meters to feet"
class(*),intent(in)           :: meters                         ! the input length in meters.
doubleprecision               :: meters_to_feet                 ! OUTPUT, the corresponding length in feet.
   doubleprecision            :: meters_local
   meters_local=anyscalar_to_double(meters)
   meters_to_feet = meters_local/12.0d0/0.0254d0
end function meters_to_feet
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
implicit none
character(len=*),parameter::ident_18="&
&@(#)M_units::cartesian_to_spherical(3f): convert Cartesian coordinates to ISO polar coordinates"
real,intent(in)  :: x,y,z
real,intent(out) :: radius,inclination,azimuth
   radius=sqrt(x**2+y**2+z**2)
   if(radius.eq.0)then
      inclination=0.0
      azimuth=0.0
   else
      inclination=acos(z/radius)
      azimuth=atan2(y,x)
   endif
end subroutine cartesian_to_spherical
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine spherical_to_cartesian(radius,inclination,azimuth,x,y,z)
implicit none
character(len=*),parameter::ident_19="&
&@(#)M_units::spherical_to_cartesian(3f): convert spherical coordinates to cartesian coordinates"
real,intent(in) :: radius,inclination,azimuth
real,intent(out)  :: x,y,z
   if(radius.eq.0)then
      x=0.0
      y=0.0
      z=0.0
   else
      x=radius*sin(inclination)*cos(azimuth)
      y=radius*sin(inclination)*sin(azimuth)
      z=radius*cos(inclination)
   endif
end subroutine spherical_to_cartesian
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine cartesian_to_polar(x,y,radius,inclination)
implicit none
character(len=*),parameter::ident_20="@(#)M_units::cartesian_to_polar(3f): convert Cartesian coordinates to polar coordinates"
real,intent(in)  :: x,y
real,intent(out) :: radius,inclination
   radius=sqrt(x**2+y**2)
   if(radius.eq.0)then
      inclination=0.0
   else
      inclination=atan2(y,x)
   endif
end subroutine cartesian_to_polar
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
subroutine polar_to_cartesian(radius,inclination,x,y)
implicit none
character(len=*),parameter::ident_21="@(#)M_units::polar_to_cartesian(3f): convert polar coordinates to cartesian coordinates"
real,intent(in) :: radius,inclination
real,intent(out)  :: x,y
   if(radius.eq.0)then
      x=0.0
      y=0.0
   else
      x=radius*cos(inclination)
      y=radius*sin(inclination)
   endif
end subroutine polar_to_cartesian
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine atomnum2symbol(atomnum,symbol)
implicit none
character(len=*),parameter::ident_22="@(#)M_units::atomnum2symbol(3f): return element symbol given atomic number"
integer,intent(in)           :: atomnum
character(len=2),intent(out) :: symbol
integer,parameter            :: nelements=109
character(len=2),save        :: symbols(nelements)

data symbols/                                                 &
& 'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
& 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', &
& 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
& 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', &
& 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
& 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', &
& 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
& 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', &
& 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
& 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', &
& 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt'/

   if(atomnum.lt.1.or.atomnum.gt.nelements)then
      write(*,*)'*atomnum2symbol* atomic number out of range (1 to 109) ',atomnum
      symbol='  '
   else
      symbol=symbols(atomnum)
   endif
end subroutine atomnum2symbol
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine symbol2atomnum(symbol,atomnum)
implicit none
character(len=*),parameter::ident_23="@(#)M_units::symbol2atomnum(3f): return atomic number given element symbol name"
character(len=2),intent(in) :: symbol
integer,intent(out)         :: atomnum
integer,parameter           :: nelements=109
integer                     :: i
character(len=2),save       :: symbols(nelements)

data symbols/                                                 &
& 'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
& 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', &
& 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
& 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', &
& 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
& 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', &
& 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
& 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', &
& 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
& 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', &
& 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt'/

FINDIT: block
   do i = 1,nelements
      if ( (symbol(1:1) .eq. symbols(i)(1:1)) .and. (symbol(2:2) .eq. symbols(i)(2:2)) )then
         atomnum=i
         exit FINDIT
      endif
   enddo
   write(*,*)'*symbol2atomnum* error: symbol not found :',symbol
   atomnum=0
endblock FINDIT
end subroutine symbol2atomnum
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental function pounds_to_kilograms ( pounds )
character(len=*),parameter::ident_24="@(#)M_units::pounds_to_kilograms(3f): converts a measurement in pounds to kilograms."
class(*),intent(in) :: pounds
   doubleprecision  :: pounds_to_kilograms
   doubleprecision  :: pounds_local
   pounds_local=anyscalar_to_double(pounds)
   pounds_to_kilograms = 0.45359237d0 * pounds_local
end function pounds_to_kilograms
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental function norm_angle_rad_double(ang)

character(len=*),parameter::ident_25="&
&@(#)M_units::norm_angle_rad_double(3fp): Return input angle given in radians as doubleprecision angle between 0 and 2pi"

doubleprecision, intent(in) :: ang
doubleprecision             :: norm_angle_rad_double
   norm_angle_rad_double = ang - dble(floor(ang/circle_rad_d)) * circle_rad_d
end function norm_angle_rad_double
!===================================================================================================================================
elemental function norm_angle_rad_real(ang)

character(len=*),parameter::ident_26="&
&@(#)M_units::norm_angle_rad_real(3fp): Return input angle given in radians as real angle between 0 and 2pi"

real, intent(in) :: ang
real             :: norm_angle_rad_real
    norm_angle_rad_real = ang - real(floor(ang/circle_rad_r)) * circle_rad_r
end function norm_angle_rad_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental function norm_angle_360_double(ang)

character(len=*),parameter::ident_27="@(#)M_units:: norm_angle_360_double(3fp): Returns angle in degrees between 0 and 360"

doubleprecision,intent(in) :: ang
doubleprecision            :: norm_angle_360_double
   norm_angle_360_double = ang - dble(floor(ang/360.d0)) * 360.d0
end function norm_angle_360_double
!===================================================================================================================================
elemental function norm_angle_360_real(ang)

character(len=*),parameter::ident_28="@(#)M_units:: norm_angle_360_real(3fp): Returns angle in degrees between 0 and 360"

real,intent(in) :: ang
real            :: norm_angle_360_real
   norm_angle_360_real = ang - dble(floor(ang/360.d0)) * 360.d0
end function norm_angle_360_real
!===================================================================================================================================
elemental function norm_angle_360_integer(ang)

character(len=*),parameter::ident_29="@(#)M_units:: norm_angle_360_integer(3fp): Returns angle in degrees between 0 and 360"

integer,intent(in) :: ang
integer            :: norm_angle_360_integer
   norm_angle_360_integer = ang - dble(floor(ang/360.d0)) * 360.d0
end function norm_angle_360_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function inf32(value)
use,intrinsic :: iso_fortran_env, only: real32
implicit none

character(len=*),parameter::ident_30="@(#)M_units:: inf32(3fp): Returns an inf (Infinity) of type real32"

character(len=3),save :: STRING='inf'
real(kind=real32) :: inf32,value
   read(STRING,*)inf32
end function inf32
!===================================================================================================================================
function inf64(value)
use,intrinsic :: iso_fortran_env, only: real64
implicit none

character(len=*),parameter::ident_31="@(#)M_units:: inf64(3fp): Returns an inf (Infinity) of type real64"

character(len=3),save :: STRING='inf'
real(kind=real64) :: inf64,value
   read(STRING,*)inf64
end function inf64
!===================================================================================================================================
function inf128(value)
use,intrinsic :: iso_fortran_env, only: real128
implicit none

character(len=*),parameter::ident_32="@(#)M_units:: inf128(3fp): Returns an inf (Infinity) of type real128"

character(len=3),save :: STRING='inf'
real(kind=real128) :: inf128,value
   read(STRING,*)inf128
end function inf128
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
function nan32(value)
use,intrinsic :: iso_fortran_env, only: real32
implicit none

character(len=*),parameter::ident_33="@(#)M_units:: nan32(3fp): Returns a NAN (Not a number) of type real32"

character(len=3),save :: STRING='NaN'
real(kind=real32) :: nan32,value
   read(STRING,*)nan32
   ! (if X is NaN the comparison with 0. is always false.)
   if ( (nan32<=0.0_real32) .or. (nan32>=0.0_real32) )then
      write(*,*)'nan(3f) did not produce a nan'
      stop
   endif
end function nan32
!===================================================================================================================================
function nan64(value)
use,intrinsic :: iso_fortran_env, only: real64
implicit none

character(len=*),parameter::ident_34="@(#)M_units:: nan64(3fp): Returns a NAN (Not a number) of type real64"

character(len=3),save :: STRING='NaN'
real(kind=real64) :: nan64,value
   read(STRING,*)nan64
   ! (if X is NaN the comparison with 0. is always false.)
   if ( (nan64<=0.0_real64) .or. (nan64>=0.0_real64) )then
      write(*,*)'nan(3f) did not produce a nan'
      stop
   endif
end function nan64
!===================================================================================================================================
function nan128(value)
use,intrinsic :: iso_fortran_env, only: real128
implicit none

!$@(#) M_units:: nan128(3fp): Returns a NAN (Not a number) of type real128

character(len=3),save :: STRING='NaN'
real(kind=real128) :: nan128,value
   read(STRING,*)nan128
   ! (if X is NaN the comparison with 0. is always false.)
   if ( (nan128<=0.0_real128) .or. (nan128>=0.0_real128) )then
      write(*,*)'nan(3f) did not produce a nan'
      stop
   endif
end function nan128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental pure function is_even(ival)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

character(len=*),parameter::ident_35="@(#)M_units::is_even(3f): determine if integer is  even"

class(*),intent(in) :: ival
logical             :: is_even
select type(ival)
   type is (integer(kind=int8))
     is_even = mod(ival, 2_int8) == 0_int8 ! This can be reduced to one line:
   type is (integer(kind=int16))
     is_even = iand(ival, 1_int16) == 0_int16 ! Quicker will be:
   type is (integer(kind=int32))
     is_even = iand(ival, 1_int32) == 0_int32  ! Quicker will be:
   type is (integer(kind=int64))
     if (mod(ival, 2_int64) == 0_int64) then
        is_even = .true.
     else
        is_even = .false.
     endif
   end select
end function is_even
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!!
!===================================================================================================================================
elemental pure function is_nan(x)
!!use IEEE_EXCEPTIONS, only : ieee_support_nan ! is IEEE NaNs supported?
use IEEE_ARITHMETIC, only : IEEE_IS_NAN       ! Determine if value is IEEE Not-a-Number.
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

character(len=*),parameter::ident_36="@(#)M_units::is_nan(3f): determine if value is  IEEE Not-a-Number"

class(*),intent(in) :: x
logical             :: is_nan
   select type(x)
      type is (real(kind=real32));      is_nan=ieee_is_nan(x)
      type is (real(kind=real64));      is_nan=ieee_is_nan(x)
      type is (real(kind=real128));     is_nan=ieee_is_nan(x)
      type is (complex);                is_nan=ieee_is_nan(real(x)).and.ieee_is_nan(aimag(x))
      !!type is (complex);                is_nan=ieee_is_nan(x%re).and.ieee_is_nan(x%im)
   end select
end function is_nan
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_units()

!! test constants
   call testit_p('pi',      real(PI)      ,  real(3.141592653589793238462643383279500d0)  ,message='')
   call testit_p('e',       real(E)       ,  real(2.718281828459045235360d0)              ,message='')
   !!call testit_p('radian',  real(RADIAN)  ,  real(57.29577951310d0)                       ,message='')
   !!call testit_p('degree',  real(DEGREE)  ,  real(0.0174532925199430d0)                   ,message='')

!! setup
   call test_acosd()
   call test_asind()
   call test_atan2d()
   call test_atand()
   call test_atomnum2symbol()
   call test_c2f()
   call test_cartesian_to_polar()
   call test_cartesian_to_spherical()
   call test_cosd()
   call test_d2r()
   call test_f2c()
   call test_feet_to_meters()
   call test_meters_to_feet()
   call test_norm_angle_360_double()
   call test_norm_angle_360_integer()
   call test_norm_angle_360_real()
   call test_norm_angle_rad_double()
   call test_norm_angle_rad_real()
   call test_polar_to_cartesian()
   call test_pounds_to_kilograms()
   call test_r2d()
   call test_sind()
   call test_spherical_to_cartesian()
   call test_symbol2atomnum()
   call test_tand()
   call test_inf()
   call test_nan()
   call test_is_nan()
   call test_is_even()
!! teardown
contains
!===================================================================================================================================
subroutine testit_p(label,value1,value2,message)
use M_anything,only : anyscalar_to_real, anyscalar_to_double
USE M_Compare_Float_Numbers
use M_math, only : accdig
use M_debug, only : unit_check, msg
class(*),intent(in) :: value1, value2
real                :: v1, v2
character(len=*)    :: label
character(len=*)    :: message
logical             :: stat
real                :: significant_digits
integer             :: ind
real                :: acurcy

   v1=anyscalar_to_real(value1)
   v2=anyscalar_to_real(value2)
   stat=v1 .EqualTo. v2

   if(.not.stat)then
!     INPUT ...
!     real,intent(in) :: x           ! First  of two real numbers to be compared.
!     real,intent(in) :: y           ! Second of two real numbers to be compared.
!     real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
!     integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
!                                    ! = 1, If tolerance is not satisfied.
!     real,intent(out) :: acurcy     ! = - LOG10 (ABS((X-Y)/Y)))
      significant_digits=int(log10(2.0**digits(0.0)))     ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A REAL NUMBER.
      call accdig (v1,v2,significant_digits-2,ACURCY,IND)
      if(ind.eq.0)stat=.true.
   endif
!-----------------------
   call unit_check(label,stat,msg=msg(label,v1,v2,trim(message),'accuracy=',acurcy,'asked for',int(significant_digits)-2,'digits'))
end subroutine testit_p
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nan()
use,intrinsic :: iso_fortran_env, only: real32, real64, real128
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
real(kind=real32) :: r32
real(kind=real64) :: r64
real(kind=real128) :: r128
   call unit_check_start('nan',msg='')
   ! (if X is NaN the comparison with 0.0 is always false.)
   r32=nan(0.0_real32)
   call unit_check('nan',.not.(r32<=0.0_real32) .and. .not.(r32>=0.0_real32),msg='real32')

   r64=nan(0.0_real64)
   call unit_check('nan',.not.(r64<=0.0_real64) .and. .not.(r64>=0.0_real64),msg='real64')

   r128=nan(0.0_real128)
   call unit_check('nan',.not.(r128<=0.0_real128) .and. .not.(r128>=0.0_real128),msg='real128')

   call unit_check_done('nan',msg='')
end subroutine test_nan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_even()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
logical,parameter     :: t=.true.
logical,parameter     :: f=.false.
   call unit_check_start('is_even',msg='')
   call unit_check('is_even', all(is_even([-10, 0, 1, 2, 3]).eqv.[t,t,f,t,f]), msg=msg('-10, 0, 1, 2, 3'))
   call unit_check_done('is_even',msg='')
end subroutine test_is_even
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_nan()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
character(len=3),save :: line='NaN'
real                  :: x
logical,parameter     :: t=.true.
logical,parameter     :: f=.false.
   call unit_check_start('is_nan',msg='')
   read(line,*)x
call unit_check('is_nan', all(is_nan([x, 0.0,-0.0,-x,-100.0,100.0,huge(0.0)]).eqv.[t,f,f,t,f,f,f]),  &
        & msg=msg('checking',x,0,-x,-100.0,100.0,huge(0.0)))
   call unit_check_done('is_nan',msg='')
end subroutine test_is_nan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inf()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('inf',msg='')
   !!call unit_check('inf', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('inf',msg='')
end subroutine test_inf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_acosd()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('acosd',msg='')
   !!call unit_check('acosd', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('acosd',msg='')
end subroutine test_acosd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_asind()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('asind',msg='')
   !!call unit_check('asind', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('asind',msg='')
end subroutine test_asind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atan2d()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('atan2d',msg='')
   !!call unit_check('atan2d', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('atan2d',msg='')
end subroutine test_atan2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atand()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('atand',msg='')
   !!call unit_check('atand', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('atand',msg='')
end subroutine test_atand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atomnum2symbol()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('atomnum2symbol',msg='')
   !!call unit_check('atomnum2symbol', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('atomnum2symbol',msg='')
end subroutine test_atomnum2symbol
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c2f()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('c2f',msg='')
   call testit_p('c2f',     c2f(0.0)   ,  32.0,message='')
   call testit_p('c2f',     c2f(100.0) , 212.0,message='')
   call testit_p('c2f',     c2f(-40.0) , -40.0,message='')
   call unit_check_done('c2f',msg='')
end subroutine test_c2f
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cartesian_to_polar()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('cartesian_to_polar',msg='')
   !!call unit_check('cartesian_to_polar', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('cartesian_to_polar',msg='')
end subroutine test_cartesian_to_polar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cartesian_to_spherical()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
use M_math,  only : accdig
implicit none
real    :: x=10.0,y=10.0,z=10.0
real    :: radius,inclination,azimuth
real    :: acurcy
integer :: ind1,ind2,ind3
   call unit_check_start('cartesian_to_spherical',msg='')

   ! 10,10,10 -> 17.32, 0.9553, 0.7854
   call cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
   call accdig(radius,      17.3205090,   5.0,acurcy,ind1)
   call accdig(inclination,  0.955316663 ,5.0,acurcy,ind2)
   call accdig(azimuth,      0.785398185 ,5.0,acurcy,ind3)
   call unit_check('cartesian_to_spherical',all([ind1,ind2,ind3].eq.0),msg=msg(x,y,z,'to',radius,inclination,azimuth))

   call unit_check_done('cartesian_to_spherical') ! if got here without being stopped assume passed test
end subroutine test_cartesian_to_spherical
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cosd()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
real, allocatable :: values(:)
integer           :: i
values=[0.0, 30.0, 45.0, 60.0, 92.0, 120.0, 135.0, 150.0, 180.0, 210.0, 240.0, 273.0, 300.0, 330.0, 360.0, -45.0]
   call unit_check_start('cosd',msg='')
   do i=1,size(values)
      call testit_p('cosd', cosd(values(i)), cos(d2r(values(i))),message=msg('value=',values(i)) )
   enddo
   call unit_check_done('cosd',msg='')

!  unit_check:       cosd  FAILED:cosd 6.12323426E-17 -4.37113883E-08 value= 90.0000000 accuracy= 0.00000000 asked for 6 digits
!  unit_check:       cosd  FAILED:cosd -1.83697015E-16 1.19248806E-08 value= 270.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_cosd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2r()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('d2r',msg='')

   call testit_p('d2r', d2r(    0.0)    , 0.0       ,message='real for 0')
   call testit_p('d2r', d2r(   45.0)    , PI/4.0    ,message='real for 45')
   call testit_p('d2r', d2r(  -45.0)    , -PI/4.0   ,message='real for -45')
   call testit_p('d2r', d2r(   90.0)    , PI/2      ,message='real for 90')
   call testit_p('d2r', d2r(  180.0)    , PI        ,message='real for 180')

   call testit_p('d2r', d2r(  0.0d0)    , 0.0d0     ,message='double for 0')
   call testit_p('d2r', d2r(  45.0d0)   , PI/4.0d0  ,message='double for 45')
   call testit_p('d2r', d2r(  -45.0d0)  , -PI/4.0d0 ,message='double for -45')
   call testit_p('d2r', d2r(  90.0d0)   , PI/2d0    ,message='double for 90')
   call testit_p('d2r', d2r(  180.0d0)  , PI        ,message='double for 180')

   call testit_p('d2r', d2r(    0)      , 0.0       ,message='integer for 0')
   call testit_p('d2r', d2r(   45)      , PI/4.0    ,message='integer for 45')
   call testit_p('d2r', d2r(  -45)      , -PI/4.0   ,message='integer for -45')
   call testit_p('d2r', d2r(   90)      , PI/2      ,message='integer for 90')
   call testit_p('d2r', d2r(  180)      , PI        ,message='integer for 180')

   call unit_check_done('d2r',msg='')

end subroutine test_d2r
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_f2c()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('f2c',msg='')
   call testit_p('f2c',     f2c(32.0)  ,   0.0,message='')
   call testit_p('f2c',     f2c(212.0) , 100.0,message='')
   call testit_p('f2c',     f2c(-40.0) , -40.0,message='')
   call unit_check_done('f2c',msg='')
end subroutine test_f2c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_feet_to_meters()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
doubleprecision, parameter :: f2m=0.3048d0
   call unit_check_start('feet_to_meters',msg=' 0.3048')

   call unit_check('feet_to_meters', &
      & all(abs(feet_to_meters([ -1.0, 0.0, 1.0 ,1.0/12.0])- [-f2m, 0.0d0, f2m, 0.0254d0]).lt.0.00001),'real')
   call unit_check('feet_to_meters', &
      & all(abs(feet_to_meters([ -1,   0,   1   ])- [-f2m, 0.0d0, f2m]).lt.0.00001),'integer')
   call unit_check('feet_to_meters', &
      & all(abs([feet_to_meters(-1.0d0),feet_to_meters(0.0d0),feet_to_meters(1.0d0)]-[-f2m, 0.0d0, f2m]).lt.0.00001),'double')

   call unit_check_done('feet_to_meters',msg='')
end subroutine test_feet_to_meters

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_meters_to_feet()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   doubleprecision, parameter :: m2f=3.2808398950131233595d0

   call unit_check_start('meters_to_feet',msg='3.2808398950131233595')

   call unit_check('meters_to_feet', &
     & all(abs(meters_to_feet([ -1.0, 0.0, 1.0 ])-[-m2f,0.0d0,m2f]).lt.0.00001d0),msg='real')
   call unit_check('meters_to_feet', &
     & all(abs(meters_to_feet([ -1,   0,   1   ])-[-m2f,0.0d0,m2f]).lt.0.00001d0) ,msg='integer')
   call unit_check('meters_to_feet', &
     & all(abs([meters_to_feet(-1d0),meters_to_feet(0.0d0),meters_to_feet(1.0d0)]-[-m2f,0.0d0,m2f]).lt.0.00001d0),msg='double')

   call unit_check_done('meters_to_feet',msg='')
end subroutine test_meters_to_feet
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_360_double()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('norm_angle_360_double',msg='')
   !!call unit_check('norm_angle_360_double', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('norm_angle_360_double',msg='')
end subroutine test_norm_angle_360_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_360_integer()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('norm_angle_360_integer',msg='')
   !!call unit_check('norm_angle_360_integer', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('norm_angle_360_integer',msg='')
end subroutine test_norm_angle_360_integer
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_360_real()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('norm_angle_360_real',msg='')
   !!call unit_check('norm_angle_360_real', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('norm_angle_360_real',msg='')
end subroutine test_norm_angle_360_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_rad_double()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('norm_angle_rad_double',msg='')
   !!call unit_check('norm_angle_rad_double', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('norm_angle_rad_double',msg='')
end subroutine test_norm_angle_rad_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_rad_real()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('norm_angle_rad_real',msg='')
   !!call unit_check('norm_angle_rad_real', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('norm_angle_rad_real',msg='')
end subroutine test_norm_angle_rad_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polar_to_cartesian()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('polar_to_cartesian',msg='')
   !!call unit_check('polar_to_cartesian', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('polar_to_cartesian',msg='')
end subroutine test_polar_to_cartesian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pounds_to_kilograms()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('pounds_to_kilograms',msg='')
   call unit_check('pounds_to_kilograms',abs(pounds_to_kilograms(1.0)-0.45359237).lt.0.00001,'real')
   call unit_check('pounds_to_kilograms',any(abs(pounds_to_kilograms([ 0, 1, 100, 200 ])-&
      &[0.0, 0.45359237, 45.359237,90.718474]).lt.0.00001),'integer')
   call unit_check('pounds_to_kilograms',abs(pounds_to_kilograms(1.0d0)-0.45359237).lt.0.00001,'double')
   call unit_check_done('pounds_to_kilograms',msg='')
end subroutine test_pounds_to_kilograms
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_r2d()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
real              :: x=real(PI)
doubleprecision   :: d=PI

   call unit_check_start('r2d',msg='')

   call testit_p('r2d', r2d(  0.0)      ,   0.0    ,message='real')
   call testit_p('r2d', r2d(  x/4)      ,  45.0    ,message='real')
   call testit_p('r2d', r2d( -x/4)      , -45.0    ,message='real')
   call testit_p('r2d', r2d(  x/2)      ,  90.0    ,message='real')
   call testit_p('r2d', r2d(  x)        , 180.0    ,message='real')

   call testit_p('r2d', r2d(  0.0d0)    ,   0.0d0  ,message='double')
   call testit_p('r2d', r2d(  d/4.0d0)  ,  45.0d0  ,message='double')
   call testit_p('r2d', r2d( -d/4.0d0)  , -45.0d0  ,message='double')
   call testit_p('r2d', r2d(  d/2.0d0)  ,  90.0d0  ,message='double')
   call testit_p('r2d', r2d(  d)        , 180.0d0  ,message='double')

   call testit_p('r2d', r2d(  0)        ,   0.0    ,message='integer')

   call unit_check_done('r2d',msg='')
end subroutine test_r2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sind()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
real, allocatable :: values(:)
integer           :: i
   values=[0.0, 30.0, 45.0, 60.0, 90.0, 120.0, 135.0, 150.0, 181.0, 210.0, 240.0, 270.0, 300.0, 330.0, 362.0, -45.0]
   call unit_check_start('sind',msg='')
   do i=1,size(values)
      call testit_p('sind',   sind(values(i))             ,  sin(d2r(values(i))),message=msg('value=',values(i))  )
   enddo
   call unit_check_done('sind',msg='')
! unit_check:       sind  FAILED:sind 1.22464685E-16 -8.74227766E-08 value= 180.000000 accuracy= 0.00000000 asked for 6 digits
! unit_check:       sind  FAILED:sind -2.44929371E-16 1.74845553E-07 value= 360.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_sind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spherical_to_cartesian()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
use M_math,  only : accdig
implicit none
real    :: x,y,z
real    :: radius,inclination,azimuth
real    :: acurcy
integer :: ind1,ind2,ind3
   call unit_check_start('spherical_to_cartesian',msg='')

   radius=17.32; inclination=0.9553; azimuth=0.7854
   x=-9999; y=-9999; z=-9999;
   call spherical_to_cartesian(radius,inclination,azimuth,x,y,z)

   call accdig(x,10.0,4.0,acurcy,ind1)
   call accdig(y,10.0,4.0,acurcy,ind2)
   call accdig(z,10.0,4.0,acurcy,ind3)

   call unit_check('spherical_to_cartesian',all([ind1,ind2,ind3].eq.0),msg=msg(radius,inclination,azimuth,'to',x,y,z))

   call unit_check_done('spherical_to_cartesian',msg='')
end subroutine test_spherical_to_cartesian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_symbol2atomnum()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('symbol2atomnum',msg='')
   !!call unit_check('symbol2atomnum', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('symbol2atomnum',msg='')
end subroutine test_symbol2atomnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tand()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
real, allocatable :: values(:)
integer                      :: i
   values=[0.0,30.0,45.0,60.0,92.0,120.0,135.0,150.0,183.0,210.0,240.0,273.0,300.0, 330.0, 362.0, -45.0]
   call unit_check_start('tand',msg='')
   do i=1,size(values)
      call testit_p('tand', tand(values(i)), tan(d2r(values(i))),message=msg('value=',values(i)))
   enddo
   call unit_check_done('tand',msg='')
! unit_check:       tand  FAILED:tand 1.63312395E+16 -22877332.0 value= 90.0000000 accuracy= -8.85361290 asked for 6 digits
! unit_check:       tand  FAILED:tand -1.22464685E-16 8.74227766E-08 value= 180.000000 accuracy= 0.00000000 asked for 6 digits
! unit_check:       tand  FAILED:tand 5.44374649E+15 -83858280.0 value= 270.000000 accuracy= -7.81235218 asked for 6 digits
! unit_check:       tand  FAILED:tand -2.44929371E-16 1.74845553E-07 value= 360.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_tand
!===================================================================================================================================
end subroutine test_suite_M_units
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_units
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
