!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_math
implicit none
private
  ! GEOMETRY
  public citer              ! determine various geometric properties of circle segment given radius and area of the segment.
  public envelope           ! Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
  public inpolygon          ! Subroutine to determine whether or not a point is in a polygon
  public locpt              ! find if a point is inside a polygonal path
  public poly_intercept     ! find points where a line intersects a polygon
  public polyarea           ! find area of a polygon
  public polyarea_shoelace  ! find area of a polygon using shoelace algorithm
  public polyarea_mid_point ! find area of a polygon
  public closest            ! find point in <X,Y> arrays closest to target point
  ! FIT
  public julfit             ! linear least square fit
  public julfit1            ! linear least square fit(y=a*x+b)
  public lowess             ! data smoothing using locally weighted regression
  public splift             ! fits a spline to the n data points given in x and y
  public splint             ! interpolates and twice differentiates a cubic spline
  public linearint          ! linear interpolation
  ! FITS
  public ju_polfit
  public ju_pvalue
  public glstsq
  private gcsgau1
  private gcsgau2
  ! INTEGRATE
  public qhfg
  public qhsg
  public qtfg
  ! STATISTICS
  public extremum       ! find the minimum and maximum value in a real array
  public bds            ! basic descriptive statistics
  public ncr            ! number of combinations of size R from N cases
  public skekurx        ! skew and kurtosis variant
  public skekur1        ! skew and kurtosis variant
  public stddev         ! standard deviation
  ! COMPARING AND ROUNDING FLOATING POINT VALUES
  public accdig         ! compare two real numbers only up to a specified number of digits
  public almost         ! function compares two real numbers only up to a specified number of digits
  public dp_accdig      ! compare two double numbers only up to a specified number of digits
  public in_margin      ! check if two reals are approximately equal using a relative margin
  public round          ! round val to specified number of significant digits
  public scale1         ! given xmin,xmax,n, find new range xminp xmaxp divisible into approximately n linear intervals of size dist
  public scale3         ! find nice log range, typically for an axis
  ! MATRIX
  public invert_2x2     ! directly invert 2x2 matrix
  public invert_3x3     ! directly invert 3x3 matrix
  public invert_4x4     ! directly invert 4x4 matrix
  public magic_square   ! create magic squares
  ! POLYNOMIAL
  public quadratic      ! return roots of quadratic equation even if complex

  public test_suite_M_math

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! From original:
!  In my experience, LAPACK is great when you wish to invert huge NxN
!  matrices, but it can be really slow for inverting smaller 2x2, 3x3,
!  and 4x4 matrices. For my use case, where I need to invert billions of
!  2x2 and 4x4 matrices instead of a few large NxN matrices, I got a 30%
!  speedup of my program replacing the LAPACK calls by direct calculations
!  of the matrix inversions. I have attached the code that I've used for
!  the 2x2, 3x3, and 4x4 cases below. The 2x2 version is quite easy
!  to derive analytically. The 3x3 and 4x4 versions are based on the
!  subroutines M33INV and M44INV by David G. Simpson; I just converted them
!  from subroutines to pure functions.
!===================================================================================================================================
!>
!!##NAME
!!    invert_2x2(3f) - [M_math] directly invert 2x2 matrix
!!
!!##SYNOPSIS
!!
!!   pure function invert_2x2(A) result(B)
!!
!!    ! integer,real,double,complex
!!
!!    integer,parameter         :: wp=kind(0|0.0|0.0d|(0.0,0.0))
!!    NNNNNN(kind=wp), intent(in) :: A(2,2)   !! Matrix
!!    NNNNNN(kind=wp)             :: B(2,2)   !! Inverse matrix
!!     where
!!    NNNNNN may be INTEGER,REAL,DOUBLEPRECISION,COMPLEX
!!
!!##DESCRIPTION
!!
!!    Directly invert 2x2 matrix for speed (versus using, e.g. LAPACK)
!!
!!##OPTIONS
!!
!!    A  original 2x2 matrix, may be INTEGER, REAL, DOUBLE, or COMPLEX
!!    B  inverted 2x2 matrix, of same type as input matrix A
!!
!!##EXAMPLE
!!
!===================================================================================================================================
interface invert_2x2
   module procedure real_invert_2x2, integer_invert_2x2, complex_invert_2x2, double_invert_2x2
end interface invert_2x2
!===================================================================================================================================
!>
!!##NAME
!!
!!    invert_3x3(3f) - [M_math] directly invert 3x3 matrix
!!
!!##SYNOPSIS
!!
!!
!!   pure function invert_3x3(A) result(B)
!!
!!    ! integer,real,double,complex
!!    integer,parameter         :: wp=kind(0|0.0|0.0d|(0.0,0.0))
!!    NNNNNN(kind=wp), intent(in) :: A(2,2)   !! Matrix
!!    NNNNNN(kind=wp)             :: B(2,2)   !! Inverse matrix
!!     where
!!    NNNNNN may be INTEGER,REAL,DOUBLEPRECISION,COMPLEX
!!
!!##DESCRIPTION
!!    Directly invert 3x3 matrix for speed (versus using, e.g. LAPACK)
!!
!!##OPTIONS
!!    A  original 3x3 matrix, may be INTEGER, REAL, DOUBLE, or COMPLEX
!!    B  inverted 3x3 matrix, of same type as input matrix A
!!
!!##EXAMPLE
!!
!===================================================================================================================================
interface invert_3x3
   module procedure real_invert_3x3, integer_invert_3x3, complex_invert_3x3, double_invert_3x3
end interface invert_3x3
!===================================================================================================================================
!>
!!##NAME
!!    invert_4x4(3f) - [M_math] directly invert 4x4 matrix
!!
!!##SYNOPSIS
!!
!!   pure function invert_4x4(A) result(B)
!!
!!    ! integer,real,double,complex
!!    integer,parameter         :: wp=kind(0|0.0|0.0d|(0.0,0.0))
!!    NNNNNN(kind=wp), intent(in) :: A(2,2)   !! Matrix
!!    NNNNNN(kind=wp)             :: B(2,2)   !! Inverse matrix
!!     where
!!    NNNNNN may be INTEGER,REAL,DOUBLEPRECISION,COMPLEX
!!
!!##DESCRIPTION
!!    Directly invert 4x4 matrix for speed (versus using, e.g. LAPACK)
!!
!!##OPTIONS
!!    A  original 4x4 matrix, may be INTEGER, REAL, DOUBLE, or COMPLEX
!!    B  inverted 4x4 matrix, of same type as input matrix A
!!
!!##EXAMPLE
!!
!===================================================================================================================================
interface invert_4x4
   module procedure real_invert_4x4, integer_invert_4x4, complex_invert_4x4, double_invert_4x4
end interface invert_4x4

contains

!>
!!##NAME
!!     julfit(3f) - [M_math:fit] linear least squares curve fits, destroys input arrays
!!
!!##SYNOPSIS
!!
!!   subroutine julfit(x,y,ixn,itype,a,b,r2)
!!
!!    integer,intent(in) :: ixn
!!    real               :: x(ixn),y(ixn)
!!    integer,intent(in) :: itype
!!    real,intent(out)   :: a,b,r2
!!
!!##DESCRIPTION
!!     use method of least squares to find a fit to the data.
!!     the expression being fitted is of one of several forms that
!!     have in common the fact that the expression will plot as
!!     a straight line if the proper axis type is selected.
!!
!!      type  x-axis y-axis   significance of a and b
!!
!!       1    linear linear   y=a*x+b         # linear function
!!       2    linear log      y=a*b**x        # exponential function
!!       3    log    linear   y=a*log10(x)+b  # logarithmic function
!!       4    log    log      y=a*x**b        # power functions:
!!                                                 hyperbolic if b <0;
!!                                                 parabolic if b > 0.
!!       5    linear log      y=a*e**(-b*x)   # a common variant of the
!!                                              exponential form.
!!
!!
!!##OPTIONS
!!   x      array of x values, input
!!   y      array of y values, input that are changed to hold the output
!!   ixn    number of points in arrays x and y to use
!!   itype  expression being solved
!!          1. Y=a*X+b
!!          2. Y=a*b**X
!!          3. Y=a*log10(X)+b
!!          4. Y=a*X**b
!!          5. Y=a*e*(-b**X)
!!
!!
!! NOTE: odd use of arrays specifically optimized for calling from USH
!!
!!##RETURNS
!!
!!   a      slope of linearized line
!!   b      y intercept of linearized line
!!   r2     correlation coefficient (1=perfect)
!!
!!          In general, if the correlation coefficient is <0.5 the correlation
!!          is regarded as insignificant. If it is >0.8 the derived linear fit
!!          is considered highly significant.
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        julfit(3f)
!! DESCRIPTION:    linear least squares curve fits, destroys input arrays
!! AUTHOR:         John S. Urban
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine julfit(x,y,ixn,itype,a,b,r2)
use M_journal, only : journal
character(len=*),parameter::ident_1="@(#)M_math::julfit(3f): linear least squares curve fits, destroys input arrays"
integer,intent(in) :: ixn
real               :: x(ixn)
real               :: y(ixn)
integer,intent(in) :: itype
real,intent(out)   :: a,b,r2
   real            :: xsmall, ysmall
   real            :: az,bz
   integer         :: i20, i30, i40, i50
!===================================================================================================================================
   if(ixn.le.1)then
      call journal('sc','*julfit* invalid number of points=',ixn)
      return
   endif
!===================================================================================================================================
   xsmall=minval(x)
   ysmall=minval(y)
!===================================================================================================================================
   select case(itype)
   case(1) !     y=a*x+b
   case(2) !     y=a*b**x
      if(ysmall.le.0)goto 999
      y(:ixn)=log10(y(:ixn))
   case(3) !     y=a*log10(x)+b
      if(xsmall.le.0)goto 999
      x(:ixn)=log10(x(:ixn))
   case(4) !     y=a*x**b
      if(ysmall.le.0)goto 999
      y(:ixn)=log10(y(:ixn))
      if(xsmall.le.0)goto 999
      x(:ixn)=log10(x(:ixn))
   case(5) !     y=a*e*(-b**x)
      if(ysmall.le.0)goto 999
      y(:ixn)=log(y(:ixn))
   case default
      call journal('sc','*julfit* invalid type=',itype)
      return
   end select
!===================================================================================================================================
   a=0.0
   b=0.0
   r2=0.0
   call julfit1(x,y,ixn,az,bz,r2)
!===================================================================================================================================
   select case(itype)
   case(1)                   ! y=a*x+b
      a=az
      b=bz
   case(2)                   ! y=a*b**x
      b=10**az
      a=10**bz
      do i20=1,ixn
      y(i20)=a*b**x(i20)
      enddo
   case(3)                   ! y=a*log10(x)+b
      b=bz
      a=az
      do i30=1,ixn
         y(i30)=a*x(i30)+b
         x(i30)=10**x(i30)
      enddo
   case(4)                   ! y=a*x**b
      b=az
      a=10**bz
      do i40=1,ixn
         x(i40)=10**x(i40)
         y(i40)=a*x(i40)**b
      enddo
   case(5)                   ! y=a*e*(-b**x)
      b=-az
      a=exp(bz)
      do i50=1,ixn
         y(i50)=a*exp(x(i50)*(-b))
      enddo
   case default
      call journal('sc','*julfit* cannot get here type=',itype)
      return
   end select
!===================================================================================================================================
   !write(*,*)'GOT HERE 3',a,b,r2
   !write(*,*)'GOT HERE 4',x(:ixn),y(:ixn)
   return
999   continue
   call journal('*lfit* cannot take log or inverse of values <= 0')
   call journal('*lfit* performing linear fit instead')
   call julfit1(x,y,ixn,a,b,r2)
end subroutine julfit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      julfit1(3f) - [M_math:fit] internal routine for linear least square fit(y=a*x+b), changes the y array
!!##SYNOPSIS
!!
!!
!!    subroutine julfit1(x,y,ixn,a,b,r2)
!!
!!       real,intent(in)    :: x(*)
!!       real               :: y(*)
!!       integer,intent(in) :: ixn
!!       real,intent(out)   :: a
!!       real,intent(out)   :: b
!!       real,intent(out)   :: r2
!!##DESCRIPTION
!!
!!    While the method of least squares often gives optimal estimates
!!    parameters for linear processes, it is very sensitive to the presence
!!    of unusual data points in the data used to fit a model, as the square
!!    of the distance from the resulting fit is used in the calculation.
!!
!!    That is, a few outliers can sometimes seriously skew the results of a
!!    least squares analysis; this makes model validation, especially with
!!    respect to outliers, critical to obtaining sound answers.
!!
!!##OPTIONS
!!     X     input X values
!!     Y     input Y values
!!     IXN   size of X and Y vectors
!!     A     multiplier
!!     B     y-intercept
!!     R2
!!##EXAMPLE
!!
!!   sample program
!!
!!       program demo_julfit1
!!          use M_math, only : julfit1
!!          implicit none
!!          intrinsic random_number
!!          integer :: points
!!          real    :: slope, intercept
!!          write(*,*)'For y=m*x+b enter M and B and number of points N:'
!!          read(*,*)slope,intercept,points
!!          call testit()
!!       contains
!!
!!          subroutine testit()
!!             real    :: x(points), y(points)
!!             real    :: slope_out, intercept_out, r2
!!             integer :: i
!!             real    :: rndnum
!!             do i=1,points
!!                x(i)=i*0.10
!!                ! assigned pseudorandom numbers from the uniform distribution in the interval 0  x < 1.
!!                call random_number(rndnum)
!!                y(i)=slope*(x(i)+4.0*(rndnum-0.5))+intercept
!!             enddo
!!             !write(*,*)(ii,x(ii),y(ii),new_line('A'),ii=1,points)
!!             call julfit1(x,y,points,slope_out,intercept_out,r2)
!!             write(*,*)'SLOPE AND INTERCEPT IN  ',slope,intercept
!!             write(*,*)'SLOPE AND INTERCEPT OUT ',slope_out,intercept_out,r2
!!          end subroutine testit
!!
!!       end program demo_julfit1
!!
!!   Results
!!
!!     $ xxx
!!     For y=m*x+b enter M and B and number of points N:
!!     10 20 1000000
!!     SLOPE AND INTERCEPT IN     10.0000000       20.0000000
!!     SLOPE AND INTERCEPT OUT    10.0000000       19.9998207       1.00000000
!!
!!     $ xxx
!!     For y=m*x+b enter M and B and number of points N:
!!     10 20 100
!!     SLOPE AND INTERCEPT IN     10.0000000       20.0000000
!!     SLOPE AND INTERCEPT OUT    9.62195778       23.3507996      0.850686848
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        julfit1(3f)
!! DESCRIPTION:    internal routine for linear least square fit(y=a*x+b), changes the y array
!!##VERSION:        1.0, 1980
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!! COPYRIGHT:      Copyright (C) 1980 John S. Urban
!! LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.
!!                 There is NO WARRANTY, to the extent permitted by law.
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine julfit1(x,y,ixn,a,b,r2)
implicit none
character(len=*),parameter::ident_2="@(#)M_math::julfit1(3f): linear least square fit of (y=a*x+b), changes the y array"
!implicit double precision(a-h,o-z)
!-----------------------------------------------------------------------------------------------------------------------------------
integer,intent(in) :: ixn
real,intent(in)    :: x(*)
real               :: y(*)

real,intent(out)   :: a
real,intent(out)   :: b
real,intent(out)   :: r2
!-----------------------------------------------------------------------------------------------------------------------------------
doubleprecision    :: sx, sy, sxy, sxx, sdel, sdelavg, xn, s, yi, ave, difi, del
integer            :: i10, i20, i30
!-----------------------------------------------------------------------------------------------------------------------------------
!  initialize
   sx=0.0d0                          ! sum of x
   sy=0.0d0                          ! sum of y
   sxy=0.0d0                         ! sum of products of x and y
   sxx=0.0d0                         ! sum of squared x values
   sdel=0.0d0                        ! sum of squares of differences between measured and calculated y values
   sdelavg=0.0d0                     ! sum of squares of differences between measured y values minus average y value
   xn=dble(ixn)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=1,ixn                      ! calculate required sums
      sx=sx+x(i10)                   ! sum of x
      sy=sy+y(i10)                   ! sum of y
      sxy=sxy+x(i10)*y(i10)          ! sum of x*y
      sxx=sxx+x(i10)*x(i10)          ! sum of x*x
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   s=(xn*sxy-sx*sy)/(xn*sxx-sx*sx)   ! compute slope of best line thru points
   yi=(sy-s*sx)/xn                   ! compute y intercept of best line thru points
   ave=sy/xn
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,ixn,1                    ! calculate correlation coefficient
      difi=y(i20)-(s*x(i20)+yi)      ! linearized difference between actual and calculated
      sdel=sdel+difi*difi            ! sum of (differences**2)
      del=y(i20)-ave                 ! linearized difference between actual and calculated average
      sdelavg=sdelavg+del*del        ! sum of (difference_from_average**2)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   r2=1.0d0-sdel/sdelavg
   a=s
   b=yi
   do i30=1,ixn
     y(i30)=a*x(i30)+b
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine julfit1
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     lowess(3f) - [M_math:fit] procedures for locally weighted regression
!!
!!##SYNOPSIS
!!
!!    Calling Sequence:
!!
!!     subroutine lowess(x, y, n, f, nsteps, delta, ys, rw, res)
!!     real,intent(in)    :: x(n)
!!     real,intent(in)    :: y(n)
!!     integer,intent(in) :: n
!!     real,intent(in)    :: f
!!     integer,intent(in) :: nsteps
!!     real,intent(in)    :: delta
!!
!!     real,intent(out)   :: ys(n)
!!     real,intent(out)   :: rw(n)
!!     real,intent(out)   :: rw(res)
!!
!!##PURPOSE
!!
!!    Lowess is a data analysis technique for producing a "smooth" set of
!!    values from a time series which has been contaminated with noise,
!!    or from a scatter plot with a "noisy" relationship between the two
!!    variables. In a time series context, the technique is an improvement
!!    over least squares smoothing when the data is not equally spaced
!!    (as least squares smoothing assumes).
!!
!!##DESCRIPTION
!!
!!    LOWESS stands for "locally weighted regression". LOWESS computes the
!!    smooth of a scatterplot of Y against X using robust locally weighted
!!    regression. Fitted values, YS, are computed at each of the values of
!!    the horizontal axis in X.
!!
!!    For lowess smoothing, the analyst can vary the size of the smoothing
!!    window. This size is given as the fraction (0 to 1) of the data that
!!    the window should cover. The default window size is .2 (which states
!!    that the smoothing window has a total width of 20% of the horizontal
!!    axis variable). The LOWESS fraction (F) controls the smoothness of the
!!    curve. For example, if it is 1.0, then the LOWESS curve is a single
!!    straight line. In general, the smaller the fraction, the more that
!!    LOWESS curve follows individual data points. To obtain a smoother
!!    LOWESS curve, increase the value of the LOWESS FRACTION.
!!
!!    This package consists of two FORTRAN procedures for smoothing
!!    scatterplots by robust locally weighted regression, or lowess. The
!!    principal routine is LOWESS which computes the smoothed values using
!!    the method described in "The Elements of Graphing Data", by William S.
!!    Cleveland (Wadsworth, 555 Morego Street, Monterey, California 93940).
!!
!!    LOWESS calls a support routine, LOWEST, the code for which is
!!    included. LOWESS also calls a routine SORT, which the user must provide.
!!
!!    To reduce the computations, LOWESS requires that the arrays X and Y,
!!    which are the horizontal and vertical coordinates, respectively, of the
!!    scatterplot, be such that X is sorted from smallest to largest. The
!!    user must therefore use another sort routine which will sort X and Y
!!    according to X.
!!
!!    To summarize the scatterplot, YS, the fitted values, should be plotted
!!    against X. No graphics routines are available in the package and must
!!    be supplied by the user.
!!
!!    The FORTRAN code for the routines LOWESS and LOWEST has been generated
!!    from higher level RATFOR programs (B. W. Kernighan, ``RATFOR: A
!!    Preprocessor for a Rational Fortran,'' Software Practice and Experience,
!!    Vol. 5 (1975).
!!
!!##OPTIONS
!!
!!    ARGUMENT DESCRIPTION
!!
!!          X =       Input; abscissas of the points on the
!!                    scatterplot; the values in X must be ordered
!!                    from smallest to largest.
!!          Y =       Input; ordinates of the points on the
!!                    scatterplot.
!!          N =       Input; dimension of X,Y,YS,RW, and RES.
!!          F =       Input; specifies the amount of smoothing; F is
!!                    the fraction of points used to compute each
!!                    fitted value; as F increases the smoothed values
!!                    become smoother; choosing F in the range .2 to
!!                    .8 usually results in a good fit; if you have no
!!                    idea which value to use, try F = .5.
!!          NSTEPS =  Input; the number of iterations in the robust
!!                    fit; if NSTEPS = 0, the nonrobust fit is
!!                    returned; setting NSTEPS equal to 2 should serve
!!                    most purposes.
!!          DELTA =   input; nonnegative parameter which may be used
!!                    to save computations; if N is less than 100, set
!!                    DELTA equal to 0.0; if N is greater than 100 you
!!                    should find out how DELTA works by reading the
!!                    additional instructions section.
!!          YS =      Output; fitted values; YS(I) is the fitted value
!!                    at X(I); to summarize the scatterplot, YS(I)
!!                    should be plotted against X(I).
!!          RW =      Output; robustness weights; RW(I) is the weight
!!                    given to the point (X(I),Y(I)); if NSTEPS = 0,
!!                    RW is not used.
!!          RES =     Output; residuals; RES(I) = Y(I)-YS(I).
!!
!!
!!    ADDITIONAL INSTRUCTIONS
!!
!!        DELTA can be used to save computations.
!!        Very roughly the algorithm is this:
!!        on the initial fit and on each of the NSTEPS iterations locally weighted regression fitted values are computed
!!        at points in X which are spaced, roughly, DELTA apart;
!!        then the fitted values at the remaining points are computed using linear interpolation.
!!        The first locally weighted regression (LWR) computation is carried out at X(1) and the last is carried out at X(N).
!!        Suppose the LWR computation is carried out at X(I).
!!        If X(I+1) is greater than or equal to X(I)+DELTA, the next LWR computation is carried out at X(I+1).
!!        If X(I+1) is less than X(I)+DELTA, the next LWR computation is carried out at the largest X(J) which is greater than
!!        or equal to X(I) but is not greater than X(I)+DELTA.
!!        Then the fitted values for X(K) between X(I) and X(J), if there are any, are computed by linear interpolation
!!        of the fitted values at X(I) and X(J).
!!        If N is less than 100 then DELTA can be set to 0.0 since the computation time will not be too great.
!!        For larger N it is typically not necessary to carry out the LWR computation for all points,
!!        so that much computation time can be saved by taking DELTA to be greater than 0.0.
!!        If DELTA = Range (X)/k then,
!!        if the values in X were uniformly scattered over the range,
!!        the full LWR computation would be carried out at approximately k points.
!!        Taking k to be 50 often works well.
!!
!!    METHOD
!!
!!        The fitted values are computed by using the nearest neighbor
!!        routine and robust locally weighted regression of degree 1
!!        with the tricube weight function. A few additional features
!!        have been added. Suppose r is FN truncated to an integer.
!!        Let h be the distance to the r-th nearest neighbor
!!        from X(I). All points within h of X(I) are used. Thus if
!!        the r-th nearest neighbor is exactly the same distance as
!!        other points, more than r points can possibly be used for
!!        the smooth at X(I). There are two cases where robust
!!        locally weighted regression of degree 0 is actually used at
!!        X(I). One case occurs when h is 0.0. The second case
!!        occurs when the weighted standard error of the X(I) with
!!        respect to the weights w(j) is less than .001 times the
!!        range of the X(I), where w(j) is the weight assigned to the
!!        j-th point of X (the tricube weight times the robustness
!!        weight) divided by the sum of all of the weights. Finally,
!!        if the w(j) are all zero for the smooth at X(I), the fitted
!!        value is taken to be Y(I).
!!
!!##DEPENDENCIES
!!
!!      o LOWEST
!!      o SORT
!!
!!    LOWEST
!!
!!    Calling sequence
!!
!!         CALL LOWEST(X,Y,N,XS,YS,NLEFT,NRIGHT,W,USERW,RW,OK)
!!
!!    PURPOSE
!!
!!        LOWEST is a support routine for LOWESS and ordinarily will
!!        not be called by the user. The fitted value, YS, is
!!        computed at the value, XS, of the horizontal axis.
!!        Robustness weights, RW, can be employed in computing the
!!        fit.
!!
!!    OPTIONS
!!         Argument description
!!
!!          X =       Input; abscissas of the points on the
!!                    scatterplot; the values in X must be ordered
!!                    from smallest to largest.
!!          Y =       Input; ordinates of the points on the
!!                    scatterplot.
!!          N =       Input; dimension of X,Y,W, and RW.
!!          XS =      Input; value of the horizontal axis at which the
!!                    smooth is computed.
!!          YS =      Output; fitted value at XS.
!!          NLEFT =   Input; index of the first point which should be
!!                    considered in computing the fitted value.
!!          NRIGHT =  Input; index of the last point which should be
!!                    considered in computing the fitted value.
!!          W =       Output; W(I) is the weight for Y(I) used in the
!!                    expression for YS, which is the sum from
!!                    I = NLEFT to NRIGHT of W(I)*Y(I); W(I) is
!!                    defined only at locations NLEFT to NRIGHT.
!!          USERW =   Input; logical variable; if USERW is .TRUE., a
!!                    robust fit is carried out using the weights in
!!                    RW; if USERW is .FALSE., the values in RW are
!!                    not used.
!!          RW =      Input; robustness weights.
!!          OK =      Output; logical variable; if the weights for the
!!                    smooth are all 0.0, the fitted value, YS, is not
!!                    computed and OK is set equal to .FALSE.; if the
!!                    fitted value is computed OK is set equal to
!!
!!    METHOD
!!
!!         The smooth at XS is computed using (robust) locally weighted
!!         regression of degree 1. The tricube weight function is used
!!         with h equal to the maximum of XS-X(NLEFT) and X(NRIGHT)-XS.
!!         Two cases where the program reverts to locally weighted
!!         regression of degree 0 are described in the documentation
!!         for LOWESS.
!!
!!##DEPENDENCIES
!!       o lowest
!!       o sort_shell ! user-supplied SORT
!!
!!##EXAMPLES
!!
!!
!!   Example program:
!!
!!       program demo_lowess
!!       use M_math, only : lowess
!!       !  test driver for lowess
!!       !  for expected output, see introduction
!!       real x(20), y(20), ys(20), rw(20), res(20)
!!       data x /1,2,3,4,5,10*6,8,10,12,14,50/
!!       data y /18,2,15,6,10,4,16,11,7,3,14,17,20,12,9,13,1,8,5,19/
!!       call lowess(x,y,20,.25,0,0.,ys,rw,res)
!!       write(*,*) ys
!!       call lowess(x,y,20,.25,0,3.,ys,rw,res)
!!       write(*,*) ys
!!       call lowess(x,y,20,.25,2,0.,ys,rw,res)
!!       write(*,*) ys
!!       end program demo_lowess
!!
!!       The following are data and output from LOWESS that can
!!       be used to check your implementation of the routines. The
!!       notation (10)v means 10 values of v.
!!
!!        X values:
!!          1  2  3  4  5  (10)6  8  10  12  14  50
!!
!!        Y values:
!!           18  2  15  6  10  4  16  11  7  3  14  17  20  12  9  13  1  8  5  19
!!
!!        YS values with F = .25, NSTEPS = 0, DELTA = 0.0
!!         13.659  11.145  8.701  9.722  10.000  (10)11.300  13.000  6.440  5.596
!!           5.456  18.998
!!
!!        YS values with F = .25, NSTEPS = 0 ,  DELTA = 3.0
!!          13.659  12.347  11.034  9.722  10.511  (10)11.300  13.000  6.440  5.596
!!            5.456  18.998
!!
!!        YS values with F = .25, NSTEPS = 2, DELTA = 0.0
!!          14.811  12.115  8.984  9.676  10.000  (10)11.346  13.000  6.734  5.744
!!          5.415  18.998
!!
!!##REFERENCE
!!     This routine is functionally based on the "netlib" routine lowess
!!     from netlib/go/lowess.f .
!!
!!     "Graphical Methods for Data Analysis", Chambers, Cleveland, Kleiner, and
!!     Tukey. Wadsworth, 1983.
!!
!!##APPLICATIONS
!!
!!     Time Series Analysis
!!
!!
!!##SEE ALSO
!!
!!    A multivariate version is available by "send dloess from a"
!!    from the NETLIB server.
!!
!!##AUTHOR
!!
!!    Bill Cleveland
!!
!!     research!alice!wsc Mon Dec 30 16:55 EST 1985
!!     W. S. Cleveland
!!     ATT Bell Laboratories
!!     Murray Hill NJ 07974
!===================================================================================================================================
!>
!! AUTHOR:     Bill Cleveland
!===================================================================================================================================
subroutine lowess(x, y, n, f, nsteps, delta, ys, rw, res)
use M_sort, only : sort_shell
character(len=*),parameter::ident_3="@(#)M_math::lowess(3f): data smoothing using locally weighted regression"
integer n
integer nsteps
real x(n), y(n), f, delta, ys(n), rw(n)
real res(n)
integer nright, i, j, iter, last, m1, m2, ns, nleft
real cut, cmad, r, d1, d2
real c1, c9, alpha, denom
logical ok
      if (n .ge. 2) goto 1
         ys(1) = y(1)
         return
! at least two, at most n points
   1  ns = max0(min0(int(f*float(n)), n), 2)
      iter = 1
         goto  3
   2     iter = iter+1
   3     if (iter .gt. nsteps+1) goto  22
! robustness iterations
         nleft = 1
         nright = ns
! index of prev estimated point
         last = 0
! index of current point
         i = 1
   4        if (nright .ge. n) goto  5
! move nleft, nright to right if radius decreases
               d1 = x(i)-x(nleft)
! if d1<=d2 with x(nright+1)==x(nright), lowest fixes
               d2 = x(nright+1)-x(i)
               if (d1 .le. d2) goto  5
! radius will not decrease by move right
               nleft = nleft+1
               nright = nright+1
               goto  4
! fitted value at x(i)
   5        call lowest(x, y, n, x(i), ys(i), nleft, nright, res, iter .gt. 1, rw, ok)
            if (.not. ok) ys(i) = y(i)
! all weights zero - copy over value (all rw==0)
            if (last .ge. i-1) goto 9
               denom = x(i)-x(last)
! skipped points -- interpolate
! non-zero - proof?
               j = last+1
                  goto  7
   6              j = j+1
   7              if (j .ge. i) goto  8
                  alpha = (x(j)-x(last))/denom
                  ys(j) = alpha*ys(i)+(1.0-alpha)*ys(last)
                  goto  6
   8           continue
! last point actually estimated
   9        last = i
! x coord of close points
            cut = x(last)+delta
            i = last+1
               goto  11
  10           i = i+1
  11           if (i .gt. n) goto  13
! find close points
               if (x(i) .gt. cut) goto  13
! i one beyond last pt within cut
               if (x(i) .ne. x(last)) goto 12
                  ys(i) = ys(last)
! exact match in x
                  last = i
  12           continue
               goto  10
! back 1 point so interpolation within delta, but always go forward
  13        i = max0(last+1, i-1)
            if (last .lt. n) goto  4
! residuals
         do  15 i = 1, n
            res(i) = y(i)-ys(i)
  15        continue
         if (iter .gt. nsteps) goto  22
! compute robustness weights except last time
         do  16 i = 1, n
            rw(i) = abs(res(i))
  16        continue
         call sort_shell(rw, order='A')             ! sort in ascending order
         m1 = n/2+1
         m2 = n-m1+1
! 6 median abs resid
         cmad = 3.0*(rw(m1)+rw(m2))
         c9 = .999*cmad
         c1 = .001*cmad
         do  21 i = 1, n
            r = abs(res(i))
            if (r .gt. c1) goto 17
               rw(i) = 1.
! near 0, avoid underflow
               goto  20
  17           if (r .le. c9) goto 18
                  rw(i) = 0.
! near 1, avoid underflow
                  goto  19
  18              rw(i) = (1.0-(r/cmad)**2)**2
  19        continue
  20        continue
  21        continue
         goto  2
  22  return
end subroutine lowess
subroutine lowest(x, y, n, xs, ys, nleft, nright, w, userw, rw, ok)
integer n
integer nleft, nright
real x(n), y(n), xs, ys, w(n), rw(n)
logical userw, ok
integer nrt, j
real a, b, c, h, r
real h1, sqrt, h9, amax1, range
   range = x(n)-x(1)
   h = amax1(xs-x(nleft), x(nright)-xs)
   h9 = .999*h
   h1 = .001*h
! sum of weights
   a = 0.0
   j = nleft
         goto  2
   1     j = j+1
   2     if (j .gt. n) goto  7
! compute weights (pick up all ties on right)
         w(j) = 0.
         r = abs(x(j)-xs)
         if (r .gt. h9) goto 5
            if (r .le. h1) goto 3
               w(j) = (1.0-(r/h)**3)**3
! small enough for non-zero weight
               goto  4
   3           w(j) = 1.
   4        if (userw) w(j) = rw(j)*w(j)
            a = a+w(j)
            goto  6
   5        if (x(j) .gt. xs) goto  7
! get out at first zero wt on right
   6     continue
         goto  1
! rightmost pt (may be greater than nright because of ties)
   7  nrt = j-1
      if (a .gt. 0.0) goto 8
         ok = .false.
         goto  16
   8     ok = .true.
! weighted least squares
         do  9 j = nleft, nrt
! make sum of w(j) == 1
            w(j) = w(j)/a
   9        continue
         if (h .le. 0.) goto 14
            a = 0.0
! use linear fit
            do  10 j = nleft, nrt
! weighted center of x values
               a = a+w(j)*x(j)
  10           continue
            b = xs-a
            c = 0.0
            do  11 j = nleft, nrt
               c = c+w(j)*(x(j)-a)**2
  11           continue
            if (sqrt(c) .le. .001*range) goto 13
               b = b/c
! points are spread out enough to compute slope
               do  12 j = nleft, nrt
                  w(j) = w(j)*(b*(x(j)-a)+1.0)
  12              continue
  13        continue
  14     ys = 0.0
         do  15 j = nleft, nrt
            ys = ys+w(j)*y(j)
  15        continue
  16  return
end subroutine lowest
!>
!!##NAME
!!    splift(3f) - [M_math:fit] fits a spline to the n data points given in x and y
!!                 and also returns first and second derivatives
!!##SYNOPSIS
!!
!!   subroutine splift(x,y,yp,ypp,n,ierr,a1,b1,an,bn)
!!
!!    real,intent(in)            :: x(n),y(n)
!!    real,intent(out)           :: yp(n),ypp(n)
!!    integer,intent(in)         :: n
!!    integer,intent(out)        :: ierr
!!    real,intent(in)            :: a1
!!    real,intent(in)            :: b1
!!    real,intent(in)            :: an
!!    real,intent(in)            :: bn
!!
!!##DESCRIPTION
!!    SPLIFT(3f) fits a spline to the N data points given in X and Y and returns
!!    the first and second derivatives in YP and YPP. The resulting spline,
!!    defined by the arrays X, Y, and YPP, may then be interpolated (if desired)
!!    using SPLINT(3f).
!!
!!    For a smoothing spline fit see SUBROUTINE SMOOTH.
!!##OPTIONS
!!
!!       X            array of abscissas (in increasing order)
!!       Y            array of ordinates
!!       N            number of data points (the dimension of X,Y,YP and YPP)
!!       A1,B1,AN,BN  end condition specifications
!!
!!                     The end conditions of the spline are
!!                          YPP(1) = A1*YPP(2) + B1
!!                     and
!!                          YPP(N) = AN*YPP(N-1) + BN,
!!                     where
!!                          ABS(A1).LT.1.0 and ABS(AN).LT.1.0.
!!
!!             The smoothest (i.e., least integral of square of
!!             second derivative) spline is obtained by A1=B1=AN=BN=0.
!!             If extrapolation outside the range from X(1) to X(N)
!!             is to be done (By SPLINT(3f), say), better results may
!!             be obtained by using A1=AN=0.5, B1=BN=0.
!!
!!##RETURNS
!!
!!       YP     Resulting derivative
!!       YPP    Resulting second derivative
!!       IERR   Error status.
!!              NORMAL CODES
!!                =0  means the requested spline was computed.
!!              ABNORMAL CODES
!!                =1  means N was too small (.LT.4).
!!                =2  means the abscissas were not strictly increasing.
!!
!!##EXAMPLE
!!
!!
!!##PEDIGREE
!!
!!    Original written by:
!!
!!      Rondall E. Jones
!!      Sandia Mathematical Program Library
!!      Applied Mathematics Division 2642
!!      Sandia Laboratories
!!      P. O. Box 5800
!!      Albuquerque, New Mexico  87115
!!      Control Data 6600 Version 5.1, 10 December 1973
!!
!!
!!      WARD implementation   S. J. Orbon        4/1/1974
!!
!!      F90+ Implementation   J. S. Urban
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        splift(3f)
!! DESCRIPTION:    fits a spline to the n data points given in x and y
!!##VERSION:        5.0: 20170129
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!===================================================================================================================================
!===================================================================================================================================
SUBROUTINE SPLIFT(X,Y,YP,YPP,N,ierr,a1,b1,an,bn)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
character(len=*),parameter::ident_4="@(#)M_math::splift(3f): fits a spline to the n data points given in x and y"
integer,intent(in)            :: N
real,intent(in)               :: X(N),Y(N)
real,intent(out)              :: YP(N),YPP(N)
integer,intent(out)           :: ierr           ! error status.
real,intent(in)               :: a1
real,intent(in)               :: b1
real,intent(in)               :: an
real,intent(in)               :: bn

   character(len=255)         :: ctemp
   real                       :: w(n,3)         ! w is a work array that must be able to hold at least N*3 numbers
   integer                    :: i, j
   integer                    :: NM1
   integer                    :: NM2
   real                       :: DOLD
   real                       :: DNEW
!-----------------------------------------------------------------------------------------------------------------------------------
   if (n.lt.4) then
      ierr=1
      call journal('*splift* number of abscissas too small (.lt.4)')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   DO I=2,N     ! make sure x(:) values are increasing monotonically
      IF ( (X(I)-X(I-1)) .gt. 0 ) cycle
      IERR=2
      call journal('sc','*splift* abscissa not strictly increasing, index=',i)
      write(ctemp,"('*splift* x,y=',g20.13,1x,g20.13)")x(i),y(i)
      call journal(ctemp)
      return
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      NM1  = N-1                                                          ! SPLITF.62
      NM2  = N-2                                                          ! SPLITF.63
!-----------------------------------------------------------------------------------------------------------------------------------
!     DEFINE THE TRIDIAGONAL MATRIX                                       ! SPLITF.68
!                                                                         ! SPLITF.69
      W(1,3) = X(2)-X(1)                                                  ! SPLITF.70
      DO I=2,NM1                                                          ! SPLITF.71
         W(I,2) = W(I-1,3)                                                ! SPLITF.72
         W(I,3) = X(I+1)-X(I)                                             ! SPLITF.73
         W(I,1) = 2.0*(W(I,2)+W(I,3))                                     ! SPLITF.74
      enddo
      W(1,1) = 4.0                                                        ! SPLITF.75
      W(1,3) =-4.0*A1                                                     ! SPLITF.76
      W(N,1) = 4.0                                                        ! SPLITF.77
      W(N,2) =-4.0*AN                                                     ! SPLITF.78
!                                                                         ! SPLITF.79
!     L U DECOMPOSITION                                                   ! SPLITF.80
!                                                                         ! SPLITF.81
      DO I=2,N                                                            ! SPLITF.82
         W(I-1,3) = W(I-1,3)/W(I-1,1)                                     ! SPLITF.83
         W(I,1)   = W(I,1) - W(I,2)*W(I-1,3)                              ! SPLITF.84
      enddo
!                                                                         ! SPLITF.85
!     DEFINE *CONSTANT* VECTOR                                            ! SPLITF.86
!                                                                         ! SPLITF.87
      YPP(1) = 4.0*B1                                                     ! SPLITF.88
      DOLD   = (Y(2)-Y(1))/W(2,2)                                         ! SPLITF.89
      DO I=2,NM2                                                          ! SPLITF.90
         DNEW   = (Y(I+1) - Y(I))/W(I+1,2)                                ! SPLITF.91
         YPP(I) = 6.0*(DNEW - DOLD)                                       ! SPLITF.92
         YP(I)  = DOLD                                                    ! SPLITF.93
         DOLD   = DNEW                                                    ! SPLITF.94
      enddo
      DNEW   = (Y(N)-Y(N-1))/(X(N)-X(N-1))                                ! SPLITF.95
      YPP(NM1) = 6.0*(DNEW - DOLD)                                        ! SPLITF.96
      YPP(N) = 4.0*BN                                                     ! SPLITF.97
      YP(NM1)= DOLD                                                       ! SPLITF.98
      YP(N)  = DNEW                                                       ! SPLITF.99
!                                                                         ! SPLITF.100
!     FORWARD SUBSTITUTION                                                ! SPLITF.101
!                                                                         ! SPLITF.102
      YPP(1) = YPP(1)/W(1,1)                                              ! SPLITF.103
      DO I=2,N                                                            ! SPLITF.104
         YPP(I) = (YPP(I) - W(I,2)*YPP(I-1))/W(I,1)                       ! SPLITF.105
      enddo
!                                                                         ! SPLITF.106
!     BACKWARD SUBSTITUTION                                               ! SPLITF.107
!                                                                         ! SPLITF.108
      DO J=1,NM1                                                          ! SPLITF.109
         I = N-J                                                          ! SPLITF.110
         YPP(I) = YPP(I) - W(I,3)*YPP(I+1)                                ! SPLITF.111
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     COMPUTE FIRST DERIVATIVES                                           ! SPLITF.113
!                                                                         ! SPLITF.114
      YP(1)  = (Y(2)-Y(1))/(X(2)-X(1)) - (X(2)-X(1))*(2.0*YPP(1) + YPP(2))/6.0
      DO I=2,NM1
         YP(I)  = YP(I) + W(I,2)*(YPP(I-1) + 2.0*YPP(I))/6.0              ! SPLITF.118
      enddo
      YP(N)  = YP(N) + (X(N)-X(NM1))*(YPP(NM1) + 2.0*YPP(N))/6.0          ! SPLITF.119
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
end subroutine splift
!===================================================================================================================================
!>
!!##NAME
!!    splint(3f) - [M_math:fit] interpolates and twice differentiates a cubic spline
!!##SYNOPSIS
!!
!!   subroutine splint (x,y,ypp,n,xi,yi,ypi,yppi,ni,kerr)
!!
!!    integer,intent(in) :: n, ni
!!    real,intent(in)    :: x(n),y(n),ypp(n),xi(ni)
!!    real,intent(out)   :: yi(ni),ypi(ni),yppi(ni)
!!
!!##DESCRIPTION
!!    SPLINT(3f) interpolates and twice differentiates a cubic spline
!!    defined by X, Y, and YPP at the abscissas in XI. The spline
!!    may have been determined by SPLIFT(3f) or SMOOTH(3f) or any other
!!    spline fitting routine which provides second derivatives.
!!
!!##OPTIONS
!!     X      array of data abscissas
!!     Y      array of data ordinates
!!     YPP    array of spline second derivatives
!!     N      number of data points (the dimension of X,Y, and YPP)
!!     XI     array of abscissas (in arbitrary order) at which
!!            the spline is to be interpolated.
!!     NI     dimension of XI, YI, YPI, and YPPI.
!!            (if NI=1, XI, YI, YPI and YPPI may be simple variables.)
!!##RETURNS
!!     YI     array of interpolated ordinates (OUTPUT)
!!     YPI    array of interpolated derivatives (OUTPUT)
!!     YPPI   array of interpolated second derivatives (OUTPUT)
!!     KERR   error status parameter (OUTPUT)
!!           NORMAL CODES
!!           =0  means the spline was evaluated at each abscissa
!!               in XI using only interpolation.
!!           =1  means the spline was evaluated at each abscissa
!!               in XI, but at least one extrapolation was performed.
!!           ABNORMAL CODE
!!           =2  means the requested number of interpolations, NI,
!!               was not positive.
!!##EXAMPLE
!!
!!
!!##PEDIGREE
!!
!!    Original written by:
!!
!!      Rondall E. Jones
!!      Sandia Mathematical Program Library
!!      Applied Mathematics Division 2642
!!      Sandia Laboratories
!!      P. O. Box 5800
!!      Albuquerque, New Mexico  87115
!!
!!    Control Data 6600 Version 5.1, 10 December 1973
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        splint(3f)
!! DESCRIPTION:    interpolates and twice differentiates a cubic spline
!!##VERSION:        5.0: 20170129
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!===================================================================================================================================
subroutine splint (x,y,ypp,n,xi,yi,ypi,yppi,ni,kerr)
character(len=*),parameter::ident_5="@(#)M_math::splint(3f): interpolates and twice differentiates a cubic spline"
integer,intent(in)  :: n
integer,intent(in)  :: ni
real,intent(in)     :: x(n),y(n),ypp(n),xi(ni)
real,intent(out)    :: yi(ni),ypi(ni),yppi(ni)
integer,intent(out) :: kerr

integer            :: nm1, k, il, ir, i
real               :: h, h2, xx
real               :: xr, xr2, xr3, xl, xl2, xl3

!  CHECK INPUT
   if (ni .le. 0)then
      kerr=2
      return
   endif
   kerr=0
   nm1= n-1
!  K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!  I IS CURRENT INDEX INTO X ARRAY.
   k  = 1
   xx = xi(1)
   if (xx.lt.x(1)) goto 90
   if (xx.gt.x(n)) goto 80
   il = 1
   ir = n
!------------------------------------
!  BISECTION SEARCH
   10 continue
      i  = (il+ir)/2
      if (i.eq.il) goto 100
      if (xx-x(i)) 20,100,30
   20 continue
      ir = i
      goto 10
   30 continue
      il = i
      goto 10
!------------------------------------
!     LINEAR FORWARD SEARCH
   50 continue
      if (xx-x(i+1)) 100,100,60
   60 continue
      if (i.ge.nm1) goto 80
      i  = i+1
      goto 50
!------------------------------------
!     EXTRAPOLATION
   80 continue
      kerr=1
      i  = nm1
      goto 100
   90 continue
      kerr=1
      i  = 1
!------------------------------------
!     INTERPOLATION
  100 continue
      h  = x(i+1) - x(i)
      h2 = h*h
      xr = (x(i+1)-xx)/h
      xr2= xr*xr
      xr3= xr*xr2
      xl = (xx-x(i))/h
      xl2= xl*xl
      xl3= xl*xl2
      yi(k) = y(i)*xr + y(i+1)*xl -h2*(ypp(i)*(xr-xr3) + ypp(i+1)*(xl-xl3))/6.0
      ypi(k) = (y(i+1)-y(i))/h +h*(ypp(i)*(1.0-3.0*xr2) - ypp(i+1)*(1.0-3.0*xl2))/6.0
      yppi(k) = ypp(i)*xr + ypp(i+1)*xl
!------------------------------------
!     NEXT POINT
      if (k.ge.ni) goto 120
      k = k+1
      xx = xi(k)
      if (xx.lt.x(1)) goto 90
      if (xx.gt.x(n)) goto 80
      if (xx-xi(k-1)) 110,100,50
  110 continue
      il = 1
      ir = i+1
  120 continue
END SUBROUTINE SPLINT
!>
!!##NAME
!!      linearint(3f) - [M_math:fit] interpolates a curve defined by X(i),Y(i) using linear interpolation at given XI(j) values
!!##SYNOPSIS
!!
!!      SUBROUTINE linearint(X,Y,N,XI,YI,NI,KERR)
!!
!!       INTEGER,intent(in)  :: N, NI
!!       REAL,intent(in)     :: X(N),Y(N),XI(NI)
!!       REAL,intent(out)    :: YI(NI)
!!       INTEGER,intent(out) :: KERR
!!##DESCRIPTION
!!##OPTIONS
!!         X      array of data abscissas
!!         Y      array of data ordinates
!!         N      number of data points (the dimension of X,Y)
!!         XI     array of abscissas (in arbitrary order) at which the curve  is to be interpolated.
!!         YI     array of interpolated ordinates (OUTPUT)
!!         NI     dimension of XI, YI (if NI=1, XI, YI  may be simple variables.)
!!         KERR   error status parameter
!!                 NORMAL CODES:
!!                 =0  means the curve was evaluated at each abscissa in XI using only interpolation.
!!                 =1  means the curve was evaluated at each abscissa in XI, but at least one extrapolation was performed.
!!                 ABNORMAL CODES:
!!                 =2  means the requested number of interpolations, NI, was not positive.
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        linearint(1)
!! DESCRIPTION:    interpolates a curve <X(i),Y(i)> using linear interpolation at given XI(j) values
!!##VERSION:        1.0, 20031123
!! AUTHOR:         John S. Urban (hacked from splint)
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!===================================================================================================================================
SUBROUTINE linearint(X,Y,N,XI,YI,NI,KERR)
implicit none
character(len=*),parameter::ident_6="@(#)M_math::linearint(3f):linear interpolation of curve X(i),Y(i) at given XI(j) values"
!
   INTEGER,intent(in)  :: N, NI
   REAL,intent(in)     :: X(N),Y(N),XI(NI)
   REAL,intent(out)    :: YI(NI)
   INTEGER,intent(out) :: KERR
!=======================================================================
   integer             :: k, i, il, ir, nm1
   real                :: xx, delta, h, v, h2, delta2
!=======================================================================
!     CHECK INPUT
      if(ni.le.0)then
         kerr=2
         return
      endif
      kerr=0
!=======================================================================
!     K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!     I IS CURRENT INDEX INTO X ARRAY.
      K  = 1
      XX = XI(1)
      IF (XX.LT.X(1)) GOTO 90 ! extrapolation
      IF (XX.GT.X(N)) GOTO 80 ! extrapolation
      IL = 1
      IR = N
      NM1= N-1
!=======================================================================
!     BISECTION SEARCH
   10 continue
      I  = (IL+IR)/2
      IF (I.EQ.IL) GOTO 100
      DELTA=XX-X(I)
      IF (DELTA.lt.0)then
        IR=I
        GOTO 10
      elseif(DELTA.gt.0)then
        IL = I
        GOTO 10
      else
        goto 100
      endif
!=======================================================================
!     LINEAR FORWARD SEARCH
   50 CONTINUE
      IF (XX-X(I+1).le.0)goto 100  ! interpolation
      IF (I.GE.NM1) GOTO 80        ! extrapolation
      I  = I+1                     ! go forward again
      GOTO 50
!=======================================================================
!     EXTRAPOLATION
   80 CONTINUE
      KERR=1
      I  = NM1
      GOTO 100
!=======================================================================
   90 KERR=1
      I  = 1
!=======================================================================
!     INTERPOLATION
  100 continue
      H  = X(I+1) - X(I)
      V  = Y(I+1) - Y(I)
      H2 = XX-X(I)
      YI(K) = Y(I) + V*(H2/H)
!     NEXT POINT
      IF (K.GE.NI) RETURN
      K = K+1
      XX = XI(K)
      IF (XX.LT.X(1)) GOTO 90
      IF (XX.GT.X(N)) GOTO 80
      DELTA2=XX-XI(K-1)
      IF (DELTA2.lt.0)then
         IL = 1
         IR = I+1
         GOTO 10
      elseif(delta2.eq.0)then
         goto 100         ! interpolate
      else
        goto 50           ! linear forward search
      endif
!=======================================================================
END SUBROUTINE linearint

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine gcsgau1(n,a,b)
use M_journal, only : journal
implicit none
!********************************************************************
!****                GRAPHICS COMPATIBILITY SYSTEM               ****
!****                    3-D DEVICE-DEPENDENT                    ****
!****                      SUBROUTINE GCSGAU1                    ****
!****                           LEVEL 7                          ****
!****               WRITTEN BY       FRED TAYLOR                 ****
!********************************************************************

!@(#) SOLVE A SYSTEM OF SIMULTANEOUS LINEAR EQUATIONS OF THE FORM

!
!     **                           **  **    **   **    **
!     * A(1,1)  A(1,2)  ...  A(1,N) *  * X(1) *   * B(1) *
!     *                             *  *      *   *      *
!     * A(2,1)  A(2,2)  ...  A(2,N) *  * X(2) *   * B(2) *
!     *                             *  *      *   *      *
!     *    .       .            .   *  *   .  * = *   .  *
!     *    .       .            .   *  *   .  *   *   .  *
!     *    .       .            .   *  *   .  *   *   .  *
!     *                             *  *      *   *      *
!     * A(N,1)  A(N,2)  ...  A(N,N) *  * X(N) *   * B(N) *
!     **                           **  **    **   **    **
!
!    WHERE MATRICES A AND B ARE KNOWN AND MATRIX X IS THE SET OF
!    UNKNOWNS TO BE DETERMINED.  N IS THE NUMBER OF EQUATIONS.
!
!     F. T. TRACY, COMPUTER ANALYSIS BRANCH USAEWES, VICKSBURG, MS. 39180
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter  :: dp=kind(0.0d0)
integer,intent(in) :: n
real(kind=dp)      :: a(11,11)
real(kind=dp)      :: b(*)

real(kind=dp)      :: amx
real(kind=dp)      :: d
real(kind=dp)      :: eps
real(kind=dp)      :: fa
real(kind=dp)      :: fm
real(kind=dp)      :: sum
real(kind=dp)      :: x(11)
integer            :: i, j, k
integer            :: kpl1
integer            :: m
integer            :: mpl1
integer            :: ncq
integer            :: nm1
!-----------------------------------------------------------------------------------------------------------------------------------
   eps = 1.0d-30
   ! Obtain upper triangular matrix and modified b matrix.
   nm1=n-1
   do 110 k=1,nm1
      ! Perform K "th" step of Gauss Elimination.462C
      kpl1=k+1
      ! Perform partial pivoting.
      ! Find maximum element in absolute value of the elements, A(K,K),
      ! A(K+1,K), ... A(N,K).
      amx=0.0d0
      do 50 i=k,n
         fa=abs(a(i,k))
         if (fa-amx) 50,50,45
45       amx=fa
         ncq=i
50    continue

      ! Check for no solution.
      if (amx-eps) 60,75,75

      ! The Gauss Elimination process has broken down because no pivot
      ! greater than the input tolerance could be found for !K! step

60    continue
      call journal('*gauss* elimination process has broken down')
      return

      ! Interchange rows K and NCQ.
75    continue
      do j=k,n
         d=a(k,j)
         a(k,j)=a(ncq,j)
         a(ncq,j)=d
      enddo
      d=b(k)
      b(k)=b(ncq)
      b(ncq)=d

      ! Perform elimination process.
      do i=kpl1,n
         ! Calculate multipliers.
         fm=-a(i,k)/a(k,k)
         do j=kpl1,n
            a(i,j)=a(k,j)*fm+a(i,j)
         enddo
         b(i)=b(k)*fm+b(i)
      enddo
110 continue

   ! Check for no solution.
   if (abs(a(n,n))-eps) 112,115,115

   ! A(N,N) is smaller than the allowable tolerance for a pivot

112 continue
   call journal('*gauss* elimination process has broken down')
   return

   ! Calculate matrix X.

   ! Perform back substitution.
115 continue
   x(n)=b(n)/a(n,n)
   do k=2,n
      m=n-k+1
      mpl1=m+1
      sum=0.
      do j=mpl1,n
         sum=a(m,j)*x(j)+sum
      enddo
      x(m)=(b(m)-sum)/a(m,m)
   enddo

   do i=1,n
      b(i)=x(i)
   enddo

end subroutine gcsgau1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine glstsq(ideg,x,y,n0,d)
use M_journal, only : journal
implicit doubleprecision(a-h,o-z)

!@(#) least squares fit to a polynomial expression

!********************************************************************
!****                graphics compatibility system               ****
!****                            basic                           ****
!****                      subroutine ulstsq                     ****
!****                          level 13                          ****
!****                  written by       Fred  Tracy              ****
!****                  modified by      John S. Urban            ****
!********************************************************************
!********************************************************************
!**** needs rewritten to normalize data
!**** so large numbers causing overflow are not generated.
!************************************************************************************************************************************
integer             :: ideg       !* ideg is  desired degree of least square fit and test
real                   x(*)
real                   y(*)
integer             :: n0         !* n0   is  number of points in curve arrays (x, y)
doubleprecision        d(*)       !* d    is  returned coefficient array

doubleprecision        a(11,11)
integer             :: i, j, k
integer             :: n
integer             :: nppl1
integer             :: nppl2
integer             :: jm1
integer             :: iz
integer             :: iexp
!------------------------------------------------------------------------------------------------------------------------------------
   n=n0
!------------------------------------------------------------------------------------------------------------------------------------
   if((ideg.lt.1).or.(ideg.gt.10))then
      call journal('*fit* invalid polynomial degree for polynomial fit')
   elseif(n.le.ideg)then ! test if enough points to do desired fit
      call journal('*fit* insufficient points for desired fit')
   else
!------------------------------------------------------------------------------------------------------------------------------------
      nppl1=ideg+1
      nppl2=ideg+2
      do j=1,nppl1
!          calculate  (d)  matrix.
         jm1=j-1
!------------------------------------------------------------------------------------------------------------------------------------
         if (jm1 .le. 0)then
            iz=1
            add=0.0d0
            do i=1,n
               add=y(i)+add
            enddo
         else
!------------------------------------------------------------------------------------------------------------------------------------
            add=0.0d0
            do i=1,n
               add=x(i)**jm1*y(i)+add
            enddo
         endif
!------------------------------------------------------------------------------------------------------------------------------------
         d(j)=add
!------------------------------------------------------------------------------------------------------------------------------------
!     calculate ((a)) matrix.
         do k=j,nppl1
            iexp=jm1+k-1
!------------------------------------------------------------------------------------------------------------------------------------
            if(iz.ne.2)then
               add=n
               iz=2
            else
!------------------------------------------------------------------------------------------------------------------------------------
               add=0.
               do i=1,n
                  add=x(i)**iexp+add
               enddo
            endif
!------------------------------------------------------------------------------------------------------------------------------------
            a(j,k)=add
            a(k,j)=add
         enddo
      enddo
!------------------------------------------------------------------------------------------------------------------------------------
!          solve system of equations
!              ((a)) * (c) = (d)
!          for (c).
!      coefficients will be in array d
      n = nppl1
      call gcsgau1(n,a,d) ! note that d is doubleprecision, a is doubleprecision
   endif
end subroutine glstsq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE GCSGAU2(N,A,B)
!********************************************************************
!********************************************************************
!****                                                            ****
!****                GRAPHICS COMPATIBILITY SYSTEM               ****
!****                                                            ****
!****                    3-D DEVICE-DEPENDENT                    ****
!****                                                            ****
!****                      SUBROUTINE GCSGAU2                    ****
!****                                                            ****
!****                           LEVEL 7                          ****
!****                                                            ****
!****               WRITTEN BY       FRED TAYLOR                 ****
!****                                                            ****
!********************************************************************
!@(#) SOLVE A SYSTEM OF SIMULTANEOUS LINEAR EQUATIONS
!     OF THE FORM
!
!     **                           **  **    **   **    **
!     * A(1,1)  A(1,2)  ...  A(1,N) *  * X(1) *   * B(1) *
!     *                             *  *      *   *      *
!     * A(2,1)  A(2,2)  ...  A(2,N) *  * X(2) *   * B(2) *
!     *                             *  *      *   *      *
!     *    .       .            .   *  *   .  * = *   .  *
!     *    .       .            .   *  *   .  *   *   .  *
!     *    .       .            .   *  *   .  *   *   .  *
!     *                             *  *      *   *      *
!     * A(N,1)  A(N,2)  ...  A(N,N) *  * X(N) *   * B(N) *
!     **                           **  **    **   **    **
!
!    WHERE MATRICES A AND B ARE KNOWN AND MATRIX X IS THE SET OF
!    UNKNOWNS TO BE DETERMINED.  N IS THE NUMBER OF EQUATIONS.
!
!     F. T. TRACY, COMPUTER ANALYSIS BRANCH USAEWES, VICKSBURG, MS. 39180
!-----------------------------------------------------------------------
   use M_journal, only : journal
   implicit doubleprecision(a-h,o-z)
   integer      :: i
   integer      :: j
   integer      :: k
   integer      :: kpl1
   integer      :: m
   integer      :: mpl1
   integer      :: n
   integer      :: ncq
   integer      :: nm1

   real         :: a
   real         :: amx
   real         :: b
   real         :: d
   real         :: eps
   real         :: fa
   real         :: fm
   real         :: sum
   real         :: x
   DIMENSION A(11,11),X(11),b(*)
!-----------------------------------------------------------------------
   EPS = 1.0d-30
!     OBTAIN UPPER TRIANGULAR MATRIX AND MODIFIED B MATRIX.
   NM1=N-1
   DO 110 K=1,NM1
!     PERFORM K "TH" STEP OF GAUSS ELIMINATION.462C
      KPL1=K+1
!     PERFORM PARTIAL PIVOTING.
!     FIND MAXIMUM ELEMENT IN ABSOLUTE VALUE OF THE ELEMENTS, A(K,K),
!     A(K+1,K), ... A(N,K).
      AMX=0.0d0
      DO 50 I=K,N
         FA=ABS(A(I,K))
         IF (FA-AMX) 50,50,45
45       AMX=FA
         NCQ=I
50    CONTINUE
!
!     CHECK FOR NO SOLUTION.
      IF (AMX-EPS) 60,75,75
!
!     THE GAUSS ELIMINATION PROCESS HAS BROKEN DOWN BECAUSE NO PIVOT
!     GREATER THAN THE INPUT TOLERANCE COULD BE FOUND FOR !K! STEP
!
60    continue
      call journal('*gauss* elimination process has broken down')
      RETURN
!
!     INTERCHANGE ROWS K AND NCQ.
75    continue
      DO J=K,N
         D=A(K,J)
         A(K,J)=A(NCQ,J)
         A(NCQ,J)=D
      ENDDO
      D=B(K)
      B(K)=B(NCQ)
      B(NCQ)=D
!
!     PERFORM ELIMINATION PROCESS.
      DO I=KPL1,N
!
!     CALCULATE MULTIPLIERS.
         FM=-A(I,K)/A(K,K)
!
         DO J=KPL1,N
            A(I,J)=A(K,J)*FM+A(I,J)
         ENDDO
         B(I)=B(K)*FM+B(I)
      ENDDO
110 CONTINUE
!
!     CHECK FOR NO SOLUTION.
   IF (ABS(A(N,N))-EPS) 112,115,115
!
!      A(N,N) IS SMALLER THAN THE ALLOWABLE
!      TOLERANCE FOR A PIVOT
!
112 CONTINUE
   call journal('*gauss* elimination process has broken down')
   RETURN
!
!     CALCULATE MATRIX X.
!
!     PERFORM BACK SUBSTITUTION.
115 CONTINUE
   X(N)=B(N)/A(N,N)
   DO K=2,N
      M=N-K+1
      MPL1=M+1
      SUM=0.
      DO J=MPL1,N
         SUM=A(M,J)*X(J)+SUM
      ENDDO
      X(M)=(B(M)-SUM)/A(M,M)
   ENDDO
!
   DO I=1,N
      B(I)=X(I)
   ENDDO
!
END SUBROUTINE GCSGAU2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   SUBROUTINE JU_POLFIT (N, X, Y, W, MAXDEG, NDEG, EPS, R, IERR, A)
!***BEGIN PROLOGUE  JU_POLFIT
!***PURPOSE   @(#) Fit discrete data in a least squares sense by polynomials in one variable.
!***LIBRARY   SLATEC
!***CATEGORY  K1A1A2
!***TYPE      SINGLE PRECISION (JU_POLFIT-S, DPOLFT-D)
!***KEYWORDS  CURVE FITTING, DATA FITTING, LEAST SQUARES, POLYNOMIAL FIT
!***AUTHOR    Shampine, L. F., (SNLA)
!             Davenport, S. M., (SNLA)
!             Huddleston, R. E., (SNLL)
!***DESCRIPTION
!
!     Abstract
!
!     Given a collection of points X(I) and a set of values Y(I) which
!     correspond to some function or measurement at each of the X(I),
!     subroutine  JU_POLFIT  computes the weighted least-squares polynomial
!     fits of all degrees up to some degree either specified by the user
!     or determined by the routine.  The fits thus obtained are in
!     orthogonal polynomial form.  Subroutine  JU_PVALUE  may then be
!     called to evaluate the fitted polynomials and any of their
!     derivatives at any point.  The subroutine  PCOEF  may be used to
!     express the polynomial fits as powers of (X-C) for any specified
!     point C.
!
!     The parameters for  JU_POLFIT  are
!
!     Input --
!         N -      the number of data points.  The arrays X, Y and W
!                  must be dimensioned at least  N  (N .GE. 1).
!         X -      array of values of the independent variable.  These
!                  values may appear in any order and need not all be
!                  distinct.
!         Y -      array of corresponding function values.
!         W -      array of positive values to be used as weights.  If
!                  W(1) is negative,  JU_POLFIT  will set all the weights
!                  to 1.0, which means unweighted least squares error
!                  will be minimized.  To minimize relative error, the
!                  user should set the weights to:  W(I) = 1.0/Y(I)**2,
!                  I = 1,...,N .
!         MAXDEG - maximum degree to be allowed for polynomial fit.
!                  MAXDEG  may be any non-negative integer less than  N.
!                  Note -- MAXDEG  cannot be equal to  N-1  when a
!                  statistical test is to be used for degree selection,
!                  i.e., when input value of  EPS  is negative.
!         EPS -    specifies the criterion to be used in determining
!                  the degree of fit to be computed.
!                  (1)  If  EPS  is input negative,  JU_POLFIT  chooses the
!                       degree based on a statistical F test of
!                       significance.  One of three possible
!                       significance levels will be used:  .01, .05 or
!                       .10.  If  EPS=-1.0 , the routine will
!                       automatically select one of these levels based
!                       on the number of data points and the maximum
!                       degree to be considered.  If  EPS  is input as
!                       -.01, -.05, or -.10, a significance level of
!                       .01, .05, or .10, respectively, will be used.
!                  (2)  If  EPS  is set to 0.,  JU_POLFIT  computes the
!                       polynomials of degrees 0 through  MAXDEG .
!                  (3)  If  EPS  is input positive,  EPS  is the RMS
!                       error tolerance which must be satisfied by the
!                       fitted polynomial.  JU_POLFIT  will increase the
!                       degree of fit until this criterion is met or
!                       until the maximum degree is reached.
!
!     Output --
!         NDEG -   degree of the highest degree fit computed.
!         EPS -    RMS error of the polynomial of degree  NDEG .
!         R -      vector of dimension at least NDEG containing values
!                  of the fit of degree  NDEG  at each of the  X(I) .
!                  Except when the statistical test is used, these
!                  values are more accurate than results from subroutine
!                  JU_PVALUE  normally are.
!         IERR -   error flag with the following possible values.
!             1 -- indicates normal execution, i.e., either
!                  (1)  the input value of  EPS  was negative, and the
!                       computed polynomial fit of degree  NDEG
!                       satisfies the specified F test, or
!                  (2)  the input value of  EPS  was 0., and the fits of
!                       all degrees up to  MAXDEG  are complete, or
!                  (3)  the input value of  EPS  was positive, and the
!                       polynomial of degree  NDEG  satisfies the RMS
!                       error requirement.
!             2 -- invalid input parameter.  At least one of the input
!                  parameters has an illegal value and must be corrected
!                  before  JU_POLFIT  can proceed.  Valid input results
!                  when the following restrictions are observed
!                       N .GE. 1
!                       0 .LE. MAXDEG .LE. N-1  for  EPS .GE. 0.
!                       0 .LE. MAXDEG .LE. N-2  for  EPS .LT. 0.
!                       W(1)=-1.0  or  W(I) .GT. 0., I=1,...,N .
!             3 -- cannot satisfy the RMS error requirement with a
!                  polynomial of degree no greater than  MAXDEG .  Best
!                  fit found is of degree  MAXDEG .
!             4 -- cannot satisfy the test for significance using
!                  current value of  MAXDEG .  Statistically, the
!                  best fit found is of order  NORD .  (In this case,
!                  NDEG will have one of the values:  MAXDEG-2,
!                  MAXDEG-1, or MAXDEG).  Using a higher value of
!                  MAXDEG  may result in passing the test.
!         A -      work and output array having at least 3N+3MAXDEG+3
!                  locations
!
!     Note - JU_POLFIT  calculates all fits of degrees up to and including
!            NDEG .  Any or all of these fits can be evaluated or
!            expressed as powers of (X-C) using  JU_PVALUE  and  PCOEF
!            after just one call to  JU_POLFIT .
!
!***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston,
!                 Curve fitting by polynomials in one variable, Report
!                 SLA-74-0270, Sandia Laboratories, June 1974.
!***ROUTINES CALLED  JU_PVALUE, JU_XERMSG
!***REVISION HISTORY  (YYMMDD)
!   740601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to JU_XERMSG.  (THJ)
!   920501  Reformatted the REFERENCES section.  (WRB)
!   920527  Corrected erroneous statements in DESCRIPTION.  (WRB)
!***END PROLOGUE  JU_POLFIT
integer      :: i100
integer      :: i20
integer      :: i200
integer      :: i30
integer      :: i300
integer      :: i40
integer      :: i400
integer      :: i50
integer      :: i500
integer      :: i60
integer      :: i70
integer      :: i80
integer      :: i88
integer      :: i90
integer      :: idegf
integer      :: ierr
integer      :: iii
integer      :: j
integer      :: jp1
integer      :: jpas
integer      :: k1
integer      :: k1pj
integer      :: k2
integer      :: k2pj
integer      :: k3
integer      :: k3pi
integer      :: k4
integer      :: k4pi
integer      :: k5
integer      :: k5pi
integer      :: ksig
integer      :: m
integer      :: maxdeg
integer      :: mop1
integer      :: n
integer      :: ndeg
integer      :: nder
integer      :: nfail
real         :: a
real         :: co
real         :: degf
real         :: den
real         :: eps
real         :: etst
real         :: f
real         :: fcrit
real         :: r
real         :: sig
real         :: sigj
real         :: sigjm1
real         :: sigpas
real         :: temp
real         :: w
real         :: w1
real         :: w11
real         :: x
real         :: xm
real         :: y
real         :: yp(0)
DOUBLE PRECISION TEMD1,TEMD2
DIMENSION X(*), Y(*), W(*), R(*), A(*)
DIMENSION CO(4,3)
SAVE CO
DATA  CO(1,1), CO(2,1), CO(3,1), CO(4,1), CO(1,2), CO(2,2),   &
&      CO(3,2), CO(4,2), CO(1,3), CO(2,3), CO(3,3),           &
&  CO(4,3)/-13.086850,-2.4648165,-3.3846535,-1.2973162,       &
&          -3.3381146,-1.7812271,-3.2578406,-1.6589279,       &
&          -1.6282703,-1.3152745,-3.2640179,-1.9829776/
!***FIRST EXECUTABLE STATEMENT  JU_POLFIT
      write(*,*)'JU_POLFIT N=',N

      do i90=1,N
         write(*,*)i90,x(i90),y(i90),w(i90),r(i90)
      enddo

      write(*,*)'MAXDEG=',MAXDEG
      write(*,*)'eps=',EPS
!***FIRST EXECUTABLE STATEMENT  JU_POLFIT
      M = ABS(N)
      IF (M .EQ. 0) GOTO 888
      IF (MAXDEG .LT. 0) GOTO 888
      A(1) = MAXDEG
      MOP1 = MAXDEG + 1
      IF (M .LT. MOP1) GOTO 888
      IF (EPS .LT. 0.0  .AND.  M .EQ. MOP1) GOTO 888
      XM = M
      ETST = EPS*EPS*XM
      IF (W(1) .GE. 0.0)then
         DO I20 = 1,M
            IF (W(I20) .LE. 0.0) GOTO 888
         ENDDO
      else
         DO I30 = 1,M
            W(I30) = 1.0
         ENDDO
      endif

      IF (EPS .GE. 0.0) GOTO 8
!
! DETERMINE SIGNIFICANCE LEVEL INDEX TO BE USED IN STATISTICAL TEST FOR
! CHOOSING DEGREE OF POLYNOMIAL FIT
!
      IF (EPS .GT. (-.55)) GOTO 5
      IDEGF = M - MAXDEG - 1
      KSIG = 1
      IF (IDEGF .LT. 10) KSIG = 2
      IF (IDEGF .LT. 5) KSIG = 3
      GOTO 8
5     CONTINUE
      KSIG = 1
      IF (EPS .LT. (-.03)) KSIG = 2
      IF (EPS .LT. (-.07)) KSIG = 3
!
! INITIALIZE INDEXES AND COEFFICIENTS FOR FITTING
!
8     CONTINUE
      K1 = MAXDEG + 1
      K2 = K1 + MAXDEG
      K3 = K2 + MAXDEG + 2
      K4 = K3 + M
      K5 = K4 + M

      DO I40 = 2,K4
         A(I40) = 0.0
      ENDDO

      W11 = 0.0
      IF (N .LT. 0) GOTO 11
!
! UNCONSTRAINED CASE
!
      DO I50 = 1,M
         K4PI = K4 + I50
         A(K4PI) = 1.0
         W11 = W11 + W(I50)
      ENDDO

      GOTO 13
!
! CONSTRAINED CASE
!
11    CONTINUE
      DO I60 = 1,M
         K4PI = K4 + I60
         W11 = W11 + W(I60)*A(K4PI)**2
      ENDDO
!
! COMPUTE FIT OF DEGREE ZERO
!
13    CONTINUE
      TEMD1 = 0.0D0
      DO I70 = 1,M
         K4PI = K4 + I70
         TEMD1 = TEMD1 + DBLE(W(I70))*DBLE(Y(I70))*DBLE(A(K4PI))
      ENDDO
      TEMD1 = TEMD1/DBLE(W11)
      A(K2+1) = TEMD1
      SIGJ = 0.0
      DO I80 = 1,M
         K4PI = K4 + I80
         K5PI = K5 + I80
         TEMD2 = TEMD1*DBLE(A(K4PI))
         R(I80) = TEMD2
         A(K5PI) = TEMD2 - DBLE(R(I80))
         SIGJ = SIGJ + W(I80)*((Y(I80)-R(I80)) - A(K5PI))**2
      ENDDO
      J = 0
!
! SEE IF POLYNOMIAL OF DEGREE 0 SATISFIES THE DEGREE SELECTION CRITERION
!
      IF (EPS) 24,26,27
!=======================================================================
!
! INCREMENT DEGREE
!
16    CONTINUE
      J = J + 1
      JP1 = J + 1
      K1PJ = K1 + J
      K2PJ = K2 + J
      SIGJM1 = SIGJ
!
! COMPUTE NEW B COEFFICIENT EXCEPT WHEN J = 1
!
      IF (J .GT. 1) A(K1PJ) = W11/W1
!
! COMPUTE NEW A COEFFICIENT
!
      TEMD1 = 0.0D0
      DO I100 = 1,M
         K4PI = K4 + I100
         TEMD2 = A(K4PI)
         TEMD1 = TEMD1 + DBLE(X(I100))*DBLE(W(I100))*TEMD2*TEMD2
      enddo
      A(JP1) = TEMD1/DBLE(W11)
!
! EVALUATE ORTHOGONAL POLYNOMIAL AT DATA POINTS
!
      W1 = W11
      W11 = 0.0
      DO I200 = 1,M
         K3PI = K3 + I200
         K4PI = K4 + I200
         TEMP = A(K3PI)
         A(K3PI) = A(K4PI)
         A(K4PI) = (X(I200)-A(JP1))*A(K3PI) - A(K1PJ)*TEMP
         W11 = W11 + W(I200)*A(K4PI)**2
      enddo
!
! GET NEW ORTHOGONAL POLYNOMIAL COEFFICIENT USING PARTIAL DOUBLE
! PRECISION
!
      TEMD1 = 0.0D0
      DO I300 = 1,M
         K4PI = K4 + I300
         K5PI = K5 + I300
         TEMD2=DBLE(W(I300))*DBLE((Y(I300)-R(I300))-A(K5PI))*DBLE(A(K4PI))
         TEMD1=TEMD1 + TEMD2
      enddo
      TEMD1 = TEMD1/DBLE(W11)
      A(K2PJ+1) = TEMD1
!
! UPDATE POLYNOMIAL EVALUATIONS AT EACH OF THE DATA POINTS, AND
! ACCUMULATE SUM OF SQUARES OF ERRORS.  THE POLYNOMIAL EVALUATIONS ARE
! COMPUTED AND STORED IN EXTENDED PRECISION.  FOR THE I-TH DATA POINT,
! THE MOST SIGNIFICANT BITS ARE STORED IN  R(I) , AND THE LEAST
! SIGNIFICANT BITS ARE IN  A(K5PI) .
!
      SIGJ = 0.0
      DO I400 = 1,M
         K4PI = K4 + I400
         K5PI = K5 + I400
         TEMD2 = DBLE(R(I400)) + DBLE(A(K5PI)) + TEMD1*DBLE(A(K4PI))
         R(I400) = TEMD2
         A(K5PI) = TEMD2 - DBLE(R(I400))
         SIGJ = SIGJ + W(I400)*((Y(I400)-R(I400)) - A(K5PI))**2
      enddo
!
! SEE IF DEGREE SELECTION CRITERION HAS BEEN SATISFIED OR IF DEGREE
! MAXDEG  HAS BEEN REACHED
!
!=======================================================================
      IF (EPS) 23,26,27
!
! COMPUTE F STATISTICS  (INPUT EPS .LT. 0.)
!
23    CONTINUE
      IF (SIGJ .EQ. 0.0) GOTO 29
      DEGF = M - J - 1
      DEN = (CO(4,KSIG)*DEGF + 1.0)*DEGF
      FCRIT = (((CO(3,KSIG)*DEGF) + CO(2,KSIG))*DEGF + CO(1,KSIG))/DEN
      FCRIT = FCRIT*FCRIT
      F = (SIGJM1 - SIGJ)*DEGF/SIGJ
      IF (F .LT. FCRIT) GOTO 25
!
! POLYNOMIAL OF DEGREE J SATISFIES F TEST
!
24    CONTINUE
      SIGPAS = SIGJ
      JPAS = J
      NFAIL = 0
      IF (MAXDEG .EQ. J) GOTO 32
      iii=24
      GOTO 16
!=======================================================================
!
! POLYNOMIAL OF DEGREE J FAILS F TEST.  IF THERE HAVE BEEN THREE
! SUCCESSIVE FAILURES, A STATISTICALLY BEST DEGREE HAS BEEN FOUND.
!
25    CONTINUE
      NFAIL = NFAIL + 1
      IF (NFAIL .GE. 3) GOTO 29
      IF (MAXDEG .EQ. J) GOTO 32
      iii=25
      GOTO 16
!=======================================================================
!
! RAISE THE DEGREE IF DEGREE  MAXDEG  HAS NOT YET BEEN REACHED  (INPUT
! EPS = 0.)
!
26    CONTINUE
      IF (MAXDEG .EQ. J) GOTO 28
      iii=26
      GOTO 16
!=======================================================================
!
! SEE IF RMS ERROR CRITERION IS SATISFIED  (INPUT EPS .GT. 0.)
!
27    CONTINUE
      IF (SIGJ .LE. ETST) GOTO 28
      IF (MAXDEG .EQ. J) GOTO 31
      iii=27
      GOTO 16
!=======================================================================
! RETURNS
28    CONTINUE
      IERR = 1
      NDEG = J
      SIG = SIGJ
      GOTO 777
!=======================================================================
29    CONTINUE
      IERR = 1
      NDEG = JPAS
      SIG = SIGPAS
      GOTO 777
!=======================================================================
888   CONTINUE
      IERR = 2
      CALL JU_XERMSG ('SLATEC', 'JU_POLFIT', 'INVALID INPUT PARAMETER.', &
      &2, 1)
      GOTO 999
!=======================================================================
31    CONTINUE
      IERR = 3
      NDEG = MAXDEG
      SIG = SIGJ
      GOTO 777
!=======================================================================
32    CONTINUE
      IERR = 4
      NDEG = JPAS
      SIG = SIGPAS
      GOTO 777
!=======================================================================
777   CONTINUE
      A(K3) = NDEG
!
! WHEN STATISTICAL TEST HAS BEEN USED, EVALUATE THE BEST POLYNOMIAL AT
! ALL THE DATA POINTS IF  R  DOES NOT ALREADY CONTAIN THESE VALUES
!
      IF(EPS .GE. 0.0  .OR.  NDEG .EQ. MAXDEG)then
         EPS = SQRT(SIG/XM)
         GOTO 999
      endif
      NDER = 0
      DO I500 = 1,M
         CALL JU_PVALUE (NDEG,NDER,X(I500),R(I500),YP,A)
      ENDDO
      EPS = SQRT(SIG/XM)
!=======================================================================
999   CONTINUE
      !----------------------------------------------
      write(*,*)'exiting ju_polfit'
      write(*,*)'ndeg=',ndeg
      write(*,*)'eps(RMS error)=',eps
      do i88=1,ndeg
         write(*,*)i88,r(i88)
      enddo
      write(*,*)'ierr=',ierr
      !----------------------------------------------
   END SUBROUTINE JU_POLFIT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*DECK XERMSG
!
!   XERMSG processes a diagnostic message in a manner determined by the
!   value of LEVEL and the current value of the library error control
!   flag, KONTRL.  See subroutine XSETF for details.
!
!    LIBRAR   A character constant (or character variable) with the name
!             of the library.
!
!    SUBROU   A character constant (or character variable) with the name
!             of the routine that detected the error.  Usually it is the
!             name of the routine that is calling XERMSG.  There are
!             some instances where a user callable library routine calls
!             lower level subsidiary routines where the error is
!             detected.  In such cases it may be more informative to
!             supply the name of the routine the user called rather than
!             the name of the subsidiary routine that detected the
!             error.
!
!    MESSG    A character constant (or character variable) with the text
!             of the error or warning message.  In the example below,
!             the message is a character constant that contains a
!             generic message.
!
!                   CALL JU_XERMSG ('SLATEC', 'MMPY',
!                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
!                  *3, 1)
!
!
!    NERR     An integer value that is chosen by the library routine's
!             author.  It must be in the range -99 to 999 (three
!             printable digits).  Each distinct error should have its
!             own error number.  These error numbers should be described
!             in the machine readable documentation for the routine.
!             The error numbers need be unique only within each routine,
!             so it is reasonable for each routine to start enumerating
!             errors from 1 and proceeding to the next integer.
!
!    LEVEL    An integer value in the range 0 to 2 that indicates the
!             level (severity) of the error.  Their meanings are
!
!            -1  A warning message.  This is used if it is not clear
!                that there really is an error, but the user's attention
!                may be needed.  An attempt is made to only print this
!                message once.
!
!             0  A warning message.  This is used if it is not clear
!                that there really is an error, but the user's attention
!                may be needed.
!
!             1  A recoverable error.  This is used even if the error is
!                so serious that the routine cannot return any useful
!                answer.  If the user has told the error package to
!                return after recoverable errors, then XERMSG will
!                return to the Library routine which can then return to
!                the user's routine.  The user may also permit the error
!                package to terminate the program upon encountering a
!                recoverable error.
!
!             2  A fatal error.  XERMSG will not return to its caller
!                after it receives a fatal error.  This level should
!                hardly ever be used; it is much better to allow the
!                user a chance to recover.  An example of one of the few
!                cases in which it is permissible to declare a level 2
!                error is a reverse communication Library routine that
!                is likely to be called repeatedly until it integrates
!                across some interval.  If there is a serious error in
!                the input such that another step cannot be taken and
!                the Library routine is called again without the input
!                error having been corrected by the caller, the Library
!                routine will probably be called forever with improper
!                input.  In this case, it is reasonable to declare the
!                error to be fatal.
!
SUBROUTINE JU_XERMSG (LIBRAR, SUBROU, MESSG, NERR, LEVEL)
!***PURPOSE  Process error messages for SLATEC and other libraries.
use M_journal, only : journal
CHARACTER(len=*),intent(in)  :: LIBRAR
CHARACTER(len=*),intent(in)  :: SUBROU
CHARACTER(len=*),intent(in)  :: MESSG
integer,intent(in)           :: nerr
integer,intent(in)           :: level
character(len=255)           :: line
character(len=255)           :: lineold=''
   write(line,101)librar,subrou,messg,nerr,level
   101 format('*',a,'*::routine:',a,':',a,' errno:',i3,' level:',i1)
   select case(level)
   case(-1)
      if(line.ne.lineold)then
         call journal(line)
         call journal('warning error')
      endif
   case(0)
      call journal(line)
      call journal('warning error')
   case(1)
      call journal(line)
      call journal('potentially recoverable warning error')
   case(2)
      call journal(line)
      call journal('fatal error')
      !!call abort()
      stop
   case default
      call journal(line)
      call journal('sc','fatal error : unknown code =',level)
      !!call abort()
      stop
   end select
end SUBROUTINE JU_XERMSG
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*DECK JU_PVALUE
   SUBROUTINE JU_PVALUE (L, NDER, X, YFIT, YP, A)
!***BEGIN PROLOGUE  JU_PVALUE
!***PURPOSE  Use the coefficients generated by POLFIT to evaluate the
!            polynomial fit of degree L, along with the first NDER of
!            its derivatives, at a specified point.
!***LIBRARY   SLATEC
!***CATEGORY  K6
!***TYPE      SINGLE PRECISION (JU_PVALUE-S, DP1VLU-D)
!***KEYWORDS  CURVE FITTING, LEAST SQUARES, POLYNOMIAL APPROXIMATION
!***AUTHOR  Shampine, L. F., (SNLA)
!           Davenport, S. M., (SNLA)
!***DESCRIPTION
!
!     Written by L. F. Shampine and S. M. Davenport.
!
!     Abstract
!
!     The subroutine  JU_PVALUE  uses the coefficients generated by  POLFIT
!     to evaluate the polynomial fit of degree  L , along with the first
!     NDER  of its derivatives, at a specified point.  Computationally
!     stable recurrence relations are used to perform this task.
!
!     The parameters for  JU_PVALUE  are
!
!     Input --
!         L -      the degree of polynomial to be evaluated.  L  may be
!                  any non-negative integer which is less than or equal
!                  to  NDEG , the highest degree polynomial provided
!                  by  POLFIT .
!         NDER -   the number of derivatives to be evaluated.  NDER
!                  may be 0 or any positive value.  If NDER is less
!                  than 0, it will be treated as 0.
!         X -      the argument at which the polynomial and its
!                  derivatives are to be evaluated.
!         A -      work and output array containing values from last
!                  call to  POLFIT .
!
!     Output --
!         YFIT -   value of the fitting polynomial of degree  L  at  X
!         YP -     array containing the first through  NDER  derivatives
!                  of the polynomial of degree  L .  YP  must be
!                  dimensioned at least  NDER  in the calling program.
!
!***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston,
!                 Curve fitting by polynomials in one variable, Report
!                 SLA-74-0270, Sandia Laboratories, June 1974.
!***ROUTINES CALLED  XERMSG
!***REVISION HISTORY  (YYMMDD)
!   740601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  JU_PVALUE
      DIMENSION YP(*),A(*)
      CHARACTER*8 XERN1, XERN2
integer      :: i10
integer      :: i20
integer      :: i30
integer      :: i40
integer      :: i50
integer      :: ic
integer      :: ilo
integer      :: in
integer      :: inp1
integer      :: iup
integer      :: k1
integer      :: k1i
integer      :: k2
integer      :: k3
integer      :: k3p1
integer      :: k3pn
integer      :: k4
integer      :: k4p1
integer      :: k4pn
integer      :: kc
integer      :: l
integer      :: lm1
integer      :: lp1
integer      :: maxord
integer      :: nder
integer      :: ndo
integer      :: ndp1
integer      :: nord
real         :: a
real         :: cc
real         :: dif
real         :: val
real         :: x
real         :: yfit
real         :: yp
!***FIRST EXECUTABLE STATEMENT  JU_PVALUE
      IF (L .LT. 0) GOTO 12
      NDO = MAX(NDER,0)
      NDO = MIN(NDO,L)
      MAXORD = A(1) + 0.5
      K1 = MAXORD + 1
      K2 = K1 + MAXORD
      K3 = K2 + MAXORD + 2
      NORD = A(K3) + 0.5
      IF (L .GT. NORD) GOTO 888
      K4 = K3 + L + 1
      IF (NDER .LT. 1) GOTO 2

      DO I10= 1,NDER
         YP(I10) = 0.0
      ENDDO

2     CONTINUE
      IF (L .GE. 2) GOTO 4
      IF (L .EQ. 1) GOTO 3
!
! L IS 0
!
      VAL = A(K2+1)
      GOTO 999
!
! L IS 1
!
3     CONTINUE
      CC = A(K2+2)
      VAL = A(K2+1) + (X-A(2))*CC
      IF (NDER .GE. 1) YP(1) = CC
      GOTO 999
!
! L IS GREATER THAN 1
!
4     CONTINUE
      NDP1 = NDO + 1
      K3P1 = K3 + 1
      K4P1 = K4 + 1
      LP1 = L + 1
      LM1 = L - 1
      ILO = K3 + 3
      IUP = K4 + NDP1

      DO I20 = ILO,IUP
         A(I20) = 0.0
      ENDDO

      DIF = X - A(LP1)
      KC = K2 + LP1
      A(K4P1) = A(KC)
      A(K3P1) = A(KC-1) + DIF*A(K4P1)
      A(K3+2) = A(K4P1)
!
! EVALUATE RECURRENCE RELATIONS FOR FUNCTION VALUE AND DERIVATIVES
!
      DO 30 I30 = 1,LM1
         IN = L - I30
         INP1 = IN + 1
         K1I = K1 + INP1
         IC = K2 + IN
         DIF = X - A(INP1)
         VAL = A(IC) + DIF*A(K3P1) - A(K1I)*A(K4P1)
         IF (NDO .LE. 0) GOTO 8

         DO I50 = 1,NDO
            K3PN = K3P1 + I50
            K4PN = K4P1 + I50
            YP(I50) = DIF*A(K3PN) + I50*A(K3PN-1) - A(K1I)*A(K4PN)
         ENDDO

!
! SAVE VALUES NEEDED FOR NEXT EVALUATION OF RECURRENCE RELATIONS
!
         DO I40 = 1,NDO
            K3PN = K3P1 + I40
            K4PN = K4P1 + I40
            A(K4PN) = A(K3PN)
            A(K3PN) = YP(I40)
         ENDDO

8        CONTINUE
         A(K4P1) = A(K3P1)
         A(K3P1) = VAL
30    CONTINUE
!=======================================================================
!
! NORMAL RETURN OR ABORT DUE TO ERROR
!
999   YFIT = VAL
      RETURN
!=======================================================================
888   CONTINUE
      WRITE (XERN1, '(I8)') L
      WRITE (XERN2, '(I8)') NORD
      CALL JU_XERMSG ('SLATEC', 'JU_PVALUE',                             &
      &   'THE ORDER OF POLYNOMIAL EVALUATION, L = ' // XERN1 //          &
      &   ' REQUESTED EXCEEDS THE HIGHEST ORDER FIT, NORD = ' // XERN2 // &
      &   ', COMPUTED BY POLFIT -- EXECUTION TERMINATED.', 8, 2)
      RETURN
!=======================================================================
!
12    CALL JU_XERMSG ('SLATEC', 'JU_PVALUE',                             &
      &   'INVALID INPUT PARAMETER.  ORDER OF POLYNOMIAL EVALUATION ' //  &
      &   'REQUESTED IS NEGATIVE -- EXECUTION TERMINATED.', 2, 2)
!=======================================================================
   END SUBROUTINE JU_PVALUE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

SUBROUTINE QHFG(X,Y,DERY,Z,NDIM)                                      ! QHFG.2
!                                                                           ! QHFG.3
!     ..................................................................    ! QHFG.4
!                                                                           ! QHFG.5
!        SUBROUTINE QHFG                                                    ! QHFG.6
!                                                                           ! QHFG.7
!        PURPOSE                                                            ! QHFG.8
!@(#)       COMPUTE VECTOR OF INTEGRAL VALUES FOR GIVEN GENERAL TABLE
!           OF ARGUMENT, FUNCTION, AND DERIVATIVE VALUES.
!                                                                           ! QHFG.11
!        USAGE                                                              ! QHFG.12
!           CALL QHFG (X,Y,DERY,Z,NDIM)                                     ! QHFG.13
!                                                                           ! QHFG.14
!        DESCRIPTION OF PARAMETERS                                          ! QHFG.15
!           X      - THE INPUT VECTOR OF ARGUMENT VALUES.                   ! QHFG.16
!           Y      - THE INPUT VECTOR OF FUNCTION VALUES.                   ! QHFG.17
!           DERY   - THE INPUT VECTOR OF DERIVATIVE VALUES.                 ! QHFG.18
!           Z      - THE RESULTING VECTOR OF INTEGRAL VALUES. Z MAY BE      ! QHFG.19
!                    IDENTICAL WITH X,Y OR DERY.                            ! QHFG.20
!           NDIM   - THE DIMENSION OF VECTORS X,Y,DERY,Z.                   ! QHFG.21
!                                                                           ! QHFG.22
!        REMARKS                                                            ! QHFG.23
!           NO ACTION IN CASE NDIM LESS THAN 1.                             ! QHFG.24
!                                                                           ! QHFG.25
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                      ! QHFG.26
!           NONE                                                            ! QHFG.27
!                                                                           ! QHFG.28
!        METHOD                                                             ! QHFG.29
!           BEGINNING WITH Z(1)=0, EVALUATION OF VECTOR Z IS DONE BY        ! QHFG.30
!           MEANS OF HERMITEAN FOURTH ORDER INTEGRATION FORMULA.            ! QHFG.31
!           FOR REFERENCE, SEE                                              ! QHFG.32
!           (1) F.B.HILDEBRAND, INTRODUCTION TO NUMERICAL ANALYSIS,         ! QHFG.33
!               MCGRAW-HILL, NEW YORK/TORONTO/LONDON, 1956, PP.314-319.     ! QHFG.34
!           (2) R.ZURMUEHL, PRAKTISCHE MATHEMATIK FUER INGENIEURE UND       ! QHFG.35
!               PHYSIKER, SPRINGER, BERLIN/GOETTINGEN/HEIDELBERG, 1963,     ! QHFG.36
!               PP.227-230.                                                 ! QHFG.37
!                                                                           ! QHFG.38
!     ..................................................................    ! QHFG.39
!                                                                           ! QHFG.40
!                                                                           ! QHFG.41
!                                                                           ! QHFG.42
integer       :: i
integer       :: ndim
real          :: dery(*)
real          :: sum1
real          :: sum2
real          :: x(*)
real          :: y(*)
real          :: z(*)
!                                                                           ! QHFG.44
   SUM2=0.0                                                                 ! QHFG.45
   if((ndim-1).eq.0)then
      Z(NDIM)=SUM2
   elseif((ndim-1).gt.0)then
      ! INTEGRATION LOOP
      DO I=2,NDIM
         SUM1=SUM2                                                          ! QHFG.50
         SUM2=0.5*(X(I)-X(I-1))                                             ! QHFG.51
         SUM2=SUM1+SUM2*((Y(I)+Y(I-1))+0.3333333*SUM2*(DERY(I-1)-DERY(I)))  ! QHFG.52
         Z(I-1)=SUM1
      enddo
   endif
END SUBROUTINE QHFG
SUBROUTINE QHSG(X,Y,FDY,SDY,Z,NDIM)                                     !   QHSG.2
!                                                                       !   QHSG.3
!     ..................................................................!   QHSG.4
!                                                                       !   QHSG.5
!        SUBROUTINE QHSG                                                !   QHSG.6
!                                                                       !   QHSG.7
!        PURPOSE                                                        !   QHSG.8
!@(#)    COMPUTE VECTOR OF INTEGRAL VALUES FOR GIVEN GENERAL TABLE
!           OF ARGUMENT, FUNCTION, FIRST DERIVATIVE,
!           AND SECOND DERIVATIVE VALUES.                               !   QHSG.11
!                                                                       !   QHSG.12
!        USAGE                                                          !   QHSG.13
!           CALL QHSG (X,Y,FDY,SDY,Z,NDIM)                              !   QHSG.14
!                                                                       !   QHSG.15
!        DESCRIPTION OF PARAMETERS                                      !   QHSG.16
!           X      - THE INPUT VECTOR OF ARGUMENT VALUES.               !   QHSG.17
!           Y      - THE INPUT VECTOR OF FUNCTION VALUES.               !   QHSG.18
!           FDY    - THE INPUT VECTOR OF FIRST DERIVATIVE.              !   QHSG.19
!           SDY    - THE INPUT VECTOR OF SECOND DERIVATIVE.             !   QHSG.20
!           Z      - THE RESULTING VECTOR OF INTEGRAL VALUES. Z MAY BE  !   QHSG.21
!                    IDENTICAL WITH X,Y,FDY OR SDY.                     !   QHSG.22
!           NDIM   - THE DIMENSION OF VECTORS X,Y,FDY,SDY,Z.            !   QHSG.23
!                                                                       !   QHSG.24
!        REMARKS                                                        !   QHSG.25
!           NO ACTION IN CASE NDIM LESS THAN 1.                         !   QHSG.26
!                                                                       !   QHSG.27
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  !   QHSG.28
!           NONE                                                        !   QHSG.29
!                                                                       !   QHSG.30
!        METHOD                                                         !   QHSG.31
!           BEGINNING WITH Z(1)=0, EVALUATION OF VECTOR Z IS DONE BY    !   QHSG.32
!           MEANS OF HERMITEAN SIXTH ORDER INTEGRATION FORMULA.         !   QHSG.33
!           FOR REFERENCE, SEE                                          !   QHSG.34
!           R.ZURMUEHL, PRAKTISCHE MATHEMATIK FUER INGENIEURE UND       !   QHSG.35
!           PHYSIKER, SPRINGER, BERLIN/GOETTINGEN/HEIDELBERG, 1963,     !   QHSG.36
!           PP.227-230.                                                 !   QHSG.37
!                                                                       !   QHSG.38
!     ..................................................................!   QHSG.39
!                                                                       !   QHSG.40
!                                                                       !   QHSG.41
!                                                                       !   QHSG.42
      integer     :: i
      integer     :: ndim
      real        :: fdy(*)
      real        :: sdy(*)
      real        :: sum1
      real        :: sum2
      real        :: x(*)
      real        :: y(*)
      real        :: z(*)
!                                                                       !   QHSG.44
      SUM2=0.0                                                          !   QHSG.45
      IF(NDIM-1)4,3,1                                                   !   QHSG.46
!                                                                       !   QHSG.47
!     INTEGRATION LOOP                                                  !   QHSG.48
1     continue
      DO I=2,NDIM                                                       !   QHSG.49
         SUM1=SUM2                                                      !   QHSG.50
         SUM2=.5*(X(I)-X(I-1))                                          !   QHSG.51
         SUM2=SUM1+SUM2*((Y(I-1)+Y(I))+.4*SUM2*((FDY(I-1)-FDY(I))+    & !   QHSG.52
         &        .1666667*SUM2*(SDY(I-1)+SDY(I))))                         !   QHSG.53
         Z(I-1)=SUM1                                                    !   QHSG.54
      ENDDO
3     Z(NDIM)=SUM2                                                      !   QHSG.55
4     continue
END SUBROUTINE QHSG
SUBROUTINE QTFG(X,Y,Z,NDIM)                                             !   QTFG.2
!                                                                       !   QTFG.3
!     ..................................................................!   QTFG.4
!                                                                       !   QTFG.5
!        SUBROUTINE QTFG                                                !   QTFG.6
!                                                                       !   QTFG.7
!        PURPOSE                                                        !   QTFG.8
!@(#)       COMPUTE VECTOR OF INTEGRAL VALUES FOR GIVEN GENERAL TABLE
!           OF ARGUMENT AND FUNCTION VALUES.
!                                                                       !   QTFG.11
!        USAGE                                                          !   QTFG.12
!           CALL QTFG (X,Y,Z,NDIM)                                      !   QTFG.13
!                                                                       !   QTFG.14
!        DESCRIPTION OF PARAMETERS                                      !   QTFG.15
!           X      - THE INPUT VECTOR OF ARGUMENT VALUES.               !   QTFG.16
!           Y      - THE INPUT VECTOR OF FUNCTION VALUES.               !   QTFG.17
!           Z      - THE RESULTING VECTOR OF INTEGRAL VALUES. Z MAY BE  !   QTFG.18
!                    IDENTICAL WITH X OR Y.                             !   QTFG.19
!           NDIM   - THE DIMENSION OF VECTORS X,Y,Z.                    !   QTFG.20
!                                                                       !   QTFG.21
!        REMARKS                                                        !   QTFG.22
!           NO ACTION IN CASE NDIM LESS THAN 1.                         !   QTFG.23
!                                                                       !   QTFG.24
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  !   QTFG.25
!           NONE                                                        !   QTFG.26
!                                                                       !   QTFG.27
!        METHOD                                                         !   QTFG.28
!           BEGINNING WITH Z(1)=0, EVALUATION OF VECTOR Z IS DONE BY    !   QTFG.29
!           MEANS OF TRAPEZOIDAL RULE (SECOND ORDER FORMULA).           !   QTFG.30
!           FOR REFERENCE, SEE                                          !   QTFG.31
!           F.B.HILDEBRAND, INTRODUCTION TO NUMERICAL ANALYSIS,         !   QTFG.32
!           MCGRAW-HILL, NEW YORK/TORONTO/LONDON, 1956, PP.75.          !   QTFG.33
!                                                                       !   QTFG.34
!     ..................................................................!   QTFG.35
!                                                                       !   QTFG.36
!                                                                       !   QTFG.37
!                                                                       !   QTFG.38
   integer,intent(in)  :: ndim
   real,intent(in)     :: x(ndim),y(ndim)
   real,intent(out)    :: z(ndim)
   integer             :: i
   real                :: sum1, sum2

   if(ndim.le.0)then
      return
   elseif(ndim.eq.1)then
      z(1)=0.0
   else
      ! integration loop
      sum2=0.0
      do i=2,ndim
         sum1=sum2
         sum2=sum2+.5*(x(i)-x(i-1))*(y(i)+y(i-1))
         z(i-1)=sum1
      enddo
      z(ndim)=sum2
   endif
end SUBROUTINE QTFG

!>
!!##NAME
!!    citer(3f) - [M_math:geometry] determine various geometric properties of circle segment
!!            given radius and area of the segment.
!!##SYNOPSIS
!!
!!    Usage:
!!
!!       SUBROUTINE CITER(A,R,H,S,C,DADH)
!!       DOUBLEPRECISION,INTENT(IN)  :: A,R
!!       DOUBLEPRECISION,INTENT(OUT) :: H,S,C,DADH
!!
!!##DESCRIPTION
!!    This subroutine determines various geometric properties of a segment
!!    of a circle given the radius of the circle and area of the segment.
!!
!!    The figure below defines the geometry under consideration. This
!!    figure was taken directly from page 12 of the CRC Standard
!!    Mathematical Tables, 21st Edition, Published by the Chemical Rubber
!!    Company, Cleveland, OH. This page of the CRC Standard Mathematical
!!    Tables covers Mensuration Formulae for Circles.
!!
!!    In the figure below, the arc labeled "S" is the portion of the circle
!!    defined by angle "THETA". "C" is a secant, "H" is the height of the
!!    segment between "S" and "C", and "D" is the shortest distance
!!    from the center of the circle to the secant "C".
!!
!!    Specifically, this subroutine determines H, S, C, and the derivative
!!    of the segment area with respect to H given the radius of the circle,
!!    "R", and the area of the segment between "S" and "C".
!!
!!    Diagram:
!!
!!      >     _________________     S
!!      >        /|\         __---*****---__
!!      >         |       _--               --_
!!      >         H      *                     *
!!      >     ___\|/___ *_______________________*
!!      >        /|\     \          C          /
!!      >         |       \                   /
!!      >         |        \                 /
!!      >         |         \               /
!!      >         |          \             /
!!      >         |           \           /
!!      >         D            \         / R
!!      >         |             \ THETA /
!!      >         |              \     /
!!      >         |               \   /
!!      >         |                \ /
!!      >     ___\|/_______________ *
!!      >
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE CITER(A,R,H,S,C,DADH)
use M_journal, only : journal
character(len=*),parameter::ident_7="&
&@(#)M_math::citer(3f): determine various geometric properties of circle segment given radius and area of the segment."
!
! COPYRIGHT (C) John S. Urban, 08/31/1995
!
!     Input and Output Variable Declarations
!
      DOUBLE PRECISION A,R,H,S,C,DADH
!
!     Internal Variable Declarations
!
      DOUBLEPRECISION AORS
      DOUBLEPRECISION THETAHI,THETALO,THETA
      DOUBLEPRECISION FLO,FHI,F
      DOUBLEPRECISION PI,TOL,X
      DOUBLEPRECISION FUNCT
      INTEGER ICOUNT
      DATA PI/3.14159265358979D0/,TOL/1.D-10/
!
!-----------------------------------------------------------------------
!
!     Statement Function Definition
!
!     The FUNCT statement function returns the scaled area of a segment
!     given the angle defining the segment.  The scaled area equals
!     the segment's area divided by the radius squared; it equals pi
!     when the input angle is 2*pi.
!
      FUNCT(X)=(X-SIN(X))/2.D0
!
!-----------------------------------------------------------------------
!
!     AORS = A Over R Squared
!          = Scaled area of segment
!          = pi when it represents the entire circle.
!
      AORS=A/R**2
!
!     The following IF-THEN-ELSEIF block insures the input area is reasonable.
!
      IF(AORS.LT.TOL) THEN
!
!       The input area is so low that the segment is virtually non-existent.
!
        THETA=0.d0
        H=0.D0
        S=0.D0
        C=0.D0
        DADH=0.D0
        RETURN
      ELSEIF(AORS.GE.PI) THEN
!
!       The input area is so high that the segment is virtually the entire circle
!
        THETA=2.D0*PI
        H=2.D0*R
        S=2.D0*PI*R
        C=0.D0
        DADH=0.D0
        RETURN
      ENDIF
!
!     The following DO-WHILE loop solves for the value of THETA which
!     corresponds to the input segment area.  The loop employs an
!     interval reduction scheme.  THETALO is the lower bound of the
!     answer, and THETAHI is the upper bound of the answer.  FLO is the
!     scaled segment area corresponding to THETALO, and FHI is the
!     scaled segment area corresponding to THETAHI.
!
      THETALO=0.D0
      FLO=0.D0
      THETAHI=2.D0*PI
      FHI=PI
!
!     The very first estimate of THETA has to be done outside of the
!     loop.  The secant method is used to make the first estimate of
!     THETA.
!
      THETA=THETALO+(THETAHI-THETALO)*(AORS-FLO)/(FHI-FLO)
      F=FUNCT(THETA)
      ICOUNT=1
10    continue
      if (abs(f-aors).gt.tol)then
        ICOUNT=ICOUNT+2
        IF(ICOUNT.GT.100) THEN
!
!         The iteration has not converged in 100 steps.  Write a
!         message to the user and abort the run.
!
          CALL journal('*citer* did not convergence in 100 iterations')
          CALL journal('run aborted')
          CALL journal(' ')
          stop 1
        ENDIF
!
!       Replace one of the bounds on THETA with the latest guess.
!
        IF(F.GT.AORS) THEN
          THETAHI=THETA
          FHI=F
        ELSE
          THETALO=THETA
          FLO=F
        ENDIF
!
!       Use the bisection method for the next guess of THETA.
!
        THETA=(THETALO+THETAHI)/2.0D0
        F=FUNCT(THETA)
!
!       Replace one of the bounds on THETA with the latest guess.
!
        IF(F.GT.AORS) THEN
          THETAHI=THETA
          FHI=F
        ELSE
          THETALO=THETA
          FLO=F
        ENDIF
!
!       Use the secant method for the next guess of THETA.
!
        THETA=THETALO+(THETAHI-THETALO)*(AORS-FLO)/(FHI-FLO)
        F=FUNCT(THETA)
        goto 10
      endif
!
!     The iteration on THETA has converged.
!
      H=R*(1.0D0-COS(THETA/2.0D0))
      S=R*THETA
      C=2.D0*R*SIN(THETA/2.D0)
      DADH=2.0D0*SQRT(2.0D0*R*H-H**2)
!
END SUBROUTINE CITER
!>
!!##NAME
!!   envelope(3f) - [M_math:geometry] Find vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
!!##SYNOPSIS
!!
!!    subroutine envelope(x, y, n, vertex, nvert)
!!
!!     integer,intent(in) :: n
!!     real,intent(in)    :: x(n), y(n)
!!     integer :: vertex(n), nvert
!!
!!##DESCRIPTION
!!         Given the points composing a polygon find the points required to draw an envelope of the polygon
!!
!!##OPTIONS
!!         x,y     the vectors describing the polygon
!!         n       the number of elements in the input vectors
!!
!!##RETURNS
!!         vertex  the number of the vertices needed to generate the envelope
!!         nvert   number of vertices returned
!!
!!##EXAMPLE
!!
!!
!!   Draw a polygon and the envelope of the polygon, and find the area of
!!   each polygon. Also place a number of small circles in the plot area colored
!!   according to whether they fall within the border of the original polygon.
!!
!!    program demo_envelope
!!    use M_draw
!!    use M_drawplus, only : page
!!    use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
!!    use M_math,     only : locpt           ! find if a point is inside a polygonal path
!!    use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
!!    implicit none
!!    integer,parameter :: n=6
!!    !  3--------------4
!!    !   \           /
!!    !     \       /
!!    !       \   /
!!    !         X 2,5
!!    !       /  !!    !     /      !!    !   /          !!    !  1--------------6
!!    real,parameter    :: x(n)=[-5.0, 0.0,-5.0, 5.0, 0.0, 5.0]
!!    real,parameter    :: y(n)=[-5.0, 0.0, 5.0, 5.0, 0.0,-5.0]
!!    real              :: xy(2,n)
!!    integer           :: vertex(n)
!!    integer           :: nvert
!!    integer           :: i
!!    integer           :: idum
!!       xy(1,:)=x
!!       xy(2,:)=y
!!       call vinit(' ')
!!       call page(-10.0,10.0,-10.0,10.0)
!!       call color(D_BLACK) ! set current color to black
!!       call clear()        ! clear to current color
!!       call polyfill(.true.)
!!       call color(D_BLUE)  ! we want to draw polygon in this color
!!       call poly2(n,xy)    ! draw filled polygon using points given
!!       idum=getkey()       ! pause for some input
!!       call color(D_CYAN)
!!       call polyhatch(.true.)
!!       call envelope(x, y, n, vertex, nvert)   ! calculate envelope
!!       call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
!!       idum=getkey()       ! pause for some input
!!       call polyhatch(.false.)
!!       call linewidth(50)
!!       call color(D_WHITE)
!!       call poly2(n,xy)    ! draw line along original points
!!       idum=getkey()       ! pause for some input
!!       call random_seed()
!!       do i=1,70
!!          call pickrandom()
!!       enddo
!!       idum=getkey()       ! pause for some input
!!       call vexit()        ! wrap up and exit graphics mode
!!       write(*,*)'polyarea=',polyarea(x,y)
!!       write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
!!    contains
!!    subroutine pickrandom()
!!    ! randomly pick a point in the plot area and color it according to whether it is inside
!!    ! the original polygon
!!    real :: pointx, pointy
!!    integer :: l, m
!!       call random_number(pointx)
!!       call random_number(pointy)
!!       pointx=pointx*20.0-10.0
!!       pointy=pointy*20.0-10.0
!!       call locpt(pointx,pointy,x,y,n,l,m)
!!       select case(l)
!!        case(-1)
!!          call color(D_RED)
!!        case(0)
!!          call color(D_YELLOW)
!!        case(1)
!!          call color(D_GREEN)
!!        case default
!!          write(*,*)'*pickrandom* internal error: L value unknown'
!!          call color(D_WHITE)
!!       end select
!!       call circle(pointx,pointy,0.2)
!!    end subroutine pickrandom
!!    end program demo_envelope
!===================================================================================================================================
!>
!! Programmer: Alan Miller
!! VERSION:    Latest revision - 12 September 1987
!! VERSION:    Fortran 90 version - 8 August 1996
!===================================================================================================================================
SUBROUTINE envelope(x, y, n, vertex, nvert) !-- saved from url=(0048)http://users.bigpond.net.au/amiller/envelope.f90
IMPLICIT NONE

character(len=*),parameter::ident_8="&
&@(#)M_math::envelope(3f):Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n."

INTEGER,INTENT(IN) :: n
REAL,INTENT(IN)    :: x(n), y(n)
INTEGER :: vertex(n), nvert
INTEGER :: iwk(n)                  ! iwk() is an integer work array which must have dimension at least n

!  On output, vertex(i), i=1, ..., nvert contains the numbers of the vertices.


!       Local variables

INTEGER :: next(n), i, i1, i2, j, jp1, jp2, i2save, i3, i2next
REAL    :: xmax, xmin, ymax, ymin, dist, dmax, dmin, x1, y1, dx, dy, x2, y2, &
&  dx1, dx2, dmax1, dmax2, dy1, dy2, temp, zero = 0.0

   IF (n < 2) RETURN

!  Choose the points with smallest & largest x- values as the
!  first two vertices of the polygon.

   IF (x(1) > x(n)) THEN
      vertex(1) = n
      vertex(2) = 1
      xmin = x(n)
      xmax = x(1)
   ELSE
      vertex(1) = 1
      vertex(2) = n
      xmin = x(1)
      xmax = x(n)
   endif

   DO i = 2, n-1
      temp = x(i)
      IF (temp < xmin) THEN
         vertex(1) = i
         xmin = temp
      ELSE IF (temp > xmax) THEN
         vertex(2) = i
         xmax = temp
      endif
   END DO

!       Special case, xmax = xmin.

   IF (xmax == xmin) THEN
      IF (y(1) > y(n)) THEN
         vertex(1) = n
         vertex(2) = 1
         ymin = y(n)
         ymax = y(1)
      ELSE
         vertex(1) = 1
         vertex(2) = n
         ymin = y(1)
         ymax = y(n)
      endif

      DO i = 2, n-1
         temp = y(i)
         IF (temp < ymin) THEN
            vertex(1) = i
            ymin = temp
         ELSE IF (temp > ymax) THEN
            vertex(2) = i
            ymax = temp
         endif
      END DO

      nvert = 2
      IF (ymax == ymin) nvert = 1
      RETURN
   endif

!  Set up two initial lists of points; those points above & those below the
!  line joining the first two vertices.    next(i) will hold the pointer to the
!  point furthest from the line joining vertex(i) to vertex(i+1) on the left
!  hand side.

   i1 = vertex(1)
   i2 = vertex(2)
   iwk(i1) = -1
   iwk(i2) = -1
   dx = xmax - xmin
   y1 = y(i1)
   dy = y(i2) - y1
   dmax = zero
   dmin = zero
   next(1) = -1
   next(2) = -1

   DO i = 1, n
      IF (i == vertex(1) .OR. i == vertex(2)) CYCLE
      dist = (y(i) - y1)*dx - (x(i) - xmin)*dy
      IF (dist > zero) THEN
         iwk(i1) = i
         i1 = i
         IF (dist > dmax) THEN
            next(1) = i
            dmax = dist
         endif
      ELSE IF (dist < zero) THEN
         iwk(i2) = i
         i2 = i
         IF (dist < dmin) THEN
            next(2) = i
            dmin = dist
         endif
      endif
   END DO

!  Ends of lists are indicated by pointers to -ve positions.

   iwk(i1) = -1
   iwk(i2) = -1
   nvert = 2

   j = 1

!  Start of main process.

!  Introduce new vertex between vertices j & j+1, if one has been found.
!  Otherwise increase j.   Exit if no more vertices.

40 continue
   IF (next(j) < 0) THEN
      IF (j == nvert) RETURN
      j = j + 1
      GOTO 40
   endif

   jp1 = j + 1
   DO i = nvert, jp1, -1
      vertex(i+1) = vertex(i)
      next(i+1) = next(i)
   END DO
   jp2 = jp1 + 1
   nvert = nvert + 1
   IF (jp2 > nvert) jp2 = 1
   i1 = vertex(j)
   i2 = next(j)
   i3 = vertex(jp2)
   vertex(jp1) = i2

!  Process the list of points associated with vertex j.   New list at vertex j
!  consists of those points to the left of the line joining it to the new
!  vertex (j+1).   Similarly for the list at the new vertex.
!  Points on or to the right of these lines are dropped.

   x1 = x(i1)
   x2 = x(i2)
   y1 = y(i1)
   y2 = y(i2)
   dx1 = x2 - x1
   dx2 = x(i3) - x2
   dy1 = y2 - y1
   dy2 = y(i3) - y2
   DMAX1 = zero
   dmax2 = zero
   next(j) = -1
   next(jp1) = -1
   i2save = i2
   i2next = iwk(i2)
   i = iwk(i1)
   iwk(i1) = -1
   iwk(i2) = -1

60 continue
   IF (i /= i2save) THEN
      dist = (y(i) - y1)*dx1 - (x(i) - x1)*dy1
      IF (dist > zero) THEN
         iwk(i1) = i
         i1 = i
         IF (dist > DMAX1) THEN
            next(j) = i
            DMAX1 = dist
         endif
      ELSE
         dist = (y(i) - y2)*dx2 - (x(i) - x2)*dy2
         IF (dist > zero) THEN
            iwk(i2) = i
            i2 = i
            IF (dist > dmax2) THEN
               next(jp1) = i
               dmax2 = dist
            endif
         endif
      endif
      i = iwk(i)
   ELSE
      i = i2next
   endif

!  Get next point from old list at vertex j.

   IF (i > 0) GOTO 60

!  End lists with -ve values.

   iwk(i1) = -1
   iwk(i2) = -1

   GOTO 40
END SUBROUTINE envelope
!>
!!##NAME
!!    inpolygon(3f) - [M_math:geometry] determine whether or not an integer point is in an integer polygon
!!
!!##SYNOPSIS
!!
!!     logical function inpolygon(xin, yin, xconv, yconv, nconv)
!!
!!      integer,intent(in)  xin, yin
!!      integer,intent(in)  nconv
!!      integer,intent(in)  xconv(nconv), yconv(nconv)
!!
!!##DESCRIPTION
!!   Given a closed polygon find if a point lies inside the polygon.
!!   Intended for integer values, like pixel images.
!!
!!##OPTIONS
!!    xin     the X coordinate of the point to be checked
!!    yin     the Y coordinate of the point to be checked
!!    xconv   contains the X coords of the polygon
!!    yconv   contains the Y coords of the polygon
!!    nconv   the number of points in the polygon
!!
!!##RESULT
!!    INPOLYGON returns .true if the point
!!    lies inside the polygon, otherwise it returns .false.
!!##EXAMPLE
!!
!!   Sample program
!!
!!   Draw a polygon and an envelope of the polygon and then calculate random
!!   points in the region and determine if they fall inside the polygon,
!!   within the accuracy of integer values.
!!
!!    program demo_inpolygon
!!    use M_draw
!!    use M_drawplus, only : page
!!    use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
!!    use M_math,     only : inpolygon       ! find if a point is inside a polygonal path
!!    use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
!!    implicit none
!!    integer,parameter :: n=6
!!    !  3--------------4
!!    !   \           /
!!    !     \       /
!!    !       \   /
!!    !         X 2,5
!!    !       /  !!    !     /      !!    !   /          !!    !  1--------------6
!!    integer,parameter    :: x(n)=[-5, 0,-5, 5, 0, 5]
!!    integer,parameter    :: y(n)=[-5, 0, 5, 5, 0,-5]
!!    real              :: xy(2,n)
!!    integer           :: vertex(n)
!!    integer           :: nvert
!!    integer           :: i
!!    integer           :: idum
!!    xy(1,:)=x
!!    xy(2,:)=y
!!    call vinit(' ')
!!    call page(-10.0,10.0,-10.0,10.0)
!!    call color(D_BLACK) ! set current color to black
!!    call clear()        ! clear to current color
!!    call polyfill(.true.)
!!    call color(D_BLUE)  ! we want to draw polygon in this color
!!    call poly2(n,xy)    ! draw filled polygon using points given
!!    idum=getkey()       ! pause for some input
!!    call color(D_CYAN)
!!    call polyhatch(.true.)
!!    call envelope(real(x), real(y), n, vertex, nvert)   ! calculate envelope
!!
!!    call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
!!    idum=getkey()       ! pause for some input
!!    call polyhatch(.false.)
!!    call linewidth(50)
!!    call color(D_WHITE)
!!    call poly2(n,xy)    ! draw line along original points
!!    idum=getkey()       ! pause for some input
!!    call random_seed()
!!    do i=1,70
!!       call pickrandom()
!!    enddo
!!    idum=getkey()       ! pause for some input
!!    call vexit()        ! wrap up and exit graphics mode
!!    write(*,*)'polyarea=',polyarea(real(x),real(y))
!!    write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
!!    contains
!!    subroutine pickrandom()
!!       ! randomly pick a point in the plot area and color it according to whether it is inside
!!       ! the original polygon
!!       real :: pointx, pointy
!!       integer :: l, m
!!       call random_number(pointx)
!!       call random_number(pointy)
!!       pointx=int(pointx*20.0-10.0)
!!       pointy=int(pointy*20.0-10.0)
!!       !call locpt(pointx,pointy,x,y,n,l,m)
!!       if(inpolygon(int(pointx),int(pointy),x,y,n))then
!!          call color(D_GREEN)
!!       else
!!          call color(D_RED)
!!       endif
!!       call circle(pointx,pointy,0.2)
!!    end subroutine pickrandom
!!    end program demo_inpolygon
!===================================================================================================================================
LOGICAL FUNCTION INPOLYGON(XIN, YIN, XCONV, YCONV, NCONV)

character(len=*),parameter::ident_9="&
&@(#)M_math::inpolygon(3f):Subroutine to determine whether or not an integer point is in a polygon of integer points"

integer,intent(in)  :: xin,yin                       ! coordinates of the point to be checked
integer,intent(in)  :: nconv                         !
INTEGER             :: XCONV(NCONV), YCONV(NCONV)
REAL                :: X,Y                           ! real copy of input point
integer             :: Z(4)= [-32701,-32701,32701,32701]
integer             :: i, j, m

      X=XIN
      Y=YIN
      IF (Z(1).eq.-32701) then
         DO I = 1,NCONV
            Z(1)=MAX(Z(1),XCONV(I))
            Z(2)=MAX(Z(2),YCONV(I))
            Z(3)=MIN(Z(3),XCONV(I))
            Z(4)=MIN(Z(4),YCONV(I))
         enddo
      endif
      INPOLYGON=.TRUE.
      IF(X .LT. Z(3) .OR. X .GT. Z(1)) INPOLYGON=.FALSE.
      IF(Y .LT. Z(4) .OR. Y .GT. Z(2)) INPOLYGON=.FALSE.
      IF(.NOT. INPOLYGON) RETURN

      J=0
      DO 90 I = 2,NCONV
         M=0
   !-----------------------------------------------------
         select case ((YCONV(I-1)-YIN)*(YIN-YCONV(I)))
         case(:-1); CYCLE
         case(0)  ;
         case(1:) ;
         end select
   !-----------------------------------------------------
         select case (YCONV(I-1)-YCONV(I))
         case(:-1); M=M-1; GOTO 70
         case(0)  ;
         case(1:) ; M=M-2 ;M=M-1;GOTO 70
         end select
   !-----------------------------------------------------
         IF ((XCONV(I-1)-X)*(X-XCONV(I))) 90,100,100
   !-----------------------------------------------------
   70 continue
         M=M+2
         IF ((Y-YCONV(I-1))*(FLOAT(XCONV(I))-XCONV(I-1))/(YCONV(I)-FLOAT(YCONV(I-1)))+XCONV(I-1)-X) 90,100,80
   80 continue
         J=J+M
   90 continue

      INPOLYGON=.FALSE.
      IF(J/4*4 .NE. J) INPOLYGON=.TRUE.
  100 CONTINUE
      END FUNCTION INPOLYGON
!>
!!##NAME
!!   locpt(3f) - [M_math:geometry] find if a point is inside a polygonal path
!!##SYNOPSIS
!!
!!   Usage:
!!
!!    subroutine locpt (x0,y0,x,y,n,l,m)
!!    real, intent(in)     :: x0, y0, x(:), y(:)
!!    integer, intent(in)  :: n
!!    integer, intent(out) :: l, m
!!
!!##DESCRIPTION
!!   Given a polygonal line connecting the vertices (X(I),Y(I)) (I = 1,...,N)
!!   taken in this order. it is assumed that the polygonal path is a loop,
!!   where (X(N),Y(N)) = (X(1),Y(1)) or there is an arc from (X(N),Y(N)) to
!!   (X(1),Y(1)). N.B. The polygon may cross itself any number of times.
!!
!!   (X0,Y0) is an arbitrary point and l and m are variables.
!!   On output, L and M are assigned the following values ...
!!
!!      L = -1   If (X0,Y0) is outside the polygonal path
!!      L =  0   If (X0,Y0) lies on the polygonal path
!!      L =  1   If (X0,Y0) is inside the polygonal path
!!
!!   M = 0 if (X0,Y0) is on or outside the path. If (X0,Y0) is inside the
!!   path then M is the winding number of the path around the point (X0,Y0).
!!
!!    o Fortran 66 version by A.H. Morris
!!    o Converted to ELF90 compatibility by Alan Miller, 15 February 1997
!!      saved from url=(0050)http://users.bigpond.net.au/amiller/NSWC/locpt.f90
!!
!!##EXAMPLE
!!
!!
!!   Draw a polygon and the envelope of the polygon, and find the area of
!!   each polygon. Also place a number of small circles in the plot area colored
!!   according to whether they fall within the border of the original polygon.
!!
!!    program demo_envelope
!!    use M_draw
!!    use M_drawplus, only : page
!!    use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
!!    use M_math,     only : locpt           ! find if a point is inside a polygonal path
!!    use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
!!    implicit none
!!    integer,parameter :: n=6
!!    !  3--------------4
!!    !   \           /
!!    !     \       /
!!    !       \   /
!!    !         X 2,5
!!    !       /  !!    !     /      !!    !   /          !!    !  1--------------6
!!    real,parameter    :: x(n)=[-5.0, 0.0,-5.0, 5.0, 0.0, 5.0]
!!    real,parameter    :: y(n)=[-5.0, 0.0, 5.0, 5.0, 0.0,-5.0]
!!    real              :: xy(2,n)
!!    integer           :: vertex(n)
!!    integer           :: nvert
!!    integer           :: i
!!    integer           :: idum
!!       xy(1,:)=x
!!       xy(2,:)=y
!!       call vinit(' ')
!!       call page(-10.0,10.0,-10.0,10.0)
!!       call color(D_BLACK) ! set current color to black
!!       call clear()        ! clear to current color
!!       call polyfill(.true.)
!!       call color(D_BLUE)  ! we want to draw polygon in this color
!!       call poly2(n,xy)    ! draw filled polygon using points given
!!       idum=getkey()       ! pause for some input
!!       call color(D_CYAN)
!!       call polyhatch(.true.)
!!       call envelope(x, y, n, vertex, nvert)   ! calculate envelope
!!       call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
!!       idum=getkey()       ! pause for some input
!!       call polyhatch(.false.)
!!       call linewidth(50)
!!       call color(D_WHITE)
!!       call poly2(n,xy)    ! draw line along original points
!!       idum=getkey()       ! pause for some input
!!       call random_seed()
!!       do i=1,70
!!          call pickrandom()
!!       enddo
!!       idum=getkey()       ! pause for some input
!!       call vexit()        ! wrap up and exit graphics mode
!!       write(*,*)'polyarea=',polyarea(x,y)
!!       write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
!!    contains
!!    subroutine pickrandom()
!!    ! randomly pick a point in the plot area and color it according to whether it is inside
!!    ! the original polygon
!!    real :: pointx, pointy
!!    integer :: l, m
!!       call random_number(pointx)
!!       call random_number(pointy)
!!       pointx=pointx*20.0-10.0
!!       pointy=pointy*20.0-10.0
!!       call locpt(pointx,pointy,x,y,n,l,m)
!!       select case(l)
!!        case(-1)
!!          call color(D_RED)
!!        case(0)
!!          call color(D_YELLOW)
!!        case(1)
!!          call color(D_GREEN)
!!        case default
!!          write(*,*)'*pickrandom* internal error: L value unknown'
!!          call color(D_WHITE)
!!       end select
!!       call circle(pointx,pointy,0.2)
!!    end subroutine pickrandom
!!    end program demo_envelope
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE locpt (x0, y0, x, y, n, l, m)
IMPLICIT NONE
character(len=*),parameter::ident_10="@(#)M_math::locpt(3f): find if a point is inside a polygonal path"
!-----------------------------------------------------------------------------------------------------------------------------------
   REAL, INTENT(IN)     :: x0, y0, x(:), y(:)
   INTEGER, INTENT(IN)  :: n
   INTEGER, INTENT(OUT) :: l, m

   !     Local variables
   INTEGER :: i, n0
   REAL    :: angle, eps, pi, pi2, sum, theta, theta1, thetai, tol, u, v
!-----------------------------------------------------------------------------------------------------------------------------------
   eps = EPSILON(1.0)   ! EPS is a machine dependent constant. EPS is the smallest number such that 1.0 + EPS > 1.0
   n0 = n
   IF (x(1) == x(n) .AND. y(1) == y(n))then
      n0 = n - 1
   endif
   pi = ATAN2(0.0, -1.0)
   pi2 = 2.0*pi
   tol = 4.0*eps*pi
   l = -1
   m = 0

   u = x(1) - x0
   v = y(1) - y0
   IF (u == 0.0 .AND. v == 0.0)then
      GOTO 20
   endif
   IF (n0 < 2)then
      RETURN
   endif
   theta1 = ATAN2(v, u)

   sum = 0.0
   theta = theta1
   DO i = 2, n0
      u = x(i) - x0
      v = y(i) - y0
      IF (u == 0.0 .AND. v == 0.0) then
         GOTO 20
      endif
      thetai = ATAN2(v, u)

      angle = ABS(thetai - theta)
      IF (ABS(angle - pi) < tol)then
         GOTO 20
      endif
      IF (angle > pi)then
         angle = angle - pi2
      endif
      IF (theta > thetai)then
         angle = -angle
      endif
      sum = sum + angle
      theta = thetai
   ENDDO
   angle = ABS(theta1 - theta)
   IF (ABS(angle - pi) < tol)then
      GOTO 20
   endif
   IF (angle > pi)then
      angle = angle - pi2
   endif
   IF (theta > theta1)then
      angle = -angle
   endif
   sum = sum + angle            ! SUM = 2*PI*M WHERE M IS THE WINDING NUMBER
   m = int(ABS(sum)/pi2 + 0.2)
   IF (m == 0) then
      RETURN
   endif
   l = 1
   IF (sum < 0.0)then
      m = -m
   endif
   RETURN

20 continue                     ! (X0, Y0) IS ON THE BOUNDARY OF THE PATH
   l = 0
END SUBROUTINE locpt
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      poly_intercept(3f) - [M_math:geometry] intersection of a straight line and polygonal path
!!##SYNOPSIS
!!
!!
!!   SUBROUTINE Poly_Intercept(a,b,x,y,n,u,v,m,num,ierr)
!!
!!    REAL, INTENT(IN)      :: a(2)
!!    REAL, INTENT(IN)      :: b(2)
!!    REAL, INTENT(IN)      :: x(:)
!!    REAL, INTENT(IN)      :: y(:)
!!    INTEGER, INTENT(IN)   :: n
!!
!!    REAL, INTENT(OUT)     :: u(:)
!!    REAL, INTENT(OUT)     :: v(:)
!!
!!    INTEGER, INTENT(IN)   :: m
!!
!!    INTEGER, INTENT(OUT)  :: num
!!    INTEGER, INTENT(OUT)  :: ierr
!!
!!##DESCRIPTION
!!    Calculates the points <U(1:num),V(1:num)> at which a line <A,B> crosses a
!!    polygon <X(1:n),Y(1:n)>, provided that number of points found (NUM)
!!    is less than or equal to the storage given the output vector <U,V>.
!!
!!    Based upon routine PFIND from the NSWC Mathematics Library.
!!
!!##OPTIONS
!!    a,b     points ( a(1),a(2) ) and ( b(1),b(2) ) defining a line
!!    x,y     the set of points ( x(i),y(i) ), i=1,2,3,....n  define a polygon
!!    n       the size of the x(:) and y(:) arrays
!!##RETURNS
!!    u,v     the arrays U and V contain the number of points at which the line
!!    m       the size of the U and V arrays
!!    num     number of intersection points found at which the line
!!            crosses the polygon in order (provided that m < size(u))
!!    ierr N  where N is ...
!!            o 0 no error detected
!!            o 1 if a = b
!!            o 2 U and V require more storage, i.e. num > m.
!!            o -i if the ith segment of the polygon is coincident with part of the line.
!!
!!##EXAMPLE
!!
!!##PEDIGREE
!!      o Based upon routine PFIND from the NSWC Mathematics Library.
!!      o Code converted using TO_F90 by Alan Miller, Date: 2000-07-04  Time: 12:24:01
!!      o Update Sun, Mar  5, 2017  8:04:46 AM
!===================================================================================================================================
!>
!! PROCEDURE:    poly_intercept(3f)
!! DESCRIPTION:  intersections of a straight line and polygonal path
!! AUTHOR:       Code converted using TO_F90 by Alan Miller
!! VERSION:      Date: 2000-07-04  Time: 12:24:01
!===================================================================================================================================
SUBROUTINE Poly_Intercept (a, b, x, y, n, u, v, m, num, ierr)
IMPLICIT NONE
character(len=*),parameter::ident_11="@(#)M_math::poly_intercept(3f): Calculates the points at which a line <A,B> crosses a polygon"

REAL, INTENT(IN)      :: a(2)
REAL, INTENT(IN)      :: b(2)
REAL, INTENT(IN)      :: x(:)
REAL, INTENT(IN)      :: y(:)
INTEGER, INTENT(IN)   :: n
REAL, INTENT(OUT)     :: u(:)
REAL, INTENT(OUT)     :: v(:)
INTEGER, INTENT(IN)   :: m
INTEGER, INTENT(OUT)  :: num
INTEGER, INTENT(OUT)  :: ierr

! Local variables

INTEGER  :: i, ind, nm1
REAL     :: d, diff, diff1, eps, h, hi, k, ki, onem, onep, p, q, s, t, tmax, tmin, tol, tol0

eps = EPSILON(1.0)  ! EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST NUMBER SUCH THAT 1.0 + EPS .GT. 1.0 .
num = 0

IF (n < 2) GO TO 200
   h = b(1) - a(1)
   k = b(2) - a(2)

IF (h == 0.0 .AND. k == 0.0) GO TO 200

   ierr = 0
   nm1 = n - 1
   tol = 4.0*eps
   tol0 = 2.0*eps
   onep = 1.0 + tol
   onem = 0.5 + (0.5 - tol0)

ind = 0

DO i = 1, nm1
   hi = x(i + 1) - x(i)
   ki = y(i + 1) - y(i)
   IF (hi == 0.0 .AND. ki == 0.0) CYCLE
   ind = 1

! Check if the line from a to b and the i-th line in the path are parallel

  s = hi*k
  t = h*ki
  d = s - t

  IF (ABS(d) <= tol*MAX(ABS(s), ABS(t))) GO TO 40
!-----------------------------------------------------------------------
!                   THE LINES ARE NOT PARALLEL
!-----------------------------------------------------------------------
  p = x(i) - a(1)
  q = y(i) - a(2)
  s = hi*q
  t = ki*p
  diff = s - t
  IF (ABS(diff) <= tol*MAX(ABS(s),ABS(t))) diff = 0.0
  s = h*q
  t = k*p
  diff1 = s - t
  IF (ABS(diff1) <= tol*MAX(ABS(s),ABS(t))) diff1 = 0.0

  s = diff/d
  t = diff1/d

  IF (s < 0.0 .OR. s > onep) CYCLE
  IF (t < 0.0 .OR. t > onep) CYCLE
  IF (num > 0 .AND. t == 0.0) CYCLE
  IF (s > 0.0) GO TO 20

!                   POINT A IS ON THE I-TH LINE

10 continue
  num = num + 1

  IF (num > m) GO TO 210
     u(num) = a(1)
     v(num) = a(2)
     CYCLE

!                   POINT B IS ON THE I-TH LINE

20 continue
   IF (s < onem) GO TO 30
21 continue
   num = num + 1
   IF (num > m) GO TO 210
      u(num) = b(1)
      v(num) = b(2)
   CYCLE

!              THE INTERIOR OF THE LINE FROM A TO B INTERSECTS WITH THE I-TH LINE

30 continue
   num = num + 1
   IF (num > m) GO TO 210
      u(num) = a(1) + s*h
      v(num) = a(2) + s*k
   CYCLE
!-----------------------------------------------------------------------
!                     THE LINES ARE PARALLEL
!-----------------------------------------------------------------------
40 continue
  IF (ABS(hi) > ABS(ki)) GO TO 50

  d = a(2) - y(i)
  IF (ABS(d) <= tol0*MAX(ABS(a(2)),ABS(y(i)))) d = 0.0
  s = d/ki

  p = x(i) + s*hi
  IF (ABS(a(1) - p) > tol*MAX(ABS(a(1)),ABS(p))) CYCLE

  d = b(2) - y(i)
  IF (ABS(d) <= tol0*MAX(ABS(b(2)),ABS(y(i)))) d = 0.0
  t = d/ki
  GO TO 60

50 d = a(1) - x(i)
  IF (ABS(d) <= tol0*MAX(ABS(a(1)),ABS(x(i)))) d = 0.0
  s = d/hi

  p = y(i) + s*ki
  IF (ABS(p - a(2)) > tol*MAX(ABS(p),ABS(a(2)))) CYCLE

  d = b(1) - x(i)
  IF (ABS(d) <= tol0*MAX(ABS(b(1)),ABS(x(i)))) d = 0.0
  t = d/hi

!              THE 2 LINES ARE PORTIONS OF THE SAME STRAIGHT INFINITE LINE

60 continue
  IF (s > 0.0 .AND. s < onem) GO TO 220
  IF (t > 0.0 .AND. t < onem) GO TO 220
  tmin = MIN(s,t)
  tmax = MAX(s,t)
  IF (tmax <= 0.0) GO TO 70
  IF (tmin >= onem) GO TO 80
  GO TO 220

70 continue
  IF (tmax < 0.0) CYCLE
  IF (num > 0) CYCLE
  IF (tmax == s) GO TO 10
  GO TO 21

80 continue
  IF (tmin > 1.0) CYCLE
  IF (tmin == s) GO TO 10
  GO TO 21

ENDDO

   IF (ind == 0) GO TO 200

   IF (num < 2) RETURN
   IF (u(num) == x(1) .AND. v(num) == y(1)) num = num - 1
RETURN

! ERROR RETURN

200 continue
   ierr = 1
   RETURN

210 continue
   ierr = 2
   num = num - 1
RETURN

220 continue
   ierr = -i

END SUBROUTINE Poly_Intercept
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!        polyarea(3f) - [M_math:geometry] compute the area bounded by a simple closed polygonal curve
!!
!!##SYNOPSIS
!!
!!    FUNCTION polyarea(x, y) RESULT(fn_val)
!!
!!       REAL, INTENT(IN)     :: x(:)
!!       REAL, INTENT(IN)     :: y(:)
!!       REAL                 :: fn_val
!!
!!##DESCRIPTION
!!    Given a sequence of points (X(I),Y(I)), polyarea(3f) computes the
!!    area bounded by the closed polygonal curve which passes through the
!!    points in the order that they are indexed. The final point of the
!!    curve is assumed to be the first point given. Therefore, it need
!!    not be listed at the end of X and Y. The polygon should be simple
!!    (e.g. It may not cross over itself).
!!
!!    If the vertices are given in counterclockwise order, the area will
!!    be positive.  If the vertices are given in clockwise order, the area
!!    will be negative.
!!
!!##OPTIONS
!!    x   x coordinates of the points that define the simple polygon
!!    y   y coordinates of the points that define the simple polygon
!!
!!    The X and Y arrays are assumed to be of the same size.
!!
!!##RETURNS
!!    fn_val   the area of the simple polygon
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    !   (0,10) ########### (10,10)
!!    !          ##       #
!!    !          # #     #
!!    !          #  #   #
!!    !          #   # #
!!    !          #    #
!!    !          #   # #
!!    !          #  #   #
!!    !          # #     #
!!    !          ##       #
!!    !     (0,0)########### (10,0)
!!
!!    program demo_polyarea
!!    use M_math, only : polyarea
!!    implicit none
!!    !                          A  B      C    D      E    F
!!    real,allocatable :: x(:)
!!    real,allocatable :: y(:)
!!
!!    x=[ 0.0, 10.0,  0.0, 10.0,  0.0,  0.0]   !! hourglass crosses itself. unexpected value
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0, 10.0]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0, 10.0,  0.0,  0.0, 10.0, 0.0,  0.0] !! crosses itself. maybe not what you expect
!!    y=[10.0, 10.0,  0.0, 10.0,  0.0, 0.0, 10.0]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0,  0.0, 10.0, 10.0,  0.0 ]     ! square
!!    y=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]     ! square
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0 ]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    end program demo_polyarea
!!
!!  polyarea=   0.00000000
!!  polyarea=  -100.000000
!!  polyarea=  -100.000000
!!  polyarea=  -100.000000
!===================================================================================================================================
!>
!! PROCEDURE:    polyarea(3f)
!! DESCRIPTION:  compute the area bounded by a closed polygonal curve
!! AUTHOR:       Code converted using TO_F90 by Alan Miller
!! VERSION:      2000-07-04  Time: 12:24:06
!===================================================================================================================================
function polyarea(x, y) result(fn_val)
implicit none

character(len=*),parameter::ident_12="@(#)M_math::polyarea(3f): compute the area bounded by a closed polygonal curve"

real, intent(in)     :: x(:)
real, intent(in)     :: y(:)
real                 :: fn_val

integer  :: i, n, nm1
real     :: a

n = min(size(x),size(y))
if (x(1) == x(n) .and. y(1) == y(n))then
   n = n - 1
endif

   select case (n)
   case (:2)
      fn_val = 0.0
   case (3)
      fn_val= 0.5*((x(2) - x(1))*(y(3) - y(1)) - (x(3) - x(1))*(y(2) - y(1)))
   case default
     nm1 = n - 1
     a = x(1)*(y(2) - y(n)) + x(n)*(y(1) - y(nm1))

     do  i = 2, nm1
       a = a + x(i)*(y(i+1) - y(i-1))
     end do

     fn_val = 0.5*a
   end select
end function polyarea
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
doubleprecision function polyarea_mid_point(N,P)     !Calculates the area enclosed by the polygon P.

! from rosetta code Sunday, December 30th, 2018 7:07:10 PM UTC-05:00

! Uses the mid-point rule for integration. Consider the line joining (x1,y1) to (x2,y2)
! The area under that line (down to the x-axis) is the y-span midpoint (y1 + y2)/2 times the width (x2 - x1)
! This is the trapezoidal rule for a single interval, and follows from simple geometry.
! Now consider a sequence of such points heading in the +x direction: each successive interval's area is positive.
! Follow with a sequence of points heading in the -x direction, back to the first point: their areas are all negative.
! The resulting sum is the area below the +x sequence and above the -x sequence: the area of the polygon.
! The point sequence can wobble as it wishes and can meet the other side, but it must not cross itself
! as would be done in a figure 8 drawn with a crossover instead of a meeting.
! A clockwise traversal (as for an island) gives a positive area; use anti-clockwise for a lake.
integer, parameter :: dc = kind(0d0)    ! double precision
INTEGER            :: N                 ! The number of points.
COMPLEX(kind=dc)   :: P(N)              ! The points.
COMPLEX(kind=dc)   :: PP,PC             ! Point Previous and Point Current.
COMPLEX(kind=dc)   :: W                 ! Polygon centre. Map coordinates usually have large offsets.
DOUBLEPRECISION    :: A                 ! The area accumulator.
INTEGER            :: I                 ! A stepper.
   IF (N.LT.3) STOP "*polyarea_mid_point* ERROR: at least three points are needed! "        !Good grief.
   W = (P(1) + P(N/3) + P(2*N/3))/3     ! An initial working average.
   W = SUM(P(1:N) - W)/N + W            ! A good working average is the average itself.
   A = 0                                ! The area enclosed by the point sequence.
   PC = P(N) - W                        ! The last point is implicitly joined to the first.
   DO I = 1,N                           ! Step through the positions.
      PP = PC                           ! Previous position.
      PC = P(I) - W                     ! Current position.
      A = (AIMAG(PC) + AIMAG(PP))*(DBLE(PC) - DBLE(PP)) + A  ! Area integral component.
   END DO                               ! On to the next position.
   polyarea_mid_point = A/2             ! Divide by two once.
END FUNCTION polyarea_mid_point         ! The units are those of the points.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!        polyarea_shoelace(3f) - [M_math:geometry] compute area bounded by a simple closed polygon using the shoelace algorithm
!!
!!##SYNOPSIS
!!
!!   function polyarea_shoelace(x, y)
!!
!!    class(*), intent(in) :: x(:)
!!    class(*), intent(in) :: y(:)
!!    doubleprecision      :: polyarea_shoelace
!!
!!##DESCRIPTION
!!    Given a sequence of points (X(I),Y(I)), polyarea_shoelace(3f) computes the
!!    area bounded by the closed polygonal curve which passes through the
!!    points in the order that they are indexed. The final point of the
!!    curve is assumed to be the first point given. Therefore, it need
!!    not be listed at the end of X and Y. The polygon should be simple
!!    (e.g. It may not cross over itself).
!!
!!    If the vertices are given in counterclockwise order, the area will
!!    be positive.  If the vertices are given in clockwise order, the area
!!    will be negative.
!!
!!##OPTIONS
!!    x   x coordinates of the points that define the simple polygon.
!!        May be any standard scalar numeric value type supported by
!!        M_anything::anyscalar_to_double(3f).
!!    y   y coordinates of the points that define the simple polygon.
!!        May be any standard scalar numeric value type supported by
!!        M_anything::anyscalar_to_double(3f).
!!
!!    The X and Y arrays are assumed to be of the same size.
!!
!!##RETURNS
!!    polyarea_shoelace   the area of the simple polygon
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    !   (0,10) ########### (10,10)
!!    !          ##       #
!!    !          # #     #
!!    !          #  #   #
!!    !          #   # #
!!    !          #    #
!!    !          #   # #
!!    !          #  #   #
!!    !          # #     #
!!    !          ##       #
!!    !     (0,0)########### (10,0)
!!
!!    program demo_polyarea_shoelace
!!    use M_math, only : polyarea_shoelace
!!    implicit none
!!    !                          A  B      C    D      E    F
!!    real,allocatable :: x(:)
!!    real,allocatable :: y(:)
!!
!!    x=[ 0.0, 10.0,  0.0, 10.0,  0.0,  0.0]   !! hourglass crosses itself. unexpected value
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0, 10.0]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    x=[ 0.0, 10.0,  0.0,  0.0, 10.0, 0.0,  0.0] !! crosses itself. maybe not what you expect
!!    y=[10.0, 10.0,  0.0, 10.0,  0.0, 0.0, 10.0]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    x=[ 0.0,  0.0, 10.0, 10.0,  0.0 ]     ! square clockwise
!!
!!    y=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    x=[ 0.0, 0.0, 10.0,  10.0,  0.0 ]     ! square counterclockwise
!!    y=[10.0, 0.0,  0.0,  10.0, 10.0 ]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    end program demo_polyarea_shoelace
!!
!!   Results:
!!
!!     polyarea_shoelace=   0.0000000000000000
!!     polyarea_shoelace=  -100.00000000000000
!!     polyarea_shoelace=  -100.00000000000000
!!     polyarea_shoelace=   100.00000000000000
!===================================================================================================================================
doubleprecision function polyarea_shoelace(x,y)
use m_anything, only : anyscalar_to_double

character(len=*),parameter::ident_13="@(#)Area enclosed by simple (non-intersecting) polygon P, by the shoelace method."

class(*),intent(in)          :: x(:), y(:)

doubleprecision,allocatable  :: px(:)    ! The X values for the points.
doubleprecision,allocatable  :: py(:)    ! The Y values for the points.
integer                      :: n        ! The number of points.
integer                      :: ipx,ipy  ! size of PX and PY
doubleprecision              :: area     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! determine number of points
   px=anyscalar_to_double(x) ! allow any scalar numeric type at cost of making copy
   py=anyscalar_to_double(y)
   ipx=ubound(px,dim=1)
   ipy=ubound(py,dim=1)
   if(ipx.ne.ipy)then
      write(*,*)'*polyarea_shoelace* WARNING: input arrays not same size'
   endif
   n=min(ipx,ipy)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   area = sum(px(1:n - 1)*py(2:n)) + px(n)*py(1) - sum(px(2:n)*py(1:n - 1)) - px(1)*py(n)
   polyarea_shoelace = area/2        ! The midpoint formula requires area halving.

END FUNCTION polyarea_shoelace       ! Negative for clockwise, positive for counter-clockwise.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    closest(3f) - [M_math:geometry] find the data point that is closest to the target point
!!##SYNOPSIS
!!
!!   function closest(xtarget,ytarget,x,y) result(location)
!!
!!    real, dimension(:), intent(in)  :: x, y
!!    real                            :: xtarget, ytarget
!!    integer                         :: location
!!
!!##DESCRIPTION
!!    Given a set of X and Y values and a target point, find the index of the closest point to the target
!!    from the points described by the <X,Y> values. The X and Y arrays are assumed to be the same size.
!!##OPTIONS
!!    XTARGET   X coordinate of target point
!!    YTARGET   Y coordinate of target point
!!    X         array of X values that defines a set of points
!!    Y         array of Y values that defines a set of points
!!##RETURNS
!!   Sample program
!!
!!    program demo_closest
!!    implicit none
!!    real,allocatable :: x(:),y(:)
!!    real             :: x1, y1
!!    integer          :: id
!!    x=[ 11.0,  100.0, -22.34, 26.4, -50.66 ]
!!    y=[-21.0,  150.0, -82.00, 40.0, 350.00 ]
!!    x1=30.0
!!    y1=44.0
!!    id=closest(x1,y1,x,y)
!!    write(*,*)'Closest point: ', x(id), y(id), ' at index ',id
!!    end program demo_closest
!!##EXAMPLE
!!
!===================================================================================================================================
function closest(xtarget,ytarget,x,y) result(location)

character(len=*),parameter::ident_14="@(#)find the index of the data point in the <X,Y> arrays that is closest to the target point"

real, dimension(:), intent(in)  :: x, y
real                            :: xtarget, ytarget
integer                         :: location
integer                         :: ind(1)
   if(size(x).eq.0.or.size(y).eq.0)then
      stop '*closest* input array has no values'
   endif
   ! probably creates a scratch array of the size of an input array
   ind = minloc( (x - xtarget) ** 2 + (y -ytarget) ** 2 )
   location=ind(1)
   !! 'Closest point: ', x(location(1)), y(location(1))
end function closest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_closest()
use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('closest',msg='')
   !!call unit_check('closest', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('closest',msg='')
end subroutine test_closest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

!>
!!##NAME
!!      extremum(3f) - [M_math:statistics] Finds the minimum and maximum value in a REAL array.
!!##SYNOPSIS
!!
!!   subroutine extremum(array,small,big)
!!
!!    real,intent(in)    :: array(:)
!!    real,intent(out)   :: small
!!    real,intent(out)   :: big
!!
!!##DESCRIPTION
!!    Finds the minimum and maximum value in a REAL array.
!!
!!##OPTIONS
!!    array  The array to find the extremes of
!!
!!##RETURNS
!!    small  least value found
!!    big    largest value found
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_extremum
!!    use M_math, only : extremum
!!    real,allocatable :: arr(:)
!!    arr=[-10.0,8.8,-5.0,0.0,5.0,10.0,-0.3]
!!    call extremum(arr,small,big)
!!    write(*,*)'ARRAY=',arr
!!    write(*,*)'SMALL=',small
!!    write(*,*)'BIG=',big
!!    end program demo_extremum
!!
!!   Results:
!!
!!     ARRAY= -10.000 8.80 -5.00 0.00 5.00 10.0 -0.300
!!     SMALL= -10.000
!!     BIG= 10.00
!! ================================================================================
!!
!!##SEE ALSO
!!    minval(3f), maxval(3f)
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine extremum(array,small,big)
implicit none
character(len=*),parameter::ident_15="@(#)M_math::extremum(3f):Find the minimum and maximum value in a REAL array"

real,intent(in)            :: array(:)
real,intent(out),optional  :: small
real,intent(out),optional  :: big

integer                    :: i10
integer                    :: n
real                       :: local_small, local_big

n=size(array)

if(n.le.0)then
   local_small=0.0
   local_big=0.0
elseif(n.eq.1)then
   local_small=array(1)
   local_big=array(1)
else
   local_small=array(1)
   local_big=array(1)
   do i10=2,n
      if(local_small.gt.array(i10))then
         local_small=array(i10)
      elseif(local_big.lt.array(i10))then
         local_big=array(i10)
      endif
   enddo
endif

if(present(big))   big=local_big
if(present(small)) small=local_small

end subroutine extremum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      bds(3f) - [M_math:statistics] Basic Statistical Measures
!!##SYNOPSIS
!!
!!
!!    subroutine bds(x,n,stat)
!!
!!     integer,intent(in) :: n
!!     real,intent(in)    :: x(n)
!!     real,intent(out)   :: STAT(13)
!!##DESCRIPTION
!!
!!     Given a vector of real values calculate common basic statistical measures for
!!     the array.
!!
!!##OPTIONS
!!      x      REAL input vector of values to determine statistical properties for
!!      n      size of input vector
!!##RETURNS
!!      stat   array of statistical measurements calculated
!!
!!         1.   mean
!!         2.   second moment about the mean
!!         3.   third moment about the mean
!!         4.   fourth moment about the mean
!!         5.   variance
!!         6.   standard deviation
!!         7.   skewness
!!         8.   kurtosis
!!         9.   sum
!!         10.  largest value
!!         11.  smallest value
!!         12.  location of largest value
!!         13.  location of smallest value
!!
!!##DEFINITIONS
!!      MEAN
!!
!!           A type of average, calculated by dividing the sum of
!!           a set of values by the number of values.
!!
!!              mean = Sum(Xi)/N
!!
!!      MEDIAN
!!
!!           A type of average, found by arranging the values in
!!           order and then selecting the one in the middle. If the
!!           total number of values in the sample is even, then the
!!           median is the mean of the two middle numbers.
!!
!!      MODE
!!
!!           The most frequent value in a group of values.
!!
!!      VARIANCE
!!
!!           The average of the square of the distance of each
!!           data point from the mean
!!
!!              variance = Sum((Xi-mean)^2))/N
!!
!!           for a population, or more commonly, for a sample the
!!           unbiased value is
!!
!!              variance = Sum((Xi-mean)^2))/(N-1)
!!
!!      STANDARD DEVIATION
!!
!!           The standard deviation is the square root of the
!!           variance.
!!
!!              sd = sqrt(variance)
!!
!!           It is the most commonly used measure of spread.
!!
!!      SKEWNESS
!!
!!           Skewness is a measure of symmetry, or more
!!           precisely, the lack of symmetry. A distribution, or
!!           data set, is symmetric if it looks the same to the left
!!           and right of the center point.
!!
!!              skewness = Sum{(X(i)-mean)^3} /((N-1)*SD^3)
!!
!!           Where SD is the standard deviation, and N is the number of
!!           samples. Some sources will use N instead of N-1 or they might
!!           present the formula in a slightly different mathematically
!!           equivalent format.
!!
!!           The skewness of symmetric data is zero
!!
!!      KURTOSIS
!!
!!           Kurtosis is a measure of whether the data are peaked
!!           or flat relative to a normal distribution. That is,
!!           data sets with high kurtosis tend to have a distinct
!!           peak near the mean, decline rather rapidly, and have
!!           heavy tails. Data sets with low kurtosis tend to have a
!!           flat top near the mean rather than a sharp peak. A
!!           uniform distribution would be the extreme case.
!!
!!              kurtosis = ( SUM{(X(i)-mean)^4} ) / ((N-1)*SD^4) -3
!!
!!           The standard normal distribution has a kurtosis of
!!           zero. Positive kurtosis indicates a "peaked"
!!           distribution and negative kurtosis indicates a "flat"
!!           distribution.
!!
!!           Although often called kurtosis, historically the above expression is
!!           for "excess kurtosis" because three is subtracted from the value.
!!           The purpose of this is to give the normal distribution a kurtosis
!!           of 0. In recent times, the term "excess kurtosis" is often simply
!!           called "kurtosis", so consider that whether to subtract 3 or not
!!           is merely a convention, not a right or wrong answer. When using a
!!           particular program, you just need to be aware of which convention.
!!
!!           Again, another frequent difference is whether they use N in
!!           the denominator or the bias corrected N-1.
!!
!!           The formulas for skewness and kurtosis are treated in
!!
!!              Sokal, R. R., &amp; Rohlf, F. J. (1995).
!!              Biometry: The principles and practice of statistics in biological
!!              research. (3rd ed.) New York: W. H. Freeman, pp. 114-115.
!!
!!           Similar formulas are given in
!!
!!              Zar, J. H., Biostatistical analysis (3rd ed)., Prentice
!!              Hall, 1996.
!!
!!           which refers to "machine formulas" and cites
!!
!!              Bennett and Franklin, Statistical analysis in chemistry and
!!              the chemical industry. NY: Wiley, 1954, at p. 81.
!!##EXAMPLES
!!
!===================================================================================================================================
SUBROUTINE BDS (X,N,STAT)
character(len=*),parameter::ident_16="&
&@(#)M_math::bds(3f): Basic Descriptive Statistics (based on a routine from the IBM collection)"
!  RETURN WITH M,U2,U3,U4,V,S,G1,G2,BIG,SMALL,IB,IS IN THAT ORDER IN LOCATIONS STAT(1) THROUGH STAT(13)
!-----------------------------------------------------------------------
!  nobody likes equivalences any more
   integer,parameter :: MEAN   =1  ! mean
   integer,parameter :: U2     =2  ! second moment about the mean
   integer,parameter :: U3     =3  ! third  moment about the mean
   integer,parameter :: U4     =4  ! fourth moment about the mean
   integer,parameter :: varnce =5  ! variance
   integer,parameter :: Sd     =6  ! standard deviation
   integer,parameter :: skew   =7  ! skewness
   integer,parameter :: kurto  =8  ! kurtosis
   integer,parameter :: SUM    =9  ! sum

   integer,parameter :: BIG    =10 ! highest value
   integer,parameter :: SMALL  =11 ! lowest value
   integer,parameter :: IB     =12 ! location of highest value
   integer,parameter :: IS     =13 ! location of lowest value

   integer,intent(in) :: n
   real,intent(in)    :: x(n)
   real,intent(out)   :: STAT(13)
   real               :: deltafixed
   real               :: deltasum
   real               :: fln
   integer            :: i, i20, i30
!-----------------------------------------------------------------------
      FLN=N
!-----------------------------------------------------------------------
!  SUM AND MEAN AND LARGEST AND SMALLEST AND LOCATION OF EXTREMES
      STAT(BIG)=X(1)                   ! biggest value
      STAT(SMALL)=X(1)                 ! smallest value
      STAT(IS)=1                       ! location of smallest
      STAT(IB)=1                       ! location of biggest
      STAT(SUM) =0.0                   ! sum of all values
      DO I=1,N
         STAT(SUM)= STAT(SUM) +X(I)    ! add all values into SUM
         IF(X(I).LT.STAT(SMALL))THEN   ! find smallest value
            STAT(SMALL)=X(I)
            STAT(IS)=I
         ENDIF
         if(X(I).GT.STAT(BIG))THEN     ! find biggest value
            STAT(BIG)=X(I)
            STAT(IB)=I
         ENDIF
      enddo
      STAT(MEAN)= STAT(SUM)/FLN
!-----------------------------------------------------------------------
!  SECOND, THIRD, FOURTH MOMENTS ABOUT THE MEAN
      STAT(u2) = 0.0
      STAT(u3) = 0.0
      STAT(u4) = 0.0
      DO I20=1,N
         deltafixed = (X(I20) - STAT(mean))
         deltasum = deltafixed
         DO I30=2,4
            deltasum = deltasum * deltafixed
            STAT(I30) = STAT(I30) + deltasum
         enddo
      enddo
!-----------------------------------------------------------------------
      STAT(u2) = STAT(u2) / FLN
      STAT(u3) = STAT(u3) / FLN
      STAT(u4) = STAT(u4) / FLN
!-----------------------------------------------------------------------
!  VARIANCE, STANDARD DEVIATION
      STAT(varnce) = FLN * STAT(u2) / (FLN-1.0)
      STAT(sd) = SQRT(STAT(varnce))
!-----------------------------------------------------------------------
!  SKEWNESS, KURTOSIS
      STAT(skew) = STAT(u3) /(STAT(u2) * SQRT(STAT(u2)))
      STAT(kurto) = STAT(u4) / (STAT(u2) * STAT(u2)) - 3.0
!-----------------------------------------------------------------------
END SUBROUTINE BDS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    skekur1(3f) - [M_math:statistics] variant on calculating skewness and kurtosis of an array
!!
!!##SYNOPSIS
!!
!!    SUBROUTINE SKEKUR1(Y,NHI,YSKEW,YKURT,IOPT)
!!
!!     real,intent(in)    ::  y(*)
!!     integer,intent(in) :: nhi
!!     real,intent(out)   :: yskew
!!     real,intent(out)   :: ykurt
!!     integer,intent(in) :: iopt
!!
!!##DESCRIPTION
!!    FOR: Computing SKEWNESS and KURTOSIS for entries 1 through NHI
!!         in vector Y. The values may be centered about either the
!!         MEAN (IOPT <> 0) or about ZERO (IOPT = 0). The traditional
!!         divisor of N (NOT N-1) is used when the MEAN is estimated.
!!
!!    CURRENT VERSION COMPLETED FEBRUARY 28, 1986
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!!
!!##AUTHOR
!!    Written by Charles P. Reeve
!===================================================================================================================================
SUBROUTINE SKEKUR1(Y,NHI,YSKEW,YKURT,IOPT)
character(len=*),parameter::ident_17="@(#)M_math::skekur1(3f): variant on calculating skewness and kurtosis of an array"
REAL,INTENT(IN)    ::  Y(*)
INTEGER,INTENT(IN) :: NHI
REAL,INTENT(OUT)   :: YSKEW
REAL,INTENT(OUT)   :: YKURT
INTEGER,INTENT(IN) :: IOPT
   REAL            :: RN
   REAL            :: S
   INTEGER         :: I
   REAL            :: T2, T3, T4
   REAL            :: D
      RN = REAL(NHI)
      IF (IOPT.EQ.0) THEN
         S = 0.0
      ELSE
         S = 0.0
         DO I = 1, NHI
            S = S+Y(I)
         enddo
         S = S/RN
      ENDIF
      T2 = 0.0
      T3 = 0.0
      T4 = 0.0
      DO I = 1, NHI
         D = Y(I)-S
         T2 = T2+D**2
         T3 = T3+D**3
         T4 = T4+D**4
      enddo
      YSKEW=SQRT(RN)*T3/T2**1.5
      YKURT=RN*T4/T2**2
END SUBROUTINE SKEKUR1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    skekurx(3f) - [M_math:statistics] Compute unbiased estimator of the population SKEWNESS and KURTOSIS
!!##SYNOPSIS
!!
!!    SUBROUTINE SKEKURX(Y,N,YSKEW,YKURT)
!!
!!       integer,intent(in) :: n
!!       real,intent(in)    :: y(*)
!!       real,intent(out)   :: yskew
!!       real,intent(out)   :: ykurt
!!
!!##DESCRIPTION
!!    This routine calculates the unbiased estimator of the population kurtosis
!!    and skewness from a subset of samples
!!
!!
!!       kurt = {n*(n+1)/((n-1)*(n-2)*(n-3))*SUM[((x(i)-xbar)/stddev)**4]} -
!!                 3*(n-1)**2/((n-2)*(n-3))
!!
!!       skew =  ( n / ((n-1)*(n-2)) *SUM{((X(i) - xbar)/stddev)**3}
!!
!!    where xbar and stddev are the sample mean and standard deviation
!!
!!    Note that this is apparently the skewness and kurtosis calculated by the
!!    MicroSoft Excel product. I checked the Excel help and Excel uses the
!!    above formulas. No references are given in the Excel documentation. Note
!!    that this converges on the standard expression for excess kurtosis and
!!    skewness as N becomes large.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!!##AUTHOR
!!      John S. Urban
!!
!===================================================================================================================================
SUBROUTINE SKEKURX(Y,N,YSKEW,YKURT)
character(len=*),parameter::ident_18="@(#)M_math::skekurx(3f): COMPUTE UNBIASED ESTIMATOR OF THE POPULATION SKEWNESS AND KURTOSIS"
      integer,intent(in) :: n
      REAL,intent(in)    :: Y(*)
      REAL,intent(out)   :: YSKEW
      REAL,intent(out)   :: YKURT
      doubleprecision    :: xbar
      doubleprecision    :: rn
      doubleprecision    :: sum
      doubleprecision    :: sum3
      doubleprecision    :: sum4
      doubleprecision    :: stddev
      integer            :: i10,i20,i30
!-----------------------------------------------------------------------
      RN = N
!-----------------------------------------------------------------------
      ! GET AVERAGE
      XBAR = 0.0d0
      DO I10 = 1, N
         XBAR = XBAR+Y(I10)
      enddo
      XBAR = XBAR/RN
!-----------------------------------------------------------------------
      ! GET STANDARD DEVIATION
      SUM=0.0
      DO I30=1,N
         SUM=SUM+(Y(I30)-XBAR)**2
      enddo
      STDDEV=SQRT(SUM/(RN-1.0d0))
!-----------------------------------------------------------------------
      SUM3=0.0
      SUM4=0.0
      DO I20=1,N
         SUM3=SUM3+((Y(I20)-XBAR)/STDDEV)**3
         SUM4=SUM4+((Y(I20)-XBAR)/STDDEV)**4
      enddo
!-----------------------------------------------------------------------
      YSKEW=RN/((RN-1.0d0)*(RN-2.0d0))*SUM3
      YKURT= RN*(RN+1.0d0) / ((RN-1.0d0)*(RN-2.0d0)*(RN-3.0d0)) * SUM4 - 3.0d0*(RN-1.0d0)**2/((RN-2.0d0)*(RN-3.0d0))
!-----------------------------------------------------------------------
END SUBROUTINE SKEKURX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      ncr(3f) - [M_math] Calculate the number of unique combinations of r objects out of n.
!!
!!##SYNOPSIS
!!
!!   subroutine ncr(n,r,ncomb,ier)
!!
!!     integer, intent(in)     :: n
!!     integer, intent(in)     :: r
!!     !!integer, parameter      :: dp = selected_real_kind(12, 60)
!!     integer, parameter      :: dp = kind(0.0d0)
!!     real (dp), intent(out)  :: ncomb
!!     integer, intent(out)    :: ier
!!
!!##DESCRIPTION
!!      Calculate the number of unique combinations of r objects out of n.
!!
!!##OPTIONS
!!      ier  returns error code
!!           * 0  if no error is detected
!!           * 1  if n < 1
!!           * 2  if r < 0
!!           * 3  if r > n
!!           * 4  if nCr > 1.e+308, i.e. if it overflows.  In this case, the
!!                natural log of nCr is returned.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_ncr
!!    use m_math, only : ncr
!!    implicit none
!!    integer, parameter  :: dp = selected_real_kind(12, 60)
!!    integer             :: n, r, ier
!!    real (dp)           :: result
!!    do
!!       write(*, '(a)', advance='no') ' Enter n, r : '
!!       read(*, *) n, r
!!       call ncr(n, r, result, ier)
!!       if (ier /= 0) then
!!          write(*, *) ' Error, IER = ', ier
!!          if (ier == 4) write(*, '(a, f12.5)') ' ln(ncr) = ', result
!!       else
!!          write(*, '(a, g16.8)') ' ncr = ', result
!!       endif
!!    enddo
!!    end program demo_ncr
!!
!!##AUTHOR
!!    Alan Miller
!===================================================================================================================================
SUBROUTINE ncr(n, r, ncomb, ier)
! Programmer: Alan.Miller @ cmis.csiro.au
! Latest revision - 28 July 1988 (Fortran 77 version)
! Code converted using TO_F90 by Alan Miller
! Date: 2000-01-20  Time: 18:08:52

IMPLICIT NONE
!!INTEGER, PARAMETER      :: dp = SELECTED_REAL_KIND(12, 60)
integer, parameter      :: dp = kind(0.0d0)

INTEGER, INTENT(IN)     :: n
INTEGER, INTENT(IN)     :: r
REAL (dp), INTENT(OUT)  :: ncomb
INTEGER, INTENT(OUT)    :: ier
INTEGER :: rr, i, nn
   IF (n < 1) THEN
      ier = 1
   ELSEIF (r < 0) THEN
      ier = 2
   ELSEIF (r > n) THEN
      ier = 3
   ELSE
      ier = 0
   ENDIF
   IF (ier /= 0) RETURN

   IF (r <= n-r) THEN
      rr = r
   ELSE
      rr = n - r
   ENDIF

   IF (rr == 0) THEN
      ncomb = 1.0_dp
      RETURN
   ENDIF

   IF (rr > 25) THEN
      ncomb = lngamma(DBLE(n+1)) - lngamma(DBLE(r+1)) - lngamma(DBLE(n-r+1))
      IF (ncomb > 709._dp) THEN
         ier = 4
      ELSE
         ncomb = EXP(ncomb)
      ENDIF
      RETURN
   ENDIF

   ncomb = n
   i = 1
   nn = n
   DO
      IF (i == rr) RETURN
      nn = nn - 1
      i = i + 1
      ncomb = (ncomb * nn) / REAL(i)
   ENDDO

END SUBROUTINE nCr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
FUNCTION lngamma(z) RESULT(lanczos)

!  Uses Lanczos-type approximation to ln(gamma) for z > 0.
!  Reference:
!       Lanczos, C. 'A precision approximation of the gamma
!               function', J. SIAM Numer. Anal., B, 1, 86-96, 1964.
!  Accuracy: About 14 significant digits except for small regions
!            in the vicinity of 1 and 2.

!  Programmer: Alan Miller
!              1 Creswick Street, Brighton, Vic. 3187, Australia
!  Latest revision - 14 October 1996

   IMPLICIT NONE
   !!INTEGER, PARAMETER    :: dp = SELECTED_REAL_KIND(12, 60)
    integer, parameter      :: dp = kind(0.0d0)
   REAL(dp), INTENT(IN)  :: z
   REAL(dp)              :: lanczos

! Local variables

REAL(dp)  :: a(9) = (/ 0.9999999999995183D0, 676.5203681218835D0, &
   -1259.139216722289D0, 771.3234287757674D0, &
   -176.6150291498386D0, 12.50734324009056D0, &
   -0.1385710331296526D0, 0.9934937113930748D-05, &
   0.1659470187408462D-06 /), zero = 0.D0,   &
   one = 1.d0, lnsqrt2pi = 0.9189385332046727D0, &
   half = 0.5d0, sixpt5 = 6.5d0, seven = 7.d0, tmp
INTEGER          :: j

   IF (z <= zero) THEN
      WRITE(*, *) 'Error: zero or -ve argument for lngamma'
      RETURN
   END IF

   lanczos = zero
   tmp = z + seven
   DO j = 9, 2, -1
      lanczos = lanczos + a(j)/tmp
      tmp = tmp - one
   END DO
   lanczos = lanczos + a(1)
   lanczos = LOG(lanczos) + lnsqrt2pi - (z + sixpt5) + (z - half)*LOG(z + sixpt5)
END FUNCTION lngamma
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    stddev(3f) - [M_math:statistics] given a real vector and the vector average calculate the standard deviation
!!
!!##SYNTAX
!!    function stddev(vector,n,avg)
!!
!!    integer,intent(in) :: n
!!    real,intent(in)    :: vector(n)
!!    real,intent(in)    :: avg
!!    real               :: stddev
!!
!!##DESCRIPTION
!!    Clearly the average gives one number around which the n observations
!!    tend to cluster. And the standard deviation gives a measure of how the
!!    n observations vary or spread about this average. The square of the
!!    standard deviation is called the variance. If we consider a unit mass
!!    at each point x(i) , then the variance is equivalent to a moment of
!!    inertia about an axis through x(avg). It is readily seen that for a
!!    fixed value of x(avg), greater spreads from the average will produce
!!    larger values of the standard deviation s. The average and the standard
!!    deviation can be used jointly to summarize where the observations are
!!    concentrated. Tchebysheff's theorem states :
!!
!!     A fraction of at least 1 - (1/k**2) of the observations lie
!!     within k standard deviations of the average. The theorem
!!     guarantees lower bounds on the percentage of observations
!!     within k standard deviations of the average.
!!
!!##OPTIONS
!!    n            the size of the input vector
!!    vector(n)    the input vector
!!    avg          the average of the input vector
!!
!!##RETURNS
!!    stddev       the standard deviation of the vector
!!
!!##EXAMPLE
!!
!!   example:
!!
!!     program demo_stddev
!!     use M_math, only : stddev
!!     implicit none
!!     integer :: i
!!     real,parameter :: vals(*)=[(i*1.0,i=0,100)]
!!        !!write(*,*)vals
!!        write(*,*)size(vals)
!!        write(*,*)sum(vals)/size(vals)
!!        write(*,*)stddev(vals,size(vals),sum(vals)/size(vals))
!!     end program demo_stddev
!!
!!   output:
!!
!!          101
!!    50.0000000
!!    29.3001709
!!
!!##AUTHOR
!!    1994 John S. Urban
!!
!!##REFERENCE
!!    From Mark's Handbook, page 17-19, 8th edition
!===================================================================================================================================
function stddev(vector,n,avg)
implicit none
character(len=*),parameter::ident_19="@(#)M_math::stddev(3f): find standard deviation of a real array"
integer,intent(in) :: n            ! number of elements in input array (vector)
real,intent(in)    :: vector(n)    ! input vector
real,intent(in)    :: avg          ! average of array
real               :: stddev

   integer         :: i10
   real            :: sum

   sum=0.0
   do i10=1,n
      sum=sum+(vector(i10)-avg)**2
   enddo
   stddev=sqrt(sum/(n-1))
end function stddev

!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    almost(3f) - [M_math] return true or false if two numbers agree up to specified number of digits
!!##SYNOPSIS
!!
!!    function almost(x,y,digits)
!!    real,intent(in) :: x,y
!!    real,intent(in) :: rdigits
!!    logical,intent(in),optional :: verbose
!!    logical                     :: almost
!!
!!##DESCRIPTION
!!    Returns true or false depending on whether the two numbers given agree to within the specified
!!    number of digits as calculated by ACCDIG(3f).
!!##OPTIONS
!!    x,y      expected and calculated values to be compared
!!    rdigits  real number representing number of digits of precision to compare
!!    verbose  optional value that specifies to print the results of the comparison if TRUE.
!!##RETURNS
!!    almost   TRUE if the input values compare up to the specified number of values
!!##EXAMPLE
!!
!!   sample:
!!
!!    program demo_almost
!!    use M_math, only : almost
!!    real    :: x, y
!!    logical :: z
!!    x=1.2345678
!!    y=1.2300000
!!    do i=1,8
!!       z=almost(x,y,real(i),verbose=.true.)
!!       write(*,*)i,z
!!    enddo
!!    end program demo_almost
!!
!!   output:
!!
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 1.0
!!            1   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 2.0
!!            2   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 3.0
!!            3   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 4.0
!!            4   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 5.0
!!            5   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 6.0
!!            6   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 7.0
!!            7   F
!!     *accdig* significant digit request too high= 8.00000000
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 8.0
!!            8   F
!===================================================================================================================================
function almost(x,y,digits,verbose)
character(len=*),parameter::ident_20="&
&@(#)M_math::almost(3f): function to compare two real numbers only up to a specified number of digits by calling ACCDIG(3f)"
real,intent(in)             :: x,y
real,intent(in)             :: digits
logical,intent(in),optional :: verbose
logical                     :: almost

   logical                  :: verbose_local
   real                     :: acurcy
   integer                  :: ind

   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif

   call accdig(x,y,digits,acurcy,ind)

   if(verbose_local)then
      write(*,*)'*almost* for values ',x,y,' agreement of ',acurcy,' digits out of requested ',digits
   endif

   if(ind.eq.0)then
      almost=.true.
   else
      almost=.false.
   endif

end function almost
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      accdig(3f) - [M_math] compare two real numbers only up to a specified number of digits
!!
!!##SYNOPSIS
!!
!!       subroutine accdig(x,y,rdgits,acurcy,ind)
!!
!!        real,intent(in)     :: X
!!        real,intent(in)     :: Y
!!        real,intent(in)     :: DIGI0
!!        real,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    the values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_accdig ! fortran 90 example
!!    use M_math, only : accdig
!!    implicit none
!!    integer :: digi
!!    integer :: i10, i20, i30
!!    integer :: ind, ind1, ind2
!!    real    :: acurcy, acurcy1, acurcy2
!!    real    :: a, b
!!    real    :: vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10**i10)
!!          call accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10**i20)
!!          call accdig(a,b,real(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(),log10(), abs(1)
!!
!!##FILES
!!      o libGPF.a
!!##LEGAL RESTRICTIONS
!!      none
!!##QA
!!    o Authors: David Hogben, John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE accdig(X,Y,digi0,ACURCY,IND)
use M_journal, only : journal
implicit none
character(len=*),parameter::ident_21="@(#)M_math::accdig(3f): compare two real numbers only up to a specified number of digits"
!     INPUT ...
      real,intent(in) :: x           ! First  of two real numbers to be compared.
      real,intent(in) :: y           ! Second of two real numbers to be compared.
      real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
      integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
                                     ! = 1, If tolerance is not satisfied.
      real,intent(out) :: acurcy     ! = - LOG10 (ABS((X-Y)/Y)))

      real     ::  diff
      real     ::  digi
      integer  ::  ireal_significant_digits
!     ==================================================================
      ireal_significant_digits=int(log10(2.**digits(0.0))) ! maximum number of significant digits in a real number.
      digi=digi0
      if(digi.le.0)then
         call journal('sc','*accdig* bad number of significant digits=',digi)
         digi=ireal_significant_digits
      else if(digi .gt. ireal_significant_digits)then
         call journal('sc','*accdig* significant digit request too high=',digi)
         digi=min(digi,real(ireal_significant_digits))
      endif
!     ..................................................................
      diff = x - y
      if (diff .eq. 0.0) then
         acurcy = digi
      else if (y .eq. 0.0) then
         acurcy = - log10 (abs (x))
      else
         acurcy = - log10 ( abs(diff) ) + log10 ( abs(y) )
      endif
!     ..................................................................
      if (acurcy .lt. digi ) then
         ind = 1
      else
         ind = 0
      endif
!     ..................................................................
END SUBROUTINE accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      dp_accdig(3f) - [M_math] compare two DOUBLEPRECISION numbers only up to a specified number of digits
!!
!!##SYNOPSIS
!!
!!       subroutine dp_accdig(x,y,rdgits,acurcy,ind)
!!
!!        doubleprecision,intent(in)  :: X
!!        doubleprecision,intent(in)  :: Y
!!        real,intent(in)             :: DIGI0
!!        doubleprecision,intent(out) :: acurcy
!!        integer,intent(out)         :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call dp_accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    the values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_dp_accdig ! fortran 90 example
!!    use M_math, only : dp_accdig
!!    implicit none
!!    integer         :: digi
!!    doubleprecision :: a, b
!!    integer         :: i10, i20, i30
!!    integer         :: ind, ind1, ind2
!!    doubleprecision :: acurcy, acurcy1, acurcy2
!!    doubleprecision :: vals(9)
!!    data vals/ &
!!      &1.234680d0,   1.2345378d0,  2.2234568d0, 1.2345678d0, &
!!      &1.2345679d0, -1.2345678d0, 76.234567d0,  2.4691356d0, &
!!      &0.0d0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0d0
!!          b=a+1.0d0/(10**i10)
!!          call dp_accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0d0
!!          b=a+1.0d0/(10**i20)
!!          call dp_accdig(a,b,dble(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call dp_accdig(1.2345678d0,vals(i30),8.0,acurcy1,ind1)
!!          call dp_accdig(vals(i30),1.2345678d0,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_dp_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(),log10(), abs(1)
!!
!!##FILES
!!      o libGPF.a
!!##LEGAL RESTRICTIONS
!!      none
!!##QA
!!    o Authors: David Hogben, John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dp_accdig (x,y,digi0,ACURCY,IND)
use M_journal, only  : journal
use M_anything, only : anyscalar_to_double
implicit none

character(len=*),parameter::ident_22="@(#)M_math::dp_accdig(3f): compare two double values only up to a specified number of digits"

!  INPUT ...
   class(*),intent(in)         :: x           ! FIRST  OF TWO DOUBLE NUMBERS TO BE COMPARED.
   class(*),intent(in)         :: y           ! SECOND OF TWO DOUBLE NUMBERS TO BE COMPARED.
   class(*),intent(in)         :: digi0       ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.

   doubleprecision             :: x_local
   doubleprecision             :: y_local

!  OUTPUT ...
   integer,intent(out)         :: ind         ! = 0, IF TOLERANCE IS     SATISFIED.
                                              ! = 1, IF TOLERANCE IS NOT SATISFIED.
   doubleprecision,intent(out) :: acurcy      ! = - LOG10 (ABS((x_local-y_local)/y_local)))
   doubleprecision             ::  diff
   doubleprecision             ::  digi
   integer                     ::  idble_significant_digits
!  ==================================================================
   x_local=anyscalar_to_double(x)
   y_local=anyscalar_to_double(y)
   digi=anyscalar_to_double(digi0)
!  ==================================================================
   idble_significant_digits=int(log10(2.0**digits(0.0d0))) ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A DOUBLE NUMBER.
   if(digi.le.0)then
      call journal('sc','*dp_accdig* bad number of significant digits=',dble(digi))
      digi=idble_significant_digits
   else if(digi .gt. idble_significant_digits)then
      call journal('sc','*dp_accdig* significant digit request too high=',dble(digi))
      digi=min(digi,dble(idble_significant_digits))
   endif
   diff = x_local - y_local
   if (diff .eq. 0.0) then
      acurcy = digi
   else if (y_local .eq. 0.0) then
      acurcy = - log10 (abs (x_local))
   else
      acurcy = - log10 ( abs(diff) ) + log10 ( abs(y_local) )
   endif
   if (acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
end subroutine dp_accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!   in_margin(3f) - [M_math] check if two reals are approximately equal using a relative margin
!!
!!##SYNOPSIS
!!
!!     elemental pure function in_margin( expected_value, measured_value, allowed_margin )
!!
!!      real, intent(in)    :: expected_value
!!      real, intent(in)    :: measured_value
!!      real, intent(in)    :: allowed_margin
!!      class(*),intent(in) :: invalue
!!
!!##DESCRIPTION
!!   Compare two values to see if they are relatively equal using the
!!   specified allowed margin. That is, see if VALUE_MEASURED is in
!!   the range VALUE_EXPECTED +- ALLOWED_ERROR where the allowed error
!!   varies with the magnitude of the values, such that the allowed error
!!   is margin * average magnitude of measured and expected).
!!
!!   So the allowed error is smaller when the magnitudes are smaller.
!!
!!##OPTIONS
!!   expected_value   First value
!!   measured_value   Second value
!!   allowed_margin   Allowed relative margin
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_in_margin
!!    use :: M_math, only : in_margin
!!    implicit none
!!    write(*,*) in_margin(4.00000,3.99999,0.000000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.00000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.000001)
!!
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.000001)
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.00001)
!!
!!    write(*,*) in_margin(4.00000,3.99999,0.00001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0001)
!!    write(*,*) in_margin(4.00000,3.99999,0.001)
!!    write(*,*) in_margin(4.00000,3.99999,0.01)
!!
!!    end program demo_in_margin
!!
!!   Results:
!!
!!     F
!!     F
!!     F
!!     F
!!     F F F F F
!!     F F F F T
!!     T
!!     T
!!     T
!!     T
!===================================================================================================================================
!===================================================================================================================================
elemental pure function in_margin(expected_value, measured_value, allowed_margin)
use :: M_anything, only : anyscalar_to_double
implicit none

character(len=*),parameter::ident_23="@(#)M_math::in_margin(3f): check if two reals are approximately equal using a relative margin"

class(*),intent(in) :: expected_value, measured_value, allowed_margin
logical             :: in_margin

   doubleprecision     :: expected, measured, margin

   expected=anyscalar_to_double(expected_value)
   measured=anyscalar_to_double(measured_value)
   margin=anyscalar_to_double(allowed_margin)

   if ( abs(expected-measured) > 0.50d0 * margin * (abs(expected)+abs(measured)) ) then
      in_margin=.false.  ! values not comparable
   else
      in_margin=.true.   ! values comparable
   endif

end function in_margin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_in_margin
use M_debug, only: unit_check, unit_check_start, unit_check_done, unit_check_good, unit_check_bad
use M_debug, only: unit_check_level
call unit_check_start('in_margin')
call unit_check_done('in_margin')
end subroutine test_in_margin
function round(val,idigits0)
implicit none
character(len=*),parameter :: ident="@(#) M_math::round(3f): round val to specified number of significant digits"
integer,parameter          :: dp=kind(0.0d0)
real(kind=dp),intent(in)   :: val
integer,intent(in)         :: idigits0
   integer                 :: idigits,ipow
   real(kind=dp)           :: aval,rnormal
   real(kind=dp)           :: round
!  this does not work very well because of round-off errors.
!  Make a better one, probably have to use machine-dependent bit shifting
   ! make sure a reasonable number of digits has been requested
   idigits=max(1,idigits0)
   aval=abs(val)
!  select a power that will normalize the number
!  (put it in the range 1 > abs(val) <= 0)
   if(aval.ge.1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal.eq.1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!>
!!##NAME
!!     scale1(3f) - [M_math] find new range xMINP XMAXP divisible into approximately N linear intervals of size DIST
!!
!!##SYNOPSIS
!!
!!   subroutine scale1(xmin, xmax, n, xminp, xmaxp, dist)
!!
!!    real,intent(in)      :: xmin, xmax
!!    integer,intent(in)   :: n
!!    real,intent(out)     :: xminp, xmaxp, dist
!!
!!##DESCRIPTION
!!
!!    Find new range divisible into approximately n linear intervals using
!!    "CACM Algorithm 463 scale1". Typically used to find nice ranges for
!!    axis scales.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_scale1
!!     use M_math, only : scale1
!!     implicit none
!!     real :: start, end
!!     real :: xminp, xmaxp, dist
!!     integer :: intervals
!!     intervals=5
!!     write(*,*)'Enter start and end values'
!!     do
!!       read(*,*,end=999)start,end
!!       call scale1(start,end,intervals,xminp,xmaxp,dist)
!!       write(*,'(a,g0,a,g0,a,i0,a,g0)') &
!!               & 'nice range is ',xminp,' to ',xmaxp,' by ', &
!!               & nint((xmaxp-xminp)/dist),' intervals of ',dist
!!     enddo
!!     999 continue
!!     end program demo_scale1
!!
!!    Example output
!!
!!     printf '3 87 \n 0.1 2.3 \n -20 30|demo_scale1
!!      Enter start and end values
!!     nice range is 0.00000000 to 100.000000 by 5 intervals of 20.0000000
!!     nice range is 0.00000000 to 2.50000000 by 5 intervals of 0.500000000
!!     nice range is -20.0000000 to 30.0000000 by 5 intervals of 10.0000000
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine scale1(xmin0, xmax0, n0, xminp, xmaxp, dist)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
implicit none
character(len=*),parameter::ident_24="&
&@(#)M_math::scale1(3f):given xmin,xmax,n, find new range xminp xmaxp divisible into approximately n linear intervals of size dist"
!-----------------------------------------------------------------------------------------------------------------------------------
   real,intent(in)      :: xmin0, xmax0
   integer,intent(in)   :: n0
   real,intent(out)     :: xminp, xmaxp, dist
!-----------------------------------------------------------------------------------------------------------------------------------
   integer              :: n
   doubleprecision      :: xmin, xmax, xhold, dist8

   ! vint is an array of acceptable values for dist (times an integer power of 10)
   doubleprecision,parameter :: vint(4)= [1.0d0, 2.0d0, 5.0d0, 10.0d0]

   ! sqr is used as break points to determine which vint value to use
   ! (sqr is an array of geometric means of adjacent values of vint).
   doubleprecision,parameter :: sqr(3)=  [sqrt(2.0d0), sqrt(10.0d0), sqrt(50.0d0)]

   doubleprecision      :: fn, a, al, b, fm1
   integer              :: i, nal, m1, ivint
   doubleprecision,parameter :: del = 0.000002d0
!-----------------------------------------------------------------------------------------------------------------------------------
   xmin=dble(xmin0)
   xmax=dble(xmax0)
   n=n0
!-----------------------------------------------------------------------------------------------------------------------------------
!  check whether proper input values were supplied
   if(xmin.gt.xmax)then ! ensure xmin is less than xmax
      call journal('*scale1* max was less than min')
      xhold=xmin
      xmin=xmax
      xmax=xhold
   endif
   if(n.le.0)then
      call journal('*scale1* number of axis divisions <= 0')
      n=5
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  avoid problem of a scale of zero length
   if(xmin.eq.xmax)then
      xmax=xmax+del
      xmin=xmin-del
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   fn=dble(n)
!  find approximate interval size a
   a = (xmax - xmin) / fn
   if (abs(a) .lt. 1.0d-30) then
      xmax = xmax + 1.0d-30
      xmin = xmin - 1.0d-30
      a = (xmax - xmin) / fn
   endif
   al = log10(a) ! from above checks, a is always positive, non-zero
   nal = int(al)
   if (a .lt. 1.0d0)then
      nal = nal - 1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  a is scaled into variable named b between 1 and 10
!  try and use integer multiplication instead of logarithmic operations
   if(nal.ge.0)then
      b= a/10.0d0**nal
   else
      b=a*10.0d0**abs(nal)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  the closest permissible value for b is found
   CLOSEST: block
   do i=1,3
      if( b .le. sqr(i) )then
         ivint=i
         exit CLOSEST
      endif
   enddo
   ivint=4
   endblock CLOSEST
!-----------------------------------------------------------------------------------------------------------------------------------
!  the interval size is computed
   if(nal.gt.0)then
      dist8 = vint(ivint) * 10.0d0**nal
   else
      dist8 = vint(ivint) / 10.0d0**abs(nal)
   endif
   fm1 = xmin / dist8
   m1 = fm1
   if (fm1 .lt. 0.0)then
      m1 = m1 - 1
   endif
   if (abs((m1 + 1.0d0 - fm1)) .lt. del)then
      m1 = m1 + 1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  the new minimum and maximum limits are found
   xminp = dist8 * dble(m1)
   fm1 = xmax / dist8
   m1 = int(fm1 + 1.0d0)
   if(fm1.lt.-1.0d0.or.abs((fm1 + 1.0d0 - dble(m1))).lt.del)then
      m1 = m1 - 1
   endif
   xmaxp = dist8 * dble(m1)
!-----------------------------------------------------------------------------------------------------------------------------------
!  adjust limits to account for round-off if necessary
   if (xminp .gt. xmin)then
      xminp = xmin
   endif
   if (xmaxp .lt. xmax)then
      xmaxp = xmax
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   dist=dist8
end subroutine scale1
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      scale3(3f) - [M_math] find nice log range
!!
!!##SYNOPSIS
!!
!!   subroutine scale3(xmin, xmax, n, xminp, xmaxp, dist)
!!
!!    real,intent(in)      :: xmin, xmax
!!    integer,intent(in)   :: n
!!    real,intent(out)     :: xminp, xmaxp, dist
!!
!!##DESCRIPTION
!!
!!    Find nice logarithmic range using "CACM Algorithm 463 scale3".
!!    Typically used to find nice ranges for axis scales. Given XMIN, XMAX
!!    and N, where N is greater than 1, find new log range. Finds a new
!!    range XMINP and XMAXP divisible into exactly N LOGARITHMIC intervals,
!!    where the ratio of adjacent uniformly spaced scale values
!!    is DIST.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_scale3
!!     use M_math, only : scale3
!!     implicit none
!!     real :: start, end
!!     real :: xminp, xmaxp, dist
!!     integer :: intervals
!!     integer :: ios
!!     ! real :: treme(2)
!!     intervals=5
!!     write(*,*)'Enter start and end values'
!!     do
!!       read(*,*,iostat=ios)start,end
!!       if(ios.ne.0)exit
!!       call scale3(start,end,intervals,xminp,xmaxp,dist)
!!       ! treme(1)=log10(xminp)
!!       ! treme(2)=log10(xmaxp)
!!       ! treme(1)=floor(treme(1))
!!       ! treme(2)=ceiling(treme(2))
!!       ! if(treme(2).eq.treme(1))treme(2)=treme(2)+1
!!       write(*,'(a,g0,a,g0,a,i0,a,g0)') &
!!               & 'nice range is 10**',log10(xminp),' to 10**',log10(xmaxp),' by ', &
!!               & nint((log10(xmaxp)-log10(xminp))/dist),' intervals of ',dist
!!     enddo
!!     end program demo_scale3
!===================================================================================================================================
!===================================================================================================================================
subroutine scale3(xmin0, xmax0, n0 , xminp, xmaxp, dist)
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
implicit none
character(len=*),parameter::ident_25="@(#)M_math::scale3(3f):find nice log range."

real,intent(in)               :: xmin0, xmax0
integer,intent(in)            :: n0
real,intent(out)              :: xminp, xmaxp, dist

   doubleprecision,parameter  :: del = 0.000002d0
   doubleprecision xmin, xmax, hold
   integer n
   doubleprecision xminl, xmaxl, fn, a, al, b, distl, fm1, fm2
   integer     nal, i, m1, m2, np, nx, iv
   doubleprecision,save :: vint(11) = [10.0d0,9.0d0,8.0d0,7.0d0,6.0d0,5.0d0,4.0d0,3.0d0,2.0d0,1.0d0,0.5d0]
!-----------------------------------------------------------------------------------------------------------------------------------
!  Check whether proper input values were supplied.
   xmin=xmin0
   xmax=xmax0
   n=n0
   if(xmin .gt. xmax)then
      hold=xmin
      xmin=xmax
      xmax=hold
      call journal('*scale3* max was less than min')
   endif
  if (n .le. 0)then
     call journal('*scale3* requested number of divisions <= 0')
     n=5
  endif
  if( xmin .le. 0.0 )then
     call journal('*scale3* zero or negative minimum value')
     xminp=xmin
     xmaxp=xmax
     dist=(xmax-xmin)/n
     return
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
! check what happens when xmin0=xmax0
  if (xmin .eq. xmax) then
     xmax=xmax*2
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
! Values are translated from the linear region to the logarithmic
  xminl = log10(xmin)
  xmaxl = log10(xmax)
  fn = dble(n)
!===================================================================================================================================
! Find approximate interval size a
  a = (xmaxl - xminl) / fn
  al = log10(a)
  nal = int(al)
  if (a .lt. 1.0d0)then
     nal = nal - 1
  endif
! a is scaled into the variable named b between 1 and 10
  b = a / 10.00d0**nal
!===================================================================================================================================
! The closest permissible value for b is found
  CLOSEST: block
  do i = 1,9
     iv=i
     if (b .lt. (10.0d0 / vint(i) + del)) exit CLOSEST
  enddo
  iv = 10
  endblock CLOSEST
!===================================================================================================================================
!     The interval size is computed
30    continue
   distl = 10.0d0**(nal + 1) / vint(iv)
   fm1 = xminl / distl
   m1 = int(fm1)
   if (fm1 .lt. 0.0d0) then
      m1 = m1 -1
   endif
   if (abs(dble(m1) + 1.0d0 - fm1) .lt. del)then
      m1 = m1 - 1
   endif
!===================================================================================================================================
!  The new minimum and maximum limits are found
   xminp = distl * dble(m1)
   fm2 = xmaxl / distl
   m2 = int(fm2 + 1.0d0)
   if (fm2 .lt. -1.0d0)then
      m2 = m2 -1
   endif
   if (abs(fm2 + 1.0d0 - dble(m2)) .lt. del)then
      m2 = m2 -1
   endif
   xmaxp = distl * dble(m2)
!===================================================================================================================================
!  Check whether another pass is necessary
   np = m2 - m1
   iv = iv + 1
   if( np .gt. n) goto 30
   nx = (n - np) / 2
   xminp = xminp - nx * distl
   xmaxp = xminp + n * distl
!===================================================================================================================================
!  Values are translated from the logarithmic into the linear region.
   dist = distl
   xminp = 10.0d0**dble(xminp)
   xmaxp = 10.0d0**dble(xmaxp)
!===================================================================================================================================
!  Adjust limits to account for round-off if necessary
   if(xminp .gt. xmin)then
      xminp = xmin
   endif
   if(xmaxp .lt. xmax)then
      xmaxp = xmax
   endif
!===================================================================================================================================
end subroutine scale3
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------

!>
!!##NAME
!!    quadratic(3f) - [M_math] calculate the roots of a quadratic formula even if they are complex
!!
!!##SYNOPSIS
!!
!!   subroutine quadratic(a,b,c,z1,z2,discriminant)
!!
!!    real,intent(in) :: a, b, c
!!    complex,intent(out) :: z1, z2
!!    real,intent(out) :: discriminant
!!
!!##DESCRIPTION
!!    Given the equation
!!
!!       a*x**2+b*x+c=0
!!
!!    Use the quadratic formula to determine the root values and the
!!    discriminant of the equation.
!!
!!##OPTIONS
!!    a,b,c  coefficients
!!
!!##RETURNS
!!    z1,z2  roots
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quadratic
!!    use M_math, only : quadratic
!!    implicit none
!!    ! Calculate and print the roots of a quadratic formula
!!    ! even if they are complex
!!    real    :: a, b, c ! coefficients
!!    complex :: z1, z2  ! roots
!!    real    :: discriminant
!!       a = 4.0
!!       b = 8.0
!!       c = 21.0
!!       call quadratic(a,b,c,z1,z2,discriminant) !  Calculate the roots
!!       if (abs(discriminant) < 0) then
!!          write(*,*) "the roots are real and equal:"
!!       else if (discriminant > 0) then
!!          write(*,*) "the roots are real:"
!!       else
!!          write(*,*) "the roots are complex:"
!!       end if
!!    !  Print the roots
!!       print *, "The roots(ie. x-intercepts)  are:"
!!       print *, "z1 =", z1
!!       print *, "z2 =", z2
!!    end program demo_quadratic
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine quadratic(a,b,c,z1,z2,discriminant)
implicit none
character(len=*),parameter::ident_26="&
&@(#)M_math::quadratic(3f): calculate the roots of a quadratic formula even if they are complex"
real,intent(in)     :: a, b, c         ! coefficients
complex,intent(out) :: z1, z2          ! roots
real,intent(out)    :: discriminant

!  Calculate the roots
if(a.ne.0)then
   z1 = (-b + sqrt (cmplx (b**2 - 4*a*c))) / (2*a)
   z2 = (-b - sqrt (cmplx (b**2 - 4*a*c))) / (2*a)
else
   ! Y=Bx+C
   ! 0=Bx+C
   ! -C=Bx
   ! -C/B
   if(B.ne.0)then
      write(*,*)'*quadratic* WARNING: If A=0 this is a linear, not quadratic, equation'
      z1 = -C/B
      z2 = -C/B
   elseif(C.eq.0)then
      write(*,*)'*quadratic* WARNING: If A,B,C are 0 this is a line on the x axis (with infinte roots), not a quadratic equation'
      z1 = 0.0
      z2 = 0.0
   else
      write(*,*)'*quadratic* WARNING: If A and B=0 this is a horizontal line not on the x axis (no roots), not a quadratic equation'
      z1 = 0.0
      z2 = 0.0
   endif
endif

   discriminant = b*b - 4.0*a*c

end subroutine quadratic

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   magic_square(3f) - [M_math] create an N x N magic square array, N>2
!!##SYNOPSIS
!!
!!   subroutine magic_square(array)
!!
!!    class(*) :: array
!!
!!##DESCRIPTION
!!    This procedure returns  a magic squares array, an n by n matrix in
!!    which each integer 1, 2, ..., n*n appears exactly once and all columns,
!!    rows, and diagonals sum to the same number.
!!
!!##OPTIONS
!!    array  An array to fill with the magic square values. The
!!           smallest dimension should be >= 3. Since a square is required
!!           only the values only array(1:n,1:n) will be filled, where
!!           n=min(rows,columns).
!!
!!           The array may be INTEGER, REAL, or DOUBLEPRECISION.
!!
!!           Note that the routine allocates an integer array of the size
!!           determined by the input routine during execution.
!!
!!##AUTHOR
!!   John S. Urban
!!
!!   Based on an algorithm for magic squares from
!!
!!     Mathematical Recreations and Essays, 12th ed.,
!!     by W. W. Rouse Ball and H. S. M. Coxeter
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_magic_square
!!    use M_math, only : magic_square
!!    implicit none
!!    integer           :: arr(15,15)
!!    integer           :: i, j, k
!!
!!       do k=1,15
!!          write(*,*)'K=',k
!!          call magic_square(arr(:k,:k))
!!          do i=1,k
!!             write(*,'(i2,":",*(i5):)')i,(int(arr(i,j)),j=1,k),sum(arr(k,:k))
!!          enddo
!!       enddo
!!    end program demo_magic_square
!===================================================================================================================================
subroutine magic_square(array)
implicit none
class(*)            :: array(:,:)
integer,allocatable :: iarray(:,:)
integer             :: cols
integer             :: rows
integer             :: n

integer             :: t
integer             :: i,  j,  m
integer             :: i1, j1, m1
integer             :: im, jm, mm
integer             :: k
integer             :: m2
!
   select type(array)
   type is (integer)
      rows=size(array,dim=1)
      cols=size(array,dim=2)
   type is (real)
      rows=size(array,dim=1)
      cols=size(array,dim=2)
   type is (doubleprecision)
      rows=size(array,dim=1)
      cols=size(array,dim=2)
   end select

   n=min(rows,cols)
   allocate(iarray(n,n))
   if (mod(n,4) .eq. 0) then
!
!  double even order
!
      k = 1
      do i = 1, n
         do j = 1, n
            iarray(i,j) = k
            if (mod(i,4)/2 .eq. mod(j,4)/2) iarray(i,j) = n*n+1 - k
            k = k+1
         enddo
      enddo
   else
      if (mod(n,2) .eq. 0) then
         m = n/2
      else
         m = n
      endif
!
!     odd order or upper corner of even order
!
      iarray(:m,:m)=0
      i = 1
      j = (m+1)/2
      mm = m*m
      do k = 1, mm
         iarray(i,j) = k
         i1 = i-1
         j1 = j+1
         if(i1.lt.1) i1 = m
         if(j1.gt.m) j1 = 1
         if(iarray(i1,j1).ne.0) then
            i1 = i+1
            j1 = j
         endif
         i = i1
         j = j1
      enddo
      if (mod(n,2) .eq. 0) then
!
!     rest of even order
!
         t = m*m
         do i = 1, m
            do j = 1, m
               im = i+m
               jm = j+m
               iarray(i,jm) = iarray(i,j) + 2*t
               iarray(im,j) = iarray(i,j) + 3*t
               iarray(im,jm) = iarray(i,j) + t
            enddo
         enddo
         m1 = (m-1)/2
         if (m1.ne.0) then
            do j = 1, m1
               call iswap(m,iarray(1:,j),iarray(m+1:,j))
            enddo
            m1 = (m+1)/2
            m2 = m1 + m
            call iswap(1,iarray(m1:m1,1),iarray(m2:m2,1))
            call iswap(1,iarray(m1:m1,m1),iarray(m2:m2,m1))
            m1 = n+1-(m-3)/2
            if(m1.le.n)then
               do j = m1, n
                  call iswap(m,iarray(1:,j),iarray(m+1:,j))
               enddo
            endif
         endif
      endif
   endif
   select type(array)
   type is (integer)
      array=iarray
   type is (real)
      array=real(iarray)
   type is (doubleprecision)
      array=dble(iarray)
   end select
   deallocate(iarray)
end subroutine magic_square
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine iswap(n,x,y)

character(len=*),parameter::ident_27="@(#)m_matrix::iswap(3f): swap two integer arrays"

integer,intent(in) :: n
integer            :: x(:),y(:)

integer            :: temp
integer            :: i

   if(n.gt.0)then
      do i = 1, n
         temp = x(n)
         x(n) = y(n)
         y(n) = temp
      enddo
   endif
end subroutine iswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_2x2(A) result(B)
character(len=*),parameter::ident_28="@(#)M_math::invert_2x2(3f): performs a direct calculation of the inverse of a 2x2 matrix"
   integer,parameter         :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: A(2,2)   !! Matrix
   real(kind=wp)             :: B(2,2)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function double_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_3x3(A) result(B)
character(len=*),parameter::ident_29="@(#)M_math::invert_3x3(3f): performs a direct calculation of the inverse of a 3x3 matrix"
   integer,parameter         :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: A(3,3)   !! Matrix
   real(kind=wp)             :: B(3,3)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function double_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_4x4(A) result(B)
character(len=*),parameter::ident_30="@(#)M_math::invert_4x4(3f): performs a direct calculation of the inverse of a 4x4 matrix"
   integer,parameter            :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: A(4,4)   !! Matrix
   real(kind=wp)             :: B(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function double_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_2x2(A) result(B)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter         :: wp=kind(0)
   integer(kind=wp), intent(in) :: A(2,2)   !! Matrix
   integer(kind=wp)             :: B(2,2)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function integer_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_3x3(A) result(B)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter         :: wp=kind(0)
   integer(kind=wp), intent(in) :: A(3,3)   !! Matrix
   integer(kind=wp)             :: B(3,3)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function integer_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0)
   integer(kind=wp), intent(in) :: A(4,4)   !! Matrix
   integer(kind=wp)             :: B(4,4)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function integer_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_2x2(A) result(B)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter         :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(2,2)   !! Matrix
   real(kind=wp)             :: B(2,2)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function real_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_3x3(A) result(B)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter         :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(3,3)   !! Matrix
   real(kind=wp)             :: B(3,3)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function real_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(4,4)   !! Matrix
   real(kind=wp)             :: B(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function real_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_2x2(A) result(B)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: A(2,2)   !! Matrix
   complex(kind=wp)             :: B(2,2)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function complex_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_3x3(A) result(B)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: A(3,3)   !! Matrix
   complex(kind=wp)             :: B(3,3)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function complex_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: A(4,4)   !! Matrix
   complex(kind=wp)             :: B(4,4)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function complex_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_math()

!! setup
! GEOMETRY
   call test_citer              ! determine various geometric properties of circle segment given radius and area of the segment.
   call test_envelope           ! Find the vertices (in clockwise order) of a polygon enclosing the points [((x(i), y(i), i=1,n)].
   call test_inpolygon          ! Subroutine to determine whether or not a point is in a polygon
   call test_locpt              ! find if a point is inside a polygonal path
   call test_poly_intercept     ! find points where a line intersects a polygon
   call test_polyarea           ! find area of a polygon
   call test_polyarea_shoelace  ! find area of a polygon using shoelace algorithm
   call test_polyarea_mid_point ! find area of a polygon
   call test_closest            ! find point closest to target
! FIT
   call test_julfit             ! linear least square fit
   call test_julfit1            ! linear least square fit(y=a*x+b)
   call test_lowess             ! data smoothing using locally weighted regression
   call test_splift             ! fits a spline to the n data points given in x and y
   call test_splint             ! interpolates and twice differentiates a cubic spline
   call test_linearint          ! linear interpolation
! FITS
   call test_ju_polfit
   call test_ju_pvalue
   call test_glstsq
!  private gcsgau1
!  private gcsgau2
! INTEGRATE
   call test_qhfg
   call test_qhsg
   call test_qtfg
! STATISTICS
   call test_extremum       ! find the minimum and maximum value in a real array
   call test_bds            ! basic descriptive statistics
   call test_ncr            ! number of combinations of size R from N cases
   call test_skekurx        ! skew and kurtosis variant
   call test_skekur1        ! skew and kurtosis variant
   call test_stddev         ! standard deviation
! COMPARING AND ROUNDING FLOATING POINT VALUES
   call test_accdig         ! compare two real numbers only up to a specified number of digits
   call test_almost         ! function compares two real numbers only up to a specified number of digits
   call test_dp_accdig      ! compare two double numbers only up to a specified number of digits
   call test_in_margin      ! check if two reals are approximately equal using a relative margin
   call test_round          ! round val to specified number of significant digits
   call test_scale1         ! given xmin,xmax,n, find new range xminp xmaxp divisible into ~ n linear intervals of size dist
   call test_scale3         ! find nice log range, typically for an axis
! MATRIX
   call test_invert_2x2     ! directly invert 2x2 matrix
   call test_invert_3x3     ! directly invert 3x3 matrix
   call test_invert_4x4     ! directly invert 4x4 matrix
   call test_magic_square   ! create magic squares
! POLYNOMIAL
   call test_quadratic      ! return roots of quadratic equation even if complex

!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invert_2x2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('invert_2x2',msg='')
   !!call unit_check('invert_2x2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('invert_2x2',msg='')
end subroutine test_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invert_3x3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('invert_3x3',msg='')
   !!call unit_check('invert_3x3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('invert_3x3',msg='')
end subroutine test_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invert_4x4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('invert_4x4',msg='')
   !!call unit_check('invert_4x4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('invert_4x4',msg='')
end subroutine test_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_accdig()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('accdig',msg='')
   !!call unit_check('accdig', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('accdig',msg='')
end subroutine test_accdig
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_almost()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('almost',msg='')
   !!call unit_check('almost', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('almost',msg='')
end subroutine test_almost
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bds()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('bds',msg='')
   !!call unit_check('bds', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('bds',msg='')
end subroutine test_bds
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_citer()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('citer',msg='')
   !!call unit_check('citer', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('citer',msg='')
end subroutine test_citer
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_complex_invert_2x2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('complex_invert_2x2',msg='')
   !!call unit_check('complex_invert_2x2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('complex_invert_2x2',msg='')
end subroutine test_complex_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_complex_invert_3x3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('complex_invert_3x3',msg='')
   !!call unit_check('complex_invert_3x3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('complex_invert_3x3',msg='')
end subroutine test_complex_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_complex_invert_4x4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('complex_invert_4x4',msg='')
   !!call unit_check('complex_invert_4x4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('complex_invert_4x4',msg='')
end subroutine test_complex_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_double_invert_2x2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('double_invert_2x2',msg='')
   !!call unit_check('double_invert_2x2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('double_invert_2x2',msg='')
end subroutine test_double_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_double_invert_3x3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('double_invert_3x3',msg='')
   !!call unit_check('double_invert_3x3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('double_invert_3x3',msg='')
end subroutine test_double_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_double_invert_4x4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('double_invert_4x4',msg='')
   !!call unit_check('double_invert_4x4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('double_invert_4x4',msg='')
end subroutine test_double_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dp_accdig()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('dp_accdig',msg='')
   !!call unit_check('dp_accdig', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('dp_accdig',msg='')
end subroutine test_dp_accdig
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_envelope()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('envelope',msg='')
   !!call unit_check('envelope', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('envelope',msg='')
end subroutine test_envelope
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_extremum()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('extremum',msg='')
   !!call unit_check('extremum', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('extremum',msg='')
end subroutine test_extremum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_glstsq()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('glstsq',msg='')
   !!call unit_check('glstsq', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('glstsq',msg='')
end subroutine test_glstsq
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_in_margin()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('in_margin',msg='')
   !!call unit_check('in_margin', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('in_margin',msg='')
end subroutine test_in_margin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inpolygon()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('inpolygon',msg='')
   !!call unit_check('inpolygon', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('inpolygon',msg='')
end subroutine test_inpolygon
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_integer_invert_2x2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('integer_invert_2x2',msg='')
   !!call unit_check('integer_invert_2x2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('integer_invert_2x2',msg='')
end subroutine test_integer_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_integer_invert_3x3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('integer_invert_3x3',msg='')
   !!call unit_check('integer_invert_3x3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('integer_invert_3x3',msg='')
end subroutine test_integer_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_integer_invert_4x4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('integer_invert_4x4',msg='')
   !!call unit_check('integer_invert_4x4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('integer_invert_4x4',msg='')
end subroutine test_integer_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ju_polfit()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('ju_polfit',msg='')
   !!call unit_check('ju_polfit', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('ju_polfit',msg='')
end subroutine test_ju_polfit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ju_pvalue()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('ju_pvalue',msg='')
   !!call unit_check('ju_pvalue', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('ju_pvalue',msg='')
end subroutine test_ju_pvalue
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julfit()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('julfit',msg='')
   !!call unit_check('julfit', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('julfit',msg='')
end subroutine test_julfit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julfit1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('julfit1',msg='')
   !!call unit_check('julfit1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('julfit1',msg='')
end subroutine test_julfit1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_linearint()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('linearint',msg='')
   !!call unit_check('linearint', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('linearint',msg='')
end subroutine test_linearint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_locpt()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('locpt',msg='')
   !!call unit_check('locpt', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('locpt',msg='')
end subroutine test_locpt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lowess()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('lowess',msg='')
   !!call unit_check('lowess', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('lowess',msg='')
end subroutine test_lowess
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_magic_square()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('magic_square',msg='')
   !!call unit_check('magic_square', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('magic_square',msg='')
end subroutine test_magic_square
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ncr()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('ncr',msg='')
   !!call unit_check('ncr', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('ncr',msg='')
end subroutine test_ncr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poly_intercept()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('poly_intercept',msg='')
   !!call unit_check('poly_intercept', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('poly_intercept',msg='')
end subroutine test_poly_intercept
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyarea()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('polyarea',msg='')
   !!call unit_check('polyarea', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('polyarea',msg='')
end subroutine test_polyarea
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyarea_mid_point()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('polyarea_mid_point',msg='')
   !!call unit_check('polyarea_mid_point', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('polyarea_mid_point',msg='')
end subroutine test_polyarea_mid_point
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyarea_shoelace()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('polyarea_shoelace',msg='')
   !!call unit_check('polyarea_shoelace', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('polyarea_shoelace',msg='')
end subroutine test_polyarea_shoelace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_qhfg()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('qhfg',msg='')
   !!call unit_check('qhfg', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('qhfg',msg='')
end subroutine test_qhfg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_qhsg()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('qhsg',msg='')
   !!call unit_check('qhsg', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('qhsg',msg='')
end subroutine test_qhsg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_qtfg()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('qtfg',msg='')
   !!call unit_check('qtfg', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('qtfg',msg='')
end subroutine test_qtfg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_quadratic()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('quadratic',msg='')
   !!call unit_check('quadratic', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('quadratic',msg='')
end subroutine test_quadratic
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_invert_2x2()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('real_invert_2x2',msg='')
   !!call unit_check('real_invert_2x2', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('real_invert_2x2',msg='')
end subroutine test_real_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_invert_3x3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('real_invert_3x3',msg='')
   !!call unit_check('real_invert_3x3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('real_invert_3x3',msg='')
end subroutine test_real_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_invert_4x4()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('real_invert_4x4',msg='')
   !!call unit_check('real_invert_4x4', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('real_invert_4x4',msg='')
end subroutine test_real_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_round()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('round',msg='')
   !!call unit_check('round', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('round',msg='')
end subroutine test_round
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('scale1',msg='')
   !!call unit_check('scale1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('scale1',msg='')
end subroutine test_scale1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale3()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('scale3',msg='')
   !!call unit_check('scale3', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('scale3',msg='')
end subroutine test_scale3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_skekur1()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('skekur1',msg='')
   !!call unit_check('skekur1', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('skekur1',msg='')
end subroutine test_skekur1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_skekurx()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('skekurx',msg='')
   !!call unit_check('skekurx', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('skekurx',msg='')
end subroutine test_skekurx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splift()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('splift',msg='')
   !!call unit_check('splift', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('splift',msg='')
end subroutine test_splift
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splint()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('splint',msg='')
   !!call unit_check('splint', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('splint',msg='')
end subroutine test_splint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stddev()

use M_debug, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg,msg
use M_debug, only : unit_check_level
   call unit_check_start('stddev',msg='')
   !!call unit_check('stddev', 0.eq.0. msg=msg('checking',100))
   call unit_check_done('stddev',msg='')
end subroutine test_stddev
!===================================================================================================================================
end subroutine test_suite_M_math
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_math
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
