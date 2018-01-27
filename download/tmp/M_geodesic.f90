!>
!!
!! This is a Fortran implementation of the geodesic algorithms described in
!!
!!   C. F. F. Karney,
!!   Algorithms for geodesics,
!!   J. Geodesy 87, 43-55 (2013);
!!   https://doi.org/10.1007/s00190-012-0578-z
!!   Addenda: https://geographiclib.sourceforge.io/geod-addenda.html
!!
!! For documentation, see
!!
!!   https://geographiclib.sourceforge.io/html/Fortran/
!!
!! The code in this directory is entirely self-contained.  In particular,
!! it does not depend on the C++ classes.  You can compile and link the
!! example programs directly with something like:
!!
!!   f95 -o geodinverse geodinverse.for geodesic.for
!!   echo 30 0 29.5 179.5 | ./geodinverse
!!
!! The two tools ngsforward and ngsinverse are replacements for the NGS
!! tools FORWARD and INVERSE available from
!!
!!   http://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
!===================================================================================================================================
module M_geodesic
!> @file geodesic.inc
!! @brief The interface file for the geodesic routines in Fortran
!!
!! Optinally insert \code
!! include 'geodesic.inc' \endcode
!! into the declaration portion of a subroutine that uses this library.
!!
!! See geodesic.for for documentation on these routines.

! omask bits: 1 = a12; 2 = m12; 4 = MM12 + MM21; 8 = SS12
! flags bits: 1 = arcmode; 2 = unroll

!> @cond SKIP

      !!block data geodat
      doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      integer digits, maxit1, maxit2
      logical :: init=.false.
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

contains
! The subroutines in this files are documented at
! https://geographiclib.sourceforge.io/html/Fortran/
!
!> @file geodesic.for
!! @brief Implementation of geodesic routines in Fortran
!!
!! This is a Fortran implementation of the geodesic algorithms described
!! in
!! - C. F. F. Karney,
!!   <a href="https://doi.org/10.1007/s00190-012-0578-z">
!!   Algorithms for geodesics</a>,
!!   J. Geodesy <b>87</b>, 43--55 (2013);
!!   DOI: <a href="https://doi.org/10.1007/s00190-012-0578-z">
!!   10.1007/s00190-012-0578-z</a>;
!!   addenda: <a href="https://geographiclib.sourceforge.io/geod-addenda.html">
!!   geod-addenda.html</a>.
!! .
!! The principal advantages of these algorithms over previous ones
!! (e.g., Vincenty, 1975) are
!! - accurate to round off for |<i>f</i>| &lt; 1/50;
!! - the solution of the inverse problem is always found;
!! - differential and integral properties of geodesics are computed.
!!
!! The shortest path between two points on the ellipsoid at (\e lat1, \e
!! lon1) and (\e lat2, \e lon2) is called the geodesic.  Its length is
!! \e s12 and the geodesic from point 1 to point 2 has forward azimuths
!! \e azi1 and \e azi2 at the two end points.
!!
!! Traditionally two geodesic problems are considered:
!! - the direct problem -- given \e lat1, \e lon1, \e s12, and \e azi1,
!!   determine \e lat2, \e lon2, and \e azi2.  This is solved by the
!!   subroutine direct().
!! - the inverse problem -- given \e lat1, \e lon1, \e lat2, \e lon2,
!!   determine \e s12, \e azi1, and \e azi2.  This is solved by the
!!   subroutine invers().
!!
!! The ellipsoid is specified by its equatorial radius \e a (typically
!! in meters) and flattening \e f.  The routines are accurate to round
!! off with double precision arithmetic provided that |<i>f</i>| &lt;
!! 1/50; for the WGS84 ellipsoid, the errors are less than 15
!! nanometers.  (Reasonably accurate results are obtained for |<i>f</i>|
!! &lt; 1/5.)  For a prolate ellipsoid, specify \e f &lt; 0.
!!
!! The routines also calculate several other quantities of interest
!! - \e SS12 is the area between the geodesic from point 1 to point 2
!!   and the equator; i.e., it is the area, measured counter-clockwise,
!!   of the geodesic quadrilateral with corners (\e lat1,\e lon1), (0,\e
!!   lon1), (0,\e lon2), and (\e lat2,\e lon2).
!! - \e m12, the reduced length of the geodesic is defined such that if
!!   the initial azimuth is perturbed by \e dazi1 (radians) then the
!!   second point is displaced by \e m12 \e dazi1 in the direction
!!   perpendicular to the geodesic.  On a curved surface the reduced
!!   length obeys a symmetry relation, \e m12 + \e m21 = 0.  On a flat
!!   surface, we have \e m12 = \e s12.
!! - \e MM12 and \e MM21 are geodesic scales.  If two geodesics are
!!   parallel at point 1 and separated by a small distance \e dt, then
!!   they are separated by a distance \e MM12 \e dt at point 2.  \e MM21
!!   is defined similarly (with the geodesics being parallel to one
!!   another at point 2).  On a flat surface, we have \e MM12 = \e MM21
!!   = 1.
!! - \e a12 is the arc length on the auxiliary sphere.  This is a
!!   construct for converting the problem to one in spherical
!!   trigonometry.  \e a12 is measured in degrees.  The spherical arc
!!   length from one equator crossing to the next is always 180&deg;.
!!
!! If points 1, 2, and 3 lie on a single geodesic, then the following
!! addition rules hold:
!! - \e s13 = \e s12 + \e s23
!! - \e a13 = \e a12 + \e a23
!! - \e SS13 = \e SS12 + \e SS23
!! - \e m13 = \e m12 \e MM23 + \e m23 \e MM21
!! - \e MM13 = \e MM12 \e MM23 &minus; (1 &minus; \e MM12 \e MM21) \e
!!   m23 / \e m12
!! - \e MM31 = \e MM32 \e MM21 &minus; (1 &minus; \e MM23 \e MM32) \e
!!   m12 / \e m23
!!
!! The shortest distance returned by the solution of the inverse problem
!! is (obviously) uniquely defined.  However, in a few special cases
!! there are multiple azimuths which yield the same shortest distance.
!! Here is a catalog of those cases:
!! - \e lat1 = &minus;\e lat2 (with neither point at a pole).  If \e
!!   azi1 = \e azi2, the geodesic is unique.  Otherwise there are two
!!   geodesics and the second one is obtained by setting [\e azi1, \e
!!   azi2] &rarr; [\e azi2, \e azi1], [\e MM12, \e MM21] &rarr; [\e
!!   MM21, \e MM12], \e SS12 &rarr; &minus;\e SS12.  (This occurs when
!!   the longitude difference is near &plusmn;180&deg; for oblate
!!   ellipsoids.)
!! - \e lon2 = \e lon1 &plusmn; 180&deg; (with neither point at a pole).
!!   If \e azi1 = 0&deg; or &plusmn;180&deg;, the geodesic is unique.
!!   Otherwise there are two geodesics and the second one is obtained by
!!   setting [\e azi1, \e azi2] &rarr; [&minus;\e azi1, &minus;\e azi2],
!!   \e SS12 &rarr; &minus;\e SS12.  (This occurs when \e lat2 is near
!!   &minus;\e lat1 for prolate ellipsoids.)
!! - Points 1 and 2 at opposite poles.  There are infinitely many
!!   geodesics which can be generated by setting [\e azi1, \e azi2]
!!   &rarr; [\e azi1, \e azi2] + [\e d, &minus;\e d], for arbitrary \e
!!   d.  (For spheres, this prescription applies when points 1 and 2 are
!!   antipodal.)
!! - \e s12 = 0 (coincident points).  There are infinitely many
!!   geodesics which can be generated by setting [\e azi1, \e azi2]
!!   &rarr; [\e azi1, \e azi2] + [\e d, \e d], for arbitrary \e d.
!!
!! These routines are a simple transcription of the corresponding C++
!! classes in <a href="https://geographiclib.sourceforge.io">
!! GeographicLib</a>.  Because of the limitations of Fortran 77, the
!! classes have been replaced by simple subroutines with no attempt to
!! save "state" across subroutine calls.  Most of the internal comments
!! have been retained.  However, in the process of transcription some
!! documentation has been lost and the documentation for the C++
!! classes, GeographicLib::Geodesic, GeographicLib::GeodesicLine, and
!! GeographicLib::PolygonAreaT, should be consulted.  The C++ code
!! remains the "reference implementation".  Think twice about
!! restructuring the internals of the Fortran code since this may make
!! porting fixes from the C++ code more difficult.
!!
!! Copyright (c) Charles Karney (2012-2017) <charles@karney.com> and
!! licensed under the MIT/X11 License.  For more information, see
!! https://geographiclib.sourceforge.io/
!!
!! This library was distributed with
!! <a href="../index.html">GeographicLib</a> 1.49.

!> Solve the direct geodesic problem
!!
!! @param[in] a the equatorial radius (meters).
!! @param[in] f the flattening of the ellipsoid.  Setting \e f = 0 gives
!!   a sphere.  Negative \e f gives a prolate ellipsoid.
!! @param[in] lat1 latitude of point 1 (degrees).
!! @param[in] lon1 longitude of point 1 (degrees).
!! @param[in] azi1 azimuth at point 1 (degrees).
!! @param[in] s12a12 if \e arcmode is not set, this is the distance
!!   from point 1 to point 2 (meters); otherwise it is the arc
!!   length from point 1 to point 2 (degrees); it can be negative.
!! @param[in] flags a bitor'ed combination of the \e arcmode and \e
!!   unroll flags.
!! @param[out] lat2 latitude of point 2 (degrees).
!! @param[out] lon2 longitude of point 2 (degrees).
!! @param[out] azi2 (forward) azimuth at point 2 (degrees).
!! @param[in] omask a bitor'ed combination of mask values
!!   specifying which of the following parameters should be set.
!! @param[out] a12s12 if \e arcmode is not set, this is the arc length
!!   from point 1 to point 2 (degrees); otherwise it is the distance
!!   from point 1 to point 2 (meters).
!! @param[out] m12 reduced length of geodesic (meters).
!! @param[out] MM12 geodesic scale of point 2 relative to point 1
!!   (dimensionless).
!! @param[out] MM21 geodesic scale of point 1 relative to point 2
!!   (dimensionless).
!! @param[out] SS12 area under the geodesic (meters<sup>2</sup>).
!!
!! \e flags is an integer in [0, 4) whose binary bits are interpreted
!! as follows
!! - 1 the \e arcmode flag
!! - 2 the \e unroll flag
!! .
!! If \e arcmode is not set, \e s12a12 is \e s12 and \e a12s12 is \e
!! a12; otherwise, \e s12a12 is \e a12 and \e a12s12 is \e s12.  It \e
!! unroll is not set, the value \e lon2 returned is in the range
!! [&minus;180&deg;, 180&deg;]; if unroll is set, the longitude variable
!! is "unrolled" so that \e lon2 &minus; \e lon1 indicates how many
!! times and in what sense the geodesic encircles the ellipsoid.
!!
!! \e omask is an integer in [0, 16) whose binary bits are interpreted
!! as follows
!! - 1 return \e a12
!! - 2 return \e m12
!! - 4 return \e MM12 and \e MM21
!! - 8 return \e SS12
!!
!! \e lat1 should be in the range [&minus;90&deg;, 90&deg;].  The value
!! \e azi2 returned is in the range [&minus;180&deg;, 180&deg;].
!!
!! If either point is at a pole, the azimuth is defined by keeping the
!! longitude fixed, writing \e lat = \e lat = &plusmn;(90&deg; &minus;
!! &epsilon;), and taking the limit &epsilon; &rarr; 0+.  An arc length
!! greater that 180&deg; signifies a geodesic which is not a shortest
!! path.  (For a prolate ellipsoid, an additional condition is necessary
!! for a shortest path: the longitudinal extent must not exceed of
!! 180&deg;.)
!!
!! Example of use:
!! \include geoddirect.for
      subroutine direct(a, f, lat1, lon1, azi1, s12a12, flags, lat2, lon2, azi2, omask, a12s12, m12, MM12, MM21, SS12)
! input
      doubleprecision a, f, lat1, lon1, azi1, s12a12
      integer flags, omask
! output
      doubleprecision lat2, lon2, azi2
! optional output
      doubleprecision a12s12, m12, MM12, MM21, SS12

      integer ord, nC1, nC1p, nC2, nA3, nA3x, nC3, nC3x, nC4, nC4x
      parameter (ord = 6, nC1 = ord, nC1p = ord, &
     &    nC2 = ord, nA3 = ord, nA3x = nA3, &
     &    nC3 = ord, nC3x = (nC3 * (nC3 - 1)) / 2, &
     &    nC4 = ord, nC4x = (nC4 * (nC4 + 1)) / 2)
      doubleprecision A3x(0:nA3x-1), C3x(0:nC3x-1), C4x(0:nC4x-1),C1a(nC1), C1pa(nC1p), C2a(nC2), C3a(nC3-1), C4a(0:nC4-1)

      doubleprecision atanhx, hypotx, AngNm, AngRnd, TrgSum, A1m1f, A2m1f, A3f, atn2dx, LatFix
      logical arcmod, unroll, arcp, redlp, scalp, areap
      doubleprecision e2, f1, ep2, n, b, c2,  &
     &    salp0, calp0, k2, eps, &
     &    salp1, calp1, ssig1, csig1, cbet1, sbet1, dn1, somg1, comg1, &
     &    salp2, calp2, ssig2, csig2, sbet2, cbet2, dn2, somg2, comg2, &
     &    ssig12, csig12, salp12, calp12, omg12, lam12, lon12, &
     &    sig12, stau1, ctau1, tau12, t, s, c, serr, E, &
     &    A1m1, A2m1, A3c, A4, AB1, AB2, &
     &    B11, B12, B21, B22, B31, B41, B42, J12

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      if (.not.init) call geoini

      e2 = f * (2 - f)
      ep2 = e2 / (1 - e2)
      f1 = 1 - f
      n = f / (2 - f)
      b = a * f1
      c2 = 0

      arcmod = mod(flags/1, 2) .eq. 1
      unroll = mod(flags/2, 2) .eq. 1

      arcp = mod(omask/1, 2) .eq. 1
      redlp = mod(omask/2, 2) .eq. 1
      scalp = mod(omask/4, 2) .eq. 1
      areap = mod(omask/8, 2) .eq. 1

      if (areap) then
        if (e2 .eq. 0) then
          c2 = a**2
        elseif (e2 .gt. 0) then
          c2 = (a**2 + b**2 * atanhx(sqrt(e2)) / sqrt(e2)) / 2
        else
          c2 = (a**2 + b**2 * atan(sqrt(abs(e2))) / sqrt(abs(e2))) / 2
        endif
      endif

      call A3cof(n, A3x)
      call C3cof(n, C3x)
      if (areap) call C4cof(n, C4x)

! Guard against underflow in salp0
      call sncsdx(AngRnd(azi1), salp1, calp1)

      call sncsdx(AngRnd(LatFix(lat1)), sbet1, cbet1)
      sbet1 = f1 * sbet1
      call norm2x(sbet1, cbet1)
! Ensure cbet1 = +dbleps at poles
      cbet1 = max(tiny, cbet1)
      dn1 = sqrt(1 + ep2 * sbet1**2)

! Evaluate alp0 from sin(alp1) * cos(bet1) = sin(alp0),
! alp0 in [0, pi/2 - |bet1|]
      salp0 = salp1 * cbet1
! Alt: calp0 = hypot(sbet1, calp1 * cbet1).  The following
! is slightly better (consider the case salp1 = 0).
      calp0 = hypotx(calp1, salp1 * sbet1)
! Evaluate sig with tan(bet1) = tan(sig1) * cos(alp1).
! sig = 0 is nearest northward crossing of equator.
! With bet1 = 0, alp1 = pi/2, we have sig1 = 0 (equatorial line).
! With bet1 =  pi/2, alp1 = -pi, sig1 =  pi/2
! With bet1 = -pi/2, alp1 =  0 , sig1 = -pi/2
! Evaluate omg1 with tan(omg1) = sin(alp0) * tan(sig1).
! With alp0 in (0, pi/2], quadrants for sig and omg coincide.
! No atan2(0,0) ambiguity at poles since cbet1 = +dbleps.
! With alp0 = 0, omg1 = 0 for alp1 = 0, omg1 = pi for alp1 = pi.
      ssig1 = sbet1
      somg1 = salp0 * sbet1
      if (sbet1 .ne. 0 .or. calp1 .ne. 0) then
        csig1 = cbet1 * calp1
      else
        csig1 = 1
      endif
      comg1 = csig1
! sig1 in (-pi, pi]
      call norm2x(ssig1, csig1)
! norm2x(somg1, comg1); -- don't need to normalize!

      k2 = calp0**2 * ep2
      eps = k2 / (2 * (1 + sqrt(1 + k2)) + k2)

      A1m1 = A1m1f(eps)
      call C1f(eps, C1a)
      B11 = TrgSum(.true., ssig1, csig1, C1a, nC1)
      s = sin(B11)
      c = cos(B11)
! tau1 = sig1 + B11
      stau1 = ssig1 * c + csig1 * s
      ctau1 = csig1 * c - ssig1 * s
! Not necessary because C1pa reverts C1a
!    B11 = -TrgSum(true, stau1, ctau1, C1pa, nC1p)

      if (.not. arcmod) call C1pf(eps, C1pa)

      if (redlp .or. scalp) then
        A2m1 = A2m1f(eps)
        call C2f(eps, C2a)
        B21 = TrgSum(.true., ssig1, csig1, C2a, nC2)
      else
! Suppress bogus warnings about unitialized variables
        A2m1 = 0
        B21 = 0
      endif

      call C3f(eps, C3x, C3a)
      A3c = -f * salp0 * A3f(eps, A3x)
      B31 = TrgSum(.true., ssig1, csig1, C3a, nC3-1)

      if (areap) then
        call C4f(eps, C4x, C4a)
! Multiplier = a^2 * e^2 * cos(alpha0) * sin(alpha0)
        A4 = a**2 * calp0 * salp0 * e2
        B41 = TrgSum(.false., ssig1, csig1, C4a, nC4)
      else
! Suppress bogus warnings about unitialized variables
        A4 = 0
        B41 = 0
      endif

      if (arcmod) then
! Interpret s12a12 as spherical arc length
        sig12 = s12a12 * degree
        call sncsdx(s12a12, ssig12, csig12)
! Suppress bogus warnings about unitialized variables
        B12 = 0
      else
! Interpret s12a12 as distance
        tau12 = s12a12 / (b * (1 + A1m1))
        s = sin(tau12)
        c = cos(tau12)
! tau2 = tau1 + tau12
        B12 = - TrgSum(.true., stau1 * c + ctau1 * s, ctau1 * c - stau1 * s, C1pa, nC1p)
        sig12 = tau12 - (B12 - B11)
        ssig12 = sin(sig12)
        csig12 = cos(sig12)
        if (abs(f) .gt. 0.01d0) then
! Reverted distance series is inaccurate for |f| > 1/100, so correct
! sig12 with 1 Newton iteration.  The following table shows the
! approximate maximum error for a = WGS_a() and various f relative to
! GeodesicExact.
!     erri = the error in the inverse solution (nm)
!     errd = the error in the direct solution (series only) (nm)
!     errda = the error in the direct solution (series + 1 Newton) (nm)
!
!       f     erri  errd errda
!     -1/5    12e6 1.2e9  69e6
!     -1/10  123e3  12e6 765e3
!     -1/20   1110 108e3  7155
!     -1/50  18.63 200.9 27.12
!     -1/100 18.63 23.78 23.37
!     -1/150 18.63 21.05 20.26
!      1/150 22.35 24.73 25.83
!      1/100 22.35 25.03 25.31
!      1/50  29.80 231.9 30.44
!      1/20   5376 146e3  10e3
!      1/10  829e3  22e6 1.5e6
!      1/5   157e6 3.8e9 280e6
          ssig2 = ssig1 * csig12 + csig1 * ssig12
          csig2 = csig1 * csig12 - ssig1 * ssig12
          B12 = TrgSum(.true., ssig2, csig2, C1a, nC1)
          serr = (1 + A1m1) * (sig12 + (B12 - B11)) - s12a12 / b
          sig12 = sig12 - serr / sqrt(1 + k2 * ssig2**2)
          ssig12 = sin(sig12)
          csig12 = cos(sig12)
! Update B12 below
        endif
      endif

! sig2 = sig1 + sig12
      ssig2 = ssig1 * csig12 + csig1 * ssig12
      csig2 = csig1 * csig12 - ssig1 * ssig12
      dn2 = sqrt(1 + k2 * ssig2**2)
      if (arcmod .or. abs(f) .gt. 0.01d0) B12 = TrgSum(.true., ssig2, csig2, C1a, nC1)
      AB1 = (1 + A1m1) * (B12 - B11)

! sin(bet2) = cos(alp0) * sin(sig2)
      sbet2 = calp0 * ssig2
! Alt: cbet2 = hypot(csig2, salp0 * ssig2)
      cbet2 = hypotx(salp0, calp0 * csig2)
      if (cbet2 .eq. 0) then
! I.e., salp0 = 0, csig2 = 0.  Break the degeneracy in this case
        cbet2 = tiny
        csig2 = cbet2
      endif
! tan(omg2) = sin(alp0) * tan(sig2)
! No need to normalize
      somg2 = salp0 * ssig2
      comg2 = csig2
! tan(alp0) = cos(sig2)*tan(alp2)
! No need to normalize
      salp2 = salp0
      calp2 = calp0 * csig2
! East or west going?
      E = sign(1d0, salp0)
! omg12 = omg2 - omg1
      if (unroll) then
         omg12 = E * (sig12 - (atan2( ssig2, csig2) - atan2( ssig1, csig1)) + (atan2(E * somg2, comg2) - atan2(E * somg1, comg1)))
      else
        omg12 = atan2(somg2 * comg1 - comg2 * somg1, comg2 * comg1 + somg2 * somg1)
      endif

      lam12 = omg12 + A3c * ( sig12 + (TrgSum(.true., ssig2, csig2, C3a, nC3-1) - B31))
      lon12 = lam12 / degree
      if (unroll) then
        lon2 = lon1 + lon12
      else
        lon2 = AngNm(AngNm(lon1) + AngNm(lon12))
      endif
      lat2 = atn2dx(sbet2, f1 * cbet2)
      azi2 = atn2dx(salp2, calp2)

      if (redlp .or. scalp) then
        B22 = TrgSum(.true., ssig2, csig2, C2a, nC2)
        AB2 = (1 + A2m1) * (B22 - B21)
        J12 = (A1m1 - A2m1) * sig12 + (AB1 - AB2)
      endif
! Add parens around (csig1 * ssig2) and (ssig1 * csig2) to ensure
! accurate cancellation in the case of coincident points.
      if (redlp) m12 = b * ((dn2 * (csig1 * ssig2) - dn1 * (ssig1 * csig2)) - csig1 * csig2 * J12)
      if (scalp) then
        t = k2 * (ssig2 - ssig1) * (ssig2 + ssig1) / (dn1 + dn2)
        MM12 = csig12 + (t * ssig2 - csig2 * J12) * ssig1 / dn1
        MM21 = csig12 - (t * ssig1 - csig1 * J12) * ssig2 / dn2
      endif

      if (areap) then
        B42 = TrgSum(.false., ssig2, csig2, C4a, nC4)
        if (calp0 .eq. 0 .or. salp0 .eq. 0) then
! alp12 = alp2 - alp1, used in atan2 so no need to normalize
          salp12 = salp2 * calp1 - calp2 * salp1
          calp12 = calp2 * calp1 + salp2 * salp1
        else
! tan(alp) = tan(alp0) * sec(sig)
! tan(alp2-alp1) = (tan(alp2) -tan(alp1)) / (tan(alp2)*tan(alp1)+1)
! = calp0 * salp0 * (csig1-csig2) / (salp0^2 + calp0^2 * csig1*csig2)
! If csig12 > 0, write
!   csig1 - csig2 = ssig12 * (csig1 * ssig12 / (1 + csig12) + ssig1)
! else
!   csig1 - csig2 = csig1 * (1 - csig12) + ssig12 * ssig1
! No need to normalize
          if (csig12 .le. 0) then
            salp12 = csig1 * (1 - csig12) + ssig12 * ssig1
          else
            salp12 = ssig12 * (csig1 * ssig12 / (1 + csig12) + ssig1)
          endif
          salp12 = calp0 * salp0 * salp12
          calp12 = salp0**2 + calp0**2 * csig1 * csig2
        endif
        SS12 = c2 * atan2(salp12, calp12) + A4 * (B42 - B41)
      endif

      if (arcp) then
        if (arcmod) then
          a12s12 = b * ((1 + A1m1) * sig12 + AB1)
        else
          a12s12 = sig12 / degree
        endif
      endif

      return
      end subroutine direct
      subroutine invers(a, f, lat1, lon1, lat2, lon2, s12, azi1, azi2, omask, a12, m12, MM12, MM21, SS12)

!> Solve the inverse geodesic problem.
!!
!! @param[in] a the equatorial radius (meters).
!! @param[in] f the flattening of the ellipsoid.  Setting \e f = 0 gives
!!   a sphere.  Negative \e f gives a prolate ellipsoid.
!! @param[in] lat1 latitude of point 1 (degrees).
!! @param[in] lon1 longitude of point 1 (degrees).
!! @param[in] lat2 latitude of point 2 (degrees).
!! @param[in] lon2 longitude of point 2 (degrees).
!! @param[out] s12 distance from point 1 to point 2 (meters).
!! @param[out] azi1 azimuth at point 1 (degrees).
!! @param[out] azi2 (forward) azimuth at point 2 (degrees).
!! @param[in] omask a bitor'ed combination of mask values
!!   specifying which of the following parameters should be set.
!! @param[out] a12 arc length from point 1 to point 2 (degrees).
!! @param[out] m12 reduced length of geodesic (meters).
!! @param[out] MM12 geodesic scale of point 2 relative to point 1
!!   (dimensionless).
!! @param[out] MM21 geodesic scale of point 1 relative to point 2
!!   (dimensionless).
!! @param[out] SS12 area under the geodesic (meters<sup>2</sup>).
!!
!! \e omask is an integer in [0, 16) whose binary bits are interpreted
!! as follows
!! - 1 return \e a12
!! - 2 return \e m12
!! - 4 return \e MM12 and \e MM21
!! - 8 return \e SS12
!!
!! \e lat1 and \e lat2 should be in the range [&minus;90&deg;, 90&deg;].
!! The values of \e azi1 and \e azi2 returned are in the range
!! [&minus;180&deg;, 180&deg;].
!!
!! If either point is at a pole, the azimuth is defined by keeping the
!! longitude fixed, writing \e lat = &plusmn;(90&deg; &minus;
!! &epsilon;), and taking the limit &epsilon; &rarr; 0+.
!!
!! The solution to the inverse problem is found using Newton's method.
!! If this fails to converge (this is very unlikely in geodetic
!! applications but does occur for very eccentric ellipsoids), then the
!! bisection method is used to refine the solution.
!!
!! Example of use:
!! \include geodinverse.for
! input
      doubleprecision a, f, lat1, lon1, lat2, lon2
      integer omask
! output
      doubleprecision s12, azi1, azi2
! optional output
      doubleprecision a12, m12, MM12, MM21, SS12

      integer ord, nA3, nA3x, nC3, nC3x, nC4, nC4x, nC
      parameter(ord=6,nA3=ord,nA3x=nA3,nC3=ord,nC3x=(nC3*(nC3-1))/2,nC4=ord,nC4x=(nC4*(nC4+1))/2,nC=ord)
      doubleprecision A3x(0:nA3x-1), C3x(0:nC3x-1), C4x(0:nC4x-1), Ca(nC)

      doubleprecision atanhx, hypotx, AngDif, AngRnd, TrgSum, Lam12f, InvSta, atn2dx, LatFix
      integer latsgn, lonsgn, swapp, numit
      logical arcp, redlp, scalp, areap, merid, tripn, tripb

      doubleprecision e2, f1, ep2, n, b, c2,  &
     &    lat1x, lat2x, salp0, calp0, k2, eps,  &
     &    salp1, calp1, ssig1, csig1, cbet1, sbet1, dbet1, dn1,  &
     &    salp2, calp2, ssig2, csig2, sbet2, cbet2, dbet2, dn2,  &
     &    slam12, clam12, salp12, calp12, omg12, lam12, lon12, lon12s,  &
     &    salp1a, calp1a, salp1b, calp1b,  &
     &    dalp1, sdalp1, cdalp1, nsalp1, alp12, somg12, comg12, domg12,  &
     &    sig12, v, dv, dnm, dummy,  &
     &    A4, B41, B42, s12x, m12x, a12x, sdomg12, cdomg12

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2, lmask
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      if (.not.init) call geoini

      f1 = 1 - f
      e2 = f * (2 - f)
      ep2 = e2 / f1**2
      n = f / ( 2 - f)
      b = a * f1
      c2 = 0

      arcp = mod(omask/1, 2) .eq. 1
      redlp = mod(omask/2, 2) .eq. 1
      scalp = mod(omask/4, 2) .eq. 1
      areap = mod(omask/8, 2) .eq. 1
      if (scalp) then
        lmask = 16 + 2 + 4
      else
        lmask = 16 + 2
      endif

      if (areap) then
        if (e2 .eq. 0) then
          c2 = a**2
        elseif (e2 .gt. 0) then
          c2 = (a**2 + b**2 * atanhx(sqrt(e2)) / sqrt(e2)) / 2
        else
          c2 = (a**2 + b**2 * atan(sqrt(abs(e2))) / sqrt(abs(e2))) / 2
        endif
      endif

      call A3cof(n, A3x)
      call C3cof(n, C3x)
      if (areap) call C4cof(n, C4x)

! Compute longitude difference (AngDiff does this carefully).  Result is
! in [-180, 180] but -180 is only for west-going geodesics.  180 is for
! east-going and meridional geodesics.
! If very close to being on the same half-meridian, then make it so.
      lon12 = AngDif(lon1, lon2, lon12s)
! Make longitude difference positive.
      if (lon12 .ge. 0) then
        lonsgn = 1
      else
        lonsgn = -1
      endif
      lon12 = lonsgn * AngRnd(lon12)
      lon12s = AngRnd((180 - lon12) - lonsgn * lon12s)
      lam12 = lon12 * degree
      if (lon12 .gt. 90) then
        call sncsdx(lon12s, slam12, clam12)
        clam12 = -clam12
      else
        call sncsdx(lon12, slam12, clam12)
      endif

! If really close to the equator, treat as on equator.
      lat1x = AngRnd(LatFix(lat1))
      lat2x = AngRnd(LatFix(lat2))
! Swap points so that point with higher (abs) latitude is point 1
! If one latitude is a nan, then it becomes lat1.
      if (abs(lat1x) .lt. abs(lat2x)) then
        swapp = -1
      else
        swapp = 1
      endif
      if (swapp .lt. 0) then
        lonsgn = -lonsgn
        call swap(lat1x, lat2x)
      endif
! Make lat1 <= 0
      if (lat1x .lt. 0) then
        latsgn = 1
      else
        latsgn = -1
      endif
      lat1x = lat1x * latsgn
      lat2x = lat2x * latsgn
! Now we have
!
!     0 <= lon12 <= 180
!     -90 <= lat1 <= 0
!     lat1 <= lat2 <= -lat1
!
! longsign, swapp, latsgn register the transformation to bring the
! coordinates to this canonical form.  In all cases, 1 means no change
! was made.  We make these transformations so that there are few cases
! to check, e.g., on verifying quadrants in atan2.  In addition, this
! enforces some symmetries in the results returned.

      call sncsdx(lat1x, sbet1, cbet1)
      sbet1 = f1 * sbet1
      call norm2x(sbet1, cbet1)
! Ensure cbet1 = +dbleps at poles
      cbet1 = max(tiny, cbet1)

      call sncsdx(lat2x, sbet2, cbet2)
      sbet2 = f1 * sbet2
      call norm2x(sbet2, cbet2)
! Ensure cbet2 = +dbleps at poles
      cbet2 = max(tiny, cbet2)

! If cbet1 < -sbet1, then cbet2 - cbet1 is a sensitive measure of the
! |bet1| - |bet2|.  Alternatively (cbet1 >= -sbet1), abs(sbet2) + sbet1
! is a better measure.  This logic is used in assigning calp2 in
! Lambda12.  Sometimes these quantities vanish and in that case we force
! bet2 = +/- bet1 exactly.  An example where is is necessary is the
! inverse problem 48.522876735459 0 -48.52287673545898293
! 179.599720456223079643 which failed with Visual Studio 10 (Release and
! Debug)

      if (cbet1 .lt. -sbet1) then
        if (cbet2 .eq. cbet1) sbet2 = sign(sbet1, sbet2)
      else
        if (abs(sbet2) .eq. -sbet1) cbet2 = cbet1
      endif

      dn1 = sqrt(1 + ep2 * sbet1**2)
      dn2 = sqrt(1 + ep2 * sbet2**2)

! Suppress bogus warnings about unitialized variables
      a12x = 0
      merid = lat1x .eq. -90 .or. slam12 .eq. 0

      if (merid) then

! Endpoints are on a single full meridian, so the geodesic might lie on
! a meridian.

! Head to the target longitude
        calp1 = clam12
        salp1 = slam12
! At the target we're heading north
        calp2 = 1
        salp2 = 0

! tan(bet) = tan(sig) * cos(alp)
        ssig1 = sbet1
        csig1 = calp1 * cbet1
        ssig2 = sbet2
        csig2 = calp2 * cbet2

! sig12 = sig2 - sig1
        sig12 = atan2(0d0 + max(0d0, csig1 * ssig2 - ssig1 * csig2), csig1 * csig2 + ssig1 * ssig2)
        call Lengs(n, sig12, ssig1, csig1, dn1, ssig2, csig2, dn2, cbet1, cbet2, lmask, s12x, m12x, dummy, MM12, MM21, ep2, Ca)

! Add the check for sig12 since zero length geodesics might yield m12 <
! 0.  Test case was
!
!    echo 20.001 0 20.001 0 | GeodSolve -i
!
! In fact, we will have sig12 > pi/2 for meridional geodesic which is
! not a shortest path.
        if (sig12 .lt. 1 .or. m12x .ge. 0) then
          if (sig12 .lt. 3 * tiny) then
            sig12 = 0
            m12x = 0
            s12x = 0
          endif
          m12x = m12x * b
          s12x = s12x * b
          a12x = sig12 / degree
        else
! m12 < 0, i.e., prolate and too close to anti-podal
          merid = .false.
        endif
      endif

      omg12 = 0
! somg12 > 1 marks that it needs to be calculated
      somg12 = 2
      comg12 = 0
      if (.not. merid .and. sbet1 .eq. 0 .and.  (f .le. 0 .or. lon12s .ge. f * 180)) then

! Geodesic runs along equator
        calp1 = 0
        calp2 = 0
        salp1 = 1
        salp2 = 1
        s12x = a * lam12
        sig12 = lam12 / f1
        omg12 = sig12
        m12x = b * sin(sig12)
        if (scalp) then
          MM12 = cos(sig12)
          MM21 = MM12
        endif
        a12x = lon12 / f1
      elseif (.not. merid) then
! Now point1 and point2 belong within a hemisphere bounded by a
! meridian and geodesic is neither meridional or equatorial.

! Figure a starting point for Newton's method
        sig12 = InvSta(sbet1, cbet1, dn1, sbet2, cbet2, dn2, lam12, slam12, clam12, f, A3x, salp1, calp1, salp2, calp2, dnm, Ca)

        if (sig12 .ge. 0) then
! Short lines (InvSta sets salp2, calp2, dnm)
          s12x = sig12 * b * dnm
          m12x = dnm**2 * b * sin(sig12 / dnm)
          if (scalp) then
            MM12 = cos(sig12 / dnm)
            MM21 = MM12
          endif
          a12x = sig12 / degree
          omg12 = lam12 / (f1 * dnm)
        else

! Newton's method.  This is a straightforward solution of f(alp1) =
! lambda12(alp1) - lam12 = 0 with one wrinkle.  f(alp) has exactly one
! root in the interval (0, pi) and its derivative is positive at the
! root.  Thus f(alp) is positive for alp > alp1 and negative for alp <
! alp1.  During the course of the iteration, a range (alp1a, alp1b) is
! maintained which brackets the root and with each evaluation of
! f(alp) the range is shrunk, if possible.  Newton's method is
! restarted whenever the derivative of f is negative (because the new
! value of alp1 is then further from the solution) or if the new
! estimate of alp1 lies outside (0,pi); in this case, the new starting
! guess is taken to be (alp1a + alp1b) / 2.

! Bracketing range
          salp1a = tiny
          calp1a = 1
          salp1b = tiny
          calp1b = -1
          tripn = .false.
          tripb = .false.
          do 10 numit = 0, maxit2-1
! the WGS84 test set: mean = 1.47, sd = 1.25, max = 16
! WGS84 and random input: mean = 2.85, sd = 0.60
            v = Lam12f(sbet1, cbet1, dn1, sbet2, cbet2, dn2,  &
     &          salp1, calp1, slam12, clam12, f, A3x, C3x, salp2, calp2,  &
     &          sig12, ssig1, csig1, ssig2, csig2,  &
     &          eps, domg12, numit .lt. maxit1, dv, Ca)
! Reversed test to allow escape with NaNs
            if (tripn) then
              dummy = 8
            else
              dummy = 1
            endif
            if (tripb .or. .not. (abs(v) .ge. dummy * tol0)) go to 20
! Update bracketing values
            if (v .gt. 0 .and. (numit .gt. maxit1 .or.  calp1/salp1 .gt. calp1b/salp1b)) then
              salp1b = salp1
              calp1b = calp1
            elseif (v .lt. 0 .and. (numit .gt. maxit1 .or.  calp1/salp1 .lt. calp1a/salp1a)) then
              salp1a = salp1
              calp1a = calp1
            endif
            if (numit .lt. maxit1 .and. dv .gt. 0) then
              dalp1 = -v/dv
              sdalp1 = sin(dalp1)
              cdalp1 = cos(dalp1)
              nsalp1 = salp1 * cdalp1 + calp1 * sdalp1
              if (nsalp1 .gt. 0 .and. abs(dalp1) .lt. pi) then
                calp1 = calp1 * cdalp1 - salp1 * sdalp1
                salp1 = nsalp1
                call norm2x(salp1, calp1)
! In some regimes we don't get quadratic convergence because
! slope -> 0.  So use convergence conditions based on dbleps
! instead of sqrt(dbleps).
                tripn = abs(v) .le. 16 * tol0
                go to 10
              endif
            endif
! Either dv was not positive or updated value was outside legal
! range.  Use the midpoint of the bracket as the next estimate.
! This mechanism is not needed for the WGS84 ellipsoid, but it does
! catch problems with more eccentric ellipsoids.  Its efficacy is
! such for the WGS84 test set with the starting guess set to alp1 =
! 90deg:
! the WGS84 test set: mean = 5.21, sd = 3.93, max = 24
! WGS84 and random input: mean = 4.74, sd = 0.99
            salp1 = (salp1a + salp1b)/2
            calp1 = (calp1a + calp1b)/2
            call norm2x(salp1, calp1)
            tripn = .false.
            tripb = abs(salp1a - salp1) + (calp1a - calp1) .lt. tolb .or. abs(salp1 - salp1b) + (calp1 - calp1b) .lt. tolb
 10       continue
 20       continue
          call Lengs(eps, sig12, ssig1, csig1, dn1, ssig2, csig2, dn2, cbet1, cbet2, lmask, s12x, m12x, dummy, MM12, MM21, ep2, Ca)
          m12x = m12x * b
          s12x = s12x * b
          a12x = sig12 / degree
          if (areap) then
            sdomg12 = sin(domg12)
            cdomg12 = cos(domg12)
            somg12 = slam12 * cdomg12 - clam12 * sdomg12
            comg12 = clam12 * cdomg12 + slam12 * sdomg12
          endif
        endif
      endif

! Convert -0 to 0
      s12 = 0 + s12x
      if (redlp) m12 = 0 + m12x

      if (areap) then
! From Lambda12: sin(alp1) * cos(bet1) = sin(alp0)
        salp0 = salp1 * cbet1
        calp0 = hypotx(calp1, salp1 * sbet1)
        if (calp0 .ne. 0 .and. salp0 .ne. 0) then
! From Lambda12: tan(bet) = tan(sig) * cos(alp)
          ssig1 = sbet1
          csig1 = calp1 * cbet1
          ssig2 = sbet2
          csig2 = calp2 * cbet2
          k2 = calp0**2 * ep2
          eps = k2 / (2 * (1 + sqrt(1 + k2)) + k2)
! Multiplier = a^2 * e^2 * cos(alpha0) * sin(alpha0).
          A4 = a**2 * calp0 * salp0 * e2
          call norm2x(ssig1, csig1)
          call norm2x(ssig2, csig2)
          call C4f(eps, C4x, Ca)
          B41 = TrgSum(.false., ssig1, csig1, Ca, nC4)
          B42 = TrgSum(.false., ssig2, csig2, Ca, nC4)
          SS12 = A4 * (B42 - B41)
        else
! Avoid problems with indeterminate sig1, sig2 on equator
          SS12 = 0
        endif

        if (.not. merid .and. somg12 .gt. 1) then
          somg12 = sin(omg12)
          comg12 = cos(omg12)
        endif

        if (.not. merid .and. comg12 .ge. 0.7071d0 .and. sbet2 - sbet1 .lt. 1.75d0) then
! Use tan(Gamma/2) = tan(omg12/2)
! * (tan(bet1/2)+tan(bet2/2))/(1+tan(bet1/2)*tan(bet2/2))
! with tan(x/2) = sin(x)/(1+cos(x))
          domg12 = 1 + comg12
          dbet1 = 1 + cbet1
          dbet2 = 1 + cbet2
          alp12 = 2 * atan2(somg12 * (sbet1 * dbet2 + sbet2 * dbet1), domg12 * ( sbet1 * sbet2 + dbet1 * dbet2 ) )
        else
! alp12 = alp2 - alp1, used in atan2 so no need to normalize
          salp12 = salp2 * calp1 - calp2 * salp1
          calp12 = calp2 * calp1 + salp2 * salp1
! The right thing appears to happen if alp1 = +/-180 and alp2 = 0, viz
! salp12 = -0 and alp12 = -180.  However this depends on the sign
! being attached to 0 correctly.  The following ensures the correct
! behavior.
          if (salp12 .eq. 0 .and. calp12 .lt. 0) then
            salp12 = tiny * calp1
            calp12 = -1
          endif
          alp12 = atan2(salp12, calp12)
        endif
        SS12 = SS12 + c2 * alp12
        SS12 = SS12 * swapp * lonsgn * latsgn
! Convert -0 to 0
        SS12 = 0 + SS12
      endif

! Convert calp, salp to azimuth accounting for lonsgn, swapp, latsgn.
      if (swapp .lt. 0) then
        call swap(salp1, salp2)
        call swap(calp1, calp2)
        if (scalp) call swap(MM12, MM21)
      endif

      salp1 = salp1 * swapp * lonsgn
      calp1 = calp1 * swapp * latsgn
      salp2 = salp2 * swapp * lonsgn
      calp2 = calp2 * swapp * latsgn

      azi1 = atn2dx(salp1, calp1)
      azi2 = atn2dx(salp2, calp2)

      if (arcp) a12 = a12x

      return
      end subroutine invers
      subroutine area(a, f, lats, lons, n, AA, PP)
!> Determine the area of a geodesic polygon
!!
!! @param[in] a the equatorial radius (meters).
!! @param[in] f the flattening of the ellipsoid.  Setting \e f = 0 gives
!!   a sphere.  Negative \e f gives a prolate ellipsoid.
!! @param[in] lats an array of the latitudes of the vertices (degrees).
!! @param[in] lons an array of the longitudes of the vertices (degrees).
!! @param[in] n the number of vertices.
!! @param[out] AA the (signed) area of the polygon (meters<sup>2</sup>).
!! @param[out] PP the perimeter of the polygon.
!!
!! \e lats should be in the range [&minus;90&deg;, 90&deg;].
!!
!! Only simple polygons (which are not self-intersecting) are allowed.
!! There's no need to "close" the polygon by repeating the first vertex.
!! The area returned is signed with counter-clockwise traversal being
!! treated as positive.
! input
      integer n
      doubleprecision a, f, lats(n), lons(n)
! output
      doubleprecision AA, PP

      integer i, omask, cross, trnsit
      doubleprecision s12, azi1, azi2, dummy, SS12, b, e2, c2, area0, atanhx, Aacc(2), Pacc(2)

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      omask = 8
      call accini(Aacc)
      call accini(Pacc)
      cross = 0
      do 10 i = 0, n-1
     call invers(a,f,lats(i+1),lons(i+1),lats(mod(i+1,n)+1),lons(mod(i+1,n)+1),s12,azi1,azi2,omask,dummy,dummy,dummy,dummy,SS12)
        call accadd(Pacc, s12)
        call accadd(Aacc, -SS12)
        cross = cross + trnsit(lons(i+1), lons(mod(i+1,n)+1))
 10   continue
      PP = Pacc(1)
      b = a * (1 - f)
      e2 = f * (2 - f)
      if (e2 .eq. 0) then
        c2 = a**2
      elseif (e2 .gt. 0) then
        c2 = (a**2 + b**2 * atanhx(sqrt(e2)) / sqrt(e2)) / 2
      else
        c2 = (a**2 + b**2 * atan(sqrt(abs(e2))) / sqrt(abs(e2))) / 2
      endif
      area0 = 4 * pi * c2
      if (mod(abs(cross), 2) .eq. 1) then
        if (Aacc(1) .lt. 0) then
          call accadd(Aacc, +area0/2)
        else
          call accadd(Aacc, -area0/2)
        endif
      endif
      if (Aacc(1) .gt. area0/2) then
        call accadd(Aacc, -area0)
      elseif (Aacc(1) .le. -area0/2) then
        call accadd(Aacc, +area0)
      endif
      AA = Aacc(1)

      return
      end subroutine area
      subroutine geover(major, minor, patch)
!> Return the version numbers for this package.
!!
!! @param[out] major the major version number.
!! @param[out] minor the minor version number.
!! @param[out] patch the patch number.
!!
!! This subroutine was added with version 1.44.

! output
      integer major, minor, patch

      major = 1
      minor = 49
      patch = 0

      return
      end subroutine geover
      subroutine geoini
      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      digits = 53
      dblmin = 0.5d0**1022
      dbleps = 0.5d0**(digits-1)

      pi = atan2(0d0, -1d0)
      degree = pi/180
! This is about cbrt(dblmin).  With other implementations, sqrt(dblmin)
! is used.  The larger value is used here to avoid complaints about a
! IEEE_UNDERFLOW_FLAG IEEE_DENORMAL signal.  This is triggered when
! invers is called with points at opposite poles.
      tiny = 0.5d0**((1022+1)/3)
      tol0 = dbleps
! Increase multiplier in defn of tol1 from 100 to 200 to fix inverse
! case 52.784459512564 0 -52.784459512563990912 179.634407464943777557
! which otherwise failed for Visual Studio 10 (Release and Debug)
      tol1 = 200 * tol0
      tol2 = sqrt(tol0)
! Check on bisection interval
      tolb = tol0 * tol2
      xthrsh = 1000 * tol2
      maxit1 = 20
      maxit2 = maxit1 + digits + 10

      init = .true.

      return
      end subroutine geoini
      subroutine Lengs(eps, sig12, ssig1, csig1, dn1, ssig2, csig2, dn2, cbet1, cbet2, omask, s12b, m12b, m0, MM12, MM21, ep2, Ca)
! input
      doubleprecision eps, sig12, ssig1, csig1, dn1, ssig2, csig2, dn2, cbet1, cbet2, ep2
      integer omask
! optional output
      doubleprecision s12b, m12b, m0, MM12, MM21
! temporary storage
      doubleprecision Ca(*)

      integer ord, nC1, nC2
      parameter (ord = 6, nC1 = ord, nC2 = ord)

      doubleprecision A1m1f, A2m1f, TrgSum
      doubleprecision m0x, J12, A1, A2, B1, B2, csig12, t, Cb(nC2)
      logical distp, redlp, scalp
      integer l

! Return m12b = (reduced length)/b; also calculate s12b = distance/b,
! and m0 = coefficient of secular term in expression for reduced length.

      distp = (mod(omask/16, 2) .eq. 1)
      redlp = (mod(omask/2, 2) .eq. 1)
      scalp = (mod(omask/4, 2) .eq. 1)

! Suppress compiler warnings
      m0x = 0
      J12 = 0
      A1 = 0
      A2 = 0
      if (distp .or. redlp .or. scalp) then
        A1 = A1m1f(eps)
        call C1f(eps, Ca)
        if (redlp .or. scalp) then
          A2 = A2m1f(eps)
          call C2f(eps, Cb)
          m0x = A1 - A2
          A2 = 1 + A2
        endif
        A1 = 1 + A1
      endif
      if (distp) then
        B1 = TrgSum(.true., ssig2, csig2, Ca, nC1) - TrgSum(.true., ssig1, csig1, Ca, nC1)
! Missing a factor of b
        s12b = A1 * (sig12 + B1)
        if (redlp .or. scalp) then
          B2 = Trgsum(.true., ssig2, csig2, Cb, nC2) - TrgSum(.true., ssig1, csig1, Cb, nC2)
          J12 = m0x * sig12 + (A1 * B1 - A2 * B2)
        endif
      elseif (redlp .or. scalp) then
! Assume here that nC1 >= nC2
        do 10 l = 1, nC2
          Cb(l) = A1 * Ca(l) - A2 * Cb(l)
 10     continue
        J12 = m0x * sig12 + (TrgSum(.true., ssig2, csig2, Cb, nC2) - TrgSum(.true., ssig1, csig1, Cb, nC2))
      endif
      if (redlp) then
        m0 = m0x
! Missing a factor of b.
! Add parens around (csig1 * ssig2) and (ssig1 * csig2) to ensure
! accurate cancellation in the case of coincident points.
        m12b = dn2 * (csig1 * ssig2) - dn1 * (ssig1 * csig2) - csig1 * csig2 * J12
      endif
      if (scalp) then
        csig12 = csig1 * csig2 + ssig1 * ssig2
        t = ep2 * (cbet1 - cbet2) * (cbet1 + cbet2) / (dn1 + dn2)
        MM12 = csig12 + (t * ssig2 - csig2 * J12) * ssig1 / dn1
        MM21 = csig12 - (t * ssig1 - csig1 * J12) * ssig2 / dn2
      endif

      return
      end subroutine Lengs
      doubleprecision function Astrd(x, y)
! Solve k^4+2*k^3-(x^2+y^2-1)*k^2-2*y^2*k-y^2 = 0 for positive root k.
! This solution is adapted from Geocentric::Reverse.
! input
      doubleprecision x, y

      doubleprecision cbrt
      doubleprecision k, p, q, r, S, r2, r3, disc, u, T3, T, ang, v, uv, w

      p = x**2
      q = y**2
      r = (p + q - 1) / 6
      if ( .not. (q .eq. 0 .and. r .lt. 0) ) then
! Avoid possible division by zero when r = 0 by multiplying equations
! for s and t by r^3 and r, resp.
! S = r^3 * s
        S = p * q / 4
        r2 = r**2
        r3 = r * r2
! The discriminant of the quadratic equation for T3.  This is zero on
! the evolute curve p^(1/3)+q^(1/3) = 1
        disc = S * (S + 2 * r3)
        u = r
        if (disc .ge. 0) then
          T3 = S + r3
! Pick the sign on the sqrt to maximize abs(T3).  This minimizes loss
! of precision due to cancellation.  The result is unchanged because
! of the way the T is used in definition of u.
! T3 = (r * t)^3
          if (T3 .lt. 0) then
            disc = -sqrt(disc)
          else
            disc = sqrt(disc)
          endif
          T3 = T3 + disc
! N.B. cbrt always returns the real root.  cbrt(-8) = -2.
! T = r * t
          T = cbrt(T3)
! T can be zero; but then r2 / T -> 0.
          if (T .ne. 0) u = u + T + r2 / T
        else
! T is complex, but the way u is defined the result is real.
          ang = atan2(sqrt(-disc), -(S + r3))
! There are three possible cube roots.  We choose the root which
! avoids cancellation.  Note that disc < 0 implies that r < 0.
          u = u + 2 * r * cos(ang / 3)
        endif
! guaranteed positive
        v = sqrt(u**2 + q)
! Avoid loss of accuracy when u < 0.
! u+v, guaranteed positive
        if (u .lt. 0) then
          uv = q / (v - u)
        else
          uv = u + v
        endif
! positive?
        w = (uv - q) / (2 * v)
! Rearrange expression for k to avoid loss of accuracy due to
! subtraction.  Division by 0 not possible because uv > 0, w >= 0.
! guaranteed positive
        k = uv / (sqrt(uv + w**2) + w)
      else
! q == 0 && r <= 0
! y = 0 with |x| <= 1.  Handle this case directly.
! for y small, positive root is k = abs(y)/sqrt(1-x^2)
        k = 0
      endif
      Astrd = k

      return
      end function Astrd
      doubleprecision function InvSta(sbet1,cbet1,dn1,sbet2,cbet2,dn2,lam12,slam12,clam12,f,A3x,salp1,calp1,salp2,calp2,dnm,Ca)
! Return a starting point for Newton's method in salp1 and calp1
! (function value is -1).  If Newton's method doesn't need to be used,
! return also salp2, calp2, and dnm and function value is sig12.
! input
      doubleprecision sbet1, cbet1, dn1, sbet2, cbet2, dn2, lam12, slam12, clam12, f, A3x(*)
! output
      doubleprecision salp1, calp1, salp2, calp2, dnm
! temporary
      doubleprecision Ca(*)

      doubleprecision hypotx, A3f, Astrd
      logical shortp
      doubleprecision f1, e2, ep2, n, etol2, k2, eps, sig12,              &
     &    sbet12, cbet12, sbt12a, omg12, somg12, comg12, ssig12, csig12,   &
     &    x, y, lamscl, betscl, cbt12a, bt12a, m12b, m0, dummy,            &
     &    k, omg12a, sbetm2, lam12x

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      f1 = 1 - f
      e2 = f * (2 - f)
      ep2 = e2 / (1 - e2)
      n = f / (2 - f)
! The sig12 threshold for "really short".  Using the auxiliary sphere
! solution with dnm computed at (bet1 + bet2) / 2, the relative error in
! the azimuth consistency check is sig12^2 * abs(f) * min(1, 1-f/2) / 2.
! (Error measured for 1/100 < b/a < 100 and abs(f) >= 1/1000.  For a
! given f and sig12, the max error occurs for lines near the pole.  If
! the old rule for computing dnm = (dn1 + dn2)/2 is used, then the error
! increases by a factor of 2.)  Setting this equal to epsilon gives
! sig12 = etol2.  Here 0.1 is a safety factor (error decreased by 100)
! and max(0.001, abs(f)) stops etol2 getting too large in the nearly
! spherical case.
      etol2 = 0.1d0 * tol2 / sqrt( max(0.001d0, abs(f)) * min(1d0, 1 - f/2) / 2 )

! Return value
      sig12 = -1
! bet12 = bet2 - bet1 in [0, pi); bt12a = bet2 + bet1 in (-pi, 0]
      sbet12 = sbet2 * cbet1 - cbet2 * sbet1
      cbet12 = cbet2 * cbet1 + sbet2 * sbet1
      sbt12a = sbet2 * cbet1 + cbet2 * sbet1

      shortp = cbet12 .ge. 0 .and. sbet12 .lt. 0.5d0 .and.  cbet2 * lam12 .lt. 0.5d0

      if (shortp) then
        sbetm2 = (sbet1 + sbet2)**2
! sin((bet1+bet2)/2)^2
!  =  (sbet1 + sbet2)^2 / ((sbet1 + sbet2)^2 + (cbet1 + cbet2)^2)
        sbetm2 = sbetm2 / (sbetm2 + (cbet1 + cbet2)**2)
        dnm = sqrt(1 + ep2 * sbetm2)
        omg12 = lam12 / (f1 * dnm)
        somg12 = sin(omg12)
        comg12 = cos(omg12)
      else
        somg12 = slam12
        comg12 = clam12
      endif

      salp1 = cbet2 * somg12
      if (comg12 .ge. 0) then
        calp1 = sbet12 + cbet2 * sbet1 * somg12**2 / (1 + comg12)
      else
        calp1 = sbt12a - cbet2 * sbet1 * somg12**2 / (1 - comg12)
      endif

      ssig12 = hypotx(salp1, calp1)
      csig12 = sbet1 * sbet2 + cbet1 * cbet2 * comg12

      if (shortp .and. ssig12 .lt. etol2) then
! really short lines
        salp2 = cbet1 * somg12
        if (comg12 .ge. 0) then
          calp2 = somg12**2 / (1 + comg12)
        else
          calp2 = 1 - comg12
        endif
        calp2 = sbet12 - cbet1 * sbet2 * calp2
        call norm2x(salp2, calp2)
! Set return value
        sig12 = atan2(ssig12, csig12)
      elseif (abs(n) .gt. 0.1d0 .or. csig12 .ge. 0 .or.  ssig12 .ge. 6 * abs(n) * pi * cbet1**2) then
! Nothing to do, zeroth order spherical approximation is OK
        continue
      else
! lam12 - pi
        lam12x = atan2(-slam12, -clam12)
! Scale lam12 and bet2 to x, y coordinate system where antipodal point
! is at origin and singular point is at y = 0, x = -1.
        if (f .ge. 0) then
! x = dlong, y = dlat
          k2 = sbet1**2 * ep2
          eps = k2 / (2 * (1 + sqrt(1 + k2)) + k2)
          lamscl = f * cbet1 * A3f(eps, A3x) * pi
          betscl = lamscl * cbet1
          x = lam12x / lamscl
          y = sbt12a / betscl
        else
! f < 0: x = dlat, y = dlong
          cbt12a = cbet2 * cbet1 - sbet2 * sbet1
          bt12a = atan2(sbt12a, cbt12a)
! In the case of lon12 = 180, this repeats a calculation made in
! Inverse.
          call Lengs(n, pi + bt12a, sbet1, -cbet1, dn1, sbet2, cbet2, dn2, cbet1, cbet2, 2, dummy, m12b, m0, dummy, dummy, ep2, Ca)
          x = -1 + m12b / (cbet1 * cbet2 * m0 * pi)
          if (x .lt. -0.01d0) then
            betscl = sbt12a / x
          else
            betscl = -f * cbet1**2 * pi
          endif
          lamscl = betscl / cbet1
          y = lam12x / lamscl
        endif

        if (y .gt. -tol1 .and. x .gt. -1 - xthrsh) then
! strip near cut
          if (f .ge. 0) then
            salp1 = min(1d0, -x)
            calp1 = - sqrt(1 - salp1**2)
          else
            if (x .gt. -tol1) then
              calp1 = 0
            else
              calp1 = 1
            endif
            calp1 = max(calp1, x)
            salp1 = sqrt(1 - calp1**2)
          endif
        else
! Estimate alp1, by solving the astroid problem.
!
! Could estimate alpha1 = theta + pi/2, directly, i.e.,
!   calp1 = y/k; salp1 = -x/(1+k);  for f >= 0
!   calp1 = x/(1+k); salp1 = -y/k;  for f < 0 (need to check)
!
! However, it's better to estimate omg12 from astroid and use
! spherical formula to compute alp1.  This reduces the mean number of
! Newton iterations for astroid cases from 2.24 (min 0, max 6) to 2.12
! (min 0 max 5).  The changes in the number of iterations are as
! follows:
!
! change percent
!    1       5
!    0      78
!   -1      16
!   -2       0.6
!   -3       0.04
!   -4       0.002
!
! The histogram of iterations is (m = number of iterations estimating
! alp1 directly, n = number of iterations estimating via omg12, total
! number of trials = 148605):
!
!  iter    m      n
!    0   148    186
!    1 13046  13845
!    2 93315 102225
!    3 36189  32341
!    4  5396      7
!    5   455      1
!    6    56      0
!
! Because omg12 is near pi, estimate work with omg12a = pi - omg12
          k = Astrd(x, y)
          if (f .ge. 0) then
            omg12a = -x * k/(1 + k)
          else
            omg12a = -y * (1 + k)/k
          endif
          omg12a = lamscl * omg12a
          somg12 = sin(omg12a)
          comg12 = -cos(omg12a)
! Update spherical estimate of alp1 using omg12 instead of lam12
          salp1 = cbet2 * somg12
          calp1 = sbt12a - cbet2 * sbet1 * somg12**2 / (1 - comg12)
        endif
      endif
! Sanity check on starting guess.  Backwards check allows NaN through.
      if (.not. (salp1 .le. 0)) then
        call norm2x(salp1, calp1)
      else
        salp1 = 1
        calp1 = 0
      endif
      InvSta = sig12

      return
      end function InvSta
      doubleprecision function Lam12f(sbet1, cbet1, dn1,                   &
     &    sbet2, cbet2, dn2, salp1, calp1, slm120, clm120, f, A3x, C3x,     &
     &    salp2, calp2, sig12, ssig1, csig1, ssig2, csig2, eps,             &
     &    domg12, diffp, dlam12, Ca)
! input
      doubleprecision sbet1, cbet1, dn1, sbet2, cbet2, dn2, salp1, calp1, slm120, clm120, f, A3x(*), C3x(*)
      logical diffp
! output
      doubleprecision salp2, calp2, sig12, ssig1, csig1, ssig2, csig2, eps, domg12
! optional output
      doubleprecision dlam12
! temporary
      doubleprecision Ca(*)

      integer ord, nC3
      parameter (ord = 6, nC3 = ord)

      doubleprecision hypotx, A3f, TrgSum

      doubleprecision f1, e2, ep2, salp0, calp0, somg1, comg1, somg2, comg2, somg12, comg12, lam12, eta, B312, k2, dummy

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      f1 = 1 - f
      e2 = f * (2 - f)
      ep2 = e2 / (1 - e2)
! Break degeneracy of equatorial line.  This case has already been
! handled.
      if (sbet1 .eq. 0 .and. calp1 .eq. 0) calp1 = -tiny

! sin(alp1) * cos(bet1) = sin(alp0)
      salp0 = salp1 * cbet1
! calp0 > 0
      calp0 = hypotx(calp1, salp1 * sbet1)

! tan(bet1) = tan(sig1) * cos(alp1)
! tan(omg1) = sin(alp0) * tan(sig1) = tan(omg1)=tan(alp1)*sin(bet1)
      ssig1 = sbet1
      somg1 = salp0 * sbet1
      csig1 = calp1 * cbet1
      comg1 = csig1
      call norm2x(ssig1, csig1)
! norm2x(somg1, comg1); -- don't need to normalize!

! Enforce symmetries in the case abs(bet2) = -bet1.  Need to be careful
! about this case, since this can yield singularities in the Newton
! iteration.
! sin(alp2) * cos(bet2) = sin(alp0)
      if (cbet2 .ne. cbet1) then
        salp2 = salp0 / cbet2
      else
        salp2 = salp1
      endif
! calp2 = sqrt(1 - sq(salp2))
!       = sqrt(sq(calp0) - sq(sbet2)) / cbet2
! and subst for calp0 and rearrange to give (choose positive sqrt
! to give alp2 in [0, pi/2]).
      if (cbet2 .ne. cbet1 .or. abs(sbet2) .ne. -sbet1) then
        if (cbet1 .lt. -sbet1) then
          calp2 = (cbet2 - cbet1) * (cbet1 + cbet2)
        else
          calp2 = (sbet1 - sbet2) * (sbet1 + sbet2)
        endif
        calp2 = sqrt((calp1 * cbet1)**2 + calp2) / cbet2
      else
        calp2 = abs(calp1)
      endif
! tan(bet2) = tan(sig2) * cos(alp2)
! tan(omg2) = sin(alp0) * tan(sig2).
      ssig2 = sbet2
      somg2 = salp0 * sbet2
      csig2 = calp2 * cbet2
      comg2 = csig2
      call norm2x(ssig2, csig2)
! norm2x(somg2, comg2); -- don't need to normalize!

! sig12 = sig2 - sig1, limit to [0, pi]
      sig12 = atan2(0d0 + max(0d0, csig1 * ssig2 - ssig1 * csig2), csig1 * csig2 + ssig1 * ssig2)

! omg12 = omg2 - omg1, limit to [0, pi]
      somg12 = 0d0 + max(0d0, comg1 * somg2 - somg1 * comg2)
      comg12 =                comg1 * comg2 + somg1 * somg2
! eta = omg12 - lam120
      eta = atan2(somg12 * clm120 - comg12 * slm120, comg12 * clm120 + somg12 * slm120);
      k2 = calp0**2 * ep2
      eps = k2 / (2 * (1 + sqrt(1 + k2)) + k2)
      call C3f(eps, C3x, Ca)
      B312 = (TrgSum(.true., ssig2, csig2, Ca, nC3-1) - TrgSum(.true., ssig1, csig1, Ca, nC3-1))
      domg12 = -f * A3f(eps, A3x) * salp0 * (sig12 + B312)
      lam12 = eta + domg12

      if (diffp) then
        if (calp2 .eq. 0) then
          dlam12 = - 2 * f1 * dn1 / sbet1
        else
          call Lengs(eps, sig12, ssig1, csig1, dn1, ssig2, csig2, dn2, cbet1, cbet2, 2, dummy, dlam12, dummy, dummy, dummy, ep2, Ca)
          dlam12 = dlam12 * f1 / (calp2 * cbet2)
        endif
      endif
      Lam12f = lam12

      return
      end function Lam12f
      doubleprecision function A3f(eps, A3x)
! Evaluate A3
      integer ord, nA3, nA3x
      parameter (ord = 6, nA3 = ord, nA3x = nA3)

! input
      doubleprecision eps
! output
      doubleprecision A3x(0: nA3x-1)

      doubleprecision polval
      A3f = polval(nA3 - 1, A3x, eps)

      return
      end function A3f
      subroutine C3f(eps, C3x, c)
! Evaluate C3 coeffs
! Elements c[1] thru c[nC3-1] are set
      integer ord, nC3, nC3x
      parameter (ord = 6, nC3 = ord, nC3x = (nC3 * (nC3 - 1)) / 2)

! input
      doubleprecision eps, C3x(0:nC3x-1)
! output
      doubleprecision c(nC3-1)

      integer o, m, l
      doubleprecision mult, polval

      mult = 1
      o = 0
      do 10 l = 1, nC3 - 1
        m = nC3 - l - 1
        mult = mult * eps
        c(l) = mult * polval(m, C3x(o), eps)
        o = o + m + 1
 10   continue

      return
      end subroutine C3f
      subroutine C4f(eps, C4x, c)
! Evaluate C4
! Elements c[0] thru c[nC4-1] are set
      integer ord, nC4, nC4x
      parameter (ord = 6, nC4 = ord, nC4x = (nC4 * (nC4 + 1)) / 2)

! input
      doubleprecision eps, C4x(0:nC4x-1)
!output
      doubleprecision c(0:nC4-1)

      integer o, m, l
      doubleprecision mult, polval

      mult = 1
      o = 0
      do 10 l = 0, nC4 - 1
        m = nC4 - l - 1
        c(l) = mult * polval(m, C4x(o), eps)
        o = o + m + 1
        mult = mult * eps
 10   continue

      return
      end subroutine C4f
doubleprecision function A1m1f(eps)
! The scale factor A1-1 = mean value of (d/dsigma)I1 - 1
! input
doubleprecision,intent(in) :: eps
   doubleprecision         :: t
   integer,parameter       :: ord=6, nA1=ord
   integer                 :: o, m
   doubleprecision         :: polval, coeff(nA1/2 + 2)= [1, 4, 64, 0, 256]

   o = 1
   m = nA1/2
   t = polval(m, coeff(o), eps**2) / coeff(o + m + 1)
   A1m1f = (t + eps) / (1 - eps)

end function A1m1f
      subroutine C1f(eps, c)
! The coefficients C1[l] in the Fourier expansion of B1
      integer ord, nC1
      parameter (ord = 6, nC1 = ord)

! input
      doubleprecision eps
! output
      doubleprecision c(nC1)

      doubleprecision eps2, d
      integer o, m, l
      doubleprecision          :: polval, coeff((nC1**2 + 7*nC1 - 2*(nC1/2))/4)=[ &
      &     -1,    6,     -16,   32,    &
      &     -9,    64,    -128,  2048,  &
      &     9,     -16,   768,          &
      &     3,     -5,    512,          &
      &     -7,    1280,                &
      &     -7,    2048   ]

      eps2 = eps**2
      d = eps
      o = 1
      do 10 l = 1, nC1
        m = (nC1 - l) / 2
        c(l) = d * polval(m, coeff(o), eps2) / coeff(o + m + 1)
        o = o + m + 2
        d = d * eps
 10   continue

      return
      end subroutine C1f
      subroutine C1pf(eps, c)
! The coefficients C1p[l] in the Fourier expansion of B1p
      integer ord, nC1p
      parameter (ord = 6, nC1p = ord)

! input
      doubleprecision eps
! output
      doubleprecision c(nC1p)

      doubleprecision eps2, d
      integer o, m, l
      doubleprecision          :: polval, coeff((nC1p**2 + 7*nC1p - 2*(nC1p/2))/4)=[ &
      &     205,    -432,   768,   1536,   &
      &     4005,   -4736,  3840,  12288,  &
      &     -225,   116,    384,           &
      &     -7173,  2695,   7680,          &
      &     3467,   7680,                  &
      &     38081,  61440 ]

      eps2 = eps**2
      d = eps
      o = 1
      do 10 l = 1, nC1p
        m = (nC1p - l) / 2
        c(l) = d * polval(m, coeff(o), eps2) / coeff(o + m + 1)
        o = o + m + 2
        d = d * eps
 10   continue

      return
      end subroutine C1pf
      doubleprecision function A2m1f(eps)
! The scale factor A2-1 = mean value of (d/dsigma)I2 - 1
! input
      doubleprecision eps

      doubleprecision t
      integer ord, nA2, o, m
      parameter (ord = 6, nA2 = ord)
      doubleprecision :: polval, coeff(nA2/2 + 2) =[ -11, -28, -192, 0, 256]

      o = 1
      m = nA2/2
      t = polval(m, coeff(o), eps**2) / coeff(o + m + 1)
      A2m1f = (t - eps) / (1 + eps)

      return
      end function A2m1f
      subroutine C2f(eps, c)
! The coefficients C2[l] in the Fourier expansion of B2
      integer ord, nC2
      parameter (ord = 6, nC2 = ord)

! input
      doubleprecision eps
! output
      doubleprecision c(nC2)

      doubleprecision eps2, d
      integer o, m, l
      doubleprecision :: polval, coeff((nC2**2 + 7*nC2 - 2*(nC2/2))/4) =[ &
      &     1,     2,     16,   32,    &
      &     35,    64,    384,  2048,  &
      &     15,    80,    768,  &
      &     7,     35,    512,  &
      &     63,    1280,  &
      &     77,    2048   ]

      eps2 = eps**2
      d = eps
      o = 1
      do 10 l = 1, nC2
        m = (nC2 - l) / 2
        c(l) = d * polval(m, coeff(o), eps2) / coeff(o + m + 1)
        o = o + m + 2
        d = d * eps
 10   continue

      return
      end subroutine C2f
      subroutine A3cof(n, A3x)
! The scale factor A3 = mean value of (d/dsigma)I3
      integer ord, nA3, nA3x
      parameter (ord = 6, nA3 = ord, nA3x = nA3)

! input
      doubleprecision n
! output
      doubleprecision A3x(0:nA3x-1)

      integer o, m, k, j
      doubleprecision :: polval, coeff((nA3**2 + 7*nA3 - 2*(nA3/2))/4)= [ &
     &   -3, 128,   &
     &   -2, -3, 64,   &
     &   -1, -3, -1, 16,   &
     &    3, -1, -2, 8,   &
     &    1, -1, 2,   &
     &    1, 1  ]

      o = 1
      k = 0
      do 10 j = nA3 - 1, 0, -1
        m = min(nA3 - j - 1, j)
        A3x(k) = polval(m, coeff(o), n) / coeff(o + m + 1)
        k = k + 1
        o = o + m + 2
 10   continue

      return
      end subroutine A3cof
      subroutine C3cof(n, C3x)
! The coefficients C3[l] in the Fourier expansion of B3
      integer ord, nC3, nC3x
      parameter (ord = 6, nC3 = ord, nC3x = (nC3 * (nC3 - 1)) / 2)

! input
      doubleprecision n
! output
      doubleprecision C3x(0:nC3x-1)

      integer o, m, l, j, k
      doubleprecision :: polval, coeff(((nC3-1)*(nC3**2 + 7*nC3 - 2*(nC3/2)))/8) =[ &
      &    3,  128,              &
      &    2,    5,  128,        &
      &   -1,    3,    3,   64,  &
      &   -1,    0,    1,    8,  &
      &   -1,    1,    4,        &
      &    5,  256,              &
      &    1,    3,  128,        &
      &   -3,   -2,    3,   64,  &
      &    1,   -3,    2,   32,  &
      &    7,  512,              &
      &  -10,    9,  384,        &
      &    5,   -9,    5,  192,  &
      &    7,  512,              &
      &  -14,    7,  512,        &
      &   21, 2560  ]

      o = 1
      k = 0
      do 20 l = 1, nC3 - 1
        do 10 j = nC3 - 1, l, -1
          m = min(nC3 - j - 1, j)
          C3x(k) = polval(m, coeff(o), n) / coeff(o + m + 1)
          k = k + 1
          o = o + m + 2
 10     continue
 20   continue

      return
      end subroutine C3cof
      subroutine C4cof(n, C4x)
! The coefficients C4[l] in the Fourier expansion of I4
      integer ord, nC4, nC4x
      parameter (ord = 6, nC4 = ord, nC4x = (nC4 * (nC4 + 1)) / 2)

! input
      doubleprecision n
! output
      doubleprecision C4x(0:nC4x-1)

      integer o, m, l, j, k
      doubleprecision :: polval, coeff((nC4 * (nC4 + 1) * (nC4 + 5)) / 6) =[ &
      &  97,      15015,   1088,   156,     45045,   -224,    -4784,  1573,   45045,   &
      &  -10656,  14144,   -4576,  -858,    45045,   &
      &  64,      624,     -4576,  6864,    -3003,   15015,   &
      &  100,     208,     572,    3432,    -12012,  30030,   45045,  &
      &  1,       9009,    -2944,  468,     135135,  5792,    1040,   -1287,  135135,  &
      &  5952,    -11648,  9152,   -2574,   135135,  &
      &  -64,     -624,    4576,   -6864,   3003,    135135,  &
      &  8,       10725,   1856,   -936,    225225,  -8448,   4992,   -1144,  225225,  &
      &  -1440,   4160,    -4576,  1716,    225225,  &
      &  -136,    63063,   1024,   -208,    105105,  &
      &  3584,    -3328,   1144,   315315,  &
      &  -128,    135135,  -2560,  832,     405405,  128,     99099   ]

      o = 1
      k = 0
      do 20 l = 0, nC4 - 1
        do 10 j = nC4 - 1, l, -1
          m = nC4 - j - 1
          C4x(k) = polval(m, coeff(o), n) / coeff(o + m + 1)
          k = k + 1
          o = o + m + 2
 10     continue
 20   continue

      return
      end subroutine C4cof
doubleprecision function sumx(u, v, t)
! input
doubleprecision u, v
! output
doubleprecision t

doubleprecision up, vpp
   sumx = u + v
   up = sumx - v
   vpp = sumx - up
   up = up - u
   vpp = vpp - v
   t = -(up + vpp)
end function sumx
doubleprecision function AngNm(x)
! input
doubleprecision x
   AngNm = mod(x, 360d0)
   if (AngNm .le. -180) then
      AngNm = AngNm + 360
   elseif (AngNm .gt. 180) then
      AngNm = AngNm - 360
   endif
end function AngNm
      doubleprecision function LatFix(x)
! input
      doubleprecision x

      LatFix = x
      if (.not. (abs(x) .gt. 90)) return
! concoct a NaN
      LatFix = sqrt(90 - abs(x))

      return
      end function LatFix
      doubleprecision function AngDif(x, y, e)
! Compute y - x.  x and y must both lie in [-180, 180].  The result is
! equivalent to computing the difference exactly, reducing it to (-180,
! 180] and rounding the result.  Note that this prescription allows -180
! to be returned (e.g., if x is tiny and negative and y = 180).  The
! error in the difference is returned in e
! input
      doubleprecision x, y
! output
      doubleprecision e

      doubleprecision d, t, sumx, AngNm
      d = AngNm(sumx(AngNm(-x), AngNm(y), t))
      if (d .eq. 180 .and. t .gt. 0) then
        d = -180
      endif
      AngDif = sumx(d, t, e)

      return
      end function AngDif
      doubleprecision function AngRnd(x)
! The makes the smallest gap in x = 1/16 - nextafter(1/16, 0) = 1/2^57
! for reals = 0.7 pm on the earth if x is an angle in degrees.  (This is
! about 1000 times more resolution than we get with angles around 90
! degrees.)  We use this to avoid having to deal with near singular
! cases when x is non-zero but tiny (e.g., 1.0e-200).  This also
! converts -0 to +0.
! input
      doubleprecision x

      doubleprecision y, z
      z = 1/16d0
      y = abs(x)
! The compiler mustn't "simplify" z - (z - y) to y
      if (y .lt. z) y = z - (z - y)
      AngRnd = sign(y, x)
      if (x .eq. 0) AngRnd = 0

      return
      end function AngRnd
      subroutine swap(x, y)
! input/output
      doubleprecision x, y

      doubleprecision z
      z = x
      x = y
      y = z

      return
      end subroutine swap
      doubleprecision function hypotx(x, y)
! input
      doubleprecision x, y

! With Fortran 2008, this becomes: hypotx = hypot(x, y)
      hypotx = sqrt(x**2 + y**2)

      return
      end function hypotx
      subroutine norm2x(x, y)
! input/output
      doubleprecision x, y

      doubleprecision hypotx, r
      r = hypotx(x, y)
      x = x/r
      y = y/r

      return
      end subroutine norm2x
doubleprecision function log1px(x)
doubleprecision,intent(in) :: x
   doubleprecision         :: y, z
      y = 1 + x
      z = y - 1
      if (z .eq. 0) then
        log1px = x
      else
        log1px = x * log(y) / z
      endif

      return
end function log1px
      doubleprecision function atanhx(x)
! input
      doubleprecision x

! With Fortran 2008, this becomes: atanhx = atanh(x)
      doubleprecision log1px, y
      y = abs(x)
      y = log1px(2 * y/(1 - y))/2
      atanhx = sign(y, x)

      return
      end function atanhx
      doubleprecision function cbrt(x)
! input
      doubleprecision x

      cbrt = sign(abs(x)**(1/3d0), x)

      return
      end function cbrt
      doubleprecision function TrgSum(sinp, sinx, cosx, c, n)
! Evaluate
! y = sinp ? sum(c[i] * sin( 2*i    * x), i, 1, n) :
!            sum(c[i] * cos((2*i-1) * x), i, 1, n)
! using Clenshaw summation.
! Approx operation count = (n + 5) mult and (2 * n + 2) add
! input
      logical sinp
      integer n
      doubleprecision sinx, cosx, c(n)

      doubleprecision ar, y0, y1
      integer n2, k

! 2 * cos(2 * x)
      ar = 2 * (cosx - sinx) * (cosx + sinx)
! accumulators for sum
      if (mod(n, 2) .eq. 1) then
        y0 = c(n)
        n2 = n - 1
      else
        y0 = 0
        n2 = n
      endif
      y1 = 0
! Now n2 is even
      do 10 k = n2, 1, -2
! Unroll loop x 2, so accumulators return to their original role
        y1 = ar * y0 - y1 + c(k)
        y0 = ar * y1 - y0 + c(k-1)
 10   continue
      if (sinp) then
! sin(2 * x) * y0
        TrgSum = 2 * sinx * cosx * y0
      else
! cos(x) * (y0 - y1)
        TrgSum = cosx * (y0 - y1)
      endif

      return
      end function TrgSum
      integer function trnsit(lon1, lon2)
! input
      doubleprecision lon1, lon2

      doubleprecision lon1x, lon2x, lon12, AngNm, AngDif, e
      lon1x = AngNm(lon1)
      lon2x = AngNm(lon2)
      lon12 = AngDif(lon1x, lon2x, e)
      trnsit = 0
      if (lon1x .le. 0 .and. lon2x .gt. 0 .and. lon12 .gt. 0) then
        trnsit = 1
      elseif (lon2x .le. 0 .and. lon1x .gt. 0 .and. lon12 .lt. 0) then
        trnsit = -1
      endif

      return
      end function trnsit
      subroutine accini(s)
! Initialize an accumulator; this is an array with two elements.
! input/output
      doubleprecision s(2)

      s(1) = 0
      s(2) = 0

      return
      end subroutine accini
      subroutine accadd(s, y)
! Add y to an accumulator.
! input
      doubleprecision y
! input/output
      doubleprecision s(2)

      doubleprecision z, u, sumx
      z = sumx(y, s(2), u)
      s(1) = sumx(z, s(1), s(2))
      if (s(1) .eq. 0) then
        s(1) = u
      else
        s(2) = s(2) + u
      endif

      return
      end subroutine accadd
      subroutine sncsdx(x, sinx, cosx)
! Compute sin(x) and cos(x) with x in degrees
! input
      doubleprecision x
! input/output
      doubleprecision sinx, cosx

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      doubleprecision r, s, c
      integer q
      r = mod(x, 360d0)
      q = nint(r / 90)
      r = (r - 90 * q) * degree
      s = sin(r)
! sin(-0) isn't reliably -0, so...
      if (x .eq. 0) s = x
      c = cos(r)
      q = mod(q + 4, 4)
      if (q .eq. 0) then
        sinx =  s
        cosx =  c
      elseif (q .eq. 1) then
        sinx =  c
        cosx = -s
      elseif (q .eq. 2) then
        sinx = -s
        cosx = -c
      else
! q.eq.3
        sinx = -c
        cosx =  s
      endif

      if (x .ne. 0) then
        sinx = 0d0 + sinx
        cosx = 0d0 + cosx
      endif

      return
      end subroutine sncsdx
      doubleprecision function atn2dx(y, x)
! input
      doubleprecision x, y

      !!doubleprecision dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh
      !!integer digits, maxit1, maxit2
      !!logical init
      !!common /geocom/ dblmin, dbleps, pi, degree, tiny, tol0, tol1, tol2, tolb, xthrsh, digits, maxit1, maxit2, init

      doubleprecision xx, yy
      integer q
      if (abs(y) .gt. abs(x)) then
        xx = y
        yy = x
        q = 2
      else
        xx = x
        yy = y
        q = 0
      endif
      if (xx .lt. 0) then
        xx = -xx
        q = q + 1
      endif
      atn2dx = atan2(yy, xx) / degree
      if (q .eq. 1) then
        if (yy .ge. 0) then
          atn2dx =  180 - atn2dx
        else
          atn2dx = -180 - atn2dx
        endif
      elseif (q .eq. 2) then
        atn2dx =  90 - atn2dx
      elseif (q .eq. 3) then
        atn2dx = -90 + atn2dx
      endif

      return
      end function atn2dx
      doubleprecision function polval(N, p, x)
! input
      integer N
      doubleprecision p(0:N), x

      integer i
      if (N .lt. 0) then
        polval = 0
      else
        polval = p(0)
      endif
      do 10 i = 1, N
        polval = polval * x + p(i)
 10   continue

      return
      end function polval

! Table of name abbreviations to conform to the 6-char limit and
! potential name conflicts.
!    A3coeff       A3cof
!    C3coeff       C3cof
!    C4coeff       C4cof
!    AngNormalize  AngNm
!    AngDiff       AngDif
!    AngRound      AngRnd
!    arcmode       arcmod
!    Astroid       Astrd
!    betscale      betscl
!    lamscale      lamscl
!    cbet12a       cbt12a
!    sbet12a       sbt12a
!    epsilon       dbleps
!    realmin       dblmin
!    geodesic      geod
!    inverse       invers
!    InverseStart  InvSta
!    Lambda12      Lam12f
!    latsign       latsgn
!    lonsign       lonsgn
!    Lengths       Lengs
!    meridian      merid
!    outmask       omask
!    shortline     shortp
!    norm          norm2x
!    SinCosSeries  TrgSum
!    xthresh       xthrsh
!    transit       trnsit
!    polyval       polval
!    LONG_UNROLL   unroll
!    sincosd       sncsdx
!    atan2d        atn2dx
!> @endcond SKIP
      subroutine bufdms(buff,lgh,hem,dd,dm,ds,ierror)
      implicit doubleprecision (a-h, o-z)
      implicit integer (i-n)
!
      logical     done,flag
!
      character*1 buff(*),abuf(21)
      character*1 ch
      character*1 hem
      integer*4   ll,lgh
      integer*4   i4,id,im,is,icond,ierror
      real*8      x(5)
!
!     set the "error flag"
!
      ierror = 0
      icond  = 0
!
!     set defaults for dd,dm,ds
!
      dd = 0.0d0
      dm = 0.0d0
      ds = 0.0d0
!
!     set default limits for "hem" flag
!
      if(     hem.eq.'N' .or. hem.eq.'S' )then
        ddmax = 90.0d0
      elseif( hem.eq.'E' .or. hem.eq.'W' )then
        ddmax = 360.0d0
      elseif( hem.eq.'A' )then
        ddmax = 360.0d0
      elseif( hem.eq.'Z' )then
        ddmax = 180.0d0
      elseif( hem.eq.'*' )then
        ddmax  = 0.0d0
        ierror = 1
      else
        ddmax = 360.0d0
      endif
!
      do 1 i=1,5
        x(i) = 0.0d0
    1 continue
!
      icolon = 0
      ipoint = 0
      icount = 0
      flag   = .true.
      jlgh   = lgh
!
      do 2 i=1,jlgh
        if( buff(i).eq.':' )then
          icolon = icolon+1
        endif
        if( buff(i).eq.'.' )then
          ipoint = ipoint+1
          flag   = .false.
        endif
        if( flag )then
          icount = icount+1
        endif
    2 continue
!
      if( ipoint.eq.1 .and. icolon.eq.0 )then
!
!       load temp buffer
!
        do 3 i=1,jlgh
          abuf(i) = buff(i)
    3   continue
        abuf(jlgh+1) = '$'
        ll = jlgh
!
        call gvalr8 (abuf,ll,r8,icond)
!
        if( icount.ge.5 )then
!
!         value is a packed decimal of ==>  DDMMSS.sssss
!
          ss = r8/10000.0d0
          id = idint( ss )
!
          r8 = r8-10000.0d0*dble(float(id))
          ss = r8/100.0d0
          im = idint( ss )
!
          r8 = r8-100.0d0*dble(float(im))
        else
!
!         value is a decimal of ==>  .xx   X.xxx   X.
!
          id = idint( r8 )
          r8 = (r8-id)*60.0d0
          im = idint( r8 )
          r8 = (r8-im)*60.0d0
        endif
!
!       account for rounding error
!
        is = idnint( r8*1.0d5 )
        if( is.ge.6000000 )then
           r8 = 0.0d0
           im = im+1
        endif
!
        if( im.ge.60 )then
          im = 0
          id = id+1
        endif
!
        dd = dble( float( id ) )
        dm = dble( float( im ) )
        ds = r8
      else
!
!       buff() value is a d,m,s of ==>  NN:NN:XX.xxx
!
        k    = 0
        next = 1
        done = .false.
        ie   = jlgh
!
        do 100 j=1,5
          ib = next
          do 90 i=ib,ie
            ch   = buff(i)
            last = i
            if( i.eq.jlgh .or. ch.eq.':' )then
              if( i.eq.jlgh )then
                done = .true.
              endif
              if( ch.eq.':' )then
                last = i-1
              endif
              goto 91
            endif
   90     continue
          goto 98
!
   91     ipoint = 0
          ik     = 0
          do 92 i=next,last
            ik = ik+1
            ch = buff(i)
            if( ch.eq.'.' )then
              ipoint = ipoint+1
            endif
            abuf(ik) = buff(i)
   92     continue
          abuf(ik+1) = '$'
!
          ll = ik
          if( ipoint.eq.0 )then
            call gvali4 (abuf,ll,i4,icond)
            r8 = dble(float( i4 ))
          else
            call gvalr8 (abuf,ll,r8,icond)
          endif
!
          k    = k+1
          x(k) = r8
!
   98     if( done )then
            goto 101
          endif
!
          next = last
   99     next = next+1
          if( buff(next).eq.':' )then
            goto 99
          endif
  100   continue
!
!       load dd,dm,ds
!
  101   if( k.ge.1 )then
          dd = x(1)
        endif
!
        if( k.ge.2 )then
          dm = x(2)
        endif
!
        if( k.ge.3 )then
          ds = x(3)
        endif
      endif
!
      if( dd.gt.ddmax  .or.  dm.ge.60.0d0 .or.  ds.ge.60.0d0 )then
        ierror = 1
        dd = 0.0d0
        dm = 0.0d0
        ds = 0.0d0
      endif
!
      if( icond.ne.0 )then
        ierror = 1
      endif
!
      return
      end subroutine bufdms
      subroutine elipss(elips)
      implicit doubleprecision(a-h,o-z)
      character*1  answer
      character*30 elips
      common/elipsoid/a,f
      write(*,*) '  Other Ellipsoids.'
      write(*,*) '  -----------------'
      write(*,*) '  '
      write(*,*) '  A) Airy 1858'
      write(*,*) '  B) Airy Modified'
      write(*,*) '  C) Australian National'
      write(*,*) '  D) Bessel 1841'
      write(*,*) '  E) Clarke 1880'
      write(*,*) '  F) Everest 1830'
      write(*,*) '  G) Everest Modified'
      write(*,*) '  H) Fisher 1960'
      write(*,*) '  I) Fisher 1968'
      write(*,*) '  J) Hough 1956'
      write(*,*) '  K) International (Hayford)'
      write(*,*) '  L) Krassovsky 1938'
      write(*,*) '  M) NWL-9D (WGS 66)'
      write(*,*) '  N) South American 1969'
      write(*,*) '  O) Soviet Geod. System 1985'
      write(*,*) '  P) WGS 72'
      write(*,*) '  Q-Z) User defined.'
      write(*,*) '  '
      write(*,*) '  Enter choice : '
      read(*,10) answer
   10 format(a1)
!
      if(answer.eq.'A'.or.answer.eq.'a') then
        a=6377563.396d0
        f=1.d0/299.3249646d0
        elips='Airy 1858'
      elseif(answer.eq.'B'.or.answer.eq.'b') then
        a=6377340.189d0
        f=1.d0/299.3249646d0
        elips='Airy Modified'
      elseif(answer.eq.'C'.or.answer.eq.'c') then
        a=6378160.d0
        f=1.d0/298.25d0
        elips='Australian National'
      elseif(answer.eq.'D'.or.answer.eq.'d') then
        a=6377397.155d0
        f=1.d0/299.1528128d0
        elips='Bessel 1841'
      elseif(answer.eq.'E'.or.answer.eq.'e') then
        a=6378249.145d0
        f=1.d0/293.465d0
        elips='Clarke 1880'
      elseif(answer.eq.'F'.or.answer.eq.'f') then
        a=6377276.345d0
        f=1.d0/300.8017d0
        elips='Everest 1830'
      elseif(answer.eq.'G'.or.answer.eq.'g') then
        a=6377304.063d0
        f=1.d0/300.8017d0
        elips='Everest Modified'
      elseif(answer.eq.'H'.or.answer.eq.'h') then
        a=6378166.d0
        f=1.d0/298.3d0
        elips='Fisher 1960'
      elseif(answer.eq.'I'.or.answer.eq.'i') then
        a=6378150.d0
        f=1.d0/298.3d0
        elips='Fisher 1968'
      elseif(answer.eq.'J'.or.answer.eq.'j') then
        a=6378270.d0
        f=1.d0/297.d0
        elips='Hough 1956'
      elseif(answer.eq.'K'.or.answer.eq.'k') then
        a=6378388.d0
        f=1.d0/297.d0
        elips='International (Hayford)'
      elseif(answer.eq.'L'.or.answer.eq.'l') then
        a=6378245.d0
        f=1.d0/298.3d0
        elips='Krassovsky 1938'
      elseif(answer.eq.'M'.or.answer.eq.'m') then
        a=6378145.d0
        f=1.d0/298.25d0
        elips='NWL-9D  (WGS 66)'
      elseif(answer.eq.'N'.or.answer.eq.'n') then
        a=6378160.d0
        f=1.d0/298.25d0
        elips='South American 1969'
      elseif(answer.eq.'O'.or.answer.eq.'o') then
        a=6378136.d0
        f=1.d0/298.257d0
        elips='Soviet Geod. System 1985'
      elseif(answer.eq.'P'.or.answer.eq.'p') then
        a=6378135.d0
        f=1.d0/298.26d0
        elips='WGS 72'
      else
        elips = 'User defined.'
!
        write(*,*) '  Enter Equatorial axis,   a : '
        read(*,*) a
        a = dabs(a)
!
        write(*,*) '  Enter either Polar axis, b or '
        write(*,*) '  Reciprocal flattening,   1/f : '
        read(*,*) ss
        ss = dabs(ss)
!
        f = 0.0d0
        if( 200.0d0.le.ss .and. ss.le.310.0d0 )then
          f = 1.d0/ss
        elseif( 6000000.0d0.lt.ss .and. ss.lt.a )then
          f = (a-ss)/a
        else
          elips = 'Error: default GRS80 used.'
          a     = 6378137.0d0
          f     = 1.0d0/298.25722210088d0
        endif
      endif
!
      return
      end subroutine elipss
      subroutine fixdms(ideg, min, sec, tol )
!
      implicit doubleprecision (a-h, o-z)
      implicit integer (i-n)
!
!     test for seconds near 60.0-tol
!
      if( sec.ge.( 60.0d0-tol ) )then
        sec  = 0.0d0
        min  = min+1
      endif
!
!     test for minutes near 60
!
      if( min.ge.60 )then
        min  = 0
        ideg = ideg+1
      endif
!
!     test for degrees near 360
!
      if( ideg.ge.360 )then
        ideg = 0
      endif
!
      return
      end subroutine fixdms
      subroutine hem_ns( lat_sn, hem )
      implicit integer (i-n)
      character*6  hem
!
      if( lat_sn.eq.1 ) then
        hem = 'North '
      else
        hem = 'South '
      endif
!
      return
      end subroutine hem_ns
      subroutine hem_ew( lon_sn, hem )
      implicit integer (i-n)
      character*6  hem
!
      if( lon_sn.eq.1 ) then
        hem = 'East  '
      else
        hem = 'West  '
      endif
!
      return
      end subroutine hem_ew
      subroutine getdeg(d,m,sec,isign,val)

!** comvert deg, min, sec to degrees

      implicit doubleprecision(a-h,j-z)

      val=(d+m/60.d0+sec/3600.d0)
      val=dble(isign)*val

      return
      end subroutine getdeg
      subroutine gvali4(buff,ll,vali4,icond)
      implicit     integer (i-n)
!
      logical      plus,sign,done,error
      character*1  buff(*)
      character*1  ch
!
!     integer*2    i
!     integer*2    l1
!
      integer*4    ich,icond
      integer*4    ll
      integer*4    vali4
!
      l1    = ll
      vali4 = 0
      icond = 0
      plus  = .true.
      sign  = .false.
      done  = .false.
      error = .false.
!
      i = 0
   10 i = i+1
      if( i.gt.l1 .or. done )then
        go to 1000
      else
        ch  = buff(i)
        ich = ichar( buff(i) )
      endif
!
      if(     ch.eq.'+' )then
!
!       enter on plus sign
!
        if( sign )then
          goto 150
        else
          sign = .true.
          goto 10
        endif
      elseif( ch.eq.'-' )then
!
!       enter on minus sign
!
        if( sign )then
          goto 150
        else
          sign = .true.
          plus = .false.
          goto 10
        endif
      elseif( ch.ge.'0' .and. ch.le.'9' )then
        goto 100
      elseif( ch.eq.' ' )then
!
!       enter on space -- ignore leading spaces
!
        if( .not.sign )then
          goto 10
        else
          buff(i) = '0'
          ich = 48
          goto 100
        endif
      elseif( ch.eq.':' )then
!
!       enter on colon -- ignore
!
        if( .not.sign )then
          goto 10
        else
          goto 1000
        endif
      elseif( ch.eq.'$' )then
!
!       enter on dollar "$"
!
        done = .true.
        goto 10
      else
!
!       something wrong
!
        goto 150
      endif
!
!     enter on numeric
!
  100 vali4 = 10*vali4+(ich-48)
      sign  = .true.
      goto 10
!
!     treat illegal character
!
  150 buff(i) = '0'
      vali4 = 0
      icond = 1
!
 1000 if( .not.plus )then
        vali4 = -vali4
      endif
!
      return
      end subroutine gvali4
      subroutine gvalr8(buff,ll,valr8,icond)
      implicit     integer (i-n)
!
      logical      plus,sign,dpoint,done
!
      character*1  buff(*)
      character*1  ch
!
!     integer*2    i, ip
!     integer*2    l1
!     integer*2    nn, num, n48
!
      integer*4    ich,icond
      integer*4    ll
!
      real*8       :: ten =10.0d0
      real*8       :: valr8
      real*8       :: zero= 0.0d0
!
!
      n48     =  48
      l1      =  ll
      icond   =   0
      valr8   =  zero
      plus    = .true.
      sign    = .false.
      dpoint  = .false.
      done    = .false.
!
!     start loop thru buffer
!
      i = 0
   10 i = i+1
      if( i.gt.l1 .or. done )then
        go to 1000
      else
        ch  = buff(i)
        nn  = ichar( ch )
        ich = nn
      endif
!
      if(     ch.eq.'+' )then
!
!       enter on plus sign
!
        if( sign )then
          goto 150
        else
          sign = .true.
          goto 10
        endif
      elseif( ch.eq.'-' )then
!
!       enter on minus sign
!
        if( sign )then
          goto 150
        else
          sign = .true.
          plus = .false.
          goto 10
        endif
      elseif( ch.eq.'.' )then
!
!       enter on decimal point
!
        ip     = 0
        sign   = .true.
        dpoint = .true.
        goto 10
      elseif( ch.ge.'0' .and. ch.le.'9' )then
        goto 100
      elseif( ch.eq.' ' )then
!
!       enter on space
!
        if( .not.sign )then
          goto 10
        else
          buff(i) = '0'
          ich = 48
          goto 100
        endif
      elseif( ch.eq.':' .or. ch.eq.'$' )then
!
!       enter on colon or "$" sign
!
        done = .true.
        goto 10
      else
!
!       something wrong
!
        goto 150
      endif
!
!     enter on numeric
!
  100 sign = .true.
      if( dpoint )then
        ip = ip+1
      endif
!
      num   = ich
      valr8 = ten*valr8+dble(float( num-n48 ))
      goto 10
!
!     treat illegal character
!
  150 buff(i) = '0'
      valr8   =  0.0d0
      icond   =  1
!
 1000 if( dpoint )then
        valr8 =  valr8/(ten**ip)
      endif
!
      if( .not.plus )then
        valr8 = -valr8
      endif
!
      return
      end subroutine gvalr8
subroutine todmsp(val,id,im,s,isign)

!** convert position degrees to deg,min,sec
!** range is [-180 to +180]

      implicit doubleprecision(a-h,o-z)

    1 if(val.gt.180) then
        val=val-180-180
        go to 1
      endif

    2 if(val.lt.-180) then
        val=val+180+180
        go to 2
      endif

      if(val.lt.0.d0) then
        isign=-1
      else
        isign=+1
      endif

      s=dabs(val)
      id=idint(s)
      s=(s-id)*60.d0
      im=idint(s)
      s=(s-im)*60.d0

!** account for rounding error

      is=idnint(s*1.d5)
      if(is.ge.6000000) then
        s=0.d0
        im=im+1
      endif
      if(im.ge.60) then
        im=0
        id=id+1
      endif

      return
end subroutine todmsp
      subroutine trim(buff,lgh,hem)
!
      implicit integer (i-n)
      character*1 ch,hem
      character*1 buff(*)
      integer*4   lgh
!
      ibeg = 1
      do 10 i=1,50
        if( buff(i).ne.' ' )then
          goto 11
        endif
        ibeg = ibeg+1
   10 continue
   11 continue
      if( ibeg.ge.50 )then
        ibeg = 1
        buff(ibeg) = '0'
      endif
!
      iend = 50
      do 20 i=1,50
        j = 51-i
        if( buff(j).eq.' ' )then
          iend = iend-1
        else
          goto 21
        endif
   20 continue
   21 continue
!
      ch = buff(ibeg)
      if( hem.eq.'N' )then
        if( ch.eq.'N' .or. ch.eq.'n' .or. ch.eq.'+' )then
          hem = 'N'
          ibeg = ibeg+1
        endif
        if( ch.eq.'S' .or. ch.eq.'s' .or. ch.eq.'-' )then
          hem = 'S'
          ibeg = ibeg+1
        endif
!
!       check for wrong hemisphere entry
!
        if( ch.eq.'E' .or. ch.eq.'e' )then
          hem = '*'
          ibeg = ibeg+1
        endif
        if( ch.eq.'W' .or. ch.eq.'w' )then
          hem = '*'
          ibeg = ibeg+1
        endif
      elseif( hem.eq.'W' )then
        if( ch.eq.'E' .or. ch.eq.'e' .or. ch.eq.'+' )then
          hem = 'E'
          ibeg = ibeg+1
        endif
        if( ch.eq.'W' .or. ch.eq.'w' .or. ch.eq.'-' )then
          hem = 'W'
          ibeg = ibeg+1
        endif
!
!       check for wrong hemisphere entry
!
        if( ch.eq.'N' .or. ch.eq.'n' )then
          hem = '*'
          ibeg = ibeg+1
        endif
        if( ch.eq.'S' .or. ch.eq.'s' )then
          hem = '*'
          ibeg = ibeg+1
        endif
      elseif( hem.eq.'A' )then
        if( .not.('0'.le.ch .and. ch.le.'9') )then
          hem = '*'
          ibeg = ibeg+1
        endif
      else
!        do nothing
      endif
!
!
      do 30 i=ibeg,iend
        ch = buff(i)
!
        if(     ch.eq.':' .or. ch.eq.'.' )then
          goto 30
        elseif( ch.eq.' ' .or. ch.eq.',' )then
          buff(i) = ':'
        elseif( '0'.le.ch .and. ch.le.'9' )then
          goto 30
        else
          buff(i) = ':'
        endif
!
   30 continue
!
!     left justify buff() array to its first character position
!     also check for a ":" char in the starting position,
!     if found!!  skip it
!
      j  = 0
      ib = ibeg
      ie = iend
!
      do 40 i=ib,ie
        if( i.eq.ibeg .and. buff(i).eq.':' )then
!
!         move the 1st position pointer to the next char &
!         do not put ":" char in buff(j) array where j=1
!
          ibeg = ibeg+1
          goto 40
        endif
        j = j+1
        buff(j) = buff(i)
   40 continue
!
!
      lgh = iend-ibeg+1
      j   = lgh+1
      buff(j) = '$'
!
!     clean-up the rest of the buff() array
!
      do 50 i=j+1,50
        buff(i) = ' '
   50 continue
!
!     save a maximum of 20 characters
!
      if( lgh.gt.20 )then
        lgh = 20
        j   = lgh+1
        buff(j) = '$'
      endif
!
      return
      end subroutine trim
end module M_geodesic
