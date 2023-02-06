      SUBROUTINE WATDRN(
     1    delzw,bcoef,thpora,grksat,grkeff,asat0,iwf,
     2    asat1,subflw,basflw,satsfc,
     3    ilg,il1,il2,wfk,delt)
c
c     * December 4, 2009, Vincent Fortin
c
c       Initial WATDRN code derived from WAT_DRAIN by Ric Soulis
c       Code entirely rewritten for three main reasons:
c       - simplify the parameterization by working with average
c         hydraulic conductivity within a layer (runs faster, but
c         more importantly is easier to understand for users)
c       - meet CCC and RPN source code standards
c       - improve readability
c
c     * December 11, 2009, Vincent Fortin
c
c       - Define grkeff as ksat * (tile slope / tile length) (1/s)
c         (take out thpor, as thpor changes with ice content)
c       - Modify computation of critical time accordingly:
c         tc = thpor / (c*grkeff)
c
c     * March 22, 2010, Vincent Fortin
c
c       - Stop computing t0 and t1 to avoid overflow, compute tc/t
c         directly when t>tc and t/tc when t<tc (where t=t0 or t1)
c       - Avoid dividing by tc to make the code more robust
c         when tc is small
c       - Simplify calculation of baseflow rate
c       - Bugfix: move satsfc calculation after computation of tc/t0
c       - Change variable name from thpor to thpora
c
c     Summary:
c
c     This routine calculates the outflow from a tilted landscape
c     element (tile). Rather than using average saturation for the 
c     element(asat0), it finds flow using the saturation at the
c     seepage face (for interflow) at at the bottom of the layer
c     (for baseflow). The routine returns:
c     1) the saturation at the end of the time step (asat1)
c     2) the interflow (subflw) and baseflow (basflw) integrated 
c        over the duration of the time step (in m)
c     3) the fraction of the surface which is saturated at the
c        surface according to the model (satsfc)
c
c     Interflow amount during the time step in m is determined from:
c     subflw = (asat0 - asat1) * thpor * delzw
c     (implicit estimation)
c
c     Baseflow amount during the time step in m is determined from:
c     basflw = grksat * asatb0**(2*bcoef+3) * delt
c     where asatb0 is the average saturation across the bottom of the
c     element at initial time (explicit estimation, 1st order)
c
c     The fraction of the surface that is saturated at the initial
c     time (satsfc) is estimated as MIN(1-t0/tc,0) where t0 is the 
c     (theoretical) time elapsed since saturation and tc is the
c     critical time at which the saturation front reaches the
c     seepage face.
c
c     Background info on WATDRN:
c
c     WATDRN is a parameterization for sub-grid scale interflow,
c     i.e. subsurface flow, which is thought to be an important flow
c     generation process following a rain event at the basin scale
c
c     The underlying principles behind WATDRN are described in
c     a paper by Soulis et al. (2000), Atmosphere-Ocean.
c     However, this code presents a simpler solution to the problem
c     which assumes that hydraulic conductivity is constant within
c     a soil layer. This is a departure from previous versions of
c     WATDRN aimed at making the code easier to understand
c     and faster to run.
c
c     Here is the basic idea: interflow is especially important
c     after a rain event which leaves the soil close to saturation.
c     Following such a rain event, water flows from the hillslope
c     to the river which creates a gradient in water content along
c     the hillslope. But land-surface models have generally as their
c     prognostic variable the mean water content of the grid box.
c     Under the hypothesis of Darcian flow along the hillslope
c     following a rain event which left the soil saturated, WATDRN
c     recovers the saturation distribution along the hillslope
c     from the bulk saturation, estimates from this the interflow
c     rate at the seepage face and integrates this rate over the
c     duration of the time step.
c
c     Given a bulk saturation value asat_t0 at the start of the
c     time step, and assuming that this bulk saturation is the
c     result of Darcian flow in the direction of the hillslope
c     starting from a saturated soil (with no rain after that),
c     there is a one-to-one relationship between bulk saturation
c     and the time elapsed since the rain stopped. So we can figure
c     out:
c
c     1. t0, the (theoretical) time elapsed since the soil was
c        saturated, knowing the bulk saturation asat0.
c     2. t1, the (theoretical) time at the end of the time step
c        (that's just t0+delt)
c     3. asat1, the bulk saturation at the end of the time step
c
c     Then interflow is proportional to the difference between
c     asat0 and asat1.
c
c     From bulk saturation, it is also possible to figure out
c     average saturation across the bottom of the element, which
c     is used to estimate baseflow, and the fraction of the surface
c     that is saturated, which can be used to separate runoff and
c     infiltration.
c
      IMPLICIT NONE
*
c     Input parameters
      INTEGER ilg         ! Size of arrays
      INTEGER il1         ! index of first grid point to process
      INTEGER il2         ! index of last grid point to process
      INTEGER wfk         ! IWF of parent call (e.g., WATROF, LATFLOW)
      REAL    delt        ! duration of the time step (s)
*
c     Input arrays
      REAL    delzw(ilg)  ! layer thickness (m)
      REAL    bcoef(ilg)  ! slope of retention curve, related to
                          ! Clapp-Hornberger connextivity index by c=2b+3
      REAL    thpora(ilg) ! Available porosity of the soil layer
                          ! (total porosity - ice content)
      REAL    grksat(ilg) ! Vertical hydraulic conductivity at saturation
                          ! at the bottom of the layer (m/s)
      REAL    grkeff(ilg) ! average value of the parameter
                          ! controlling the time scale of interflow process
                          ! ksat * (tile slope / tile length) (1/s)
      REAL    asat0(ilg)  ! bulk saturation at initial time
      INTEGER iwf(ilg)    ! IWF flag (runs if == wfk)
*
c     Output arrays
      REAL    asat1(ilg)  ! bulk saturation at the end of the time step
      REAL    subflw(ilg) ! interflow amount during the time step (m)
      REAL    basflw(ilg) ! baseflow rate during the time step (m)
      REAL    satsfc(ilg) ! saturated fraction of the surface (0 to 1)
*
c     Work arrays
      REAL    c(ilg)      ! Clapp-Hornberger connectivity index (c>1)
      REAL    cm1(ilg)    ! c-1
      REAL    c2m1(ilg)   ! 2*c-1
      REAL    asatc(ilg)  ! bulk saturation at the critical time tc
      REAL    asat00(ilg) ! MIN(1,asat0) because we don't handle supersaturated soils
      REAL    tc(ilg)     ! critical time at which the seepage face becomes unsaturated
      REAL    ratiot(ilg) ! ratio tc/t (t0 or t1) if t>tc and t/tc if t<=tc
      LOGICAL satspf(ilg) ! indicates if seepage face is saturated
                          ! equivalent to knowing if t<=tc
*
c     Local variables
      INTEGER i           ! Array index
*
c     return if no nml is expected to run in this cycle
      IF (.NOT. any(iwf == wfk)) RETURN
*
c**********************************************************************
c     STEP 0: Initialize a few things before we start
c             - output variables
c             - functions of the input parameters
c**********************************************************************
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk .or. asat0(i) == 0.0) CYCLE
c        c and c factors
         c(i)    = 2.*bcoef(i)+3.
         cm1(i)  = c(i)-1.
         c2m1(i) = 2.*c(i)-1.
c        bulk saturation at critical time
c        (just before the seepage face becomes unsaturated)
         asatc(i) = 1.-1./c(i)
c        layer average saturation asat0 may be greater than 1
c        e.g. frost heave but it is not possible for wat_drain
         asat00(i) = MIN(1.,asat0(i))
c        assess if seepage face is saturated at initial time
         satspf(i) = asat00(i) .GE. asatc(i)
      ENDDO
*
c**********************************************************************
c     STEP 1: Find theoretical time t0 elapsed since element was last
c             saturated and estimate baseflow rate at initial time
c             Also estimate fraction of surface that is saturated
c**********************************************************************
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk  .or. asat0(i) == 0.0) CYCLE
c        determine time at which seepage face becomes unsaturated
         tc(i) = thpora(i)/(c(i)*grkeff(i))
      ENDDO
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk  .or. asat0(i) == 0.0) CYCLE
c        find theoretical start of recession (t0) from bulk saturation
c        and at the same time estimate baseflow based on rate at t0
         IF (satspf(i)) THEN
c           saturated seepage face at initial time:
c           compute t0/tc
            ratiot(i) = c(i)*(1.-asat00(i))
c           normalized baseflow rate
            basflw(i) = 1.-c(i)*c(i)/c2m1(i)*(1.-asat00(i))
c           the fraction of the surface that is saturated at t0
c           varies linearly with t0/tc
            satsfc(i) = 1.-ratiot(i)
         ELSE
c           unsaturated seepage face at initial time:
c           calculate tc/t0 instead of t0 to avoid overflow
            ratiot(i) = (asat00(i)/asatc(i))**cm1(i)
c           normalized baseflow rate
            basflw(i) = cm1(i)/c2m1(i)*ratiot(i)*asat00(i)/asatc(i)
c           the fraction of the surface that is saturated at t0 is zero
            satsfc(i) = 0.
         ENDIF
      ENDDO
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk  .or. asat0(i) == 0.0) CYCLE
c        Compute baseflow in m from normalized baseflow rate
         basflw(i) = grksat(i)*basflw(i)*delt
      ENDDO
*
c**********************************************************************
c     STEP 2: Find theoretical time t1 at the end of the time step
c**********************************************************************
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk  .or. asat0(i) == 0.0) CYCLE
         IF (satspf(i)) THEN
c           Assess if seepage face will still be saturated at the
c           end of the time step
            satspf(i) = tc(i)*ratiot(i)+delt .LE. tc(i)
            IF (satspf(i)) THEN
c              Seepage face still saturated, compute t1/tc from t0/tc
               ratiot(i) = (tc(i)*ratiot(i)+delt)/tc(i)
            ELSE
c              Seepage face not saturated anymore, compute tc/t1
               ratiot(i) = tc(i)/(tc(i)*ratiot(i)+delt)
            END IF
         ELSE
c           If seepage face was not saturated initially, we compute
c           tc/t1=tc/(t0+delt) from tc/t0
            ratiot(i) = tc(i)*ratiot(i)/(tc(i)+delt*ratiot(i))
         END IF
      ENDDO
*
c**********************************************************************
c     STEP 3: Obtain bulk saturation at the end of the time step
c             and interflow amount
c**********************************************************************
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk  .or. asat0(i) == 0.0) CYCLE
         IF (satspf(i)) THEN
c           saturated seepage face at the end of the time step
            asat1(i) = 1.-ratiot(i)/c(i)
         ELSE
c           unsaturated seepage face at the end of the time step
            asat1(i) = asatc(i)*ratiot(i)**(1./cm1(i))
         ENDIF
      ENDDO
*
      DO i=il1,il2
c        cycle if not using watrof or latflow
         IF (iwf(i) /= wfk  .or. asat0(i) == 0.0) CYCLE
c        Sanity check: bulk saturation should not increase with time
         asat1(i) = MIN(asat00(i),asat1(i))
c        Obtain interflow from the difference in bulk saturation
         subflw(i) = (asat00(i)-asat1(i))*thpora(i)*delzw(i)
      ENDDO
*
      RETURN
      END
