      SUBROUTINE WATROF(THLIQ,THICE,ZPOND,TPOND,OVRFLW,TOVRFL,
     1                  SUBFLW,TSUBFL,RUNOFF,TRUNOF,FI,ZPLIM,
     2                  XSLOPE,XDRAINH,MANNING_N,DD,KSAT,TBARW,
     3                  DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                  ISAND,IWF,IG,ILG,IL1,IL2,BULK_FC)


C     * AUG 26/20 - D.PRINCZ.   CHANGED THE DIMENSIONS OF SUBFLW/TSUBFL
C                               TO PRESERVE THE PER-LAYER VALUES FOR
C                               INTERFLOW. THE TILE TOTALS ARE THE SUMS
C                               OF THE VALUES ALONG THE 2ND DIMENSION.
C     * MAR 03/10 - M.A.MEKONNEN/B.DAVISON/M.MACDONALD
C     *             RE-WRITTEN FOR TWO REASONS:
C     *             -TO USE VINCENT'S VERSION OF WATDRN;
C     *             -TO INCLUDE MORE COMMENTS.
C     * SEP 16/06 - R.SOULIS/F.SEGLENIEKS/A.PIETRONIRO/B.DAVISON.
C     *             MODIFICATIONS TO OVERLAND FLOW.
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3.
C     * MAR 30/05 - D.VERSEGHY. ADDITIONAL FIELDS.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 02/02 - R.SOULIS/D.VERSEGHY. UPDATES DEVELOPED AT
C     *             WATERLOO.
C     * DEC 10/01 - R.SOULIS/K.SNELGROVE/T.WHIDDEN/D.VERSEGHY
C     *             WATFLOOD ROUTINE TO CALCULATE OVERLAND FLOW AND
C     *             INTERFLOW COMPONENTS OF SURFACE RUNOFF.
C

C------------------ADDITIONAL CLARIFICATION-----------------------------------------------
C
C     FOR A SLOPING ELEMENT, WATROF COMPUTES:
C         1. OVERLAND FLOW AT THE SURFACE BASED ON THE AVAILABLE PONDING DEPTH;
C         2. LATERAL FLOW FROM EACH LAYER BASED ON THE AVAILABLE WATER IN EACH SOIL LAYER.
C
C     THE BASE FLOW FROM EACH LAYER IS ALSO COMPUTED BUT IT IS NOT USED AT PRESENT. SO,
C     THE TOTAL BASEFLOW REMAINS THE SAME AS THE ONE CALCULATED IN CLASS.
C
C-----------------------------------------------------------------------------------------
C     DEFINITIONS
C     IWF         - FLAG GOVERNING OVERLAND AND LATERAL FLOW CALCULATIONS
C                   0 REPRESENTS FLAT ELEMENT - CLASS CALCULATES OVERLAND AND LATERAL FLOW
C                   NE 0 REPRESENTS SLOPING ELEMENT - WATROF CALCULATES OVERLAND AND LATERAL FLOW
C     IG          - TOTAL NUMBER OF SOIL LAYERS
C     ILG         - TOTAL NUMBER OF ELEMENTS
C     IL1         - STARTING INDEX OF ACTIVE ELEMENT
C     IL2         - FINAL INDEX OF ACTIVE ELEMENT
C     THLIQ       - VOLUMETRIC LIQUID WATER CONTENT OF SOIL LAYERS
C     THICE       - VOLUMETRIC FROZEN WATER CONTENT OF SOIL LAYERS
C     FI          - FRACTIONAL COVERAGE OF SUBAREA IN QUESTION ON MODELLED AREA
C     ZPLIM       - SUBAREA MAXIMUM PONDING DEPTH
C     XSLOPE      - SURFACE SLOPE
C     GRKFAC      - WATROF PARAMETER USED WHEN RUNNING MESH CODE ? NEEDS MORE CLARIFICATION
C     WFCINT      - WATROF PARAMETER USED WHEN RUNNING MESH CODE ? NEEDS MORE CLARIFICATION
C     TBARW       - TEMPERATURE OF WATER IN SOIL LAYER
C     ZPOND       - DEPTH OF PONDED WATER ON SURFACE
C     TPOND       - SUBAREA TEMPERATURE OF SURFACE PONDED WATER
C     OVRFLW      - OVERLAND FLOW FROM TOP OF SOIL COLUMN
C     TOVRFL      - TEMPERATURE OF OVERLAND FLOW
C     SUBFLW      - INTERFLOW FROM SIDES OF SOIL COLUMN
C     TSUBFL      - TEMPERATURE OF INTERFLOW FROM SIDES OF SOIL COLUMN
C     RUNOFF      - TOTAL RUNOFF
C     TRUNOF      - TEMPERATURE OF TOTAL RUNOFF
C     DELZW       - PERMEABLE THICKNESS OF SOIL LAYER
C     THPOR       - PORE VOLUME IN SOIL LAYER
C     THLMIN      - RESIDUAL SOIL LIQUID WATER CONTENT REMAINING AFTER FREEZING OR EVAPORATION
C     PSISAT      - SOIL MOISTURE SUCTION AT SATURATION
C     BI          - CLAPP AND HORNBERGER EMPIRICAL “B” PARAMETER
C     ISAND       - SAND CONTENT FLAG ? NEEDS MORE CLARIFICATION FOR THE VALUES
C     BULK_FC     - BULK FIELD CAPACITY
C     DELT        - TIME STEP
C     TFREZ       - FREEZING POINT OF WATER
C     MANNING_N   - MANNING'S ROUGHNESS COEFFICIENT
C     DD          - DRAINAGE DENSITY
C     ASAT_T0     - BULK SATURATION AT INITIAL TIME
C     ASAT_T1     - BULK SATURATION AT FINAL TIME
C
      IMPLICIT NONE
C
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL  THLIQ (ILG,IG),  THICE (ILG,IG)
C
      REAL  ZPOND (ILG),     TPOND (ILG),     OVRFLW(ILG),
     1      TOVRFL(ILG),  SUBFLW(ILG,IG),
     2      RUNOFF(ILG),     TRUNOF(ILG)
C
C     * INPUT ARRAYS.
C
      REAL  FI    (ILG),    ZPLIM (ILG),     XSLOPE(ILG),
     1      xdrainh(ILG),    ksat(ILG),       TBARW (ILG,IG)
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL  DELZW (ILG,IG), THPOR (ILG,IG),  THLMIN(ILG,IG),
     1      BI    (ILG,IG)

      INTEGER                ISAND (ILG,IG)
C
C     * WORK ARRAYS.
C
      REAL  DODRN (ILG),    DOVER (ILG),
     1      DIDRN (ILG,IG), BULK_FC(ILG,IG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ
C
      COMMON /CLASS1/ DELT,TFREZ

C     * INTERNAL SCALARS AND VECTORS
      REAL VEL_T0(ILG),NUC_DOVER(ILG),MANNING_N(ILG),DD(ILG),
     1     GRKEFF(ILG),ASAT_T0(ILG),ASAT_T1(ILG),DELZWJ(ILG),
     2     BIJ(ILG),THPORJ(ILG),ASAT0(ILG),ASAT1(ILG),SATFC(ILG),
     3     DAVAIL,DTOT,SUBFLWJ(ILG),TSUBFL(ILG,IG),THLIQ_AVAIL(ILG),
     4     THPOR_AVAIL(ilg),BASFLWJ(ILG),XLAMBDA,ktop,kl,h0,c1,c2,
     +     ztop(ilg,ig)

      INTEGER IG,ILG,IL1,IL2,I,J
      INTEGER IWF(ILG)

      real exav

C-----------------------------------------------------------------------------------------
C     return if no nml is expected to run in this cycle
      if(.not. any(iwf == 1)) return

C-----------------------------------------------------------------------------------------
C     coefficients
      c1 = 2.0/3.0
      c2 = 1.5 !3.0/2.0

C-----------------------------------------------------------------------------------------
C     parameter - will be used to compute xdrainh (the fractional change in horizontal
C     conductivity in a depth change h0) in Vincent's new formula.
      h0 = 1.0

C-----------------------------------------------------------------------------------------
C     loop through each element

      do i = il1,il2

C-----------------------------------------------------------------------------------------
C        skip if using flat class
         if (iwf(i) /= 1) cycle

C        ---------------------------------------------------------------------------------
C        compute overland flow and add to runoff and to the overall overland flow
C        ---------------------------------------------------------------------------------
         if(fi(i) .gt. 0.0 .and. zpond(i) .gt. zplim(i))then

C           ------------------------------------------------------------------------------
C           calculate the depth of water available for overland flow
C           ------------------------------------------------------------------------------
            dover(i) = zpond(i)-zplim(i)

C           ------------------------------------------------------------------------------
C           calculate the flow velocity at the beginning of the timestep
C           (based on kinematic wave velocity) - eqn (1) in notes on overland flow
C           ------------------------------------------------------------------------------
            vel_t0(i) = (dover(i)**c1)*sqrt(xslope(i))/(manning_n(i))

C           ------------------------------------------------------------------------------
C           calculate a normalized unconstrained overland flow to avoid numerical
C           problems with a division of small dover(i) values.
c           eqn (29) in notes on overland flow
C           ------------------------------------------------------------------------------
            nuc_dover(i) = -2*dd(i)*vel_t0(i)*delt

C           ------------------------------------------------------------------------------
C           constrained overland flow - limited by physically possible flow.
C           eqn (30) in notes on overland flow
C           ------------------------------------------------------------------------------
            dodrn(i) = dover(i)*(1.0-1./((1.0-c1*nuc_dover(i))**c2))

C           ------------------------------------------------------------------------------
C           add overland flow to runoff and to the overall overland flow
C           ------------------------------------------------------------------------------
            if(runoff(i) .gt. 1.0e-08) then
               trunof(i) = (trunof(i)*runoff(i)+(tpond(i)+tfrez)*
     1                      dodrn(i))/(runoff(i)+dodrn(i))
            endif
            runoff(i)    = runoff(i) + dodrn(i)
            if(dodrn(i) .gt. 1.0e-08)then
               tovrfl(i) = (tovrfl(i)*ovrflw(i)+(tpond(i)+tfrez)*
     1                      fi(i)*dodrn(i))/(ovrflw(i)+fi(i)*dodrn(i))
               ovrflw(i) = ovrflw(i) + fi(i)*dodrn(i)

C              ---------------------------------------------------------------------------
C              subtract overland flow depth from the ponding depth
C              ---------------------------------------------------------------------------
               zpond(i)  = zpond(i)- dodrn(i)
            endif
         endif
      enddo

C-----------------------------------------------------------------------------------------
C     compute interflow flow from each layer
C-----------------------------------------------------------------------------------------
      thliq_avail = 0.0
      thpor_avail = 0.0
      asat_t0     = 0.0
      ztop        = 0.0

C-----------------------------------------------------------------------------------------
C     loop through each soil layer
      do j = 1,ig

C        ---------------------------------------------------------------------------------
C        loop through each element
         do i = il1,il2

C        ---------------------------------------------------------------------------------
C        skip if not using watrof
           if (iwf(i) /= 1) cycle

C        ---------------------------------------------------------------------------------
C        form vecotors for the layer - to be compatible with WATDRN arguments
           delzwj(i)   = delzw(i,j)
           bij(i)      = bi(i,j)
           thporj(i)   = thpor(i,j)

C        ---------------------------------------------------------------------------------
C        Find the top of each soil layer for the calculation of grkeff
           if(j .lt. ig)ztop(i,j+1) = ztop(i,j) - delzw(i,j)
           if(fi(i) .gt. 0.0 .and. isand(i,j) .ge. -2 .and.
     1         delzw(i,j) .gt. 0.0)then

C              ---------------------------------------------------------------------------
C              determine available liquidwater in layer
C              ---------------------------------------------------------------------------
               thliq_avail(i) = max(0.0,thliq(i,j)-thlmin(i,j))

C              ---------------------------------------------------------------------------
C              determine available porosity
C              ---------------------------------------------------------------------------
               thpor_avail(i)    = max(thliq(i,j),thlmin(i,j),
     1                              thpor(i,j)-thice(i,j))

C              ---------------------------------------------------------------------------
C              saturation defined as liquid water content over available porosity
C              ---------------------------------------------------------------------------
               asat_t0(i)     = thliq_avail(i)/thpor_avail(i)
            endif

C           ------------------------------------------------------------------------------
C           grkeff - average value of the parameter controlling the time scale of
C                    interflow process - kl * (tile slope / tile length) (1/s)
C           Note: this formula is not the same as the one in Fhydro2_VF_20100226.f
C                 and needs to be confirmed by Vincent.
C           ------------------------------------------------------------------------------
c*       Integration of k across the layer -> kl
            xlambda   = -log(xdrainh(i))/h0
            ktop      = ksat(i)*exp(xlambda*ztop(i,j))
            kl        = ktop * exav(xlambda*delzw(i,j))
            grkeff(i) = kl*xslope(i)*2.0*dd(i)/(1+xslope(i)**2)
            thpor_avail(i) = max(thlmin(i,j),thpor_avail(i))
         enddo

C        ---------------------------------------------------------------------------------
C        compute interflow from the layer (subflowj). Baseflow from the layer (basflwj) is
C        also computed but is not used at present.
C        ---------------------------------------------------------------------------------
         call watdrn (delzwj,bij,thpor_avail,ksat,grkeff,asat_t0,iwf,
     1                asat_t1,subflwj,basflwj,satfc,
     2                ilg,il1,il2,1,delt)

C        ---------------------------------------------------------------------------------
C        loop through each element

         do i = il1,il2

C           ------------------------------------------------------------------------------
C           skip if not using watrof
            if (iwf(i) /= 1) cycle

C           -----------------------------------------------------------------------------
C           allow lateral flow if liquid water content is greater than
c           bulk field capacity.
C           -----------------------------------------------------------------------------
            if(thliq_avail(i).gt.0.0.and.thliq(i,j).ge.bulk_fc(i,j))then
               didrn(i,j) = subflwj(i)

C              ---------------------------------------------------------------------------
C              compute davail: volume of available water in a soil layer per land
C                              area [m^3/m^2]
C              ---------------------------------------------------------------------------
               davail = thliq_avail(i)*delzw(i,j)

C              ---------------------------------------------------------------------------
C              limit the lateral flow not to exceed the available water in the layer
C              ---------------------------------------------------------------------------
               didrn(i,j) = max(0.0,min(davail,didrn(i,j)))

C              ---------------------------------------------------------------------------
C              add the lateral flow to the runoff and to the subflow
C              ---------------------------------------------------------------------------
               if(didrn(i,j).gt.1.0e-8)then
                  trunof(i)  = (trunof(i)*runoff(i)+tbarw(i,j)*
     1                         didrn(i,j))/(runoff(i)+didrn(i,j))
                  runoff(i)  = runoff(i)+didrn(i,j)
                  subflw(i,j)= subflw(i,j)+fi(i)*didrn(i,j)

C                 ------------------------------------------------------------------------
C                 remove the lateral flow from the layer
C                 ------------------------------------------------------------------------
                  thliq(i,j) = thliq(i,j)-didrn(i,j)/delzw(i,j)
               endif
            endif
         enddo
      enddo

      return
      end

c**********************************************************************
c
c     function exav(x)
c     finds average of an exponential function exp(-x)
c     over the interval [0,x], which is (1-exp(-x))/x
c     deals with limit values of 1-x/2 when x->0 and 1/x when x->inf
c
c**********************************************************************
*
      function exav(x)
      implicit none
*
      real exphuge,expbig,expsmall,x,exav
      data exphuge/1.0e+9/,expbig/1.0e+8/,expsmall/1.0e-8/
*
      if (x .gt. exphuge) then
         exav = 0.0
      elseif (x .gt. expbig) then
         exav = 1.0/x
      elseif (x .gt. expsmall) then
         exav = (1.0-exp(-x))/x
      else
         exav = 1.0-x/2.0
      endif
      return
      end
