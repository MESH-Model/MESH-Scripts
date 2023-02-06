      SUBROUTINE LATFLOW(THLIQ,THICE,TPOND,OVRFLW,TOVRFL,
     1                  SUBFLW,TSUBFL,RUNOFF,TRUNOF,fi,ZPLIM,
     2                  XSLOPE,XDRAINH,MANNING_N,DD,KSAT,TBARW,
     3                  DELZW,THPOR,THLMIN,BI,DIDRN,
     4                  ISAND,IWF,IG,ILG,IL1,IL2,BULK_FC,
C-------kam----for PDM------------------------------------------------------------
     5                  ZPNDPRE,ZPOND,FSTR,CMIN,CMAX,B,CSTR,UMQ)

C-----kam-------------------------------------------------------------------------------
C     Previously developed overlandflow generation algorithm WATROF used
C     Manning's equation for overland flow generation and Richard's equation
C     for interflow and baseflow generation. It assumes a certain surface
C     ponding capacity (ZPLIM) and subtract this from ponding depth (ZPOND).
C     Critical water holding capacity and non-contributing area was not considered
C     in this algorithm, which is one of the main reason of WATROF's failure in Prairies.
C     Muluneh et al. (2014) addresses contributing area (FSTR) and critical water holding
C     capacity (CSTR) in PDMROF algorithm using PDM concept of Moore (2007).
C     PDMROF algoritm has the limitation of no sub-surface flow component and
C     no routed direct runoff. LATFLOW algorithm attempts to overcomes the
C     limitation of PDMROF by including interflow and baseflow  component
C     from WATROF and including Manning's formula to estimate direct
C     overland runoff.

C-----References---------------------------------------------------------------
C     1. Mekonnen et al. (2014). Towards an improved land surface scheme for
C          prairie landscapes, Journal of Hydrology.
C     2. MOORE (2007). THE PDM RAINFALL-RUNOFF MODEL,
C          HYDROLOGY AND EARTH SYSTEM SCIENCES, VOL. 11, PP. 483-499

C-----Log-----------------------------------------------------------------------
C     * AUG 26/20 - D.PRINCZ.   CHANGED THE DIMENSIONS OF SUBFLW/TSUBFL
C                               TO PRESERVE THE PER-LAYER VALUES FOR
C                               INTERFLOW. THE TILE TOTALS ARE THE SUMS
C                               OF THE VALUES ALONG THE 2ND DIMENSION.
C     * JUN 24/20 - D.Princz: Bug-fixes (to consider when Q /= U).
C     * AUG 01/15 - Kam.: PDM concept in WATROF algorithm to include
C     *             interflow and baseflow component in PDMROF algorithm
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

C-----DEFINITIONS---------------------------------------------------------------------
C     IWF         - FLAG GOVERNING OVERLAND AND LATERAL FLOW CALCULATIONS
C                   0 REPRESENTS FLAT ELEMENT - CLASS CALCULATES OVERLAND AND LATERAL FLOW
C                   NE 0 REPRESENTS SLOPING ELEMENT - WATROF CALCULATES OVERLAND AND LATERAL FLOW
C     IG          - TOTAL NUMBER OF SOIL LAYERS
C     ILG         - TOTAL NUMBER OF ELEMENTS
C     IL1         - STARTING INDEX OF ACTIVE ELEMENT
C     IL2         - FINAL INDEX OF ACTIVE ELEMENT
C     THLIQ       - VOLUMETRIC LIQUID WATER CONTENT OF SOIL LAYERS
C     THICE       - VOLUMETRIC FROZEN WATER CONTENT OF SOIL LAYERS
C     fi          - FRACTIONAL COVERAGE OF SUBAREA IN QUESTION ON MODELLED AREA
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

C-----Added for PDM--------------------------------------------------------
C     COEFFICIENTS OF PARETO DISTRIBUTION FOR DIRECT RUNOFF GENERATION
C     CMAX        - MAXIMUM POTHOLE PONDING CAPACITY [M]
C     CMIN        - MINIMUM POTHOLE PONDING CAPACITY [M] (MINIMUM AMOUNT NOT AVAILABLE FOR OVERLAND FLOW)
C     B           - SHAPE FACTOR OF THE PARETO DISTRIBUTION FUNCTION
C     CSTR        - CRITICAL PONDING DEPTH FOR A GIVEN STORAGE [M]
C     FSTR        - DISTRIBUTION FUNCTION VALUE AT CRITICAL PONDING DEPTH [] (THIS IS THE CONTRIBUTING AREA FRACTION)
C     U           - DIRECT RUNOFF BETWEEN TIME T AND T + DELTAT [M]
C     Q           - OVERLAND FLOW (ROUTED DIRECT RUNOFF) BETWEEN TIME T AND T + DELTAT [M]
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
     2      RUNOFF(ILG),     TRUNOF(ILG),     ZPNDPRE(ILG)
C
C     * INPUT ARRAYS.
C
      REAL  fi    (ILG),    ZPLIM (ILG),     XSLOPE(ILG),
     1      xdrainh(ILG),    ksat(ILG),       TBARW (ILG,IG)
C 
C     * SOIL INFORMATION ARRAYS.
C
      REAL  DELZW (ILG,IG), THPOR (ILG,IG),  THLMIN(ILG,IG),   
     1      BI    (ILG,IG)

      INTEGER  ISAND (ILG,IG)
      real exav
C
C     * WORK ARRAYS.
C
      REAL  DIDRN (ILG,IG), BULK_FC(ILG,IG)
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

C-----kam------- Added variable for PDM------------------------------------
      REAL FSTR(ILG),CMIN(ILG),CMAX(ILG)
      REAL U(ILG),Q(ILG),B(ILG),CSTR(ILG), UMQ(ILG)
      REAL CMXMCMN,SMXMCMN,BP1,IBP1,SMAX,RNET,SUM_FSTR,T_FSTR

C-----------------------------------------------------------------------------------------
C     Return if no nml is expected to run in this cycle
      if(.not. any(iwf == 3)) return

C-----coefficients-------------------------------------------------------------------------
      c1 = 2.0/3.0
      c2 = 1.5 !3.0/2.0

C-----------------------------------------------------------------------------------------
C     parameter - will be used to compute xdrainh (the fractional change in horizontal 
C     conductivity in a depth change h0) in Vincent's new formula.
      h0 = 1.0

C-----kam---Initialization UMQ------------------------------------------
      UMQ = 0.0

C ----INITIALIZE WORKING VARIABLES - RUNOFF GENERATION-----
      U         = 0.0
      Q         = 0.0
      RNET      = 0.0
      SMAX      = 0.0
      BP1       = 0.0
      IBP1      = 0.0
      CMXMCMN   = 0.0
      SMXMCMN   = 0.0
      SUM_FSTR = 0.0

      do i = IL1,IL2

C-----------------------------------------------------------------------------------------
C     Cycle if using flat class
      if(iwf(i) /= 3) cycle

      if(fi(I) .gt. 0.0)then
                 B(i) = max(0.0, B(i))

C-----LIMIT THE MINIMUM STORAGE TO CMIN------------------
                 CMIN(i) = max(0.0,CMIN(i))
                 CMAX(i) = max(CMIN(i),CMAX(i))

C-----Storage for previous time step----------------------
                 ZPNDPRE(i) = max(CMIN(i),ZPNDPRE(i))

C-----REDUNDANT OPERATIONS---------------------------------
                 BP1 = B(i) + 1.0
                 IBP1 = 1.0 / BP1
                 CMXMCMN = CMAX(i) - CMIN(i)

C-----MAXIMUM STORAGE---------------------------------
        SMAX = IBP1 * (B(i) * CMIN(i) + CMAX(i))

C-----CRITICAL POND DEPTH CORRESPONDING TO ZPNDPRE--------
                 SMXMCMN = SMAX - CMIN(i)
                 CSTR(i) = CMIN(i) + CMXMCMN * (1.0 -
     1         max((SMAX - ZPNDPRE(i)) / SMXMCMN, 0.0) ** IBP1)

C-----CONTRIBUTING AREA FRACTION - DIAGNOSTIC ARRAY--------
              FSTR(i) = 1.0 - (1.0 - CSTR(i)/CMAX(i))**B(i)
              SUM_FSTR = SUM_FSTR + FSTR(i)
              T_FSTR = SUM_FSTR/i
C  ---CALCULATE THE NET CHANGE IN THE DEPTH OF THE PONDED---
C  ---WATER WITHIN DELTAT (BETWEEN TIME T AND T + DELTAT)---
                 RNET = ZPOND(i) - ZPNDPRE(i)

        if(RNET.gt.1.0E-06.and.ZPOND(i).gt.CMIN(i))then

C-----CALCULATE THE CRITICAL POND DEPTH AT T + DELTAT AND---
C-----LIMIT IT TO THE MAXIMUM PONDING DEPTH-----------------
                 CSTR(i) = min(CMAX(i), CSTR(i) + RNET)

                 ZPOND(i) = CMIN(i) + SMXMCMN * (1.0 -
     1                 ((CMAX(i) - CSTR(i)) / CMXMCMN) ** BP1)

C  ---calculate the depth of water available for overland flow----------------------
                 U(i) = MAX(0.0, RNET - (ZPOND(i) - ZPNDPRE(i)))
        endif

C-----calculate the flow velocity at the beginning of the timestep
C-----(based on kinematic wave velocity) - eqn (1) in notes on overland flow
            vel_t0(i) = (U(i)**c1)*sqrt(xslope(i))/(manning_n(i))

C-----calculate a normalized unconstrained overland flow to avoid numerical
C-----problems with a division of small U(i) values.
C-----eqn (29) in notes on overland flow
            nuc_dover(i) = -2*dd(i)*vel_t0(i)*delt

C-----CALCULATE OVERLAND FLOW AS DIRECT RUNOFF ROUTED TO THE END OF THE TILE
C-----eqn (30) in notes on overland flow
            Q(i) = U(i)*(1.0-1./((1.0-c1*nuc_dover(i))**c2))

C-----add overland flow to runoff and to the overall overland flow-----
            if(runoff(i) .gt. 1.0e-08) then
               trunof(i) = (trunof(i)*runoff(i)+(tpond(i)+tfrez)*
     1                      Q(i))/(runoff(i)+Q(i))
            endif
            runoff(i)    = runoff(i) + Q(i)

            if(Q(i) .gt. 1.0e-08)then
               tovrfl(i) = (tovrfl(i)*ovrflw(i)+(tpond(i)+tfrez)*
     1                      fi(i)*Q(i))/(ovrflw(i)+fi(i)*Q(i))
               ovrflw(i) = ovrflw(i) + fi(i)*Q(i)
            endif
      endif

C-------Book keeping regarding storage----------------------
C           It is necessary to check how much of the available excess of
C           (ZPOND-ZPONDPRE) after left as runoff. The calculation of
C           'Q' may not necessarily equal 'U' (by parameterization),
C           thus the 'ZPOND' must be updated to consider any excess NOT
C           lost by 'Q'.
                 ZPOND(i) = ZPOND(i) + U(i) - Q(i)
                 U(i) = Q(i)
                 ZPNDPRE(i) = ZPOND(i)

C--------BOOKKEEPING FOR ACTUAL RUNOFF CALCULATION----------
C           UMQ appears in PDMROF to negate its internal calculation of
C           'Q' (marked "inactive"), which isn't added to runoff/
C           overland flow and thus would be unaccounted for in the
C           balance. In this case, 'Q' is actually added to runoff (as
C           'U' is in PDMROF), so there should be no negation of this
C           value, and thus this calculation should equal zero. This
C           make-shift balance happens outside of the core CLASS
C           routines (in the CLASS driver/module).
                 UMQ(i)=U(i) - Q(i)
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
C        cycle if not using latflow
         if(iwf(i) /= 3) cycle

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
     2                ilg,il1,il2,3,delt)

C        ---------------------------------------------------------------------------------
C        loop through each element

         do i = il1,il2

C           ------------------------------------------------------------------------------
C           cycle if not using latflow
            if(iwf(i) /= 3) cycle

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
