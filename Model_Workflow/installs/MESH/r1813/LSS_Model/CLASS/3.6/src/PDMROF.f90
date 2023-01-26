SUBROUTINE PDMROF (IWF,    ILG,    IL1,    IL2,    FI,       &
                   SPRE,   S,      FSTR,   TPOND,            &
                   OVRFLW, TOVRFL, RUNOFF, TRUNOF, TFREZ,    &
                   CMIN,   CMAX,   B,      K1,     K2,       &
                   UM1,    QM1,    QM2,    UMQ,    DELT)

!----------------------------------------------------------------------
!
!                  NOV 12/2012 - MULUNEH A. MEKONNEN
!
!----------------------------------------------------------------------
!     CONTRIBUTORS TO THE PDMROF ALGORITHM ARE LISTED BELOW:
!     1. CHRIS SPENCE PRESENTED THE IDEA AND BASIC FLOW CHART OF
!        REPRESENTING CONTRIBUTING AREA AS A FUNCTION OF
!        THRESHOLD-BASED BASIN STORAGE,
!     2. ALAIN PIETRONIRO ADVISED TO DESIGN A SIMPLE AND YET
!        PRACTICAL APPROACH OF DEALING WITH THE CONTRIBUTING
!        AREA ISSUE. THE APPROACH INCLUDES TO FIRST START WITH
!        A SENSITIVITY ANALYSIS AND THEN TO TEST ANY EVENTUAL
!        ALGORITHM WITH THE 2011 ASSINIBOINE FLOODING,
!     3. HOWARD WHEATER INTRODUCED THE IDEA OF PROBABILITY
!        DISTRIBUTION MODEL, AND
!     4. MULUNEH A. MEKONNEN, BRUCE DAVISON AND ANDREW IRESON
!        REFINED THE PDMROF ALGORITHM TO ITS CURRENT FORM AND
!        ALSO MADE IT COMPATIBLE WITH THE MESH MODEL.

!----------------------------------------------------------------------

!     THE PDMROF ALGORITHM ASSUMES A GIVEN TILE (GRU) AS A SERIES OF
!     POTHOLES WHOSE PONDING CAPACITY CAN BE REPRESENTED USING THE
!     PARETO PROBABILITY DISTRIBUTION (MOORE, 2007, 1985) WITH THREE
!     PARAMETERS: CMIN, CMAX AND B.

!     THE BASIC IDEA IS THAT POINT RUNOFF GENERATION IS DIRECTLY RELATED
!     TO THE AVERAGE DEPTH OF THE PONDING WATER OVER THE TILE SURFACE.
!     POTHOLES WITH SMALLER PONDING CAPACITY THAN A CERTAIN DYNAMIC
!     CRITICAL DEPTH (WHICH IS A FUNCTION OF THE AVERAGE DEPTH OF PONDED
!     WATER IN THE TILE) WILL GENERATE DIRECT RUNOFF. THE FRACTION
!     OF THIS POTHOLES CONSTITUTES THE FRACTION OF THE CONTRIBUTING AREA
!     AND HENCE THE TILE CONTRIBUTING AREA DYNAMICALLY CHANGES AS THE
!     AVERAGE DEPTH OF THE PONDED WATER IN THE TILE CHANGES WITH TIME.

!     THE DIRECT RUNOFF FROM THE CONTRIBUTING POTHOLES IS THEN ROUTED
!     VIA SURFACE STORAGE. THE SURFACE STORAGE COMPONENT USED HERE IS
!     A CASCADE OF TWO LINEAR RESERVOIRS, WITH TIME CONSTANTS K1 AND
!     K2, EXPRESSED AS AN EQUIVALENT SECOND ORDER TRANSFER FUNCTION
!     MODEL CONSTRAINED TO PRESERVE CONTINUITY (O'CONNOR, 1982).

!     THE PDMROF ALGORITHM IS INITIATED AS A FIRST ATTEMPT OF IMPROVING
!     PFRA'S STATIC STREAM FLOW CONTRIBUTING/NON-CONTRIBUTING AREA
!     REPRESENTATION. PDMROF IN ITS CURRENT VERSION DOESN'T HANDLE
!     HYSTERESIS EFFECTS AND CONNECTIVITY ISSUES.

!----------------------------------------------------------------------
!     REFERENCES:
!	1. MOORE, R.J., 2007. THE PDM RAINFALL-RUNOFF MODEL.
! 	   HYDROLOGY AND EARTH SYSTEM SCIENCES, VOL. 11, PP. 483-499

!	2. MOORE, R.J., 1985. THE PROBABILITY-DISTRIBUTED PRINCIPLE AND
!          RUNOFF PRODUCTION AT POINT AND BASIN SCALES.
! 	   HYDROLOGY AND EARTH SYSTEM SCIENCES, VOL. 30, PP. 273-297

!       3. KIERAN M. O'CONNOR, 1982. DERIVATION OF DISCRETELY COINCIDENT
!          FORMS OF CONTINUOUS LINEAR TIME-INVARIANT MODELS USING THE
!          TRANSFER FUNCTION APPROACH.
!          JOURNAL OF HYDROLOGY, VOL. 59, PP. 1-48.

!----------------------------------------------------------------------
!     DEFINITIONS
!----------------------------------------------------------------------
! COEFFICIENTS OF PARETO DISTRIBUTION FOR DIRECT RUNOFF GENERATION
!     CMAX        - MAXIMUM POTHOLE PONDING CAPACITY [M]
!     CMIN        - MINIMUM POTHOLE PONDING CAPACITY [M]
!                  (MINIMUM AMOUNT NOT AVAILABLE FOR OVERLAND FLOW)
!     B           - SHAPE FACTOR OF THE PARETO DISTRIBUTION FUNCTION

!----------------------------------------------------------------------
! TIME CONSTANTS FOR THE TWO CASCADING LINEAR RESERVOIRS FOR OVERLAND
! OR STORAGE ROUTING
!     K1 [HR] AND K2 [HR]

!----------------------------------------------------------------------
! COEFFICIENTS FOR AN EQUIVALENT SECOND ORDER TRANSFER FUNCTION MODEL
! CONSTRAINED TO PRESERVE CONTINUITY - DERIVED FROM K1 AND K2
!     DELTA1
!     DELTA2
!     OMEGA0
!     OMEGA1

!----------------------------------------------------------------------
!     ILG         - TOTAL NUMBER OF ELEMENTS
!     IL1         - STARTING INDEX OF ACTIVE ELEMENT
!     IL2         - FINAL INDEX OF ACTIVE ELEMENT
!     FI          - FRACTIONAL COVERAGE OF SUBAREA IN QUESTION ON
!                   MODELLED AREA
!     SPRE        - PREVIOUS TILE PONDING STORAGE [M]
!     S           - CURRENT TILE PONDING STORAGE  [M]
!     SMAX        - MAXIMUM TILE PONDING STORAGE  [M]
!     CSTR        - CRITICAL PONDING DEPTH FOR A GIVEN STORAGE [M]
!     FSTR        - DISTRIBUTION FUNCTION VALUE AT CRITICAL PONDING DEPTH []
!                 - THIS IS THE CONTRIBUTING AREA FRACTION
!     TPOND       - TEMPERATURE OF SURFACE PONDED WATER [C]
!     OVRFLW      - OVERLAND FLOW FROM TOP OF SOIL COLUMN [M]
!     TOVRFL      - TEMPERATURE OF OVERLAND FLOW [C]
!     RUNOFF      - TOTAL RUNOFF [M]
!     TRUNOF      - TEMPERATURE OF TOTAL RUNOFF [C]
!     DELT        - TIME STEP [S]
!     TFREZ       - FREEZING TEMPERATURE OF WATER [K]
!     U           - DIRECT RUNOFF BETWEEN TIME T AND T + DELTAT [M]
!     UM1         - DIRECT RUNOFF BETWEEN TIME T - DELTAT AND T [M]
!     Q           - OVERLAND FLOW (ROUTED DIRECT RUNOFF) BETWEEN
!                   TIME T AND T + DELTAT [M]
!     QM1         - OVERLAND FLOW (ROUTED DIRECT RUNOFF) BETWEEN
!                   TIME T AND T - DELTAT [M]
!     QM2         - OVERLAND FLOW (ROUTED DIRECT RUNOFF) BETWEEN
!                   TIME T - DELTAT AND T - 2 * DELTAT [M]

!----------------------------------------------------------------------

IMPLICIT NONE

!     * INPUT SCALARS
INTEGER ILG, IL1, IL2

!     * INPUT ARRAYS
INTEGER IWF(ILG)
REAL  FI(ILG), CMIN(ILG), CMAX(ILG), B(ILG), K1(ILG), K2(ILG)
REAL  TFREZ,   DELT,      DELTHR,    KMIN,   K1M,     K2M

!     * INPUT/OUTPUT ARRAYS
REAL  SPRE    (ILG), TPOND (ILG), S     (ILG), FSTR (ILG),             &
      OVRFLW  (ILG), TOVRFL(ILG), RUNOFF(ILG), TRUNOF(ILG)

!     * OUTPUT ARRAYS
REAL  UM1     (ILG), QM1   (ILG), QM2   (ILG), UMQ   (ILG)

!     * WORKING SCALARS
INTEGER I

!     * WORKING ARRAYS - RUNOFF GENERATION
REAL  CMXMCMN, SMXMCMN,  BP1,  IBP1, SMAX, RNET, CSTR
REAL  U(ILG)

!     * WORKING ARRAYS - ROUTING
REAL  K2MK1,     DELTK1,  DELTK2,  DEL1STR, DEL2STR, DEL1STRM1,        &  
      DEL2STRM1, DELTA1,  DELTA2,  OMEGA0,  OMEGA1
REAL  Q(ILG)

!----------------------------------------------------------------------
!     RETURN IF NO NML IS EXPECTED TO RUN IN THIS CYCLE
      IF(.NOT. ANY(IWF == 2)) RETURN

!----------------------------------------------------------------------
      UMQ       = 0.0

!     DELT VALUE IN HOUR
      DELTHR = DELT / 3600.0

!     INITIALIZE WORKING VARIABLES - RUNOFF GENERATION
      U         = 0.0
      RNET      = 0.0
      SMAX      = 0.0
      BP1       = 0.0
      IBP1      = 0.0
      CMXMCMN   = 0.0
      SMXMCMN   = 0.0
      CSTR      = 0.0

!     INITIALIZE WORKING VARIABLES - RUNOFF GENERATION
!     CURRENTLY TURNED OFF
      Q         = 0.0
      K2MK1     = 0.0
      DELTK1    = 0.0
      DELTK2    = 0.0
      DEL1STR   = 0.0
      DEL2STR   = 0.0
      DEL1STRM1 = 0.0
      DEL2STRM1 = 0.0

!----------------------------------------------------------------------
!     LOOP THROUGH EACH ACTIVE ELEMENT
      DO I = IL1, IL2

!        CYCLE IF NOT USING PDMROF
         IF(IWF(I) /= 2) CYCLE

!        --------------------------------------------------------------
!        DO THE COMPUTATION IF VEGETATION CATEGORY EXISTS IN THE TILE
!        --------------------------------------------------------------
         IF(FI(I) .GT. 0.0)THEN

!              --------------------------------------------------------
!                           PART I - RUNOFF GENERATION
!              --------------------------------------------------------

!              --------------------------------------------------------
!              AVOID UNREALISTIC SITUATION WHERE THE USER OR AN
!              AUTOCALIBRATION ALGORITHM MAY SPECIFY:
!              1. A NEGATIVE SHAPE COEFFICIENT (B VALUE), 
!              2. A NEGATIVE STORAGE CAPACITY, AND
!              3. A MAXIMUM STORAGE VALUE SMALLER THAN THE MINIMUM
!                 STORAGE
!              --------------------------------------------------------
               B(I)    = MAX(0.0, B(I))
               CMIN(I) = MAX(0.0, CMIN(I))
               CMAX(I) = MAX(CMIN(I), CMAX(I))

!              --------------------------------------------------------
!              LIMIT THE MINIMUM STORAGE TO CMIN
!              --------------------------------------------------------
               SPRE(I) = MAX(CMIN(I), SPRE(I))

!              --------------------------------------------------------
!              TIME SAVING OPERATIONS THAT WOULD BE REPEATED OFTEN OTHERWISE
!              --------------------------------------------------------
               BP1     = B(I) + 1.0
               IBP1    = 1.0 / BP1
               CMXMCMN = CMAX(I) - CMIN(I)

!              --------------------------------------------------------
!              MAXIMUM STORAGE
!              --------------------------------------------------------
               SMAX = IBP1 * (B(I) * CMIN(I) + CMAX(I))

!              --------------------------------------------------------
!              CRITICAL POND DEPTH CORRESPONDING TO SPRE
!              --------------------------------------------------------
               SMXMCMN = SMAX - CMIN(I)
               CSTR    = CMIN(I) + CMXMCMN * (1.0 -                   &
                         max((SMAX - SPRE(I)) / SMXMCMN, 0.0) ** IBP1)

!              --------------------------------------------------------
!              CONTRIBUTING AREA FRACTION - DIAGNOSTIC ARRAY
!              --------------------------------------------------------
               FSTR(I) = 1.0 - (1.0 - CSTR/CMAX(I))**B(I)

!              --------------------------------------------------------
!              CALCULATE THE NET CHANGE IN THE DEPTH OF THE PONDED
!              WATER WITHIN DELTAT (BETWEEN TIME T AND T + DELTAT)
!              --------------------------------------------------------
               RNET = S(I) - SPRE(I)

!              --------------------------------------------------------
!              FOR A WET PERIOD AND IF THE PONDED WATER EXCEEDS THE
!              MINIMUM STORAGE BELOW WHICH RUNOFF IS NOT POSSIBLE
!              --------------------------------------------------------
               IF(RNET .GT. 1.0E-06 .AND. S(I) .GT. CMIN(I))THEN


!                 -----------------------------------------------------
!                 CALCULATE THE CRITICAL POND DEPTH AT T + DELTAT AND
!                 LIMIT IT TO THE MAXIMUM PONDING DEPTH
!                 -----------------------------------------------------
                  CSTR = MIN(CMAX(I), CSTR + RNET)
                  
!                 -----------------------------------------------------
!                 CALCULATE THE NEW DEPTH OF THE PONDED WATER, S AT
!                 T + DELTAT. THIS TAKES INTO ACCOUNT THE LOSS DUE TO
!                 DIRECT RUNOFF WITHIN DELTAT (T TO T + DELTAT).
!                 NOTE THAT BECAUSE CSTR IS BOUND NOT TO EXCEED CMAX
!                 S WILL NOT EXCEED SMAX.
!                 -----------------------------------------------------
                  S(I) = CMIN(I) + SMXMCMN * (1.0 -                   &
                         ((CMAX(I) - CSTR) / CMXMCMN) ** BP1)

!                 -----------------------------------------------------
!                 CALCULATE DIRECT RUNOFF
!                 -----------------------------------------------------
                  U(I) = MAX(0.0, RNET - (S(I) - SPRE(I)))

               ENDIF

!              --------------------------------------------------------
!                           PART II - ROUTING - CURRENTLY TURNED OFF 
!              --------------------------------------------------------

!              --------------------------------------------------------
!              AVOID UNREALISTIC SITUATION WHERE THE USER OR AN
!              AUTOCALIBRATION ALGORITHM MAY SPECIFY ZERO OR NEGATIVE
!              VALUES FOR ROUTING COEFFICIENTS
!              --------------------------------------------------------
               K1(I) = MAX(1.0E-10, K1(I))
               K2(I) = MAX(1.0E-10, K2(I))

!              --------------------------------------------------------
!              REDUNDANT OPERATIONS
!              --------------------------------------------------------
               DELTK1 = DELTHR / K1(I)
               DELTK2 = DELTHR / K2(I)

!              --------------------------------------------------------
!              COMPUTE DELTA1, DELTA2, OMEGA0 AND OMEGA1
!              --------------------------------------------------------
               DEL1STR = EXP(-DELTK1)
               DEL2STR = EXP(-DELTK2)
               DELTA1  = -DEL1STR - DEL2STR
               DELTA2  = DEL1STR * DEL2STR

               IF(K1(I) .EQ. K2(I))THEN
                  OMEGA0    = 1.0 - (1.0 + DELTK1) * DEL1STR
                  OMEGA1    = (DEL1STR - 1.0 + DELTK1) * DEL1STR
               ELSE
                  DEL1STRM1 = DEL1STR - 1.0
                  DEL2STRM1 = DEL2STR - 1.0
                  K2MK1     = K2(I) - K1(I)
                  OMEGA0    = (K1(I) * DEL1STRM1 - K2(I) * DEL2STRM1)  &
                               / K2MK1
                  OMEGA1    = (K2(I) * DEL2STRM1 * DEL1STR -           &
                               K1(I) * DEL1STRM1 * DEL2STR) / K2MK1
               ENDIF

!              --------------------------------------------------------
!              CALCULATE OVERLAND FLOW AS DIRECT RUNOFF ROUTED TO THE
!              END OF THE TILE
!              --------------------------------------------------------
               Q(I) = Max(0.0, -DELTA1 * QM1(I) - DELTA2 * QM2(I) +    &
                                OMEGA0 * U(I) + OMEGA1 * UM1(I))


!              --------------------------------------------------------
!              CALCULATE TOTAL OVERLAND FLOW AND TOTAL RUNOFF
!              NOTE THAT THE ACTUAL RUNOFF CONTRIBUTION IS Q NOT U!!!
!              --------------------------------------------------------
               IF(RUNOFF(I) .GT. 1.0E-08)                              &
                  TRUNOF(I) = (TRUNOF(I) * RUNOFF(I) +                 &
                              (TPOND(I) + TFREZ) * U(I)) /             &
                              (RUNOFF(I) + U(I))

               RUNOFF(I)    = RUNOFF(I) + U(I)

               IF(U(I) .GT. 1.0E-08)                                   &
                  TOVRFL(I) = (TOVRFL(I) * OVRFLW(I) +                 &
                              (TPOND(I) + TFREZ) * FI(I) * U(I)) /     &
                              (OVRFLW(I) + FI(I) * U(I))

               OVRFLW(I) = OVRFLW(I) + FI(I) * U(I)

         ENDIF

!        --------------------------------------------------------------
!        BOOKKEEPING FOR NEXT TIME STEP - FOR RUNOFF GENERATION
!        --------------------------------------------------------------
         SPRE(I) = S(I)

!        --------------------------------------------------------------
!        BOOKKEEPING FOR NEXT TIME STEP -  FOR ROUTING
!        --------------------------------------------------------------
         QM2(I) = QM1(I)
         QM1(I) = Q(I)
         UM1(I) = U(I)

!        --------------------------------------------------------------
!        BOOKKEEPING FOR ACTUAL RUNOFF CALCULATION
!        --------------------------------------------------------------
         UMQ(I) = U(I) - Q(I)

      ENDDO

END
