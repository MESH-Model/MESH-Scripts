      SUBROUTINE GRINFL(IVEG,THLIQ,THICE,TBARW,BASFLW,TBASFL,
     1                  RUNOFF,TRUNOF,ZFAV,LZFAV,THLINV,QFG,
     2                  WLOST,FI,EVAP,R,TR,TPOND,ZPOND,DT,
     3                  ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                  DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                  THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                  THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                  ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                  DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                  THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     A                  THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                  IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                  NEND,ISIMP,IGDR,
     D                  IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
C
C     * OCT 18/11 - M.LAZARE.   PASS IN "IGDR" AS AN INPUT FIELD 
C     *                         (ORIGINATING IN CLASSB) TO
C     *                         GRDRAN AND WEND.
C     * APR 04/11 - D.VERSEGHY. MODIFY TEST IN 150 LOOP TO USE DELZ
C     *                         INSTEAD OF XDRAIN.
C     * JAN 06/09 - D.VERSEGHY. MODIFY DELZX AND ZBOTX OF BOTTOM LAYER;
C     *                         ADDITIONAL THLIQ CHECK IN 350 LOOP;
C     *                         PASS ADDITIONAL VARIABLES TO WEND.
C     * MAR 27/08 - D.VERSEGHY. MOVE VISCOSITY ADJUSTMENT TO WPREP.
C     * OCT 31/06 - R.SOULIS.   ADJUST GRKSAT FOR VISCOSITY OF WATER
C     *                         AND PRESENCE OF ICE; ADJUST THPOR FOR
C     *                         PRESENCE OF ICE.
C     * MAR 22/06 - D.VERSEGHY. UNCONDITIONALLY DEFINE VARIABLES FOR
C     *                         ALL "IF" STATEMENTS.
C     * SEP 28/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 400 LOOP.
C     * MAR 23/05 - D.VERSEGHY.R.SOULIS. PASS ADDITIONAL VARIABLES 
C     *                         TO GRDRAN AND WEND; PASS OUT ZFAV, 
C     *                         LZFAV, THLINV; CALCULATE GRKTLD 
C     *                         INTERNALLY; REVISE CALCULATION OF 
C     *                         THLINF.
C     * MAR 16/04 - D.VERSEGHY. TREAT FROZEN SOIL WATER AS ICE
C     *                         VOLUME RATHER THAN AS EQUIVALENT
C     *                         LIQUID WATER VOLUME.
C     * SEP 24/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 27/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE
C     *                         CALCULATIONS AGAINST ROUNDOFF ERRORS.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. PASS NEW VARIABLE IN FOR CALCULATION
C     *                         OF BASEFLOW.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * APR 17/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         BUG FIX: INITIALIZE FDT AND TFDT
C     *                         TO ZERO.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).            
C     * APR 11/89 - D.VERSEGHY. UPDATE SOIL LAYER TEMPERATURES AND 
C     *                         LIQUID MOISTURE CONTENTS FOR 
C     *                         INFILTRATING CONDITIONS (I.E.
C     *                         PONDED WATER OR RAINFALL OCCURRING
C     *                         WITHIN CURRENT TIMESTEP).
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,N
C  
C     * INPUT/OUTPUT FIELDS.
C
      REAL THLIQ (ILG,IG), THICE (ILG,IG), TBARW (ILG,IG)
C                        
      REAL BASFLW(ILG),    TBASFL(ILG),    RUNOFF(ILG),    TRUNOF(ILG),
     1     QFG   (ILG),    WLOST (ILG),    ZFAV  (ILG),    THLINV(ILG)
C
      INTEGER              LZFAV (ILG)
C
C     * INPUT FIELDS.
C
      REAL FI    (ILG),    EVAP  (ILG),    R     (ILG),    TR    (ILG), 
     1     TPOND (ILG),    ZPOND (ILG),    DT    (ILG)    
C
C     * WORK FIELDS (FOR ALL CALLED ROUTINES AS WELL).
C
      REAL ZMAT  (ILG,IGP2,IGP1)
C
      REAL WMOVE (ILG,IGP2),   TMOVE (ILG,IGP2),
     1     GRKSATF(ILG,IG),    THPORF(ILG,IG)
C
      REAL THLIQX(ILG,IGP1),   THICEX(ILG,IGP1),   TBARWX(ILG,IGP1),
     1     DELZX (ILG,IGP1),   ZBOTX (ILG,IGP1),   FDT   (ILG,IGP1),
     2     TFDT  (ILG,IGP1),   PSIF  (ILG,IGP1),   THLINF(ILG,IGP1),   
     3     GRKINF(ILG,IGP1),   THLMAX(ILG,IG),     THTEST(ILG,IG),     
     4     ZRMDR (ILG,IGP1),   FDUMMY(ILG,IGP1),   TDUMMY(ILG,IGP1),
     5     THLDUM(ILG,IG),     THIDUM(ILG,IG),     TDUMW (ILG,IG)
C
      REAL TRMDR (ILG),    ZF    (ILG),    FMAX  (ILG),    TUSED (ILG),
     1     RDUMMY(ILG),    ZERO  (ILG),    WEXCES(ILG),    FDTBND(ILG),    
     2     WADD  (ILG),    TADD  (ILG),    WADJ  (ILG),    TIMPND(ILG),    
     3     DZF   (ILG),    DTFLOW(ILG),    THLNLZ(ILG),    THLQLZ(ILG),    
     4     DZDISP(ILG),    WDISP (ILG),    WABS  (ILG)  
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG), THLRET(ILG,IG), THLMIN(ILG,IG),
     1     BI    (ILG,IG), PSISAT(ILG,IG), GRKSAT(ILG,IG), 
     3     THLRAT(ILG,IG), THFC  (ILG,IG),
     4     DELZW (ILG,IG), ZBOTW (ILG,IG), XDRAIN(ILG),  DELZ(IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL PSIINF,GRK,PSI
C
C     * VARIOUS INTEGER ARRAYS.
C
      INTEGER              ISAND (ILG,IG), IGRN  (ILG),    IGRD  (ILG),
     1                     IFILL (ILG),    IZERO (ILG),    LZF   (ILG),
     2                     NINF  (ILG),    IFIND (ILG),    ITER  (ILG),
     3                     NEND  (ILG),    ISIMP (ILG),    IGDR  (ILG)    
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C     * DETERMINE POINTS WHICH SATISFY CONDITIONS FOR THESE CALCULATIONS
C     * AND STORE THEM AS HAVING NON-ZERO VALUES FOR WORK ARRAY "IGRN".
C
      DO 50 I=IL1,IL2
          IF(FI(I).GT.0. .AND. 
     1       ISAND(I,1).GT.-4 .AND. DT(I).GT.0. .AND.
     2       (R(I).GT.0. .OR. ZPOND(I).GT.0.))                     THEN
              IGRN(I)=1
              RDUMMY(I)=0.
          ELSE
              IGRN(I)=0
          ENDIF
   50 CONTINUE
C
C     * ADJUST GRKSAT FOR VISCOSITY OF WATER AND PRESENCE OF ICE;
C     * ADJUST THPOR FOR PRESENCE OF ICE.
C     * INITIALIZATION; DETERMINATION OF SOIL HYDRAULIC CONDUCTIVITIES
C     * AND SOIL MOISTURE SUCTION ACROSS WETTING FRONT.
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          IF(IGRN(I).GT.0)                                         THEN
              THLIQX(I,J)=THLIQ(I,J)                                                      
              THICEX(I,J)=THICE(I,J)                                                      
              TBARWX(I,J)=TBARW(I,J)                                                      
              DELZX(I,J)=DELZW(I,J)                                                        
              ZBOTX(I,J)=ZBOTW(I,J)                                                        
              FDT (I,J)=0.0
              TFDT(I,J)=0.0
              IF(ISAND(I,J).GT.-3)                             THEN
                  GRKSATF(I,J)=GRKSAT(I,J)*(1.0-MAX(0.0,MIN(1.0,
     1                THICE(I,J)/THPOR(I,J))))**2
                  THPORF(I,J)=MAX((THPOR(I,J)-THICE(I,J)-0.00001),
     1                THLIQ(I,J),THLMIN(I,J))                
                  THLINF(I,J)=MAX(THLIQ(I,J),THLMIN(I,J),
     1                        THLRAT(I,J)*(THPOR(I,J)-
     2                        THICE(I,J)-0.00001))
                  GRKINF(I,J)=GRKSATF(I,J)*(THLINF(I,J)/THPORF(I,J))
     1                        **(2.*BI(I,J)+3.)
              ELSE
                  GRKSATF(I,J)=0.0
                  THPORF(I,J)=0.0
                  THLINF(I,J)=0.0
                  GRKINF(I,J)=0.0
              ENDIF
          ENDIF
  100 CONTINUE
C
      DO 150 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              IF(DELZW(I,IG).LT.DELZ(IG))                 THEN
                  THLIQX(I,IG+1)=0.0
                  THICEX(I,IG+1)=0.0
                  TBARWX(I,IG+1)=0.0
                  DELZX(I,IG+1)=0.0
                  THLINF(I,IG+1)=0.0
                  GRKINF(I,IG+1)=0.0
              ELSE
                  THLIQX(I,IG+1)=THLIQX(I,IG)                                                     
                  THICEX(I,IG+1)=THICEX(I,IG)                                                     
                  TBARWX(I,IG+1)=TBARWX(I,IG)                                                     
                  DELZX(I,IG+1)=999999.                                                        
                  THLINF(I,IG+1)=THLINF(I,IG)                                                     
                  GRKINF(I,IG+1)=GRKINF(I,IG)*XDRAIN(I)
              ENDIF
              ZBOTX (I,IG+1)=ZBOTX(I,IG)+DELZX(I,IG+1)
              FDT   (I,IG+1)=0.0
              TFDT  (I,IG+1)=0.0
          ENDIF
  150 CONTINUE
C                                                 
      DO 200 J=1,IG
      DO 200 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
             IF(THPOR(I,J).GT.0.0001)                     THEN
                 PSIINF=MAX(PSISAT(I,J)*(THLINF(I,J)/THPORF(I,J))**
     1                        (-BI(I,J)),PSISAT(I,J))                    
                 GRK=MIN(GRKSATF(I,J)*(THLIQ(I,J)/THPORF(I,J))**
     1                     (2.*BI(I,J)+3.),GRKSATF(I,J))                   
                 PSI=MAX(PSISAT(I,J)*(THLIQ(I,J)/THPORF(I,J))**
     1                     (-BI(I,J)),PSISAT(I,J))
             ELSE
                 PSIINF=PSISAT(I,J)
                 GRK=GRKSATF(I,J)
                 PSI=PSISAT(I,J)
             ENDIF
             IF(THLINF(I,J).GT.THLIQ(I,J))                  THEN 
                PSIF(I,J)=MAX(BI(I,J)*(GRKINF(I,J)*PSIINF-GRK*PSI)/
     1                    (GRKINF(I,J)*(BI(I,J)+3.)), 0.0) 
             ELSE                                                                    
                PSIF(I,J)=0.0                                                         
             ENDIF                                                                   
          ENDIF
  200 CONTINUE
C
      DO 250 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
             PSIF(I,IG+1)=PSIF(I,IG)      
             TRMDR(I)=DELT
          ELSE
             TRMDR(I)=0. 
          ENDIF
  250 CONTINUE
C    
      DO 300 J=1,IGP2
      DO 300 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN 
             WMOVE(I,J)=0.0                 
             TMOVE(I,J)=0.0
          ENDIF
  300 CONTINUE
C
C     * DETERMINE STARTING POSITION OF WETTING FRONT; INITIALIZATION
C     * FOR SATURATED INFILTRATION.
C
      DO 400 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN 
              IFILL(I)=1
              ZF(I)=0.0
              LZF(I)=1
              IF(ZPOND(I).GT.0. .OR. GRKINF(I,1).LT.1.0E-12)   THEN                               
                  NINF(I)=2
                  TMOVE(I,2)=TBARWX(I,1)
                  IFILL(I)=0
              ENDIF
              DO 350 J=1,IG
                  IF(THLIQ(I,J).GE.(THLINF(I,J)-1.0E-6) .AND.
     1                    THLIQ(I,J).GT.0.0001 .AND. LZF(I).EQ.J)  THEN
                      ZF(I)=ZBOTW(I,J)
                      LZF(I)=J+1
                      NINF(I)=J+2
                      WMOVE(I,J+1)=THLIQ(I,J)*DELZW(I,J)
                      TMOVE(I,J+1)=TBARWX(I,J)
                      TMOVE(I,J+2)=TBARWX(I,J+1)
                      IFILL(I)=0
                  ENDIF
  350         CONTINUE
          ELSE
              IFILL(I)=0
              LZF(I)=0
              NINF(I)=0
          ENDIF
  400 CONTINUE
C
C     * IF SATURATED INFILTRATION CONDITIONS ARE NOT PRESENT AT ONCE
C     * (IFILL=1), CALL "WFILL" TO DO PROCESSING FOR PERIOD OF
C     * UNSATURATED INFILTRATION.
C                                                            
      CALL WFILL(WMOVE,TMOVE,LZF,NINF,ZF,TRMDR,R,TR,
     1           PSIF,GRKINF,THLINF,THLIQX,TBARWX,
     2           DELZX,ZBOTX,DZF,TIMPND,WADJ,WADD,
     3           IFILL,IFIND,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
C
C     * CALL "WFLOW" TO DO PROCESSING FOR PERIOD OF SATURATED
C     * INFILTRATION.
C
      CALL WFLOW(WMOVE,TMOVE,LZF,NINF,TRMDR,TPOND,ZPOND,
     1           R,TR,EVAP,PSIF,GRKINF,THLINF,THLIQX,TBARWX,
     2           DELZX,ZBOTX,FMAX,ZF,DZF,DTFLOW,THLNLZ,
     3           THLQLZ,DZDISP,WDISP,WABS,ITER,NEND,ISIMP,
     4           IGRN,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
C
C     * RECALCULATE TEMPERATURES AND LIQUID MOISTURE CONTENTS OF
C     * SOIL LAYERS FOLLOWING INFILTRATION.
C
      CALL WEND(THLIQX,THICEX,TBARWX,ZPOND,TPOND,
     1          BASFLW,TBASFL,RUNOFF,TRUNOF,FI,
     2          WMOVE,TMOVE,LZF,NINF,TRMDR,THLINF,DELZX,
     3          ZMAT,ZRMDR,FDTBND,WADD,TADD,FDT,TFDT,
     4          THLMAX,THTEST,THLDUM,THIDUM,TDUMW,
     5          TUSED,RDUMMY,ZERO,WEXCES,XDRAIN,
     6          THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     7          THFC,DELZW,ISAND,IGRN,IGRD,IGDR,IZERO,
     8          IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
C
      DO 800 J=1,IG
      DO 800 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              THLIQ(I,J)=THLIQX(I,J)                                                      
              THICE(I,J)=THICEX(I,J)                                                      
              TBARW(I,J)=TBARWX(I,J)      
          ENDIF                                                
  800 CONTINUE
C
      DO 850 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. LZF(I).LT.IG+1)                     THEN
              ZFAV(I)=(ZF(I)+MAX(ZBOTW(I,LZF(I))-DELZW(I,LZF(I)),0.0))/
     1                2.0
              LZFAV(I)=LZF(I)
              THLINV(I)=THLINF(I,LZF(I))
          ELSE
              ZFAV(I)=0.0
              LZFAV(I)=0
              THLINV(I)=0.0
          ENDIF                                                
  850 CONTINUE
C
C     * IF TIME REMAINS IN THE CURRENT MODEL STEP AFTER INFILTRATION
C     * HAS CEASED (TRMDR>0), CALL "GRDRAN" TO CALCULATE WATER FLOWS
C     * BETWEEN LAYERS FOR THE REMAINDER OF THE TIME STEP.
C
      CALL GRDRAN(IVEG,THLIQ,THICE,TBARW,FDUMMY,TDUMMY,BASFLW,
     1            TBASFL,RUNOFF,TRUNOF,QFG,WLOST,FI,EVAP,ZERO,ZERO,
     2            TRMDR,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     3            BI,PSISAT,GRKSAT,THFC,DELZW,XDRAIN,ISAND,IZERO,
     4            IZERO,IGRD,IGDR,
     5            IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
C
      RETURN                                                                      
      END       
