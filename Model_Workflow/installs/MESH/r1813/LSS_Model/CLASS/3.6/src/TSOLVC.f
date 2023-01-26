      SUBROUTINE TSOLVC(ISNOW,FI,
     1                 QSWNET,QSWNC,QSWNG,QLWOUT,QLWOC,QLWOG,QTRANS,
     2                 QSENS,QSENSC,QSENSG,QEVAP,QEVAPC,QEVAPG,EVAPC,
     3                 EVAPG,EVAP,TCAN,QCAN,TZERO,QZERO,GZERO,QMELTC,
     4                 QMELTG,RAICAN,SNOCAN,CDH,CDM,RIB,TAC,QAC,
     5                 CFLUX,FTEMP,FVAP,ILMO,UE,H,QFCF,QFCL,HTCC,
     6                 QSWINV,QSWINI,QLWIN,TPOTA,TA,QA,VA,VAC,PADRY,
     7                 RHOAIR,ALVISC,ALNIRC,ALVISG,ALNIRG,TRVISC,TRNIRC,
     7                 FSVF,CRIB,CPHCHC,CPHCHG,CEVAP,TADP,TVIRTA,RC,
     8                 RBCOEF,ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,
     A                 FCOR,GCONST,GCOEFF,TGND,TRSNOW,FSNOWC,FRAINC,
     B                 CHCAP,CMASS,PCPR,IWATER,IEVAP,ITERCT,
     C                 ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                 TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                 RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,
     F                 TPOTG,RESID,
     G                 TCANO,WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,
     H                 IEVAPC,TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,A,B,
     I                 LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                 AILCG,FCANC,CO2CONC,RMATCTEM,
     K                 THLIQ,FIELDSM,WILTSM,ISAND,IG,COSZS,PRESSG,
     L                 XDIFFUS,ICTEM,IC,CO2I1,CO2I2,
     M                 ICTEMMOD,SLAI,FCANCMX,L2MAX,
     N                 NOL2PFTS,CFLUXV,ANVEG,RMLVEG)
C
C     * NOV 11/11 - M.LAZARE. - INCORPORATES CTEM. THIS INVOLVES
C     *                         SEVERAL CHANGES AND NEW OUTPUT ROUTINES.
C     *                         QSWNVC IS PROMOTED TO A WORK ARRAY
C     *                         SINCE PASSED AS INPUT TO THE NEW CALLED
C     *                         PHOTOSYNTHESIS ROUTINE "PHTSYN3". THE
C     *                         CTEM CANOPY RESISTANCE COMING OUT OF
C     *                         THIS ROUTINE, "RCPHTSYN" IS STORED INTO
C     *                         THE USUAL "RC" ARRAY AS LONG AS THE
C     *                         BONE-DRY SOIL FLAG IS NOT SET (RC=1.E20).
C     *                         WE ALSO HAVE TO PASS "TA" THROUGH FROM
C     *                         CLASST. FINALLY, "ISAND", "FIELDSM" AND
C     *                         "WILTSM" ARE PASSED THROUGH TO PHTSYN3
C     *                         FOR CTEM.
C     * OCT 14/11 - D.VERSEGHY. FOR POST-ITERATION CLEANUP WITH N-R SCHEME,
C     *                         REMOVE CONDITION INVOLVING LAST ITERATION
C     *                         TEMPERATURE.
C     * DEC 07/09 - D.VERSEGHY. RESTORE EVAPOTRANSPIRATION WHEN 
C     *                         PRECIPITATION IS OCCURRING; ADD EVAPC
C     *                         TO EVAP WHEN DEPOSITION OF WATER ON
C     *                         CANOPY IS OCCURRING.
C     * MAR 13/09 - D.VERSEGHY. REPLACE COMMON BLOCK SURFCON WITH CLASSD2;
C     *                         REVISED CALL TO FLXSURFZ.
C     * JAN 20/09 - D.VERSEGHY. CORRECT CALCULATION OF TPOTG.
C     * JAN 06/09 - E.CHAN/D.VERSEGHY. SET UPPER LIMIT FOR TSTEP IN
C     *                         N-R ITERATION SCHEME.
C     * FEB 26/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS; REMOVE
C     *                         "ILW" SWITCH; SUPPRESS WATER VAPOUR FLUX
C     *                         IF PRECIPITATION IS OCCURRING.
C     * FEB 19/07 - D.VERSEGHY. UPDATE CANOPY WATER STORES IN THIS 
C     *                         ROUTINE INSTEAD OF CANADD FOR CASES
C     *                         OF WATER DEPOSITION.
C     * MAY 17/06 - D.VERSEGHY. ADD IL1 AND IL2 TO CALL TO FLXSURFZ;
C     *                         REMOVE JL FROM CALL TO DRCOEF.
C     * APR 15/05 - D.VERSEGHY. SUBLIMATION OF INTERCEPTED SNOW TAKES
C     *                         PLACE BEFORE EVAPORATION OF INTERCEPTED
C     *                         RAIN.
C     * APR 14/05 - Y.DELAGE.   REFINEMENTS TO N-R ITERATION SCHEME.
C     * FEB 23/05 - D.VERSEGHY. INCORPORATE A SWITCH TO USE EITHER THE
C     *                         BISECTION ITERATION SCHEME WITH CANOPY
C     *                         AIR PARAMETRIZATION, OR THE NEWTON-
C     *                         RAPHSON ITERATION SCHEME WITH MODIFIED
C     *                         ZOH.
C     * JAN 31/05 - Y.DELAGE.   USE THE CANOPY AIR RESISTANCE TO CALCULATE A 
C     *                         ROUGHNESS LENGTH FOR TEMPERATURE AND HUMIDITY.
C     *                         REPLACE SECANT METHOD BY NEWTON-RAPHSON SCHEME
C     *                         FOR BOTH ITERATION LOOPS.
C     *                         LIMIT NUMBER OF ITERATIONS (ITERMX) TO 5 AND
C     *                         APPLY CORRECTIONS IF RESIDUE REMAINS.
C     * JAN 12/05 - P.BARTLETT/D.VERSEGHY. MODIFICATION TO CALCULATION
C     *                         OF RBINV; ALLOW SUBLIMATION OF FROZEN
C     *                         WATER ONLY ONTO SNOW-COVERED PORTION
C     *                         OF CANOPY.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS
C     *                         FROM ROUNDOFF ERRORS.
C     * NOV 07/02 - Y.DELAGE/D.VERSEGHY. NEW CALL TO FLXSURFZ; VIRTUAL
C     *                         AND POTENTIAL TEMPERATURE CORRECTIONS.
C     * NOV 01/02 - P.BARTLETT. MODIFICATIONS TO CALCULATIONS OF QAC
C     *                         AND RB.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * MAR 28/02 - D.VERSEGHY. STREAMLINED SUBROUTINE CALL.
C     * MAR 10/02 - M.LAZARE.   VECTORIZE LOOP 650 BY SPLITTING INTO TWO.
C     * JAN 18/02 - P.BARTLETT/D.VERSEGHY. NEW "BETA" FORMULATION FOR 
C     *                         BARE SOIL EVAPORATION BASED ON LEE AND
C     *                         PIELKE.
C     * APR 11/01 - M.LAZARE.   SHORTENED "CLASS2" COMMON BLOCK.
C     * OCT 06/00 - D.VERSEGHY. CONDITIONAL "IF" IN ITERATION SEQUENCE
C     *                         TO AVOID DIVIDE BY ZERO.
C     * DEC 16/99 - A.WU/D.VERSEGHY. REVISED CANOPY TURBULENT FLUX 
C     *                              FORMULATION: ADD PARAMETRIZATION
C     *                              OF CANOPY AIR TEMPERATURE.
C     * DEC 07/99 - A.WU/D.VERSEGHY. NEW SOIL EVAPORATION FORMULATION.
C     * JUL 24/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         REPLACE BISECTION METHOD IN SURFACE 
C     *                         TEMPERATURE ITERATION SCHEME WITH 
C     *                         SECANT METHOD FOR FIRST TEN ITERATIONS.
C     *                         PASS QZERO,QA,ZOMS,ZOHS TO REVISED
C     *                         DRCOEF (ZOMS AND ZOHS ALSO NEW WORK ARRAYS
C     *                         PASSED TO THIS ROUTINE).
C     * JUN 20/97 - D.VERSEGHY. PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.  ALSO, PASS SWITCH "ILW"
C     *                         THROUGH SUBROUTINE CALL, SPECIFYING 
C     *                         WHETHER QLWIN REPRESENTS INCOMING
C     *                         (ILW=1) OR NET (ILW=2) LONGWAVE
C     *                         RADIATION ABOVE THE GROUND.
C     * NOV 30/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         NEW DRAG COEFFICIENT AND RELATED FIELDS,
C     *                         NOW DETERMINED IN ROUTINE "DRCOEF".
C     * OCT 04/94 - D.VERSEGHY. CHANGE "CALL ABORT" TO "CALL XIT" TO
C     *                         ENABLE RUNNING ON PCS.
C     * JAN 24/94 - M.LAZARE.   UNFORMATTED I/O COMMENTED OUT IN LOOPS
C     *                         200 AND 600. 
C     * JUL 29/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         ADD TRANSMISSION THROUGH SNOWPACK TO
C     *                         "QSWNET" FOR DIAGNOSTIC PURPOSES. 
C     * OCT 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. ITERATIVE TEMPERATURE CALCULATIONS 
C     *                         FOR VEGETATION CANOPY AND UNDERLYING
C     *                         SURFACE.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ISNOW,ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,I,N
C
      INTEGER NUMIT,IBAD,NIT,ITERMX
C
C     * OUTPUT ARRAYS.
C
      REAL QSWNET(ILG),    QSWNC (ILG),    QSWNG (ILG),    QLWOUT(ILG),   
     1     QLWOC (ILG),    QLWOG (ILG),    QTRANS(ILG),    QSENS (ILG),    
     2     QSENSC(ILG),    QSENSG(ILG),    QEVAP (ILG),    QEVAPC(ILG),    
     3     QEVAPG(ILG),    EVAPC (ILG),    EVAPG (ILG),    TCAN  (ILG),   
     4     QCAN  (ILG),    TZERO (ILG),    QZERO (ILG),    GZERO (ILG),    
     5     QMELTC(ILG),    QMELTG(ILG),    RAICAN(ILG),    SNOCAN(ILG),    
     6     CDH   (ILG),    CDM   (ILG),    RIB   (ILG),    TAC   (ILG),    
     7     QAC   (ILG),    CFLUX (ILG),    FTEMP (ILG),    FVAP  (ILG),    
     8     ILMO  (ILG),    UE    (ILG),    H     (ILG),
     9     QFCF  (ILG),    QFCL  (ILG),    HTCC  (ILG),    EVAP  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    QSWINV(ILG),    QSWINI(ILG),    QLWIN (ILG),    
     1     TPOTA (ILG),    TA    (ILG),    QA    (ILG),    VA    (ILG), 
     2     VAC   (ILG),    PADRY (ILG),    RHOAIR(ILG),    ALVISC(ILG), 
     3     ALNIRC(ILG),    ALVISG(ILG),    ALNIRG(ILG),    TRVISC(ILG),
     4     TRNIRC(ILG),    FSVF  (ILG),    CRIB  (ILG),    CPHCHC(ILG), 
     5     CPHCHG(ILG),    CEVAP (ILG),    TADP  (ILG),    TVIRTA(ILG),
     6     RC    (ILG),    RBCOEF(ILG),    ZOSCLH(ILG),    ZOSCLM(ILG),
     7     ZRSLFH(ILG),    ZRSLFM(ILG),    ZOH   (ILG),    ZOM   (ILG),
     8     FCOR  (ILG),    GCONST(ILG),    GCOEFF(ILG),    TGND  (ILG),    
     9     TRSNOW(ILG),    FSNOWC(ILG),    FRAINC(ILG),    
     A     CHCAP (ILG),    CMASS (ILG),    PCPR  (ILG)
C
      INTEGER              IWATER(ILG),    IEVAP (ILG),    
     1                     ITERCT(ILG,6,50)
C
C     * ARRAYS FOR CTEM.
C
      REAL AILCG(ILG,ICTEM),     FCANC(ILG,ICTEM),        CO2CONC(ILG),
     1     CO2I1(ILG,ICTEM),     CO2I2(ILG,ICTEM),          COSZS(ILG),
     3          PRESSG(ILG),         XDIFFUS(ILG),     SLAI(ILG,ICTEM),
     4                     RMATCTEM(ILG,ICTEM,IG),  FCANCMX(ILG,ICTEM),
     5     ANVEG(ILG,ICTEM),    RMLVEG(ILG,ICTEM),       THLIQ(ILG,IG),
     6      FIELDSM(ILG,IG),       WILTSM(ILG,IG),      CFLUXV(ILG)
 
      INTEGER ISAND(ILG,IG)
C
      INTEGER ICTEM, ICTEMMOD, L2MAX, NOL2PFTS(IC), IC, IG
C
C     * LOCAL WORK ARRAYS FOR CTEM.
C
      REAL RCPHTSYN(ILG), QSWNVC(ILG)  
C
C     * GENERAL INTERNAL WORK ARRAYS.
C
      REAL TSTEP (ILG),    TVIRTC(ILG),    TVIRTG(ILG),
     1     EVBETA(ILG),    XEVAP (ILG),    EVPWET(ILG),    Q0SAT (ILG),
     2     RA    (ILG),    RB    (ILG),    RAGINV(ILG),    RBINV (ILG),    
     3     RBTINV(ILG),    RBCINV(ILG),    TVRTAC(ILG),    
     4     TPOTG (ILG),    RESID (ILG),    TCANO (ILG),    
     5     TRTOP (ILG),    QSTOR (ILG),    A     (ILG),    B     (ILG),    
     6     LZZ0  (ILG),    LZZ0T (ILG),    
     7     FM    (ILG),    FH    (ILG),    WZERO (ILG),    XEVAPM(ILG),    
     8     DCFLXM(ILG),    WC    (ILG),    DRAGIN(ILG),    CFLUXM(ILG),
     9     CFSENS(ILG),    CFEVAP(ILG),    QSGADD(ILG),    CFLX  (ILG)
C
      INTEGER              ITER  (ILG),    NITER (ILG),    IEVAPC(ILG),
     1                     KF1   (ILG),    KF2   (ILG)    
C
C     * TEMPORARY VARIABLES.
C
      REAL QSWNVG,QSWNIG,QSWNIC,HFREZ,HCONV,
     1     RCONV,HCOOL,HMELT,SCONV,HWARM,WCAN,DQ0DT,
     2     DRDT0,QEVAPT,BOWEN,DCFLUX,DXEVAP,TCANT,QEVAPCT,
     3     TZEROT,YEVAP,RAGCO,EZERO
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,HCPW,HCPICE,
     1     HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,
     2     RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,DELTA,CGRAV,CKARM,CPD,
     3     AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
C
C-----------------------------------------------------------------------
C     * INITIALIZATION AND PRE-ITERATION SEQUENCE.
C
      IF(ITCG.LT.2) THEN
          ITERMX=12
      ELSE
          ITERMX=5
      ENDIF
C      IF(ISNOW.EQ.0) THEN
C          EZERO=0.0
C      ELSE
C          EZERO=2.0
C      ENDIF
      EZERO=0.0
      RAGCO=1.9E-3
C
      DO 50 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(ISNOW.EQ.0)                      THEN
                  TRTOP(I)=0.
              ELSE
                  TRTOP(I)=TRSNOW(I)
              ENDIF
              QSWNVG=QSWINV(I)*TRVISC(I)*(1.0-ALVISG(I)) 
              QSWNIG=QSWINI(I)*TRNIRC(I)*(1.0-ALNIRG(I))
              QSWNG(I)=QSWNVG+QSWNIG                    
              QTRANS(I)=QSWNG(I)*TRTOP(I)   
              QSWNG(I)=QSWNG(I)-QTRANS(I)  
              QSWNVC(I)=QSWINV(I)*(1.0-ALVISC(I))-QSWNVG
              QSWNIC=QSWINI(I)*(1.0-ALNIRC(I))-QSWNIG    
              QSWNC(I)=QSWNVC(I)+QSWNIC
              IF(ABS(TCAN(I)).LT.1.0E-3)        TCAN(I)=TPOTA(I)
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
C
              IF(TCAN(I).GE.TFREZ)                         THEN
                  A(I)=17.269      
                  B(I)=35.86      
              ELSE
                  A(I)=21.874   
                  B(I)=7.66    
              ENDIF           
              WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1             (TCAN(I)-B(I)))/PADRY(I)           
              QCAN(I)=WCAN/(1.0+WCAN)   
              TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
              IF(ITC.EQ.2) THEN
                  TAC(I)=TCAN(I)
                  QAC(I)=QA(I)
              ENDIF
              TVRTAC(I)=TAC(I)*(1.0+0.61*QAC(I))   
C                                                                                 
              IF(SNOCAN(I).GT.0.)             THEN
                  CPHCHC(I)=CLHVAP+CLHMLT
              ELSE                                                                        
                  CPHCHC(I)=CLHVAP                                                           
              ENDIF                                                                       
              RBINV(I)=RBCOEF(I)*SQRT(VAC(I))
              RB(I)=1.0/RBINV(I)
              TZERO(I)=TGND(I)
              TCANO(I)=TCAN(I)
              TSTEP(I)=1.0
              ITER(I)=1
              NITER(I)=1
              QMELTC(I)=0.0    
              QMELTG(I)=0.0   
              IF(ISNOW.EQ.1)                               THEN
                  KF1(I)=1
                  KF2(I)=2
              ELSE
                  KF1(I)=4
                  KF2(I)=5
              ENDIF
          ENDIF
   50 CONTINUE
C
C     * CALL PHOTOSYNTHESIS SUBROUTINE HERE TO GET A NEW ESTIMATE OF
C     * RC BASED ON PHOTOSYNTHESIS.
C
C      IF(ICTEMMOD.EQ.1)                                            THEN
C        CALL PHTSYN3(  AILCG, FCANC,     TCAN, CO2CONC,  PRESSG,    FI,
C     1                CFLUXV,    QA,   QSWNVC,      IC,   THLIQ, ISAND,
C     2                   TAC,        RMATCTEM,   COSZS, XDIFFUS,   ILG,
C     3                   IL1,   IL2,       IG,   ICTEM,   ISNOW,  SLAI,
C     4               FIELDSM,WILTSM,  FCANCMX,   L2MAX,NOL2PFTS,
C     5              RCPHTSYN, CO2I1,    CO2I2,   ANVEG,  RMLVEG)
C
C       * KEEP CLASS RC FOR BONEDRY POINTS (DIANA'S FLAG OF 1.E20) SUCH
C       * THAT WE GET (BALT-BEG) CONSERVATION.
C
C        DO 70 I =IL1,IL2
C          IF(RC(I).LE.10000.) THEN
C            RC(I)=RCPHTSYN(I)
C          ENDIF
C   70   CONTINUE
C      ENDIF
C
C     * ITERATION FOR SURFACE TEMPERATURE OF GROUND UNDER CANOPY.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS
C     * ON THE CURRENT LATITUDE CIRCLE(S).
C  
  100 CONTINUE
C
      NUMIT=0
      DO 125 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(TZERO(I).GE.TFREZ)                           THEN
                  A(I)=17.269     
                  B(I)=35.86     
              ELSE              
                  A(I)=21.874  
                  B(I)=7.66   
              ENDIF  
              WZERO(I)=0.622*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/
     1              (TZERO(I)-B(I)))/PADRY(I)           
              Q0SAT(I)=WZERO(I)/(1.0+WZERO(I))    
              IF(IWATER(I).GT.0)                              THEN
                  EVBETA(I)=1.0        
                  QZERO(I)=Q0SAT(I)
              ELSE                            
                  EVBETA(I)=CEVAP(I)
                  QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QAC(I)
                  IF(QZERO(I).GT.QAC(I) .AND. IEVAP(I).EQ.0) THEN                   
                      EVBETA(I)=0.0
                      QZERO(I)=QAC(I)
                  ENDIF
              ENDIF
C
              TPOTG(I)=TZERO(I)-8.0*ZOM(I)*GRAV/CPD
              TVIRTG(I)=TPOTG(I)*(1.0+0.61*QZERO(I))   
              IF(TVIRTG(I).GT.TVRTAC(I)+1.)                   THEN
                  RAGINV(I)=RAGCO*(TVIRTG(I)-TVRTAC(I))**0.333333
                  DRAGIN(I)=0.333*RAGCO*(TVIRTG(I)-TVRTAC(I))**(-.667)
              ELSEIF(TVIRTG(I).GT.(TVRTAC(I)+0.001))          THEN 
                  RAGINV(I)=RAGCO*(TVIRTG(I)-TVRTAC(I))
                  DRAGIN(I)=RAGCO
              ELSE
                  RAGINV(I)=0.0
                  DRAGIN(I)=0.0
              ENDIF
C
              QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              QSENSG(I)=RHOAIR(I)*SPHAIR*RAGINV(I)*
     1            (TPOTG(I)-TAC(I))
              EVAPG (I)=RHOAIR(I)*(QZERO(I)-QAC(I))*RAGINV(I)
              QEVAPG(I)=CPHCHG(I)*EVAPG(I)    
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              RESID(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1            QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
              IF(ABS(RESID(I)).LT.5.0)                     ITER(I)=0
              IF(ABS(TSTEP(I)).LT.1.0E-2)                  ITER(I)=0
              IF(NITER(I).EQ.ITERMX .AND. ITER(I).EQ.1)    ITER(I)=-1
          ENDIF
125   CONTINUE
C
      IF(ITCG.LT.2) THEN
C
C     * OPTION #1: BISECTION ITERATION METHOD.
C
      DO 150 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(NITER(I).EQ.1) THEN
                  IF(RESID(I).GT.0.0) THEN
                      TZERO(I)=TZERO(I)+TSTEP(I)
                  ELSE
                      TZERO(I)=TZERO(I)-TSTEP(I)
                  ENDIF
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))   THEN 
                      TSTEP(I)=-TSTEP(I)/2.0   
                  ENDIF
                  TZERO(I)=TZERO(I)+TSTEP(I)
              ENDIF
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  150 CONTINUE
C
      ELSE
C
C     * OPTION #2: NEWTON-RAPHSON ITERATION METHOD.
C
      DO 175 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              DQ0DT=-WZERO(I)*A(I)*(B(I)-TFREZ)/((TZERO(I)-B(I))*
     1               (1.0+WZERO(I)))**2*EVBETA(I)
              DRDT0=-4.0*SBC*TZERO(I)**3
     1               -GCOEFF(I)-RHOAIR(I)*SPHAIR*
     2              (RAGINV(I)+(TPOTG(I)-TAC(I))*DRAGIN(I))-
     3               CPHCHG(I)*RHOAIR(I)*(DQ0DT*RAGINV(I)
     4              +(QZERO(I)-QAC(I))*DRAGIN(I))
              TSTEP(I)=-RESID(I)/DRDT0
              IF(ABS(TSTEP(I)).GT.20.0) TSTEP(I)=SIGN(10.0,TSTEP(I))
              TZERO(I)=TZERO(I)+TSTEP(I)
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  175 CONTINUE
C
      ENDIF
C
      IF(NUMIT.GT.0)                                    GO TO 100
C
C     * IF CONVERGENCE HAS NOT BEEN REACHED FOR ITERATION METHOD #2, 
C     * CALCULATE TEMPERATURE AND FLUXES ASSUMING NEUTRAL STABILITY 
C     * AND USING BOWEN RATIO APPROACH.
C
      IF(ITCG.EQ.2)                                                 THEN
C
      DO 200 I=IL1,IL2
          IF(ITER(I).EQ.-1)                                  THEN
             TZEROT=TVIRTC(I)/(1.0+0.61*QZERO(I))
             IF(ABS(RESID(I)).GT.15.) THEN
                TZERO(I)=TZEROT
                IF(TZERO(I).GE.TFREZ)                        THEN
                  A(I)=17.269
                  B(I)=35.86
                ELSE
                  A(I)=21.874
                  B(I)=7.66
                ENDIF
                WZERO(I)=0.622*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/
     1              (TZERO(I)-B(I)))/PADRY(I)
                Q0SAT(I)=WZERO(I)/(1.0+WZERO(I))
                QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QAC(I)
                QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
                GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
                RESID(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1              QLWOC(I)-QLWOG(I)-GZERO(I)
                QEVAPT=CPHCHG(I)*(QZERO(I)-QAC(I))
                BOWEN=SPHAIR*(TZERO(I)-TAC(I))/
     1             SIGN(MAX(ABS(QEVAPT),1.E-6),QEVAPT)
                QEVAPG(I)=RESID(I)/SIGN(MAX(ABS(1.+BOWEN),0.1),1.+BOWEN)
                QSENSG(I)=RESID(I)-QEVAPG(I)
                RESID(I)=0.
                EVAPG(I)=QEVAPG(I)/CPHCHG(I)
             ENDIF
          ENDIF
  200 CONTINUE
C
      ENDIF
C
      IBAD=0
C
      DO 225 I=IL1,IL2
C          IF(FI(I).GT.0. .AND. ITER(I).EQ.-1)                     THEN 
C              WRITE(6,6250) I,JL,NITER(I),RESID(I),TZERO(I),RIB(I)
C6250          FORMAT('0SUBCAN ITERATION LIMIT',3X,3I3,3(F8.2,E12.4))
C          ENDIF                                            
          IF(FI(I).GT.0.)                                           THEN
              IF(TZERO(I).LT.173.16 .OR. TZERO(I).GT.373.16)    THEN
                  IBAD=I
              ENDIF
          ENDIF
 225  CONTINUE
C
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6370) IBAD,N,TZERO(IBAD),NITER(IBAD),ISNOW
 6370     FORMAT('0BAD GROUND ITERATION TEMPERATURE',3X,2I8,F16.2,2I4)
          WRITE(6,6380) QSWNG(IBAD),FSVF(IBAD),QLWIN(IBAD),QLWOC(IBAD),
     1        QLWOG(IBAD),QSENSG(IBAD),QEVAPG(IBAD),GZERO(IBAD)
          WRITE(6,6380) TCAN(IBAD)
          CALL XIT('TSOLVC',-1)
      ENDIF
C
C     * POST-ITERATION CLEAN-UP.
C
      DO 250 I=IL1,IL2
          IF(FI(I).GT.0.)                                        THEN
              IF((IWATER(I).EQ.1 .AND. TZERO(I).LT.TFREZ) .OR. 
     1              (IWATER(I).EQ.2 .AND. TZERO(I).GT.TFREZ))  THEN
                  TZERO(I)=TFREZ      
                  WZERO(I)=0.622*611.0/PADRY(I)
                  QZERO(I)=WZERO(I)/(1.0+WZERO(I))
                  TPOTG(I)=TZERO(I)-8.0*ZOM(I)*GRAV/CPD
                  TVIRTG(I)=TPOTG(I)*(1.0+0.61*QZERO(I))   
C
                  QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
                  GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
                  IF(TVIRTG(I).GT.(TVRTAC(I)+0.001))         THEN 
                      RAGINV(I)=RAGCO*(TVIRTG(I)-TVRTAC(I))**0.333333
                      QSENSG(I)=RHOAIR(I)*SPHAIR*RAGINV(I)*
     1                          (TPOTG(I)-TAC(I))
                      EVAPG (I)=RHOAIR(I)*(QZERO(I)-QAC(I))*RAGINV(I)
                  ELSE                  
                      RAGINV(I)=0.0
                      QSENSG(I)=0.0    
                      EVAPG (I)=0.0   
                  ENDIF              
                  QEVAPG(I)=CPHCHG(I)*EVAPG(I)   
                  QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                 QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
                  RESID(I)=0.0
              ENDIF                                                                   
C
              IF(ABS(EVAPG(I)).LT.1.0E-8) THEN
                  RESID(I)=RESID(I)+QEVAPG(I)
                  EVAPG(I)=0.0
                  QEVAPG(I)=0.0
              ENDIF
              IF(RESID(I).GT.15. .AND. QEVAPG(I).GT.10. .AND. PCPR(I)
     1                   .LT.1.0E-8)                 THEN
                  QEVAPG(I)=QEVAPG(I)+RESID(I)
              ELSE
                  QSENSG(I)=QSENSG(I)+RESID(I)
              ENDIF
              ITERCT(I,KF2(I),NITER(I))=ITERCT(I,KF2(I),NITER(I))+1
          ENDIF                                                                   
  250 CONTINUE
C
C     * PRE-ITERATION SEQUENCE FOR VEGETATION CANOPY.
C
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              QSGADD(I)=0.0
              IF(ITC.EQ.2) THEN
                  QSGADD(I)=QSENSG(I)
                  TAC(I)=TCAN(I)
                  QAC(I)=QCAN(I)
                  TVRTAC(I)=TVIRTC(I)
              ENDIF
              ITER(I)=1
              NITER(I)=1      
              TSTEP(I)=1.0
              CFLUXM(I)=0.0
              DCFLXM(I)=0.0
          ENDIF
  300 CONTINUE
C
      IF(ITC.LT.2) THEN
          ITERMX=12
      ELSE
          ITERMX=5
      ENDIF
C
C     * ITERATION FOR CANOPY TEMPERATURE.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS
C     * ON THE CURRENT LATITUDE CIRCLE(S).
C  
  400 CONTINUE
C
      NUMIT=0
      NIT=0
      DO 450 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                     THEN    
              NIT=NIT+1
              IF(ITC.EQ.1) THEN
                  IF(TCAN(I).GE.TFREZ)                       THEN
                      A(I)=17.269      
                      B(I)=35.86      
                  ELSE
                      A(I)=21.874   
                      B(I)=7.66    
                  ENDIF           
                  WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                 (TCAN(I)-B(I)))/PADRY(I)           
                  QCAN(I)=WCAN/(1.0+WCAN)   
                  TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
              ENDIF
          ENDIF
  450 CONTINUE      
C
      IF(NIT.GT.0)                                                  THEN 
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT)
C     * AND OTHER RELATED QUANTITIES BETWEEN CANOPY AIR SPACE AND 
C     * ATMOSPHERE.
C
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF(CDM,CDH,RIB,CFLUX,QAC,QA,ZOSCLM,ZOSCLH,
     1                  CRIB,TVRTAC,TVIRTA,VA,FI,ITER,
     2                  ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TAC,QAC,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,ITER,JL )
        ENDIF
C
C     * CALCULATE CANOPY AIR TEMPERATURE AND SPECIFIC HUMIDITY OF 
C     * CANOPY AIR (FIRST WITHOUT RC TO CHECK FOR CONDENSATION; 
C     * IF NO CONDENSATION EXISTS, RECALCULATE).
C
        IF(ITC.EQ.1) THEN
C
        DO 475 I=IL1,IL2
            IF (FI(I).GT.0. .AND. ITER(I).EQ.1)                THEN    
                XEVAP(I)=RBINV(I)
                QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1              QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+CFLUX(I))
                IF(QAC(I).LT.QCAN(I))                     THEN
                   IF(FSNOWC(I).GT.0.0)               THEN 
                       XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RB(I)
                   ELSE   
                       XEVAP(I)=FRAINC(I)/RB(I)+(1.0-FRAINC(I))/
     1                          (RB(I)+RC(I))                            
                       QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1                     QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+
     2                     CFLUX(I))
                   ENDIF 
                ELSE
                    IF(FSNOWC(I).GT.1.0E-5) THEN
                        XEVAP(I)=FSNOWC(I)/RB(I)
                    ELSE
                        XEVAP(I)=1.0/RB(I)
                    ENDIF
                ENDIF
                TAC(I)=(TCAN(I)*RBINV(I)+TPOTG(I)*RAGINV(I)+
     1              TPOTA(I)*CFLUX(I))/(RBINV(I)+RAGINV(I)+CFLUX(I))
                TVRTAC(I)=TAC(I)*(1.0+0.61*QAC(I))  
                CFSENS(I)=RBINV(I)
                CFEVAP(I)=XEVAP(I)
            ENDIF
475     CONTINUE
C
        ELSE
C
        DO 500 I=IL1,IL2
            IF (FI(I).GT.0. .AND. ITER(I).EQ.1)                THEN    
                CFLX(I)=RBINV(I)*CFLUX(I)/(RBINV(I)+CFLUX(I))
                CFLX(I)=CFLUX(I)+(CFLX(I)-CFLUX(I))*
     1              MIN(1.0,QSWINV(I)*0.04)
                RA(I)=1.0/CFLX(I)
                IF(QA(I).LT.QCAN(I))                     THEN
                   IF(FSNOWC(I).GT.0.0)               THEN 
                       XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RA(I)
                   ELSE   
                       XEVAP(I)=FRAINC(I)/RA(I)+(1.0-FRAINC(I))/
     1                          (RA(I)+RC(I))                            
                   ENDIF 
                ELSE
                    IF(FSNOWC(I).GT.1.0E-5) THEN
                        XEVAP(I)=FSNOWC(I)/RA(I)
                    ELSE
                        XEVAP(I)=1.0/RA(I)
                    ENDIF
                ENDIF
                IF(TCAN(I).GE.TFREZ)                         THEN
                    A(I)=17.269      
                    B(I)=35.86      
                ELSE
                    A(I)=21.874   
                    B(I)=7.66    
                ENDIF           
                WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1               (TCAN(I)-B(I)))/PADRY(I)           
                WC(I)=WCAN
                QCAN(I)=WCAN/(1.0+WCAN)   
                QCAN(I)=RA(I)*XEVAP(I)*QCAN(I)+(1.0-RA(I)*XEVAP(I))*
     1              QA(I)
                TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                CFSENS(I)=CFLX(I)
                CFEVAP(I)=CFLX(I)
                TAC(I)=TPOTA(I)
                QAC(I)=QA(I)
            ENDIF
500     CONTINUE
C
        ENDIF
C
C     * CALCULATE THE TERMS IN THE ENERGY BALANCE AND SOLVE.
C
        DO 525 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              QSENSC(I)=RHOAIR(I)*SPHAIR*CFSENS(I)*(TCAN(I)-TAC(I))
              IF(FRAINC(I).GT.0. .OR. FSNOWC(I).GT.0. .OR. 
     1           RC(I).LE.5000. .OR. QAC(I).GT.QCAN(I))       THEN 
                  EVAPC(I)=RHOAIR(I)*CFEVAP(I)*(QCAN(I)-QAC(I))
                  IEVAPC(I)=1
              ELSE     
                  EVAPC(I)=0.0   
                  IEVAPC(I)=0
                  QCAN(I)=QA(I)
              ENDIF             
              IF(EVAPC(I).LT.0. .AND. TCAN(I).GT.TADP(I)) EVAPC(I)=0.0
              IF(SNOCAN(I).GT.0.)                            THEN
                  EVPWET(I)=(CLHVAP+CLHMLT)*SNOCAN(I)/DELT
              ELSE
                  EVPWET(I)=CLHVAP*RAICAN(I)/DELT
              ENDIF
              IF((FRAINC(I)+FSNOWC(I)).GT.0.50 .AND. 
     1                        EVAPC(I).GT.EVPWET(I))         THEN
                  EVAPC(I)=EVPWET(I)
                  IEVAPC(I)=0
              ENDIF
              QEVAPC(I)=CPHCHC(I)*EVAPC(I)  
              QSTOR (I)=CHCAP(I)*(TCAN(I)-TCANO(I))/DELT
              RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1             (1.0-FSVF(I))+QSGADD(I)-QSENSC(I)-QEVAPC(I)-
     2             QSTOR(I)-QMELTC(I)
              IF(ABS(RESID(I)).LT.5.0)                       ITER(I)=0
              IF(ABS(TSTEP(I)).LT. 1.0E-2)                   ITER(I)=0
              IF(NITER(I).EQ.ITERMX .AND. ITER(I).EQ.1)      ITER(I)=-1
          ENDIF
  525   CONTINUE     
C
      IF(ITC.LT.2) THEN
C
C     * OPTION #1: SECANT/BISECTION ITERATION METHOD.
C
        DO 550 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(NITER(I).EQ.1) THEN
                  IF(RESID(I).GT.0.0) THEN
                      TCAN(I)=TCAN(I)+TSTEP(I)
                  ELSE
                      TCAN(I)=TCAN(I)-TSTEP(I)
                  ENDIF
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))    THEN 
                      TSTEP(I)=-TSTEP(I)/2.0   
                  ENDIF
                  TCAN(I)=TCAN(I)+TSTEP(I)
              ENDIF
              IF(ABS(TCAN(I)-TFREZ).LT.1.0E-6)             TCAN(I)=TFREZ
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  550   CONTINUE
C
      ELSE
C
C     * OPTION #2: NEWTON-RAPHSON ITERATION METHOD.
C
        DO 575 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(NITER(I).GT.1)                              THEN
                  DCFLUX=(CFLX(I)-CFLUXM(I))/
     1               SIGN(MAX(.001,ABS(TSTEP(I))),TSTEP(I))
                  IF(ABS(TVIRTA(I)-TVIRTC(I)).LT.0.4)
     1                DCFLUX=MAX(DCFLUX,0.8*DCFLXM(I))
                  DXEVAP=(XEVAP(I)-XEVAPM(I))/
     1                SIGN(MAX(.001,ABS(TSTEP(I))),TSTEP(I))
              ELSE
                  DCFLUX=0.
                  DXEVAP=0.
              ENDIF
              XEVAPM(I)=XEVAP(I)
              CFLUXM(I)=CFLX(I)
              DCFLXM(I)=DCFLUX
              DRDT0=-4.0*SBC*TCAN(I)*TCAN(I)*TCAN(I)*(1.0-FSVF(I))*
     1              2.0-RHOAIR(I)*SPHAIR*(CFLX(I)+MAX(0.,
     2              TCAN(I)-TPOTA(I))*DCFLUX)+IEVAPC(I)*CPHCHC(I)*
     3              RHOAIR(I)*(XEVAP(I)*WC(I)*A(I)*(B(I)-TFREZ)/
     4              ((TCAN(I)-B(I))*(1.0+WC(I)))**2-(QCAN(I)-QA(I))*
     5              DXEVAP)-CHCAP(I)/DELT
              TSTEP(I)=-RESID(I)/DRDT0
              TSTEP(I)=MAX(-10.,MIN(5.,TSTEP(I)))
              TCAN(I)=TCAN(I)+TSTEP(I)
              IF(ABS(TCAN(I)-TFREZ).LT.1.0E-3)             TCAN(I)=TFREZ
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
              TAC(I)=TCAN(I)
              QAC(I)=QCAN(I)
              TVRTAC(I)=TVIRTC(I)
          ENDIF
  575   CONTINUE
C
      ENDIF
C
      ENDIF
      IF(NUMIT.GT.0)                                    GO TO 400
C
C     * IF CONVERGENCE HAS NOT BEEN REACHED FOR ITERATION METHOD #2, 
C     * CALCULATE TEMPERATURE AND FLUXES ASSUMING NEUTRAL STABILITY. 
C
      IF(ITC.EQ.2) THEN
C
        NUMIT=0
        DO 600 I=IL1,IL2
          IEVAPC(I)=0
          IF(ITER(I).EQ.-1)                   THEN
            TCANT=TVIRTA(I)/(1.0+0.61*QCAN(I))
            IF(ABS(RESID(I)).GT.100.)  THEN
               TCAN(I)=TCANT
               IF(TCAN(I).GE.TFREZ)                         THEN
                  A(I)=17.269
                  B(I)=35.86
               ELSE
                  A(I)=21.874
                  B(I)=7.66
               ENDIF
               WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1             (TCAN(I)-B(I)))/PADRY(I)
               QCAN(I)=WCAN/(1.0+WCAN)
               IF(FSNOWC(I).GT.0.0)               THEN
                   YEVAP=FRAINC(I)+FSNOWC(I)
               ELSE
                   YEVAP=FRAINC(I)+(1.0-FRAINC(I))*10./(10.+RC(I))
               ENDIF
               QCAN(I)=YEVAP*QCAN(I)+(1.0-YEVAP)*QA(I)
               QSTOR(I)=CHCAP(I)*(TCAN(I)-TCANO(I))/DELT
               QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
               RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1             (1.0-FSVF(I))+QSENSG(I)-QSTOR(I)
               IF(RESID(I).GT.0.) THEN
                   QEVAPC(I)=RESID(I)
               ELSE
                   QEVAPC(I)=RESID(I)*0.5
               ENDIF
               QSENSC(I)=RESID(I)-QEVAPC(I)
               RESID(I)=0.
               EVAPC(I)=QEVAPC(I)/CPHCHC(I)
               TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
               NUMIT=NUMIT+1
               IEVAPC(I)=1
            ENDIF
          ENDIF
  600   CONTINUE
c
      IF(NUMIT.GT.0) THEN
         IF(ISLFD.LT.2) THEN
            CALL DRCOEF(CDM,CDH,RIB,CFLUX,QA,QA,ZOSCLM,ZOSCLH,
     1                  CRIB,TVIRTC,TVIRTA,VA,FI,IEVAPC,
     2                  ILG,IL1,IL2)
         ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TCAN,QCAN,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,IEVAPC,JL )
         ENDIF
      ENDIF
C
      ENDIF
C
      IBAD=0
C
      DO 625 I=IL1,IL2
C         IF(FI(I).GT.0. .AND. ITER(I).EQ.-1)                      THEN 
C             WRITE(6,6350) I,JL,NITER(I),RESID(I),TCAN(I),RIB(I)
C6350         FORMAT('0CANOPY ITERATION LIMIT',3X,3I3,3(F8.2,E12.4))            
C         ENDIF                                            
          IF(FI(I).GT.0. .AND. (TCAN(I).LT.173.16 .OR.
     1                           TCAN(I).GT.373.16))                THEN
              IBAD=I
          ENDIF  
  625 CONTINUE
C  
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6375) IBAD,JL,TCAN(IBAD),NITER(IBAD),ISNOW
 6375     FORMAT('0BAD CANOPY ITERATION TEMPERATURE',3X,2I3,F16.2,2I4)
          WRITE(6,6380) QSWNC(IBAD),QLWIN(IBAD),QLWOG(IBAD),
     1                  QLWOC(IBAD),QSENSG(IBAD),QSENSC(IBAD),
     2                  QEVAPC(IBAD),QSTOR(IBAD),QMELTC(IBAD)
          WRITE(6,6380) TCAN(IBAD),TPOTA(IBAD),TZERO(IBAD)
 6380     FORMAT(2X,9F10.2)
          CALL XIT('TSOLVC',-2)
      ENDIF
C
C     * POST-ITERATION CLEAN-UP.
C
      NIT=0
      DO 650 I=IL1,IL2
          IF(FI(I).GT.0.) THEN
              IF(RAICAN(I).GT.0. .AND. TCAN(I).LT.TFREZ)      THEN 
                  QSTOR(I)=-CHCAP(I)*TCANO(I)/DELT
                  ITER(I)=1
                  NIT=NIT+1
                  HFREZ=CHCAP(I)*(TFREZ-TCAN(I))   
                  HCONV=RAICAN(I)*CLHMLT          
                  IF(HFREZ.LE.HCONV)                       THEN 
                     RCONV=HFREZ/CLHMLT          
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)*RCONV/RAICAN(I)
                     FRAINC(I)=FRAINC(I)-FRAINC(I)*RCONV/RAICAN(I)
                     SNOCAN(I)=SNOCAN(I)+RCONV  
                     RAICAN(I)=RAICAN(I)-RCONV 
                     TCAN  (I)=TFREZ          
                     QMELTC(I)=-CLHMLT*RCONV/DELT
                     WCAN=0.622*611.0/PADRY(I)  
                     QCAN(I)=WCAN/(1.0+WCAN)                                                 
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ELSE                    
                     HCOOL=HFREZ-HCONV   
                     SNOCAN(I)=SNOCAN(I)+RAICAN(I)  
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)
                     FRAINC(I)=0.0
                     TCAN  (I)=-HCOOL/(SPHVEG*CMASS(I)+SPHICE*
     1                         SNOCAN(I))+TFREZ  
                     QMELTC(I)=-CLHMLT*RAICAN(I)/DELT
                     RAICAN(I)=0.0                 
                     A(I)=21.874                  
                     B(I)=7.66                   
                     WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                    (TCAN(I)-B(I)))/PADRY(I)           
                     QCAN(I)=WCAN/(1.0+WCAN)    
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ENDIF    
                  CHCAP(I)=SPHVEG*CMASS(I)+SPHICE*SNOCAN(I)+
     1                     SPHW*RAICAN(I)
                  QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
              ELSE
                  ITER(I)=0
              ENDIF                       
              IF(ITC.EQ.2) THEN
                  TAC(I)=TCAN(I)
                  QAC(I)=QCAN(I)
                  TVRTAC(I)=TVIRTC(I)
              ENDIF
          ENDIF
  650 CONTINUE      
C
      DO 675 I=IL1,IL2
          IF(FI(I).GT.0.) THEN
              IF(SNOCAN(I).GT.0. .AND. TCAN(I).GT.TFREZ)    THEN
                  QSTOR(I)=-CHCAP(I)*TCANO(I)/DELT
                  ITER(I)=1
                  NIT=NIT+1
                  HMELT=CHCAP(I)*(TCAN(I)-TFREZ)    
                  HCONV=SNOCAN(I)*CLHMLT           
                  IF(HMELT.LE.HCONV)                       THEN 
                     SCONV=HMELT/CLHMLT           
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)*SCONV/SNOCAN(I)
                     FSNOWC(I)=FSNOWC(I)-FSNOWC(I)*SCONV/SNOCAN(I)
                     SNOCAN(I)=SNOCAN(I)-SCONV   
                     RAICAN(I)=RAICAN(I)+SCONV  
                     TCAN  (I)=TFREZ           
                     QMELTC(I)=CLHMLT*SCONV/DELT
                     WCAN=0.622*611.0/PADRY(I)  
                     QCAN(I)=WCAN/(1.0+WCAN)  
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ELSE                       
                     HWARM=HMELT-HCONV      
                     RAICAN(I)=RAICAN(I)+SNOCAN(I)    
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)
                     FSNOWC(I)=0.0
                     TCAN  (I)=HWARM/(SPHVEG*CMASS(I)+SPHW*
     1                         RAICAN(I))+TFREZ                         
                     QMELTC(I)=CLHMLT*SNOCAN(I)/DELT
                     SNOCAN(I)=0.0                   
                     A(I)=17.269      
                     B(I)=35.86      
                     WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                    (TCAN(I)-B(I)))/PADRY(I)           
                     QCAN(I)=WCAN/(1.0+WCAN)   
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ENDIF                       
                  CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)+
     1                     SPHICE*SNOCAN(I)
                  QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
              ENDIF                       
              IF(ITC.EQ.2) THEN
                  TAC(I)=TCAN(I)
                  QAC(I)=QCAN(I)
                  TVRTAC(I)=TVIRTC(I)
              ENDIF
          ENDIF
  675 CONTINUE      
C
      IF(NIT.GT.0)                                         THEN 
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT)
C     * AND OTHER RELATED QUANTITIES BETWEEN CANOPY AIR SPACE AND 
C     * ATMOSPHERE.
C
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF(CDM,CDH,RIB,CFLUX,QAC,QA,ZOSCLM,ZOSCLH,
     1                  CRIB,TVRTAC,TVIRTA,VA,FI,ITER,
     2                  ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TAC,QAC,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,ITER,JL )
        ENDIF
      ENDIF
C
C     * REMAINING CALCULATIONS.
C
      IF(ITC.EQ.1) THEN
C
      DO 700 I=IL1,IL2
          IF (FI(I).GT.0. .AND. ITER(I).EQ.1)                THEN    
              XEVAP(I)=RBINV(I)
              QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1            QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+CFLUX(I))
              IF(QAC(I).LT.QCAN(I))                     THEN
                 IF(FSNOWC(I).GT.0.0)               THEN 
                     XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RB(I)
                 ELSE   
                     XEVAP(I)=FRAINC(I)/RB(I)+(1.0-FRAINC(I))/
     1                        (RB(I)+RC(I))                            
                     QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1                   QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+
     2                   CFLUX(I))
                 ENDIF 
              ELSE
                  IF(FSNOWC(I).GT.1.0E-5) THEN
                      XEVAP(I)=FSNOWC(I)/RB(I)
                  ELSE
                      XEVAP(I)=1.0/RB(I)
                  ENDIF
              ENDIF
              TAC(I)=(TCAN(I)*RBINV(I)+TPOTG(I)*RAGINV(I)+
     1            TPOTA(I)*CFLUX(I))/(RBINV(I)+RAGINV(I)+CFLUX(I))
              TVRTAC(I)=TAC(I)*(1.0+0.61*QAC(I))  
              CFSENS(I)=RBINV(I)
              CFEVAP(I)=XEVAP(I)
          ENDIF
700   CONTINUE
C
      ELSE
C
      DO 750 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              CFLX(I)=RBINV(I)*CFLUX(I)/(RBINV(I)+CFLUX(I))
              CFLX(I)=CFLUX(I)+(CFLX(I)-CFLUX(I))*
     1            MIN(1.0,QSWINV(I)*0.04)
              RA(I)=1.0/CFLX(I)
              IF(QA(I).LT.QCAN(I))                     THEN
                 IF(FSNOWC(I).GT.0.0)               THEN 
                     XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RA(I)
                 ELSE   
                     XEVAP(I)=FRAINC(I)/RA(I)+(1.0-FRAINC(I))/
     1                        (RA(I)+RC(I))                            
                 ENDIF 
              ELSE
                  IF(FSNOWC(I).GT.1.0E-5) THEN
                      XEVAP(I)=FSNOWC(I)/RA(I)
                  ELSE
                      XEVAP(I)=1.0/RA(I)
                  ENDIF
              ENDIF
              QCAN(I)=RA(I)*XEVAP(I)*QCAN(I)+(1.0-RA(I)*
     1            XEVAP(I))*QA(I)
              CFSENS(I)=CFLX(I)
              CFEVAP(I)=CFLX(I)
              TAC(I)=TPOTA(I)
              QAC(I)=QA(I)
          ENDIF
750   CONTINUE
C
      ENDIF
C
      DO 800 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(SNOCAN(I).GT.0.)             THEN
                  CPHCHC(I)=CLHVAP+CLHMLT
              ELSE                                                                        
                  CPHCHC(I)=CLHVAP                                                           
              ENDIF                                                                       
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              QSENSC(I)=RHOAIR(I)*SPHAIR*CFSENS(I)*(TCAN(I)-TAC(I))
              IF(FRAINC(I).GT.0. .OR. FSNOWC(I).GT.0. .OR. 
     1           RC(I).LE.5000. .OR. QAC(I).GT.QCAN(I))       THEN 
                  EVAPC(I)=RHOAIR(I)*CFEVAP(I)*(QCAN(I)-QAC(I))
              ELSE                 
                  EVAPC(I)=0.0        
              ENDIF                    
              IF(EVAPC(I).LT.0. .AND. TCAN(I).GE.TADP(I)) EVAPC(I)=0.0
              IF(SNOCAN(I).GT.0.)                            THEN
                  EVPWET(I)=(CLHVAP+CLHMLT)*SNOCAN(I)/DELT
              ELSE
                  EVPWET(I)=CLHVAP*RAICAN(I)/DELT
              ENDIF
              IF((FRAINC(I)+FSNOWC(I)).GT.0.50 .AND. 
     1            EVAPC(I).GT.EVPWET(I)) EVAPC(I)=EVPWET(I)
              QEVAPC(I)=CPHCHC(I)*EVAPC(I)    
              RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1             (1.0-FSVF(I))+QSGADD(I)-QSENSC(I)-QEVAPC(I)-
     2             QSTOR(I)-QMELTC(I)
          ENDIF
  800 CONTINUE
C
      DO 850 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN 
              IF(ABS(EVAPC(I)).LT.1.0E-8) THEN
                  RESID(I)=RESID(I)+QEVAPC(I)
                  EVAPC(I)=0.0
                  QEVAPC(I)=0.0
              ENDIF
              QSENSC(I)=QSENSC(I)+RESID(I)
              IF(ABS(TZERO(I)-TFREZ).LT.1.0E-3) THEN
                  QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
              ELSE
                  GZERO(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)
              ENDIF
              IF(EVAPC(I).LT.0.) THEN
                  IF(SNOCAN(I).GT.0.)             THEN
                      SNOCAN(I)=SNOCAN(I)-EVAPC(I)*DELT
                      QFCF(I)=QFCF(I)+FI(I)*EVAPC(I)
                      HTCC(I)=HTCC(I)-FI(I)*TCAN(I)*SPHICE*EVAPC(I)
                  ELSE
                      RAICAN(I)=RAICAN(I)-EVAPC(I)*DELT
                      QFCL(I)=QFCL(I)+FI(I)*EVAPC(I)
                      HTCC(I)=HTCC(I)-FI(I)*TCAN(I)*SPHW*EVAPC(I)
                  ENDIF
                  EVAP(I)=EVAP(I)+FI(I)*EVAPC(I)
                  EVAPC(I)=0.0
                  CHCAP(I)=SPHVEG*CMASS(I)+SPHICE*SNOCAN(I)+
     1                     SPHW*RAICAN(I)
              ENDIF
              QSWNET(I)=QSWNG(I)+QSWNC(I)+QTRANS(I)
              QLWOUT(I)=FSVF(I)*QLWOG(I)+(1.0-FSVF(I))*QLWOC(I)
              QSENS(I)=QSENSC(I)+QSENSG(I)-QSGADD(I)
              QEVAP(I)=QEVAPC(I)+QEVAPG(I)    
              EVAPC(I)=EVAPC(I)/RHOW         
              EVAPG(I)=EVAPG(I)/RHOW
              ITERCT(I,KF1(I),NITER(I))=ITERCT(I,KF1(I),NITER(I))+1
          ENDIF
  850 CONTINUE
      IF (ICTEMMOD.EQ.1) THEN
C
C       * STORE AERODYNAMIC CONDUCTANCE FOR USE IN NEXT TIME STEP
C
        DO 900 I = IL1, IL2
          IF(FI(I).GT.0.)                                          THEN 
            CFLUXV(I) = CFLUX(I)
          ELSE
            CFLUXV(I) = 0.
          ENDIF
  900   CONTINUE
      ENDIF
C                                                                                  
C                                           
      RETURN                                                                      
      END
