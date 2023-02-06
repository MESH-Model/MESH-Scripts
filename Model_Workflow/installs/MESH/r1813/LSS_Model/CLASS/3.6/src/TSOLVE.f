      SUBROUTINE TSOLVE(ISNOW,FI,
     1                  QSWNET,QLWOUT,QTRANS,QSENS,QEVAP,EVAP,
     2                  TZERO,QZERO,GZERO,QMELT,CDH,CDM,RIB,CFLUX,
     3                  FTEMP,FVAP,ILMO,UE,H,
     4                  QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5                  ALVISG,ALNIRG,CRIB,CPHCH,CEVAP,TVIRTA,
     6                  ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                  GCONST,GCOEFF,TSTART,TRSNOW,PCPR,
     8                  IWATER,IEVAP,ITERCT,ISAND,
     9                  ISLFD,ITG,ILG,IG,IL1,IL2,JL,
     A                  TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                  DCFLXM,CFLUXM,WZERO,TRTOP,A,B,
     C                  LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF)
C
C     * OCT 14/11 - D.VERSEGHY. FOR POST-ITERATION CLEANUP WITH N-R SCHEME,
C     *                         REMOVE CONDITION INVOLVING LAST ITERATION
C     *                         TEMPERATURE.
C     * DEC 07/09 - D.VERSEGHY. RESTORE EVAPORATION WHEN PRECIPITATION
C     *                         IS OCCURRING.
C     * MAR 13/09 - D.VERSEGHY. REPLACE SURFCON COMMON BLOCK WITH CLASSD2;
C     *                         REVISED CALL TO FLXSURFZ.
C     * JAN 06/09 - D.VERSEGHY/M.LAZARE. SPLIT IF CONDITIONS FRAMING
C     *                         300 LOOP.
C     * FEB 25/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS; REMOVE
C     *                         "ILW" SWITCH; SUPPRESS WATER VAPOUR FLUX
C     *                         IF PRECIPITATION IS OCCURRING.
C     * MAY 17/06 - D.VERSEGHY. SUPPRESS EVAPORATION WHEN PONDED WATER
C     *                         IS FREEZING; ADD IL1 AND IL2 TO CALL TO
C     *                         FLXSURFZ; REMOVE JL FROM CALL TO DRCOEF.
C     * APR 13/05 - R.BROWN. ADD WINDLESS TRANFER COEFFICIENT TO QSENS
C     *                         CALCULATION FOR SNOW PACKS.
C     * DEC 17/04 - Y.DELAGE/D.VERSEGHY. ADD SWITCH TO USE EITHER SECANT/
C     *                         BISECTION OR NEWTON-RAPHSON ITERATION
C     *                         SCHEME (WITH NUMBER OF ITERATIONS LIMITED
C     *                         TO FIVE AND CORRECTION FOR REMAINING
C     *                         RESIDUAL).
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS
C     *                         FROM ROUNDOFF ERRORS.
C     * NOV 07/02 - Y.DELAGE/D.VERSEGHY. NEW CALL TO FLXSURFZ.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * MAR 28/02 - D.VERSEGHY. STREAMLINED SUBROUTINE CALL.
C     *                         BYPASS EVAPORATION EFFICIENCY PARAMETER 
C     *                         IN CASES OF CONDENSATION.
C     * JAN 18/02 - P.BARTLETT/D.VERSEGHY. NEW "BETA" FORMULATION FOR 
C     *                         BARE SOIL EVAPORATION BASED ON LEE AND
C     *                         PIELKE.
C     * APR 11/01 - M.LAZARE.   SHORTENED "CLASS2" COMMON BLOCK.
C     * OCT 06/00 - D.VERSEGHY. CONDITIONAL "IF" IN ITERATION SEQUENCE
C     *                         TO AVOID DIVIDE BY ZERO.
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
C     *                         NOW DETERMINED IN ROUTINE "DRCOEF"
C     *                         "CFLUX" NOW WORK FIELD INSTEAD OF "CLIMIT".
C     * OCT 04/94 - D.VERSEGHY. CHANGE "CALL ABORT" TO "CALL XIT" TO
C     *                         ENABLE RUNNING ON PCS.
C     * JAN 24/94 - M.LAZARE.   UNFORMATTED I/O COMMENTED OUT IN LOOP 200.
C     * JUL 29/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REMOVE RE-DEFINITION OF QMELT NEAR END
C     *                         (SINCE DONE ELSEWHERE ALREADY) AND
C     *                         REDEFINE QSWNET FOR DIAGNOSTIC PURPOSES
C     *                         TO INCLUDE TRANSMISSION THROUGH 
C     *                         SNOWPACK.
C     * OCT 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.                  
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ITERATIVE SURFACE TEMPERATURE 
C     *                         CALCULATIONS FOR SNOW/SOIL.
C
      IMPLICIT NONE

C     * INTEGER CONSTANTS.
C
      INTEGER ISNOW,ISLFD,ITG,ILG,IG,IL1,IL2,JL,I
C
      INTEGER NUMIT,NIT,IBAD,ITERMX
C
C     * OUTPUT ARRAYS.
C
      REAL QSWNET(ILG),    QLWOUT(ILG),    QTRANS(ILG),    QSENS (ILG),    
     1     QEVAP (ILG),    EVAP  (ILG),    TZERO (ILG),    QZERO (ILG),    
     2     GZERO (ILG),    QMELT (ILG),    CDH   (ILG),    CDM   (ILG),    
     3     RIB   (ILG),    CFLUX (ILG),    FTEMP (ILG),    FVAP  (ILG),    
     4     ILMO  (ILG),    UE    (ILG),    H     (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    QSWINV(ILG),    QSWINI(ILG),    QLWIN (ILG),   
     1     TPOTA (ILG),    QA    (ILG),    VA    (ILG),    PADRY (ILG),    
     2     RHOAIR(ILG),    ALVISG(ILG),    ALNIRG(ILG),    CRIB  (ILG),    
     3     CPHCH (ILG),    CEVAP (ILG),    TVIRTA(ILG),    
     4     ZOSCLH(ILG),    ZOSCLM(ILG),    ZRSLFH(ILG),    ZRSLFM(ILG),
     5     ZOH   (ILG),    ZOM   (ILG),    GCONST(ILG),    GCOEFF(ILG),
     6     TSTART(ILG),    TRSNOW(ILG),    FCOR  (ILG),    PCPR  (ILG)

C
      INTEGER          IWATER(ILG),        IEVAP (ILG)   
      INTEGER          ITERCT(ILG,6,50),   ISAND(ILG,IG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL TSTEP (ILG),    TVIRTS(ILG),    EVBETA(ILG),    Q0SAT (ILG),
     1     RESID (ILG),    DCFLXM(ILG),    CFLUXM(ILG),    TRTOP (ILG),    
     2     A     (ILG),    B     (ILG),
     3     LZZ0  (ILG),    LZZ0T (ILG),    FM    (ILG),    FH    (ILG),
     4     WZERO (ILG)
C
      INTEGER              ITER  (ILG),    NITER (ILG),    JEVAP (ILG),
     1                     KF    (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL QSWNV,QSWNI,DCFLUX,DRDT0,TZEROT,QEVAPT,BOWEN,EZERO
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
C-----------------------------------------------------------------------
C     * INITIALIZATION AND PRE-ITERATION SEQUENCE.
C
      IF(ITG.LT.2) THEN
          ITERMX=12
      ELSE
          ITERMX=5
      ENDIF
C
C      IF(ISNOW.EQ.0) THEN
C          EZERO=0.0
C      ELSE
C          EZERO=2.0
C      ENDIF
       EZERO=0.0
C
      DO 50 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(ISNOW.EQ.0)                      THEN
                  TRTOP(I)=0.
              ELSE
                  TRTOP(I)=TRSNOW(I)
              ENDIF
              QSWNV=QSWINV(I)*(1.0-ALVISG(I))   
              QSWNI=QSWINI(I)*(1.0-ALNIRG(I))  
              QSWNET(I)=QSWNV+QSWNI           
              QTRANS(I)=QSWNET(I)*TRTOP(I)   
              QSWNET(I)=QSWNET(I)-QTRANS(I) 
              TZERO(I)=TSTART(I)            
              TSTEP(I)=1.0
              ITER(I)=1
              NITER(I)=1
C
              QMELT(I)=0.0                   
              RESID(I)=999999.
              DCFLXM(I)=0.0
              CFLUX(I)=0.0
              IF(ISNOW.EQ.1)                      THEN
                  KF(I)=3
              ELSE
                  KF(I)=6
              ENDIF
          ENDIF
   50 CONTINUE
C
C     * ITERATION SECTION.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS 
C     * ON THE CURRENT LATITUDE CIRCLE(S). 
C  
  100 CONTINUE
C
      NUMIT=0
      NIT=0
      DO 150 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              NIT=NIT+1
              CFLUXM(I)=CFLUX(I)
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
              IF(IWATER(I).GT.0)                              THEN
                  EVBETA(I)=1.0
                  QZERO(I)=Q0SAT(I)
              ELSE
                  EVBETA(I)=CEVAP(I)
                  QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QA(I)
                  IF(QZERO(I).GT.QA(I) .AND. IEVAP(I).EQ.0) THEN
                      EVBETA(I)=0.0
                      QZERO(I)=QA(I)
                  ENDIF
              ENDIF
              TVIRTS(I)=TZERO(I)*(1.0+0.61*QZERO(I))
          ENDIF
  150 CONTINUE      
C
      IF(NIT.GT.0)                                                  THEN
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C     * OTHER RELATED QUANTITIES.
C
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF (CDM,CDH,RIB,CFLUX,QZERO,QA,ZOSCLM,ZOSCLH,
     1                   CRIB,TVIRTS,TVIRTA,VA,FI,ITER,
     2                   ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TZERO,QZERO,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,ITER,JL )
        ENDIF
C
C     * REMAINING CALCULATIONS.
C
        DO 175 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              QLWOUT(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              IF(TZERO(I).LT.TPOTA(I))                        THEN
                  QSENS(I)=(RHOAIR(I)*SPHAIR*CFLUX(I)+EZERO)*(TZERO(I)-
     1                TPOTA(I))
              ELSE
                  QSENS(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TZERO(I)-
     1                TPOTA(I))
              ENDIF
              EVAP(I)=RHOAIR(I)*CFLUX(I)*(QZERO(I)-QA(I)) 
              QEVAP(I)=CPHCH(I)*EVAP(I)      
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              RESID(I)=QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1                 GZERO(I)                             
              IF(ABS(RESID(I)).LT.5.0)                       ITER(I)=0
              IF(ABS(TSTEP(I)).LT. 1.0E-2)                   ITER(I)=0
              IF(NITER(I).EQ.ITERMX .AND. ITER(I).EQ.1)      ITER(I)=-1
          ENDIF
175     CONTINUE
      ENDIF
C
      IF(ITG.LT.2) THEN
C
C     * OPTION #1: BISECTION ITERATION METHOD.
C
      IF(NIT.GT.0)                                                  THEN
        DO 180 I=IL1,IL2      
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN    
              IF(NITER(I).EQ.1) THEN
                  IF(RESID(I).GT.0.0) THEN
                      TZERO(I)=TZERO(I)+1.0
                  ELSE
                      TZERO(I)=TZERO(I)-1.0
                  ENDIF
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))    THEN 
                      TSTEP(I)=-TSTEP(I)/2.0     
                  ENDIF
                  TZERO(I)=TZERO(I)+TSTEP(I)
              ENDIF
          ENDIF
C
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  180   CONTINUE
      ENDIF
C
c     DO 185 I=IL1,IL2
C         IF(FI(I).GT.0. .AND. ITER(I).EQ.-1)                      THEN 
C             WRITE(6,6250) I,JL,RESID(I),TZERO(I),RIB(I)
C6250         FORMAT('0GROUND ITERATION LIMIT',3X,2I3,3(F8.2,E12.4))            
C         ENDIF                              
c 185 CONTINUE
C
      IF(NUMIT.GT.0)                                    GO TO 100
C
      ELSE
C
C     * OPTION #2: NEWTON-RAPHSON ITERATION METHOD.
C
      IF(NIT.GT.0)                                                  THEN
        DO 190 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                      THEN
              IF(NITER(I).GT.1)                                 THEN
                  DCFLUX=(CFLUX(I)-CFLUXM(I))/
     1                SIGN(MAX(.001,ABS(TSTEP(I))),TSTEP(I))
                  IF(ABS(TVIRTA(I)-TVIRTS(I)).LT.0.4)
     1                DCFLUX=MAX(DCFLUX,0.8*DCFLXM(I))
                  DCFLXM(I)=DCFLUX
              ELSE
                  DCFLUX=0.
              ENDIF
              DRDT0= -4.0*SBC*TZERO(I)**3
     1           -RHOAIR(I)*SPHAIR*(CFLUX(I)+MAX(0.,TZERO(I)-TPOTA(I))
     2           *DCFLUX) -GCOEFF(I)
     3           +CPHCH(I)*RHOAIR(I)*(CFLUX(I)*WZERO(I)*A(I)
     4           *EVBETA(I)*(B(I)-TFREZ)/((TZERO(I)-B(I))*
     5           (1.0+WZERO(I)))**2-(QZERO(I)-QA(I))*DCFLUX)
              TSTEP(I)=-RESID(I)/DRDT0
              TSTEP(I)=MAX(-10.,MIN(5.,TSTEP(I)))
              TZERO(I)=TZERO(I)+TSTEP(I)
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  190   CONTINUE
      ENDIF
C
      IF(NUMIT.GT.0)                                    GO TO 100
C
C     * IF CONVERGENCE HAS NOT BEEN REACHED, CALCULATE TEMPERATURE AND
C     * FLUXES ASSUMING NEUTRAL STABILITY.
C
      DO 195 I=IL1,IL2
          NUMIT=0
          JEVAP(I)=0
          IF(FI(I).GT.0. .AND.ITER(I).EQ.-1)                       THEN
              TZEROT=TVIRTA(I)/(1.0+0.61*QZERO(I))
              IF(ABS(RESID(I)).GT.50.) THEN
                  TZERO(I)=TZEROT
                  IF(TZERO(I).GE.TFREZ)                        THEN
                      A(I)=17.269
                      B(I)=35.86
                  ELSE
                      A(I)=21.874
                      B(I)=7.66
                  ENDIF
                  WZERO(I)=0.622*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/
     1                (TZERO(I)-B(I)))/PADRY(I)
                  Q0SAT(I)=WZERO(I)/(1.0+WZERO(I))
                  QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QA(I)
                  QLWOUT(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
                  GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
                  RESID(I)=QSWNET(I)+QLWIN(I)-QLWOUT(I)-GZERO(I)
                  IF(RESID(I).GT.0.)                 THEN
                      QEVAP(I)=RESID(I)
                  ELSE
                      QEVAP(I)=RESID(I)*0.5
                  ENDIF
                  QSENS(I)=RESID(I)-QEVAP(I)
                  RESID(I)=0.
                  EVAP(I)=QEVAP(I)/CPHCH(I)
                  TVIRTS(I)=TZERO(I)*(1.0+0.61*QZERO(I))
                  JEVAP(I)=1
                  NUMIT=NUMIT+1
              ENDIF
          ENDIF
  195 CONTINUE
C
      IF(NUMIT.GT.0)                   THEN
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF (CDM,CDH,RIB,CFLUX,QZERO,QA,ZOSCLM,ZOSCLH,
     1                   CRIB,TVIRTS,TVIRTA,VA,FI,JEVAP,
     2                   ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TZERO,QZERO,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,JEVAP,JL )
        ENDIF
      ENDIF
C
      ENDIF
C
C     * CHECK FOR BAD ITERATION TEMPERATURES.
C
      IBAD=0
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. (TZERO(I).LT.173.16 .OR. 
     1                           TZERO(I).GT.373.16))               THEN 
              IBAD=I
          ENDIF  
 200  CONTINUE
C
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6275) IBAD,JL,TZERO(IBAD),NITER(IBAD),ISNOW
 6275     FORMAT('0BAD ITERATION TEMPERATURE',3X,2I3,F16.2,2I4)
          WRITE(6,6280) QSWNET(IBAD),QLWIN(IBAD),QSENS(IBAD),
     1        QEVAP(IBAD),GZERO(IBAD),CFLUX(IBAD),RIB(IBAD)
 6280     FORMAT(2X,7F12.4)
          CALL XIT('TSOLVE',-1)
      ENDIF 
C
C     * POST-ITERATION CLEAN-UP. 
C
      NIT=0
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(((IWATER(I).EQ.1 .AND. TZERO(I).LT.TFREZ) .OR. 
     1            (IWATER(I).EQ.2 .AND. TZERO(I).GT.TFREZ)) .OR.
     2            (ISAND(I,1).EQ.-4 .AND. TZERO(I).GT.TFREZ))   THEN 
                  TZERO(I)=TFREZ        
                  WZERO(I)=0.622*611.0/PADRY(I)
                  QZERO(I)=WZERO(I)/(1.0+WZERO(I))    
                  TVIRTS(I)=TZERO(I)*(1.0+0.61*QZERO(I))
                  ITER(I)=1
                  NIT=NIT+1 
              ELSE
                  ITER(I)=0
              ENDIF  
          ENDIF
  300 CONTINUE
C
      IF(NIT.GT.0)                                                  THEN 
C
C       * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C       * OTHER RELATED QUANTITIES.
C
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF (CDM,CDH,RIB,CFLUX,QZERO,QA,ZOSCLM,ZOSCLH,
     1                   CRIB,TVIRTS,TVIRTA,VA,FI,ITER,
     2                   ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TZERO,QZERO,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,ITER,JL )
        ENDIF
      ENDIF
C
C     * REMAINING CALCULATIONS.
C
      DO 350 I=IL1,IL2 
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              QLWOUT(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              IF(TZERO(I).LT.TPOTA(I))                        THEN
                  QSENS(I)=(RHOAIR(I)*SPHAIR*CFLUX(I)+EZERO)*(TZERO(I)-
     1                TPOTA(I))
              ELSE
                  QSENS(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TZERO(I)-
     1                TPOTA(I))
              ENDIF
              EVAP(I)=RHOAIR(I)*CFLUX(I)*(QZERO(I)-QA(I)) 
              QEVAP(I)=CPHCH(I)*EVAP(I)       
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              QMELT(I)=QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1                 GZERO(I)                             
              RESID(I)=0.0
              IF(QMELT(I).LT.0.0) THEN
                  QMELT(I)=QMELT(I)+QEVAP(I)
                  QEVAP(I)=0.0
                  EVAP(I) =0.0
              ENDIF
          ENDIF                              
C
          IF(FI(I).GT.0.)                                 THEN
              IF(ABS(EVAP(I)).LT.1.0E-8) THEN
                  RESID(I)=RESID(I)+QEVAP(I)
                  EVAP(I)=0.0
                  QEVAP(I)=0.0
              ENDIF
              IF((ISNOW.EQ.1 .AND. QMELT(I).LT.0.0) .OR.
     1            (ISNOW.EQ.0 .AND. QMELT(I).GT.0.0))     THEN
                  GZERO(I)=GZERO(I)+QMELT(I)
                  QMELT(I)=0.0
              ENDIF
C              QSENS(I)=QSENS(I)+0.5*RESID(I)
C              GZERO(I)=GZERO(I)+0.5*RESID(I)
              QSENS(I)=QSENS(I)+RESID(I)
              QSWNET(I)=QSWNET(I)+QTRANS(I)
              EVAP(I)=EVAP(I)/RHOW
              ITERCT(I,KF(I),NITER(I))=ITERCT(I,KF(I),NITER(I))+1
          ENDIF
  350 CONTINUE
C
      RETURN                                                                      
      END  
