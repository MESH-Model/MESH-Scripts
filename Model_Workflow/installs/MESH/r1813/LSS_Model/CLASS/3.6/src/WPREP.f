      SUBROUTINE WPREP(THLQCO, THLQGO, THLQCS, THLQGS, THICCO, THICGO,
     1                 THICCS, THICGS, HCPCO,  HCPGO,  HCPCS,  HCPGS,
     2                 GRKSC,  GRKSG,  GRKSCS, GRKSGS,
     3                 SPCC,   SPCG,   SPCCS,  SPCGS,  TSPCC,  TSPCG,
     4                 TSPCCS, TSPCGS, RPCC,   RPCG,   RPCCS,  RPCGS,
     5                 TRPCC,  TRPCG,  TRPCCS, TRPCGS, EVPIC,  EVPIG,
     6                 EVPICS, EVPIGS, ZPONDC, ZPONDG, ZPNDCS, ZPNDGS,
     7                 XSNOWC, XSNOWG, XSNOCS, XSNOGS, ZSNOWC, ZSNOWG,
     8                 ZSNOCS, ZSNOGS, ALBSC,  ALBSG,  ALBSCS, ALBSGS, 
     9                 RHOSC,  RHOSG,  HCPSC,  HCPSG,  HCPSCS, HCPSGS, 
     A                 RUNFC,  RUNFG,  RUNFCS, RUNFGS,
     B                 TRUNFC, TRUNFG, TRNFCS, TRNFGS, TBASC,  TBASG,  
     C                 TBASCS, TBASGS, GFLXC,  GFLXG,  GFLXCS, GFLXGS,
     D                 SUBLC,  SUBLCS, WLOSTC, WLOSTG, WLSTCS, WLSTGS,
     E                 RAC,    RACS,   SNC,    SNCS,   TSNOWC, TSNOWG,
     F                 OVRFLW, SUBFLW, BASFLW, TOVRFL, TSUBFL, TBASFL, 
     G                 PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     H                 QFN,    QFG,    QFC,    HMFG,   
     I                 ROVG,   ROFC,   ROFN,   TRUNOF,
     +                 ICE,    TICE,   ICEG,   TICEG,  ICEGS,  TICEGS,
     J                 THLIQX, THICEX, THLDUM, THIDUM,
     J                 DT,     RDUMMY, ZERO,   IZERO,  DELZZ,
     K                 FC,     FG,     FCS,    FGS,    
     L                 THLIQC, THLIQG, THICEC, THICEG, HCPC,   HCPG,
     M                 TBARC,  TBARG,  TBARCS, TBARGS, TBASE,  TSURX,
     N                 FSVF,   FSVFS,  RAICAN, SNOCAN, RAICNS, SNOCNS, 
     O                 EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, 
     P                 RPCP,   TRPCP,  SPCP,   TSPCP,  RHOSNI, ZPOND,  
     Q                 ZSNOW,  ALBSNO, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     R                 THPOR,  HCPS,   GRKSAT, ISAND,  DELZW,  DELZ,
     S                 ILG,    IL1,    IL2,    JL,     IG,     IGP1,
     T                 NLANDCS,NLANDGS,NLANDC, NLANDG, RADD,   SADD,
     U                 BI, PSISAT, DD, XSLOPE, BULK_FC  )
C
C     * AUG 26/20 - D.PRINCZ.   CHANGED THE DIMENSIONS OF SUBFLW/TSUBFL
C                               TO PRESERVE THE PER-LAYER VALUES FOR
C                               INTERFLOW. THE TILE TOTALS ARE THE SUMS
C                               OF THE VALUES ALONG THE 2ND DIMENSION.
C     * JUN 10/20 - D.PRINCZ.   ADDED ICE AND TICE (ICEBAL).
C     * AUG 25/11 - D.VERSEGHY. REFINE CALCULATION OF TEMPERATURE OF
C     *                         LUMPED PRECIPITATION.
C     * NOV 24/09 - D.VERSEGHY. RESTORE EVAPOTRANSPIRATION WHEN
C     *                         PRECIPITATION IS OCCURRING.
C     * MAR 27/08 - D.VERSEGHY. MOVE MODIFICATION OF GRKSAT IN PRESENCE
C     *                         OF ICE TO GRINFL AND GRDRAN.
C     * FEB 19/07 - D.VERSEGHY. MODIFICATIONS TO REFLECT SHIFT OF CANOPY
C     *                         WATER DEPOSITION CALCULATIONS TO TSOLVC,
C     *                         AND SUPPRESSION OF ALL EVAPOTRANSPIRATION 
C     *                         WHEN PRECIPITATION IS OCCURRING.
C     * MAR 23/06 - D.VERSEGHY. MODIFY CALCULATIONS OF SNOW THERMAL
C     *                         PROPERTIES TO ACCOUNT FOR WATER CONTENT.
C     * MAR 21/06 - P.BARTLETT. INITIALIZE ADDITIONAL VARIABLES TO ZERO.
C     * DEC 07/05 - D.VERSEGHY. ADD INITIALIZATION OF TBASE SUBAREAS.
C     * OCT 05/05 - D.VERSEGHY. MODIFY DELZZ CALCULATION FOR IG>3.
C     * APR 15/05 - D.VERSEGHY. SUBLIMATION OF INTERCEPTED SNOW TAKES
C     *                         PLACE BEFORE EVAPORATION OF INTERCEPTED
C     *                         RAIN.
C     * MAR 30/05 - D.VERSEGHY/R.SOULIS. ADD RUNOFF TEMPERATURE 
C     *                         INITIALIZATIONS AND MODIFICATION OF 
C     *                         GRKSAT FOR TEMPERATURE AND PRESENCE 
C     *                         OF ICE.
C     * SEP 13/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 21/04 - Y.DELAGE, R.HARVEY, D.VERSEGHY. NEW LOWER LIMIT 
C     *                         ON RADD AND SADD.
C     * SEP 26/02 - D.VERSEGHY. MODIFICATIONS ASSOCIATED WITH BUGFIX
C     *                         IN SUBCAN.
C     * AUG 06/02 - D.VERSEGHY. SHORTENED CLASS3 COMMON BLOCK.
C     * JUN 18/02 - D.VERSEGHY. MOVE PARTITIONING OF PRECIPITATION
C     *                         BETWEEN RAINFALL AND SNOWFALL INTO
C     *                         "CLASSI"; TIDY UP SUBROUTINE CALL;
C     *                         CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * OCT 04/01 - M.LAZARE.   NEW DIAGNOSTIC FIELD "ROVG".
C     * NOV 09/00 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS FROM 
C     *                         SUBCAN INTO THIS ROUTINE.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 02/95 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION OF
C     *                         OVERLAND FLOW.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE USE OF "WLOST":
C     *                         COMPLETION OF WATER BUDGET DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         INITIALIZE TWO NEW DIAGNOSTIC FIELDS.
C     * AUG 20/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISED CALCULATION OF CANOPY 
C     *                         SUBLIMATION RATE.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NEW DIAGNOSTIC FIELDS. 
C     * APR 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. PREPARATION AND INITIALIZATION FOR
C     *                         LAND SURFACE WATER BUDGET CALCULATIONS.

      IMPLICIT NONE
C                                                     
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IG,IGP1,I,J,NLANDCS,NLANDGS,NLANDC,NLANDG
C
C     * OUTPUT ARRAYS.
C
      REAL THLQCO(ILG,IG),THLQGO(ILG,IG),THLQCS(ILG,IG),THLQGS(ILG,IG),               
     1     THICCO(ILG,IG),THICGO(ILG,IG),THICCS(ILG,IG),THICGS(ILG,IG),               
     2     HCPCO (ILG,IG),HCPGO (ILG,IG),HCPCS (ILG,IG),HCPGS (ILG,IG), 
     3     GRKSC (ILG,IG),GRKSG (ILG,IG),GRKSCS(ILG,IG),GRKSGS(ILG,IG),
     4     GFLXC (ILG,IG),GFLXG (ILG,IG),GFLXCS(ILG,IG),GFLXGS(ILG,IG),
     5     THLDUM(ILG,IG),THIDUM(ILG,IG),QFC   (ILG,IG),HMFG  (ILG,IG)
C
      REAL THLIQX(ILG,IGP1),THICEX(ILG,IGP1)
C
      REAL SPCC  (ILG),   SPCG  (ILG),   SPCCS (ILG),   SPCGS (ILG),
     1     TSPCC (ILG),   TSPCG (ILG),   TSPCCS(ILG),   TSPCGS(ILG),
     2     RPCC  (ILG),   RPCG  (ILG),   RPCCS (ILG),   RPCGS (ILG),
     3     TRPCC (ILG),   TRPCG (ILG),   TRPCCS(ILG),   TRPCGS(ILG),
     4     EVPIC (ILG),   EVPIG (ILG),   EVPICS(ILG),   EVPIGS(ILG),
     5     ZPONDC(ILG),   ZPONDG(ILG),   ZPNDCS(ILG),   ZPNDGS(ILG),
     6     XSNOWC(ILG),   XSNOWG(ILG),   XSNOCS(ILG),   XSNOGS(ILG),
     7     ZSNOWC(ILG),   ZSNOWG(ILG),   ZSNOCS(ILG),   ZSNOGS(ILG),
     8     ALBSC (ILG),   ALBSG (ILG),   ALBSCS(ILG),   ALBSGS(ILG),
     9     RHOSC (ILG),   RHOSG (ILG),  
     A     HCPSC (ILG),   HCPSG (ILG),   HCPSCS(ILG),   HCPSGS(ILG),
     B     RUNFC (ILG),   RUNFG (ILG),   RUNFCS(ILG),   RUNFGS(ILG),
     C     TRUNFC(ILG),   TRUNFG(ILG),   TRNFCS(ILG),   TRNFGS(ILG),
     D     TBASC (ILG),   TBASG (ILG),   TBASCS(ILG),   TBASGS(ILG)
C
      REAL SUBLC (ILG),   SUBLCS(ILG),   WLOSTC(ILG),   WLOSTG(ILG),
     1     WLSTCS(ILG),   WLSTGS(ILG),   RAC   (ILG),   RACS  (ILG),
     2     SNC   (ILG),   SNCS  (ILG),   TSNOWC(ILG),   TSNOWG(ILG),
     3     OVRFLW(ILG),SUBFLW(ILG,IG),   BASFLW(ILG),   
     4     TOVRFL(ILG),TSUBFL(ILG,IG),   TBASFL(ILG),
     5     PCFC  (ILG),   PCLC  (ILG),   PCPN  (ILG),   PCPG  (ILG),
     6     QFCF  (ILG),   QFCL  (ILG),   QFN   (ILG),   QFG   (ILG),
     7     ROVG  (ILG),   ROFC  (ILG),   ROFN  (ILG),   
     8     TRUNOF(ILG),   DT    (ILG),   RDUMMY(ILG),   ZERO  (ILG),
     +     ICE   (ILG),   TICE  (ILG),
     +     ICEG  (ILG),   TICEG (ILG),   ICEGS (ILG),   TICEGS(ILG)
C 
      INTEGER             IZERO (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),
     1     FSVF  (ILG),   FSVFS (ILG),   RAICAN(ILG),   SNOCAN(ILG),   
     2     RAICNS(ILG),   SNOCNS(ILG),   EVAPC (ILG),   EVAPCG(ILG),
     3     EVAPG (ILG),   EVAPCS(ILG),   EVPCSG(ILG),   EVAPGS(ILG),
     4     RPCP  (ILG),   TRPCP (ILG),   SPCP  (ILG),   TSPCP (ILG),
     5     RHOSNI(ILG),   ZPOND (ILG),   ZSNOW (ILG),   ALBSNO(ILG),  
     6     WSNOCS(ILG),   WSNOGS(ILG),   RHOSCS(ILG),   RHOSGS(ILG),
     7     TBASE (ILG),   TSURX(ILG,4)
C
      REAL THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),           
     1     TBARC (ILG,IG),TBARG (ILG,IG),TBARCS(ILG,IG),TBARGS(ILG,IG),
     2     HCPC  (ILG,IG),HCPG  (ILG,IG)
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG),HCPS  (ILG,IG),GRKSAT(ILG,IG),
     1     DELZZ (ILG,IG),DELZW (ILG,IG),DELZ  (IG), 
     2     BI(ILG,IG),PSISAT(ILG,IG),DD(ILG),XSLOPE(ILG),BULK_FC(ILG,IG)
C
      INTEGER             ISAND (ILG,IG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL RADD  (ILG),   SADD  (ILG)  
C                                                                                  
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C     * INITIALIZE 2-D ARRAYS.
C
      DO 50 J=1,IG
      !$omp parallel do
      DO 50 I=IL1,IL2                                                               
          THLQCO(I,J)=0.0                                                           
          THLQGO(I,J)=0.0                                                           
          THLQCS(I,J)=0.0                                                           
          THLQGS(I,J)=0.0
          THICCO(I,J)=0.0                                                           
          THICGO(I,J)=0.0                                                           
          THICCS(I,J)=0.0                                                           
          THICGS(I,J)=0.0
          HCPCO (I,J)=0.0                                                            
          HCPGO (I,J)=0.0                                                            
          HCPCS (I,J)=0.0                                                            
          HCPGS (I,J)=0.0                                                            
          GRKSC (I,J)=0.0                                                            
          GRKSG (I,J)=0.0                                                            
          GRKSCS(I,J)=0.0                                                            
          GRKSGS(I,J)=0.0                                                            
          GFLXC (I,J)=0.0
          GFLXG (I,J)=0.0
          GFLXCS(I,J)=0.0
          GFLXGS(I,J)=0.0
          QFC   (I,J)=0.0
          HMFG  (I,J)=0.0
          THLDUM(I,J)=0.0
          THIDUM(I,J)=0.0
          SUBFLW(I,J)=0.0
          TSUBFL(I,J)=0.0
          IF(J.EQ.3.AND.IG.EQ.3) THEN
              DELZZ (I,J)=DELZW(I,J)
          ELSE
              DELZZ (I,J)=DELZ(J)
          ENDIF
c         calculate BULK_FC, make sure we don't divide by 0 (for slope = 0 or bedrock i.e. %sand = -2)
          IF(XSLOPE(I).gt.0.0.AND.THPOR(I,J).gt.0.0) THEN
              BULK_FC(I,J)= (THPOR(I,J)/(BI(I,J)-1))*
     +          ((2*DD(I)*PSISAT(I,J)*BI(I,J)/XSLOPE(I))**(1/BI(I,J)))*
     +          ((3*BI(I,J)+2)**((BI(I,J)-1)/BI(I,J))-
     +           (2*BI(I,J)+2)**((BI(I,J)-1)/BI(I,J)))
          ELSE
c         make sure that BULK_FC is high to shut off interflow
              BULK_FC(I,J) = 2.0
          ENDIF
c          print *, "I = ", I, "J = ", J, "BULK_FC(I,J) = ", BULK_FC(I,J)
   50 CONTINUE
C 
      DO 75 J=1,IGP1
      !$omp parallel do
      DO 75 I=IL1,IL2
          THLIQX(I,J)=0.0
          THICEX(I,J)=0.0
   75 CONTINUE
C
C     * INITIALIZE OTHER DIAGNOSTIC AND WORK ARRAYS.
C
!$omp parallel do
      DO 100 I=IL1,IL2
          EVPICS(I)=EVAPCS(I)+EVPCSG(I)
          EVPIGS(I)=EVAPGS(I)
          EVPIC (I)=EVAPC (I)+EVAPCG(I)
          EVPIG (I)=EVAPG (I)
          TSNOWC(I)=0.0
          TSNOWG(I)=0.0
          WLOSTC(I)=0.0                                                                  
          WLOSTG(I)=0.0                                                                  
          WLSTCS(I)=0.0                                                                  
          WLSTGS(I)=0.0                                                                  
          RAC   (I)=RAICAN(I)
          RACS  (I)=RAICNS(I)                                                                 
          SNC   (I)=SNOCAN(I)                                                                  
          SNCS  (I)=SNOCNS(I)
          PCFC  (I)=0.0
          PCLC  (I)=0.0
          PCPN  (I)=0.0
          PCPG  (I)=0.0
          QFN   (I)=0.0
          QFG   (I)=0.0
          ROVG  (I)=0.0
          ROFC  (I)=0.0
          ROFN  (I)=0.0
          OVRFLW(I)=0.0
!-          SUBFLW(I)=0.0
          BASFLW(I)=0.0
          TOVRFL(I)=0.0
!-          TSUBFL(I)=0.0
          TBASFL(I)=0.0
          ZPONDC(I)=0.0
          ZPONDG(I)=0.0
          ZPNDCS(I)=0.0
          ZPNDGS(I)=0.0
          XSNOWC(I)=0.0
          XSNOWG(I)=0.0
          XSNOCS(I)=0.0
          XSNOGS(I)=0.0
          ZSNOWC(I)=0.0
          ZSNOWG(I)=0.0
          ZSNOCS(I)=0.0
          ZSNOGS(I)=0.0
          ALBSC (I)=0.0
          ALBSG (I)=0.0
          ALBSCS(I)=0.0
          ALBSGS(I)=0.0
          RHOSC (I)=0.0
          RHOSG (I)=0.0
          HCPSC (I)=0.0
          HCPSG (I)=0.0
          HCPSCS(I)=0.0
          HCPSGS(I)=0.0
          RUNFC (I)=0.0
          RUNFG (I)=0.0
          RUNFCS(I)=0.0
          RUNFGS(I)=0.0
          TRUNFC(I)=0.0
          TRUNFG(I)=0.0
          TRNFCS(I)=0.0
          TRNFGS(I)=0.0
          TRUNOF(I)=0.0
          ICE   (I)=0.0
          TICE  (I)=0.0
          ICEG  (I)=0.0
          TICEG (I)=0.0
          ICEGS (I)=0.0
          TICEGS(I)=0.0
          TBASC (I)=TBASE(I)-TFREZ
          TBASG (I)=TBASE(I)-TFREZ
          TBASCS(I)=TBASE(I)-TFREZ
          TBASGS(I)=TBASE(I)-TFREZ
          DT    (I)=DELT
          RDUMMY(I)=0.
          ZERO  (I)=0.
          IZERO (I)=0 
C                                                                 
C     * PRECIPITATION DIAGNOSTICS.
C
          IF(RPCP(I).GT.0.)                                      THEN 
              PCLC(I)=(FCS(I)*(1.0-FSVFS(I))+FC(I)*(1.0-FSVF(I)))*
     1                RPCP(I)*RHOW
              PCPN(I)=(FCS(I)*FSVFS(I)+FGS(I))*RPCP(I)*RHOW
              PCPG(I)=(FC(I)*FSVF(I)+FG(I))*RPCP(I)*RHOW
          ENDIF
C
          IF(SPCP(I).GT.0.)                                      THEN 
              PCFC(I)=(FCS(I)*(1.0-FSVFS(I))+FC(I)*(1.0-FSVF(I)))*
     1                SPCP(I)*RHOSNI(I)
              PCPN(I)=PCPN(I)+(FCS(I)*FSVFS(I)+FGS(I)+
     1                FC(I)*FSVF(I)+FG(I))*SPCP(I)*RHOSNI(I)
          ENDIF
  100 CONTINUE
C
C     * RAINFALL/SNOWFALL RATES AND OTHER INITIALIZATION PROCEDURES
C     * OVER GRID CELL SUBAREAS. DOWNWARD WATER FLUXES ARE LUMPED
C     * TOGETHER WITH PRECIPITATION, AND UPWARD AND DOWNWARD WATER
C     * FLUXES CANCEL OUT. CORRECTION MADE TO SOIL SATURATED HYDRAULIC
C     * CONDUCTIVITY FOR WATER VISCOSITY EFFECTS. 
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C
      IF(NLANDCS.GT.0)                                              THEN
C
          DO 200 J=1,IG
          !$omp parallel do
          DO 200 I=IL1,IL2
              IF(FCS(I).GT.0.)                                THEN 
                  THLQCS(I,J)=THLIQC(I,J)                                               
                  THICCS(I,J)=THICEC(I,J)                                               
                  HCPCS (I,J)=HCPC  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSCS(I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARCS(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSCS(I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  200     CONTINUE
C
!$omp parallel do
          DO 250 I=IL1,IL2
              IF(FCS(I).GT.0.)                           THEN  
                  IF(SNOCNS(I).GT.0.)      THEN                                                  
                      SUBLCS(I)=EVAPCS(I)
                      EVAPCS(I)=0.0
                  ELSE                                                                    
                      SUBLCS(I)=0.0                                                          
                  ENDIF
                  IF(SUBLCS(I).GT.0.0) THEN
                      QFCF(I)=QFCF(I)+FCS(I)*SUBLCS(I)*RHOW
                  ELSE
                      QFCF(I)=QFCF(I)+FCS(I)*(1.0-FSVFS(I))*SUBLCS(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*SUBLCS(I)*RHOW
                  ENDIF
                  IF(EVAPCS(I).GT.0.0) THEN
                      QFCL(I)=QFCL(I)+FCS(I)*EVAPCS(I)*RHOW
                  ELSE
                      QFCL(I)=QFCL(I)+FCS(I)*(1.0-FSVFS(I))*EVAPCS(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*EVAPCS(I)*RHOW
                  ENDIF
C
                  IF(SPCP(I).GT.0. .OR. SUBLCS(I).LT.0.) THEN                                      
                      SADD(I)=SPCP(I)-SUBLCS(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          IF(SUBLCS(I).GT.0.) THEN
                              QFCF(I)=QFCF(I)-FCS(I)*FSVFS(I)*
     1                                SUBLCS(I)*RHOW
                              QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*
     1                                SUBLCS(I)*RHOW
                          ENDIF
                          SPCCS (I)=SADD(I)                                                        
                          IF(SPCP(I).GT.0.0) THEN
                              TSPCCS(I)=TSPCP(I)+TFREZ                                                   
                          ELSE
                              TSPCCS(I)=MIN(TSURX(I,1),TFREZ)
                          ENDIF
                          SUBLCS(I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FCS(I)*FSVFS(I)*SPCP(I)*
     1                        RHOSNI(I)
                          PCFC(I)=PCFC(I)+FCS(I)*FSVFS(I)*SPCP(I)*
     1                        RHOSNI(I)
                          SUBLCS(I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCCS (I)=0.0                                                         
                          TSPCCS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCCS(I)=0.0                                                             
                      TSPCCS(I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0. .OR. EVAPCS(I).LT.0.) THEN                                      
                      RADD(I)=RPCP(I)-EVAPCS(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          IF(EVAPCS(I).GT.0.) THEN
                              QFCL(I)=QFCL(I)-FCS(I)*FSVFS(I)*
     1                                EVAPCS(I)*RHOW
                              QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*
     1                                EVAPCS(I)*RHOW
                          ENDIF
                          RPCCS (I)=RADD(I)                                                        
                          IF(RPCP(I).GT.0.0) THEN
                              TRPCCS(I)=TRPCP(I)+TFREZ                                                   
                          ELSE
                              TRPCCS(I)=MAX(TSURX(I,1),TFREZ)
                          ENDIF
                          EVAPCS(I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FCS(I)*FSVFS(I)*RPCP(I)*RHOW
                          PCLC(I)=PCLC(I)+FCS(I)*FSVFS(I)*RPCP(I)*RHOW
                          EVAPCS(I)=-RADD(I)                                                    
                          RPCCS (I)=0.0                                                         
                          TRPCCS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCCS(I)=0.0                                                             
                      TRPCCS(I)=0.0                                                            
                  ENDIF                                                                   
                  ZPNDCS(I)=ZPOND (I)                                                            
                  ZSNOCS(I)=ZSNOW (I)                                                            
                  ALBSCS(I)=ALBSNO(I)                                                           
                  HCPSCS(I)=HCPICE*RHOSCS(I)/RHOICE+HCPW*WSNOCS(I)/
     1                      (RHOW*ZSNOCS(I))
                  QFN   (I)=QFN(I)+FCS(I)*EVPCSG(I)*RHOW
              ENDIF
  250     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
C
!$omp parallel do
          DO 300 J=1,IG
          DO 300 I=IL1,IL2
              IF(FGS(I).GT.0.)                                 THEN 
                  THLQGS(I,J)=THLIQG(I,J)                                               
                  THICGS(I,J)=THICEG(I,J)                                               
                  HCPGS (I,J)=HCPG  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSGS(I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARGS(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSGS(I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  300     CONTINUE
C
!$omp parallel do
          DO 350 I=IL1,IL2
              IF(FGS(I).GT.0.)                              THEN 
                  QFN(I)=QFN(I)+FGS(I)*EVAPGS(I)*RHOW
                  IF(SPCP(I).GT.0. .OR. EVAPGS(I).LT.0.) THEN                                      
                      SADD(I)=SPCP(I)-EVAPGS(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          SPCGS (I)=SADD(I)                                                        
                          IF(SPCP(I).GT.0.0) THEN
                              TSPCGS(I)=TSPCP(I)
                          ELSE
                              TSPCGS(I)=MIN((TSURX(I,2)-TFREZ),0.0)
                          ENDIF
                          EVAPGS(I)=0.0                                                      
                      ELSE                                                                
                          EVAPGS(I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCGS (I)=0.0                                                         
                          TSPCGS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCGS (I)=0.0                                                             
                      TSPCGS(I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0.)                         THEN                                      
                      RADD(I)=RPCP(I)-EVAPGS(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          RPCGS (I)=RADD(I)                                                        
                          TRPCGS(I)=TRPCP(I)
                          EVAPGS(I)=0.0                                                      
                      ELSE                                                                
                          EVAPGS(I)=-RADD(I)                                                    
                          RPCGS (I)=0.0                                                         
                          TRPCGS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCGS (I)=0.0                                                             
                      TRPCGS(I)=0.0                                                            
                  ENDIF                                                                   
                  ZPNDGS(I)=ZPOND (I)                                                            
                  ZSNOGS(I)=ZSNOW (I)                                                            
                  ALBSGS(I)=ALBSNO(I)                                                           
                  HCPSGS(I)=HCPICE*RHOSGS(I)/RHOICE+HCPW*WSNOGS(I)/
     1                      (RHOW*ZSNOGS(I))
              ENDIF
  350     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C
      IF(NLANDC.GT.0)                                               THEN
C
          DO 400 J=1,IG
          !$omp parallel do
          DO 400 I=IL1,IL2
              IF(FC(I).GT.0.)                                 THEN  
                  THLQCO(I,J)=THLIQC(I,J)                                               
                  THICCO(I,J)=THICEC(I,J)                                               
                  HCPCO (I,J)=HCPC  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSC (I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARC(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSC (I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  400     CONTINUE
C
!$omp parallel do
          DO 450 I=IL1,IL2
              IF(FC(I).GT.0.)                            THEN 
                  IF(SNOCAN(I).GT.0.)      THEN                                                  
                      SUBLC(I)=EVAPC(I)
                      EVAPC(I)=0.0
                  ELSE                                                                    
                      SUBLC(I)=0.0                                                          
                  ENDIF
                  IF(SUBLC(I).GT.0.0) THEN
                      QFCF(I)=QFCF(I)+FC(I)*SUBLC(I)*RHOW
                  ELSE
                      QFCF(I)=QFCF(I)+FC(I)*(1.0-FSVF(I))*SUBLC(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FC(I)*FSVF(I)*SUBLC(I)*RHOW
                  ENDIF
                  IF(EVAPC(I).GT.0.0) THEN
                      QFCL(I)=QFCL(I)+FC(I)*EVAPC(I)*RHOW
                  ELSE
                      QFCL(I)=QFCL(I)+FC(I)*(1.0-FSVF(I))*EVAPC(I)*
     1                        RHOW
                      QFG(I)=QFG(I)+FC(I)*FSVF(I)*EVAPC(I)*RHOW
                  ENDIF
C
                  IF(SPCP(I).GT.0. .OR. SUBLC(I).LT.0.)  THEN                                      
                      SADD(I)=SPCP(I)-SUBLC(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          IF(SUBLC(I).GT.0.) THEN
                              QFCF(I)=QFCF(I)-FC(I)*FSVF(I)*SUBLC(I)*
     1                                RHOW
                              QFN(I)=QFN(I)+FC(I)*FSVF(I)*SUBLC(I)*
     1                                RHOW
                          ENDIF
                          SPCC  (I)=SADD(I)                                                        
                          IF(SPCP(I).GT.0.0) THEN
                             TSPCC (I)=TSPCP(I)+TFREZ                                                   
                          ELSE
                             TSPCC(I)=MIN(TSURX(I,3),TFREZ)
                          ENDIF
                          SUBLC (I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FC(I)*FSVF(I)*SPCP(I)*
     1                        RHOSNI(I)
                          PCFC(I)=PCFC(I)+FC(I)*FSVF(I)*SPCP(I)*
     1                        RHOSNI(I)
                          SUBLC (I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCC  (I)=0.0                                                         
                          TSPCC (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCC  (I)=0.0                                                             
                      TSPCC (I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0. .OR. EVAPC(I).LT.0.)  THEN                                      
                      RADD(I)=RPCP(I)-EVAPC(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          IF(EVAPC(I).GT.0.) THEN
                              QFCL(I)=QFCL(I)-FC(I)*FSVF(I)*EVAPC(I)*
     1                                RHOW
                              QFG(I)=QFG(I)+FC(I)*FSVF(I)*EVAPC(I)*
     1                                RHOW
                          ENDIF
                          RPCC  (I)=RADD(I)                                                        
                          IF(RPCP(I).GT.0.0) THEN
                              TRPCC (I)=TRPCP(I)+TFREZ  
                          ELSE
                              TRPCC (I)=MAX(TSURX(I,3),TFREZ)
                          ENDIF
                          EVAPC (I)=0.0                                                      
                      ELSE   
                          PCPG(I)=PCPG(I)-FC(I)*FSVF(I)*RPCP(I)*RHOW
                          PCLC(I)=PCLC(I)+FC(I)*FSVF(I)*RPCP(I)*RHOW
                          EVAPC (I)=-RADD(I)                                                    
                          RPCC  (I)=0.0                                                         
                          TRPCC (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCC  (I)=0.0                                                             
                      TRPCC (I)=0.0                                                            
                  ENDIF         
                  ZPONDC(I)=ZPOND (I)                                                            
                  ZSNOWC(I)=0.
                  RHOSC (I)=0.
                  HCPSC (I)=0.
                  QFG   (I)=QFG(I)+FC(I)*EVAPCG(I)*RHOW
              ENDIF
  450     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR BARE GROUND.
C
      IF(NLANDG.GT.0)                                               THEN
C
          DO 500 J=1,IG
          !$omp parallel do
          DO 500 I=IL1,IL2
              IF(FG(I).GT.0.)                                 THEN 
                  THLQGO(I,J)=THLIQG(I,J)                                               
                  THICGO(I,J)=THICEG(I,J)                                               
                  HCPGO (I,J)=HCPG  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSG (I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARG(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSG (I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  500     CONTINUE
C
!$omp parallel do
          DO 550 I=IL1,IL2
              IF(FG(I).GT.0.)                              THEN 
                  QFG(I)=QFG(I)+FG(I)*EVAPG(I)*RHOW
                  IF(SPCP(I).GT.0.)                 THEN                                      
                      SADD(I)=SPCP(I)-EVAPG(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          QFN(I)=QFN(I)+FG(I)*EVAPG(I)*RHOW
                          QFG(I)=QFG(I)-FG(I)*EVAPG(I)*RHOW
                          SPCG  (I)=SADD(I)                                                        
                          TSPCG (I)=TSPCP(I)
                          EVAPG (I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FG(I)*SPCP(I)*RHOSNI(I)
                          PCPG(I)=PCPG(I)+FG(I)*SPCP(I)*RHOSNI(I)
                          EVAPG (I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCG  (I)=0.0                                                         
                          TSPCG (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCG  (I)=0.0                                                             
                      TSPCG (I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0. .OR. EVAPG(I).LT.0.)   THEN                                      
                      RADD(I)=RPCP(I)-EVAPG(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          RPCG  (I)=RADD(I)                                                        
                          IF(RPCP(I).GT.0.0) THEN
                              TRPCG (I)=TRPCP(I)
                          ELSE
                              TRPCG (I)=MAX((TSURX(I,4)-TFREZ),0.0)
                          ENDIF
                          EVAPG (I)=0.0                                                      
                      ELSE                                                                
                          EVAPG (I)=-RADD(I)                                                    
                          RPCG  (I)=0.0                                                         
                          TRPCG (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCG (I)=0.0                                                             
                      TRPCG(I)=0.0                                                            
                  ENDIF     
                  ZPONDG(I)=ZPOND (I)                                                            
                  ZSNOWG(I)=0.
                  RHOSG (I)=0.
                  HCPSG (I)=0.
              ENDIF
  550     CONTINUE
      ENDIF
C
      RETURN                                                                      
      END
