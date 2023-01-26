      SUBROUTINE TPREP(THLIQC, THLIQG, THICEC, THICEG, TBARC,  TBARG,             
     1                 TBARCS, TBARGS, HCPC,   HCPG,   TCTOPC, TCBOTC,
     2                 TCTOPG, TCBOTG, HCPSCS, HCPSGS, TCSNOW, TSNOCS, 
     3                 TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, TCANO,  
     4                 TCANS,  CEVAP,  IEVAP,  TBAR1P, WTABLE, ZERO,
     5                 EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS,            
     6                 GSNOWC, GSNOWG, GZEROC, GZEROG, GZROCS, GZROGS,
     7                 QMELTC, QMELTG, EVAP,
     8                 TPONDC, TPONDG, TPNDCS, TPNDGS, QSENSC, QSENSG, 
     9                 QEVAPC, QEVAPG, TACCO,  QACCO,  TACCS,  QACCS,  
     A                 ILMOX,  UEX,    HBLX,
     B                 ILMO,   UE,     HBL,    
     C                 ST,     SU,     SV,     SQ,     CDH,    CDM,              
     D                 QSENS,  QEVAP,  QLWAVG, 
     E                 FSGV,   FSGS,   FSGG,   FLGV,   FLGS,   FLGG,   
     F                 HFSC,   HFSS,   HFSG,   HEVC,   HEVS,   HEVG,   
     G                 HMFC,   HMFN,   QFCF,   QFCL,   EVPPOT, ACOND,  
     H                 DRAG,   THLIQ,  THICE,  TBAR,   ZPOND,  TPOND,  
     I                 THPOR,  THLMIN, THLRET, THFC,   HCPS,   TCS,    
     J                 TA,     RHOSNO, TSNOW,  ZSNOW,  WSNOW,  TCAN,
     K                 FC,     FCS,    DELZ,   DELZW,  ZBOTW,
     L                 ISAND,  ILG,    IL1,    IL2,    JL,     IG,  
     M                 FVEG,   TCSATU, TCSATF, FTEMP,  FTEMPX, FVAP,
     N                 FVAPX,  RIB,    RIBX  )           
C
C     * NOV 24/11 - R.HARVEY.   NEW SNOW THERMAL CONDUCTIVITY FROM
C     *                         STURM ET AL. (1997).
C     * OCT 12/11 - M.LAZARE.   REMOVED TSURF.     
C     * AUG   /08 - J.P.PAQUIN. ADD CALCULATION FOR FTEMP, FVAP AND
C     *                         RIB FOR OUTPUT IN GEM (IMPLEMENTED BY
C     *                         L. DUARTE ON OCT. 28/08).
C     * MAR 20/08 - D.VERSEGHY. REMOVE TBAR3, TCTOP3, TCBOT3.
C     * DEC 12/07 - D.VERSEGHY. MAJOR REVISIONS TO CALCULATION OF
C     *                         SOIL THERMAL CONDUCTIVITY.
C     * MAY 18/06 - D.VERSEGHY. ADJUST CALCULATION OF TBAR1P FOR ROCK
C     *                         SOILS; LIMIT CALCULATION OF TBAR3(I,3)
C     *                         TO UPPER 4.1 M OF SOIL; CORRECT WTABLE
C     *                         TO ACCOUNT FOR PRESENCE OF ICE.
C     * MAR 23/06 - D.VERSEGHY. MODIFY CALCULATION OF HCPSNO TO ACCOUNT
C     *                         FOR PRESENCE OF WATER IN SNOWPACK.
C     * MAR 21/06 - P.BARTLETT. INITIALIZE ADDITIONAL VARIABLES TO ZERO.
C     * OCT 04/05 - D.VERSEGHY. NEW VARIABLES TBAR3,TCTOP3,TCBOT3.
C     * APR 08/05 - Y.DELAGE. TCTOP VARIES GRADUALLY WITH ZPOND TO TCW.
C     * MAR 16/05 - D.VERSEGHY. TREAT FROZEN SOIL WATER AS ICE
C     *                         VOLUME RATHER THAN AS EQUIVALENT 
C     *                         LIQUID WATER VOLUME; REVERSE ORDER
C     *                         IN LOOP 500.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 05/04 - Y.DELAGE/D.VERSEGHY. INITIALIZE NEW DIAGNOSTIC
C     *                         VARIABLES ILMO, UE AND HBL.
C     * JUL 30/02 - D.VERSEGHY. MOVE CALCULATION OF VEGETATION 
C     *                         STOMATAL RESISTANCE INTO APREP
C     *                         AND CANALB; SHORTENED CLASS3
C     *                         COMMON BLOCK.
C     * JUN 17/02 - D.VERSEGHY. NEW THERMAL ARRAYS FOR SURFACE 
C     *                         TEMPERATURE ITERATION, WITH PONDED
C     *                         WATER ROLLED INTO SOIL UPPER LAYER;
C     *                         SHORTENED CLASS4 COMMON BLOCK.
C     * MAR 20/02 - D.VERSEGHY. MOVE CALCULATION OF BACKGROUND SOIL 
C     *                         PROPERTIES INTO "CLASSB"; UPDATES 
C     *                         TO MAKE ZPOND A PROGNOSTIC VARIABLE.
C     * FEB 27/02 - D.VERSEGHY. RECALCULATE WILTING POINT BASED ON
C     *                         FIELD CAPACITY.
C     * JAN 18/02 - D.VERSEGHY. INTRODUCTION OF CALCULATION OF FIELD
C     *                         CAPACITY AND NEW BARE SOIL EVAPORATION
C     *                         PARAMETERS.
C     * APR 11/01 - M.LAZARE.   SHORTENED "CLASS2" COMMON BLOCK.     
C     * NOV 01/00 - A.WU/D.VERSEGHY. EXTEND MINERAL SOIL CALCULATION
C     *                              OF SOIL EVAPORATION "BETA" TO
C     *                              ORGANIC SOILS.
C     * SEP 19/00 - A.WU/D.VERSEGHY. CHANGE CALCULATION OF THERMAL
C     *                              CONDUCTIVITY FOR ORGANIC SOILS,
C     *                              USING METHOD OF FAROUKI (1981).
C     *                              ALSO, CALCULATE STOMATAL RESISTANCE
C     *                              USING VEGETATION-VARYING 
C     *                              COEFFICIENTS FOR ENVIRONMENTAL
C     *                              VARIABLES.
C     * FEB 14/00 - D.VERSEGHY. INSERT CALCULATION OF WATER TABLE DEPTH
C     *                         FOR ORGANIC SOILS.
C     * DEC 07/99 - A.WU/D.VERSEGHY.  INCORPORATE CALCULATION OF "BETA"
C     *                               PARAMETER FOR NEW SOIL EVAPORATION
C     *                               FORMULATION.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 24/97 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         SET RC AND RCS TO ZERO FOR GRID CELLS
C     *                         WITH NO VEGETATION.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REMOVE SUBTRACTION OF RESIDUAL SOIL
C     *                         MOISTURE CONTENT IN CALCULATIONS OF
C     *                         "PSIZRO" AND "PSII".
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         INITIALIZE THREE NEW DIAGNOSTIC FIELDS.
C     * NOV 12/94 - D.VERSEGHY. SET INITIAL TEMPERATURE OF EMERGING
C     *                         CANOPY TO TA INSTEAD OF TO ZERO.
C     * JAN 31/94 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         INTRODUCE LIMITING VALUES INTO
C     *                         CALCULATION OF "PSIZRO" TO AVOID
C     *                         OVERFLOWS.
C     * JUL 27/93 - D.VERSEGHY/M.LAZARE. INITIALIZE NEW DIAGNOSTIC 
C     *                                  FIELDS FSGV,FSGG,FLGV,FLGG,
C     *                                  HFSC,HFSG,HMFC.
C     * MAY 06/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  MODIFICATIONS TO CANOPY
C     *                                  RESISTANCE TO ADD "RCS"
C     *                                  FIELD FOR SNOW-COVERED
C     *                                  CANOPY. 
C     * JUL 04/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.                            
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).            
C     * APR 11/89 - D.VERSEGHY. PREPARATION AND INITIALIZATION FOR
C     *                         LAND SURFACE ENERGY BUDGET 
C     *                         CALCULATIONS.
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IG,I,J  
C                                                                                 
C     * OUTPUT ARRAYS.                                                            
C                                                                                 
      REAL TBARC (ILG,IG),TBARG (ILG,IG),TBARCS(ILG,IG),TBARGS(ILG,IG),
     1     THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),           
     2     HCPC  (ILG,IG),HCPG  (ILG,IG),TCTOPC(ILG,IG),TCBOTC(ILG,IG),
     3     TCTOPG(ILG,IG),TCBOTG(ILG,IG)
C                                                                                 
      REAL HCPSCS(ILG),   HCPSGS(ILG),   TCSNOW(ILG),   TSNOGS(ILG),   
     1     TSNOCS(ILG),   WSNOCS(ILG),   WSNOGS(ILG),   RHOSCS(ILG),   
     2     RHOSGS(ILG),   TCANO (ILG),   TCANS (ILG),   CEVAP (ILG),   
     3     TBAR1P(ILG),   WTABLE(ILG),   ZERO  (ILG), 
     4     TPONDC(ILG),   TPONDG(ILG),   TPNDCS(ILG),   TPNDGS(ILG)
C
      INTEGER             IEVAP (ILG)
C                                                                                 
C     * OUTPUT ARRAYS WHICH ARE INTERNAL WORK ARRAYS FOR CLASST                   
C     * AND ARE INITIALIZED TO ZERO HERE.                                               
C                                                                                 
      REAL EVAPC (ILG),   EVAPCG(ILG),   EVAPG (ILG),   EVAPCS(ILG),              
     1     EVPCSG(ILG),   EVAPGS(ILG),   GSNOWC(ILG),   GSNOWG(ILG),   
     2     GZEROC(ILG),   GZEROG(ILG),   GZROCS(ILG),   GZROGS(ILG),
     3     QMELTC(ILG),   QMELTG(ILG),   EVAP  (ILG),
     4     QSENSC(ILG),   QSENSG(ILG),   QEVAPC(ILG),   QEVAPG(ILG),  
     5     TACCO (ILG),   QACCO (ILG),   TACCS (ILG),   QACCS (ILG)   
C
C     * DIAGNOSTIC ARRAYS.
C
      REAL ST    (ILG),   SU    (ILG),   SV    (ILG),   SQ    (ILG),
     1     CDH   (ILG),   CDM   (ILG),  
     2     QSENS (ILG),   QEVAP (ILG),   QLWAVG(ILG), 
     3     FSGV  (ILG),   FSGS  (ILG),   FSGG  (ILG),   FLGV  (ILG),   
     4     FLGS  (ILG),   FLGG  (ILG),   HFSC  (ILG),   HFSS  (ILG),   
     5     HFSG  (ILG),   HEVC  (ILG),   HEVS  (ILG),   HEVG  (ILG),   
     6     HMFC  (ILG),   HMFN  (ILG),   QFCF  (ILG),   QFCL  (ILG),
     7     EVPPOT(ILG),   ACOND (ILG),   DRAG  (ILG),   ILMO  (ILG),   
     8     UE    (ILG),   HBL   (ILG),   ILMOX (ILG),   UEX   (ILG),
     9     HBLX  (ILG),   FTEMP (ILG),   FTEMPX(ILG),   FVAP  (ILG),
     A     FVAPX (ILG),   RIB   (ILG),   RIBX  (ILG)
C
C     * INPUT ARRAYS.                                                             
C                                                                                 
      REAL THLIQ (ILG,IG),THICE (ILG,IG),TBAR  (ILG,IG),
     1     ZPOND (ILG),   TPOND (ILG)
C                                                                                 
      REAL TA    (ILG),   RHOSNO(ILG),   TSNOW (ILG),   ZSNOW (ILG),   
     1     WSNOW (ILG),   TCAN  (ILG),   FC    (ILG),   FCS   (ILG) 
C
C     * SOIL PROPERTY ARRAYS.                                     
C                                                                                 
      REAL THPOR(ILG,IG), THLMIN(ILG,IG),THLRET(ILG,IG),
     1     THFC  (ILG,IG),HCPS  (ILG,IG),TCS   (ILG,IG)

      REAL DELZW(ILG,IG), ZBOTW(ILG,IG), DELZ(IG)
C                                                                                 
      INTEGER       ISAND (ILG,IG)
C                                                                                 
C     * INTERNAL WORK FIELDS FOR THIS ROUTINE.                                    
C                                                                                 
      REAL FVEG  (ILG),   TCSATU(ILG),   TCSATF(ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL SATRAT,THLSAT,THISAT,TCDRY,TCKAPU,TCKAPF,TCRATU,TCRATF,
     1     TCSOLU,TCSOLF,TCSOIL,TSUM1,TSUM2,ZSUM
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,
     1     TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,
     2     HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     3     TCGLAC,CLHMLT,CLHVAP
C                                                                                 
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C                                                                                 
C----------------------------------------------------------------------           
C     * INITIALIZE 2-D AND 3-D ARRAYS.                                                    
C                                                                                 
      DO 50 J=1,IG                
      !$omp parallel do                                                
      DO 50 I=IL1,IL2                                                             
          THLIQG(I,J)=THLIQ(I,J)                                                  
          THICEG(I,J)=THICE(I,J)                                                  
          THLIQC(I,J)=THLIQ(I,J)                                                  
          THICEC(I,J)=THICE(I,J)                                                  
          TBARCS(I,J)=0.0                                                         
          TBARGS(I,J)=0.0                                                         
          TBARC (I,J)=0.0                                                         
          TBARG (I,J)=0.0
          TCTOPC(I,J)=0.0
          TCBOTC(I,J)=0.0
          TCTOPG(I,J)=0.0
          TCBOTG(I,J)=0.0
   50 CONTINUE                                                                    
C                                                                                 
C     * INITIALIZE 1-D INTERNAL WORK FIELDS AND DIAGNOSTIC ARRAYS.
C                                 
!$omp parallel do                                                
      DO 100 I=IL1,IL2                                                            
          FVEG  (I)=FC(I)+FCS(I)                                                  
          IF(TCAN(I).GT.5.0) THEN
              TCANS (I)=TCAN(I)  
              TCANO (I)=TCAN(I) 
          ELSE
              TCANS (I)=TA(I)  
              TCANO (I)=TA(I) 
          ENDIF
          EVAPC (I)=0.                                                            
          EVAPCG(I)=0.                                                            
          EVAPG (I)=0.                                                            
          EVAPCS(I)=0.                                                            
          EVPCSG(I)=0.                                                            
          EVAPGS(I)=0.                                                            
          GSNOWC(I)=0.                                                            
          GSNOWG(I)=0.                                                            
          GZEROC(I)=0.                                                            
          GZEROG(I)=0.                                                            
          GZROCS(I)=0.
          GZROGS(I)=0.
          QMELTC(I)=0.                                                            
          QMELTG(I)=0.
          QSENSC(I)=0.
          QSENSG(I)=0.
          QEVAPC(I)=0.
          QEVAPG(I)=0.
          TPONDC(I)=0.
          TPONDG(I)=0.
          TPNDCS(I)=0.
          TPNDGS(I)=0.
          TACCS (I)=0.
          QACCS (I)=0.
          TACCO (I)=0.
          QACCO (I)=0.
          ST    (I)=0.
          SU    (I)=0.
          SV    (I)=0.
          SQ    (I)=0.
          CDH   (I)=0.
          CDM   (I)=0.
          QSENS (I)=0.
          QEVAP (I)=0.
          EVAP  (I)=0.
          QLWAVG(I)=0.
          FSGV  (I)=0.
          FSGS  (I)=0.
          FSGG  (I)=0.
          FLGV  (I)=0. 
          FLGS  (I)=0. 
          FLGG  (I)=0.
          HFSC  (I)=0.
          HFSS  (I)=0.
          HFSG  (I)=0.
          HEVC  (I)=0.
          HEVS  (I)=0.
          HEVG  (I)=0.
          HMFC  (I)=0.
          HMFN  (I)=0.
          QFCF  (I)=0.
          QFCL  (I)=0.
          EVPPOT(I)=0.
          ACOND (I)=0.
          DRAG  (I)=0.
          ILMO  (I)=0.
          UE    (I)=0.
          HBL   (I)=0.
          ILMOX (I)=0.
          UEX   (I)=0.
          HBLX  (I)=0.
          ZERO  (I)=0.
          FTEMP (I)=0.
          FVAP  (I)=0.
          RIB   (I)=0.
          FTEMPX(I)=0.
          FVAPX (I)=0.
          RIBX  (I)=0.
          WTABLE(I)=9999.
  100 CONTINUE                                                                    
C                                                                                 
C     * SURFACE EVAPORATION EFFICIENCY FOR BARE SOIL ENERGY BALANCE
C     * CALCULATIONS.                                 
C
!$omp parallel do
      DO 200 I=IL1,IL2    
          IF(THLIQG(I,1).LT.(THLMIN(I,1)+0.001)) THEN    
              IEVAP(I)=0  
              CEVAP(I)=0.0
          ELSEIF(THLIQG(I,1).GT.THFC(I,1)) THEN
              IEVAP(I)=1   
              CEVAP(I)=1.0
          ELSE
              IEVAP(I)=1
              CEVAP(I)=0.25*(1.0-COS(3.14159*THLIQG(I,1)/THFC(I,1)))**2
          ENDIF
  200 CONTINUE  
C                                                                                 
C     * VOLUMETRIC HEAT CAPACITIES OF SOIL LAYERS.
C                                                                                 
      DO 300 J=1,IG       
      !$omp parallel do                                                       
      DO 300 I=IL1,IL2                                                            
          IF(ISAND(I,1).GT.-4)                                     THEN          
              HCPG(I,J)=HCPW*THLIQG(I,J)+HCPICE*THICEG(I,J)+
     1            HCPS(I,J)*(1.0-THPOR(I,J))
              HCPC(I,J)=HCPW*THLIQC(I,J)+HCPICE*THICEC(I,J)+
     1            HCPS(I,J)*(1.0-THPOR(I,J))
          ELSE                                                                    
              HCPC(I,J)=HCPICE                                                    
              HCPG(I,J)=HCPICE                                                    
          ENDIF                                                                   
  300 CONTINUE                                                                    
C                                                                                 
C     * THERMAL PROPERTIES OF SNOW.
C                      
!$omp parallel do                                                           
      DO 400 I=IL1,IL2                                                            
          IF(ZSNOW(I).GT.0.)                                        THEN          
              HCPSCS(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1            (RHOW*ZSNOW(I)) 
              HCPSGS(I)=HCPSCS(I)
C             TCSNOW(I)=2.576E-6*RHOSNO(I)*RHOSNO(I)+0.074                        
              IF(RHOSNO(I).LT.156.0) THEN
                  TCSNOW(I)=0.234E-3*RHOSNO(I)+0.023
              ELSE
                  TCSNOW(I)=3.233E-6*RHOSNO(I)*RHOSNO(I)-1.01E-3*
     1                RHOSNO(I)+0.138
              ENDIF
              IF(FVEG(I).LT.1.)                                 THEN              
                  TSNOGS(I)=TSNOW(I)                                              
                  WSNOGS(I)=WSNOW(I)
                  RHOSGS(I)=RHOSNO(I)
              ELSE                                                                
                  TSNOGS(I)=0.0                                                   
                  WSNOGS(I)=0.0
                  RHOSGS(I)=0.0
              ENDIF                                                               
              IF(FVEG(I).GT.0.)                                 THEN              
                  TSNOCS(I)=TSNOW(I)                                              
                  WSNOCS(I)=WSNOW(I)
                  RHOSCS(I)=RHOSNO(I)
              ELSE                                                                
                  TSNOCS(I)=0.0                                                   
                  WSNOCS(I)=0.0
                  RHOSCS(I)=0.0
              ENDIF                                                               
          ELSE                                                                    
              TSNOGS(I)=0.0                                                       
              WSNOGS(I)=0.0
              RHOSGS(I)=0.0
              TSNOCS(I)=0.0                                                       
              WSNOCS(I)=0.0
              RHOSCS(I)=0.0
              TCSNOW(I)=0.0
          ENDIF                                                                   
  400 CONTINUE                                                                    
C                                                                                 
C     * THERMAL CONDUCTIVITIES OF SOIL LAYERS AND DEPTH OF WATER
C     * TABLE IN ORGANIC SOILS.                                         
C                                                                                 
      DO 500 J=IG,1,-1
      !$omp parallel do
      DO 500 I=IL1,IL2                                                            
          IF    (ISAND(I,1).EQ.-4)                              THEN          
              TCTOPG(I,J)=TCGLAC                                                     
              TCBOTG(I,J)=TCGLAC
          ELSEIF(ISAND(I,J).EQ.-3)                              THEN
              TCTOPC(I,J)=TCSAND
              TCTOPG(I,J)=TCSAND
              TCBOTC(I,J)=TCSAND
              TCBOTG(I,J)=TCSAND
          ELSEIF(ISAND(I,J).EQ.-2)                          THEN 
              IF ((THLIQG(I,J)+THICEG(I,J)).GT.(THLRET(I,J)+0.0001)) 
     1                                                 THEN
                  WTABLE(I)=ZBOTW(I,J)-DELZW(I,J)*MIN(1.0,
     1                      (THLIQG(I,J)+THICEG(I,J)-THLRET(I,J))/
     2                      (THPOR(I,J)-THLRET(I,J)))
              ENDIF
              IF (THLIQG(I,J).GT.(THLRET(I,J)+0.0001)) THEN
                  SATRAT=MIN((THLRET(I,J)+THICEG(I,J))/
     1                   THPOR(I,J), 1.0)              
                  THLSAT=THLIQG(I,J)/(THLIQG(I,J)+THICEG(I,J))          
                  THISAT=THICEG(I,J)/(THLIQG(I,J)+THICEG(I,J))          
                  TCDRY=0.30*EXP(-2.0*THPOR(I,J))
                  TCSATU(I)=TCW*THPOR(I,J)+TCS(I,J)*(1.0-THPOR(I,J))
                  TCSATF(I)=TCICE*THPOR(I,J)+TCS(I,J)*(1.0-THPOR(I,J))
                  TCRATU=0.6*SATRAT/(1.0-0.4*SATRAT)
                  TCRATF=0.25*SATRAT/(1.0-0.75*SATRAT)
                  TCSOLU=(TCSATU(I)-TCDRY)*TCRATU+TCDRY                              
                  TCSOLF=(TCSATF(I)-TCDRY)*TCRATF+TCDRY
                  TCSOIL=TCSOLU*THLSAT+TCSOLF*THISAT
                  IF(DELZW(I,J).GT.0.0) THEN
                      TCTOPC(I,J)=TCSOIL
                      TCTOPG(I,J)=TCSOIL
                  ELSE
                      TCTOPC(I,J)=TCSAND
                      TCTOPG(I,J)=TCSAND
                  ENDIF
                  IF(DELZW(I,J).LT.DELZ(J)) THEN
                      TCBOTC(I,J)=TCSAND
                      TCBOTG(I,J)=TCSAND
                  ELSE
                      TCBOTC(I,J)=TCSATU(I)*THLSAT+TCSATF(I)*THISAT
                      TCBOTG(I,J)=TCSATU(I)*THLSAT+TCSATF(I)*THISAT
                  ENDIF
                  IF(J.EQ.1) THEN
                      TCTOPC(I,J)=TCTOPC(I,J)+(TCW-TCTOPC(I,J))*
     1                            MIN(ZPOND(I),1.0E-2)*100.0
                      TCTOPG(I,J)=TCTOPC(I,J)
                  ENDIF
              ELSE
                  SATRAT=MIN((THLIQG(I,J)+THICEG(I,J))/
     1                   THPOR(I,J), 1.0)              
                  THLSAT=THLIQG(I,J)/(THLIQG(I,J)+THICEG(I,J))          
                  THISAT=THICEG(I,J)/(THLIQG(I,J)+THICEG(I,J))          
                  TCDRY=0.30*EXP(-2.0*THPOR(I,J))
                  TCSATU(I)=TCW*THPOR(I,J)+TCS(I,J)*(1.0-THPOR(I,J))
                  TCSATF(I)=TCICE*THPOR(I,J)+TCS(I,J)*(1.0-THPOR(I,J))
                  TCRATU=0.6*SATRAT/(1.0-0.4*SATRAT)
                  TCRATF=0.25*SATRAT/(1.0-0.75*SATRAT)
                  TCSOLU=(TCSATU(I)-TCDRY)*TCRATU+TCDRY                              
                  TCSOLF=(TCSATF(I)-TCDRY)*TCRATF+TCDRY
                  TCSOIL=TCSOLU*THLSAT+TCSOLF*THISAT
                  IF(DELZW(I,J).GT.0.0) THEN
                      TCTOPC(I,J)=TCSOIL
                      TCTOPG(I,J)=TCSOIL
                  ELSE
                      TCTOPC(I,J)=TCSAND
                      TCTOPG(I,J)=TCSAND
                  ENDIF
                  IF(DELZW(I,J).LT.DELZ(J)) THEN
                      TCBOTC(I,J)=TCSAND
                      TCBOTG(I,J)=TCSAND
                  ELSE
                      TCBOTC(I,J)=TCSOIL
                      TCBOTG(I,J)=TCSOIL
                  ENDIF
                  IF(J.EQ.1) THEN
                      TCTOPC(I,J)=TCTOPC(I,J)+(TCW-TCTOPC(I,J))*
     1                            MIN(ZPOND(I),1.0E-2)*100.0
                      TCTOPG(I,J)=TCTOPC(I,J)
                  ENDIF
              ENDIF    
          ELSE
              SATRAT=MIN((THLIQG(I,J)+THICEG(I,J))/
     1               THPOR(I,J), 1.0)              
              THLSAT=THLIQG(I,J)/(THLIQG(I,J)+THICEG(I,J))          
              THISAT=THICEG(I,J)/(THLIQG(I,J)+THICEG(I,J))          
              TCDRY=0.75*EXP(-2.76*THPOR(I,J))
              TCSATU(I)=TCW*THPOR(I,J)+TCS(I,J)*(1.0-THPOR(I,J))
              TCSATF(I)=TCICE*THPOR(I,J)+TCS(I,J)*(1.0-THPOR(I,J))
              TCKAPU=(4.0*REAL(ISAND(I,J))+1.9*REAL(100-ISAND(I,J)))/
     1               100.0
              TCKAPF=(1.2*REAL(ISAND(I,J))+0.85*REAL(100-ISAND(I,J)))/
     1               100.0
              TCRATU=TCKAPU*SATRAT/(1.0+(TCKAPU-1.0)*SATRAT)
              TCRATF=TCKAPF*SATRAT/(1.0+(TCKAPF-1.0)*SATRAT)
              TCSOLU=(TCSATU(I)-TCDRY)*TCRATU+TCDRY                              
              TCSOLF=(TCSATF(I)-TCDRY)*TCRATF+TCDRY
              TCSOIL=TCSOLU*THLSAT+TCSOLF*THISAT
              IF(DELZW(I,J).GT.0.0) THEN
                  TCTOPC(I,J)=TCSOIL
                  TCTOPG(I,J)=TCSOIL
C                  IF(J.EQ.1) TCTOPC(I,J)=TCTOPC(I,J)*0.1
              ELSE
                  TCTOPC(I,J)=TCSAND
                  TCTOPG(I,J)=TCSAND
              ENDIF
              IF(DELZW(I,J).LT.DELZ(J)) THEN
                  TCBOTC(I,J)=TCSAND
                  TCBOTG(I,J)=TCSAND
              ELSE
                  TCBOTC(I,J)=TCSOIL
                  TCBOTG(I,J)=TCSOIL
              ENDIF
              IF(J.EQ.1) THEN
                  TCTOPC(I,J)=TCTOPC(I,J)+(TCW-TCTOPC(I,J))*
     1                        MIN(ZPOND(I),1.0E-2)*100.0
                  TCTOPG(I,J)=TCTOPC(I,J)
              ENDIF
          ENDIF                                                                   
  500 CONTINUE                                                                    
C                                                                           
C     * ADD PONDED WATER TEMPERATURE TO FIRST SOIL LAYER FOR USE
C     * IN GROUND HEAT FLUX CALCULATIONS.
C
      !$omp parallel do
      DO 600 I=IL1,IL2
          IF(ZPOND(I).GT.0.)                          THEN 
              TBAR1P(I)=(TPOND(I)*HCPW*ZPOND(I) + 
     1                  TBAR(I,1)*(HCPG(I,1)*DELZW(I,1)+
     2                  HCPSND*(DELZ(1)-DELZW(I,1))))/
     3                  (HCPW*ZPOND(I)+HCPG(I,1)*DELZW(I,1)+
     4                  HCPSND*(DELZ(1)-DELZW(I,1)))
          ELSE
              TBAR1P(I)=TBAR(I,1)
          ENDIF
  600 CONTINUE
C
      RETURN                                                                      
      END 
