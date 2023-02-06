      SUBROUTINE APREP(FC,FG,FCS,FGS,PAICAN,PAICNS,FSVF,FSVFS, 
     1            FRAINC,FSNOWC,FRAICS,FSNOCS,RAICAN,RAICNS,SNOCAN,
     2            SNOCNS,DISP,DISPS,ZOMLNC,ZOMLCS,ZOELNC,ZOELCS,
     3            ZOMLNG,ZOMLNS,ZOELNG,ZOELNS,CHCAP,CHCAPS,CMASSC,
     4            CMASCS,CWLCAP,CWFCAP,CWLCPS,CWFCPS,RBCOEF,FROOT,
     5            ZPLIMC,ZPLIMG,ZPLMCS,ZPLMGS,HTCC,HTCS,HTC,WTRC,
     6            WTRS,WTRG,CMAI,PAI,PAIS,AIL,FCAN,FCANS,PSIGND,
     7            FCANMX,ZOLN,PAIMAX,PAIMIN,CWGTMX,ZRTMAX,
     8            PAIDAT,HGTDAT,THLIQ,THICE,TBAR,RCAN,SNCAN,
     9            TCAN,GROWTH,ZSNOW,TSNOW,FSNOW,RHOSNO,SNO,Z0ORO,
     A            ZBLEND,ZPLMG0,ZPLMS0,
     B            TA,RHOAIR,RADJ,DLON,RHOSNI,DELZ,DELZW,ZBOTW,
     C            THPOR,THLMIN,PSISAT,BI,PSIWLT,HCPS,ISAND,
     D            ILG,IL1,IL2,JL,IC,ICP1,IG,IDAY,IDISP,IZREF,IWF,
     E            IPAI,IHGT,RMAT,H,HS,CWCPAV,GROWA,GROWN,GROWB,
     F            RRESID,SRESID,FRTOT,
     G            FCANCMX,ICTEM,ICTEMMOD,RMATC,
     H            AILC,PAIC,AILCG,L2MAX,NOL2PFTS,
     I            AILCGS,FCANCS,FCANC)
C
C     * NOV 15/11 - M.LAZARE.   CTEM ADDED. CALCULATIONS ARE DIFFERENT
C     *                         IN SEVERAL AREAS, UNDER CONTROL OF
C     *                         "ICTEMMOD" SWITCH (ICTEMMOD=0 REVERTS
C     *                         BACK TO APREP4 FORMULATION). THIS 
C     *                         INCLUDES NEW INPUT "PAIC".
C     * OCT 07/11 - V.FORTIN/D.VERSEGHY. MAKE THE LIMITING PONDING DEPTH 
C     *                         CALCULATION OVER ORGANIC SOILS THE SAME 
C     *                         AS OVER MINERAL SOILS (LOOP 175).
C     * DEC 23/09 - D.VERSEGHY. IN LIMITING PONDING DEPTH CALCULATIONS,
C     *                         IDENTIFY PEATLANDS WHERE ISAND(I,2)=-2
C     * JAN 06/09 - D.VERSEGHY. REINTRODUCE CHECKS ON FRACTIONAL AREAS.
C     * MAR 25/08 - D.VERSEGHY. DISTINGUISH BETWEEN LEAF AREA INDEX
C     *                         AND PLANT AREA INDEX.
C     * JAN 17/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS; REMOVE
C     *                         SUPERFLUOUS CHECKS ON FRACTIONAL AREAS.
C     * NOV 30/06 - E.CHAN/M.LAZARE/D.VERSEGHY. CHANGE RADJ TO REAL;
C     *                         ENSURE CONSISTENCY IN CALCULATION
C     *                         OF FRACTIONAL CANOPY AREAS.
C     * SEP 13/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 100,
C     *                         450 LOOPS.
C     * MAR 14/05 - D.VERSEGHY. RENAME SCAN TO SNCAN (RESERVED NAME
C     *                         IN F90); TREAT SOIL FROZEN WATER AS ICE
C     *                         VOLUME RATHER THAN AS EQUIVALENT WATER.
C     * MAR 03/05 - Y.DELAGE.   ADD CONTRIBUTION OF SUBGRID-SCALE
C     *                         OROGRAPHY TO ROUGHNESS LENGTH.
C     * JAN 12/05 - P.BARTLETT/D.VERSEGHY. DETERMINE SEPARATE CANOPY
C     *                         WATER INTERCEPTION CAPACITIES FOR
C     *                         RAIN AND SNOW, AND NEW FRACTIONAL
C     *                         CANOPY COVERAGE OF INTERCEPTED RAIN
C     *                         AND SNOW; DEFINE NEW PARAMETER RBCOEF
C     *                         FOR RBINV CALCULATION IN TSOLVC.
C     * NOV 03/04 - D.VERSEGHY. CHANGE RADJ AND DLON TO GATHERED 
C     *                         VARIABLES AND REMOVE ILAND ARRAY;
C     *                         ADD "IMPLICIT NONE" COMMAND.
C     * JUL 05/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS
C     *                         AGAINST ROUNDOFF ERRORS.
C     * JUL 02/03 - D.VERSEGHY. RATIONALIZE ASSIGNMENT OF RESIDUAL
C     *                         CANOPY MOISTURE TO SOIL LAYERS.
C     * DEC 05/02 - Y.DELAGE/D.VERSEGHY. ADD PARTS OF CANOPY AIR MASS TO 
C     *                         CANOPY MASS ONLY IF IDISP=0 OR IZREF=2.
C     *                         ALSO, REPLACE LOGARITHMIC AVERAGING OF
C     *                         ROUGHNESS HEIGHTS WITH BLENDING HEIGHT
C     *                         AVERAGING.
C     * JUL 31/02 - D.VERSEGHY. MOVE CALCULATION OF PSIGND AND FULL 
C     *                         CALCULATION OF FROOT INTO THIS ROUTINE
C     *                         FROM TPREP; REMOVE CALCULATION OF RCMIN.
C     *                         SHORTENED CLASS3 COMMON BLOCK.
C     * JUL 23/02 - D.VERSEGHY. MOVE ADDITION OF AIR TO CANOPY MASS
C     *                         INTO THIS ROUTINE; SHORTENED CLASS4
C     *                         COMMON BLOCK.
C     * MAR 18/02 - D.VERSEGHY. MOVE CALCULATION OF SOIL PROPERTIES INTO
C     *                         ROUTINE "CLASSB"; ALLOW FOR ASSIGNMENT 
C     *                         OF SPECIFIED TIME-VARYING VEGETATION
C     *                         HEIGHT AND LEAF AREA INDEX.
C     * SEP 19/00 - D.VERSEGHY. ADD CALCULATION OF VEGETATION-DEPENDENT
C     *                         COEFFICIENTS FOR DETERMINATION OF STOMATAL 
C     *                         RESISTANCE.
C     * APR 12/00 - D.VERSEGHY. RCMIN NOW VARIES WITH VEGETATION TYPE:
C     *                         PASS IN BACKGROUND ARRAY "RCMINX".
C     * DEC 16/99 - A.WU/D.VERSEGHY. ADD CALCULATION OF NEW LEAF DIMENSION 
C     *                              PARAMETER FOR REVISED CANOPY TURBULENT
C     *                              TRANSFER FORMULATION.
C     * NOV 16/98 - M.LAZARE.   "DLON" NOW PASSED IN AND USED DIRECTLY
C     *                         (INSTEAD OF INFERRING FROM "LONSL" AND 
C     *                         "ILSL" WHICH USED TO BE PASSED) TO CALCULATE
C     *                         GROWTH INDEX. THIS IS DONE TO MAKE THE PHYSICS
C     *                         PLUG COMPATIBLE FOR USE WITH THE RCM WHICH 
C     *                         DOES NOT HAVE EQUALLY-SPACED LONGITUDES.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * OCT 11/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUG FIX: TO AVOID ROUND-OFF ERRORS,
C     *                         SET CANOPY COVER EQUAL TO 1 IF THE
C     *                         CALCULATED SUM OF FC AND FCS IS
C     *                         VERY CLOSE TO 1.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     *                         ALSO CORRECT BUG IN CALCULATION OF
C     *                         DEGLON, AND USE IDISP TO DETERMINE
C     *                         METHOD OF CALCULATING DISP AND DISPS.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 16/95 - D.VERSEGHY. THREE NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * NOV 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         RATIONALIZE CALCULATION OF RCMIN. 
C     * NOV 12/94 - D.VERSEGHY. FIX BUGS IN SENESCING LIMB OF CROP
C     *                         GROWTH INDEX AND IN CANOPY MASS
C     *                         CALCULATION.
C     * MAY 06/93 - M.LAZARE/D.VERSEGHY. CLASS - VERSION 2.1.
C     *                                  USE NEW "CANEXT" CANOPY 
C     *                                  EXTINCTION ARRAY TO DEFINE
C     *                                  SKY-VIEW FACTORS. ALSO, CORRECT
C     *                                  MINOR BUG WHERE HAD "IF(IN.LE.9)..."
C     *                                  INSTEAD OF "IF(IN.GT.9)...".  
C     * DEC 12/92 - M.LAZARE.   MODIFIED FOR MULTIPLE LATITUDES.
C     * OCT 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATION OF LAND SURFACE CANOPY 
C     *                         PARAMETERS.
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IC,ICP1,IG,IDAY,IDISP,IZREF,
     1        IPAI,IHGT,I,J,K,IN,NL
      INTEGER IWF(ILG)
C                                                                                 
C     * OUTPUT ARRAYS USED ELSEWHERE IN CLASS.                                    
C                                                                                 
      REAL FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),          
     1     PAICAN(ILG),   PAICNS(ILG),   FSVF  (ILG),   FSVFS (ILG),   
     2     FRAINC(ILG),   FSNOWC(ILG),   FRAICS(ILG),   FSNOCS(ILG),
     3     RAICAN(ILG),   RAICNS(ILG),   SNOCAN(ILG),   SNOCNS(ILG),   
     4     DISP  (ILG),   DISPS (ILG),  
     5     ZOMLNC(ILG),   ZOMLCS(ILG),   ZOELNC(ILG),   ZOELCS(ILG),          
     6     ZOMLNG(ILG),   ZOMLNS(ILG),   ZOELNG(ILG),   ZOELNS(ILG),          
     7     RBCOEF(ILG),   CHCAP (ILG),   CHCAPS(ILG),   
     8     CMASSC(ILG),   CMASCS(ILG),   CWLCAP(ILG),   CWFCAP(ILG),   
     9     CWLCPS(ILG),   CWFCPS(ILG),   ZPLIMC(ILG),   ZPLIMG(ILG),   
     A     ZPLMCS(ILG),   ZPLMGS(ILG),   HTCC  (ILG),   HTCS  (ILG),   
     B     WTRC  (ILG),   WTRS  (ILG),   WTRG  (ILG),   CMAI  (ILG)  
C                                                                                 
      REAL FROOT (ILG,IG),  HTC   (ILG,IG)
C                                                                                 
C     * OUTPUT ARRAYS ONLY USED ELSEWHERE IN CLASSA.                              
C                                                                                 
      REAL PAI   (ILG,IC),  PAIS  (ILG,IC),  AIL   (ILG,IC),
     1     FCAN  (ILG,IC),  FCANS (ILG,IC),  PSIGND(ILG)
C                                                                                 
C     * INPUT ARRAYS.                                      
C                                                                                 
      REAL FCANMX(ILG,ICP1),                 ZOLN  (ILG,ICP1),                    
     1     PAIMAX(ILG,IC),  PAIMIN(ILG,IC),  CWGTMX(ILG,IC),                      
     2     ZRTMAX(ILG,IC),  PAIDAT(ILG,IC),  HGTDAT(ILG,IC),
     3     THLIQ (ILG,IG),  THICE (ILG,IG),  TBAR  (ILG,IG) 
C                                                                                 
      REAL RCAN  (ILG),     SNCAN (ILG),     TCAN  (ILG),     
     1     GROWTH(ILG),     ZSNOW (ILG),     TSNOW (ILG),          
     2     FSNOW (ILG),     RHOSNO(ILG),     SNO   (ILG),     
     3     TA    (ILG),     RHOAIR(ILG),     DLON  (ILG),
     4     Z0ORO (ILG),     ZBLEND(ILG),     RHOSNI(ILG),
     5     ZPLMG0(ILG),     ZPLMS0(ILG),     RADJ  (ILG)
C
C     * SOIL PROPERTY ARRAYS.                                     
C                                                                                 
      REAL DELZW(ILG,IG),   ZBOTW(ILG,IG),   THPOR(ILG,IG),   
     1     THLMIN(ILG,IG),  PSISAT(ILG,IG),  BI   (ILG,IG),
     2     PSIWLT(ILG,IG),  HCPS (ILG,IG)
C                                                                                 
      INTEGER               ISAND (ILG,IG)
C                                               
C     * OTHER DATA ARRAYS WITH NON-VARYING VALUES.
C                                                                                 
      REAL GROWYR(18,4,2),  DELZ  (IG),      ZORAT (4),
     1     CANEXT(4),       XLEAF (4)
C                                                                                 
C     * WORK ARRAYS NOT USED ELSEWHERE IN CLASSA.                          
C                                                                                 
      REAL RMAT (ILG,IC,IG),H     (ILG,IC),  HS    (ILG,IC),                      
     1     CWCPAV(ILG),     GROWA (ILG),     GROWN (ILG),     
     2     GROWB (ILG),     RRESID(ILG),     SRESID(ILG),
     3     FRTOT (ILG) 
C
C     * TEMPORARY VARIABLES.
C
      REAL DAY,GROWG,FSUM,SNOI,ZSNADD,THSUM,THICEI,THLIQI,ZROOT,
     1     ZROOTG,FCOEFF,PSII,LZ0ORO,THR_LAI
C
C     * CTEM-RELATED FIELDS.
C
      REAL  AILC (ILG,IC),       PAIC   (ILG,IC),
     1      AILCG(ILG,ICTEM),    AILCGS (ILG,ICTEM),
     2      RMATC(ILG,IC,IG),    FCANCMX(ILG,ICTEM),  
     3      FCANC(ILG,ICTEM),    FCANCS (ILG,ICTEM)
C
C     * INTERNAL WORK FIELD.
C
      REAL  SFCANCMX(ILG,IC)
C
      INTEGER ICTEM, M, N, K1, K2, L2MAX, NOL2PFTS(IC), ICTEMMOD
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,PI,ZOLNG,ZOLNS,ZOLNI,
     3     ZORATG     
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS6/ PI,GROWYR,ZOLNG,ZOLNS,ZOLNI,ZORAT,ZORATG     
      COMMON /CLASS7/ CANEXT,XLEAF
C-----------------------------------------------------------------------          
      IF(IC.NE.4)                               CALL XIT('APREP',-2)
C
C     * INITIALIZE DIAGNOSTIC AND OTHER ARRAYS.
C
      DO 100 I=IL1,IL2
          HTCC(I) =0.0
          HTCS(I) =0.0
          DO 50 J=1,IG
              HTC(I,J)=0.0
   50     CONTINUE
          WTRC(I) =0.0
          WTRS(I) =0.0
          WTRG(I) =0.0
          FRTOT(I)=0.0
          DISP  (I)=0.                                                            
          ZOMLNC(I)=0.                                                            
          ZOELNC(I)=1.                                                            
          DISPS (I)=0.                                                            
          ZOMLCS(I)=0.                                                            
          ZOELCS(I)=1.                                                            
          ZOMLNG(I)=0.                                                            
          ZOELNG(I)=0.                                                            
          ZOMLNS(I)=0.                                                            
          ZOELNS(I)=0.                                                            
          CMASSC(I)=0.                                                            
          CMASCS(I)=0.                                                            
          PSIGND(I)=1.0E+5
  100 CONTINUE
C 
C     * DETERMINE GROWTH INDEX FOR CROPS (VEGETATION TYPE 3).
C     * MUST USE UN-GATHERED LONGITUDES TO COMPUTE ACTUAL LONGITUDE/
C     * LATITUDE VALUES.  
C                                                                                 
      DAY=FLOAT(IDAY)                                                             
C
C     * FOR CTEM, CROP GROWTH IS BUILT IN, SO GROWA=1.
C
      IF (ICTEMMOD.EQ.0) THEN
        DO 120 I=IL1,IL2
          IN = INT( (RADJ(I)+PI/2.0)*18.0/PI ) + 1
          IF(DLON(I).GT.190. .AND. DLON(I).LT.330.)            THEN           
              NL=2                                                            
          ELSE                                                                
              NL=1                                                            
          ENDIF                                                               
          IF(GROWYR(IN,1,NL).LT.0.1)                           THEN           
              GROWA(I)=1.0                                                    
          ELSE                                                                
              IF(IN.GT.9)                                 THEN
                IF(DAY.GE.GROWYR(IN,2,NL).AND.DAY.LT.GROWYR(IN,3,NL))           
     1              GROWA(I)=1.0                                                
                IF(DAY.GE.GROWYR(IN,4,NL).OR.DAY.LT.GROWYR(IN,1,NL))            
     1              GROWA(I)=0.0                                
              ELSE
                IF(DAY.GE.GROWYR(IN,2,NL).OR.DAY.LT.GROWYR(IN,3,NL))           
     1              GROWA(I)=1.0                                                
                IF(DAY.GE.GROWYR(IN,4,NL).AND.DAY.LT.GROWYR(IN,1,NL))            
     1              GROWA(I)=0.0                                
              ENDIF                
              IF(DAY.GE.GROWYR(IN,1,NL).AND.DAY.LT.GROWYR(IN,2,NL))           
     1            GROWA(I)=(DAY-GROWYR(IN,1,NL))/(GROWYR(IN,2,NL)-            
     2                     GROWYR(IN,1,NL))                                   
              IF(DAY.GE.GROWYR(IN,3,NL).AND.DAY.LT.GROWYR(IN,4,NL))           
     1            GROWA(I)=(GROWYR(IN,4,NL)-DAY)/(GROWYR(IN,4,NL)-            
     2                     GROWYR(IN,3,NL))                                   
              GROWA(I)=MAX(0.0,MIN(GROWA(I),1.0))
              IF(GROWA(I).LT.1.0E-5) GROWA(I)=0.0
          ENDIF                                                               
  120   CONTINUE                                                                
      ELSE
        DO I=IL1,IL2
          GROWA(I)=1.
        ENDDO
      ENDIF
C                                                                                 
C     * DETERMINE GROWTH INDICES FOR NEEDLELEAF TREES, BROADLEAF
C     * TREES AND GRASS (VEGETATION TYPES 1, 2 AND 4); CALCULATE
C     * VEGETATION HEIGHT, CORRECTED FOR GROWTH STAGE FOR CROPS
C     * AND FOR SNOW COVER FOR CROPS AND GRASS; CALCULATE CURRENT
C     * LEAF AREA INDEX FOR FOUR VEGETATION TYPES.
C
      DO 150 I=IL1,IL2                                                            
          GROWN(I)=ABS(GROWTH(I))                                                 
          IF(GROWTH(I).GT.0.0)                      THEN                          
              GROWB(I)=MIN(1.0,GROWTH(I)*2.0)                                   
          ELSE                                                                    
              GROWB(I)=MAX(0.0,(ABS(GROWTH(I))*2.0-1.0))                        
          ENDIF                                                                   
          GROWG=1.0                                                               
C                                                                                 
          IF(IHGT.EQ.0) THEN
              H(I,1)=10.0*EXP(ZOLN(I,1))                                              
              H(I,2)=10.0*EXP(ZOLN(I,2))                                              
              H(I,3)=10.0*EXP(ZOLN(I,3))*GROWA(I)                                     
              H(I,4)=10.0*EXP(ZOLN(I,4))                                              
          ELSE
              H(I,1)=HGTDAT(I,1)
              H(I,2)=HGTDAT(I,2)
              H(I,3)=HGTDAT(I,3)
              H(I,4)=HGTDAT(I,4)
          ENDIF
          HS(I,1)=H(I,1)                                                          
          HS(I,2)=H(I,2)                                                          
          HS(I,3)=MAX(H(I,3)-ZSNOW(I),1.0E-3)                                       
          HS(I,4)=MAX(H(I,4)-ZSNOW(I),1.0E-3)                                       
C                 
          IF (ICTEMMOD.EQ.0) THEN                                                                
            IF(IPAI.EQ.0) THEN
              PAI(I,1)=PAIMIN(I,1)+GROWN(I)*(PAIMAX(I,1)-PAIMIN(I,1))                 
              PAI(I,2)=PAIMIN(I,2)+GROWB(I)*(PAIMAX(I,2)-PAIMIN(I,2))                 
              PAI(I,3)=PAIMIN(I,3)+GROWA(I)*(PAIMAX(I,3)-PAIMIN(I,3))                 
              PAI(I,4)=PAIMIN(I,4)+GROWG   *(PAIMAX(I,4)-PAIMIN(I,4))                 
            ELSE
              PAI(I,1)=PAIDAT(I,1)
              PAI(I,2)=PAIDAT(I,2)
              PAI(I,3)=PAIDAT(I,3)
              PAI(I,4)=PAIDAT(I,4)
            ENDIF
            AIL(I,1)=PAI(I,1)*0.90
            AIL(I,2)=MAX((PAI(I,2)-PAIMIN(I,2)),0.0)
            AIL(I,3)=PAI(I,3)
            AIL(I,4)=PAI(I,4)
          ELSE
            AIL(I,1)=AILC(I,1)
            AIL(I,2)=AILC(I,2)
            AIL(I,3)=AILC(I,3)
            AIL(I,4)=AILC(I,4)
            PAI(I,1)=PAIC(I,1)
            PAI(I,2)=PAIC(I,2)
            PAI(I,3)=PAIC(I,3)
            PAI(I,4)=PAIC(I,4)
          ENDIF 
C
          PAIS(I,1)=PAI(I,1)                                                      
          PAIS(I,2)=PAI(I,2)                                                      
          IF(H(I,3).GT.0.0) THEN                                                  
              PAIS(I,3)=PAI(I,3)*HS(I,3)/H(I,3)                                   
          ELSE                                                                    
              PAIS(I,3)=0.0                                                       
          ENDIF                                                                   
          IF(H(I,4).GT.0.0) THEN                                                  
              PAIS(I,4)=PAI(I,4)*HS(I,4)/H(I,4)                                   
          ELSE                                                                    
              PAIS(I,4)=0.0                                                       
          ENDIF                                                                   
  150 CONTINUE                                                                    
C
C     * ADJUST FRACTIONAL COVERAGE OF GRID CELL FOR CROPS AND
C     * GRASS IF LAI FALLS BELOW A SET THRESHOLD VALUE DUE TO 
C     * GROWTH STAGE OR SNOW COVER; RESET LAI TO THE THRESHOLD
C     * VALUE; CALCULATE RESULTANT GRID CELL COVERAGE BY CANOPY, 
C     * BARE GROUND, CANOPY OVER SNOW AND SNOW OVER BARE GROUND.
C     * 
C     * ALSO CALCULATE SURFACE DETENTION CAPACITY FOR FOUR
C     * GRID CELL SUBAREAS BASED ON VALUES SUPPLIED BY 
C     * U. OF WATERLOO:
C     *        IMPERMEABLE SURFACES: 0.001 M.
C     *        BARE SOIL:            0.002 M.
C     *        LOW VEGETATION:       0.003 M.
C     *        FOREST:               0.01  M.
C                                                                                 
C     * LAI THRESHOLD VALUE FOR CTEM IS SET TO 0.05; STANDARD
C     * CLASS VALUE IS SET TO 1.0.  
C
      IF (ICTEMMOD.EQ.1) THEN
         THR_LAI=0.05
      ELSE
         THR_LAI=1.0
      ENDIF
C
      DO 175 I=IL1,IL2                                                            
          FCAN(I,1)=FCANMX(I,1)*(1.0-FSNOW(I))                                    
          FCAN(I,2)=FCANMX(I,2)*(1.0-FSNOW(I))                                    
          IF(FCAN(I,1).LT.1.0E-5) FCAN(I,1)=0.0
          IF(FCAN(I,2).LT.1.0E-5) FCAN(I,2)=0.0
          IF(PAI(I,3).LT.THR_LAI) THEN                                                
              FCAN(I,3)=FCANMX(I,3)*(1.0-FSNOW(I))*PAI(I,3)                       
              PAI (I,3)=THR_LAI
          ELSE                                                                    
              FCAN(I,3)=FCANMX(I,3)*(1.0-FSNOW(I))                                
          ENDIF                                                                   
          IF(PAI(I,4).LT.THR_LAI) THEN                                                
              FCAN(I,4)=FCANMX(I,4)*(1.0-FSNOW(I))*PAI(I,4)                       
              PAI (I,4)=THR_LAI                                                       
          ELSE                                                                    
              FCAN(I,4)=FCANMX(I,4)*(1.0-FSNOW(I))                                
          ENDIF                                                                   
          IF(FCAN(I,3).LT.1.0E-5) FCAN(I,3)=0.0
          IF(FCAN(I,4).LT.1.0E-5) FCAN(I,4)=0.0
C                                                                                 
          FCANS(I,1)=FCANMX(I,1)*FSNOW(I)                                         
          FCANS(I,2)=FCANMX(I,2)*FSNOW(I)                                         
          IF(FCANS(I,1).LT.1.0E-5) FCANS(I,1)=0.0
          IF(FCANS(I,2).LT.1.0E-5) FCANS(I,2)=0.0
          IF(PAIS(I,3).LT.THR_LAI) THEN                                               
              FCANS(I,3)=FCANMX(I,3)*FSNOW(I)*PAIS(I,3)                           
              PAIS (I,3)=THR_LAI                                                      
          ELSE                                                                    
              FCANS(I,3)=FCANMX(I,3)*FSNOW(I)                                     
          ENDIF                                                                   
          IF(PAIS(I,4).LT.THR_LAI) THEN                                               
              FCANS(I,4)=FCANMX(I,4)*FSNOW(I)*PAIS(I,4)                           
              PAIS (I,4)=THR_LAI                                                      
          ELSE                                                                    
              FCANS(I,4)=FCANMX(I,4)*FSNOW(I)                                     
          ENDIF                                                                   
          IF(FCANS(I,3).LT.1.0E-5) FCANS(I,3)=0.0
          IF(FCANS(I,4).LT.1.0E-5) FCANS(I,4)=0.0
C                                                                                 
          FC (I)=FCAN(I,1)+FCAN(I,2)+FCAN(I,3)+FCAN(I,4)                
          FG (I)=1.0-FSNOW(I)-FC(I)                                     
          FCS(I)=FCANS(I,1)+FCANS(I,2)+FCANS(I,3)+FCANS(I,4)            
          FGS(I)=FSNOW(I)-FCS(I)                                        
          IF(ABS(1.0-FCS(I)-FC(I)).LT.1.0E-5) THEN
              IF(FCS(I).LT.1.0E-5) THEN
                FSNOW (I)=0.0 
              ELSE IF (FC(I).LT.1.0E-5) THEN
                FSNOW(I)= 1.0  
              ENDIF
              IF(FCS(I).GT.0.) THEN
                FCANS(I,1)=FCANS(I,1)*FSNOW(I)/FCS(I)
                FCANS(I,2)=FCANS(I,2)*FSNOW(I)/FCS(I)
                FCANS(I,3)=FCANS(I,3)*FSNOW(I)/FCS(I)
                FCANS(I,4)=FCANS(I,4)*FSNOW(I)/FCS(I)
              ENDIF
              IF(FC(I).GT.0.) THEN
                FCAN(I,1)=FCAN(I,1)*(1.0-FSNOW(I))/FC(I)
                FCAN(I,2)=FCAN(I,2)*(1.0-FSNOW(I))/FC(I)
                FCAN(I,3)=FCAN(I,3)*(1.0-FSNOW(I))/FC(I)
                FCAN(I,4)=FCAN(I,4)*(1.0-FSNOW(I))/FC(I)
              ENDIF
              FCS(I)=MIN(FSNOW(I),1.0)
              FC(I)=1.0-FCS(I)
              FGS(I)=0.0
              FG(I)=0.0
          ENDIF
          FC (I)=MAX(FC (I),0.0)
          FG (I)=MAX(FG (I),0.0)
          FCS(I)=MAX(FCS(I),0.0)
          FGS(I)=MAX(FGS(I),0.0)
          FSUM=(FCS(I)+FGS(I)+FC(I)+FG(I))
          FC (I)=FC (I)/FSUM
          FG (I)=FG (I)/FSUM
          FCS(I)=FCS(I)/FSUM
          FGS(I)=FGS(I)/FSUM
          IF(ABS(1.0-FCS(I)-FGS(I)-FC(I)-FG(I)).GT.1.0E-5) 
     1                                   CALL XIT('APREP',-1)
C
          IF(IWF(I).EQ.0) THEN
              IF(ISAND(I,1).EQ.-4) THEN
                  ZPLIMG(I)=0.001
              ELSEIF(ISAND(I,1).EQ.-3) THEN
                  ZPLIMG(I)=0.001
              ELSE
                  ZPLIMG(I)=0.002
              ENDIF
              IF(FGS(I).GT.0.0) THEN
                  ZPLMGS(I)=(ZPLIMG(I)*FSNOW(I)*(1.0-FCANMX(I,1)-
     1                      FCANMX(I,2)-FCANMX(I,3)-FCANMX(I,4))+
     2                      ZPLIMG(I)*(FSNOW(I)*FCANMX(I,3)-
     3                      FCANS(I,3))+0.003*(FSNOW(I)*FCANMX(I,4)-
     4                      FCANS(I,4)))/FGS(I)
              ELSE
                  ZPLMGS(I)=0.0
              ENDIF
              IF(FC(I).GT.0.0) THEN
                  ZPLIMC(I)=(0.01*(FCAN(I,1)+FCAN(I,2))+0.003*
     1                      (FCAN(I,3)+FCAN(I,4)))/FC(I)
              ELSE
                  ZPLIMC(I)=0.0
              ENDIF
              IF(FCS(I).GT.0.0) THEN
                  ZPLMCS(I)=(0.01*(FCANS(I,1)+FCANS(I,2))+0.003*
     1                      (FCANS(I,3)+FCANS(I,4)))/FCS(I)
              ELSE
                  ZPLMCS(I)=0.0
              ENDIF
          ELSE
              ZPLMCS(I)=ZPLMS0(I)
              ZPLMGS(I)=ZPLMS0(I)
              ZPLIMC(I)=ZPLMG0(I)
              ZPLIMG(I)=ZPLMG0(I)
          ENDIF
  175 CONTINUE                                                                    
C                                                                                 
C     * PARTITION INTERCEPTED LIQUID AND FROZEN MOISTURE BETWEEN
C     * CANOPY OVERLYING BARE GROUND AND CANOPY OVERLYING SNOW,
C     * USING DIFFERENT EFFECTIVE LEAF AREAS FOR EACH.  ADD
C     * RESIDUAL TO SOIL MOISTURE OR SNOW (IF PRESENT); CALCULATE
C     * RELATIVE FRACTIONS OF LIQUID AND FROZEN INTERCEPTED 
C     * MOISTURE ON CANOPY.
C                                                                                 
      DO 200 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                     THEN                
              PAICAN(I)=(FCAN(I,1)*PAI(I,1)+FCAN(I,2)*PAI(I,2)+                   
     1                   FCAN(I,3)*PAI(I,3)+FCAN(I,4)*PAI(I,4))/FC(I)             
          ELSE                                                                    
              PAICAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              PAICNS(I)=(FCANS(I,1)*PAIS(I,1)+FCANS(I,2)*PAIS(I,2)+               
     1                   FCANS(I,3)*PAIS(I,3)+FCANS(I,4)*PAIS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              PAICNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          CWLCAP(I)=0.20*PAICAN(I)                                                
          CWLCPS(I)=0.20*PAICNS(I)                                                
C
          RRESID(I)=0.0
          IF(RCAN(I).LT.1.0E-5 .OR. (FC(I)+FCS(I)).LT.1.0E-5) THEN
              RRESID(I)=RRESID(I)+RCAN(I)
              RCAN(I)=0.0
          ENDIF
C
          IF(RCAN(I).GT.0. .AND. (FC(I)+FCS(I)).GT.0.)        THEN                
              RCAN(I)=RCAN(I)/(FC(I)+FCS(I))                                      
              IF(PAICAN(I).GT.0.0)                 THEN                           
                  RAICAN(I)=RCAN(I)*(FC(I)+FCS(I))/(FC(I)+FCS(I)*                 
     1                      PAICNS(I)/PAICAN(I))                                  
              ELSE                                                                
                  RAICAN(I)=0.0                                                   
              ENDIF                                                               
              IF(PAICNS(I).GT.0.0)                 THEN                           
                  RAICNS(I)=RCAN(I)*(FC(I)+FCS(I))/(FCS(I)+FC(I)*                 
     1                      PAICAN(I)/PAICNS(I))                                  
              ELSE                                                               
                  RAICNS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              RAICAN(I)=0.0                                                       
              RAICNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF(FC(I).GT.0.)                                     THEN                
              PAICAN(I)=(0.7*FCAN(I,1)*PAI(I,1)+FCAN(I,2)*PAI(I,2)+                   
     1                   FCAN(I,3)*PAI(I,3)+FCAN(I,4)*PAI(I,4))/FC(I)             
          ELSE                                                                    
              PAICAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              PAICNS(I)=(0.7*FCANS(I,1)*PAIS(I,1)+FCANS(I,2)*PAIS(I,2)+               
     1                   FCANS(I,3)*PAIS(I,3)+FCANS(I,4)*PAIS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              PAICNS(I)=0.0                                                       
          ENDIF                                                                   
C
          CWFCAP(I)=6.0*PAICAN(I)*(0.27+46.0/RHOSNI(I))
          CWFCPS(I)=6.0*PAICNS(I)*(0.27+46.0/RHOSNI(I))
C
          SRESID(I)=0.0
          IF(SNCAN(I).LT.1.0E-5 .OR. (FC(I)+FCS(I)).LT.1.0E-5) THEN
              SRESID(I)=SRESID(I)+SNCAN(I)
              SNCAN(I)=0.0
          ENDIF
C
          IF(SNCAN(I).GT.0. .AND. (FC(I)+FCS(I)).GT.0.)        THEN                
              SNCAN(I)=SNCAN(I)/(FC(I)+FCS(I))                                      
              IF(PAICAN(I).GT.0.0)                 THEN                           
                  SNOCAN(I)=SNCAN(I)*(FC(I)+FCS(I))/(FC(I)+FCS(I)*                 
     1                      PAICNS(I)/PAICAN(I))                                  
              ELSE                                                                
                  SNOCAN(I)=0.0                                                   
              ENDIF                                                               
              IF(PAICNS(I).GT.0.0)                 THEN                           
                  SNOCNS(I)=SNCAN(I)*(FC(I)+FCS(I))/(FCS(I)+FC(I)*                 
     1                      PAICAN(I)/PAICNS(I))                                  
              ELSE                                                                
                  SNOCNS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              SNOCAN(I)=0.0                                                       
              SNOCNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF(CWFCAP(I).GT.0.0)                                  THEN
              FSNOWC(I)=MIN(SNOCAN(I)/CWFCAP(I),1.0)
          ELSE
              FSNOWC(I)=0.0
          ENDIF
          IF(CWFCPS(I).GT.0.0)                                  THEN
              FSNOCS(I)=MIN(SNOCNS(I)/CWFCPS(I),1.0)
          ELSE
              FSNOCS(I)=0.0
          ENDIF
C
          IF(CWLCAP(I).GT.0.0)                                  THEN
              FRAINC(I)=MIN(RAICAN(I)/CWLCAP(I),1.0)
          ELSE
              FRAINC(I)=0.0
          ENDIF
          IF(CWLCPS(I).GT.0.0)                                  THEN
              FRAICS(I)=MIN(RAICNS(I)/CWLCPS(I),1.0)
          ELSE                                                                    
              FRAICS(I)=0.0                                                       
          ENDIF                                                                   
          FRAINC(I)=MAX(0.0,MIN(FRAINC(I)-FSNOWC(I),1.0))
          FRAICS(I)=MAX(0.0,MIN(FRAICS(I)-FSNOCS(I),1.0))
C                                                                                 
          IF(RAICAN(I).GT.CWLCAP(I))                            THEN
              RRESID(I)=RRESID(I)+FC(I)*(RAICAN(I)-CWLCAP(I))
              RAICAN(I)=CWLCAP(I)
          ENDIF
          IF(SNOCAN(I).GT.CWFCAP(I))                            THEN
              SRESID(I)=SRESID(I)+FC(I)*(SNOCAN(I)-CWFCAP(I))
              SNOCAN(I)=CWFCAP(I)
          ENDIF
C
          IF(RAICNS(I).GT.CWLCPS(I))                            THEN
              RRESID(I)=RRESID(I)+FCS(I)*(RAICNS(I)-CWLCPS(I))
              RAICNS(I)=CWLCPS(I)
          ENDIF
          IF(SNOCNS(I).GT.CWFCPS(I))                            THEN
              SRESID(I)=SRESID(I)+FCS(I)*(SNOCNS(I)-CWFCPS(I))
              SNOCNS(I)=CWFCPS(I)
          ENDIF
C
          WTRC (I)=WTRC(I)-(RRESID(I)+SRESID(I))/DELT
          HTCC (I)=HTCC(I)-TCAN(I)*(SPHW*RRESID(I)+SPHICE*SRESID(I))/
     1             DELT
          IF(FSNOW(I).GT.0.0)                      THEN                           
              SNOI=SNO(I)
              ZSNADD=SRESID(I)/(RHOSNO(I)*FSNOW(I))                               
              ZSNOW(I)=ZSNOW(I)+ZSNADD
              SNO(I)=ZSNOW(I)*FSNOW(I)*RHOSNO(I)                                  
              TSNOW(I)=(TCAN(I)*SPHICE*SRESID(I)+TSNOW(I)*HCPICE*
     1                 SNOI/RHOICE)/(HCPICE*SNO(I)/RHOICE)
              HTCS (I)=HTCS(I)+TCAN(I)*SPHICE*SRESID(I)/DELT
              WTRS (I)=WTRS(I)+SRESID(I)/DELT
              SRESID(I)=0.0
          ENDIF                                                                   
C
          DO 190 J=1,IG
              IF(DELZW(I,J).GT.0.0 .AND. (RRESID(I).GT.0.0
     1                  .OR. SRESID(I).GT.0.0))                THEN
                  THSUM=THLIQ(I,J)+THICE(I,J)+
     1                (RRESID(I)+SRESID(I))/(RHOW*DELZW(I,J))
                  IF(THSUM.LT.THPOR(I,J)) THEN
                      THICEI=THICE(I,J) 
                      THLIQI=THLIQ(I,J)
                      THICE(I,J)=THICE(I,J)+SRESID(I)/
     1                    (RHOICE*DELZW(I,J))                        
                      THLIQ(I,J)=THLIQ(I,J)+RRESID(I)/
     1                    (RHOW*DELZW(I,J))                             
                      TBAR(I,J)=(TBAR(I,J)*((DELZ(J)-DELZW(I,J))*
     1                    HCPSND+DELZW(I,J)*(THLIQI*HCPW+THICEI*
     2                    HCPICE+(1.0-THPOR(I,J))*HCPS(I,J)))+TCAN(I)*
     3                    (RRESID(I)*HCPW/RHOW+SRESID(I)*HCPICE/RHOICE))
     4                    /((DELZ(J)-DELZW(I,J))*HCPSND+DELZW(I,J)*
     5                    (HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+HCPS(I,J)*
     6                    (1.0-THPOR(I,J))))
                      HTC(I,J)=HTC(I,J)+TCAN(I)*(RRESID(I)*HCPW/RHOW+
     1                    SRESID(I)*HCPICE/RHOICE)/DELT
                      WTRG (I)=WTRG(I)+(RRESID(I)+SRESID(I))/DELT
                      RRESID(I)=0.0
                      SRESID(I)=0.0
                  ENDIF
              ENDIF
  190     CONTINUE
C
  200 CONTINUE                                                                    
C                                                                                 
C     * CALCULATION OF ROUGHNESS LENGTHS FOR HEAT AND MOMENTUM AND
C     * ZERO-PLANE DISPLACEMENT FOR CANOPY OVERLYING BARE SOIL AND
C     * CANOPY OVERLYING SNOW.
C                                                                                 
      DO 250 J=1,IC                                                               
      DO 250 I=IL1,IL2                                                            
          IF(FC(I).GT.0. .AND. H(I,J).GT.0.)                     THEN             
              IF(IDISP.EQ.1)   DISP(I)=DISP(I)+FCAN (I,J)*
     1                                 LOG(0.7*H(I,J))                     
              ZOMLNC(I)=ZOMLNC(I)+FCAN (I,J)/
     1                  ((LOG(ZBLEND(I)/(0.1*H(I,J))))**2)
              ZOELNC(I)=ZOELNC(I)*
     1                  (0.01*H(I,J)*H(I,J)/ZORAT(IC))**FCAN(I,J)
          ENDIF                                                                   
          IF(FCS(I).GT.0. .AND. HS(I,J).GT.0.)                   THEN             
              IF(IDISP.EQ.1)   DISPS(I)=DISPS (I)+FCANS(I,J)*
     1                         LOG(0.7*HS(I,J))                    
              ZOMLCS(I)=ZOMLCS(I)+FCANS(I,J)/
     1                  ((LOG(ZBLEND(I)/(0.1*HS(I,J))))**2)
              ZOELCS(I)=ZOELCS(I)*
     1                  (0.01*HS(I,J)*HS(I,J)/ZORAT(IC))**FCANS(I,J)
          ENDIF                                                                   
  250 CONTINUE                                                                    
C                                                                                 
      DO 275 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              IF(IDISP.EQ.1)   DISP(I)=EXP(DISP(I)/FC(I))                                        
              ZOMLNC(I)=ZBLEND(I)/EXP(SQRT(1.0/(ZOMLNC(I)/FC(I)))) 
              ZOELNC(I)=LOG(ZOELNC(I)**(1.0/FC(I))/ZOMLNC(I))
              ZOMLNC(I)=LOG(ZOMLNC(I))
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                       THEN             
              IF(IDISP.EQ.1)   DISPS(I)=EXP(DISPS(I)/FCS(I))                                      
              ZOMLCS(I)=ZBLEND(I)/EXP(SQRT(1.0/(ZOMLCS(I)/FCS(I)))) 
              ZOELCS(I)=LOG(ZOELCS(I)**(1.0/FCS(I))/ZOMLCS(I))
              ZOMLCS(I)=LOG(ZOMLCS(I))
          ENDIF                                                                   
  275 CONTINUE                                                                    
C                                                                                 
C     * ADJUST ROUGHNESS LENGTHS OF BARE SOIL AND SNOW-COVERED BARE
C     * SOIL FOR URBAN ROUGHNESS IF PRESENT.
C                                                                                 
      DO 300 I=IL1,IL2                                                            
          IF(FG(I).GT.0.)                                        THEN             
              IF(ISAND(I,1).NE.-4)                   THEN                         
                  ZOMLNG(I)=((FG(I)-FCANMX(I,5)*(1.0-FSNOW(I)))*ZOLNG+            
     1                      FCANMX(I,5)*(1.0-FSNOW(I))*ZOLN(I,5))/FG(I)           
              ELSE                                                                
                  ZOMLNG(I)=ZOLNI                                                 
              ENDIF                                                               
              ZOELNG(I)=ZOMLNG(I)-LOG(ZORATG)                                    
          ENDIF                                                                   
          IF(FGS(I).GT.0.)                                       THEN             
              ZOMLNS(I)=((FGS(I)-FCANMX(I,5)*FSNOW(I))*ZOLNS+                     
     1                  FCANMX(I,5)*FSNOW(I)*ZOLN(I,5))/FGS(I)                    
              ZOELNS(I)=ZOMLNS(I)-LOG(ZORATG)                                    
          ENDIF                                                                   
  300 CONTINUE                                                                    
C                                                                                 
C     * ADD CONTRIBUTION OF OROGRAPHY TO MOMENTUM ROUGNESS LENGTH
C
      DO 325 I=IL1,IL2
          IF(Z0ORO(I).GT.1.0E-4) THEN
              LZ0ORO=LOG(Z0ORO(I))
          ELSE
              LZ0ORO=-10.0
          ENDIF
          ZOMLNC(I)=MAX(ZOMLNC(I),LZ0ORO)
          ZOMLCS(I)=MAX(ZOMLCS(I),LZ0ORO)
          ZOMLNG(I)=MAX(ZOMLNG(I),LZ0ORO)
          ZOMLNS(I)=MAX(ZOMLNS(I),LZ0ORO)
  325  CONTINUE
C     
C     * CALCULATE HEAT CAPACITY FOR CANOPY OVERLYING BARE SOIL AND
C     * CANOPY OVERLYING SNOW.
C     * ALSO CALCULATE INSTANTANEOUS GRID-CELL AVERAGED CANOPY MASS.
C                                                                                 
      DO 350 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                       THEN                     
              CMASSC(I)=(FCAN(I,1)*CWGTMX(I,1)+FCAN (I,2)*CWGTMX(I,2)+                   
     1                   FCAN(I,3)*CWGTMX(I,3)*GROWA(I)+
     2                   FCAN(I,4)*CWGTMX(I,4))/FC (I)           
              IF(IDISP.EQ.0) THEN
                  CMASSC(I)=CMASSC(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.7*
     1                     (FCAN(I,1)*H(I,1)+FCAN(I,2)*H(I,2)+
     2                      FCAN(I,3)*H(I,3)+FCAN(I,4)*H(I,4))/FC(I)
              ENDIF
              IF(IZREF.EQ.2) THEN
                  CMASSC(I)=CMASSC(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.1*
     1                     (FCAN(I,1)*H(I,1)+FCAN(I,2)*H(I,2)+
     2                      FCAN(I,3)*H(I,3)+FCAN(I,4)*H(I,4))/FC(I)
              ENDIF
          ENDIF                                                                          
          IF(FCS(I).GT.0.)                                      THEN                     
              CMASCS(I)=(FCANS(I,1)*CWGTMX(I,1)+FCANS(I,2)*CWGTMX(I,2)+                  
     1                   FCANS(I,3)*CWGTMX(I,3)*GROWA(I)
     2                  *HS(I,3)/MAX(H(I,3),HS(I,3))+                            
     3                   FCANS(I,4)*CWGTMX(I,4)                         
     4                  *HS(I,4)/MAX(H(I,4),HS(I,4)))/FCS(I)                     
              IF(IDISP.EQ.0) THEN
                  CMASCS(I)=CMASCS(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.7*
     1                      (FCANS(I,1)*HS(I,1)+FCANS(I,2)*HS(I,2)+
     2                       FCANS(I,3)*HS(I,3)+FCANS(I,4)*HS(I,4))/
     3                       FCS(I)
              ENDIF
              IF(IZREF.EQ.2) THEN
                  CMASCS(I)=CMASCS(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.1*
     1                      (FCANS(I,1)*HS(I,1)+FCANS(I,2)*HS(I,2)+
     2                       FCANS(I,3)*HS(I,3)+FCANS(I,4)*HS(I,4))/
     3                       FCS(I)
              ENDIF
          ENDIF                                                                   
          CHCAP (I)=SPHVEG*CMASSC(I)+SPHW*RAICAN(I)+SPHICE*SNOCAN(I)              
          CHCAPS(I)=SPHVEG*CMASCS(I)+SPHW*RAICNS(I)+SPHICE*SNOCNS(I)              
          HTCC  (I)=HTCC(I)-SPHVEG*CMAI(I)*TCAN(I)/DELT
          IF(CMAI(I).LT.1.0E-5 .AND. (CMASSC(I).GT.0.0 .OR.
     1              CMASCS(I).GT.0.0)) TCAN(I)=TA(I)
          CMAI  (I)=FC(I)*CMASSC(I)+FCS(I)*CMASCS(I)
          HTCC  (I)=HTCC(I)+SPHVEG*CMAI(I)*TCAN(I)/DELT
          RBCOEF(I)=0.0
  350 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE VEGETATION ROOTING DEPTH AND FRACTION OF ROOTS 
C     * IN EACH SOIL LAYER (SAME FOR SNOW/BARE SOIL CASES).
C     * ALSO CALCULATE LEAF BOUNDARY RESISTANCE PARAMETER RBCOEF.
C                                                                                 
      DO 450 J=1,IC                                                               
      DO 450 I=IL1,IL2                                                            
        IF (ICTEMMOD.EQ.1) THEN
          RMAT(I,J,1)=RMATC(I,J,1)
          RMAT(I,J,2)=RMATC(I,J,2)
          RMAT(I,J,3)=RMATC(I,J,3)
        ELSE
          ZROOT=ZRTMAX(I,J)
          IF(J.EQ.3) ZROOT=ZRTMAX(I,J)*GROWA(I)                                   
          ZROOTG=0.0
          DO 375 K=1,IG
              ZROOTG=ZROOTG+DELZW(I,K)
375       CONTINUE
          ZROOT=MIN(ZROOT,ZROOTG)
          DO 400 K=1,IG
              IF(ZROOT.LE.(ZBOTW(I,K)-DELZW(I,K)+0.0001))          THEN
                  RMAT(I,J,K)=0.0
              ELSEIF(ZROOT.LE.ZBOTW(I,K))                          THEN             
                  RMAT(I,J,K)=(EXP(-3.0*(ZBOTW(I,K)-DELZW(I,K)))-
     1                EXP(-3.0*ZROOT))/(1.0-EXP(-3.0*ZROOT))
              ELSE                                                                    
                  RMAT(I,J,K)=(EXP(-3.0*(ZBOTW(I,K)-DELZW(I,K)))-
     1                EXP(-3.0*ZBOTW(I,K)))/(1.0-EXP(-3.0*ZROOT))
              ENDIF
400       CONTINUE
        ENDIF
C
        IF((FC(I)+FCS(I)).GT.0.)                               THEN             
            RBCOEF(I)=RBCOEF(I)+
     1                (FCAN(I,J)*XLEAF(J)*(SQRT(PAI(I,J))/0.75)*
     2                (1.0-EXP(-0.75*SQRT(PAI(I,J))))+
     3                FCANS(I,J)*XLEAF(J)*(SQRT(PAIS(I,J))/0.75)*
     4                (1.0-EXP(-0.75*SQRT(PAIS(I,J)))))/
     5                (FC(I)+FCS(I))                                          
        ENDIF                                                                   
  450 CONTINUE                                                                    
C                                                                                 
      DO 500 J=1,IG                                                               
      DO 500 I=IL1,IL2                                                            
          IF((FC(I)+FCS(I)).GT.0.)                               THEN             
              FROOT(I,J)=((FCAN(I,1)+FCANS(I,1))*RMAT(I,1,J) +                    
     1                    (FCAN(I,2)+FCANS(I,2))*RMAT(I,2,J) +                    
     2                    (FCAN(I,3)+FCANS(I,3))*RMAT(I,3,J) +                    
     3                    (FCAN(I,4)+FCANS(I,4))*RMAT(I,4,J))/                    
     4                    (FC(I)+FCS(I))                                          
          ELSE                                                                    
              FROOT(I,J)=0.0                                                      
          ENDIF                                                                   
  500 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE SKY-VIEW FACTORS FOR BARE GROUND AND SNOW 
C     * UNDERLYING CANOPY.                                                         
C                                                                                 
      DO 600 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              FSVF (I)=(FCAN (I,1)*EXP(CANEXT(1)*PAI (I,1)) +                          
     1                  FCAN (I,2)*EXP(CANEXT(2)*PAI (I,2)) +                          
     2                  FCAN (I,3)*EXP(CANEXT(3)*PAI (I,3)) +                          
     3                  FCAN (I,4)*EXP(CANEXT(4)*PAI (I,4)))/FC (I)                    
          ELSE                                                                    
              FSVF (I)=0.                                                         
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                       THEN             
              FSVFS(I)=(FCANS(I,1)*EXP(CANEXT(1)*PAIS(I,1)) +                          
     1                  FCANS(I,2)*EXP(CANEXT(2)*PAIS(I,2)) +                          
     2                  FCANS(I,3)*EXP(CANEXT(3)*PAIS(I,3)) +                          
     3                  FCANS(I,4)*EXP(CANEXT(4)*PAIS(I,4)))/FCS(I)                    
          ELSE                                                                    
              FSVFS(I)=0.                                                         
          ENDIF                                                                   
  600 CONTINUE                                       
C                                                                                  
C     * CALCULATE BULK SOIL MOISTURE SUCTION FOR STOMATAL RESISTANCE.
C     * CALCULATE FRACTIONAL TRANSPIRATION EXTRACTED FROM SOIL LAYERS.
C
      DO 650 J=1,IG                                                               
      DO 650 I=IL1,IL2                                                            
          IF(FCS(I).GT.0.0 .OR. FC(I).GT.0.0)                      THEN          
              IF(THLIQ(I,J).GT.(THLMIN(I,J)+0.01) .AND. 
     1                           FROOT(I,J).GT.0.)             THEN               
                  PSII=PSISAT(I,J)*(THLIQ(I,J)/THPOR(I,J))**(-BI(I,J))
                  PSII=MIN(PSII,PSIWLT(I,J))
                  PSIGND(I)=MIN(PSIGND(I),PSII)                                 
                  FROOT(I,J)=FROOT(I,J)*(PSIWLT(I,J)-PSII)/
     1                       (PSIWLT(I,J)-PSISAT(I,J))          
                  FRTOT(I)=FRTOT(I)+FROOT(I,J)                                    
              ELSE
                  FROOT(I,J)=0.0
              ENDIF                                                               
          ENDIF                                                                   
  650 CONTINUE                                                                    
C                                                                                 
      DO 700 J=1,IG                                                               
      DO 700 I=IL1,IL2                                                            
          IF(FRTOT(I).GT.0.)                                       THEN           
              FROOT(I,J)=FROOT(I,J)/FRTOT(I)                                      
          ENDIF                                                                   
  700 CONTINUE                                                                    
C 
C     * CALCULATE EFFECTIVE LEAF AREA INDICES FOR TRANSPIRATION.
C
      DO 800 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                     THEN                
              PAICAN(I)=(FCAN(I,1)*PAI(I,1)+FCAN(I,2)*PAI(I,2)+                   
     1                   FCAN(I,3)*PAI(I,3)+FCAN(I,4)*PAI(I,4))/FC(I)             
          ELSE                                                                    
              PAICAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              PAICNS(I)=(FCANS(I,1)*PAIS(I,1)+FCANS(I,2)*PAIS(I,2)+               
     1                   FCANS(I,3)*PAIS(I,3)+FCANS(I,4)*PAIS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              PAICNS(I)=0.0                                                       
          ENDIF                                                                   
  800 CONTINUE
C
      IF (ICTEMMOD.EQ.1) THEN
C
C       * ESTIMATE FCANC AND FCANCS FOR USE BY PHTSYN SUBROUTINE BASED ON
C       * FCAN AND FCANS FOR CTEM PFTS.
C
        DO 810 J = 1, IC
        DO 810 I = IL1, IL2
          SFCANCMX(I,J)=0.0  ! SUM OF FCANCMXS
  810   CONTINUE
C
        K1=0
        DO 830 J = 1, IC
          IF(J.EQ.1) THEN
            K1 = K1 + 1
          ELSE
            K1 = K1 + NOL2PFTS(J-1)
          ENDIF
          K2 = K1 + NOL2PFTS(J) - 1
          DO 820 M = K1, K2
          DO 820 I = IL1, IL2
              SFCANCMX(I,J)=SFCANCMX(I,J)+FCANCMX(I,M)
  820     CONTINUE
  830   CONTINUE
C
        K1=0
        DO 860 J = 1, IC
          IF(J.EQ.1) THEN
            K1 = K1 + 1
          ELSE
            K1 = K1 + NOL2PFTS(J-1)
          ENDIF
          K2 = K1 + NOL2PFTS(J) - 1
          DO 850 M = K1, K2
          DO 850 I = IL1, IL2
             IF(SFCANCMX(I,J).GT.1E-20) THEN
               FCANC(I,M)  = FCAN(I,J) * (FCANCMX(I,M)/SFCANCMX(I,J))
               FCANCS(I,M) = FCANS(I,J)* (FCANCMX(I,M)/SFCANCMX(I,J))
             ELSE
               FCANC(I,M)  = 0.0
               FCANCS(I,M) = 0.0
             ENDIF
  850     CONTINUE
  860   CONTINUE
      ENDIF
C                                                                                 
      RETURN                                                                      
      END 
