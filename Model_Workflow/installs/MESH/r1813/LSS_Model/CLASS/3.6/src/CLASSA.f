      SUBROUTINE CLASSA(FC,     FG,     FCS,    FGS,    ALVSCN, ALIRCN,
     1                  ALVSG,  ALIRG,  ALVSCS, ALIRCS, ALVSSN, ALIRSN,           
     2                  ALVSGC, ALIRGC, ALVSSC, ALIRSC, TRVSCN, TRIRCN, 
     3                  TRVSCS, TRIRCS, FSVF,   FSVFS,  
     4                  RAICAN, RAICNS, SNOCAN, SNOCNS, FRAINC, FSNOWC, 
     5                  FRAICS, FSNOCS, DISP,   DISPS,  ZOMLNC, ZOMLCS, 
     6                  ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, 
     7                  CHCAP,  CHCAPS, CMASSC, CMASCS, CWLCAP, CWFCAP,
     8                  CWLCPS, CWFCPS, RC,     RCS,    RBCOEF, FROOT,  
     9                  ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TRSNOW, ZSNOW,  
     A                  WSNOW,  ALVS,   ALIR,   HTCC,   HTCS,   HTC,    
     B                  WTRC,   WTRS,   WTRG,   CMAI,   FSNOW,
     C                  FCANMX, ZOLN,   ALVSC,  ALIRC,  PAIMAX, PAIMIN, 
     D                  CWGTMX, ZRTMAX, RSMIN,  QA50,   VPDA,   VPDB,
     E                  PSIGA,  PSIGB,  PAIDAT, HGTDAT, ACVDAT, ACIDAT, 
     F                  ASVDAT, ASIDAT, AGVDAT, AGIDAT, ALGWET, ALGDRY, 
     G                  THLIQ,  THICE,  TBAR,   RCAN,   SNCAN,  TCAN,   
     H                  GROWTH, SNO,    TSNOW,  RHOSNO, ALBSNO, ZBLEND,
     I                  Z0ORO,  SNOLIM, ZPLMG0, ZPLMS0, 
     J                  FCLOUD, TA,     VPD,    RHOAIR, COSZS,  
     K                  QSWINV, RADJ,   DLON,   RHOSNI, DELZ,   DELZW,  
     L                  ZBOTW,  THPOR,  THLMIN, PSISAT, BI,     PSIWLT, 
     M                  HCPS,   ISAND,  
     N                  FCANCMX,ICTEM,  ICTEMMOD,       RMATC,
     O                  AILC,   PAIC,   L2MAX,  NOL2PFTS,
     P                  AILCG,  AILCGS, FCANC,  FCANCS,
     Q                  IDAY,   ILG,    IL1,    IL2,    
     R                  JL,N,   IC,     ICP1,   IG,     IDISP,  IZREF,
     S                  IWF,    IPAI,   IHGT,   IALC,   IALS,   IALG)
C
C     * NOV 14/11 - M.LAZARE.   IMPLEMENT CTEM SUPPORT, PRIMARILY
C     *                         INVOLVING ADDITIONAL FIELDS TO PASS
C     *                         IN/OUT OF NEW APREP ROUTINE. THIS 
C     *                         INCLUDES NEW INPUT ARRAY "PAIC".
C     * NOV 30/06 - D.VERSEGHY. CONVERT RADJ TO REGULAR PRECISION.
C     * APR 13/06 - D.VERSEGHY. SEPARATE GROUND AND SNOW ALBEDOS FOR 
C     *                         OPEN AND CANOPY-COVERED AREAS; KEEP
C     *                         FSNOW AS OUTPUT ARRAY.
C     * APR 06/06 - D.VERSEGHY. INTRODUCE MODELLING OF WSNOW.
C     * MAR 14/05 - D.VERSEGHY. RENAME SCAN TO SNCAN (RESERVED NAME
C     *                         IN F90); CHANGE SNOLIM FROM CONSTANT
C     *                         TO VARIABLE.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * DEC 05/02 - D.VERSEGHY. NEW PARAMETERS FOR APREP.
C     * JUL 31/02 - D.VERSEGHY. MODIFICATIONS ASSOCIATED WITH NEW
C     *                         CALCULATION OF STOMATAL RESISTANCE.
C     *                         SHORTENED CLASS3 COMMON BLOCK.
C     * JUL 23/02 - D.VERSEGHY. MODIFICATIONS TO MOVE ADDITION OF AIR
C     *                         TO CANOPY MASS INTO APREP; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * MAR 18/02 - D.VERSEGHY. NEW CALLS TO ALL SUBROUTINES TO ENABLE
C     *                         ASSIGNMENT OF USER-SPECIFIED VALUES TO
C     *                         ALBEDOS AND VEGETATION PROPERTIES; NEW
C     *                         "CLASS8" COMMON BLOCK; MOVE CALCULATION 
C     *                         OF "FCLOUD" INTO CLASS DRIVER.
C     * SEP 19/00 - D.VERSEGHY. PASS ADDITIONAL ARRAYS TO APREP IN COMMON 
C     *                         BLOCK CLASS7, FOR CALCULATION OF NEW 
C     *                         STOMATAL RESISTANCE COEFFICIENTS USED
C     *                         IN TPREP.
C     * APR 12/00 - D.VERSEGHY. RCMIN NOW VARIES WITH VEGETATION TYPE:
C     *                         PASS IN BACKGROUND ARRAY "RCMINX".
C     * DEC 16/99 - D.VERSEGHY. ADD "XLEAF" ARRAY TO CLASS7 COMMON BLOCK
C     *                         AND CALCULATION OF LEAF DIMENSION PARAMETER
C     *                         "DLEAF" IN APREP.
C     * NOV 16/98 - M.LAZARE.   "DLON" NOW PASSED IN AND USED DIRECTLY
C     *                         (INSTEAD OF INFERRING FROM "LONSL" AND 
C     *                         "ILSL" WHICH USED TO BE PASSED) TO PASS
C     *                         TO APREP TO CALCULATE GROWTH INDEX. THIS
C     *                         IS DONE TO MAKE THE PHYSICS PLUG COMPATIBLE
C     *                         FOR USE WITH THE RCM WHICH DOES NOT HAVE
C     *                         EQUALLY-SPACED LONGITUDES.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         FIX BUG TO CALCULATE GROUND ALBEDO
C     *                         UNDER CANOPIES AS WELL AS OVER BARE
C     *                         SOIL.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     *                         ALSO, PASS IDISP TO SUBROUTINE APREP.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 16/95 - D.VERSEGHY. THREE NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * OCT 14/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF FCLOUD TO
C     *                         HANDLE CASES WHERE INCOMING SOLAR
C     *                         RADIATION IS ZERO AT LOW SUN ANGLES.
C     * NOV 24/92 - M.LAZARE.   CLASS - VERSION 2.1.
C     *                         MODIFIED FOR MULTIPLE LATITUDES.
C     * OCT 13/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. VISIBLE AND NEAR-IR ALBEDOS AND 
C     *                         TRANSMISSIVITIES FOR COMPONENTS OF
C     *                         LAND SURFACE.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IDAY,ILG,IL1,IL2,JL,IC,ICP1,IG,IDISP,IZREF,
     1        IPAI,IHGT,IALC,IALS,IALG,I,J,N
      INTEGER IWF(ILG)
C
C     * OUTPUT ARRAYS.
C
      REAL FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),    
     1     ALVSCN(ILG),   ALIRCN(ILG),   ALVSG (ILG),   ALIRG (ILG),     
     2     ALVSCS(ILG),   ALIRCS(ILG),   ALVSSN(ILG),   ALIRSN(ILG),   
     3     ALVSGC(ILG),   ALIRGC(ILG),   ALVSSC(ILG),   ALIRSC(ILG),
     3     TRVSCN(ILG),   TRIRCN(ILG),   TRVSCS(ILG),   TRIRCS(ILG),  
     4     FSVF  (ILG),   FSVFS (ILG),   
     5     RAICAN(ILG),   RAICNS(ILG),   SNOCAN(ILG),   SNOCNS(ILG),   
     6     FRAINC(ILG),   FSNOWC(ILG),   FRAICS(ILG),   FSNOCS(ILG),
     7     DISP  (ILG),   DISPS (ILG),   ZOMLNC(ILG),   ZOMLCS(ILG),   
     8     ZOELNC(ILG),   ZOELCS(ILG),   ZOMLNG(ILG),   ZOMLNS(ILG),   
     9     ZOELNG(ILG),   ZOELNS(ILG),   CHCAP (ILG),   CHCAPS(ILG),   
     A     CMASSC(ILG),   CMASCS(ILG),   CWLCAP(ILG),   CWFCAP(ILG),   
     B     CWLCPS(ILG),   CWFCPS(ILG),   RC    (ILG),   RCS   (ILG),   
     C     ZPLIMC(ILG),   ZPLIMG(ILG),   ZPLMCS(ILG),   ZPLMGS(ILG),   
     D     RBCOEF(ILG),   TRSNOW(ILG),   ZSNOW (ILG),   WSNOW (ILG),
     E     ALVS  (ILG),   ALIR  (ILG),   HTCC  (ILG),   HTCS  (ILG),   
     F     WTRC  (ILG),   WTRS  (ILG),   WTRG  (ILG),   CMAI  (ILG),
     G     FSNOW (ILG)
C
      REAL FROOT (ILG,IG),  HTC   (ILG,IG)
C
C     * INPUT ARRAYS DEPENDENT ON LONGITUDE.
C  
      REAL FCANMX(ILG,ICP1),   ZOLN  (ILG,ICP1),
     1     ALVSC (ILG,ICP1),   ALIRC (ILG,ICP1),
     2     PAIMAX(ILG,IC),     PAIMIN(ILG,IC),     CWGTMX(ILG,IC),                     
     3     ZRTMAX(ILG,IC),     RSMIN (ILG,IC),     QA50  (ILG,IC),
     4     VPDA  (ILG,IC),     VPDB  (ILG,IC),     PSIGA (ILG,IC),
     5     PSIGB (ILG,IC),     PAIDAT(ILG,IC),     HGTDAT(ILG,IC),     
     4     ACVDAT(ILG,IC),     ACIDAT(ILG,IC),
     5     THLIQ (ILG,IG),     THICE (ILG,IG),     TBAR  (ILG,IG)  
C
      REAL ASVDAT(ILG),   ASIDAT(ILG),   AGVDAT(ILG),   AGIDAT(ILG),
     1     ALGWET(ILG),   ALGDRY(ILG),   RHOSNI(ILG),   Z0ORO (ILG),
     2     RCAN  (ILG),   SNCAN (ILG),   TCAN  (ILG),   GROWTH(ILG),      
     3     SNO   (ILG),   TSNOW (ILG),   RHOSNO(ILG),   ALBSNO(ILG),
     4     FCLOUD(ILG),   TA    (ILG),   VPD   (ILG),   RHOAIR(ILG),
     5     COSZS (ILG),   QSWINV(ILG),   DLON  (ILG),   ZBLEND(ILG),
     6     SNOLIM(ILG),   ZPLMG0(ILG),   ZPLMS0(ILG),   RADJ  (ILG)
C
C    * SOIL PROPERTY ARRAYS.
C
      REAL DELZW (ILG,IG),  ZBOTW (ILG,IG),  THPOR (ILG,IG),  
     1     THLMIN(ILG,IG),  PSISAT(ILG,IG),  BI    (ILG,IG),
     2     PSIWLT(ILG,IG),  HCPS  (ILG,IG)
C
      INTEGER   ISAND (ILG,IG)
C
C     * OTHER DATA ARRAYS WITH NON-VARYING VALUES.
C
      REAL GROWYR(18,4,2),  DELZ  (IG),      ZORAT (4),
     1     CANEXT(4),       XLEAF (4)
C
C     * CTEM-RELATED FIELDS.
C
      REAL FCANCMX(ILG,ICTEM),   RMATC(ILG,IC,IG),
     1      AILC  (ILG,IC),      PAIC (ILG,IC),
     2      AILCG (ILG,ICTEM),   AILCGS(ILG,ICTEM),
     3      FCANC(ILG,ICTEM),    FCANCS(ILG,ICTEM)

      INTEGER ICTEM, ICTEMMOD, L2MAX, NOL2PFTS(IC)
C                                                                                 
C     * INTERNAL WORK ARRAYS FOR THIS AND ASSOCIATED SUBROUTINES.
C
      REAL RMAT (ILG,IC,IG),H     (ILG,IC),  HS    (ILG,IC),
     1     PAI   (ILG,IC),  PAIS  (ILG,IC),  FCAN  (ILG,IC),  
     2     FCANS (ILG,IC),  CXTEFF(ILG,IC),  AIL   (ILG,IC),
     3     RCACC (ILG,IC),  RCG   (ILG,IC),  RCV   (ILG,IC)
C
      REAL PSIGND(ILG),     CWCPAV(ILG),     
     1     GROWA (ILG),     GROWN (ILG),     GROWB (ILG),     
     2     RRESID(ILG),     SRESID(ILG),     FRTOT (ILG),
     3     TRVS  (ILG),     TRIR  (ILG),     RCT   (ILG),     
     4     GC    (ILG),     PAICAN(ILG),     PAICNS(ILG) 
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,
     1     TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,
     2     HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,
     3     RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,PI,ZOLNG,ZOLNS,ZOLNI,
     4     ZORATG,ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
C
      COMMON /CLASS1/ DELT,TFREZ                                               
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS6/ PI,GROWYR,ZOLNG,ZOLNS,ZOLNI,ZORAT,ZORATG                   
      COMMON /CLASS7/ CANEXT,XLEAF
      COMMON /CLASS8/ ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
                                                                                  
C------------------------------------------------------------------
C     * CALCULATION OF SNOW DEPTH ZSNOW AND FRACTIONAL SNOW COVER
C     * FSNOW; INITIALIZATION OF COMPUTATIONAL ARRAYS. 
C                                                                                  
      DO 100 I=IL1,IL2                                                            
          IF(SNO(I).GT.0.0) THEN                                              
              ZSNOW(I)=SNO(I)/RHOSNO(I)                                       
              IF(ZSNOW(I).GE.(SNOLIM(I)-0.00001)) THEN                                     
                  FSNOW(I)=1.0                                                   
              ELSE                                                            
                  FSNOW(I)=ZSNOW(I)/SNOLIM(I)
                  ZSNOW(I)=SNOLIM(I)
                  WSNOW(I)=WSNOW(I)/FSNOW(I)
              ENDIF                                                           
          ELSE                                                                
              ZSNOW(I)=0.0                                                    
              FSNOW(I)=0.0                                                       
          ENDIF
C
          ALVSCN(I)=0.0                                                   
          ALIRCN(I)=0.0                                                   
          ALVSCS(I)=0.0  
          ALIRCS(I)=0.0    
          TRVSCN(I)=0.0                                                   
          TRIRCN(I)=0.0                                                   
          TRVSCS(I)=0.0                                                   
          TRIRCS(I)=0.0
          ALVSSN(I)=0.0                                                   
          ALIRSN(I)=0.0                                                   
          ALVSG (I)=0.0
          ALIRG (I)=0.0
          ALVSGC(I)=0.0
          ALIRGC(I)=0.0
          ALVSSC(I)=0.0
          ALIRSC(I)=0.0
          TRSNOW(I)=0.0                                                       
  100 CONTINUE
C
C     * PREPARATION.
C
      CALL APREP (FC,FG,FCS,FGS,PAICAN,PAICNS,FSVF,FSVFS, 
     1            FRAINC,FSNOWC,FRAICS,FSNOCS,RAICAN,RAICNS,SNOCAN,
     2            SNOCNS,DISP,DISPS,ZOMLNC,ZOMLCS,ZOELNC,ZOELCS,
     3            ZOMLNG,ZOMLNS, ZOELNG,ZOELNS,CHCAP,CHCAPS,CMASSC,
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
C     * SNOW ALBEDOS AND TRANSMISSIVITY.
C 
      CALL SNOALBA(ALVSSN,ALIRSN,ALVSSC,ALIRSC,ALBSNO,TRSNOW,
     1             ZSNOW,FSNOW,ASVDAT,ASIDAT,
     2             ILG,IG,IL1,IL2,JL,IALS)
C
C     * BARE SOIL ALBEDOS.
C
      CALL GRALB(ALVSG,ALIRG,ALVSGC,ALIRGC,
     1            ALGWET,ALGDRY,THLIQ,FSNOW,ALVSC(1,5),ALIRC(1,5),
     2            FCANMX(1,5),AGVDAT,AGIDAT,ISAND,
     3            ILG,IG,IL1,IL2,JL,IALG) 
C
C     * CANOPY ALBEDOS AND TRANSMISSIVITIES, AND VEGETATION
C     * STOMATAL RESISTANCE.
C
      CALL CANALB(ALVSCN,ALIRCN,ALVSCS,ALIRCS,TRVSCN,TRIRCN,
     1            TRVSCS,TRIRCS,RC,RCS,
     2            ALVSC,ALIRC,RSMIN,QA50,VPDA,VPDB,PSIGA,PSIGB,
     3            FC,FCS,FSNOW,FSNOWC,FSNOCS,FCAN,FCANS,PAI,PAIS,
     4            AIL,PSIGND,FROOT,FCLOUD,COSZS,QSWINV,VPD,TA,
     5            ACVDAT,ACIDAT,ALVSGC,ALIRGC,ALVSSC,ALIRSC,
     6            ILG,IL1,IL2,JL,IC,ICP1,IG,IALC,
     7            CXTEFF,TRVS,TRIR,RCACC,RCG,RCV,RCT,GC) 
C
C     * EFFECTIVE WHOLE-SURFACE VISIBLE AND NEAR-IR ALBEDOS.
C
      DO 500 I=IL1,IL2
          ALVS(I)=FC(I)*ALVSCN(I)+FG(I)*ALVSG(I)+FCS(I)*ALVSCS(I)+
     1            FGS(I)*ALVSSN(I)                                                
          ALIR(I)=FC(I)*ALIRCN(I)+FG(I)*ALIRG(I)+FCS(I)*ALIRCS(I)+            
     1            FGS(I)*ALIRSN(I)                                                
  500 CONTINUE
C
      RETURN                                                                      
      END
