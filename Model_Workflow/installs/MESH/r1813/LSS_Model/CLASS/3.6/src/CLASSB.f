      SUBROUTINE CLASSB(THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     1                  THLRAT,HCPS,TCS,THFC,PSIWLT,
     2                  DELZW,ZBOTW,ALGWET,ALGDRY,
     3                  SAND,CLAY,ORGM,DELZ,ZBOT,SDEPTH,
     4                  ISAND,IGDR,NL,NM,IL1,IL2,IM,IG,ICTEMMOD,
     5                  WC_THPOR,WC_THLRET,WC_THLMIN,WC_BI,
     6                  WC_PSISAT,WC_GRKSAT,WC_HCPS,WC_TCS)
C
C     * NOV 11/11 - M.LAZARE.   - IMPLEMENT CTEM CHOICE OF
C     *                           ALGDRY DETERMINED BY ADDED
C     *                           PASSED SWITCH "ICTEMMOD". 
C     * OCT 18/11 - M.LAZARE.   - REMOVE UNUSED "IORG".
C     *                         - CHANGE "THSAND", "THORG"
C     *                           AND "THFINE" FROM ARRAYS
C     *                           (INTERNAL ONLY) TO SCALAR. 
C     *                         - IGDR NOW PASSED OUT TO BE
C     *                           USED IN GRINFL/GRDRAN/WEND.
C     *                         - PASS IN IL1 AND IL2 TO
C     *                           DEFINE LOOPS.
C     * OCT 08/11 - M.LAZARE.   ALGDRY CHANGED BACK TO FORMULA
C     *                         USED IN GCM15I (.0056->.0046).
C     * SEP 27/11 - D.VERSEGHY. CONSTRAIN DELZW TO BE >= 5 CMS
C     *                         TO AVOID PROBLEMATIC UNDERSHOOTS.
C     * AUG 25/11 - D.VERSEGHY. USE THFC FORMULATION FOR BOTTOM
C     *                         LAYER AT BOTTOM OF SOIL PERMEABLE
C     *                         DEPTH.
C     * DEC 23/09 - V.FORTIN.   REVISE CALCULATION OF THFC FOR
C     *                         BOTTOM LAYER IN MINERAL SOILS 
C     *                         ACCORDING TO SOULIS ET AL. (2009).
C     * JAN 06/09 - D.VERSEGHY. REVERSE ORDER OF 200 AND 300 LOOPS.
C     * DEC 11/07 - D.VERSEGHY. CHANGE CALCULATION OF TCS FROM
C     *                         GEOMETRIC MEAN TO LINEAR MEAN.
C     * FEB 07/07 - D.VERSEGHY. SET THFC TO THLRET FOR ORGANIC SOILS;
C     *                         STREAMLINE SOME CALCULATIONS.
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 300 LOOP.
C     * APR 06/05 - D.VERSEGHY. MOVE CALCULATION OF GRKTLD
C     *                         INTO GRINFL; REVISED CALCULATION
C     *                         OF ALGDRY (WITH M.LAZARE).
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 04/03 - D.VERSEGHY. PERMEABLE THICKNESS OF SOIL
C     *                         LAYERS CONSTRAINED TO >= 1 MM;
C     *                         PROTECT SENSITIVE CALCULATIONS
C     *                         AGAINST ROUNDOFF ERROR.
C     * JUN 28/02 - D.VERSEGHY. ASSIGN SOIL HYDROLOGICAL AND
C     *                         THERMAL PROPERTIES BASED ON
C     *                         SAND, CLAY AND ORGANIC MATTER
C     *                         CONTENT.
C
      USE FLAGS
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NL,NM,IL1,IL2,IM,IG,ICTEMMOD,I,J,M
C
C     * OUTPUT ARRAYS.
C
      REAL THPOR (NL,NM,IG),  THLRET(NL,NM,IG),  THLMIN(NL,NM,IG),
     1     BI    (NL,NM,IG),  PSISAT(NL,NM,IG),  GRKSAT(NL,NM,IG),  
     2     THLRAT(NL,NM,IG),  HCPS  (NL,NM,IG),  
     3     TCS   (NL,NM,IG),  THFC  (NL,NM,IG),  PSIWLT(NL,NM,IG),
     4     DELZW (NL,NM,IG),  ZBOTW (NL,NM,IG),
     4     ALGWET(NL,NM),     ALGDRY(NL,NM)
C
      INTEGER                 ISAND (NL,NM,IG),  IGDR  (NL,NM)
C
C     * INPUT ARRAYS.
C
      REAL SAND  (NL,NM,IG),  CLAY  (NL,NM,IG),  ORGM  (NL,NM,IG),
     1     DELZ  (IG),        ZBOT  (IG),        SDEPTH(NL,NM) 
C
      REAL THPORG (3),      THRORG (3),      THMORG (3),
     1     BORG   (3),      PSISORG(3),      GRKSORG(3)
C
C     * TEMPORARY VARIABLES.
C
      REAL VSAND,VORG,VFINE,VTOT,AEXP,ABC,THSAND,THFINE,THORG

C     * VARIABLES FOR SOIL.INI FILE.
      REAL WC_THPOR (NL,NM,IG),WC_THLRET(NL,NM,IG),
     1     WC_THLMIN(NL,NM,IG),WC_BI    (NL,NM,IG),
     2     WC_PSISAT(NL,NM,IG),WC_GRKSAT(NL,NM,IG),
     3     WC_HCPS  (NL,NM,IG),WC_TCS   (NL,NM,IG)	  
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL TCW,TCICE,TCSAND,TCFINE,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPFIN,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP 
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCFINE,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPFIN,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS5/ THPORG,THRORG,THMORG,BORG,PSISORG,GRKSORG
C---------------------------------------------------------------------
C
      DO 50 M=1,IM
      DO 50 I=IL1,IL2
          IGDR(I,M)=1
50    CONTINUE
C
      DO 100 J=1,IG
      DO 100 M=1,IM
      DO 100 I=IL1,IL2
          ISAND (I,M,J)=NINT(SAND(I,M,J))                                               
          IF(ISAND(I,M,J).GT.-3) IGDR(I,M)=J
100   CONTINUE
C
      DO 200 M=1,IM
      DO 200 I=IL1,IL2
          DO 150 J=1,IG
              IF(ISAND(I,M,1).EQ.-4) THEN
                  DELZW(I,M,J)=DELZ(J)
                  ISAND(I,M,J)=-4
              ELSEIF(ISAND(I,M,J).EQ.-3) THEN
                  DELZW(I,M,J)=0.0
              ELSEIF(SDEPTH(I,M).GE.ZBOT(J)) THEN
                  DELZW(I,M,J)=DELZ(J)
              ELSEIF(SDEPTH(I,M).LT.(ZBOT(J)-DELZ(J)+0.025)) THEN
                  DELZW(I,M,J)=0.0
                  ISAND(I,M,J)=-3
              ELSE
                  DELZW(I,M,J)=MAX(0.05,(SDEPTH(I,M)-(ZBOT(J)-DELZ(J))))
              ENDIF
              ZBOTW(I,M,J)=MAX(0.0,ZBOT(J)-DELZ(J))+DELZW(I,M,J)
150       CONTINUE
          IF(SAND(I,M,1).GE.0.0) THEN
              ALGWET(I,M)=0.08+0.0006*SAND(I,M,1)
              IF(ICTEMMOD.EQ.0) THEN
                  ALGDRY(I,M)=MIN(0.14+0.0046*SAND(I,M,1), 0.45) ! FOR GLC2000
              ELSE
                  ALGDRY(I,M)=MIN(0.14+0.0027*SAND(I,M,1), 0.41) ! FOR CTEM   
              ENDIF
          ELSE
              ALGWET(I,M)=0.0
              ALGDRY(I,M)=0.0
          ENDIF
200   CONTINUE
C
      DO 300 J=1,IG
      DO 300 M=1,IM
      DO 300 I=IL1,IL2
          IF(ISAND(I,M,J).EQ.-4) THEN
              THPOR (I,M,J)=0.0
              THLRET(I,M,J)=0.0
              THLMIN(I,M,J)=0.0
              BI    (I,M,J)=0.0
              PSISAT(I,M,J)=0.0
              GRKSAT(I,M,J)=0.0
              THLRAT(I,M,J)=0.0
              HCPS(I,M,J)=HCPICE
              TCS(I,M,J)=TCICE
              THFC(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
          ELSEIF(ISAND(I,M,J).EQ.-3) THEN
              THPOR (I,M,J)=0.0
              THLRET(I,M,J)=0.0
              THLMIN(I,M,J)=0.0
              BI    (I,M,J)=0.0
              PSISAT(I,M,J)=0.0
              GRKSAT(I,M,J)=0.0
              THLRAT(I,M,J)=0.0
              HCPS(I,M,J)=HCPSND
              TCS(I,M,J)=TCSAND
              THFC(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
          ELSEIF(ISAND(I,M,J).EQ.-2) THEN
              THPOR (I,M,J)=THPORG(MIN(J,3))
              THLRET(I,M,J)=THRORG(MIN(J,3))
              THLMIN(I,M,J)=THMORG(MIN(J,3))
              BI    (I,M,J)=BORG(MIN(J,3))
              PSISAT(I,M,J)=PSISORG(MIN(J,3))
              GRKSAT(I,M,J)=GRKSORG(MIN(J,3))
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              HCPS(I,M,J)=HCPOM
              TCS(I,M,J)=TCOM
              THFC(I,M,J)=THLRET(I,M,J)
              PSIWLT(I,M,J)=PSISAT(I,M,J)*(THLMIN(I,M,J)/
     1            THPOR(I,M,J))**(-BI(I,M,J))
          ELSEIF(SAND(I,M,J).GE.0.0) THEN
            IF (SOILINIFLAG == 5) THEN
              THPOR (I,M,J) = WC_THPOR (I,M,J)
              THLRET(I,M,J) = WC_THLRET(I,M,J)
              THLMIN(I,M,J) = WC_THLMIN(I,M,J)
              BI    (I,M,J) = WC_BI    (I,M,J)
              PSISAT(I,M,J) = WC_PSISAT(I,M,J)
              GRKSAT(I,M,J) = WC_GRKSAT(I,M,J)
              HCPS  (I,M,J) = WC_HCPS(I,M,J)
              TCS   (I,M,J) = WC_TCS(I,M,J)
            ELSE
              THPOR (I,M,J)=(-0.126*SAND(I,M,J)+48.9)/100.0
              THLRET(I,M,J)=0.04
              THLMIN(I,M,J)=0.04
              BI    (I,M,J)=0.159*CLAY(I,M,J)+2.91
              PSISAT(I,M,J)=0.01*EXP(-0.0302*SAND(I,M,J)+4.33)
              GRKSAT(I,M,J)=7.0556E-6*(EXP(0.0352*SAND(I,M,J)-2.035))
              VSAND=SAND(I,M,J)/(RHOSOL*100.0)
              VORG=ORGM(I,M,J)/(RHOOM*100.0)
              VFINE=(100.0-SAND(I,M,J)-ORGM(I,M,J))/(RHOSOL*100.0)
              VTOT=VSAND+VFINE+VORG
              THSAND=(1.0-THPOR(I,M,J))*VSAND/VTOT
              THORG=(1.0-THPOR(I,M,J))*VORG/VTOT
              THFINE=1.0-THPOR(I,M,J)-THSAND-THORG
              HCPS(I,M,J)=(HCPSND*THSAND+HCPFIN*THFINE+
     1            HCPOM*THORG)/(1.0-THPOR(I,M,J))
              TCS(I,M,J)=(TCSAND*THSAND+TCOM*THORG+
     1            TCFINE*THFINE)/(1.0-THPOR(I,M,J))
            ENDIF
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              IF(J.NE.IGDR(I,M))                       THEN
                  THFC(I,M,J)=THPOR(I,M,J)*(1.157E-9/GRKSAT(I,M,J))**
     1                (1.0/(2.0*BI(I,M,J)+3.0))
              ELSE
                  AEXP=(BI(I,M,J)-1.0)/BI(I,M,J)
                  ABC=(3.0*BI(I,M,J)+2.0)**AEXP-
     1                (2.0*BI(I,M,J)+2.0)**AEXP
                  THFC(I,M,J)=(ABC*THPOR(I,M,J)/(BI(I,M,J)-1.0))*
     1                (PSISAT(I,M,J)*BI(I,M,J)/SDEPTH(I,M))**
     2                (1.0/BI(I,M,J))
              ENDIF
              PSIWLT(I,M,J)=PSISAT(I,M,J)*(MAX(0.5*THFC(I,M,J),
     1            THLMIN(I,M,J))/THPOR(I,M,J))**(-BI(I,M,J))
          ENDIF
300   CONTINUE
C
      RETURN
      END
