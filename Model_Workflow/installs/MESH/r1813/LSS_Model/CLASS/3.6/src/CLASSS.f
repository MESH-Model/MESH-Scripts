      SUBROUTINE CLASSS(TBARROW,THLQROW,THICROW,GFLXROW,TSFSROW,
     1                  TPNDROW,ZPNDROW,TBASROW,ALBSROW,TSNOROW, 
     2                  RHOSROW,SNOROW, TCANROW,RCANROW,SCANROW, 
     3                  GROROW, CMAIROW,TACROW, QACROW, WSNOROW,
     4                  ILMOS,JLMOS,IWMOS,JWMOS,
     5                  NL,NM,ILG,IL1,IL2,IG,IC,ICP1,
     6                  TBARGAT,THLQGAT,THICGAT,GFLXGAT,TSFSGAT,
     7                  TPNDGAT,ZPNDGAT,TBASGAT,ALBSGAT,TSNOGAT,
     8                  RHOSGAT,SNOGAT, TCANGAT,RCANGAT,SCANGAT,
     9                  GROGAT, CMAIGAT,TACGAT, QACGAT, WSNOGAT,
     A                  MANNROW,MANNGAT,DDROW,DDGAT,
     B                  SANDROW,SANDGAT,CLAYROW,CLAYGAT,XSLPROW,XSLPGAT,
     C                  DrySnowRow,SnowAgeROW,DrySnowGAT,SnowAgeGAT,
     D                  TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT,
     E                  DriftROW, SublROW, DepositionROW,
     F                  DriftGAT, SublGAT, DepositionGAT)
C
C     * OCT 25/11 - M.LAZARE.   REMOVE OPERATIONS ON INTERNAL
C     *                         ROT ARRAYS (NOW DONE DIRECTLY
C     *                         GAT->ROW IN SFCPROC).
C     * OCT 07/11 - M.LAZARE.   REMOVE TSF.
C     * OCT 05/11 - M.LAZARE.   ADD SFCH.
C     * OCT 04/11 - M.LAZARE.   REMOVE ITCT.
C     * MAR 23/06 - D.VERSEGHY. ADD WSNO,FSNO.
C     * MAR 18/05 - D.VERSEGHY. ADDITIONAL VARIABLES.
C     * FEB 18/05 - D.VERSEGHY. ADD "TSFS" VARIABLES.
C     * AUG 05/04 - D.VERSEGHY. ADD NEW DIAGNOSTIC VARIABLES
C     *                         ILMO, UE AND HBL.
C     * AUG 15/02 - D.VERSEGHY. SCATTER OPERATION ON CLASS 
C     *                         VARIABLES.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NL,NM,ILG,IL1,IL2,IG,IC,ICP1,K,L,M
C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C
      REAL    TBARROW(NL,NM,IG), THLQROW(NL,NM,IG), THICROW(NL,NM,IG), 
     1        GFLXROW(NL,NM,IG)

      REAL    TSFSROW(NL,NM,4)

      REAL    TPNDROW(NL,NM),    ZPNDROW(NL,NM),    TBASROW(NL,NM),   
     1        ALBSROW(NL,NM),    TSNOROW(NL,NM),    RHOSROW(NL,NM),   
     2        SNOROW (NL,NM),    TCANROW(NL,NM),    RCANROW(NL,NM),   
     3        SCANROW(NL,NM),    GROROW (NL,NM),    TACROW (NL,NM),  
     4        QACROW (NL,NM),    WSNOROW(NL,NM),    CMAIROW(NL,NM) 
C
      REAL    TBARGAT(ILG,IG),   THLQGAT(ILG,IG),   THICGAT(ILG,IG), 
     1        GFLXGAT(ILG,IG)

      REAL    TSFSGAT(ILG,4)

      REAL    TPNDGAT(ILG),      ZPNDGAT(ILG),      TBASGAT(ILG),   
     1        ALBSGAT(ILG),      TSNOGAT(ILG),      RHOSGAT(ILG),   
     2        SNOGAT (ILG),      TCANGAT(ILG),      RCANGAT(ILG),   
     3        SCANGAT(ILG),      GROGAT (ILG),      TACGAT (ILG),  
     4        QACGAT (ILG),      WSNOGAT(ILG),      CMAIGAT(ILG)

C     * SAND AND CLAY
      REAL    SANDROW(NL,NM,IG), CLAYROW(NL,NM,IG)
      REAL    SANDGAT(ILG,IG),   CLAYGAT(ILG,IG)
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      INTEGER  ILMOS (ILG),  JLMOS  (ILG),  IWMOS  (ILG),  JWMOS (ILG)
C
C     * LAND SURFACE DIAGNOSTIC VARIABLES.
C
      REAL  DrySnowRow(NL,NM),SnowAgeROW(NL,NM),
     1      TSNOdsROW(NL,NM),  RHOSdsROW(NL,NM),
     2     DriftROW(NL,NM), SublROW(NL,NM), DepositionROW(NL,NM),
     3      DrySnowGAT(ILG), SnowAgeGAT(ILG),
     4      TSNOdsGAT (ILG), RHOSdsGAT(ILG),
     5      DriftGAT  (ILG), SublGAT(ILG), DepositionGAT(ILG)

C * WATROF DECLARATIONS
      REAL  DDROW(NL,NM),MANNROW(NL,NM),DDGAT(ILG),MANNGAT(ILG)
      REAL  XSLPROW(NL,NM),XSLPGAT(ILG)
C
C----------------------------------------------------------------------
!$omp parallel do
      DO 100 K=IL1,IL2
          TPNDROW(ILMOS(K),JLMOS(K))=TPNDGAT(K)  
          ZPNDROW(ILMOS(K),JLMOS(K))=ZPNDGAT(K)  
          TBASROW(ILMOS(K),JLMOS(K))=TBASGAT(K)  
          ALBSROW(ILMOS(K),JLMOS(K))=ALBSGAT(K)  
          TSNOROW(ILMOS(K),JLMOS(K))=TSNOGAT(K)  
          RHOSROW(ILMOS(K),JLMOS(K))=RHOSGAT(K)  
          SNOROW (ILMOS(K),JLMOS(K))=SNOGAT (K)  
          WSNOROW(ILMOS(K),JLMOS(K))=WSNOGAT(K)  
          TCANROW(ILMOS(K),JLMOS(K))=TCANGAT(K)  
          RCANROW(ILMOS(K),JLMOS(K))=RCANGAT(K)  
          SCANROW(ILMOS(K),JLMOS(K))=SCANGAT(K)  
          GROROW (ILMOS(K),JLMOS(K))=GROGAT (K)  
          TACROW (ILMOS(K),JLMOS(K))=TACGAT (K)  
          QACROW (ILMOS(K),JLMOS(K))=QACGAT (K)  
          CMAIROW(ILMOS(K),JLMOS(K))=CMAIGAT(K)  
          DDROW (ILMOS(K),JLMOS(K))=DDGAT(K)  
          MANNROW (ILMOS(K),JLMOS(K))=MANNGAT(K)
          XSLPROW (ILMOS(K),JLMOS(K))=XSLPGAT(K)
          DrySnowRow(ILMOS(K),JLMOS(K))=DrySnowGAT(K)
          SnowAgeROW(ILMOS(K),JLMOS(K))=SnowAgeGAT(K)
          TSNOdsROW(ILMOS(K),JLMOS(K))=TSNOdsGAT(K)
          RHOSdsROW(ILMOS(K),JLMOS(K))=RHOSdsGAT(K)
          DriftROW(ILMOS(K),JLMOS(K))=DriftGAT(K)
          SublROW(ILMOS(K),JLMOS(K))=SublGAT(K)
          DepositionROW(ILMOS(K),JLMOS(K))=DepositionGAT(K)
  100 CONTINUE
C
      DO 200 L=1,IG
      !$omp parallel do
      DO 200 K=IL1,IL2
          TBARROW(ILMOS(K),JLMOS(K),L)=TBARGAT(K,L)
          THLQROW(ILMOS(K),JLMOS(K),L)=THLQGAT(K,L)
          THICROW(ILMOS(K),JLMOS(K),L)=THICGAT(K,L)
          GFLXROW(ILMOS(K),JLMOS(K),L)=GFLXGAT(K,L)
          SANDROW(ILMOS(K),JLMOS(K),L)=SANDGAT(K,L)
          CLAYROW(ILMOS(K),JLMOS(K),L)=CLAYGAT(K,L)
  200 CONTINUE
C
      DO 300 L=1,4
      !$omp parallel do
      DO 300 K=IL1,IL2
          TSFSROW(ILMOS(K),JLMOS(K),L)=TSFSGAT(K,L)
  300 CONTINUE
C
      RETURN
      END
