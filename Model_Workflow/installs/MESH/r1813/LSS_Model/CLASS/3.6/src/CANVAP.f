      SUBROUTINE CANVAP(EVAP,SUBL,RAICAN,SNOCAN,TCAN,THLIQ,TBAR,ZSNOW,
     1                  WLOST,CHCAP,QFCF,QFCL,QFN,QFC,HTCC,HTCS,HTC,
     2                  FI,CMASS,TSNOW,HCPSNO,RHOSNO,FROOT,THPOR,
     3                  THLMIN,DELZW,EVLOST,RLOST,IROOT,
     4                  IG,ILG,IL1,IL2,JL,N   )
C                                                                                 
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3.
C     * SEP 13/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 20/02 - D.VERSEGHY. TIDY UP SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE SOIL 
C     *                         PERMEABLE DEPTH.
C     * DEC 30/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUGFIXES IN CALCULATION OF QFN AND 
C     *                         QFC.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE CALCULATION OF WLOST;
C     *                         REFINE CALCULATION OF QFCL.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3. 
C     *                         ADDITIONAL DIAGNOSTIC CALCULATIONS -
C     *                         HTCC AND HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C                                        NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATE ACTUAL EVAPORATION, 
C     *                         SUBLIMATION AND TRANSPIRATION FROM
C     *                         VEGETATION CANOPY.
C
      IMPLICIT NONE
C                                                                
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I,J,N
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL THLIQ (ILG,IG), TBAR  (ILG,IG), QFC   (ILG,IG),
     1     HTC   (ILG,IG)
C
      REAL EVAP  (ILG),    SUBL  (ILG),    RAICAN(ILG),    SNOCAN(ILG),
     1     TCAN  (ILG),    ZSNOW (ILG),    WLOST (ILG),    CHCAP (ILG),    
     2     QFCF  (ILG),    QFCL  (ILG),    QFN   (ILG),    
     3     HTCC  (ILG),    HTCS  (ILG)    
     3     
C
C     * INPUT ARRAYS.
C
      REAL FROOT (ILG,IG), THPOR(ILG,IG),  THLMIN(ILG,IG),
     1     DELZW (ILG,IG)
C
      REAL FI    (ILG),    CMASS (ILG),    TSNOW (ILG),    
     1     HCPSNO(ILG),    RHOSNO(ILG)
C
C     * WORK ARRAYS.
C
      REAL EVLOST(ILG),    RLOST (ILG)
C
      INTEGER              IROOT (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL SLOST,THTRAN,THLLIM
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
C     * INITIALIZE ARRAYS.
C     * (THE WORK ARRAY "IROOT" INDICATES POINTS WHERE TRANSPIRATION
C     * CAN OCCUR.)
C
      DO 50 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              RLOST (I)=0.0
              EVLOST(I)=0.0 
              IROOT (I)=0
              HTCC  (I)=HTCC(I)-FI(I)*TCAN(I)*CHCAP(I)/DELT
              HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF
50    CONTINUE
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              HTC (I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*THLIQ(I,J)*
     1            HCPW*DELZW(I,J)/DELT
              IF(FROOT(I,J).GT.1.0E-5) IROOT(I)=1
          ENDIF
  100 CONTINUE
C
C     * SUBLIMATION CASE.  IF SNOW ON CANOPY IS INSUFFICIENT TO SUPPLY
C     * DEMAND, RESIDUAL IS TAKEN FIRST FROM SNOW UNDERLYING CANOPY AND
C     * THEN FROM LIQUID WATER ON CANOPY.
C
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. SUBL(I).GT.0.)                      THEN 
              SLOST=SUBL(I)*DELT*RHOW                                                    
              IF(SLOST.LE.SNOCAN(I))                          THEN  
                  SNOCAN(I)=SNOCAN(I)-SLOST                                                 
                  SUBL(I)=0.0                                                            
              ELSE                                                                    
                  SLOST=SLOST-SNOCAN(I)                                                  
                  QFCF(I)=QFCF(I)-FI(I)*SLOST/DELT
                  SNOCAN(I)=0.0                                                          
                  IF(SLOST.LE.ZSNOW(I)*RHOSNO(I))           THEN                                      
                      ZSNOW(I)=ZSNOW(I)-SLOST/RHOSNO(I)                                        
                      SUBL(I)=0.0                                                        
                      QFN(I)=QFN(I)+FI(I)*SLOST/DELT
                  ELSE                                                                
                      SLOST=SLOST-ZSNOW(I)*RHOSNO(I)                                        
                      QFN(I)=QFN(I)+FI(I)*ZSNOW(I)*RHOSNO(I)/DELT
                      ZSNOW(I)=0.0                                                       
                      WLOST(I)=WLOST(I)-SLOST*CLHMLT/CLHVAP                                     
                      EVAP(I)=EVAP(I)+SLOST*(CLHMLT+CLHVAP)/
     1                        (CLHVAP*DELT*RHOW)              
                      QFCL(I)=QFCL(I)+FI(I)*SLOST*(CLHMLT+CLHVAP)/
     1                        (CLHVAP*DELT)
                  ENDIF                                                               
              ENDIF                                                                   
          ENDIF
  200 CONTINUE
C
C     * EVAPORATION.  IF WATER ON CANOPY IS INSUFFICIENT TO SUPPLY
C     * DEMAND, ASSIGN RESIDUAL TO TRANSPIRATION.
C
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0. .AND. EVAP(I).GT.0.)                      THEN
              RLOST(I)=EVAP(I)*RHOW*DELT
              IF(RLOST(I).LE.RAICAN(I))                         THEN 
                  RAICAN(I)=RAICAN(I)-RLOST(I)
                  EVAP  (I)=0.
                  RLOST (I)=0.
              ELSE                                                                    
                  RLOST(I)=RLOST(I)-RAICAN(I)                                                  
                  QFCL(I)=QFCL(I)-FI(I)*RLOST(I)/DELT
                  IF(IROOT(I).EQ.0) EVLOST(I)=RLOST(I)
                  EVAP  (I)=0. 
                  RAICAN(I)=0.
              ENDIF
          ENDIF
  300 CONTINUE
C
C     * TRANSPIRATION.
C
      DO 400 J=1,IG
      DO 400 I=IL1,IL2 
          IF(FI(I).GT.0. .AND. IROOT(I).GT.0)                     THEN
              IF(DELZW(I,J).GT.0.0) THEN
                  THTRAN=RLOST(I)*FROOT(I,J)/(RHOW*DELZW(I,J))                      
              ELSE
                  THTRAN=0.0
              ENDIF
              IF(THPOR(I,J).LT.THLMIN(I,J))           THEN
                  THLLIM=THPOR(I,J)
              ELSE
                  THLLIM=THLMIN(I,J)
              ENDIF
              IF(THTRAN.LE.(THLIQ(I,J)-THLLIM))                 THEN                        
                  QFC  (I,J)=QFC(I,J)+FI(I)*RLOST(I)*FROOT(I,J)/DELT
                  THLIQ(I,J)=THLIQ(I,J)-THTRAN                                
              ELSE                                                        
                  QFC  (I,J)=QFC(I,J)+FI(I)*(THLIQ(I,J)-THLLIM)*RHOW*
     1                       DELZW(I,J)/DELT
                  EVLOST (I)=EVLOST(I)+(THTRAN+THLLIM-THLIQ(I,J))*RHOW*            
     1                       DELZW(I,J)                                             
                  THLIQ(I,J)=THLLIM
              ENDIF                                                       
          ENDIF
  400 CONTINUE                                                        
C
C     * CLEANUP.
C
      DO 500 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              CHCAP(I)=RAICAN(I)*SPHW+SNOCAN(I)*SPHICE+CMASS(I)*SPHVEG
              WLOST(I)=WLOST(I)+EVLOST(I)  
              HTCC  (I)=HTCC(I)+FI(I)*TCAN(I)*CHCAP(I)/DELT
              HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF
  500 CONTINUE
C
      DO 550 J=1,IG
      DO 550 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              HTC (I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*THLIQ(I,J)*
     1            HCPW*DELZW(I,J)/DELT
          ENDIF
  550 CONTINUE
C                                                                        
      RETURN                                                                      
      END                                                                                 
