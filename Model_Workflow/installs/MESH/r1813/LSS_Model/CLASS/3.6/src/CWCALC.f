      SUBROUTINE CWCALC(TCAN,RAICAN,SNOCAN,FRAINC,FSNOWC,CHCAP,
     1                  HMFC,HTCC,FI,CMASS,ILG,IL1,IL2,JL) 
C                                                                 
C     * MAR 25/08 - D.VERSEGHY. UPDATE FRAINC AND FSNOWC.
C     * SEP 24/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 20/02 - D.VERSEGHY. COSMETIC REARRANGEMENT OF
C     *                         SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * MAR 17/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 13/91 - D.VERSEGHY. ADJUST CANOPY TEMPERATURE AND
C     *                         INTERCEPTED LIQUID/FROZEN 
C     *                         MOISTURE STORES FOR FREEZING/
C     *                         THAWING.
C
      IMPLICIT NONE
C                                       
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * I/O ARRAYS.
C
      REAL TCAN  (ILG),    RAICAN(ILG),    SNOCAN(ILG),
     1     FRAINC(ILG),    FSNOWC(ILG),    CHCAP (ILG),    
     2     HMFC  (ILG),    HTCC  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    CMASS (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL HFREZ,HCONV,RCONV,HCOOL,HMELT,SCONV,HWARM
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
C---------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                        THEN
              HTCC  (I)=HTCC(I)-FI(I)*TCAN(I)*CHCAP(I)/DELT
              IF(RAICAN(I).GT.0. .AND. TCAN(I).LT.TFREZ)      THEN                                    
                  HFREZ=CHCAP(I)*(TFREZ-TCAN(I))                                                
                  HCONV=RAICAN(I)*CLHMLT                                                     
                  IF(HFREZ.LE.HCONV)                       THEN 
                     RCONV=HFREZ/CLHMLT                                                  
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)*RCONV/RAICAN(I)
                     FRAINC(I)=FRAINC(I)-FRAINC(I)*RCONV/RAICAN(I)
                     SNOCAN(I)=SNOCAN(I)+RCONV                                                 
                     RAICAN(I)=RAICAN(I)-RCONV                                                 
                     TCAN  (I)=TFREZ                                                          
                     HMFC  (I)=HMFC(I)-FI(I)*CLHMLT*RCONV/DELT
                     HTCC  (I)=HTCC(I)-FI(I)*CLHMLT*RCONV/DELT
                  ELSE                                                                    
                     HCOOL=HFREZ-HCONV                                                   
                     SNOCAN(I)=SNOCAN(I)+RAICAN(I)                                                
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)
                     FRAINC(I)=0.0
                     TCAN  (I)=-HCOOL/(SPHVEG*CMASS(I)+SPHICE*
     1                         SNOCAN(I))+TFREZ  
                     HMFC  (I)=HMFC(I)-FI(I)*CLHMLT*RAICAN(I)/DELT
                     HTCC  (I)=HTCC(I)-FI(I)*CLHMLT*RAICAN(I)/DELT
                     RAICAN(I)=0.0                                                          
                  ENDIF                                                                   
              ENDIF                                                                       
              IF(SNOCAN(I).GT.0. .AND. TCAN(I).GT.TFREZ)        THEN 
                  HMELT=CHCAP(I)*(TCAN(I)-TFREZ)                                                
                  HCONV=SNOCAN(I)*CLHMLT                                                     
                  IF(HMELT.LE.HCONV)                       THEN 
                     SCONV=HMELT/CLHMLT                                                  
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)*SCONV/SNOCAN(I)
                     FSNOWC(I)=FSNOWC(I)-FSNOWC(I)*SCONV/SNOCAN(I)
                     SNOCAN(I)=SNOCAN(I)-SCONV                                                 
                     RAICAN(I)=RAICAN(I)+SCONV                                                 
                     TCAN(I)=TFREZ                                                          
                     HMFC  (I)=HMFC(I)+FI(I)*CLHMLT*SCONV/DELT
                     HTCC  (I)=HTCC(I)+FI(I)*CLHMLT*SCONV/DELT
                  ELSE                                                                    
                     HWARM=HMELT-HCONV                                                   
                     RAICAN(I)=RAICAN(I)+SNOCAN(I)                                                
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)
                     FSNOWC(I)=0.0
                     TCAN(I)=HWARM/(SPHVEG*CMASS(I)+SPHW*RAICAN(I))+
     1                       TFREZ                         
                     HMFC  (I)=HMFC(I)+FI(I)*CLHMLT*SNOCAN(I)/DELT
                     HTCC  (I)=HTCC(I)+FI(I)*CLHMLT*SNOCAN(I)/DELT
                     SNOCAN(I)=0.0                                                          
                  ENDIF                                                                   
              ENDIF             
              CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)+SPHICE*SNOCAN(I)
              HTCC (I)=HTCC(I)+FI(I)*TCAN(I)*CHCAP(I)/DELT
          ENDIF                                
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END 
