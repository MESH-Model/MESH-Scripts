      SUBROUTINE SNOALBW(ALBSNO,RHOSNO,ZSNOW,HCPSNO,TSNOW,
     1                   FI,S,RMELT,WSNOW,RHOMAX,ISAND,
     2                   ILG,IG,IL1,IL2,JL)       
C
C     * MAR 07/07 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 10/04 - R.HARVEY/D.VERSEGHY. INCREASE SNOW ALBEDO 
C     *                         REFRESHMENT AND WETTING THRESHOLDS.
C     * AUG 04/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE
C     *                         CALCULATIONS AGAIST ROUNDOFF 
C     *                         ERRORS.
C     * APR 21/04 - F.SEGLENIEKS/D.VERSEGHY. BUG FIX IN SNOW
C     *                         TEMPERATURE COMPARISONS.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * OCT 20/00 - R.BROWN/D.VERSEGHY. MODIFIED SNOW DENSITY
C     *                                 CALCULATIONS, ACCOUNTING
C     *                                 FOR SETTLING IN WARM AND
C     *                                 COLD SNOW.
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         SPECIFY LOCATION OF ICE SHEETS
C     *                         BY SOIL TEXTURE ARRAY RATHER
C     *                         THAN BY SOIL COLOUR INDEX.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * MAR 13/92 - M.LAZARE.   CLASS - VERSION 2.1.
C     *                         CODE FOR MODEL VERSION GCM7 -
C     *                         DIVIDE PREVIOUS SUBROUTINE 
C     *                         "SNOALB" INTO "SNOALBA" AND
C     *                         "SNOALBW" AND VECTORIZE.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. CALCULATE DECREASE IN SNOW ALBEDO
C     *                         AND INCREASE IN DENSITY DUE TO
C     *                         AGING. (ASSIGN DIFFERENT LOWER
C     *                         SNOW ALBEDO LIMITS FOR DRY AND
C     *                         MELTING SNOW.)
C                                                                                 
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IG,IL1,IL2,JL,I,IPTBAD
C                                                                                 
C     * OUTPUT ARRAYS.                                                            
C                                                                                 
      REAL ALBSNO(ILG),   RHOSNO(ILG),   ZSNOW (ILG),   HCPSNO(ILG)
C                                                                                 
C     * INPUT ARRAYS.                                                             
C                                                                                 
      REAL TSNOW (ILG),   FI    (ILG),   S     (ILG),   RMELT (ILG),
     1     WSNOW (ILG)
C 
      INTEGER             ISAND (ILG,IG)
C
C     * WORK ARRAY.                                                             
C                                                                                 
      REAL RHOMAX(ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL TIMFAC,RHOOLD
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
C----------------------------------------------------------------------               
      IPTBAD=0                                                                    
      DO 100 I=IL1,IL2  
          IF(ZSNOW(I).GT.0. .AND. 
     1            FI  (I).GT.0. .AND. S(I).LT.1.0E-6)             THEN
              IF(ALBSNO(I).GT.0.5001 .AND. (RMELT(I).GT.1.0E-7 .OR.
     1                TSNOW(I).GE.-0.01)) THEN
                  ALBSNO(I)=(ALBSNO(I)-0.50)*EXP(-0.01*DELT/3600.0)+
     1                0.50                    
              ELSE IF(ALBSNO(I).GT.0.7001 .AND. RMELT(I).LE.1.0E-7)
     1                                                            THEN
                  ALBSNO(I)=(ALBSNO(I)-0.70)*EXP(-0.01*DELT/3600.0)+
     1                0.70
              ENDIF                                                           
          ENDIF
C                                                       
          IF(FI(I).GT.0. .AND. ZSNOW(I).GT.0.0001)                THEN
              IF(TSNOW(I).LT.-0.01)                   THEN
                  RHOMAX(I)=450.0-(204.7/ZSNOW(I))*
     1                (1.0-EXP(-ZSNOW(I)/0.673))
              ELSE
                  RHOMAX(I)=700.0-(204.7/ZSNOW(I))*
     1                (1.0-EXP(-ZSNOW(I)/0.673))
              ENDIF
          ENDIF
C
          IF(FI(I).GT.0. .AND. ZSNOW(I).GT.0.0001 .AND. 
     1       RHOSNO(I).LT.(RHOMAX(I)-0.01))                       THEN
              RHOOLD=RHOSNO(I)                                                       
              RHOSNO(I)=(RHOSNO(I)-RHOMAX(I))*EXP(-0.01*DELT/3600.0)+
     1            RHOMAX(I)
              ZSNOW(I)=ZSNOW(I)*RHOOLD/RHOSNO(I) 
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1            (RHOW*ZSNOW(I))
          ENDIF
          IF((ALBSNO(I).LT.0.49 .OR. ALBSNO(I).GT.1.0) .AND. 
     1       ZSNOW (I).GT.0. .AND. FI(I).GT.0.)               IPTBAD=I
  100 CONTINUE                                                                    
C
      IF(IPTBAD.NE.0) THEN                                                        
         WRITE(6,6100) IPTBAD,JL,ALBSNO(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALBSNO = ',F10.5)            
         CALL XIT('SNOALBW',-1)                                                               
      ENDIF                                                                       
C                                                                                 
      RETURN                                                                      
      END        
