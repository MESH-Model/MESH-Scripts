      SUBROUTINE SNOADD(ALBSNO,TSNOW,RHOSNO,ZSNOW,HCPSNO,HTCS,
     1                  FI,S,TS,RHOSNI,WSNOW,ILG,IL1,IL2,JL)
C
C     * NOV 17/11 - M.LAZARE.   CHANGE SNOW ALBEDO REFRESHMENT 
C     *                         THRESHOLD (SNOWFALL IN CURRENT
C     *                         TIMESTEP) FROM 0.005 TO 1.E-4 M. 
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 10/04 - R.HARVEY/D.VERSEGHY. INCREASE SNOW ALBEDO 
C     *                         REFRESHMENT THRESHOLD; ADD
C     *                         "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ACCUMULATION OF SNOW ON GROUND.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL ALBSNO(ILG),   TSNOW (ILG),   RHOSNO(ILG),   ZSNOW (ILG),
     1     HCPSNO(ILG),   HTCS  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),   S     (ILG),   TS    (ILG),   RHOSNI(ILG),
     1     WSNOW (ILG)

C     * TEMPORARY VARIABLES.
C
      REAL SNOFAL,HCPSNP
C                                                                                 
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1     SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,
     2     CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. S(I).GT.0.)                         THEN
              HTCS  (I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                  ZSNOW(I)/DELT
              SNOFAL=S(I)*DELT                      
              IF(SNOFAL.GE.1.E-4)                               THEN 
                  ALBSNO(I)=0.84                                                             
              ELSE IF(.NOT.(ZSNOW(I).GT.0.))                THEN
                  ALBSNO(I)=0.50                                                         
              ENDIF                                                                   
              HCPSNP=HCPICE*RHOSNI(I)/RHOICE
              TSNOW (I)=((TSNOW(I)+TFREZ)*ZSNOW(I)*HCPSNO(I) +
     1                   (TS   (I)+TFREZ)*SNOFAL  *HCPSNP)/
     2                  (ZSNOW(I)*HCPSNO(I) + SNOFAL*HCPSNP) -
     3                   TFREZ
              RHOSNO(I)=(ZSNOW(I)*RHOSNO(I) + SNOFAL*RHOSNI(I))/
     1                  (ZSNOW(I)+SNOFAL)                          
              ZSNOW (I)=ZSNOW(I)+SNOFAL                                                          
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                  (RHOW*ZSNOW(I))
              HTCS  (I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                  ZSNOW(I)/DELT
          ENDIF                                                 
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END    
