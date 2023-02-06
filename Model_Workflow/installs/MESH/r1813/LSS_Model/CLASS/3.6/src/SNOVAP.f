      SUBROUTINE SNOVAP(RHOSNO,ZSNOW,HCPSNO,TSNOW,EVAP,QFN,QFG,HTCS,
     1                  WLOST,TRUNOF,RUNOFF,TOVRFL,OVRFLW,
     2                  FI,R,S,RHOSNI,WSNOW,ILG,IL1,IL2,JL)
C
C     * AUG 25/11 - D.VERSEGHY. CORRECT CALCULATION OF TRUNOF
C     *                         AND TOVRFL.
C     * FEB 22/07 - D.VERSEGHY. NEW ACCURACY LIMITS FOR R AND S.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * APR 11/01 - M.LAZARE.   CHECK FOR EXISTENCE OF SNOW BEFORE
C     *                         PERFORMING CALCULATIONS.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 16/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         INCORPORATE DIAGNOSTIC ARRAY "WLOST". 
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         ADDITIONAL DIAGNOSTIC CALCULATION -
C     *                         UPDATE HTCS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. SUBLIMATION FROM SNOWPACK.
C                                          
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL RHOSNO(ILG),   ZSNOW (ILG),   HCPSNO(ILG),   TSNOW (ILG), 
     1     EVAP  (ILG),   QFN   (ILG),   QFG   (ILG),   HTCS  (ILG),
     2     WLOST (ILG),   TRUNOF(ILG),   RUNOFF(ILG),   TOVRFL(ILG),
     3     OVRFLW(ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),   R     (ILG),   S     (ILG),   RHOSNI(ILG),
     1     WSNOW (ILG)   
C
C     * TEMPORARY VARIABLES.
C
      REAL ZADD,ZLOST,ZREM
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
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. (S(I).LT.1.0E-11 .OR. R(I).LT.1.0E-11)
     1                .AND. ZSNOW(I).GT.0.)                       THEN
              HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              IF(EVAP(I).LT.0.)                             THEN 
                  ZADD=-EVAP(I)*DELT*RHOW/RHOSNI(I)
                  RHOSNO(I)=(ZSNOW(I)*RHOSNO(I)+ZADD*RHOSNI(I))/
     1                      (ZSNOW(I)+ZADD)                          
                  ZSNOW (I)=ZSNOW(I)+ZADD                                                        
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  EVAP  (I)=0.0                                                                
              ELSE                                                                        
                  ZLOST=EVAP(I)*DELT*RHOW/RHOSNO(I)
                  IF(ZLOST.LE.ZSNOW(I))                     THEN 
                      ZSNOW(I)=ZSNOW(I)-ZLOST                                                   
                      EVAP (I)=0.0                                                            
                      HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                  ELSE                                                                    
                      ZREM=(ZLOST-ZSNOW(I))*RHOSNO(I)/RHOW
                      ZSNOW(I)=0.0                                                           
                      HCPSNO(I)=0.0
                      EVAP(I)=ZREM*(CLHMLT+CLHVAP)/(CLHVAP*DELT)
                      WLOST(I)=WLOST(I)-ZREM*RHOW*CLHMLT/CLHVAP
                      IF(RUNOFF(I).GT.0. .OR. WSNOW(I).GT.0.)
     1                 TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TSNOW(I)+TFREZ)*
     1                      WSNOW(I)/RHOW)/(RUNOFF(I)+WSNOW(I)/RHOW)
                      RUNOFF(I)=RUNOFF(I)+WSNOW(I)/RHOW
                      IF(OVRFLW(I).GT.0. .OR. WSNOW(I).GT.0.)
     1                 TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TSNOW(I)+TFREZ)*
     1                      FI(I)*WSNOW(I)/RHOW)/(OVRFLW(I)+FI(I)*
     2                      WSNOW(I)/RHOW)
                      OVRFLW(I)=OVRFLW(I)+FI(I)*WSNOW(I)/RHOW
                      TSNOW(I)=0.0 
                      WSNOW(I)=0.0
                      QFN(I)=QFN(I)-FI(I)*ZREM*RHOW/DELT
                      QFG(I)=QFG(I)+FI(I)*EVAP(I)*RHOW
                  ENDIF                                                                   
              ENDIF 
              HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF                                                                      
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END        
