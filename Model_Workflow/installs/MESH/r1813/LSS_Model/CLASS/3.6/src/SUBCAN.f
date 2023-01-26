      SUBROUTINE SUBCAN(IWATER,R,TR,S,TS,RHOSNI,EVAPG,QFN,QFG,
     1                  PCPN,PCPG,FI,ILG,IL1,IL2,JL)
C
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 21/04 - D.VERSEGHY. NEW LOWER LIMITS ON RADD AND SADD,
C     *                         CONSISTENT WITH WPREP.
C     * SEP 26/02 - D.VERSEGHY. BUGFIX IN CALCULATIONS OF QFN/QFG.
C     * JUL 24/02 - D.VERSEGHY. MODIFICAITONS TO ALLOW FOR 
C     *                         SIMULTANEOUS RAINFALL AND SNOWFALL;
C     *                         CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * JUN 20/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL.
C     * NOV 09/00 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS INTO
C     *                         WPREP.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         INCORPORATE DIAGNOSTICS.
C     * APR 21/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. PERFORM "WPREP" CALCULATIONS UNDER 
C     *                         CANOPY: LUMP DOWNWARD WATER VAPOUR 
C     *                         FLUXES TOGETHER WITH PRECIPITATION 
C     *                         REACHING GROUND.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IWATER,ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG),    TR    (ILG),    S     (ILG),    TS    (ILG),
     1     RHOSNI(ILG),    EVAPG (ILG),    QFN   (ILG),    QFG   (ILG),
     2     PCPN  (ILG),    PCPG  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL SADD,RADD
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
          IF(FI(I).GT.0. .AND. IWATER.EQ.2)                        THEN
              IF(S(I).GT.0. .OR. EVAPG(I).LT.0.)             THEN  
                  SADD=S(I)-EVAPG(I)*RHOW/RHOSNI(I)
                  IF(ABS(SADD).LT.1.0E-12) SADD=0.0
                  IF(SADD.GT.0.)                        THEN
                      S(I)=SADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-SADD*RHOSNI(I)/RHOW                                             
                      S(I)=0.0                                                               
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  S(I)=0.0                                                                   
                  TS(I)=0.0                                                                  
              ENDIF
C
              IF(R(I).GT.0. .OR. EVAPG(I).LT.0.)            THEN  
                  RADD=R(I)-EVAPG(I)                                                            
                  IF(ABS(RADD).LT.1.0E-12) RADD=0.0
                  IF(RADD.GT.0.)                     THEN 
                      R(I)=RADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-RADD                                                         
                      R(I)=0.0                                                               
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  R(I)=0.0                                                                   
                  TR(I)=0.0   
              ENDIF                                                               
          ENDIF                                                                       
  100 CONTINUE
C                                                                                  
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. IWATER.EQ.1)                        THEN
              IF(R(I).GT.0. .OR. EVAPG(I).LT.0.)            THEN  
                  RADD=R(I)-EVAPG(I)                                                            
                  IF(ABS(RADD).LT.1.0E-12) RADD=0.0
                  IF(RADD.GT.0.)                     THEN 
                      R(I)=RADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-RADD                                                         
                      R(I)=0.0                                                               
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  R(I)=0.0                                                                   
                  TR(I)=0.0   
              ENDIF                                                               
C
              IF(S(I).GT.0. .OR. EVAPG(I).LT.0.)             THEN  
                  SADD=S(I)-EVAPG(I)*RHOW/RHOSNI(I)
                  IF(ABS(SADD).LT.1.0E-12) SADD=0.0
                  IF(SADD.GT.0.)                        THEN
                      S(I)=SADD                                                              
                      QFN(I)=QFN(I)+FI(I)*EVAPG(I)*RHOW
                      QFG(I)=QFG(I)-FI(I)*EVAPG(I)*RHOW
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-SADD*RHOSNI(I)/RHOW                                             
                      PCPN(I)=PCPN(I)-FI(I)*S(I)*RHOSNI(I)
                      PCPG(I)=PCPG(I)+FI(I)*S(I)*RHOSNI(I)
                      S(I)=0.0                                                               
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  S(I)=0.0                                                                   
                  TS(I)=0.0                                                                  
              ENDIF
          ENDIF                                                                       
  200 CONTINUE
C                                                                                  
      RETURN                                                                      
      END       
