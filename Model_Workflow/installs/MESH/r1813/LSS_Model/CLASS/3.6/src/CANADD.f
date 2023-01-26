      SUBROUTINE CANADD(IWATER,R,TR,S,TS,RAICAN,SNOCAN,TCAN,CHCAP,
     1                  HTCC,ROFC,ROVG,PCPN,PCPG,FI,FSVF,
     2                  CWLCAP,CWFCAP,CMASS,RHOSNI,TSURX,RDRIP,SDRIP,
     3                  ILG,IL1,IL2,JL)
C                                                                                 
C     * NOV 22/06 - E.CHAN/D.VERSEGHY. UNCONDITIONALLY SET TR AND TS.
C     * JAN 05/05 - P.BARTLETT. CORRECT/REFINE SNOW INTERCEPTION
C     *                         CALCULATIONS.
C     * SEP 13/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 29/02 - D.VERSEGHY/S.FASSNACHT. NEW SNOW INTERCEPTION
C     *                                     ALGORITHM,
C     * JUL 24/02 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS FROM
C     *                         CLASSW INTO THIS ROUTINE; CHANGE
C     *                         RHOSNI FROM CONSTANT TO VARIABLE.
C     * JUN 20/02 - D.VERSEGHY. ADDITIONAL DIAGNOSTIC CALCULATIONS.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATE CANOPY INTERCEPTION; ADD
C     *                         THROUGHFALL AND CANOPY DRIP TO
C     *                         PRECIPITATION REACHING GROUND.
C     *                         ADJUST CANOPY TEMPERATURE AND HEAT
C     *                         CAPACITY.
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
     1     RAICAN(ILG),    SNOCAN(ILG),    TCAN  (ILG),    CHCAP (ILG),
     2     HTCC  (ILG),    ROFC  (ILG),    ROVG  (ILG),    PCPN  (ILG),
     3     PCPG  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    FSVF  (ILG),    CWLCAP(ILG),    CWFCAP(ILG),    
     1     CMASS (ILG),    RHOSNI(ILG),    TSURX (ILG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL RDRIP (ILG),    SDRIP (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL RTHRU,RINT,STHRU,SINT,TRCAN,TSCAN,RWXCES,SLOAD,SWXCES,
     1     SNUNLD,CHCAPI,TCANI
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
          RDRIP(I)=0.0
          SDRIP(I)=0.0
          IF(FI(I).GT.0. .AND. (R(I).GT.0. .OR. S(I).GT.0. .OR.
     1            RAICAN(I).GT.0. .OR. SNOCAN(I).GT.0.))           THEN
              RTHRU=R(I)*FSVF(I)                                                                
              RINT=(R(I)-RTHRU)*DELT*RHOW                                                    
              STHRU=S(I)*FSVF(I)                                                                
              SINT=(S(I)-STHRU)*DELT*RHOSNI(I)
              IF((RAICAN(I)+RINT).GT.0.)                 THEN
                  TRCAN=(RAICAN(I)*TCAN(I)+RINT*TR(I))/(RAICAN(I)+RINT)                               
              ELSE                                                                        
                  TRCAN=0.0                                                               
              ENDIF                                                                       
              IF((SNOCAN(I)+SINT).GT.0.)                 THEN 
                  TSCAN=(SNOCAN(I)*TCAN(I)+SINT*TS(I))/(SNOCAN(I)+SINT)                               
              ELSE                                                                        
                  TSCAN=0.0                                                               
              ENDIF                                                                       
C                                        
              RWXCES=RINT+RAICAN(I)-CWLCAP(I)
              IF(RWXCES.GT.0.)                           THEN 
                  RDRIP(I)=RWXCES/(DELT*RHOW)  
                  IF((RDRIP(I)+RTHRU).GT.0.)       THEN                                           
                      TR(I)=(RDRIP(I)*TRCAN+RTHRU*TR(I))/
     1                      (RDRIP(I)+RTHRU)                             
                  ELSE                                                                    
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
                  R(I)=RDRIP(I)+RTHRU                                                           
                  RAICAN(I)=CWLCAP(I)
              ELSE
                  R(I)=RTHRU                                                                 
                  RAICAN(I)=RAICAN(I)+RINT                                                      
              ENDIF
C
              SLOAD=(CWFCAP(I)-SNOCAN(I))*(1.0-EXP(-SINT/CWFCAP(I)))
              SWXCES=SINT-SLOAD
              SNUNLD=(SLOAD+SNOCAN(I))*(1.0-EXP(-1.157E-6*DELT))
              IF(SWXCES.GT.0. .OR. SNUNLD.GT.0.)                 THEN 
                  SDRIP(I)=(MAX(SWXCES,0.0)+SNUNLD)/(DELT*RHOSNI(I))
                  IF((SDRIP(I)+STHRU).GT.0.)       THEN                                           
                      TS(I)=(SDRIP(I)*TSCAN+STHRU*TS(I))/
     1                      (SDRIP(I)+STHRU)                             
                  ELSE                                                                    
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
                  S(I)=SDRIP(I)+STHRU                                                           
                  SNOCAN(I)=SNOCAN(I)+SINT-MAX(SWXCES,0.0)-SNUNLD
              ELSE                                                                        
                  S(I)=STHRU                                                                 
                  SNOCAN(I)=SNOCAN(I)+SINT                                                      
              ENDIF
C
              CHCAPI  =CHCAP(I)
              TCANI   =TCAN(I)
              CHCAP(I)=RAICAN(I)*SPHW+SNOCAN(I)*SPHICE+CMASS(I)*SPHVEG                                
              TCAN (I)=(RAICAN(I)*SPHW*TRCAN+SNOCAN(I)*SPHICE*TSCAN+
     1                 CMASS(I)*SPHVEG*TCAN(I))/CHCAP(I)
              HTCC (I)=HTCC(I)+FI(I)*(CHCAP(I)*TCAN(I)-CHCAPI*TCANI)/
     1                 DELT
              IF(R(I).GT.0.0)                      THEN
                  TR(I)=TR(I)-TFREZ                                                                 
              ELSE
                  TR(I)=MAX(TSURX(I)-TFREZ,0.0)
              ENDIF
              IF(S(I).GT.0.0)                      THEN
                  TS(I)=TS(I)-TFREZ                                                                 
              ELSE
                  TS(I)=MIN(TSURX(I)-TFREZ,0.0)
              ENDIF
              IF(IWATER.EQ.2) THEN
                  ROFC(I)=ROFC(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
                  PCPN(I)=PCPN(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
              ENDIF
              IF(IWATER.EQ.1) THEN
                  ROFC(I)=ROFC(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
                  ROVG(I)=ROVG(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
                  PCPN(I)=PCPN(I)+FI(I)*SDRIP(I)*RHOSNI(I)
                  PCPG(I)=PCPG(I)+FI(I)*RDRIP(I)*RHOW
              ENDIF
          ENDIF
  100 CONTINUE                                                                        
C
      RETURN                                                                      
      END
