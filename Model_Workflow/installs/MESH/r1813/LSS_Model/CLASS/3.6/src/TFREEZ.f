      SUBROUTINE TFREEZ(ZPOND,TPOND,ZSNOW,TSNOW,ALBSNO,RHOSNO,HCPSNO,
     1                  GZERO,HMFG,HTCS,HTC,WTRS,WTRG,FI,QFREZ,
     2                  WSNOW,TA,TBAR,ISAND,IG,ILG,IL1,IL2,JL)
C
C     * JAN 06/09 - D.VERSEGHY. SET QFREZ TO ZERO AFTER CALCULATION
C     *                         OF HADD.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 20/02 - D.VERSEGHY. COSMETIC CHANGES TO SUBROUTINE CALL;
C     *                         SHORTENED CLASS4 COMMON BLOCK.
C     * MAY 24/02 - D.VERSEGHY. PASS IN ENTIRE SOIL TEMPERATURE
C     *                         ARRAY.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * AUG 16/95 - D.VERSEGHY. TWO NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. FREEZING OF PONDED WATER.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL ZPOND (ILG),    TPOND (ILG),    ZSNOW (ILG),    TSNOW (ILG),
     1     ALBSNO(ILG),    RHOSNO(ILG),    HCPSNO(ILG),    GZERO (ILG),
     2     HTCS  (ILG),    WTRS  (ILG),    WTRG  (ILG)
C
      REAL HMFG  (ILG,IG), HTC   (ILG,IG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    QFREZ (ILG),    WSNOW (ILG),    TA    (ILG),    
     1     TBAR  (ILG,IG)
C
      INTEGER              ISAND (ILG,IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL ZFREZ,HADD,HCOOL,HCONV,TTEST,TLIM,HEXCES
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
          IF(FI(I).GT.0. .AND. ZPOND(I).GT.0. .AND. (TPOND(I).LT.0. 
     1                     .OR. QFREZ(I).LT.0.))           THEN
             HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
             ZFREZ=0.0
             HADD=-QFREZ(I)*DELT                                                            
             QFREZ(I)=0.0
             IF(TPOND(I).LT.0.)            THEN                                                       
                 HADD=HADD-TPOND(I)*HCPW*ZPOND(I)
                 TPOND(I)=0.0              
             ENDIF                                                                       
             HCOOL=TPOND(I)*HCPW*ZPOND(I)                                                      
             HCONV=HCOOL+CLHMLT*RHOW*ZPOND(I)                                               
             HTC (I,1)=HTC (I,1)-FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
             IF(HADD.LE.HCOOL)             THEN                                                      
                TPOND(I)=TPOND(I)-HADD/(HCPW*ZPOND(I))                                           
                HTC(I,1)=HTC(I,1)+FI(I)*HADD/DELT
             ELSE IF(HADD.LE.HCONV)        THEN                                                  
                HADD=HADD-HCOOL                                                         
                ZFREZ=HADD/(CLHMLT*RHOW)                                                
                ZPOND(I)=ZPOND(I)-ZFREZ                                                       
                HTC(I,1)=HTC(I,1)+FI(I)*HCOOL/DELT
                ZFREZ=ZFREZ*RHOW/RHOICE                                                 
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                TSNOW(I)=TSNOW(I)*HCPSNO(I)*ZSNOW(I)/(HCPSNO(I)*ZSNOW(I)
     1                   +HCPICE*ZFREZ)                    
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)
     1                   +ZFREZ)                        
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                   (RHOW*ZSNOW(I))
                TPOND(I)=0.0                                                               
             ELSE                                                                        
                HADD=HADD-HCONV                                                         
                ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                HTC(I,1)=HTC(I,1)+FI(I)*HCOOL/DELT
                TTEST=-HADD/(HCPICE*ZFREZ)                                              
                IF(ZSNOW(I).GT.0.0) THEN
                    TLIM=MIN(TSNOW(I),TBAR(I,1))
                ELSE
                    TLIM=MIN(TA(I)-TFREZ,TBAR(I,1))
                ENDIF
                TLIM=MIN(TLIM,0.0)
                IF(TTEST.LT.TLIM)       THEN                                    
                   HEXCES=HADD+TLIM*HCPICE*ZFREZ                         
                   GZERO(I)=GZERO(I)-HEXCES/DELT                                             
                   HTC(I,1)=HTC(I,1)+FI(I)*(HADD-HEXCES)/DELT
                   TSNOW(I)=(TSNOW(I)*HCPSNO(I)*ZSNOW(I)+
     1                      TLIM*HCPICE*ZFREZ)          
     2                      /(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)                                    
                ELSE                                                                    
                   TSNOW(I)=(TSNOW(I)*HCPSNO(I)*ZSNOW(I)+TTEST*HCPICE*
     1                       ZFREZ)/(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)                                    
                   HTC(I,1)=HTC(I,1)+FI(I)*HADD/DELT
                ENDIF                                                                   
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)+
     1                    ZFREZ)                        
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                ZPOND(I)=0.0                                                               
                TPOND(I)=0.0                                                               
             ENDIF                                                                       
             HTC (I,1)=HTC (I,1)+FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
             HMFG(I,1)=HMFG(I,1)-FI(I)*CLHMLT*RHOICE*ZFREZ/DELT
             WTRS(I)=WTRS(I)+FI(I)*ZFREZ*RHOICE/DELT
             WTRG(I)=WTRG(I)-FI(I)*ZFREZ*RHOICE/DELT
             HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
          IF(FI(I).GT.0. .AND.ISAND(I,1).GT.-4)                    THEN
             HTC (I,1)=HTC (I,1)-FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
          ENDIF
  100 CONTINUE
C                                                                           
      RETURN                                                                      
      END        
