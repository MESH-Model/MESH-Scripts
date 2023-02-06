      SUBROUTINE TWCALC(TBAR,THLIQ,THICE,HCP,TBARW,HMFG,HTC,
     1                  FI,EVAP,THPOR,THLMIN,HCPS,DELZW,
     2                  DELZZ,ISAND,IG,ILG,IL1,IL2,JL)
C
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * MAY 16/03 - Y.DELAGE/D.VERSEGHY. BUGFIX IN FREEZING/
C     *                                  THAWING CALCULATIONS
C     *                                  (PRESENT SINCE V.2.7)
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. COSMETIC REARRANGEMENT OF 
C     *                         SUBROUTINE CALL.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT. 
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U - 
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ADJUST SOIL LAYER TEMPERATURES
C     *                         AND LIQUID/FROZEN MOISTURE CONTENTS
C     *                         FOR FREEZING/THAWING.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I,J
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), THLIQ (ILG,IG), THICE (ILG,IG), 
     1     HCP   (ILG,IG), TBARW (ILG,IG), HMFG  (ILG,IG),
     2     HTC   (ILG,IG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    EVAP  (ILG)
C
      REAL THPOR (ILG,IG), THLMIN(ILG,IG), HCPS  (ILG,IG), 
     1     DELZW (ILG,IG), DELZZ (ILG,IG)
C
      INTEGER              ISAND (ILG,IG)   
C
C     * TEMPORARY VARIABLES.
C
      REAL THFREZ,THEVAP,HADD,THMELT
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
      DO 100 J=1,IG                                                               
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. DELZW(I,J).GT.0. .AND. ISAND(I,1).GT.-4)
     1                                                              THEN
              HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                   HCPS(I,J)*(1.-THPOR(I,J))                                                   
              HTC  (I,J)=HTC(I,J)-FI(I)*(HCP(I,J)*DELZW(I,J)+
     1                   HCPSND*(DELZZ(I,J)-DELZW(I,J)))*
     2                   (TBAR(I,J)+TFREZ)/DELT
              IF(TBAR(I,J).LT.0. .AND. THLIQ(I,J).GT.THLMIN(I,J)) THEN                        
                  THFREZ=-(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOW*
     2                    DELZW(I,J))                              
                  IF(J.EQ.1)                               THEN 
                      THEVAP=EVAP(I)*DELT/DELZW(I,J)                                          
                  ELSE                                                                
                      THEVAP=0.0                                                      
                  ENDIF                                                               
                  IF((THLIQ(I,J)-THLMIN(I,J)-THEVAP).GT.0.0)      THEN 
                    IF(THFREZ.LE.(THLIQ(I,J)-THLMIN(I,J)-THEVAP)) THEN                         
                      HMFG(I,J)=HMFG(I,J)-FI(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FI(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      THLIQ(I,J)=THLIQ(I,J)-THFREZ                                        
                      THICE(I,J)=THICE(I,J)+THFREZ*RHOW/RHOICE                            
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                    ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)-FI(I)*(THLIQ(I,J)-
     1                    THLMIN(I,J)-THEVAP)*CLHMLT*RHOW*DELZW(I,J)/
     2                    DELT
                      HTC(I,J)=HTC(I,J)-FI(I)*(THLIQ(I,J)-THLMIN(I,J)-
     1                          THEVAP)*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HADD=(THFREZ-(THLIQ(I,J)-THLMIN(I,J)-THEVAP))*
     1                     CLHMLT*RHOW*DELZW(I,J)
                      THICE(I,J)=THICE(I,J)+(THLIQ(I,J)-THLMIN(I,J)-
     1                           THEVAP)*RHOW/RHOICE          
                      THLIQ(I,J)=THLMIN(I,J)+THEVAP                                          
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J)) 
                      TBAR (I,J)=-HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                    ENDIF                                                               
                  ENDIF                                                               
              ENDIF
C                                                                   
              IF(TBAR(I,J).GT.0. .AND. THICE(I,J).GT.0.)        THEN                           
                  THMELT=(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                   DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOICE*
     2                   DELZW(I,J))                             
                  IF(THMELT.LE.THICE(I,J))                 THEN 
                      HMFG(I,J)=HMFG(I,J)+FI(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FI(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      THICE(I,J)=THICE(I,J)-THMELT                                        
                      THLIQ(I,J)=THLIQ(I,J)+THMELT*RHOICE/RHOW                            
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J)) 
                      TBAR (I,J)=0.0                                                   
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)+FI(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FI(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HADD=(THMELT-THICE(I,J))*CLHMLT*RHOICE*
     1                     DELZW(I,J)
                      THLIQ(I,J)=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                          
                      THICE(I,J)=0.0                                                    
                      HCP  (I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
              HTC  (I,J)=HTC(I,J)+FI(I)*(HCP(I,J)*DELZW(I,J)+
     1                   HCPSND*(DELZZ(I,J)-DELZW(I,J)))*
     2                   (TBAR(I,J)+TFREZ)/DELT
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*
     1                 (HCPW*THLIQ(I,J)+HCPICE*THICE(I,J))*
     2                 DELZW(I,J)/DELT
          ENDIF                                                      
          TBARW(I,J)=TBAR(I,J)
  100 CONTINUE                                                                    
C                                                                                  
      RETURN                                                                      
      END 
