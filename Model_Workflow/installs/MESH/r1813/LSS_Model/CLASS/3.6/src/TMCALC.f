      SUBROUTINE TMCALC(TBAR,THLIQ,THICE,HCP,TPOND,ZPOND,TSNOW,ZSNOW,
     1                  ALBSNO,RHOSNO,HCPSNO,TBASE,OVRFLW,TOVRFL,
     2                  RUNOFF,TRUNOF,HMFG,HTC,HTCS,WTRS,WTRG,
     3                  FI,TBARW,GZERO,G12,G23,GGEO,TA,WSNOW,
     4                  TCTOP,TCBOT,GFLUX,ZPLIM,THPOR,THLMIN,HCPS,
     5                  DELZW,DELZZ,DELZ,ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
C
C     * FEB 22/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS.
C     * NOV 20/06 - D.VERSEGHY. ADD GEOTHERMAL HEAT FLUX.
C     * APR 03/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW,
C     * DEC 07/05 - D.VERSEGHY. REVISED HEAT FLUX CALCULATION BETWEEN
C     *                         SOIL AND TBASE IN LAYER 3.
C     * OCT 06/05 - D.VERSEGHY. MODIFY FOR CASES WHERE IG>3.
C     * MAR 30/05 - D.VERSEGHY. ADD RUNOFF TEMPERATURE CALCULATION.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * MAY 16/03 - Y.DELAGE/D.VERSEGHY. BUGFIX IN FREEZING/
C     *                                  THAWING CALCULATIONS
C     *                                  (PRESENT SINCE V.2.7)
C     * JUN 17/02 - D.VERSEGHY. REMOVE INCORPORATION OF PONDED WATER
C     *                         INTO FIRST LAYER SOIL MOISTURE;
C     *                         UPDATE SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. PASS IN SWITCH TO DO SURFACE FLOW
C     *                         CALCULATION ONLY IF WATFLOOD ROUTINES
C     *                         ARE NOT BEING RUN.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION
C     *                         OF OVERLAND FLOW.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * AUG 16/95 - D.VERSEGHY. TWO NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATIONS OF TBAR AND HTC;
C     *                         ALLOW SPECIFICATION OF LIMITING POND 
C     *                         DEPTH "PNDLIM" (PARALLEL CHANGES
C     *                         MADE SIMULTANEOUSLY IN CLASSW).
C     * NOV 01/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISED VERSION WITH IN-LINED CODE
C     *                         FROM TWCALC AND TFREEZ TO PERMIT 
C     *                         FREEZING AND THAWING OF SOIL LAYERS
C     *                         AT THE END OF EACH TIME STEP.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. STORE PONDED WATER INTO FIRST 
C     *                         SOIL LAYER LIQUID WATER; STEP
C     *                         AHEAD SOIL LAYER TEMPERATURES 
C     *                         USING CONDUCTION HEAT FLUX
C     *                         CALCULATED AT TOP AND BOTTOM
C     *                         OF EACH LAYER.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I,J,N
      INTEGER IWF(ILG)
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TBAR  (ILG,IG),   THLIQ (ILG,IG),   THICE (ILG,IG),
     1     HCP   (ILG,IG),   HMFG  (ILG,IG),   HTC   (ILG,IG)
C
      REAL TPOND (ILG),      ZPOND (ILG),      TSNOW (ILG),
     1     ZSNOW (ILG),      ALBSNO(ILG),      RHOSNO(ILG),       
     2     HCPSNO(ILG),      TBASE (ILG),      OVRFLW(ILG),
     3     TOVRFL(ILG),      RUNOFF(ILG),      TRUNOF(ILG),
     4     HTCS  (ILG),      WTRS  (ILG),      WTRG  (ILG)     
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),      TBARW (ILG,IG),   GZERO (ILG),      
     1     G12   (ILG),      G23   (ILG),      GGEO  (ILG),
     2     TA    (ILG),      WSNOW (ILG), 
     3     TCTOP (ILG,IG),   TCBOT (ILG,IG),   ZPLIM (ILG)
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG),   THLMIN(ILG,IG),   HCPS  (ILG,IG),   
     1     DELZW (ILG,IG),   DELZZ (ILG,IG),   DELZ  (IG)
C
      INTEGER                ISAND (ILG,IG)   
C
C     * TEMPORARY VARIABLES.
C
      REAL GFLUX (ILG,IG)
C
      REAL GP1,ZFREZ,HADD,HCONV,TTEST,TLIM,HEXCES,THFREZ,THMELT,G3B
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
C
C     * CALCULATE SUBSURFACE AND OVERLAND RUNOFF TERMS; ADJUST
C     * SURFACE PONDING DEPTH.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. IWF(I).EQ.0 .AND.
     1                         (ZPOND(I)-ZPLIM(I)).GT.1.0E-8)     THEN
              TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TPOND(I)+TFREZ)*
     1                  (ZPOND(I)-ZPLIM(I)))/(RUNOFF(I)+
     2                  (ZPOND(I)-ZPLIM(I)))
              RUNOFF(I)=RUNOFF(I)+(ZPOND(I)-ZPLIM(I))
              TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TPOND(I)+TFREZ)*
     1                  FI(I)*(ZPOND(I)-ZPLIM(I)))/(OVRFLW(I)+
     2                  FI(I)*(ZPOND(I)-ZPLIM(I)))
              OVRFLW(I)=OVRFLW(I)+FI(I)*(ZPOND(I)-ZPLIM(I))
              ZPOND(I)=MIN(ZPOND(I),ZPLIM(I))
          ENDIF
  100 CONTINUE
C
C     * UPDATE SOIL TEMPERATURES AFTER GROUND WATER MOVEMENT.
C
      DO 200 J=1,IG
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. DELZW(I,J).GT.0. .AND. ISAND(I,1).GT.-4)
     1                                                            THEN
              HTC(I,J)=HTC(I,J)+FI(I)*((TBARW(I,J)+TFREZ)*
     1                 HCPW*THLIQ(I,J)+(TBAR(I,J)+TFREZ)*
     2                 HCPICE*THICE(I,J))*DELZW(I,J)/DELT
              HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                 HCPS(I,J)*(1.-THPOR(I,J))                                                   
              TBAR(I,J)=((TBARW(I,J)+TFREZ)*HCPW*THLIQ(I,J)*
     1                  DELZW(I,J)+(TBAR(I,J)+TFREZ)*((HCPICE*
     2                  THICE(I,J)+HCPS(I,J)*(1.-THPOR(I,J)))*
     3                  DELZW(I,J)+HCPSND*(DELZZ(I,J)-DELZW(I,J))))/
     4                  (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     5                  DELZW(I,J)))-TFREZ
          ENDIF
  200 CONTINUE
C
C     * STEP AHEAD POND TEMPERATURE; CHECK FOR FREEZING, AND ADD
C     * FROZEN WATER TO SNOW PACK.
C
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. ZPOND(I).GT.0.0)
     1                                                             THEN
              HTC(I,1)=HTC(I,1)+FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1            ZPOND(I)/DELT
              GP1=ZPOND(I)*(G12(I)-GZERO(I))/(ZPOND(I)+DELZZ(I,1))+
     1            GZERO(I)
              TPOND(I) =TPOND(I)+(GZERO(I)-GP1)*DELT/(HCPW*ZPOND(I))
              GZERO(I)=GP1
          ENDIF
  300 CONTINUE
C
      DO 400 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. ZPOND(I).GT.0. 
     1                   .AND. TPOND(I).LT.0.)
     1                                                              THEN
             HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1               ZSNOW(I)/DELT
             ZFREZ=0.0
             HADD=-TPOND(I)*HCPW*ZPOND(I)                                              
             TPOND(I)=0.0                                                               
             HCONV=CLHMLT*RHOW*ZPOND(I)                                               
             HTC(I,1)=HTC(I,1)-FI(I)*HCPW*TFREZ*ZPOND(I)/DELT
             IF(HADD.LE.HCONV)             THEN                                                      
                ZFREZ=HADD/(CLHMLT*RHOW)                                                
                ZPOND(I)=ZPOND(I)-ZFREZ                                                       
                ZFREZ=ZFREZ*RHOW/RHOICE                                                 
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                TSNOW(I)=TSNOW(I)*HCPSNO(I)*ZSNOW(I)/(HCPSNO(I)*ZSNOW(I)
     1                  +HCPICE*ZFREZ)                    
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)
     1                   +ZFREZ)                        
                IF(ZSNOW(I).GT.0.0) THEN
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                   (RHOW*ZSNOW(I))
                ELSE
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                ENDIF
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
             ELSE                                                                        
                HADD=HADD-HCONV                                                         
                ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                TTEST=-HADD/(HCPICE*ZFREZ)                                              
                IF(ZSNOW(I).GT.0.0) THEN
                    TLIM=MIN(TSNOW(I),TBAR(I,1))
                ELSE
                    TLIM=MIN(TA(I)-TFREZ,TBAR(I,1))
                ENDIF
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
     1                     ZFREZ)                        
                IF(ZSNOW(I).GT.0.0) THEN
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                   (RHOW*ZSNOW(I))
                ELSE
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                ENDIF
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                ZPOND(I)=0.0                                                               
             ENDIF                                                                       
             HTC (I,1)=HTC (I,1)+FI(I)*HCPW*TFREZ*ZPOND(I)/DELT
             HMFG(I,1)=HMFG(I,1)-FI(I)*CLHMLT*RHOICE*ZFREZ/DELT
             WTRS(I)=WTRS(I)+FI(I)*ZFREZ*RHOICE/DELT
             WTRG(I)=WTRG(I)-FI(I)*ZFREZ*RHOICE/DELT
             HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
  400 CONTINUE
C
C     * STEP AHEAD SOIL LAYER TEMPERATURES; CHECK FOR FREEZING OR
C     * THAWING.
C
      IF(IG.GT.3) THEN
      DO 500 J=4,IG
          DO 500 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4)            THEN
                  GFLUX(I,J)=(TCBOT(I,J-1)+TCTOP(I,J))*(TBAR(I,J-1)-
     1                       TBAR(I,J))/(DELZ(J-1)+DELZ(J))
              ENDIF
  500     CONTINUE
      ENDIF
C     
      DO 550 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4)               THEN
              TBAR(I,1)=TBAR(I,1)+(GZERO(I)-G12(I))*DELT/
     1                  (HCP(I,1)*DELZW(I,1)+HCPSND*(DELZZ(I,1)-
     2                  DELZW(I,1)))
              TBAR(I,2)=TBAR(I,2)+(G12  (I)-G23(I))*DELT/
     1                  (HCP(I,2)*DELZW(I,2)+HCPSND*(DELZZ(I,2)-
     2                  DELZW(I,2)))                         
              IF(IG.EQ.3)                                THEN
                  IF(DELZZ(I,3).GT.0.0)       THEN
                      IF(DELZZ(I,IG).LT.(DELZ(IG)-1.0E-5)) THEN
                          G3B=(TCBOT(I,3)+TCTOP(I,3))*(TBAR(I,3)-
     1                        TBASE(I))/DELZ(3)
                          TBAR(I,3)=TBAR(I,3)+(G23(I)-G3B)*DELT/
     1                        (HCP(I,3)*DELZZ(I,3))
                          TBASE(I)=TBASE(I)+(G3B-GGEO(I))*DELT/
     2                        (HCPSND*(DELZ(3)-DELZZ(I,3)))   
                      ELSE
                          TBAR(I,3)=TBAR(I,3)+(G23(I)-GGEO(I))*DELT/
     1                        (HCP(I,3)*DELZW(I,3))
                      ENDIF
                  ELSE
                      TBASE(I)=TBASE(I)+(G23(I)-GGEO(I))*DELT/
     1                    (HCPSND*DELZ(3))
                  ENDIF
                  HTC(I,3)=HTC(I,3)-FI(I)*GGEO(I)
              ELSE
                  TBAR(I,3)=TBAR(I,3)+G23(I)*DELT/
     1                      (HCP(I,3)*DELZW(I,3)+HCPSND*(DELZ(3)-
     2                      DELZW(I,3)))                         
              ENDIF
              GFLUX(I,1)=GZERO(I)
              GFLUX(I,2)=G12(I)
              GFLUX(I,3)=G23(I)
          ENDIF
  550 CONTINUE
C
      DO 600 J=3,IG                                                               
      DO 600 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. IG.GT.3)   THEN
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
              IF(J.EQ.3)                                THEN
                  TBAR(I,J)=TBAR(I,J)-GFLUX(I,J+1)*DELT/
     1                      (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                      DELZW(I,J)))                         
              ELSEIF(J.EQ.IG)                           THEN
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GGEO(I))*DELT/
     1                      (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                      DELZW(I,J)))                         
              ELSE
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GFLUX(I,J+1))*DELT/
     1                  (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                  DELZW(I,J)))                         
              ENDIF
              HTC(I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
          ENDIF
  600 CONTINUE
C     
      DO 700 J=1,IG                                                               
      DO 700 I=IL1,IL2
          IF(FI(I).GT.0. .AND. DELZW(I,J).GT.0. .AND. ISAND(I,1).GT.-4)
     1                                                            THEN
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
              IF(TBAR(I,J).LT.0. .AND. THLIQ(I,J).GT.THLMIN(I,J))
     1                                                           THEN                        
                  THFREZ=-(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOW*
     2                    DELZW(I,J))                              
                  IF(THFREZ.LE.(THLIQ(I,J)-THLMIN(I,J))) THEN                         
                      HMFG(I,J)=HMFG(I,J)-FI(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FI(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      THLIQ(I,J)=THLIQ(I,J)-THFREZ                                        
                      THICE(I,J)=THICE(I,J)+THFREZ*RHOW/RHOICE                            
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)-FI(I)*(THLIQ(I,J)-
     1                    THLMIN(I,J))*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FI(I)*(THLIQ(I,J)-
     1                    THLMIN(I,J))*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HADD=(THFREZ-(THLIQ(I,J)-THLMIN(I,J)))*CLHMLT*
     1                     RHOW*DELZW(I,J)
                      THICE(I,J)=THICE(I,J)+(THLIQ(I,J)-
     1                           THLMIN(I,J))*RHOW/RHOICE          
                      THLIQ(I,J)=THLMIN(I,J)
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=-HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
C                                                                   
              IF(TBAR(I,J).GT.0. .AND. THICE(I,J).GT.0.)        THEN                           
                  THMELT=(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOICE*
     2                    DELZW(I,J))                             
                  IF(THMELT.LE.THICE(I,J))                 THEN 
                      HMFG(I,J)=HMFG(I,J)+FI(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FI(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      THICE(I,J)=THICE(I,J)-THMELT                                        
                      THLIQ(I,J)=THLIQ(I,J)+THMELT*RHOICE/RHOW                            
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)+FI(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FI(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HADD=(THMELT-THICE(I,J))*CLHMLT*RHOICE*
     1                          DELZW(I,J)
                      THLIQ(I,J)=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                          
                      THICE(I,J)=0.0                                                    
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
              HTC(I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
          ENDIF
  700 CONTINUE
C                                                                                  
      RETURN                                                                      
      END 
