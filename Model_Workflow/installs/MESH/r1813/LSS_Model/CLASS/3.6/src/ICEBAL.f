      SUBROUTINE ICEBAL(TBAR,TPOND,ZPOND,TSNOW,RHOSNO,ZSNOW,HCPSNO,
     1                  ALBSNO,HMFG,HTCS,HTC,WTRS,WTRG,GFLUX,
     2                  RUNOFF,TRUNOF,OVRFLW,TOVRFL,ZPLIM,GGEO,
     3                  FI,EVAP,R,TR,GZERO,G12,G23,HCP,QMELT,WSNOW,
     +                  ICE,TICE,
     4                  ZMAT,TMOVE,WMOVE,ZRMDR,TADD,ZMOVE,TBOT,DELZ,
     +                  FREZTH, SWELIM, SNDENLIM,
     5                  ISAND,ICONT,IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
C
C     * OCT 30/20 - M.ELSHAMY   UPDATED THE SNOW TO ICE CONVERSION IN CASES
C                               OF EXCEEDING SWE LIMIT TO CONVERT EXCESS OVER LIMIT   	
C     * JUL 20/20 - D.PRINCZ.   MODIFIED THE CALCULATION OF HTC TO
C                               CONSIDER CHANGE IN ZPOND WHEN USING IWF
C                               (ICEBAL).
C     * JUN 10/20 - D.PRINCZ.   ADDED ICE AND TICE (ICEBAL).
C     * JUN 10/20 - D.PRINCZ.   CHANGED THRESHOLD AND LIMITS IN CHECKS
C                               IN LOOP 500.
C                               TO CONFIGURABLE VALUES.
C     * DEC 27/07 - D.VERSEGHY. ADD GEOTHERMAL HEAT FLUX; ADD ICE MASS
C     *                         LOSS TO RUNOFF.
C     * NOV 01/06 - D.VERSEGHY. ALLOW PONDING OF WATER ON ICE SHEETS.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * OCT 07/05 - D.VERSEGHY. MODIFY FOR CASES WHERE IG>3.
C     * MAR 30/05 - D.VERSEGHY. ADD RUNOFF TEMPERATURE CALCULATION;
C     *                         REMOVE UPDATE TO WTRG IN LOOP 300
C     *                         (BUGFIX).
C     * SEP 24/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 24/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. PASS IN SWITCH TO CALCULATE SURFACE FLOW
C     *                         ONLY IF WATFLOOD ROUTINES ARE NOT CALLED.
C     * NOV 16/98 - M.LAZARE.   "WTRG" UPDATED TO GAIN ICE MASS AS "WTRS"
C     *                         LOSES SNOW MASS IN SNOW->ICE CONVERSION
C     *                         (TWO PLACES).
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY 
C     *                         BETWEEN SOIL LAYERS IN MAIN CODE.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. STEP AHEAD "SOIL" LAYER TEMPERATURES
C     *                         OVER CONTINENTAL ICE SHEETS; ASSIGN
C     *                         PONDED WATER TO RUNOFF; ADJUST LAYER
C     *                         DEPTHS FOR ACCUMULATION/ABLATION.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,K,N
      INTEGER IWF(ILG)
C
C     * INPUT/OUTPUT FIELDS.
C                                                                                 
      REAL TBAR  (ILG,IG), HMFG  (ILG,IG), HTC   (ILG,IG),
     1     GFLUX (ILG,IG)
C
      REAL TPOND (ILG),    ZPOND (ILG),    TSNOW (ILG),    RHOSNO(ILG),    
     1     ZSNOW (ILG),    HCPSNO(ILG),    ALBSNO(ILG),    HTCS  (ILG),    
     2     WTRS  (ILG),    WTRG  (ILG),    RUNOFF(ILG),    TRUNOF(ILG),
     3     OVRFLW(ILG),    TOVRFL(ILG),    ICE   (ILG),    TICE  (ILG)
C
C     * INPUT FIELDS.
C
      REAL FI    (ILG),    EVAP  (ILG),    R     (ILG),    TR    (ILG), 
     1     GZERO (ILG),    G12   (ILG),    G23   (ILG),    QMELT (ILG),
     2     WSNOW (ILG),    ZPLIM (ILG),    GGEO  (ILG)
C
      REAL HCP   (ILG,IG)
C
      INTEGER              ISAND (ILG,IG)
C
      REAL DELZ  (IG)
C
C     * THRESHOLDS AND LIMITS FOR ICEBAL.
C           FREZTH=-2.0
C           SWELIM=100.
C           SNDENLIM=900.
C
      REAL, INTENT(IN) :: FREZTH(ILG), SWELIM(ILG), SNDENLIM(ILG)
C
C     * WORK FIELDS.
C
      REAL ZMAT  (ILG,IGP2,IGP1),          TMOVE (ILG,IGP2),
     1     WMOVE (ILG,IGP2),               ZRMDR (ILG,IGP1)
C
      REAL TADD  (ILG),    ZMOVE (ILG),    TBOT  (ILG) 
C
      INTEGER              ICONT (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL TZERO,RADD,QADD,ZMELT,GP1,HCOOL,HWARM,HFREZ,ZFREZ,SNOCONV
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
C     * ADD RAINFALL OR SNOWMELT TO PONDED WATER AND ASSIGN EXCESS
C     * TO RUNOFF.  CHECK FOR POND FREEZING.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                THEN
!              IF(R(I).GT.0.)                                THEN 
!                 RADD=R(I)*DELT                                                             
!                 TPOND(I)=((TPOND(I)+TFREZ)*ZPOND(I)+(TR(I)+TFREZ)*
!     1               RADD)/(ZPOND(I)+RADD)-TFREZ
!                 ZPOND(I)=ZPOND(I)+RADD                                                        
!                 HTC (I,1)=HTC(I,1)+FI(I)*(TR(I)+TFREZ)*HCPW*
!     1                     RADD/DELT
!              ENDIF                                                                       
              IF(IWF(I).EQ.0 .AND. (ZPOND(I)-ZPLIM(I)).GT.1.0E-8) THEN
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TPOND(I)+TFREZ)*
     1                   (ZPOND(I)-ZPLIM(I)))/(RUNOFF(I)+ZPOND(I)-
     2                   ZPLIM(I))
                  RUNOFF(I)=RUNOFF(I)+ZPOND(I)-ZPLIM(I)
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TPOND(I)+TFREZ)*
     1                   FI(I)*(ZPOND(I)-ZPLIM(I)))/(OVRFLW(I)+
     2                   FI(I)*(ZPOND(I)-ZPLIM(I)))
                  OVRFLW(I)=OVRFLW(I)+FI(I)*(ZPOND(I)-ZPLIM(I)) 
!                  HTC(I,1)=HTC(I,1)-FI(I)*(TPOND(I)+TFREZ)*HCPW*
!     1                   (ZPOND(I)-ZPLIM(I))/DELT
                  ZPOND(I)=MIN(ZPOND(I),ZPLIM(I))
              ENDIF
              IF(ZPOND(I).GT.0.0) THEN
                  HTC(I,1)=HTC(I,1)+FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                   ZPOND(I)/DELT
              ENDIF
              IF(TPOND(I).GT.0.001)                           THEN
                  HCOOL=TPOND(I)*HCPW*ZPOND(I)
                  HWARM=-TBAR(I,1)*HCPICE*DELZ(1)
                  IF(HWARM.GT.HCOOL)                     THEN
                      TBAR(I,1)=TBAR(I,1)+HCOOL/(HCPICE*DELZ(1))
                      TPOND(I)=0.0
                  ELSE
                      TBAR(I,1)=0.0
                      TPOND(I)=0.0
                      QMELT(I)=QMELT(I)+(HCOOL-HWARM)/DELT
                  ENDIF
              ENDIF
          ENDIF
  100 CONTINUE
C
      DO 125 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                THEN
              IF(TBAR(I,1).LT.FREZTH(I) .AND. ZPOND(I).GT.1.0E-8) THEN
                  HFREZ=ZPOND(I)*RHOW*CLHMLT
                  HWARM=-TBAR(I,1)*HCPICE*DELZ(1)
                  IF(HWARM.GE.HFREZ) THEN
                      TBAR(I,1)=TBAR(I,1)+HFREZ/(HCPICE*DELZ(1))
                      HMFG(I,1)=HMFG(I,1)-FI(I)*HFREZ/DELT
                      HTC(I,1)=HTC(I,1)-FI(I)*HCPW*TFREZ*ZPOND(I)/DELT
                      ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                      ZPOND(I)=0.0
                      TPOND(I)=0.0
                      HTCS(I)=HTCS(I)+FI(I)*HCPICE*TFREZ*ZFREZ/DELT
                      IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                      TSNOW(I)=((TSNOW(I)+TFREZ)*HCPSNO(I)*ZSNOW(I)+
     1                     TFREZ*HCPICE*ZFREZ)          
     2                     /(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)-TFREZ
                      RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/
     1                    (ZSNOW(I)+ZFREZ)                        
                      ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                      HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                      WTRS(I)=WTRS(I)+FI(I)*ZFREZ*RHOICE/DELT
                      WTRG(I)=WTRG(I)-FI(I)*ZFREZ*RHOICE/DELT
                  ENDIF                                                                       
              ENDIF
          ENDIF
  125 CONTINUE
C
C     * STEP AHEAD ICE LAYER TEMPERATURES.
C
      IF(IG.GT.3) THEN
      DO 150 J=4,IG
          DO 150 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)            THEN
                  GFLUX(I,J)=2.0*TCGLAC*(TBAR(I,J-1)-TBAR(I,J))/
     1                       (DELZ(J-1)+DELZ(J))
              ENDIF
  150     CONTINUE
      ENDIF
C     
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              IF(IG.EQ.3) THEN
                  TZERO=TBAR(I,1)+DELZ(1)*(GZERO(I)+0.5*G12(I))/
     1                (3.0*TCGLAC)                        
                  TBOT(I)=TZERO-(G23(I)*(DELZ(3)+DELZ(2))+G12(I)*
     1                    (DELZ(1)+DELZ(2))+GZERO(I)*DELZ(1))/
     2                    (2.0*TCGLAC)
                  TBAR(I,3)=TBAR(I,3)-GGEO(I)*DELT/
     1                    (HCP(I,3)*DELZ(3))                           
              ELSE
                  TBOT(I)=TBAR(I,IG)
              ENDIF
              TBAR(I,1)=TBAR(I,1)+(GZERO(I)-G12(I))*DELT/
     1                   (HCP(I,1)*DELZ(1))                       
              TBAR(I,2)=TBAR(I,2)+(G12(I)-G23(I))*DELT/
     1                   (HCP(I,2)*DELZ(2))                         
              TBAR(I,3)=TBAR(I,3)+G23(I)*DELT/
     1                   (HCP(I,3)*DELZ(3))                           
              GFLUX(I,1)=GZERO(I)
              GFLUX(I,2)=G12(I)
              GFLUX(I,3)=G23(I)
          ENDIF
  200 CONTINUE
C
      IF(IG.GT.3)                                                  THEN
      DO 250 J=3,IG                                                               
      DO 250 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                 THEN
              HTC (I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                  DELZ(J)/DELT
              IF(J.EQ.3)                                THEN
                  TBAR(I,J)=TBAR(I,J)-GFLUX(I,J+1)*DELT/
     1                      (HCP(I,J)*DELZ(J))
              ELSEIF(J.EQ.IG)                           THEN
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GGEO(I))*DELT/
     1                      (HCP(I,J)*DELZ(J))
              ELSE
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GFLUX(I,J+1))*DELT/
     1                      (HCP(I,J)*DELZ(J))
              ENDIF
              HTC (I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                  DELZ(J)/DELT
          ENDIF
  250 CONTINUE
      ENDIF
C
C     * IF LAYER TEMPERATURES OVERSHOOT ZERO, ADD EXCESS HEAT TO
C     * HEAT OF MELTING.
C
      DO 300 J=1,IG
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              IF(TBAR(I,J).GT.0.)                            THEN  
                  QADD=TBAR(I,J)*HCPICE*DELZ(J)/DELT  
                  QMELT(I)=QMELT(I)+QADD
                  HTC(I,J)=HTC(I,J)-FI(I)*QADD
                  HTC(I,1)=HTC(I,1)+FI(I)*QADD
                  TBAR(I,J)=0.0                                                       
              ENDIF
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                 DELZ(J)/DELT
          ENDIF
  300 CONTINUE
C
C     * APPLY CALCULATED HEAT OF MELTING TO UPPER ICE LAYER; ADD MELTED
C     * WATER TO TOTAL RUNOFF; CALCULATE DEPTH OF ICE REMOVED BY MELTING
C     * AND SUBLIMATION; RECALCULATE ICE LAYER TEMPERATURES.
C     
      DO 325 J=1,IG-1
      DO 325 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              TMOVE(I,J)=TBAR(I,J+1)
          ENDIF
  325 CONTINUE                                                                
C
      DO 350 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN                         
              IF(QMELT(I).GT.0. .OR. EVAP(I).GT.0.)           THEN                                        
                  TMOVE(I,IG)=TBOT(I)                                                           
                  ZMELT=QMELT(I)*DELT/((0.0-TBAR(I,1))*HCPICE+
     1                  CLHMLT*RHOICE)                 
                  IF(ZMELT.GT.0.)                   THEN
                      TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TFREZ*ZMELT*
     1                   RHOICE/RHOW)/(RUNOFF(I)+ZMELT*RHOICE/RHOW)
                      TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TFREZ*FI(I)*ZMELT*
     1                   RHOICE/RHOW)/(OVRFLW(I)+FI(I)*ZMELT*RHOICE/
     1                   RHOW)
                  ENDIF
                  RUNOFF(I)=RUNOFF(I)+ZMELT*RHOICE/RHOW                                         
                  OVRFLW(I)=OVRFLW(I)+FI(I)*ZMELT*RHOICE/RHOW                                         
                  HMFG(I,1)=HMFG(I,1)+FI(I)*CLHMLT*RHOICE*ZMELT/DELT
                  HTC (I,1)=HTC(I,1)-FI(I)*(QMELT(I)-CLHMLT*RHOICE*
     1                     ZMELT/DELT)
                  ZMOVE (I)=ZMELT+EVAP(I)*DELT*RHOW/RHOICE
                  WTRG  (I)=WTRG(I)+FI(I)*ZMOVE(I)*RHOICE/DELT
              ENDIF
          ENDIF                                                                       
  350 CONTINUE                                       
C
      DO 400 J=1,IG
      DO 400 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       (QMELT(I).GT.0. .OR. EVAP(I).GT.0.))                   THEN 
              TBAR(I,J)=(TBAR(I,J)*(DELZ(J)-ZMOVE(I))+TMOVE(I,J)*
     1                   ZMOVE(I))/DELZ(J)
          ENDIF                                                                       
  400 CONTINUE                                                                
C
C     * IF SNOW PACK EXCEEDS 100 KG M-2 OR SNOW DENSITY EXCEEDS 
C     * 900 KG M-3, CONVERT EXCESS TO ICE AND MOVE THE LOCATIONS
C     * OF THE ICE LAYERS ACCORDINGLY.
C
      DO 500 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              ICONT(I)=0
              SNOCONV=0.
              HTCS(I)=HTCS(I)-FI(I)*(TSNOW(I)+TFREZ)*HCPSNO(I)*
     1                ZSNOW(I)/DELT
              IF((RHOSNO(I)*ZSNOW(I)).GT.SWELIM(I)) THEN
                  SNOCONV=RHOSNO(I)*ZSNOW(I)-SWELIM(I)
                  WMOVE(I,1)=SNOCONV/RHOICE                                
                  TMOVE(I,1)=TSNOW(I)                                                      
                  WTRS(I)=WTRS(I)-FI(I)*WMOVE(I,1)*RHOICE/DELT
                  WTRG(I)=WTRG(I)+FI(I)*WMOVE(I,1)*RHOICE/DELT
                  ZSNOW(I)=ZSNOW(I)-WMOVE(I,1)                                                
                  RHOSNO(I)=SWELIM(I)/ZSNOW(I)
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  ICONT(I)=1
              ELSE IF(RHOSNO(I).GE.SNDENLIM(I)) THEN
                  SNOCONV=ZSNOW(I)*RHOSNO(I)
                  WMOVE(I,1)=SNOCONV/RHOICE                                        
                  TMOVE(I,1)=TSNOW(I)                                                      
                  WTRS(I)=WTRS(I)-FI(I)*(SNOCONV+WSNOW(I))/DELT
                  WTRG(I)=WTRG(I)+FI(I)*(SNOCONV+WSNOW(I))/DELT
                  ZSNOW(I)=0.0                                                           
                  RHOSNO(I)=0.0                                                          
                  HCPSNO(I)=0.0                                                          
                  IF(WSNOW(I).GT.0.0)                    THEN
                      TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TSNOW(I)+TFREZ)*
     1                      WSNOW(I)/RHOW)/(RUNOFF(I)+WSNOW(I)/RHOW)
                      RUNOFF(I)=RUNOFF(I)+WSNOW(I)/RHOW
                      TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TSNOW(I)+TFREZ)*
     1                      FI(I)*WSNOW(I)/RHOW)/(OVRFLW(I)+FI(I)*
     2                      WSNOW(I)/RHOW)
                      OVRFLW(I)=OVRFLW(I)+FI(I)*WSNOW(I)/RHOW
                  ENDIF
                  TSNOW(I)=0.0
                  WSNOW(I)=0.0
                  ICONT(I)=1
              ENDIF                     
              HTCS(I)=HTCS(I)+FI(I)*(TSNOW(I)+TFREZ)*HCPSNO(I)*
     1                ZSNOW(I)/DELT
              IF(SNOCONV.GT.0.) TICE(I)=(TICE(I)*ICE(I)+
     1                TBAR(I,IG)*SNOCONV/RHOW)/(ICE(I)+SNOCONV/RHOW)
              ICE(I)=ICE(I)+SNOCONV
          ENDIF
  500 CONTINUE
C               
      DO 550 J=1,IG
      DO 550 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              ZRMDR(I,J)=DELZ(J)                                                    
          ENDIF                                                  
  550 CONTINUE                                                            
C               
      DO 600 J=1,IG
      DO 600 K=1,IG+1
      DO 600 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              ZMAT(I,K,J)=0.0 
          ENDIF
  600 CONTINUE
C
      DO 650 J=2,IG+1
      DO 650 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              WMOVE(I,J)=DELZ(J-1)                                                  
              TMOVE(I,J)=TBAR(I,J-1)                                                
          ENDIF
  650 CONTINUE
C
      DO 700 K=1,IG+1
      DO 700 J=1,IG
      DO 700 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              IF(ZRMDR(I,J).GT.0. .AND. WMOVE(I,K).GT.0.)      THEN                    
                  ZMAT(I,K,J)=WMOVE(I,K)                                          
                  IF(ZMAT(I,K,J).GE.ZRMDR(I,J))           THEN                              
                      ZMAT(I,K,J)=ZRMDR(I,J)                                      
                      WMOVE(I,K)=WMOVE(I,K)-ZRMDR(I,J)                              
                      ZRMDR(I,J)=0.0                                            
                  ELSE                                                        
                      ZRMDR(I,J)=ZRMDR(I,J)-ZMAT(I,K,J)                             
                      WMOVE(I,K)=0.0                                            
                  ENDIF                                                       
              ENDIF                                                           
          ENDIF
  700 CONTINUE
C
      DO 900 J=1,IG
          DO 750 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)               THEN
                  TADD(I)=0.
              ENDIF
  750     CONTINUE
C
          DO 800 K=1,IG+1
          DO 800 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1           ICONT(I).EQ.1)                                     THEN
                  TADD(I)=TADD(I)+TMOVE(I,K)*ZMAT(I,K,J)
              ENDIF                                    
  800     CONTINUE                                                            
C
          DO 850 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)               THEN
                  TADD(I)=TADD(I)+TBAR(I,J)*ZRMDR(I,J)                                        
                  TBAR(I,J)=TADD(I)/DELZ(J)
                  HTC(I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                     DELZ(J)/DELT
              ENDIF                                              
  850     CONTINUE
  900 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
