      SUBROUTINE TSPOST(GSNOW,TSNOW,WSNOW,RHOSNO,QMELTG,
     1                  GZERO,TSNBOT,HTCS,HMFN,
     2                  GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                  TSURF,ZSNOW,TCSNOW,HCPSNO,QTRANS,
     4                  FI,DELZ,ILG,IL1,IL2,JL,IG            )
C
C     * AUG 16/06 - D.VERSEGHY. MAJOR REVISION TO IMPLEMENT THERMAL
C     *                         SEPARATION OF SNOW AND SOIL.
C     * MAR 23/06 - D.VERSEGHY. ADD CALCULATIONS TO ALLOW FOR WATER 
C     *                         FREEZING IN SNOWPACK.
C     * OCT 04/05 - D.VERSEGHY. MODIFY 300 LOOP FOR CASES WHERE IG>3.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 17/02 - D.VERSEGHY. RESET PONDED WATER TEMPERATURE 
C     *                         USING CALCULATED GROUND HEAT FLUX;
C     *                         SHORTENED CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND
C     *                         BOTTOMS OF SOIL LAYERS, AND
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         FIX BUG IN CALCULATION OF FLUXES
C     *                         BETWEEN SOIL LAYERS (PRESENT SINCE 
C     *                         RELEASE OF CLASS VERSION 2.5).
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF TBARPR(I,1).
C     * APR 10/92 - M.LAZARE.   CLASS - VERSION 2.2.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T4LAYR" INTO
C     *                         "TSPREP" AND "TSPOST" AND VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE HEAT FLUXES BETWEEN SNOW/SOIL
C     *                         LAYERS; CONSISTENCY CHECK ON CALCULATED 
C     *                         SURFACE LATENT HEAT OF MELTING/
C     *                         FREEZING; STEP AHEAD SNOW LAYER 
C     *                         TEMPERATURE AND ASSIGN EXCESS HEAT TO
C     *                         MELTING IF NECESSARY; DISAGGREGATE
C     *                         FIRST SOIL LAYER TEMPERATURE INTO
C     *                         PONDED WATER AND SOIL TEMPERATURES;
C     *                         ADD SHORTWAVE RADIATION TRANSMITTED
C     *                         THROUGH SNOWPACK TO HEAT FLUX AT TOP
C     *                         OF FIRST SOIL LAYER; CONVERT LAYER
C     *                         TEMPERATURES TO DEGREES C.
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IG,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL GZERO (ILG),    TSNBOT(ILG)
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL GSNOW (ILG),    TSNOW (ILG),    WSNOW (ILG),    RHOSNO(ILG),
     1     QMELTG(ILG),    HTCS  (ILG),    HMFN  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL TSURF (ILG),    ZSNOW (ILG),    TCSNOW(ILG),    HCPSNO(ILG),    
     1     QTRANS(ILG),    GCONST(ILG),    GCOEFF(ILG),    FI    (ILG),
     2     GCONSTS(ILG),   GCOEFFS(ILG),   TBAR(ILG,IG),   DELZ  (IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL GSNOLD,HADD,HCONV,WFREZ
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1     SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              GSNOLD=GCOEFFS(I)*TSURF(I)+GCONSTS(I)
              TSNBOT(I)=(ZSNOW(I)*TSNOW(I)+DELZ(1)*TBAR(I,1))/
     1             (ZSNOW(I)+DELZ(1))
C              TSNBOT(I)=0.90*TSNOW(I)+0.10*TBAR(I,1)
C              TSNBOT(I)=TSURF(I)-GSNOLD*ZSNOW(I)/(2.0*TCSNOW(I))
              TSNBOT(I)=MIN(TSNBOT(I),TFREZ)
              GZERO(I)=GCOEFF(I)*TSNBOT(I)+GCONST(I)
              IF(QMELTG(I).LT.0.)                               THEN
                  GSNOW(I)=GSNOW(I)+QMELTG(I)                                                      
                  QMELTG(I)=0.                                                              
              ENDIF                                                                       
              TSNOW(I)=TSNOW(I)+(GSNOW(I)-GZERO(I))*DELT/
     1                          (HCPSNO(I)*ZSNOW(I))-TFREZ                         
              IF(TSNOW(I).GT.0.)                                THEN
                  QMELTG(I)=QMELTG(I)+TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  GSNOW(I)=GSNOW(I)-TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  TSNOW(I)=0.                                                               
              ENDIF                                                                       
              GZERO(I)=GZERO(I)+QTRANS(I)
          ENDIF
  100 CONTINUE
C 
      DO 200 I=IL1,IL2
           IF(FI(I).GT.0. .AND. TSNOW(I).LT.0. .AND. WSNOW(I).GT.0.)
     1                                                              THEN
             HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
             HADD=-TSNOW(I)*HCPSNO(I)*ZSNOW(I)
             HCONV=CLHMLT*WSNOW(I)
             IF(HADD.LE.HCONV)                           THEN                                                  
                 WFREZ=HADD/CLHMLT
                 HADD=0.0
                 WSNOW(I)=MAX(0.0,WSNOW(I)-WFREZ)
                 TSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1               (RHOW*ZSNOW(I))
             ELSE                                                                        
                 HADD=HADD-HCONV                                                         
                 WFREZ=WSNOW(I)
                 WSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                 TSNOW(I)=-HADD/(HCPSNO(I)*ZSNOW(I))
             ENDIF
             HMFN(I)=HMFN(I)-FI(I)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)-FI(I)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
  200 CONTINUE
C
      RETURN                                                                      
      END
