      SUBROUTINE TNPOST(TBARPR,G12,G23,TPOND,GZERO,QFREZG,GCONST,
     1                  GCOEFF,TBAR,TCTOP,TCBOT,HCP,ZPOND,TSURF,
     2                  TBASE,TBAR1P,A1,A2,B1,B2,C2,FI,IWATER,
     3                  ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG       )
C
C     * NOV 01/06 - D.VERSEGHY. ALLOW PONDING ON ICE SHEETS.
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
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         FIX BUG IN CALCULATION OF FLUXES
C     *                         BETWEEN SOIL LAYERS (PRESENT SINCE 
C     *                         FIRST RELEASE OF VERSION 2.5).
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF TBARPR(I,1).
C     * APR 10/92 - M.LAZARE.   CLASS - VERSION 2.2.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T3LAYR" INTO
C     *                         "TNPREP" AND "TNPOST" AND VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE HEAT FLUXES BETWEEN SOIL 
C     *                         LAYERS; DISAGGREGATE FIRST SOIL LAYER
C     *                         TEMPERATURE INTO PONDED WATER AND
C     *                         SOIL TEMPERATURES; CONSISTENCY CHECK 
C     *                         ON CALCULATED SURFACE LATENT HEAT OF
C     *                         MELTING/FREEZING; CONVERT SOIL LAYER
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
      REAL TBARPR(ILG,IG)
C
      REAL G12   (ILG),    G23   (ILG),    TPOND (ILG)
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL GZERO (ILG),    QFREZG(ILG)    
C
C     * INPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), TCTOP (ILG,IG), TCBOT (ILG,IG),
     1     HCP   (ILG,IG), DELZW (ILG,IG)
C
      REAL ZPOND (ILG),    TSURF (ILG),    TBASE (ILG),    TBAR1P(ILG),
     1     A1    (ILG),    A2    (ILG),    B1    (ILG),    B2    (ILG),    
     2     C2    (ILG),    FI    (ILG),    GCONST(ILG),    GCOEFF(ILG)
C
      INTEGER              IWATER(ILG),    ISAND (ILG,IG)
C
      REAL DELZ  (IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL GZROLD,DELZ1
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
              GZROLD=GCOEFF(I)*TSURF(I)+GCONST(I)
              G12(I)=(TSURF(I)-TBAR1P(I)-A1(I)*GZROLD)/B1(I)
              G23(I)=(TSURF(I)-TBAR(I,2)-A2(I)*GZROLD-B2(I)*G12(I))/
     1               C2(I)                                    
              IF(ZPOND(I).GT.0.)                                THEN 
                  DELZ1=DELZ(1)+ZPOND(I)
                  TPOND(I)=(GZROLD/TCTOP(I,1)-G12(I)/TCBOT(I,1))*
     1                     (ZPOND(I)*ZPOND(I)-DELZ1*DELZ1)/(6.0*DELZ1)-
     2                     GZROLD*(ZPOND(I)-DELZ1)/(2.0*TCTOP(I,1))+
     3                     TBAR1P(I)-TFREZ
                  TBARPR(I,1)=((HCP(I,1)*DELZW(I,1)+HCPSND*(DELZ(1)-
     1                        DELZW(I,1))+HCPW*ZPOND(I))*TBAR1P(I)-
     2                        HCPW*ZPOND(I)*(TPOND(I)+TFREZ))/
     3                        (HCP(I,1)*DELZW(I,1)+HCPSND*(DELZ(1)-
     4                        DELZW(I,1)))-TFREZ
              ELSE                                                                        
                  TPOND(I)=0.                                                               
                  TBARPR(I,1)=TBAR(I,1)-TFREZ                                             
              ENDIF           
C
              IF((IWATER(I).EQ.1 .AND. QFREZG(I).GT.0.) .OR.
     1           (IWATER(I).EQ.2 .AND. QFREZG(I).LT.0.) .OR.
     2            IWATER(I).EQ.0)                               THEN              
                  GZERO(I)=GZERO(I)+QFREZG(I)                                                      
                  QFREZG(I)=0.                                                              
              ENDIF
          ENDIF
  100 CONTINUE
C 
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              TBARPR(I,2)=TBAR(I,2)-TFREZ
              IF(DELZW(I,3).GT.0.0 .AND. DELZW(I,3).LT.DELZ(3)
     1                             .AND. IG.EQ.3)              THEN
                  TBARPR(I,3)=(TBAR(I,3)*(HCP(I,3)*DELZW(I,3)+
     1                         HCPSND*(DELZ(3)-DELZW(I,3)))-TBASE(I)*
     2                         HCPSND*(DELZ(3)-DELZW(I,3)))/(HCP(I,3)*
     3                         DELZW(I,3))-TFREZ
              ELSE
                  DO 150 J=3,IG
                      TBARPR(I,J)=TBAR(I,J)-TFREZ
  150             CONTINUE
              ENDIF
          ENDIF
  200 CONTINUE                                                                    
C
      RETURN
      END 
