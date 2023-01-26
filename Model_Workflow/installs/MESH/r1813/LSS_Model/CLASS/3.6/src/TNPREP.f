      SUBROUTINE TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                  GCONST,CPHCHG,IWATER, 
     2                  TBAR,TCTOP,TCBOT,
     3                  FI,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     4                  ISAND,ILG,IL1,IL2,JL,IG                     )
C
C     * MAR 03/08 - D.VERSEGHY. ASSIGN TCTOP3 AND TCBOT3 ON THE BASIS
C     *                         OF SUBAREA VALUES FROM TPREP; REPLACE
C     *                         THREE-LEVEL TEMPERATURE AND THERMAL
C     *                         CONDUCTIVITY VECTORS WITH STANDARD
C     *                         VALUES.
C     * AUG 16/06 - D.VERSEGHY. REMOVE TSTART.
C     * MAY 24/05 - D.VERSEGHY. LIMIT DELZ3 TO <= 4.1 M.
C     * OCT 04/05 - D.VERSEGHY. USE THREE-LAYER TBAR,TCTOP,TCBOT.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/02 - D.VERSEGHY. SHORTENED CLASS3 COMMON BLOCK,
C     * JUN 17/02 - D.VERSEGHY. USE NEW LUMPED SOIL AND PONDED
C     *                         WATER TEMPERATURE FOR FIRST LAYER;
C     *                         SHORTENED COMMON BLOCK.
C     * MAR 28/02 - D.VERSEGHY. CHANGE POND THRESHOLD VALUE FOR
C     *                         CALCULATION OF "IWATER".
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND
C     *                         BOTTOMS OF SOIL LAYERS.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         SURFACE TREATED AS WATER ONLY IF
C     *                         ZPOND > 1 MM.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * NOV 28/94 - M. LAZARE.  CLASS - VERSION 2.3.
C     *                         TCSATW,TCSATI DECLARED REAL(16).
C     * APR 10/92 - M. LAZARE.  CLASS - VERSION 2.1.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T3LAYR"
C     *                         INTO "TNPREP" AND "TNPOST" AND
C     *                         VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE COEFFICIENTS FOR GROUND HEAT
C     *                         FLUX, EXPRESSED AS A LINEAR FUNCTION
C     *                         OF SURFACE TEMPERATURE.  COEFFICIENTS
C     *                         ARE CALCULATED FROM LAYER TEMPERATURES,
C     *                         THICKNESSES AND THERMAL CONDUCTIVITIES,
C     *                         ASSUMING A QUADRATIC VARIATION OF
C     *                         TEMPERATURE WITH DEPTH WITHIN EACH
C     *                         SOIL LAYER. SET THE SURFACE LATENT 
C     *                         HEAT OF VAPORIZATION OF WATER AND 
C     *                         THE STARTING TEMPERATURE FOR THE 
C     *                         ITERATION IN "TSOLVC"/"TSOLVE".
C                                                                                 
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IG,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL A1    (ILG),    A2    (ILG),    B1    (ILG),
     1     B2    (ILG),    C2    (ILG),    GDENOM(ILG),    GCOEFF(ILG),
     2     GCONST(ILG),    CPHCHG(ILG)
C
      INTEGER              IWATER(ILG)     
C
C     * INPUT ARRAYS.
C
      REAL TBAR  (ILG,IG), TCTOP (ILG,IG), TCBOT (ILG,IG)
C
      REAL FI    (ILG),    ZPOND (ILG),    TBAR1P(ILG),
     1     TCSNOW(ILG),    ZSNOW (ILG)
C
      INTEGER              ISAND (ILG,IG)
C
      REAL DELZ  (IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL DELZ1,A3,B3,C3,TCZERO
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,
     2     SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C     * INITIALIZATION OF ARRAYS.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              DELZ1=DELZ(1)+ZPOND(I)                                                         
              IF(ZPOND(I).GT.0.5E-3)                          THEN
                  IWATER(I)=1
              ELSE                                                                        
                  IF(ISAND(I,1).GT.-4)                THEN
                      IWATER(I)=0
                  ELSE
                      IWATER(I)=2
                  ENDIF
              ENDIF    
C
              IF(IWATER(I).EQ.2)                                    THEN
                  CPHCHG(I)=CLHVAP+CLHMLT
              ELSE                                                                        
                  CPHCHG(I)=CLHVAP
              ENDIF                                                                   
C
              IF(ZSNOW(I).GT.0.0) THEN
                  TCZERO=1.0/(0.5/TCSNOW(I)+0.5/TCTOP(I,1))
              ELSE
                  TCZERO=TCTOP(I,1)
              ENDIF
              A1(I)=DELZ1/(3.0*TCZERO)
              A2(I)=DELZ1/(2.0*TCZERO)
              A3=A2(I)                                                                       
              B1(I)=DELZ1/(6.0*TCBOT(I,1))
              B2(I)=DELZ1/(2.0*TCBOT(I,1))+DELZ(2)/(3.0*TCTOP(I,2))
              B3=DELZ1/(2.0*TCBOT(I,1))+DELZ(2)/(2.0*TCTOP(I,2))
              C2(I)=DELZ(2)/(6.0*TCBOT(I,2))
              C3=DELZ(2)/(2.0*TCBOT(I,2))+DELZ(3)/(3.0*TCTOP(I,3))
              GDENOM(I)=A1(I)*(B2(I)*C3-B3*C2(I))-B1(I)*(A2(I)*C3-
     1                  A3*C2(I))                                    
              GCOEFF(I)=(B2(I)*C3-B3*C2(I)-B1(I)*(C3-C2(I)))/GDENOM(I) 
              GCONST(I)=(-TBAR1P(I)*(B2(I)*C3-B3*C2(I))+
     1                    TBAR(I,2)*B1(I)*C3-
     2                    TBAR(I,3)*B1(I)*C2(I))/GDENOM(I)           
          ENDIF                                                            
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END       
