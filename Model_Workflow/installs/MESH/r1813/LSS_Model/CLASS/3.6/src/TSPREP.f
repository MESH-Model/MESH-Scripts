      SUBROUTINE TSPREP(GCOEFFS,GCONSTS,CPHCHG,IWATER,
     1                  FI,ZSNOW,TSNOW,TCSNOW,
     2                  ILG,IL1,IL2,JL      )
C
C     * AUG 16/06 - D.VERSEGHY. MAJOR REVISION TO IMPLEMENT THERMAL
C     *                         SEPARATION OF SNOW AND SOIL.
C     * MAY 24/06 - D.VERSEGHY. LIMIT DELZ3 TO <= 4.1 M.
C     * OCT 04/05 - D.VERSEGHY. USE THREE-LAYER TBAR,TCTOP,TCBOT.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/02 - D.VERSEGHY. SHORTENED CLASS3 COMMON BLOCK.
C     * JUN 17/02 - D.VERSEGHY. USE NEW LUMPED SOIL AND PONDED WATER
C     *                         TEMPERATURE FOR FIRST LAYER; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND 
C     *                         BOTTOMS OF SOIL LAYERS.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * NOV 28/94 - M. LAZARE.  CLASS - VERSION 2.3.
C     *                         TCSATW,TCSATI DECLARED REAL(16).
C     * APR 10/92 - M. LAZARE.  CLASS - VERSION 2.1.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T4LAYR" 
C     *                         INTO "TSPREP" AND "TSPOST" AND
C     *                         VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE COEFFICIENTS FOR GROUND HEAT
C     *                         FLUX, EXPRESSED AS A LINEAR FUNCTION OF
C     *                         SURFACE TEMPERATURE. COEFFICIENTS ARE
C     *                         CALCULATED FROM LAYER TEMPERATURES, 
C     *                         THICKNESSES AND THERMAL CONDUCTIVITIES,
C     *                         ASSUMING A QUADRATIC VARIATION OF
C     *                         TEMPERATURE WITH DEPTH WITHIN EACH
C     *                         SOIL/SNOW LAYER. SET THE SURFACE 
C     *                         LATENT HEAT OF VAPORIZATION OF WATER
C     *                         AND THE STARTING TEMPERATURE FOR THE
C     *                         ITERATION IN "TSOLVC"/"TSOLVE".
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL GCOEFFS(ILG),   GCONSTS(ILG),   CPHCHG(ILG)
C
      INTEGER              IWATER(ILG)     
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG),    ZSNOW (ILG),    TSNOW (ILG),    TCSNOW(ILG)
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
C     * CALCULATE COEFFICIENTS.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              GCOEFFS(I)=3.0*TCSNOW(I)/ZSNOW(I)
              GCONSTS(I)=-3.0*TCSNOW(I)*TSNOW(I)/ZSNOW(I)
              IWATER(I)=2                                                                    
              CPHCHG(I)=CLHVAP+CLHMLT
          ENDIF
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END
