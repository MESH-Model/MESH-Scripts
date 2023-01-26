      SUBROUTINE GRALB(ALVSG,ALIRG,ALVSGC,ALIRGC,
     1                 ALGWET,ALGDRY,THLIQ,FSNOW,ALVSU,ALIRU,FCMXU,
     2                 AGVDAT,AGIDAT,ISAND,
     3                 ILG,IG,IL1,IL2,JL,IALG)
C
C     * APR 13/06 - D.VERSEGHY. SEPARATE ALBEDOS FOR OPEN AND
C     *                         CANOPY-COVERED GROUND.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 04/03 - D.VERSEGHY. RATIONALIZE CALCULATION OF URBAN
C     *                         ALBEDO.
C     * MAR 18/02 - D.VERSEGHY. UPDATES TO ALLOW ASSIGNMENT OF USER-
C     *                         SPECIFIED VALUES TO GROUND ALBEDO.
C     *                         PASS IN ICE AND ORGANIC ALBEDOS
C     *                         VIA NEW COMMON BLOCK "CLASS8".
C     * FEB 07/02 - D.VERSEGHY. REVISIONS TO BARE SOIL ALBEDO
C     *                         CALCULATIONS; REMOVAL OF SOIL
C     *                         MOISTURE EXTRAPOLATION TO SURFACE.
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CALCULATE SOIL ALBEDO FROM PERCENT
C     *                         SAND CONTENT RATHER THAN FROM COLOUR 
C     *                         INDEX.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         FIX BUG TO CALCULATE GROUND ALBEDO
C     *                         UNDER CANOPIES AS WELL AS OVER BARE
C     *                         SOIL.
C     * NOV 29/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         "CALL ABORT" CHANGED TO "CALL XIT",
C     *                         TO ENABLE RUNNING RUN ON PC'S.
C     * FEB 12/93 - D.VERSEGHY/M.LAZARE. INCREASE DRY SOIL ALBEDO TO 
C     *                                  0.45 FROM 0.35. 
C     * MAR 13/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CLASS - VERSION 2.0.
C     *                         CODE FOR MODEL VERSION GCM7U (WITH
C     *                         CANOPY). 
C     * APR 11/89 - D.VERSEGHY. CALCULATE VISIBLE AND NEAR-IR SOIL
C     *                         ALBEDOS BASED ON TEXTURE AND SURFACE
C     *                         WETNESS. (SET TO ICE ALBEDOS OVER
C     *                         CONTINENTAL ICE SHEETS.)
C
      IMPLICIT NONE
C                
C     * INTEGER CONSTANTS.
C
      INTEGER  ILG,IG,IL1,IL2,JL,IALG,IPTBAD,I
C
C     * OUTPUT ARRAYS.
C
      REAL ALVSG (ILG),   ALIRG (ILG),  ALVSGC (ILG),  ALIRGC (ILG)
C
C     * INPUT ARRAYS.
C
      REAL ALGWET(ILG),   ALGDRY(ILG),   THLIQ (ILG,IG),
     2     FSNOW (ILG),   ALVSU (ILG),   ALIRU (ILG),   FCMXU (ILG),
     1     AGVDAT(ILG),   AGIDAT(ILG)
C
      INTEGER    ISAND  (ILG,IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL FURB,ALBSOL
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
C
      COMMON /CLASS8/ ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
C---------------------------------------------------------------------
      IPTBAD=0
      DO 100 I=IL1,IL2
         IF(IALG.EQ.0)                                          THEN
            IF(ISAND(I,1).GE.0)                          THEN
                FURB=MAX(FCMXU(I),1.0E-5)                                    
                IF(THLIQ(I,1).GE.0.26) THEN  
                   ALBSOL=ALGWET(I)            
                ELSEIF(THLIQ(I,1).LE.0.22) THEN 
                   ALBSOL=ALGDRY(I)              
                ELSE                         
                   ALBSOL=THLIQ(I,1)*(ALGWET(I)-ALGDRY(I))/0.04+
     1                    ALGDRY(I)-5.50*(ALGWET(I)-ALGDRY(I)) 
                ENDIF                         
C
                ALVSG(I)=2.0*ALBSOL/3.0    
                ALIRG(I)=2.0*ALVSG(I)     
                ALVSG(I)=(1.0-FURB)*ALVSG(I)+FURB*ALVSU(I)
                ALIRG(I)=(1.0-FURB)*ALIRG(I)+FURB*ALIRU(I)
                IF(ALVSG(I).GT.1.0.OR.ALVSG(I).LT.0.0) IPTBAD=I
                IF(ALIRG(I).GT.1.0.OR.ALIRG(I).LT.0.0) IPTBAD=I
            ELSE IF(ISAND(I,1).EQ.-4 .AND.
     &              ALVSU(I).GT.0.0 .AND. ALIRU(I).GT.0.0)THEN
                ALVSG(I)=ALVSU(I)
                ALIRG(I)=ALIRU(I)
            ELSE IF(ISAND(I,1).EQ.-4)                    THEN
                ALVSG(I)=ALVSI
                ALIRG(I)=ALIRI
            ELSE IF(ISAND(I,1).EQ.-3)                    THEN
                ALVSG(I)=2.0*ALBRCK/3.0                                                        
                ALIRG(I)=2.0*ALVSG(I)                                                             
            ELSE IF(ISAND(I,1).EQ.-2)                    THEN
                ALVSG(I)=ALVSO
                ALIRG(I)=ALIRO
            ENDIF
         ELSEIF(IALG.EQ.1)                                     THEN
            ALVSG(I)=AGVDAT(I)
            ALIRG(I)=AGIDAT(I)
         ENDIF     
         ALVSGC(I)=ALVSG(I)
         ALIRGC(I)=ALIRG(I)
  100 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSG(IPTBAD),ALIRG(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I8,',',I3,'), ALVSG,ALIRG = ',2F10.5)
         CALL XIT('GRALB',-1)
      ENDIF

      RETURN                                                                      
      END
