      SUBROUTINE SNOALBA(ALVSSN,ALIRSN,ALVSSC,ALIRSC,ALBSNO,
     1                   TRSNOW,ZSNOW,FSNOW,ASVDAT,ASIDAT,
     2                   ILG,IG,IL1,IL2,JL,IALS)
C
C     * FEB 05/07 - D.VERSEGHY. STREAMLINE CALCULATIONS OF
C     *                         ALVSSN AND ALIRSN.
C     * APR 13/06 - D.VERSEGHY. SEPARATE ALBEDOS FOR OPEN AND 
C     *                         CANOPY-COVERED SNOW.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * MAR 18/02 - D.VERSEGHY. UPDATES TO ALLOW ASSIGNMENT OF
C     *                         USER-SPECIFIED VALUES TO SNOW
C     *                         ALBEDO.
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         SPECIFY LOCATION OF ICE SHEETS
C     *                         BY SOIL TEXTURE ARRAY RATHER
C     *                         THAN BY SOIL COLOUR INDEX.
C     * NOV 29/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         CALL ABORT CHANGED TO CALL XIT TO 
C     *                         ENABLE RUNNING ON PC'S.
C     * MAR 13/92 - M.LAZARE.   CODE FOR MODEL VERSION GCM7 -
C     *                         DIVIDE PREVIOUS SUBROUTINE 
C     *                         "SNOALB" INTO "SNOALBA" AND
C     *                         "SNOALBW" AND VECTORIZE.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. DISAGGREGATE SNOW ALBEDO INTO
C     *                         VISIBLE AND NEAR-IR PORTIONS;
C     *                         CALCULATE TRANSMISSIVITY TO
C     *                         SHORTWAVE RADIATION.
C
      IMPLICIT NONE
C    
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IG,IL1,IL2,JL,IALS,IPTBAD,I
C
C     * OUTPUT ARRAYS.
C
      REAL   ALVSSN(ILG),  ALIRSN(ILG),  ALVSSC(ILG),  ALIRSC(ILG),
     1       TRSNOW(ILG)
C
C     * INPUT ARRAYS.
C
      REAL   ALBSNO(ILG),  ZSNOW (ILG),  FSNOW (ILG),
     1       ASVDAT(ILG),  ASIDAT(ILG)
C
C------------------------------------------------------------------
      IPTBAD=0
      DO 100 I=IL1,IL2                                           
         IF(ALBSNO(I).LT.0.50.AND.ALBSNO(I).GT.0.499) ALBSNO(I)=0.50                      
         IF(FSNOW(I).GT.0.0 .AND. IALS.EQ.0)              THEN  
             IF(ALBSNO(I).GT.0.70)                    THEN
                 ALVSSN(I)=0.79*(ALBSNO(I)-0.70)+0.84                                         
                 ALIRSN(I)=1.21*(ALBSNO(I)-0.70)+0.56                                         
             ELSE
                 ALVSSN(I)=0.97*(ALBSNO(I)-0.50)+0.62                                         
                 ALIRSN(I)=1.03*(ALBSNO(I)-0.50)+0.38                                         
             ENDIF
             IF(ALVSSN(I).GT.0.999.OR.ALVSSN(I).LT.0.001) IPTBAD=I
             IF(ALIRSN(I).GT.0.999.OR.ALIRSN(I).LT.0.001) IPTBAD=I
         ELSE IF(FSNOW(I).GT.0.0 .AND. IALS.EQ.1)         THEN  
             ALVSSN(I)=ASVDAT(I)
             ALIRSN(I)=ASIDAT(I)
         ENDIF                                                                   
         ALVSSC(I)=ALVSSN(I)
         ALIRSC(I)=ALIRSN(I)
         TRSNOW(I)=EXP(-25.0*ZSNOW(I))                                                 
  100 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSSN(IPTBAD),ALIRSN(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSSN,ALIRSN = ',2F10.5)
         CALL XIT('SNOALBA',-1)
      ENDIF
C
      RETURN
      END
