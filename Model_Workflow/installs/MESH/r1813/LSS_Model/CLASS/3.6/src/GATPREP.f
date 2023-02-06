      SUBROUTINE GATPREP(ILMOS,JLMOS,IWMOS,JWMOS,
     1                   NML,NMW,GCROW,FAREA,MOSID,
     2                   NL,NM,ILG,IL1,IL2,IM)
C
C     * DEC 28/11 - D.VERSEGHY. CHANGE ILGM BACK TO ILG AND
C     *                         ILG TO NL FOR CONSISTENCY WITH
C     *                         BOTH STAND-ALONE AND GCM
C     *                         CONVENTIONS.
C     * OCT 22/11 - M.LAZARE. REMOVE OCEAN/ICE CODE (NOW DONE
C     *                       IN COISS).
C     * OCT 21/11 - M.LAZARE. COSMETIC: ILG->ILGM AND NLAT->ILG,
C     *                       TO BE CONSISTENT WITH MODEL
C     *                       CONVENTION. ALSO GCGRD->GCROW.
C     * JUN 12/06 - E.CHAN.  DIMENSION IWAT AND IICE BY ILG.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 09/02 - D.VERSEGHY/M.LAZARE. DETERMINE INDICES FOR
C     *                        GATHER-SCATTER OPERATIONS ON
C     *                        CURRENT LATITUDE LOOP.
C     
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER   NML,NMW,NL,NM,ILG,IL1,IL2,IM,I,J
C
C     * OUTPUT FIELDS.
C
      INTEGER  ILMOS  (ILG),  JLMOS  (ILG),  IWMOS  (ILG),
     1         JWMOS  (ILG)  
C
C     * INPUT FIELDS.
C 
      REAL     GCROW  (NL), FAREA (NL,NM)
C
      INTEGER  MOSID  (NL,NM)
C---------------------------------------------------------------------
      NML=0
      NMW=0

      DO 200 I=IL1,IL2
          IF(GCROW(I).LE.-0.5)                               THEN
              DO 100 J=1,IM
                  IF(FAREA(I,J).GT.0.0)              THEN
                      IF(MOSID(I,J).GT.0)    THEN
                          NML=NML+1
                          ILMOS(NML)=I
                          JLMOS(NML)=J
                      ELSE
                          NMW=NMW+1
                          IWMOS(NMW)=I
                          JWMOS(NMW)=J
                      ENDIF
                  ENDIF
  100         CONTINUE
          ENDIF
  200 CONTINUE
C
      RETURN
      END
