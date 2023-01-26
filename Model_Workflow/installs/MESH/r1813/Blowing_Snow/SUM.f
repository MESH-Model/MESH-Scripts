      SUBROUTINE SUM(TQsalt, TQsusp, SBsum, SBsalt, DriftH, SublH)
C
      IMPLICIT NONE
C
C     * INPUT/OUTPUT VARIABLES.
C
      REAL DriftH, SublH
C
C     * INPUT VARIABLES.
C
      REAL TQsalt, TQsusp, SBsum, SBsalt
C
C     * TEMPORARY VARIABLES.
C
      REAL SBsumsalt
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT
C
      COMMON /CLASS1/ DELT
C
C-----------------------------------------------------------------------
      !> total sublimation
C
        SBsumsalt = SBsum + SBsalt
        IF(SBsumsalt.GE.0.)  THEN
           SublH = 0.0
         ELSE
           !> -mgmm2s to kg/m^2/s
           SublH=(SBsum+SBsalt)*(-1.0)
        ENDIF
C
         !> kg/m-width/s
         DriftH=(TQsalt+TQsusp)
      !>/ Convert to interval values
C
         !> kg/m^2/s to kg/m^2/interval
         SublH=SublH*DELT
         !> kg/m-width/s to kg/m-width/interval
         DriftH=DriftH*DELT
C
      RETURN
      END