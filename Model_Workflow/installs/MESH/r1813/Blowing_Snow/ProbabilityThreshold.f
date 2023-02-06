      SUBROUTINE ProbabilityThreshold(ZSNOW,t,Uten_Prob,
     1              Probability,Threshold,S,SnowAge,DrySnow)
C
C     * SEP 2010 - M.MACDONALD. Calculate the probability of blowing snow 
C     *                         occurence according to Li and Pomeroy (2000).
C     *                         
      IMPLICIT NONE
C
C     * INPUT/OUTPUT VARIABLES.
C
      REAL Probability, Threshold,
     1     SnowAge, DrySnow
C
C     * INPUT VARIABLES.
C
      REAL ZSNOW, t, Uten_Prob, S
C
C     * TEMPORARY VARIABLES.
C
      REAL Wind, Mean, Variance, c, Temp
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT
C
      COMMON /CLASS1/ DELT
C
C--------------------------------------------------------------
!>Probability of blowing snow occurrence and threshold wind
!>speeds determined by ambient air temperature and snow age
C
           Wind=0.0
           Probability=0.0
C
      Temp=t-273.16
C
      IF(ZSNOW.LE.0.0) THEN
      !> no snow available
           DrySnow=0.0
           SnowAge=0.0
           !> Threshold wind speed (m/s)
           Threshold=9.43+0.18*Temp+0.0033*Temp**2
      ELSEIF(S.GE.0. .AND. Temp.LT.0.0) THEN
      !> with concurrent snowfall: new dry snow
           DrySnow=1.0
           SnowAge=DELT/3600 !> [hours]
           Mean=0.365*Temp+0.00706*Temp**2
     1         +0.91*LOG(SnowAge)+11.0
           Variance=0.145*Temp+0.00196*Temp**2+4.23
           DO 100 WHILE (Wind.LE.Uten_Prob .AND.
     1                   Uten_Prob.GE.3.0)
           !> Wind < 3 m/s too weak for dry snow transport
              Wind=Wind+0.1
              c=(-(Wind - Mean)**2)/(2.0*Variance**2)
              Probability=Probability+(1.0/(Variance*2.5055))
     1                    *(EXP(c))*0.1
  100      CONTINUE
           !> (m/s)
           Threshold=9.43+0.18*Temp+0.0033*Temp**2
      ELSEIF(Temp.LT.0.0 .AND. DrySnow.EQ.1.0) THEN
      !> without concurrent snowfall: old dry snow
           SnowAge=SnowAge+DELT/3600
           Mean = 0.365*Temp+0.00706*Temp**2
     1           +0.91*LOG(SnowAge)+11.0
           Variance=0.145*Temp+0.00196*Temp**2+4.23
           DO 200 WHILE (Wind.LE.Uten_Prob .AND.
     1                    Uten_Prob.GE.3.0)
           !> Wind < 3 m/s too weak for dry snow transport
              Wind=Wind+0.1
              c=(-(Wind - Mean)**2)/(2.0*Variance**2)
              Probability=Probability+(1.0/(Variance * 2.5055))
     1                    *(EXP(c))*0.1
  200      CONTINUE
          !> (m/s)
          Threshold=9.43+0.18*Temp+0.0033*Temp**2
      ELSEIF(Temp.GE.0.0 .OR. DrySnow.EQ.0.0) THEN
           !> or wet snow remains on the ground
           DrySnow=0.0
           SnowAge=0.0
           Mean=21.0
           Variance=7.0
C
           DO 300 WHILE (Wind.LE.Uten_Prob .AND.
     1                    Uten_Prob.GE.3.0)
                   !> Wind<3m/s too weak for dry snow transport
                      Wind=Wind+0.1
                      c=(-(Wind - Mean)**2)/(2.0*Variance**2)
                      Probability=Probability+(1.0/(Variance*
     1                            2.5055))*(EXP(c))*0.1
  300      CONTINUE
           !> (m/s)
           Threshold = 9.9
      ENDIF
C
      RETURN
      END