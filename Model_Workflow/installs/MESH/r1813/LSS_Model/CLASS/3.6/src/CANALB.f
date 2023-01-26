      SUBROUTINE CANALB(ALVSCN,ALIRCN,ALVSCS,ALIRCS,TRVSCN,TRIRCN,
     1                  TRVSCS,TRIRCS,RC,RCS,
     2                  ALVSC,ALIRC,RSMIN,QA50,VPDA,VPDB,PSIGA,PSIGB,
     3                  FC,FCS,FSNOW,FSNOWC,FSNOCS,FCAN,FCANS,PAI,PAIS,
     4                  AIL,PSIGND,FROOT,FCLOUD,COSZS,QSWINV,VPD,TA,
     5                  ACVDAT,ACIDAT,ALVSGC,ALIRGC,ALVSSC,ALIRSC,
     6                  ILG,IL1,IL2,JL,IC,ICP1,IG,IALC,
     7                  CXTEFF,TRVS,TRIR,RCACC,RCG,RCV,RCT,GC) 
C
C     * DEC 21/11 - M.LAZARE.   DEFINE CONSTANTS "EXPMAX1", EXPMAX2",
C     *                         "EXPMAX3" TO AVOID REDUNDANT EXP
C     *                         CALCULATIONS.
C     * OCT 16/08 - R.HARVEY.   ADD LARGE LIMIT FOR EFFECTIVE
C     *                         EXTINCTION COEFFICIENT (CXTEFF) IN
C     *                         (RARE) CASES WHEN CANOPY TRANSMISSIVITY
C     *                         IN THE VISIBLE IS ZERO EXACTLY.
C     * MAR 25/08 - D.VERSEGHY. DISTINGUISH BETWEEN LEAF AREA INDEX
C     *                         AND PLANT AREA INDEX.
C     * OCT 19/07 - D.VERSEGHY. SIMPLIFY ALBEDO CALCULATIONS FOR
C     *                         SNOW-FREE CROPS AND GRASS; CORRECT
C     *                         BUG IN CALCULATION OF RC.
C     * APR 13/06 - P.BARTLETT/D.VERSEGHY. CORRECT OVERALL CANOPY 
C     *                                    ALBEDO, INTRODUCE SEPARATE
C     *                                    GROUND AND SNOW ALBEDOS FOR
C     *                                    OPEN OR CANOPY-COVERED AREAS.
C     * MAR 21/06 - P.BARTLETT. PROTECT RC CALCULATION AGAINST DIVISION
C     *                         BY ZERO.
C     * SEP 26/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 600 LOOP.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 05/04 - D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS AGAINST
C     *                         ROUNDOFF ERRORS.
C     * JAN 24/02 - P.BARTLETT/D.VERSEGHY. REFINE CALCULATION OF NEW
C     *                                    STOMATAL RESISTANCES.
C     * JUL 30/02 - P.BARTLETT/D.VERSEGHY. NEW STOMATAL RESISTANCE
C     *                                    FORMULATION INCORPORATED.
C     * MAR 18/02 - D.VERSEGHY. ALLOW FOR ASSIGNMENT OF SPECIFIED TIME-
C     *                         VARYING VALUES OF VEGETATION SNOW-FREE
C     *                         ALBEDO.
C     * NOV 29/94 - M.LAZARE. CLASS - VERSION 2.3.
C     *                       CALL ABORT CHANGED TO CALL XIT TO ENABLE 
C     *                       RUNNING ON PC'S.
C     * MAY 06/93 - D.VERSEGHY. EXTENSIVE MODIFICATIONS TO CANOPY
C     *                         ALBEDO LOOPS. 
C     * MAR 03/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CANOPY ALBEDOS AND TRANSMISSIVITIES.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IC,ICP1,IG,IALC,I,J,IPTBAD,JPTBAD,JPTBDI
C
C     * OUTPUT ARRAYS.
C
      REAL ALVSCN(ILG),   ALIRCN(ILG),   ALVSCS(ILG),   ALIRCS(ILG),
     1     TRVSCN(ILG),   TRIRCN(ILG),   TRVSCS(ILG),   TRIRCS(ILG),
     2     RC    (ILG),   RCS   (ILG)
C
C     * 2-D INPUT ARRAYS.                                                   
C                        
      REAL ALVSC (ILG,ICP1),         ALIRC (ILG,ICP1),
     1     RSMIN (ILG,IC),           QA50  (ILG,IC),
     2     VPDA  (ILG,IC),           VPDB  (ILG,IC),
     3     PSIGA (ILG,IC),           PSIGB (ILG,IC),
     4     FCAN  (ILG,IC),           FCANS (ILG,IC), 
     5     PAI   (ILG,IC),           PAIS  (ILG,IC),                              
     6     ACVDAT(ILG,IC),           ACIDAT(ILG,IC),
     7     AIL   (ILG,IC),           FROOT (ILG,IG)
C
C     * 1-D INPUT ARRAYS.
C
      REAL FC    (ILG),   FCS   (ILG),   FSNOW (ILG),   FSNOWC(ILG),
     1     FSNOCS(ILG),   PSIGND(ILG),   FCLOUD(ILG),   COSZS (ILG),   
     2     QSWINV(ILG),   VPD   (ILG),   TA    (ILG),   ALVSGC(ILG),   
     3     ALIRGC(ILG),   ALVSSC(ILG),   ALIRSC(ILG)
C
C     * OTHER DATA ARRAYS.
C
      REAL CANEXT(4),     XLEAF (4)
C
C     * WORK ARRAYS.
C
      REAL CXTEFF(ILG,IC),           RCACC (ILG,IC),
     1     RCV   (ILG,IC),           RCG   (ILG,IC),
     2     RCT   (ILG),              GC    (ILG),
     3     TRVS  (ILG),              TRIR  (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL SVF,ALVSCX,ALIRCX,ALVSN,ALIRN,ALVSS,ALIRS,
     1     TRTOT,FRMAX,EXPMAX1,EXPMAX2,EXPMAX3
C
C     * COMMON BLOCK AND OTHER PARAMETERS.
C
      REAL DELT,TFREZ,ALVSWC,ALIRWC,
     1     TRCLRV,TRCLDV,TRCLRT,TRCLDT,CXTLRG
C                                                                                  
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS7/ CANEXT,XLEAF
 
      DATA ALVSWC,CXTLRG
     1    /  0.17,1.0E20  /
C----------------------------------------------------------------------
C
C     * ASSIGN CONSTANT EXPONENTIATION TERMS: EXPMAX1=EXP(-0.4/0.9659),
C     * EXPMAX2=EXP(-0.4/0.7071),EXPMAX3=EXP(-0.4/0.2588)
C
      EXPMAX1=0.6609
      EXPMAX2=0.5680
      EXPMAX3=0.2132
C
C     * INITIALIZE WORK ARRAYS.
C
      DO 50 I=IL1,IL2
          RCT(I)=0.0
          GC(I)=0.0
          RC(I)=0.0
50    CONTINUE
      DO 100 J=1,IC
      DO 100 I=IL1,IL2
          CXTEFF(I,J)=0.0
          RCACC(I,J)=0.0
          RCG(I,J)=0.0
          RCV(I,J)=0.0
100   CONTINUE
C
C     * ALBEDO AND TRANSMISSIVITY CALCULATIONS FOR CANOPY OVER 
C     * BARE SOIL.
C
C     * NEEDLELEAF TREES.
C
      J=1
      DO 150 I=IL1,IL2                                                                                  
          IF(COSZS(I).GT.0. .AND. FCAN(I,J).GT.0.)                  THEN 
              TRCLRV=EXP(-0.4*PAI(I,J)/COSZS(I))                                    
              TRCLDV=0.30*EXP(-0.4*PAI(I,J)/0.9659)+0.50*EXP(-0.4*               
     1               PAI(I,J)/0.7071)+0.20*EXP(-0.4*PAI(I,J)/0.2588)   
              TRCLRT=EXP(-0.3*PAI(I,J)/COSZS(I))                                    
              TRCLDT=0.30*EXP(-0.3*PAI(I,J)/0.9659)+0.50*EXP(-0.3*              
     1               PAI(I,J)/0.7071)+0.20*EXP(-0.3*PAI(I,J)/0.2588)   
              TRVS(I)=FCLOUD(I)*TRCLDV+(1.0-FCLOUD(I))*TRCLRV
              IF(TRVS(I).GT.0.0001)                           THEN
                  CXTEFF(I,J)=-LOG(TRVS(I))/MAX(PAI(I,J),1.0E-5)
              ELSE
                  CXTEFF(I,J)=CXTLRG
              ENDIF
              TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
              TRIR(I)= 2.*TRTOT-TRVS(I)
              TRVSCN(I)=TRVSCN(I)+FCAN(I,J)*TRVS(I)
              TRIRCN(I)=TRIRCN(I)+FCAN(I,J)*TRIR(I)
          ENDIF
  150 CONTINUE
C
      DO 200 I=IL1,IL2                                               
          IF(COSZS(I).GT.0. .AND. FCAN(I,J).GT.0.)               THEN
              SVF=EXP(CANEXT(J)*PAI(I,J))
              ALIRWC=ALIRC(I,J)+0.04
              IF(IALC.EQ.0) THEN
                  ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSC(I,J)
                  ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRC(I,J)
                  ALVSN=(1.0-SVF)*ALVSCX+SVF*TRVS(I)*ALVSGC(I)
                  ALIRN=(1.0-SVF)*ALIRCX+SVF*TRIR(I)*ALIRGC(I)
              ELSE
                  ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ACVDAT(I,J)
                  ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ACIDAT(I,J)
                  ALVSN=(1.0-SVF)*ALVSCX+SVF*ACVDAT(I,J)
                  ALIRN=(1.0-SVF)*ALIRCX+SVF*ACIDAT(I,J)
              ENDIF
              ALVSCN(I)=ALVSCN(I)+FCAN(I,J)*ALVSN
              ALIRCN(I)=ALIRCN(I)+FCAN(I,J)*ALIRN
          ENDIF
  200 CONTINUE
C
C     * BROADLEAF TREES.
C
      J=2
      DO 250 I=IL1,IL2                                                                                  
          IF(COSZS(I).GT.0. .AND. FCAN(I,J).GT.0.)                  THEN
              TRCLRV=MIN(EXP(-0.7*PAI(I,J)),EXP(-0.4/COSZS(I)))                   
              TRCLDV=0.30*MIN(EXP(-0.7*PAI(I,J)),EXPMAX1)             
     1              +0.50*MIN(EXP(-0.7*PAI(I,J)),EXPMAX2)              
     2              +0.20*MIN(EXP(-0.7*PAI(I,J)),EXPMAX3)              
              TRCLRT=MIN(EXP(-0.4*PAI(I,J)),EXP(-0.4/COSZS(I)))                   
              TRCLDT=0.30*MIN(EXP(-0.4*PAI(I,J)),EXPMAX1)+            
     1               0.50*MIN(EXP(-0.4*PAI(I,J)),EXPMAX2)+              
     2               0.20*MIN(EXP(-0.4*PAI(I,J)),EXPMAX3)               
              TRVS(I)=FCLOUD(I)*TRCLDV+(1.0-FCLOUD(I))*TRCLRV
              IF(TRVS(I).GT.0.0001)                           THEN
                  CXTEFF(I,J)=-LOG(TRVS(I))/MAX(PAI(I,J),1.0E-5)
              ELSE
                  CXTEFF(I,J)=CXTLRG
              ENDIF
              TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
              TRIR(I)= 2.*TRTOT-TRVS(I)
              TRVSCN(I)=TRVSCN(I)+FCAN(I,J)*TRVS(I)
              TRIRCN(I)=TRIRCN(I)+FCAN(I,J)*TRIR(I)
          ENDIF
  250 CONTINUE
C
      DO 300 I=IL1,IL2                                               
          IF(COSZS(I).GT.0. .AND. FCAN(I,J).GT.0.)               THEN
              SVF=EXP(CANEXT(J)*PAI(I,J))
              ALIRWC=ALIRC(I,J)+0.04
              IF(IALC.EQ.0) THEN
                  ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSC(I,J)
                  ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRC(I,J)
                  ALVSN=(1.0-SVF)*ALVSCX+SVF*TRVS(I)*ALVSGC(I)
                  ALIRN=(1.0-SVF)*ALIRCX+SVF*TRIR(I)*ALIRGC(I)
              ELSE
                  ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ACVDAT(I,J)
                  ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ACIDAT(I,J)
                  ALVSN=(1.0-SVF)*ALVSCX+SVF*ACVDAT(I,J)
                  ALIRN=(1.0-SVF)*ALIRCX+SVF*ACIDAT(I,J)
              ENDIF
              ALVSCN(I)=ALVSCN(I)+FCAN(I,J)*ALVSN
              ALIRCN(I)=ALIRCN(I)+FCAN(I,J)*ALIRN
          ENDIF
  300 CONTINUE
C
C     * CROPS AND GRASS.
C
      DO 350 J=3,IC
      DO 350 I=IL1,IL2                                                                                  
          IF(COSZS(I).GT.0. .AND. FCAN(I,J).GT.0.)                  THEN
              TRCLRV=EXP(-0.5*PAI(I,J)/COSZS(I))                                    
              TRCLDV=0.30*EXP(-0.5*PAI(I,J)/0.9659)+0.50*EXP(-0.5*               
     1               PAI(I,J)/0.7071)+0.20*EXP(-0.5*PAI(I,J)/0.2588)
              TRCLRT=EXP(-0.4*PAI(I,J)/COSZS(I))                                    
              TRCLDT=0.30*EXP(-0.4*PAI(I,J)/0.9659)+0.50*EXP(-0.4*              
     1               PAI(I,J)/0.7071)+0.20*EXP(-0.4*PAI(I,J)/0.2588)                
              TRVS(I)=FCLOUD(I)*TRCLDV+(1.0-FCLOUD(I))*TRCLRV
              IF(TRVS(I).GT.0.0001)                           THEN
                  CXTEFF(I,J)=-LOG(TRVS(I))/MAX(PAI(I,J),1.0E-5)
              ELSE
                  CXTEFF(I,J)=CXTLRG
              ENDIF
              TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
              TRIR(I)= 2.*TRTOT-TRVS(I)
              TRVSCN(I)=TRVSCN(I)+FCAN(I,J)*TRVS(I)
              TRIRCN(I)=TRIRCN(I)+FCAN(I,J)*TRIR(I)
          ENDIF
  350 CONTINUE
C
      DO 400 J=3,IC
      DO 400 I=IL1,IL2                                                     
          IF(COSZS(I).GT.0. .AND. FCAN(I,J).GT.0.)               THEN
              SVF=EXP(CANEXT(J)*PAI(I,J))
              ALIRWC=ALIRC(I,J)+0.04
              IF(IALC.EQ.0) THEN
                  ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ALVSC(I,J)
                  ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ALIRC(I,J)
                  ALVSN=(1.0-SVF)*ALVSCX+SVF*TRVS(I)*ALVSGC(I)
                  ALIRN=(1.0-SVF)*ALIRCX+SVF*TRIR(I)*ALIRGC(I)
              ELSE
                  ALVSCX=FSNOWC(I)*ALVSWC+(1.0-FSNOWC(I))*ACVDAT(I,J)
                  ALIRCX=FSNOWC(I)*ALIRWC+(1.0-FSNOWC(I))*ACIDAT(I,J)
                  ALVSN=(1.0-SVF)*ALVSCX+SVF*ACVDAT(I,J)
                  ALIRN=(1.0-SVF)*ALIRCX+SVF*ACIDAT(I,J)
              ENDIF
              ALVSCN(I)=ALVSCN(I)+FCAN(I,J)*ALVSN
              ALIRCN(I)=ALIRCN(I)+FCAN(I,J)*ALIRN
          ENDIF   
  400 CONTINUE
C
C     * TOTAL ALBEDOS.
C
      IPTBAD=0                           
      DO 450 I=IL1,IL2
          IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                      THEN
              ALVSCN(I)=ALVSCN(I)/FC(I)                                                        
               ALIRCN(I)=ALIRCN(I)/FC(I)
          ENDIF
          IF(ALVSCN(I).GT.1. .OR. ALVSCN(I).LT.0.) IPTBAD=I
          IF(ALIRCN(I).GT.1. .OR. ALIRCN(I).LT.0.) IPTBAD=I
  450 CONTINUE                                                                
C
      IF(IPTBAD.NE.0) THEN
          WRITE(6,6100) IPTBAD,JL,ALVSCN(IPTBAD),ALIRCN(IPTBAD)
 6100     FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSCN,ALIRCN = ',2F10.5)
          CALL XIT('CANALB',-1)    
      ENDIF                                                                                  
C
C     * TOTAL TRANSMISSIVITIES.
C
      IPTBAD=0
      DO 475 I=IL1,IL2
          IF(FC(I).GT.0. .AND. COSZS(I).GT.0.)                     THEN
              TRVSCN(I)=TRVSCN(I)/FC(I)
              TRIRCN(I)=TRIRCN(I)/FC(I)
              TRVSCN(I)=MIN( TRVSCN(I), 0.90*(1.0-ALVSCN(I)) )
              TRIRCN(I)=MIN( TRIRCN(I), 0.90*(1.0-ALIRCN(I)) )
          ENDIF
          IF(TRVSCN(I).GT.1. .OR. TRVSCN(I).LT.0.) IPTBAD=I
          IF(TRIRCN(I).GT.1. .OR. TRIRCN(I).LT.0.) IPTBAD=I
  475 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
          WRITE(6,6300) IPTBAD,JL,TRVSCN(IPTBAD),TRIRCN(IPTBAD)
 6300     FORMAT('0AT (I,J)= (',I3,',',I3,'), TRVSCN,TRIRCN = ',2F10.5)
          CALL XIT('CANALB',-3)    
      ENDIF                                                                                  
C----------------------------------------------------------------------
C
C     * ALBEDO AND TRANSMISSIVITY CALCULATIONS FOR CANOPY OVER SNOW.
C
C     * NEEDLELEAF TREES.
C
      J=1
      DO 500 I=IL1,IL2                                                                                  
          IF(COSZS(I).GT.0. .AND. FCANS(I,J).GT.0.)               THEN
              TRCLRV=EXP(-0.4*PAIS(I,J)/COSZS(I))                                    
              TRCLDV=0.30*EXP(-0.4*PAIS(I,J)/0.9659)+0.50*EXP(-0.4*               
     1               PAIS(I,J)/0.7071)+0.20*EXP(-0.4*PAIS(I,J)/0.2588)   
              TRCLRT=EXP(-0.3*PAIS(I,J)/COSZS(I))                                    
              TRCLDT=0.30*EXP(-0.3*PAIS(I,J)/0.9659)+0.50*EXP(-0.3*              
     1               PAIS(I,J)/0.7071)+0.20*EXP(-0.3*PAIS(I,J)/0.2588)   
              TRVS(I)=FCLOUD(I)*TRCLDV+(1.0-FCLOUD(I))*TRCLRV
              TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
              TRIR(I)= 2.*TRTOT-TRVS(I)
              TRVSCS(I)=TRVSCS(I)+FCANS(I,J)*TRVS(I)
              TRIRCS(I)=TRIRCS(I)+FCANS(I,J)*TRIR(I)
          ENDIF
  500 CONTINUE
C
      DO 550 I=IL1,IL2                                               
          IF(COSZS(I).GT.0. .AND. FCANS(I,J).GT.0.)             THEN
              ALIRWC=ALIRC(I,J)+0.04
              IF(IALC.EQ.0) THEN
                  ALVSCX=FSNOCS(I)*ALVSWC+(1.0-FSNOCS(I))*ALVSC(I,J)
                  ALIRCX=FSNOCS(I)*ALIRWC+(1.0-FSNOCS(I))*ALIRC(I,J)
              ELSE
                  ALVSCX=FSNOCS(I)*ALVSWC+(1.0-FSNOCS(I))*ACVDAT(I,J)
                  ALIRCX=FSNOCS(I)*ALIRWC+(1.0-FSNOCS(I))*ACIDAT(I,J)
              ENDIF
              SVF=EXP(CANEXT(J)*PAIS(I,J))
              ALVSS=(1.0-SVF)*ALVSCX+SVF*TRVS(I)*ALVSSC(I)
              ALIRS=(1.0-SVF)*ALIRCX+SVF*TRIR(I)*ALIRSC(I)
              ALVSCS(I)=ALVSCS(I)+FCANS(I,J)*ALVSS
              ALIRCS(I)=ALIRCS(I)+FCANS(I,J)*ALIRS
          ENDIF
  550 CONTINUE
C
C     * BROADLEAF TREES.
C
      J=2
      DO 600 I=IL1,IL2                                                                                  
          IF(COSZS(I).GT.0. .AND. FCANS(I,J).GT.0.)               THEN
              TRCLRV=MIN(EXP(-0.7*PAIS(I,J)),EXP(-0.4/COSZS(I)))                   
              TRCLDV=0.30*MIN(EXP(-0.7*PAIS(I,J)),EXPMAX1)             
     1              +0.50*MIN(EXP(-0.7*PAIS(I,J)),EXPMAX2)              
     2              +0.20*MIN(EXP(-0.7*PAIS(I,J)),EXPMAX3)              
              TRCLRT=MIN(EXP(-0.4*PAIS(I,J)),EXP(-0.4/COSZS(I)))                   
              TRCLDT=0.30*MIN(EXP(-0.4*PAIS(I,J)),EXPMAX1)+            
     1               0.50*MIN(EXP(-0.4*PAIS(I,J)),EXPMAX2)+              
     2               0.20*MIN(EXP(-0.4*PAIS(I,J)),EXPMAX3)               
              TRVS(I)=FCLOUD(I)*TRCLDV+(1.0-FCLOUD(I))*TRCLRV
              TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
              TRIR(I)= 2.*TRTOT-TRVS(I)
              TRVSCS(I)=TRVSCS(I)+FCANS(I,J)*TRVS(I)
              TRIRCS(I)=TRIRCS(I)+FCANS(I,J)*TRIR(I)
          ENDIF
  600 CONTINUE
C
      DO 650 I=IL1,IL2                                               
          IF(COSZS(I).GT.0. .AND. FCANS(I,J).GT.0.)             THEN
              ALIRWC=ALIRC(I,J)+0.04
              IF(IALC.EQ.0) THEN
                  ALVSCX=FSNOCS(I)*ALVSWC+(1.0-FSNOCS(I))*ALVSC(I,J)
                  ALIRCX=FSNOCS(I)*ALIRWC+(1.0-FSNOCS(I))*ALIRC(I,J)
              ELSE
                  ALVSCX=FSNOCS(I)*ALVSWC+(1.0-FSNOCS(I))*ACVDAT(I,J)
                  ALIRCX=FSNOCS(I)*ALIRWC+(1.0-FSNOCS(I))*ACIDAT(I,J)
              ENDIF
              SVF=EXP(CANEXT(J)*PAIS(I,J))
              ALVSS=(1.0-SVF)*ALVSCX+SVF*TRVS(I)*ALVSSC(I)
              ALIRS=(1.0-SVF)*ALIRCX+SVF*TRIR(I)*ALIRSC(I)
              ALVSCS(I)=ALVSCS(I)+FCANS(I,J)*ALVSS
              ALIRCS(I)=ALIRCS(I)+FCANS(I,J)*ALIRS
          ENDIF
  650 CONTINUE
C
C     * CROPS AND GRASS.
C
      DO 700 J=3,IC
      DO 700 I=IL1,IL2                                                                                  
          IF(COSZS(I).GT.0. .AND. FCANS(I,J).GT.0.)               THEN
              TRCLRV=EXP(-0.5*PAIS(I,J)/COSZS(I))                                    
              TRCLDV=0.30*EXP(-0.5*PAIS(I,J)/0.9659)+0.50*EXP(-0.5*               
     1               PAIS(I,J)/0.7071)+0.20*EXP(-0.5*PAIS(I,J)/0.2588)
              TRCLRT=EXP(-0.4*PAIS(I,J)/COSZS(I))                                    
              TRCLDT=0.30*EXP(-0.4*PAIS(I,J)/0.9659)+0.50*EXP(-0.4*              
     1               PAIS(I,J)/0.7071)+0.20*EXP(-0.4*PAIS(I,J)/0.2588)                
              TRVS(I)=FCLOUD(I)*TRCLDV+(1.0-FCLOUD(I))*TRCLRV
              TRTOT =FCLOUD(I)*TRCLDT+(1.0-FCLOUD(I))*TRCLRT
              TRIR(I)= 2.*TRTOT-TRVS(I)
              TRVSCS(I)=TRVSCS(I)+FCANS(I,J)*TRVS(I)
              TRIRCS(I)=TRIRCS(I)+FCANS(I,J)*TRIR(I)
          ENDIF
  700 CONTINUE
C
      DO 750 J=3,IC
      DO 750 I=IL1,IL2                                                     
          IF(COSZS(I).GT.0. .AND. FCANS(I,J).GT.0.)             THEN
              ALIRWC=ALIRC(I,J)+0.04
              IF(IALC.EQ.0) THEN
                  ALVSCX=FSNOCS(I)*ALVSWC+(1.0-FSNOCS(I))*ALVSC(I,J)
                  ALIRCX=FSNOCS(I)*ALIRWC+(1.0-FSNOCS(I))*ALIRC(I,J)
              ELSE
                  ALVSCX=FSNOCS(I)*ALVSWC+(1.0-FSNOCS(I))*ACVDAT(I,J)
                  ALIRCX=FSNOCS(I)*ALIRWC+(1.0-FSNOCS(I))*ACIDAT(I,J)
              ENDIF
              SVF=EXP(CANEXT(J)*PAIS(I,J))
              ALVSS=(1.0-SVF)*ALVSCX+SVF*TRVS(I)*ALVSSC(I)
              ALIRS=(1.0-SVF)*ALIRCX+SVF*TRIR(I)*ALIRSC(I)
              ALVSCS(I)=ALVSCS(I)+FCANS(I,J)*ALVSS
              ALIRCS(I)=ALIRCS(I)+FCANS(I,J)*ALIRS
          ENDIF 
  750 CONTINUE                                      
C
C     * TOTAL ALBEDOS AND CONSISTENCY CHECKS.
C
      IPTBAD=0                           
      DO 775 I=IL1,IL2
          IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                 THEN
              ALVSCS(I)=ALVSCS(I)/FCS(I)                                                        
              ALIRCS(I)=ALIRCS(I)/FCS(I)
          ENDIF
          IF(ALVSCS(I).GT.1. .OR. ALVSCS(I).LT.0.) IPTBAD=I
          IF(ALIRCS(I).GT.1. .OR. ALIRCS(I).LT.0.) IPTBAD=I
  775 CONTINUE                                                                
C
C     * TOTAL TRANSMISSIVITIES AND CONSISTENCY CHECKS.
C
      IPTBAD=0
      JPTBAD=0
      DO 800 I=IL1,IL2
          IF(FCS(I).GT.0. .AND. COSZS(I).GT.0.)                   THEN
              TRVSCS(I)=TRVSCS(I)/FCS(I)
              TRIRCS(I)=TRIRCS(I)/FCS(I)
              TRVSCS(I)=MIN( TRVSCS(I), 0.90*(1.0-ALVSCS(I)) )
              TRIRCS(I)=MIN( TRIRCS(I), 0.90*(1.0-ALIRCS(I)) )
          ENDIF
          IF(TRVSCS(I).GT.1. .OR. TRVSCS(I).LT.0.) IPTBAD=I
          IF(TRIRCS(I).GT.1. .OR. TRIRCS(I).LT.0.) IPTBAD=I
          IF((1.-ALVSCN(I)-TRVSCN(I)).LT.0.)     THEN
              JPTBAD=1000+I
              JPTBDI=I
          ENDIF  
          IF((1.-ALVSCS(I)-TRVSCS(I)).LT.0.)     THEN
              JPTBAD=2000+I
              JPTBDI=I
          ENDIF
          IF((1.-ALIRCN(I)-TRIRCN(I)).LT.0.)     THEN
              JPTBAD=3000+I
              JPTBDI=I
          ENDIF
          IF((1.-ALIRCS(I)-TRIRCS(I)).LT.0.)     THEN
              JPTBAD=4000+I
              JPTBDI=I
          ENDIF
  800 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
          WRITE(6,6400) IPTBAD,JL,TRVSCS(IPTBAD),TRIRCS(IPTBAD)
 6400     FORMAT('0AT (I,J)= (',I3,',',I3,'), TRVSCS,TRIRCS = ',2F10.5)
          CALL XIT('CANALB',-4)    
      ENDIF
C
      IF(IPTBAD.NE.0) THEN
          WRITE(6,6200) IPTBAD,JL,ALVSCS(IPTBAD),ALIRCS(IPTBAD)
 6200     FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSCS,ALIRCS = ',2F10.5)
          CALL XIT('CANALB',-2)    
      ENDIF                                                                                  
C
      IF(JPTBAD.NE.0) THEN
          WRITE(6,6500) JPTBDI,JL,JPTBAD
 6500     FORMAT('0AT (I,J)= (',I3,',',I3,'), JPTBAD =  ',I5)
          CALL XIT('CANALB',-5)    
      ENDIF                      
C-----------------------------------------------------------------------
C
C     * BULK STOMATAL RESISTANCES FOR CANOPY OVERLYING SNOW AND CANOPY
C     * OVERLYING BARE SOIL.
C
      DO 850 I=IL1,IL2
          IF((FCS(I)+FC(I)).GT.0.0)                               THEN
              IF(TA(I).LE.268.15)                          THEN
                  RCT(I)=250.
              ELSEIF(TA(I).LT.278.15)                      THEN
                  RCT(I)=1./(1.-(278.15-TA(I))*.1)
              ELSEIF(TA(I).GT.313.15)                      THEN
                  IF(TA(I).GE.323.15)               THEN
                      RCT(I)=250.
                  ELSE
                      RCT(I)=1./(1.-(TA(I)-313.15)*0.1)
                  ENDIF
              ELSE
                  RCT(I)=1.
              ENDIF
          ENDIF
850   CONTINUE
C
      DO 900 J=1,IC
      DO 900 I=IL1,IL2
          IF(FCAN(I,J).GT.0.)                                     THEN
              IF(VPD(I).GT.0. .AND. VPDA(I,J).GT.0.0)          THEN
                  IF(ABS(VPDB(I,J)).GT.1.0E-5)       THEN
                      RCV(I,J)=MAX(1.,((VPD(I)/10.)**VPDB(I,J))/
     1                         VPDA(I,J))
                  ELSE
                      RCV(I,J)=1./EXP(-VPDA(I,J)*VPD(I)/10.)
                  ENDIF
              ELSE
                  RCV(I,J)=1.0
              ENDIF
              IF(PSIGA(I,J).GT.0.0)                            THEN
                  RCG(I,J)=1.+(PSIGND(I)/PSIGA(I,J))**PSIGB(I,J)
              ELSE
                  RCG(I,J)=1.0
              ENDIF
              IF(QSWINV(I).GT.0. .AND. COSZS(I).GT.0. .AND.
     1            CXTEFF(I,J).GT.1.0E-5 .AND. RCG(I,J).LT.1.0E5)  THEN
                RCACC(I,J)=MIN(CXTEFF(I,J)*RSMIN(I,J)/LOG((QSWINV(I)+
     1            QA50(I,J)/CXTEFF(I,J))/(QSWINV(I)*EXP(-CXTEFF(I,J)*
     2            PAI(I,J))+QA50(I,J)/CXTEFF(I,J)))*RCV(I,J)*RCG(I,J)*
     3            RCT(I),5000.)
                RCACC(I,J)=MAX(RCACC(I,J),10.0)
              ELSE
                RCACC(I,J)=5000.
              ENDIF
              RC(I)=RC(I)+FCAN(I,J)/RCACC(I,J)
          ENDIF
900   CONTINUE
C
      DO 950 I=IL1,IL2   
          IF((FCS(I)+FC(I)).GT.0.)                                THEN
              FRMAX=0.0
              DO 925 J=1,IG
                  FRMAX=MAX(FRMAX,FROOT(I,J))
925           CONTINUE
              IF(FRMAX .LT. 1.0E-6)                           THEN
                  RCS(I)=1.0E+20
                  RC(I) =1.0E+20
              ELSEIF(QSWINV(I).LT.2.0)                        THEN
                  RCS(I)=5000.0
                  RC(I)=5000.0  
              ELSE                                                                
                  RCS(I)=5000.0
                  IF(RC(I).GT.0) THEN
                      RC(I)=FC(I)/RC(I)
                  ELSE
                      RC(I)=5000.0
                  ENDIF
              ENDIF                                                               
          ELSE
              RC(I)=0.0
              RCS(I)=0.0
          ENDIF                                                                   
  950 CONTINUE                                                                    
C                                                                                 
      RETURN                                                                      
      END
