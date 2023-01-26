      SUBROUTINE WFLOW(WMOVE,TMOVE,LZF,NINF,TRMDR,TPOND,ZPOND,
     1                 R,TR,EVAP,PSIF,GRKINF,THLINF,THLIQX,TBARWX,
     2                 DELZX,ZBOTX,FMAX,ZF,DZF,DTFLOW,THLNLZ,
     3                 THLQLZ,DZDISP,WDISP,WABS,ITER,NEND,ISIMP,
     4                 IGRN,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
                                                                         
C     * FEB 09/09 - J.P.BLANCHETTE. INCREASE LIMITING VALUE OF NEND.
C     * JAN 06/09 - D.VERSEGHY. CHECKS ON WETTING FRONT LOCATION
C     *                         IN 500 LOOP.
C     * AUG 07/07 - D.VERSEGHY. INCREASE ITERATION COUNTER NEND;
C     *                         MOVE CALCULATION OF FMAX FROM
C     *                         GRINFL TO THIS ROUTINE.
C     * MAY 17/06 - D.VERSEGHY. PROTECT AGAINST DIVISIONS BY ZERO.
C     * SEP 13/05 - D.VERSEGHY. REPLACE HARD-CODED 4 WITH IGP1.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 30/04 - D.VERSEGHY/Y.DELAGE. PROTECT SENSITIVE 
C     *                         CALCULATIONS AGAINST ROUNDOFF ERRORS.
C     * JUN 21/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL.
C     * MAR 04/02 - D.VERSEGHY. DEFINE "NEND" FOR ALL CASES.
C     * DEC 16/94 - D.VERSEGHY. BUG FIX - SPECIFY TMOVE BEHIND
C     *                         WETTING FRONT AFTER ANY FULL-LAYER 
C     *                         JUMP DOWNWARD.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. SATURATED FLOW OF WATER THROUGH SOIL.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,NPNTS,NIT,N
C
C     * INPUT/OUTPUT FIELDS.
C                      
      REAL WMOVE (ILG,IGP2),   TMOVE (ILG,IGP2)
C
      INTEGER                  LZF   (ILG),        NINF  (ILG) 
C
      REAL TRMDR (ILG),        TPOND (ILG),        ZPOND (ILG)
C
C     * INPUT FIELDS.
C
      REAL R     (ILG),        TR    (ILG),        EVAP  (ILG),
     1     PSIF  (ILG,IGP1),   GRKINF(ILG,IGP1),   THLINF(ILG,IGP1),
     2     THLIQX(ILG,IGP1),   TBARWX(ILG,IGP1),   DELZX (ILG,IGP1),
     3     ZBOTX (ILG,IGP1),   FMAX  (ILG),        ZF    (ILG)
C
      INTEGER                  IGRN  (ILG)
C
C     * INTERNAL WORK FIELDS.
C
      REAL DZF   (ILG),        DTFLOW(ILG),        THLNLZ(ILG),        
     1     THLQLZ(ILG),        DZDISP(ILG),        WDISP (ILG),        
     2     WABS  (ILG)
C
      INTEGER                  ITER  (ILG),        NEND  (ILG),
     1                         ISIMP (ILG)    
C 
C     * TEMPORARY VARIABLES.
C
      REAL RESID,FINF,ZPTEST,WINF
C 
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ
C
      COMMON /CLASS1/ DELT,TFREZ
C-----------------------------------------------------------------------
C
C     * CALCULATE ITERATION ENDPOINT "NEND", AND SET SWITCH "ITER" TO 1
C     * FOR POINTS OVER WHICH THIS SUBROUTINE IS TO BE PERFORMED.
C
      DO 50 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. TRMDR(I).GT.0.0)                    THEN
              RESID=MOD(TRMDR(I),120.)                                                      
              IF(RESID.GT.0.)                       THEN  
                  NEND(I)=NINT(TRMDR(I)/120.+0.5)+100
              ELSE                                                                        
                  NEND(I)=NINT(TRMDR(I)/120.)+100
              ENDIF
              ITER(I)=1
              FMAX(I)=999999. 
          ELSE
              NEND(I)=0  
              ITER(I)=0  
          ENDIF
   50 CONTINUE
      NIT=1
C                     
      DO 75 J=1,IGP1
      DO 75 I=IL1,IL2
          IF(ITER(I).GT.0 .AND. LZF(I).GT.1 .AND. J.LT.LZF(I))     THEN   
              FMAX(I)=MIN(GRKINF(I,J),FMAX(I))                                      
          ENDIF                                                                   
   75 CONTINUE
C
  100 CONTINUE

C     * BEGINNING OF ITERATION SEQUENCE.
C     * SET OR RESET NUMBER OF POINTS TO BE PROCESSED ON THE CURRENT 
C     * LATITUDE CIRCLE(S).
C
      NPNTS=0                                  
C
C     * IF THE WATER CONTENT OF THE CURRENT SOIL LAYER EQUALS OR EXCEEDS
C     * THE WATER CONTENT BEHIND THE WETTING FRONT, INSTANTANEOUSLY 
C     * RELOCATE WETTING FRONT TO BOTTOM OF CURRENT SOIL LAYER; 
C     * RE-EVALUATE INFILTRATION PARAMETERS, UPDATE WATER MOVEMENT 
C     * MATRIX, SET SWITCH "ISIMP" TO 1 AND DROP TO END OF ITERATION 
C     * LOOP.
C     * (SOME ARRAYS ARE GATHERED (ON LZF) TO AVOID MULTIPLE INDIRECT-
C     * ADDRESSING REFERENCES IN THE ENSUING LOOPS.)
C
      DO 200 I=IL1,IL2
          IF(ITER(I).EQ.1)                                          THEN
              THLNLZ(I)=THLINF(I,LZF(I))
              THLQLZ(I)=THLIQX(I,LZF(I))
              IF(THLQLZ(I).GT.(THLNLZ(I)-1.0E-6) .AND. LZF(I).LT.IGP1
     1                .AND. THLQLZ(I).GT.0.0001)               THEN             
                  ZF(I)=ZBOTX(I,LZF(I))                                                   
                  WMOVE(I,NINF(I))=THLQLZ(I)*DELZX(I,LZF(I))                              
                  TMOVE(I,NINF(I))=TBARWX(I,LZF(I))                              
                  FINF=GRKINF(I,LZF(I))*(ZF(I)+ZPOND(I)+PSIF(I,LZF(I)))/
     1                 ZF(I)                        
                  FMAX(I)=MIN(FMAX(I),FINF)                                      
                  LZF(I)=LZF(I)+1                                                       
                  NINF(I)=NINF(I)+1
                  ISIMP(I)=1                                                     
              ELSE                                            
                  ISIMP(I)=0
              ENDIF
          ELSE
              ISIMP(I)=0
          ENDIF
  200 CONTINUE
C
C     * INFILTRATION CALCULATIONS TAKING FINITE TIME. SET TIMESTEP OF
C     * CURRENT ITERATION PASS AND CHECK HYDRAULIC CONDUCTIVITY OF
C     * CURRENT SOIL LAYER. IF ZERO, RECALCULATE POND DEPTH AND POND
C     * TEMPERATURE AND SET "ISIMP" TO -1; ELSE SET "ISIMP" TO -2.
C
      DO 300 I=IL1,IL2
          IF(ITER(I).EQ.1 .AND. ISIMP(I).NE.1)                      THEN
              DTFLOW(I)=MIN(TRMDR(I),120.)
              IF(GRKINF(I,LZF(I)).GT.1.0E-12)                   THEN
                  ISIMP(I)=-2
              ELSE
                  TPOND(I)=(ZPOND(I)*TPOND(I)+R(I)*DTFLOW(I)*TR(I))/
     1                     (ZPOND(I)+R(I)*DTFLOW(I))
                  IF(ABS(R(I)-EVAP(I)).GT.1.0E-11)            THEN                                 
                      ZPTEST=ZPOND(I)+(R(I)-EVAP(I))*DTFLOW(I)                               
                  ELSE                                                        
                      ZPTEST=ZPOND(I)                                            
                  ENDIF                                                       
                  IF(ZPTEST.LT.0.)                        THEN                                      
                      DTFLOW(I)=ZPOND(I)/(EVAP(I)-R(I))                                       
                      ZPOND(I)=0.0                                               
                  ELSE                                                        
                      ZPOND(I)=ZPTEST                                            
                  ENDIF                                                       
                  ISIMP(I)=-1
              ENDIF
          ENDIF
  300 CONTINUE
C
C     * "ISIMP"=-2: NORMAL SATURATED INFILTRATION UNDER PISTON-FLOW
C     * CONDITIONS. CALCULATE CURRENT INFILTRATION RATE (FINF); WATER
C     * INFILTRATING DURING CURRENT ITERATION PASS (WINF); SOIL WATER
C     * DISPLACED INTO EMPTY PORES AHEAD OF WETTING FRONT (WDISP);
C     * AND SOIL WATER OVERTAKEN BY DISPLACED SOIL WATER (WABS).
C     * RE-EVALUATE POND TEMPERATURE AND POND DEPTH; UPDATE WATER
C     * MOVEMENT MATRIX; ADJUST CURRENT POSITION OF WETTING FRONT.
C
      DO 400 I=IL1,IL2
          IF(ITER(I).EQ.1 .AND. ISIMP(I).EQ.-2)                     THEN
              IF(LZF(I).LT.IGP1)                            THEN  
                  IF(ZF(I).GT.1.0E-7)                THEN
                      FINF=GRKINF(I,LZF(I))*(ZF(I)+ZPOND(I)+
     1                     PSIF(I,LZF(I)))/ZF(I)    
                  ELSE                                                    
                      FINF=GRKINF(I,1)                                      
                  ENDIF                                                   
              ELSE
                  FINF=GRKINF(I,LZF(I))*(ZF(I)+ZPOND(I))/ZF(I)                 
              ENDIF
              IF(ZPOND(I).LT.1.0E-8 .AND. FINF.GT.R(I))      FINF=R(I)
              IF(FINF.GT.FMAX(I)) FINF=FMAX(I)                              
              WINF=FINF*DTFLOW(I)                                            
              IF(LZF(I).LT.IGP1)                            THEN
                  DZF(I)=WINF/THLNLZ(I)                                    
                  WDISP(I)=DZF(I)*THLQLZ(I)                                   
                  DZDISP(I)=WDISP(I)/(THLNLZ(I)-THLQLZ(I))                  
                  WABS(I)=DZDISP(I)*THLQLZ(I)                                 
                  IF((ZF(I)+DZF(I)+DZDISP(I)).GT.ZBOTX(I,LZF(I))) THEN
                     DTFLOW(I)=(ZBOTX(I,LZF(I))-ZF(I))/
     1                         (FINF/THLNLZ(I)+
     2                         (FINF*THLQLZ(I))/
     3                         (THLNLZ(I)*               
     4                         (THLNLZ(I)-THLQLZ(I))))                     
                     WINF=FINF*DTFLOW(I)          
                     DZF(I)=WINF/THLNLZ(I)                                
                     WDISP(I)=DZF(I)*THLQLZ(I)                               
                     DZDISP(I)=WDISP(I)/(THLNLZ(I)-THLQLZ(I))              
                     WABS(I)=DZDISP(I)*THLQLZ(I)                             
                  ENDIF                                                   
              ENDIF
              IF(ZPOND(I)+R(I)*DTFLOW(I).GT.1.0E-8)          THEN
                  TPOND(I)=(ZPOND(I)*TPOND(I)+R(I)*DTFLOW(I)*TR(I))/
     1                     (ZPOND(I)+R(I)*DTFLOW(I))
              ENDIF
              IF(ABS(R(I)-FINF-EVAP(I)).GT.1.0E-11)          THEN 
                  ZPTEST=ZPOND(I)+R(I)*DTFLOW(I)-WINF-EVAP(I)*
     1                   DTFLOW(I)                      
              ELSE                                                    
                  ZPTEST=ZPOND(I)                                        
              ENDIF                                                   
              IF(ZPTEST.LT.0.)                               THEN  
                  DTFLOW(I)=ZPOND(I)/(FINF+EVAP(I)-R(I))      
                  WINF=FINF*DTFLOW(I)                                        
                  ZPOND(I)=0.0                                           
                  IF(LZF(I).LT.IGP1)                    THEN
                     DZF(I)=WINF/THLNLZ(I)                                
                     WDISP(I)=DZF(I)*THLQLZ(I)                               
                     DZDISP(I)=WDISP(I)/(THLNLZ(I)-THLQLZ(I))              
                     WABS(I)=DZDISP(I)*THLQLZ(I)                             
                  ENDIF
              ELSE                                                    
                  ZPOND(I)=ZPTEST                                        
              ENDIF                                                   
              IF((WMOVE(I,1)+WINF).GT.0.)                    THEN 
                 TMOVE(I,1)=(WMOVE(I,1)*TMOVE(I,1)+WINF*TPOND(I))/            
     1                      (WMOVE(I,1)+WINF)                                 
              ENDIF                                                   
              WMOVE(I,1)=WMOVE(I,1)+WINF                                  
          ENDIF
  400 CONTINUE
C
C     * (THIS PORTION OF THE ABOVE DO-LOOP WAS SPLIT OFF ON THE CRAY
C     * BECAUSE IT WOULD NOT VECTORIZE. ONE MIGHT TRY AND RE-COMBINE 
C     * IT ON THE SX-3 (GOES IN FIRST PART OF IF BLOCK)).
C
      DO 450 I=IL1,IL2
          IF(ITER(I).EQ.1 .AND. ISIMP(I).EQ.-2 .AND.
     1       LZF(I).LT.IGP1)                                        THEN   
              WMOVE(I,NINF(I))=WMOVE(I,NINF(I))+WDISP(I)+WABS(I)                      
              ZF(I)=ZF(I)+DZF(I)+DZDISP(I)
          ENDIF
  450 CONTINUE  
C
C     * CALCULATE REMAINING ITERATION TIME; RE-EVALUATE INFILTRATION
C     * PARAMETERS.
C
      DO 500 I=IL1,IL2
          IF(ITER(I).EQ.1 .AND. ISIMP(I).NE.1)                      THEN
              TRMDR(I)=TRMDR(I)-DTFLOW(I)                                                  
              IF(ABS(ZF(I)-ZBOTX(I,LZF(I))).LT.1.0E-6 .AND.
     1                TRMDR(I).GT.0. .AND. LZF(I).LT.IGP1 .AND.
     1                THLQLZ(I).GT.0.0001)                       THEN             
                  FINF=GRKINF(I,LZF(I))*(ZBOTX(I,LZF(I))+ZPOND(I)+
     1                 PSIF(I,LZF(I)))/ZBOTX(I,LZF(I))
                  FMAX(I)=MIN(FMAX(I),FINF)                                              
                  LZF(I)=LZF(I)+1                                                   
                  NINF(I)=NINF(I)+1                                                 
                  TMOVE(I,NINF(I))=TBARWX(I,LZF(I))                                     
              ENDIF                                                           
          ENDIF                                                                   
  500 CONTINUE
C
C     * INCREMENT ITERATION COUNTER ("NIT") AND SEE IF ANY POINTS STILL
C     * REMAIN TO BE DONE (USING "NPNTS"). IF SO, RETURN TO BEGINNING 
C     * TO COMPLETE THESE REMAINING POINTS.
C
      NIT=NIT+1
C
      DO 600 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN 
              IF(NIT.LE.NEND(I) .AND. ITER(I).EQ.1 .AND.
     1           (ZPOND(I).GT.1.0E-8 .OR. R(I).GT.0.)  .AND.
     2           TRMDR(I).GT.0.)                           THEN
                  NPNTS=NPNTS+1
              ELSE
                  ITER(I)=0
              ENDIF
          ENDIF
  600 CONTINUE
C
      IF(NPNTS.GT.0)                                         GO TO 100                       
C                                                                                  
      RETURN                                                                      
      END        
