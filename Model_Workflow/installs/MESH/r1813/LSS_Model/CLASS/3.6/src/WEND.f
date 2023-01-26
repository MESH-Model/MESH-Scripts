      SUBROUTINE WEND(THLIQX,THICEX,TBARWX,ZPOND,TPOND,
     1                BASFLW,TBASFL,RUNOFF,TRUNOF,FI,
     2                WMOVE,TMOVE,LZF,NINF,TRMDR,THLINF,DELZX,
     3                ZMAT,ZRMDR,FDTBND,WADD,TADD,FDT,TFDT,
     4                THLMAX,THTEST,THLDUM,THIDUM,TDUMW,
     5                TUSED,RDUMMY,ZERO,WEXCES,XDRAIN,
     6                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     7                THFC,DELZW,ISAND,IGRN,IGRD,IGDR,IZERO,
     8                IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )

C     * OCT 18/11 - M.LAZARE.   PASS IN "IGDR" AS AN INPUT FIELD 
C     *                         (ORIGINATING IN CLASSB) RATHER
C     *                         THAN REPEATING THE CALCULATION HERE
C     *                         AS AN INTERNAL WORK FIELD.
C     * DEC 15/10 - D.VERSEGHY. ALLOW FOR BASEFLOW WHEN BEDROCK
C     *                         LIES WITHIN SOIL PROFILE.
C     * JAN 06/09 - D.VERSEGHY. ADD ZPOND AND TPOND TO SUBROUTINE
C     *                         CALL; ASSIGN RESIDUAL OF WMOVE TO
C     *                         PONDED WATER; REVISE LOOP 550;
C     *                         DELETE CALCULATION OF FDTBND.
C     * MAY 17/06 - D.VERSEGHY. PROTECT AGAINST DIVISIONS BY ZERO.
C     * OCT 21/05 - D.VERSEGHY. FIX MINOR BUGS IN CLEANUP AND
C     *                         RUNOFF TEMPERATURE CALCULATION.
C     * MAR 23/05 - D.VERSEGHY. ADD VARIABLES TO GRDRAN CALL;
C     *                         ADD CALCULATION OF RUNOFF
C     *                         TEMPERATURE.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * APR 24/03 - D.VERSEGHY. ADD CHECK FOR OVERFLOW IN SOIL
C     *                         LAYER CONTAINING WETTING FRONT.
C     * OCT 15/02 - D.VERSEGHY. BUGFIX IN CALCULATION OF FDTBND
C     *                         (PRESENT ONLY IN PROTOTYPE
C     *                         VERSIONS OF CLASS VERSION 3.0).
C     * JUN 21/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. ADD SEPARATE CALCULATION OF BASEFLOW
C     *                         AT BOTTOM OF SOIL COLUMN.
C     * OCT 20/97 - D.VERSEGHY. APPLY ACCURACY LIMIT ON FLOWS IN AND
C     *                         OUT OF LAYER CONTAINING WETTING FRONT,
C     *                         IN ORDER TO ENSURE MOISTURE CONSERVATION.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * APR 24/92 - D.VERSEGHY,M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. RECALCULATE LIQUID MOISTURE CONTENT
C     *                         OF SOIL LAYERS AFTER INFILTRATION
C     *                         AND EVALUATE FLOW ("RUNOFF") FROM
C     *                         BOTTOM OF SOIL COLUMN.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,K,N
C
C     * OUTPUT FIELDS.
C
      REAL THLIQX(ILG,IGP1), THICEX(ILG,IGP1), TBARWX(ILG,IGP1), 
     1     ZPOND (ILG),      TPOND (ILG),      BASFLW(ILG),      
     2     TBASFL(ILG),      RUNOFF(ILG),      TRUNOF(ILG)
C
C     * INPUT FIELDS.
C
      REAL WMOVE (ILG,IGP2), TMOVE (ILG,IGP2), THLINF(ILG,IGP1),
     1     FI    (ILG),      TRMDR (ILG),      DELZX (ILG,IGP1)
C
      INTEGER                LZF   (ILG),      NINF  (ILG), 
     1                       IGRN  (ILG)    
C
C     * INTERNAL WORK ARRAYS.
C
      REAL ZMAT  (ILG,IGP2,IGP1),  ZRMDR (ILG,IGP1)
C
      REAL FDTBND(ILG),    WADD  (ILG),    TADD  (ILG) 
C
C     * INTERNAL ARRAYS USED IN CALLING GRDRAN.
C
      REAL FDT   (ILG,IGP1), TFDT  (ILG,IGP1)
C
      REAL THLMAX(ILG,IG), THTEST(ILG,IG), THLDUM(ILG,IG),
     1     THIDUM(ILG,IG), TDUMW (ILG,IG)          
C
      REAL TUSED (ILG),    RDUMMY(ILG),    ZERO  (ILG),
     1     WEXCES(ILG)
C
      INTEGER              IGRD  (ILG),    IZERO (ILG),
     1                     IGDR  (ILG) 
C
C     * TEMPORARY VARIABLES.
C
      REAL WREM,TREM,THDRAN,THINFL,WDRA,TDRA
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG), THLRET(ILG,IG), THLMIN(ILG,IG), 
     1     BI    (ILG,IG), PSISAT(ILG,IG), GRKSAT(ILG,IG), 
     2     THFC  (ILG,IG), DELZW (ILG,IG), XDRAIN(ILG)
C  
      INTEGER              ISAND (ILG,IG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
C     * DETERMINE AMOUNT OF TIME OUT OF CURRENT MODEL STEP DURING WHICH 
C     * INFILTRATION WAS OCCURRING.
C     * SET WORK ARRAY "TUSED" TO ZERO FOR POINTS WHERE WETTING FRONT
C     * IS BELOW BOTTOM OF LOWEST SOIL LAYER TO SUPPRESS CALCULATIONS
C     * DONE IN "GRDRAN".
C
      DO 100 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. LZF(I).LE.IG)                       THEN
              TUSED(I)=DELT-TRMDR(I)
          ELSE
              TUSED(I)=0.
          ENDIF
  100 CONTINUE
C
C     * INITIALIZATION.
C
      DO 125 J=1,IG
      DO 125 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              THLDUM(I,J)=THLIQX(I,J)                                                 
              THIDUM(I,J)=THICEX(I,J)                                                 
              TDUMW (I,J)=TBARWX(I,J)                  
          ENDIF
  125 CONTINUE 
C
C     * CALL "GRDRAN" WITH COPIES OF CURRENT LIQUID AND FROZEN SOIL
C     * MOISTURE CONTENTS AND LAYER TEMPERATURES TO DETERMINE MOISTURE
C     * FLOW BETWEEN LAYERS BELOW THE WETTING FRONT.
C
      CALL GRDRAN(IVEG,THLDUM,THIDUM,TDUMW,FDT,TFDT,RDUMMY,RDUMMY,
     1            RDUMMY,RDUMMY,RDUMMY,RDUMMY,FI,ZERO,ZERO,ZERO,
     2            TUSED,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     3            BI,PSISAT,GRKSAT,THFC,DELZW,XDRAIN,ISAND,LZF,
     4            IZERO,IGRD,IGDR,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
C
C     * INITIALIZATION OF ARRAYS IN PREPARATION FOR RE-ALLOCATION OF
C     * MOISTURE STORES WITHIN SOIL LAYERS; SUPPRESS WATER FLOWS 
C     * CALCULATED IN GRDRAN ABOVE WETTING FRONT; CONSISTENCY CHECK
C     * FOR WATER FLOWS INTO LAYER CONTAINING WETTING FRONT.
C
      DO 150 I=IL1,IL2
          IF(IGRN(I).GT.0)                                       THEN
               NINF(I)=MIN(NINF(I),IGP1)                                                        
          ENDIF                                                                       
  150 CONTINUE
C
      DO 200 J=IGP1,1,-1                                                           
      DO 200 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              ZRMDR(I,J)=DELZX(I,J)
              IF(J.LE.LZF(I))                                 THEN
                  FDT(I,J)=0.0
              ENDIF
              IF(J.LT.IGP1)                                   THEN
                  IF(J.EQ.LZF(I).AND.FDT(I,J+1).LT.0.)  THEN
                      FDT(I,J+1)=0.0
                  ENDIF
              ENDIF
          ENDIF
  200 CONTINUE
C
      DO 300 J=1,IGP1                                                             
      DO 300 K=1,IGP1
      DO 300 I=IL1,IL2
          IF(IGRN(I).GT.0)                       THEN
              ZMAT(I,K,J)=0.0                                                       
          ENDIF
  300 CONTINUE                        
C
C     * ASSIGN VALUES IN MATRIX "ZMAT": DETERMINE DEPTH OUT OF EACH
C     * SOIL LAYER J WHICH IS FILLED BY WATER FROM RESERVOIR K
C     * IN "WMOVE"; FIND THE DEPTH "ZRMDR" LEFT OVER WITHIN EACH
C     * SOIL LAYER.
C
      DO 400 K=1,IGP1
      DO 400 J=1,IGP1
      DO 400 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                       THEN
              IF(ZRMDR(I,J).GT.1.0E-5 .AND. WMOVE(I,K).GT.0.) THEN                        
                  ZMAT(I,K,J)=WMOVE(I,K)/THLINF(I,J)                                    
                  IF(ZMAT(I,K,J).GT.ZRMDR(I,J)) THEN                                  
                      ZMAT(I,K,J)=ZRMDR(I,J)                                          
                      WMOVE(I,K)=WMOVE(I,K)-ZRMDR(I,J)*THLINF(I,J)                        
                      ZRMDR(I,J)=0.0                                                
                  ELSE                                                            
                      ZRMDR(I,J)=ZRMDR(I,J)-ZMAT(I,K,J)                                 
                      WMOVE(I,K)=0.0                                                
                  ENDIF                                                           
              ENDIF                                                               
          ENDIF
  400 CONTINUE
C
      DO 450 J=1,IGP1
      DO 450 I=IL1,IL2
          IF(IGRN(I).GT.0. AND. WMOVE(I,J).GT.0.0)                  THEN
              TPOND(I)=(TPOND(I)*ZPOND(I)+TMOVE(I,J)*WMOVE(I,J))/
     1            (ZPOND(I)+WMOVE(I,J))
              ZPOND(I)=ZPOND(I)+WMOVE(I,J)
          ENDIF
 450  CONTINUE
C
C     * ADD WATER CONTENT AND TEMPERATURE CHANGES DUE TO INFILTRATION
C     * (WADD, TADD) AND DRAINAGE (WDRA, TDRA) TO WATER REMAINING IN
C     * EACH SOIL LAYER AFTER THESE PROCESSES (WREM, TREM).
C
      DO 600 J=IG,1,-1
          DO 500 I=IL1,IL2
              IF(IGRN(I).GT.0)                                      THEN
                  WADD(I)=0.
                  TADD(I)=0.
              ENDIF
  500     CONTINUE
C  
          DO 525 K=1,IGP1
          DO 525 I=IL1,IL2
              IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                   THEN
                  WADD(I)=WADD(I)+THLINF(I,J)*ZMAT(I,K,J)                                       
                  TADD(I)=TADD(I)+TMOVE(I,K)*THLINF(I,J)*ZMAT(I,K,J)
              ENDIF
  525     CONTINUE
C
          DO 550 I=IL1,IL2
              IF(IGRN(I).GT.0 .AND. DELZW(I,J).GT.1.0E-4)           THEN
                 IF(ZRMDR(I,J).GT.1.0E-5)                 THEN 
                    WREM=THLIQX(I,J)*ZRMDR(I,J)                                             
                    TREM=TBARWX(I,J)*THLIQX(I,J)*ZRMDR(I,J)                                   
                 ELSE                                                                    
                    WREM=0.0                                                            
                    TREM=0.0                                                            
                 ENDIF                                                                   
                 IF(J.EQ.LZF(I))                      THEN    
                    THINFL=(WADD(I)+WREM+FDT(I,J)-FDT(I,J+1))/DELZW(I,J)                                 
                    IF(THINFL.LT.THLMIN(I,J))   THEN                                           
                       FDT(I,J+1)=WADD(I)+WREM+FDT(I,J)-THLMIN(I,J)*
     1                            DELZW(I,J)
                    ENDIF                                                               
                 ENDIF                                                                   
                 WDRA=FDT(I,J)-FDT(I,J+1)                                                    
                 TDRA=FDT(I,J)*TFDT(I,J)-FDT(I,J+1)*TFDT(I,J+1)                                  
                 THLIQX(I,J)=(WADD(I)+WREM+WDRA)/DELZW(I,J)
                 THLIQX(I,J)=MAX(THLIQX(I,J),THLMIN(I,J))
                 TBARWX(I,J)=(TADD(I)+TREM+TDRA)/(THLIQX(I,J)*
     1                        DELZW(I,J))
              ENDIF                          
  550     CONTINUE
  600 CONTINUE
C
C     * CALCULATE FLOW OUT OF BOTTOM OF SOIL COLUMN DUE TO INFILTRATION
C     * AND GRAVITY DRAINAGE AND ADD TO TOTAL RUNOFF AND BASEFLOW.
C
      DO 700 K=1,IGP1
      DO 700 I=IL1,IL2
          IF(IGRN(I).GT.0)                                        THEN
              IF(LZF(I).EQ.IGP1 .AND. K.LE.NINF(I) .AND. 
     1                THLINF(I,IGP1)*ZMAT(I,K,IGP1).GT.0.0)   THEN 
                  TBASFL(I)=(TBASFL(I)*BASFLW(I)+FI(I)*(TMOVE(I,K)+
     1                TFREZ)*THLINF(I,IGP1)*ZMAT(I,K,IGP1))/(BASFLW(I)+
     2                FI(I)*THLINF(I,IGP1)*ZMAT(I,K,IGP1))                
                  BASFLW(I)=BASFLW(I)+FI(I)*THLINF(I,IGP1)*
     1                      ZMAT(I,K,IGP1)
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TMOVE(I,K)+TFREZ)*
     1                THLINF(I,IGP1)*ZMAT(I,K,IGP1))/(RUNOFF(I)+
     2                THLINF(I,IGP1)*ZMAT(I,K,IGP1))
                  RUNOFF(I)=RUNOFF(I)+THLINF(I,IGP1)*ZMAT(I,K,IGP1)
              ELSE IF(K.EQ.(IGDR(I)+1) .AND. FDT(I,K).GT.1.0E-8)  THEN
                  TBASFL(I)=(TBASFL(I)*BASFLW(I)+FI(I)*(TFDT(I,K)+
     1                TFREZ)*FDT(I,K))/(BASFLW(I)+FI(I)*FDT(I,K))
                  BASFLW(I)=BASFLW(I)+FI(I)*FDT(I,K)
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TFDT(I,K)+TFREZ)*
     1                FDT(I,K))/(RUNOFF(I)+FDT(I,K))
                  RUNOFF(I)=RUNOFF(I)+FDT(I,K)
              ENDIF                              
          ENDIF
  700 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
