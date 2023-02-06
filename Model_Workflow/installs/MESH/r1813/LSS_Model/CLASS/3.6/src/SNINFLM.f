      SUBROUTINE SNINFLM(R,TR,ZSNOW,TSNOW,RHOSNO,HCPSNO,WSNOW,
     1                  HTCS,HMFN,PCPG,ROFN,FI,ILG,IL1,IL2,JL,
     2                  NCOUNT,RUNOFF,TRUNOF,OVRFLW,TOVRFL,
     3                  SOIL_POR_MAX,SOIL_DEPTH,S0,T_ICE_LENS,
     3                  THTOT,TBAR1,t0_ACC,
     4                  SI,TSI,INFILTYPE,SNOWMELTD,SNOWMELTD_LAST,
     5                  MELTRUNOFF,SNOWINFIL,CUMSNOWINFIL,
     6                  FRZC,
     7                  FROZENSOILINFILFLAG,ZPOND,TPOND)
C
C     * JAN 06/12 - D.PRINCZ/B.DAVISON. UPDATED DISTRIBUTION OF
C                                       MELTWATER
C     * DEC 20/10 - M.A.MEKONNEN/D.BRUCE. MODIFIED FOR FROZEN SOIL
C                                         INFILTRATION
C     * DEC 23/09 - D.VERSEGHY. RESET WSNOW TO ZERO WHEN SNOW
C     *                         PACK DISAPPEARS.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         NEW DIAGNOSTIC FIELD "ROFN" ADDED.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. RAIN INFILTRATION INTO SNOWPACK.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG),    TR    (ILG),    ZSNOW (ILG),    TSNOW (ILG),
     1     RHOSNO(ILG),    HCPSNO(ILG),    WSNOW (ILG),    HTCS  (ILG),
     2     HMFN  (ILG),    PCPG  (ILG),    ROFN  (ILG)
C
      INTEGER NCOUNT,INFILTYPE(ILG),
     1        FROZENSOILINFILFLAG
      REAL    t0_ACC,SOIL_POR_MAX,SOIL_DEPTH,S0,T_ICE_LENS
      REAL    RUNOFF(ILG),TRUNOF(ILG),OVRFLW(ILG),TOVRFL(ILG),SWE(ILG),
     1        THTOT(ILG),TBAR1(ILG),
     2        SI(ILG),TSI(ILG),SNOWMELTD(ILG),
     3        SNOWMELTD_LAST(ILG),SNOWINFIL(ILG),CUMSNOWINFIL(ILG),
     4        MELTRUNOFF(ILG),FRZC(ILG),
     5        ZPOND(ILG),TPOND(ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL RAIN,HRCOOL,HRFREZ,HSNWRM,HSNMLT,ZMELT,ZFREZ,WSNCAP,WAVAIL
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
C
      WSNCAP=0.04
C      WSNCAP=0.0
C-----------------------------------------------------------------------

      SWE = ZSNOW*RHOSNO/RHOW*1000.0 ! UNIT -> mm
      
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. R(I).GT.0. .AND. ZSNOW(I).GT.0.)
     1                                                              THEN
              HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              RAIN=R(I)*DELT                                            
              HRCOOL=TR(I)*HCPW*RAIN                                    
              HRFREZ=CLHMLT*RHOW*RAIN                                   
              HSNWRM=(0.0-TSNOW(I))*HCPSNO(I)*ZSNOW(I)                  
              HSNMLT=CLHMLT*RHOSNO(I)*ZSNOW(I)                          
              IF(HRCOOL.GE.(HSNWRM+HSNMLT))                 THEN        
                  HRCOOL=HRCOOL-(HSNWRM+HSNMLT)                         
                  ZMELT=ZSNOW(I)*RHOSNO(I)/RHOW                         
                  HMFN(I)=HMFN(I)+FI(I)*CLHMLT*ZMELT*RHOW/DELT
                  HTCS(I)=HTCS(I)+FI(I)*CLHMLT*ZMELT*RHOW/DELT
                  TR(I)=HRCOOL/(HCPW*(ZMELT+RAIN+WSNOW(I)/RHOW))        
                  R(I)=R(I)+(ZMELT+WSNOW(I)/RHOW)/DELT
                  ZSNOW(I)=0.0                                          
                  TSNOW(I)=0.0                                          
                  RHOSNO(I)=0.0                                         
                  HCPSNO(I)=0.0                                         
                  WSNOW(I)=0.0
              ELSE IF(HRCOOL.GE.HSNWRM .AND. HRCOOL.LT.(HSNWRM+HSNMLT))
     1                                                      THEN
                  HSNMLT=HRCOOL-HSNWRM                                  
                  ZMELT=HSNMLT/(CLHMLT*RHOSNO(I))                       
                  HMFN(I)=HMFN(I)+FI(I)*CLHMLT*ZMELT*RHOSNO(I)/DELT
                  HTCS(I)=HTCS(I)+FI(I)*CLHMLT*ZMELT*RHOSNO(I)/DELT
                  ZSNOW(I)=ZSNOW(I)-ZMELT                               
                  WAVAIL=ZMELT*RHOSNO(I)+WSNOW(I)
                  IF(WAVAIL.GT.(WSNCAP*ZSNOW(I)*RHOSNO(I))) THEN
                      WSNOW(I)=WSNCAP*ZSNOW(I)*RHOSNO(I)
                      ZMELT=(WAVAIL-WSNOW(I))/RHOW
                  ELSE
                      WSNOW(I)=WAVAIL
                      ZMELT=0.0
                  ENDIF
                  TSNOW(I)=0.0                                          
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  TR(I)=0.0                                             
                  R(I)=R(I)+ZMELT/DELT                                  
              ELSE IF(HSNWRM.GE.(HRCOOL+HRFREZ))            THEN        
                  HSNWRM=(HRCOOL+HRFREZ)-HSNWRM                         
                  HMFN(I)=HMFN(I)-FI(I)*HRFREZ/DELT
                  HTCS(I)=HTCS(I)-FI(I)*HRFREZ/DELT
                  RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOW*RAIN)/ZSNOW(I)     
                  IF(RHOSNO(I).GT.RHOICE)      THEN                     
                      ZSNOW(I)=RHOSNO(I)*ZSNOW(I)/RHOICE                
                      RHOSNO(I)=RHOICE                                  
                  ENDIF                                                 
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  TSNOW(I)=HSNWRM/(HCPSNO(I)*ZSNOW(I))                  
                  TR(I)=0.0                                             
                  R(I)=0.0                                              
              ELSE IF(HSNWRM.GE.HRCOOL .AND. HSNWRM.LT.(HRCOOL+HRFREZ)) 
     1                                                      THEN
                  HRFREZ=HSNWRM-HRCOOL                                  
                  ZFREZ=HRFREZ/(CLHMLT*RHOW)                            
                  HMFN(I)=HMFN(I)-FI(I)*CLHMLT*ZFREZ*RHOW/DELT
                  HTCS(I)=HTCS(I)-FI(I)*CLHMLT*ZFREZ*RHOW/DELT
                  RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOW*ZFREZ)/ZSNOW(I)    
                  IF(RHOSNO(I).GT.RHOICE)      THEN                     
                      ZSNOW(I)=RHOSNO(I)*ZSNOW(I)/RHOICE                
                      RHOSNO(I)=RHOICE                                  
                  ENDIF                                                 
                  WAVAIL=(RAIN-ZFREZ)*RHOW+WSNOW(I)
                  IF(WAVAIL.GT.(WSNCAP*ZSNOW(I)*RHOSNO(I))) THEN
                      WSNOW(I)=WSNCAP*ZSNOW(I)*RHOSNO(I)
                      WAVAIL=WAVAIL-WSNOW(I)
                  ELSE
                      WSNOW(I)=WAVAIL
                      WAVAIL=0.0
                  ENDIF
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  R(I)=WAVAIL/(RHOW*DELT)
                  TR(I)=0.0                                             
                  TSNOW(I)=0.0                                          
              ENDIF                                                     
              HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              PCPG(I)=PCPG(I)+FI(I)*R(I)*RHOW
              ROFN(I)=ROFN(I)+FI(I)*R(I)*RHOW
          ENDIF
  100 CONTINUE
C     
C     CALCULATION OF MELTWATER (FROZEN SOIL INFILTRATION)
C
      CALL FROZEN(FI,R,TBAR1,THTOT,SWE,
     1            SOIL_POR_MAX,SOIL_DEPTH,S0,T_ICE_LENS, 
     2            t0_ACC,FRZC,DELT,NCOUNT,ILG,IL1,IL2,
     3            SI,TSI,INFILTYPE,SNOWMELTD,SNOWMELTD_LAST, 
     4            MELTRUNOFF,SNOWINFIL,CUMSNOWINFIL)
C
C     RE-DISTRIBUTION OF MELTWATER
C
      DO I=IL1,IL2
C
      IF(FI(I).GT.0.0 .AND. MELTRUNOFF(I).GT.0.0)THEN
C
C     FROZENSOILINFILFLAG = 1;
C     MELTWATER IS MOVED TO RUNOFF AND DECLASSIFIED AS
C     RAINFALL. THE TEMPERATURE OF THE FLOWS ARE UPDATED.
C
      IF(FROZENSOILINFILFLAG.EQ.1)THEN
        TRUNOF(I)  = (TRUNOF(I)*RUNOFF(I)+TR(I)*MELTRUNOFF(I)*DELT)
     1               /(RUNOFF(I)+MELTRUNOFF(I)*DELT)
        RUNOFF(I)  = RUNOFF(I)+MELTRUNOFF(I)*DELT
        TOVRFL(I)  = (TOVRFL(I)*OVRFLW(I)+TR(I)*FI(I)*MELTRUNOFF(I)
     1               *DELT)/(OVRFLW(I)+FI(I)*MELTRUNOFF(I)*DELT)
        OVRFLW(I)  = OVRFLW(I)+FI(I)*MELTRUNOFF(I)*DELT
        R(I)       = MAX(0.0,R(I)-MELTRUNOFF(I))
C
C     FROZENSOILINFILFLAG = 2;
C     MELTWATER IS MOVED TO THE PONDING DEPTH AND DECLASSIFIED AS
C     RAINFALL. THE TEMPERATURE OF THE PONDING DEPTH IS UPDATED.
C
      ELSE IF(FROZENSOILINFILFLAG.EQ.2)THEN
        TPOND(I)  = (TPOND(I)*ZPOND(I)+TR(I)*FI(I)*MELTRUNOFF(I)*DELT)
     1              /(ZPOND(I)+FI(I)*MELTRUNOFF(I)*DELT)
        ZPOND(I)  = ZPOND(I)+FI(I)*MELTRUNOFF(I)*DELT
        R(I)      = MAX(0.0,R(I)-FI(I)*MELTRUNOFF(I))
C
      ENDIF !>FROZENSOILINFILFLAG.EQ.N
C
      ENDIF !>FI(I).GT.0.0 .AND. MELTRUNOFF(I).GT.0.0
C
      ENDDO !>I=IL1,IL2

      RETURN
      END        
