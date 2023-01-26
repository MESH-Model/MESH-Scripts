      SUBROUTINE ICEADD(TPOND,ZPOND,HTC,FI,R,TR,DELZ,
     1                  ISAND,IG,ILG,IL1,IL2,JL,N)
C
C     * JUL 20/20 - D.PRINCZ.   MODIFIED THE CALCULATION OF HTC TO
C                               CONSIDER CHANGE IN ZPOND WHEN USING IWF
C                               (ICEBAL).
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I,J,K,N
C
C     * INPUT/OUTPUT FIELDS.
C
      REAL HTC   (ILG,IG)
C
      REAL TPOND (ILG),    ZPOND (ILG)
C
C     * INPUT FIELDS.
C
      REAL FI    (ILG),    R     (ILG),    TR    (ILG)
C
      INTEGER              ISAND (ILG,IG)
C
      REAL DELZ  (IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL RADD
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW
C                                    
      COMMON /CLASS1/ DELT,TFREZ                                               
      COMMON /CLASS4/ HCPW
C-----------------------------------------------------------------------
C
C     * ADD RAINFALL OR SNOWMELT TO PONDED WATER FOR ISAND -4.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                THEN
              IF(ZPOND(I).GT.0.)                                THEN 
                 HTC (I,1)=HTC(I,1)-FI(I)*(TPOND(I)+TFREZ)*HCPW*
     1                     ZPOND(I)/DELT
              ENDIF                                                                       
              IF(R(I).GT.0.)                                THEN 
                 RADD=R(I)*DELT                                                             
                 TPOND(I)=((TPOND(I)+TFREZ)*ZPOND(I)+(TR(I)+TFREZ)*
     1               RADD)/(ZPOND(I)+RADD)-TFREZ
                 ZPOND(I)=ZPOND(I)+RADD                                                        
              ENDIF                                                                       
          ENDIF
  100 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
