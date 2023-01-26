      SUBROUTINE CLASSZ(ISTEP,  CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, 
     1                  WTVSTP, WTSSTP, WTGSTP,
     2                  FSGV,   FLGV,   HFSC,   HEVC,   HMFC,   HTCC,
     3                  FSGS,   FLGS,   HFSS,   HEVS,   HMFN,   HTCS,
     4                  FSGG,   FLGG,   HFSG,   HEVG,   HMFG,   HTC,
     5                  PCFC,   PCLC,   QFCF,   QFCL,   ROFC,   WTRC,
     6                  PCPN,   QFN,    ROFN,   WTRS,   PCPG,   QFG,
     7                  QFC,    ROF,    WTRG,   CMAI,   RCAN,   SCAN,   
     8                  TCAN,   SNO,    WSNOW,  TSNOW,  THLIQ,  THICE,  
     9                  HCPS,   THPOR,  DELZW,  TBAR,   ZPOND,  TPOND,  
     +                  ICE,    TICE,
     A                  DELZ,   FCS,    FGS,    FC,     FG,
     B                  IL1,    IL2,    ILG,    IG,     N, 
     C                  Drift,  Subl)
C
C     * JUN 10/20 - D.PRINCZ.   ADDED ICE AND TICE (ICEBAL).
C     * JUN 11/18 - D.PRINCZ.   REPLACED 'STOP' WITH CALL TO XIT
C     *                         TO FORCE PROG ABORT.
C     * OCT 01/10 - M.MACDONALD.ADDED BLOWING SNOW ARRAYS (PBSM).
C     * JAN 06/09 - D.VERSEGHY. MORE VARIABLES IN PRINT STATEMENTS;
C     *                         SLIGHTLY INCREASED ACCURACY LIMITS.
C     * NOV 10/06 - D.VERSEGHY. CHECK THAT SUMS OF ENERGY AND WATER
C     *                         FLUXES FOR CANOPY, SNOW AND SOIL MATCH
C     *                         CHANGES IN HEAT AND WATER STORAGE OVER
C     *                         CURRENT TIMESTEP.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER  ISTEP,IL1,IL2,ILG,IG,N,I,J
C
C     * DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER 
C     * BALANCES.
C
      REAL CTVSTP(ILG),   CTSSTP(ILG),   CT1STP(ILG),   CT2STP(ILG),
     1     CT3STP(ILG),   WTVSTP(ILG),   WTSSTP(ILG),   WTGSTP(ILG)
C
      REAL QSUMV,QSUMS,QSUM1,QSUM2,QSUM3,WSUMV,WSUMS,WSUMG
C
C     * INPUT ARRAYS.
C
      REAL FSGV  (ILG),   FLGV  (ILG),   HFSC  (ILG),   HEVC  (ILG),
     1     HMFC  (ILG),   HTCC  (ILG),   FSGS  (ILG),   FLGS  (ILG),
     2     HFSS  (ILG),   HEVS  (ILG),   HMFN  (ILG),   HTCS  (ILG),
     3     FSGG  (ILG),   FLGG  (ILG),   HFSG  (ILG),   HEVG  (ILG),
     4     HMFG  (ILG,IG),HTC   (ILG,IG),
     5     PCFC  (ILG),   PCLC  (ILG),   QFCF  (ILG),   QFCL  (ILG),
     6     ROFC  (ILG),   WTRC  (ILG),   PCPN  (ILG),   QFN   (ILG),
     7     ROFN  (ILG),   WTRS  (ILG),   PCPG  (ILG),   QFG   (ILG),
     8     QFC   (ILG,IG),
     9     ROF   (ILG),   WTRG  (ILG),   CMAI  (ILG),  
     A     RCAN  (ILG),   SCAN  (ILG),   TCAN  (ILG),   SNO   (ILG),
     B     WSNOW (ILG),   TSNOW (ILG),   
     C     THLIQ (ILG,IG),THICE (ILG,IG),HCPS  (ILG,IG),
     D     THPOR (ILG,IG),DELZW (ILG,IG),TBAR  (ILG,IG),
     E     ZPOND (ILG),   TPOND (ILG),
     +     ICE   (ILG),   TICE  (ILG),
     +     DELZ  (IG),
     F     FCS   (ILG),   FGS   (ILG),   FC    (ILG),   FG    (ILG),
     G     Drift (ILG),   Subl  (ILG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ
      REAL HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,
     1     SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C
C =================================================================
C
      IF(ISTEP.EQ.0) THEN
C
C     * SET BALANCE CHECK VARIABLES FOR START OF CURRENT TIME STEP.
C
      DO 100 I=IL1,IL2
          WTGSTP(I)=0.0
          CTVSTP(I)=-(CMAI(I)*SPHVEG+RCAN(I)*SPHW+
     1             SCAN(I)*SPHICE)*TCAN(I)
          CTSSTP(I)=-TSNOW(I)*(HCPICE*SNO(I)/RHOICE+
     1             HCPW*WSNOW(I)/RHOW)
          CT1STP(I)=-((HCPW*THLIQ(I,1)+HCPICE*THICE(I,1)
     1             +HCPS(I,1)*(1.0-THPOR(I,1)))*DELZW(I,1)+
     2             HCPSND*(DELZ(1)-DELZW(I,1)))*TBAR(I,1)-
     3             HCPW*ZPOND(I)*TPOND(I)
          CT2STP(I)=-((HCPW*THLIQ(I,2)+HCPICE*THICE(I,2)
     1             +HCPS(I,2)*(1.0-THPOR(I,2)))*DELZW(I,2)+
     2             HCPSND*(DELZ(2)-DELZW(I,2)))*TBAR(I,2)
          CT3STP(I)=-((HCPW*THLIQ(I,3)+HCPICE*THICE(I,3)
     1             +HCPS(I,3)*(1.0-THPOR(I,3)))*DELZW(I,3)+
     2             HCPSND*(DELZ(3)-DELZW(I,3)))*TBAR(I,3)
          WTVSTP(I)=-(RCAN(I)+SCAN(I))
          WTSSTP(I)=-SNO(I)-WSNOW(I)
          DO 50 J=1,IG
              WTGSTP(I)=WTGSTP(I)-
     1             (THLIQ(I,J)*RHOW+THICE(I,J)*RHOICE)*
     2             DELZW(I,J)
 50       CONTINUE
          WTGSTP(I)=WTGSTP(I)-ZPOND(I)*RHOW-ICE(I)
100   CONTINUE
C
      ENDIF
C
      IF(ISTEP.EQ.1) THEN
C
C     * CHECK ENERGY AND WATER BALANCES OVER THE CURRENT TIME STEP.
C
      DO 200 I=IL1,IL2
          CTVSTP(I)=CTVSTP(I)+(CMAI(I)*SPHVEG+RCAN(I)*SPHW+
     1             SCAN(I)*SPHICE)*TCAN(I)
          CTSSTP(I)=CTSSTP(I)+TSNOW(I)*(HCPICE*SNO(I)/RHOICE+
     1             HCPW*WSNOW(I)/RHOW)
          CT1STP(I)=CT1STP(I)+((HCPW*THLIQ(I,1)+HCPICE*THICE(I,1)
     1             +HCPS(I,1)*(1.0-THPOR(I,1)))*DELZW(I,1)+
     2             HCPSND*(DELZ(1)-DELZW(I,1)))*TBAR(I,1)+
     3             HCPW*ZPOND(I)*TPOND(I)
          CT2STP(I)=CT2STP(I)+((HCPW*THLIQ(I,2)+HCPICE*THICE(I,2)
     1             +HCPS(I,2)*(1.0-THPOR(I,2)))*DELZW(I,2)+
     2             HCPSND*(DELZ(2)-DELZW(I,2)))*TBAR(I,2)
          CT3STP(I)=CT3STP(I)+((HCPW*THLIQ(I,3)+HCPICE*THICE(I,3)
     1             +HCPS(I,3)*(1.0-THPOR(I,3)))*DELZW(I,3)+
     2             HCPSND*(DELZ(3)-DELZW(I,3)))*TBAR(I,3)
          CTVSTP(I)=CTVSTP(I)/DELT
          CTSSTP(I)=CTSSTP(I)/DELT
          CT1STP(I)=CT1STP(I)/DELT
          CT2STP(I)=CT2STP(I)/DELT
          CT3STP(I)=CT3STP(I)/DELT
          WTVSTP(I)=WTVSTP(I)+RCAN(I)+SCAN(I)
          WTSSTP(I)=WTSSTP(I)+SNO(I)+WSNOW(I)
          DO 150 J=1,IG
              WTGSTP(I)=WTGSTP(I)+
     1             (THLIQ(I,J)*RHOW+THICE(I,J)*RHOICE)*
     1             DELZW(I,J)
150       CONTINUE
          WTGSTP(I)=WTGSTP(I)+ZPOND(I)*RHOW+ICE(I)
200   CONTINUE
C
      DO 400 I=IL1,IL2
          QSUMV=FSGV(I)+FLGV(I)-HFSC(I)-HEVC(I)-
     1          HMFC(I)+HTCC(I)
          QSUMS=FSGS(I)+FLGS(I)-HFSS(I)-HEVS(I)-
     1          HMFN(I)+HTCS(I)
          QSUM1=FSGG(I)+FLGG(I)-HFSG(I)-HEVG(I)-
     1          HMFG(I,1)+HTC(I,1)
          QSUM2=-HMFG(I,2)+HTC(I,2)
          QSUM3=-HMFG(I,3)+HTC(I,3)
          WSUMV=(PCFC(I)+PCLC(I)-
     1          QFCF(I)-QFCL(I)-ROFC(I)+
     2              WTRC(I))*DELT
!> MK MacDonald: added Drift & Subl to WSUMS
          WSUMS=(PCPN(I)-QFN(I)-
     1              ROFN(I)+WTRS(I))*DELT
     2              -Drift(I)-Subl(I)
          WSUMG=(PCPG(I)-QFG(I)-
     1              ROF(I)+WTRG(I))*DELT
          DO 250 J=1,IG
              WSUMG=WSUMG-QFC(I,J)*DELT
250       CONTINUE
          IF(ABS(CTVSTP(I)-QSUMV).GT.1.0) THEN
              WRITE(6,6441) N,I,CTVSTP(I),QSUMV
6441          FORMAT(2X,'CANOPY ENERGY BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) FSGV(I),FLGV(I),HFSC(I),
     1             HEVC(I),HMFC(I),HTCC(I)
              WRITE(6,6450) RCAN(I),SCAN(I),TCAN(I)
              CALL XIT('CLASSZ',-1)
          ENDIF
          IF(ABS(CTSSTP(I)-QSUMS).GT.7.0) THEN
              WRITE(6,6442) N,I,CTSSTP(I),QSUMS
6442          FORMAT(2X,'SNOW ENERGY BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) FSGS(I),FLGS(I),HFSS(I),
     1            HEVS(I),HMFN(I),HTCS(I)
              WRITE(6,6450) TSNOW(I),SNO(I),WSNOW(I)
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I)
              CALL XIT('CLASSZ',-2)
          ENDIF
          IF(ABS(CT1STP(I)-QSUM1).GT.5.0) THEN
              WRITE(6,6443) N,I,CT1STP(I),QSUM1
              WRITE(6,6450) FSGG(I),FLGG(I),HFSG(I),
     1            HEVG(I),HMFG(I,1),HTC(I,1)
              WRITE(6,6450) FSGS(I),FLGS(I),HFSS(I),
     1            HEVS(I),HMFN(I),HTCS(I)
              WRITE(6,6450) THLIQ(I,1)*RHOW*DELZW(I,1),
     *            THLIQ(I,2)*RHOW*DELZW(I,2),
     *            THLIQ(I,3)*RHOW*DELZW(I,3),
     *            THICE(I,1)*RHOICE*DELZW(I,1),
     *            THICE(I,2)*RHOICE*DELZW(I,2),
     *            THICE(I,3)*RHOICE*DELZW(I,3),
     *            ZPOND(I)*RHOW
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I),
     1            DELZW(I,1),DELZW(I,2),DELZW(I,3)
6443          FORMAT(2X,'LAYER 1 ENERGY BALANCE  ',2I8,2F20.8)
              CALL XIT('CLASSZ',-3)
          ENDIF
          IF(ABS(CT2STP(I)-QSUM2).GT.5.0) THEN
              WRITE(6,6444) N,I,CT2STP(I),QSUM2
6444          FORMAT(2X,'LAYER 2 ENERGY BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) HMFG(I,2),HTC(I,2),
     1            THLIQ(I,2),THICE(I,2),THPOR(I,2),TBAR(I,2)-TFREZ
              WRITE(6,6450) HMFG(I,3),HTC(I,3),
     1            THLIQ(I,3),THICE(I,3),THPOR(I,3),TBAR(I,3)-TFREZ
              WRITE(6,6450) HMFG(I,1),HTC(I,1),
     1            THLIQ(I,1),THICE(I,1),THPOR(I,1),TBAR(I,1)-TFREZ
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I),
     1            DELZW(I,2),HCPS(I,2),DELZW(I,3)
6451          FORMAT(2X,7E20.6)
              CALL XIT('CLASSZ',-4)
          ENDIF
          IF(ABS(CT3STP(I)-QSUM3).GT.10.0) THEN
              WRITE(6,6445) N,I,CT3STP(I),QSUM3
6445          FORMAT(2X,'LAYER 3 ENERGY BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) HMFG(I,3),HTC(I,3),
     1            TBAR(I,3)
              WRITE(6,6450) THLIQ(I,3),THICE(I,3),HCPS(I,3),
     1                      THPOR(I,3),DELZW(I,3)
              CALL XIT('CLASSZ',-5)
          ENDIF
          IF(ABS(WTVSTP(I)-WSUMV).GT.1.0E-3) THEN
              WRITE(6,6446) N,WTVSTP(I),WSUMV
6446          FORMAT(2X,'CANOPY WATER BALANCE  ',I8,2F20.8)
              CALL XIT('CLASSZ',-6)
          ENDIF
          IF(ABS(WTSSTP(I)-WSUMS).GT.1.0E-2) THEN
              WRITE(6,6447) N,I,WTSSTP(I),WSUMS
6447          FORMAT(2X,'SNOW WATER BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) PCPN(I)*DELT,QFN(I)*DELT,
     1            ROFN(I)*DELT,WTRS(I)*DELT
              WRITE(6,6450) Drift(I),Subl(I)
              WRITE(6,6450) SNO(I),WSNOW(I),TSNOW(I)-TFREZ
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I)
              CALL XIT('CLASSZ',-7)
          ENDIF
          IF(ABS(WTGSTP(I)-WSUMG).GT.1.0E-1) THEN
              WRITE(6,6448) N,I,WTGSTP(I),WSUMG
6448          FORMAT(2X,'GROUND WATER BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) PCPG(I)*DELT,QFG(I)*DELT,
     1            QFC(I,1)*DELT,QFC(I,2)*DELT,
     2            QFC(I,3)*DELT,ROF(I)*DELT,
     3            WTRG(I)*DELT
              DO 390 J=1,IG
                  WRITE(6,6450) THLIQ(I,J)*RHOW*DELZW(I,J),
     *                THICE(I,J)*RHOICE*DELZW(I,J),
     *                DELZW(I,J)
390           CONTINUE
              WRITE(6,6450) ZPOND(I)*RHOW,ICE(I)
6450          FORMAT(2X,7F15.6)
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I)
              CALL XIT('CLASSZ',-8)
          ENDIF
400   CONTINUE
C
      ENDIF
C
      RETURN
      END

