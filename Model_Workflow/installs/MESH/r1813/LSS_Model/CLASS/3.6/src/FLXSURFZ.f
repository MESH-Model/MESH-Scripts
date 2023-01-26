      SUBROUTINE FLXSURFZ(CDM, CDH, CTU, RIB, FTEMP, FVAP, ILMO,
     X                    UE, FCOR, TA , QA , ZU, ZT, VA,
     Y                    TG , QG , H , Z0 , Z0T,
     %                    LZZ0, LZZ0T, FM, FH,N,IL1,IL2,FI,ITER,JL )
      IMPLICIT NONE
      INTEGER N,IL1,IL2,ITER(N),JL
      REAL CDM(N),CDH(N),CTU(N),RIB(N),FCOR(N),ILMO(N)
      REAL FTEMP(N),FVAP(N),TA(N),QA(N),ZU(N),VA(N)
      REAL TG(N),QG(N),H(N),Z0(N),UE(N),ZT(N)
      REAL Z0T(N),LZZ0(N),LZZ0T(N)
      REAL fm(N),fh(N)
      REAL FI(N)
c
cAuthor
c          Y.Delage (Jul 1990)
cRevision
c 001      G. Pellerin (Jun 94) New function for unstable case
c 002      G. Pellerin (Jui 94) New formulation for stable case
c 003      B. Bilodeau (Nov 95) Replace VK by KARMAN
c 004      M. Desgagne (Dec 95) Add safety code in function ff
c                               and ensures that RIB is non zero
c 005      R. Sarrazin (Jan 96) Correction for H
c 006      C. Girard (Nov 95) - Diffuse T instead of Tv
c 007      G. Pellerin (Feb 96) Revised calculation for H (stable)
c 008      G. Pellerin (Feb 96) Remove corrective terms to CTU
c 009      Y. Delage and B. Bilodeau (Jul 97) - Cleanup
c 010      Y. Delage (Feb 98) - Addition of HMIN
c 011      Y. Delage (Sept 00) - Set top of surface layer at ZU +Z0
c                              - Output UE instead of UE**2
c                              - Initialise ILMO and H
c                              - Change iteration scheme for stable case
c                              - Introduce log-linear profile for near-
c                                 neutral stable cases
c                              - set VAMIN inside flxsurf
c 012      Y. Delage (Oct 00) - Input of wind and temperatur/humidity
c                                at different levels
c 013      D. Verseghy (Nov 02) - Add calculation of CDH
c 014      D. Verseghy (Nov 04) - Pass in JL for troubleshooting
c 015      V. Fortin (Jun 06) - Modify calculations of LZZ0 and LZZ0T
c                               to avoid numerical problems with log
c 016      D. Verseghy (Jun 06) - Loop over IL1-IL2 instead of 1-N
c 017      J.P. Paquin (Aug 08) - "Synchronization" with flxsurf3
c                               - Insert code from stabfunc2.cdk (v4.5)
c                               - VAMIN=2.5 m/s to be coherent with ISBA  
c                                 (temporary measure VAMIN should be added
c                                  to physics constants)
c                                 (changes implemented by L.Duarte on Oct 08)
c 018      R. Harvey   (Oct 08) - Add fractional subregion cover FI
c                                 (in addition to ITER) to control
c                                 calculations over relevant points.
c 019      B. Dugas (after L. Spacek for flxsurf3 v_5.0.2) (Jan 09)
c                               - Correction of the log-linear profile
c                               - Double precision for rib calculations
c                               - VAMIN is now retreived from CLASSD3
c
cObject
c          to calculate surface layer transfer coefficients and fluxes
c          FLXSURFZ is a variant of FLXSURF3 that permits to input
c          wind and temperature (humidity) at different levels
c
cArguments
c
c          - Output -
c CDM      transfer coefficient of momentum squared
c CTU      transfer coefficient of temperature times UE
c RIB      bulk Richardson number
c FTEMP    temperature flux
c FVAP     vapor flux
c ILMO     (1/length of Monin-Obukov)
c UE       friction velocity 
c H        height of the boundary layer
c FM       momentum stability function
c FH       heat stability function
c LZZ0     log ((zu+z0)/z0)
c LZZ0T    log ((zt+z0)/z0t)
c          - Input -
c FCOR     Coriolis factor
c ZU       height of wind input
c ZT       height of temperature and humidity input
c TA       potential temperature at first predictive level above surface
c QA       specific humidity     "    "      "        "      "     "
c VA       wind speed            "    "      "        "      "     "
c TG       surface temperature
c QG       specific humidity at the surface
c Z0       roughness length for momentum      flux calculations
c Z0T      roughness length for heat/moisture flux calculations
c N        horizontal dimension
c
cNotes
c          SEE DELAGE AND GIRARD BLM 58 (19-31)
c                "       BLM 82 (23-48)
c
c     DIVERSES CONSTANTES PHYSIQUES 
c
      REAL AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX,CLM
      REAL DELTA,GRAV,KARMAN,CPD
      COMMON / PHYCON / DELTA,GRAV,KARMAN,CPD
      COMMON / CLASSD2 / AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
      INTEGER J
      INTEGER IT,ITMAX
      REAL HMAX,CORMIN,EPSLN
      REAL RAC3,CM,CT,ZP
      REAL F,G,DG
      REAL HI,HE,HS,unsl
      REAL*8 DTHV,TVA,TVS
      REAL HL,U
      REAL CS,XX,XX0,YY,YY0
      REAL ZB,DD,ILMOX
      REAL DF,ZZ,betsasx
      REAL aa,bb,cc
      SAVE HMAX,CORMIN,EPSLN
      SAVE ITMAX
      REAL,SAVE::VMODMIN=-1.0
c
      REAL            VAMIN
c     COMMON /CLASSD3/VAMIN
      DATA VAMIN / 1.0 /
c
      DATA CORMIN, HMAX /0.7E-4 ,  1500.0/
      DATA ITMAX / 3 /
      DATA EPSLN / 1.0e-05 /
c
      DF(ZZ)=(1-ZZ*HI)*sqrt(1+(4*AS*BETA*ilmo(j))*ZZ/(1-ZZ*HI))
c
      RAC3=sqrt(3.0)
      CS=AS*2.5
      IF(VMODMIN<0.0) VMODMIN=SQRT(VAMIN)
      betsasx=1./asx
c
      DO J=IL1,IL2
      IF(FI(J).GT.0. .AND. ITER(J).EQ.1) THEN
c
c  CALCULATE THE RICHARDSON NUMBER
        ZP=ZU(J)**2/(ZT(J)+Z0(J)-Z0T(J))
        u=max(vmodmin,va(j))
        tva=(1.d0+DELTA*QA(J))*TA(J)
        tvs=(1.d0+DELTA*QG(J))*TG(J)
        dthv=tva-tvs
        RIB(J)=GRAV/(tvs+0.5*dthv)*ZP*dthv/(u*u)
        if (rib(j).ge.0.0) rib(j) = max(rib(j), EPSLN)
        if (rib(j).lt.0.0) rib(j) = min(rib(j),-EPSLN)
c
c  FIRST APPROXIMATION TO ILMO
        LZZ0(J)=LOG(Z0(J)+ZU(J))-LOG(Z0(J))
        LZZ0T(J)=LOG(ZT(J)+Z0(J))-LOG(Z0T(J))
        IF(RIB(J).GT.0.)  THEN
           FM(J)=LZZ0(J)+CS*RIB(J)/max(2*z0(j),1.0)
           FH(J)=BETA*(LZZ0T(J)+CS*RIB(J))/
     1           max(sqrt(z0(j)*z0t(j)),1.0)
           ILMO(J)=RIB(J)*FM(J)*FM(J)/(ZP*FH(J))
           F=MAX(ABS(FCOR(J)),CORMIN)
           H(J)=BS*sqrt(KARMAN*u/(ILMO(J)*F*fm(j)))
        ELSE
           FM(J)=LZZ0(J)-min(0.7+log(1-rib(j)),LZZ0(J)-1)
           FH(J)=BETA*(LZZ0T(J)-min(0.7+log(1-rib(j)),LZZ0T(J)-1))
        ENDIF
        ILMO(J)=RIB(J)*FM(J)*FM(J)/(ZP*FH(J))
      ENDIF
      ENDDO
c - - - - - - - - -  BEGINNING OF ITERATION LOOP - - - - - - - - - - - 
      DO 35 IT=1,ITMAX
      DO 35 J=IL1,IL2
      IF(FI(J).GT.0. .AND. ITER(J).EQ.1) THEN
        u=max(vmodmin,va(j))
        ZP=ZU(J)**2/(ZT(J)+Z0(J)-Z0T(J))
        IF(RIB(J).GT.0.)  THEN
c----------------------------------------------------------------------
c  STABLE CASE
       	ILMO(J)=max(EPSLN,ILMO(J))
        hl=(ZU(J)+10*Z0(J))*FACTN
        F=MAX(ABS(FCOR(J)),CORMIN)
        hs=BS*sqrt(KARMAN*u/(ILMO(J)*F*fm(j)))
        H(J)=MAX(HMIN,hs,hl,factn/(4*AS*BETA*ILMO(J)))
        HI=1/H(J)
        unsl=ILMO(J)
cCDIR IEXPAND
        fm(J)=LZZ0(J)+psi(ZU(J)+Z0(J),hi,unsl)-psi(Z0(J),hi,unsl)
cCDIR IEXPAND
        fh(J)=BETA*(LZZ0T(J)+psi(ZT(J)+Z0(J),hi,unsl)
     1                      -psi(Z0T(J)     ,hi,unsl))
        DG=-ZP*FH(J)/(FM(J)*FM(J))*(1+beta*(DF(ZT(J)+Z0(J))-DF(Z0T(J)))/
     1          (2*FH(J))-(DF(ZU(J)+Z0(J))-DF(Z0(J)))/FM(J))
c----------------------------------------------------------------------
c  UNSTABLE CASE
      ELSE
        ILMO(J)=MIN(0.,ILMO(J))
cCDIR IEXPAND
         FM(J)=fmi(zu(j)+z0(j),z0 (j),lzz0 (j),ilmo(j),xx,xx0)
cCDIR IEXPAND
         FH(J)=fhi(zt(j)+z0(j),z0t(j),lzz0t(j),ilmo(j),yy,yy0)
         DG=-ZP*FH(J)/(FM(J)*FM(J))*(1+beta/FH(J)*(1/YY-1/YY0)-2/FM(J)*
     %                (1/XX-1/XX0))
      ENDIF
c----------------------------------------------------------------------
      IF(IT.LT.ITMAX) THEN
             G=RIB(J)-FH(J)/(FM(J)*FM(J))*ZP*ILMO(J)
             ILMO(J)=ILMO(J)-G/DG
      ENDIF
      ENDIF
   35 CONTINUE
c - - - - - -  - - - END OF ITERATION LOOP - - - - - - - - - - - - - -
      DO 80 J=IL1,IL2
      IF(FI(J).GT.0. .AND. ITER(J).EQ.1) THEN
        u=max(vmodmin,va(j))
        if(asx.lt.as) then
c----------------------------------------------------------------------
c  CALCULATE ILMO AND STABILITY FUNCTIONS FROM LOG-LINEAR PROFILE
c     (SOLUTION OF A QUADRATIC EQATION)
c
           zb=zu(j)/(zt(j)+z0(j)-z0t(j))
c  DISCRIMINANT
           dd=(beta*lzz0t(j)*zb)**2-4*rib(j)*asx*lzz0(j)*
     1        (beta*lzz0t(j)*zb-lzz0(j))
           if(rib(j).gt.0..and.rib(j).lt.betsasx.and.dd.ge.0.) then
c  COEFFICIENTS
              aa=asx*asx*rib(j)-asx
              bb=-beta*lzz0t(j)*zb+2*rib(j)*asx*lzz0(j)
              cc=rib(j)*lzz0(j)**2
c  SOLUTION
              if(bb>=0)then
                 ilmox=(-bb-sqrt(dd))
     1                 /(2*zu(j)*aa)
              else
                 ilmox=2*cc/(zu(j)*(-bb+sqrt(dd)))
              endif
              if(ilmox.lt.ilmo(j)) then
                 ilmo(j)=ilmox
                 fm(j)=lzz0(j)+asx*zu(j)*ilmox
                 fh(j)=beta*lzz0t(j)+asx*(zt(j)+z0(j)-z0t(j))*ilmox
              endif
           endif
        endif
c----------------------------------------------------------------------
        CM=KARMAN/FM(J)
        CT=KARMAN/FH(J)
        UE(J)=u*CM
        CDM(J)=CM**2
        CTU(J)=CT*UE(J)
        CDH(J)=CM*CT
        if (rib(j).gt.0.0) then
c             cas stable
              H(J)=MIN(H(J),hmax)
        else
c             cas instable
              F=MAX(ABS(FCOR(J)),CORMIN)
              he=max(HMIN,0.3*UE(J)/F)
              H(J)=MIN(he,hmax)
        endif
        FTEMP(J)=-CTU(J)*(TA(J)-TG(J))
        FVAP(J)=-CTU(J)*(QA(J)-QG(J))
      ENDIF
   80 CONTINUE
      RETURN
      CONTAINS

C   The following code is taken from the RPN/CMC physics library file
C   /usr/local/env/armnlib/modeles/PHY_shared/ops/v_4.5/RCS/stabfunc2.cdk,v

C   Internal function FMI
C   Stability function for momentum in the unstable regime (ilmo<0)
c   Reference: Delage Y. and Girard C. BLM 58 (19-31) Eq. 19
c
      REAL FUNCTION FMI(Z2,Z02,LZZ02,ILMO2,X,X0)
      implicit none
C
      REAL, INTENT(IN ) :: Z2,Z02,LZZ02,ILMO2
      REAL, INTENT(OUT) :: X,X0
c
      X =(1-CI*Z2 *BETA*ILMO2)**(0.16666666)
      X0=(1-CI*Z02*BETA*ILMO2)**(0.16666666)
      FMI=LZZ02+LOG((X0+1)**2*SQRT(X0**2-X0+1)*(X0**2+X0+1)**1.5
     %               /((X+1)**2*SQRT(X**2-X+1)*(X**2+X+1)**1.5))
     %              +RAC3*ATAN(RAC3*((X**2-1)*X0-(X0**2-1)*X)/
     %              ((X0**2-1)*(X**2-1)+3*X*X0))
c
      RETURN
      END FUNCTION FMI
c
C   Internal function FHI
C   Stability function for heat and moisture in the unstable regime (ilmo<0)
c   Reference: Delage Y. and Girard C. BLM 58 (19-31) Eq. 17
c
      REAL FUNCTION FHI(Z2,Z0T2,LZZ0T2,ILMO2,Y,Y0)
      implicit none
C
      REAL, INTENT(IN ) :: Z2,Z0T2,LZZ0T2,ILMO2
      REAL, INTENT(OUT) :: Y,Y0
c
      Y =(1-CI*Z2  *BETA*ILMO2)**(0.33333333)
      Y0=(1-CI*Z0T2*BETA*ILMO2)**(0.33333333)
      FHI=BETA*(LZZ0T2+1.5*LOG((Y0**2+Y0+1)/(Y**2+Y+1))+RAC3*
     %        ATAN(RAC3*2*(Y-Y0)/((2*Y0+1)*(2*Y+1)+3)))
c
      RETURN
      END FUNCTION FHI
C
C   Internal function psi
C   Stability function for momentum in the stable regime (unsl>0)
c   Reference :  Y. Delage, BLM, 82 (p23-48) (Eqs.33-37)
c
      REAL FUNCTION PSI(Z2,HI2,ILMO2)
      implicit none
C
      REAL a,b,c,d
      REAL, INTENT(IN ) :: ILMO2,Z2,HI2
c
      d = 4*AS*BETA*ILMO2
      c = d*hi2 - hi2**2
      b = d - 2*hi2
      a = sqrt(1 + b*z2 - c*z2**2)
      psi = 0.5 * (a-z2*hi2-log(1+b*z2*0.5+a)-
     +            b/(2*sqrt(c))*asin((b-2*c*z2)/d))
c
      RETURN
      END FUNCTION PSI
      END
