      SUBROUTINE PBSMrates(E_StubHt, Uthr, DriftH, SublH,
     1                  t, u, sh, Fetch,
     2                  N_S, A_S, mBeta, p,I,N)
C
C     * SEP 2010 - M.MACDONALD. Single column calculations for blowing snow
C     *                         transport and sublimation. Equation
C     *                         numbers refer to JW Pomeroy thesis (1988).
C     *            
      IMPLICIT NONE
C
C     * INPUT/OUTPUT VARIABLES.
C
      REAL DriftH, SublH
      INTEGER  I, N !> strictly used for debugging
C
C     * INPUT VARIABLES.
C
      REAL E_StubHt, Uthr, t, u, sh, N_S, A_S, p, mBeta, Fetch
C
C     * TEMPORARY VARIABLES.
C
      REAL  A,      Alpha,  B,      Bd,     Bound,  C,
     1  Diff,   DmDt,   Es,     H,
     2  Htran,  Hsalt,  Inc,    Lamb,   Lambda, Lb,
     3  Mpm,    Mpr,    Nh,     Nsalt,
     4  Nz,     Nuss,   Omega,  TQsalt,
     5  TQsum,  Qz,     RauTerm,
     6  Reyn,   SBsalt, Sbz,    SBsum,
     7  SigmaZ, Sigma2, SvDens, Usthr,  Ustar,
     8  UstarZ, Uz,     Vs,     Vsalt,  Sigma,
     9  Vsusp,  Z,      Zr,     Zstb,
     A  a0, a1, a2, a3, a4, a5, a6, eT, Xwater, Pwater, rh
C
C!    * PBSM PARAMETERS
 !    REAL rhoo, Qstar, MMM, RR, LATH, DICE, ZD, XD, gg,
 !   1     Betaa, C1, C2, C3, M1KAARMAN, M2KAARMAN, M_PI,
 !   2     DegToRad

    ! COMMON /PBSM/   rhoo, Qstar, MMM, RR, LATH, DICE, ZD, XD, gg,
    !1                Betaa, C1, C2, C3, M1KAARMAN, M2KAARMAN, M_PI,
    !2                DegToRad
C     *PBSM CONSTANTS
      REAL  rhoo, Qstar, MMM, RR, LATH, DICE, ZD, XD, gg, Betaa, 
     1      C1, C2, C3, M1KAARMAN, M2KAARMAN, M_PI, DegToRad
          rhoo=1.23
          Qstar=120.0
          MMM=18.01
          RR=8313.
          LATH=2.838E6
          DICE=900.
          ZD=0.3
          XD=300.
          gg=9.80
          Betaa=170.0
          C1=2.8
          C2=1.6
          C3=4.2
          M1KAARMAN=0.4
          M2KAARMAN=0.16
          M_PI=3.1415926535898
          DegToRad=0.017453292
C--------------------------------------------------------------
C
!>     Modified Calculations for Mean Particle Mass in this 
!>     version program to calculate blowing snow horizontal
!>     flux, sublimation rate and latent heat flux due to
!>     snow sublimation for a variety of windspeeds,
!>     boundary layers and surface conditions.
!>
!>     All variable and constants entered into the programme
!>     are in SI and use Canadian Atmospheric Environement
!>     Service Meteorological data format. Snow transport is
!>     in kg per square meter per half hour from the surface
!>     to 5 metres height. Sublimation is totaled to the top
!>     of the boundary layer for diffusion, based on the
!>     meteorological Fetch and is expressed in millimeters
!>     of blowing snow lost over a square meter of snow surface
!>     per half hour.
C
   !>Compute stubble coefficients
C
      !> Lettau, used for susp Z0
      !Zstb=0.0048*E_StubHt*100.0
      !> Essery et al (1999) from Lettau (1969)
      Zstb = 0.5*N_S*A_S*E_StubHt
      !> Raupach Eq. 1
      Lambda=N_S*A_S*E_StubHt
      !> Raupach Eq. 4
      Sigma=(M_PI*A_S)/(4.0*E_StubHt)
C
   !> Calculate the flux for interval
      !> Total saltation flux
      TQsalt=0.0
      !> Total Suspension
      TQsum=0.0
      SBsalt=0.0
      SBsum=0.0
      DriftH=0.0
      SublH=0.0
C
!> convert specific (kg/kg) to relative humidity (0.xx)
      IF (t.GT.273.16)  THEN
       !> coefficients with respect to watewr
        a0=6.107799961
        a1=4.436518521E-1
        a2=1.428945805E-2
        a3=2.650648471E-4
        a4=3.031240396E-6
        a5=2.034080948E-8
        a6=6.136820929E-11
      ELSE
       !>coefficients with respect to ice
        a0=6.109177956
        a1=5.034698970E-1
        a2=1.886013408E-2
        a3=4.176223716E-4
        a4=5.824720280E-6
        a5=4.838803174E-8
        a6=1.838826904E-10
      ENDIF
      eT=a0+(t-273.16)*(a1+(t-273.16)*(a2+(t-273.16)*(a3+(t-273.16)
     1   *(a4+(t-273.16)*(a5+(t-273.16)*a6)))))
      Xwater=28.9644/(18.01534/sh+28.9644-18.01534)
      Pwater=Xwater*p/100   ! *10
      rh=Pwater/eT
C
      !> therm. cond. of atm. (J/(msK))
      Lamb=0.00063*t+0.0673
      !> diffus. of w.vap. atmos. (m^2/s
      Diff=2.06E-5*(t/273.0)**1.75
      B=LATH*MMM/(RR*t)-1.0
C
   !> find undersaturation of w. vapour at 2 metres
      !> {sat pressure}
      Es=611.15*EXP(22.452*(t-273.0)/t)
      !> {sat density}
      SvDens=(Es*MMM)/(RR*t)
C
      !> {undersaturation at 2 m}
      Sigma2=rh-1.0
C
      IF(u.GT.Uthr) THEN
C
   !> define saltation parameters and calculate saltation
   !>    rate using 10/1987 MODEL OF BLOWING SNOW EQUATIONS
C
         !>{Eq. 6.3}
         Usthr=0.03697*Uthr
         !>{Eq. 6.2 rev}
         Ustar=0.02264*u**1.295
         !>{Raupach}
         IF(E_StubHt.GT.0.0001) THEN
           RauTerm=1.0/((1.0-Sigma*Lambda)*(1.0+mBeta*Lambda))
         ELSE
           RauTerm=1.0
         ENDIF
         !>{Eq. 4.13}
         Hsalt=C2/(2.0*gg)*Ustar**2
         !>{Eq. 4.14 updated}
         Nsalt=2.0*rhoo/(C2*C3*Ustar)*(RauTerm-(Usthr**2)/(Ustar**2))
C
         IF(Nsalt.LE.0.0) THEN
            CALL SUM (TQsalt, TQsum, SBsum, SBsalt, DriftH, SublH)
            RETURN
         ENDIF
C
         !> Eq. 4.20
         TQsalt=C1*rhoo*Usthr/(gg*C3*Ustar)*(Ustar**2*RauTerm-Usthr**2)
C
   !> calculate sublimation rate in the saltation layer
C
         Mpr=0.0001
         Htran=0.9*M_PI*(Mpr**2)*Qstar
         Alpha=5.0
C
         !> Eq. 6.20, Revised in May. 1997
         SigmaZ=Sigma2*(1.019+0.027*LOG(Hsalt))
         IF(SigmaZ.GT.-0.01) THEN
           SigmaZ=-0.01
         ENDIF
         !> Eq. 6.25
         Vsalt=0.6325*Ustar+2.3*Usthr
         !> Eq. 6.22
         Reyn=(2.0*Mpr*Vsalt)/1.88E-5
         !> Eq. 6.22
         Nuss=1.79+0.606*SQRT(Reyn)
         A=Lamb*t*Nuss;
         C=1.0/(Diff*SvDens*Nuss)
         DmDt=((2.0*M_PI*Mpr*SigmaZ)-(Htran*B/A))/((LATH*B/A)+C)
C
         !>{Eq. 6.16} {Gamma Dist. Corr.}
         Mpm=4.0/3.0*M_PI*DICE*Mpr*Mpr**2*(1.0+3.0/Alpha+2.0/(Alpha**2))
         !> Sublimation rate coefficient Eq. 6.13
         Vs=DmDt/Mpm
         !> Eq. 6.11
         SBsalt=Vs*Nsalt*Hsalt
C
   !> calculate mass flux in the suspended layers and the sublimation
   !>     rate for layers of height Inc from height r to b
C
         !> Eq. 5.27
         Zr=0.05628*Ustar
         Alpha=15.0
         Inc=0.0001
C
   !> Loop to find the first suspended drift density level, r
   !>     from the reference level Zr
   !>     To preserve continuity with saltation the first suspended
   !>     level drift density is less than or equal to Nsalt.
C
         TQsum=0.0
         SBsum=0.0
C
         Z=Zr+Inc
         DO 100 WHILE (Z.LE.0.15)
            Nz=0.8*EXP(-1.55*((0.05628*Ustar)**(-0.544)-Z**(-0.544)))
C
            !> Eq. 5.26, Revised in Dec. 1995
            IF(Nz.LE.Nsalt) THEN
              GOTO 1000
            ENDIF
            Z=Z+Inc
  100    CONTINUE
 1000    Lb=Z+Inc
         Z=Lb
         Inc=0.001
C
   !> find height of fully-developed boundary layer for turbulent
   !>     diffusion using a form of Pasquills plume dispersion eq.
   !>     iterate towards Bound
C
         Bd=1.0
         !> Eq. 6.6
         Bound=ZD+(M2KAARMAN*(Fetch-XD)*(LOG(Bd*162.926/
     1     (Ustar**2))*LOG(ZD*162.926/(Ustar**2)))**(-0.5))
         DO 200 WHILE (ABS(Bound-Bd).GT.0.001)
            Bd=Bound
            !> Eq. 6.9
         Bound=ZD+(M2KAARMAN*(Fetch-XD)*(LOG(Bd*162.926/
     1     (Ustar**2))*LOG(ZD*162.926/(Ustar**2)))**(-0.5))
  200    CONTINUE
C
C
   !> Loop to calculate the suspended mass flux up to 5 metres
   !>     and the total sublimation rate to the top of the boundary layer
   !>   at increments of 1 mm to 50cm & increments of 10 cm to b
C
 2000    H=Z+Inc;
         DO 300 WHILE (H.LE.Bound)
            Nh=0.8*EXP(-1.55*((0.05628*Ustar)**(-0.544)-H**(-0.544)))
            Nz=Nh
            !> Eq. 5.17a
            UstarZ=Ustar*(1.2/(1.2+Nz))**0.5
            !> Eq. 4.17r
            Uz=(UstarZ/M1KAARMAN)*LOG(H/((0.00613*(Ustar**2))+Zstb))
            IF(Uz.GT.0.) THEN
              !> Eq. 6.15
              Mpr= (4.6E-5)*H**(-0.258)
              IF(H.GE.5.0) THEN
                Mpr=30E-6
              ENDIF
C
              Htran=0.9*M_PI*Mpr**2*Qstar
              !> Eq. 6.14
              Alpha=4.08+12.6*H
              IF(H.GE.1.5) THEN
                Alpha=25.0
              ENDIF
C
              !> Eq. 6.20, Revised in May. 1997
              SigmaZ=Sigma2*(1.019+0.027*LOG(H))
              IF(SigmaZ.GT.-0.01) THEN
                SigmaZ=-0.01
              ENDIF
              !> Eq. 5.18
              Omega=(1.1E7)*Mpr**1.8
              Vsusp=Omega+0.0106*Uz**1.36
              !> Eq. 6.22
              Reyn=(2.0*Mpr*Vsusp)/(1.88E-5)
              !> Eq. 6.21
              Nuss=1.79+0.606*SQRT(Reyn)
              A=Lamb*t*Nuss
              C=1.0/(Diff*SvDens*Nuss)
              DmDt=((2.0*M_PI*Mpr*SigmaZ)-(Htran*B/A))/((LATH*B/A)+C)
              !> Eq. 6.16 Gamma Dist. Corr.
              Mpm=1.333*M_PI*DICE*(Mpr**2)*Mpr *
     1         (1.0+3.0/Alpha+2.0/(Alpha**2))
              !> Eq. 6.13
              Vs=DmDt/Mpm
              !> mg
              Sbz=Vs*Nz*Inc
              !>Eq. 6.12
              SBsum=SBsum+Sbz
              !> Eq. 5.4
              Qz=Nz*Uz*Inc
             !> Eq. 5.5
             IF(H.GE.5.0) THEN
               Qz=0.0
             ENDIF
                TQsum=TQsum+Qz
             IF(Nz.GE.0.00001) THEN
                IF((H-Inc).GE.0.5 .AND. H.LT.0.6) THEN
                  Inc=0.1
                  Z=0.5
                  GOTO 2000
                ENDIF
             ELSE
                CALL SUM (TQsalt, TQsum, SBsum, SBsalt, DriftH, SublH)
             RETURN
             ENDIF
C
            ELSE
               TQsalt=0.0
               TQsum=0.0
               SBsalt=0.0
               SBsum=0.0
               CALL SUM (TQsalt, TQsum, SBsum, SBsalt, DriftH, SublH)
               RETURN
            ENDIF
            H=H+Inc
  300    CONTINUE
      ENDIF
      CALL SUM (TQsalt, TQsum, SBsum, SBsalt, DriftH, SublH)
C
      RETURN
      END