      SUBROUTINE PBSMrun(ZSNOW,WSNOW,SNO,RHOSNO,TSNOW,HTCS,
     1       ZSNOCS,ZSNOGS,ZSNOWC,ZSNOWG,
     2       HCPSCS,HCPSGS,HCPSC,HCPSG,
     3       TSNOWC,TSNOWG,TSNOCS,TSNOGS,
     4       RHOSC,RHOSG,RHOSCS,RHOSGS,
     5       XSNOWC,XSNOWG,XSNOCS,XSNOGS,
     6       WSNOCS,WSNOGS,
     7       FC, FG, FCS, FGS,
     9       fetch, N_S, A_S, Ht,
     9       ST, SU, SQ, PRES, PRE,
     +       DrySnow, SnowAge, Drift, Subl,
     +       TSNOWds,
     +       ILG,IL1,IL2,N,ZREFM,ZOMLCS,ZOMLNS)
!    +                 ST, SU, SQ, PRES, 
!    1                Drift, Subl, TSNOWds, RHOSNOds, ZSNOWds,
!    2                TSNOW, RHOSNO, ZSNOW, WSNOW, SNO,
!    3                S, DrySnow, SnowAge,
!    4                ILG,IL1,IL2,JL,
!    5                fetch, N_S, A_S, Ht, N,
!    6                FC, FG, FCS, FGS, HTCS,
!    7                ZSNOWC,ZSNOWG,ZSNOCS,ZSNOGS,
!    8                HCPSC,HCPSG,HCPSCS,HCPSGS,
!    9                TSNOWC,TSNOWG,TSNOCS,TSNOGS,
!    +                RHOSC,RHOSG,RHOSCS,RHOSGS,
!    +                XSNOWC,XSNOWG,XSNOCS,XSNOGS,
!    +                WSNOCS,WSNOGS)
C
C     * OCT 2010 - M.MACDONALD. Driver for PBSM calculations.
C     *                         Single column calculations for blowing snow
C     *                         transport and sublimation. Based on
C     *                         JW Pomeroy thesis (1988; UofS), Pomeroy et al.
C     *                         (1993; JH), and Pomeroy and Li (2000; JGR).
C     *                         
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,I,N
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL ZSNOW(ILG), WSNOW(ILG), SNO(ILG),TSNOW(ILG),RHOSNO(ILG),
     1     DrySnow(ILG), SnowAge(ILG),HTCS(ILG),
     2     ZSNOWC(ILG),ZSNOWG(ILG),ZSNOCS(ILG),ZSNOGS(ILG),
     3     HCPSC(ILG),HCPSG(ILG),HCPSCS(ILG),HCPSGS(ILG),
     4     TSNOWC(ILG),TSNOWG(ILG),TSNOCS(ILG),TSNOGS(ILG),
     5     RHOSC(ILG),RHOSG(ILG),RHOSCS(ILG),RHOSGS(ILG),
     6     XSNOWC(ILG),XSNOWG(ILG),XSNOCS(ILG),XSNOGS(ILG),
     7     WSNOCS(ILG),WSNOGS(ILG)
C
C     * INPUT ARRAYS.
C
      REAL ST(ILG), SU(ILG), SQ(ILG),
     1     S(ILG), PRES(ILG), PRE(ILG),
     2     N_S(ILG), A_S(ILG), Ht(ILG),fetch(ILG),
     3     FC(ILG), FG(ILG), FCS(ILG), FGS(ILG),
     4     ZREFM(ILG),ZOMLCS(ILG),ZOMLNS(ILG)
C
C     * OUTPUT ARRAYS.
C
      REAL Subl(ILG),Drift(ILG),TSNOWds(ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL Znod, Ustar, Ustn, E_StubHt, Lambda, Ut,
     1     Uten_Prob, DriftH, SublH, Prob, mBeta,
     2     gru_loss, sub_loss, swe, sub_zloss,
     3     RHOSNOds,ZSNOWds,u10,z0
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
C!    * PBSM PARAMETERS
 !    REAL rhoo, Qstar, MMM, RR, LATH, DICE, ZD, XD, gg,
 !   1     Betaa, C1, C2, C3, M1KAARMAN, M2KAARMAN, M_PI,
 !   2     DegToRad
!
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
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
C      
C--------------------------------------------------------------
C
      DO 100 I=IL1,IL2
       Drift(I)=0.0
       Subl(I)=0.0
       Prob=0.0
       DriftH=0.0
       SublH=0.0
C
       IF(ZSNOW(I).GT.1.0E-3) THEN
C
         !Set values for mB for partitioning shear stress over vegetation
         !(different for vegetation categories;
         ! see MacDonald, Pomeroy & Pietroniro (2009, Hydrol. Proc.))
         IF(FCS(I).GE.FGS(I)) THEN
          mBeta=32.0
         ELSE
          mBeta=170.0
         ENDIF
       !>GRU-level snow transport & sublimation calculations
C         !> depths(m), SWE(mm; kg/m^2)
         E_StubHt=Ht(I)-ZSNOW(I) !height of vegetation above snowpack
         IF(E_StubHt.LT.0.0001) THEN
           E_StubHt=0.0001
         ENDIF !IF(E_StubHt.LT.0.0001) THEN
C
         if((FCS(I)+FGS(I)).gt.0.)then
           z0=(FCS(I)*exp(ZOMLCS(I))+FGS(I)*exp(ZOMLNS(I)))
     1         /(FCS(I)+FGS(I))
         else
           z0=E_StubHt*2/3
         endif
         u10=SU(I)*log(10./z0)/log(ZREFM(I)/z0)
         
           !> Eq. 6.2 rev. Pomeroy thesis, Ustar over fallow
           Ustar=0.02264*u10**1.295 !friction velocity
C
         IF(E_StubHt.GT.0.01) THEN
           !> Eq. 29, Snowcover Accumulation, Relocation & Management book (1995)
           Znod=(Ustar**2)/163.3+0.5*N_S(I)*E_StubHt*A_S(I)
           !> Raupach Eq. 1
           Lambda=N_S(I)*A_S(I)*E_StubHt
           Ustn=Ustar*SQRT((mBeta*Lambda)/(1.0+mBeta*Lambda))
           Uten_Prob=(LOG(10.0/Znod))/M1KAARMAN*SQRT(Ustar-Ustn)
         ELSE
           Uten_Prob=u10
         ENDIF !IF(E_StubHt.GT.0.01) THEN
C
         !>Calculate probability of blowing snow occurence
         CALL ProbabilityThreshold(ZSNOW(I),ST(I),Uten_Prob,
     1               Prob,Ut,PRE(I),SnowAge(I),DrySnow(I))
C
         IF(Prob.GT.0.001)  THEN
           Ut=Ut*0.8
C
	   !>Single column calculations of blowing snow transport & sublimation
	   CALL PBSMrates(E_StubHt,Ut,DriftH,SublH,
     1               ST(I),SU(I),SQ(I),fetch(I),N_S(I),A_S(I),
     2               mBeta,PRES(I),I,N)
C
           Drift(I)=DriftH*Prob/fetch(I)
           Subl(I)=SublH*Prob
C
!>         handle insufficient snowpack
C
           IF(Drift(I)+Subl(I).GT.SNO(I)) THEN
             Subl(I)=SNO(I)*Subl(I)/(Subl(I)+Drift(I))
             Drift(I)=SNO(I)-Subl(I)
           ENDIF
C
           RHOSNOds=RHOSNO(I)
           ZSNOWds=(Drift(I)+Subl(I))/RHOSNOds
           ZSNOW(I)=ZSNOW(I)-ZSNOWds
           IF(ZSNOW(I).LE.0.) THEN
             ZSNOW(I)=0.
             IF(WSNOW(I).GT.0.) THEN
               Subl(I)=Subl(I)+WSNOW(I)
               WSNOW(I)=0.
               WSNOCS(I)=0.
               WSNOGS(I)=0.
             ENDIF
           ENDIF
           TSNOWds(I)=TSNOW(I)
         ELSE
         !>  calculations for non-blowing snow events
           Subl(I)=0.0
           Drift(I)=0.0
           ZSNOWds=0.0
           TSNOWds(I)=TSNOW(I)
         ENDIF !IF(Prob.GT.0.001)  THEN
       ELSE
       !>  calculations for non-blowing snow events
         Subl(I)=0.0
         Drift(I)=0.0
         ZSNOWds=0.0
         TSNOWds(I)=TSNOW(I)
       ENDIF !IF(ZSNOW(I).GT.1.0E-3) THEN
C
    !> Recalculate subarea snow properties after snow transport for all four subareas
          !if blowing snow occured
          IF(Drift(I).GT.0. .OR. Subl(I).GT.0.) THEN 
                 !snow mass loss is sum of transport + sublimation
                 gru_loss=Drift(I)+Subl(I)
                 IF(FC(I).GT.0. .AND. ZSNOWC(I).GT.0.) THEN
                   HTCS(I)=HTCS(I)-FC(I)*HCPSC(I)*(TSNOWC(I)+TFREZ)
     1                  *ZSNOWC(I)/DELT
                   swe=RHOSC(I)*ZSNOWC(I) !pre-BS SWE [kg/m^2]
                   !SWE to be removed after blowing snow [kg/m^2]
                   sub_loss=gru_loss* 
     1                      FC(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
     1                      *(FC(I)+FG(I)+FCS(I)+FGS(I))/FC(I)
                   !depth to be removed after blowing snow [m]
                   sub_zloss=sub_loss/RHOSC(I)
                   !if calculated depth loss exceeds snow available
                   IF(sub_zloss.GE.ZSNOWC(I)) THEN 
                     !rescale gru-level SWE mass loss
                     sub_loss=swe*FC(I)/(FC(I)+FG(I)+FCS(I)+FGS(I)) 
                     ZSNOWC(I)=0. !set snow depth to zero
                   !snowpack depth is greater than calculated blowing snow loss
                   ELSE 
                     ZSNOWC(I)=ZSNOWC(I)-sub_zloss 
                     sub_loss=sub_loss*FC(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                   ENDIF
                   !subtract this subarea's blowing snow mass loss from GRU total
                   gru_loss=gru_loss-sub_loss 
                   if(ZSNOWC(I).gt.0.)then
                   HCPSC(I)=HCPICE*RHOSC(I)/RHOICE
                   else
                   HCPSC(I)=0.
                   endif
                   HTCS(I)=HTCS(I)+FC(I)*HCPSC(I)*(TSNOWC(I)+TFREZ)
     1                  *ZSNOWC(I)/DELT
                   IF(ZSNOWC(I).GT.0.) THEN
                     XSNOWC(I)=1.0
                   ELSE
                     XSNOWC(I)=0.
                   ENDIF
                 ENDIF
                 IF(FG(I).GT.0. .AND. ZSNOWG(I).GT.0.) THEN
                   HTCS(I)=HTCS(I)-FG(I)*HCPSG(I)*(TSNOWG(I)+TFREZ)
     1                  *ZSNOWG(I)/DELT
                   swe=RHOSG(I)*ZSNOWG(I)
                   sub_loss=gru_loss*
     1                      FG(I)/(FG(I)+FCS(I)+FGS(I))
     1                      *(FC(I)+FG(I)+FCS(I)+FGS(I))/FG(I)
                   sub_zloss=sub_loss/RHOSG(I)
                   IF(sub_zloss.GE.ZSNOWG(I)) THEN
                     sub_loss=swe*FG(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                     ZSNOWG(I)=0.
                   ELSE
                     ZSNOWG(I)=ZSNOWG(I)-sub_zloss
                     sub_loss=sub_loss*FG(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                   ENDIF
                   gru_loss=gru_loss-sub_loss
                   if(ZSNOWG(I).gt.0.)then
                   HCPSG(I)=HCPICE*RHOSG(I)/RHOICE
                   else
                   HCPSG(I)=0.
                   endif
                   HTCS(I)=HTCS(I)+FG(I)*HCPSG(I)*(TSNOWG(I)+TFREZ)
     1                  *ZSNOWG(I)/DELT
                   IF(ZSNOWG(I).GT.0.) THEN
                     XSNOWG(I)=1.0
                   ELSE
                     XSNOWG(I)=0.
                   ENDIF
                 ENDIF
                 IF(FCS(I).GT.0. .AND. ZSNOCS(I).GT.0.) THEN
                   HTCS(I)=HTCS(I)-FCS(I)*HCPSCS(I)*(TSNOCS(I)
     1                  +TFREZ)*ZSNOCS(I)/DELT
                   swe=RHOSCS(I)*ZSNOCS(I)
                   sub_loss=gru_loss*
     1                      FCS(I)/(FCS(I)+FGS(I))
     1                      *(FC(I)+FG(I)+FCS(I)+FGS(I))/FCS(I)
                   sub_zloss=sub_loss/RHOSCS(I)
                   IF(sub_zloss.GE.ZSNOCS(I)) THEN
                     sub_loss=swe*FCS(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                     ZSNOCS(I)=0.
                     WSNOCS(I)=0.
                   ELSE
                     ZSNOCS(I)=ZSNOCS(I)-sub_zloss
                    sub_loss=sub_loss*FCS(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                   ENDIF
                   gru_loss=gru_loss-sub_loss
                   if(ZSNOCS(I).gt.0.)then
                   HCPSCS(I)=HCPICE*RHOSCS(I)/RHOICE+HCPW*WSNOCS(I)/
     1                  (RHOW*ZSNOCS(I))
                   else
                   HCPSCS(I)=0.
                   WSNOCS(I)=0.
                   endif
                   HTCS(I)=HTCS(I)+FCS(I)*HCPSCS(I)*(TSNOCS(I)
     1                  +TFREZ)*ZSNOCS(I)/DELT
                   IF(ZSNOCS(I).GT.0.) THEN
                     XSNOCS(I)=1.0
                   ELSE
                     XSNOCS(I)=0.
                   ENDIF
                 ENDIF
                 IF(FGS(I).GT.0. .AND. ZSNOGS(I).GT.0.) THEN
                   HTCS(I)=HTCS(I)-FGS(I)*HCPSGS(I)*(TSNOGS(I)
     1                  +TFREZ)*ZSNOGS(I)/DELT
                   swe=RHOSGS(I)*ZSNOGS(I)
                   sub_loss=gru_loss*
     1                      FGS(I)/(FGS(I))
     1                      *(FC(I)+FG(I)+FCS(I)+FGS(I))/FGS(I)
                   sub_zloss=sub_loss/RHOSGS(I)
                   IF(sub_zloss.GE.ZSNOGS(I)) THEN
                     sub_loss=swe*FGS(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                     ZSNOGS(I)=0.
                     WSNOGS(I)=0.
                   ELSE
                     ZSNOGS(I)=ZSNOGS(I)-sub_zloss
                    sub_loss=sub_loss*FGS(I)/(FC(I)+FG(I)+FCS(I)+FGS(I))
                   ENDIF
                   gru_loss=gru_loss-sub_loss
                   if(ZSNOGS(I).gt.0.)then
                   HCPSGS(I)=HCPICE*RHOSGS(I)/RHOICE+HCPW*WSNOGS(I)/
     1                  (RHOW*ZSNOGS(I))
                   else
                     HCPSGS(I)=0.
                     WSNOGS(I)=0.
                   endif
                   HTCS(I)=HTCS(I)+FGS(I)*HCPSGS(I)*(TSNOGS(I)
     1                  +TFREZ)*ZSNOGS(I)/DELT
                   IF(ZSNOGS(I).GT.0.) THEN
                     XSNOGS(I)=1.0
                   ELSE
                     XSNOGS(I)=0.
                   ENDIF
                 ENDIF
	    	     !> Calculate snowpack properties at GRU-level
                 if(XSNOCS(I).gt.0.or.XSNOGS(I).gt.0.or. 
     +                XSNOWC(I).gt.0.or.XSNOWG(I).gt.0.)then
                 TSNOW(I)=(FCS(I)*(TSNOCS(I)+TFREZ)*HCPSCS(I)*
     1                  ZSNOCS(I)*XSNOCS(I) +
     2                  FGS(I)*(TSNOGS(I)+TFREZ)*HCPSGS(I)*
     3                  ZSNOGS(I)*XSNOGS(I) +
     4                  FC(I) *(TSNOWC(I)+TFREZ)*HCPSC(I)*
     5                  ZSNOWC(I)*XSNOWC(I) +
     6                  FG(I) *(TSNOWG(I)+TFREZ)*HCPSG(I)*
     7                  ZSNOWG(I)*XSNOWG(I))/
     8                 (FCS(I)*HCPSCS(I)*ZSNOCS(I)*XSNOCS(I) +
     9                  FGS(I)*HCPSGS(I)*ZSNOGS(I)*XSNOGS(I) +
     A                  FC(I) *HCPSC(I)*ZSNOWC(I)*XSNOWC(I) +
     B                  FG(I) *HCPSG(I)*ZSNOWG(I)*XSNOWG(I))
                 RHOSNO(I)=(FCS(I)*RHOSCS(I)*ZSNOCS(I)*XSNOCS(I) +   
     1                   FGS(I)*RHOSGS(I)*ZSNOGS(I)*XSNOGS(I) +   
     2                   FC(I)*RHOSC(I)*ZSNOWC(I)*XSNOWC(I) +   
     3                   FG(I)*RHOSG(I)*ZSNOWG(I)*XSNOWG(I))/    
     4                  (FCS(I)*ZSNOCS(I)*XSNOCS(I) +
     5                   FGS(I)*ZSNOGS(I)*XSNOGS(I) +                 
     6                   FC(I)*ZSNOWC(I)*XSNOWC(I) +
     7                   FG(I)*ZSNOWG(I)*XSNOWG(I))
                 ZSNOW(I)=FCS(I)*ZSNOCS(I) + FGS(I)*ZSNOGS(I) +
     1                 FC(I)*ZSNOWC(I) + FG(I)*ZSNOWG(I)
                 SNO(I)=ZSNOW(I)*RHOSNO(I)
                 WSNOW(I)=FCS(I)*WSNOCS(I) + FGS(I)*WSNOGS(I)
                 else
                 TSNOW(I)=TFREZ
                 RHOSNO(I)=0.
                 ZSNOW(I)=0.
                 SNO(I)=0.
                 Subl(I)=Subl(I)+WSNOW(I)
                 WSNOW(I)=0.
                 endif
          ENDIF !Drift(I).GT.0. .OR. Subl(I).GT.0.
  100 CONTINUE
      RETURN
      END