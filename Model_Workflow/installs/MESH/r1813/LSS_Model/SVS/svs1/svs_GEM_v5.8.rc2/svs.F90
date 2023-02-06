
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer,
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer
!version 3 or (at your option) any later version that should be found at:
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software;
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec),
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
!** S/P SVS
!
subroutine svs(BUS, BUSSIZ, PTSURF, PTSURFSIZ, DT, KOUNT, TRNCH, N, M, NK)
   use sfclayer_mod, only: sl_prelim,sl_sfclayer,SL_OK

!VV DEBUT MODIFICATION POUR MESH
#ifdef RUNSVS
    use runsvs_mod
#endif
!   use sfcbus_mod
!VV FIN MODIFICATION POUR MESH

   use sfc_options
   use svs_configs
   implicit none
#include <arch_specific.hf>
!
!Author
!          S. Belair (January 1997), M. Abrahamowicz, S.Z. Husain (2012) 
!Revisions
!
! 001      Rewrite ISBA into SVS (multi-budget, multi-layer isba)
!              Add sea ice surface
!Object
!          Multitasking of the surface scheme SVS
!
!Arguments
!
!               - Input/Output -
! BUS           bus of surface variables
!
!               - Input -
! BUSSIZ        size of the surface bus
! PTSURF        surface pointers
! PTSURFSIZ     dimension of ptsurf
! KOUNT         number of timestep
! TRNCH         row number
! DT            timestep
! N             running length
! M             horizontal dimension
! NK            vertical dimension
!
!
!

   integer BUSSIZ, LONCLEF, VSIZ, N, NK, KOUNT, TRNCH
   real DT
   real,target :: bus(bussiz)
   integer PTSURFSIZ
   integer PTSURF(PTSURFSIZ)
   include "thermoconsts.inc"

   integer SURFLEN


!VV DEBUT MODIFICATION POUR MESH
integer ptr, x, j, k
#ifdef RUNSVS
#include "xptsurf.cdk"
#endif

! WARNING !!!! x in bus(x(varname,1,1)) is defined in the line below
! it is now case sensitive
!#define x(fptr,fj,fk) ptsurf(vd%fptr%i)+(fk-1)*surflen+fj-1
! so defined X() also to make it NOT case sensitive
!#define X(fptr,fj,fk) x(fptr,fj,fk)
!VV FIN MODIFICATION POUR MESH


   integer, parameter :: INDX_SFC = INDX_SOIL

   logical, parameter :: TDIAGLIM_FALSE = .false.



!     
! LOCAL ARRAYS defined for variables passed to 
! explicit interface in sl_prelim, sl_sfcmod ... need to pass arrays not address of first
! element, so use:
! bus(x(varname,i,k) :)        instead of 
! bus(x(varname,i,k)  )
! PASSING BUSES WILL NOT WORK FOR EXPLICIT INTERFACE... DIMENSION of VARIABLES
! DEFINED LOCALLY based on size of first variable... which in this case is WHOLE! BUS

   real,pointer,dimension(:) :: hum
   real,pointer,dimension(:) :: psm
   real,pointer,dimension(:) :: ttm
   real,pointer,dimension(:) :: uum
   real,pointer,dimension(:) :: vvm
   real,pointer,dimension(:) :: z0h
   real,pointer,dimension(:) :: z0m
   real,pointer,dimension(:) :: zdlat
   real,pointer,dimension(:) :: zfcor
   real,pointer,dimension(:) :: zqdiag
   real,pointer,dimension(:) :: zqsurf
!VV DEBUT MODIFICATION POUR MESH
!   real,pointer,dimension(:) :: zsnodp
!VV FIN MODIFICATION POUR MESH
   real,pointer,dimension(:) :: ztdiag
   real,pointer,dimension(:) :: zthetaa
   real,pointer,dimension(:) :: ztsa
   real,pointer,dimension(:) :: zudiag
   real,pointer,dimension(:) :: zvdiag
   real,pointer,dimension(:) :: zzusl
   real,pointer,dimension(:) :: zztsl

!
!

!******************************************************
!     LOCAL ARRAYS  --- ALPHABETICAL ORDER
!******************************************************
!

!VV DEBUT MODIFICATION POUR MESH
!   integer i,k,j,m, masklat50(n)
   integer i,m, masklat50(n)
!VV FIN MODIFICATION POUR MESH

   real,dimension(n) :: alva, cg, cvpa, del, dwaterdt
   real,dimension(n) :: esnofrac, esvnofrac, eva, gamva, hrsurf
   real,dimension(n) :: leff, lesnofrac, lesvnofrac, rainrate_mm
   real,dimension(n) :: rgla, rhoa, snowrate_mm, stom_rs, stomra
   real,dimension(n) :: suncosa, sunother1, sunother2, sunother3
   real,dimension(n) :: sunother4, trad, tva, vdir, vmod,  wrmax, wvegt
! 
   real, dimension(n,nl_svs) :: isoilt, wsoilt
!
!******************************************************
!
      real,pointer,dimension(:) :: zfsolis
!      
      REAL HZ, HZ0, JULIEN, JULIAND    
!
!
!---------------------------------------------------------
!  ROUGNESS
!

!  GENERAL COMMENT:
!   want z0m *with orography* at resolution coarser than 3km... 
!  and local onl_svsy at higher resolution. Makes sure that the local z0 
!  calculated based on veg. height database, and NOT
!  on look-up table (values are quite weak...)
!
!     Conversion factor to convert from momemtum roughness to thermal roughness
      REAL, PARAMETER :: Z0M_TO_Z0H = 0.2
!
!     Thermal roughness for snow
      REAL, PARAMETER :: Z0HSNOW = 0.010
!
!     Momentum roughness for bare ground 
      REAL, PARAMETER :: Z0MBG = 0.05
!---------------------------------------------------------
!
!     In the offline mode the t-step 0 is (correctly) not performed
      IF (FLUVERT.EQ.'SURFACE'.AND.KOUNT.EQ.0) RETURN
!
      SURFLEN = M


! assign pointers
      hum      (1:n) => bus( x(humoins,1,nk)     : )
      psm      (1:n) => bus( x(pmoins,1,1)       : )
      ttm      (1:n) => bus( x(tmoins,1,nk)      : )
      uum      (1:n) => bus( x(umoins,1,nk)      : )
      vvm      (1:n) => bus( x(vmoins,1,nk)      : )
      z0h      (1:n) => bus( x(z0t,1,indx_sfc)   : )
      z0m      (1:n) => bus( x(z0,1,indx_sfc)    : )
      zdlat    (1:n) => bus( x(dlat,1,1)         : )
      zfcor    (1:n) => bus( x(fcor,1,1)         : )
      zqdiag   (1:n) => bus( x(qdiag,1,1)        : )
      zqsurf   (1:n) => bus( x(qsurf,1,indx_sfc) : )
!VV DEBUT MODIFICATION POUR MESH
!      zsnodp   (1:n) => bus( x(snodp,1,indx_sfc) : )
!VV FIN MODIFICATION POUR MESH
      ztdiag   (1:n) => bus( x(tdiag,1,1)        : )
      zthetaa  (1:n) => bus( x(thetaa,1,1)       : )
      ztsa     (1:n) => bus( x(tsa,1,1)          : )     
      zudiag   (1:n) => bus( x(udiag,1,1)        : )
      zvdiag   (1:n) => bus( x(vdiag,1,1)        : )
      zzusl    (1:n) => bus( x(zusl,1,1)         : )
      zztsl    (1:n) => bus( x(ztsl,1,1)         : )
!  
!
      IF (RADSLOPE) THEN
         zFSOLIS(1:n)   => bus( x(fluslop,1,1)      : )
      ELSE
         zFSOLIS(1:n)   => bus( x(flusolis,1,1)     : )
      ENDIF

    
      ! CONVERT RAINRATE AND SNOWRATE FROM M/S TO MM/S TO MATCH UNITS OF
      ! OTHER WATER FLUXES (EVAPORATION etc.)
      
      DO I=1,N
          rainrate_mm(i) = bus(x(rainrate,i,1)) * M_TO_MM
          snowrate_mm(i) = bus(x(snowrate,i,1)) * M_TO_MM
      ENDDO



!     CALCULATE GREENWICH HOUR
      HZ0 = DATE(5) + float(DATE(6))/360000.0 
!
      HZ = AMOD ( HZ0+(FLOAT(KOUNT)*DT)/3600. , 24. )
!
!                     Determine the current julian day
!
      JULIEN = JULIAND( DT, KOUNT, DATE )
!
!     Get cosinus of solar angle at LOCAL HOUR
! 
! 
      CALL SUNCOS1(SUNCOSA,SUNOTHER1,SUNOTHER2,SUNOTHER3,SUNOTHER4, N, & 
          bus(x (DLAT,1,1)), BUS(x(DLON,1,1)),  &
           HZ, JULIEN, DATE, .false.)

      IF(KOUNT.EQ.1) then
         ! at KOUNT eq. 0 have RETURN, so can't use kount=1 
         DO I=1,N
!           Make sure rootdp does not exceed total soil depth
            bus(x(ROOTDP,I,1))=min( bus(x(ROOTDP,I,1)) , DL_SVS(NL_SVS) )
         END DO
      ENDIF

      ! Calculate mask for VF26 , |LAT|<=50 have masklat50=1, otherwise masklat50=0

      DO I=1,N

         if( abs (   bus(x(DLAT,I,1)) * 180./acos(-1.)  ) .le.50.) then
            masklat50(i)=1
         else
            masklat50(i)=0
         endif


      ENDDO
!
!******************************************************************
!                  SVS SUBROUTINES START HERE
!******************************************************************
!

      i = sl_prelim(ttm,hum,uum,vvm,psm,zzusl,VMOD,VDIR,TVA,RHOA, &
           min_wind_speed=2.5,min_wind_reduc='linear')


      if (i /= SL_OK) then
         print*, 'Aborting in SVS() because of error returned by sl_prelim()'
         stop
      endif

!
!

      CALL SOILI_SVS( BUS(x(WSOIL ,1,1)), &
           BUS(x(ISOIL  ,1,1)), &  
           BUS(x(SNOMA  ,1,1)), BUS(x(SNVMA  ,1,1)), &   
           BUS(x(SNORO  ,1,1)), BUS(x(SNVRO  ,1,1)), &  
           BUS(x(VEGH   ,1,1)), &  
           BUS(x(VEGL   ,1,1)), BUS(x(CGSAT  ,1,1)), &  
           BUS(x(WSAT   ,1,1)), BUS(x(WWILT  ,1,1)), &  
           BUS(x(BCOEF  ,1,1)), &  
           BUS(x(CVH    ,1,1)), BUS(x(CVL    ,1,1)), &  
           BUS(x(ALVH   ,1,1)), BUS(x(ALVL   ,1,1)), &   
           BUS(x(EMISVH ,1,1)), BUS(x(EMISVL ,1,1)), &  
           BUS(x(EMISTG ,1,1)), &
           BUS(x(RGLVH  ,1,1)), BUS(x(RGLVL  ,1,1)), &  
           BUS(x(STOMRVH,1,1)), BUS(x(STOMRVL,1,1)), &  
           BUS(x(GAMVH  ,1,1)), BUS(x(GAMVL  ,1,1)), &           
           BUS(x(LAIVH  ,1,1)), BUS(x(LAIVL  ,1,1)),  & 
           BUS(x(Z0MVH  ,1,1)),   &
           BUS(x(Z0MVL  ,1,1)), BUS(x(z0,1,indx_soil)), &  
           BUS(x(CLAY   ,1,1)), BUS(x(SAND   ,1,1)), &  
           BUS(x(DECIDUOUS,1,1)),BUS(x(EVERGREEN,1,1)), &  
           BUS(x(LAIDECI,1,1)),   &
           Z0HSNOW, Z0MBG, Z0M_TO_Z0H, &
           BUS(x(SVS_WTA,1,1)), CG, &
           BUS(x(PSNGRVL,1,1)),  &  
           BUS(x(Z0T  ,1,indx_soil)),  & 
           BUS(x(ALGR,1,1)),BUS(x(EMISGR,1,1)), &  
           BUS(x(PSNVH  ,1,1)), BUS(x(PSNVHA ,1,1)), &  
           ALVA, BUS(x(LAIVA  ,1,1)), CVPA, EVA,BUS(x(Z0HA ,1,1)), RGLA, STOMRA,   &
           GAMVA, N)
!
!     

      CALL VEGI_SVS ( zfsolis,   &
           bus(x(tmoins ,1,nk)), bus(x(TVEGE   ,1,1)),   &  
           bus(x(humoins,1,nk)), bus(x(pmoins  ,1,1)),   &  
           BUS(x(WSOIL ,1,1)),  &
           RGLA                ,  &   
           bus(x(LAIVA  ,1,1))     , bus(x(LAIVH   ,1,1)),   &  
           STOMRA,     &
           GAMVA, bus(x(WWILT   ,1,1)),      &
           bus(x(WFC     ,1,1)), SUNCOSA,     &
           bus(x(ROOTDP     ,1,1)),  bus(x(D50   ,1,1)),    &
           bus(x(D95   ,1,1)),  BUS(x(PSNGRVL,1,1)), &
           BUS(x(VEGH   ,1,1)), BUS(x(VEGL   ,1,1)), &
           bus(x(RST     ,1,1)),     &
           bus(x(SKYVIEW ,1,1)), bus(x(VEGTRANS,1,1)),   &   
           bus(x(frootd   ,1,1)), bus(x(acroot ,1,1)), WRMAX, N)

      IF(KOUNT.EQ.1) then
         DO I=1,N
            STOM_RS(I) = bus(x(RST,I,1))
         ENDDO
         ! long-term ... define default value for rcctem in inisurf
      else
  
         IF( USE_PHOTO ) THEN

            DO I=1,N
               STOM_RS(I) =  bus(x(RCCTEM,I,1))
            END DO
         ELSE
            DO I=1,N
               STOM_RS(I) = bus(x(RST,I,1))
            END DO
         ENDIF
      
      endif

!
      CALL DRAG_SVS ( bus(x(TGROUND,1,1)), bus(x(TVEGE,1,1)),  &   
           bus(x(WSOIL ,1,1)) ,  &   
           bus(x(WVEG   ,1,1)), bus(x(thetaa,1,1)),  &   
           VMOD, VDIR,  bus(x(humoins,1,nk)),     &
           bus(x(pmoins ,1,1)), STOM_RS,   &  
           bus(x(z0    ,1,indx_soil)), bus(x(WFC,1,1)),      &
           bus(x(LAIVA,1,1)), WRMAX, bus(x(zusl,1,1)), bus(x(ztsl,1,1)),    & 
           bus(x (DLAT,1,1)), &
           bus(x(FCOR,1,1)),bus(x(Z0HA ,1,1)),Z0MBG,  Z0M_TO_Z0H,  &  
           bus(x(RESAGR,1,1)), bus(x(RESAVG,1,1)), &    
           bus(x(HUSURF,1,1)),   &  
           HRSURF,      &
           bus(x(HV,1,1)), DEL,     &
           N )  
!
!
! calculate diagnostics at hghtm_diag=zu,hghtt_diag=zt
! do this because my want to change diagnostic height, but calculate
! temperature and winds at a diff/constant level for use in snow subroutines...
! for now in svs ... don't do it ... will test it later.... because tdiag will be
! recalculated locally.... based on TSA rather than aggregated TDIAG .... 
! ! 
   
!    if (kount.eq.0) then

!     t_diag=bus(x(tdiag    ,1,1)), &
!                      q_diag=bus(x(qdiag    ,1,1)), &
!                      u_diag=bus(x(udiag    ,1,1)), &
!                      v_diag=bus(x(vdiag    ,1,1)), &


!       my_ta = ztdiag
!       my_ua = zudiag
!       my_va = zvdiag
!    else
!       i = sl_sfclayer(zthetaa,hum,vmod,vdir,zzusl,zztsl,ztsoil1,zqsurf, &
!            z0m,z0h,zdlat,zfcor,hghtm_diag=zu,hghtt_diag=zt,             &
!            t_diag=my_ta,u_diag=my_ua,v_diag=my_va,tdiaglim=ISBA_TDIAGLIM) 
!       if (i /= SL_OK) then
!          print*, 'Aborting in isba() because of error returned by sl_sfclayer()'
!          stop
!       endif
!    endif


      CALL SNOW_ALONE ( bus(x(TSNOW,1,1)), bus(x(TSNOW,1,2)),  &     
                     bus(x(SNORO,1,1)),    &   
                     bus(x(SNOAL,1,1)), bus(x(WSNOW,1,1)),  &     
                     bus(x(SNODPL,1,1)),   &   
                     bus(x(SNOMA,1,1)),   &   
                     bus(x(PMOINS,1,1)), VMOD, VDIR,  RHOA,   &    
                     bus(x(THETAA,1,1)),   &   
                     zfsolis, bus(x(FDSI,1,1)),  &    
                     bus(x(HUMOINS,1,nk)), rainrate_mm,  &      
                     snowrate_mm,  BUS(x(TMOINS,1,NK)),  &     
                     bus(x(TDIAG,1,1)),     &   
                     bus(x(UDIAG,1,1)), bus(x(VDIAG,1,1)),  &     
                     bus(x(TSNAVG,1,1)),&
                     bus(x(MELTS,1,1)), bus(x(MELTSR,1,1)),   &    
                     bus(x(RNETSA,1,1)), bus(x(HFLUXSA,1,1)),  &     
                     LESNOFRAC, ESNOFRAC,   &      
                     bus(x(RSNOWSA,1,1)), bus(x(SNODEN,1,1)), bus(x(RESASA,1,1)),  &    
                     DT, bus(x(Z0,1,indx_soil)), Z0HSNOW, bus(x(FCOR,1,1)),  &     
                     bus(x(zusl,1,1)), bus(x(ztsl,1,1)), &
                     bus(x (DLAT,1,1)), bus(x(PSNGRVL ,1,1)), N)     
!    
!
      CALL SNOW_VEG ( bus(x(TSNOWVEG  ,1,1)), bus(x(TSNOWVEG,1,2)),  &  
                     bus(x(SNVRO     ,1,1)),   & 
                     bus(x(SNVAL     ,1,1)), bus(x(WSNV    ,1,1)),   & 
                     bus(x(SNVDP     ,1,1)),    &
                     bus(x(SNVMA     ,1,1)),    &
                     bus(x(PMOINS    ,1,1)), VMOD, VDIR, RHOA,   &  
                     bus(x(THETAA    ,1,1)),    &
                     zfsolis, bus(x(FDSI    ,1,1)),  &  
                     bus(x(HUMOINS   ,1,nk)), rainrate_mm,  &  
                     snowrate_mm, bus(x(TMOINS  ,1,NK)),  &  
                     bus(x(TDIAG     ,1,1)),     &
                     bus(x(UDIAG     ,1,1)), bus(x(VDIAG   ,1,1)),   &
		     bus(x(VEGH   ,1,1)) ,   & 
                     bus(x(SKYVIEW   ,1,1)), bus(x(VEGTRANS,1,1)),   & 
                     bus(x(TVEGE     ,1,2)), bus(x(EMISVH ,1,1)),    &
                     bus(x(WVEG      ,1,1)), bus(x(TSVAVG,1,1)), &
                     bus(x(RNETSV    ,1,1)), bus(x(HFLUXSV ,1,1)),  &  
                     LESVNOFRAC            , ESVNOFRAC,     & 
                     bus(x(RSNOWSV   ,1,1)), bus(x(SNVDEN  ,1,1)), bus(x(RESASV ,1,1)), &  
                     DT,    &
                     bus(x(Z0,1,indx_soil)),  Z0HSNOW, &
                     bus(x(FCOR   ,1,1)), bus(x (DLAT,1,1)),  & 
                     bus(x(zusl,1,1)), bus(x(ztsl,1,1)), bus(x(PSNVHA ,1,1)), N)

!
      CALL EBUDGET_SVS(bus(x(TSA ,1,1)),  &  
                  bus(x(WSOIL     ,1,1)) , bus(x(ISOIL,1,1)),  &   
                  bus(x(TGROUND    ,1,1)) , bus(x(TGROUND,1,2)),   & 
                  bus(x(TVEGE      ,1,1)) , bus(x(TVEGE,1,2)),   &   
                  DT                     , VMOD, VDIR, bus(x(DLAT,1,1)),   &   
                  zfsolis ,ALVA ,bus(x(laiva,1,1)),GAMVA ,     & 
                  BUS(x(ALGR,1,1))        , BUS(x(EMISGR,1,1)),    & 
                  bus(x(FDSI       ,1,1)) , bus(x(thetaa,1,1)),    &   
                  bus(x(FCOR       ,1,1)) , bus(x(zusl,1,1)),    &  
                  bus(x(ztsl       ,1,1)) , bus(x(humoins    ,1,nk)), &
                  bus(x(pmoins,1,1)), RHOA, BUS(x(SVS_WTA,1,1)), &
                  bus(x(Z0,1,indx_soil)) , bus(x(Z0T,1,indx_soil)),&
                  HRSURF,       & 
                  bus(x(HV         ,1,1)) , DEL, STOM_RS ,& 
                  CG,CVPA,EVA,bus(x(PSNGRVL    ,1,1)) ,    &    
                  bus(x(RESAGR,1,1)), bus(x(RESAVG,1,1)),   &
		  bus(x(RESASA,1,1)), bus(x(RESASV,1,1)),   &        
                  bus(x(RNETSA     ,1,1)) , bus(x(HFLUXSA,1,1)),   &   
                  LESNOFRAC               , ESNOFRAC,   &   
                  bus(x(SNOAL      ,1,1)) ,    &  
                  bus(x(TSNOW      ,1,1)) ,    &  
                  bus(x(RNETSV     ,1,1)) , bus(x(HFLUXSV ,1,1)),   &   
                  LESVNOFRAC              , ESVNOFRAC,    &    
                  bus(x(SNVAL      ,1,1)) ,    &  
                  bus(x(TSNOWVEG   ,1,1)) ,   &   
                  bus(x(VEGH       ,1,1)) , bus(x(VEGL   ,1,1)),   &   
                  bus(x(PSNVH      ,1,1)) ,    &   
                  bus(x(PSNVHA     ,1,1)),  bus(x(SKYVIEW   ,1,1)),   &   
                  rainrate_mm,bus(x(WVEG   ,1,1)),bus(x(snoma,1,1)),&
                  bus(x(snvma,1,1)),&
                  bus(x(ALVIS,1,indx_soil)),     & 
                  bus(x(RNET_S     ,1,1)),    &   
                  bus(x(FC  ,1,indx_soil)), bus(x(FV  ,1,indx_soil)),   &    
                  bus(x(LEG        ,1,1)) , bus(x(LEV  ,1,1)),    &   
                  bus(x(LES        ,1,1)) , bus(x(LESV   ,1,1)),    &  
                  bus(x(LER        ,1,1)) , bus(x(LETR       ,1,1)) ,   &  
                  bus(x(EG         ,1,1)) ,   &    
                  bus(x(ER         ,1,1)) , bus(x(ETR    ,1,1)),    &  
                  bus(x(FL         ,1,1)),  bus(x(EFLUX      ,1,1)) ,    &  
                  bus(x(BM         ,1,1)) , bus(x(FQ   ,1,1)),    &  
                  bus(x(bt, 1,indx_soil)) , bus(x(RESAEF,1,1)),   &  
                  LEFF                    , DWATERDT,     & 
                  bus(x(FTEMP,1,indx_soil)), BUS(x(FVAP,1,indx_soil)),   &   
                  bus(x(qsurf,1,indx_soil)), bus(x(frv ,1,indx_soil)),   &   
                  bus(x(ALFAT      ,1,1)) , bus(x(ALFAQ      ,1,1)) ,    &  
                  bus(x(ilmo  ,1,indx_soil)), bus(x(hst  ,1,indx_soil)), &   
                  TRAD, N )
!
!

      CALL HYDRO_SVS ( DT,      & 
           bus(x(eg      ,1,1)), bus(x(er      ,1,1)),&
           bus(x(etr     ,1,1)), rainrate_mm         ,&
           bus(x(rsnowsa ,1,1)), bus(x(rsnowsv ,1,1)),&
           bus(x(impervu ,1,1)), bus(x(vegl    ,1,1)),&
           bus(x(vegh    ,1,1)), bus(x(psngrvl ,1,1)),&
           bus(x(psnvha  ,1,1)), bus(x(acroot  ,1,1)),&
           wrmax,                bus(x(wsat    ,1,1)),&
           bus(x(ksat    ,1,1)), bus(x(psisat  ,1,1)),&
           bus(x(bcoef   ,1,1)), bus(x(fbcof   ,1,1)),&
           bus(x(wfcint  ,1,1)), bus(x(grkef   ,1,1)),&
           bus(x(snoma   ,1,1)), bus(x(snvma   ,1,1)),&
           bus(x(wveg    ,1,1)), wvegt               ,&
           bus(x(wsoil   ,1,1)), wsoilt              ,&
           bus(x(isoil   ,1,1)), isoilt              ,&
           bus(x(ksatc   ,1,1)), bus(x(khc     ,1,1)),&
           bus(x(psi     ,1,1)), bus(x(grksat  ,1,1)),&
           bus(x(wfcdp   ,1,1)), bus(x(watflow ,1,1)),&
           bus(x(latflw  ,1,1)), &
           bus(x(runofftot ,1,indx_soil)), N)



      IF( USE_PHOTO ) THEN


         CALL PHTSYN_SVS ( BUS(x(LAIVF26,1,1))  , BUS(x(VEGF   ,1,1)), &
                        BUS(x(TVEGE  ,1,1))  , BUS(x(PMOINS ,1,1)), &
                        BUS(x(RESAVG ,1,1))  , BUS(x(HUMOINS,1,NK)), &
                        zfsolis              , BUS(x(WSOIL ,1,1)), &
                        BUS(x(FROOTD ,1,1))  , SUNCOSA            , &
                        BUS(x(WFC    ,1,1))  , BUS(x(WWILT  ,1,1)), &
                        MASKLAT50            , BUS(x(VGCTEM ,1,1))  , &
                        BUS(x(LAICTEM,1,1))  ,                      &
                        BUS(x(RCCTEM ,1,1))  , BUS(x(CO2I1  ,1,1)), &
                        BUS(x(AVG_GWSOL,1,1)), &
                        NCLASS, N)

      ENDIF
!
      CALL UPDATE_SVS ( WSOILT, ISOILT, WVEGT, &
                       bus(x(latflw  ,1,1)), bus(x(watflow ,1,1)),  &
                       bus(x(WSOIL   ,1,1)), bus(x(ISOIL   ,1,1)),  &
                       bus(x(WVEG    ,1,1)), bus(x(WSOILM  , 1,1)), &
                       bus(x(latflaf ,1,1)), bus(x(drainaf ,1,1)),  &
                       N )
!
  
 !# Compute values at the diagnostic level
     
      i = sl_sfclayer(zthetaa,hum,vmod,vdir,zzusl,zztsl,ztsa,zqsurf, &
           z0m,z0h,zdlat,zfcor,hghtm_diag=zu,hghtt_diag=zt,             &
           t_diag=ztdiag,q_diag=zqdiag,u_diag=zudiag,v_diag=zvdiag,&
           tdiaglim=TDIAGLIM_FALSE) 


      if (i /= SL_OK) then
         print*, 'Aborting in svs() because of error returned by sl_sfclayer()'
         stop
      endif


     

!VDIR NODEP

      do i=1,n
!
!
        bus(x(tsurf  ,i,1        )) = bus(x(tsa  ,i,1        ))
        bus(x(tsrad  ,i,1        )) = TRAD(i)
!
!       CALCULATE LAND-ATMOSPHERE OUTCOMING WATER FLUX
        BUS(x(WFLUX,I,1)) = RHOA(I)*BUS(x(EFLUX,I,1))
	BUS(x(ACCEVAP,I,1)) = BUS(x(ACCEVAP,I,1)) + BUS(x(WFLUX,I,1)) * DT
!
!       CALCULATE MEAN SNOW DEPTH FOR ESTHETIC PURPOSE ONLY
!VV DEBUT MODIFICATION POUR MESH
!        zsnodp(i) = bus(x(VEGH,i,1)) * bus(x(SNVDP,i,1)) + (1. -  bus(x(VEGH,i,1))) * bus(x(SNODPL,i,1))
!VV FIN MODIFICATION POUR MESH
      end do
!
!Modif VV Phasage MESH
#ifndef RUNSVS
!     FILL THE ARRAYS TO BE AGGREGATED LATER IN S/R AGREGE
      CALL FILLAGG ( BUS, BUSSIZ, PTSURF, PTSURFSIZ, INDX_SOIL,  &  
                    SURFLEN )
!
#endif


      RETURN
      END
