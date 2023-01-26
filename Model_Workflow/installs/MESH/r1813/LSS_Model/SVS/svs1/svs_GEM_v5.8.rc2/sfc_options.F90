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

module sfc_options
   implicit none 
   public
   save
   
   !#
   integer, parameter :: CLASS_URB = 21 !# Class "urban"
   integer, parameter :: NCLASS    = 26 !# NUMBER OF CLASSES FOR NATURAL COVERS
   integer, parameter :: NCLASSURB = 12 !# NUMBER OF URBAN CLASSES
   integer, parameter :: NL = 3 !# nombre de niveaux dans la glace marine


   !# ----  FOR SVS LAND SCHEME ------
   integer, parameter :: MAX_NL_SVS = 50 !# maximum number of soil layers specified by user
   integer, parameter :: NL_SVS_DEFAULT = 7 ! default number of soil layers
   ! default depth of default soil layers in [METERS]
   real, dimension(NL_SVS_DEFAULT):: DP_SVS_DEFAULT =  (/ 0.05, 0.1, 0.2, 0.4, 1.0, 2.0, 3.0 /)
   !#----------------------------------

   real, parameter :: CRITEXTURE = 0.1
   real, parameter :: CRITLAC    = 0.01
   real, parameter :: CRITMASK   = 0.001
   real, parameter :: CRITSNOW   = 0.0001
   real, parameter :: CRITWATER  = 0.001
   real, parameter :: HIMIN      = 0.001
   real, parameter :: MINICEDP   = 0.05 
   real, parameter :: N0RIB = 1.0E-5
   real, parameter :: SNOH0 = 0.1
   real, parameter :: VAMIN = 1.0e-4
   real, parameter :: ZU = 10.
   real, parameter :: ZT = 1.5

   !# sfclayer_mod vars
   real              :: as = 12.
   real              :: ci = 40.

   !# 
   logical           :: climat     = .false.
   logical           :: cplocn     = .false.
   integer           :: date(14)   = 0
   real              :: delt       = 0.
   logical           :: do_surface = .false.
   character(len=16) :: fluvert    = 'NIL'
   character(len=16) :: radia      = 'NIL'
   logical           :: radslope   = .false.

   !# Adjust surface temperature over snow after reading (coherency check)
   logical           :: adj_i0_snow = .true.
   namelist /surface_cfgs/ adj_i0_snow

   !# Parameter for stability function
   real              :: beta        = 0.
   namelist /surface_cfgs/ beta

   !# Diurnal SST scheme
   !# * 'NIL    ' :
   !# * 'FAIRALL' :
   character(len=16) :: diusst      = 'NIL'
   namelist /surface_cfgs/ diusst
   character(len=*), parameter :: DIUSST_OPT(2) = (/ &
        'NIL    ', &
        'FAIRALL'  &
        /)

   !# Diurnal SST scheme active coolskin if .true.
   logical           :: diusst_coolskin = .true.
   namelist /surface_cfgs/ diusst_coolskin

   !# Diurnal SST scheme active warmlayer if .true.
   logical           :: diusst_warmlayer = .true.
   namelist /surface_cfgs/ diusst_warmlayer

   !# Depth of soil layers in [METERS] in SVS land surface scheme (schmsol=SVS)
   real :: dp_svs(MAX_NL_SVS) = -1.0
   namelist /surface_cfgs/ dp_svs

   !# Uses dry adiabat if .true.
   logical           :: drylaps     = .true.
   namelist /surface_cfgs/ drylaps

   !# Set water temperature of ice-covered lakes to 0C for points north of 
   !# ice line if .true.
   !# needs an initialization file otherwise the model stops
   logical           :: icelac      = .false.
   namelist /surface_cfgs/ icelac

   !# Sea ice melting
   logical           :: icemelt     = .false.
   namelist /surface_cfgs/ icemelt

   !# Implicit surface fluxes if .true.; explicit fluxes if .false.
   logical           :: impflx      = .false.
   namelist /surface_cfgs/ impflx

   !# If .true. make sure there is soil water where MG > critmask (0.1%);
   !# WSoil = 0.3 if < critwater (0.001)
   logical           :: isba_i1_minval = .true.
   namelist /surface_cfgs/ isba_i1_minval

   !# If .true. apply temporary fix to ISBA 
   !# * timestep dependent KCOEF
   !# * No PSN factor for meting and freezing
   logical           :: isba_melting_fix = .false.
   namelist /surface_cfgs/ isba_melting_fix

   !# If .true., do not consider "latent heat realease due to liquid water 
   !# refreezing in the snowpack" in the surface energy budget WHEN the 
   !# superficial surface temperature is above zero.
   logical           :: isba_no_warm_sn_freez = .false.
   namelist /surface_cfgs/ isba_no_warm_sn_freez

   !# Last/Deepest soil layer considered during the accumulation of 
   !# lateral flow and drainage. Drainage is taken as the vertical flux
   !# leaving layer KHYD, and lateral flow as the sum of lateral flows from 
   !# layers 1 to KHYD
!VV DEBUT MODIFICATION POUR MESH
!   integer           :: khyd    = -1
!   namelist /surface_cfgs/ khyd
!VV FIN MODIFICATION POUR MESH

   !# Minimum fraction of leads in sea ice.&nbsp; Multiply ice fraction by (1.-leadfrac) 
   real              :: leadfrac    = 0.03
   namelist /surface_cfgs/ leadfrac

   !# Limit snow depth to 10 cm for calculation of heat conductivity of snow 
   !# over sea-ice and glacier if .true.  
   logical           :: limsnodp    = .false.
   namelist /surface_cfgs/ limsnodp

   !# (coupling) fluxes over ocean are taken from ocean model if .true.
   logical           :: owflux      = .false.
   namelist /surface_cfgs/ owflux

   !# read-in land surface emissivity if .true.
   logical           :: read_emis     = .false.
   namelist /surface_cfgs/ read_emis

   !# Takes into account effect of ocean salinity on saturation specific
   !# humidity at ocean surface (boundary condition for LH flux calculation)
   logical           :: salty_qsat  = .false.
   namelist /surface_cfgs/ salty_qsat

   !# Land surface processes
   !# * 'NIL ' :
   !# * 'ISBA' :
   character(len=16) :: schmsol     = 'ISBA'
   namelist /surface_cfgs/ schmsol
   character(len=*), parameter :: SCHMSOL_OPT(3) = (/ &
        'NIL ', &
        'ISBA', &
        'SVS '  &
        /)

   !# Urban surface processes
   !# * 'NIL' :
   !# * 'TEB' :
   character(len=16) :: schmurb     = 'NIL'
   namelist /surface_cfgs/ schmurb
   character(len=*), parameter :: SCHMURB_OPT(2) = (/ &
        'NIL', &
        'TEB'  &
        /)

   !#  Soil texture database/calculations for SVS land surface scheme
   !# * 'CLASSIC' : 3 layers of sand & clay info
   !# * 'GSDE' : 8 layers of sand & clay info
   character(len=16) :: soiltext    = 'GSDE'
   namelist /surface_cfgs/ soiltext
   character(len=*), parameter :: SOILTEXT_OPT(4) = (/ &
        'GSDE     ',  &
        'SLC      ',  &
        'SOILGRIDS',  &
        'NIL      ' &
        /)

   !# Use snow albedo "I6" directly if .true.;
   !# Use snow age "XA" to calculate snow albedo if .false.
   logical           :: snoalb_anl  = .true.
   namelist /surface_cfgs/ snoalb_anl

   !# Limit temperature inversions to 8K/40m in surface layer if .true.  
   logical           :: tdiaglim    = .false.
   namelist /surface_cfgs/ tdiaglim


   !# OPTION FOR CALCULATION of AVERAGE LAND SURFACE TEMPERATURE AND HUMIDITY IN SVS
   !# .FALSE. :  Area-average only calculation for sfc T and Hum.
   !# .TRUE.  :  Option that uses effective surface temperature  and specific humidity instead
   !             of composite (area-averaged only) counterparts in surface flux calculations (D. Deacu)
   logical           :: use_eff_surf_tq    = .false.
   namelist /surface_cfgs/ use_eff_surf_tq

   !# OPTION TO USE PHOTOSYNTHESIS CODE FOR STOMATAL RESISTANCE in SVS
   logical           :: use_photo = .true.
   namelist /surface_cfgs/ use_photo
 
   !# Factor multiplying stomatal resistance in ISBA
   real              :: veg_rs_mult = 1.
   namelist /surface_cfgs/ veg_rs_mult

   !# Use directional roughness length if .true.
   logical           :: z0dir       = .false.
   namelist /surface_cfgs/ z0dir

   !# Constant value of thermal roughness length (m) applied over water within 
   !# latitudinal band defined by z0tlat 
   real              :: z0hcon      = 4.0e-5
   namelist /surface_cfgs/ z0hcon

   !# Minimum value of roughness length (m) over water 
   real              :: z0min       = 1.5e-5
   namelist /surface_cfgs/ z0min

   !# Momentum roughness length formulation over water
   !# * 'CHARNOCK' :
   !# * 'BELJAARS' :
   character(len=16) :: z0mtype     = 'CHARNOCK'
   namelist /surface_cfgs/ z0mtype
   character(len=*), parameter :: Z0MTYPE_OPT(2) = (/ &
        'CHARNOCK', &
        'BELJAARS' &
        /)

   !# Latitude (2 elements, in degrees) used to specify Z0T over water
   !# * If |lat| <= Z0TLAT(1) constant Z0T. 
   !# * If |lat| >= Z0TLAT(2) Charnock's relation.
   !# * In between, linear interpolation is used.
   real              :: z0tlat(2)   = 0.
   namelist /surface_cfgs/ z0tlat

   !# Thermal roughness length formulation over water described by eq. 3 of 
   !# Deacu et al. (2012)
   logical           :: z0trdps300  = .false.
   namelist /surface_cfgs/ z0trdps300

   !# Height (m) of T and Q input for sfc fluxes calc.
   real              :: zta         = -1.
   namelist /surface_cfgs/ zta

   !# Height (m) of wind input for sfc fluxes calc.
   real              :: zua         = -1.
   namelist /surface_cfgs/ zua


   !# Compute thermal stress indicators (comfort) if .true.
   logical           :: thermal_stress = .false.
   namelist /surface_cfgs/ thermal_stress

   !# Adjust wind diagnostic in TEB with building height  if .true.
   logical           :: urb_diagwind = .false.
   namelist /surface_cfgs/ urb_diagwind

   !# Adjust temperature diagnostic in TEB in the street  if .true.
   logical           :: urb_diagtemp = .false.
   namelist /surface_cfgs/ urb_diagtemp

   !# Fix computation of tan(zenith) for sun angle in TEB
   logical           :: urb_tanzen_fix = .false.
   namelist /surface_cfgs/ urb_tanzen_fix

contains

!VV DEBUT MODIFICATION POUR MESH
!   function sfc_options_init() result(F_istat)
!      use sfclayer_mod, only: sl_get, SL_OK
!      implicit none 
!      integer :: F_istat
!#include <msg.h>
!#include <rmnlib_basics.hf>
!      logical, save :: init_L = .false.
!      F_istat = RMN_OK
!      if (init_L) return
!      init_L = .true.
!      F_istat = sl_get('as',as)
!      F_istat = min(sl_get('beta',beta),F_istat)
!      F_istat = min(sl_get('ci',ci),F_istat)
!      if (.not.RMN_IS_OK(F_istat)) &
!           call msg(MSG_ERROR,'(sfc_options_init) cannot retrieve AS, BETA or CI')
!      return
!   end function sfc_options_init
!VV FIN MODIFICATION POUR MESH

end module sfc_options
