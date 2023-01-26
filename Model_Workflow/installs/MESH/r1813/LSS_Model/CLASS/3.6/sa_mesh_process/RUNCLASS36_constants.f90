module RUNCLASS36_constants

    implicit none

    !> Constants.
    !* ICAN: Number of vegetation canopy types.
    !* ICP1: Number of vegetation canopy types including urban area.
    !* ICTEM: Number of CTEM vegetation categories (1: not using CTEM).
    integer, parameter :: ICAN = 4, ICP1 = ICAN + 1, ICTEM = 1

    !> Control flags.
    !* ALL: DESCRIPTIONS ARE WRITTEN WHERE RUN_OPTIONS.INI IS READ
!-    integer IDISP, IZREF, ISLFD, IPCP, IWF, IPAI, IHGT, IALC, &
!-        IALS, IALG, ITG, ITC, ITCG
    integer ICTEMMOD

    !> Flags for WATROF.
    !* VICEFLG: Vertical ice flag or limit.
    !* HICEFLG: Horizontal ice flag or limit.
    integer :: LZFFLG = 0, EXTFLG = 0, IWFICE = 3, ERRFLG = 1, IWFOFLW
    real :: VICEFLG = 3.0, PSI_LIMIT = 1.0, HICEFLG = 1.0

    !> Common parameters.
    integer K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11
    real X1, X2, X3, X4, G, GAS, X5, X6, CPRES, GASV, X7, CPI, X8, &
        CELZRO, X9, X10, X11, X12, X13, X14, X15, SIGMA, X16, DELTIM, &
        DELT, TFREZ, RGAS, RGASV, GRAV, SBC, VKC, CT, VMIN, TCW, TCICE, &
        TCSAND, TCCLAY, TCOM, TCDRYS, RHOSOL, RHOOM, HCPW, HCPICE, &
        HCPSOL, HCPOM, HCPSND, HCPCLY, SPHW, SPHICE, SPHVEG, SPHAIR, &
        RHOW, RHOICE, TCGLAC, CLHMLT, CLHVAP, PI, ZOLNG, ZOLNS, ZOLNI, &
        ZORATG, ALVSI, ALIRI, ALVSO, ALIRO, ALBRCK, DELTA, CGRAV, &
        CKARM, CPD, AS, ASX, CI, BS, BETA, FACTN, HMIN, ANGMAX
    real, dimension(ICAN) :: CANEXT, XLEAF, ZORAT
    real, dimension(3) :: THPORG, THRORG, THMORG, BORG, PSISORG, &
        GRKSORG
    real, dimension(18, 4, 2) :: GROWYR

    type CLASS_parameters
        real, dimension(:), allocatable :: FREZTH, SWELIM, SNDENLIM
    end type

    !> Common CLASS blocks defined and set in CLASSBD and CLASSD.
    common /PARAMS/ X1, X2, X3, X4, G, GAS, X5, X6, CPRES, &
        GASV, X7
    common /PARAM1/ CPI, X8, CELZRO, X9, X10, X11
    common /PARAM3/ X12, X13, X14, X15, SIGMA, X16
    common /TIMES/ DELTIM, K1, K2, K3, K4, K5, K6, K7, K8, K9, &
        K10, K11
    common /CLASS1/ DELT, TFREZ
    common /CLASS2/ RGAS, RGASV, GRAV, SBC, VKC, CT, VMIN
    common /CLASS3/ TCW, TCICE, TCSAND, TCCLAY, TCOM, TCDRYS, &
        RHOSOL, RHOOM
    common /CLASS4/ HCPW, HCPICE, HCPSOL, HCPOM, HCPSND, &
        HCPCLY, SPHW, SPHICE, SPHVEG, SPHAIR, RHOW, &
        RHOICE, TCGLAC, CLHMLT, CLHVAP
    common /CLASS5/ THPORG, THRORG, THMORG, BORG, PSISORG, &
        GRKSORG
    common /CLASS6/ PI, GROWYR, ZOLNG, ZOLNS, ZOLNI, ZORAT, &
        ZORATG
    common /CLASS7/ CANEXT, XLEAF
    common /CLASS8/ ALVSI, ALIRI, ALVSO, ALIRO, ALBRCK
    common /PHYCON/ DELTA, CGRAV, CKARM, CPD
    common /CLASSD2/ AS, ASX, CI, BS, BETA, FACTN, HMIN, ANGMAX

end module
