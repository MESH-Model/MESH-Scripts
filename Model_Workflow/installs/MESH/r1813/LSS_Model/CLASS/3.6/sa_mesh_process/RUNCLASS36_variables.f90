module RUNCLASS36_variables

    use RUNCLASS36_constants

    implicit none

    !> Constants for output (read from file).
    !* TITLE: PROJECT DESCRIPTOR (6 COLUMNS: 4 CHARACTER STRINGS)
    !* NAME: AUTHOR, RESEARCHER (6 COLUMNS: 4 CHARACTER STRINGS)
    !* PLACE: SITE LOCATION, BASIN (6 COLUMNS: 4 CHARACTER STRINGS)
    character(4) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, &
        TITLE6, NAME1, NAME2, NAME3, NAME4, NAME5, NAME6, &
        PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6

    integer JLAT

    integer IDISP, IZREF, ISLFD, IPCP, IPAI, IHGT, IALC, &
        IALS, IALG, ITG, ITC, ITCG

    !> CLASS diagnostic output starting and stopping dates.
    !* JOUT1: DAILY-AVERAGED OUTPUT START DAY (JULIAN FROM YEAR START)
    !* JOUT2: DAILY-AVERAGED OUTPUT STOP DAY (JULIAN FROM YEAR START)
    !* JAV1: DAILY-AVERAGED OUTPUT START YEAR
    !* JAV2: DAILY-AVERAGED OUTPUT STOP YEAR
    !* KOUT1: YEARLY-AVERAGED OUTPUT START DAY (JULIAN FROM YEAR START)
    !* KOUT2: YEARLY-AVERAGED OUTPUT STOP DAY (JULIAN FROM YEAR START)
    !* KAV1: YEARLY-AVERAGED OUTPUT START YEAR
    !* KAV2: YEARLY-AVERAGED OUTPUT STOP YEAR
!-    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2
!-    integer IHOUR, IMIN, IDAY, IYEAR

    !> CTEM-related fields (inactive in current version of CLASS).
!todo: isolate CTEM from CLASS code (if possible).
    real, dimension(:), allocatable :: &
        CO2CONC, COSZS, XDIFFUSC, CFLUXCG, CFLUXCS
    real, dimension(:, :), allocatable :: &
        AILCG, AILCGS, FCANC, FCANCS, CO2I1CG, CO2I1CS, CO2I2CG, CO2I2CS, &
        SLAI, FCANCMX, ANCSVEG, ANCGVEG, RMLCSVEG, RMLCGVEG, &
        AILC, PAIC, &
        FIELDSM, WILTSM
    real, dimension(:, :, :), allocatable :: &
        RMATCTEM, RMATC
    integer, dimension(:), allocatable :: NOL2PFTS
!    integer ICTEMMOD
    integer L2MAX

    !> WATROF variables.
!todo: isolate WATROF from CLASS code.
    !* DD (DDEN): Drainage density.
    !* MANN (WFSF): Manning's n.
    real, dimension(:), allocatable :: DDGAT, MANNGAT

    !> FROZENSOILINIFLAG variables.
!todo: isolate from CLASS code if possible.
    integer NMELT, NYEARS
    real SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
    integer, dimension(:), allocatable :: INFILTYPE
    real, dimension(:), allocatable :: SI, TSI, SNOWMELTD, SNOWMELTD_LAST, &
        SNOWINFIL, CUMSNOWINFILCS, MELTRUNOFF, CUMSNOWINFILGS, t0_ACC

    !> PDMROF.
!todo: isolate PDMROF from CLASS code
    real, dimension(:), allocatable :: &
        CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
        UM1CS, UM1C, UM1G, UM1GS, &
        QM1CS, QM1C, QM1G, QM1GS, &
        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
        FSTRCS, FSTRC, FSTRG, FSTRGS

    type CLASS_forcing_input
        real, dimension(:), allocatable :: &
            FDL, FSIH, FSVH, PRE, PRES, QA, TA, UL, VL, VMOD
!            FCLO, ZBLD, ZRFH, ZRFM
    end type

    type CLASS_prognostic_variables
        real, dimension(:), allocatable :: &
            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
            TCAN, TPND, TSNO, WSNO, ZPND
        real, dimension(:, :), allocatable :: &
            TBAR, THIC, THLQ, TSFS
    end type

    type CLASS_surface_variables

!WATROF GRKF
!WATROF WFCI
!WATROF WFSF
!WATROF XSLP

        !> Dimension: NML
        real, dimension(:), allocatable :: &
            AGID, AGVD, ALGD, ALGW, ASID, ASVD, DRN, FARE, GRKF, &
            SDEP, WFCI, WFSF, XSLP, ZPLG, ZPLS, ZSNL
        integer, dimension(:), allocatable :: MID, IWF

        !> Dimension: NML, IGND
        integer, dimension(:), allocatable :: IGDR
        integer, dimension(:, :), allocatable :: &
            IORG, ISND
        real, dimension(:, :), allocatable :: &
            BI, CLAY, DELZW, GRKS, HCPS, ORGM, PSIS, PSIW, SAND, TCS, &
            THFC, THM, THP, THR, THRA, ZBTW

        !> Dimension: NML, ICAN
        real, dimension(:, :), allocatable :: &
            ACID, ACVD, CMAS, HGTD, PAID, PAMN, PAMX, PSGA, PSGB, &
            QA50, ROOT, RSMN, VPDA, VPDB

        !> Dimension: NML, ICP1
        real, dimension(:, :), allocatable :: &
            ALIC, ALVC, FCAN, LNZ0
    end type

    type CLASS_glacier_variables
        real, dimension(:), allocatable :: &
            FREZTH, SWELIM, SNDENLIM
    end type

    type CLASS_atmospheric_variables
        real, dimension(:), allocatable :: &
            CSZ, DLON, FCLO, GC, GGEO, PADR, RADJ, RHOA, RHSI, RPCP, &
            RPRE, SPCP, SPRE, TADP, TRPC, TSPC, VPD, Z0OR, ZBLD, ZDH, &
            ZDM, ZRFH, ZRFM
!            FDL, FSIH, FSVH, PRE, PRES, QA, TA, UL, UV, VL
    end type

    type CLASS_diagnostic_variables
        integer ISUM
        integer, dimension(:, :, :), allocatable :: ITCT
        real, dimension(:), allocatable :: &
            ALIR, ALVS, CDH, CDM, DR, EF, FCS, FGS, FC, FG, FLGG, &
            FLGS, FLGV, FSGG, FSGS, FSGV, FSNO, GA, GTE, HBL, HEVC, &
            HEVG, HEVS, HFS, HFSC, HFSG, HFSS, HMFC, HMFN, HTCC, HTCS, &
            ILMO, PCFC, PCLC, PCPG, PCPN, PET, QEVP, QFCF, QFCL, QFG, &
            QFN, QFS, QFX, QG, ROF, ROFB, ROFC, ROFN, ROFO, &
            ROVG, SFCQ, SFCT, SFCU, SFCV, TFX, TROB, TROF, TROO, &
            TSF, UE, WTAB, WTRC, WTRG, WTRS, ICE, TICE
        real, dimension(:, :), allocatable :: &
            ROFS, TROS, GFLX, HMFG, HTC, QFC
    end type

    type CLASS_averaged_variables
        real, dimension(:), allocatable :: &
            ALIR, ALVS, EVAP, FSIN, FLIN, FLUT, GRO, BBGT, HFS, HMFN, &
            OVR, PRE, PRES, QA, QEVP, RCAN, RHOS, ROF, SNCAN, SNO, TA, &
            TBAR, THAL, THIC, THLQ, TCAN, TSNO, UV, WSNO, WTBL
    end type

!todo: Move this?
    type HydrologyParameters
        real, dimension(:, :), allocatable :: &
            FRZCROW, &
            CMAXROW, CMINROW, BROW, K1ROW, K2ROW
    end type

    type RUNCLASS36_parameters
        type(CLASS_parameters) constant, grid, tile, gru
    end type

    !> Type: RUNCLASS36_Flags
    !* PROCESS_ACTIVE: Flag to enable CLASS.
    !* INTERFLOWFLAG: IWF flag (global).
    type RUNCLASS36_flags
        logical :: PROCESS_ACTIVE = .true.
        integer :: INTERFLOWFLAG = 1
        type(RUNCLASS36_parameters) pm
    end type

!todo: may need to move these.
    type(HydrologyParameters), save :: hp
    type(RUNCLASS36_flags), save :: RUNCLASS36_flgs

!todo: may need to move these.
    type(CLASS_forcing_input), save :: cfi
    type(CLASS_prognostic_variables), save :: cpv
    type(CLASS_surface_variables), save :: csfv
    type(CLASS_glacier_variables), save :: cglv
    type(CLASS_atmospheric_variables), save :: catv
    type(CLASS_diagnostic_variables), save :: cdv

end module
