module RUNCLASS36_config

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    implicit none

    !>  CONSTANTS AND TEMPORARY VARIABLES.
    real FSDOWN1, FSDOWN2, FSDOWN3, RDAY, &
        DECL, HOUR, COSZ

    integer NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI
    real, dimension(:), allocatable :: &
        FRZCGAT

    !> CANOPY AND SOIL INFORMATION (CLASS):
    !> THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
    !> OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
    !> CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).
    !* ALL: DEFINITIONS IN CLASS DOCUMENTATION (CLASS.INI)
    real, dimension(:), allocatable :: XDGAT, &
        KSGAT

    !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
    real, dimension(:), allocatable :: ZDMGRD, &
        ZDHGRD, RADJGRD, CSZGRD, &
        PADRGRD, VPDGRD, &
        TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, RHSIGRD, &
        FCLOGRD, DLONGRD, Z0ORGRD, GGEOGRD, UVGRD, XDIFFUS, &
        RPREGRD, SPREGRD, VMODGRD

    !> LAND SURFACE DIAGNOSTIC VARIABLES:
    real, dimension(:), allocatable :: &
        SFRHGAT, &
        QLWOGAT, FTEMP, &
        FVAP, RIB
    real, dimension(:), allocatable :: CDHGRD, CDMGRD, HFSGRD, &
        TFXGRD, QEVPGRD, QFSGRD, QFXGRD, PETGRD, GAGRD, EFGRD, GTGRD, &
        QGGRD, TSFGRD, ALVSGRD, ALIRGRD, FSNOGRD, SFCTGRD, SFCUGRD, &
        SFCVGRD, SFCQGRD, FSGVGRD, FSGSGRD, FSGGGRD, FLGVGRD, FLGSGRD, &
        FLGGGRD, HFSCGRD, HFSSGRD, HFSGGRD, HEVCGRD, HEVSGRD, HEVGGRD, &
        HMFCGRD, HMFNGRD, HTCCGRD, HTCSGRD, PCFCGRD, PCLCGRD, PCPNGRD, &
        PCPGGRD, QFGGRD, QFNGRD, QFCLGRD, QFCFGRD, ROFGRD, ROFOGRD, &
        ROFSGRD, ROFBGRD, ROFCGRD, ROFNGRD, ROVGGRD, WTRCGRD, WTRSGRD, &
        WTRGGRD, DRGRD, WTABGRD, ILMOGRD, UEGRD, HBLGRD

    real, dimension(:, :), allocatable :: HMFGGRD, HTCGRD, QFCGRD, GFLXGRD

    !> CROSS-CLASS VARIABLES (CLASS):
    !> ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
    !> SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").
    real, dimension(:), allocatable :: RBCOEF, &
        ZSNOW, FSVF, FSVFS, ALVSCN, ALIRCN, ALVSG, &
        ALIRG, ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, &
        ALIRSC, TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC, RCS, FRAINC, &
        FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP, DISPS, ZOMLNC, &
        ZOELNC, ZOMLNG, &
        ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, TRSNOW, CHCAP, CHCAPS, &
        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, G23C, &
        G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, EVAPC, &
        EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, RAICAN, &
        SNOCAN, RAICNS, SNOCNS, CWLCAP, CWFCAP, CWLCPS, CWFCPS, TSNOCS, &
        TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, TPONDC, TPONDG, TPNDCS, &
        TPNDGS, ZPLMCS, ZPLMGS, ZPLIMC, ZPLIMG
    real, dimension(:, :), allocatable :: TBARC, TBARG, TBARCS, &
        TBARGS, THLIQC, THLIQG, THICEC, THICEG, FROOT, HCPC, HCPG, &
        TCTOPC, TCBOTC, TCTOPG, TCBOTG

    !> BALANCE ERRORS (CLASS):
    !> DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER
    !> BALANCES.
    real, dimension(:), allocatable :: CTVSTP, CTSSTP, CT1STP, &
        CT2STP, CT3STP, WTVSTP, WTSSTP, WTGSTP

    contains

    subroutine RUNCLASS36_init(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing
        use FLAGS

        !> For CLASS output.
        use RUNCLASS36_save_output

        use PBSM_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer NA, NTYPE, NML, NSL, l, k, ik, jk, m, j, i, iun, ierr
        real FRAC
        character(len = DEFAULT_LINE_LENGTH) line

        !> Return if the process is not active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) then
            return
        else
            call print_new_section("RUNCLASS36 is active.")
            call increase_tab()
        end if

        !> Check for required variables.
        ierr = 0
        if (.not. associated(vs%tile%fsin)) then
            call print_error("The driving variable '" // VN_FSIN // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (.not. associated(vs%tile%flin)) then
            call print_error("The driving variable '" // VN_FLIN // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (.not. associated(vs%tile%ta)) then
            call print_error("The driving variable '" // VN_TA // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (.not. associated(vs%tile%qa)) then
            call print_error("The driving variable '" // VN_QA // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (.not. associated(vs%tile%pres)) then
            call print_error("The driving variable '" // VN_PRES // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (.not. associated(vs%tile%uv)) then
            call print_error("The driving variable '" // VN_UV // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (IPCP == 4) then
            if (.not. associated(vs%tile%prern) .or. .not. associated(vs%tile%presno)) then
                call print_error( &
                    "'IPCP 4' is active but the driving variables '" // VN_PRERN // "' and '" // VN_PRESNO // &
                    "' are not active or not associated with an input file.")
                ierr = 1
            else if (associated(vs%tile%pre)) then
                call print_info( &
                    "'IPCP 4' is active with the '" // VN_PRERN // "' and '" // VN_PRESNO // "' variables. The '" // VN_PRE // &
                    "' variable is also active but inputs on the field are not being used.")
            end if
        else if (.not. associated(vs%tile%pre)) then
            call print_error("The driving variable '" // VN_PRE // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (ierr /= 0) then
            call reset_tab()
            call print_error( &
                "The variables required to drive the model are not active or have not been associated with an input file.")
            call program_abort()
        end if

        !> Local variables.
        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND
        NML = shd%lc%NML
        DELT = ic%dts

        !> ALLOCATE ALL VARIABLES

1114 format(/1x, 'Error allocating ', a, ' variables.', &
            /1x, 'Check that these bounds are within an acceptable range.', /)
1118 format(3x, a, ': ', i6)

        allocate(XDGAT(NML), KSGAT(NML), stat = ierr)

        if (ierr /= 0) then
            print 1114, 'canopy and soil info.'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Canopy types with urban areas', ICP1
            print 1118, 'Canopy types', ICAN
            print 1118, 'Soil layers', NSL
            stop
        end if

        !> WATROF FLAGS AND VARIABLES:
        allocate(DDGAT(NML), MANNGAT(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'WATROF'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

        !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
        allocate(ZDMGRD(NA), &
                 ZDHGRD(NA), RADJGRD(NA), &
                 CSZGRD(NA), &
                 PADRGRD(NA), VPDGRD(NA), &
                 TADPGRD(NA), RHOAGRD(NA), RPCPGRD(NA), TRPCGRD(NA), &
                 SPCPGRD(NA), TSPCGRD(NA), RHSIGRD(NA), &
                 FCLOGRD(NA), DLONGRD(NA), Z0ORGRD(NA), GGEOGRD(NA), UVGRD(NA), &
                 XDIFFUS(NA), &
                 RPREGRD(NA), SPREGRD(NA), VMODGRD(NA), &
                 stat = ierr)
        if (ierr /= 0) then
            print 1114, 'atmospheric and grid-cst.'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

        !> LAND SURFACE DIAGNOSTIC VARIABLES:
        allocate(SFRHGAT(NML), &
                 QLWOGAT(NML), &
                 FTEMP(NML), FVAP(NML), RIB(NML), &
                 CDHGRD(NA), CDMGRD(NA), HFSGRD(NA), &
                 TFXGRD(NA), QEVPGRD(NA), QFSGRD(NA), QFXGRD(NA), PETGRD(NA), &
                 GAGRD(NA), EFGRD(NA), GTGRD(NA), &
                 QGGRD(NA), TSFGRD(NA), ALVSGRD(NA), ALIRGRD(NA), FSNOGRD(NA), &
                 SFCTGRD(NA), SFCUGRD(NA), &
                 SFCVGRD(NA), SFCQGRD(NA), FSGVGRD(NA), FSGSGRD(NA), &
                 FSGGGRD(NA), FLGVGRD(NA), FLGSGRD(NA), &
                 FLGGGRD(NA), HFSCGRD(NA), HFSSGRD(NA), HFSGGRD(NA), &
                 HEVCGRD(NA), HEVSGRD(NA), HEVGGRD(NA), &
                 HMFCGRD(NA), HMFNGRD(NA), HTCCGRD(NA), HTCSGRD(NA), &
                 PCFCGRD(NA), PCLCGRD(NA), PCPNGRD(NA), &
                 PCPGGRD(NA), QFGGRD(NA), QFNGRD(NA), QFCLGRD(NA), QFCFGRD(NA), &
                 ROFGRD(NA), ROFOGRD(NA), &
                 ROFSGRD(NA), ROFBGRD(NA), ROFCGRD(NA), ROFNGRD(NA), &
                 ROVGGRD(NA), WTRCGRD(NA), WTRSGRD(NA), &
                 WTRGGRD(NA), DRGRD(NA), WTABGRD(NA), ILMOGRD(NA), UEGRD(NA), &
                 HBLGRD(NA), &
                 HMFGGRD(NA, NSL), HTCGRD(NA, NSL), QFCGRD(NA, NSL), &
                 GFLXGRD(NA, NSL), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'land surface diagnostic'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Soil layers', NSL
            stop
        end if

        !> CROSS-CLASS VARIABLES (CLASS):
        allocate(TBARC(NML, NSL), TBARG(NML, NSL), &
                 TBARCS(NML, NSL), &
                 TBARGS(NML, NSL), THLIQC(NML, NSL), &
                 THLIQG(NML, NSL), THICEC(NML, NSL), &
                 THICEG(NML, NSL), FROOT(NML, NSL), &
                 HCPC(NML, NSL), HCPG(NML, NSL), &
                 TCTOPC(NML, NSL), TCBOTC(NML, NSL), &
                 TCTOPG(NML, NSL), TCBOTG(NML, NSL), &
                 RBCOEF(NML), &
                 ZSNOW(NML), &
                 FSVF(NML), FSVFS(NML), ALVSCN(NML), &
                 ALIRCN(NML), ALVSG(NML), &
                 ALIRG(NML), ALVSCS(NML), ALIRCS(NML), &
                 ALVSSN(NML), ALIRSN(NML), ALVSGC(NML), &
                 ALIRGC(NML), ALVSSC(NML), &
                 ALIRSC(NML), TRVSCN(NML), TRIRCN(NML), &
                 TRVSCS(NML), TRIRCS(NML), RC(NML), &
                 RCS(NML), FRAINC(NML), &
                 FSNOWC(NML),FRAICS(NML),FSNOCS(NML), &
                 CMASSC(NML), CMASCS(NML), &
                 DISP(NML), DISPS(NML), ZOMLNC(NML), &
                 ZOELNC(NML), ZOMLNG(NML), &
                 ZOELNG(NML), ZOMLCS(NML), ZOELCS(NML), &
                 ZOMLNS(NML), ZOELNS(NML), TRSNOW(NML), &
                 CHCAP(NML), CHCAPS(NML), &
                 GZEROC(NML), GZEROG(NML), GZROCS(NML), &
                 GZROGS(NML), G12C(NML), G12G(NML), &
                 G12CS(NML), G12GS(NML), G23C(NML), &
                 G23G(NML), G23CS(NML), G23GS(NML), &
                 QFREZC(NML), QFREZG(NML), QMELTC(NML), &
                 QMELTG(NML), EVAPC(NML), &
                 EVAPCG(NML), EVAPG(NML), EVAPCS(NML), &
                 EVPCSG(NML), EVAPGS(NML), TCANO(NML), &
                 TCANS(NML), RAICAN(NML), &
                 SNOCAN(NML), RAICNS(NML), SNOCNS(NML), &
                 CWLCAP(NML), CWFCAP(NML), CWLCPS(NML), &
                 CWFCPS(NML), TSNOCS(NML), &
                 TSNOGS(NML), RHOSCS(NML), RHOSGS(NML), &
                 WSNOCS(NML), WSNOGS(NML), TPONDC(NML), &
                 TPONDG(NML), TPNDCS(NML), &
                 TPNDGS(NML), ZPLMCS(NML), ZPLMGS(NML), &
                 ZPLIMC(NML), ZPLIMG(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'cross-CLASS'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Soil layers', NSL
            stop
        end if

        !> BALANCE ERRORS (CLASS):
        allocate(CTVSTP(NML), CTSSTP(NML), &
                 CT1STP(NML), &
                 CT2STP(NML), CT3STP(NML), WTVSTP(NML), &
                 WTSSTP(NML), WTGSTP(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'balance error diagnostic'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

        !> CTEM ERRORS (CLASS):
        allocate(CO2CONC(NML), COSZS(NML), XDIFFUSC(NML), CFLUXCG(NML), CFLUXCS(NML), &
                 AILCG(NML, ICTEM), AILCGS(NML, ICTEM), FCANC(NML, ICTEM), FCANCS(NML, ICTEM), &
                 CO2I1CG(NML, ICTEM), CO2I1CS(NML, ICTEM), CO2I2CG(NML, ICTEM), CO2I2CS(NML, ICTEM), &
                 SLAI(NML, ICTEM), FCANCMX(NML, ICTEM), ANCSVEG(NML, ICTEM), ANCGVEG(NML, ICTEM), &
                 RMLCSVEG(NML, ICTEM), RMLCGVEG(NML, ICTEM), &
                 AILC(NML, ICAN), PAIC(NML, ICAN), FIELDSM(NML, NSL), WILTSM(NML, NSL), &
                 RMATCTEM(NML, ICTEM, NSL), RMATC(NML, ICAN, NSL), NOL2PFTS(ICAN), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'CTEM'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Canopy types', ICAN
            print 1118, 'Soil layers', NSL
            print 1118, 'CTEM flag', ICTEM
            stop
        end if

        !> Forcing input.
        allocate(cfi%FDL(NML), cfi%FSIH(NML), cfi%FSVH(NML), cfi%PRE(NML), cfi%PRES(NML), cfi%QA(NML), cfi%TA(NML), cfi%UL(NML), &
                 cfi%VL(NML), cfi%VMOD(NML))

        !> Prognostic variables.
        allocate(cpv%ALBS(NML), cpv%CMAI(NML), cpv%GRO(NML), cpv%QAC(NML), cpv%RCAN(NML), cpv%RHOS(NML), cpv%SNCAN(NML), &
                 cpv%SNO(NML), cpv%TAC(NML), cpv%TBAS(NML), cpv%TCAN(NML), cpv%TPND(NML), cpv%TSNO(NML), cpv%WSNO(NML), &
                 cpv%ZPND(NML))
        allocate(cpv%TBAR(NML, NSL), cpv%THIC(NML, NSL), cpv%THLQ(NML, NSL))
        allocate(cpv%TSFS(NML, 4))

        !> Land-surface variables.
        allocate(csfv%AGID(NML), csfv%AGVD(NML), csfv%ALGD(NML), csfv%ALGW(NML), csfv%ASID(NML), csfv%ASVD(NML), csfv%DRN(NML), &
                 csfv%FARE(NML), csfv%GRKF(NML), csfv%MID(NML), csfv%IWF(NML), csfv%SDEP(NML), csfv%WFCI(NML), csfv%WFSF(NML), &
                 csfv%XSLP(NML), csfv%ZPLG(NML), csfv%ZPLS(NML), csfv%ZSNL(NML))
        allocate(csfv%IGDR(NML))
        allocate(csfv%IORG(NML, NSL), csfv%ISND(NML, NSL))
        allocate(csfv%BI(NML, NSL), csfv%CLAY(NML, NSL), csfv%DELZW(NML, NSL), csfv%GRKS(NML, NSL), csfv%HCPS(NML, NSL), &
                 csfv%ORGM(NML, NSL), csfv%PSIS(NML, NSL), csfv%PSIW(NML, NSL), csfv%SAND(NML, NSL), csfv%TCS(NML, NSL), &
                 csfv%THFC(NML, NSL), csfv%THM(NML, NSL), csfv%THP(NML, NSL), csfv%THR(NML, NSL), csfv%THRA(NML, NSL), &
                 csfv%ZBTW(NML, NSL))
        allocate(csfv%ACID(NML, ICAN), csfv%ACVD(NML, ICAN), csfv%CMAS(NML, ICAN), csfv%HGTD(NML, ICAN), csfv%PAID(NML, ICAN), &
                 csfv%PAMN(NML, ICAN), csfv%PAMX(NML, ICAN), csfv%PSGA(NML, ICAN), csfv%PSGB(NML, ICAN), csfv%QA50(NML, ICAN), &
                 csfv%ROOT(NML, ICAN), csfv%RSMN(NML, ICAN), csfv%VPDA(NML, ICAN), csfv%VPDB(NML, ICAN))
        allocate(csfv%ALIC(NML, ICP1), csfv%ALVC(NML, ICP1), csfv%FCAN(NML, ICP1), csfv%LNZ0(NML, ICP1))

        !> Glacier variables.
        allocate(cglv%FREZTH(NML), cglv%SWELIM(NML), cglv%SNDENLIM(NML))

        !> Atmospheric variables.
        allocate(catv%CSZ(NML), catv%DLON(NML), catv%FCLO(NML), catv%GC(NML), catv%GGEO(NML), catv%PADR(NML), catv%RADJ(NML), &
                 catv%RHOA(NML), catv%RHSI(NML), catv%RPCP(NML), catv%RPRE(NML), catv%SPCP(NML), catv%SPRE(NML), catv%TADP(NML), &
                 catv%TRPC(NML), catv%TSPC(NML), catv%VPD(NML), catv%Z0OR(NML), catv%ZBLD(NML), catv%ZDH(NML), catv%ZDM(NML), &
                 catv%ZRFH(NML), catv%ZRFM(NML))

        !> Diagnostic variables.
        allocate(cdv%ITCT(NML, 6, 50))
        allocate(cdv%ALIR(NML), cdv%ALVS(NML), cdv%CDH(NML), cdv%CDM(NML), cdv%DR(NML), cdv%EF(NML), cdv%FCS(NML), cdv%FGS(NML), &
                 cdv%FC(NML), cdv%FG(NML), cdv%FLGG(NML), cdv%FLGS(NML), cdv%FLGV(NML), cdv%FSGG(NML), cdv%FSGS(NML), &
                 cdv%FSGV(NML), cdv%FSNO(NML), cdv%GA(NML), cdv%GTE(NML), cdv%HBL(NML), cdv%HEVC(NML), cdv%HEVG(NML), &
                 cdv%HEVS(NML), cdv%HFS(NML), cdv%HFSC(NML), cdv%HFSG(NML), cdv%HFSS(NML), cdv%HMFC(NML), cdv%HMFN(NML), &
                 cdv%HTCC(NML), cdv%HTCS(NML), cdv%ILMO(NML), cdv%PCFC(NML), cdv%PCLC(NML), cdv%PCPG(NML), cdv%PCPN(NML), &
                 cdv%PET(NML), cdv%QEVP(NML), cdv%QFCF(NML), cdv%QFCL(NML), cdv%QFG(NML), cdv%QFN(NML), cdv%QFS(NML), &
                 cdv%QFX(NML), cdv%QG(NML), cdv%ROF(NML), cdv%ROFB(NML), cdv%ROFC(NML), cdv%ROFN(NML), cdv%ROFO(NML), &
                 cdv%ROVG(NML), cdv%SFCQ(NML), cdv%SFCT(NML), cdv%SFCU(NML), cdv%SFCV(NML), cdv%TFX(NML), &
                 cdv%TROB(NML), cdv%TROF(NML), cdv%TROO(NML), cdv%TSF(NML), cdv%UE(NML), cdv%WTAB(NML), &
                 cdv%WTRC(NML), cdv%WTRG(NML), cdv%WTRS(NML), cdv%ICE(NML), cdv%TICE(NML))
        allocate(cdv%ROFS(NML, NSL), cdv%TROS(NML, NSL), &
                 cdv%GFLX(NML, NSL), cdv%HMFG(NML, NSL), cdv%HTC(NML, NSL), cdv%QFC(NML, NSL))

        !> Read an initial value for geothermal flux from file.
        if (GGEOFLAG == 1) then
            iun = fls%fl(mfk%f18)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f18)%fn)), status = 'old', action = 'read', iostat = ierr)
            read(iun, *) GGEOGRD(1)
            close(iun)
        else
            GGEOGRD(1) = 0.0
        end if

        !> Initialize PBSM or allocate and initialize variables used in CLASS even if PBSM is not enabled.
        if (pbsm%PROCESS_ACTIVE) then
            call PBSM_init(fls, shd, cm)
        else

            !> Variables used in CLASSZ.
            allocate(pbsm%vs%Drift(NML), pbsm%vs%Subl(NML))
            pbsm%vs%Drift = 0.0; pbsm%vs%Subl = 0.0

            !> Variables used in CLASSW.
            !> These are initialized in WPREP.
            allocate(ZSNOCS(NML), ZSNOGS(NML), ZSNOWC(NML), ZSNOWG(NML), &
                     HCPSCS(NML), HCPSGS(NML), HCPSC(NML), HCPSG(NML), &
                     TSNOWC(NML), TSNOWG(NML), &
                     RHOSC(NML), RHOSG(NML), &
                     XSNOWC(NML), XSNOWG(NML), XSNOCS(NML), XSNOGS(NML))
        end if

        !> Distribute variables.
        catv%ZRFM(il1:il2) = pm%tile%zrfm(il1:il2)
        catv%ZRFH(il1:il2) = pm%tile%zrfh(il1:il2)
        catv%ZBLD(il1:il2) = pm%tile%zbld(il1:il2)
        catv%GC(il1:il2) = pm%tile%gc(il1:il2)
        csfv%FARE(il1:il2) = pm%tile%fare(il1:il2)
        csfv%MID(il1:il2) = pm%tile%mid(il1:il2)
        csfv%IWF(il1:il2) = pm%tile%iwf(il1:il2)
        csfv%FCAN(il1:il2, :) = pm%tile%fcan(il1:il2, :)
        csfv%LNZ0(il1:il2, :) = pm%tile%lnz0(il1:il2, :)
        csfv%ALVC(il1:il2, :) = pm%tile%alvc(il1:il2, :)
        csfv%ALIC(il1:il2, :) = pm%tile%alic(il1:il2, :)
        csfv%PAMX(il1:il2, :) = pm%tile%lamx(il1:il2, :)
        csfv%PAMN(il1:il2, :) = pm%tile%lamn(il1:il2, :)
        csfv%CMAS(il1:il2, :) = pm%tile%cmas(il1:il2, :)
        csfv%ROOT(il1:il2, :) = pm%tile%root(il1:il2, :)
        csfv%RSMN(il1:il2, :) = pm%tile%rsmn(il1:il2, :)
        csfv%QA50(il1:il2, :) = pm%tile%qa50(il1:il2, :)
        csfv%VPDA(il1:il2, :) = pm%tile%vpda(il1:il2, :)
        csfv%VPDB(il1:il2, :) = pm%tile%vpdb(il1:il2, :)
        csfv%PSGA(il1:il2, :) = pm%tile%psga(il1:il2, :)
        csfv%PSGB(il1:il2, :) = pm%tile%psgb(il1:il2, :)
        csfv%DRN(il1:il2) = pm%tile%drn(il1:il2)
        csfv%SDEP(il1:il2) = pm%tile%sdep(il1:il2)
        csfv%XSLP(il1:il2) = pm%tile%xslp(il1:il2)
        DDGAT(il1:il2) = pm%tile%dd(il1:il2)
        MANNGAT(il1:il2) = pm%tile%mann(il1:il2)
        XDGAT(il1:il2) = pm%tile%grkf(il1:il2)
        KSGAT(il1:il2) = pm%tile%ks(il1:il2)
        csfv%SAND(il1:il2, :) = pm%tile%sand(il1:il2, :)
        csfv%CLAY(il1:il2, :) = pm%tile%clay(il1:il2, :)
        csfv%ORGM(il1:il2, :) = pm%tile%orgm(il1:il2, :)
        cpv%CMAI(il1:il2) = vs%tile%cmas(il1:il2)
        cpv%WSNO(il1:il2) = vs%tile%lqwssno(il1:il2)
        cpv%QAC(il1:il2) = vs%tile%qacan(il1:il2)
        cpv%TCAN(il1:il2) = vs%tile%tcan(il1:il2)
        cpv%TAC(il1:il2) = vs%tile%tacan(il1:il2)
        cpv%TSNO(il1:il2) = vs%tile%tsno(il1:il2)
        cpv%TPND(il1:il2) = vs%tile%tpnd(il1:il2)
        cpv%ZPND(il1:il2) = vs%tile%zpnd(il1:il2)
        cpv%RCAN(il1:il2) = vs%tile%lqwscan(il1:il2)
        cpv%SNCAN(il1:il2) = vs%tile%fzwscan(il1:il2)
        cpv%SNO(il1:il2) = vs%tile%sno(il1:il2)
        cpv%ALBS(il1:il2) = vs%tile%albsno(il1:il2)
        cpv%RHOS(il1:il2) = vs%tile%rhosno(il1:il2)
        cpv%GRO(il1:il2) = vs%tile%gro(il1:il2)
        cpv%TSFS(il1:il2, :) = vs%tile%tsfs(il1:il2, :)
        cpv%TBAR(il1:il2, :) = vs%tile%tsol(il1:il2, :)
        cpv%THLQ(il1:il2, :) = vs%tile%thlqsol(il1:il2, :)
        cpv%THIC(il1:il2, :) = vs%tile%thicsol(il1:il2, :)
        cpv%TBAS(il1:il2) = vs%tile%tbas(il1:il2)
        csfv%ZSNL(il1:il2) = pm%tile%zsnl(il1:il2)
        csfv%ZPLG(il1:il2) = pm%tile%zplg(il1:il2)
        csfv%ZPLS(il1:il2) = pm%tile%zpls(il1:il2)
        if (allocated(RUNCLASS36_flgs%pm%tile%FREZTH)) then
            if (DIAGNOSEMODE) then
                call print_message('ICEBAL_FREEZE_THRESHOLD (FREZTH) override is ACTIVE.')
                if (allocated(RUNCLASS36_flgs%pm%constant%FREZTH)) then
                    write(line, FMT_GEN) 'Uniform value: ', RUNCLASS36_flgs%pm%constant%FREZTH
                    call print_message(line)
                end if
                if (allocated(RUNCLASS36_flgs%pm%gru%FREZTH)) then
                    write(line, FMT_GEN) 'GRU value: ', (RUNCLASS36_flgs%pm%gru%FREZTH(j), j = 1, NTYPE)
                    call print_message(line)
                end if
            end if
            cglv%FREZTH(il1:il2) = RUNCLASS36_flgs%pm%tile%FREZTH(il1:il2)
        else
            cglv%FREZTH(il1:il2) = -2.0
        end if
        if (allocated(RUNCLASS36_flgs%pm%tile%SWELIM)) then
            if (DIAGNOSEMODE) then
                call print_message('ICEBAL_SWE_LIMIT (SWELIM) override is ACTIVE.')
                if (allocated(RUNCLASS36_flgs%pm%constant%SWELIM)) then
                    write(line, FMT_GEN) 'Uniform value: ', RUNCLASS36_flgs%pm%constant%SWELIM
                    call print_message(line)
                end if
                if (allocated(RUNCLASS36_flgs%pm%gru%SWELIM)) then
                    write(line, FMT_GEN) 'GRU value: ', (RUNCLASS36_flgs%pm%gru%SWELIM(j), j = 1, NTYPE)
                    call print_message(line)
                end if
            end if
            cglv%SWELIM(il1:il2) = RUNCLASS36_flgs%pm%tile%SWELIM(il1:il2)
        else
            cglv%SWELIM(il1:il2) = 100.0
        end if
        if (allocated(RUNCLASS36_flgs%pm%tile%SNDENLIM)) then
            if (DIAGNOSEMODE) then
                call print_message('ICEBAL_SNOW_DENSITY_LIMIT (SNDENLIM) override is ACTIVE.')
                if (allocated(RUNCLASS36_flgs%pm%constant%SNDENLIM)) then
                    write(line, FMT_GEN) 'Uniform value: ', RUNCLASS36_flgs%pm%constant%SNDENLIM
                    call print_message(line)
                end if
                if (allocated(RUNCLASS36_flgs%pm%gru%SNDENLIM)) then
                    write(line, FMT_GEN) 'GRU value: ', (RUNCLASS36_flgs%pm%gru%SNDENLIM(j), j = 1, NTYPE)
                    call print_message(line)
                end if
            end if
            cglv%SNDENLIM(il1:il2) = RUNCLASS36_flgs%pm%tile%SNDENLIM(il1:il2)
        else
            cglv%SNDENLIM(il1:il2) = 900.0
        end if

        cdv%ITCT = 0

        !> FROZENSOILINFILFLAG 1.
        allocate(FRZCGAT(NML), stat = ierr)
        allocate(INFILTYPE(NML), SI(NML), TSI(NML), &
                 SNOWMELTD(NML), SNOWMELTD_LAST(NML), SNOWINFIL(NML), &
                 CUMSNOWINFILCS(NML), MELTRUNOFF(NML), CUMSNOWINFILGS(NML))
!todo: move to read_parameters
        if (FROZENSOILINFILFLAG == 1) then
            NMELT = 1
            INFILTYPE = 2 !> INITIALIZED WITH UNLIMITED INFILTRATION
            SNOWMELTD = 0.0
            SNOWINFIL = 0.0
            CUMSNOWINFILCS = 0.0
            CUMSNOWINFILGS = 0.0
            MELTRUNOFF = 0.0
            SI = 0.20
            TSI = -0.10
            do k = il1, il2
                FRZCGAT(k) = hp%FRZCROW(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end do
        end if

        !> IWF 2 (PDMROF) and IWF 3 (LATFLOW).
        allocate(&
            CMINPDM(NML), CMAXPDM(NML), BPDM(NML), K1PDM(NML), K2PDM(NML), &
            ZPNDPRECS(NML), ZPONDPREC(NML), ZPONDPREG(NML), ZPNDPREGS(NML), &
            UM1CS(NML), UM1C(NML), UM1G(NML), UM1GS(NML), &
            QM1CS(NML), QM1C(NML), QM1G(NML), QM1GS(NML), &
            QM2CS(NML), QM2C(NML), QM2G(NML), QM2GS(NML), &
            UMQ(NML), &
            FSTRCS(NML), FSTRC(NML), FSTRG(NML), FSTRGS(NML))
        ZPNDPRECS = 0.0; ZPONDPREC = 0.0; ZPONDPREG = 0.0; ZPNDPREGS = 0.0
        UM1CS = 0.0; UM1C = 0.0; UM1G = 0.0; UM1GS = 0.0
        QM1CS = 0.0; QM1C = 0.0; QM1G = 0.0; QM1GS = 0.0
        QM2CS = 0.0; QM2C = 0.0; QM2G = 0.0; QM2GS = 0.0
        UMQ = 0.0
        FSTRCS = 0.0; FSTRC = 0.0; FSTRG = 0.0; FSTRGS = 0.0
        if (any(csfv%IWF == 2) .or. any(csfv%IWF == 3)) then
            do k = il1, il2
                ik = shd%lc%ILMOS(k)
                jk = shd%lc%JLMOS(k)
                CMINPDM(k) = hp%CMINROW(ik, jk)
                CMAXPDM(k) = hp%CMAXROW(ik, jk)
                BPDM(k) = hp%BROW(ik, jk)
                K1PDM(k) = hp%K1ROW(ik, jk)
                K2PDM(k) = hp%K2ROW(ik, jk)
            end do
        end if

        !> *********************************************************************
        !> Call CLASSBG to set more CLASS variables
        !> *********************************************************************
        !> bjd - July 25, 2005: For inputting field measured soil properties.
        call CLASSBG(csfv%THP, csfv%THR, csfv%THM, csfv%BI, csfv%PSIS, csfv%GRKS, &
                     csfv%THRA, csfv%HCPS, csfv%TCS, csfv%THFC, csfv%PSIW, &
                     csfv%DELZW, csfv%ZBTW, csfv%ALGW, csfv%ALGD, &
                     csfv%SAND, csfv%CLAY, csfv%ORGM, shd%lc%sl%DELZ, shd%lc%sl%ZBOT, csfv%SDEP, &
                     csfv%ISND, csfv%IGDR, NML, il1, il2, NSL, ICTEMMOD, &
                     pm%gru%thpor, pm%gru%thlret, pm%gru%thlmin, pm%gru%bi, &
                     pm%gru%psisat, pm%gru%grksat, pm%gru%hcps, pm%gru%tcs, &
                     NA, NTYPE, shd%lc%ILG, shd%lc%ILMOS, shd%lc%JLMOS)

        pm%tile%alwet(il1:il2) = csfv%ALGW(il1:il2)
        pm%tile%aldry(il1:il2) = csfv%ALGD(il1:il2)
        pm%tile%thpor(il1:il2, :) = csfv%THP(il1:il2, :)
        pm%tile%thlret(il1:il2, :) = csfv%THR(il1:il2, :)
        pm%tile%thlmin(il1:il2, :) = csfv%THM(il1:il2, :)
        pm%tile%bi(il1:il2, :) = csfv%BI(il1:il2, :)
        pm%tile%psisat(il1:il2, :) = csfv%PSIS(il1:il2, :)
        pm%tile%grksat(il1:il2, :) = csfv%GRKS(il1:il2, :)
        pm%tile%thlrat(il1:il2, :) = csfv%THRA(il1:il2, :)
        pm%tile%hcps(il1:il2, :) = csfv%HCPS(il1:il2, :)
        pm%tile%tcs(il1:il2, :) = csfv%TCS(il1:il2, :)
        pm%tile%thfc(il1:il2, :) = csfv%THFC(il1:il2, :)
        pm%tile%psiwlt(il1:il2, :) = csfv%PSIW(il1:il2, :)
        vs%tile%dzsolhyd(il1:il2, :) = csfv%DELZW(il1:il2, :)
        vs%tile%zsolhyd(il1:il2) = sum(csfv%ZBTW(il1:il2, :), 2)

        !> CLASS output files.
        if (WF_NUM_POINTS > 0) call CLASSOUT_open_files(shd)

        do k = il1, il2
            ik = shd%lc%ILMOS(k)
            catv%RADJ(k) = shd%ylat(ik)*PI/180.0
            catv%DLON(k) = shd%xlng(ik)
        end do
        catv%Z0OR = 0.0
        catv%GGEO(:) = GGEOGRD(1)
        catv%ZDM = 10.0
        catv%ZDH = 2.0

    end subroutine

    subroutine read_init_prog_variables_class_row(fls, shd)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        type(fl_ids) fls
        type(ShedGridParams) shd

        !> For SAVERESUMEFLAG 3
        real(kind = 4), dimension(:, :), allocatable :: ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
            RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
            TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW
        real(kind = 4), dimension(:, :, :), allocatable :: TBARROW, THICROW, THLQROW, TSFSROW

        integer NA, NTYPE, NSL, k, ik, jk, iun, ierr

        !> Return if the process is not active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Local indices.
        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND

        !> Open the resume state file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)), status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Allocate temporary variables.
        allocate(ALBSROW(NA, NTYPE), CMAIROW(NA, NTYPE), GROROW(NA, NTYPE), QACROW(NA, NTYPE), RCANROW(NA, NTYPE), &
                 RHOSROW(NA, NTYPE), SCANROW(NA, NTYPE), SNOROW(NA, NTYPE), TACROW(NA, NTYPE), TBASROW(NA, NTYPE), &
                 TCANROW(NA, NTYPE), TPNDROW(NA, NTYPE), TSNOROW(NA, NTYPE), WSNOROW(NA, NTYPE), ZPNDROW(NA, NTYPE), &
                 TBARROW(NA, NTYPE, NSL), THICROW(NA, NTYPE, NSL), THLQROW(NA, NTYPE, NSL), TSFSROW(NA, NTYPE, 4))

        !> Read inital values from the file.
        read(iun) ALBSROW
        read(iun) CMAIROW
        read(iun) GROROW
        read(iun) QACROW
        read(iun) RCANROW
        read(iun) RHOSROW
        read(iun) SCANROW
        read(iun) SNOROW
        read(iun) TACROW
        read(iun) TBARROW
        read(iun) TBASROW
        read(iun) TCANROW
        read(iun) THICROW
        read(iun) THLQROW
        read(iun) TPNDROW
        read(iun) TSFSROW
        read(iun) TSNOROW
        read(iun) WSNOROW
        read(iun) ZPNDROW

        !> Close the file to free the unit.
        close(iun)

        !> Scatter the temporary variables.
        do k = il1, il2

            !> Grab the grid and GRU of the current tile.
            ik = shd%lc%ILMOS(k)
            jk = shd%lc%JLMOS(k)

            !> Assign values.
            vs%tile%albsno(k) = ALBSROW(ik, jk)
            vs%tile%cmas(k) = CMAIROW(ik, jk)
            vs%tile%gro(k) = GROROW(ik, jk)
            vs%tile%qacan(k) = QACROW(ik, jk)
            vs%tile%lqwscan(k) = RCANROW(ik, jk)
            vs%tile%rhosno(k) = RHOSROW(ik, jk)
            vs%tile%fzwscan(k) = SCANROW(ik, jk)
            vs%tile%sno(k) = SNOROW(ik, jk)
            vs%tile%tacan(k) = TACROW(ik, jk)
            vs%tile%tsol(k, :) = TBARROW(ik, jk, :)
            vs%tile%tbas(k) = TBASROW(ik, jk)
            vs%tile%tcan(k) = TCANROW(ik, jk)
            vs%tile%thicsol(k, :) = THICROW(ik, jk, :)
            vs%tile%thlqsol(k, :) = THLQROW(ik, jk, :)
            vs%tile%tpnd(k) = TPNDROW(ik, jk)
            vs%tile%tsfs(k, :) = TSFSROW(ik, jk, :)
            vs%tile%tsno(k) = TSNOROW(ik, jk)
            vs%tile%lqwssno(k) = WSNOROW(ik, jk)
            vs%tile%zpnd(k) = ZPNDROW(ik, jk)

        end do

        !> Deallocate temporary variables.
        deallocate(ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
                   RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
                   TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW, &
                   TBARROW, THICROW, THLQROW, TSFSROW)

    end subroutine

    subroutine save_init_prog_variables_class_row(fls, shd)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        type(fl_ids) fls
        type(ShedGridParams) shd

        !> For SAVERESUMEFLAG 3
        real(kind = 4), dimension(:, :), allocatable :: ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
            RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
            TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW
        real(kind = 4), dimension(:, :, :), allocatable :: TBARROW, THICROW, THLQROW, TSFSROW

        integer NA, NTYPE, NSL, k, ik, jk, iun, ierr

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Local indices.
        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND

        !> Open the resume state file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)), status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Allocate and initialize temporary variables.
        allocate(ALBSROW(NA, NTYPE), CMAIROW(NA, NTYPE), GROROW(NA, NTYPE), QACROW(NA, NTYPE), RCANROW(NA, NTYPE), &
                 RHOSROW(NA, NTYPE), SCANROW(NA, NTYPE), SNOROW(NA, NTYPE), TACROW(NA, NTYPE), TBASROW(NA, NTYPE), &
                 TCANROW(NA, NTYPE), TPNDROW(NA, NTYPE), TSNOROW(NA, NTYPE), WSNOROW(NA, NTYPE), ZPNDROW(NA, NTYPE), &
                 TBARROW(NA, NTYPE, NSL), THICROW(NA, NTYPE, NSL), THLQROW(NA, NTYPE, NSL), TSFSROW(NA, NTYPE, 4))
        ALBSROW = 0.0; CMAIROW = 0.0; GROROW = 0.0; QACROW = 0.0; RCANROW = 0.0; RHOSROW = 0.0
        SCANROW = 0.0; SNOROW = 0.0; TACROW = 0.0; TBASROW = 0.0; TCANROW = 0.0; TPNDROW = 0.0
        TSNOROW = 0.0; WSNOROW = 0.0; ZPNDROW = 0.0
        TBARROW = 0.0; THICROW = 0.0; THLQROW = 0.0; TSFSROW = 0.0

        !> Gather the temporary variables.
        do k = 1, shd%lc%NML

            !> Grab the grid and GRU of the current tile.
            ik = shd%lc%ILMOS(k)
            jk = shd%lc%JLMOS(k)

            !> Assign values.
            ALBSROW(ik, jk) = real(vs%tile%albsno(k), kind = 4)
            CMAIROW(ik, jk) = real(vs%tile%cmas(k), kind = 4)
            GROROW(ik, jk) = real(vs%tile%gro(k), kind = 4)
            QACROW(ik, jk) = real(vs%tile%qacan(k), kind = 4)
            RCANROW(ik, jk) = real(vs%tile%lqwscan(k), kind = 4)
            RHOSROW(ik, jk) = real(vs%tile%rhosno(k), kind = 4)
            SCANROW(ik, jk) = real(vs%tile%fzwscan(k), kind = 4)
            SNOROW(ik, jk) = real(vs%tile%sno(k), kind = 4)
            TACROW(ik, jk) = real(vs%tile%tacan(k), kind = 4)
            TBARROW(ik, jk, :) = real(vs%tile%tsol(k, :), kind = 4)
            TBASROW(ik, jk) = real(vs%tile%tbas(k), kind = 4)
            TCANROW(ik, jk) = real(vs%tile%tcan(k), kind = 4)
            THICROW(ik, jk, :) = real(vs%tile%thicsol(k, :), kind = 4)
            THLQROW(ik, jk, :) = real(vs%tile%thlqsol(k, :), kind = 4)
            TPNDROW(ik, jk) = real(vs%tile%tpnd(k), kind = 4)
            TSFSROW(ik, jk, :) = real(vs%tile%tsfs(k, :), kind = 4)
            TSNOROW(ik, jk) = real(vs%tile%tsno(k), kind = 4)
            WSNOROW(ik, jk) = real(vs%tile%lqwssno(k), kind = 4)
            ZPNDROW(ik, jk) = real(vs%tile%zpnd(k), kind = 4)

        end do

        !> Read inital values from the file.
        write(iun) ALBSROW
        write(iun) CMAIROW
        write(iun) GROROW
        write(iun) QACROW
        write(iun) RCANROW
        write(iun) RHOSROW
        write(iun) SCANROW
        write(iun) SNOROW
        write(iun) TACROW
        write(iun) TBARROW
        write(iun) TBASROW
        write(iun) TCANROW
        write(iun) THICROW
        write(iun) THLQROW
        write(iun) TPNDROW
        write(iun) TSFSROW
        write(iun) TSNOROW
        write(iun) WSNOROW
        write(iun) ZPNDROW

        !> Close the file to free the unit.
        close(iun)

        !> Deallocate temporary variables.
        deallocate(ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
                   RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
                   TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW, &
                   TBARROW, THICROW, THLQROW, TSFSROW)

    end subroutine

    subroutine RUNCLASS36_finalize(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> For GRU-based end of run prognostic variables.
        real, dimension(:, :), allocatable :: tcan, rcan, sncan, gro, zpnd, tpnd, sno, tsno, albs, rhos
        real, dimension(:, :, :), allocatable :: tbar, thlq, thic
        integer, dimension(:), allocatable :: kc
        character cfmt*3, cfmtt*1000
        character(len = DEFAULT_LINE_LENGTH) line_buffer
        integer NA, NTYPE, NML, NSL, m, k, ik, jk, j, i, ignd, iun, ierr

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Local indices.
        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NML = shd%lc%NML
        NSL = shd%lc%IGND

        !> Write data to the output summary file.
        if (ECHOTXTMODE) then

            !> CLASS states for prognostic variables.
            allocate(tcan(3, NTYPE), rcan(3, NTYPE), sncan(3, NTYPE), gro(3, NTYPE), zpnd(3, NTYPE), tpnd(3, NTYPE), &
                     sno(3, NTYPE), tsno(3, NTYPE), albs(3, NTYPE), rhos(3, NTYPE), &
                     tbar(3, NTYPE, NSL), thlq(3, NTYPE, NSL), thic(3, NTYPE, NSL), kc(NTYPE))
            tcan = 0.0; rcan = 0.0; sncan = 0.0; gro = 0.0; zpnd = 0.0; tpnd = 0.0
            sno = 0.0; tsno = 0.0; albs = 0.0; rhos = 0.0
            tbar = 0.0; thlq = 0.0; thic = 0.0; kc = 0

            !> Loop through the GRUs.
            do m = 1, NTYPE

                !> Cycle if the GRU does not exist.
                if (count(shd%lc%JLMOS(1:NML) == m) == 0) cycle

                !> Canopy.
                tcan(3, m) = maxval(vs%tile%tcan, shd%lc%JLMOS(1:NML) == m)
                if (tcan(3, m) > 0.0) then
                    tcan(1, m) = sum(vs%tile%tcan, shd%lc%JLMOS(1:NML) == m .and. &
                                     vs%tile%tcan /= 0.0)/count(shd%lc%JLMOS(1:NML) == m .and. vs%tile%tcan /= 0.0)
                    tcan(2, m) = minval(vs%tile%tcan, shd%lc%JLMOS(1:NML) == m .and. vs%tile%tcan /= 0.0)
                end if
                where (tcan < 173.16 .or. tcan > 373.16 .or. tcan == 0.0) tcan = 273.16
                rcan(2, m) = minval(vs%tile%lqwscan, shd%lc%JLMOS(1:NML) == m)
                rcan(3, m) = maxval(vs%tile%lqwscan, shd%lc%JLMOS(1:NML) == m)
                rcan(1, m) = sum(vs%tile%lqwscan, shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                sncan(2, m) = minval(vs%tile%fzwscan, shd%lc%JLMOS(1:NML) == m)
                sncan(3, m) = maxval(vs%tile%fzwscan, shd%lc%JLMOS(1:NML) == m)
                sncan(1, m) = sum(vs%tile%fzwscan, shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                gro(2, m) = minval(vs%tile%gro, shd%lc%JLMOS(1:NML) == m)
                gro(3, m) = maxval(vs%tile%gro, shd%lc%JLMOS(1:NML) == m)
                gro(1, m) = sum(vs%tile%gro, shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)

                !> Ponded water at surface.
                zpnd(2, m) = minval(vs%tile%zpnd, shd%lc%JLMOS(1:NML) == m)
                zpnd(3, m) = maxval(vs%tile%zpnd, shd%lc%JLMOS(1:NML) == m)
                zpnd(1, m) = sum(vs%tile%zpnd, shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                tpnd(3, m) = maxval(vs%tile%tpnd, shd%lc%JLMOS(1:NML) == m)
                if (tpnd(3, m) > 0.0) then
                    tpnd(1, m) = sum(vs%tile%tpnd, shd%lc%JLMOS(1:NML) == m .and. &
                                     vs%tile%tpnd /= 0.0)/count(shd%lc%JLMOS(1:NML) == m .and. vs%tile%tpnd /= 0.0)
                    tpnd(2, m) = minval(vs%tile%tpnd, shd%lc%JLMOS(1:NML) == m .and. vs%tile%tpnd /= 0.0)
                end if
                where (tpnd < 173.16 .or. tpnd > 373.16 .or. tpnd == 0.0) tpnd = 273.16

                !> Snow.
                sno(2, m) = minval(vs%tile%sno, shd%lc%JLMOS(1:NML) == m)
                sno(3, m) = maxval(vs%tile%sno, shd%lc%JLMOS(1:NML) == m)
                sno(1, m) = sum(vs%tile%sno, shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                tsno(3, m) = maxval(vs%tile%tsno, shd%lc%JLMOS(1:NML) == m)
                if (tsno(3, m) > 0.0) then
                    tsno(1, m) = sum(vs%tile%tsno, shd%lc%JLMOS(1:NML) == m .and. &
                                     vs%tile%tsno /= 0.0)/count(shd%lc%JLMOS(1:NML) == m .and. vs%tile%tsno /= 0.0)
                    tsno(2, m) = minval(vs%tile%tsno, shd%lc%JLMOS(1:NML) == m .and. vs%tile%tsno /= 0.0)
                end if
                where (tsno < 173.16 .or. tsno > 373.16 .or. tsno == 0.0) tsno = 273.16
                if (sno(3, m) > 0.0) then
                    albs(1, m) = sum(vs%tile%albsno, shd%lc%JLMOS(1:NML) == m .and. &
                                     vs%tile%sno > 0.0)/count(shd%lc%JLMOS(1:NML) == m .and. vs%tile%sno > 0.0)
                    albs(2, m) = minval(vs%tile%albsno, shd%lc%JLMOS(1:NML) == m .and. vs%tile%sno > 0.0)
                    albs(3, m) = maxval(vs%tile%albsno, shd%lc%JLMOS(1:NML) == m .and. vs%tile%sno > 0.0)
                end if
                rhos(3, m) = maxval(vs%tile%rhosno, shd%lc%JLMOS(1:NML) == m)
                if (rhos(3, m) > 0.0) then
                    rhos(1, m) = sum(vs%tile%rhosno, shd%lc%JLMOS(1:NML) == m .and. &
                                     vs%tile%rhosno /= 0.0)/count(shd%lc%JLMOS(1:NML) == m .and. vs%tile%rhosno /= 0.0)
                    rhos(2, m) = minval(vs%tile%rhosno, shd%lc%JLMOS(1:NML) == m .and. vs%tile%rhosno /= 0.0)
                end if

                !> Soil.
                do j = 1, NSL
                    tbar(1, m, j) = sum(vs%tile%tsol(:, j), shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                    tbar(2, m, j) = minval(vs%tile%tsol(:, j), shd%lc%JLMOS(1:NML) == m)
                    tbar(3, m, j) = maxval(vs%tile%tsol(:, j), shd%lc%JLMOS(1:NML) == m)
                    thlq(1, m, j) = sum(vs%tile%thlqsol(:, j), shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                    thlq(2, m, j) = minval(vs%tile%thlqsol(:, j), shd%lc%JLMOS(1:NML) == m)
                    thlq(3, m, j) = maxval(vs%tile%thlqsol(:, j), shd%lc%JLMOS(1:NML) == m)
                    thic(1, m, j) = sum(vs%tile%thicsol(:, j), shd%lc%JLMOS(1:NML) == m)/count(shd%lc%JLMOS(1:NML) == m)
                    thic(2, m, j) = minval(vs%tile%thicsol(:, j), shd%lc%JLMOS(1:NML) == m)
                    thic(3, m, j) = maxval(vs%tile%thicsol(:, j), shd%lc%JLMOS(1:NML) == m)
                end do
            end do

            !> Write to file.
            if (NRSOILAYEREADFLAG > 3) then
                ignd = min(NRSOILAYEREADFLAG, NSL)
            else if (NRSOILAYEREADFLAG == 1) then
                ignd = NSL
            else
                ignd = 3
            end if
            write(cfmt, '(i3)') ignd
            call print_echo_txt('')
            call print_echo_txt('End of run prognostic states')
            write(line_buffer, '(3x, (a), i4)') 'Number of GRUs: ', NTYPE
            call print_echo_txt(trim(line_buffer))
            do i = 1, 3
                call print_echo_txt('')
                select case (i)
                    case (1); call print_echo_txt('Average values')
                    case (2); call print_echo_txt('Minimum values')
                    case (3); call print_echo_txt('Maximum values')
                end select
                do m = 1, NTYPE
                    write(line_buffer, "(3x, 'GRU ', i3, ':')") m
                    call print_echo_txt(trim(line_buffer))
                    cfmtt = "(" // trim(adjustl(cfmt)) // "(f10.3), 3(f10.3), " // &
                            "2x, '!> TBAR(1:" // trim(adjustl(cfmt)) // ")/TCAN/TSNO/TPND')"
                    write(line_buffer, cfmtt) ((tbar(i, m, j) - 273.16), j = 1, ignd), &
                        (tcan(i, m) - 273.16), (tsno(i, m) - 273.16), (tpnd(i, m) - 273.16)
                    call print_echo_txt(trim(line_buffer))
                    cfmtt = "(" // trim(adjustl(cfmt)) // "(f10.3), " // trim(adjustl(cfmt)) // "(f10.3), f10.3, " // &
                            "2x, '!> THLQ(1:" // trim(adjustl(cfmt)) // ")/THIC(1:" // trim(adjustl(cfmt)) // ")/ZPND')"
                    write(line_buffer, cfmtt) (thlq(i, m, j), j = 1, ignd), (thic(i, m, j), j = 1, ignd), zpnd(i, m)
                    call print_echo_txt(trim(line_buffer))
                    write(line_buffer, "(6(f10.3), 2x, '!> RCAN/SNCAN/SNO/ALBS/RHOS/GRO')") &
                        rcan(i, m), sncan(i, m), sno(i, m), albs(i, m), rhos(i, m), gro(i, m)
                    call print_echo_txt(trim(line_buffer))
                end do
            end do

        end if

    end subroutine

end module
