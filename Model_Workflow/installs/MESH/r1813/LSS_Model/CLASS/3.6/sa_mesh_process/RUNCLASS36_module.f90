module RUNCLASS36_module

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    implicit none

    contains

    subroutine RUNCLASS36_within_tile(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> For internal variables.
        use RUNCLASS36_config

        !> For CLASS output.
        use RUNCLASS36_save_output

        use FLAGS

        !> For PBSM.
        use PBSM_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer NA, NTYPE, NML, IGND, k, ik, j, i
        real FRAC

        !* ierr: Diagnostic error/status return from various subroutines.
        integer :: ierr = 0

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

!> *********************************************************************
!> Start of the NML-based LSS loop.
!> *********************************************************************

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NML = shd%lc%NML
        IGND = shd%lc%IGND

        !> N is only used for debugging purposes.
        !> N = ic%ts_count

        if (ipid /= 0 .or. izero == 0) then

            !> Transfer driving variables.
            cfi%FSVH(il1:il2) = vs%tile%fsin(il1:il2)/2.0
            cfi%FSIH(il1:il2) = vs%tile%fsin(il1:il2)/2.0
            cfi%FDL(il1:il2) = vs%tile%flin(il1:il2)
            if (IPCP == 4) then
                catv%RPRE(il1:il2) = vs%tile%prern(il1:il2)
                catv%SPRE(il1:il2) = vs%tile%presno(il1:il2)

                !> Required to activate 'CLASSI'.
                cfi%PRE(il1:il2) = catv%RPRE(il1:il2) + catv%SPRE(il1:il2)
            else
                cfi%PRE(il1:il2) = vs%tile%pre(il1:il2)
            end if
            cfi%TA(il1:il2) = vs%tile%ta(il1:il2)
            cfi%UL(il1:il2) = vs%tile%uv(il1:il2)
            cfi%PRES(il1:il2) = vs%tile%pres(il1:il2)
            cfi%QA(il1:il2) = vs%tile%qa(il1:il2)

            cfi%VMOD(il1:il2) = max(VMIN, cfi%UL(il1:il2))

            !> This estimates the fractional cloud cover (FCLOGRD) by the basis
            !> of the solar zenith angle and the occurrence of precipitation.
            !> Assumed to be 1 (100%) when precipitation occurs and somewhere
            !> in the range of [0.1, 1] based on the location of the sun in the
            !> sky when precipitation is not occuring. (0.1 when the sun is at
            !> the zenith, 1 when the sun is at the horizon).
            RDAY = real(ic%now%jday) + (real(ic%now%hour) + real(ic%now%mins)/60.0)/24.0
            DECL = sin(2.0*PI*(284.0 + RDAY)/365.0)*23.45*PI/180.0
            HOUR = (real(ic%now%hour) + real(ic%now%mins)/60.0)*PI/12.0 - PI

            do k = il1, il2
                COSZ = sin(catv%RADJ(k))*sin(DECL) + cos(catv%RADJ(k))*cos(DECL)*cos(HOUR)
                catv%CSZ(k) = sign(max(abs(COSZ), 1.0e-3), COSZ)
                if (cfi%PRE(k) > 0.0) then
                    catv%FCLO(k) = 1.0
                else
                    catv%FCLO(k) = max(0.0, min(1.0 - 0.9*COSZ, 1.0))
                end if
            end do

            !> Grab states.
            cpv%QAC(il1:il2) = vs%tile%qacan(il1:il2)
            cpv%RCAN(il1:il2) = vs%tile%lqwscan(il1:il2)
            cpv%SNCAN(il1:il2) = vs%tile%fzwscan(il1:il2)
            cpv%TAC(il1:il2) = vs%tile%tacan(il1:il2)
            cpv%TCAN(il1:il2) = vs%tile%tcan(il1:il2)
            cpv%CMAI(il1:il2) = vs%tile%cmas(il1:il2)
            cpv%GRO(il1:il2) = vs%tile%gro(il1:il2)
            cpv%SNO(il1:il2) = vs%tile%sno(il1:il2)
            cpv%ALBS(il1:il2) = vs%tile%albsno(il1:il2)
            cpv%RHOS(il1:il2) = vs%tile%rhosno(il1:il2)
            cpv%TSNO(il1:il2) = vs%tile%tsno(il1:il2)
            cpv%WSNO(il1:il2) = vs%tile%lqwssno(il1:il2)
            cpv%TPND(il1:il2) = vs%tile%tpnd(il1:il2)
            cpv%ZPND(il1:il2) = vs%tile%zpnd(il1:il2)
            cpv%TSFS(il1:il2, :) = vs%tile%tsfs(il1:il2, :)
            cpv%TBAS(il1:il2) = vs%tile%tbas(il1:il2)
            cpv%THIC(il1:il2, :) = vs%tile%thicsol(il1:il2, :)
            cpv%THLQ(il1:il2, :) = vs%tile%thlqsol(il1:il2, :)
            cpv%TBAR(il1:il2, :) = vs%tile%tsol(il1:il2, :)

            !> Initialize diagnostic variables for PBSM.
            if (pbsm%PROCESS_ACTIVE) then
                pbsm%vs%Drift(il1:il2) = 0.0
                pbsm%vs%Subl(il1:il2) = 0.0
                pbsm%vs%Deposition(il1:il2) = 0.0
            end if

            !> INITIALIZATION OF DIAGNOSTIC VARIABLES SPLIT OUT OF CLASSG
            !> FOR CONSISTENCY WITH GCM APPLICATIONS.
            cdv%CDH = 0.0
            cdv%CDM = 0.0
            cdv%HFS = 0.0
            cdv%TFX = 0.0
            cdv%QEVP = 0.0
            cdv%QFS = 0.0
            cdv%QFX = 0.0
            cdv%PET = 0.0
            cdv%GA = 0.0
            cdv%EF = 0.0
            cdv%GTE = 0.0
            cdv%QG = 0.0
            cdv%ALVS = 0.0
            cdv%ALIR = 0.0
            cdv%SFCT = 0.0
            cdv%SFCU = 0.0
            cdv%SFCV = 0.0
            cdv%SFCQ = 0.0
            cdv%FSNO = 0.0
            cdv%FSGV = 0.0
            cdv%FSGS = 0.0
            cdv%FSGG = 0.0
            cdv%FLGV = 0.0
            cdv%FLGS = 0.0
            cdv%FLGG = 0.0
            cdv%HFSC = 0.0
            cdv%HFSS = 0.0
            cdv%HFSG = 0.0
            cdv%HEVC = 0.0
            cdv%HEVS = 0.0
            cdv%HEVG = 0.0
            cdv%HMFC = 0.0
            cdv%HMFN = 0.0
            cdv%HTCC = 0.0
            cdv%HTCS = 0.0
            cdv%PCFC = 0.0
            cdv%PCLC = 0.0
            cdv%PCPN = 0.0
            cdv%PCPG = 0.0
            cdv%QFG = 0.0
            cdv%QFN = 0.0
            cdv%QFCF = 0.0
            cdv%QFCL = 0.0
            cdv%ROF = 0.0
            cdv%ROFO = 0.0
            cdv%ROFS = 0.0
            cdv%ROFB = 0.0
            cdv%TROF = 0.0
            cdv%TROO = 0.0
            cdv%TROS = 0.0
            cdv%TROB = 0.0
            cdv%ROFC = 0.0
            cdv%ROFN = 0.0
            cdv%ROVG = 0.0
            cdv%WTRC = 0.0
            cdv%WTRS = 0.0
            cdv%WTRG = 0.0
            cdv%DR = 0.0
            cdv%HMFG = 0.0
            cdv%HTC = 0.0
            cdv%QFC = 0.0
            cdv%GFLX = 0.0
            cdv%ITCT = 0
            cdv%ICE = 0.0
            cdv%TICE = 0.0

            call CLASSI(catv%VPD, catv%TADP, catv%PADR, catv%RHOA, catv%RHSI, &
                        catv%RPCP, catv%TRPC, catv%SPCP, catv%TSPC, cfi%TA, cfi%QA, &
                        cfi%PRE, catv%RPRE, catv%SPRE, cfi%PRES, &
                        IPCP, NML, il1, il2)

            call CLASSZ(0, CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        cdv%FSGV, cdv%FLGV, cdv%HFSC, cdv%HEVC, cdv%HMFC, cdv%HTCC, &
                        cdv%FSGS, cdv%FLGS, cdv%HFSS, cdv%HEVS, cdv%HMFN, cdv%HTCS, &
                        cdv%FSGG, cdv%FLGG, cdv%HFSG, cdv%HEVG, cdv%HMFG, cdv%HTC, &
                        cdv%PCFC, cdv%PCLC, cdv%QFCF, cdv%QFCL, cdv%ROFC, cdv%WTRC, &
                        cdv%PCPN, cdv%QFN, cdv%ROFN, cdv%WTRS, cdv%PCPG, cdv%QFG, &
                        cdv%QFC, cdv%ROF, cdv%WTRG, cpv%CMAI, cpv%RCAN, cpv%SNCAN, &
                        cpv%TCAN, cpv%SNO, cpv%WSNO, cpv%TSNO, cpv%THLQ, cpv%THIC, &
                        csfv%HCPS, csfv%THP, csfv%DELZW, cpv%TBAR, cpv%ZPND, cpv%TPND, &
                        cdv%ICE, cdv%TICE, &
                        shd%lc%sl%DELZ, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, &
                        il1, il2, NML, IGND, ic%ts_count, &
                        pbsm%vs%Drift, pbsm%vs%Subl)

            !> ALBEDO AND TRANSMISSIVITY CALCULATIONS; GENERAL VEGETATION
            !> CHARACTERISTICS.
            call CLASSA(cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, ALVSCN, ALIRCN, &
                        ALVSG, ALIRG, ALVSCS, ALIRCS, ALVSSN, ALIRSN, &
                        ALVSGC, ALIRGC, ALVSSC, ALIRSC, TRVSCN, TRIRCN, &
                        TRVSCS, TRIRCS, FSVF, FSVFS, &
                        RAICAN, RAICNS, SNOCAN, SNOCNS, FRAINC, FSNOWC, &
                        FRAICS, FSNOCS, &
                        DISP, DISPS, ZOMLNC, ZOMLCS, &
                        ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, &
                        CHCAP, CHCAPS, CMASSC, CMASCS, CWLCAP, CWFCAP, &
                        CWLCPS, CWFCPS, RC, RCS, RBCOEF, FROOT, &
                        ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TRSNOW, ZSNOW, &
                        cpv%WSNO, cdv%ALVS, cdv%ALIR, cdv%HTCC, cdv%HTCS, cdv%HTC, &
                        cdv%WTRC, cdv%WTRS, cdv%WTRG, cpv%CMAI, cdv%FSNO, &
                        csfv%FCAN, csfv%LNZ0, csfv%ALVC, csfv%ALIC, csfv%PAMX, csfv%PAMN, &
                        csfv%CMAS, csfv%ROOT, csfv%RSMN, csfv%QA50, csfv%VPDA, csfv%VPDB, &
                        csfv%PSGA, csfv%PSGB, csfv%PAID, csfv%HGTD, csfv%ACVD, csfv%ACID, &
                        csfv%ASVD, csfv%ASID, csfv%AGVD, csfv%AGID, csfv%ALGW, csfv%ALGD, &
                        cpv%THLQ, cpv%THIC, cpv%TBAR, cpv%RCAN, cpv%SNCAN, cpv%TCAN, &
                        cpv%GRO, cpv%SNO, cpv%TSNO, cpv%RHOS, cpv%ALBS, catv%ZBLD, &
                        catv%Z0OR, csfv%ZSNL, csfv%ZPLG, csfv%ZPLS, &
                        catv%FCLO, cfi%TA, catv%VPD, catv%RHOA, catv%CSZ, &
                        cfi%FSVH, catv%RADJ, catv%DLON, catv%RHSI, shd%lc%sl%DELZ, csfv%DELZW, &
                        csfv%ZBTW, csfv%THP, csfv%THM, csfv%PSIS, csfv%BI, csfv%PSIW, &
                        csfv%HCPS, csfv%ISND, &
                        FCANCMX, ICTEM, ICTEMMOD, RMATC, &
                        AILC, PAIC, L2MAX, NOL2PFTS, &
                        AILCG, AILCGS, FCANC, FCANCS, &
                        ic%now%jday, NML, il1, il2, &
                        JLAT, ic%ts_count, ICAN, ICAN + 1, IGND, IDISP, IZREF, &
                        csfv%IWF, IPAI, IHGT, IALC, IALS, IALG)

            !> SURFACE TEMPERATURE AND FLUX CALCULATIONS.
            call CLASST(TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, &
                        G23C, G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        EVAPC, EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP, CHCAPS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        cdv%ITCT, cdv%CDH, cdv%CDM, cdv%HFS, cdv%TFX, cdv%QEVP, cdv%QFS, cdv%QFX, &
                        cdv%PET, cdv%GA, cdv%EF, cdv%GTE, cdv%QG, cdv%SFCT, cdv%SFCU, cdv%SFCV, &
                        cdv%SFCQ, SFRHGAT, cdv%FSGV, cdv%FSGS, cdv%FSGG, cdv%FLGV, cdv%FLGS, cdv%FLGG, &
                        cdv%HFSC, cdv%HFSS, cdv%HFSG, cdv%HEVC, cdv%HEVS, cdv%HEVG, cdv%HMFC, cdv%HMFN, &
                        cdv%HTCC, cdv%HTCS, cdv%HTC, cdv%QFCF, cdv%QFCL, cdv%DR, cdv%WTAB, cdv%ILMO, &
                        cdv%UE, cdv%HBL, cpv%TAC, cpv%QAC, catv%ZRFM, catv%ZRFH, catv%ZDM, catv%ZDH, &
                        catv%VPD, catv%TADP, catv%RHOA, cfi%FSVH, cfi%FSIH, cfi%FDL, cfi%UL, cfi%VL, &
                        cfi%TA, cfi%QA, catv%PADR, cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, RBCOEF, &
                        FSVF, FSVFS, cfi%PRES, cfi%VMOD, ALVSCN, ALIRCN, ALVSG, ALIRG, &
                        ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC, &
                        TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC, RCS, cdv%WTRG, QLWOGAT, &
                        FRAINC, FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP, DISPS, &
                        ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, &
                        cpv%TBAR, cpv%THLQ, cpv%THIC, cpv%TPND, cpv%ZPND, cpv%TBAS, cpv%TCAN, cpv%TSNO, &
                        ZSNOW, TRSNOW, cpv%RHOS, cpv%WSNO, csfv%THP, csfv%THR, csfv%THM, csfv%THFC, &
                        catv%RADJ, cfi%PRE, csfv%HCPS, csfv%TCS, cpv%TSFS, shd%lc%sl%DELZ, csfv%DELZW, csfv%ZBTW, &
                        FTEMP, FVAP, RIB, csfv%ISND, &
                        AILCG, AILCGS, FCANC, FCANCS, CO2CONC, CO2I1CG, CO2I1CS, CO2I2CG, &
                        CO2I2CS, COSZS, XDIFFUSC, SLAI, ICTEM, ICTEMMOD, RMATCTEM, &
                        FCANCMX, L2MAX, NOL2PFTS, CFLUXCG, CFLUXCS, ANCSVEG, ANCGVEG, &
                        RMLCSVEG, RMLCGVEG, FIELDSM, WILTSM, &
                        ITC, ITCG, ITG, NML, il1, il2, JLAT, ic%ts_count, ICAN, &
                        IGND, IZREF, ISLFD, NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI)

            !> FROZENSOILINFILFLAG 1.
            if (FROZENSOILINFILFLAG == 1) then

                !> bruce davison - only increase NMELT if we don't start the run on January 1st, otherwise t0_ACC allocation is too large
                !> and the model crashes if the compiler is checking for array bounds when t0_ACC is passed into CLASSW with size NMELT
                if (ic%now%jday == 1 .and. ic%ts_daily == 48) then
                    if (ic%start%jday == 1 .and. ic%ts_count < 49) then
                        continue ! NMELT should stay = 1
                    else
                        NMELT = NMELT + 1
                    end if
                    CUMSNOWINFILCS = 0.0
                    CUMSNOWINFILGS = 0.0
                    INFILTYPE = 2
                end if
            end if

            !> WATER BUDGET CALCULATIONS.
            call CLASSW(cpv%THLQ, cpv%THIC, cpv%TBAR, cpv%TCAN, cpv%RCAN, cpv%SNCAN, &
                        cdv%ROF, cdv%TROF, cpv%SNO, cpv%TSNO, cpv%RHOS, cpv%ALBS, &
                        cpv%WSNO, cpv%ZPND, cpv%TPND, cpv%GRO, cpv%TBAS, cdv%GFLX, &
                        cdv%PCFC, cdv%PCLC, cdv%PCPN, cdv%PCPG, cdv%QFCF, cdv%QFCL, &
                        cdv%QFN, cdv%QFG, cdv%QFC, cdv%HMFC, cdv%HMFG, cdv%HMFN, &
                        cdv%HTCC, cdv%HTCS, cdv%HTC, cdv%ROFC, cdv%ROFN, cdv%ROVG, &
                        cdv%WTRS, cdv%WTRG, cdv%ROFO, cdv%ROFS, cdv%ROFB, &
                        cdv%TROO, cdv%TROS, cdv%TROB, cdv%QFS, cdv%ICE, cdv%TICE, &
                        TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, catv%RPCP, catv%TRPC, &
                        catv%SPCP, catv%TSPC, cfi%PRE, cfi%TA, catv%RHSI, catv%GGEO, &
                        cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, EVAPC, EVAPCG, EVAPG, EVAPCS, &
                        EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, FROOT, FSVF, &
                        FSVFS, CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO, &
                        TCANS, CHCAP, CHCAPS, CMASSC, CMASCS, ZSNOW, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, &
                        G12CS, G12GS, G23C, G23G, G23CS, G23GS, &
                        TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, cpv%TSFS, &
                        TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        csfv%THP, csfv%THR, csfv%THM, csfv%BI, csfv%PSIS, csfv%GRKS, &
                        csfv%THRA, csfv%THFC, csfv%DRN, csfv%HCPS, shd%lc%sl%DELZ, &
                        csfv%DELZW, csfv%ZBTW, csfv%XSLP, XDGAT, csfv%WFSF, KSGAT, &
                        cglv%FREZTH, cglv%SWELIM, cglv%SNDENLIM, &
                        csfv%ISND, csfv%IGDR, csfv%IWF, NML, il1, il2, ic%ts_count, &
                        JLAT, ICAN, IGND, IGND + 1, IGND + 2, &
                        NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI, &
                        MANNGAT, DDGAT, ic%ts_daily, &
                        t0_ACC(NMELT), SI, TSI, INFILTYPE, SNOWMELTD, SNOWMELTD_LAST, &
                        MELTRUNOFF, SNOWINFIL, CUMSNOWINFILCS, CUMSNOWINFILGS, &
                        SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, FRZCGAT, &
!FOR PDMROF
                        CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
                        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
                        UM1CS, UM1C, UM1G, UM1GS, &
                        QM1CS, QM1C, QM1G, QM1GS, &
                        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
                        FSTRCS, FSTRC, FSTRG, FSTRGS, &
                        pbsm%PROCESS_ACTIVE, &
                        ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
                        HCPSCS, HCPSGS, HCPSC, HCPSG, &
                        TSNOWC, TSNOWG, RHOSC, RHOSG, &
                        XSNOWC, XSNOWG, XSNOCS, XSNOGS)

            !> Single column blowing snow calculations (PBSM).
            call PBSM_within_tile( &
                ZSNOW, cpv%WSNO, cpv%SNO, cpv%RHOS, cpv%TSNO, cdv%HTCS, &
                TSNOCS, TSNOGS, &
                RHOSCS, RHOSGS,&
                WSNOCS, WSNOGS, &
                cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, &
                cdv%SFCT, cdv%SFCU, cdv%SFCQ, &
                catv%ZRFM, ZOMLCS, ZOMLNS, &
                NML, &
                fls, shd, cm)

            call CLASSZ(1, CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        cdv%FSGV, cdv%FLGV, cdv%HFSC, cdv%HEVC, cdv%HMFC, cdv%HTCC, &
                        cdv%FSGS, cdv%FLGS, cdv%HFSS, cdv%HEVS, cdv%HMFN, cdv%HTCS, &
                        cdv%FSGG, cdv%FLGG, cdv%HFSG, cdv%HEVG, cdv%HMFG, cdv%HTC, &
                        cdv%PCFC, cdv%PCLC, cdv%QFCF, cdv%QFCL, cdv%ROFC, cdv%WTRC, &
                        cdv%PCPN, cdv%QFN, cdv%ROFN, cdv%WTRS, cdv%PCPG, cdv%QFG, &
                        cdv%QFC, cdv%ROF, cdv%WTRG, cpv%CMAI, cpv%RCAN, cpv%SNCAN, &
                        cpv%TCAN, cpv%SNO, cpv%WSNO, cpv%TSNO, cpv%THLQ, cpv%THIC, &
                        csfv%HCPS, csfv%THP, csfv%DELZW, cpv%TBAR, cpv%ZPND, cpv%TPND, &
                        cdv%ICE, cdv%TICE, &
                        shd%lc%sl%DELZ, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, &
                        il1, il2, NML, IGND, ic%ts_count, &
                        pbsm%vs%Drift, pbsm%vs%Subl)

            !> Distribution of blowing snow mass between tiles (PBSM).
            call PBSM_within_grid( &
                cpv%TSNO, ZSNOW, &
                cpv%RHOS, cpv%SNO, TSNOCS, RHOSCS, TSNOGS, &
                RHOSGS, &
                catv%GC, csfv%FARE, &
                WSNOCS, WSNOGS, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, &
                cdv%TROO, cdv%ROFO, cdv%TROF, cdv%ROF, cdv%ROFN, cdv%PCPG, cdv%HTCS, cpv%WSNO, &
                NML, &
                fls, shd, cm)

            cdv%ROF = cdv%ROF - UMQ

        end if !(ipid /= 0 .or. izero == 0) then

        !> CLASS output files.
        if (WF_NUM_POINTS > 0) call CLASSOUT_update_files(shd)

        !> Copy internal variables back to MESH.
        if (associated(vs%tile%prern)) vs%tile%prern(il1:il2) = catv%RPCP(il1:il2)*RHOW !from [m s-1] to [kg ms-2 s-1].
        if (associated(vs%tile%presno)) vs%tile%presno(il1:il2) = catv%SPCP(il1:il2)*catv%RHSI(il1:il2) !from [m s-1] to [kg ms-2 s-1].
        vs%tile%lqwscan(il1:il2) = cpv%RCAN(il1:il2)
        vs%tile%fzwscan(il1:il2) = cpv%SNCAN(il1:il2)
        vs%tile%cmas(il1:il2) = cpv%CMAI(il1:il2)
        vs%tile%tacan(il1:il2) = cpv%TAC(il1:il2)
        vs%tile%tcan(il1:il2) = cpv%TCAN(il1:il2)
        vs%tile%qacan(il1:il2) = cpv%QAC(il1:il2)
        vs%tile%gro(il1:il2) = cpv%GRO(il1:il2)
        vs%tile%sno(il1:il2) = cpv%SNO(il1:il2)
        vs%tile%albsno(il1:il2) = cpv%ALBS(il1:il2)
        vs%tile%fsno(il1:il2) = cdv%FSNO(il1:il2)
        vs%tile%drainsno(il1:il2) = cdv%ROFN(il1:il2)
        vs%tile%rhosno(il1:il2) = cpv%RHOS(il1:il2)
        vs%tile%lqwssno(il1:il2) = cpv%WSNO(il1:il2)
        vs%tile%tsno(il1:il2) = cpv%TSNO(il1:il2)
        vs%tile%alvs(il1:il2) = cdv%ALVS(il1:il2)
        vs%tile%alir(il1:il2) = cdv%ALIR(il1:il2)
        vs%tile%gte(il1:il2) = cdv%GTE(il1:il2)
        vs%tile%zpnd(il1:il2) = cpv%ZPND(il1:il2)
        vs%tile%tpnd(il1:il2) = cpv%TPND(il1:il2)
        vs%tile%pndcaf(il1:il2) = &
            FSTRCS(il1:il2)*cdv%FCS(il1:il2) + FSTRC(il1:il2)*cdv%FC(il1:il2) + &
            FSTRG(il1:il2)*cdv%FG(il1:il2) + FSTRGS(il1:il2)*cdv%FGS(il1:il2)
        vs%tile%potevp(il1:il2) = cdv%PET(il1:il2)
        vs%tile%et(il1:il2) = cdv%QFS(il1:il2)
        vs%tile%ovrflw(il1:il2) = cdv%ROFO(il1:il2)
        vs%tile%qevp(il1:il2) = cdv%QEVP(il1:il2)
        vs%tile%qsens(il1:il2) = cdv%HFS(il1:il2)
        vs%tile%gzero(il1:il2) = cdv%GFLX(il1:il2, 1)
        vs%tile%tsfs(il1:il2, :) = cpv%TSFS(il1:il2, :)
        vs%tile%tsurf(il1:il2) = &
            cpv%TSFS(il1:il2, 1)*cdv%FCS(il1:il2) + cpv%TSFS(il1:il2, 2)*cdv%FGS(il1:il2) + &
            cpv%TSFS(il1:il2, 3)*cdv%FC(il1:il2) + cpv%TSFS(il1:il2, 4)*cdv%FG(il1:il2)
        vs%tile%tbas(il1:il2) = cpv%TBAS(il1:il2)
        vs%tile%latflw(il1:il2, :) = cdv%ROFS(il1:il2, :)
        vs%tile%thicsol(il1:il2, :) = cpv%THIC(il1:il2, :)
        vs%tile%thlqsol(il1:il2, :) = cpv%THLQ(il1:il2, :)
        vs%tile%tsol(il1:il2, :) = cpv%TBAR(il1:il2, :)
        vs%tile%gflx(il1:il2, :) = cdv%GFLX(il1:il2, :)
        vs%tile%drainsol(il1:il2) = cdv%ROFB(il1:il2)

    end subroutine

end module
