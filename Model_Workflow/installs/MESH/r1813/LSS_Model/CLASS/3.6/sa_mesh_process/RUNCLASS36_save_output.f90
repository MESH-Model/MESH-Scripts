module RUNCLASS36_save_output

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    implicit none

    !> GRID OUTPUT POINTS
    !* WF_NUM_POINTS: NUMBER OF GRID OUTPUTS
    integer WF_NUM_POINTS

    type OutputPoints

        !* N_OUT: GRID SQUARE TO OUTPUT
        !* II_OUT: GRU TO OUTPUT
        integer, dimension(:), allocatable :: N_OUT, II_OUT, K_OUT

        !* DIR_OUT: OUTPUT DIRECTORY (10 CHARACTER STRING)
        character*10, dimension(:), allocatable :: DIR_OUT

        integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2

    end type

    type CLASSOUT_VARS
        real, dimension(:), allocatable :: &
            PREACC, GTACC, QEVPACC, EVAPACC, HFSACC, HMFNACC, &
            ROFACC, ROFOACC, ROFSACC, ROFBACC, WTBLACC, ALVSACC, ALIRACC, &
            RHOSACC, TSNOACC, WSNOACC, TCANACC, SNOACC, &
            RCANACC, SCANACC, GROACC, FSINACC, FLINACC, FLUTACC, &
            TAACC, UVACC, PRESACC, QAACC
        real, dimension(:, :), allocatable :: &
            TBARACC, THLQACC, THICACC, THALACC, GFLXACC
        integer, dimension(:), allocatable :: ISNOACC, ICANACC, IALACC
    end type !CLASSOUT_VARS

    type(OutputPoints), save :: op
    type(CLASSOUT_VARS), save :: co

    contains

    subroutine CLASSOUT_open_files(shd)

        use mpi_module
        use sa_mesh_common

        type(ShedGridParams) :: shd

        integer NSL
        character(len = DEFAULT_LINE_LENGTH) line, fmt
        character(len = DEFAULT_FIELD_LENGTH) str
        integer k, j, i, ierr

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE .or. WF_NUM_POINTS == 0) return

        !> Summarize CLASS outputs locations.
        call print_message('Found these locations for CLASS output:')
        call increase_tab()
        write(line, FMT_GEN) 'Folder', 'Grid No.', 'GRU'
        call print_message(line)
        do i = 1, WF_NUM_POINTS
            write(line, FMT_GEN) op%DIR_OUT(i), op%N_OUT(i), op%II_OUT(i)
            call print_message(line)
        end do
        call decrease_tab()

        !> Configuration checks.
        do i = 1, WF_NUM_POINTS
            if (i < WF_NUM_POINTS) then

                !> Check for repeated points.
                do j = i + 1, WF_NUM_POINTS
                    if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
                        write(line, "('Grid ', i5, ', GRU ', i4)") op%N_OUT(i), op%II_OUT(i)
                        call print_error('Output is repeated for ' // trim(adjustl(line)))
                        call program_abort()
                    end if
                end do
            else

                !> Check that the output path exists.
                write(line, FMT_GEN) ipid
                open( &
                    100, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/tmp' // trim(adjustl(line)), status = 'unknown', &
                    iostat = ierr)
                if (ierr /= 0) then
                    write(line, FMT_GEN) i
                    call print_error('The output folder for point ' // trim(adjustl(line)) // ' does not exist.')
                    call increase_tab()
                    call print_message('Location: ' // trim(adjustl(op%DIR_OUT(i))))
                    call decrease_tab()
                    call program_abort()
                else
                    close(100, status = 'delete')
                end if
            end if

            !> Check that point lies inside the basin.
            if (op%N_OUT(i) > shd%NAA) then
                write(line, FMT_GEN) i
                call print_error('Output point ' // trim(adjustl(line)) // ' is outside the basin.')
                call increase_tab()
                write(line, FMT_GEN) shd%NAA
                call print_message('Number of grids inside the basin: ' // trim(adjustl(line)))
                call decrease_tab()
                call program_abort()
            end if
        end do

        !> Find the NML index that corresponds to the location.
        op%K_OUT = 0
        do k = il1, il2
            do i = 1, WF_NUM_POINTS
                if (op%N_OUT(i) == shd%lc%ILMOS(k) .and. op%II_OUT(i) == shd%lc%JLMOS(k)) op%K_OUT(i) = k
            end do
        end do

        !> Allocate and initialize the CLASS output variables.
        NSL = shd%lc%IGND
        allocate( &
            co%PREACC(WF_NUM_POINTS), co%GTACC(WF_NUM_POINTS), co%QEVPACC(WF_NUM_POINTS), co%EVAPACC(WF_NUM_POINTS), &
            co%HFSACC(WF_NUM_POINTS), co%HMFNACC(WF_NUM_POINTS), &
            co%ROFACC(WF_NUM_POINTS), co%ROFOACC(WF_NUM_POINTS), co%ROFSACC(WF_NUM_POINTS), co%ROFBACC(WF_NUM_POINTS), &
            co%WTBLACC(WF_NUM_POINTS), co%ALVSACC(WF_NUM_POINTS), co%ALIRACC(WF_NUM_POINTS), &
            co%RHOSACC(WF_NUM_POINTS), co%TSNOACC(WF_NUM_POINTS), co%WSNOACC(WF_NUM_POINTS), &
            co%TCANACC(WF_NUM_POINTS), co%SNOACC(WF_NUM_POINTS), &
            co%RCANACC(WF_NUM_POINTS), co%SCANACC(WF_NUM_POINTS), co%GROACC(WF_NUM_POINTS), co%FSINACC(WF_NUM_POINTS), &
            co%FLINACC(WF_NUM_POINTS), co%FLUTACC(WF_NUM_POINTS), &
            co%TAACC(WF_NUM_POINTS), co%UVACC(WF_NUM_POINTS), co%PRESACC(WF_NUM_POINTS), co%QAACC(WF_NUM_POINTS), &
            co%TBARACC(WF_NUM_POINTS, NSL), co%THLQACC(WF_NUM_POINTS, NSL), &
            co%THICACC(WF_NUM_POINTS, NSL), &
            co%THALACC(WF_NUM_POINTS, NSL), co%GFLXACC(WF_NUM_POINTS, NSL), &
            co%ISNOACC(WF_NUM_POINTS), co%ICANACC(WF_NUM_POINTS), co%IALACC(WF_NUM_POINTS))
        co%PREACC = 0.0; co%GTACC = 0.0; co%QEVPACC = 0.0; co%EVAPACC = 0.0
        co%HFSACC = 0.0; co%HMFNACC = 0.0
        co%ROFACC = 0.0; co%ROFOACC = 0.0; co%ROFSACC = 0.0; co%ROFBACC = 0.0
        co%WTBLACC = 0.0; co%ALVSACC = 0.0; co%ALIRACC = 0.0
        co%RHOSACC = 0.0; co%TSNOACC = 0.0; co%WSNOACC = 0.0
        co%TCANACC = 0.0; co%SNOACC = 0.0
        co%RCANACC = 0.0; co%SCANACC = 0.0; co%GROACC = 0.0; co%FSINACC = 0.0
        co%FLINACC = 0.0; co%FLUTACC = 0.0
        co%TAACC = 0.0; co%UVACC = 0.0; co%PRESACC = 0.0; co%QAACC = 0.0
        co%TBARACC = 0.0; co%THLQACC = 0.0
        co%THICACC = 0.0
        co%THALACC = 0.0; co%GFLXACC = 0.0
        co%ISNOACC = 0; co%ICANACC = 0; co%IALACC = 0

        !> Open the files if the GAT-index of the output point resides on this node.
        do i = 1, WF_NUM_POINTS
            if ((ipid /= 0 .or. izero == 0) .and. (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2)) then

                !> Open the files in the appropriate directory.
                j = 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF1.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF2.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF3.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF4.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF5.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF6.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF7.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF8.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/CLASSOF9.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/GRU_water_balance.csv')

                !> Write project header information.
                do j = 1, 9
                    write(150 + i*10 + j, "('CLASS TEST RUN:     ', 6a4)") TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
                    write(150 + i*10 + j, "('RESEARCHER:         ', 6a4)") NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
                    write(150 + i*10 + j, "('INSTITUTION:        ', 6a4)") PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
                end do

                !> CLASSOF1.
                write(150 + i*10 + 1, "('IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,BEG," // &
                    'GTOUT,SNOACC(I),RHOSACC(I),WSNOACC(I),ALTOT,ROFACC(I),' // &
                    "ROFOACC(I),ROFSACC(I),ROFBACC(I)')")

                !> CLASSOF2.
                fmt = ''
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // 'TBARACC(I ' // trim(str) // ')-TFREZ,THLQACC(I ' // &
                        trim(str) // '),THICACC(I ' // trim(str) // '),'
                end do
                write(150 + i*10 + 2, "('IDAY,IYEAR," // trim(fmt) // "TCN,RCANACC(I),SCANACC(I),TSN,ZSN')")

                !> CLASSOF3.
                write(150 + i*10 + 3, "('IDAY,IYEAR,FSINACC(I),FLINACC(I)," // &
                    'TAACC(I)-TFREZ,UVACC(I),PRESACC(I),QAACC(I),PREACC(I),' // &
                    "EVAPACC(I)')")

                !> CLASSOF4.
                write(150 + i*10 + 4, "('IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE," // &
                    'SNOMLT,BEG,GTOUT,SNOROW(I M),RHOSROW(I M),WSNOROW(I M),ALTOT,' // &
                    "ROFROW(I M),TPN,ZPNDROW(I M),ZPND,FSTR')")

                !> CLASSOF5.
                fmt = ''
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // 'TBARROW(I ' // trim(str) // ')-TFREZ,THLQROW(I ' // &
                        trim(str) // '),THICROW(I ' // trim(str) // '),'
                end do
                write(150 + i*10 + 5, "('IHOUR,IMIN,IDAY,IYEAR," // trim(fmt) // "TCN,RCANROW(I M),SCANROW(I M),TSN,ZSN')")

                !> CLASSOF6.
                write(150 + i*10 + 6, "('IHOUR,IMIN,IDAY,FSDOWN(I),FDLGRD(I)," // &
                    "PREGRD(I),TAGRD(I)-TFREZ,UVGRD(I),PRESGRD(I),QAGRD(I)')")

                !> CLASSOF7.
                write(150 + i*10 + 7,"('TROFROW(I M),TROOROW(I M),TROSROW(I M)," // &
                    'TROBROW(I M),ROFROW(I M),ROFOROW(I M),ROFSROW(I M),' // &
                    "ROFBROW(I M),FCS(I),FGS(I),FC(I),FG(I)')")

                !> CLASSOF8.
                fmt = ''
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // ',HMFGROW(I M ' // trim(str) // ')'
                end do
                fmt = trim(fmt) // ',HTCCROW(I M),HTCSROW(I M)'
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // ',HTCROW(I M ' // trim(str) // ')'
                end do
                write(150 + i*10 + 8, "('FSGVROW(I M),FSGSROW(I M),FSGGROW(I M)," // &
                    'FLGVROW(I M),FLGSROW(I M),FLGGROW(I M),HFSCROW(I M),' // &
                    'HFSSROW(I M),HFSGROW(I M),HEVCROW(I M),HEVSROW(I M),' // &
                    'HEVGROW(I M),HMFCROW(I M),HMFNROW(I M)' // trim(fmt) // "')")

                !> CLASSOF9.
                fmt = ''
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // 'QFCROW(I M ' // trim(str) // '),'
                end do
                write(150 + i*10 + 9, "('PCFCROW(I M),PCLCROW(I M),PCPNROW(I M)," // &
                    'PCPGROW(I M),QFCFROW(I M),QFCLROW(I M),QFNROW(I M),QFGROW(I M),' // trim(fmt) // 'ROFCROW(I M),' // &
                    'ROFNROW(I M),ROFOROW(I M),ROFROW(I M),WTRCROW(I M),' // &
                    "WTRSROW(I M),WTRGROW(I M)')")

                !> GRU water balance file.
                fmt = ''
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // 'THLQ' // trim(str) // ','
                end do
                do j = 1, NSL
                    write(str, *) j
                    str = adjustl(str)
                    fmt = trim(fmt) // 'THIC' // trim(str) // ','
                end do
                write(150 + i*10 + 10, "('IHOUR,IMIN,IDAY,IYEAR," // &
                    'PRE,EVAP,ROF,ROFO,ROFS,ROFB,' // &
                    'SCAN,RCAN,SNO,WSNO,ZPND,' // trim(fmt) // "')")
            end if
        end do

    end subroutine

    subroutine CLASSOUT_update_files(shd)

        use mpi_module
        use sa_mesh_common
        use model_dates

        type(ShedGridParams), intent(in) :: shd

        !* I_OUT: OUTPUT GRID SQUARE TEMPORARY STORE
        integer DELT, NSL, I_OUT
        real ALTOT, FSSTAR, FLSTAR, QH, QE, BEG, SNOMLT, ZSN, TCN, TSN, TPN, GTOUT
        real ZPND, FSTR
        character(len = DEFAULT_FIELD_LENGTH) str
        integer NSUM, k, j, i

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE .or. WF_NUM_POINTS == 0) return

        !> Constant variables.
        DELT = ic%dts
        NSL = shd%lc%IGND
        write(str, *) NSL

        !> Write to CLASSOF* output files.
        do i = 1, WF_NUM_POINTS
            if ((ipid /= 0 .or. izero == 0) .and. (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2)) then

                !> Grab the identity of the tile.
                k = op%K_OUT(i)

                !> Update output variables.
                if ((cfi%FSVH(k) + cfi%FSIH(k)) > 0.0) then
                    ALTOT = (cdv%ALVS(k) + cdv%ALIR(k))/2.0
                else
                    ALTOT = 0.0
                end if
                FSSTAR = (cfi%FSVH(k) + cfi%FSIH(k))*(1.0 - ALTOT)
                FLSTAR = cfi%FDL(k) - SBC*cdv%GTE(k)**4
                QH = cdv%HFS(k)
                QE = cdv%QEVP(k)
                BEG = FSSTAR + FLSTAR - QH - QE
                SNOMLT = cdv%HMFN(k)
                if (cpv%RHOS(k) > 0.0) then
                    ZSN = cpv%SNO(k)/cpv%RHOS(k)
                else
                    ZSN = 0.0
                end if
                if (cpv%TCAN(k) > 0.01) then
                    TCN = cpv%TCAN(k) - TFREZ
                else
                    TCN = 0.0
                end if
                if (cpv%TSNO(k) > 0.01) then
                    TSN = cpv%TSNO(k) - TFREZ
                else
                    TSN = 0.0
                end if
                if (cpv%TPND(k) > 0.01) then
                    TPN = cpv%TPND(k) - TFREZ
                else
                    TPN = 0.0
                end if
                if (shd%wc%ILG == 1) then
                    GTOUT = cdv%GTE(k) - TFREZ
                else
                    GTOUT = 0.0
                end if
                ZPND = ZPNDPRECS(k)*cdv%FCS(k) + ZPONDPREC(k)*cdv%FC(k) + ZPONDPREG(k)*cdv%FG(k) + ZPNDPREGS(k)*cdv%FGS(k)
                FSTR = FSTRCS(k)*cdv%FCS(k) + FSTRC(k)*cdv%FC(k) + FSTRG(k)*cdv%FG(k) + FSTRGS(k)*cdv%FGS(k)

                !> Write to the CLASSOF* output files for sub-hourly output.
                if (((op%KOUT1 == 0 .and. op%JOUT1 == 0) .or. &
                    ((ic%now%year == op%KOUT1 .and. ic%now%jday >= op%JOUT1) .or. (ic%now%year > op%KOUT1))) .and. &
                    ((op%KOUT2 == 0 .and. op%JOUT2 == 0) .or. &
                    ((ic%now%year == op%KOUT2 .and. ic%now%jday < op%JOUT2) .or. (ic%now%year < op%KOUT2))) .and. &
                    .not. (op%KOUT1 == -1 .or. op%JOUT1 == -1 .or. op%KOUT2 == -1 .or. op%JOUT2 == -1)) then
                    write(150 + i*10 + 4, &
                          "(i2,',', i3,',', i5,',', i6,',', 9(f8.2,','), 2(f7.3,','), e11.3,',', f8.2,',', 3(f12.4,','))") &
                        ic%now%hour, ic%now%mins, ic%now%jday, ic%now%year, FSSTAR, FLSTAR, QH, &
                        QE, SNOMLT, BEG, GTOUT, cpv%SNO(k), &
                        cpv%RHOS(k), cpv%WSNO(k), ALTOT, cdv%ROF(k)*DELT, &
                        TPN, cpv%ZPND(k), ZPND, FSTR
                    write(150 + i*10 + 5, "(i2,',', i3,',', i5,',', i6,',', " // trim(adjustl(str)) // &
                          "(f7.2,',', 2(f6.3,',')), f8.2,',', 2(f8.4,','), f8.2,',', f8.3,',')") &
                        ic%now%hour, ic%now%mins, ic%now%jday, ic%now%year, &
                        (cpv%TBAR(k, j) - TFREZ, cpv%THLQ(k, j), &
                        cpv%THIC(k, j), j = 1, NSL), TCN, &
                        cpv%RCAN(k), cpv%SNCAN(k), TSN, ZSN
                    write(150 + i*10 + 6, &
                          "(i2,',', i3,',', i5,',', 2(f10.2,','), f12.6,',', f10.2,',', f8.2,',', f10.2,',', f15.9,',')") &
                        ic%now%hour, ic%now%mins, ic%now%jday, cfi%FSVH(k) + cfi%FSIH(k), cfi%FDL(k), &
                        cfi%PRE(k), cfi%TA(k) - TFREZ, cfi%VMOD(k), cfi%PRES(k), &
                        cfi%QA(k)
                    write(150 + i*10 + 7, "(999(e11.4,','))") &
                        cdv%TROF(k), cdv%TROO(k), sum(cdv%TROS(k, :))/max(1, count(cdv%TROS(k, :) > 0.0)), &
                        cdv%TROB(k), cdv%ROF(k), cdv%ROFO(k), &
                        sum(cdv%ROFS(k, :)), cdv%ROFB(k), &
                        cdv%FCS(k), cdv%FGS(k), cdv%FC(k), cdv%FG(k)
                    write(150 + i*10 + 8, "(999(f12.4,','))") &
                        cdv%FSGV(k), cdv%FSGS(k), cdv%FSGG(k), &
                        cdv%FLGV(k), cdv%FLGS(k), cdv%FLGG(k), &
                        cdv%HFSC(k), cdv%HFSS(k), cdv%HFSG(k), &
                        cdv%HEVC(k), cdv%HEVS(k), cdv%HEVG(k), &
                        cdv%HMFC(k), cdv%HMFN(k), &
                        (cdv%HMFG(k, j), j = 1, NSL), &
                        cdv%HTCC(k), cdv%HTCS(k), &
                        (cdv%HTC(k, j), j = 1, NSL)
                    write(150 + i*10 + 9, "(999(e12.4,','))") &
                        cdv%PCFC(k), cdv%PCLC(k), cdv%PCPN(k), &
                        cdv%PCPG(k), cdv%QFCF(k), cdv%QFCL(k), &
                        cdv%QFN(k), cdv%QFG(k), (cdv%QFC(k, j), j = 1, NSL), &
                        cdv%ROFC(k), cdv%ROFN(k), &
                        cdv%ROFO(k), cdv%ROF(k), cdv%WTRC(k), &
                        cdv%WTRS(k), cdv%WTRG(k)
                    write(150 + i*10 + 10, "(i2,',', i3,',', i5,',', i6,',', 999(f14.6,','))") &
                        ic%now%hour, ic%now%mins, ic%now%jday, ic%now%year, cfi%PRE(k)*DELT, cdv%QFS(k)*DELT, &
                        cdv%ROF(k)*DELT, cdv%ROFO(k)*DELT, sum(cdv%ROFS(k, :))*DELT, cdv%ROFB(k)*DELT, &
                        cpv%SNCAN(k), cpv%RCAN(k), cpv%SNO(k), cpv%WSNO(k), &
                        cpv%ZPND(k)*RHOW, (cpv%THLQ(k, j)*RHOW*csfv%DELZW(k, j), j = 1, NSL), &
                        (cpv%THIC(k, j)*RHOICE*csfv%DELZW(k, j), j = 1, NSL)
                end if

                !> Accumulate variables for daily output.
                co%PREACC(i) = co%PREACC(i) + cfi%PRE(k)*DELT
                co%GTACC(i) = co%GTACC(i) + cdv%GTE(k)
                co%QEVPACC(i) = co%QEVPACC(i) + cdv%QEVP(k)
                co%EVAPACC(i) = co%EVAPACC(i) + cdv%QFS(k)*DELT
                co%HFSACC(i) = co%HFSACC(i) + cdv%HFS(k)
                co%HMFNACC(i) = co%HMFNACC(i) + cdv%HMFN(k)
                co%ROFACC(i) = co%ROFACC(i) + cdv%ROF(k)*DELT
                co%ROFOACC(i) = co%ROFOACC(i) + cdv%ROFO(k)*DELT
                co%ROFSACC(i) = co%ROFSACC(i) + sum(cdv%ROFS(k, :))*DELT
                co%ROFBACC(i) = co%ROFBACC(i) + cdv%ROFB(k)*DELT
                co%WTBLACC(i) = co%WTBLACC(i) + cdv%WTAB(k)
                do j = 1, NSL
                    co%TBARACC(i, j) = co%TBARACC(i, j) + cpv%TBAR(k, j)
                    co%THLQACC(i, j) = co%THLQACC(i, j) + cpv%THLQ(k, j)
                    co%THICACC(i, j) = co%THICACC(i, j) + cpv%THIC(k, j)
                    co%THALACC(i, j) = co%THALACC(i, j) + cpv%THLQ(k, j) + cpv%THIC(k, j)
                    co%GFLXACC(i, j) = co%GFLXACC(i, j) + cdv%GFLX(k, j)
                end do
                if ((cfi%FSVH(k) + cfi%FSIH(k)) > 0.0) then
                    co%ALVSACC(i) = co%ALVSACC(i) + cdv%ALVS(k)
                    co%ALIRACC(i) = co%ALIRACC(i) + cdv%ALIR(k)
                    co%IALACC(i) = co%IALACC(i) + 1
                end if
                if (cpv%SNO(k) > 0.0) then
                    co%RHOSACC(i) = co%RHOSACC(i) + cpv%RHOS(k)
                    co%TSNOACC(i) = co%TSNOACC(i) + cpv%TSNO(k)
                    co%WSNOACC(i) = co%WSNOACC(i) + cpv%WSNO(k)
                    co%ISNOACC(i) = co%ISNOACC(i) + 1
                end if
                if (cpv%TCAN(k) > 0.5) then
                    co%TCANACC(i) = co%TCANACC(i) + cpv%TCAN(k)
                    co%ICANACC(i) = co%ICANACC(i) + 1
                end if
                co%SNOACC(i) = co%SNOACC(i) + cpv%SNO(k)
                co%RCANACC(i) = co%RCANACC(i) + cpv%RCAN(k)
                co%SCANACC(i) = co%SCANACC(i) + cpv%SNCAN(k)
                co%GROACC(i) = co%GROACC(i) + cpv%GRO(k)
                co%FSINACC(i) = co%FSINACC(i) + cfi%FSVH(k) + cfi%FSIH(k)
                co%FLINACC(i) = co%FLINACC(i) + cfi%FDL(k)
                co%FLUTACC(i) = co%FLUTACC(i) + SBC*cdv%GTE(k)**4
                co%TAACC(i) = co%TAACC(i) + cfi%TA(k)
                co%UVACC(i) = co%UVACC(i) + cfi%VMOD(k)
                co%PRESACC(i) = co%PRESACC(i) + cfi%PRES(k)
                co%QAACC(i) = co%QAACC(i) + cfi%QA(k)

                !> Write daily output if in the last time-step of the day.
                if (ic%now%day /= ic%next%day) then

                    !> Calculate the average of variables that should not accumulate.
                    NSUM = ic%ts_daily
                    co%GTACC(i) = co%GTACC(i)/real(NSUM)
                    co%QEVPACC(i) = co%QEVPACC(i)/real(NSUM)
                    co%HFSACC(i) = co%HFSACC(i)/real(NSUM)
                    co%HMFNACC(i) = co%HMFNACC(i)/real(NSUM)
                    co%WTBLACC(i) = co%WTBLACC(i)/real(NSUM)
                    co%TBARACC(i, :) = co%TBARACC(i, :)/real(NSUM)
                    co%THLQACC(i, :) = co%THLQACC(i, :)/real(NSUM)
                    co%THICACC(i, :) = co%THICACC(i, :)/real(NSUM)
                    co%THALACC(i, :) = co%THALACC(i, :)/real(NSUM)
                    if (co%IALACC(i) > 0) then
                        co%ALVSACC(i) = co%ALVSACC(i)/real(co%IALACC(i))
                        co%ALIRACC(i) = co%ALIRACC(i)/real(co%IALACC(i))
                    end if
                    co%SNOACC(i) = co%SNOACC(i)/real(NSUM)
                    if (co%ISNOACC(i) > 0) then
                        co%RHOSACC(i) = co%RHOSACC(i)/real(co%ISNOACC(i))
                        co%TSNOACC(i) = co%TSNOACC(i)/real(co%ISNOACC(i))
                        co%WSNOACC(i) = co%WSNOACC(i)/real(co%ISNOACC(i))
                    end if
                    if (co%ICANACC(i) > 0) then
                        co%TCANACC(i) = co%TCANACC(i)/real(co%ICANACC(i))
                    end if
                    co%RCANACC(i) = co%RCANACC(i)/real(NSUM)
                    co%SCANACC(i) = co%SCANACC(i)/real(NSUM)
                    co%GROACC(i) = co%GROACC(i)/real(NSUM)
                    co%FSINACC(i) = co%FSINACC(i)/real(NSUM)
                    co%FLINACC(i) = co%FLINACC(i)/real(NSUM)
                    co%FLUTACC(i) = co%FLUTACC(i)/real(NSUM)
                    co%TAACC(i) = co%TAACC(i)/real(NSUM)
                    co%UVACC(i) = co%UVACC(i)/real(NSUM)
                    co%PRESACC(i) = co%PRESACC(i)/real(NSUM)
                    co%QAACC(i) = co%QAACC(i)/real(NSUM)
                    ALTOT = (co%ALVSACC(i) + co%ALIRACC(i))/2.0
                    FSSTAR = co%FSINACC(i)*(1.0 - ALTOT)
                    FLSTAR = co%FLINACC(i) - co%FLUTACC(i)
                    QH = co%HFSACC(i)
                    QE = co%QEVPACC(i)
                    BEG = FSSTAR + FLSTAR - QH - QE
                    SNOMLT = co%HMFNACC(i)
                    if (co%RHOSACC(i) > 0.0) then
                        ZSN = co%SNOACC(i)/co%RHOSACC(i)
                    else
                        ZSN = 0.0
                    end if
                    if (co%TCANACC(i) > 0.01) then
                        TCN = co%TCANACC(i) - TFREZ
                    else
                        TCN = 0.0
                    end if
                    if (co%TSNOACC(i) > 0.01) then
                        TSN = co%TSNOACC(i) - TFREZ
                    else
                        TSN = 0.0
                    end if
                    if (shd%wc%ILG == 1) then
                        GTOUT = co%GTACC(i) - TFREZ
                    else
                        GTOUT = 0.0
                    end if

                    !> Variables that accumulate.
!                    co%PREACC(i) = co%PREACC(i)
!                    co%EVAPACC(i) = co%EVAPACC(i)
!                    co%ROFACC(i) = co%ROFACC(i)
!                    co%ROFOACC(i) = co%ROFOACC(i)
!                    co%ROFSACC(i) = co%ROFSACC(i)
!                    co%ROFBACC(i) = co%ROFBACC(i)

                    !> Write to the CLASSOF* output files for daily accumulated output.
                    if (((op%KAV1 == 0 .and. op%JAV1 == 0) .or. &
                        ((ic%now%year == op%KAV1 .and. ic%now%jday >= op%JAV1) .or. (ic%now%year > op%KAV1))) .and. &
                        ((op%KAV2 == 0 .and. op%JAV2 == 0) .or. &
                        ((ic%now%year == op%KAV2 .and. ic%now%jday < op%JAV2) .or. (ic%now%year < op%KAV2))) .and. &
                        .not. (op%KAV1 == -1 .or. op%JAV1 == -1 .or. op%KAV2 == -1 .or. op%JAV2 == -1)) then
                        write(150 + i*10 + 1, "(i4,',', i5,',', 9(f8.2,','), 2(f8.3,','), 999(f12.4,','))") &
                            ic%now%jday, ic%now%year, FSSTAR, FLSTAR, QH, QE, SNOMLT, &
                            BEG, GTOUT, co%SNOACC(i), co%RHOSACC(i), &
                            co%WSNOACC(i), ALTOT, co%ROFACC(i), co%ROFOACC(i), &
                            co%ROFSACC(i), co%ROFBACC(i)
                        write(150 + i*10 + 2, "(i4,',', i5,',', " // adjustl(str) // "((f8.2,','), " // &
                              "2(f6.3,',')), f8.2,',', 2(f7.4,','), 2(f8.2,','))") &
                            ic%now%jday, ic%now%year, (co%TBARACC(i, j) - TFREZ, &
                            co%THLQACC(i, j), co%THICACC(i, j), j = 1, NSL), &
                            TCN, co%RCANACC(i), co%SCANACC(i), TSN, ZSN
                        write(150 + i*10 + 3, "(i4,',', i5,',', 3(f9.2,','), f8.2,',', " // &
                              "f10.2,',', e12.3,',', 2(f12.3,','))") &
                            ic%now%jday, ic%now%year, co%FSINACC(i), co%FLINACC(i), &
                            co%TAACC(i) - TFREZ, co%UVACC(i), co%PRESACC(i), &
                            co%QAACC(i), co%PREACC(i), co%EVAPACC(i)
                    end if

                    !> Reset the CLASS output variables.
                    co%PREACC(i) = 0.0; co%GTACC(i) = 0.0; co%QEVPACC(i) = 0.0; co%EVAPACC(i) = 0.0
                    co%HFSACC(i) = 0.0; co%HMFNACC(i) = 0.0
                    co%ROFACC(i) = 0.0; co%ROFOACC(i) = 0.0; co%ROFSACC(i) = 0.0; co%ROFBACC(i) = 0.0
                    co%WTBLACC(i) = 0.0; co%ALVSACC(i) = 0.0; co%ALIRACC(i) = 0.0
                    co%RHOSACC(i) = 0.0; co%TSNOACC(i) = 0.0; co%WSNOACC(i) = 0.0
                    co%TCANACC(i) = 0.0; co%SNOACC(i) = 0.0
                    co%RCANACC(i) = 0.0; co%SCANACC(i) = 0.0; co%GROACC(i) = 0.0; co%FSINACC(i) = 0.0
                    co%FLINACC(i) = 0.0; co%FLUTACC(i) = 0.0
                    co%TAACC(i) = 0.0; co%UVACC(i) = 0.0; co%PRESACC(i) = 0.0; co%QAACC(i) = 0.0
                    co%TBARACC(i, :) = 0.0; co%THLQACC(i, :) = 0.0
                    co%THICACC(i, :) = 0.0
                    co%THALACC(i, :) = 0.0; co%GFLXACC(i, :) = 0.0
                    co%ISNOACC(i) = 0; co%ICANACC(i) = 0; co%IALACC(i) = 0
                end if
            end if
        end do

    end subroutine

end module
