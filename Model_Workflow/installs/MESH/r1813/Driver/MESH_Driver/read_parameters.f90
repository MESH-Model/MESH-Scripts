!>
!> Description:
!>  Subroutine to read input parameters from file. Parameters shared
!>  by SA_MESH are accessible by 'sa_mesh_variables'. Other
!>  parameters are accessible by their respecitve process module(s).
!>
subroutine read_parameters(fls, shd, cm, ierr)

    use strings
    use mpi_module
    use model_files_variables
    use sa_mesh_common
    use FLAGS
    use climate_forcing_variabletypes

    use RUNCLASS36_variables
    use runsvs_mesh
    use irrigation_module
    use WF_ROUTE_config
    use rte_module
    use baseflow_module
    use cropland_irrigation_variables
    use PBSM_module

    implicit none

    !> Input variables.
    type(fl_ids):: fls
    type(ShedGridParams) :: shd
    type(CLIM_INFO) :: cm

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables for parsing INPUTPARAMSFORM.
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args
    integer nargs

    !> Local variables.
    integer NA, NAA, NTYPE, NRVR, NML, NSL, k, j, ignd, i, n, z

    !> Initialize the return status.
    ierr = 0

    !> Reset spacing for screen output.
    call reset_tab()

    !> Assign commonly used indices to local variables.
    NA = shd%NA
    NAA = shd%NAA
    NTYPE = shd%lc%NTYPE
    NML = shd%lc%NML
    NSL = shd%lc%IGND
    NRVR = shd%NRVR

    !>
    !> ALLOCATE AND INITIALIZE VARIABLES.
    !>

    !> Allocate and initialize SA_MESH parameters.
    call input_parameters_init(shd, ierr)
    if (ierr /= 0) goto 97

    !> RUNCLASS36 (interflow flag).
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
        pm%gru%iwf = RUNCLASS36_flgs%INTERFLOWFLAG
        if (.not. allocated(hp%CMAXROW)) then
            allocate( &
                hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE), hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE), hp%K2ROW(NA, NTYPE), &
                stat = ierr)
            if (ierr /= 0) goto 97
        end if
        hp%CMAXROW = 0.0; hp%CMINROW = 0.0; hp%BROW = 0.0; hp%K1ROW = 0.0; hp%K2ROW = 0.0
    end if

    !> Irrigation module.
    call irrigation_parameters_allocate(irrm%pm, NML, ierr)
    call irrigation_parameters_allocate(irrm%pm_grid, NA, ierr)
    call irrigation_parameters_allocate(irrm%pm_gru, NTYPE, ierr)

    !> WF_ROUTE (Watflood, 1988).
    if (WF_RTE_flgs%PROCESS_ACTIVE) then
        if (.not. allocated(wfp%r1)) then
            allocate(wfp%r1(NRVR), wfp%r2(NRVR), wfp%aa1(NRVR), wfp%aa2(NRVR), wfp%aa3(NRVR), wfp%aa4(NRVR), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        wfp%r1 = 2.0; wfp%r2 = 0.0; wfp%aa1 = 1.0; wfp%aa2 = 11.0; wfp%aa3 = 0.43; wfp%aa4 = 1.0
    end if

    !> RPN RTE (Watflood, 2007).
    if (rteflg%PROCESS_ACTIVE) then
        if (.not. allocated(rtepm%r1n)) then
            allocate(rtepm%r1n(NA), rtepm%r2n(NA), rtepm%mndr(NA), rtepm%widep(NA), &
                     rtepm%aa2(NA), rtepm%aa3(NA), rtepm%aa4(NA), &
                     rtepm_iak%r1n(NRVR), rtepm_iak%r2n(NRVR), rtepm_iak%mndr(NRVR), rtepm_iak%widep(NRVR), &
                     rtepm_iak%aa2(NRVR), rtepm_iak%aa3(NRVR), rtepm_iak%aa4(NRVR), &
                     stat = ierr)
            if (ierr /= 0) goto 97
        end if
        rtepm%r1n = 0.0; rtepm%r2n = 0.0; rtepm%mndr = 1.0; rtepm%widep = 10.0
        rtepm%aa2 = 1.1; rtepm%aa3 = 0.043; rtepm%aa4 = 1.0
        rtepm_iak%r1n = 0.0; rtepm_iak%r2n = 0.0; rtepm_iak%mndr = 0.0; rtepm_iak%widep = 0.0
        rtepm_iak%aa2 = 0.0; rtepm_iak%aa3 = 0.0; rtepm_iak%aa4 = 0.0
    end if

    !> PBSM (blowing snow).
    if (pbsm%PROCESS_ACTIVE) then
        if (.not. allocated(pbsm%pm_gru%fetch)) then
            allocate( &
                pbsm%pm_gru%fetch(NTYPE), pbsm%pm_gru%Ht(NTYPE), pbsm%pm_gru%N_S(NTYPE), pbsm%pm_gru%A_S(NTYPE), &
                pbsm%pm_gru%Distrib(NTYPE), &
                pbsm%pm_grid%fetch(NA), pbsm%pm_grid%Ht(NA), pbsm%pm_grid%N_S(NA), pbsm%pm_grid%A_S(NA), pbsm%pm_grid%Distrib(NA), &
                pbsm%pm%fetch(NML), pbsm%pm%Ht(NML), pbsm%pm%N_S(NML), pbsm%pm%A_S(NML), pbsm%pm%Distrib(NML), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        pbsm%pm_gru%fetch = 0.0; pbsm%pm_gru%Ht = 0.0; pbsm%pm_gru%N_S = 0.0; pbsm%pm_gru%A_S = 0.0
        pbsm%pm_gru%Distrib = 0.0
        pbsm%pm_grid%fetch = 0.0; pbsm%pm_grid%Ht = 0.0; pbsm%pm_grid%N_S = 0.0; pbsm%pm_grid%A_S = 0.0; pbsm%pm_grid%Distrib = 0.0
        pbsm%pm%fetch = 0.0; pbsm%pm%Ht = 0.0; pbsm%pm%N_S = 0.0; pbsm%pm%A_S = 0.0; pbsm%pm%Distrib = 0.0
    end if

    !> FROZENSOILINFILFLAG 1.
    if (FROZENSOILINFILFLAG == 1) then
        if (.not. allocated(hp%FRZCROW)) then
            allocate(hp%FRZCROW(NA, NTYPE), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        hp%FRZCROW = 0.0
        NYEARS = max(ic%stop%year - ic%start%year + 1, 1)
        if (.not. allocated(t0_ACC)) allocate(t0_ACC(NYEARS))
        t0_ACC = 0.0
    end if

    !> BASEFLOWFLAG 1 (Luo, 2012).
    if (bflm%BASEFLOWFLAG == 1) then
        if (.not. allocated(bflm%pm%dgw)) then
            allocate(bflm%pm%dgw(NML), bflm%pm%agw(NML), bflm%pm_gru%dgw(NTYPE), bflm%pm_gru%agw(NTYPE), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        bflm%pm%dgw = 0.0; bflm%pm%agw = 0.0; bflm%pm_gru%dgw = 0.0; bflm%pm_gru%agw = 0.0
    end if

    !> BASEFLOWFLAG == 2 (lower zone storage).
    if (bflm%BASEFLOWFLAG == 2) then
        if (.not. allocated(bflm%pm%pwr)) then
            allocate(bflm%pm%pwr(NML), bflm%pm%flz(NML), &
                     bflm%pm_iak%pwr(NRVR), bflm%pm_iak%flz(NRVR), bflm%pm_gru%pwr(NTYPE), bflm%pm_gru%flz(NTYPE), &
                     bflm%pm_grid%pwr(NA), bflm%pm_grid%flz(NA), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        bflm%pm%pwr = 0.0; bflm%pm%flz = 0.0
        bflm%pm_iak%pwr = 0.0; bflm%pm_iak%flz = 0.0; bflm%pm_gru%pwr = 0.0; bflm%pm_gru%flz = 0.0
        bflm%pm_grid%pwr = 0.0; bflm%pm_grid%flz = 0.0
    end if

    !> Cropland irrigation module (CROPLANDIRRIGATION > 0).
    if (cifg%PROCESS_ACTIVE) then
        if (.not. allocated(ciprot%jdsow)) then
            allocate( &
                ciprot%jdsow(NTYPE), ciprot%ldini(NTYPE), ciprot%lddev(NTYPE), ciprot%ldmid(NTYPE), ciprot%ldlate(NTYPE), &
                ciprot%Kcini(NTYPE), ciprot%Kcdev(NTYPE), ciprot%Kcmid(NTYPE), ciprot%Kclate(NTYPE), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        ciprot%jdsow = 0; ciprot%ldini = 0; ciprot%lddev = 0; ciprot%ldmid = 0; ciprot%ldlate = 0
        ciprot%Kcini = 0.0; ciprot%Kcdev = 0.0; ciprot%Kcmid = 0.0; ciprot%Kclate = 0.0
        if (.not. allocated(cip%jdsow)) then
            allocate( &
                cip%jdsow(NML), cip%ldini(NML), cip%lddev(NML), cip%ldmid(NML), cip%ldlate(NML), &
                cip%Kcini(NML), cip%Kcdev(NML), cip%Kcmid(NML), cip%Kclate(NML), stat = ierr)
            if (ierr /= 0) goto 97
        end if
        cip%jdsow = 0; cip%ldini = 0; cip%lddev = 0; cip%ldmid = 0; cip%ldlate = 0
        cip%Kcini = 0.0; cip%Kcdev = 0.0; cip%Kcmid = 0.0; cip%Kclate = 0.0
    end if

    !>
    !> READ FROM FILE.
    !>

    !> Parse the INPUTPARAMSFORM control flag to get INPUTPARAMSFORMFLAG.
    !> Default behaviour is to read the 'ini' format files.
    INPUTPARAMSFORMFLAG = radix(INPUTPARAMSFORMFLAG)**0
    call parse(INPUTPARAMSFORM, ' ', args, nargs)
    do n = 2, nargs
        select case (args(n))
            case ('only')
                INPUTPARAMSFORMFLAG = 0
            case ('none')
                INPUTPARAMSFORMFLAG = 0
                exit
            case ('r2c')
                INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(INPUTPARAMSFORMFLAG)**1
            case ('csv')
                INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(INPUTPARAMSFORMFLAG)**2
            case ('txt')
                INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(INPUTPARAMSFORMFLAG)**3
            case ('netcdf', 'nc')
                INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(INPUTPARAMSFORMFLAG)**4
        end select
    end do

    !> Check for a bad value of INPUTPARAMSFORMFLAG (unless all modules are disabled).
    if (INPUTPARAMSFORMFLAG == 0) then
        if (.not. ro%RUNLSS .and. .not. ro%RUNCHNL) then
            call print_remark('No parameter files specified.')
            return
        else
            ierr = 1
            call print_error( &
                'Unrecognized parameter file format. Revise INPUTPARAMSFORMFLAG in ' // trim(fls%fl(mfk%f53)%fn) // '.')
            return
        end if
    end if

    !> Read from the 'ini' files.
    if (btest(INPUTPARAMSFORMFLAG, 0)) then
        z = 0
        if (ro%RUNLSS) then
            call READ_PARAMETERS_CLASS(shd, fls, cm, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNLSS .or. ro%RUNCHNL) then
            call READ_PARAMETERS_HYDROLOGY(shd, fls, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNLSS) then
            call READ_SOIL_INI(fls, shd, z); if (z /= 0) ierr = z
        end if
        if (ierr /= 0) return
    end if

    !> Read from the 'csv' file.
    if (btest(INPUTPARAMSFORMFLAG, 2)) then
        call read_parameters_csv(shd, 100, 'MESH_parameters.csv', ierr)
        if (ierr /= 0) return
    end if

    !> Read from the 'txt' file.
    if (btest(INPUTPARAMSFORMFLAG, 3)) then
        call read_parameters_csv(shd, 100, 'MESH_parameters.txt', ierr)
        if (ierr /= 0) return
    end if

    !> Read from the 'r2c' file.
    if (btest(INPUTPARAMSFORMFLAG, 1)) then
        call read_parameters_r2c(shd, 100, 'MESH_parameters.r2c', ierr)
        if (ierr /= 0) return
    end if

    !> Read from the 'nc' file.
    if (btest(INPUTPARAMSFORMFLAG, 4)) then
#ifdef NETCDF
        call read_parameters_nc(shd, 'MESH_parameters.nc', '', '', '', '', '', '', ierr)
        if (ierr /= 0) return
#else
        call print_error( &
            "The format of a parameter input file is specified as NetCDF but the module is not active. " // &
            "A version of MESH compiled with the NetCDF library must be used to read files in this format.")
        ierr = 1
        return
#endif
    end if

    !>
    !> DISTRIBUTE.
    !>

    !> Constants.

    !> RUNCLASS36 and RUNSVS113.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
        pm%tile%zrfm(:) = pm%gru%zrfm(1)
        pm%tile%zrfh(:) = pm%gru%zrfh(1)
    end if

    !> RUNCLASS36.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
        pm%tile%zbld(:) = pm%gru%zbld(1)
        pm%tile%gc(:) = pm%gru%gc(1)
        if (allocated(RUNCLASS36_flgs%pm%constant%FREZTH)) then
            if (.not. allocated(RUNCLASS36_flgs%pm%tile%FREZTH)) then
                allocate(RUNCLASS36_flgs%pm%tile%FREZTH(shd%lc%NML))
            end if
            RUNCLASS36_flgs%pm%tile%FREZTH(:) = RUNCLASS36_flgs%pm%constant%FREZTH(1)
        end if
        if (allocated(RUNCLASS36_flgs%pm%constant%SWELIM)) then
            if (.not. allocated(RUNCLASS36_flgs%pm%tile%SWELIM)) then
                allocate(RUNCLASS36_flgs%pm%tile%SWELIM(shd%lc%NML))
            end if
            RUNCLASS36_flgs%pm%tile%SWELIM(:) = RUNCLASS36_flgs%pm%constant%SWELIM(1)
        end if
        if (allocated(RUNCLASS36_flgs%pm%constant%SNDENLIM)) then
            if (.not. allocated(RUNCLASS36_flgs%pm%tile%SNDENLIM)) then
                allocate(RUNCLASS36_flgs%pm%tile%SNDENLIM(shd%lc%NML))
            end if
            RUNCLASS36_flgs%pm%tile%SNDENLIM(:) = RUNCLASS36_flgs%pm%constant%SNDENLIM(1)
        end if
    end if

    !> Irrigation module.
    irrm%PROCESS_ACTIVE = (any(irrm%pm_grid%irflg == 1) .or. any(irrm%pm_gru%irflg == 1))

    !> Parameters.

    !> From GRU.
    if (btest(INPUTPARAMSFORMFLAG, 0) .or. btest(INPUTPARAMSFORMFLAG, 2)) then
        do k = 1, shd%lc%NML

            !> GRU index.
            i = shd%lc%JLMOS(k)

            !> RUNCLASS36 and RUNSVS113.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                pm%tile%fcan(k, :) = pm%gru%fcan(i, :)
                pm%tile%lnz0(k, :) = pm%gru%lnz0(i, :)
                pm%tile%sdep(k) = pm%gru%sdep(i)
                pm%tile%xslp(k) = pm%gru%xslp(i)
                pm%tile%dd(k) = pm%gru%dd(i)/1000.0
                pm%tile%sand(k, :) = pm%gru%sand(i, :)
                pm%tile%clay(k, :) = pm%gru%clay(i, :)
            end if

            !> RUNCLASS36.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                pm%tile%fare(k) = pm%gru%fare(i)
                pm%tile%mid(k) = max(1, pm%gru%mid(i))
                pm%tile%iwf(k) = pm%gru%iwf(i)
                pm%tile%alvc(k, :) = pm%gru%alvc(i, :)
                pm%tile%alic(k, :) = pm%gru%alic(i, :)
                pm%tile%lamx(k, :) = pm%gru%lamx(i, :)
                pm%tile%lamn(k, :) = pm%gru%lamn(i, :)
                pm%tile%cmas(k, :) = pm%gru%cmas(i, :)
                pm%tile%root(k, :) = pm%gru%root(i, :)
                pm%tile%rsmn(k, :) = pm%gru%rsmn(i, :)
                pm%tile%qa50(k, :) = pm%gru%qa50(i, :)
                pm%tile%vpda(k, :) = pm%gru%vpda(i, :)
                pm%tile%vpdb(k, :) = pm%gru%vpdb(i, :)
                pm%tile%psga(k, :) = pm%gru%psga(i, :)
                pm%tile%psgb(k, :) = pm%gru%psgb(i, :)
                pm%tile%drn(k) = pm%gru%drn(i)
                pm%tile%mann(k) = pm%gru%mann(i)
                pm%tile%grkf(k) = pm%gru%grkf(i)
                pm%tile%ks(k) = pm%gru%ks(i)
                pm%tile%orgm(k, :) = pm%gru%orgm(i, :)
                pm%tile%zsnl(k) = pm%gru%zsnl(i)
                pm%tile%zplg(k) = pm%gru%zplg(i)
                pm%tile%zpls(k) = pm%gru%zpls(i)
                if (allocated(RUNCLASS36_flgs%pm%gru%FREZTH)) then
                    if (.not. allocated(RUNCLASS36_flgs%pm%tile%FREZTH)) then
                        allocate(RUNCLASS36_flgs%pm%tile%FREZTH(shd%lc%NML))
                    end if
                    RUNCLASS36_flgs%pm%tile%FREZTH(k) = RUNCLASS36_flgs%pm%gru%FREZTH(i)
                end if
                if (allocated(RUNCLASS36_flgs%pm%gru%SWELIM)) then
                    if (.not. allocated(RUNCLASS36_flgs%pm%tile%SWELIM)) then
                        allocate(RUNCLASS36_flgs%pm%tile%SWELIM(shd%lc%NML))
                    end if
                    RUNCLASS36_flgs%pm%tile%SWELIM(k) = RUNCLASS36_flgs%pm%gru%SWELIM(i)
                end if
                if (allocated(RUNCLASS36_flgs%pm%gru%SNDENLIM)) then
                    if (.not. allocated(RUNCLASS36_flgs%pm%tile%SNDENLIM)) then
                        allocate(RUNCLASS36_flgs%pm%tile%SNDENLIM(shd%lc%NML))
                    end if
                    RUNCLASS36_flgs%pm%tile%SNDENLIM(k) = RUNCLASS36_flgs%pm%gru%SNDENLIM(i)
                end if
            end if

            !> Irrigation module.
            if (irrm%pm_gru%irflg(i) /= 0) irrm%pm%irflg(k) = irrm%pm_gru%irflg(i)
            if (irrm%pm_gru%t1(i) /= 0) irrm%pm%t1(k) = irrm%pm_gru%t1(i)
            if (irrm%pm_gru%t2(i) /= 0) irrm%pm%t2(k) = irrm%pm_gru%t2(i)
            if (irrm%pm_gru%ijday1(i) /= 0) irrm%pm%ijday1(k) = irrm%pm_gru%ijday1(i)
            if (irrm%pm_gru%ijday2(i) /= 0) irrm%pm%ijday2(k) = irrm%pm_gru%ijday2(i)
            if (irrm%pm_gru%ignd(i) /= 0) irrm%pm%ignd(k) = irrm%pm_gru%ignd(i)
            if (irrm%pm_gru%thlmin(i) /= 0) irrm%pm%thlmin(k) = irrm%pm_gru%thlmin(i)

            !> BASEFLOWFLAG 1 (Luo, 2012).
            if (bflm%BASEFLOWFLAG == 1) then
                bflm%pm%dgw(k) = bflm%pm_gru%dgw(i)
                bflm%pm%agw(k) = bflm%pm_gru%agw(i)
            end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                bflm%pm%pwr(k) = bflm%pm_gru%pwr(i)
                bflm%pm%flz(k) = bflm%pm_gru%flz(i)
            end if

            !> Cropland irrigation module.
            if (cifg%PROCESS_ACTIVE) then
                cip%jdsow(k) = ciprot%jdsow(i)
                cip%ldini(k) = ciprot%ldini(i)
                cip%lddev(k) = ciprot%lddev(i)
                cip%ldmid(k) = ciprot%ldmid(i)
                cip%ldlate(k) = ciprot%ldlate(i)
                cip%Kcini(k) = ciprot%Kcini(i)
                cip%Kcdev(k) = ciprot%Kcdev(i)
                cip%Kcmid(k) = ciprot%Kcmid(i)
                cip%Kclate(k) = ciprot%Kclate(i)
            end if

            !> Abstraction point location.
            if (pm%gru%iabsp(i) /= 0) pm%tile%iabsp(k) = pm%gru%iabsp(i)

            !> PBSM (blowing snow).
            if (pbsm%PROCESS_ACTIVE) then
                pbsm%pm%fetch(k) = pbsm%pm_gru%fetch(i)
                pbsm%pm%Ht(k) = pbsm%pm_gru%Ht(i)
                pbsm%pm%N_S(k) = pbsm%pm_gru%N_S(i)
                pbsm%pm%A_S(k) = pbsm%pm_gru%A_S(i)
                pbsm%pm%Distrib(k) = pbsm%pm_gru%Distrib(i)
            end if

        end do !k = 1, shd%lc%NML
    end if

    !> From river class (IAK) if not read by grid.
    if (NRVR > 0) then
        do k = 1, NAA

            !> River class index (IAK).
            i = shd%IAK(k)

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                if (bflm%pm_iak%pwr(i) /= 0.0) bflm%pm_grid%pwr(k) = bflm%pm_iak%pwr(i)
                if (bflm%pm_iak%flz(i) /= 0.0) bflm%pm_grid%flz(k) = bflm%pm_iak%flz(i)
            end if

            !> RPN RTE (Watflood, 2007).
            if (rteflg%PROCESS_ACTIVE) then
                if (rtepm_iak%r1n(i) /= 0.0) rtepm%r1n(k) = rtepm_iak%r1n(i)
                if (rtepm_iak%r2n(i) /= 0.0) rtepm%r2n(k) = rtepm_iak%r2n(i)
                if (rtepm_iak%mndr(i) /= 0.0) rtepm%mndr(k) = rtepm_iak%mndr(i)
                if (rtepm_iak%widep(i) /= 0.0) rtepm%widep(k) = rtepm_iak%widep(i)
                if (rtepm_iak%aa2(i) /= 0.0) rtepm%aa2(k) = rtepm_iak%aa2(i)
                if (rtepm_iak%aa3(i) /= 0.0) rtepm%aa3(k) = rtepm_iak%aa3(i)
                if (rtepm_iak%aa4(i) /= 0.0) rtepm%aa4(k) = rtepm_iak%aa4(i)
            end if

        end do !k = 1, NAA
        do k = 1, shd%lc%NML

            !> Grid index.
            i = shd%lc%ILMOS(k)

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                if (bflm%pm_iak%pwr(shd%IAK(i)) /= 0.0) bflm%pm%pwr(k) = bflm%pm_iak%pwr(shd%IAK(i))
                if (bflm%pm_iak%flz(shd%IAK(i)) /= 0.0) bflm%pm%flz(k) = bflm%pm_iak%flz(shd%IAK(i))
            end if

        end do !k = 1, shd%lc%NML
    end if

    !> From grid.
    if (btest(INPUTPARAMSFORMFLAG, 1) .or. btest(INPUTPARAMSFORMFLAG, 4)) then
        do k = 1, shd%lc%NML

            !> Omit GRU's with mosaic ID >= 100 and < 1000 from being assigned grid-based values (special condition).
            if (pm%tile%mid(k) >= 100 .and. pm%tile%mid(k) < 1000) cycle

            !> Grid index.
            i = shd%lc%ILMOS(k)

            !> RUNCLASS36 and RUNSVS113.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                if (allocated(shd%SLOPE_INT)) then
                    if (shd%SLOPE_INT(i) /= 0.0) pm%tile%xslp(k) = shd%SLOPE_INT(i)
                end if
                if (allocated(shd%DRDN)) then
                    if (shd%DRDN(i) /= 0.0) pm%tile%dd(k) = shd%DRDN(i)
                end if
                if (any(pm%grid%fcan(i, :) /= 0.0)) pm%tile%fcan(k, :) = pm%grid%fcan(i, :)
                if (any(pm%grid%lnz0(i, :) /= 0.0)) pm%tile%lnz0(k, :) = pm%grid%lnz0(i, :)
                if (pm%grid%sdep(i) /= 0.0) pm%tile%sdep(k) = pm%grid%sdep(i)
                if (pm%grid%xslp(i) /= 0.0) pm%tile%xslp(k) = pm%grid%xslp(i)
                if (pm%grid%dd(i) /= 0.0) pm%tile%dd(k) = pm%grid%dd(i)
                if (any(pm%grid%sand(i, :) /= 0.0)) pm%tile%sand(k, :) = pm%grid%sand(i, :)
                if (any(pm%grid%clay(i, :) /= 0.0)) pm%tile%clay(k, :) = pm%grid%clay(i, :)
                if (any(pm%grid%orgm(i, :) /= 0.0)) pm%tile%orgm(k, :) = pm%grid%orgm(i, :)
            end if

            !> RUNCLASS36.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                if (pm%grid%iwf(i) /= -1) pm%tile%iwf(k) = pm%grid%iwf(i)
                if (allocated(RUNCLASS36_flgs%pm%grid%FREZTH)) then
                    if (.not. allocated(RUNCLASS36_flgs%pm%tile%FREZTH)) then
                        allocate(RUNCLASS36_flgs%pm%tile%FREZTH(shd%lc%NML))
                    end if
                    RUNCLASS36_flgs%pm%tile%FREZTH(k) = RUNCLASS36_flgs%pm%grid%FREZTH(i)
                end if
                if (allocated(RUNCLASS36_flgs%pm%grid%SWELIM)) then
                    if (.not. allocated(RUNCLASS36_flgs%pm%tile%SWELIM)) then
                        allocate(RUNCLASS36_flgs%pm%tile%SWELIM(shd%lc%NML))
                    end if
                    RUNCLASS36_flgs%pm%tile%SWELIM(k) = RUNCLASS36_flgs%pm%grid%SWELIM(i)
                end if
                if (allocated(RUNCLASS36_flgs%pm%grid%SNDENLIM)) then
                    if (.not. allocated(RUNCLASS36_flgs%pm%tile%SNDENLIM)) then
                        allocate(RUNCLASS36_flgs%pm%tile%SNDENLIM(shd%lc%NML))
                    end if
                    RUNCLASS36_flgs%pm%tile%SNDENLIM(k) = RUNCLASS36_flgs%pm%grid%SNDENLIM(i)
                end if
            end if

            !> Irrigation module.
            if (irrm%pm_grid%irflg(i) /= 0) irrm%pm%irflg(k) = irrm%pm_grid%irflg(i)
            if (irrm%pm_grid%t1(i) /= 0) irrm%pm%t1(k) = irrm%pm_grid%t1(i)
            if (irrm%pm_grid%t2(i) /= 0) irrm%pm%t2(k) = irrm%pm_grid%t2(i)
            if (irrm%pm_grid%ijday1(i) /= 0) irrm%pm%ijday1(k) = irrm%pm_grid%ijday1(i)
            if (irrm%pm_grid%ijday2(i) /= 0) irrm%pm%ijday2(k) = irrm%pm_grid%ijday2(i)
            if (irrm%pm_grid%ignd(i) /= 0) irrm%pm%ignd(k) = irrm%pm_grid%ignd(i)
            if (irrm%pm_grid%thlmin(i) /= 0) irrm%pm%thlmin(k) = irrm%pm_grid%thlmin(i)

            !> Abstraction point location.
            if (pm%grid%iabsp(i) /= 0) pm%tile%iabsp(k) = pm%grid%iabsp(i)

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                if (bflm%pm_grid%pwr(i) /= 0.0) bflm%pm%pwr(k) = bflm%pm_grid%pwr(i)
                if (bflm%pm_grid%flz(i) /= 0.0) bflm%pm%flz(k) = bflm%pm_grid%flz(i)
            end if

            !> PBSM (blowing snow).
            if (pbsm%PROCESS_ACTIVE) then
                if (pbsm%pm_grid%fetch(i) /= 0.0) pbsm%pm%fetch(k) = pbsm%pm_grid%fetch(i)
                if (pbsm%pm_grid%Ht(i) /= 0.0) pbsm%pm%Ht(k) = pbsm%pm_grid%Ht(i)
                if (pbsm%pm_grid%N_S(i) /= 0.0) pbsm%pm%N_S(k) = pbsm%pm_grid%N_S(i)
                if (pbsm%pm_grid%A_S(i) /= 0.0) pbsm%pm%A_S(k) = pbsm%pm_grid%A_S(i)
                if (pbsm%pm_grid%Distrib(i) /= 0.0) pbsm%pm%Distrib(k) = pbsm%pm_grid%Distrib(i)
            end if

        end do !k = 1, shd%lc%NML
    end if

    !> Determine the "last configured layer" read from file (CLASS default: 3).
    if (NRSOILAYEREADFLAG > 3) then
        ignd = min(NRSOILAYEREADFLAG, NSL)
    else if (NRSOILAYEREADFLAG == 1) then
        ignd = 0
    else
        ignd = 3
    end if

    !> Distribute soil variables to layers lower than the "last configured layer".
    do j = 2, shd%lc%IGND
        if (j > ignd .and. ignd > 0) then
            where (pm%tile%sand(:, j) == 0.0) pm%tile%sand(:, j) = pm%tile%sand(:, ignd)
            where (pm%tile%clay(:, j) == 0.0) pm%tile%clay(:, j) = pm%tile%clay(:, ignd)
            where (pm%tile%orgm(:, j) == 0.0) pm%tile%orgm(:, j) = pm%tile%orgm(:, ignd)
        end if
    end do

    !> Check for impermeable soils.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then

        !> Check the first layer for impermeable soils.
        where (pm%tile%sdep == 0.0 .and. pm%tile%sand(:, 1) > -2.5)
            pm%tile%sand(:, 1) = -3.0
            pm%tile%clay(:, 1) = -3.0
            pm%tile%orgm(:, 1) = -3.0
        end where

        !> Check for impermeable soils.
        do j = 2, shd%lc%IGND
            where (pm%tile%sdep < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. pm%tile%sand(:, j) > -2.5)
                pm%tile%sand(:, j) = -3.0
                pm%tile%clay(:, j) = -3.0
                pm%tile%orgm(:, j) = -3.0
            end where
        end do
    end if

    !> Nudge SDEP to the nearest interface between soil layers in the profile.
    if (NUDGESDEPFLAG == 1 .and. shd%lc%IGND > 0) then

        !> Within the first layer, nudge to the first boundary and not to zero so it remains active.
!todo: Double-check this is okay with SVS.
        !> Hydrologically inactive layers should be identified with SAND flag <= -3.
        !> Limit depths deeper than the soil profile to the last boundary for consistency.
        where (pm%tile%sdep <= shd%lc%sl%zbot(1))
            pm%tile%sdep = shd%lc%sl%zbot(1)
        elsewhere (pm%tile%sdep > shd%lc%sl%zbot(shd%lc%IGND))
            pm%tile%sdep = shd%lc%sl%zbot(shd%lc%IGND)
        end where

        !> Nudge in-between depths to the closest boundary.
        do j = 2, shd%lc%IGND
            where (pm%tile%sdep > shd%lc%sl%zbot(j - 1) .and. pm%tile%sdep <= (0.5*shd%lc%sl%delz(j) + shd%lc%sl%zbot(j - 1)))
                pm%tile%sdep = shd%lc%sl%zbot(j - 1)
            elsewhere (pm%tile%sdep > (0.5*shd%lc%sl%delz(j) + shd%lc%sl%zbot(j - 1)) .and. pm%tile%sdep <= shd%lc%sl%zbot(j))
                pm%tile%sdep = shd%lc%sl%zbot(j)
            end where
        end do

    end if

    return

97  call print_error('Unable to allocate model parameters.')
    return

end subroutine
