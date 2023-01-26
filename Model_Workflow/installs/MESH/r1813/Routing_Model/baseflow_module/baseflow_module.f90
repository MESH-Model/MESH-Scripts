!> Description:
!>  BASEFLOW module.
module baseflow_module

    implicit none

    !> Description:
    !>  Container for BASEFLOWFLAG parameters.
    !>
    !> Variables (luo_2012):
    !*  dgw: Delay time of the overlying soil layers in the  aquifer (hour).
    !*  agw: Recession constant of the aquifer.
    !>
    !> Variables (wf_lzs):
    !*  flz: lower zone function (mm).
    !*  pwr: exponent on the lower zone storage in the lower zone funnction.
    type baseflow_parameters
        real, dimension(:), allocatable :: dgw, agw
        real, dimension(:), allocatable :: pwr, flz
    end type

    !> Description:
    !>  Container for BASEFLOWFLAG variables.
    !>
    !> Variables:
    !*  WrchrgIni: Initial constant recharge for cold start (mm/hr).
    !*  QbIni: Initial constant baseflow for cold start (mm/hr).
    type baseflow_variables
        real WrchrgIni, QbIni
    end type

    !> BASEFLOWFLAG (1)
    real, dimension(:), allocatable :: Wseep, Wrchrg
    real, dimension(:), pointer, save :: Qb
    real :: Wrchrg_new, Qb_new

    !> BASEFLOWFLAG (2)
    real, dimension(:), allocatable :: dlz, lzs

    !> Description:
    !>  Container for BASEFLOWFLAG variables, parameters, and options.
    !>
    !> Variables:
    !*  BASEFLOWFLAG: Flag that specifies the active baseflow routine.
    !*  BUCKETFLAG: Specifies the scale of the aquifer (e.g., tile, grid, etc.).
    !*  dts: Time-step of the baseflow routine.
    type baseflow_container
        type(baseflow_parameters) :: pm, pm_iak, pm_gru, pm_grid
        type(baseflow_variables) :: vs
        integer :: BASEFLOWFLAG = 0, BUCKETFLAG = 0, dts = 0
    end type

    type(baseflow_container), save :: bflm

    contains

    subroutine bflm_init(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer NA, NML, NTYPE, NRVR, iun, n, i, ierr
        character(len = DEFAULT_LINE_LENGTH) line

        !> Return if BASEFLOWFLAG is not active.
        if (bflm%BASEFLOWFLAG == 0) return

        NA = shd%NA
        NML = shd%lc%NML
        NTYPE = shd%lc%NTYPE
        NRVR = shd%NRVR

        !> Summarize current BASEFLOWFLAG configuration to output.
        select case (bflm%BASEFLOWFLAG)
            case (1, 2)
                call print_message('BASEFLOW component is ACTIVE.')
            case default
                write(line, FMT_GEN) bflm%BASEFLOWFLAG
                call print_warning('BASEFLOWFLAG ' // trim(adjustl(line)) // ' not supported.')
        end select
        if (DIAGNOSEMODE) then
            call increase_tab()
            write(line, '(i8)') bflm%dts/60
            line = 'hf=' // adjustl(line)
            select case (bflm%BUCKETFLAG)
                case (1)
                    line = 'grid ' // adjustl(line)
                case default
                    line = 'tile ' // adjustl(line)
            end select
            select case (bflm%BASEFLOWFLAG)
                case (1)
                    line = 'BASEFLOWFLAG  luo_2012 ' // adjustl(line)
                    call print_message(line)
                    write(line, FMT_GEN) 'WRCHRG_INI', bflm%vs%WrchrgIni
                    call print_message(line)
                    write(line, FMT_GEN) 'QB_INI', bflm%vs%QbIni
                    call print_message(line)
                    write(line, FMT_GEN) 'DGWSH', (bflm%pm_gru%dgw(i), i = 1, NTYPE)
                    call print_message(line)
                    write(line, FMT_GEN) 'AGWSH', (bflm%pm_gru%agw(i), i = 1, NTYPE)
                    call print_message(line)
                case (2)
                    line = 'BASEFLOWFLAG  wf_lzs ' // adjustl(line)
                    call print_message(line)
                    if (any(bflm%pm_gru%pwr /= 0.0)) then
                        write(line, FMT_GEN) 'pwr_gru', (bflm%pm_gru%pwr(i), i = 1, NTYPE)
                        call print_message(line)
                    else if (any(bflm%pm_iak%pwr /= 0.0)) then
                        write(line, FMT_GEN) 'pwr_iak', (bflm%pm_iak%pwr(i), i = 1, NRVR)
                        call print_message(line)
                    else if (any(bflm%pm_grid%pwr /= 0.0)) then
                        write(line, FMT_GEN) 'pwr_grid (min., max.)', minval(bflm%pm_grid%pwr), maxval(bflm%pm_grid%pwr)
                        call print_message(line)
                    end if
                    if (any(bflm%pm_gru%pwr /= 0.0)) then
                        write(line, FMT_GEN) 'flz_gru', (bflm%pm_gru%flz(i), i = 1, NTYPE)
                        call print_message(line)
                    else if (any(bflm%pm_iak%pwr /= 0.0)) then
                        write(line, FMT_GEN) 'flz_iak', (bflm%pm_iak%flz(i), i = 1, NRVR)
                        call print_message(line)
                    else if (any(bflm%pm_grid%pwr /= 0.0)) then
                        write(line, FMT_GEN) 'flz_grid (min., max.)', minval(bflm%pm_grid%flz), maxval(bflm%pm_grid%flz)
                        call print_message(line)
                    end if
            end select
            call decrease_tab()
        end if

        !> Allocate and initialize local variables.
        vs%tile%stggw = bflm%vs%WrchrgIni
        vs%grid%stggw = bflm%vs%WrchrgIni
        select case (bflm%BASEFLOWFLAG)
            case (1)
                allocate(Wseep(NML), Wrchrg(NML), Qb(NML))
                Wseep = 0.0
                Wrchrg = vs%tile%stggw
                Qb = bflm%vs%QbIni
            case (2)
                if (bflm%BUCKETFLAG == 1) then
                    allocate(dlz(NA), lzs(NA))
                    dlz = 0.0
                    lzs = vs%grid%stggw
                    bflm%pm_grid%flz = 1.0 - (1.0 - bflm%pm_grid%flz)
                end if
        end select

    end subroutine

    subroutine bflm_resume_read(fls, shd)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        real(kind = 4), dimension(:), allocatable :: lzs_r4, Qb_r4
        integer iun, ierr

        !> Return if BASEFLOWFLAG is not active.
        if (bflm%BASEFLOWFLAG == 0) return

        !> Resume states from file.
        select case (bflm%BASEFLOWFLAG)
            case (1)
                iun = fls%fl(mfk%f883)%iun
                open( &
                    iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012', action = 'read', status = 'old', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) then
                    call print_error( &
                        'Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012' // ' to resume states.')
                    call program_abort()
                end if
                allocate(lzs_r4(shd%lc%NML), Qb_r4(shd%lc%NML))
                lzs_r4 = 0.0
                Qb_r4 = 0.0
                read(iun) lzs_r4
                read(iun) Qb_r4
                close(iun)
                vs%tile%stggw = lzs_r4
                Qb = Qb_r4
            case (2)
                iun = fls%fl(mfk%f883)%iun
                open( &
                    iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz', action = 'read', status = 'old', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) then
                    call print_error( &
                        'Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz' // ' to resume states.')
                    call program_abort()
                end if
                allocate(lzs_r4(shd%lc%NML))
                lzs_r4 = 0.0
                read(iun) lzs_r4
                close(iun)
                vs%tile%stggw = lzs_r4
        end select

    end subroutine

    subroutine bflm_within_tile(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer k

        !> Return if BASEFLOWFLAG is not active.
        if (bflm%BASEFLOWFLAG == 0) return

        !> Calculate contribution of baseflow to lower zone storage and redistribute runoff.
        select case (bflm%BASEFLOWFLAG)
            case (1)
                Wseep(il1:il2) = vs%tile%drainsol(il1:il2)*3600.0
                Wrchrg(il1:il2) = vs%tile%stggw(il1:il2)
                do k = il1, il2
                    call baseFlow_luo2012(Wseep(k), bflm%pm%dgw(k), Wrchrg(k), bflm%pm%agw(k), Qb(k), 1.0, Wrchrg_new, Qb_new)
                    vs%tile%drainsol(k) = Qb_new/3600.0
                    Qb(k) = Qb_new
                    vs%tile%stggw(k) = Wrchrg_new
                end do
            case (2)
                vs%tile%stggw(il1:il2) = vs%tile%stggw(il1:il2) + vs%tile%drainsol(il1:il2)*ic%dts
        end select

    end subroutine

    subroutine bflm_within_grid(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer k

        !> Return if BASEFLOWFLAG is not active.
        if (bflm%BASEFLOWFLAG == 0) return

        !> Calculate contribution of baseflow to lower zone storage and redistribute runoff.
        select case (bflm%BASEFLOWFLAG)
            case (2)
                if ((bflm%dts - ic%dts*ic%ts_hourly) == 0) then
                    lzs(i1:i2) = vs%grid%stggw(i1:i2)
                    call baseflow_wfqlz(bflm%pm_grid%flz, bflm%pm_grid%pwr, lzs, dlz, shd%NA, i1, i2)
                    dlz(i1:i2) = max(min(dlz(i1:i2), lzs(i1:i2)), 0.0)/real(bflm%dts/ic%dts)
                end if
                vs%grid%drainsol(i1:i2) = dlz(i1:i2)/real(ic%dts)
                vs%grid%stggw(i1:i2) = vs%grid%stggw(i1:i2) - vs%grid%drainsol(i1:i2)*ic%dts
                do k = il1, il2
                    vs%tile%drainsol(k) = vs%grid%drainsol(shd%lc%ILMOS(k))
                    vs%tile%stggw(k) = vs%grid%stggw(shd%lc%ILMOS(k))
                end do
        end select

    end subroutine

    subroutine bflm_resume_save(fls, shd)

        use mpi_module
        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer ierr, iun

        !> Return if not the head node or if BASEFLOWFLAG is not active.
        if (.not. ISHEADNODE .or. bflm%BASEFLOWFLAG == 0) return

        !> Save states to file.
        select case (bflm%BASEFLOWFLAG)
            case (1)
                iun = fls%fl(mfk%f883)%iun
                open( &
                    iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012', action = 'write', status = 'replace', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) then
                    call print_error( &
                        'Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012' // ' to save states.')
                    call program_abort()
                end if
                write(iun) real(vs%tile%stggw, kind = 4)
                write(iun) real(Qb, kind = 4)
                close(iun)
            case (2)
                iun = fls%fl(mfk%f883)%iun
                open( &
                    iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz', action = 'write', status = 'replace', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) then
                    call print_error( &
                        'Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz' // ' to save states.')
                    call program_abort()
                end if
                write(iun) real(vs%tile%stggw, kind = 4)
                close(iun)
        end select

    end subroutine

    subroutine bflm_finalize(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Return if BASEFLOWFLAG is not active.
        if (bflm%BASEFLOWFLAG == 0) return

    end subroutine

    !> Description:
    !>  Parse BASEFLOWFLAG.
    !>
    !> Input variables:
    !*  flag: BASEFLOWFLAG from file.
    subroutine bflm_parse_flag(flag)

        use strings
        use model_dates

        !> Input variables.
        character(len = *), intent(in) :: flag

        !> Local variables.
        character(len = 200), dimension(20) :: args
        integer nargs, n, i, ierr

        !> Default behaviour: All baseflow routines disabled.
        bflm%BASEFLOWFLAG = 0

        !> Parse the flag for options.
        call parse(flag, ' ', args, nargs)

        !> Assign options.
        do i = 2, nargs

            !> Old numeric option assigns presets.
            !> Word options override presets.
            if (args(i) == '1' .or. lowercase(args(i)) == 'luo_2012') then
                bflm%BASEFLOWFLAG = 1
                bflm%BUCKETFLAG = 0
                bflm%dts = ic%dts
            else if (args(i) == '2' .or. lowercase(args(i)) == 'wf_lzs') then
                bflm%BASEFLOWFLAG = 2
                bflm%BUCKETFLAG = 1
                bflm%dts = 3600
            else if (lowercase(args(i)) == 'grid') then
                bflm%BUCKETFLAG = 1
            else if (lowercase(args(i)) == 'tile') then
                bflm%BUCKETFLAG = 0
            else if (lowercase(args(i)(1:3)) == 'hf=') then
                call value(args(i)(4:), n, ierr)
                if (ierr == 0) bflm%dts = n*60
            end if
        end do

    end subroutine

end module
