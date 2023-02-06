!> Description:
!>  Module to manage input climate forcing data.
module climate_forcing

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use climate_forcing_config
    use climate_forcing_io
    use print_routines

    implicit none

    contains

    !> Description:
    !>  Initializes the climate forcing object, including the allocation
    !>  of variables, and opens the climate files for forcing data.
    !>  Resumes states from the climate forcing state file, if enabled.
    !>
    !> Input/output variables.
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  ii1: Start index in the GAT vector.
    !*  ii2: Stop index in the GAT vector.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    function climate_module_init(fls, shd, ii1, ii2, cm) result(ENDDATA)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        !> model_dates: 'ic' counter variable.
        !> model_variables: 'vs' variable.
        use model_files_variables
        use shd_variables
        use model_dates
        use model_variables
!-        use FLAGS

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams) shd
        integer, intent(in) :: ii1, ii2

        !> Input/output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer vid, iun, isteps1, isteps2, month, day, t, s, k, j, i, ierr
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) date1, date2, time

        ENDDATA = .false.

        !> Allocate the climate forcing variable.
!?        cm%nclim = ck%nn
!?        if (allocated(cm%dat)) deallocate(cm%dat)
!?        allocate(cm%dat(cm%nclim))

        !> Set the default file name and map the climate GRD/GAT/GRU variables to 'vs'.
        if (cm%dat(ck%FB)%factive) then
            if (len_trim(cm%dat(ck%FB)%fname) == 0) then
                cm%dat(ck%FB)%fname = 'basin_shortwave'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%fsin)) allocate(vs%grid%fsin(vs%grid%dim_length))
                cm%dat(ck%FB)%GRD => vs%grid%fsin(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%fsin)) allocate(vs%tile%fsin(vs%tile%dim_length))
                cm%dat(ck%FB)%GAT => vs%tile%fsin(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%fsin)) allocate(vs%gru%fsin(vs%gru%dim_length))
                cm%dat(ck%FB)%GRU => vs%gru%fsin(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%FI)%factive) then
            if (len_trim(cm%dat(ck%FI)%fname) == 0) then
                cm%dat(ck%FI)%fname = 'basin_longwave'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%flin)) allocate(vs%grid%flin(vs%grid%dim_length))
                cm%dat(ck%FI)%GRD => vs%grid%flin(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%flin)) allocate(vs%tile%flin(vs%tile%dim_length))
                cm%dat(ck%FI)%GAT => vs%tile%flin(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%flin)) allocate(vs%gru%flin(vs%gru%dim_length))
                cm%dat(ck%FI)%GRU => vs%gru%flin(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%RT)%factive) then
            if (len_trim(cm%dat(ck%RT)%fname) == 0) then
                cm%dat(ck%RT)%fname = 'basin_rain'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%pre)) allocate(vs%grid%pre(vs%grid%dim_length))
                cm%dat(ck%RT)%GRD => vs%grid%pre(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%pre)) allocate(vs%tile%pre(vs%tile%dim_length))
                cm%dat(ck%RT)%GAT => vs%tile%pre(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%pre)) allocate(vs%gru%pre(vs%gru%dim_length))
                cm%dat(ck%RT)%GRU => vs%gru%pre(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%RR)%factive) then
            if (len_trim(cm%dat(ck%RR)%fname) == 0) then
                cm%dat(ck%RR)%fname = 'basin_liquid_precip'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%prern)) allocate(vs%grid%prern(vs%grid%dim_length))
                cm%dat(ck%RR)%GRD => vs%grid%prern(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%prern)) allocate(vs%tile%prern(vs%tile%dim_length))
                cm%dat(ck%RR)%GAT => vs%tile%prern(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%prern)) allocate(vs%gru%prern(vs%gru%dim_length))
                cm%dat(ck%RR)%GRU => vs%gru%prern(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%SR)%factive) then
            if (len_trim(cm%dat(ck%SR)%fname) == 0) then
                cm%dat(ck%SR)%fname = 'basin_solid_precip'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%presno)) allocate(vs%grid%presno(vs%grid%dim_length))
                cm%dat(ck%SR)%GRD => vs%grid%presno(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%presno)) allocate(vs%tile%presno(vs%tile%dim_length))
                cm%dat(ck%SR)%GAT => vs%tile%presno(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%presno)) allocate(vs%gru%presno(vs%gru%dim_length))
                cm%dat(ck%SR)%GRU => vs%gru%presno(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%TT)%factive) then
            if (len_trim(cm%dat(ck%TT)%fname) == 0) then
                cm%dat(ck%TT)%fname = 'basin_temperature'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%ta)) allocate(vs%grid%ta(vs%grid%dim_length))
                cm%dat(ck%TT)%GRD => vs%grid%ta(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%ta)) allocate(vs%tile%ta(vs%tile%dim_length))
                cm%dat(ck%TT)%GAT => vs%tile%ta(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%ta)) allocate(vs%gru%ta(vs%gru%dim_length))
                cm%dat(ck%TT)%GRU => vs%gru%ta(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%UV)%factive) then
            if (len_trim(cm%dat(ck%UV)%fname) == 0) then
                cm%dat(ck%UV)%fname = 'basin_wind'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%uv)) allocate(vs%grid%uv(vs%grid%dim_length))
                cm%dat(ck%UV)%GRD => vs%grid%uv(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%uv)) allocate(vs%tile%uv(vs%tile%dim_length))
                cm%dat(ck%UV)%GAT => vs%tile%uv(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%uv)) allocate(vs%gru%uv(vs%gru%dim_length))
                cm%dat(ck%UV)%GRU => vs%gru%uv(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%WD)%factive) then
            if (len_trim(cm%dat(ck%WD)%fname) == 0) then
                cm%dat(ck%WD)%fname = 'basin_winddir'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%wdir)) allocate(vs%grid%wdir(vs%grid%dim_length))
                cm%dat(ck%WD)%GRD => vs%grid%wdir(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%wdir)) allocate(vs%tile%wdir(vs%tile%dim_length))
                cm%dat(ck%WD)%GAT => vs%tile%wdir(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%wdir)) allocate(vs%gru%wdir(vs%gru%dim_length))
                cm%dat(ck%WD)%GRU => vs%gru%wdir(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%P0)%factive) then
            if (len_trim(cm%dat(ck%P0)%fname) == 0) then
                cm%dat(ck%P0)%fname = 'basin_pres'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%pres)) allocate(vs%grid%pres(vs%grid%dim_length))
                cm%dat(ck%P0)%GRD => vs%grid%pres(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%pres)) allocate(vs%tile%pres(vs%tile%dim_length))
                cm%dat(ck%P0)%GAT => vs%tile%pres(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%pres)) allocate(vs%gru%pres(vs%gru%dim_length))
                cm%dat(ck%P0)%GRU => vs%gru%pres(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%HU)%factive) then
            if (len_trim(cm%dat(ck%HU)%fname) == 0) then
                cm%dat(ck%HU)%fname = 'basin_humidity'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%qa)) allocate(vs%grid%qa(vs%grid%dim_length))
                cm%dat(ck%HU)%GRD => vs%grid%qa(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%qa)) allocate(vs%tile%qa(vs%tile%dim_length))
                cm%dat(ck%HU)%GAT => vs%tile%qa(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%qa)) allocate(vs%gru%qa(vs%gru%dim_length))
                cm%dat(ck%HU)%GRU => vs%gru%qa(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%N0)%factive) then
            if (len_trim(cm%dat(ck%N0)%fname) == 0) then
                cm%dat(ck%N0)%fname = 'WR_runoff'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%rff)) allocate(vs%grid%rff(vs%grid%dim_length))
                cm%dat(ck%N0)%GRD => vs%grid%rff(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%rff)) allocate(vs%tile%rff(vs%tile%dim_length))
                cm%dat(ck%N0)%GAT => vs%tile%rff(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%rff)) allocate(vs%gru%rff(vs%gru%dim_length))
                cm%dat(ck%N0)%GRU => vs%gru%rff(1:shd%lc%NTYPE)
            end if
        end if
        if (cm%dat(ck%O1)%factive) then
            if (len_trim(cm%dat(ck%O1)%fname) == 0) then
                cm%dat(ck%O1)%fname = 'WR_recharge'
            end if
            if (associated(vs%grid)) then
                if (.not. associated(vs%grid%rchg)) allocate(vs%grid%rchg(vs%grid%dim_length))
                cm%dat(ck%O1)%GRD => vs%grid%rchg(1:shd%NA)
            end if
            if (associated(vs%tile)) then
                if (.not. associated(vs%tile%rchg)) allocate(vs%tile%rchg(vs%tile%dim_length))
                cm%dat(ck%O1)%GAT => vs%tile%rchg(1:shd%lc%NML)
            end if
            if (associated(vs%gru)) then
                if (.not. associated(vs%gru%rchg)) allocate(vs%gru%rchg(vs%gru%dim_length))
                cm%dat(ck%O1)%GRU => vs%gru%rchg(1:shd%lc%NTYPE)
            end if
        end if

        !> Read from file to override default configuration.
        call open_config(cm)

        !> Allocate GRD/GAT/GRU variables because no equivalent 'vs' variables exist for 'MET'.
        if (cm%dat(ck%MET)%factive) then
            allocate(cm%dat(ck%MET)%GRD(shd%NA), cm%dat(ck%MET)%GAT(shd%lc%NML), cm%dat(ck%MET)%GRU(shd%lc%NTYPE))
        end if

        !> Initialize climate variables.
        call reset_tab()
        call print_message('READING: Climate forcing variables')
        call increase_tab()
        do vid = 1, cm%nclim

            !> Cycle if the variable is not active.
            if (.not. cm%dat(vid)%factive) cycle

            !> Assign a unit number to the file.
            cm%dat(vid)%fiun = cm%basefileunit + vid

            !> Open the file.
            if (open_data(shd, cm, vid)) goto 999

            !> Print field to screen.
            call print_message("OPENING: " // trim(cm%dat(vid)%fpath))
            call increase_tab()

            !> Check if the file is in the legacy binary format.
            if (cm%dat(vid)%ffmt == 0) then
                call print_error('Forcing data in the legacy binary format (*.bin) are no longer supported.')
                call print_message('These data must be converted to one of the supported formats.')
                call program_abort()
            end if

            !> Check that the forcing record is not less than the model time-step.
!todo: Could probably find a way to accommodate this (e.g., accumulating/averaging/etc...).
            if (cm%dat(vid)%hf < ic%dtmins) then
                write(line, FMT_GEN) ic%dtmins
                call print_error('The forcing data time-step is less than the model time-step: ' // trim(adjustl(line)) // ' mins')
                call print_message('Aggregate the data to the model time-step.')
                call program_abort()
            end if

            !> Check if the time-step is divisible by the model time-step.
            if (mod(cm%dat(vid)%hf, ic%dtmins) /= 0) then
                call print_error('The forcing data time-step must be divisible by the model time-step.')
                call increase_tab()
                write(line, FMT_GEN) cm%dat(vid)%hf
                call print_message('Data time-step: ' // trim(adjustl(line)) // ' mins')
                write(line, FMT_GEN) ic%dtmins
                call print_message('Model time-step: ' // trim(adjustl(line)) // ' mins')
                call decrease_tab()
                call program_abort()
            end if

            !> Warn of unsupprted interpolation flag option.
            if (cm%dat(vid)%ipflg > 1) then
                write(line, FMT_GEN) cm%dat(vid)%ipflg
                call print_warning('INTERPOLATIONFLAG ' // trim(adjustl(line)) // ' is not supported and has no effect.')
                cm%dat(vid)%ipflg = 0
            end if

            !> Remark on INTERPOLATIONFLAG if the data and model use the same time-step.
            if (cm%dat(vid)%ipflg == 1 .and. cm%dat(vid)%hf == ic%dtmins) then
                line = 'INTERPOLATIONFLAG is active but has no effect. The climate forcing data and model have the same time-step.'
                call print_remark(line)
                cm%dat(vid)%ipflg = 0
            end if

            !> Preserve the last record skipped with INTERPOLATIONFLAG 2.
!?            if (INTERPOLATIONFLAG == 2) nrs = nrs - 1

            !> Activate fields for INTERPOLATIONFLAG.
            if (cm%dat(vid)%ipflg == 1) then
                if (allocated(cm%dat(vid)%ipdat)) deallocate(cm%dat(vid)%ipdat)
                allocate(cm%dat(vid)%ipdat(size(cm%dat(vid)%blocks, 1), 2))
            end if

        !> Allocate and initialize the alpha coefficient for the default series.
!            allocate(cm%dat(vid)%alpha(cm%dat(vid)%nseries))
!            cm%dat(vid)%alpha = 1.0 / cm%dat(vid)%nseries

        !> Special case two sources of precipitation with alpha constant.
!todo generalize this
!?            if (vid == ck%RT .and. cm%dat(ck%RT)%ffmt == 6) then
!?                call Init_clim_data(ck%RT, 921, cm)
!?                call Init_clim_data(8, 922, cm)
!?                return
!?            end if

            !> Allocate the data series.
!-            allocate(cm%dat(vid)%GRD(shd%NA), cm%dat(vid)%GAT(shd%lc%NML), cm%dat(vid)%GRU(shd%lc%NTYPE))

            !> Skip records in the file to the simulation start date.
            isteps1 = jday_to_tsteps( &
                cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%jday, cm%dat(vid)%start_date%hour, &
                cm%dat(vid)%start_date%mins, cm%dat(vid)%hf)
            isteps2 = jday_to_tsteps(ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins, cm%dat(vid)%hf)
            if (isteps2 < isteps1) then
                call print_error('The first record occurs after the simulation start date.')
                call print_message( &
                    'The record must start on or after the simulation start date.')
                call increase_tab()
                call Julian2MonthDay(cm%dat(vid)%start_date%jday, cm%dat(vid)%start_date%year, month, day)
                write(line, "(i4, '/', i2.2, '/', i2.2, ' ', i2.2, ':', i2.2, ' (', 4i4, ')')") &
                    cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%month, cm%dat(vid)%start_date%day, &
                    cm%dat(vid)%start_date%hour, cm%dat(vid)%start_date%mins, &
                    cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%jday, &
                    cm%dat(vid)%start_date%hour, cm%dat(vid)%start_date%mins
                call print_message('First record occurs on: ' // trim(line))
                write(line, "(i4, '/', i2.2, '/', i2.2, ' ', i2.2, ':', i2.2, ' (', 4i4, ')')") &
                    ic%start%year, ic%start%month, ic%start%day, ic%start%hour, ic%start%mins, &
                    ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
                call print_message('Simulation start date: ' // trim(line))
                call decrease_tab()
                call program_abort()
            end if
            cm%dat(vid)%iskip = (isteps2 - isteps1)
            if (cm%dat(vid)%iskip > 0) then
                write(line, FMT_GEN) cm%dat(vid)%iskip
                call print_message('Skipping ' // trim(adjustl(line)) // ' records.')
                if (update_data(shd, cm, vid, cm%dat(vid)%iskip)) goto 999
            end if
            call decrease_tab()
        end do

        !> Print summary of climate forcing variables.
        if (DIAGNOSEMODE) then
            call print_message('Diagnostic summary:')
            call increase_tab()
            write(line, FMT_GEN) &
                'Variable', 'Name', 'File format', 'Frame length', 'Blocks in-mem.', 'No. series', 'Ref. date', '(YJD)', 'Ref. time'
            call print_message(line)
            do i = 1, cm%nclim
                if (cm%dat(i)%factive) then
                    write(date1, FMT_DATE) cm%dat(i)%start_date%year, cm%dat(i)%start_date%month, cm%dat(i)%start_date%day
                    write(date2, FMT_DATE_YJD) cm%dat(i)%start_date%year, cm%dat(i)%start_date%jday
                    write(time, FMT_TIME) cm%dat(i)%start_date%hour, cm%dat(i)%start_date%mins, 0
                    write(line, FMT_GEN) &
                        cm%dat(i)%id_var, cm%dat(i)%fname, cm%dat(i)%ffmt, cm%dat(i)%hf, cm%dat(i)%nblocks, cm%dat(i)%nseries, &
                        date1, date2, time
                    call print_message(line)
                end if
            end do
            call decrease_tab()
        end if

        return

999     ENDDATA = .true.

    end function

    subroutine climate_module_parse_flag(climate_variable, input_flag, ierr)

        use strings

        !> Input variables.
        character(len = *), intent(in) :: input_flag

        !> Input/output variables.
        type(clim_series) climate_variable

        !> Output variables.
        integer ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code, args(50)
        integer n, j, z

        !> Set the return variable.
        ierr = 0

        !> Split the string.
        call parse(input_flag, ' ', args, n)

        !> Return if no arguments exist.
        if (.not. n >= 2) return

        !> Legacy numeric options.
        select case (args(2))

            !> ASCII R2C format.
            case ('1')
                climate_variable%ffmt = 1

            !> CSV format.
            case ('2')
                climate_variable%ffmt = 2

            !> Binary sequential format.
            case ('3')
                climate_variable%ffmt = 3

            !> Rank-ordered text (ASCII) format.
            case ('4')
                climate_variable%ffmt = 4

            !> Old-format frames to read in to memory.
            case ('5')
                if (n >= 4) then
                    call value(args(3), climate_variable%ffmt, z)
                    if (z /= 0) ierr = z
                    call value(args(4), climate_variable%nblocks, z)
                    if (z /= 0) ierr = z
                end if

            !> netCDF format.
            case ('7')
                climate_variable%ffmt = 7
        end select

        !> Assign the options of the flag.
        do j = 2, n

            !> Reset the local error.
            z = 0

            !> Single-word options.
            select case (lowercase(args(j)))

                !> ASCII R2C format.
                case ('r2c')
                    climate_variable%ffmt = 1

                !> CSV format.
                case ('csv')
                    climate_variable%ffmt = 2

                !> Binary sequential format.
                case ('seq')
                    climate_variable%ffmt = 3

                !> Rank-ordered text (ASCII) format.
                case ('asc')
                    climate_variable%ffmt = 4

                !> netCDF format.
                case ('nc')
                    climate_variable%ffmt = 7

                !> netCDF format (vector/subbasin).
                case ('nc_subbasin', 'nc_hru')
                    climate_variable%ffmt = 8
            end select

            !> Multi-word options.
            if (args(j)(1:3) == 'hf=') then

                !> Frame length/file time-stepping.
                call value(args(j)(4:), climate_variable%hf, z)
                if (z /= 0) ierr = z
            else if (args(j)(1:11) == 'start_date=') then

                !> First date of record.
                if (len_trim(args(j)) >= 15) then
                    call value(args(j)(12:15), climate_variable%start_date%year, z)
                    if (z == 0) then
                        if (len_trim(args(j)) >= 17) then
                            call value(args(j)(16:17), climate_variable%start_date%month, z)
                            if (z == 0) then
                                if (len_trim(args(j)) >= 19) then
                                    call value(args(j)(18:19), climate_variable%start_date%day, z)
                                    if (z == 0) then
                                        if (len_trim(args(j)) >= 21) then
                                            call value(args(j)(20:21), climate_variable%start_date%hour, z)
                                            if (z == 0) then
                                                if (len_trim(args(j)) >= 23) then
                                                    call value(args(j)(22:23), climate_variable%start_date%mins, z)
                                                else
                                                    climate_variable%start_date%mins = 0
                                                end if
                                            end if
                                        else
                                            climate_variable%start_date%hour = 0
                                        end if
                                    end if
                                else
                                    climate_variable%start_date%day = 1
                                end if
                                climate_variable%start_date%jday = &
                                    get_jday( &
                                        climate_variable%start_date%month, climate_variable%start_date%day, &
                                        climate_variable%start_date%year)
                            end if
                        else
                            climate_variable%start_date%month = 1
                        end if
                    else
                        ierr = z
                    end if
                end if
            else if (args(j)(1:4) == 'nts=') then

                !> Number of frames to read in to memory.
                call value(args(j)(5:), climate_variable%nblocks, z)
                if (z /= 0) ierr = z
            else if (args(j)(1:6) == 'fname=') then

                !> Base file name (without extension).
                climate_variable%fname = adjustl(args(j)(7:))
            else if (args(j)(1:6) == 'fpath=') then

                !> Full path including file name and extension.
                climate_variable%fpath = adjustl(args(j)(7:))
            else if (args(j)(1:9) == 'name_var=') then

                !> Variable name.
                climate_variable%id_var = adjustl(args(j)(10:))
            else if (args(j)(1:9) == 'name_lat=') then

                !> Name of latitude dimension (for specific formats).
                climate_variable%name_lat = adjustl(args(j)(10:))
            else if (args(j)(1:9) == 'name_lon=') then

                !> Name of longitude dimension (for specific formats).
                climate_variable%name_lon = adjustl(args(j)(10:))
            else if (args(j)(1:10) == 'name_time=') then

                !> Name of time dimension (for specific formats).
                climate_variable%name_time = adjustl(args(j)(11:))
            else if (args(j)(1:3) == 'cm=') then

                !> Data multiplier.
                call value(args(j)(4:), climate_variable%cm, z)
                if (z /= 0) ierr = z
            else if (args(j)(1:3) == 'ca=') then

                !> Data additive factor.
                call value(args(j)(4:), climate_variable%ca, z)
                if (z /= 0) ierr = z
            else if (args(j)(1:12) == 'n_skip_cols=') then

                !> Number of leading columns (to skip).
                call value(args(j)(13:), climate_variable%n_skip_cols, z)
                if (z /= 0) ierr = z
            else if (args(j)(1:11) == 'time_shift=') then

                !> Time shift to apply to time-stamps (for specific formats).
                call value(args(j)(12:), climate_variable%time_shift, z)
                if (z /= 0) ierr = z
            end if

            !> Check for errors.
            if (z /= 0) then
                write(code, FMT_GEN) j
                call print_warning('An error occurred parsing argument ' // trim(adjustl(code)) // ' of ' // trim(args(1)) // '.')
            end if
        end do

    end subroutine

    !> Description:
    !>  Resume states to the climate forcing state file.
    !>
    !> Input/output variables:
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    function climate_module_resume_read(fls, shd, cm) result(ENDDATA)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        use model_files_variables
        use shd_variables

        !> Input variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Input/output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer(kind = 4) i4, iblock_i4, itimestep_i4, ipflg_i4
        real(kind = 4), dimension(:, :), allocatable :: blocks_r4, ipdat_r4
        integer vid, ierr, iun
        character(len = DEFAULT_LINE_LENGTH) line

        ENDDATA = .false.

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open( &
            iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat', action = 'read', status = 'old', &
            form = 'unformatted', access = 'sequential', iostat = ierr)
        if (ierr /= 0) then
            call print_error('Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat' // ' to resume states.')
            call program_abort()
        end if

        !> Stop if the state file does not contain the expected number of climate variables.
        read(iun) i4
        if (int(i4) /= 7) then
            call print_error('Incompatible ranking in climate state file.')
            call increase_tab()
            write(line, FMT_GEN) i4
            call print_message('Number of clim. variables read: ' // trim(adjustl(line)))
            write(line, FMT_GEN) 7
            call print_message('Number of clim. variables expected: ' // trim(adjustl(line)))
            call decrease_tab()
            call program_abort()
        end if

        !> Loop through variables in the climate forcing object and read the states from file.
        do vid = 1, 7

            !> Read the state of the climate variable (in case reading into memory).
            allocate(blocks_r4(size(cm%dat(vid)%blocks, 1), size(cm%dat(vid)%blocks, 2)))
            blocks_r4 = 0.0
            read(iun) blocks_r4
            cm%dat(vid)%blocks = real(blocks_r4, kind(cm%dat(vid)%blocks))
            read(iun) iblock_i4
            cm%dat(vid)%iblock = int(iblock_i4, kind(cm%dat(vid)%iblock))
            deallocate(blocks_r4)

            !> Read the last time-step read from file.
            read(iun) itimestep_i4
            cm%dat(vid)%itimestep = int(itimestep_i4, kind(cm%dat(vid)%itimestep))

            !> Read the interpolation state (if active).
            read(iun) ipflg_i4
            cm%dat(vid)%ipflg = int(ipflg_i4, kind(cm%dat(vid)%ipflg))
            if (cm%dat(vid)%ipflg == 1) then
                allocate(ipdat_r4(size(cm%dat(vid)%ipdat, 1), size(cm%dat(vid)%ipdat, 2)))
                ipdat_r4 = 0.0
                read(iun) ipdat_r4
                cm%dat(vid)%ipdat = real(ipdat_r4, kind(cm%dat(vid)%ipdat))
                deallocate(ipdat_r4)

                !> INTERPOLATIONFLAG 1 requires an additional frame be read from the next time-step.
                if (cm%dat(vid)%itimestep == 0) then
                    if (update_data(shd, cm, vid, 0)) goto 999
                    cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                end if
            end if
        end do

        !> Close the file to free the unit.
        close(iun)

        return

999     ENDDATA = .true.

    end function

    !> Description:
    !>  Updates climate forcing data, either from memory or from file.
    !>
    !> Input/output variables:
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  ii1: Start index in the GAT vector.
    !*  ii2: Stopp index in the GAT vector.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    function climate_module_update_data(fls, shd, ii1, ii2, cm) result(ENDDATA)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        !> model_dates: 'ic' counter variable.
        use model_files_variables
        use shd_variables
        use model_dates
!-        use FLAGS

        !> Required for 'value' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams) shd
        integer, intent(in) :: ii1, ii2

        !> Input/output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer ierr, vid, t, s, k, j, i
        real rt, alpha

        ENDDATA = .false.

        !> Loop through variables in the climate forcing object.
        do vid = 1, cm%nclim

            !> Update data if the climate variable is active.
            if (cm%dat(vid)%factive) then

                !> INTERPOLATIONFLAG 1 requires an additional frame be read in the first time-step.
                if (ic%ts_count == 1 .and. cm%dat(vid)%ipflg == 1) then
                    if (update_data(shd, cm, vid, 0)) goto 999
                    cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                end if

                !> Grab data from file.
                if (cm%dat(vid)%itimestep == 0) then

                    !> Update the input forcing data.
                    if (update_data(shd, cm, vid, 0)) goto 999

                    !> Apply conditions to the series of data is such conditions exist.
                    if (cm%dat(vid)%nseries > 0) then
                        do s = 1, cm%dat(vid)%nseries
                            select case (cm%dat(vid)%series(s)%attrtype)
                                case ('gru')
                                    call value(cm%dat(vid)%series(s)%attr(1), j, ierr)
                                    call value(cm%dat(vid)%series(s)%attr(2), alpha, ierr)
                                    forall (k = ii1:ii2, shd%lc%JLMOS(k) == j)
                                        cm%dat(vid)%GAT(k) = cm%dat(vid)%GAT(k)*alpha
                                    end forall
                            end select
                        end do
                    end if

                    !> Update interpolation fields.
                    if (cm%dat(vid)%ipflg == 1) then
                        cm%dat(vid)%ipdat(:, 1) = cm%dat(vid)%ipdat(:, 2)
                        cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                    end if

                end if

                !> Interpolate intermediate values.
                if (cm%dat(vid)%ipflg == 1) then
                    cm%dat(vid)%blocks(:, cm%dat(vid)%iblock) = cm%dat(vid)%ipdat(:, 1) + &
                        min(1.0, real(cm%dat(vid)%itimestep)/cm%dat(vid)%hf)*(cm%dat(vid)%ipdat(:, 2) - cm%dat(vid)%ipdat(:, 1))
                end if

                !> Extract data from the climate variable.
                select case (cm%dat(vid)%blocktype)

                    case (1)

                        !> Block type: GRD (Grid).
                        cm%dat(vid)%GRD = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        do k = ii1, ii2
                            cm%dat(vid)%GAT(k) = cm%dat(vid)%GRD(shd%lc%ILMOS(k))
                        end do
                        do k = ii1, ii2
                            cm%dat(vid)%GRU(shd%lc%JLMOS(k)) = cm%dat(vid)%GAT(k)
                        end do

                    case (2)

                        !> Block type: GRU.
                        cm%dat(vid)%GRU = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        cm%dat(vid)%GRD = 0.0
                        do k = ii1, ii2
                            j = shd%lc%JLMOS(k)
                            i = shd%lc%ILMOS(k)
                            cm%dat(vid)%GAT(k) = cm%dat(vid)%GRU(j)
                            cm%dat(vid)%GRD(i) = cm%dat(vid)%GRD(i) + shd%lc%ACLASS(i, j)*cm%dat(vid)%GRU(j)
                        end do

                    case (3)

                        !> Block type: GAT (Land element).
                        cm%dat(vid)%GAT = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        cm%dat(vid)%GRD = 0.0
                        do k = ii1, ii2
                            j = shd%lc%JLMOS(k)
                            i = shd%lc%ILMOS(k)
                            cm%dat(vid)%GRD(i) = cm%dat(vid)%GRD(i) + shd%lc%ACLASS(i, j)*cm%dat(vid)%GAT(k)
                            cm%dat(vid)%GRU(j) = cm%dat(vid)%GAT(k)
                        end do

                    case default
                        call print_error('Unable to read blocks from ' // trim(cm%dat(vid)%fpath) // '.')
                        call program_abort()

                end select

                !> Increment the time-step of the variable.
                cm%dat(vid)%itimestep = cm%dat(vid)%itimestep + ic%dtmins
                if (cm%dat(vid)%itimestep >= cm%dat(vid)%hf) then
                    cm%dat(vid)%itimestep = 0
                end if

                !> Update the count of the current block.
                if (cm%dat(vid)%nblocks > 1 .and. cm%dat(vid)%itimestep == 0) then
                    cm%dat(vid)%iblock = cm%dat(vid)%iblock + 1
                    if (cm%dat(vid)%iblock > cm%dat(vid)%nblocks) then
                        cm%dat(vid)%iblock = 1
                    end if
                end if
            end if
        end do

        !> Advance line if 'MET' format file is active (special condition).
        if (cm%dat(ck%MET)%factive .and. cm%dat(ck%MET)%itimestep == 0) then
            if (update_data(shd, cm, ck%MET, 1)) goto 999
        end if

        return

999     ENDDATA = .true.

    end function

    !> Description:
    !>  Saves states to the climate forcing state file.
    !>
    !> Input/output variables:
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    subroutine climate_module_resume_save(fls, shd, cm)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        use model_files_variables
        use shd_variables

        !> mpi_module: 'ipid' variable to identify node.
        use mpi_module

        !> Input variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Input/output variables.
        type(clim_info) cm

        !> Local variables.
        integer vid, ierr, iun

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open( &
            iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat', action = 'write', status = 'replace', &
            form = 'unformatted', access = 'sequential', iostat = ierr)
        if (ierr /= 0) then
            call print_error('Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat' // ' to save states.')
            call program_abort()
        end if

        !> Write the number of climate variables.
        write(iun) int(7, kind = 4)

        !> Loop through variables in the climate forcing object and write the states to file.
        do vid = 1, 7

            !> Save the state of the climate variable (in case reading into memory).
            write(iun) real(cm%dat(vid)%blocks, kind = 4)
            write(iun) int(cm%dat(vid)%iblock, kind = 4)

            !> Save the current time-step read from file.
            write(iun) int(cm%dat(vid)%itimestep, kind = 4)

            !> Save the interpolation state (if active).
            write(iun) int(cm%dat(vid)%ipflg, kind = 4)
            if (cm%dat(vid)%ipflg == 1) then
                write(iun) real(cm%dat(vid)%ipdat, kind = 4)
            end if
        end do

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    !> Description:
    !>  Clean-up at the end of the run.
    !>
    !> Input/output variables:
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    subroutine climate_module_finalize(fls, shd, cm)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        use model_files_variables
        use shd_variables

        !> mpi_module: 'ipid' variable to identify node.
        use mpi_module

        !> Input variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Input/output variables.
        type(clim_info) cm

    end subroutine

end module
