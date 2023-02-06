!> Description:
!>  Subroutine to resume the run state from file.
subroutine resumerun_read(fls, shd, cm, ierr)

    use mesh_io_options
    use sa_mesh_common
    use climate_forcing
    use sa_mesh_run_within_tile
    use sa_mesh_run_within_grid
    use sa_mesh_run_between_grid

    !> Process modules.
    use FLAGS, only: NRSOILAYEREADFLAG
    use RUNCLASS36_constants
    use RUNCLASS36_config
    use RUNCLASS36_variables
    use runsvs_mesh
    use WF_ROUTE_config
    use area_watflood, only: fhr
    use rte_module
    use baseflow_module
    use save_basin_output
    use SIMSTATS
!>>>>>zone-based storage
    use FLAGS, only: RESERVOIRFLAG
    use reservoir, only: resrvs
!<<<<<zone-based storage

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd
    type(clim_info) cm

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer iun, ignd, k, j, i, m, z
    character(len = DEFAULT_LINE_LENGTH) args(100), line, fname
    logical lstate

    !> Initialize the return status.
    ierr = 0

    !> Reset spacing for screen output.
    call reset_tab()

    !> Distribute GRU-based values.
    do k = 1, shd%lc%NML

        !> Grab the indices of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> RUNCLASS36 and RUNSVS113.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
            vs%tile%tcan(k) = vs%gru%tcan(m) + TFREZ
            vs%tile%tsno(k) = vs%gru%tsno(m) + TFREZ
            vs%tile%rhosno(k) = vs%gru%rhosno(m)
            vs%tile%albsno(k) = vs%gru%albsno(m)
            vs%tile%tsol(k, :) = vs%gru%tsol(m, :) + TFREZ
            vs%tile%thlqsol(k, :) = vs%gru%thlqsol(m, :)
            vs%tile%thicsol(k, :) = vs%gru%thicsol(m, :)
        end if

        !> RUNCLASS36.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
            vs%tile%tacan(k) = vs%gru%tcan(m) + TFREZ
            vs%tile%qacan(k) = 0.5e-2
            vs%tile%tpnd(k) = vs%gru%tpnd(m) + TFREZ
            vs%tile%zpnd(k) = vs%gru%zpnd(m)
            vs%tile%lqwscan(k) = vs%gru%lqwscan(m)
            vs%tile%fzwscan(k) = vs%gru%fzwscan(m)
            vs%tile%sno(k) = vs%gru%sno(m)
            vs%tile%gro(k) = vs%gru%gro(m)
            vs%tile%tsfs(k, 1) = TFREZ
            vs%tile%tsfs(k, 2) = TFREZ
            vs%tile%tsfs(k, 3) = vs%gru%tsol(m, 1) + TFREZ
            vs%tile%tsfs(k, 4) = vs%gru%tsol(m, 1) + TFREZ
            vs%tile%tbas(k) = vs%gru%tsol(m, shd%lc%IGND) + TFREZ
        end if

    end do !k = 1, shd%lc%NML

    !> Distribute soil states to layers lower than the "last configured layer".
    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then

        !> Determine the "last configured layer" read from file (CLASS default: 3).
        if (NRSOILAYEREADFLAG > 3) then
            ignd = min(NRSOILAYEREADFLAG, shd%lc%IGND)
        else if (NRSOILAYEREADFLAG == 1) then
            ignd = 0
        else
            ignd = 3
        end if

        !> Assign states to layers lower than the "last configured layer" read from file.
        if (ignd > 0) then
            do j = (ignd + 1), shd%lc%IGND
                vs%tile%tsol(:, j) = vs%tile%tsol(:, ignd)
                vs%tile%thlqsol(:, j) = vs%tile%thlqsol(:, ignd)
                vs%tile%thicsol(:, j) = vs%tile%thicsol(:, ignd)
            end do
        end if
    end if

!?    !> Check for auto resume file.
!?    if (vs%flgs%resume%state == FLAG_AUTO) then
!?        fname = 'auto_resume.ini'
!?!+        call reset_tab()
!?        call print_message('READING: ' // trim(fname))
!?!+        call increase_tab()
!?        inquire(file = fname, exist = lstate)
!?        if (lstate) then
!?
!?            !> Open the file.
!?            iun = 100
!?            open(iun, file = fname, status = 'old', action = 'read', iostat = z)
!?            if (z /= 0) then
!?                call print_error('Unable to open the file.')
!?                call program_abort()
!?            end if
!?
!?            !> Read the simulation start date from the file.
!?            read(iun, *, iostat = z) ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
!?            if (z /= 0) then
!?                call print_error('An error occurred reading the simulation resume date from the file.')
!?                call program_abort()
!?            end if
!?            write(line, "(i5, '/', i3.3, ' ', i2.2, ':', i2.2)") ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
!?            call print_message( &
!?                'Simulation start revised to: ' // trim(adjustl(line)) // '. The previous run state will be resumed.')
!?            close(iun)
!?        else
!?
!?            !> Print a warning if the resume file does not exist.
!?            call print_warning( &
!?                'Auto-resume is active but ' // trim(fname) // ' cannot be found. No previous run state is resumed.')
!?
!?            !> Override the resume functionality.
!?            vs%flgs%resume%state = FLAG_OFF
!?        end if
!?    end if

    !> Read files.
    if (.not. vs%flgs%resume%state == FLAG_OFF) then

        !> txt: In text format.

        !> seq: Sequential binary format.
        if (btest(vs%flgs%resume%flo%ext, FILE_TYPE_SEQ)) then

            !> Append the date to the default resume filename for auto resume.
            if (vs%flgs%resume%state == FLAG_AUTO) then
                write(line, "(i4.4, '_', i3.3)") ic%start%year, ic%start%jday
                fname = fls%fl(mfk%f883)%fn
                fls%fl(mfk%f883)%fn = trim(fname(1:index(fname, '.'))) // trim(adjustl(line)) // trim(fname(index(fname, '.'):))
            end if
            if (index(vs%flgs%resume%bin, '+STASONLY') == 0 .and. index(vs%flgs%resume%bin, '+CLASSPROG') == 0) then
                lstate = climate_module_resume_read(fls, shd, cm)
                call read_init_prog_variables_class(fls, shd)
                call runsvs_mesh_resume_states_seq(fls, shd, resume_ts = .true.)
                call bflm_resume_read(fls, shd)
                call WF_ROUTE_resume_read(fls, shd)
                call run_rte_resume_read(fls, shd)
                call run_save_basin_output_resume_read(fls, shd)
                call stats_state_resume(fls)
!>>>>>zone-based storage
                if (RESERVOIRFLAG == 2) then
                    iun = 100
                    if (vs%flgs%resume%state == FLAG_AUTO) then
                        open(iun, file = 'zone_storage_states.' // trim(adjustl(line)) // '.txt', action = 'read', status = 'old')
                    else
                        open(iun, file = 'zone_storage_states.txt', action = 'read', status = 'old')
                    end if
                    read(iun, *) (resrvs%rsvr(i)%stoSIM(1), i = 1, resrvs%nreserv)
                    read(iun, *) (resrvs%rsvr(i)%flowSIM(1), i = 1, resrvs%nreserv)
                    close(iun)
                end if
!<<<<<zone-based storage
            else if (index(vs%flgs%resume%bin, '+CLASSPROG') == 0) then
                call read_init_prog_variables_class(fls, shd)
                call runsvs_mesh_resume_states_seq(fls, shd, resume_ts = .false.)
                call bflm_resume_read(fls, shd)
                call WF_ROUTE_resume_read_nots(fls, shd)
                call run_rte_resume_read_nots(fls, shd)
!>>>>>zone-based storage
                if (RESERVOIRFLAG == 2) then
                    iun = 100
                    if (vs%flgs%resume%state == FLAG_AUTO) then
                        open(iun, file = 'zone_storage_states.' // trim(adjustl(line)) // '.txt', action = 'read', status = 'old')
                    else
                        open(iun, file = 'zone_storage_states.txt', action = 'read', status = 'old')
                    end if
                    read(iun, *) (resrvs%rsvr(i)%stoSIM(1), i = 1, resrvs%nreserv)
                    read(iun, *) (resrvs%rsvr(i)%flowSIM(1), i = 1, resrvs%nreserv)
                    close(iun)
                end if
!<<<<<zone-based storage
            else
                call read_init_prog_variables_class_row(fls, shd)
                call runsvs_mesh_resume_states_seq(fls, shd, resume_ts = .false.)
            end if
            if (vs%flgs%resume%state == FLAG_AUTO) then
                fls%fl(mfk%f883)%fn = fname
            end if
        end if

        !> r2c: From r2c by grid.

        !> csv: From CSV by GRU.

        !> NetCDF.
        if (btest(vs%flgs%resume%flo%ext, FILE_TYPE_NC4)) then
            fname = 'MESH_initial_values.nc'
            if (vs%flgs%resume%state == FLAG_AUTO) then
                write(line, "(i4.4, '_', i3.3)") ic%start%year, ic%start%jday
                fname = trim(fname(1:index(fname, '.'))) // trim(adjustl(line)) // trim(fname(index(fname, '.'):))
            end if
            call read_initial_states_nc(fls, shd, trim(fname), ierr)
            if (ierr /= 0) then
                call print_error("An error occurred reading variables from the file '" // trim(fname) // "'.")
                call program_abort()
            end if
        end if
    end if

    !> Update derived values.
    call run_within_tile_stas_update(fls, shd, cm)
    call run_within_grid_stas_update(fls, shd, cm)
    call run_within_grid_stas_basin_update(fls, shd, cm)

end subroutine
