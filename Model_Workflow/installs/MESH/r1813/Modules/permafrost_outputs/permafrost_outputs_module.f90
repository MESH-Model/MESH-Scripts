module permafrost_outputs_module

    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'model_dates' required for 'ic' counter.
    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use sa_mesh_common
    use model_dates
    use model_files_variables
    use mpi_module

    implicit none

    !> Variable names.
    character(len = *), parameter :: PMFRSTVN_ALT = 'ALT'
    character(len = *), parameter :: PMFRSTVN_ALTDOY = 'ALT_DOY'
    character(len = *), parameter :: PMFRSTVN_ALTENV = 'ALT_ENV'
    character(len = *), parameter :: PMFRSTVN_TAVG = 'TAVG'
    character(len = *), parameter :: PMFRSTVN_TMAX = 'TMAX'
    character(len = *), parameter :: PMFRSTVN_TMIN = 'TMIN'
    character(len = *), parameter :: PMFRSTVN_TRNG = 'TRNG'
    character(len = *), parameter :: PMFRSTVN_DZAA = 'DZAA'

    !> Description:
    !>  Data type for parameters.
    !>
    !> Variables:
    !*  dzaa_ttol: Temperature threshold for zero oscillation depth. [K].
    type permafrost_outputs_parameters
        real, dimension(:), allocatable :: dzaa_ttol
    end type

    !> Description:
    !>  Data type for variables.
    !>
    !> Variables:
    !*  alt: Active layer thickness calculated using daily average temperature (1: Tile index). [m].
    !*  altdoy: Day of year when ALT is observed (1: Tile index). [--].
    !*  altenv: Active layer thickness calculated using the annual temperature envelope (1: Tile index). [m].
    !*  tavg: Average daily soil temperature (1: Tile index; 2: Soil layer). [K].
    !*  tmax: Annual maximum of daily soil temperature (1: Tile index; 2: Soil layer). [K].
    !*  tmin: Annual minimum of daily soil temperature (1: Tile index; 2: Soil layer). [K].
    !*  trng: Range/envelope of the annual maximum and minimum soil temperatures (1: Tile index; 2: Soil layer). [K].
    !*  dzaa: Depth of zero annual amplitude, where the range/envelope of the annual maximum and minimum soil temperatures in within the threshold (1: Tile index; 2: TTOL). [m].
    type permafrost_outputs_fields
        type(output_fields_surrogate) alt, altdoy, altenv
        type(output_fields_surrogate), dimension(:), allocatable :: tavg, tmax, tmin, trng
        type(output_fields_surrogate), dimension(:), allocatable :: dzaa
    end type

    !> Description:
    !>  Container for flags, parameters, and variables.
    !>
    !> Variables:
    !*  pm: Parameter group.
    !*  y, m, d: Output interval of variables. [--].
    type permafrost_outputs_container
        logical :: PROCESS_ACTIVE = .false.
        type(permafrost_outputs_parameters) pm
        type(permafrost_outputs_fields) out
    end type

    type(permafrost_outputs_container), save :: prmfst

    contains

    subroutine permafrost_outputs_init(fls, shd, vname)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        character(len = *), intent(in) :: vname

        !> Local variables.
        integer na, nml, nsl, n, j, ierr

        !> Local variables.
        na = shd%NA; nml = shd%lc%NML; nsl = shd%lc%IGND

        !> Check if model dependencies are active.
        if (.not. associated(vs%tile%tsol) .or. .not. associated(vs%grid%tsol)) then
            call print_error( &
                "The active configuration for permafrost outputs is not valid. A necessary dependency for the output " // &
                "of variable '" // trim(vname) // "' is not active.")
            call program_abort()
        end if

        !> TAVG and temperature statistics (for all outputs).
        if (.not. allocated(prmfst%out%tavg)) then
            allocate(prmfst%out%tavg(nsl), prmfst%out%tmax(nsl), prmfst%out%tmin(nsl), prmfst%out%trng(nsl))
            do j = 1, shd%lc%IGND
                allocate( &
                    prmfst%out%tavg(j)%y_tile(nml), prmfst%out%tavg(j)%d_tile(nml), &
                    prmfst%out%tavg(j)%y_grid(na), prmfst%out%tavg(j)%d_grid(na))
                prmfst%out%tavg(j)%y_tile = 0.0; prmfst%out%tavg(j)%d_tile = 0.0
                prmfst%out%tavg(j)%y_grid = 0.0; prmfst%out%tavg(j)%d_grid = 0.0

                !> TMAX.
                allocate(prmfst%out%tmax(j)%y_tile(nml), prmfst%out%tmax(j)%y_grid(na))
                prmfst%out%tmax(j)%y_tile = 100.0; prmfst%out%tmax(j)%y_grid = 100.0

                !> TMIN.
                allocate(prmfst%out%tmin(j)%y_tile(nml), prmfst%out%tmin(j)%y_grid(na))
                prmfst%out%tmin(j)%y_tile = 900.0; prmfst%out%tmin(j)%y_grid = 900.0

                !> TRNG.
                allocate(prmfst%out%trng(j)%y_tile(nml), prmfst%out%trng(j)%y_grid(na))
                prmfst%out%trng(j)%y_tile = 0.0; prmfst%out%trng(j)%y_grid = 0.0
            end do
        end if

        !> ALT and ALT_DOY.
        if (vname == PMFRSTVN_ALT .or. vname == PMFRSTVN_ALTDOY) then
            if (.not. associated(prmfst%out%alt%d_tile)) then
                allocate( &
                    prmfst%out%alt%y_tile(nml), prmfst%out%alt%d_tile(nml), &
                    prmfst%out%alt%y_grid(na), prmfst%out%alt%d_grid(na))
                prmfst%out%alt%y_tile = 0.0; prmfst%out%alt%d_tile = 0.0
                prmfst%out%alt%y_grid = 0.0; prmfst%out%alt%d_grid = 0.0
                allocate(prmfst%out%altdoy%y_tile(nml), prmfst%out%altdoy%y_grid(na))
                prmfst%out%altdoy%y_tile = 0.0; prmfst%out%altdoy%y_grid = 0.0
            end if
        end if

        !> ALT_ENV.
        if (vname == PMFRSTVN_ALTENV) then
            if (.not. associated(prmfst%out%altenv%y_tile)) then
                allocate(prmfst%out%altenv%y_tile(nml), prmfst%out%altenv%y_grid(na))
                prmfst%out%altenv%y_tile = 0.0; prmfst%out%altenv%y_grid = 0.0
            end if
        end if

        !> DZAA.
        if (vname == PMFRSTVN_DZAA) then

            !> Set zero tolerance if none were specified.
            if (.not. allocated(prmfst%pm%dzaa_ttol)) then
                allocate(prmfst%pm%dzaa_ttol(1))
                prmfst%pm%dzaa_ttol(1) = 0.1
            end if
            if (.not. allocated(prmfst%out%dzaa)) then
                allocate(prmfst%out%dzaa(size(prmfst%pm%dzaa_ttol)))
                do j = 1, size(prmfst%pm%dzaa_ttol)
                    if (.not. associated(prmfst%out%dzaa(j)%y_tile)) then
                        allocate(prmfst%out%dzaa(j)%y_tile(nml), prmfst%out%dzaa(j)%y_grid(na))
                        prmfst%out%dzaa(j)%y_tile = 0.0; prmfst%out%dzaa(j)%y_grid = 0.0
                    end if
                end do
            end if
        end if

        !> Enable the routine.
        prmfst%PROCESS_ACTIVE = .true.

    end subroutine

    subroutine permafrost_outputs_update(fls, shd)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer j, k, n
        real zbot(shd%lc%IGND)
        real tavg_tile(shd%lc%NML, shd%lc%IGND), tmax_tile(shd%lc%NML, shd%lc%IGND), tmin_tile(shd%lc%NML, shd%lc%IGND)
        real tavg_grid(shd%NA, shd%lc%IGND), tmax_grid(shd%NA, shd%lc%IGND), tmin_grid(shd%NA, shd%lc%IGND)

        !> Return if the process is not enabled.
        if (.not. prmfst%PROCESS_ACTIVE .or. ipid /= 0) return

        !> Local variables.
        zbot = shd%lc%sl%zbot

        !> Update daily temperature values.
        do j = 1, shd%lc%IGND

            !> Reset daily and yearly values if the first time-step in the day and year.
            if (ic%ts_daily == 1) then
                prmfst%out%tavg(j)%d_tile = 0.0
                prmfst%out%tavg(j)%d_grid = 0.0
            end if
            if (ic%ts_yearly == 1) then
                prmfst%out%tavg(j)%y_tile = 0.0; prmfst%out%tmax(j)%y_tile = 100.0; prmfst%out%tmin(j)%y_tile = 900.0
                prmfst%out%tavg(j)%y_grid = 0.0; prmfst%out%tmax(j)%y_grid = 100.0; prmfst%out%tmin(j)%y_grid = 900.0
                if (associated(prmfst%out%alt%d_tile)) then
                    prmfst%out%alt%y_tile = 0.0
                    prmfst%out%alt%y_grid = 0.0
                    prmfst%out%altdoy%y_tile = 0.0
                    prmfst%out%altdoy%y_grid = 0.0
                end if
                if (associated(prmfst%out%altenv%y_tile)) then
                    prmfst%out%altenv%y_tile = 0.0
                    prmfst%out%altenv%y_grid = 0.0
                end if
            end if

            !> Tile-based.
            where (vs%tile%tsol(:, j) > 173.16 .and. vs%tile%tsol(:, j) < 373.16)
                prmfst%out%tavg(j)%d_tile = prmfst%out%tavg(j)%d_tile + vs%tile%tsol(:, j)
            elsewhere
                prmfst%out%tavg(j)%d_tile = out%NO_DATA
            end where

            !> Grid-based.
            where (vs%grid%tsol(:, j) > 173.16 .and. vs%grid%tsol(:, j) < 373.16)
                prmfst%out%tavg(j)%d_grid = prmfst%out%tavg(j)%d_grid + vs%grid%tsol(:, j)
            elsewhere
                prmfst%out%tavg(j)%d_grid = out%NO_DATA
            end where
        end do

        !> End of day outputs (daily).
        if (ic%now%day /= ic%next%day) then

            !> Calculate statistics and transform the variables to an array compatible with the function call.
            do j = 1, shd%lc%IGND

                !> Tile-based.
                where (prmfst%out%tavg(j)%d_tile /= out%NO_DATA)
                    prmfst%out%tavg(j)%d_tile = prmfst%out%tavg(j)%d_tile/ic%ts_daily
                    tavg_tile(:, j) = prmfst%out%tavg(j)%d_tile
                elsewhere
                    tavg_tile(:, j) = 0.0
                    prmfst%out%tavg(j)%d_tile = out%NO_DATA
                end where

                !> Grid-based.
                where (prmfst%out%tavg(j)%d_grid /= out%NO_DATA)
                    prmfst%out%tavg(j)%d_grid = prmfst%out%tavg(j)%d_grid/ic%ts_daily
                    tavg_grid(:, j) = prmfst%out%tavg(j)%d_grid
                elsewhere
                    tavg_grid(:, j) = 0.0
                    prmfst%out%tavg(j)%d_grid = out%NO_DATA
                end where
            end do

            !> Calculate ALT using daily average temperature (assign NO_DATA value if ALT == 0.0).
            if (associated(prmfst%out%alt%d_tile)) then
                call permafrost_alt(tavg_tile, zbot, prmfst%out%alt%d_tile, shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                where (.not. prmfst%out%alt%d_tile > 0.0) prmfst%out%alt%d_tile = out%NO_DATA
                call permafrost_alt(tavg_grid, zbot, prmfst%out%alt%d_grid, shd%NA, shd%lc%IGND, 1, shd%NA)
                where (.not. prmfst%out%alt%d_grid > 0.0) prmfst%out%alt%d_grid = out%NO_DATA

                !> Store the day when ALT occurs for yearly output.
                where (prmfst%out%alt%d_tile > prmfst%out%alt%y_tile)
                    prmfst%out%alt%y_tile = prmfst%out%alt%d_tile
                    prmfst%out%altdoy%y_tile = ic%now%jday
                end where
                where (prmfst%out%alt%d_grid > prmfst%out%alt%y_grid)
                    prmfst%out%alt%y_grid = prmfst%out%alt%d_grid
                    prmfst%out%altdoy%y_grid = ic%now%jday
                end where
            end if

            !> Yearly statistics (based on daily values).
            do j = 1, shd%lc%IGND

                !> Tile-based.
                where (prmfst%out%tavg(j)%d_tile /= out%NO_DATA)
                    prmfst%out%tavg(j)%y_tile = prmfst%out%tavg(j)%y_tile + prmfst%out%tavg(j)%d_tile*ic%ts_daily
                    prmfst%out%tmax(j)%y_tile = max(prmfst%out%tmax(j)%y_tile, prmfst%out%tavg(j)%d_tile)
                    prmfst%out%tmin(j)%y_tile = min(prmfst%out%tmin(j)%y_tile, prmfst%out%tavg(j)%d_tile)
                elsewhere
                    prmfst%out%tavg(j)%y_tile = out%NO_DATA
                end where

                !> Grid-based.
                where (prmfst%out%tavg(j)%d_grid /= out%NO_DATA)
                    prmfst%out%tavg(j)%y_grid = prmfst%out%tavg(j)%y_grid + prmfst%out%tavg(j)%d_grid*ic%ts_daily
                    prmfst%out%tmax(j)%y_grid = max(prmfst%out%tmax(j)%y_grid, prmfst%out%tavg(j)%d_grid)
                    prmfst%out%tmin(j)%y_grid = min(prmfst%out%tmin(j)%y_grid, prmfst%out%tavg(j)%d_grid)
                elsewhere
                    prmfst%out%tavg(j)%y_grid = out%NO_DATA
                end where
            end do

            !> End of year outputs (yearly).
            if (ic%now%year /= ic%next%year) then

                !> Calculate statistics and transform the variables to an array compatible with the function call.
                do j = 1, shd%lc%IGND

                    !> Tile-based.
                    where (prmfst%out%tavg(j)%y_tile /= out%NO_DATA)
                        prmfst%out%tavg(j)%y_tile = prmfst%out%tavg(j)%y_tile/ic%ts_yearly
                        prmfst%out%trng(j)%y_tile = prmfst%out%tmax(j)%y_tile - prmfst%out%tmin(j)%y_tile
                        tavg_tile(:, j) = prmfst%out%tavg(j)%y_tile
                        tmax_tile(:, j) = prmfst%out%tmax(j)%y_tile
                        tmin_tile(:, j) = prmfst%out%tmin(j)%y_tile
                    elsewhere
                        tavg_tile(:, j) = 0.0
                        tmax_tile(:, j) = 0.0
                        tmin_tile(:, j) = 0.0
                        prmfst%out%tavg(j)%y_tile = out%NO_DATA
                        prmfst%out%tmax(j)%y_tile = out%NO_DATA
                        prmfst%out%tmin(j)%y_tile = out%NO_DATA
                        prmfst%out%trng(j)%y_tile = out%NO_DATA
                    end where

                    !> Grid-based.
                    where (prmfst%out%tavg(j)%y_grid /= out%NO_DATA)
                        prmfst%out%tavg(j)%y_grid = prmfst%out%tavg(j)%y_grid/ic%ts_yearly
                        prmfst%out%trng(j)%y_grid = prmfst%out%tmax(j)%y_grid - prmfst%out%tmin(j)%y_grid
                        tavg_grid(:, j) = prmfst%out%tavg(j)%y_grid
                        tmax_grid(:, j) = prmfst%out%tmax(j)%y_grid
                        tmin_grid(:, j) = prmfst%out%tmin(j)%y_grid
                    elsewhere
                        tavg_grid(:, j) = 0.0
                        tmax_grid(:, j) = 0.0
                        tmin_grid(:, j) = 0.0
                        prmfst%out%tavg(j)%y_grid = out%NO_DATA
                        prmfst%out%tmax(j)%y_grid = out%NO_DATA
                        prmfst%out%tmin(j)%y_grid = out%NO_DATA
                        prmfst%out%trng(j)%y_grid = out%NO_DATA
                    end where
                end do

                !> Calculate ALT using annual temperature envelope (assign NO_DATA value if ALT == 0.0).
                if (associated(prmfst%out%altenv%y_tile)) then
                    call permafrost_alt(tmax_tile, zbot, prmfst%out%altenv%y_tile, shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                    where (.not. prmfst%out%altenv%y_tile > 0.0) prmfst%out%altenv%y_tile = out%NO_DATA
                    call permafrost_alt(tmax_grid, zbot, prmfst%out%altenv%y_grid, shd%NA, shd%lc%IGND, 1, shd%NA)
                    where (.not. prmfst%out%altenv%y_grid > 0.0) prmfst%out%altenv%y_grid = out%NO_DATA
                end if

                !> Assign NO_DATA value where ALT fields based on daily temperature equal zero.
                if (associated(prmfst%out%alt%d_tile)) then
                    where (.not. prmfst%out%alt%y_tile > 0.0) prmfst%out%alt%y_tile = out%NO_DATA
                    where (.not. prmfst%out%alt%y_grid > 0.0) prmfst%out%alt%y_grid = out%NO_DATA
                    where (.not. prmfst%out%altdoy%y_tile > 0.0) prmfst%out%altdoy%y_tile = out%NO_DATA
                    where (.not. prmfst%out%altdoy%y_grid > 0.0) prmfst%out%altdoy%y_grid = out%NO_DATA
                end if

                !> Calculate DZAA using annual temperature envelope (assign NO_DATA value if DZAA == 0.0).
                if (allocated(prmfst%out%dzaa)) then
                    do j = 1, size(prmfst%pm%dzaa_ttol)

                        !> Tile-based.
                        call permafrost_dzaa( &
                            tmax_tile, tmin_tile, zbot, prmfst%pm%dzaa_ttol(j), prmfst%out%dzaa(j)%y_tile, &
                            shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                        where (.not. prmfst%out%dzaa(j)%y_tile > 0.0) prmfst%out%dzaa(j)%y_tile = out%NO_DATA

                        !> Grid-based.
                        call permafrost_dzaa( &
                            tmax_grid, tmin_grid, zbot, prmfst%pm%dzaa_ttol(j), prmfst%out%dzaa(j)%y_grid, &
                            shd%NA, shd%lc%IGND, 1, shd%NA)
                        where (.not. prmfst%out%dzaa(j)%y_grid > 0.0) prmfst%out%dzaa(j)%y_grid = out%NO_DATA
                    end do
                end if
            end if
        end if

    end subroutine

end module
