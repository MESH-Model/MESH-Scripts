!> Description:
!>  Subroutine to read parameters from a netCDF 'nc' format file.
!>  Values are parsed by order of RANK and stored in variables that must
!>  allocated 1:NA.
!>
!> Input:
!*  shd: Basin 'shed' object (properties).
!*  fname: Full path to the file.
!*  dim_x_name: Name of the 'x' dimension (optional override).
!*  dim_y_name: Name of the 'y' dimension (optional override).
!*  dim_gru_name: Name of the 'GRU' dimension (optional override).
!*  dim_can_name: Name of the 'canopy' dimension (optional override).
!*  dim_vf_name: Name of the 'VF' dimension (optional override).
!*  dim_sol_name: Name of the 'soil' dimension (optional override).
!*
!> Output variables:
!*  ierr: Return status.
subroutine read_parameters_nc( &
    shd, fname, &
    dim_x_name, dim_y_name, dim_gru_name, dim_can_name, dim_vf_name, dim_sol_name, &
    ierr)

    !> strings: For 'lowercase' function.
    !> sa_mesh_common: For common MESH variables and routines.
    !> nc_io: For routines to read netCDF 'nc' format file.
    !> parse_utilities: For parsing utilities and flags.
    use strings
    use sa_mesh_common
    use nc_io
    use parse_utilities

    !> Process modules: Required for process variables, parameters.
    use RUNCLASS36_variables
    use runsvs_mesh
    use baseflow_module
    use rte_module
    use PBSM_module
    use mountain_module

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    character(len = *), intent(in) :: fname

    !> Input variables (optional).
    character(len = *), intent(in), optional :: dim_x_name, dim_y_name, dim_gru_name, dim_can_name, dim_vf_name, dim_sol_name

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables (reading).
    character(len = DEFAULT_FIELD_LENGTH) :: &
        dim_x = '', dim_y = '', dim_m = '', dim_c = '', dim_v = '', dim_s = '', projection, units, field, code
    real, allocatable :: dat_r(:), dat2_r(:, :), dat3_r(:, :, :)
    real fill_r
    real, parameter :: deg_threshold = 1.0E-4
    integer, allocatable :: nvf_levels(:), dat2_i(:, :)
    integer :: ngru = -1, ncan = -1, nvf = -1, nsol = -1, fill_i, iun, nvars, ndims, ncount, m, n, k, i, z
    logical ltest

    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Open the file.
    call nc4_open_input(fname, iun = iun, ierr = ierr)
    if (ierr /= 0) return

    !> Get the projection.
    call nc4_get_projection(iun, projection, ierr = ierr)
    if (ierr /= 0) then
        goto 999
    else if (lowercase(projection) /= lowercase(shd%CoordSys%Proj)) then
        call print_error( &
            "The projection of the file '" // trim(projection) // "' does not match the expected projection of '" // &
            trim(shd%CoordSys%Proj) // "' of the domain.")
        ierr = 1
        goto 999
    else
        if (DIAGNOSEMODE) call print_message("The projection of the file is '" // trim(shd%CoordSys%Proj) // "'.")
    end if

    !> Check bounds.
    if (present(dim_x_name)) dim_x = dim_x_name
    if (present(dim_y_name)) dim_y = dim_y_name
    select case (shd%CoordSys%Proj)
        case ('LATLONG')

            !> Check longitude values.
            if (len_trim(dim_x) == 0) dim_x = 'lon'
            call nc4_get_variable(iun, dim_x, dat = dat_r, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                ierr = z
            else
                ltest = (size(dat_r) == shd%xCount)
                if (ltest) then
                    do n = 1, shd%NA
                        ltest = (ltest .and. (shd%CoordSys%lon(n) == dat_r(shd%xxx(n))))
                    end do
                end if
                if (.not. ltest) then
                    call print_error("The longitudinal reference in the file ('" // trim(dim_x) // "') does not match the domain.")
                    ierr = 1
                end if
            end if
            deallocate(dat_r)

            !> Check latitude values.
            if (len_trim(dim_y) == 0) dim_y = 'lat'
            call nc4_get_variable(iun, dim_y, dat = dat_r, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                ierr = z
            else
                ltest = (size(dat_r) == shd%yCount)
                if (ltest) then
                    do n = 1, shd%NA
                        ltest = (ltest .and. (shd%CoordSys%lat(n) == dat_r(shd%yyy(n))))
                    end do
                end if
                if (.not. ltest) then
                    call print_error("The latitudinal reference in the file ('" // trim(dim_y) // "') does not match the domain.")
                    ierr = 1
                end if
            end if
        case ('rotated_latitude_longitude')

            !> Check rotated longitude values.
            if (len_trim(dim_x) == 0) dim_x = 'rlon'
            call nc4_get_variable(iun, dim_x, dat = dat_r, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                ierr = z
            else
                ltest = (size(dat_r) == shd%xCount)
                if (ltest) then
                    do i = 1, shd%xCount
                        ltest = (ltest .and. (shd%CoordSys%rlon(i) == dat_r(i)))
                    end do
                end if
                if (.not. ltest) then
                    call print_error("The longitudinal reference in the file ('" // trim(dim_x) // "') does not match the domain.")
                    ierr = 1
                end if
            end if
            deallocate(dat_r)

            !> Check rotated latitude values.
            if (len_trim(dim_y) == 0) dim_y = 'rlat'
            call nc4_get_variable(iun, dim_y, dat = dat_r, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                ierr = z
            else
                ltest = (size(dat_r) == shd%yCount)
                if (ltest) then
                    do i = 1, shd%yCount
                        ltest = (ltest .and. (shd%CoordSys%rlat(i) == dat_r(i)))
                    end do
                end if
                if (.not. ltest) then
                    call print_error("The latitudinal reference in the file ('" // trim(dim_y) // "') does not match the domain.")
                    ierr = 1
                end if
            end if
    end select
    if (ierr /= 0) goto 999

    !> Get the number of variables in the file.
    call nc4_inquire_file(iun, nvars = nvars, ierr = ierr)
    if (ierr /= 0) goto 999

    !> Get and validate dimensions (if present in the file).
    if (present(dim_gru_name)) dim_m = dim_gru_name
    if (len_trim(dim_m) == 0) dim_m = 'ngru'
    if (nc4_inquire_dimension(iun, dim_m)) then
        call nc4_get_dimension(iun, dim_m, dim_length = ngru, ierr = z)
        if (z /= 0) then
            ngru = -1
        else if (ngru /= shd%lc%NTYPE) then
            write(field, FMT_GEN) shd%lc%NTYPE
            call print_error("The number of GRUs '" // trim(dim_m) // "' does not match the expected value of " // trim(adjustl(field)) // ".")
            ierr = 1
        end if
    end if
    if (present(dim_can_name)) dim_c = dim_can_name
    if (len_trim(dim_c) == 0) dim_c = 'ncan'
    if (nc4_inquire_dimension(iun, dim_c)) then
        call nc4_get_dimension(iun, dim_c, dim_length = ncan, ierr = z)
        if (z /= 0) then
            ncan = -1
        else if (ncan /= 5) then
            call print_error("The number of canopies '" // trim(dim_c) // "' does not match the expected value of 5.")
            ierr = 1
        end if
    end if
    if (present(dim_vf_name)) dim_v = dim_vf_name
    if (len_trim(dim_v) == 0) dim_v = 'nvf'
    if (nc4_inquire_dimension(iun, dim_v)) then
        call nc4_get_dimension(iun, dim_v, dim_length = nvf, ierr = z)
        if (z /= 0) then
            nvf = -1
        else if (nvf /= 26) then
            call print_error("The number of vegetation types '" // trim(dim_v) // "' does not match the expected value of 26.")
            ierr = 1
        else
            allocate(nvf_levels(nvf))
            call nc4_get_variable(iun, dim_v, dat = nvf_levels, fill = fill_i, ierr = z)
            if (z /= 0) then
                nvf = -1
                deallocate(nvf_levels)
                write(code, FMT_GEN) z
                call print_error( &
                    "The IDs of the 'VF' vegetation types could not be read from the file (Code: " // trim(adjustl(code)) // ").")
                ierr = 1
            else if (maxval(nvf_levels) > 1199 .or. minval(nvf_levels) < 1174) then
                write(field, FMT_GEN) maxval(nvf_levels)
                write(code, FMT_GEN) minval(nvf_levels)
                call print_error( &
                    "The IDs of the vegetation types are not in the expected range of " // trim(adjustl(code)) // " to " // &
                    trim(adjustl(field)) // ".")
                ierr = 1
            end if
        end if
    end if
    if (present(dim_sol_name)) dim_s = dim_sol_name
    if (len_trim(dim_s) == 0) dim_s = 'nsol'
    if (nc4_inquire_dimension(iun, dim_s)) then
        call nc4_get_dimension(iun, dim_s, dim_length = nsol, ierr = z)
        if (z /= 0) then
            nsol = -1
        else if (nsol /= shd%lc%IGND) then
            write(field, FMT_GEN) shd%lc%IGND
            call print_error( &
                "The number of soil layers '" // trim(dim_s) // "' does not match the expected value of " &
                // trim(adjustl(field)) // ".")
            ierr = 1
        end if
    end if
    if (ierr /= 0) goto 999

    !> Scan and assign variables.
    ncount = 0
    do i = 1, nvars

        !> Get the name of the variable.
        call nc4_get_variable_name(iun, i, field, ierr)
        if (ierr /= 0) goto 999

        !> Get the number of dimensions.
        call nc4_get_variable_attributes(iun, field, ndims = ndims, ierr = ierr)
        if (ierr /= 0) goto 999

        !> Determine and assign the variable.
        z = radix(z)**pstat%NORMAL_STATUS
        select case (lowercase(field))

            !> RUNCLASS36 and RUNSVS113.
            case ('fcan')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ndims == 2) then
                        call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                        do n = 1, shd%NA
                            pm%grid%fcan(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                        end do
                    else
                        call nc4_get_variable(iun, field, dim_x, dim_y, dim_c, dat3_r, fill_r, ierr = ierr)
                        do m = 1, ncan
                            do n = 1, shd%NA
                                pm%grid%fcan(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                            end do
                        end do
                        deallocate(dat3_r)
                    end if
                end if
            case ('lnz0')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ndims == 2) then
                        call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                        do n = 1, shd%NA
                            pm%grid%lnz0(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                        end do
                        if (svs_mesh%PROCESS_ACTIVE) then
                            if (.not. allocated(svs_mesh%vs%lnz0)) allocate(svs_mesh%vs%lnz0(shd%lc%NML))
                            do k = 1, shd%lc%NML
                                svs_mesh%vs%lnz0(k) = dat2_r(shd%xxx(shd%lc%ILMOS(k)), shd%yyy(shd%lc%ILMOS(k)))
                            end do
                        end if
                    else
                        call nc4_get_variable(iun, field, dim_x, dim_y, dim_c, dat3_r, fill_r, ierr = ierr)
                        do m = 1, ncan
                            do n = 1, shd%NA
                                pm%grid%lnz0(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                            end do
                        end do
                        deallocate(dat3_r)
                    end if
                end if
            case ('vf')
                if (svs_mesh%PROCESS_ACTIVE) then
                    if (.not. allocated(svs_mesh%vs%vf)) allocate(svs_mesh%vs%vf(shd%lc%NML, nvf))
                    call nc4_get_variable(iun, field, dim_x, dim_y, dim_v, dat3_r, fill_r, ierr = ierr)
                    do m = 1, nvf
                        do k = 1, shd%lc%NML
                            svs_mesh%vs%vf(k, 1200 - nvf_levels(m)) = dat3_r(shd%xxx(shd%lc%ILMOS(k)), shd%yyy(shd%lc%ILMOS(k)), m)
                        end do
                    end do
                    deallocate(dat3_r)
                end if
            case ('sdep')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pm%grid%sdep(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('xslp')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pm%grid%xslp(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('dd', 'dden')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pm%grid%dd(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do

                    !> Unit conversion to m m**-2 if units are km km**-2 (WATROF/WATDRN expects m m**-2).
                    if (index(lowercase(units), 'km') > 0) then
                        call print_remark("'" // trim(field) // "' units converted from 'km km**-2' to 'm m**-2'.")
                        pm%grid%dd = pm%grid%dd/1000.0
                    end if
                end if
            case ('sand')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ndims == 2) then
                        call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                        do n = 1, shd%NA
                            pm%grid%sand(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                        end do
                    else
                        call nc4_get_variable(iun, field, dim_x, dim_y, dim_s, dat3_r, fill_r, ierr = ierr)
                        do m = 1, nsol
                            do n = 1, shd%NA
                                pm%grid%sand(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                            end do
                        end do
                        deallocate(dat3_r)
                    end if
                end if
            case ('clay')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ndims == 2) then
                        call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                        do n = 1, shd%NA
                            pm%grid%clay(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                        end do
                    else
                        call nc4_get_variable(iun, field, dim_x, dim_y, dim_s, dat3_r, fill_r, ierr = ierr)
                        do m = 1, nsol
                            do n = 1, shd%NA
                                pm%grid%clay(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                            end do
                        end do
                        deallocate(dat3_r)
                    end if
                end if
            case ('orgm')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ndims == 2) then
                        call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                        do n = 1, shd%NA
                            pm%grid%orgm(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                        end do
                    else
                        call nc4_get_variable(iun, field, dim_x, dim_y, dim_s, dat3_r, fill_r, ierr = ierr)
                        do m = 1, nsol
                            do n = 1, shd%NA
                                pm%grid%orgm(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                            end do
                        end do
                        deallocate(dat3_r)
                    end if
                end if

            !> RUNCLASS36.
            case ('iwf')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_i, fill_i, ierr = ierr)
                    do n = 1, shd%NA
                        pm%grid%iwf(n) = dat2_i(shd%xxx(n), shd%yyy(n))
                    end do
                end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            case ('pwr')
                if (bflm%BASEFLOWFLAG == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        bflm%pm_grid%pwr(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('flz')
                if (bflm%BASEFLOWFLAG == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        bflm%pm_grid%flz(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if

            !> RPN RTE (Watflood, 2007).
            case ('r1n')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%r1n(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('r2n')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%r2n(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('mndr')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%mndr(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('widep')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%widep(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('aa2')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%aa2(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('aa3')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%aa3(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('aa4')
                if (rteflg%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        rtepm%aa4(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
!                case ('theta')
!                case ('kcond')

            !> PBSM (blowing snow).
            case ('fetch')
                if (pbsm%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pbsm%pm_grid%fetch(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('ht')
                if (pbsm%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pbsm%pm_grid%Ht(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('n_s')
                if (pbsm%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pbsm%pm_grid%N_S(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('a_s')
                if (pbsm%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pbsm%pm_grid%A_S(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('distrib')
                if (pbsm%PROCESS_ACTIVE) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        pbsm%pm_grid%Distrib(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if

            !> SOLARADJUSTFLAG.
            case ('elevation')
                if (.not. allocated(mountain_mesh%pm%elev)) allocate(mountain_mesh%pm%elev(shd%NA, shd%lc%NTYPE))
                if (ndims == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        mountain_mesh%pm%elev(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                else
                    call nc4_get_variable(iun, field, dim_x, dim_y, dim_m, dat3_r, fill_r, ierr = ierr)
                    do m = 1, ngru
                        do n = 1, shd%NA
                            mountain_mesh%pm%elev(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                        end do
                    end do
                    deallocate(dat3_r)
                end if
            case ('slope')
                if (.not. allocated(mountain_mesh%pm%slope)) allocate(mountain_mesh%pm%slope(shd%NA, shd%lc%NTYPE))
                if (ndims == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        mountain_mesh%pm%slope(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                else
                    call nc4_get_variable(iun, field, dim_x, dim_y, dim_m, dat3_r, fill_r, ierr = ierr)
                    do m = 1, ngru
                        do n = 1, shd%NA
                            mountain_mesh%pm%slope(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                        end do
                    end do
                    deallocate(dat3_r)
                end if
            case ('aspect')
                if (.not. allocated(mountain_mesh%pm%aspect)) allocate(mountain_mesh%pm%aspect(shd%NA, shd%lc%NTYPE))
                if (ndims == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        mountain_mesh%pm%aspect(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                else
                    call nc4_get_variable(iun, field, dim_x, dim_y, dim_m, dat3_r, fill_r, ierr = ierr)
                    do m = 1, ngru
                        do n = 1, shd%NA
                            mountain_mesh%pm%aspect(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                        end do
                    end do
                    deallocate(dat3_r)
                end if
            case ('delta')
                if (.not. allocated(mountain_mesh%pm%delta)) allocate(mountain_mesh%pm%delta(shd%NA, shd%lc%NTYPE))
                if (ndims == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        mountain_mesh%pm%delta(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                else
                    call nc4_get_variable(iun, field, dim_x, dim_y, dim_m, dat3_r, fill_r, ierr = ierr)
                    do m = 1, ngru
                        do n = 1, shd%NA
                            mountain_mesh%pm%delta(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                        end do
                    end do
                    deallocate(dat3_r)
                end if
            case ('curvature')
                if (.not. allocated(mountain_mesh%pm%curvature)) allocate(mountain_mesh%pm%curvature(shd%NA, shd%lc%NTYPE))
                if (ndims == 2) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        mountain_mesh%pm%curvature(n, :) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                else
                    call nc4_get_variable(iun, field, dim_x, dim_y, dim_m, dat3_r, fill_r, ierr = ierr)
                    do m = 1, nsol
                        do n = 1, shd%NA
                            mountain_mesh%pm%curvature(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
                        end do
                    end do
                    deallocate(dat3_r)
                end if
        end select

        !> Check for errors.
        if (ierr /= 0) then
            exit
        else if (.not. btest(z, pstat%NORMAL_STATUS)) then
            ierr = z
            write(code, FMT_GEN) z
            call print_error( &
                "An error occurred reading '" // trim(adjustl(field)) // "' from the file (Code: " // trim(adjustl(code)) // ").")
        else
            ncount = ncount + 1
        end if
    end do
    if (ierr /= 0) then
        call print_error("Errors occurred reading attributes from the file.")
        goto 999
    end if

    !> Print number of active parameters.
    write(field, FMT_GEN) ncount
    call print_message('Active parameters in file: ' // trim(adjustl(field)))

    !> Close the file to free the unit.
999 continue
    call nc4_close_file(iun, fname, ierr = z)
#endif

end subroutine
