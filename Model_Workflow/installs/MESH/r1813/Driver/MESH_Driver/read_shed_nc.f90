!> Description:
!>  Subroutine to read basin properties from a netCDF 'nc' format
!>  file. Values are parsed by order of RANK and stored in variables
!>  that must be allocated 1:NA.
!>
!> Input variables:
!*  fname: Full path to the file.
!*  dim_x_name: Name of the 'x' dimension (optional override).
!*  dim_y_name: Name of the 'y' dimension (optional override).
!*  dim_c_name: Name of the 'character length' dimension (optional override).
!*  dim_m_name: Name of the 'GRU' dimension (optional override).
!>
!> Output variables:
!*  ierr: Return status.
!>
!> Input/output variables:
!*  shd: Basin 'shed' object (properties).
subroutine read_shed_nc(shd, fname, dim_x_name, dim_y_name, dim_c_name, dim_m_name, ierr)

    !> strings: For 'lowercase' function.
    !> sa_mesh_common: For common MESH variables and routines.
    !> nc_io: For routines to read netCDF 'nc' format file.
    !> parse_utilities: For parsing utilities and flags.
    use strings
    use sa_mesh_common
    use nc_io
    use parse_utilities

    implicit none

    !> Input variables.
    character(len = *), intent(in) :: fname

    !> Input variables (optional).
    character(len = *), intent(in), optional :: dim_x_name, dim_y_name, dim_c_name, dim_m_name

    !> Output variables.
    integer, intent(out) :: ierr

    !> Input/output variables.
    type(ShedGridParams) shd

    !> Local variables (reading).
    character(len = DEFAULT_FIELD_LENGTH) units
    character(len = DEFAULT_FIELD_LENGTH), allocatable :: dat_c(:)
    character(len = 1) fill_c
    real, allocatable :: dat2_r(:, :), dat3_r(:, :, :)
    real fill_r
    real, parameter :: deg_threshold = 1.0E-4
    integer, allocatable :: dat2_i(:, :)
    integer natts, nvars, fill_i

    !> Local variables.
    character(len = DEFAULT_FIELD_LENGTH) :: dim_x = '', dim_y = '', dim_m = '', field, code
    integer iun, x, y, m, n, i, z

    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Open the file.
    call nc4_open_input(fname, iun = iun, ierr = ierr)
    if (ierr /= 0) return

    !> Get the projection.
    call nc4_get_projection( &
        iun, shd%CoordSys%Proj, &
        shd%CoordSys%Ellips, shd%CoordSys%Zone, shd%CoordSys%earth_radius, shd%CoordSys%grid_north_pole_latitude, &
        shd%CoordSys%grid_north_pole_longitude, &
        name_dim_crs = 'crs', &
        ierr = ierr)
    if (ierr /= 0) then
        goto 999
    else
        if (DIAGNOSEMODE) call print_message("The projection of the file is '" // trim(shd%CoordSys%Proj) // "'.")
    end if

    !> Reference latitude/longitude values (projection dependent).
    if (present(dim_x_name)) dim_x = dim_x_name
    if (present(dim_y_name)) dim_y = dim_y_name
    select case (shd%CoordSys%Proj)
        case ('LATLONG')

            !> Get longitude and latitude and set respective counts.
            if (len_trim(dim_x) == 0) dim_x = 'lon'
            call nc4_get_variable(iun, dim_x, dat = shd%CoordSys%lon, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                call print_error("An error occurred reading the lontigude '" // trim(dim_x) // "' variable.")
                ierr = z
            else if (any(shd%CoordSys%lon == fill_r)) then
                call print_error("The reference '" // trim(dim_x) // "' field contains 'NO_DATA' values.")
                ierr = 1
            else
                shd%xCount = size(shd%CoordSys%lon)
                shd%xDelta = 0.0
                n = 0
                do i = 2, shd%xCount
                    if (i < shd%xCount) then
                        if ( &
                            ((shd%CoordSys%lon(i) - shd%CoordSys%lon(i - 1)) - &
                             (shd%CoordSys%lon(i + 1) - shd%CoordSys%lon(i))) > deg_threshold) n = n + 1
                    end if
                    shd%xDelta = shd%xDelta + (shd%CoordSys%lon(i) - shd%CoordSys%lon(i - 1))
                end do
                shd%xDelta = shd%xDelta/(shd%xCount - 1)
                shd%xOrigin = minval(shd%CoordSys%lon) - shd%xDelta/2.0
                if (n > 0) then
                    call print_warning( &
                        "The spacing of longitudes is not regular inside this file. Conversion or writing outputs to file formats " // &
                        "that implicitly use regular grids is not recommended.")
                end if
            end if
            select case (lowercase(units))
                case ('degrees', 'decimal_degrees', 'degrees_east')
                case default
                    call print_warning( &
                        "The units '" // trim(units) // "' of the longitude variable are different from the expected units of " // &
                        "'degrees', 'decimal_degrees', or 'degrees_east'.")
            end select
            if (len_trim(dim_y) == 0) dim_y = 'lat'
            call nc4_get_variable(iun, dim_y, dat = shd%CoordSys%lat, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                call print_error("An error occurred reading the latitude '" // trim(dim_y) // "' variable.")
                ierr = z
            else if (any(shd%CoordSys%lat == fill_r)) then
                call print_error("The reference '" // trim(dim_y) // "' field contains 'NO_DATA' values.")
                ierr = 1
            else
                shd%yCount = size(shd%CoordSys%lat)
                shd%yDelta = 0.0
                n = 0
                do i = 2, shd%yCount
                    if (i < shd%yCount) then
                        if ( &
                            ((shd%CoordSys%lat(i) - shd%CoordSys%lat(i - 1)) - &
                             (shd%CoordSys%lat(i + 1) - shd%CoordSys%lat(i))) > deg_threshold) n = n + 1
                    end if
                    shd%yDelta = shd%yDelta + (shd%CoordSys%lat(i) - shd%CoordSys%lat(i - 1))
                end do
                shd%yDelta = shd%yDelta/(shd%yCount - 1)
                shd%yOrigin = minval(shd%CoordSys%lat) - shd%yDelta/2.0
            end if
            select case (lowercase(units))
                case ('degrees', 'decimal_degrees', 'degrees_north')
                case default
                    call print_warning( &
                        "The units '" // trim(units) // "' of the latitude variable are different from the expected units of " // &
                        "'degrees', 'decimal_degrees', or 'degrees_north'.")
            end select
        case ('rotated_latitude_longitude')

            !> Get rotated longitude and latitude and set respective counts.
            if (len_trim(dim_x) == 0) dim_x = 'rlon'
            call nc4_get_variable(iun, dim_x, dat = shd%CoordSys%rlon, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                call print_error("An error occurred reading the latitude '" // trim(dim_x) // "' variable.")
                ierr = z
            else if (any(shd%CoordSys%rlon == fill_r)) then
                call print_error("The reference '" // trim(dim_x) // "' field contains 'NO_DATA' values.")
                ierr = 1
            else
                shd%xCount = size(shd%CoordSys%rlon)
            end if
            select case (lowercase(units))
                case ('degrees', 'decimal_degrees', 'degrees_north')
                case default
                    call print_warning( &
                        "The units '" // trim(units) // "' of the latitude variable are different from the expected units of " // &
                        "'degrees', 'decimal_degrees', or 'degrees_north'.")
            end select
            if (len_trim(dim_y) == 0) dim_y = 'rlat'
            call nc4_get_variable(iun, dim_y, dat = shd%CoordSys%rlat, fill = fill_r, units = units, ierr = z)
            if (z /= 0) then
                call print_error("An error occurred reading the latitude '" // trim(dim_y) // "' variable.")
                ierr = z
            else if (any(shd%CoordSys%rlat == fill_r)) then
                call print_error("The reference '" // trim(dim_y) // "' field contains 'NO_DATA' values.")
                ierr = 1
            else
                shd%yCount = size(shd%CoordSys%rlat)
            end if
            select case (lowercase(units))
                case ('degrees', 'decimal_degrees', 'degrees_north')
                case default
                    call print_warning( &
                        "The units '" // trim(units) // "' of the latitude variable are different from the expected units of " // &
                        "'degrees', 'decimal_degrees', or 'degrees_north'.")
            end select
    end select
    if (ierr /= 0) goto 999

    !> Get 'Rank'.
    allocate(shd%RNKGRD(shd%yCount, shd%xCount))
    shd%RNKGRD = 0
    call nc4_get_variable(iun, 'Rank', dim_x, dim_y, dat2_i, fill_i, ierr = ierr)
    if (ierr /= 0) then
        goto 999
    else
        shd%RNKGRD = transpose(dat2_i)
    end if
    if (all(shd%RNKGRD == 0) .or. all(shd%RNKGRD == fill_i)) then
        write(field, FMT_GEN) fill_i
        call print_error("All values in the 'Rank' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
        ierr = 1
        goto 999
    end if

    !> Determine 'NA'.
    shd%NA = count(shd%RNKGRD /= fill_i .and. shd%RNKGRD > 0)
    shd%AL = 1.0
    shd%DebugGridNo = shd%NA

    !> Create the 'xxx' and 'yyy' reference tables.
    allocate(shd%xxx(shd%NA), shd%yyy(shd%NA))
    do x = 1, shd%xCount
        do y = 1, shd%yCount
            if (shd%RNKGRD(y, x) > 0) then
                shd%xxx(shd%RNKGRD(y, x)) = x
                shd%yyy(shd%RNKGRD(y, x)) = y
            end if
        end do
    end do
    if (any(shd%xxx == 0) .or. any(shd%yyy == 0)) then
        call print_warning("There are active cells that are not associated with an X/Y indexed location within the domain.")
    end if

    !> Get the number of attributes and variables in the file.
    call nc4_inquire_file(iun, natts = natts, nvars = nvars, ierr = ierr)
    if (ierr /= 0) goto 999

    !> Scan and assign global attributes.
    if (DIAGNOSEMODE) then
        write(code, FMT_GEN) natts
        call print_message(trim(adjustl(code)) // " attributes found in the file.")
    end if
    do i = 1, natts

        !> Get the name of the attribute.
        call nc4_get_attribute_name(iun, i, attribute_name = field, ierr = ierr)
        if (ierr /= 0) goto 999

        !> Determine and assign the attribute.
        z = 0
        select case (lowercase(field))
            case ('nominalgridsize_al')
                call nc4_get_attribute(iun, field, shd%AL, ierr = z)
            case ('debuggridno')
                call nc4_get_attribute(iun, field, shd%DebugGridNo, ierr = z)
        end select

        !> Check for errors.
        if (z /= 0) then
            ierr = z
            write(code, FMT_GEN) z
            call print_error( &
                "An error occurred reading '" // trim(adjustl(field)) // "' from the file (Code: " // trim(adjustl(code)) // ").")
        end if
    end do
    if (ierr /= 0) then
        call print_error("Errors occurred reading attributes from the file.")
        goto 999
    end if

    !> Scan and assign variables.
    if (DIAGNOSEMODE) then
        write(code, FMT_GEN) nvars
        call print_message(trim(adjustl(code)) // " variables found in the file.")
    end if
    do i = 1, nvars

        !> Get the name of the variable.
        call nc4_get_variable_name(iun, i, field, ierr)
        if (ierr /= 0) goto 999

        !> Identify and assign the variable.
        z = radix(z)**pstat%NORMAL_STATUS
        select case (lowercase(field))
            case ('next')
                call allocate_variable(shd%NEXT, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_i, fill_i, ierr = ierr)
                    do n = 1, shd%NA
                        shd%NEXT(n) = dat2_i(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('iak')
                call allocate_variable(shd%IAK, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_i, fill_i, ierr = ierr)
                    do n = 1, shd%NA
                        shd%IAK(n) = dat2_i(shd%xxx(n), shd%yyy(n))
                    end do

                    !> Update the number of river classes.
                    shd%NRVR = maxval(shd%IAK)
                end if
            case ('chnlslope')
                call allocate_variable(shd%SLOPE_CHNL, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%SLOPE_CHNL(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('chnllength')
                call allocate_variable(shd%CHNL_LEN, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%CHNL_LEN(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('chnl')
                call allocate_variable(shd%ICHNL, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_i, fill_i, ierr = ierr)
                    do n = 1, shd%NA
                        shd%ICHNL(n) = dat2_i(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('reach')
                call allocate_variable(shd%IREACH, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_i, fill_i, ierr = ierr)
                    do n = 1, shd%NA
                        shd%IREACH(n) = dat2_i(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('da')
                call allocate_variable(shd%DA, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%DA(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('gridarea')
                call allocate_variable(shd%AREA, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%AREA(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do

                    !> Calculate 'FRAC'.
                    call allocate_variable(shd%FRAC, shd%NA, z)
                    if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                        shd%FRAC = shd%AREA/shd%AL/shd%AL
                    end if
                end if
            case ('bankfull')
                call allocate_variable(shd%BNKFLL, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%BNKFLL(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('elev')
                call allocate_variable(shd%ELEV, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%ELEV(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
!?            case ('intslope')
!?                call allocate_variable(shd%SLOPE_TOPO, shd%NA, z)
!?                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
!?                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
!?                    do n = 1, shd%NA
!?                        shd%SLOPE_TOPO(n) = dat2_r(shd%xxx(n), shd%yyy(n))
!?                    end do
!?                end if
            case ('demslope')
                call allocate_variable(shd%SLOPE_INT, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%SLOPE_INT(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do
                end if
            case ('drdn')
                call allocate_variable(shd%DRDN, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_x, dim_y, dat2_r, fill_r, ierr = ierr)
                    do n = 1, shd%NA
                        shd%DRDN(n) = dat2_r(shd%xxx(n), shd%yyy(n))
                    end do

                    !> Unit conversion to m m**-2 if units are km km**-2 (WATROF/WATDRN expects m m**-2).
                    if (index(lowercase(units), 'km') > 0) then
                        call print_remark("'" // trim(field) // "' units converted from 'km km**-2' to 'm m**-2'.")
                        shd%DRDN = shd%DRDN/1000.0
                    end if
                end if
            case ('latitude', 'lat')
                call nc4_get_variable(iun, field, dim_x, dim_y, shd%CoordSys%xylat, fill_r, ierr = ierr)
                if (ierr == 0) then
                    call allocate_variable(shd%ylat, shd%NA, z)
                    if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                        call map_to_field(shd%CoordSys%xylat, shd%xxx, shd%yyy, shd%ylat, z)
                    end if
                end if
            case ('longitude', 'lon')
                call nc4_get_variable(iun, field, dim_x, dim_y, shd%CoordSys%xylon, fill_r, ierr = ierr)
                if (ierr == 0) then
                    call allocate_variable(shd%xlng, shd%NA, z)
                    if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                        call map_to_field(shd%CoordSys%xylon, shd%xxx, shd%yyy, shd%xlng, z)
                    end if
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
        end if
    end do
    if (ierr /= 0) then
        call print_error("Errors occurred reading attributes from the file.")
        goto 999
    end if

    !> Get the number of GRUs (dimension).
    if (present(dim_m_name)) dim_m = dim_m_name
    if (len_trim(dim_m) == 0) dim_m = 'ngru'
    call nc4_get_dimension(iun, dim_m, dim_length = shd%lc%NTYPE, ierr = ierr)
    if (ierr /= 0) goto 999

    !> Get GRU names.
    call nc4_get_variable(iun, 'LandUse', dim_m, dim_c_name, dat_c, fill_c, ierr = ierr)
    if (ierr /= 0) then
        call print_warning("Unable to read GRU names as 'LandUse'. Default names will be assumed.")
    end if

    !> Get GRU fractions (ACLASS).
    call nc4_get_variable(iun, 'GRU', dim_x, dim_y, dim_m, dat3_r, fill_r, ierr = ierr)
    if (ierr /= 0) then
        goto 999
    else
        if (all(dat3_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'GRU' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            ierr = 1
            goto 999
        else
            call allocate_variable(shd%lc%ACLASS, shd%NA, shd%lc%NTYPE, istat = ierr)
            if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                do m = 1, shd%lc%NTYPE
                    call map_to_field(dat3_r(:, :, m), shd%xxx, shd%yyy, shd%lc%ACLASS(:, m), z)
                end do
            end if
        end if
    end if

    !> Adjust GRU count (exclude impervious).
    shd%lc%NTYPE = shd%lc%NTYPE - 1

    !> Determine 'NAA'.
    shd%NAA = count(shd%NEXT /= 0)

    !> Check grid dimension.
    if (shd%NA < 1 .or. shd%NAA < 1) then
        call print_error('No grids are defined inside the basin.')
        call increase_tab()
        write(field, FMT_GEN) shd%NA
        call print_message('Number of grids read from file: ' // trim(adjustl(field)))
        call decrease_tab()
        ierr = 1
        goto 999
    end if
    if (shd%NAA >= shd%NA) then
        call print_warning('No outlets exist in the basin.')
        call increase_tab()
        write(field, FMT_GEN) shd%NA
        call print_message('Total number of grids: ' // trim(adjustl(field)))
        write(field, FMT_GEN) shd%NAA
        call print_message('Total number of grids inside the basin: ' // trim(adjustl(field)))
        call decrease_tab()
    end if

    !> Close the file to free the unit.
999 continue
    call nc4_close_file(iun, fname, ierr = z)
#endif

end subroutine
