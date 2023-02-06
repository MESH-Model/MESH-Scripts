!> Description:
!>  Subroutine to read subbasin properties from a netCDF 'nc' format
!>  file. Values are parsed by ngru and stored in variables
!>
!> Input variables:
!*  fname: Full path to the file.
!*  dim_n_name: Name of the 'n' dimension (optional override).
!*  dim_m_name: Name of the 'm' dimension (optional override).
!>
!> Output variables:
!*  ierr: Return status.
!>
!> Input/output variables:
!*  shd: Basin 'shed' object (properties).
subroutine read_shed_nc_subbasin(shd, fname, dim_n_name, dim_m_name, ierr)

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
    character(len = *), intent(in), optional :: dim_n_name, dim_m_name

    !> Output variables.
    integer, intent(out) :: ierr

    !> Input/output variables.
    type(ShedGridParams) shd

    !> Local variables (reading).
    real fill_r
    integer ndims, natts, nvars, fill_i

    !> Local variables.
    character(len = DEFAULT_FIELD_LENGTH) :: dim_n = '', dim_m = '', field, code
    integer iun, n, i, z, m
    character(len = DEFAULT_LINE_LENGTH) line


    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Open the file.
    call nc4_open_input(fname, iun = iun, ierr = ierr)
    if (ierr /= 0) return\

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

    !> 'nc_subbasin' only supports 'LATLONG' projection.
    if (shd%CoordSys%Proj /= 'LATLONG') then
        call print_error("The projection '" // trim(shd%CoordSys%Proj) // "' is not supported for point-based file formats.")
    end if

    !> Get attributes from the file.
    call nc4_inquire_file(iun, ndims = ndims, natts = natts, nvars = nvars, ierr = ierr)
    if (ierr /= 0) goto 999

    !> Find and read dimensions.
    if (DIAGNOSEMODE) then
        write(code, FMT_GEN) ndims
        call print_message(trim(adjustl(code)) // " dimensions found in the file.")
    end if
    if (present(dim_n_name)) dim_n = dim_n_name
    if (len_trim(dim_n) == 0) dim_n = 'subbasin'
    shd%NA = -1
    if (present(dim_m_name)) dim_m = dim_m_name
    if (len_trim(dim_m) == 0) dim_m = 'gru'
    shd%lc%NTYPE = -1
    do n = 1, ndims

        !> Get the name of the dimension.
        call nc4_get_dimension_name(iun, n, dim_name = field, dim_length = fill_i, ierr = ierr)
        if (ierr /= 0) goto 999

        !> Determine and assign the dimension.
        if (lowercase(field) == dim_n) then
            shd%NA = fill_i
        else if (lowercase(field) == dim_m) then
            shd%lc%NTYPE = fill_i
        end if
    end do

    !> Check for errors.
    if (.not. shd%NA > 0) then

        !> The number of points could not be determined.
        call print_error("The number of active points could not be read from the file.")
        goto 999
    end if

    !> Check for GRUs.
    if (.not. shd%lc%NTYPE > 1) then

        !> Assume 2x GRU when no GRUs are defined in the file and an HLSS is active.
        if (ro%RUNLSS) then
            call print_remark("No GRUs were found in the file.")
        end if
        shd%lc%NTYPE = 2
    end if

    !> Create dummy grid arrays for 1xNA vector and allocate the 'ACLASS' field.
    shd%yCount = shd%NA
    shd%xCount = 1
    allocate(shd%lc%ACLASS(shd%NA, shd%lc%NTYPE), shd%RNKGRD(shd%yCount, shd%xCount), shd%xxx(shd%NA), shd%yyy(shd%NA))
    shd%lc%ACLASS = 0.0
    shd%lc%ACLASS(:, 1) = 1.0
    shd%xxx = 1
    shd%RNKGRD = 0
    do i = 1, shd%NA
        shd%yyy(i) = i
        shd%RNKGRD(i, 1) = i
    end do

    !> Scan and assign variables.
    if (DIAGNOSEMODE) then
        write(code, FMT_GEN) nvars
        call print_message(trim(adjustl(code)) // " variables found in the file.")
    end if
    n = 0
    do i = 1, nvars

        !> Get the name of the variable.
        call nc4_get_variable_name(iun, i, field, ierr)
        if (ierr /= 0) goto 999

        !> Identify and assign the variable.
        z = radix(z)**pstat%NORMAL_STATUS
        select case (lowercase(field))

            !> Basin attributes (general).
            case ('latitude', 'lat')
                call allocate_variable(shd%ylat, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%ylat, fill_r, ierr = ierr)
                end if
            case ('longitude', 'lon')
                call allocate_variable(shd%xlng, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%xlng, fill_r, ierr = ierr)
                end if
            case ('next')
                call allocate_variable(shd%NEXT, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%NEXT, fill_i, ierr = ierr)
                end if
            case ('gridarea')
                call allocate_variable(shd%AREA, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%AREA, fill_r, ierr = ierr)
                end if
            case ('elev')
                call allocate_variable(shd%ELEV, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%ELEV, fill_r, ierr = ierr)
                end if
!?            case ('intslope')
!?                call allocate_variable(shd%SLOPE_TOPO, shd%NA, z)
!?                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
!?                    call nc4_get_variable(iun, field, dim_n, shd%SLOPE_TOPO, fill_r, ierr = ierr)
!?                end if

            !> Drainage/routing attributes.
            case ('iak')
                call allocate_variable(shd%IAK, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%IAK, fill_i, ierr = ierr)
                end if
            case ('chnlslope')
                call allocate_variable(shd%SLOPE_CHNL, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%SLOPE_CHNL, fill_r, ierr = ierr)
                end if
            case ('chnllength')
                call allocate_variable(shd%CHNL_LEN, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%CHNL_LEN, fill_r, ierr = ierr)
                end if
            case ('chnl')
                call allocate_variable(shd%ICHNL, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%ICHNL, fill_i, ierr = ierr)
                end if
            case ('reach')
                call allocate_variable(shd%IREACH, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%IREACH, fill_i, ierr = ierr)
                end if
            case ('da')
                call allocate_variable(shd%DA, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%DA, fill_r, ierr = ierr)
                end if
            case ('bankfull')
                call allocate_variable(shd%BNKFLL, shd%NA, z)
                if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
                    call nc4_get_variable(iun, field, dim_n, shd%BNKFLL, fill_r, ierr = ierr)
                end if

            !> GRUs.
            case ('gru')
                call nc4_get_variable(iun, field, dim_n, dim_m, shd%lc%ACLASS, fill_r, ierr = ierr)
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

            !> Increment the 'active' counter.
            n = n + 1
        end if
    end do
    if (ierr /= 0) then
        call print_error("Errors occurred reading variables from the file.")
        goto 999
    end if

    !> Adjust GRU count (exclude impervious).
    shd%lc%NTYPE = max(shd%lc%NTYPE - 1, 1)

    !> Checks.
    if (.not. allocated(shd%NEXT)) then
        call print_error("The 'Next' attribute is missing in the file.")
        ierr = 1
    end if
    if (.not. allocated(shd%ylat)) then
        call print_error("The 'Latitude' attribute is missing in the file.")
        ierr = 1
    end if
    if (.not. allocated(shd%xlng)) then
        call print_error("The 'Longitude' attribute is missing in the file.")
        ierr = 1
    end if
    if (.not. allocated(shd%AREA)) then
        call print_error("The 'GridArea' attribute is missing in the file.")
        ierr = 1
    end if
    if (ro%RUNCHNL) then

        !> Set 'IAK' to a single class if the field is not provided.
        if (.not. allocated(shd%IAK)) then
            call print_remark("'IAK' is missing in the file. A single river class is assumed.")
            allocate(shd%IAK(shd%NA))
            shd%IAK = 1
        end if

        !> Required fields.
        if (.not. allocated(shd%SLOPE_CHNL)) then
            call print_error("The 'ChnlSlope' attribute is missing in the file.")
            ierr = 1
        end if
        if (.not. allocated(shd%CHNL_LEN)) then
            call print_error("The 'ChnlLength' attribute is missing in the file.")
            ierr = 1
        end if
        if (.not. allocated(shd%DA) .and. .not. allocated(shd%AREA)) then
            call print_error("Neither 'DA' nor 'GridArea' were found in the file. At least one of these fields is required.")
            ierr = 1
        end if
    end if
    if (ierr /= 0) goto 999

    !> Derive the number of HRUs inside the basin.
    shd%NAA = count(shd%NEXT /= 0)

    !> Set the nominal area to 1.0 if not present (will normalize itself).
    if (.not. shd%AL > 0.0) shd%AL = 1.0

    !> Calculate 'FRAC'.
    allocate(shd%FRAC(shd%NA))
    shd%FRAC = shd%AREA/shd%AL/shd%AL

    !> Derived fields (if routing is enabled).
    if (ro%RUNCHNL) then

        !> Set the number of river classes from 'IAK' (whether provided or assumed per the check above).
        shd%NRVR = maxval(shd%IAK)

        !> Calculate 'DA' from 'AREA' if not provided.
        if (.not. allocated(shd%DA)) then
            allocate(shd%DA(shd%NA))
            shd%DA = shd%AREA/1000000.0
            do i = 1, shd%NAA
                shd%DA(shd%Next(i)) = shd%DA(shd%Next(i)) + shd%DA(i)
            end do
        end if

        !> Assume no reaches if 'REACH' was not provided.
        if (.not. allocated(shd%IREACH)) then
            allocate(shd%IREACH(shd%NA))
            shd%IREACH = 0
        end if

        !> Set 'BNKFLL' to a dummy value if not provided.
        if (.not. allocated(shd%BNKFLL)) then
            allocate(shd%BNKFLL(shd%NA))
            shd%BNKFLL = 0.0
        end if

        !> Set 'ICHNL' to a dummy value if not provided (not used).
        if (.not. allocated(shd%ICHNL)) then
            allocate(shd%ICHNL(shd%NA))
            shd%ICHNL = 1
        end if
    end if

    !> Print number of active parameters.
    write(line, FMT_GEN) n
    call print_message('Active variables in the file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
999 continue
    call nc4_close_file(iun, fname, ierr = z)
#endif

end subroutine
