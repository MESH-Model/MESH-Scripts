!> Description:
!>  Subroutine to read basin properties from file, in delimited format.
!>  Values are parsed by order of RANK and stored in variables
!>  that must be allocated 1:NA.
!>
!> Input variables:
!*  fname: Full path to the file (default: './MESH_drainage_database.txt').
!>
!> Input/output variables:
!*  shd: Basin 'shed' object (properties).
!>
!> Output variables:
!*  ierr: Return status.
subroutine read_shed_csv(shd, fname, ierr)

    !> strings: For 'readline', 'compact', 'parse', 'uppercase' and 'lowercase' functions.
    !> sa_mesh_common: For common MESH variables and routines.
    !> parse_utilities: For 'assign_line_args' function.
    use strings
    use sa_mesh_common
    use parse_utilities

    !> Process modules: Required for process variables, parameters.
    use baseflow_module
    use rte_module
    use runsvs_mesh
    use FLAGS

    implicit none

    !> Input/output variables.
    type(ShedGridParams) shd
    character(len = *), intent(in) :: fname
    integer, intent(out) :: ierr

    !> Local variables.
    integer iun, nargs, n, p, b, k, i, istat, z
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH), dimension(:), allocatable :: args
    character(len = DEFAULT_FIELD_LENGTH) field1, field2

    !> Initialize the return status.
    ierr = 0

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open the file. Check if the file exists.')
        return
    end if

    !> Find and read 'Rank' and dimensions.
    shd%NA = -1
    shd%lc%NTYPE = -1
    z = 0
    p = 0
    b = 0
    do while (z == 0)

        !> Compact and reduce the line to any instance of '#' or '!'.
        call readline(iun, line, z)
        if (z /= 0) exit
        if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
        if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
        call compact(line)

        !> Replace commas with spaces and parse the fields in the line.
        do i = 1, len_trim(line)
            if (line(i:i) == ',') line(i:i) = ' '
        end do
        call parse_line_values(line, args, istat = istat)
        nargs = size(args)

        !> Cycle if no arguments exist.
        if (nargs < 1) cycle

        !> Check argument.
        select case (lowercase(args(1)))
            case ('rank')
                shd%NA = (nargs - 1)
            case ('latitude')
                p = (nargs - 1)
            case ('longitude')
                b = (nargs - 1)
            case ('ngru')
                call value(args(2), shd%lc%NTYPE, z)
        end select
    end do

    !> Check for errors.
    if (shd%NA == -1) then

        !> Derive the number elements from the latitude and longitude fields.
        if (p /= b) then
            call print_warning("The number of 'latitudes' and 'longitudes' read from file do not match.")
        end if
        shd%NA = min(p, b)
    end if
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

    !> Rewind the file.
    rewind(iun)

    !> Read and parse each line.
    shd%AL = -1.0
    z = 0
    n = 1
    do while (z == 0)

        !> Compact and reduce the line to any instance of '#' or '!'.
        call readline(iun, line, z)
        if (z /= 0) exit
        if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
        if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
        call compact(line)

        !> Replace commas with spaces and parse the fields in the line.
        do i = 1, len_trim(line)
            if (line(i:i) == ',') line(i:i) = ' '
        end do
        call parse_line_values(line, args, istat = istat)
        nargs = size(args)

        !> Cycle if no arguments exist.
        if (nargs < 1) cycle

        !> Reset variables used for message passing.
        p = shd%NA
        b = 0

        !> Shift 'nargs' by 1 to account for the leading field name.
        nargs = nargs - 1

        !> Assign and distribute the field.
        select case (lowercase(args(1)))

            !> Basin attributes (general).
            case ('projection')
                call assign_line_args(shd%CoordSys%Proj, args(2), istat)
                p = 1
            case ('ellipsoid')
                call assign_line_args(shd%CoordSys%Ellips, args(2), istat)
                p = 1
            case ('latitude', 'lat')
                call assign_line_args(shd%ylat, p, args(2:), istat)
            case ('longitude', 'lon')
                call assign_line_args(shd%xlng, p, args(2:), istat)
            case ('next')
                call assign_line_args(shd%NEXT, p, args(2:), istat)
            case ('gridarea')
                call assign_line_args(shd%AREA, p, args(2:), istat)
            case ('elev')
                call assign_line_args(shd%ELEV, p, args(2:), istat)
!?            case ('intslope')

            !> Drainage/routing attributes.
            case ('iak')
                call assign_line_args(shd%IAK, p, args(2:), istat)
            case ('chnlslope')
                call assign_line_args(shd%SLOPE_CHNL, p, args(2:), istat)
            case ('chnllength')
                call assign_line_args(shd%CHNL_LEN, p, args(2:), istat)
            case ('chnl')
                call assign_line_args(shd%ICHNL, p, args(2:), istat)
            case ('reach')
                call assign_line_args(shd%IREACH, p, args(2:), istat)
            case ('da')
                call assign_line_args(shd%DA, p, args(2:), istat)
            case ('bankfull')
                call assign_line_args(shd%BNKFLL, p, args(2:), istat)

            !> GRUs.
            case ('gru')
                call assign_line_args(k, args(2), istat)
                istat = pstat%NORMAL_STATUS
                nargs = nargs - 1
                b = shd%lc%NTYPE
                call assign_line_args(shd%lc%ACLASS, p, b, args(3:), pkey%MAP_ASSIGN_ORDER1, istat, k)
                b = 0

            !> Unrecognized (ignored).
            case default
                p = 0
                b = 0
        end select

        !> Status flags (fatal and/or warning).
        if (p /= 0) then
            if ((b == 0 .and. nargs > p) .or. (b > 0 .and. nargs > b)) then
                write(field1, FMT_GEN) nargs
                if (b > 0) then
                    write(field2, FMT_GEN) b
                else
                    write(field2, FMT_GEN) p
                end if
                call print_warning( &
                    "'" // trim(adjustl(args(1))) // "' contains " // trim(adjustl(field1)) // " fields but only " // &
                    trim(adjustl(field2)) // " are being used.")
            else if ((b == 0 .and. nargs /= p) .or. (b > 0 .and. nargs /= b)) then
                write(field1, FMT_GEN) nargs
                if (b > 0) then
                    write(field2, FMT_GEN) b
                else
                    write(field2, FMT_GEN) p
                end if
                call print_warning( &
                    "'" // trim(adjustl(args(1))) // "' only contains " // trim(adjustl(field1)) // " fields when " // &
                    trim(adjustl(field2)) // " are expected.")
            end if
        end if
        if (btest(istat, pstat%MISMATCHED_PRECISION)) then
            call print_warning( &
                "The precision or length of the '" // trim(adjustl(args(1))) // "' values exceeds the precision or length " // &
                "of the assigned field. Truncation may have occurred.")
        end if
        if (btest(istat, pstat%CONVERSION_ERROR)) then
            call print_warning("An error occurred assigning '" // trim(adjustl(args(1))) // "' values.")
        end if
        if (btest(istat, pstat%ALLOCATION_ERROR)) then
            call print_error("An error occurred allocating '" // trim(adjustl(args(1))) // "'.")
            ierr = 1
        end if
        if (btest(istat, pstat%MISMATCHED_BOUNDS)) then
            call print_error("Mismatched bounds encountered while assigning '" // trim(adjustl(args(1))) // "'.")
            ierr = 1
        end if
        n = n + 1
    end do
    if (ierr /= 0) goto 999

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

    !> Derive the number of points inside the basin.
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
    call print_message('Active variables in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
999 close(iun)

    return

end subroutine
