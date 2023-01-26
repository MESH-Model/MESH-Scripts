module nc_io

    !> 'print_routines': For print routines, format statements, and line lengths and limits.
    !* 'mesh_io_options': For I/O constants.
    !* 'model_dates': For 'ic' counter.
    !* 'netcdf': For netCDF library.
    !* 'typesizes': For data types used by netCDF library.
    use print_routines
    use parse_utilities
    use mesh_io_options
    use model_dates
#ifdef NETCDF
    use netcdf
    use typesizes
#endif
    use strings
#ifdef NETCDF

    implicit none

    interface nc4_get_attribute
        module procedure nc4_get_attribute_real
        module procedure nc4_get_attribute_int
        module procedure nc4_get_attribute_char
    end interface

    interface nc4_map_variable
        module procedure nc4_map_variable_dat2_real
        module procedure nc4_map_variable_dat2_int
        module procedure nc4_map_variable_dat3_real
        module procedure nc4_map_variable_dat3_int
        module procedure nc4_map_variable_dat4_real
        module procedure nc4_map_variable_dat4_int
        module procedure nc4_map_variable_dat5_real
        module procedure nc4_map_variable_dat5_int
    end interface

    interface nc4_get_fillvalue
        module procedure nc4_get_fillvalue_real
        module procedure nc4_get_fillvalue_int
        module procedure nc4_get_fillvalue_char
    end interface

    interface nc4_get_data
        module procedure nc4_get_data_scalar_real
        module procedure nc4_get_data_scalar_int
        module procedure nc4_get_data_scalar_char
        module procedure nc4_get_data_1d_real
        module procedure nc4_get_data_1d_int
        module procedure nc4_get_data_1d_char
        module procedure nc4_get_data_2d_real
        module procedure nc4_get_data_2d_int
        module procedure nc4_get_data_3d_real
        module procedure nc4_get_data_3d_int
        module procedure nc4_get_data_4d_real
        module procedure nc4_get_data_4d_int
        module procedure nc4_get_data_5d_real
        module procedure nc4_get_data_5d_int
    end interface

    interface nc4_get_variable
        module procedure nc4_get_variable_scalar_real
        module procedure nc4_get_variable_scalar_int
        module procedure nc4_get_variable_scalar_char
        module procedure nc4_get_variable_1d_real
        module procedure nc4_get_variable_1d_int
        module procedure nc4_get_variable_1d_char
        module procedure nc4_get_variable_2d_real
        module procedure nc4_get_variable_2d_int
        module procedure nc4_get_variable_3d_real
        module procedure nc4_get_variable_3d_int
        module procedure nc4_get_variable_4d_real
        module procedure nc4_get_variable_4d_int
        module procedure nc4_get_variable_5d_real
        module procedure nc4_get_variable_5d_int
    end interface

    interface nc4_add_attribute
        module procedure nc4_add_attribute_real
        module procedure nc4_add_attribute_int
        module procedure nc4_add_attribute_char
    end interface

    interface nc4_add_dimension
        module procedure nc4_define_dimension
    end interface

    interface nc4_add_data
        module procedure nc4_add_data_scalar_real
        module procedure nc4_add_data_scalar_int
        module procedure nc4_add_data_scalar_char
        module procedure nc4_add_data_1d_real
        module procedure nc4_add_data_1d_int
        module procedure nc4_add_data_1d_char
        module procedure nc4_add_data_2d_real
        module procedure nc4_add_data_2d_int
        module procedure nc4_add_data_3d_real
        module procedure nc4_add_data_3d_int
        module procedure nc4_add_data_4d_real
        module procedure nc4_add_data_4d_int
        module procedure nc4_add_data_5d_real
        module procedure nc4_add_data_5d_int
    end interface

    interface nc4_add_variable
        module procedure nc4_add_variable_scalar_real
        module procedure nc4_add_variable_scalar_int
        module procedure nc4_add_variable_scalar_char
        module procedure nc4_add_variable_1d_real
        module procedure nc4_add_variable_1d_int
        module procedure nc4_add_variable_1d_char
        module procedure nc4_add_variable_2d_real
        module procedure nc4_add_variable_2d_int
        module procedure nc4_add_variable_3d_real
        module procedure nc4_add_variable_3d_int
        module procedure nc4_add_variable_4d_real
        module procedure nc4_add_variable_4d_int
        module procedure nc4_add_variable_5d_real
        module procedure nc4_add_variable_5d_int
    end interface

    contains

    subroutine nc4_open_input(fpath, quiet, iun, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fpath

        !> Input variables (optional).
        logical, intent(in), optional :: quiet

        !> Output variables.
        integer, intent(out) :: iun, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Print a message (if not quiet).
        if (present(quiet)) q = quiet
        if (.not. q) then
            call reset_tab()
            call print_message("READING: " // trim(fpath))
            call increase_tab()
        end if

        !> Open the file with read access.
        ierr = nf90_open(fpath, NF90_NOWRITE, iun)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            if (q) then
                call print_error("An error occurred opening the file (Code: " // trim(adjustl(code)) // "): " // trim(fpath))
            else
                call print_error("Unable to open the file (Code: " // trim(adjustl(code)) // ").")
            end if
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_inquire_file(iun, ndims, natts, nvars, dim_unlimited_id, ierr)

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables (optional).
        integer, intent(out), optional :: ndims, natts, nvars, dim_unlimited_id

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Set the return variables.
        ierr = 0

        !> Number of dimensions.
        if (present(ndims)) then
            z = nf90_inquire(iun, ndimensions = ndims)
            if (z /= NF90_NOERR) ierr = 1
        end if

        !> Number of attributes.
        if (present(natts)) then
            z = nf90_inquire(iun, nattributes = natts)
            if (z /= NF90_NOERR) ierr = 1
        end if

        !> Number of variables.
        if (present(nvars)) then
            z = nf90_inquire(iun, nvariables = nvars)
            if (z /= NF90_NOERR) ierr = 1
        end if

        !> Get the ID of the dimension of 'NF90_UNLIMITED' type.
        if (present(dim_unlimited_id)) then
            z = nf90_inquire(iun, unlimitedDimId = dim_unlimited_id)
            if (z /= NF90_NOERR) ierr = 1
        end if

        !> Check for errors.
        if (ierr /= 0) then
            call print_error("An error occurred reading the number of dimensions, attributes, or variables in the file.")
        end if

    end subroutine

    subroutine nc4_get_projection( &
        iun, projection, &
        datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
        name_dim_crs, &
        ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in), optional :: name_dim_crs

        !> Output variables.
        character(len = *), intent(out) :: projection

        !> Output variables (optional).
        character(len = *), intent(out), optional :: datum, zone_id
        real, intent(out), optional :: earth_radius, grid_north_pole_latitude, grid_north_pole_longitude
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) crs_name, field, code
        real val1, val2
        integer vid, z

        !> Initialize output variables.
        projection = 'UNKNOWN'
        if (present(datum)) datum = 'UNKNOWN'
        if (present(zone_id)) zone_id = 'UNKNOWN'
        if (present(earth_radius)) earth_radius = 0.0
        if (present(grid_north_pole_latitude)) grid_north_pole_latitude = 0.0
        if (present(grid_north_pole_longitude)) grid_north_pole_longitude = 0.0

        !> Create variable.
        crs_name = 'crs'
        if (present(name_dim_crs)) crs_name = adjustl(name_dim_crs)
        ierr = nf90_inq_varid(iun, crs_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "A CRS projection '" // trim(crs_name) // "' was not found in the file (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

        !> Get projection.
        ierr = nf90_get_att(iun, vid, 'grid_mapping_name', field)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The 'grid_mapping_name' attribute was not found for the variable in the file " // &
                "(Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            select case (lowercase(field))
                case ('latitude_longitude')

                    !> Latitude/longitude (compatible with 'LATLONG').
                    projection = 'LATLONG'

                    !> Get datum.
                    if (present(datum)) then
                        ierr = 0
                        z = nf90_get_att(iun, vid, 'semi_major_axis', val1)
                        if (z /= NF90_NOERR) then
                            write(code, FMT_GEN) z
                            call print_warning( &
                                "The 'semi_major_axis' for the projection '" // trim(field) // "' was not found " // &
                                "(Code: " // trim(adjustl(code)) // ").")
                            ierr = 1
                        end if
                        z = nf90_get_att(iun, vid, 'inverse_flattening', val2)
                        if (z /= NF90_NOERR) then
                            write(code, FMT_GEN) z
                            call print_warning( &
                                "The 'inverse_flattening' for the projection '" // trim(field) // "' was not found " // &
                                "(Code: " // trim(adjustl(code)) // ").")
                            ierr = 1
                        end if
                        if (ierr == 0) then

                            !> Ellipsoid/datum specification (from EnSim/GK manual; version: September, 2010).
                            if (val1 == 6378137.0 .and. val2 == 298.257223563) then
                                datum = 'WGS84'
                            else if (val1 == 6378135.0 .and. val2 == 298.26) then
                                datum = 'WGS72'
                            else if (val1 == 6378137.0 .and. val2 == 298.257222101) then
                                datum = 'NAD83'
                            else if (val1 == 6378206.4 .and. val2 == 294.9786982) then
                                datum = 'NAD27'
                            else if (val1 == 6371000.0 .and. val2 == 0.0) then
                                datum = 'SPHERE'
                            else
                                write(line, FMT_CSV) val1, val2
                                call print_warning( &
                                    "Unknown datum from the 'semi_major_axis' and 'inverse_flattening' attributes (" // &
                                    trim(adjustl(line)) // ").")
                            end if
                        end if
                    end if
                case ('rotated_latitude_longitude')

                    !> Rotated latitude/longitude (per netCDF specification -- not compatible with 'ROTLATLONG' via EnSim).
                    projection = 'rotated_latitude_longitude'

                    !> Get datum.
                    ierr = 0
                    if (present(earth_radius)) then
                        z = nf90_get_att(iun, vid, 'earth_radius', val1)
                        if (z /= NF90_NOERR) then
                            write(code, FMT_GEN) z
                            call print_warning( &
                                "The 'earth_radius' for the projection '" // trim(field) // "' was not found " // &
                                "(Code: " // trim(adjustl(code)) // ").")
                            ierr = 1
                        else
                            earth_radius = val1
                        end if
                    end if
                    if (present(grid_north_pole_latitude)) then
                        z = nf90_get_att(iun, vid, 'grid_north_pole_latitude', val1)
                        if (z /= NF90_NOERR) then
                            write(code, FMT_GEN) z
                            call print_warning( &
                                "The 'grid_north_pole_latitude' for the projection '" // trim(field) // "' was not found " // &
                                "(Code: " // trim(adjustl(code)) // ").")
                            ierr = 1
                        else
                            grid_north_pole_latitude = val1
                        end if
                    end if
                    if (present(grid_north_pole_longitude)) then
                        z = nf90_get_att(iun, vid, 'grid_north_pole_longitude', val1)
                        if (z /= NF90_NOERR) then
                            write(code, FMT_GEN) z
                            call print_warning( &
                                "The 'grid_north_pole_longitude' for the projection '" // trim(field) // "' was not found " // &
                                "(Code: " // trim(adjustl(code)) // ").")
                            ierr = 1
                        else
                            grid_north_pole_longitude = val1
                        end if
                    end if
                case default
                    call print_warning("Unknown or unsupported projection '" // trim(field) // "' in the file.")
                    ierr = 1
                    return
            end select
        end if

    end subroutine

    real(kind = EightByteReal) function nc4_time_from_date_components(year, month, day, hour, minutes, seconds, ierr)

        !* 'model_dates': for 'get_jday' and 'get_jdate' functions.
        use model_dates

        !> Input variables.
        integer, intent(in) :: year, month, day

        !> Input variables (optional).
        integer, intent(in), optional :: hour, minutes, seconds

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) field
        real(kind = EightByteReal) :: h = 0.0, m = 0.0, s = 0.0

        !> Variables.
        if (present(hour)) h = hour
        if (present(minutes)) m = minutes
        if (present(seconds)) s = seconds

        !> Check the reference year.
        if (year < 1601) then
            write(field, FMT_GEN) year
            call print_error( &
                "The reference year (" // trim(adjustl(field)) // ") of the specified date is less than 1601, " // &
                "which is not compatible with the Gregorian-based date parsing utilities of the model.")
            ierr = 1
        else

            !> Calculate the reference datetime in hours.
            nc4_time_from_date_components = &
                real( &
                    (1461*(year + 4800 + (month - 14)/12))/4 + (367*(month - 2 - 12*((month - 14)/12)))/12 - &
                    (3*((year + 4900 + (month - 14)/12)/100))/4 + day - 32075, kind = EightByteReal) + &
                h/24.0 + m/24.0/60.0 + s/24.0/60.0/60.0
            ierr = 0
        end if

    end function

    subroutine nc4_date_components_from_time(datetime, year, month, day, jday, hour, minutes, seconds, ierr)

        !* 'model_dates': for 'Julian2MonthDay' function.
        use model_dates

        !> Input variables.
        real(kind = EightByteReal), intent(in) :: datetime

        !> Output variables (optional).
        integer, intent(out), optional :: year, month, day, jday, hour, minutes, seconds

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer a, b, c, y, m, d, j, h, n, s

        !> Back-calculate components from the reference datetime in hours.
        a = 4*(floor(datetime) + 1401 + (((4*floor(datetime) + 274277)/146097)*3)/4 - 38) + 3
        b = mod(a, 1461)/4
        c = 5*b + 2
        d = (mod(c, 153))/5 + 1
        m = mod((c/153 + 2), 12) + 1
        y = a/1461 - 4716 + (12 + 2 - m)/12
        j = get_jday(m, d, y)
        s = mod(nint((datetime - floor(datetime))*24.0*60.0*60.0), 60)
        n = mod(nint((datetime - floor(datetime))*24.0*60.0 - s/60.0), 60)
        h = nint((datetime - floor(datetime))*24.0 - n/60.0 - s/60.0/60.0)
        ierr = 0

        !> Variables.
        if (present(year)) year = y
        if (present(month)) month = m
        if (present(day)) day = d
        if (present(jday)) jday = j
        if (present(hour)) hour = h
        if (present(minutes)) minutes = n
        if (present(seconds)) seconds = s

    end subroutine

    subroutine nc4_get_reference_time( &
        iun, &
        name_time, time_shift, &
        year, month, day, jday, hour, minutes, seconds, tid, dtype, units, reference_time, &
        ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_time
        real, intent(in), optional :: time_shift

        !> Output variables (optional).
        integer, intent(out), optional :: year, month, day, jday, hour, minutes, seconds, tid, dtype
        character(len = *), intent(out), optional :: units
        real(kind = EightByteReal), intent(out), optional :: reference_time

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) vname, field, code
        real(kind = EightByteReal) t0_r8
        integer y, m, d, h, n, s, v, x, z

        !> Get the ID of the variable.
        if (present(name_time)) then
            vname = trim(name_time)
        else
            vname = 'time'
        end if
        ierr = nf90_inq_varid(iun, vname, v)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(vname) // "' cound not be found in file (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else if (present(tid)) then
            tid = v
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, v, 'units', field)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error("The units cannot be found for the '" // trim(vname) // "' variable.")
            ierr = 1
            return
        else if (present(units)) then
            units = field
        end if

        !> Check the units of the time dimension.
        if (index(field, 'since') > 1) then
            call parse_datetime( &
                field((index(field, 'since') + 6):), &
                y, m, d, h, n, s, istat = z)
            if (btest(z, pstat%CONVERSION_ERROR)) z = 1
        else
            call print_error( &
                "An error occurred reading the units of '" // trim(vname) // "' from the file. " // &
                "The units should be in the format: [seconds/minutes/hours/days] since yyyy/MM/dd HH:mm:ss[.SSSSSS]")
            ierr = 1
            return
        end if

        !> Check the calendar type.
        ierr = nf90_get_att(iun, v, 'calendar', field)
        if (ierr /= NF90_NOERR .or. (lowercase(field) /= 'gregorian' .and. lowercase(field) /= 'standard')) then
            call print_warning( &
                "The reference calendar for the '" // trim(vname) // "' variable is not set or is not " // &
                "equal to 'gregorian' or 'standard'. Errors may occur with deriving time-stamps from the file.")
        end if

        !> Get the data type of the variable.
        !> Only integer, float, and double types are supported.
        ierr = nf90_inquire_variable(iun, v, xtype = x)
        if (ierr == NF90_NOERR) then
            if (present(dtype)) dtype = x
            select case (x)
                case (NF90_BYTE, NF90_SHORT, NF90_INT, NF90_INT64, NF90_FLOAT, NF90_DOUBLE)
                case default
                    ierr = NF90_EBADTYPE
            end select
        end if
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) x
            call print_error( &
                "The data type of the '" // trim(vname) // "' variable (" // trim(adjustl(code)) // ") is not supported.")
            ierr = 1
            return
        end if

        !> Calculate the reference date from the units of the time dimension.
        t0_r8 = nc4_time_from_date_components(y, m, d, h, n, s, ierr)
        if (ierr /= 0) return
        if (present(time_shift)) then
            t0_r8 = t0_r8 + real(time_shift, kind = EightByteReal)/24.0
        end if
        if (present(reference_time)) reference_time = t0_r8

        !> Recalculate the components.
        call nc4_date_components_from_time(t0_r8, year, month, day, jday, hour, minutes, seconds, ierr)

    end subroutine

    subroutine nc4_get_time( &
        iun, &
        tid, reference_time, units, name_time, time_shift, start, &
        year, month, day, jday, hour, minutes, seconds, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun

        !> Input variables (optional).
        integer, intent(in), optional :: tid
        real(kind = EightByteReal), intent(in), optional :: reference_time
        character(len = *), intent(in), optional :: units
        character(len = *), intent(in), optional :: name_time
        real, intent(in), optional :: time_shift
        integer, intent(in), optional :: start(:)

        !> Output variables (optional).
        integer, intent(out), optional :: year, month, day, jday, hour, minutes, seconds

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) vname, vunits, field, code
        integer i, vid
        real(kind = EightByteReal) t0, t1_r8(1)

        !> Set the variable name.
        if (present(name_time)) then
            vname = trim(name_time)
        else
            vname = 'time'
        end if

        !> Get the reference time and variables (if not provided).
        if (.not. present(tid) .or. .not. present(reference_time) .or. .not. present(units)) then
            call nc4_get_reference_time(iun, vname, time_shift, tid = vid, units = vunits, reference_time = t0, ierr = ierr)
            if (ierr /= 0) return
        end if

        !> Assign optional variables.
        if (present(tid)) vid = tid
        if (present(units)) vunits = units
        if (present(reference_time)) t0 = reference_time

        !> Get the time value.
        ierr = nf90_get_var(iun, vid, t1_r8, start = start)
        if (ierr /= 0) return

        !> Add the value to the reference time (converting to units of 'hours').
        i = index(vunits, 'since')
        if (i > 1) then
            select case (vunits(1:(i - 1)))
                case ('seconds')
                    t1_r8 = t0 + t1_r8/24.0/60.0/60.0
                case ('minutes')
                    t1_r8 = t0 + t1_r8/24.0/60.0
                case ('hours')
                    t1_r8 = t0 + t1_r8/24.0
                case ('days')
                    t1_r8 = t0 + t1_r8
                case default
                    call print_error( &
                        "Unsupported units for the '" // trim(vname) // "' variable. " // &
                        "The units should be in the format: [seconds/minutes/hours/days] since yyyy/MM/dd HH:mm:ss[.SSSSSS]")
                    ierr = 1
                    return
            end select
        end if

        !> Calculate the components.
        call nc4_date_components_from_time(t1_r8(1), year, month, day, jday, hour, minutes, seconds, ierr)

    end subroutine

    logical function nc4_inquire_attribute(iun, attribute_name, vid)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        integer, intent(in), optional :: vid

        !> Local variables.
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Check if the attribute exists.
        nc4_inquire_attribute = (nf90_inquire_attribute(iun, v, attribute_name) == NF90_NOERR)

    end function

    subroutine nc4_get_attribute_name(iun, aid, vid, attribute_name, ierr)

        !> Input variables.
        integer, intent(in) :: iun, aid

        !> Input variables (optional).
        integer, intent(in), optional :: vid

        !> Output variables.
        character(len = *), intent(out) :: attribute_name
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Get the attribute name.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if
        ierr = nf90_inq_attname(iun, v, aid, attribute_name)
        if (ierr /= NF90_NOERR) then
            write(field, FMT_GEN) aid
            write(code, FMT_GEN) ierr
            call print_error( &
                "The attribute with ID (" // trim(adjustl(field)) // ") was not found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

    end subroutine

    subroutine nc4_get_attribute_type(iun, vid, attribute_name, dtype, length, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out), optional :: dtype
        integer, intent(out), optional :: length
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if
        ierr = nf90_inquire_attribute(iun, v, attribute_name, xtype = dtype, len = length)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading information about the '" // trim(attribute_name) // "' attribute (Code: " // &
                trim(adjustl(code)) // ").")
            ierr = 1
            if (present(dtype)) dtype = 0
            if (present(length)) length = 0
        end if

    end subroutine

    subroutine nc4_get_attribute_real( &
        iun, attribute_name, &
        attribute_value, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        real, intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The attribute '" // trim(attribute_name) // "' cannot be found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            attribute_value = NF90_FILL_REAL
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_attribute_int( &
        iun, attribute_name, &
        attribute_value, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The attribute '" // trim(attribute_name) // "' cannot be found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            attribute_value = NF90_FILL_INT
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_attribute_char( &
        iun, attribute_name, &
        attribute_value, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name

        !> Input variables (optional).
        integer, intent(in), optional :: vid

        !> Output variables.
        character(len = *), intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The attribute '" // trim(attribute_name) // "' cannot be found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            attribute_value = NF90_FILL_CHAR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    logical function nc4_inquire_dimension(iun, dimension_name, did)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: dimension_name

        !> Output variables (optional).
        integer, intent(out), optional :: did

        !> Local variables.
        integer d

        !> Check if the dimension exists.
        nc4_inquire_dimension = (nf90_inq_dimid(iun, dimension_name, d) == NF90_NOERR)
        if (present(did)) did = d

    end function

    subroutine nc4_get_dimension( &
        iun, dim_name, &
        did, dim_length, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: dim_name

        !> Output variables (optional).
        integer, intent(out), optional :: did, dim_length

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer d

        !> Get the ID of the dimension.
        ierr = nf90_inq_dimid(iun, dim_name, d)
        if (present(did)) did = d
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The dimension '" // trim(dim_name) // "' was not found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

        !> Get the size of the dimension.
        if (present(dim_length)) then
            ierr = nf90_inquire_dimension(iun, d, len = dim_length)
            if (ierr /= NF90_NOERR) then
                call print_error( &
                    "An error occurred reading the size of the '" // trim(dim_name) // "' dimension " // &
                    "(Code: " // trim(adjustl(code)) // ").")
                ierr = 1
                return
            end if
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_dimension_name(&
        iun, did, &
        dim_name, dim_length, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, did

        !> Output variables (optional).
        character(len = *), intent(out), optional :: dim_name
        integer, intent(out), optional :: dim_length

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) field, code

        !> Get attributes of the dimension.
        ierr = nf90_inquire_dimension(iun, did, name = dim_name, len = dim_length)
        if (ierr /= NF90_NOERR) then
            write(field, FMT_GEN) did
            write(code, FMT_GEN) ierr
            call print_error( &
                "The dimension with ID (" // trim(adjustl(field)) // ") was not found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

    end subroutine

    logical function nc4_inquire_variable(iun, variable_name)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: variable_name

        !> Local variables.
        integer v

        !> Check if the variable exists.
        nc4_inquire_variable = (nf90_inq_varid(iun, variable_name, v) == NF90_NOERR)

    end function

    subroutine nc4_get_variable_name(iun, vid, standard_name, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid

        !> Output variables.
        character(len = *), intent(out) :: standard_name
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) field, code

        !> Get the variable ID.
        ierr = nf90_inquire_variable(iun, vid, name = standard_name)
        if (ierr /= NF90_NOERR) then
            write(field, FMT_GEN) vid
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable with ID (" // trim(adjustl(field)) // ") was not found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

    end subroutine

    subroutine nc4_get_variable_attributes( &
        iun, standard_name, &
        vid, long_name, units, dtype, ndims, dimids, natts, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *) standard_name

        !> Output variables.
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids
        integer, intent(out), optional :: natts
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer n, v

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, v)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' was not found (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            if (present(vid)) vid = v
        end if

        !> Get the long name of the variable.
        if (present(long_name)) then
            ierr = nf90_get_att(iun, v, 'long_name', long_name)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the long_name of the '" // trim(standard_name) // "' variable " // &
                    "(Code: " // trim(adjustl(code)) // ").")
                long_name = ''
            end if
        end if

        !> Get the units of the variable.
        if (present(units)) then
            ierr = nf90_get_att(iun, v, 'units', units)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the units of the '" // trim(standard_name) // "' variable " // &
                    "(Code: " // trim(adjustl(code)) // ").")
                units = ''
            end if
        end if

        !> Get the data type of the variable.
        if (present(dtype)) then
            dtype = 0
            ierr = nf90_inquire_variable(iun, v, xtype = dtype)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_error( &
                    "An error occurred reading the data type of the '" // trim(standard_name) // "' variable " // &
                    "(Code: " // trim(adjustl(code)) // ").")
                ierr = 1
            end if
        end if

        !> Get the dimension IDs of the variable.
        if (present(ndims) .or. present(dimids)) then
            ierr = nf90_inquire_variable(iun, v, ndims = n)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the number of dimensions of the '" // trim(standard_name) // "' variable " // &
                    "(Code: " // trim(adjustl(code)) // ").")
                n = 0
            else
                if (present(dimids)) then
                    if (allocated(dimids)) deallocate(dimids)
                    allocate(dimids(n), stat = ierr)
                    if (ierr /= 0) then
                        write(code, FMT_GEN) ierr
                        call print_warning( &
                            "An error occurred allocating dimensions for the '" // trim(standard_name) // "' variable " // &
                            "(Code: " // trim(adjustl(code)) // ").")
                    else
                        ierr = nf90_inquire_variable(iun, v, dimids = dimids)
                        if (ierr /= NF90_NOERR) then
                            write(code, FMT_GEN) ierr
                            call print_warning( &
                                "An error occurred reading the dimensions of the '" // trim(standard_name) // "' variable " // &
                                "(Code: " // trim(adjustl(code)) // ").")
                        end if
                    end if
                end if
                if (present(ndims)) ndims = n
            end if
        end if

        !> Get attributes.
        if (present(natts)) then
            ierr = nf90_inquire_variable(iun, v, natts = natts)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the number of attributes for the '" // trim(standard_name) // &
                    "' variable (Code: " // trim(adjustl(code)) // ").")
            end if
        end if

    end subroutine

    subroutine nc4_get_dimension_order( &
        iun, standard_name, dim1_name, dim1_order, &
        dim2_name, dim2_order, dim3_name, dim3_order, dim4_name, dim4_order, dim5_name, dim5_order, &
        dim6_name, dim6_order, dim7_name, dim7_order, &
        dim_names, dim_lengths, dim_unlimited_id, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: dim2_name, dim3_name, dim4_name, dim5_name, dim6_name, dim7_name

        !> Output variables.
        integer, intent(out) :: dim1_order, ierr

        !> Output variables (optional).
        integer, intent(out), optional :: &
            dim2_order, dim3_order, dim4_order, dim5_order, dim6_order, dim7_order, dim_unlimited_id
        character(len = DEFAULT_FIELD_LENGTH), dimension(:), allocatable, optional :: dim_names
        integer, dimension(:), allocatable, optional :: dim_lengths

        !> Local variables.
        integer l, n, i, z
        integer, dimension(:), allocatable :: dimids
        character(len = DEFAULT_FIELD_LENGTH) field, code, dim_name

        !> Get the dimension IDs of the variable.
        call nc4_get_variable_attributes(iun, standard_name, ndims = n, dimids = dimids, ierr = ierr)
        if (ierr /= 0) return

        !> Check for a dimension of 'NF90_UNLIMITED' type.
        if (present(dim_unlimited_id)) then
            call nc4_inquire_file(iun, dim_unlimited_id = dim_unlimited_id, ierr = ierr)
            if (ierr /= 0) return
        end if

        !> Allocate the variable for dimension names (output).
        if (present(dim_names)) allocate(dim_names(n))

        !> Allocate the variable for dimension lengths.
        if (present(dim_lengths)) allocate(dim_lengths(n))

        !> Assign the IDs of the dimensions.
        ierr = 0
        do i = 1, n

            !> Get the dimension name.
            z = nf90_inquire_dimension(iun, dimids(i), name = dim_name, len = l)
            if (z /= NF90_NOERR) then
                write(field, FMT_GEN) i
                write(code, FMT_GEN) z
                call print_warning( &
                    "An error occurred reading the name of dimension " // trim(adjustl(field)) // " of the '" // &
                    trim(standard_name) // "' variable (Code: " // trim(adjustl(code)) // ").")
                ierr = 1
                z = 0
            else

                !> Save the dimension name.
                if (present(dim_names)) dim_names(i) = adjustl(dim_name)

                !> Save the dimension length.
                if (present(dim_lengths)) dim_lengths(i) = l

                !> Check against the provided names.
                if (lowercase(dim_name) == lowercase(dim1_name)) then
                    dim1_order = i
                else if (present(dim2_name)) then
                    if (lowercase(dim_name) == lowercase(dim2_name)) then
                        if (present(dim2_order)) dim2_order = i
                    else if (present(dim3_name)) then
                        if (lowercase(dim_name) == lowercase(dim3_name)) then
                            if (present(dim3_order)) dim3_order = i
                        else if (present(dim4_name)) then
                            if (lowercase(dim_name) == lowercase(dim4_name)) then
                                if (present(dim4_order)) dim4_order = i
                            else if (present(dim5_name)) then
                                if (lowercase(dim_name) == lowercase(dim5_name)) then
                                    if (present(dim5_order)) dim5_order = i
                                else if (present(dim6_name)) then
                                    if (lowercase(dim_name) == lowercase(dim6_name)) then
                                        if (present(dim6_order)) dim6_order = i
                                    else if (present(dim7_name)) then
                                        if (lowercase(dim_name) == lowercase(dim7_name)) then
                                            if (present(dim7_order)) dim7_order = i
                                        else
                                            if (present(dim7_order)) dim7_order = -1
                                            z = 1
                                        end if
                                    else
                                        if (present(dim6_order)) dim6_order = -1
                                        z = 1
                                    end if
                                else
                                    if (present(dim5_order)) dim5_order = -1
                                    z = 1
                                end if
                            else
                                if (present(dim4_order)) dim4_order = -1
                                z = 1
                            end if
                        else
                            if (present(dim3_order)) dim3_order = -1
                            z = 1
                        end if
                    else
                        if (present(dim2_order)) dim2_order = -1
                        z = 1
                    end if
                else
                    dim1_order = -1
                    z = 1
                end if

                !> Try to derive the unassigned dimension when all other orders were found.
                if (n == 2 .and. present(dim2_order)) then
                    if (dim2_order == -1) then
                        if (dim1_order == 1) then
                            dim2_order = 2
                        else
                            dim2_order = 1
                        end if
                        z = 0
                    end if
                end if

                !> Check for unassigned dimensions.
                if (z /= 0) then
                    call print_warning( &
                        "The dimension '" // trim(adjustl(dim_name)) // "' is not associated with the '" // &
                        trim(standard_name) // "' variable.")
                    z = 0
                end if
            end if
        end do

    end subroutine

    subroutine nc4_map_variable_dat2_real( &
        iun, standard_name, dat, dat2, &
        dim1_order, dim2_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order
        character(len = *), intent(in) :: standard_name
        real, dimension(:, :), intent(in) :: dat2

        !> Input/output variables.
        real, dimension(:, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) size1, size2, field, code
        integer i, z

        !> Initialize output variable.
        ierr = 0

        !> Check variable dimensions.
        z = 0
        do i = 1, size(shape(dat))
            if (dim1_order == i .and. size(dat2, dim1_order) /= size(dat, 1)) then
                write(code, FMT_GEN) dim1_order
                write(size2, FMT_GEN) size(dat2, dim1_order)
                z = 1
            else if (dim2_order == i .and. size(dat2, dim2_order) /= size(dat, 2)) then
                write(code, FMT_GEN) dim2_order
                write(size2, FMT_GEN) size(dat2, dim2_order)
                z = 1
            end if

            !> Check for errors.
            if (z /= 0) then
                write(field, FMT_GEN) i
                write(size1, FMT_GEN) size(dat, i)
                call print_error( &
                    "The size of the internal variable in dimension " // trim(adjustl(field)) // " (" // &
                    trim(adjustl(size1)) // ") is different than the size of the mapped column " // trim(adjustl(code)) // &
                    " (" // trim(adjustl(size2)) // ") of the '" // trim(standard_name) // "' variable read from file.")
                ierr = 1
                z = 0
            end if
        end do
        if (ierr /= 0) return

        !> Map the field.
        if (dim1_order == 1 .and. dim2_order == 2) then
            dat = dat2
        else
            dat = transpose(dat2)
        end if

    end subroutine

    subroutine nc4_map_variable_dat2_int( &
        iun, standard_name, dat, dat2, &
        dim1_order, dim2_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order
        character(len = *), intent(in) :: standard_name
        integer, dimension(:, :), intent(in) :: dat2

        !> Input/output variables.
        integer, dimension(:, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        real, dimension(size(dat, 1), size(dat, 2)) :: dat_d
        real, dimension(size(dat2, 1), size(dat2, 2)) :: dat2_d

        !> Call base routine.
        dat2_d = real(dat2)
        call nc4_map_variable( &
            iun, standard_name, dat_d, dat2_d, &
            dim1_order, dim2_order, &
            ierr)
        if (ierr /= 0) return
        dat = int(dat_d)

    end subroutine

    subroutine nc4_map_variable_dat3_real( &
        iun, standard_name, dat, dat3, &
        dim1_order, dim2_order, dim3_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order, dim3_order
        character(len = *), intent(in) :: standard_name
        real, dimension(:, :, :), intent(in) :: dat3

        !> Input output variables.
        real, dimension(:, :, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) size1, size2, field, code
        integer d1, d2, d3, i, z

        !> Initialize output variable.
        ierr = 0

        !> Check variable dimensions.
        z = 0
        do i = 1, size(shape(dat))
            if (dim1_order == i .and. size(dat3, dim1_order) /= size(dat, 1)) then
                write(code, FMT_GEN) dim1_order
                write(size2, FMT_GEN) size(dat3, dim1_order)
                z = 1
            else if (dim2_order == i .and. size(dat3, dim2_order) /= size(dat, 2)) then
                write(code, FMT_GEN) dim2_order
                write(size2, FMT_GEN) size(dat3, dim2_order)
                z = 1
            else if (dim3_order == i .and. size(dat3, dim3_order) /= size(dat, 3)) then
                write(code, FMT_GEN) dim3_order
                write(size2, FMT_GEN) size(dat3, dim3_order)
                z = 1
            end if

            !> Check for errors.
            if (z /= 0) then
                write(field, FMT_GEN) i
                write(size1, FMT_GEN) size(dat, i)
                call print_error( &
                    "The size of the internal variable in dimension " // trim(adjustl(field)) // " (" // &
                    trim(adjustl(size1)) // ") is different than the size of the mapped column " // trim(adjustl(code)) // &
                    " (" // trim(adjustl(size2)) // ") of the '" // trim(standard_name) // "' variable read from file.")
                ierr = 1
                z = 0
            end if
        end do
        if (ierr /= 0) return

        !> Map the field.
        if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 3) then
            do d3 = 1, size(dat3, dim3_order)
                do d2 = 1, size(dat3, dim2_order)
                    do d1 = 1, size(dat3, dim1_order)
                        dat(d1, d2, d3) = dat3(d1, d2, d3)
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 3) then
            do d3 = 1, size(dat3, dim3_order)
                do d1 = 1, size(dat3, dim1_order)
                    do d2 = 1, size(dat3, dim2_order)
                        dat(d1, d2, d3) = dat3(d2, d1, d3)
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 2) then
            do d2 = 1, size(dat3, dim2_order)
                do d3 = 1, size(dat3, dim3_order)
                    do d1 = 1, size(dat3, dim1_order)
                        dat(d1, d2, d3) = dat3(d1, d3, d2)
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 2) then
            do d1 = 1, size(dat3, dim1_order)
                do d3 = 1, size(dat3, dim3_order)
                    do d2 = 1, size(dat3, dim2_order)
                        dat(d1, d2, d3) = dat3(d2, d3, d1)
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 1) then
            do d2 = 1, size(dat3, dim2_order)
                do d1 = 1, size(dat3, dim1_order)
                    do d3 = 1, size(dat3, dim3_order)
                        dat(d1, d2, d3) = dat3(d3, d1, d2)
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 1) then
            do d1 = 1, size(dat3, dim1_order)
                do d2 = 1, size(dat3, dim2_order)
                    do d3 = 1, size(dat3, dim3_order)
                        dat(d1, d2, d3) = dat3(d3, d2, d1)
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine nc4_map_variable_dat3_int( &
        iun, standard_name, dat, dat3, &
        dim1_order, dim2_order, dim3_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order, dim3_order
        character(len = *), intent(in) :: standard_name
        integer, dimension(:, :, :), intent(in) :: dat3

        !> Input/output variables.
        integer, dimension(:, :, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        real, dimension(size(dat, 1), size(dat, 2), size(dat, 3)) :: dat_d
        real, dimension(size(dat3, 1), size(dat3, 2), size(dat3, 3)) :: dat3_d

        !> Call base routine.
        dat3_d = real(dat3)
        call nc4_map_variable( &
            iun, standard_name, dat_d, dat3_d, &
            dim1_order, dim2_order, dim3_order, &
            ierr)
        if (ierr /= 0) return
        dat = int(dat_d)

    end subroutine

    subroutine nc4_map_variable_dat4_real( &
        iun, standard_name, dat, dat4, &
        dim1_order, dim2_order, dim3_order, dim4_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order, dim3_order, dim4_order
        character(len = *), intent(in) :: standard_name
        real, dimension(:, :, :, :), intent(in) :: dat4

        !> Input output variables.
        real, dimension(:, :, :, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) size1, size2, field, code
        integer d1, d2, d3, d4, i, z

        !> Initialize output variable.
        ierr = 0

        !> Check variable dimensions.
        z = 0
        do i = 1, size(shape(dat))
            if (dim1_order == i .and. size(dat4, dim1_order) /= size(dat, 1)) then
                write(code, FMT_GEN) dim1_order
                write(size2, FMT_GEN) size(dat4, dim1_order)
                z = 1
            else if (dim2_order == i .and. size(dat4, dim2_order) /= size(dat, 2)) then
                write(code, FMT_GEN) dim2_order
                write(size2, FMT_GEN) size(dat4, dim2_order)
                z = 1
            else if (dim3_order == i .and. size(dat4, dim3_order) /= size(dat, 3)) then
                write(code, FMT_GEN) dim3_order
                write(size2, FMT_GEN) size(dat4, dim3_order)
                z = 1
            else if (dim4_order == i .and. size(dat4, dim4_order) /= size(dat, 4)) then
                write(code, FMT_GEN) dim4_order
                write(size2, FMT_GEN) size(dat4, dim4_order)
                z = 1
            end if

            !> Check for errors.
            if (z /= 0) then
                write(field, FMT_GEN) i
                write(size1, FMT_GEN) size(dat, i)
                call print_error( &
                    "The size of the internal variable in dimension " // trim(adjustl(field)) // " (" // &
                    trim(adjustl(size1)) // ") is different than the size of the mapped column " // trim(adjustl(code)) // &
                    " (" // trim(adjustl(size2)) // ") of the '" // trim(standard_name) // "' variable read from file.")
                ierr = 1
                z = 0
            end if
        end do
        if (ierr /= 0) return

        !> Map the field.
        if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 4) then
            do d4 = 1, size(dat4, dim4_order)
                do d3 = 1, size(dat4, dim3_order)
                    do d2 = 1, size(dat4, dim2_order)
                        do d1 = 1, size(dat4, dim1_order)
                            dat(d1, d2, d3, d4) = dat4(d1, d2, d3, d4)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 4) then
            do d4 = 1, size(dat4, dim4_order)
                do d3 = 1, size(dat4, dim3_order)
                    do d1 = 1, size(dat4, dim1_order)
                        do d2 = 1, size(dat4, dim2_order)
                            dat(d1, d2, d3, d4) = dat4(d2, d1, d3, d4)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 4) then
            do d4 = 1, size(dat4, dim4_order)
                do d2 = 1, size(dat4, dim2_order)
                    do d3 = 1, size(dat4, dim3_order)
                        do d1 = 1, size(dat4, dim1_order)
                            dat(d1, d2, d3, d4) = dat4(d1, d3, d2, d4)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 4) then
            do d4 = 1, size(dat4, dim4_order)
                do d1 = 1, size(dat4, dim1_order)
                    do d3 = 1, size(dat4, dim3_order)
                        do d2 = 1, size(dat4, dim2_order)
                            dat(d1, d2, d3, d4) = dat4(d2, d3, d1, d4)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 4) then
            do d4 = 1, size(dat4, dim4_order)
                do d2 = 1, size(dat4, dim2_order)
                    do d1 = 1, size(dat4, dim1_order)
                        do d3 = 1, size(dat4, dim3_order)
                            dat(d1, d2, d3, d4) = dat4(d3, d1, d2, d4)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 4) then
            do d4 = 1, size(dat4, dim4_order)
                do d1 = 1, size(dat4, dim1_order)
                    do d2 = 1, size(dat4, dim2_order)
                        do d3 = 1, size(dat4, dim3_order)
                            dat(d1, d2, d3, d4) = dat4(d3, d2, d1, d4)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 3) then
            do d3 = 1, size(dat4, dim3_order)
                do d4 = 1, size(dat4, dim4_order)
                    do d2 = 1, size(dat4, dim2_order)
                        do d1 = 1, size(dat4, dim1_order)
                            dat(d1, d2, d3, d4) = dat4(d1, d2, d4, d3)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 3) then
            do d3 = 1, size(dat4, dim3_order)
                do d4 = 1, size(dat4, dim4_order)
                    do d1 = 1, size(dat4, dim1_order)
                        do d2 = 1, size(dat4, dim2_order)
                            dat(d1, d2, d3, d4) = dat4(d2, d1, d4, d3)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 3) then
            do d2 = 1, size(dat4, dim2_order)
                do d4 = 1, size(dat4, dim4_order)
                    do d3 = 1, size(dat4, dim3_order)
                        do d1 = 1, size(dat4, dim1_order)
                            dat(d1, d2, d3, d4) = dat4(d1, d3, d4, d2)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 3) then
            do d1 = 1, size(dat4, dim1_order)
                do d4 = 1, size(dat4, dim4_order)
                    do d3 = 1, size(dat4, dim3_order)
                        do d2 = 1, size(dat4, dim2_order)
                            dat(d1, d2, d3, d4) = dat4(d2, d3, d4, d1)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 3) then
            do d2 = 1, size(dat4, dim2_order)
                do d4 = 1, size(dat4, dim4_order)
                    do d1 = 1, size(dat4, dim1_order)
                        do d3 = 1, size(dat4, dim3_order)
                            dat(d1, d2, d3, d4) = dat4(d3, d1, d4, d2)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 3) then
            do d1 = 1, size(dat4, dim1_order)
                do d4 = 1, size(dat4, dim4_order)
                    do d2 = 1, size(dat4, dim2_order)
                        do d3 = 1, size(dat4, dim3_order)
                            dat(d1, d2, d3, d4) = dat4(d3, d2, d4, d1)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 2) then
            do d2 = 1, size(dat4, dim2_order)
                do d3 = 1, size(dat4, dim3_order)
                    do d4 = 1, size(dat4, dim4_order)
                        do d1 = 1, size(dat4, dim1_order)
                            dat(d1, d2, d3, d4) = dat4(d1, d4, d3, d2)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 2) then
            do d1 = 1, size(dat4, dim1_order)
                do d3 = 1, size(dat4, dim3_order)
                    do d4 = 1, size(dat4, dim4_order)
                        do d2 = 1, size(dat4, dim2_order)
                            dat(d1, d2, d3, d4) = dat4(d2, d4, d3, d1)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 2) then
            do d3 = 1, size(dat4, dim3_order)
                do d2 = 1, size(dat4, dim2_order)
                    do d4 = 1, size(dat4, dim4_order)
                        do d1 = 1, size(dat4, dim1_order)
                            dat(d1, d2, d3, d4) = dat4(d1, d4, d2, d3)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 2) then
            do d3 = 1, size(dat4, dim3_order)
                do d1 = 1, size(dat4, dim1_order)
                    do d4 = 1, size(dat4, dim4_order)
                        do d2 = 1, size(dat4, dim2_order)
                            dat(d1, d2, d3, d4) = dat4(d2, d4, d1, d3)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 2) then
            do d1 = 1, size(dat4, dim1_order)
                do d2 = 1, size(dat4, dim2_order)
                    do d4 = 1, size(dat4, dim4_order)
                        do d3 = 1, size(dat4, dim3_order)
                            dat(d1, d2, d3, d4) = dat4(d3, d4, d2, d1)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 2) then
            do d2 = 1, size(dat4, dim2_order)
                do d1 = 1, size(dat4, dim1_order)
                    do d4 = 1, size(dat4, dim4_order)
                        do d3 = 1, size(dat4, dim3_order)
                            dat(d1, d2, d3, d4) = dat4(d3, d4, d1, d2)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 1) then
            do d1 = 1, size(dat4, dim1_order)
                do d3 = 1, size(dat4, dim3_order)
                    do d2 = 1, size(dat4, dim2_order)
                        do d4 = 1, size(dat4, dim4_order)
                            dat(d1, d2, d3, d4) = dat4(d4, d2, d3, d1)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 1) then
            do d2 = 1, size(dat4, dim2_order)
                do d3 = 1, size(dat4, dim3_order)
                    do d1 = 1, size(dat4, dim1_order)
                        do d4 = 1, size(dat4, dim4_order)
                            dat(d1, d2, d3, d4) = dat4(d4, d1, d3, d2)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 1) then
            do d1 = 1, size(dat4, dim1_order)
                do d2 = 1, size(dat4, dim2_order)
                    do d3 = 1, size(dat4, dim3_order)
                        do d4 = 1, size(dat4, dim4_order)
                            dat(d1, d2, d3, d4) = dat4(d4, d3, d2, d1)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 1) then
            do d2 = 1, size(dat4, dim2_order)
                do d1 = 1, size(dat4, dim1_order)
                    do d3 = 1, size(dat4, dim3_order)
                        do d4 = 1, size(dat4, dim4_order)
                            dat(d1, d2, d3, d4) = dat4(d4, d3, d1, d2)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 1) then
            do d3 = 1, size(dat4, dim3_order)
                do d2 = 1, size(dat4, dim2_order)
                    do d1 = 1, size(dat4, dim1_order)
                        do d4 = 1, size(dat4, dim4_order)
                            dat(d1, d2, d3, d4) = dat4(d4, d1, d2, d3)
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 1) then
            do d3 = 1, size(dat4, dim3_order)
                do d1 = 1, size(dat4, dim1_order)
                    do d2 = 1, size(dat4, dim2_order)
                        do d4 = 1, size(dat4, dim4_order)
                            dat(d1, d2, d3, d4) = dat4(d4, d2, d1, d3)
                        end do
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine nc4_map_variable_dat4_int( &
        iun, standard_name, dat, dat4, &
        dim1_order, dim2_order, dim3_order, dim4_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order, dim3_order, dim4_order
        character(len = *), intent(in) :: standard_name
        integer, dimension(:, :, :, :), intent(in) :: dat4

        !> Input/output variables.
        integer, dimension(:, :, :, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        real, dimension(size(dat, 1), size(dat, 2), size(dat, 3), size(dat, 4)) :: dat_d
        real, dimension(size(dat4, 1), size(dat4, 2), size(dat4, 3), size(dat4, 4)) :: dat4_d

        !> Call base routine.
        dat4_d = real(dat4)
        call nc4_map_variable( &
            iun, standard_name, dat_d, dat4_d, &
            dim1_order, dim2_order, dim3_order, dim4_order, &
            ierr)
        if (ierr /= 0) return
        dat = int(dat_d)

    end subroutine

    subroutine nc4_map_variable_dat5_real( &
        iun, standard_name, dat, dat5, &
        dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order
        character(len = *), intent(in) :: standard_name
        real, dimension(:, :, :, :, :), intent(in) :: dat5

        !> Input output variables.
        real, dimension(:, :, :, :, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) size1, size2, field, code
        integer d1, d2, d3, d4, d5, i, z

        !> Initialize output variable.
        ierr = 0

        !> Check variable dimensions.
        z = 0
        do i = 1, size(shape(dat))
            if (dim1_order == i .and. size(dat5, dim1_order) /= size(dat, 1)) then
                write(code, FMT_GEN) dim1_order
                write(size2, FMT_GEN) size(dat5, dim1_order)
                z = 1
            else if (dim2_order == i .and. size(dat5, dim2_order) /= size(dat, 2)) then
                write(code, FMT_GEN) dim2_order
                write(size2, FMT_GEN) size(dat5, dim2_order)
                z = 1
            else if (dim3_order == i .and. size(dat5, dim3_order) /= size(dat, 3)) then
                write(code, FMT_GEN) dim3_order
                write(size2, FMT_GEN) size(dat5, dim3_order)
                z = 1
            else if (dim4_order == i .and. size(dat5, dim4_order) /= size(dat, 4)) then
                write(code, FMT_GEN) dim4_order
                write(size2, FMT_GEN) size(dat5, dim4_order)
                z = 1
            else if (dim5_order == i .and. size(dat5, dim5_order) /= size(dat, 5)) then
                write(code, FMT_GEN) dim5_order
                write(size2, FMT_GEN) size(dat5, dim5_order)
                z = 1
            end if

            !> Check for errors.
            if (z /= 0) then
                write(field, FMT_GEN) i
                write(size1, FMT_GEN) size(dat, i)
                call print_error( &
                    "The size of the internal variable in dimension " // trim(adjustl(field)) // " (" // &
                    trim(adjustl(size1)) // ") is different than the size of the mapped column " // trim(adjustl(code)) // &
                    " (" // trim(adjustl(size2)) // ") of the '" // trim(standard_name) // "' variable read from file.")
                ierr = 1
                z = 0
            end if
        end do
        if (ierr /= 0) return

        !> Map the field.
        if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 4 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d2, d3, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 4 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d3 = 1, size(dat5, dim3_order)
                         do d1 = 1, size(dat5, dim1_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d1, d3, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 4 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d3, d2, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 4 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d3, d1, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 4 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d1, d2, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 4 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d2, d1, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 3 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d2, d4, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 3 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d1, d4, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 3 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d3, d4, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 3 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d3, d4, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 3 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d1, d4, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 3 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d2, d4, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 2 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d4, d3, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 2 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d4, d3, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 2 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d4, d2, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 2 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d4, d1, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 2 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d4, d2, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 2 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d4, d1, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 1 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d2, d3, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 1 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d1, d3, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 1 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d3, d2, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 1 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d3, d1, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 1 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d1, d2, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 1 .and. dim5_order == 5) then
            do d5 = 1, size(dat5, dim5_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d2, d1, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 5 .and. dim5_order == 4) then
            do d4 = 1, size(dat5, dim4_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d2, d3, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 5 .and. dim5_order == 4) then
            do d4 = 1, size(dat5, dim4_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d1, d3, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 5 .and. dim5_order == 4) then
            do d4 = 1, size(dat5, dim4_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d3, d2, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 5 .and. dim5_order == 4) then
            do d4 = 1, size(dat5, dim4_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d3, d1, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 5 .and. dim5_order == 4) then
            do d4 = 1, size(dat5, dim4_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d1, d2, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 5 .and. dim5_order == 4) then
            do d4 = 1, size(dat5, dim4_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d2, d1, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 5 .and. dim4_order == 3 .and. dim5_order == 4) then
            do d3 = 1, size(dat5, dim3_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d2, d4, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 5 .and. dim4_order == 3 .and. dim5_order == 4) then
            do d3 = 1, size(dat5, dim3_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d1, d4, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 5 .and. dim3_order == 2 .and. dim4_order == 3 .and. dim5_order == 4) then
            do d2 = 1, size(dat5, dim2_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d3, d4, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 3 .and. dim5_order == 4) then
            do d1 = 1, size(dat5, dim1_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d3, d4, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 5 .and. dim3_order == 1 .and. dim4_order == 3 .and. dim5_order == 4) then
            do d2 = 1, size(dat5, dim2_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d1, d4, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 3 .and. dim5_order == 4) then
            do d1 = 1, size(dat5, dim1_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d2, d4, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 5 .and. dim3_order == 3 .and. dim4_order == 2 .and. dim5_order == 4) then
            do d2 = 1, size(dat5, dim2_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d4, d3, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 2 .and. dim5_order == 4) then
            do d1 = 1, size(dat5, dim1_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d4, d3, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 5 .and. dim4_order == 2 .and. dim5_order == 4) then
            do d3 = 1, size(dat5, dim3_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d4, d2, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 5 .and. dim4_order == 2 .and. dim5_order == 4) then
            do d3 = 1, size(dat5, dim3_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d4, d1, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 2 .and. dim5_order == 4) then
            do d1 = 1, size(dat5, dim1_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d4, d2, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 5 .and. dim3_order == 1 .and. dim4_order == 2 .and. dim5_order == 4) then
            do d2 = 1, size(dat5, dim2_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d4, d1, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 1 .and. dim5_order == 4) then
            do d1 = 1, size(dat5, dim1_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d2, d3, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 5 .and. dim3_order == 3 .and. dim4_order == 1 .and. dim5_order == 4) then
            do d2 = 1, size(dat5, dim2_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d1, d3, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 1 .and. dim5_order == 4) then
            do d1 = 1, size(dat5, dim1_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d3, d2, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 5 .and. dim3_order == 2 .and. dim4_order == 1 .and. dim5_order == 4) then
            do d2 = 1, size(dat5, dim2_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d3, d1, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 5 .and. dim4_order == 1 .and. dim5_order == 4) then
            do d3 = 1, size(dat5, dim3_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d1, d2, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 5 .and. dim4_order == 1 .and. dim5_order == 4) then
            do d3 = 1, size(dat5, dim3_order)
                do d5 = 1, size(dat5, dim5_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d2, d1, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 5 .and. dim4_order == 4 .and. dim5_order == 3) then
            do d3 = 1, size(dat5, dim3_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d2, d5, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 5 .and. dim4_order == 4 .and. dim5_order == 3) then
            do d3 = 1, size(dat5, dim3_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d1, d5, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 5 .and. dim3_order == 2 .and. dim4_order == 4 .and. dim5_order == 3) then
            do d2 = 1, size(dat5, dim2_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d3, d5, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 4 .and. dim5_order == 3) then
            do d1 = 1, size(dat5, dim1_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d3, d5, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 5 .and. dim3_order == 1 .and. dim4_order == 4 .and. dim5_order == 3) then
            do d2 = 1, size(dat5, dim2_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d1, d5, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 4 .and. dim5_order == 3) then
            do d1 = 1, size(dat5, dim1_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d2, d5, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 5 .and. dim5_order == 3) then
            do d4 = 1, size(dat5, dim4_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d2, d5, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 5 .and. dim5_order == 3) then
            do d4 = 1, size(dat5, dim4_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d1, d5, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 5 .and. dim5_order == 3) then
            do d4 = 1, size(dat5, dim4_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d3, d5, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 2 .and. dim4_order == 5 .and. dim5_order == 3) then
            do d4 = 1, size(dat5, dim4_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d3, d5, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 5 .and. dim5_order == 3) then
            do d4 = 1, size(dat5, dim4_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d1, d5, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 1 .and. dim4_order == 5 .and. dim5_order == 3) then
            do d4 = 1, size(dat5, dim4_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d2, d5, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 5 .and. dim4_order == 2 .and. dim5_order == 3) then
            do d3 = 1, size(dat5, dim3_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d4, d5, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 5 .and. dim4_order == 2 .and. dim5_order == 3) then
            do d3 = 1, size(dat5, dim3_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d4, d5, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 5 .and. dim3_order == 4 .and. dim4_order == 2 .and. dim5_order == 3) then
            do d2 = 1, size(dat5, dim2_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d4, d5, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 2 .and. dim5_order == 3) then
            do d1 = 1, size(dat5, dim1_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d4, d5, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 5 .and. dim3_order == 1 .and. dim4_order == 2 .and. dim5_order == 3) then
            do d2 = 1, size(dat5, dim2_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d4, d5, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 2 .and. dim5_order == 3) then
            do d1 = 1, size(dat5, dim1_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d4, d5, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 5 .and. dim4_order == 1 .and. dim5_order == 3) then
            do d3 = 1, size(dat5, dim3_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d2, d5, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 5 .and. dim4_order == 1 .and. dim5_order == 3) then
            do d3 = 1, size(dat5, dim3_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d1, d5, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 5 .and. dim3_order == 2 .and. dim4_order == 1 .and. dim5_order == 3) then
            do d2 = 1, size(dat5, dim2_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d3, d5, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 1 .and. dim5_order == 3) then
            do d1 = 1, size(dat5, dim1_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d3, d5, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 5 .and. dim3_order == 4 .and. dim4_order == 1 .and. dim5_order == 3) then
            do d2 = 1, size(dat5, dim2_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d1, d5, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 1 .and. dim5_order == 3) then
            do d1 = 1, size(dat5, dim1_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d5 = 1, size(dat5, dim5_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d2, d5, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 5 .and. dim3_order == 3 .and. dim4_order == 4 .and. dim5_order == 2) then
            do d2 = 1, size(dat5, dim2_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d5, d3, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 4 .and. dim5_order == 2) then
            do d1 = 1, size(dat5, dim1_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d5, d3, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 5 .and. dim4_order == 4 .and. dim5_order == 2) then
            do d3 = 1, size(dat5, dim3_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d5, d2, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 5 .and. dim4_order == 4 .and. dim5_order == 2) then
            do d3 = 1, size(dat5, dim3_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d5, d1, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 4 .and. dim5_order == 2) then
            do d1 = 1, size(dat5, dim1_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d5, d2, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 5 .and. dim3_order == 1 .and. dim4_order == 4 .and. dim5_order == 2) then
            do d2 = 1, size(dat5, dim2_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d5, d1, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 5 .and. dim3_order == 4 .and. dim4_order == 3 .and. dim5_order == 2) then
            do d2 = 1, size(dat5, dim2_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d5, d4, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 3 .and. dim5_order == 2) then
            do d1 = 1, size(dat5, dim1_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d5, d4, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 5 .and. dim4_order == 3 .and. dim5_order == 2) then
            do d3 = 1, size(dat5, dim3_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d5, d4, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 5 .and. dim4_order == 3 .and. dim5_order == 2) then
            do d3 = 1, size(dat5, dim3_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d5, d4, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 3 .and. dim5_order == 2) then
            do d1 = 1, size(dat5, dim1_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d5, d4, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 5 .and. dim3_order == 1 .and. dim4_order == 3 .and. dim5_order == 2) then
            do d2 = 1, size(dat5, dim2_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d5, d4, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 5 .and. dim5_order == 2) then
            do d4 = 1, size(dat5, dim4_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d5, d3, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 1 .and. dim3_order == 3 .and. dim4_order == 5 .and. dim5_order == 2) then
            do d4 = 1, size(dat5, dim4_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d5, d3, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 1 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 5 .and. dim5_order == 2) then
            do d4 = 1, size(dat5, dim4_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d1 = 1, size(dat5, dim1_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d1, d5, d2, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 1 .and. dim3_order == 4 .and. dim4_order == 5 .and. dim5_order == 2) then
            do d4 = 1, size(dat5, dim4_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d2 = 1, size(dat5, dim2_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d2, d5, d1, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 1 .and. dim4_order == 5 .and. dim5_order == 2) then
            do d4 = 1, size(dat5, dim4_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d5, d2, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 1 .and. dim4_order == 5 .and. dim5_order == 2) then
            do d4 = 1, size(dat5, dim4_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d3 = 1, size(dat5, dim3_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d3, d5, d1, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 5 .and. dim3_order == 3 .and. dim4_order == 1 .and. dim5_order == 2) then
            do d2 = 1, size(dat5, dim2_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d5, d3, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 1 .and. dim5_order == 2) then
            do d1 = 1, size(dat5, dim1_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d5, d3, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 5 .and. dim4_order == 1 .and. dim5_order == 2) then
            do d3 = 1, size(dat5, dim3_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d5, d2, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 5 .and. dim4_order == 1 .and. dim5_order == 2) then
            do d3 = 1, size(dat5, dim3_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d5, d1, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 1 .and. dim5_order == 2) then
            do d1 = 1, size(dat5, dim1_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d5, d2, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 5 .and. dim3_order == 4 .and. dim4_order == 1 .and. dim5_order == 2) then
            do d2 = 1, size(dat5, dim2_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d5 = 1, size(dat5, dim5_order)
                            do d4 = 1, size(dat5, dim4_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d4, d5, d1, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 4 .and. dim5_order == 1) then
            do d1 = 1, size(dat5, dim1_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d2, d3, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 5 .and. dim3_order == 3 .and. dim4_order == 4 .and. dim5_order == 1) then
            do d2 = 1, size(dat5, dim2_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d3 = 1, size(dat5, dim3_order)
                         do d1 = 1, size(dat5, dim1_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d1, d3, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 4 .and. dim5_order == 1) then
            do d1 = 1, size(dat5, dim1_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d3, d2, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 5 .and. dim3_order == 2 .and. dim4_order == 4 .and. dim5_order == 1) then
            do d2 = 1, size(dat5, dim2_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d3, d1, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 5 .and. dim4_order == 4 .and. dim5_order == 1) then
            do d3 = 1, size(dat5, dim3_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d1, d2, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 5 .and. dim4_order == 4 .and. dim5_order == 1) then
            do d3 = 1, size(dat5, dim3_order)
                do d4 = 1, size(dat5, dim4_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d2, d1, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 3 .and. dim5_order == 1) then
            do d1 = 1, size(dat5, dim1_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d2, d4, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 5 .and. dim3_order == 4 .and. dim4_order == 3 .and. dim5_order == 1) then
            do d2 = 1, size(dat5, dim2_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d1, d4, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 3 .and. dim5_order == 1) then
            do d1 = 1, size(dat5, dim1_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d3, d4, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 5 .and. dim3_order == 2 .and. dim4_order == 3 .and. dim5_order == 1) then
            do d2 = 1, size(dat5, dim2_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d3, d4, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 5 .and. dim4_order == 3 .and. dim5_order == 1) then
            do d3 = 1, size(dat5, dim3_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d1, d4, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 5 .and. dim4_order == 3 .and. dim5_order == 1) then
            do d3 = 1, size(dat5, dim3_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d4 = 1, size(dat5, dim4_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d2, d4, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 2 .and. dim5_order == 1) then
            do d1 = 1, size(dat5, dim1_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d4, d3, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 5 .and. dim3_order == 3 .and. dim4_order == 2 .and. dim5_order == 1) then
            do d2 = 1, size(dat5, dim2_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d4, d3, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 5 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 2 .and. dim5_order == 1) then
            do d1 = 1, size(dat5, dim1_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d4, d2, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 5 .and. dim3_order == 4 .and. dim4_order == 2 .and. dim5_order == 1) then
            do d2 = 1, size(dat5, dim2_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d4, d1, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 5 .and. dim4_order == 2 .and. dim5_order == 1) then
            do d3 = 1, size(dat5, dim3_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d4, d2, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 5 .and. dim4_order == 2 .and. dim5_order == 1) then
            do d3 = 1, size(dat5, dim3_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d4 = 1, size(dat5, dim4_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d4, d1, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 2 .and. dim3_order == 3 .and. dim4_order == 5 .and. dim5_order == 1) then
            do d4 = 1, size(dat5, dim4_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d2, d3, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 4 .and. dim3_order == 3 .and. dim4_order == 5 .and. dim5_order == 1) then
            do d4 = 1, size(dat5, dim4_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d3 = 1, size(dat5, dim3_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d1, d3, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 4 .and. dim2_order == 3 .and. dim3_order == 2 .and. dim4_order == 5 .and. dim5_order == 1) then
            do d4 = 1, size(dat5, dim4_order)
                do d1 = 1, size(dat5, dim1_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d3, d2, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 4 .and. dim3_order == 2 .and. dim4_order == 5 .and. dim5_order == 1) then
            do d4 = 1, size(dat5, dim4_order)
                do d2 = 1, size(dat5, dim2_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d3 = 1, size(dat5, dim3_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d3, d1, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 2 .and. dim2_order == 3 .and. dim3_order == 4 .and. dim4_order == 5 .and. dim5_order == 1) then
            do d4 = 1, size(dat5, dim4_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d2 = 1, size(dat5, dim2_order)
                        do d1 = 1, size(dat5, dim1_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d1, d2, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (dim1_order == 3 .and. dim2_order == 2 .and. dim3_order == 4 .and. dim4_order == 5 .and. dim5_order == 1) then
            do d4 = 1, size(dat5, dim4_order)
                do d3 = 1, size(dat5, dim3_order)
                    do d1 = 1, size(dat5, dim1_order)
                        do d2 = 1, size(dat5, dim2_order)
                            do d5 = 1, size(dat5, dim5_order)
                                dat(d1, d2, d3, d4, d5) = dat5(d5, d2, d1, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine nc4_map_variable_dat5_int( &
        iun, standard_name, dat, dat5, &
        dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order
        character(len = *), intent(in) :: standard_name
        integer, dimension(:, :, :, :, :), intent(in) :: dat5

        !> Input/output variables.
        integer, dimension(:, :, :, :, :) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        real, dimension(size(dat, 1), size(dat, 2), size(dat, 3), size(dat, 4), size(dat, 5)) :: dat_d
        real, dimension(size(dat5, 1), size(dat5, 2), size(dat5, 3), size(dat5, 4), size(dat5, 5)) :: dat5_d

        !> Call base routine.
        dat5_d = real(dat5)
        call nc4_map_variable( &
            iun, standard_name, dat_d, dat5_d, &
            dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, &
            ierr)
        if (ierr /= 0) return
        dat = int(dat_d)

    end subroutine

    subroutine nc4_check_dtype(standard_name, dtype, dtype_expected, ierr)

        !> Input variables.
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dtype, dtype_expected

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = 0

        !> Check data type.
        code = ''
        select case (dtype_expected)
            case (NF90_INT)

                !> Integer, including byte, short and long integers.
                select case (dtype)
                    case (NF90_INT64)
                        if (kind(1) == 4) then
                            call print_warning( &
                                "The variable '" // trim(standard_name) // "' being read from file as 'int64' " // &
                                "is being transferred to a variable of lower precision ('int'). Truncation may occur.")
                        end if
                    case (NF90_BYTE, NF90_SHORT, NF90_INT)
                    case default
                        code = 'int'
                end select
            case (NF90_REAL)

                !> Real, including byte, short and long integers, and float and double.
                select case (dtype)
                    case (NF90_DOUBLE)
                        if (kind(1.0) == 4) then
                            call print_warning( &
                                "The variable '" // trim(standard_name) // "' being read from file as 'double' " // &
                                "is being transferred to a variable of lower precision ('float'). Truncation may occur.")
                        end if
                    case (NF90_BYTE, NF90_SHORT, NF90_INT, NF90_INT64, NF90_FLOAT)
                    case default
                        code = 'real'
                end select
            case (NF90_CHAR)

                !> Character, excluding variable length string (NF90_STRING).
                if (dtype /= dtype_expected) code = 'char'
            case default

                !> Unrecognized.
                write(code, FMT_GEN) dtype_expected
                call print_error("Unknown data type (Code: " // trim(adjustl(code)) // ").")
                ierr = 1
                return
        end select

        !> Print error message.
        if (len_trim(code) > 0) then
            call print_error( &
                "The data type of the '" // trim(standard_name) // "' variable is not the expected data type of '" // &
                trim(code) // "'.")
            ierr = 1
        end if

    end subroutine

    subroutine nc4_get_fillvalue_real(iun, standard_name, vid, fill, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Output variables.
        real, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of the '" // trim(standard_name) // "' variable " // &
                "(Code: " // trim(adjustl(code)) // ").")
            fill = NF90_FILL_REAL
        end if

        !> Reset return status.
        ierr = 0

    end subroutine

    subroutine nc4_get_fillvalue_int(iun, standard_name, vid, fill, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Output variables.
        integer, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of the '" // trim(standard_name) // "' variable " // &
                "(Code: " // trim(adjustl(code)) // ").")
            fill = NF90_FILL_INT
        end if

        !> Reset return status.
        ierr = 0

    end subroutine

    subroutine nc4_get_fillvalue_char(iun, standard_name, vid, fill, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Output variables.
        character(len = 1), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of the '" // trim(standard_name) // "' variable " // &
                "(Code: " // trim(adjustl(code)) // ").")
            fill = NF90_FILL_CHAR
        end if

        !> Reset return status.
        ierr = 0

    end subroutine

    subroutine nc4_check_variable_dimensions(standard_name, size_dat, ndims, ierr)

        !> Input variables.
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: size_dat, ndims

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) field, code

        !> Check the dimensions of the variable.
        if (size_dat /= ndims) then
            write(field, FMT_GEN) size_dat
            write(code, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(code)) // ") does not match the expected number of dimensions " // &
                " of " // trim(adjustl(field)) // " for the '" // trim(standard_name) // "' variable.")
            ierr = 1
            return
        else

            !> Normal return status.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_check_variable( &
        iun, standard_name, &
        dtype_expected, size_dat, dim1_name, dim2_name, dim3_name, dim4_name, dim5_name, &
        vid, long_name, units, dtype, ndims, dimids, natts, &
        dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, dim_lengths, dim_unlimited_id, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dtype_expected
        character(len = *), intent(in), optional :: dim1_name, dim2_name, dim3_name, dim4_name, dim5_name

        !> Input/output variables (optional).
        integer, dimension(:), optional :: size_dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: &
            vid, dtype, ndims, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, dim_unlimited_id
        integer, dimension(:), allocatable, intent(out), optional :: dimids, dim_lengths
        integer, intent(out), optional :: natts

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer l, n, t, v

        !> Get variable attributes (return if an error occurred).
        call nc4_get_variable_attributes(iun, standard_name, v, long_name, units, t, n, dimids, natts, ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v
        if (present(dtype)) dtype = t
        if (present(ndims)) ndims = n

        !> Check the data type of the variable.
        if (present(dtype_expected)) then
            call nc4_check_dtype(standard_name, t, dtype_expected, ierr)
            if (ierr /= 0) return
        end if

        !> Check the dimensions of the variable.
        if (present(size_dat)) then
            call nc4_check_variable_dimensions(standard_name, size(size_dat), n, ierr)
            if (ierr /= 0) return
        end if

        !> Get the order of the dimensions.
        if (n > 1 .and. present(dim1_name)) then
            call nc4_get_dimension_order( &
                iun, standard_name, dim1_name, dim1_order, &
                dim2_name, dim2_order, dim3_name, dim3_order, dim4_name, dim4_order, dim5_name, dim5_order, &
                dim_lengths = dim_lengths, dim_unlimited_id = dim_unlimited_id, ierr = ierr)
            if (ierr /= 0) return
        end if

        !> Check the size of the dimensions.
        ierr = 0
        if (present(size_dat) .and. present(dim_lengths)) then
            if (present(dim1_order)) then
                if (dim1_order == -1 .and. present(dim1_name)) then
                    call print_error( &
                        "The dimension '" // trim(dim1_name) // "' was not found in the file.")
                    ierr = 1
                else if (size_dat(1) == 0) then
                    size_dat(1) = dim_lengths(dim1_order)
                else if (size_dat(1) /= dim_lengths(dim1_order)) then
                    write(field, FMT_GEN) size_dat(1)
                    write(code, FMT_GEN) dim_lengths(dim1_order)
                    call print_error( &
                        "The size of the '" // trim(standard_name) // "' variable in the 1st dimension (" // &
                        trim(adjustl(code)) // ") is different than the expected size of (" // trim(adjustl(field)) // ").")
                    ierr = 1
                end if
            end if
            if (present(dim2_order)) then
                if (dim2_order == -1 .and. present(dim2_name)) then
                    call print_error( &
                        "The dimension '" // trim(dim2_name) // "' was not found in the file.")
                    ierr = 1
                else if (size_dat(2) == 0) then
                    size_dat(2) = dim_lengths(dim2_order)
                else if (size_dat(2) /= dim_lengths(dim2_order)) then
                    write(field, FMT_GEN) size_dat(2)
                    write(code, FMT_GEN) dim_lengths(dim2_order)
                    call print_error( &
                        "The size of the '" // trim(standard_name) // "' variable in the 2nd dimension (" // &
                        trim(adjustl(code)) // ") is different than the expected size of (" // trim(adjustl(field)) // ").")
                    ierr = 1
                end if
            end if
            if (present(dim3_order)) then
                if (dim3_order == -1 .and. present(dim3_name)) then
                    call print_error( &
                        "The dimension '" // trim(dim3_name) // "' was not found in the file.")
                    ierr = 1
                else if (size_dat(3) == 0) then
                    size_dat(3) = dim_lengths(dim3_order)
                else if (size_dat(3) /= dim_lengths(dim3_order)) then
                    write(field, FMT_GEN) size_dat(3)
                    write(code, FMT_GEN) dim_lengths(dim3_order)
                    call print_error( &
                        "The size of the '" // trim(standard_name) // "' variable in the 3rd dimension (" // &
                        trim(adjustl(code)) // ") is different than the expected size of (" // trim(adjustl(field)) // ").")
                    ierr = 1
                end if
            end if
            if (present(dim4_order)) then
                if (dim4_order == -1 .and. present(dim4_name)) then
                    call print_error( &
                        "The dimension '" // trim(dim4_name) // "' was not found in the file.")
                    ierr = 1
                else if (size_dat(4) == 0) then
                    size_dat(4) = dim_lengths(dim4_order)
                else if (size_dat(4) /= dim_lengths(dim4_order)) then
                    write(field, FMT_GEN) size_dat(4)
                    write(code, FMT_GEN) dim_lengths(dim4_order)
                    call print_error( &
                        "The size of the '" // trim(standard_name) // "' variable in the 4th dimension (" // &
                        trim(adjustl(code)) // ") is different than the expected size of (" // trim(adjustl(field)) // ").")
                    ierr = 1
                end if
            end if
            if (present(dim5_order)) then
                if (dim5_order == -1 .and. present(dim5_name)) then
                    call print_error( &
                        "The dimension '" // trim(dim5_name) // "' was not found in the file.")
                    ierr = 1
                else if (size_dat(5) == 0) then
                    size_dat(5) = dim_lengths(dim5_order)
                else if (size_dat(5) /= dim_lengths(dim5_order)) then
                    write(field, FMT_GEN) size_dat(5)
                    write(code, FMT_GEN) dim_lengths(dim5_order)
                    call print_error( &
                        "The size of the '" // trim(standard_name) // "' variable in the 5th dimension (" // &
                        trim(adjustl(code)) // ") is different than the expected size of (" // trim(adjustl(field)) // ").")
                    ierr = 1
                end if
            end if
        else if (present(size_dat) .and. present(dim1_name)) then
            if (size_dat(1) == 0) then
                call nc4_get_dimension(iun, dim1_name, dim_length = size_dat(1), ierr = ierr)
            end if
        end if

    end subroutine

    subroutine nc4_get_data_scalar_real( &
        iun, standard_name, vid, dat, &
        quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input/output variables.
        real dat

        !> Input variables (optional).
        logical, intent(in), optional :: quiet

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                write(code, FMT_GEN) ierr
                call print_error( &
                    "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                    trim(adjustl(code)) // ").")
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_data_scalar_int( &
        iun, standard_name, vid, dat, &
        quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        logical, intent(in), optional :: quiet

        !> Input/output variables.
        integer dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                write(code, FMT_GEN) ierr
                call print_error( &
                    "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                    trim(adjustl(code)) // ").")
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_data_scalar_char( &
        iun, standard_name, vid, dat, &
        quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        logical, intent(in), optional :: quiet

        !> Input/output variables.
        character dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                write(code, FMT_GEN) ierr
                call print_error( &
                    "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                    trim(adjustl(code)) // ").")
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_data_1d_real( &
        iun, standard_name, vid, dat, &
        start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Input variables (optional).
        integer, dimension(:), intent(in), optional :: start
        logical, intent(in), optional :: quiet

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_data_1d_int( &
        iun, standard_name, vid, dat, &
        start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, dimension(:), intent(in), optional :: start
        logical, intent(in), optional :: quiet

        !> Input/output variables.
        integer, dimension(:) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_data_1d_char( &
        iun, standard_name, vid, dat, &
        start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, dimension(:), intent(in), optional :: start
        logical, intent(in), optional :: quiet

        !> Input/output variables.
        character(len = *), dimension(:) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_data_2d_real( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(2), dim1_order, dim2_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        real dat(:, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real, allocatable :: dat2(:, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat2(dim_lengths(1), dim_lengths(2)))
        else
            allocate(dat2(size(dat, 1), size(dat, 2)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat2, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if (present(dim1_order) .and. present(dim2_order)) then
                call nc4_map_variable(iun, standard_name, dat, dat2, dim1_order, dim2_order, ierr)
            else
                dat = dat2
            end if
        end if

    end subroutine

    subroutine nc4_get_data_2d_int( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(2), dim1_order, dim2_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        integer dat(:, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer, allocatable :: dat2(:, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat2(dim_lengths(1), dim_lengths(2)))
        else
            allocate(dat2(size(dat, 1), size(dat, 2)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat2, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if (present(dim1_order) .and. present(dim2_order)) then
                call nc4_map_variable(iun, standard_name, dat, dat2, dim1_order, dim2_order, ierr)
            else
                dat = dat2
            end if
        end if

    end subroutine

    subroutine nc4_get_data_3d_real( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, dim3_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(3), dim1_order, dim2_order, dim3_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        real dat(:, :, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real, allocatable :: dat3(:, :, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)))
        else
            allocate(dat3(size(dat, 1), size(dat, 2), size(dat, 3)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat3, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if (present(dim1_order) .and. present(dim2_order) .and. present(dim3_order)) then
                call nc4_map_variable(iun, standard_name, dat, dat3, dim1_order, dim2_order, dim3_order, ierr)
            else
                dat = dat3
            end if
        end if

    end subroutine

    subroutine nc4_get_data_3d_int( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, dim3_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(3), dim1_order, dim2_order, dim3_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        integer dat(:, :, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer, allocatable :: dat3(:, :, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)))
        else
            allocate(dat3(size(dat, 1), size(dat, 2), size(dat, 3)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat3, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if (present(dim1_order) .and. present(dim2_order) .and. present(dim3_order)) then
                call nc4_map_variable(iun, standard_name, dat, dat3, dim1_order, dim2_order, dim3_order, ierr)
            else
                dat = dat3
            end if
        end if

    end subroutine

    subroutine nc4_get_data_4d_real( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(4), dim1_order, dim2_order, dim3_order, dim4_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        real dat(:, :, :, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real, allocatable :: dat4(:, :, :, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat4(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)))
        else
            allocate(dat4(size(dat, 1), size(dat, 2), size(dat, 3), size(dat, 4)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat4, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if (present(dim1_order) .and. present(dim2_order) .and. present(dim3_order) .and. present(dim4_order)) then
                call nc4_map_variable(iun, standard_name, dat, dat4, dim1_order, dim2_order, dim3_order, dim4_order, ierr)
            else
                dat = dat4
            end if
        end if

    end subroutine

    subroutine nc4_get_data_4d_int( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(4), dim1_order, dim2_order, dim3_order, dim4_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        integer dat(:, :, :, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer, allocatable :: dat4(:, :, :, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat4(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)))
        else
            allocate(dat4(size(dat, 1), size(dat, 2), size(dat, 3), size(dat, 4)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat4, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if (present(dim1_order) .and. present(dim2_order) .and. present(dim3_order) .and. present(dim4_order)) then
                call nc4_map_variable(iun, standard_name, dat, dat4, dim1_order, dim2_order, dim3_order, dim4_order, ierr)
            else
                dat = dat4
            end if
        end if

    end subroutine

    subroutine nc4_get_data_5d_real( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(5), dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        real dat(:, :, :, :, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real, allocatable :: dat5(:, :, :, :, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat5(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4), dim_lengths(5)))
        else
            allocate(dat5(size(dat, 1), size(dat, 2), size(dat, 3), size(dat, 4), size(dat, 5)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat5, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if ( &
                present(dim1_order) .and. present(dim2_order) .and. present(dim3_order) .and. present(dim4_order) .and. &
                present(dim5_order)) then
                call nc4_map_variable( &
                    iun, standard_name, dat, dat5, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, ierr)
            else
                dat = dat5
            end if
        end if

    end subroutine

    subroutine nc4_get_data_5d_int( &
        iun, standard_name, vid, dat, &
        dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, start, quiet, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_lengths(5), dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, start(:)
        logical, intent(in), optional :: quiet

        !> Input/output variable.
        integer dat(:, :, :, :, :)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer, allocatable :: dat5(:, :, :, :, :)
        logical :: q = .false.

        !> Allocate output variable (in the order of dimensions in the file, mapped to desired dimensions later).
        if (present(dim_lengths)) then
            allocate(dat5(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4), dim_lengths(5)))
        else
            allocate(dat5(size(dat, 1), size(dat, 2), size(dat, 3), size(dat, 4), size(dat, 5)))
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat5, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            if (present(quiet)) q = quiet
            if (.not. q) then
                if (ierr == NF90_EINVALCOORDS) then

                    !> Generally signifies end of file (e.g., outside 'time' bound).
                    call print_remark("Reached end of file reading '" // trim(standard_name) //"'.")
                else

                    !> Other read error (print generic message).
                    write(code, FMT_GEN) ierr
                    call print_error( &
                        "An error occurred reading data from the '" // trim(standard_name) // "' variable (Code: " // &
                        trim(adjustl(code)) // ").")
                end if
            end if
            ierr = 1
        else

            !> No errors.
            ierr = 0

            !> Map variable.
            if ( &
                present(dim1_order) .and. present(dim2_order) .and. present(dim3_order) .and. present(dim4_order) .and. &
                present(dim5_order)) then
                call nc4_map_variable( &
                    iun, standard_name, dat, dat5, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, ierr)
            else
                dat = dat5
            end if
        end if

    end subroutine

    subroutine nc4_get_variable_scalar_real( &
        iun, standard_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Output variables.
        real, intent(out) :: dat, fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer v

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_REAL, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        ierr = nf90_get_var(iun, v, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading data from the '" // trim(standard_name) // "' variable.")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_variable_scalar_int( &
        iun, standard_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Output variables.
        integer, intent(out) :: dat, fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer v

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_INT, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        ierr = nf90_get_var(iun, v, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading data from the '" // trim(standard_name) // "' variable.")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_variable_scalar_char( &
        iun, standard_name, &
        name_dim_char_length, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim_char_length

        !> Output variables.
        character(len = *), intent(out) :: dat
        character(len = 1), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dim1_name, field, code
        character(len = :), allocatable :: dat_c
        integer, dimension(:), allocatable :: dim_lengths
        integer dim1_order, n, v

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_CHAR, &
            dim1_name = name_dim_char_length, dim1_order = dim1_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)

        !> Check the dimensions of the variable.
        n = size(dim_lengths)
        if (n /= 1) then
            write(code, FMT_GEN) n
            call print_error( &
                "The number of dimensions (" // trim(adjustl(code)) // ") is not supported for the scalar data type of " // &
                "'char' for the '" // trim(standard_name) // "' variable, " // &
                "which requires 1 dimension specifying the string length.")
            ierr = 1
            return
        end if

        !> Check the string length.
        if (len(dat) < dim_lengths(1)) then
            write(field, FMT_GEN) len(dat)
            write(code, FMT_GEN) dim_lengths(1)
            call print_warning( &
                "The length of the '" // trim(standard_name) // "' variable in the file (" // trim(adjustl(code)) // ") " // &
                "is greater than the lengh of the internal variable (" // trim(adjustl(field)) // "). " // &
                "Characters in excess of this length will be truncated.")
        end if

        !> Allocate the string.
        allocate(character(len = dim_lengths(1)) :: dat_c)

        !> Read variable.
        if (ierr == 0) ierr = nf90_get_var(iun, v, dat_c)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading data from the '" // trim(standard_name) // "' variable.")
            ierr = 1
        else
            dat = adjustl(dat_c)
            ierr = 0
        end if

    end subroutine

    subroutine nc4_get_variable_1d_real( &
        iun, standard_name, &
        name_dim, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim

        !> Input/output variables.
        real, dimension(:), allocatable :: dat

        !> Output variables.
        real, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dim1_name
        integer size_dat(1), dim1_order, v

        !> Check the dimension name.
        if (present(name_dim)) then
            dim1_name = trim(name_dim)
        else
            dim1_name = trim(standard_name)
        end if

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            size_dat(1) = size(dat)
        else
            size_dat(1) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_REAL, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) allocate(dat(size_dat(1)))
        call nc4_get_data(iun, standard_name, v, dat, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_1d_int( &
        iun, standard_name, &
        name_dim, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim

        !> Input/output variables.
        integer, dimension(:), allocatable :: dat

        !> Output variables.
        integer, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dim1_name
        integer size_dat(1), dim1_order, v

        !> Check the dimension name.
        if (present(name_dim)) then
            dim1_name = trim(name_dim)
        else
            dim1_name = trim(standard_name)
        end if

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            size_dat(1) = size(dat)
        else
            size_dat(1) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_INT, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) allocate(dat(size_dat(1)))
        call nc4_get_data(iun, standard_name, v, dat, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_1d_char( &
        iun, standard_name, &
        name_dim, name_dim_char_length, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim, name_dim_char_length

        !> Input/output variables.
        character(len = *), dimension(:), allocatable :: dat

        !> Output variables.
        character(len = 1), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dim1_name, field, code
        character(len = :), dimension(:), allocatable :: dat_c
        integer, dimension(:), allocatable :: dim_lengths
        integer dim1_order, dim2_order, n, v

        !> Check the dimension name.
        if (present(name_dim)) then
            dim1_name = trim(name_dim)
        else
            dim1_name = trim(standard_name)
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_CHAR, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = name_dim_char_length, dim2_order = dim2_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Check the dimensions of the variable.
        n = size(dim_lengths)
        if (n /= 2) then
            write(code, FMT_GEN) n
            call print_error( &
                "The number of dimensions (" // trim(adjustl(code)) // ") is not supported for the vector data type of " // &
                "'char' for the '" // trim(standard_name) // "' variable, " // &
                "which requires 2 dimensions specifying the size of the vector and the string length.")
            ierr = 1
            return
        end if

        !> Check the string length.
        if (len(dat) < dim_lengths(dim2_order)) then
            write(field, FMT_GEN) len(dat)
            write(code, FMT_GEN) dim_lengths(dim2_order)
            call print_warning( &
                "The length of the '" // trim(standard_name) // "' variable in the file (" // trim(adjustl(code)) // ") " // &
                "is greater than the lengh of the internal variable (" // trim(adjustl(field)) // "). " // &
                "Characters in excess of this length will be truncated.")
        end if

        !> Check the size of the dimension.
        if (allocated(dat)) then
            if (size(dat) /= dim_lengths(dim1_order)) then
                write(field, FMT_GEN) size(dat)
                write(code, FMT_GEN) dim_lengths(dim1_order)
                call print_error( &
                    "The size of the '" // trim(standard_name) // "' variable in the 1st dimension (" // &
                    trim(adjustl(code)) // ") is different than the expected size of (" // trim(adjustl(field)) // ").")
                ierr = 1
                return
            end if
        else

            !> Allocate the variable.
            allocate(dat(dim_lengths(dim1_order)))
        end if

        !> Allocate the string.
        allocate(character(len = dim_lengths(dim2_order)) :: dat_c(dim_lengths(dim1_order)))

        !> Read variable.
        call nc4_get_data(iun, standard_name, v, dat_c, ierr = ierr)
        if (ierr == 0) then
            dat = adjustl(dat_c)
        end if

    end subroutine

    subroutine nc4_get_variable_2d_real( &
        iun, standard_name, dim1_name, dim2_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name

        !> Input/output variable.
        real, allocatable :: dat(:, :)

        !> Output variables.
        real, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(2), dim1_order, dim2_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_REAL, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate(dat(dim_lengths(dim1_order), dim_lengths(dim2_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_2d_int( &
        iun, standard_name, dim1_name, dim2_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name

        !> Input/output variable.
        integer, allocatable :: dat(:, :)

        !> Output variables.
        integer, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(2), dim1_order, dim2_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_INT, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate(dat(dim_lengths(dim1_order), dim_lengths(dim2_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_3d_real( &
        iun, standard_name, dim1_name, dim2_name, dim3_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name, dim3_name

        !> Input/output variable.
        real, allocatable :: dat(:, :, :)

        !> Output variables.
        real, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(3), dim1_order, dim2_order, dim3_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_REAL, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            dim3_name = dim3_name, dim3_order = dim3_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate(dat(dim_lengths(dim1_order), dim_lengths(dim2_order), dim_lengths(dim3_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, dim3_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_3d_int( &
        iun, standard_name, dim1_name, dim2_name, dim3_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name, dim3_name

        !> Input/output variable.
        integer, allocatable :: dat(:, :, :)

        !> Output variables.
        integer, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(3), dim1_order, dim2_order, dim3_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_INT, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            dim3_name = dim3_name, dim3_order = dim3_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate(dat(dim_lengths(dim1_order), dim_lengths(dim2_order), dim_lengths(dim3_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, dim3_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_4d_real( &
        iun, standard_name, dim1_name, dim2_name, dim3_name, dim4_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name, dim3_name, dim4_name

        !> Input/output variable.
        real, allocatable :: dat(:, :, :, :)

        !> Output variables.
        real, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(4), dim1_order, dim2_order, dim3_order, dim4_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_REAL, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            dim3_name = dim3_name, dim3_order = dim3_order, &
            dim4_name = dim4_name, dim4_order = dim4_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate(dat(dim_lengths(dim1_order), dim_lengths(dim2_order), dim_lengths(dim3_order), dim_lengths(dim4_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_4d_int( &
        iun, standard_name, dim1_name, dim2_name, dim3_name, dim4_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name, dim3_name, dim4_name

        !> Input/output variable.
        integer, allocatable :: dat(:, :, :, :)

        !> Output variables.
        integer, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(4), dim1_order, dim2_order, dim3_order, dim4_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_INT, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            dim3_name = dim3_name, dim3_order = dim3_order, &
            dim4_name = dim4_name, dim4_order = dim4_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate(dat(dim_lengths(dim1_order), dim_lengths(dim2_order), dim_lengths(dim3_order), dim_lengths(dim4_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_5d_real( &
        iun, standard_name, dim1_name, dim2_name, dim3_name, dim4_name, dim5_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name, dim3_name, dim4_name, dim5_name

        !> Input/output variable.
        real, allocatable :: dat(:, :, :, :, :)

        !> Output variables.
        real, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(4), dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_REAL, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            dim3_name = dim3_name, dim3_order = dim3_order, &
            dim4_name = dim4_name, dim4_order = dim4_order, &
            dim5_name = dim5_name, dim5_order = dim5_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate( &
                dat(dim_lengths(dim1_order), dim_lengths(dim2_order), dim_lengths(dim3_order), dim_lengths(dim4_order), &
                    dim_lengths(dim5_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, ierr = ierr)

    end subroutine

    subroutine nc4_get_variable_5d_int( &
        iun, standard_name, dim1_name, dim2_name, dim3_name, dim4_name, dim5_name, &
        dat, fill, &
        vid, long_name, units, dtype, ndims, dimids, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dim1_name, dim2_name, dim3_name, dim4_name, dim5_name

        !> Input/output variable.
        integer, allocatable :: dat(:, :, :, :, :)

        !> Output variables.
        integer, intent(out) :: fill
        integer, intent(out) :: ierr

        !> Output variables (optional).
        character(len = *), intent(out), optional :: long_name, units
        integer, intent(out), optional :: vid, dtype, ndims
        integer, dimension(:), allocatable, intent(out), optional :: dimids

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer size_dat(4), dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, l, v

        !> Check the internal variable dimensions.
        if (allocated(dat)) then
            do l = 1, size(size_dat)
                size_dat(l) = size(dat, l)
            end do
        else
            size_dat(:) = 0
        end if

        !> Get variable information.
        call nc4_check_variable( &
            iun, standard_name, NF90_INT, &
            size_dat, &
            dim1_name = dim1_name, dim1_order = dim1_order, &
            dim2_name = dim2_name, dim2_order = dim2_order, &
            dim3_name = dim3_name, dim3_order = dim3_order, &
            dim4_name = dim4_name, dim4_order = dim4_order, &
            dim5_name = dim5_name, dim5_order = dim5_order, &
            vid = v, long_name = long_name, units = units, dtype = dtype, ndims = ndims, dimids = dimids, &
            dim_lengths = dim_lengths, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Get the fill value of the variable.
        call nc4_get_fillvalue(iun, standard_name, v, fill, ierr)
        if (ierr /= 0) return

        !> Read variable.
        if (.not. allocated(dat)) then
            allocate( &
                dat(dim_lengths(dim1_order), dim_lengths(dim2_order), dim_lengths(dim3_order), dim_lengths(dim4_order), &
                    dim_lengths(dim5_order)))
        end if
        call nc4_get_data( &
            iun, standard_name, v, dat, dim_lengths, dim1_order, dim2_order, dim3_order, dim4_order, dim5_order, ierr = ierr)

    end subroutine

    subroutine nc4_open_output(fpath, quiet, iun, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fpath

        !> Input variables (optional).
        logical, intent(in), optional :: quiet

        !> Output variables.
        integer, intent(out) :: iun, ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .false.

        !> Print a message (if not quiet).
        if (present(quiet)) q = quiet
        if (.not. q) then
            call reset_tab()
            call print_message("OPENING: " // trim(fpath) // " (for output)")
            call increase_tab()
        end if

        !> Open the file with write access.
        ierr = nf90_create(fpath, NF90_NETCDF4, iun)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            if (q) then
                call print_error("An error occurred opening the file (Code: " // trim(adjustl(code)) // "): " // trim(fpath))
            else
                call print_error("Unable to open the file (Code: " // trim(adjustl(code)) // ").")
            end if
            ierr = 1
            return
        end if

        !> Assign global meta attributes.
        ierr = NF90_NOERR
        write(line, FMT_DATETIME_DASHES_YMD) &
            ic%run_start%year, ic%run_start%month, ic%run_start%day, ic%run_start%hour, ic%run_start%mins, 0
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'title', 'SA_MESH model outputs')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'source', 'SA_MESH') !((version info, but somehow also configuration CLASS/SVS/etc..))
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'history', trim(adjustl(line)) // ' - Created.')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'references', 'SA_MESH') !(('https://wiki.usask.ca/display/MESH/'))

        !> Add coding convention.
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'Conventions', 'CF-1.6')

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error("Errors occurred while saving global attributes to the file (Code " // trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_projection( &
        iun, projection, &
        datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
        ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: projection

        !> Input variables (optional).
        character(len = *), intent(in), optional :: datum, zone_id
        real, intent(in), optional :: earth_radius, grid_north_pole_latitude, grid_north_pole_longitude

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer vid

        !> Create variable.
        ierr = nf90_def_var(iun, 'crs', NF90_INT, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error("An error occurred creating the projection in the file (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

        !> Assign projection.
        ierr = NF90_NOERR
        select case (lowercase(projection))
            case ('latlong')

                !> Regular lat/lon.
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping_name', 'latitude_longitude')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'longitude_of_prime_meridian', 0.0)

                !> Ellipsoid/datum specification (from EnSim/GK manual; version: September, 2010).
                if (present(datum)) then
                    select case (lowercase(datum))
                        case ('wgs84')
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378137.0)
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 298.257223563)
                        case ('wgs72')
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378135.0)
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 298.26)
                        case ('grs80', 'nad83')
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378137.0)
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 298.257222101)
                        case ('clarke1866', 'nad27')
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378206.4)
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 294.9786982)
                        case ('sphere')
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6371000.0)
                            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 0.0)
                    end select
                else
                    ierr = 1
                end if
            case ('rotated_latitude_longitude')

                !> netCDF format 'rotated_latitude_longitude' (not compatible with 'ROTLATLONG' for EnSim outputs).
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping_name', 'rotated_latitude_longitude')
                if (ierr == NF90_NOERR .and. present(earth_radius)) then
                    ierr = nf90_put_att(iun, vid, 'earth_radius', earth_radius)
                else
                    ierr = 1
                end if
                if (ierr == NF90_NOERR .and. present(grid_north_pole_latitude)) then
                    ierr = nf90_put_att(iun, vid, 'grid_north_pole_latitude', grid_north_pole_latitude)
                else
                    ierr = 1
                end if
                if (ierr == NF90_NOERR .and. present(grid_north_pole_longitude)) then
                    ierr = nf90_put_att(iun, vid, 'grid_north_pole_longitude', grid_north_pole_longitude)
                else
                    ierr = 1
                end if
            case default

                !> Unknown.
                call print_error("Unknown or unsupported projection '" // trim(projection) // "'.")
                ierr = 1
                return
        end select

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "Errors occurred saving the '" // trim(projection) // "' projection to the file (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_define_dimension(iun, dim_name, dim_length, did, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: dim_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_length

        !> Output variables.
        integer, intent(out) :: did, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Create dimension.
        if (present(dim_length)) then
            ierr = nf90_def_dim(iun, dim_name, dim_length, did)
        else
            ierr = nf90_def_dim(iun, dim_name, NF90_UNLIMITED, did)
        end if

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred adding the '" // trim(dim_name) // "' dimension to the file (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_set_time_units(iun, vtime, ts_freq, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vtime, ts_freq

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        character(len = DEFAULT_LINE_LENGTH) line

        !> Set units based on 'ts_freq'.
        write(line, FMT_DATETIME_DASHES_YMD) &
            ic%now%year, ic%now%month, ic%now%day, ic%now%hour, ic%now%mins, 0
        select case (ts_freq)
            case (FREQ_MONTHLY, FREQ_SEASONAL, FREQ_DAILY)
                ierr = nf90_put_att(iun, vtime, 'units', 'days since ' // trim(adjustl(line)))
            case (FREQ_YEARLY)
                ierr = nf90_put_att(iun, vtime, 'units', 'years since ' // trim(adjustl(line)))
            case (FREQ_HOURLY)
                ierr = nf90_put_att(iun, vtime, 'units', 'hours since ' // trim(adjustl(line)))
            case (FREQ_PTS)
                ierr = nf90_put_att(iun, vtime, 'units', 'minutes since ' // trim(adjustl(line)))
            case default
                ierr = nf90_put_att(iun, vtime, 'units', 'seconds since ' // trim(adjustl(line)))
        end select
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vtime, 'long_name', 'time')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vtime, 'standard_name', 'time')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vtime, 'axis', 'T')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vtime, 'calendar', 'gregorian')

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "Errors occurred saving attributes of the 'time' variable to the file (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_time(iun, dtype, ts_freq, dtime, vtime, ierr)

        !> Input variables.
        integer, intent(in) :: iun, dtype, ts_freq

        !> Output variables.
        integer, intent(out) :: dtime, vtime, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Create dimension for 'time'.
        call nc4_define_dimension(iun, 'time', did = dtime, ierr = ierr)
        if (ierr /= 0) return

        !> Create variable.
        ierr = nf90_def_var(iun, 'time', dtype, (/dtime/), vtime)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error("An error occurred creating the 'time' variable in the file (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        end if

        !> Set units based on 'ts_freq'.
        call nc4_set_time_units(iun, vtime, ts_freq, ierr)

    end subroutine

    subroutine nc4_set_coordinate_units(iun, projection, vlat, vlon, vrlat, vrlon, ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun, vlat, vlon
        character(len = *), intent(in) :: projection

        !> Input variables (optional).
        integer, intent(in), optional :: vrlat, vrlon

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        character(len = DEFAULT_LINE_LENGTH) line

        !> Assign projection.
        ierr = NF90_NOERR
        select case (lowercase(projection))
            case ('latlong')

                !> Regular lat/lon (latitude, longitude).
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'standard_name', 'latitude')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'long_name', 'latitude')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'units', 'degrees_north')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'axis', 'Y')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'standard_name', 'longitude')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'long_name', 'longitude')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'units', 'degrees_east')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'axis', 'X')
            case ('rotated_latitude_longitude')

                !> netCDF format 'rotated_latitude_longitude' (not compatible with 'ROTLATLONG' for EnSim outputs).
                !> (grid_latitude, grid_longitude, latitude, longitude).
                if (present(vrlat) .and. present(vrlon)) then
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlat, 'standard_name', 'grid_latitude')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlat, 'long_name', 'latitude in rotated pole grid')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlat, 'units', 'degrees')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlat, 'axis', 'Y')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlon, 'standard_name', 'grid_longitude')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlon, 'long_name', 'longitude in rotated pole grid')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlon, 'units', 'degrees')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vrlon, 'axis', 'X')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'standard_name', 'latitude')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'long_name', 'latitude')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlat, 'units', 'degrees_north')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'standard_name', 'longitude')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'long_name', 'longitude')
                    if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vlon, 'units', 'degrees_east')
                else
                    call print_error( &
                        "Missing the necessary attributes to define the '" // trim(projection) // "' reference coordinates.")
                    ierr = 1
                end if
            case default

                !> Unknown.
                call print_error("Unknown or unsupported projection '" // trim(projection) // "'.")
                ierr = 1
                return
        end select

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "Errors occurred setting attributes for the reference coordinates in the file (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_coordinates( &
        iun, projection, &
        lats, lons, rlats, rlons, xylats, xylons, &
        datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
        dim1_id, dim2_id, &
        ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: projection

        !> Input variables (optional).
        real, dimension(:), intent(in), optional :: lats, lons, rlats, rlons
        real, dimension(:, :), intent(in), optional :: xylats, xylons
        character(len = *), intent(in), optional :: datum, zone_id
        real, intent(in), optional :: &
            earth_radius, grid_north_pole_latitude, grid_north_pole_longitude

        !> Output variables (optional).
        integer, intent(out), optional :: dim1_id, dim2_id

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer :: dy = -1, dx = -1, vry = -1, vrx = -1, vy = -1, vx = -1

        !> Switch between projections.
        ierr = 0
        select case (lowercase(projection))
            case ('latlong')

                !> Regular lat/lon.
                if (.not. present(lats) .or. .not. present(lons)) then
                    call print_error( &
                        "Missing the 'lat' and 'lon' locations " // &
                        "required to write the '" // trim(projection) // "' projection to file.")
                    ierr = 1
                end if

                !> Define dimensions and add coordinates.
                if (ierr == 0) call nc4_define_dimension(iun, 'lat', size(lats), dy, ierr)
                if (ierr == 0) call nc4_add_variable(iun, 'lat', dy, lats, vid = vy, ierr = ierr)
                if (ierr == 0) call nc4_define_dimension(iun, 'lon', size(lons), dx, ierr)
                if (ierr == 0) call nc4_add_variable(iun, 'lon', dx, lons, vid = vx, ierr = ierr)

                !> Set coordinate units.
                if (ierr == 0) call nc4_set_coordinate_units(iun, projection, vlat = vy, vlon = vx, ierr = ierr)
            case ('rotated_latitude_longitude')

                !> netCDF format 'rotated_latitude_longitude' (not compatible with 'ROTLATLONG' for EnSim outputs).
                if (.not. present(rlats) .or. .not. present(rlons) .or. .not. present(xylats) .or. .not. present(xylons)) then
                    call print_error( &
                        "Missing the 'rlat' and 'rlon' locations and 'xylat' and 'xylon' reference variables " // &
                        "required to write the '" // trim(projection) // "' projection to file.")
                    ierr = 1
                end if

                !> Define dimensions and add coordinates.
                if (ierr == 0) call nc4_define_dimension(iun, 'rlat', size(rlats), dy, ierr)
                if (ierr == 0) call nc4_add_variable(iun, 'rlat', dy, rlats, vid = vry, ierr = ierr)
                if (ierr == 0) call nc4_define_dimension(iun, 'rlon', size(rlons), dx, ierr)
                if (ierr == 0) call nc4_add_variable(iun, 'rlon', dx, rlons, vid = vrx, ierr = ierr)
                if (ierr == 0) call nc4_add_variable(iun, 'lat', dx, dy, xylats, vid = vy, ierr = ierr)
                if (ierr == 0) call nc4_add_variable(iun, 'lon', dx, dy, xylons, vid = vx, ierr = ierr)

                !> Set coordinate units.
                if (ierr == 0) then
                    call nc4_set_coordinate_units(iun, projection, vrlat = vry, vrlon = vrx, vlat = vy, vlon = vx, ierr = ierr)
                end if
            case default

                !> Unknown.
                call print_error("Unknown or unsupported projection '" // trim(projection) // "'.")
                ierr = 1
        end select

        !> Assign output variables.
        if (present(dim1_id)) dim1_id = dy
        if (present(dim2_id)) dim2_id = dx

    end subroutine

    subroutine nc4_add_attribute_real(iun, vid, attribute_name, attribute_value, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        real, intent(in) :: attribute_value

        !> Input variables (optional).
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Set the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(attribute_name) // "' attribute (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_attribute_int(iun, vid, attribute_name, attribute_value, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        integer, intent(in) :: attribute_value

        !> Input variables (optional).
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Set the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(attribute_name) // "' attribute (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_attribute_char(iun, vid, attribute_name, attribute_value, ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: attribute_name
        character(len = *), intent(in) :: attribute_value

        !> Input variables (optional).
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Set the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(attribute_name) // "' attribute (Code: " // trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_set_variable_standard_attributes(iun, vid, standard_name, long_name, units, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Assign the attributes.
        ierr = NF90_NOERR
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'standard_name', standard_name)
        if (present(long_name)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'long_name', long_name)
        end if
        if (present(units)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'units', units)
        end if

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) code
            call print_error( &
                "Errors occurred saving the standard name for the '" // trim(standard_name) // "' variable (Code: " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_set_crs(iun, vid, standard_name, crs_coordinates, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name, crs_coordinates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Set CRS (reference).
        ierr = nf90_put_att(iun, vid, 'coordinates', trim(crs_coordinates))
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping', 'crs')

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred assigning the CRS to the '" // trim(standard_name) // "' variable (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_set_variable_attributes_real( &
        iun, vid, standard_name, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Assign attributes.
        if (present(fill)) then
            ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        else if (kind(1.0) == 8) then
            ierr = nf90_put_att(iun, vid, '_FillValue', NF90_FILL_DOUBLE)
        else
            ierr = nf90_put_att(iun, vid, '_FillValue', NF90_FILL_FLOAT)
        end if
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_attributes(iun, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if
        if (present(constmul)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
        end if
        if (present(constadd)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
        end if
        if (present(constrmin)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
        end if
        if (present(constrmax)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
        end if

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "Errors occurred saving attributes for the '" // trim(standard_name) // "' variable (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_set_variable_attributes_int( &
        iun, vid, standard_name, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Assign attributes.
        if (present(fill)) then
            ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        else
            ierr = nf90_put_att(iun, vid, '_FillValue', NF90_FILL_INT)
        end if
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_attributes(iun, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if
        if (present(constmul)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
        end if
        if (present(constadd)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
        end if
        if (present(constrmin)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
        end if
        if (present(constrmax)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
        end if

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "Errors occurred saving attributes for the '" // trim(standard_name) // "' variable (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_set_variable_attributes_char( &
        iun, vid, standard_name, &
        long_name, units, fill, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units
        character(len = 1), intent(in), optional :: fill

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Assign attributes.
        if (present(fill)) then
            ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        else
            ierr = nf90_put_att(iun, vid, '_FillValue', NF90_FILL_CHAR)
        end if
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_attributes(iun, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "Errors occurred saving attributes for the '" // trim(standard_name) // "' variable (Code " // &
                trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_define_variable( &
        iun, standard_name, dtype, &
        name_dim_char_length, &
        dim1_id, dim2_id, dim3_id, dim4_id, dim5_id, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dtype
        character(len = *), intent(in) :: standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim_char_length
        integer, intent(in), optional :: dim1_id, dim2_id, dim3_id, dim4_id, dim5_id

        !> Output variables.
        integer, intent(out) :: ierr

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dim_name, code
        integer did_c, d, v

        !> Create variable (based on type).
        select case (dtype)
            case (NF90_REAL, NF90_INT)

                !> Derive data type.
                if (dtype == NF90_REAL .and. kind(1.0) == 8) then
                    d = NF90_DOUBLE
                else
                    d = dtype
                end if

                !> Add the variable (for known numeric types).
                if (present(dim1_id) .and. present(dim2_id) .and. present(dim3_id) .and. present(dim4_id) .and. present(dim5_id)) &
                    then
                    ierr = nf90_def_var(iun, standard_name, d, (/dim1_id, dim2_id, dim3_id, dim4_id, dim5_id/), v)
                else if (present(dim1_id) .and. present(dim2_id) .and. present(dim3_id) .and. present(dim4_id)) then
                    ierr = nf90_def_var(iun, standard_name, d, (/dim1_id, dim2_id, dim3_id, dim4_id/), v)
                else if (present(dim1_id) .and. present(dim2_id) .and. present(dim3_id)) then
                    ierr = nf90_def_var(iun, standard_name, d, (/dim1_id, dim2_id, dim3_id/), v)
                else if (present(dim1_id) .and. present(dim2_id)) then
                    ierr = nf90_def_var(iun, standard_name, d, (/dim1_id, dim2_id/), v)
                else if (present(dim1_id)) then
                    ierr = nf90_def_var(iun, standard_name, d, (/dim1_id/), v)
                else
                    ierr = nf90_def_var(iun, standard_name, d, (/0/), v)
                end if
            case (NF90_CHAR)

                !> Prepare to add the variable for type 'char'.
                if (present(name_dim_char_length)) then

                    !> Copy the dimension name.
                    dim_name = trim(name_dim_char_length)
                else

                    !> Derive the dimension name.
                    write(code, FMT_GEN) DEFAULT_FIELD_LENGTH
                    dim_name = 'string' // trim(adjustl(code))

                    !> Check to see if the dimension already exists.
                    if (.not. nc4_inquire_dimension(iun, dim_name, did_c)) then
                        call nc4_define_dimension(iun, dim_name, DEFAULT_FIELD_LENGTH, did_c, ierr)
                        if (ierr /= 0) return
                    end if
                end if

                !> Add the variable (for type 'char').
                if (present(dim1_id)) then
                    ierr = nf90_def_var(iun, standard_name, dtype, (/dim1_id, did_c/), v)
                else
                    ierr = nf90_def_var(iun, standard_name, dtype, (/did_c/), v)
                end if
            case default

                !> Unknown type.
                write(code, FMT_GEN) dtype
                call print_error("Unknown data type (" // trim(adjustl(code)) // ") for '" // trim(standard_name) // "' variable.")
                ierr = 1
                return
        end select
        if (present(vid)) vid = v

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred adding the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_scalar_real( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_scalar_int( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_scalar_char( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        character(len = *), intent(in) :: dat

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_1d_real( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_1d_int( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_1d_char( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        character(len = *), intent(in) :: dat(:)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_2d_real( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_2d_int( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_3d_real( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_3d_int( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_4d_real( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_4d_int( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_5d_real( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_data_5d_int( &
        iun, standard_name, vid, dat, &
        start, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Write data.
        ierr = nf90_put_var(iun, vid, dat, start = start)

        !> Check for errors.
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred saving the '" // trim(standard_name) // "' variable (Code " // trim(adjustl(code)) // ").")
            ierr = 1
            return
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_add_variable_scalar_real( &
        iun, standard_name, dat, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_REAL, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_real( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, ierr = ierr)

    end subroutine

    subroutine nc4_add_variable_scalar_int( &
        iun, standard_name, dat, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_INT, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_int( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, ierr = ierr)

    end subroutine

    subroutine nc4_add_variable_scalar_char( &
        iun, standard_name, dat, &
        name_dim_char_length, long_name, units, fill, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: standard_name, dat

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim_char_length, long_name, units
        character(len = 1), intent(in), optional :: fill

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_CHAR, &
            name_dim_char_length, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_char( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, ierr = ierr)

    end subroutine

    subroutine nc4_add_variable_1d_real( &
        iun, standard_name, dim1_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_REAL, &
            dim1_id = dim1_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_real( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_1d_int( &
        iun, standard_name, dim1_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_INT, &
            dim1_id = dim1_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_int( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_1d_char( &
        iun, standard_name, dim1_id, dat, &
        start, &
        name_dim_char_length, &
        long_name, units, fill, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id
        character(len = *), intent(in) :: standard_name
        character(len = *), intent(in) :: dat(:)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: name_dim_char_length, long_name, units
        character(len = 1), intent(in), optional :: fill

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_CHAR, &
            name_dim_char_length, &
            dim1_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_char( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_2d_real( &
        iun, standard_name, dim1_id, dim2_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_REAL, &
            dim1_id = dim1_id, dim2_id = dim2_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_real( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_2d_int( &
        iun, standard_name, dim1_id, dim2_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_INT, &
            dim1_id = dim1_id, dim2_id = dim2_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_int( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_3d_real( &
        iun, standard_name, dim1_id, dim2_id, dim3_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id, dim3_id
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_REAL, &
            dim1_id = dim1_id, dim2_id = dim2_id, dim3_id = dim3_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_real( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_3d_int( &
        iun, standard_name, dim1_id, dim2_id, dim3_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id, dim3_id
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_INT, &
            dim1_id = dim1_id, dim2_id = dim2_id, dim3_id = dim3_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_int( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_4d_real( &
        iun, standard_name, dim1_id, dim2_id, dim3_id, dim4_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id, dim3_id, dim4_id
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_REAL, &
            dim1_id = dim1_id, dim2_id = dim2_id, dim3_id = dim3_id, dim4_id = dim4_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_real( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_4d_int( &
        iun, standard_name, dim1_id, dim2_id, dim3_id, dim4_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id, dim3_id, dim4_id
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_INT, &
            dim1_id = dim1_id, dim2_id = dim2_id, dim3_id = dim3_id, dim4_id = dim4_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_int( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_5d_real( &
        iun, standard_name, dim1_id, dim2_id, dim3_id, dim4_id, dim5_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id, dim3_id, dim4_id, dim5_id
        character(len = *), intent(in) :: standard_name
        real, intent(in) :: dat(:, :, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        real, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_REAL, &
            dim1_id = dim1_id, dim2_id = dim2_id, dim3_id = dim3_id, dim4_id = dim4_id, dim5_id = dim5_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_real( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_add_variable_5d_int( &
        iun, standard_name, dim1_id, dim2_id, dim3_id, dim4_id, dim5_id, dat, &
        start, &
        long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, &
        ierr)

        !> Input variables.
        integer, intent(in) :: iun, dim1_id, dim2_id, dim3_id, dim4_id, dim5_id
        character(len = *), intent(in) :: standard_name
        integer, intent(in) :: dat(:, :, :, :, :)

        !> Input variables (optional).
        integer, intent(in), optional :: start(:)
        character(len = *), intent(in), optional :: long_name, units
        integer, intent(in), optional :: fill, constmul, constadd, constrmax, constrmin

        !> Output variables (optional).
        integer, intent(out), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer :: v

        !> Define variable.
        call nc4_define_variable( &
            iun, standard_name, NF90_INT, &
            dim1_id = dim1_id, dim2_id = dim2_id, dim3_id = dim3_id, dim4_id = dim4_id, dim5_id = dim5_id, &
            vid = v, &
            ierr = ierr)
        if (ierr /= 0) return
        if (present(vid)) vid = v

        !> Set attributes.
        call nc4_set_variable_attributes_int( &
            iun, v, standard_name, &
            long_name = long_name, units = units, fill = fill, &
            constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
            ierr = ierr)
        if (ierr /= 0) return

        !> Write data.
        call nc4_add_data(iun, standard_name, v, dat, start, ierr)

    end subroutine

    subroutine nc4_close_file(iun, fpath, quiet, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fpath

        !> Input variables (optional).
        logical, intent(in), optional :: quiet

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        logical :: q = .true.

        !> Close the file.
        if (present(quiet)) q = quiet
        ierr = nf90_close(iun)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            if (q) then
                call print_error("An error occurred closing the file (Code: " // trim(adjustl(code)) // "): " // trim(fpath))
            else
                call print_error("Unable to close the file (Code: " // trim(adjustl(code)) // ").")
            end if
            ierr = 1
        else
            ierr = 0
        end if

    end subroutine

    subroutine nc4_define_output_variable_xy( &
        fpath, standard_name, projection, &
        lats, lons, rlats, rlons, xylats, xylons, &
        datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
        quiet, long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, dx, dy, &
        iun, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fpath, standard_name, projection

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units, datum, zone_id
        real, dimension(:), intent(in), optional :: lats, lons, rlats, rlons
        real, dimension(:, :), intent(in), optional :: xylats, xylons
        real, intent(in), optional :: &
            earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, fill, constmul, constadd, constrmax, constrmin
        logical, intent(in), optional :: quiet

        !> Output variables (optional).
        integer, intent(out), optional :: vid, dx, dy

        !> Output variables.
        integer, intent(out) :: iun, ierr

        !> Local variables.
        integer :: x = -1, y = -1, v = -1

        !> Open the file (for output).
        call nc4_open_output(fpath, quiet, iun, ierr)

        !> Add projection.
        if (ierr == 0) then
            call nc4_add_projection( &
                iun, projection, &
                datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
                ierr)
        end if

        !> Define dimensions and add coordinates.
        if (ierr == 0) then
            call nc4_add_coordinates( &
                iun, projection, &
                lats, lons, rlats, rlons, xylats, xylons, &
                datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
                dim1_id = y, dim2_id = x, &
                ierr = ierr)
        end if

        !> Define the data variable.
        if (ierr == 0) then
            call nc4_define_variable( &
                iun, standard_name, NF90_REAL, &
                dim1_id = x, dim2_id = y, &
                vid = v, &
                ierr = ierr)
        end if

        !> Set attributes.
        if (ierr == 0) then
            call nc4_set_variable_attributes_real( &
                iun, v, standard_name, &
                long_name = long_name, units = units, &
                fill = fill, constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
                ierr = ierr)
        end if

        !> Set CRS.
        if (ierr == 0) call nc4_set_crs(iun, v, standard_name, 'lon lat', ierr)

        !> Assign output variables.
        if (present(vid)) vid = v
        if (present(dx)) dx = x
        if (present(dy)) dy = y

    end subroutine

    subroutine nc4_define_output_variable_xyt( &
        fpath, standard_name, projection, ts_freq, &
        lats, lons, rlats, rlons, xylats, xylons, &
        datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
        quiet, long_name, units, fill, constmul, constadd, constrmax, constrmin, &
        vid, vtime, dx, dy, dtime, &
        iun, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fpath, standard_name, projection
        integer, intent(in) :: ts_freq

        !> Input variables (optional).
        character(len = *), intent(in), optional :: long_name, units, datum, zone_id
        real, dimension(:), intent(in), optional :: lats, lons, rlats, rlons
        real, dimension(:, :), intent(in), optional :: xylats, xylons
        real, intent(in), optional :: &
            earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, fill, constmul, constadd, constrmax, constrmin
        logical, intent(in), optional :: quiet

        !> Output variables (optional).
        integer, intent(out), optional :: vid, vtime, dx, dy, dtime

        !> Output variables.
        integer, intent(out) :: iun, ierr

        !> Local variables.
        integer :: x = -1, y = -1, t = -1, v = -1, vt = -1

        !> Open the file (for output).
        call nc4_open_output(fpath, quiet, iun, ierr)

        !> Add projection.
        if (ierr == 0) then
            call nc4_add_projection( &
                iun, projection, &
                datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
                ierr)
        end if

        !> Define dimensions and add coordinates.
        if (ierr == 0) then
            call nc4_add_coordinates( &
                iun, projection, &
                lats, lons, rlats, rlons, xylats, xylons, &
                datum, zone_id, earth_radius, grid_north_pole_latitude, grid_north_pole_longitude, &
                dim1_id = y, dim2_id = x, &
                ierr = ierr)
        end if

        !> Add time.
        if (ierr == 0) call nc4_add_time(iun, NF90_INT, ts_freq, dtime = t, vtime = vt, ierr = ierr)

        !> Define the data variable.
        if (ierr == 0) then
            call nc4_define_variable( &
                iun, standard_name, NF90_REAL, &
                dim1_id = x, dim2_id = y, dim3_id = t, &
                vid = v, &
                ierr = ierr)
        end if

        !> Set attributes.
        if (ierr == 0) then
            call nc4_set_variable_attributes_real( &
                iun, v, standard_name, &
                long_name = long_name, units = units, &
                fill = fill, constmul = constmul, constadd = constadd, constrmax = constrmax, constrmin = constrmin, &
                ierr = ierr)
        end if

        !> Set CRS.
        if (ierr == 0) call nc4_set_crs(iun, v, standard_name, 'lon lat', ierr)

        !> Assign output variables.
        if (present(vid)) vid = v
        if (present(vtime)) vtime = vt
        if (present(dx)) dx = x
        if (present(dy)) dy = y
        if (present(dtime)) dtime = t

    end subroutine

    subroutine nc4_add_data_xy(iun, standard_name, vid, xxx, yyy, size_x, size_y, fill, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid, xxx(:), yyy(:), size_x, size_y
        character(len = *), intent(in) :: standard_name
        real, dimension(:), intent(in) :: dat

        !> Input variables (optional).
        real, intent(in), optional :: fill

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z
        real, dimension(size_x, size_y) :: dat2
        real f

        !> Get attributes from the file.
        ierr = 0
        if (present(fill)) then
            f = fill
        else
            call nc4_get_fillvalue(iun, standard_name, vid, f, ierr)
        end if

        !> Transfer 'dat' to the 2D 'dat2' array.
        call field_to_mapped_values(dat, xxx, yyy, f, dat2, z)
        if (btest(z, pstat%COUNT_MISMATCH)) ierr = 1

        !> Write data.
        if (ierr == 0) call nc4_add_data(iun, standard_name, vid, dat2, ierr = ierr)

    end subroutine

    subroutine nc4_add_data_xyt(iun, standard_name, ts_freq, tid, vid, xxx, yyy, size_x, size_y, fill, dat, dates, ierr)

        !> Input variables.
        integer, intent(in) :: iun, ts_freq, tid, vid, xxx(:), yyy(:), size_x, size_y
        character(len = *), intent(in) :: standard_name
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Input variables (optional).
        real, intent(in), optional :: fill

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, z
        integer, dimension(size(dat, 2)) :: dat1
        real, dimension(size_x, size_y, size(dat, 2)) :: dat3
        real(kind = EightByteReal) t0_r8, t1_r8
        real f

        !> Get attributes from the file.
        ierr = 0
        if (present(fill)) then
            f = fill
        else
            call nc4_get_fillvalue(iun, standard_name, vid, f, ierr)
        end if

        !> Calculate the reference time.
        t0_r8 = nc4_time_from_date_components( &
            ic%start%year, ic%start%month, ic%start%day, ic%start%hour, ic%start%mins, ierr = ierr)
        select case (ts_freq)
            case (FREQ_MONTHLY, FREQ_SEASONAL, FREQ_DAILY)
                t0_r8 = t0_r8
            case (FREQ_YEARLY)
                t0_r8 = ic%start%year
            case (FREQ_HOURLY)
                t0_r8 = t0_r8*24.0
            case (FREQ_PTS)
                t0_r8 = t0_r8*24.0*60.0
            case default
                t0_r8 = t0_r8*24.0*60.0*60.0
        end select

        !> Loop for time.
        do t = 1, size(dat, 2)

            !> Transfer 'dat' to the 2D 'dat3' array.
            call field_to_mapped_values(dat(:, t), xxx, yyy, f, dat3(:, :, t), z)
            if (btest(z, pstat%COUNT_MISMATCH)) then
                ierr = 1
                exit
            end if

            !> Calculate the offset from the reference time and save to the 'dat1' array.
            !>  dates(2, t) -> year
            !>  dates(3, t) -> month
            !>  dates(4, t) -> day
            !>  dates(5, t) -> hour
            !>  dates(6, t) -> mins
            t1_r8 = nc4_time_from_date_components( &
                dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t), ierr = ierr)
            select case (ts_freq)
                case (FREQ_MONTHLY, FREQ_SEASONAL, FREQ_DAILY)
                    t1_r8 = t1_r8
                case (FREQ_YEARLY)
                    t1_r8 = dates(2, t)
                case (FREQ_HOURLY)
                    t1_r8 = t1_r8*24.0
                case (FREQ_PTS)
                    t1_r8 = t1_r8*24.0*60.0
                case default
                    t1_r8 = t1_r8*24.0*60.0*60.0
            end select
            dat1(t) = int(t1_r8 - t0_r8)
        end do

        !> Write time.
        if (ierr == 0) call nc4_add_data(iun, standard_name, tid, dat1, start = (/dates(1, 1)/), ierr = ierr)

        !> Write data.
        if (ierr == 0) call nc4_add_data(iun, standard_name, vid, dat3, start = (/1, 1, dates(1, 1)/), ierr = ierr)

    end subroutine
#endif

end module
