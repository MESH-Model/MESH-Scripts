module mesh_io

    !> 'mesh_io_variables': For I/O field types, options and constants.
    use mesh_io_variables

    !> 'print_routines': For print routines, format statements, and line lengths and limits.
    use print_routines

    !> 'field_utilities': For mapping functions for I/O fields.
    use field_utilities

    implicit none

    !> Local variables.
    !*  BASE_FILE_IUNIT: Active file unit.
    integer, private, save :: BASE_FILE_IUNIT = 11

    private check_projection, check_dimension

    interface assign_cell_values
        module procedure assign_cell_values_real
        module procedure assign_cell_values_int
    end interface

    interface assign_cell_values_pntr
        module procedure assign_cell_values_real_pntr
        module procedure assign_cell_values_int_pntr
    end interface

    interface assign_tile_values
        module procedure assign_tile_values_real
        module procedure assign_tile_values_int
    end interface

    interface assign_tile_values_pntr
        module procedure assign_tile_values_real_pntr
        module procedure assign_tile_values_int_pntr
    end interface

    contains

    subroutine check_projection(reference_projection, file_projection, quiet, error_status)

        !> Input/output variables.
        character(len = *), intent(in) :: reference_projection, file_projection
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        integer ierr
        logical v

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Check projection (from file to reference).
        ierr = 0
        if (lowercase(file_projection) /= lowercase(reference_projection)) then
            if (v) call print_error( &
                "The projection of the file '" // trim(file_projection) // "' does not match the '" // &
                trim(reference_projection) // "' projection for the active domain.")
            ierr = 1
        else
            if (v) call print_info("The projection of the file is '" // trim(file_projection) // "'.")
        end if

        !> Return status.
        error_status = ierr

    end subroutine

    subroutine check_dimension( &
        file_label, reference_label, file_values, reference_values, file_units, reference_units, null_value, quiet, error_status)

        !> Input/output variables.
        character(len = *), intent(in) :: file_label
        character(len = *), intent(in) :: reference_label
        real, dimension(:), intent(in) :: file_values
        real, dimension(:), intent(in) :: reference_values
        character(len = *), intent(in), optional :: file_units
        character(len = *), dimension(:), intent(in), optional :: reference_units
        real, intent(in), optional :: null_value
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = SHORT_FIELD_LENGTH) code1, code2
        integer i, ierr
        logical v, ltest

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Check units (if present).
        ierr = 0
        if (present(file_units) .and. present(reference_units)) then
            ltest = .false.
            i = 1
            do while (.not. ltest .and. i <= size(reference_units))
                ltest = (lowercase(file_units) == lowercase(reference_units(i)))
                i = i + 1
            end do
            if (.not. ltest) then
                if (v) call print_warning( &
                    "The " // trim(reference_label) // " reference '" // trim(file_label) // &
                    "' contains unrecognized or unsupported units '" // trim(file_units) // "'.")
            end if
        end if

        !> Check dimension size.
        ltest = (size(file_values) == size(reference_values))
        if (.not. ltest) then
            if (v) then
                write(code1, *) size(file_values)
                write(code2, *) size(reference_values)
                call print_error( &
                    "The number of values does not match between the file (" // trim(adjustl(code1)) // ") and active domain (" // &
                    trim(adjustl(code2)) // ") for the " // trim(reference_label) // " reference.")
            end if
            ierr = 1
        end if

        !> Check values.
        if (ierr == 0) then

            !> Compare values.
            ltest = .true.
            do i = 1, size(reference_values)
                ltest = (ltest .and. (reference_values(i) == file_values(i)))
            end do
            if (.not. ltest) then
                if (v) call print_error( &
                    "The values do not match between the file and active domain for the  " // trim(reference_label) // &
                    " reference.")
                ierr = 1
            end if

            !> Check for missing values.
            if (present(null_value)) then
                if (any(file_values == null_value)) then
                    if (v) call print_error("The " // trim(reference_label) // " reference contains missing values.")
                    ierr = 1
                end if
            end if
        end if

        !> Return status.
        error_status = ierr

    end subroutine

    subroutine validate_input_file_spatial_reference(input_file, quiet, error_status)

        !> Variables.
        use projection_variables, only: pj

        !> I/O modules.
        use ensim_io
#ifdef NETCDF
        use nc_io
#endif

        !> Input/output variables.
        !*  input_file: Input file object ('io_file').
        !*  quiet: .true. to suppress block formatting (optional, default: .false.).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_file), intent(in) :: input_file
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        integer ierr
        logical v

        !> File utility variables.
        type(ensim_keyword), dimension(:), allocatable :: vkeyword
        integer nkeyword
#ifdef NETCDF
        character(len = SHORT_FIELD_LENGTH) :: projection, units, field
        real, allocatable :: dat_r(:)
        real fill_r
        integer, allocatable :: dimids(:)
        integer i, dim_length, ndims, nvars
#endif

        !> Error status.
        error_status = 0

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Validate the spatial reference of the file (compared to 'pj').
        ierr = 0
        select type (input_file)

            !> r2c ASCII format.
            class is (io_file_r2c)

                !> Rewind the file.
                rewind(input_file%iunit)

                !> Read the header in the file.
                if (v) call print_message("Reading header information.")
                call parse_header_ensim(input_file%iunit, vkeyword, nkeyword, ierr)
                if (ierr /= 0) then

                    !> Check the spatial reference matches 'pj'.
                    if (v) call print_message("Checking the spatial reference of the file.")
                    call validate_header_spatial( &
                        vkeyword, nkeyword, &
                        pj%projection, size(pj%lon), pj%dx, pj%llc_x, size(pj%lat), pj%dy, pj%llc_y, &
                        ierr)
                    error_status = 1
                end if
#ifdef NETCDF

            !> NetCDF format.
            class is (io_file_nc)

                !> Read the projection from the file.
                if (v) call print_message("Checking the spatial reference of the file.")
                call nc4_get_projection(input_file%iunit, projection, ierr = ierr)
                if (ierr == 0) then

                    !> Check if the projection matches 'pj'.
                    call check_projection(projection, pj%projection, quiet = .not. v, error_status = ierr)

                    !> Get the number of variables in the file.
                    call nc4_inquire_file(input_file%iunit, nvars = nvars, ierr = ierr)
                    if (ierr == 0) then

                        !> Scan the variables for latitudes/longitudes.
                        do i = 1, nvars

                            !> Get the name of the variable.
                            call nc4_get_variable_name(input_file%iunit, i, field, ierr)
                            if (ierr /= 0) cycle

                            !> Get information about the dimensions.
                            call nc4_get_variable_attributes( &
                                input_file%iunit, field, units = units, ndims = ndims, dimids = dimids, ierr = ierr)

                            !> Cycle if a multi-dimension field.
                            if (ndims /= 1) cycle

                            !> Get the dimension length.
                            call nc4_get_dimension_name(input_file%iunit, dimids(1), dim_length = dim_length, ierr = ierr)
                            if (ierr /= 0) then
                                cycle
                            else

                                !> Allocate the temporary data array.
                                allocate(dat_r(dim_length))
                            end if

                            !> Check values.
                            select case (lowercase(field))
                                case ('lat', 'latitude')
                                    call nc4_get_data(input_file%iunit, field, vid = i, dat = dat_r, ierr = ierr)
                                    if (ierr == 0) then
                                        call check_dimension( &
                                            field, 'latitudinal', dat_r, pj%lat, units, &
                                            (/DIM_UNITS_DEGREES, DIM_UNITS_DECIMAL_DEGREES, DIM_UNITS_DEGREES_NORTH/), fill_r, &
                                            quiet = .not. v, error_status = ierr)
                                        if (ierr /= 0) error_status = 1
                                    end if
                                case ('rlat')
                                    call nc4_get_data(input_file%iunit, field, vid = i, dat = dat_r, ierr = ierr)
                                    if (ierr == 0) then
                                        call check_dimension( &
                                            field, 'rotated-latitudinal', dat_r, pj%lat, units, &
                                            (/DIM_UNITS_DEGREES, DIM_UNITS_DECIMAL_DEGREES, DIM_UNITS_DEGREES_NORTH/), fill_r, &
                                            quiet = .not. v, error_status = ierr)
                                        if (ierr /= 0) error_status = 1
                                    end if
                                case ('lon', 'longitude')
                                    call nc4_get_data(input_file%iunit, field, vid = i, dat = dat_r, ierr = ierr)
                                    if (ierr == 0) then
                                        call check_dimension( &
                                            field, 'longitudinal', dat_r, pj%lon, units, &
                                            (/DIM_UNITS_DEGREES, DIM_UNITS_DECIMAL_DEGREES, DIM_UNITS_DEGREES_EAST/), fill_r, &
                                            quiet = .not. v, error_status = ierr)
                                        if (ierr /= 0) error_status = 1
                                    end if
                                case ('rlon')
                                    call nc4_get_data(input_file%iunit, field, vid = i, dat = dat_r, ierr = ierr)
                                    if (ierr == 0) then
                                        call check_dimension( &
                                            field, 'rotated-longitudinal', dat_r, pj%lon, units, &
                                            (/DIM_UNITS_DEGREES, DIM_UNITS_DECIMAL_DEGREES, DIM_UNITS_DEGREES_EAST/), fill_r, &
                                            quiet = .not. v, error_status = ierr)
                                        if (ierr /= 0) error_status = 1
                                    end if
                            end select

                            !> Clean-up.
                            if (allocated(dat_r)) deallocate(dat_r)
                        end do
                    end if
                end if
#endif
        end select

    end subroutine

    subroutine open_input_file(input_file, quiet, error_status)

        !> I/O modules.
        use ensim_io
        use txt_io
#ifdef NETCDF
        use nc_io
#endif

        !> Input/output variables.
        !*  input_file: Input file object ('io_file').
        !*  quiet: .true. to suppress block formatting (optional, default: .false.).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_file) input_file
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = SHORT_FIELD_LENGTH) code
        integer iun, ierr
        logical v

        !> Error status.
        error_status = 0

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Check for a valid path.
        if (len_trim(input_file%full_path) == 0) then
            if (v) call print_error("Missing file name in 'open_input_file'. Unable to open file.")
            error_status = 1
            return
        else

            !> Print a message (if not quiet).
            if (v) then
                call reset_tab()
                call print_message("READING: " // trim(input_file%full_path))
                call increase_tab()
            end if
        end if

        !> Open the file with read access.
        ierr = 0
        select type (input_file)

            !> r2c ASCII format.
            class is (io_file_r2c)
                call open_ensim_input(BASE_FILE_IUNIT, input_file%full_path, ierr)
                if (ierr == 0) then
                    input_file%iunit = BASE_FILE_IUNIT
                    BASE_FILE_IUNIT = BASE_FILE_IUNIT + 1
                end if

            !> Simple text formats (txt, asc, CSV).
            class is (io_file_txt_delimited)
                open(BASE_FILE_IUNIT, file = input_file%full_path, action = 'read', status = 'old', iostat = ierr)
                if (ierr == 0) then
                    input_file%iunit = BASE_FILE_IUNIT
                    BASE_FILE_IUNIT = BASE_FILE_IUNIT + 1
                end if

            !> Binary sequential format (seq).
            class is (io_file_seq)
                open( &
                    BASE_FILE_IUNIT, file = input_file%full_path, action = 'read', status = 'old', form = 'unformatted', &
                    access = 'sequential', iostat = ierr)
                if (ierr == 0) then
                    input_file%iunit = BASE_FILE_IUNIT
                    BASE_FILE_IUNIT = BASE_FILE_IUNIT + 1
                end if

            !> NetCDF format.
            class is (io_file_nc)
#ifdef NETCDF
                call nc4_open_input(input_file%full_path, .true., iun, ierr)
                if (ierr == 0) then
                    input_file%iunit = iun
                end if
#else
                if (.not. v) call print_error("Unable to open file '" // trim(input_file%full_path) // "'.")
                call print_error( &
                    "A file file in NetCDF format is specified for '" // trim(input_file%full_path) // &
                    "' but the NetCDF module is not active. " // &
                    "A version of MESH compiled with the NetCDF library must be used to read files in input_file format.")
                ierr = 1
#endif

            !> CLASS MET format.
            class is (io_file_met)
                open(BASE_FILE_IUNIT, file = input_file%full_path, action = 'read', status = 'old', iostat = ierr)
                if (ierr == 0) then
                    input_file%iunit = BASE_FILE_IUNIT
                    BASE_FILE_IUNIT = BASE_FILE_IUNIT + 1
                end if

            !> Unknown file format.
            class default
                if (v) call print_error("Unknown or unsupported file format.")
                ierr = 1
        end select

        !> Check for I/O error.
        if (ierr /= 0) then
            write(code, *) ierr
            if (v) call print_error("An error occurred opening the file (Code: " // trim(adjustl(code)) // ").")
            error_status = ierr
        end if

    end subroutine

    subroutine read_file_fields_to_buffer(input_file, quiet, error_status)

        !> Parsing utilities.
        use strings, only: uppercase, lowercase
        use date_utilities, only: jday_to_date

        !> Variables.
        use variable_names

        !> I/O modules.
        use ensim_io
        use txt_io
#ifdef NETCDF
        use nc_io
#endif

        !> Input/output variables.
        !*  input_file: Input file object ('io_file').
        !*  quiet: .true. to suppress block formatting (optional, default: .false.).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_file) input_file
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field_wrapper), dimension(:), allocatable :: file_buffer
        character(len = SHORT_FIELD_LENGTH) :: code
        integer n, j, i, natts, nvars, nplus, ierr
        logical v

        !> File utility variables.
        type(ensim_keyword), dimension(:), allocatable :: vkeyword
        type(ensim_attr), dimension(:), allocatable :: vattr
        real, allocatable :: dat3_r(:, :, :)
        integer, allocatable :: dat1_i(:)
        integer x, y, nattrs, class_count
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_names, args
        character(len = DEFAULT_LINE_LENGTH) line_buffer
#ifdef NETCDF
        character(len = SHORT_FIELD_LENGTH) :: description, units, aname, vname
        character(len = :), allocatable :: dat1_c(:), dat_c
        character fill_c
        real(kind = kind(0.0d0)), allocatable :: dat1_d(:)
        real, allocatable :: dat5_r(:, :, :, :, :), dat4_r(:, :, :, :), dat2_r(:, :), dat1_r(:)
        real dat_r, fill_r
        integer, dimension(:), allocatable :: dimids, dim_lengths, mapped_dim_order
        integer, allocatable :: dat2_i(:, :), dat3_i(:, :, :), dat4_i(:, :, :, :), dat5_i(:, :, :, :, :)
        integer dtype, time_order, atype, alength, ndims, dat_i, fill_i
#endif

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Counters used in saving variables and for the summary.
        natts = -1
        nvars = -1

        !> Counter adjustment for special cases.
        nplus = 0

        !> Read data in the file.
        if (allocated(input_file%fields)) then
            n = size(input_file%fields) + 1
        else
            n = 1
        end if
        ierr = 0
        select type (input_file)

            !> r2c ASCII format.
            type is (io_file_r2c)

                !> Rewind the file.
                rewind(input_file%iunit)

                !> Read the header.
                call parse_header_ensim(input_file%iunit, vkeyword, nattrs, ierr)

                !> Get a list of attributes and variables in the file.
                if (ierr == 0) call parse_header_attribute_ensim(input_file%iunit, vkeyword, nattrs, vattr, nvars, ierr)

                !> Determine type of file.
                input_file%multi_frame = is_multi_frame(input_file%iunit)

                !> Read and save the attributes and variables to the list of fields.
                if (ierr == 0) then

                    !> Allocate the working list of fields.
                    allocate(file_buffer(nvars + nattrs))

                    !> Loop through the attributes.
                    class_count = 0
                    do i = 1, nattrs

                        !> Skip 'attributename', 'attributetype' and 'attributeunits'.
                        select case (lowercase(vkeyword(i)%keyword(2:)))
                            case ('attributename', 'attributetype', 'attributeunits')
                            case default

                                !> Save the field (omit the leading ':' from the name).
                                line_buffer = ''
                                do j = 1, size(vkeyword(i)%words)
                                    line_buffer = trim(line_buffer) // trim(vkeyword(i)%words(j))
                                end do
                                allocate(file_buffer(n)%field, source = io_field_char( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    label = trim(vkeyword(i)%keyword(2:)), dat = trim(line_buffer), id = i))
                                n = n + 1

                                !> Special cases.
                                if (lowercase(vkeyword(i)%keyword(2:)) == 'classcount' .and. size(vkeyword(i)%words) > 0) then

                                    !> Number of GRUs (land cover).
                                    read(vkeyword(i)%words(1), *, iostat = ierr) class_count
                                    if (ierr /= 0) class_count = 0
                                else if (lowercase(vkeyword(i)%keyword(2:)) == 'xcount' .and. size(vkeyword(i)%words) > 0) then

                                    !> Number of cells in the 'x' dimension.
                                    read(vkeyword(i)%words(1), *, iostat = ierr) x
                                    if (ierr /= 0) x = 0
                                else if (lowercase(vkeyword(i)%keyword(2:)) == 'ycount' .and. size(vkeyword(i)%words) > 0) then

                                    !> Number of cells in the 'y' dimension.
                                    read(vkeyword(i)%words(1), *, iostat = ierr) y
                                    if (ierr /= 0) y = 0
                                end if
                        end select
                    end do
                    if (nvars == 0) then
                        call print_error("No variables were found in the file.")
                        error_status = 1
                        return
                    else if (x == 0 .or. y == 0) then
                        call print_error("The 'x' and 'y' dimensions ('xCount' and 'yCount') were not found in the file.")
                        error_status = 1
                        return
                    end if

                    !> Advance past the end of the header.
                    if (ierr == 0) call advance_past_header(input_file%iunit, input_file%full_path, ierr)

                    !> Load data.
                    if (.not. input_file%multi_frame) then

                        !> Read and parse the variable data.
                        if (ierr == 0) call load_data_r2c(input_file%iunit, input_file%full_path, vattr, nvars, x, y, .false., ierr)

                        !> Set the dimension names.
                        allocate(dim_names(2))
                        dim_names = (/DIM_NAME_X, DIM_NAME_Y/)

                        !> Special cases.
                        if (class_count > 0) then

                            !> GRUs.
                            do i = (nvars - class_count + 1), nvars

                                !> Register the field (assuming 'attributetype' from known variable).
                                write(code, *) (i - (nvars - class_count))
                                allocate(file_buffer(n)%field, source = io_field_real2d( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    cell_map = null(), tile_map = null(), &
                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                    label = trim(DIM_NAME_GRU) // ' ' // trim(adjustl(code)), short_name = trim(vattr(i)%attr), &
                                    units = trim(vattr(i)%units), dim_names = dim_names, dat = vattr(i)%val, id = i))
                                n = n + 1
                            end do
                        end if

                        !> Loop through the variables.
                        do i = 1, (nvars - class_count)

                            !> Save the field.
                            select case (vattr(i)%type)
                                case ('integer')
                                    allocate(file_buffer(n)%field, source = io_field_int2d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        dim_names = dim_names, dat = int(vattr(i)%val)))
                                case ('float', '')
                                    allocate(file_buffer(n)%field, source = io_field_real2d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                        dim_names = dim_names, dat = vattr(i)%val))
                                case default
                                    if (v) call print_warning( &
                                        "The variable '" // trim(vattr(i)%attr) // "' is an unknown or unsupported data " // &
                                        "format '" // trim(vattr(i)%type) // "'.")
                            end select
                            if (allocated(file_buffer(n)%field)) then
                                file_buffer(n)%field%label = trim(vattr(i)%attr)
                                file_buffer(n)%field%id = i
                                file_buffer(n)%field%short_name = trim(vattr(i)%attr)
                                file_buffer(n)%field%units = trim(vattr(i)%units)
                                n = n + 1
                            end if
                        end do

                        !> Invalidate the current position in the file.
                        input_file%ipos = -1
                    else if (nvars /= 1) then

                        !> Print an error if the multi-frame file does not contain one variable.
                        write(code, *) nvars
                        call print_error( &
                            "The multi-frame file contains " // trim(adjustl(code)) // " variables when it should contain one.")
                        error_status = 1
                        return
                    else

                        !> Set the dimension names.
                        allocate(dim_names(3))
                        dim_names = (/DIM_NAME_X, DIM_NAME_Y, DIM_NAME_T/)

                        !> Save meta-information and register the field.
                        i = 1
                        select case (vattr(i)%type)
                            case ('float', '')
                                allocate(dat3_r(x, y, input_file%block_interval))
                                dat3_r = huge(dat3_r)
                                allocate(file_buffer(n)%field, source = io_field_real3d( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    cell_map = null(), tile_map = null(), &
                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                    dim_names = dim_names, dat = dat3_r, time_order = 3))
                                deallocate(dat3_r)
                            case default
                                if (v) call print_warning( &
                                    "The variable '" // trim(vattr(i)%attr) // "' is an unsupported data format '" // &
                                    trim(vattr(i)%type) // "' for multi-frame files.")
                        end select
                        if (allocated(file_buffer(n)%field)) then
                            if (allocated(input_file%field_map)) then

                                !> Special condition: Override the source name in the field map with the one read from file.
                                input_file%field_map(1, 1) = trim(vattr(i)%attr)
                            end if
                            file_buffer(n)%field%label = trim(vattr(i)%attr)
                            file_buffer(n)%field%id = i
                            file_buffer(n)%field%short_name = trim(vattr(i)%attr)
                            file_buffer(n)%field%units = trim(vattr(i)%units)
                            n = n + 1
                        end if

                        !> Set the current position in the file.
                        input_file%ipos = 1
                    end if

                    !> Adjust the number of attributes in the file.
                    natts = nattrs - nvars
                end if

            !> Simple text formats (txt, asc, CSV).
            type is (io_file_txt_delimited)

                !> Rewind the file.
                rewind(input_file%iunit)

                !> Skip leading lines.
                do i = 1, input_file%n_skip_rows
                    call read_txt_line(input_file%iunit, ierr = ierr)
                    if (ierr /= 0) then
                        error_status = 1
                        return
                    end if
                end do

                !> Check if a multi-frame file.
                if (input_file%multi_frame) then

                    !> Set the number of active variables based on existing field definitions in the file.
                    if (allocated(input_file%fields)) n = size(input_file%fields)

                    !> Print an error if the file contains more than one field.
                    if (n /= 1) then
                        write(code, *) n
                        call print_error( &
                            "A time-series in this file format must contain one field while the file contains (" // &
                            trim(adjustl(code)) // ") fields.")
                        error_status = 1
                        return
                    else
                        n = n + 1
                    end if

                    !> Set the current position in the file and return.
                    input_file%ipos = 1
                else

                    !> Count the number of lines in the file.
                    call count_txt_lines(input_file%iunit, nvars, ierr)
                    if (ierr /= 0) then
                        error_status = 1
                        return
                    end if

                    !> Allocate the working list of fields.
                    allocate(file_buffer(nvars))

                    !> Rewind the file.
                    rewind(input_file%iunit)

                    !> Skip leading lines.
                    do i = 1, input_file%n_skip_rows
                        call read_txt_line(input_file%iunit, ierr = ierr)
                        if (ierr /= 0) cycle
                    end do

                    !> Loop through the records.
                    do i = 1, nvars

                        !> Read the line.
                        call read_txt_line(input_file%iunit, args, input_file%delimiter, ierr = ierr)
                        if (ierr /= 0) cycle

                        !> Cycle if no argments exist.
                        if (size(args) < 1) cycle

                        !> Cycle if the first argument does not start with a string character.
                        if (.not. is_letter(args(1)(1:1))) cycle

                        !> Allocate 'dim_names'.
                        allocate(dim_names(1), source = (/input_file%dim_names/))

                        !> Special cases.
                        select case (lowercase(args(1)))

                            !> GRUs.
                            case (DIM_NAME_GRU)
                                allocate(file_buffer(n)%field, source = io_field_char1d( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    cell_map = null(), tile_map = null(), &
                                    label = trim(DIM_NAME_GRU) // ' ' // trim(args(2)), dim_names = dim_names, &
                                    dat = args(3:), id = i))
                                n = n + 1
                                cycle
                        end select

                        !> Save the field.
                        if (size(args) > 2) then
                            allocate(file_buffer(n)%field, source = io_field_char1d( &
                                mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                cell_map = null(), tile_map = null(), &
                                label = trim(args(1)), dim_names = dim_names, dat = args(2:), id = i))
                        else
                            allocate(file_buffer(n)%field, source = io_field_char( &
                                mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                label = trim(args(1)), dat = trim(args(2)), id = i))
                        end if
                        n = n + 1
                    end do
                end if

            !> Binary sequential format (seq).
            class is (io_file_seq)

                !> Check if a multi-frame file.
                if (input_file%multi_frame) then

                    !> Set the number of active variables based on existing field definitions in the file.
                    if (allocated(input_file%fields)) n = size(input_file%fields)

                    !> Print an error if the file contains more than one field.
                    if (n /= 1) then
                        write(code, *) n
                        call print_error( &
                            "A time-series in this file format must contain one field while the file contains (" // &
                            trim(adjustl(code)) // ") fields.")
                        error_status = 1
                        return
                    else
                        n = n + 1
                    end if

                    !> Set the current position in the file and return.
                    input_file%ipos = 1
                end if
#ifdef NETCDF

            !> NetCDF format.
            type is (io_file_nc)

                !> Get the number of attribute and variables.
                call nc4_inquire_file(input_file%iunit, natts = natts, nvars = nvars, ierr = ierr)
                if (ierr /= 0) then
                    error_status = 1
                    return
                end if

                !> Allocate the working list of fields.
                allocate(file_buffer(natts + nvars))

                !> Loop through each attribute.
                do i = 1, natts

                    !> Get the name of the attribute.
                    ierr = 0
                    call nc4_get_attribute_name(input_file%iunit, i, attribute_name = aname, ierr = ierr)
                    if (ierr /= 0) cycle

                    !> Get the data type of the attribute.
                    call nc4_get_attribute_type( &
                        input_file%iunit, attribute_name = aname, dtype = atype, length = alength, ierr = ierr)
                    if (ierr /= 0) cycle

                    !> Read and save the attribute.
                    select case (atype)
                        case (NF90_INT64, NF90_BYTE, NF90_SHORT, NF90_INT)
                            call nc4_get_attribute(input_file%iunit, aname, dat_i, ierr = ierr)
                            if (ierr == 0) then
                                allocate(file_buffer(n)%field, source = io_field_int( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    dat = dat_i))
                            end if
                        case (NF90_FLOAT, NF90_DOUBLE)
                            call nc4_get_attribute(input_file%iunit, aname, dat_r, ierr = ierr)
                            if (ierr == 0) then
                                allocate(file_buffer(n)%field, source = io_field_real( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                    dat = dat_r))
                            end if
                        case (NF90_CHAR)
                            allocate(character(len = alength) :: dat_c)
                            call nc4_get_attribute(input_file%iunit, aname, dat_c, ierr = ierr)
                            if (ierr == 0) then
                                allocate(file_buffer(n)%field, source = io_field_char( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    dat = trim(dat_c)))
                            end if
                            deallocate(dat_c)
                    end select
                    if (allocated(file_buffer(n)%field)) then
                        file_buffer(n)%field%label = trim(aname)
                        file_buffer(n)%field%id = i
                        n = n + 1
                    end if
                end do

                !> Loop through each variable.
                do i = 1, nvars

                    !> Get the name of the variable.
                    ierr = 0
                    call nc4_get_variable_name(input_file%iunit, i, vname, ierr)
                    if (ierr /= 0) cycle

                    !> Get attributes of the variable.
                    call nc4_get_variable_attributes( &
                        input_file%iunit, vname, dtype = dtype, ndims = ndims, dimids = dimids, natts = nattrs, ierr = ierr)
                    if (ierr /= 0) cycle

                    !> Get dimension information.
                    if (allocated(dim_names)) deallocate(dim_names)
                    if (allocated(dim_lengths)) deallocate(dim_lengths)
                    time_order = 0
                    if (ndims > 5) then
                        if (v) then
                            write(code, *) ndims
                            call print_warning( &
                                "The variable '" // trim(vname) // "' contains an unsupported number of dimensions (" // &
                                trim(adjustl(code)) // ").")
                        end if
                        cycle
                    else if (ndims > 0) then
                        allocate(dim_names(ndims), dim_lengths(ndims))
                        ierr = 0
                        do j = 1, ndims
                            call nc4_get_dimension_name( &
                                input_file%iunit, dimids(j), dim_name = dim_names(j), dim_length = dim_lengths(j), ierr = ierr)
                            if (ierr /= 0) then
                                dim_names(j) = ''
                            else

                                !> Check for time dimension.
                                select case (dim_names(j))
                                    case (DIM_NAME_TIME, DIM_NAME_T)
                                        time_order = j
                                end select
                            end if
                        end do

                        !> Overwrite the size of the 'time' dimension.
                        if (time_order > 0) dim_lengths(time_order) = input_file%block_interval
                    end if

                    !> Check for standard attributes.
                    fill_r = huge(0.0)
                    fill_i = huge(0)
                    fill_c = achar(0)
                    do j = 1, nattrs

                        !> Get the attribute name.
                        ierr = 0
                        call nc4_get_attribute_name(input_file%iunit, aid = j, vid = i, attribute_name = aname, ierr = ierr)
                        if (ierr == 0) then

                            !> Assign the attribute value (assuming 'dtype' by known attribute names).
                            select case (lowercase(aname))
                                case ('long_name')
                                    call nc4_get_attribute(input_file%iunit, aname, description, vid = i, ierr = ierr)
                                    if (ierr /= 0) description = ''
                                case ('units')
                                    call nc4_get_attribute(input_file%iunit, aname, units, vid = i, ierr = ierr)
                                    if (ierr /= 0) units = ''
                                case ('_fillvalue')
                                    call nc4_get_attribute_type(input_file%iunit, i, aname, dtype = atype, ierr = ierr)
                                    if (ierr == 0) then
                                        select case (atype)
                                            case (NF90_INT64, NF90_BYTE, NF90_SHORT, NF90_INT)
                                                call nc4_get_attribute(input_file%iunit, aname, fill_i, vid = i, ierr = ierr)
                                            case (NF90_FLOAT, NF90_DOUBLE)
                                                call nc4_get_attribute(input_file%iunit, aname, fill_r, vid = i, ierr = ierr)
                                            case (NF90_CHAR)
                                                call nc4_get_attribute(input_file%iunit, aname, fill_c, vid = i, ierr = ierr)
                                        end select
                                    end if
                            end select
                        end if
                    end do

                    !> Special cases.
                    select case (lowercase(vname))

                        !> CRS information (projection).
                        case (DIM_NAME_CRS)

                            !> Expand variable list.
                            call expand_field_list(file_buffer, nattrs, ierr)

                            !> Loop through the attributes.
                            do j = 1, nattrs

                                !> Get the attribute name.
                                ierr = 0
                                call nc4_get_attribute_name(input_file%iunit, aid = j, vid = i, attribute_name = aname, ierr = ierr)
                                if (ierr /= 0) cycle

                                !> Read and assign the attributes as new fields (assume 'dtype' from known variable).
                                select case (lowercase(aname))
                                    case ('grid_mapping_name')
                                        call nc4_get_attribute_type(input_file%iunit, i, aname, length = alength, ierr = ierr)
                                        if (ierr == 0) then
                                            allocate(character(len = alength) :: dat_c)
                                            call nc4_get_attribute(input_file%iunit, aname, dat_c, vid = i, ierr = ierr)
                                            if (ierr == 0) then
                                                allocate(file_buffer(n)%field, source = io_field_char( &
                                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                    dat = trim(dat_c)))
                                            end if
                                            deallocate(dat_c)
                                        end if
                                    case ('semi_major_axis', 'inverse_flattening', 'earth_radius', &
                                        'grid_north_pole_latitude', 'grid_north_pole_longitude')
                                        call nc4_get_attribute(input_file%iunit, aname, dat_r, vid = i, ierr = ierr)
                                        if (ierr == 0) then
                                            allocate(file_buffer(n)%field, source = io_field_real( &
                                                mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                                dat = dat_r))
                                        end if
                                end select
                                if (allocated(file_buffer(n)%field)) then
                                    file_buffer(n)%field%label = trim(aname)
                                    file_buffer(n)%field%id = i
                                    n = n + 1
                                    nplus = nplus + 1
                                end if
                            end do

                            !> Cycle to skip assigning the 'crs' variable itself.
                            cycle

                        !> GRUs.
                        case (DIM_NAME_GRU, DIM_NANE_LANDCOVER)

                            !> Get dimensions (assuming 'ndims' from known variable).
                            call get_dimension_order(dim_names, (/DIM_NAME_M/), mapped_dim_order, ierr)

                            !> Get dimension length.
                            call nc4_get_dimension( &
                                input_file%iunit, dim_names(mapped_dim_order(1)), dim_length = class_count, ierr = ierr)
                            if (ierr /= 0) cycle

                            !> Add the GRUs.
                            if (class_count > 0) then

                                !> Expand variable list.
                                call expand_field_list(file_buffer, class_count + 1, ierr)

                                !> Add a field for the dimension.
                                allocate(file_buffer(n)%field, source = io_field_int( &
                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                    label = trim(DIM_NAME_NGRU), dat = class_count, id = i))
                                n = n + 1

                                !> Read the variable from file (assuming possible shapes from known variable).
                                ierr = 0
                                select case (size(dim_names))
                                    case (2)
                                        allocate(dat2_r(dim_lengths(1), dim_lengths(2)))
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat2_r, ierr = ierr)
                                    case (3)
                                        allocate(dat3_r(dim_lengths(1), dim_lengths(2), dim_lengths(3)))
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat3_r, ierr = ierr)
                                    case default
                                        call print_warning( &
                                            "Unknown format of GRU (land cover) inputs from '" // trim(vname) // "'.")
                                        cycle
                                end select
                                if (ierr /= 0) then
                                    call print_warning("An error occurred reading the '" // trim(vname) // "' variable from file.")
                                    cycle
                                end if

                                !> Assign each GRU as a new field (assuming 'dtype' from known variable).
                                do j = 1, class_count

                                    !> Read the variable from file (assuming possible shapes from known variable).
                                    if (allocated(dat2_r)) then
                                        select case (mapped_dim_order(1))
                                            case (2)
                                                dat1_r = (/dat2_r(:, j)/)
                                                allocate(file_buffer(n)%field, source = io_field_real1d( &
                                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                    cell_map = null(), tile_map = null(), &
                                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                                    dim_names = (/dim_names(1)/), dat = dat1_r))
                                                deallocate(dat1_r)
                                            case default
                                                dat1_r = (/dat2_r(j, :)/)
                                                allocate(file_buffer(n)%field, source = io_field_real1d( &
                                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                    cell_map = null(), tile_map = null(), &
                                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                                    dim_names = (/dim_names(2)/), dat = dat1_r))
                                                deallocate(dat1_r)
                                        end select
                                    else if (allocated(dat3_r)) then
                                        select case (mapped_dim_order(1))
                                            case (3)
                                                dat2_r = dat3_r(:, :, j)
                                                allocate(file_buffer(n)%field, source = io_field_real2d( &
                                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                    cell_map = null(), tile_map = null(), &
                                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                                    dim_names = (/dim_names(1), dim_names(2)/), dat = dat2_r))
                                                deallocate(dat2_r)
                                            case (2)
                                                dat2_r = dat3_r(:, j, :)
                                                allocate(file_buffer(n)%field, source = io_field_real2d( &
                                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                    cell_map = null(), tile_map = null(), &
                                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                                    dim_names = (/dim_names(1), dim_names(3)/), dat = dat2_r))
                                                deallocate(dat2_r)
                                            case default
                                                dat2_r = dat3_r(j, :, :)
                                                allocate(file_buffer(n)%field, source = io_field_real2d( &
                                                    mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                    cell_map = null(), tile_map = null(), &
                                                    mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                                    dim_names = (/dim_names(2), dim_names(3)/), dat = dat2_r))
                                                deallocate(dat2_r)
                                        end select
                                    end if
                                    write(code, *) j
                                    file_buffer(n)%field%label = trim(DIM_NAME_GRU) // ' ' // trim(adjustl(code))
                                    file_buffer(n)%field%id = i
                                    file_buffer(n)%field%short_name = 'GRU'
                                    file_buffer(n)%field%description = description
                                    file_buffer(n)%field%units = units
                                    n = n + 1
                                    nplus = nplus + 1
                                end do
                            end if
                            if (allocated(dat2_r)) deallocate(dat2_r)
                            if (allocated(dat3_r)) deallocate(dat3_r)
                            if (allocated(mapped_dim_order)) deallocate(mapped_dim_order)

                            !> Cycle to skip assinging the 'gru' variable itself.
                            cycle

                        !> Time (if multi-frame).
                        case (DIM_NAME_TIME, DIM_NAME_T)

                            !> Expand variable list.
                            call expand_field_list(file_buffer, nattrs, ierr)

                            !> Check calendar.
                            call nc4_get_attribute_type(input_file%iunit, i, 'calendar', length = alength, ierr = ierr)
                            if (ierr == 0) then
                                allocate(character(len = alength) :: dat_c)
                                call nc4_get_attribute(input_file%iunit, 'calendar', dat_c, vid = i, ierr = ierr)
                                if (ierr /= 0) then
                                    if (v) call print_warning( &
                                        "Unable to read the 'calendar' attribute from the '" // trim(vname) // "' variable.")
                                else
                                    select case (dat_c)
                                        case ('gregorian', 'standard')
                                        case default
                                            if (v) call print_warning( &
                                                "The calendar '" // dat_c // "' of the '" // trim(vname) // &
                                                "' variable is unknown or not supported.")
                                    end select
                                end if
                                deallocate(dat_c)
                            end if

                            !> Mark file as multi-frame.
                            input_file%multi_frame = .true.

                            !> Set the current position in the file.
                            input_file%ipos = 1

                            !> Derive the start-time (from the first record of the 'time' variable).
                            if (ndims /= 1) then
                                write(code, *) ndims
                                call print_error( &
                                    "The '" // trim(vname) // "' variable contains an incompatible number of dimensions " // &
                                    "for the time dimension (" // trim(adjustl(code)) // "). " // &
                                    "The variable must contain only one dimension.")
                                error_status = 1
                                return
                            else
                                call nc4_get_time( &
                                    input_file%iunit, &
                                    year = input_file%start%year, month = input_file%start%month, day = input_file%start%day, &
                                    jday = input_file%start%jday, &
                                    hour = input_file%start%hour, minutes = input_file%start%minutes, &
                                    time_shift = input_file%time_offset, &
                                    ierr = ierr)
                                if (ierr /= 0) then
                                    error_status = 1
                                    return
                                end if
                            end if

                            !> Cycle to skip assinging the 'time' variable itself.
                            cycle
                    end select

                    !> Register the field.
                    ierr = 0
                    select case (dtype)
                        case (NF90_INT64, NF90_BYTE, NF90_SHORT, NF90_INT)
                            select case (ndims)
                                case (5)
                                    allocate(dat5_i(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4), dim_lengths(5)))
                                    dat5_i = huge(dat5_i)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat5_i, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_int5d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat5_i, no_data_value = fill_i))
                                    deallocate(dat5_i)
                                case (4)
                                    allocate(dat4_i(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)))
                                    dat4_i = huge(dat4_i)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat4_i, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_int4d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat4_i, no_data_value = fill_i))
                                    deallocate(dat4_i)
                                case (3)
                                    allocate(dat3_i(dim_lengths(1), dim_lengths(2), dim_lengths(3)))
                                    dat3_i = huge(dat3_i)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat3_i, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_int3d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat3_i, no_data_value = fill_i))
                                    deallocate(dat3_i)
                                case (2)
                                    allocate(dat2_i(dim_lengths(1), dim_lengths(2)))
                                    dat2_i = huge(dat2_i)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat2_i, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_int2d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat2_i, no_data_value = fill_i))
                                    deallocate(dat2_i)
                                case (1)
                                    allocate(dat1_i(dim_lengths(1)))
                                    dat1_i = huge(dat1_i)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat1_i, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_int1d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat1_i, no_data_value = fill_i))
                                    deallocate(dat1_i)
                                case default
                                    call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat_i, ierr = ierr)
                                    if (ierr == 0) then
                                        allocate(file_buffer(n)%field, source = io_field_int( &
                                            mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                            dat = dat_i))
                                    end if
                            end select
                        case (NF90_FLOAT, NF90_DOUBLE)
                            select case (ndims)
                                case (5)
                                    allocate(dat5_r(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4), dim_lengths(5)))
                                    dat5_r = huge(dat5_r)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat5_r, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_real5d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat5_r, no_data_value = fill_r))
                                    deallocate(dat5_r)
                                case (4)
                                    allocate(dat4_r(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)))
                                    dat4_r = huge(dat4_r)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat4_r, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_real4d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat4_r, no_data_value = fill_r))
                                    deallocate(dat4_r)
                                case (3)
                                    allocate(dat3_r(dim_lengths(1), dim_lengths(2), dim_lengths(3)))
                                    dat3_r = huge(dat3_r)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat3_r, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_real3d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat3_r, no_data_value = fill_r))
                                    deallocate(dat3_r)
                                case (2)
                                    allocate(dat2_r(dim_lengths(1), dim_lengths(2)))
                                    dat2_r = huge(dat2_r)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat2_r, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_real2d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat2_r, no_data_value = fill_r))
                                    deallocate(dat2_r)
                                case (1)
                                    allocate(dat1_r(dim_lengths(1)))
                                    dat1_r = huge(dat1_r)
                                    if (time_order == 0) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat1_r, ierr = ierr)
                                    end if
                                    allocate(file_buffer(n)%field, source = io_field_real1d( &
                                        mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                        cell_map = null(), tile_map = null(), &
                                        mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                        dim_names = dim_names, time_order = time_order, dat = dat1_r, no_data_value = fill_r))
                                    deallocate(dat1_r)
                                case default
                                    call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat_r, ierr = ierr)
                                    if (ierr == 0) then
                                        allocate(file_buffer(n)%field, source = io_field_real( &
                                            mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                            mapped_dat_cell_interp = null(), mapped_dat_tile_interp = null(), &
                                            dat = dat_r))
                                    end if
                            end select
                        case (NF90_CHAR)
                            if (time_order > 0) then
                                call print_error("'char' type time-series are not supported for variable '" // trim(vname) // "'.")
                                cycle
                            end if
                            select case (ndims)
                                case (2)

                                    !> Try to determine the non-character length dimension.
                                    call get_dimension_order( &
                                        dim_names, &
                                        (/DIM_NAME_X, DIM_NAME_Y, DIM_NAME_M, DIM_NAME_K, DIM_NAME_N, DIM_NAME_B, DIM_NAME_G/), &
                                        mapped_dim_order, ierr)
                                    if (count(mapped_dim_order == 1) == 1) then
                                        allocate(character(len = dim_lengths(2)) :: dat1_c(dim_lengths(1)))
                                        j = 1
                                    else if (count(mapped_dim_order == 2) == 1) then
                                        allocate(character(len = dim_lengths(1)) :: dat1_c(dim_lengths(2)))
                                        j = 2
                                    end if
                                    if (allocated(dat1_c)) then
                                        call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat1_c, ierr = ierr)
                                        if (ierr == 0) then
                                            allocate(file_buffer(n)%field, source = io_field_char1d( &
                                                mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                                cell_map = null(), tile_map = null(), &
                                                dim_names = (/dim_names(j)/), dat = dat1_c, no_data_value = fill_c))
                                        end if
                                        deallocate(dat1_c)
                                    end if
                                case (1)
                                    allocate(character(len = dim_lengths(1)) :: dat_c)
                                    call nc4_get_data(input_file%iunit, vname, vid = i, dat = dat_c, ierr = ierr)
                                    if (ierr == 0) then
                                        allocate(file_buffer(n)%field, source = io_field_char( &
                                            mapped_dim_order = null(), mapped_dat_cell = null(), mapped_dat_tile = null(), &
                                            dat = dat_c))
                                    end if
                                    deallocate(dat_c)
                                case default
                                    if (v) then
                                        write(code, *) ndims
                                        call print_warning( &
                                            "The variable '" // trim(vname) // "' of type 'character' contains an unsupported " // &
                                            "number of dimensions (" // trim(adjustl(code)) // ").")
                                    end if
                            end select
                        case default
                            if (v) then
                                write(code, *) ndims
                                call print_warning( &
                                    "The variable '" // trim(vname) // "' is an unknown or unsupported data format (Code: " // &
                                    trim(adjustl(code)) // ").")
                            end if
                    end select
                    if (allocated(file_buffer(n)%field)) then
                        file_buffer(n)%field%label = trim(vname)
                        file_buffer(n)%field%id = i
                        file_buffer(n)%field%short_name = vname
                        file_buffer(n)%field%description = description
                        file_buffer(n)%field%units = units
                        n = n + 1
                    end if
                end do
#endif

            !> CLASS MET format.
            class is (io_file_met)

                !> Rewind the file.
                rewind(input_file%iunit)

                !> Check if a multi-frame file.
                if (input_file%multi_frame) then

                    !> Set the number of active variables based on existing field definitions in the file.
                    if (allocated(input_file%fields)) n = size(input_file%fields) + 1

                    !> Set the current position in the file and return.
                    input_file%ipos = 1

                    !> Read the start date.
                    allocate(dat1_i(4))
                    read(input_file%iunit, *, iostat = ierr) dat1_i
                    if (ierr == 0) then
                        input_file%start%year = dat1_i(4)
                        input_file%start%jday = dat1_i(3)
                        call jday_to_date(dat1_i(4), dat1_i(3), input_file%start%month, input_file%start%day)
                        input_file%start%hour = dat1_i(1)
                        input_file%start%minutes = dat1_i(2)
                    else
                        call print_warning("Unable to read the time associated with the first frame in the file.")
                        ierr = 0
                    end if

                    !> Rewind file.
                    rewind(input_file%iunit)
                end if
        end select

        !> Adjust the total number of fields read (since it increments at the end of the loop).
        n = n - 1

        !> Print diagnostic summary.
        if (DIAGNOSEMODE) then
            if (natts /= -1) then
                write(code, *) natts
                call print_message(trim(adjustl(code)) // " attributes found in the file.")
            end if
            if (nvars /= -1) then
                write(code, *) nvars + nplus
                call print_message(trim(adjustl(code)) // " variables found in the file.")
            end if
            if (n > 0) then
                write(code, *) n
                call print_message(trim(adjustl(code)) // " valid fields found in the file.")
            end if
        end if
        if (n == 0 .and. v) then
            call print_warning("No valid fields were found in the file.")
        end if

        !> Combine list to 'input_file'.
        if (allocated(file_buffer)) then
            call combine_field_list(input_file%fields, file_buffer, error_status)
        end if

        !> Print a diagnostic summary of active fields in the file.
        if (allocated(input_file%fields)) then
            do i = 1, size(input_file%fields)

                !> Map dimension orders.
                select type (this => input_file%fields(i)%field)
                    class is (io_field_Nd)

                        !> Map dimensions of the variable.
                        if (allocated(this%dim_names)) then
                            if (this%time_order == 1 .and. size(this%dim_names) == 1) then

                                !> Identify the field as a basin-wide value 'DIM_NAME_B' if the only dimension is time 'DIM_NAME_T'.
                                allocate(this%mapped_dim_order(len(MAP_ORDER_LIST)))
                                this%mapped_dim_order = 0
                                this%mapped_dim_order(MAP_ORDER_B) = 1
                            else

                                !> Map spatial dimensions.
                                call get_dimension_order(this%dim_names, MAP_ORDER_LIST, this%mapped_dim_order, ierr)
                            end if
                        else

                            !> Print an error if the spatial dimensions of the multi-dimensional field cannot be derived.
                            call print_error( &
                                "The spatial dimensions of the field '" // trim(this%label) // &
                                "' are not recognized or are undefined.")
                            error_status = 1
                        end if

                        !> Derive field name and level from ID.
                        call get_field_name_and_level( &
                            input_file%fields(i)%field%label, input_file%fields(i)%field%field_name, &
                            input_file%fields(i)%field%level, input_file%fields(i)%field%level_id, ierr)
                        if (ierr /= 0) then
                            call print_warning( &
                                "An error occurred identifying the label associated with the '" // &
                                trim(input_file%fields(i)%field%label) // "' variable.")
                        end if
                    class default

                        !> Assume a basin-wide value 'DIM_NAME_B' for scalar fields.
                        allocate(this%mapped_dim_order(len(MAP_ORDER_LIST)))
                        this%mapped_dim_order = 0
                        this%mapped_dim_order(MAP_ORDER_B) = 1
                end select

                !> Print message.
                if (DIAGNOSEMODE) call print_info("Found the variable '" // trim(input_file%fields(i)%field%label) // "'.")

                !> Check if the field is mapped to an external ID.
                if (allocated(input_file%field_map)) then
                    do j = 1, size(input_file%field_map, 1)
                        if (input_file%fields(i)%field%label == input_file%field_map(j, 1)) then

                            !> Check if the field name is mapping to the same field name.
                            if (input_file%field_map(j, 1) /= input_file%field_map(j, 2)) then

                                !> Save the internal name.
                                input_file%fields(i)%field%mapped_name = trim(input_file%field_map(j, 2))
                                call print_remark( &
                                    "Mapping the field '" // trim(input_file%fields(i)%field%label) // "' to the '" // &
                                    trim(input_file%field_map(j, 2)) // "' variable.")

                                !> Update field name.
                                input_file%fields(i)%field%field_name = input_file%fields(i)%field%mapped_name
                            end if
                            exit
                        end if
                    end do
                end if
            end do
        end if

    end subroutine

    subroutine activate_variable_from_field(input_field, error_status)

        !> Modules.
        use strings, only: uppercase
        use variable_names
        use model_variables, only: vs

        !> Input/output variables.
        !*  input_field: Input field to assign to 'output_field'.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_field), intent(in) :: input_field
        integer, intent(out) :: error_status

        !> Local variables
        integer i, isaved

        !> Status.
        error_status = 0

        !> Identify and assign variables.
        isaved = 1
        select case (uppercase(input_field%field_name))

            !> Meteorology/climatology variables.
            case (VN_FSIN)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%fsin)) then
                        allocate(vs%grid%fsin(vs%grid%dim_length))
                        vs%grid%fsin = huge(vs%grid%fsin)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%fsin)) then
                        allocate(vs%tile%fsin(vs%tile%dim_length))
                        vs%tile%fsin = huge(vs%tile%fsin)
                    end if
                end if
            case (VN_FLIN)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%flin)) then
                        allocate(vs%grid%flin(vs%grid%dim_length))
                        vs%grid%flin = huge(vs%grid%flin)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%flin)) then
                        allocate(vs%tile%flin(vs%tile%dim_length))
                        vs%tile%flin = huge(vs%tile%flin)
                    end if
                end if
            case (VN_TA)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%ta)) then
                        allocate(vs%grid%ta(vs%grid%dim_length))
                        vs%grid%ta = huge(vs%grid%ta)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%ta)) then
                        allocate(vs%tile%ta(vs%tile%dim_length))
                        vs%tile%ta = huge(vs%tile%ta)
                    end if
                end if
            case (VN_QA)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%qa)) then
                        allocate(vs%grid%qa(vs%grid%dim_length))
                        vs%grid%qa = huge(vs%grid%qa)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%qa)) then
                        allocate(vs%tile%qa(vs%tile%dim_length))
                        vs%tile%qa = huge(vs%tile%qa)
                    end if
                end if
            case (VN_PRES)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%pres)) then
                        allocate(vs%grid%pres(vs%grid%dim_length))
                        vs%grid%pres = huge(vs%grid%pres)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%pres)) then
                        allocate(vs%tile%pres(vs%tile%dim_length))
                        vs%tile%pres = huge(vs%tile%pres)
                    end if
                end if
            case (VN_UU)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%uu)) then
                        allocate(vs%grid%uu(vs%grid%dim_length))
                        vs%grid%uu = huge(vs%grid%uu)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%uu)) then
                        allocate(vs%tile%uu(vs%tile%dim_length))
                        vs%tile%uu = huge(vs%tile%uu)
                    end if
                end if
            case (VN_VV)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%vv)) then
                        allocate(vs%grid%vv(vs%grid%dim_length))
                        vs%grid%vv = huge(vs%grid%vv)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%vv)) then
                        allocate(vs%tile%vv(vs%tile%dim_length))
                        vs%tile%vv = huge(vs%tile%vv)
                    end if
                end if
            case (VN_UV)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%uv)) then
                        allocate(vs%grid%uv(vs%grid%dim_length))
                        vs%grid%uv = huge(vs%grid%uv)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%uv)) then
                        allocate(vs%tile%uv(vs%tile%dim_length))
                        vs%tile%uv = huge(vs%tile%uv)
                    end if
                end if
            case (VN_WDIR)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%wdir)) then
                        allocate(vs%grid%wdir(vs%grid%dim_length))
                        vs%grid%wdir = huge(vs%grid%wdir)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%wdir)) then
                        allocate(vs%tile%wdir(vs%tile%dim_length))
                        vs%tile%wdir = huge(vs%tile%wdir)
                    end if
                end if
            case (VN_PRERN)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%prern)) then
                        allocate(vs%grid%prern(vs%grid%dim_length))
                        vs%grid%prern = huge(vs%grid%prern)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%prern)) then
                        allocate(vs%tile%prern(vs%tile%dim_length))
                        vs%tile%prern = huge(vs%tile%prern)
                    end if
                end if
            case (VN_PRESNO)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%presno)) then
                        allocate(vs%grid%presno(vs%grid%dim_length))
                        vs%grid%presno = huge(vs%grid%presno)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%presno)) then
                        allocate(vs%tile%presno(vs%tile%dim_length))
                        vs%tile%presno = huge(vs%tile%presno)
                    end if
                end if
            case (VN_PRE)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%pre)) then
                        allocate(vs%grid%pre(vs%grid%dim_length))
                        vs%grid%pre = huge(vs%grid%pre)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%pre)) then
                        allocate(vs%tile%pre(vs%tile%dim_length))
                        vs%tile%pre = huge(vs%tile%pre)
                    end if
                end if

            !> Routing variables.
            case (VN_RFF)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%rff)) then
                        allocate(vs%grid%rff(vs%grid%dim_length))
                        vs%grid%rff = huge(vs%grid%rff)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%rff)) then
                        allocate(vs%tile%rff(vs%tile%dim_length))
                        vs%tile%rff = huge(vs%tile%rff)
                    end if
                end if
            case (VN_RCHG)
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%rchg)) then
                        allocate(vs%grid%rchg(vs%grid%dim_length))
                        vs%grid%rchg = huge(vs%grid%rchg)
                    end if
                end if
                if (associated(vs%tile)) then
                    if (.not. associated(vs%tile%rchg)) then
                        allocate(vs%tile%rchg(vs%tile%dim_length))
                        vs%tile%rchg = huge(vs%tile%rchg)
                    end if
                end if

            !> Others (not saved).
            case default
                isaved = 0
        end select

        !> Check 'isaved' status.
        if (isaved == 1) then
            if (DIAGNOSEMODE) call print_info("Activated the variable '" // trim(input_field%label) // "'.")
        end if

    end subroutine

    subroutine activate_variables_from_field_list(field_list, error_status)

        !> Input/output variables.
        !*  field_list: List of fields (read from file).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        type(io_field_wrapper), dimension(:), intent(in) :: field_list
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Status.
        error_status = 0

        !> Loop through the fields to assign remaining variables.
        ierr = 0
        do i = 1, size(field_list)
            call activate_variable_from_field(field_list(i)%field, ierr)
            if (ierr /= 0) error_status = ierr
        end do

    end subroutine

    subroutine read_frame_from_file(input_file, skip_data, quiet, error_status)
#ifdef NETCDF

        !> I/O modules.
        use nc_io
#endif

        !> Input/output variables.
        !*  input_file: Input file object ('io_file').
        !*  skip_data: .true. to skip data and just move the 'ipos' position in the file.
        !*  quiet: .true. to suppress block formatting (optional, default: .false.).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_file) input_file
        logical, intent(in), optional :: skip_data
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        logical v, s

        !> File utility variables.
        character(len = SHORT_FIELD_LENGTH) str
        real(kind = 4), allocatable :: dat1_rk4(:)
        real, allocatable :: dat1_r(:)
        integer t, j, i, nrecs, ierr
#ifdef NETCDF
        integer start5(5), start4(4), start3(3), start2(2), start1(1)
#endif

        !> Error status.
        error_status = 0

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Return if not a multi-frame file or the file contains no fields.
        if (.not. input_file%multi_frame .or. .not. allocated(input_file%fields)) return

        !> Check if skipping data.
        s = .false.
        if (present(skip_data)) s = skip_data

        !> Read data in the file.
        nrecs = 0
        select type (input_file)

            !> r2c ASCII format.
            class is (io_file_r2c)

                !> Read from the file (assume field type from known file type).
                select type (this => input_file%fields(1)%field)
                    class is (io_field_real3d)
                        do t = 1, input_file%block_interval

                            !> Read values.
                            read(input_file%iunit, *, iostat = ierr) !frame marker
                            if (ierr == 0) read(input_file%iunit, *, iostat = ierr) &
                                ((this%dat(i, j, t), i = 1, size(this%dat, 1)), j = 1, size(this%dat, 2))
                            if (ierr == 0) read(input_file%iunit, *, iostat = ierr) !frame marker
                            if (ierr /= 0) then
                                error_status = 1
                                exit
                            end if
                            nrecs = nrecs + 1

                            !> Skip frames one at a time.
                            if (s) exit
                        end do
                end select

            !> Simple text formats (txt, asc, CSV).
            class is (io_file_txt_delimited)

                !> Read from the file.
                ierr = 0
                if (s) then

                    !> Skip line.
                    read(input_file%iunit, *, iostat = ierr)
                    if (ierr /= 0) then
                        error_status = 1
                    else
                        nrecs = 1
                    end if
                else

                    !> Read blocks.
                    do t = 1, input_file%block_interval

                        !> Assign values to the field (assume field type from known file type).
                        select type (this => input_file%fields(1)%field)
                            class is (io_field_real2d)

                                !> Read values (skipping the specified number of columns from the start of the line).
                                read(input_file%iunit, *, iostat = ierr) &
                                    (str, i = 1, input_file%n_skip_cols), (this%dat(i, t), i = 1, size(this%dat))
                                if (ierr /= 0) then
                                    error_status = 1
                                    exit
                                else

                                    !> Apply conversions.
                                    this%dat(:, t) = this%const_mul*this%dat(:, t) + this%const_add
                                end if
                        end select

                        !> Increment the number of records read.
                        nrecs = nrecs + 1
                    end do
                end if

            !> Binary sequential format (seq).
            class is (io_file_seq)

                !> Read from the file.
                ierr = 0
                if (s) then

                    !> Skip record.
                    read(input_file%iunit, iostat = ierr)
                    if (ierr == 0) read(input_file%iunit, iostat = ierr)
                    if (ierr /= 0) then
                        error_status = 1
                    else
                        nrecs = 1
                    end if
                else

                    !> Read blocks.
                    do t = 1, input_file%block_interval

                        !> Assign values to the field (assume field type from known file type).
                        select type (this => input_file%fields(1)%field)
                            class is (io_field_real2d)

                                !> Read values.
                                allocate(dat1_rk4(size(this%dat, 1)))
                                read(input_file%iunit, iostat = ierr) !date marker
                                if (ierr == 0) read(input_file%iunit, iostat = ierr) (dat1_rk4(i), i = 1, size(this%dat, 1))
                                if (ierr /= 0) then
                                    error_status = 1
                                    exit
                                else

                                    !> Apply conversions.
                                    this%dat(:, t) = this%const_mul*real(dat1_rk4, kind = kind(this%dat)) + this%const_add
                                end if
                                deallocate(dat1_rk4)
                        end select

                        !> Increment the number of records read.
                        nrecs = nrecs + 1
                    end do
                end if
#ifdef NETCDF

            !> NetCDF format.
            class is (io_file_nc)

                !> Loop through each registered field.
                if (s) then
                    nrecs = 1
                else
                    do i = 1, size(input_file%fields)

                        !> Read the field.
                        start5 = 1
                        start4 = 1
                        start3 = 1
                        start2 = 1
                        start1 = 1
                        ierr = 0
                        select type (this => input_file%fields(i)%field)
                            class is (io_field_int5d)
                                if (this%time_order > 0) then
                                    start5(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start5, ierr = ierr)
                                end if
                            class is (io_field_int4d)
                                if (this%time_order > 0) then
                                    start4(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start4, ierr = ierr)
                                end if
                            class is (io_field_int3d)
                                if (this%time_order > 0) then
                                    start3(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start3, ierr = ierr)
                                end if
                            class is (io_field_int2d)
                                if (this%time_order > 0) then
                                    start2(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start2, ierr = ierr)
                                end if
                            class is (io_field_int1d)
                                if (this%time_order > 0) then
                                    start1(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start1, ierr = ierr)
                                end if
                            class is (io_field_real5d)
                                if (this%time_order > 0) then
                                    start5(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start5, ierr = ierr)
                                    if (ierr == 0) then

                                        !> Apply conversions.
                                        this%dat = this%const_mul*this%dat + this%const_add
                                    end if
                                end if
                            class is (io_field_real4d)
                                if (this%time_order > 0) then
                                    start4(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start4, ierr = ierr)
                                    if (ierr == 0) then

                                        !> Apply conversions.
                                        this%dat = this%const_mul*this%dat + this%const_add
                                    end if
                                end if
                            class is (io_field_real3d)
                                if (this%time_order > 0) then
                                    start3(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start3, ierr = ierr)
                                    if (ierr == 0) then

                                        !> Apply conversions.
                                        this%dat = this%const_mul*this%dat + this%const_add
                                    end if
                                end if
                            class is (io_field_real2d)
                                if (this%time_order > 0) then
                                    start2(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start2, ierr = ierr)
                                    if (ierr == 0) then

                                        !> Apply conversions.
                                        this%dat = this%const_mul*this%dat + this%const_add
                                    end if
                                end if
                            class is (io_field_real1d)
                                if (this%time_order > 0) then
                                    start1(this%time_order) = input_file%ipos
                                    call nc4_get_data(input_file%iunit, this%label, this%id, this%dat, start = start1, ierr = ierr)
                                    if (ierr == 0) then

                                        !> Apply conversions.
                                        this%dat = this%const_mul*this%dat + this%const_add
                                    end if
                                end if
                        end select

                        !> Check for errors.
                        if (ierr /= 0) then
                            error_status = 1
                        else
                            nrecs = input_file%block_interval
                        end if
                    end do
                end if
#endif

            !> CLASS MET format.
            class is (io_file_met)

                !> Allocate the temporary variable.
                if (input_file%rr_sr) then
                    allocate(dat1_r(9))
                else
                    allocate(dat1_r(7))
                end if

                !> Read from the file.
                ierr = 0
                if (s) then

                    !> Skip line.
                    read(input_file%iunit, *, iostat = ierr)
                    if (ierr /= 0) then
                        error_status = 1
                    else
                        nrecs = 1
                    end if
                else

                    !> Read blocks.
                    do t = 1, input_file%block_interval

                        !> Read fields from the line (skipping the first four columns of date information).
                        read(input_file%iunit, *, iostat = ierr) (str, i = 1, 4), (dat1_r(i), i = 1, size(dat1_r))
                        if (ierr /= 0) then
                            error_status = 1
                            exit
                        else

                            !> Assign fields.
                            do i = 1, size(input_file%fields)
                                select type (this => input_file%fields(i)%field)
                                    class is (io_field_real1d)

                                        !> Apply conversions.
                                        this%dat(t) = this%const_mul*dat1_r(this%id) + this%const_add
                                end select
                            end do

                            !> Increment the number of records read.
                            nrecs = nrecs + 1
                        end if
                    end do
                end if
        end select

        !> Increment the positional counter.
        input_file%ipos = input_file%ipos + nrecs

    end subroutine

    subroutine create_ranked_field_and_maps(input_field, error_status)

        !> Variables.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field) input_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer, dimension(:), allocatable :: dim_lengths
        integer j, i

        !> Status.
        error_status = 0

        !> Return if the variable group is not active.
        if (.not. associated(vs%grid) .and. .not. associated(vs%tile)) then
            error_status = 1
            return
        end if

        !> Return if the dimensions of the field are not defined.
        select type (input_field)
            class is (io_field_Nd)
                if (.not. allocated(input_field%dim_names)) then
                    error_status = 1
                    return
                end if
        end select

        !> Allocate the map.
        select type (input_field)
            class is (io_field_real5d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(5, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(5, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(5), source = shape(input_field%dat))
            class is (io_field_real4d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(4, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(4, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(4), source = shape(input_field%dat))
            class is (io_field_real3d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(3, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(3, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(3), source = shape(input_field%dat))
            class is (io_field_real2d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(2, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(2, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(2), source = shape(input_field%dat))
            class is (io_field_real1d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(1, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(1, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(1), source = shape(input_field%dat))
            class is (io_field_real)
                if (associated(vs%grid)) then
                    allocate(input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%mapped_dat_tile(vs%tile%dim_length))
                end if

                !> Return since mapping is necessary.
                return
            class is (io_field_int5d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(5, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(5, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(5), source = shape(input_field%dat))
            class is (io_field_int4d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(4, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(4, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(4), source = shape(input_field%dat))
            class is (io_field_int3d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(3, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(3, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(3), source = shape(input_field%dat))
            class is (io_field_int2d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(2, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(2, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(2), source = shape(input_field%dat))
            class is (io_field_int1d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(1, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(1, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(1), source = shape(input_field%dat))
            class is (io_field_int)
                if (associated(vs%grid)) then
                    allocate(input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%mapped_dat_tile(vs%tile%dim_length))
                end if

                !> Return since mapping is necessary.
                return
            class is (io_field_char1d)
                if (associated(vs%grid)) then
                    allocate(input_field%cell_map(1, vs%grid%dim_length), input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%tile_map(1, vs%tile%dim_length), input_field%mapped_dat_tile(vs%tile%dim_length))
                end if
                allocate(dim_lengths(1), source = shape(input_field%dat))
            class is (io_field_char)
                if (associated(vs%grid)) then
                    allocate(input_field%mapped_dat_cell(vs%grid%dim_length))
                end if
                if (associated(vs%tile)) then
                    allocate(input_field%mapped_dat_tile(vs%tile%dim_length))
                end if

                !> Return since mapping is necessary.
                return
            class default

                !> Return since not a field that requires allocation or mapping.
                return
        end select

        !> Attach the maps of known dimensions.
        select type (input_field)
            class is (io_field_Nd)
                if ( &
                    (associated(vs%grid) .and. .not. allocated(input_field%cell_map)) .or. &
                    (associated(vs%tile) .and. .not. allocated(input_field%tile_map))) then

                    !> Return if the mapping variable has not been allocated.
                    error_status = 1
                    return
                end if
                if (allocated(input_field%cell_map)) input_field%cell_map = 0
                if (allocated(input_field%tile_map)) input_field%tile_map = 0
                do i = 1, size(input_field%mapped_dim_order)

                    !> Identify the map.
                    if (input_field%mapped_dim_order(MAP_ORDER_X) > 0 .and. input_field%mapped_dim_order(MAP_ORDER_y) > 0) then
                        if (allocated(input_field%cell_map)) then
                            input_field%cell_map(input_field%mapped_dim_order(MAP_ORDER_X), :) = vs%grid%from_grid_x
                            input_field%cell_map(input_field%mapped_dim_order(MAP_ORDER_Y), :) = vs%grid%from_grid_y
                        end if
                        if (allocated(input_field%tile_map)) then
                            input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_X), :) = vs%tile%from_grid_x
                            input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_Y), :) = vs%tile%from_grid_y
                        end if
                    else if (input_field%mapped_dim_order(MAP_ORDER_M) > 0) then
                        if (allocated(input_field%tile_map)) then
                            input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_M), :) = vs%tile%from_gru
                        end if
                    else if (input_field%mapped_dim_order(MAP_ORDER_K) > 0) then
                        if (allocated(input_field%cell_map)) then
                            input_field%cell_map(input_field%mapped_dim_order(MAP_ORDER_K), :) = vs%grid%from_riverclass
                        end if
                        if (allocated(input_field%tile_map)) then
                            input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_K), :) = vs%tile%from_riverclass
                        end if
                    else if (input_field%mapped_dim_order(MAP_ORDER_N) > 0) then
                        if (allocated(input_field%cell_map)) then
                            do j = 1, vs%grid%dim_length
                                input_field%cell_map(input_field%mapped_dim_order(MAP_ORDER_N), j) = j
                            end do
                        end if
                        if (allocated(input_field%tile_map)) then
                            input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_N), :) = vs%tile%from_cell
                        end if
                    else if (input_field%mapped_dim_order(MAP_ORDER_B) > 0) then
                        if (allocated(input_field%cell_map)) then
                            input_field%cell_map(input_field%mapped_dim_order(MAP_ORDER_B), :) = 1
                        end if
                        if (allocated(input_field%tile_map)) then
                            input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_B), :) = 1
                        end if
                    else if (input_field%mapped_dim_order(MAP_ORDER_G) > 0) then
                        if (allocated(input_field%tile_map)) then
                            do j = 1, vs%tile%dim_length
                                input_field%tile_map(input_field%mapped_dim_order(MAP_ORDER_G), j) = j
                            end do
                        end if
                    end if
                end do

                !> Check for unmapped dimensions.
                do i = 1, size(dim_lengths)
                    if (i == input_field%time_order) then

                        !> Set an initial index to the 'time' dimension.
                        if (associated(vs%grid)) input_field%cell_map(i, :) = 1
                        if (associated(vs%tile)) input_field%tile_map(i, :) = 1
                    else if (dim_lengths(i) == 1) then

                        !> Allow a map if the size of the unmapped dimensions is one.
                        if (associated(vs%grid)) then
                            if (all(input_field%cell_map(i, :) == 0)) input_field%cell_map(i, :) = 1
                        end if
                        if (associated(vs%tile)) then
                            if (all(input_field%tile_map(i, :) == 0)) input_field%tile_map(i, :) = 1
                        end if
                    else

                        !> Check for unassigned field.
                        if (associated(vs%grid)) then
                            if (all(input_field%cell_map(i, :) == 0)) error_status = 1
                        end if
                        if (associated(vs%tile)) then
                            if (all(input_field%tile_map(i, :) == 0)) error_status = 1
                        end if
                    end if
                end do
        end select

    end subroutine

    subroutine map_field_to_ranked_output(input_field, time_id, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field) input_field
        integer, intent(in), optional :: time_id
        integer, intent(out) :: error_status

        !> Local variables.
        real, allocatable :: dat1_r(:)
        integer t, i

        !> Status.
        error_status = 0

        !> Reset fields.
        select type (input_field)
            class is (io_field_real)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = huge(input_field%mapped_dat_cell)
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = huge(input_field%mapped_dat_tile)
            class is (io_field_real1d)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = huge(input_field%mapped_dat_cell)
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = huge(input_field%mapped_dat_tile)
            class is (io_field_int)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = huge(input_field%mapped_dat_cell)
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = huge(input_field%mapped_dat_tile)
            class is (io_field_int1d)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = huge(input_field%mapped_dat_cell)
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = huge(input_field%mapped_dat_tile)
            class is (io_field_char)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = huge(input_field%mapped_dat_cell)
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = huge(input_field%mapped_dat_tile)
            class is (io_field_char1d)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = huge(input_field%mapped_dat_cell)
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = huge(input_field%mapped_dat_tile)
        end select

        !> Return if maps are not defined.
        select type (input_field)
            class is (io_field_Nd)
                if (allocated(input_field%cell_map)) then
                    if (all(input_field%cell_map == 0)) return
                else if (allocated(input_field%tile_map)) then
                    if (all(input_field%tile_map == 0)) return
                end if
        end select

        !> Time index.
        t = 1
        if (present(time_id)) t = time_id

        !> Map the field.
        select type (input_field)

            !> real.
            class is (io_field_real)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = input_field%dat
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = input_field%dat

            !> real(:).
            class is (io_field_real1d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat(input_field%cell_map(1, i))
                        end do
                    else
                        input_field%mapped_dat_cell = input_field%dat
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat(input_field%tile_map(1, i))
                        end do
                    else
                        input_field%mapped_dat_tile = input_field%dat
                    end if
                end if

            !> real(:, :).
            class is (io_field_real2d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat(input_field%cell_map(1, i), input_field%cell_map(2, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat(input_field%tile_map(1, i), input_field%tile_map(2, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> real(:, :, :).
            class is (io_field_real3d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat( &
                                input_field%cell_map(1, i), input_field%cell_map(2, i), input_field%cell_map(3, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat( &
                                input_field%tile_map(1, i), input_field%tile_map(2, i), input_field%tile_map(3, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> real(:, :, :, :).
            class is (io_field_real4d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat( &
                                input_field%cell_map(1, i), input_field%cell_map(2, i), input_field%cell_map(3, i), &
                                input_field%cell_map(4, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat( &
                                input_field%tile_map(1, i), input_field%tile_map(2, i), input_field%tile_map(3, i), &
                                input_field%tile_map(4, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> real(:, :, :, :, :).
            class is (io_field_real5d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat( &
                                input_field%cell_map(1, i), input_field%cell_map(2, i), input_field%cell_map(3, i), &
                                input_field%cell_map(4, i), input_field%cell_map(5, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat( &
                                input_field%tile_map(1, i), input_field%tile_map(2, i), input_field%tile_map(3, i), &
                                input_field%tile_map(4, i), input_field%tile_map(5, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> integer.
            class is (io_field_int)
                if (allocated(input_field%mapped_dat_cell)) input_field%mapped_dat_cell = input_field%dat
                if (allocated(input_field%mapped_dat_tile)) input_field%mapped_dat_tile = input_field%dat

            !> integer(:).
            class is (io_field_int1d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat(input_field%cell_map(1, i))
                        end do
                    else
                        input_field%mapped_dat_cell = input_field%dat
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat(input_field%tile_map(1, i))
                        end do
                    else
                        input_field%mapped_dat_tile = input_field%dat
                    end if
                end if

            !> integer(:, :).
            class is (io_field_int2d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat(input_field%cell_map(1, i), input_field%cell_map(2, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat(input_field%tile_map(1, i), input_field%tile_map(2, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> integer(:, :, :).
            class is (io_field_int3d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat( &
                                input_field%cell_map(1, i), input_field%cell_map(2, i), input_field%cell_map(3, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat( &
                                input_field%tile_map(1, i), input_field%tile_map(2, i), input_field%tile_map(3, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> integer(:, :, :, :).
            class is (io_field_int4d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat( &
                                input_field%cell_map(1, i), input_field%cell_map(2, i), input_field%cell_map(3, i), &
                                input_field%cell_map(4, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat( &
                                input_field%tile_map(1, i), input_field%tile_map(2, i), input_field%tile_map(3, i), &
                                input_field%tile_map(4, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> integer(:, :, :, :, :).
            class is (io_field_int5d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        if (input_field%time_order > 0) input_field%cell_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_cell)
                            input_field%mapped_dat_cell(i) = input_field%dat( &
                                input_field%cell_map(1, i), input_field%cell_map(2, i), input_field%cell_map(3, i), &
                                input_field%cell_map(4, i), input_field%cell_map(5, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        if (input_field%time_order > 0) input_field%tile_map(input_field%time_order, :) = t
                        do i = 1, size(input_field%mapped_dat_tile)
                            input_field%mapped_dat_tile(i) = input_field%dat( &
                                input_field%tile_map(1, i), input_field%tile_map(2, i), input_field%tile_map(3, i), &
                                input_field%tile_map(4, i), input_field%tile_map(5, i))
                        end do
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> character(:)
            class is (io_field_char1d)
                if (allocated(input_field%mapped_dat_cell)) then
                    if (allocated(input_field%cell_map)) then
                        call assign_field(input_field, dat1_r, error_status)
                        if (error_status == 0) then
                            do i = 1, size(input_field%mapped_dat_cell)
                                input_field%mapped_dat_cell(i) = dat1_r(input_field%cell_map(1, i))
                            end do
                        end if
                    else
                        call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                    end if
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    if (allocated(input_field%tile_map)) then
                        call assign_field(input_field, dat1_r, error_status)
                        if (error_status == 0) then
                            do i = 1, size(input_field%mapped_dat_tile)
                                input_field%mapped_dat_tile(i) = dat1_r(input_field%tile_map(1, i))
                            end do
                        end if
                    else
                        call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                    end if
                end if

            !> character.
            class is (io_field_char)
                if (allocated(input_field%mapped_dat_cell)) then
                    call assign_field(input_field, input_field%mapped_dat_cell, error_status)
                end if
                if (allocated(input_field%mapped_dat_tile)) then
                    call assign_field(input_field, input_field%mapped_dat_tile, error_status)
                end if
        end select

    end subroutine

    subroutine create_mapped_output_from_field(input_field, time_id, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        !*  input_field: Input field to assign to 'output_field'.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_field) input_field
        integer, intent(in), optional :: time_id
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, time_order, iwarn, ierr

        !> Map dimensions and transfer the time order to a local variable (for diagnostic output).
        time_order = 0
        iwarn = 0
        ierr = 0
        select type (input_field)
            class is (io_field_Nd)
                time_order = input_field%time_order
                if (.not. allocated(input_field%cell_map) .and. .not. allocated(input_field%tile_map)) then

                    !> Map the dimensions.
                    call create_ranked_field_and_maps(input_field, ierr)
                    if (ierr /= 0) iwarn = 2
                end if
        end select

        !> Map the variable.
        call map_field_to_ranked_output(input_field, time_id, ierr)
        if (ierr /= 0) iwarn = 1

        !> Special conditions not covered by mapping alone.
        if (input_field%mapped_dim_order(MAP_ORDER_M) > 0) then
            select type (input_field)
                class is (io_field_realNd)
                    if (all(input_field%mapped_dat_tile /= huge(input_field%mapped_dat_tile))) then
                        input_field%mapped_dat_cell = 0.0
                        do i = 1, vs%tile%dim_length
                            input_field%mapped_dat_cell(vs%tile%from_cell(i)) = &
                                input_field%mapped_dat_cell(vs%tile%from_cell(i)) + &
                                input_field%mapped_dat_tile(i)*vs%tile%area_weight(i)
                        end do
                    end if
            end select
        else if (input_field%mapped_dim_order(MAP_ORDER_G) > 0) then
            select type (input_field)
                class is (io_field_realNd)
                    if (all(input_field%mapped_dat_tile /= huge(input_field%mapped_dat_tile))) then
                        input_field%mapped_dat_cell = 0.0
                        do i = 1, vs%tile%dim_length
                            input_field%mapped_dat_cell(vs%tile%from_cell(i)) = &
                                input_field%mapped_dat_cell(vs%tile%from_cell(i)) + &
                                input_field%mapped_dat_tile(i)*vs%tile%area_weight(i)
                        end do
                    end if
            end select
        end if

        !> Check status.
        if (.not. time_order > 0) then
            select case (iwarn)
                case (2)
                    call print_error( &
                        "An error occurred identifying and mapping the spatial dimensions of the '" // trim(input_field%label) // &
                        "' variable.")
                    error_status = 1
                case (1)
                    call print_error("An error occurred mapping the '" // trim(input_field%label) // "' variable.")
                    error_status = 1
            end select
        end if

    end subroutine

    subroutine create_mapped_output_from_field_list(field_list, time_id, error_status)

        !> Input/output variables.
        !*  field_list: List of fields (read from file).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        type(io_field_wrapper), dimension(:), intent(in) :: field_list
        integer, intent(in), optional :: time_id
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Status.
        error_status = 0

        !> Loop through the fields to assign remaining variables.
        ierr = 0
        do i = 1, size(field_list)
            call create_mapped_output_from_field(field_list(i)%field, time_id, ierr)
            if (ierr /= 0) error_status = ierr
        end do

    end subroutine

    subroutine assign_cell_values_real(input_field, ranked_output, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:), allocatable :: ranked_output
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. allocated(ranked_output)) allocate(ranked_output(vs%grid%dim_length))
        ranked_output = huge(ranked_output)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output = input_field%mapped_dat_cell
            class is (io_field_realNd)
                ranked_output = input_field%mapped_dat_cell
            class is (io_field_int)
                ranked_output = real(input_field%mapped_dat_cell)
            class is (io_field_intNd)
                ranked_output = real(input_field%mapped_dat_cell)
            class is (io_field_char)
                ranked_output = input_field%mapped_dat_cell
            class is (io_field_char1d)
                ranked_output = input_field%mapped_dat_cell
        end select

        !> Check for errors.
        if (all(ranked_output == huge(ranked_output))) error_status = 1

    end subroutine

    subroutine assign_cell_values_real_pntr(input_field, ranked_output_pntr, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:), pointer :: ranked_output_pntr
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. associated(ranked_output_pntr)) allocate(ranked_output_pntr(vs%grid%dim_length))
        ranked_output_pntr = huge(ranked_output_pntr)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output_pntr = input_field%mapped_dat_cell
            class is (io_field_realNd)
                ranked_output_pntr = input_field%mapped_dat_cell
            class is (io_field_int)
                ranked_output_pntr = real(input_field%mapped_dat_cell)
            class is (io_field_intNd)
                ranked_output_pntr = real(input_field%mapped_dat_cell)
            class is (io_field_char)
                ranked_output_pntr = input_field%mapped_dat_cell
            class is (io_field_char1d)
                ranked_output_pntr = input_field%mapped_dat_cell
        end select

        !> Check for errors.
        if (all(ranked_output_pntr == huge(ranked_output_pntr))) error_status = 1

    end subroutine

    subroutine assign_cell_values_int(input_field, ranked_output, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:), allocatable :: ranked_output
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. allocated(ranked_output)) allocate(ranked_output(vs%grid%dim_length))
        ranked_output = huge(ranked_output)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output = int(input_field%mapped_dat_cell)
            class is (io_field_realNd)
                ranked_output = int(input_field%mapped_dat_cell)
            class is (io_field_int)
                ranked_output = input_field%mapped_dat_cell
            class is (io_field_intNd)
                ranked_output = input_field%mapped_dat_cell
            class is (io_field_char)
                ranked_output = int(input_field%mapped_dat_cell)
            class is (io_field_char1d)
                ranked_output = int(input_field%mapped_dat_cell)
        end select

        !> Check for errors.
        if (all(ranked_output == huge(ranked_output))) error_status = 1

    end subroutine

    subroutine assign_cell_values_int_pntr(input_field, ranked_output_pntr, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:), pointer :: ranked_output_pntr
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. associated(ranked_output_pntr)) allocate(ranked_output_pntr(vs%grid%dim_length))
        ranked_output_pntr = huge(ranked_output_pntr)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output_pntr = int(input_field%mapped_dat_cell)
            class is (io_field_realNd)
                ranked_output_pntr = int(input_field%mapped_dat_cell)
            class is (io_field_int)
                ranked_output_pntr = input_field%mapped_dat_cell
            class is (io_field_intNd)
                ranked_output_pntr = input_field%mapped_dat_cell
            class is (io_field_char)
                ranked_output_pntr = int(input_field%mapped_dat_cell)
            class is (io_field_char1d)
                ranked_output_pntr = int(input_field%mapped_dat_cell)
        end select

        !> Check for errors.
        if (all(ranked_output_pntr == huge(ranked_output_pntr))) error_status = 1

    end subroutine

    subroutine assign_tile_values_real(input_field, ranked_output, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:), allocatable :: ranked_output
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. allocated(ranked_output)) allocate(ranked_output(vs%tile%dim_length))
        ranked_output = huge(ranked_output)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output = input_field%mapped_dat_tile
            class is (io_field_realNd)
                ranked_output = input_field%mapped_dat_tile
            class is (io_field_int)
                ranked_output = real(input_field%mapped_dat_tile)
            class is (io_field_intNd)
                ranked_output = real(input_field%mapped_dat_tile)
            class is (io_field_char)
                ranked_output = input_field%mapped_dat_tile
            class is (io_field_char1d)
                ranked_output = input_field%mapped_dat_tile
        end select

        !> Check for errors.
        if (all(ranked_output == huge(ranked_output))) error_status = 1

    end subroutine

    subroutine assign_tile_values_real_pntr(input_field, ranked_output_pntr, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:), pointer :: ranked_output_pntr
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. associated(ranked_output_pntr)) allocate(ranked_output_pntr(vs%tile%dim_length))
        ranked_output_pntr = huge(ranked_output_pntr)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output_pntr = input_field%mapped_dat_tile
            class is (io_field_realNd)
                ranked_output_pntr = input_field%mapped_dat_tile
            class is (io_field_int)
                ranked_output_pntr = real(input_field%mapped_dat_tile)
            class is (io_field_intNd)
                ranked_output_pntr = real(input_field%mapped_dat_tile)
            class is (io_field_char)
                ranked_output_pntr = input_field%mapped_dat_tile
            class is (io_field_char1d)
                ranked_output_pntr = input_field%mapped_dat_tile
        end select

        !> Check for errors.
        if (all(ranked_output_pntr == huge(ranked_output_pntr))) error_status = 1

    end subroutine

    subroutine assign_tile_values_int(input_field, ranked_output, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:), allocatable :: ranked_output
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. allocated(ranked_output)) allocate(ranked_output(vs%tile%dim_length))
        ranked_output = huge(ranked_output)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output = int(input_field%mapped_dat_tile)
            class is (io_field_realNd)
                ranked_output = int(input_field%mapped_dat_tile)
            class is (io_field_int)
                ranked_output = input_field%mapped_dat_tile
            class is (io_field_intNd)
                ranked_output = input_field%mapped_dat_tile
            class is (io_field_char)
                ranked_output = int(input_field%mapped_dat_tile)
            class is (io_field_char1d)
                ranked_output = int(input_field%mapped_dat_tile)
        end select

        !> Check for errors.
        if (all(ranked_output == huge(ranked_output))) error_status = 1

    end subroutine

    subroutine assign_tile_values_int_pntr(input_field, ranked_output_pntr, error_status)

        !> Modules.
        use model_variables, only: vs

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:), pointer :: ranked_output_pntr
        integer, intent(out) :: error_status

        !> Reset variable.
        if (.not. associated(ranked_output_pntr)) allocate(ranked_output_pntr(vs%tile%dim_length))
        ranked_output_pntr = huge(ranked_output_pntr)

        !> Assign 'cell' value from field.
        select type (input_field)
            class is (io_field_real)
                ranked_output_pntr = int(input_field%mapped_dat_tile)
            class is (io_field_realNd)
                ranked_output_pntr = int(input_field%mapped_dat_tile)
            class is (io_field_int)
                ranked_output_pntr = input_field%mapped_dat_tile
            class is (io_field_intNd)
                ranked_output_pntr = input_field%mapped_dat_tile
            class is (io_field_char)
                ranked_output_pntr = int(input_field%mapped_dat_tile)
            class is (io_field_char1d)
                ranked_output_pntr = int(input_field%mapped_dat_tile)
        end select

        !> Check for errors.
        if (all(ranked_output_pntr == huge(ranked_output_pntr))) error_status = 1

    end subroutine

    subroutine assign_variable_from_field(input_field, error_status)

        !> Modules.
        use strings, only: uppercase
        use variable_names
        use model_variables, only: vs

        !> Input/output variables.
        !*  input_field: Input field to assign to 'output_field'.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        class(io_field) input_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer time_order, isaved

        !> Status.
        error_status = 0

        !> Transfer the time order to a local variable (for diagnostic output).
        time_order = 0
        select type (input_field)
            class is (io_field_Nd)
                time_order = input_field%time_order
        end select

        !> Identify and assign variables.
        isaved = 1
        select case (uppercase(input_field%field_name))

            !> Basin attributes (general).
            case (VN_NEXT)
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%next_id, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%next_id, error_status)
            case (VN_GRIDAREA)
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%surface_area, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%surface_area, error_status)
            case (VN_ELEV)
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%topo_elev, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%topo_elev, error_status)
            case (VN_TOPOSLOPE, 'INTSLOPE')
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%topo_slope, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%topo_slope, error_status)

            !> Drainage/routing attributes.
            case (VN_CHNLSLOPE)
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%chnl_slope, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%chnl_slope, error_status)
            case (VN_CHNLLENGTH)
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%chnl_length, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%chnl_length, error_status)
            case (VN_ICHNL, 'CHNL')
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%ichnl, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%ichnl, error_status)
            case (VN_IREACH, 'REACH')
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%ireach, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%ireach, error_status)
            case (VN_DA)
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%drainage_area, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%drainage_area, error_status)
            case (VN_BNKFLL, 'BANKFULL')
                if (associated(vs%grid)) call assign_cell_values(input_field, vs%grid%bankfull, error_status)
                if (associated(vs%tile)) call assign_tile_values(input_field, vs%tile%bankfull, error_status)

            !> Meteorology/climatology variables.
            case (VN_FSIN)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%fsin, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%fsin, error_status)
            case (VN_FLIN)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%flin, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%flin, error_status)
            case (VN_TA)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%ta, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%ta, error_status)
            case (VN_QA)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%qa, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%qa, error_status)
            case (VN_PRES)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%pres, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%pres, error_status)
            case (VN_UU)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%uu, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%uu, error_status)
            case (VN_VV)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%vv, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%vv, error_status)
            case (VN_UV)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%uv, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%uv, error_status)
            case (VN_WDIR)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%wdir, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%wdir, error_status)
            case (VN_PRERN)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%prern, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%prern, error_status)
            case (VN_PRESNO)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%presno, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%presno, error_status)
            case (VN_PRE)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%pre, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%pre, error_status)

            !> Routing variables.
            case (VN_RFF)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%rff, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%rff, error_status)
            case (VN_RCHG)
                if (associated(vs%grid)) call assign_cell_values_pntr(input_field, vs%grid%rchg, error_status)
                if (associated(vs%tile)) call assign_tile_values_pntr(input_field, vs%tile%rchg, error_status)

            !> Others (not saved).
            case default
                isaved = 0
        end select

        !> Check 'isaved' status.
        if (isaved == 1 .and. .not. time_order > 0) then
            if (DIAGNOSEMODE) call print_info("Assigned the variable '" // trim(input_field%label) // "'.")
        end if

    end subroutine

    subroutine assign_variables_from_field_list(field_list, error_status)

        !> Input/output variables.
        !*  field_list: List of fields (read from file).
        !*  error_status: Status returned by the operation (optional; 0: normal).
        type(io_field_wrapper), dimension(:), intent(in) :: field_list
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Status.
        error_status = 0

        !> Loop through the fields to assign remaining variables.
        ierr = 0
        do i = 1, size(field_list)
            call assign_variable_from_field(field_list(i)%field, ierr)
            if (ierr /= 0) error_status = ierr
        end do

    end subroutine

    subroutine create_output_file(output_file, field_list, quiet, error_status)

        !> Input variables.
        !*  output_file: Output file definition ('io_file_info' structure).
        !*  field_list: Fields to be written to file (list of 'io_field_wrapper').
        !*  quiet: .true. to suppress block formatting (optional, default: .false.).
        type(io_file_info), intent(in) :: output_file
        type(io_field_wrapper), dimension(:), intent(in) :: field_list
        logical, intent(in), optional :: quiet

        !> Output variables.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        integer, intent(out), optional :: error_status

        !> Local variables.
        logical v, ltest

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Check for a valid path.
        if (len_trim(output_file%full_path) == 0) then
            if (v) call print_error("Missing file name in 'open_input_file'. Unable to open file.")
            error_status = 1
            return
        else

            !> Print a message (if not quiet).
            if (v) then
                call reset_tab()
                call print_message("SAVING: " // trim(output_file%full_path))
                call increase_tab()
            end if
        end if

        !> Check compatibility.
        ltest = .true.

    end subroutine

    subroutine append_output_frame(field_list, file_format, file_unit, quiet, error_status)

        !> Input variables.
        !*  field_list: Variables to save to file (list of 'io_field' types).
        !*  file_format: Key specifying file format (must be one from 'mesh_io_options').
        !*  file_unit: Unit associated with the opened file.
        !*  quiet: .true. to suppress block formatting (optional, default: .false.).
        type(io_field_wrapper), dimension(:), intent(in) :: field_list
        integer, intent(in) :: file_format
        integer, intent(out) :: file_unit
        logical, intent(in), optional :: quiet

        !> Output variables.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        integer, intent(out), optional :: error_status

        !> Local variables.
        logical v

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

    end subroutine

end module