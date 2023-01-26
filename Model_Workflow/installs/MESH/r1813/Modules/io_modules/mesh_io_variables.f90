module mesh_io_variables

    !> 'mesh_io_options': For I/O constants.
    use mesh_io_options

    implicit none

    !> Description:
    !>  Structure to store generic variable definition.
    !>
    !> Variables:
    !*  short_name: Short name (e.g., for labelling, variable and output file names).
    !*  description: Description.
    !*  units: Units.
    type io_variable_info
        character(len = SHORT_FIELD_LENGTH) :: short_name = ''
        character(len = LONG_FIELD_LENGTH) :: description = ''
        character(len = SHORT_FIELD_LENGTH) :: units = ''
    end type

    !> Description:
    !>  Generic structure for an instance of a variable.
    !>
    !> Variables:
    !*  label: Label or ID.
    !*  id: Variable ID (e.g., if from an indexed input file).
    !*  field_name: Field name derived from label or ID.
    !*  level: Level or category derived from label or ID.
    !*  level_id: Level ID for numeric level derived from label or ID.
    !*  mapped_name: Name if mapped to an internal variable.
    !*  mapped_dim_order: Pre-derived mapping to spatial orders.
    !*  dat: Data frame.
    !*  dim_names: Names of the dimensions of dimensional fields.
    !*  time_order: Index of the time dimension for multi-dimensional fields.
    !*  cell_map: Pre-derived mapping of the multi-dimensional field to 'cell' field.
    !*  tile_map: Pre-derived mapping of the multi-dimensional field to 'tile' field.
    !*  mapped_dat_cell: Mapped values by 'cell_map'.
    !*  mapped_dat_tile: Mapped values by 'tile_map'.
    !*  mapped_dat_cell_interp: Previous mapped 'cell' values used for temporal interpolation.
    !*  mapped_dat_tile_interp: Previous mapped 'tile' values used for temporal interpolation.
    !*  const_mul: Multiplicative factor (applied for input fields, default: 1.0).
    !*  const_add: Additive factor (applied for input fields, default: 0.0).
    !*  no_data_value: No data value when used in an array (default: huge).
    !*  valid_max: Upper limit of reasonable values for numeric fields (default: huge).
    !*  valid_min: Lower limit of reasonable values for numeric fields (default: tiny).
    type, extends(io_variable_info), abstract :: io_field
        character(len = SHORT_FIELD_LENGTH) :: label = ''
        integer :: id = -1
        character(len = SHORT_FIELD_LENGTH) :: field_name = ''
        character(len = SHORT_FIELD_LENGTH) :: level = ''
        integer :: level_id = -1
        character(len = SHORT_FIELD_LENGTH) :: mapped_name = ''
        integer, dimension(:), allocatable :: mapped_dim_order
    end type

    !> Extensions.
    type, abstract, extends(io_field) :: io_field_Nd
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_names
        integer :: time_order = 0
        integer, dimension(:, :), allocatable :: cell_map
        integer, dimension(:, :), allocatable :: tile_map
    end type
    type, extends(io_field) :: io_field_real
        real, dimension(:), allocatable :: mapped_dat_cell
        real, dimension(:), allocatable :: mapped_dat_tile
        real :: dat = huge(0.0)
        real, dimension(:, :), allocatable :: mapped_dat_cell_interp
        real, dimension(:, :), allocatable :: mapped_dat_tile_interp
    end type
    type, extends(io_field_Nd) :: io_field_realNd
        real, dimension(:), allocatable :: mapped_dat_cell
        real, dimension(:), allocatable :: mapped_dat_tile
        real, dimension(:, :), allocatable :: mapped_dat_cell_interp
        real, dimension(:, :), allocatable :: mapped_dat_tile_interp
        real :: const_mul = 1.0
        real :: const_add = 0.0
        real :: no_data_value = huge(0.0)
        real :: valid_max = huge(0.0)
        real :: valid_min = tiny(0.0)
    end type
    type, extends(io_field_realNd) :: io_field_real1d
        real, dimension(:), allocatable :: dat
!        contains
!        final :: io_field_real1d_destructor
    end type
    type, extends(io_field_realNd) :: io_field_real2d
        real, dimension(:, :), allocatable :: dat
!        contains
!        final :: io_field_real2d_destructor
    end type
    type, extends(io_field_realNd) :: io_field_real3d
        real, dimension(:, :, :), allocatable :: dat
!        contains
!        final :: io_field_real3d_destructor
    end type
    type, extends(io_field_realNd) :: io_field_real4d
        real, dimension(:, :, :, :), allocatable :: dat
!        contains
!        final :: io_field_real4d_destructor
    end type
    type, extends(io_field_realNd) :: io_field_real5d
        real, dimension(:, :, :, :, :), allocatable :: dat
!        contains
!        final :: io_field_real5d_destructor
    end type
    type, extends(io_field) :: io_field_int
        integer, dimension(:), allocatable :: mapped_dat_cell
        integer, dimension(:), allocatable :: mapped_dat_tile
        integer :: dat = huge(0)
    end  type
    type, extends(io_field_Nd) :: io_field_intNd
        integer, dimension(:), allocatable :: mapped_dat_cell
        integer, dimension(:), allocatable :: mapped_dat_tile
        integer :: no_data_value = huge(0)
        integer :: valid_max = huge(0)
        integer :: valid_min = 0
    end type
    type, extends(io_field_intNd) :: io_field_int1d
        integer, dimension(:), allocatable :: dat
!        contains
!        final :: io_field_int1d_destructor
    end type
    type, extends(io_field_intNd) :: io_field_int2d
        integer, dimension(:, :), allocatable :: dat
!        contains
!        final :: io_field_int2d_destructor
    end type
    type, extends(io_field_intNd) :: io_field_int3d
        integer, dimension(:, :, :), allocatable :: dat
!        contains
!        final :: io_field_int3d_destructor
    end type
    type, extends(io_field_intNd) :: io_field_int4d
        integer, dimension(:, :, :, :), allocatable :: dat
!        contains
!        final :: io_field_int4d_destructor
    end type
    type, extends(io_field_intNd) :: io_field_int5d
        integer, dimension(:, :, :, :, :), allocatable :: dat
!        contains
!        final :: io_field_int5d_destructor
    end type
    type, extends(io_field) :: io_field_char
        real, dimension(:), allocatable :: mapped_dat_cell
        real, dimension(:), allocatable :: mapped_dat_tile
        character(len = SHORT_FIELD_LENGTH) :: dat = ''
    end type
    type, extends(io_field_Nd) :: io_field_char1d
        real, dimension(:), allocatable :: mapped_dat_cell
        real, dimension(:), allocatable :: mapped_dat_tile
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dat
        character :: no_data_value = achar(0)
!        contains
!        final :: io_field_char1d_destructor
    end type

    !> Description:
    !>  Wrapper for variables that extend 'io_field'.
    type io_field_wrapper
        class(io_field), allocatable :: field
    end type

    !> Destructors.
    private &
        io_field_real1d_destructor, io_field_int1d_destructor, io_field_char1d_destructor, &
        io_field_real2d_destructor, io_field_int2d_destructor, &
        io_field_real3d_destructor, io_field_int3d_destructor, &
        io_field_real4d_destructor, io_field_int4d_destructor, &
        io_field_real5d_destructor, io_field_int5d_destructor

    !> Description:
    !>  Structure for storing date components.
    !>
    !> Variables:
    !*  year: Standard four-digit year.
    !*  month: Month of year (1-12).
    !*  day: Day of month.
    !*  jday: Day of year.
    !*  hour: Hour of day (0-23).
    !*  minutes: Minutes in hour (0-59).
    type io_date
        integer :: year = 0
        integer :: month = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: hour = 0
        integer :: minutes = 0
    end type

    !> Description:
    !>  Structure to store file information.
    !>
    !> Variables:
    !*  full_path: Full file path, including extension.
    !*  ext: Extension type (if cannot be determined from 'io_file' extension, default: unknown).
    !*  iunit: File handle/unit.
    type io_file_info
        character(len = LONG_FIELD_LENGTH) :: full_path = ''
        integer :: ext = FILE_TYPE_NUL
        integer :: iunit = -1
    end type

    !> Description:
    !>  Structure to store file information and fields.
    !>
    !> Variables:
    !*  label: Unique file label or ID.
    !*  subset_ids: Active subset of indices to print for output files (default: no subset).
    !*  multi_frame: .true. if a multi-frame file (default: .false.).
    !*  freq: Frame frequency type for multi-frame files (default: unknown type).
    !*  freq_interval: Frequency interval in units of 'freq' for multi-frame files (default: 1).
    !*  istep: Current step in 'freq_interval' (start: 1).
    !*  start: Date associated with the first frame in a multi-frame file ('io_date' type).
    !*  time_offset: Offset to apply in calculating 'start' in hours (default: 0.0).
    !*  block_interval: Number of blocks to read at-a-time for multi-frame files (default: 1).
    !*  iblock: Current block in 'block_interval' (start: 1).
    !*  temporal_interpolation: Flag used for temporal interpolation of the field.
    !*  interp_weights: Weights used for interpolation.
    !*  ipos: Current I/O position in file for multi-frame file (start: 1).
    !*  fields: List of fields in the file.
    !*  field_map: Map of file field name to internal field name (in list, 1: field name; 2: internal name).
    type, extends(io_file_info) :: io_file
        character(len = SHORT_FIELD_LENGTH) :: label = ''
        integer, dimension(:), allocatable :: subset_ids
        logical :: multi_frame = .false.
        integer :: freq = FREQ_NUL
        integer :: freq_interval = 0
        integer :: istep = 1
        type(io_date) :: start = io_date()
        real :: time_offset = 0.0
        integer :: block_interval = 1
        integer :: iblock = 1
        integer :: temporal_interpolation = 0
        real, dimension(:), allocatable :: interp_weights
        integer :: ipos = 1
        type(io_field_wrapper), dimension(:), allocatable :: fields
        character(len = SHORT_FIELD_LENGTH), dimension(:, :), allocatable :: field_map
!        contains
!        final :: io_file_destructor
    end type

    !> Description:
    !>  Structure for space- or comma-separated file.
    !>
    !> Variables:
    !*  n_skip_rows: Number of rows to skip (default: 0).
    !*  n_skip_cols: Number of columns to skip (default: 0).
    !*  dim_names: Names of dimensions of fields in the file.
    !*  delimiter: Delimiter (default: ' ').
    type, extends(io_file) :: io_file_txt_delimited
        integer :: n_skip_rows = 0
        integer :: n_skip_cols = 0
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_names
        character :: delimiter = ' '
    end type

    !> Description:
    !>  Structure for EnSim Hydrologic/Green Kenue r2c file.
    !>
    !> Variables:
    !*  binary: .true. if data is stored in binary format.
    type, extends(io_file) :: io_file_r2c
        logical :: binary = .false.
    end type

    !> Description:
    !>  Structure for sequential binary format file.
    !>
    !> Variables:
    !*  dim_names: Names of dimensions of fields in the file.
    type, extends(io_file) :: io_file_seq
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_names
    end type

    !> Description:
    !>  Structure for NetCDF file.
    !>
    !> Variables:
    !*  flatten_outputs: .true. to flatten outputs (default: .false.).
    !*      Flattened outputs compact dimensions, e.g.:
    !*      SAND_1(y, x) instead of SAND(y, x, s).
    !*  dim_name_x: Override for 'x' dimension.
    !*  dim_name_y: Override for 'y' dimension.
    !*  dim_name_t: Override for 't' dimension.
    !*  dim_name_m: Override for 'm' dimension.
    !*  dim_name_s: Override for 's' dimension.
    !*  dim_name_c: Override for 'c' dimension.
    !*  dim_name_g: Override for 'g' dimension.
    !*  const_mul: Multiplicative factor applied uniformly to fields in the file (default: 1.0).
    !*  const_add: Additive factor applied uniformly to fields in the file (default: 0.0).
    type, extends(io_file) :: io_file_nc
        logical :: flatten_output = .false.
        character(len = SHORT_FIELD_LENGTH) :: dim_name_x = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_y = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_t = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_m = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_s = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_c = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_g = ''
        real :: const_mul = 1.0
        real :: const_add = 0.0
    end type

    !> Description:
    !>  Structure for CLASS MET format file.
    !>
    !> Variables:
    !*  rr_sr: .true. to read extra columns from the file for PRERN/PRESNO.
    type, extends(io_file) :: io_file_met
        logical :: rr_sr = .false.
    end type

    !> Description:
    !>  Wrapper for variables that extend 'io_file'.
    type io_file_wrapper
        class(io_file), allocatable :: file
    end type

    !> Destructors.
    private io_file_destructor

    contains

    subroutine io_field_real1d_destructor(this)

        !> Input/output variables.
        type(io_field_real1d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_real2d_destructor(this)

        !> Input/output variables.
        type(io_field_real2d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_real3d_destructor(this)

        !> Input/output variables.
        type(io_field_real3d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_real4d_destructor(this)

        !> Input/output variables.
        type(io_field_real4d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_real5d_destructor(this)

        !> Input/output variables.
        type(io_field_real5d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_int1d_destructor(this)

        !> Input/output variables.
        type(io_field_int1d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_int2d_destructor(this)

        !> Input/output variables.
        type(io_field_int2d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_int3d_destructor(this)

        !> Input/output variables.
        type(io_field_int3d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_int4d_destructor(this)

        !> Input/output variables.
        type(io_field_int4d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_int5d_destructor(this)

        !> Input/output variables.
        type(io_field_int5d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_field_char1d_destructor(this)

        !> Input/output variables.
        type(io_field_char1d) this

        !> Deallocate array.
        if (allocated(this%dim_names)) deallocate(this%dim_names)
        if (allocated(this%dat)) deallocate(this%dat)

    end subroutine

    subroutine io_file_destructor(this)

        !> Input/output variables.
        type(io_file) this

        !> Local variables.
        integer i

        !> Deallocate field objects.
        if (allocated(this%subset_ids)) deallocate(this%subset_ids)
        if (allocated(this%field_map)) deallocate(this%field_map)
!-        if (allocated(this%fields)) then
!-            do i = 1, size(this%fields)
!-                if (allocated(this%fields(i)%field)) deallocate(this%fields(i)%field)
!-            end do
!-            deallocate(this%fields)
!-        end if

    end subroutine

end module
