module climate_forcing_variabletypes

    use model_dates

    implicit none

    type clim_info_series

        integer nattr
        character(20) attrtype
        character(200), dimension(:), allocatable :: attr

    end type

!-    type clim_info_transform_series

        !* nattr: Number of attributes in the transform.
        !* attrtype: Type of transform.
        !* attr: Attributes (e.g., coefficients) of the transform [-].
        !* tfs: Array for work or for the transformed series.
!-        integer :: nattr = 0
!-        integer attrtype
!-        real, dimension(:), allocatable :: attr
!-        real, dimension(:, :), allocatable :: tfs

!-    end type

    type clim_series

        !* id_var: Climate variable name and ID.
        !* factive: Returns .true. if the variable is active.
        !* ffmt: Input file format.
        !* fname: Input file name.
        !* fpath: Full path to the forcing input file, including extension.
        !* fiun: Input file unit.
        !* fopen: Returns .true. if an input file for the variable has been opened.
        character(200) :: id_var = ''
        logical :: factive = .false.
        integer :: ffmt = 1
        character(200) :: fname = ''
        character(200) :: fpath = ''
        integer fiun
        logical :: fopen = .false.

        !* name_lon: Name of the longitude (x) dimension when seeking the attribute from file (default: lon).
        !* name_lat: Name of the latitude (y) dimension when seeking the attribute from file (default: lat).
        !* name_time: Name of the time (t) dimension when seeking the attribute from file (default: time).
        !* ncol_lon: Position of the longitude (x) dimension in variable matrix (default: none).
        !* ncol_lat: Position of the latitude (y) dimension in variable matrix (default: none).
        !* ncol_time: Position of the time dimension in variable matrix (default: none).
        !* ndims: Number of dimensions (default: none).
        !* dim_order_case: Order of the dimensions in variable matrix,
        !*  case 1 = (lon, lat, time), 2 = (lat, lon, time), 3 = (lon, time, lat)
        !*  case 4 = (lat, time, lon), 5 = (time, lon, lat), 6 = (time, lat, lon)
        !* dim_length: Respective lengths of the dimensions (in dimension order) in variable matrix.
        !* time_shift: Time shift relative to apply to the dates of record (default: 0).
        !* vid: Variable ID (default: none).
        !* tid: Time ID (default: none).
        character(200) :: name_lon = 'lon'
        character(200) :: name_lat = 'lat'
        character(200) :: name_time = 'time'
        integer :: ncol_lon = 0
        integer :: ncol_lat = 0
        integer :: ncol_time = 0
!-        integer :: ndims = -1
!-        integer :: dim_order_case = 0
        integer, dimension(:), allocatable :: dim_length
        real :: time_shift = 0.0
        integer :: vid = -1
        integer :: tid = -1

        !* GRD: Values for forcing data (Bounds: 1: Grid).
        !>      Values are averaged to the grid-level for grid-based
        !>      processing and certain output. These gridded values are
        !>      not used to drive the model, as they are not compatible
        !>      with data at the GRU- or GAT-level.
        !* GRU: Values for forcing data (Bounds: 1: GRU).
        !* GAT: Values for forcing data (Bounds: 1: Land Element).
        real, dimension(:), pointer :: GRD
        real, dimension(:), pointer :: GRU
        real, dimension(:), pointer :: GAT

        !* nblocks: Number of frames of blocks of data to read into memory.
        !* blocktype: Type of data being stored (1 = GRD; 2 = GRU; 3 = GAT).
        !* blocks: Forcing data (Bounds: 1: Element; 2: nblocks).
        !* iblock: Index of the current block in data to memory [-].
        !* iskip: Number of frames to skip or that have been skipped in the run (default: 0).
        integer :: nblocks = 1
        integer :: blocktype = 1
        real, dimension(:, :), allocatable :: blocks
        integer :: iblock = 1
        integer :: iskip = 0

        !* cm: Multiplicative conversion factor where 'new_value = cm*old_value' (default: 1.0).
        !* ca: Additive conversion factor where 'new_value = old_value + ca' (default: 0.0).
        real :: cm = 1.0
        real :: ca = 0.0

        !* start_date: Starting date of the data in the file.
        !* n_skip_cols: Number of columns to skip (list-directed formats only).
        !* hf: Increment of minutes passed in each frame of data [mins].
        !* itimestep: Current time-step [mins].
        type(counter_date) :: start_date
        integer :: n_skip_cols = 0
        integer :: hf = 30
        integer :: itimestep = 0

        !* ipflg: INTERPOLATIONFLAG (0: none, 1: active).
        !* ipwgt: Interpolation type (1: arithmetic mean; 2: harmonic mean).
        !* ipdat: Array to store the states of the forcing data [-] (Bounds: 1: Element; 2: interpolation/previous time-step state).
        integer :: ipflg = 0
        integer :: ipwgt = 1
        real, dimension(:, :), allocatable :: ipdat

        !* nseries: Number of series in the definition.
        !* series: Definitions for the series.
        integer :: nseries = 0
        type(clim_info_series), dimension(:), allocatable :: series

    end type

    type clim_info

        integer :: basefileunit = 86

        !* nclim: Number of climate variables.
        !* start_date: Starting date of the data (general).
        !* dat: Climate variables.
        integer :: nclim = 13
!-        type(counter_date) :: start_date
        type(clim_series) :: dat(13)

    end type !clim_info

end module
