module projection_variables

    use mesh_io_options

    implicit none

    type projection_variable_fields

        !> Generic attributes.
        character(len = LONG_FIELD_LENGTH) :: projection = ''
        character(len = SHORT_FIELD_LENGTH) :: ellipsoid = ''

        !> Grid-based specification (e.g., EnSim Hydrologic/GreenKenue rectangular cell 'r2c').
        real :: llc_x = 0.0
        real :: llc_y = 0.0
        real :: dx = 0.0
        real :: dy = 0.0
        real :: nominal_side_length = 0.0

        !> Latitude/longitude (NetCDF).
        real :: semi_major_axis = 0.0
        real :: inverse_flattening = 0.0

        !> UTM.
        character(len = SHORT_FIELD_LENGTH) :: zone = ''

        !> EnSim Hydrologic/GreenKenue rotated latitude/longitude.
        real :: centre_latitude = 0.0
        real :: centre_longitude = 0.0
        real :: rotation_latitude = 0.0
        real :: rotation_longitude = 0.0

        !> Rotated latitude/longitude (NetCDF).
        real :: earth_radius = 0.0
        real :: grid_north_pole_latitude = 0.0
        real :: grid_north_pole_longitude = 0.0

        !> Coordinates (latitude/longitude).
        real, dimension(:), allocatable :: lon
        real, dimension(:), allocatable :: lat

        !> Coordinates (projected).
        real, dimension(:, :), allocatable :: lon_xy
        real, dimension(:, :), allocatable :: lat_xy
    end type

    type(projection_variable_fields), save :: pj

end module
