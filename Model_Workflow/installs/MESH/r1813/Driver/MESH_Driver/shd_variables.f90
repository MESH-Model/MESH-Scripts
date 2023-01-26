!>
!> Module for storing shd/basin variable types.
!>
module shd_variables

    implicit none

    type CoordSysParams

        !* Proj: Projection (formerly CoordSys). [-]
        character(100) :: Proj = ''

        !* Ellips: Ellipsoid (formerly Datum). [-]
        character(10) :: Ellips = ''

        !* Zone: Zone if the projection is UTM. [-]
        character(10) :: Zone = ''

        !> Attributes for 'LATLONG', and 'latitude_longitude' projection.
        real, dimension(:), allocatable :: lat
        real, dimension(:), allocatable :: lon

        !> Attributes for 'ROTLATLONG' projection (definitions from the 'rpnpy' code).
        !> Pole coordinate: Where (rlat, rlon) = (0.0, 180.0) on the rotated equator.
        !*  CentreLatitude: Standard file (fstd) format 'xlat1' (latitude of the pole coordinate). [degrees].
        !*  CentreLongitude: Standard file (fstd) format 'xlon1' (longitude of of the pole coordinate). [degrees].
        real :: CentreLatitude = 0.0
        real :: CentreLongitude = 0.0
        !> Rotation: Reference point on the rotated equator east of (xlat1, xlon1), which defines the rotation.
        !*  RotationLatitude: Standard file (fstd) format 'xlat2' (latitude of the reference point on the rotated equator). [degrees].
        !*  RotationLongitude: Standard file (fstd) format 'xlon2' (longitue of the reference point on the rotated equator). [degrees].
        real :: RotationLatitude = 0.0
        real :: RotationLongitude = 0.0

        !> Attributes for 'rotated_latitude_longitude' (not compatible with 'ROTLATLONG').
        real :: earth_radius = 0.0
        real :: grid_north_pole_latitude = 0.0
        real :: grid_north_pole_longitude = 0.0
        real, dimension(:), allocatable :: rlat
        real, dimension(:), allocatable :: rlon
        real, dimension(:, :), allocatable :: xylat
        real, dimension(:, :), allocatable :: xylon

    end type

    type ContribElemsParams

        !* ILG: Total potential number of contributing elements in the system. [-]
        integer :: ILG = 0

        !* NTYPE: Number of sub-grid tiles. [-]
        integer :: NTYPE = 0

        !* NML: Number of active contributing elements in the system. [-]
        integer :: NML = 0

        !* ACLASS: Weighted contribution of the tiles to the grid. [-]
        real, dimension(:, :), allocatable :: ACLASS

        !* ILMOS: Index of the order of the grid in NML. [-]
        integer, dimension(:), allocatable :: ILMOS

        !* JLMOS: Index of the order of the sub-grid tile in NML. [-]
        integer, dimension(:), allocatable :: JLMOS

    end type

    type SoilLayer

        !* DELZ: Depth of the soil layer. [m]
        real, dimension(:), allocatable :: DELZ

        !* ZBOT: Depth at the bottom of the soil layer to the top of the column. [m]
        real, dimension(:), allocatable :: ZBOT

    end type

    !> This type extends the ContribElemsParams type so it includes
    !> all the same parameters as listed in the ContribElemsParams
    !> type as well as those listed below.
    type, extends(ContribElemsParams) :: LandElemsParams

        !* IGND: Number of soil layers. [-]
        integer :: IGND = 0

        !* sl: Soil layers.
        type(SoilLayer) :: sl

    end type

    type GridParams

        type(CoordSysParams) :: CoordSys

        !* NA: Total number of grids in the basin. [-]
        integer :: NA = 0

        !* NAA: Total number of contributing grids in the basin. [-]
        integer :: NAA = 0

        !> DebugGridNo: The ID of a cell (i.e., 'RANK') to use for specific diagnostic outputs. [--].
        integer :: DebugGridNo = 0

        real :: &
            xOrigin = 0.0, &
            yOrigin = 0.0

        real :: AL = 0.0

        real :: &
            GRDN = 0.0, &
            GRDE = 0.0

        integer :: &
            xCount = 0, &
            yCount = 0

        real :: &
            xDelta = 0.0, &
            yDelta = 0.0

        !* xxx: Column of the grid in the x-direction. [-]
        integer, dimension(:), allocatable :: xxx

        !* yyy: Row of the grid in the y-direction. [-]
        integer, dimension(:), allocatable :: yyy

        !> Minimum and maximum extent of the grid in minutes or kms,
        !> depending on the projection of the coordinate system.
        integer :: iyMax = 0
        integer :: iyMin = 0
        integer :: jxMax = 0
        integer :: jxMin = 0

        !* ylat: Latitude of the grid in the y-direction. [-]
        real, dimension(:), allocatable :: ylat

        !* xlng: Longitude of the grid in the x-direction. [-]
        real, dimension(:), allocatable :: xlng

        integer, dimension(:, :), allocatable :: RNKGRD

    end type

    !> This type extends GridParams so it includes all the same
    !> parameters listed in the GridParams type as well as those
    !> listed below.
    type, extends(GridParams) :: LandGridParams

        !> LU: Information about the contributing land units.
        type(LandElemsParams) :: lc

        !* ELEV: Elevation of the grid. [m]
        real, dimension(:), allocatable :: ELEV

        !* SLOPE_INT: Internal slope (may exist in other files as 'intslope' or 'sl1'). [m m^-1]
        real, dimension(:), allocatable :: SLOPE_INT

        !* AREA: Area of the grid. [m^2]
        real, dimension(:), allocatable :: AREA

    end type

    !> This type extends LandGridParams so it includes all the same
    !> parameters listed in the LandGridParams type as well as
    !> those listed below.
    type, extends(LandGridParams) :: ShedGridParams

        !* NRVR: Number of river classes. [-]
        integer :: NRVR = 0

        !* wc: Information about contributing water bodies.
        type(ContribElemsParams) :: wc

        !> NEXT: Rank of the grid that is immediately downstream of this grid. [-]
        integer, dimension(:), allocatable :: NEXT

        !* DA: Drainage area [m^2]
        real, dimension(:), allocatable :: DA

        !* DRDN: Drainage density [m m^-2]
        real, dimension(:), allocatable :: DRDN

        !* FRAC:
        real, dimension(:), allocatable :: FRAC

        !* SLOPE_CHNL: Channel slope
        real, dimension(:), allocatable :: SLOPE_CHNL

        !* CHNL_LEN: Channel length (may exist in other files as 'chnllength', 'rl', or 'ch_length')
        real, dimension(:), allocatable :: CHNL_LEN

        !* BNKFLL:
        real, dimension(:), allocatable :: BNKFLL

        !* IAK:
        integer, dimension(:), allocatable :: IAK

        !* IROUGH:
        integer, dimension(:), allocatable :: IROUGH

        !* ICHNL:
        integer, dimension(:), allocatable :: ICHNL

        !* IREACH:
        integer, dimension(:), allocatable :: IREACH

    end type

    !* SHDFILEFLAG: Configuration options for reading the drainage database.
    character(len = 100), save :: SHDFILEFLAG = ''

    !* SHDFFMT: Drainage database file format (1: r2c; 2: class_ini; default: 1).
    integer, save :: SHDFILEFMT = 1

    !* SHDTOMAPFLAG: Option to create a WATFLOOD 'map' file from the drainage database.
    logical, save  :: SHDTOMAPFLAG = .false.

end module
