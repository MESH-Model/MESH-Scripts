!> Description:
!>  Contains variable types for common parameters of the model, including
!>  parameters for the river channel routing and land surface schemes.
module input_parameters

    implicit none

    !* INPUTPARAMSFORM: Determines how parameters are read from file.
    !>  Options:
    !>      - ini:  From CLASS.ini and Hydrology.ini (default).
    !>      - csv:  From CSV by GRU.
    !>      - r2c:  From r2c by grid.
    character(len = 80), save :: INPUTPARAMSFORM = ''
    integer, save :: INPUTPARAMSFORMFLAG = 1

    !> Description:
    !>  Container for parameters.
    type input_parameters_fields

        !* gc: Ground cover type. [--].
        !* fare: Active fraction of the grid cell. [--].
        !* xslp: Estimated average slope of the GRU. [--].
        !* mid: Mosaic type of the tile. [--].
        !* iwf: Overland and interflow routing flag. [--].
        real, dimension(:), allocatable :: gc
        real, dimension(:), allocatable :: fare
        real, dimension(:), allocatable :: xslp
        integer, dimension(:), allocatable :: mid
        integer, dimension(:), allocatable :: iwf
        integer, dimension(:), allocatable :: iabsp

        !* fcan: Annual maximum fraction of the grid-cell occupied by vegetation category or land cover. [--].
        !* z0or: Orographic roughness length. [m].
        !* lnz0: Natural logarithm of the roughness length of the vegetation category or land cover. [--].
        !* alvc: Average visible albedo of the vegetation category when fully-leafed or of the land cover. [--].
        !* alic: Average near-infrared albedo of the vegetation category when fully-leafed or of the land cover. [--].
        !* lamx: Annual maximum leaf-area index of the vegetation category. [--].
        !* lamn: Annual minimum leaf-area index of the vegetation category. [--].
        !* cmas: Annual maximum canopy mass of the vegetation category. [kg m-2].
        !* root: Annual maximum rooting depth of the vegetation category. [m].
        !* rsmn: Minimum stomatal resistance of the vegetation category. [s m-1].
        !* qa50: Reference value of shortwave radiation used in the calculation of the stomatal resistance of the vegetation category. [W m-2].
        !* vpda: Vapor pressure deficit coefficient 'A' used in the calculation of the stomatal resistance of the vegetation category. [--].
        !* vpdb: Vapor pressure deficit coefficient 'B' used in the calculation of the stomatal resistance of the vegetation category. [--].
        !* psga: Soil moisture suction coefficient 'A' used in the calculation of the stomatal resistance of the vegetation category. [--].
        !* psgb: Soil moisture suction coefficient 'B' used in the calculation of the stomatal resistance of the vegetation category. [--].
        real, dimension(:, :), allocatable :: fcan
        real, dimension(:, :), allocatable :: z0or
        real, dimension(:, :), allocatable :: lnz0
        real, dimension(:, :), allocatable :: alvc
        real, dimension(:, :), allocatable :: alic
        real, dimension(:, :), allocatable :: lamx
        real, dimension(:, :), allocatable :: lamn
        real, dimension(:, :), allocatable :: cmas
        real, dimension(:, :), allocatable :: root
        real, dimension(:, :), allocatable :: rsmn
        real, dimension(:, :), allocatable :: qa50
        real, dimension(:, :), allocatable :: vpda
        real, dimension(:, :), allocatable :: vpdb
        real, dimension(:, :), allocatable :: psga
        real, dimension(:, :), allocatable :: psgb

        !* zbld: Height into the atmosphere for aggregating surface roughness (usually in the order of 50-100 m). [m].
        !* zrfh: Reference height (measurement height) for temperature and humidity. [m].
        !* zrfm: Reference height (measurement height) for wind speed. [m].
        !* zplg: Maximum depth of liquid water allowed to be stored on the ground surface for snow-free areas. [m].
        real, dimension(:), allocatable :: zbld
        real, dimension(:), allocatable :: zrfh
        real, dimension(:), allocatable :: zrfm
        real, dimension(:), allocatable :: zplg

        !* zsnl: Minimum depth to consider 100% cover of snow on the ground surface. [m].
        !* zpls: Maximum depth of liquid water allowed to be stored on the ground surface for snow-covered areas. [m].
        real, dimension(:), allocatable :: zsnl
        real, dimension(:), allocatable :: zpls

        !* sand: Percent content of sand in the mineral soil. [%].
        !* clay: Percent content of clay in the mineral soil. [%].
        !* orgm: Percent content of organic matter in the mineral soil. [%].
        !* sdep: Permeable depth of the soil column. [m].
        !* delz: Layer thickness. [m].
        !* zbot: Depth to bottom of the column. [m].
        !* alwet: Reference all-wave albedo of wet soil for modelled area. [--].
        !* aldry: Reference all-wave albedo of dry soil for modelled area. [--].
        !* thpor: Pore volume. [m3 m-3].
        !* thlret: Liquid water retention capacity for organic soil. [m3 m-3].
        !* thlmin: Residual soil liquid water content remaining after freezing or evaporation. [m3 m-3].
        !* thlrat: Fractional saturation of soil at half the saturated hydraulic conductivity. [--].
        !* bi: Clapp and Hornberger empirical parameter. [--].
        !* psisat: Soil moisture suction at saturation. [m].
        !* psiwlt: Soil moisture suction at wilting point. [m].
        !* grksat: Hydraulic conductivity of soil at saturation. [m s-1].
        !* thfc: Field capacity. [m3 m-3].
        !* hcps: Volumetric heat capacity of soil matter. [J m-3 K-1].
        !* tcs: Thermal conductivity of soil. [W m-1 K-1].
        real, dimension(:), allocatable :: sdep
        real, dimension(:), allocatable :: alwet
        real, dimension(:), allocatable :: aldry
        real, dimension(:), allocatable :: delz
        real, dimension(:), allocatable :: zbot
        real, dimension(:, :), allocatable :: sand
        real, dimension(:, :), allocatable :: clay
        real, dimension(:, :), allocatable :: orgm
        real, dimension(:, :), allocatable :: thpor
        real, dimension(:, :), allocatable :: thlret
        real, dimension(:, :), allocatable :: thlmin
        real, dimension(:, :), allocatable :: thlrat
        real, dimension(:, :), allocatable :: bi
        real, dimension(:, :), allocatable :: psisat
        real, dimension(:, :), allocatable :: psiwlt
        real, dimension(:, :), allocatable :: grksat
        real, dimension(:, :), allocatable :: thfc
        real, dimension(:, :), allocatable :: hcps
        real, dimension(:, :), allocatable :: tcs

        !* drn: Drainage index, set to 1.0 to allow the soil physics to model drainage or to a value between 0.0 and 1.0 to impede drainage. [--].
        !* dd: Estimated drainage density of the GRU. [km km-2].
        !* grkf: Fraction of the saturated surface soil conductivity moving in the horizontal direction. [--].
        !* mann: Manning's n. [s m-1/3].
        !* ks: Saturated surface soil conductivity. [m s-1].
        real, dimension(:), allocatable :: drn
        real, dimension(:), allocatable :: dd
        real, dimension(:), allocatable :: grkf
        real, dimension(:), allocatable :: mann
        real, dimension(:), allocatable :: ks
    end type

    !> Description:
    !>  Container for a group of parameters.
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  gru: By GRU 1:NTYPE.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    type input_parameters_groups
        type(input_parameters_fields) :: tile, gru, grid
    end type

    !> Instance of SA_MESH parameter groups.
    type(input_parameters_groups), save :: pm

    contains

    !> Description:
    !>  Subroutine to reset/zero a group of parameters.
    !>
    !> Variables:
    !*  group: Group of parameters.
    !*  ierr: Returns status.
    subroutine input_parameters_group_reset(group, ierr)

        !> Input/output variables.
        type(input_parameters_fields) group

        !> Output variables.
        integer, intent(out) :: ierr
        
        !> Initialize the return status.
        ierr = 0

        if (allocated(group%gc)) group%gc = 0.0
        if (allocated(group%fare)) group%fare = 0.0
        if (allocated(group%xslp)) group%xslp = 0.0
        if (allocated(group%mid)) group%mid = 0
        if (allocated(group%iwf)) group%iwf = -1
        if (allocated(group%iabsp)) group%iabsp = 0
        if (allocated(group%fcan)) group%fcan = 0.0
        if (allocated(group%z0or)) group%z0or = 0.0
        if (allocated(group%lnz0)) group%lnz0 = 0.0
        if (allocated(group%alvc)) group%alvc = 0.0
        if (allocated(group%alic)) group%alic = 0.0
        if (allocated(group%lamx)) group%lamx = 0.0
        if (allocated(group%lamn)) group%lamn = 0.0
        if (allocated(group%cmas)) group%cmas = 0.0
        if (allocated(group%root)) group%root = 0.0
        if (allocated(group%rsmn)) group%rsmn = 0.0
        if (allocated(group%qa50)) group%qa50 = 0.0
        if (allocated(group%vpda)) group%vpda = 0.0
        if (allocated(group%vpdb)) group%vpdb = 0.0
        if (allocated(group%psga)) group%psga = 0.0
        if (allocated(group%psgb)) group%psgb = 0.0
        if (allocated(group%zbld)) group%zbld = 0.0
        if (allocated(group%zrfh)) group%zrfh = 0.0
        if (allocated(group%zrfm)) group%zrfm = 0.0
        if (allocated(group%zplg)) group%zplg = 0.0
        if (allocated(group%zsnl)) group%zsnl = 0.0
        if (allocated(group%zpls)) group%zpls = 0.0
        if (allocated(group%sdep)) group%sdep = 0.0
        if (allocated(group%alwet)) group%alwet = 0.0
        if (allocated(group%aldry)) group%aldry = 0.0
        if (allocated(group%delz)) group%delz = 0.0
        if (allocated(group%zbot)) group%zbot = 0.0
        if (allocated(group%sand)) group%sand = 0.0
        if (allocated(group%clay)) group%clay = 0.0
        if (allocated(group%orgm)) group%orgm = 0.0
        if (allocated(group%thpor)) group%thpor = 0.0
        if (allocated(group%thlret)) group%thlret = 0.0
        if (allocated(group%thlmin)) group%thlmin = 0.0
        if (allocated(group%thlrat)) group%thlrat = 0.0
        if (allocated(group%bi)) group%bi = 0.0
        if (allocated(group%psisat)) group%psisat = 0.0
        if (allocated(group%psiwlt)) group%psiwlt = 0.0
        if (allocated(group%grksat)) group%grksat = 0.0
        if (allocated(group%thfc)) group%thfc = 0.0
        if (allocated(group%hcps)) group%hcps = 0.0
        if (allocated(group%tcs)) group%tcs = 0.0
        if (allocated(group%drn)) group%drn = 0.0
        if (allocated(group%dd)) group%dd = 0.0
        if (allocated(group%grkf)) group%grkf = 0.0
        if (allocated(group%mann)) group%mann = 0.0
        if (allocated(group%ks)) group%ks = 0.0

    end subroutine

    !> Description:
    !>  Subroutine to reset/zero all parameters.
    !>
    !> Variables:
    !*  shd: Basin information.
    !*  ierr: Return status
    subroutine input_parameters_reset(shd, ierr)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Reset groups.
        if (ro%RUNTILE) then
            call input_parameters_group_reset(pm%tile, z); if (z /= 0) ierr = z
            call input_parameters_group_reset(pm%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call input_parameters_group_reset(pm%grid, z); if (z /= 0) ierr = z
        end if

    end subroutine

    !> Description:
    !>  Subroutine to allocate a group of parameters.
    !>
    !> Variables:
    !*  group: Group of parameters.
    !*  n: Index of elements (e.g., tiles, grids).
    !*  nsl: Number of layers.
    !*  ncan: Number of vegetated canopies.
    !*  ncan1: Number of canopies (including vegetated and non-vegetated canopies).
    !*  ierr: Return status
    subroutine input_parameters_group_allocate(group, n, nsl, ierr)

        !> Input variables.
        integer, intent(in) :: n, nsl

        !> Input/output variables.
        type(input_parameters_fields) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer, parameter :: ncan = 4, ncan1 = 5
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Allocate parameters.
        allocate(group%gc(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fare(n), stat = z); if (z /= 0) ierr = z
        allocate(group%xslp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%mid(n), stat = z); if (z /= 0) ierr = z
        allocate(group%iwf(n), stat = z); if (z /= 0) ierr = z
        allocate(group%iabsp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fcan(n, ncan1), stat = z); if (z /= 0) ierr = z
        allocate(group%z0or(n, ncan1), stat = z); if (z /= 0) ierr = z
        allocate(group%lnz0(n, ncan1), stat = z); if (z /= 0) ierr = z
        allocate(group%alvc(n, ncan1), stat = z); if (z /= 0) ierr = z
        allocate(group%alic(n, ncan1), stat = z); if (z /= 0) ierr = z
        allocate(group%lamx(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%lamn(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%cmas(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%root(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%rsmn(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%qa50(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%vpda(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%vpdb(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%psga(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%psgb(n, ncan), stat = z); if (z /= 0) ierr = z
        allocate(group%zbld(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zrfh(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zrfm(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zplg(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsnl(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zpls(n), stat = z); if (z /= 0) ierr = z
        allocate(group%sdep(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alwet(n), stat = z); if (z /= 0) ierr = z
        allocate(group%aldry(n), stat = z); if (z /= 0) ierr = z
        allocate(group%delz(nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%zbot(nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%sand(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%clay(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%orgm(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thpor(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thlret(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thlmin(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thlrat(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%bi(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%psisat(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%psiwlt(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%grksat(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thfc(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%hcps(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tcs(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%drn(n), stat = z); if (z /= 0) ierr = z
        allocate(group%dd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%grkf(n), stat = z); if (z /= 0) ierr = z
        allocate(group%mann(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ks(n), stat = z); if (z /= 0) ierr = z

    end subroutine

    !> Description:
    !>  Subroutine to allocate parameters.
    !>
    !> Variables:
    !*  shd: Basin information.
    !*  ierr: Return status
    subroutine input_parameters_init(shd, ierr)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Allocate and initialize groups.
        if (ro%RUNLSS) then
            call input_parameters_group_allocate(pm%tile, shd%lc%NML, shd%lc%IGND, z); if (z /= 0) ierr = z
            call input_parameters_group_reset(pm%tile, z); if (z /= 0) ierr = z
            call input_parameters_group_allocate(pm%gru, shd%lc%NTYPE, shd%lc%IGND, z); if (z /= 0) ierr = z
            call input_parameters_group_reset(pm%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call input_parameters_group_allocate(pm%grid, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            call input_parameters_group_reset(pm%grid, z); if (z /= 0) ierr = z
        end if

    end subroutine

end module
