!> Description:
!>  Contains variable types for model variables, such
!>  as components of the water and energy balances, streamflow channels,
!>  and reservoirs.
module model_variables

    use mesh_io_options
    use mesh_io_variables, only: io_file_info

    implicit none

    !* SAVE/RESUMEFLAG: Saves or resume states from file.
    !>  Legacy options:
    !>      - 0: Disabled (new option: none).
    !>      - 1: Not supported.
    !>      - 2: Not supported.
    !>      - 3: CLASS prognostic states in binary sequential format (new option: seq only class).
    !>      - 4: All resume variables in binary sequential format (new option: seq).
    !>      - 5: All prognostic states in binary sequential format (new option: seq only states).
    !>  Options:
    !>      - FLAG_OFF: Do not save or resume the run state to and from file (default).
    !>      - FLAG_ON: Save and resume run state to and from file.
    !>      - FLAG_AUTO: Automatically resume the run state in the presence of auto_resume.ini (RESUMEFLAG only).
    !>  File format options (enables SAVERESUMEFLAG):
    !>      - FILE_TYPE_SEQ: Sequential binary format.
    !>  Output frequency options (default is only at the end of the run):
    !>      - FREQ_MLY: Before the beginning of the next month.
    !>      - FREQ_YLY: Before the beginning of the next year.
    character(len = LONG_FIELD_LENGTH), save :: RESUMEFLAG = 'none'
    character(len = LONG_FIELD_LENGTH), save :: SAVERESUMEFLAG = 'none'

    !> Type: io_state_flag
    !>
    !> Variables:
    !*  active: .true. if active.
    !*  freq: Frequency for I/O functions that are repeated.
    !*  bin: Read/write directives.
    !*  flo: File properties.
    type io_state_flag
        integer :: state = FLAG_OFF
        integer :: freq = FREQ_NUL
        character(len = LONG_FIELD_LENGTH) :: bin = ''
        type(io_file_info) flo
    end type

    !> Type: io_state_flags
    !>  Container for types of 'io_state_flag'.
    type io_state_flags
        type(io_state_flag) save, resume, assim
    end type

    !> Description:
    !>  Container for variables.
    type model_variables_fields

        !> Meteorology/climatology variables.
        !* fsin: Incoming shortwave radiation at the surface. [W m**-2].
        !* fsvs: Visible component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsir: Infrared component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsdr: Direct component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsdff: Diffuse component of incoming shortwave radiation at the surface. [W m**-2].
        !* flin: Incoming longwave radiation at the surface. [W m**-2].
        !* ta: Air temperature (at user-specified reference height). [K].
        !* qa: Specific humidity (at user-specificed reference height). [kg kg**-1].
        !* pres: Air pressure at the surface. [Pa].
        !* uu: U-component of wind speed (at user-specified reference height). [m s**-1].
        !* vv: V-component of wind speed (at user-specified reference height). [m s**-1].
        !* uv: Wind speed (at user-specified reference height). [m s**-1].
        !* wdir: Wind direction (at user-specified referenced height). [degrees].
        !* prern: Total incoming liquid precipitation rate. [kg m**-2 s**-1].
        !* presno: Total incoming solid precipitation rate. [kg m**-2 s**-1].
        !* pre: Total incoming precipitation rate. [kg m**-2 s**-1].
        real, dimension(:), pointer :: fsin => null()
        real, dimension(:), pointer :: fsvs => null()
        real, dimension(:), pointer :: fsir => null()
        real, dimension(:), pointer :: fsdr => null()
        real, dimension(:), pointer :: fsdff => null()
        real, dimension(:), pointer :: flin => null()
        real, dimension(:), pointer :: ta => null()
        real, dimension(:), pointer :: qa => null()
        real, dimension(:), pointer :: pres => null()
        real, dimension(:), pointer :: uu => null()
        real, dimension(:), pointer :: vv => null()
        real, dimension(:), pointer :: uv => null()
        real, dimension(:), pointer :: wdir => null()
        real, dimension(:), pointer :: prern => null()
        real, dimension(:), pointer :: presno => null()
        real, dimension(:), pointer :: pre => null()

        !> Canopy variables.
        !* lqwscan: Liquid water interception in the canopy. [kg m**-2].
        !* fzwscan: Frozen water interception in the canopy. [kg m**-2].
        !* cmas: Organic mass of the canopy. [kg m**-2].
        !* tacan: Air temperature in the canopy. [K].
        !* qacan: Specific humidity of air in the canopy. [kg kg**-1].
        !* tcan: Vegetation canopy temperature. [K].
        !* gro: Vegetation growth index. [--].
        real, dimension(:), pointer :: lqwscan => null()
        real, dimension(:), pointer :: fzwscan => null()
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tacan => null()
        real, dimension(:), pointer :: qacan => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: gro => null()

        !> Snow variables.
        !* fsno: Fraction of fully snow covered area. [fraction].
        !* sno: Snow mass. [kg m**-2].
        !* rhosno: Snow density. [kg m**-3].
        !* zsno: Snow depth. [m].
        !* lqwssno: Liquid water content of the snow. [kg m**-2].
        !* tsno: Snowpack temperature. [K].
        !* albsno: Snow albedo. [fraction].
        !* drainsno: Drainage from the bottom of the snowpack (runoff rate). [kg m**-2 s**-1].
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: rhosno => null()
!-        real, dimension(:), pointer :: zsno => null()
        real, dimension(:), pointer :: lqwssno => null()
        real, dimension(:), pointer :: tsno => null()
        real, dimension(:), pointer :: albsno => null()
        real, dimension(:), pointer :: drainsno => null()

        !> Surface variables.
        !* albt: Total albedo of the surface (visible and near-infrared). [fraction].
        !* alvs: Visible component of the total albedo of the surface. [fraction].
        !* alir: Near-infrared components of the total albedo of the surface. [fraction].
        !* gte: Effective black-body temperature at the surface. [K].
        !* zpnd: Depth of ponded water. [m].
        !* lqwspnd: Liquid water storage of ponded water. [kg m**-2].
        !* tpnd: Temperature of ponded water. [K].
        !* pndcaf: Contributing fraction of ponded water (PDMROF). [fraction].
        !* potevp: Potential evaporation rate. [kg m**-2 s**-1].
        !* et: Evapotranspiration rate. [kg m**-2 s**-1].
        !* evpb: Evaporation efficiency (ET to POTEVP) of the canopy. [--].
        !* arrd: Arridity index (PRE to POTEVP). [--].
        !* ovrflw: Overland runoff rate. [kg m**-2 s**-1].
        !* qevp: Latent heat flux at the surface. [W m**-2].
        !* qsens: Sensible heat flux at the surface. [W m**-2].
        !* gzero: Heat flux into the ground. [W m**-2].
        !* tsfs: Ground surface temperature over subarea. [K].
        !* tsurf: Surface temperature. [K].
        real, dimension(:), pointer :: albt => null()
        real, dimension(:), pointer :: alvs => null()
        real, dimension(:), pointer :: alir => null()
        real, dimension(:), pointer :: gte => null()
        real, dimension(:), pointer :: zpnd => null()
!-        real, dimension(:), pointer :: lqwspnd => null()
        real, dimension(:), pointer :: tpnd => null()
        real, dimension(:), pointer :: pndcaf => null()
        real, dimension(:), pointer :: potevp => null()
        real, dimension(:), pointer :: et => null()
!-        real, dimension(:), pointer :: evpb => null()
!-        real, dimension(:), pointer :: arrd => null()
        real, dimension(:), pointer :: ovrflw => null()
        real, dimension(:), pointer :: qevp => null()
        real, dimension(:), pointer :: qsens => null()
        real, dimension(:), pointer :: gzero => null()
        real, dimension(:, :), pointer :: tsfs => null()
        real, dimension(:), pointer :: tsurf => null()

        !> Ice/glacier variables.
        !* lqwsice: Liquid water storage of ice. [kg m**-2].
        !* tice: Temperature of ice. [K].
        real, dimension(:), pointer :: lqwsice => null()
        real, dimension(:), pointer :: tice => null()

        !> Subsurface/soil variables.
        !* dzsol: Thickness of the soil layer. [m].
        !* dzsolhyd: Permeable thickness of the soil layer. [m].
        !* thlqsol: Volumetric liquid water content of the soil. [m3 m**-3].
        !* thicsol: Volumetric frozen water content of the soil. [m3 m**-3].
        !* lqwssol: Liquid water storage in the soil. [kg m**-2].
        !* fzwssol: Frozen water storage in the soil. [kg m**-2].
        !* tsol: Temperature of the soil. [K].
        !* gflx: Heat conduction between soil layers. [W m**-2].
        !* latflw: Interflow runoff rate. [kg m**-2 s**-1].
        !* zsol: Depth to the bottom of the soil column. [m].
        !* zsolhyd: Permeable depth of the soil layer. [m].
        !* zsolsat: Depth to the first saturated layer in the soil column (presumed water table). [m].
        !* ggeo: Geothermal heat flux. [W m**-2].
        !* tbas: Temperature of bedrock in third soil layer. [K].
        !* drainsol: Drainage from the bottom of the permeable soil column (runoff rate). [kg m**-2 s**-1].
        real, dimension(:, :), pointer :: dzsol => null()
        real, dimension(:, :), pointer :: dzsolhyd => null()
        real, dimension(:, :), pointer :: thlqsol => null()
        real, dimension(:, :), pointer :: thicsol => null()
!-        real, dimension(:, :), pointer :: lqwssol => null()
!-        real, dimension(:, :), pointer :: fzwssol => null()
        real, dimension(:, :), pointer :: tsol => null()
        real, dimension(:, :), pointer :: gflx => null()
        real, dimension(:, :), pointer :: latflw => null()
        real, dimension(:), pointer :: zsol => null()
        real, dimension(:), pointer :: zsolhyd => null()
        real, dimension(:), pointer :: zsolsat => null()
        real, dimension(:), pointer :: ggeo => null()
        real, dimension(:), pointer :: tbas => null()
        real, dimension(:), pointer :: drainsol => null()

        !> Groundwater/lower zone storage variables.
        !* rchg: Drainage into groundwater/lower zone storage. [mm].
        !* stggw: Groundwater/lower zone storage. [mm].
        !* lkg: Leakage from groundwater/lower zone storage. [mm].
        !* dzs: Deep aquifer water storage. [mm].
        real, dimension(:), pointer :: rchg => null()
        real, dimension(:), pointer :: stggw => null()
        real, dimension(:), pointer :: lkg => null()
!-        real, dimension(:), pointer :: dzs => null()

        !> Diagnostic variables.
        !* stge: Total energy stored in the system. [W m**-2].
        !* stgw: Total liquid water storage in the land surface. [kg m**-2].
!-        real, dimension(:), pointer :: stge => null()
!-        real, dimension(:), pointer :: stgw => null()

        !> Routing variables.
        !* rff: Total runoff (from all surface, subsurface, and groundwater components). [mm].
        !* qi: Flow rate entering the channel. [m**3 s**-1].
        !* qo: Flow rate leaving the channel (discharge). [m**3 s**-1].
        !* stgch: Channel storage. [m**3].
        !* zlvl: Stage level. [m].
        !* div: Storage diverted to a routing element. [m**3].
        !* abstr: Storage abstracted from a routing element. [m**3].
        real, dimension(:), pointer :: rff => null()
        real, dimension(:), pointer :: qi => null()
        real, dimension(:), pointer :: qo => null()
        real, dimension(:), pointer :: stgch => null()
        real, dimension(:), pointer :: zlvl => null()
        real, dimension(:), pointer :: div => null()
        real, dimension(:), pointer :: abstr => null()

        !> Basin attributes (general).
        integer, dimension(:), allocatable :: next_id
        real, dimension(:), allocatable :: surface_area
        real, dimension(:), allocatable :: area_weight
        real, dimension(:), allocatable :: topo_elev
        real, dimension(:), allocatable :: topo_slope

        !> Routing attributes.
        real, dimension(:), allocatable :: chnl_slope
        real, dimension(:), allocatable :: chnl_length
        integer, dimension(:), allocatable :: ichnl
        integer, dimension(:), allocatable :: ireach
        real, dimension(:), allocatable :: drainage_area
        real, dimension(:), allocatable :: bankfull

        !> Reference fields.
        real, dimension(:), allocatable :: lon
        real, dimension(:), allocatable :: lat

        !> Maps.
        integer, dimension(:), allocatable :: from_grid_x
        integer, dimension(:), allocatable :: from_grid_y
        integer, dimension(:), allocatable :: from_gru
        integer, dimension(:), allocatable :: from_riverclass
        integer, dimension(:), allocatable :: from_cell

        !> Indices.
        character(len = SHORT_FIELD_LENGTH) :: dim_name = ''
        integer :: dim_length = 0
    end type

    !> Description:
    !>  Container for a group of variables.
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  gru: By GRU 1:NTYPE.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    type model_variables_groups
        type(model_variables_fields), pointer :: tile, gru, grid, basin
        type(io_state_flags) flgs
    end type

    !> State of SA_MESH variables in the current time-step.
    !*  vs: Group of variables (e.g., tile, grid).
    type(model_variables_groups), save :: vs

    !> Description:
    !>  Generic structure for an instance of a variable in a list.
    !>
    !> Variables:
    !*  dat: Data frame.
    type, abstract :: model_variable
        character(len = SHORT_FIELD_LENGTH) :: id = ''
    end type

    !> Extensions.
    type, extends(model_variable) :: model_variable_real1d
        real, dimension(:), pointer :: dat => null()
    end type
    type, extends(model_variable) :: model_variable_int1d
        integer, dimension(:), pointer :: dat => null()
    end type

    !> Description:
    !>  Wrapper for variables that extend 'model_variable'.
    type model_variable_wrapper
        class(model_variable), allocatable :: var
    end type

    contains

    !> Description:
    !>  Subroutine to reset/zero a group of variables.
    !>
    !> Variables:
    !*  group: Group of variables.
    !*  ierr: Return status
    subroutine model_variables_group_reset(group, ierr)

        !> Input/output variables.
        type(model_variables_fields) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Initialize the return status.
        ierr = 0

        !> Meteorology/climatology variables.
        if (associated(group%fsin)) group%fsin = 0.0
        if (associated(group%fsvs)) group%fsvs = 0.0
        if (associated(group%fsir)) group%fsir = 0.0
        if (associated(group%fsdr)) group%fsdr = 0.0
        if (associated(group%fsdff)) group%fsdff = 0.0
        if (associated(group%flin)) group%flin = 0.0
        if (associated(group%ta)) group%ta = 0.0
        if (associated(group%qa)) group%qa = 0.0
        if (associated(group%pres)) group%pres = 0.0
        if (associated(group%uu)) group%uu = 0.0
        if (associated(group%vv)) group%vv = 0.0
        if (associated(group%uv)) group%uv = 0.0
        if (associated(group%wdir)) group%wdir = 0.0
        if (associated(group%prern)) group%prern = 0.0
        if (associated(group%presno)) group%presno = 0.0
        if (associated(group%pre)) group%pre = 0.0

        !> Canopy variables.
        if (associated(group%lqwscan)) group%lqwscan = 0.0
        if (associated(group%fzwscan)) group%fzwscan = 0.0
        if (associated(group%cmas)) group%cmas = 0.0
        if (associated(group%tacan)) group%tacan = 0.0
        if (associated(group%qacan)) group%qacan = 0.0
        if (associated(group%tcan)) group%tcan = 0.0
        if (associated(group%gro)) group%gro = 0.0

        !> Snow variables.
        if (associated(group%fsno)) group%fsno = 0.0
        if (associated(group%sno)) group%sno = 0.0
        if (associated(group%rhosno)) group%rhosno = 0.0
!-        if (associated(group%zsno)) group%zsno = 0.0
        if (associated(group%lqwssno)) group%lqwssno = 0.0
        if (associated(group%tsno)) group%tsno = 0.0
        if (associated(group%albsno)) group%albsno = 0.0
        if (associated(group%drainsno)) group%drainsno = 0.0

        !> Surface variables.
        if (associated(group%albt)) group%albt = 0.0
        if (associated(group%alvs)) group%alvs = 0.0
        if (associated(group%alir)) group%alir = 0.0
        if (associated(group%gte)) group%gte = 0.0
        if (associated(group%zpnd)) group%zpnd = 0.0
!-        if (associated(group%lqwspnd)) group%lqwspnd = 0.0
        if (associated(group%tpnd)) group%tpnd = 0.0
        if (associated(group%pndcaf)) group%pndcaf = 0.0
        if (associated(group%potevp)) group%potevp = 0.0
        if (associated(group%et)) group%et = 0.0
!-        if (associated(group%evpb)) group%evpb = 0.0
!-        if (associated(group%arrd)) group%arrd = 0.0
        if (associated(group%ovrflw)) group%ovrflw = 0.0
        if (associated(group%qevp)) group%qevp = 0.0
        if (associated(group%qsens)) group%qsens = 0.0
        if (associated(group%gzero)) group%gzero = 0.0
        if (associated(group%tsfs)) group%tsfs = 0.0
        if (associated(group%tsurf)) group%tsurf = 0.0

        !> Ice/glacier variables.
        if (associated(group%lqwsice)) group%lqwsice = 0.0
        if (associated(group%tice)) group%tice = 0.0

        !> Subsurface/soil variables.
        if (associated(group%dzsol)) group%dzsol = 0.0
        if (associated(group%dzsolhyd)) group%dzsolhyd = 0.0
        if (associated(group%thlqsol)) group%thlqsol = 0.0
        if (associated(group%thicsol)) group%thicsol = 0.0
!-        if (associated(group%lqwssol)) group%lqwssol = 0.0
!-        if (associated(group%fzwssol)) group%fzwssol = 0.0
        if (associated(group%tsol)) group%tsol = 0.0
        if (associated(group%gflx)) group%gflx = 0.0
        if (associated(group%latflw)) group%latflw = 0.0
        if (associated(group%zsol)) group%zsol = 0.0
        if (associated(group%zsolhyd)) group%zsolhyd = 0.0
        if (associated(group%zsolsat)) group%zsolsat = 0.0
        if (associated(group%ggeo)) group%ggeo = 0.0
        if (associated(group%tbas)) group%tbas = 0.0
        if (associated(group%drainsol)) group%drainsol = 0.0

        !> Groundwater/lower zone storage variables.
        if (associated(group%rchg)) group%rchg = 0.0
        if (associated(group%stggw)) group%stggw = 0.0
        if (associated(group%lkg)) group%lkg = 0.0
!-        if (associated(group%dzs)) group%dzs = 0.0

        !> Diagnostic variables.
!-        if (associated(group%stge)) group%stge = 0.0
!-        if (associated(group%stgw)) group%stgw = 0.0

        !> Routing variables.
        if (associated(group%rff)) group%rff = 0.0
        if (associated(group%qi)) group%qi = 0.0
        if (associated(group%qo)) group%qo = 0.0
        if (associated(group%stgch)) group%stgch = 0.0
        if (associated(group%zlvl)) group%zlvl = 0.0
        if (associated(group%div)) group%div = 0.0
        if (associated(group%abstr)) group%abstr = 0.0

    end subroutine

    !> Description:
    !>  Subroutine to reset/zero all variables.
    !>
    !> Variables:
    !*  shd: Basin information.
    !*  ierr: Return status
    subroutine model_variables_reset(shd, ierr)

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
            call model_variables_group_reset(vs%tile, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call model_variables_group_reset(vs%grid, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%basin, z); if (z /= 0) ierr = z
        end if

    end subroutine

    !> Description:
    !>  Subroutine to allocate a group of variables.
    !>
    !> Variables:
    !*  group: Group of variables.
    !*  n: Index of elements (e.g., tiles, grids).
    !*  nsl: Number of layers.
    !*  ierr: Return status
    subroutine model_variables_group_allocate(group, n, nsl, ierr)

        !> Input variables.
        integer, intent(in) :: n, nsl

        !> Input/output variables.
        type(model_variables_fields), pointer :: group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Allocate group.
        if (.not. associated(group)) allocate(group)

        !> Meteorology/climatology variables.
!-        allocate(group%fsin(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsvs(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsir(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsdr(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsdff(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%flin(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%ta(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%qa(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%pres(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%uu(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%vv(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%uv(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%wdir(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%prern(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%presno(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%pre(n), stat = z); if (z /= 0) ierr = z

        !> Canopy variables.
        allocate(group%lqwscan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fzwscan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%cmas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tacan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qacan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tcan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gro(n), stat = z); if (z /= 0) ierr = z

        !> Snow variables.
        allocate(group%fsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%sno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rhosno(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%zsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwssno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%albsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%drainsno(n), stat = z); if (z /= 0) ierr = z

        !> Surface variables.
        allocate(group%albt(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alvs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alir(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gte(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zpnd(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%lqwspnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tpnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%pndcaf(n), stat = z); if (z /= 0) ierr = z
        allocate(group%potevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%et(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%evpb(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%arrd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ovrflw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qsens(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gzero(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsfs(n, 4), stat = z); if (z /= 0) ierr = z
        allocate(group%tsurf(n), stat = z); if (z /= 0) ierr = z

        !> Ice/glacier variables.
        allocate(group%lqwsice(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tice(n), stat = z); if (z /= 0) ierr = z

        !> Subsurface/soil variables.
        allocate(group%dzsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%dzsolhyd(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thlqsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thicsol(n, nsl), stat = z); if (z /= 0) ierr = z
!-        allocate(group%lqwssol(n, nsl), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fzwssol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%gflx(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%latflw(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%zsol(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsolhyd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsolsat(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ggeo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tbas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%drainsol(n), stat = z); if (z /= 0) ierr = z

        !> Groundwater/lower zone storage variables.
        allocate(group%rchg(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stggw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lkg(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%dzs(n), stat = z); if (z /= 0) ierr = z

        !> Diagnostic variables.
!-        allocate(group%stge(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%stgw(n), stat = z); if (z /= 0) ierr = z

        !> Routing variables.
        allocate(group%rff(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qi(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stgch(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zlvl(n), stat = z); if (z /= 0) ierr = z
        allocate(group%div(n), stat = z); if (z /= 0) ierr = z
        allocate(group%abstr(n), stat = z); if (z /= 0) ierr = z

    end subroutine

    !> Description:
    !>  Subroutine to allocate variables.
    !>
    !> Variables:
    !*  shd: Basin information.
    !*  ierr: Return status
    subroutine model_variables_init(shd, ierr)

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
        if (ro%RUNTILE) then
            call model_variables_group_allocate(vs%tile, shd%lc%NML, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%tile)) then
                vs%tile%dim_length = shd%lc%NML
                vs%tile%dim_name = 'tile'
            end if
            call model_variables_group_reset(vs%tile, z); if (z /= 0) ierr = z
            call model_variables_group_allocate(vs%gru, shd%lc%NTYPE, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%gru)) then
                vs%gru%dim_length = shd%lc%NTYPE
                vs%gru%dim_name = 'gru'
            end if
            call model_variables_group_reset(vs%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call model_variables_group_allocate(vs%grid, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%grid)) then
                vs%grid%dim_length = shd%NA
                vs%grid%dim_name = 'grid'
            end if
            call model_variables_group_reset(vs%grid, z); if (z /= 0) ierr = z
            call model_variables_group_allocate(vs%basin, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%basin)) then
                vs%basin%dim_length = shd%NA
                vs%basin%dim_name = 'basin'
            end if
            call model_variables_group_reset(vs%basin, z); if (z /= 0) ierr = z
        end if

    end subroutine

end module
