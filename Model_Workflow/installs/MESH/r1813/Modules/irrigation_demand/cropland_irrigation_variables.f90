module cropland_irrigation_variables

    !> Variable type: cropland_irrigation_parameters
    !>  Description: Parameters used by the cropland irrigation module.
    !>
    !> Variables:
    !*  jdsow: Day in year to start stage development (use the state variable GRO if <= 0). [day in year, 1-365/366].
    !*  ldini: Length of initial stage. [days].
    !*  lddev: Length of development state. [days].
    !*  ldmid: Length of middle stage. [days].
    !*  ldlate: Length of late stage. [days].
    !*  Kcini: Crop factor during the initial stage. [--].
    !*  Kcdev: Crop factor during the development state. [--].
    !*  Kcmid: Crop factor during the middle stage. [--].
    !*  Kclate: Crop factor during the late stage. [--].
    type cropland_irrigation_parameters
        integer, dimension(:), allocatable :: jdsow, ldini, lddev, ldmid, ldlate
        real, dimension(:), allocatable :: Kcini, Kcdev, Kcmid, Kclate
    end type

    !* cip: Instance of parameters used by the cropland irrigation module.
    type(cropland_irrigation_parameters), save :: cip

    !* cip: Instance of parameters used by the cropland irrigation module (ROT indexing).
    type(cropland_irrigation_parameters), save :: ciprot

    !> Variable type: cropland_irrigation_variables_vars
    !>  Description: Internal states used by the cropland irrigation module.
    !>
    !> Variables:
    !*  lqws2_mm: State of soil water storage in the previous period. [mm].
    !*  lqws1_mm: State of soil water storage in the present period. [mm].
    !*  pre_mm: Accumulation of precipitation over the period. [mm].
    !*  pevp_mm: Accumulation of potential evapotranspiration over the period. [mm].
    !*  icu_mm: Irrigation demand at the end of the period. [mm].
    type cropland_irrigation_variables_vars
        real, dimension(:), allocatable :: lqws2_mm, lqws1_mm, pre_mm, pevp_mm, icu_mm
    end type

    !> Variable type: cropland_irrigation_variables_fkey
    !>  Description: Internal file keys used by the cropland irrigation module.
    !>
    !> Variables:
    !*  KDLY: Daily output
    !*  KHLY: Hourly output
    !*  KTS: Per time-step output
    type cropland_irrigation_variables_fkey
        integer :: KDLY = 0, KHLY = 2, KTS = 3
        integer :: kmin = 0, kmax = 3
    end type

    !> Variable type: cropland_irrigation_variables_int
    !>  Description: Internal variables used by the cropland irrigation module.
    !>
    !> Variables:
    !*  icrop: Internal crop active variables (GRU has FCAN(3) > 0.0). [--].
    !*  jdini: Day in the year the initial stage begins. [day in year, 1-365/366].
    !*  jddev: Day in the year the development stage beings. [day in year, 1-365/366].
    !*  jdmid: Day in the year the middle stage begins. [day in year, 1-365/366].
    !*  jdlate: Day in the year the late stage begins. [day in year, 1-365/366].
    !*  jdend: Day in the year the growth stage ends. [day in year, 1-365/366].
    type cropland_irrigation_variables_int
        type(cropland_irrigation_variables_fkey) :: fk
        integer, dimension(:), allocatable :: icrop, jdini, jddev, jdmid, jdlate, jdend
        type(cropland_irrigation_variables_vars), dimension(:), allocatable :: vars
    end type

    !* civ: Instance of variables used by the cropland irrigation module.
    type(cropland_irrigation_variables_int), save :: civ

    !> Variable type: cropland_irrigation_variables_vars
    !>  Description: Internal states used by the cropland irrigation module.
    !>
    !> Variables:
    !*  icu_mm: Irrigation demand at the end of the period. [mm].
    type cropland_irrigation_aggregated_output
        real, dimension(:), allocatable :: icu_na_mm, icu_aclass_mm, icu_frac_mm
    end type

    !* ciago: Instance of aggregated output from the cropland irrigation module.
    type(cropland_irrigation_aggregated_output), save :: ciago

    !> Variable type: cropland_irrigation_flags
    !>  Description: Flags used by the cropland irrigation module.
    !>
    !> Variables:
    !*  PROCESS_ACTIVE: .true. to enable the cropland irrigation module.
    type cropland_irrigation_flags
        logical :: PROCESS_ACTIVE = .false.
        integer :: ts_flag = 0
    end type

    !* cifg: Instance of flags used by the cropland irrigation module.
    type(cropland_irrigation_flags), save :: cifg

end module
