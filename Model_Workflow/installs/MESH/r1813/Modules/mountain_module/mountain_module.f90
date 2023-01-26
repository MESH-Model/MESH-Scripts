!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>  Calls "calc_rsrd_adjusted".
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!>
!> Notes:
!>  - 2018/02/01: Converted to Fortran (exact copy)
!>  - 2018/02/02: Fortran code optimized/consolidated
!>      ('program' component replaced by 'solar_adjust_module')
!>  - 2019/10/10: Upgraded into Mountain MESH (renamed 'mountain_module')
module mountain_module

    implicit none

    !> Description:
    !> Type for parameters (options).
    !>
    !> Variables:
    !*  Curveweight: wind model curvature weight
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    !*  ilapse: Flag to specify lapse rate table. [--].
    !>      0: None.
    !>      1: Use hour month hour temperature and dewpoint lapse rate and others variable are monthly derived from 2.5km GEM (Oct, 2016 to Sept, 2019). (default).
    !>      2: Use hour month hour temperature and dewpoint lapse rate derived from station data from AEP and EC other variable are monthly derived lapse rate from 2.5km GEM (Oct, 2002 to Sept, 2015).
    !>      3: Use table based on literature values (Thornton, 1997).
    !*  ipre: Flag to specify precipitation adjustment method. [--].
    !>      0: None.
    !>      1: Elevation Range with Maximum elevation Method (ERMM) (Zhang et al. (2018)) and (Tesfa et al. (2019)) (default).
    !>      2: Adjustment based on Thornton, 1997 or derived lapse-rate.
    !*  itemp: Flag to specify temperature adjustment method. [--].
    !>      0: None.
    !>      1: Lapse-rate adjustment. (default).
    !*  ipres: Flag to specify pressure adjustment method. [--].
    !>      0: None.
    !>      1: Elevation adjustment. (default).
    !*  ihumd: Flag to specify specific humidity adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Murray, 1967. (default).
    !>      2: Adjustment based on Kunkel, 1989.
    !*  irlds: Flag to specify longwave radiation adjustment method. [--].
    !>      0: None.
    !>      1: Lapse-rate adjustment based on topography based on temperature and vapour pressure (Brutsaert (1975)). (default).
    !>      2: Adjustment based on topography derived or literature based lapse rate Marty et al., 2002).
    !*  iwind: Flag to specify wind speed adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Liston and Sturm, 1998 (requires wind direction, winddir). (default).
    !>      2: Lapse-rate adjustment.
    !*  iphase: Flag to specify precipitation phase partitioning method. [--].
    !>      0: Partioning to 0.0 degrees C.
    !>      1: Partitioning based on Harder and Pomeroy, 2013. (default).
    !*  irsrd: Flag to specify shortwave radiation adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Garnier and Ohmura, 1970. (default).
    !>      2: Liston, Elder - 2006 - A Meteorological Distribution System for High-Resolution Terrestrial Modeling (MicroMet).
    !*  idecl: Flag to specify calculation to use for declination. [--].
    !>      0: Alternate approach.
    !>      1: Based on Dingman, 2015 and Iqbal, 1983. (default).
    !*  elev: Weighted average elevation of GRUs. [m].
    !*  delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  slope: Weighted average slope of surface (1: grid; 2: GRU). [degrees].
    !*  aspect: Weighted average aspect of surface (1: grid; 2: GRU). [degrees].
    !*  delta_elevmax: Weighted average difference in elevation divided by the maximum elevation (1: grid; 2: GRU). [--].
    !*  curvature: Weighted average curvature of the surface. [--].
    !*  plapse: Table of lapse rate values for precipitation. [--].
    !*  tlapse: Table of lapse rate values for temperature. [hour, month].
    !*  dtlapse: Table of lapse rate values for dew point temperature. [hour, month].
    !*  lwlapse: Table of lapse rate values for longwave radiation. [--].
    !*  wlapse: Table of lapse rate values for wind speed. [--].
    type mountain_parameters
        real :: CurveWeight = 0.50
        integer :: CalcFreq = 288
        integer :: ilapse = 1
        integer :: ipre = 1
        integer :: itemp = 1
        integer :: ipres = 1
        integer :: ihumd = 1
        integer :: irlds = 1
        integer :: iwind = 1
        integer :: iphase = 1
        integer :: irsrd = 1
        integer :: idecl = 1
        real, dimension(:, :), allocatable :: elev, slope, aspect, delta, &
        delta_elevmax, curvature
        real, dimension(12) :: plapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(288) :: tlapse = (/ &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(288) :: dtlapse = (/ &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: lwlapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: wlapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
    end type

    !> Description:
    !> Type for variables/constants.
    !>
    !> Variables:
    !*  xlng: Longitude. [degrees].
    !*  ylat: Latitude. [degrees].
    !*  elev: Weighted average elevation. [m].
    !*  slope: Weighted average slope of the surface. [degrees].
    !*  aspect: Weighted average aspect of the surface. [degrees].
    !*  delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  delta_elevmax: Weighted average difference in elevation divided by the maximum elevation (1: grid; 2: GRU). [--].
    !*  curvature: Weighted average curvature of the surface. [--].
    type mountain_variables
        real, dimension(:), allocatable :: &
            elev, xlng, ylat, slope, aspect, delta, delta_elevmax, curvature
    end type

    !> Description:
    !> Type for 'mountain' parameters and variables.
    !>
    !> Variables:
    !*  pm: Parameters and options.
    !*  vs: Variables.
    !*  PROCESS_ACTIVE: .true. to enable 'MOUNTAINMESH'; .false. otherwise (default: .false.).
    type mountain_container
        type(mountain_parameters) pm
        type(mountain_variables) vs
        logical :: PROCESS_ACTIVE = .false.
        character(len = 1000) :: RUNOPTIONSFLAG = ''
    end type

    !* fsadj: Instance of 'mountain' parameters and variables.
    type(mountain_container), save :: mountain_mesh

    contains

    subroutine mountain_extract_value(arg, ierr)

        !> Required for the 'parse', 'lowercase', and 'value' functions.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: arg

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer nargs, z
        character(len = len(arg)) args(50)

        !> Status.
        ierr = 0

        !> Return argument contains no '=' (designates option).
        if (scan(arg, '=') == 0) return

        !> Parse the option (using '=').
        !> Return if there is no value assigned to the option.
        call parse(arg, '=', args, nargs)
        if (nargs <= 1) return

        !> Assign the value.
        z = 0
        select case (lowercase(args(1)))
            case ('curveweight')
                call value(args(2), mountain_mesh%pm%CurveWeight, z)
            case ('calcfreq')
                call value(args(2), mountain_mesh%pm%CalcFreq, z)
            case ('ilapse')
                call value(args(2), mountain_mesh%pm%ilapse, z)
            case ('ipre')
                call value(args(2), mountain_mesh%pm%ipre, z)
            case ('itemp')
                call value(args(2), mountain_mesh%pm%itemp, z)
            case ('ipres')
                call value(args(2), mountain_mesh%pm%ipres, z)
            case ('ihumd')
                call value(args(2), mountain_mesh%pm%ihumd, z)
            case ('irlds')
                call value(args(2), mountain_mesh%pm%irlds, z)
            case ('iwind')
                call value(args(2), mountain_mesh%pm%iwind, z)
            case ('iphase')
                call value(args(2), mountain_mesh%pm%iphase, z)
            case ('irsrd')
                call value(args(2), mountain_mesh%pm%irsrd, z)
            case ('idecl')
                call value(args(2), mountain_mesh%pm%idecl, z)
            case default
                ierr = 2
        end select

        !> Check for conversion error.
        if (z /= 0) ierr = 1

    end subroutine

    subroutine mountain_parse_options(flg, ierr)

        !> Required for MESH 'print_warning' routine.
        use sa_mesh_common

        !> Required for the 'parse', 'lowercase', and 'uppercase' functions.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: flg

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer nargs, i, z
        character(len = 20) args(20)

        !> Initialize the error variable.
        ierr = 0

        !> Assume if the flag is populated that the routine is enabled.
        !> Disabled if the 'off' keyword provided.
        if (len_trim(flg) > 0) then
            mountain_mesh%PROCESS_ACTIVE = .true.
        else
            return
        end if

        !> Parse the flag.
        call parse(flg, ' ', args, nargs)

        !> Check the keywords.
        do i = 2, nargs

            !> Exit if any of the keywords have disabled the routine.
            if (.not. mountain_mesh%PROCESS_ACTIVE) return

            !> Reset the error variable.
            z = 0

            !> Specific options.
            select case (lowercase(args(i)))
                case ('off', '0')

                    !> 'off' or '0' disables the routine.
                    mountain_mesh%PROCESS_ACTIVE = .false.
                    exit
                case ('none')

                    !> Disable all options (to simplify enabling a subset).
                    mountain_mesh%pm%ilapse = 0
                    mountain_mesh%pm%ipre = 0
                    mountain_mesh%pm%itemp = 0
                    mountain_mesh%pm%ipres = 0
                    mountain_mesh%pm%ihumd = 0
                    mountain_mesh%pm%irlds = 0
                    mountain_mesh%pm%iwind = 0
                    mountain_mesh%pm%iphase = 0
                    mountain_mesh%pm%irsrd = 0
                    mountain_mesh%pm%idecl = 0
                case default

                    !> Other options.
                    call mountain_extract_value(args(i), z)
                    if (z == 2) then
                        call print_warning("Unrecognized option on '" // uppercase(trim(args(1))) // "': " // trim(args(i)))
                    else if (z == 1) then
                        call print_warning( &
                            "An error occurred parsing the '" // trim(args(i)) // "' option on '" // &
                            uppercase(trim(args(1))) // "'.")
                        ierr = z
                    end if
            end select
        end do

    end subroutine

    subroutine mountain_init(fls, shd, cm)

        !> Required for MESH variables and options.
        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to the subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer k, ierr
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) val

        !> Initialize the error variable.
        ierr = 0

        !> Parse options.
        call mountain_parse_options(mountain_mesh%RUNOPTIONSFLAG, ierr)

        !> Return if module is not enabled.
        if (.not. mountain_mesh%PROCESS_ACTIVE) then
            if (allocated(mountain_mesh%pm%slope)) deallocate(mountain_mesh%pm%slope)
            if (allocated(mountain_mesh%pm%aspect)) deallocate(mountain_mesh%pm%aspect)
            if (allocated(mountain_mesh%pm%delta)) deallocate(mountain_mesh%pm%delta)
            if (allocated(mountain_mesh%pm%delta_elevmax)) deallocate(mountain_mesh%pm%delta_elevmax)
            if (allocated(mountain_mesh%pm%curvature)) deallocate(mountain_mesh%pm%curvature)
            return
        end if

        !> Allocate variables.
        allocate( &
            mountain_mesh%vs%elev(il1:il2), mountain_mesh%vs%xlng(il1:il2), &
            mountain_mesh%vs%ylat(il1:il2), mountain_mesh%vs%slope(il1:il2), &
            mountain_mesh%vs%aspect(il1:il2), mountain_mesh%vs%delta(il1:il2), &
            mountain_mesh%vs%delta_elevmax(il1:il2), &
            mountain_mesh%vs%curvature(il1:il2))
            mountain_mesh%vs%slope = 0.0
            mountain_mesh%vs%aspect = 0.0
            mountain_mesh%vs%delta = 0.0
            mountain_mesh%vs%delta_elevmax = 0.0
            mountain_mesh%vs%curvature = 0.0

        !> Assign values.
        do k = il1, il2

            !> Pull generic values from drainage_database.r2c
            mountain_mesh%vs%elev(k) = shd%ELEV(shd%lc%ILMOS(k))
            mountain_mesh%vs%xlng(k) = shd%XLNG(shd%lc%ILMOS(k))
            mountain_mesh%vs%ylat(k) = shd%YLAT(shd%lc%ILMOS(k))

            !> Overwrite with values provided by GRU (e.g., parameters.r2c).
            if (allocated(mountain_mesh%pm%elev)) then
                mountain_mesh%vs%elev(k) = mountain_mesh%pm%elev(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%slope)) then
                mountain_mesh%vs%slope(k) = mountain_mesh%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k))

                !> Overwrite estimated average slope of the GRU.
                pm%tile%xslp(k) = tan(mountain_mesh%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*3.14159265/180.0)
            end if
            if (allocated(mountain_mesh%pm%aspect)) then
                mountain_mesh%vs%aspect(k) = mountain_mesh%pm%aspect(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%delta)) then
                mountain_mesh%vs%delta(k) = mountain_mesh%pm%delta(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%delta_elevmax)) then
                mountain_mesh%vs%delta_elevmax(k) = mountain_mesh%pm%delta_elevmax(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%curvature)) then
                mountain_mesh%vs%curvature(k) = mountain_mesh%pm%curvature(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
        end do

        !> De-allocate 'ROW' based fields (from parameters file).
        if (allocated(mountain_mesh%pm%slope)) deallocate(mountain_mesh%pm%slope)
        if (allocated(mountain_mesh%pm%aspect)) deallocate(mountain_mesh%pm%aspect)
        if (allocated(mountain_mesh%pm%delta)) deallocate(mountain_mesh%pm%delta)
        if (allocated(mountain_mesh%pm%delta_elevmax)) deallocate(mountain_mesh%pm%delta_elevmax)
        if (allocated(mountain_mesh%pm%curvature)) deallocate(mountain_mesh%pm%curvature)

        !> Print summary and remark that the process is active.
        call print_new_section("MOUNTAINMESH is active.")
        call increase_tab()

        !> Print configuration information to file if 'DIAGNOSEMODE' is active.
        if (DIAGNOSEMODE) then
            line = 'MOUNTAINMESH on'
            write(val, FMT_GEN) mountain_mesh%pm%CurveWeight
            line = trim(line) // ' CurveWeight=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%CalcFreq
            line = trim(line) // ' CalcFreq=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ilapse
            line = trim(line) // ' ilapse=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ipre
            line = trim(line) // ' ipre=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%itemp
            line = trim(line) // ' itemp=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ipres
            line = trim(line) // ' ipres=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ihumd
            line = trim(line) // ' ihumd=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%irlds
            line = trim(line) // ' irlds=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%iwind
            line = trim(line) // ' iwind=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%iphase
            line = trim(line) // ' iphase=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%irsrd
            line = trim(line) // ' irsrd=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%idecl
            line = trim(line) // ' idecl=' // trim(adjustl(val))
            call print_message(line)
        end if

        !> Check values, print error messages for invalid values.
        !> The check is of 'GAT'-based variables, for which all tiles should have valid values.
        if (mod(24*60, mountain_mesh%pm%CalcFreq) /= 0) then
            write(line, FMT_GEN) mountain_mesh%pm%CalcFreq
            call print_error("'CalcFreq' must evenly divide into minutes in the day. 1440 mod " // trim(adjustl(line)) // " /= 0.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%elev < 0.0)) then
            call print_error("Values of 'elevation' are less than zero.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%slope < 0.0)) then
            call print_error("Values of 'slope' are less than zero.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%aspect < 0.0)) then
            call print_error("Values of 'aspect' are less than zero.")
            ierr = 1
        end if
        if (mountain_mesh%pm%iwind == 1 .and. .not. cm%dat(ck%WD)%factive) then
            call print_error("'iwind' option 1 requires wind direction, but the driving variable is not active.")
            ierr = 1
        end if

        !> Data tables.
        select case (mountain_mesh%pm%ilapse)
            case (1)

                !> Option 1:
                !>  Tables of hour month hour lapse rate derived from the high
                !>  resolution (2.5km by 2.5km) GEM run for the period Oct, 2016
                !>  to Sept, 2019.
                mountain_mesh%pm%plapse = (/ 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30 /)
                mountain_mesh%pm%tlapse = (/ &
                    6.11, 4.68, 5.23, 6.21, 7.42, 7.36, 6.54, 6.40, 6.02, 6.34, 7.06, 6.71, &
                    6.13, 4.65, 5.14, 6.15, 7.20, 7.26, 6.48, 6.26, 5.98, 6.35, 7.03, 6.69, &
                    6.12, 4.62, 5.11, 6.13, 7.06, 7.20, 6.41, 6.22, 6.01, 6.38, 6.96, 6.64, &
                    6.10, 4.60, 5.12, 6.13, 6.99, 7.16, 6.32, 6.22, 6.05, 6.38, 6.87, 6.60, &
                    6.06, 4.54, 5.17, 6.16, 6.93, 7.10, 6.28, 6.21, 6.06, 6.38, 6.81, 6.55, &
                    6.05, 4.49, 5.22, 6.19, 6.89, 7.08, 6.29, 6.20, 6.05, 6.36, 6.72, 6.49, &
                    6.02, 4.45, 5.21, 6.19, 6.92, 7.09, 6.36, 6.20, 6.06, 6.33, 6.63, 6.44, &
                    6.02, 4.43, 5.16, 6.22, 7.17, 7.09, 6.41, 6.25, 6.07, 6.30, 6.57, 6.41, &
                    6.00, 4.43, 5.16, 6.48, 7.49, 7.22, 6.28, 6.16, 6.10, 6.29, 6.52, 6.38, &
                    5.95, 4.45, 5.55, 6.89, 7.81, 7.44, 6.47, 6.20, 6.18, 6.45, 6.50, 6.33, &
                    6.00, 4.64, 6.11, 7.18, 8.06, 7.67, 6.71, 6.40, 6.38, 6.76, 6.71, 6.43, &
                    6.22, 4.83, 6.22, 7.31, 8.39, 7.67, 6.74, 6.47, 6.47, 7.13, 7.35, 6.75, &
                    6.50, 4.95, 6.47, 7.49, 8.52, 7.75, 6.92, 6.72, 6.64, 7.49, 7.74, 7.03, &
                    6.73, 5.03, 6.58, 7.58, 8.54, 7.82, 7.05, 6.90, 6.76, 7.76, 8.02, 7.25, &
                    6.87, 5.14, 6.61, 7.61, 8.53, 7.85, 7.09, 7.01, 6.80, 7.91, 8.18, 7.38, &
                    6.89, 5.23, 6.61, 7.61, 8.51, 7.85, 7.20, 7.05, 6.81, 7.95, 8.21, 7.43, &
                    6.78, 5.28, 6.62, 7.63, 8.44, 7.81, 7.27, 7.06, 6.80, 7.81, 8.11, 7.36, &
                    6.51, 5.27, 6.55, 7.61, 8.41, 7.87, 7.15, 7.18, 6.73, 7.61, 7.89, 7.15, &
                    6.16, 5.21, 6.40, 7.54, 8.44, 7.88, 7.02, 7.22, 6.59, 7.29, 7.57, 6.89, &
                    5.88, 5.09, 6.18, 7.39, 8.40, 7.82, 6.96, 7.05, 6.51, 6.92, 7.31, 6.66, &
                    5.72, 5.01, 5.90, 7.09, 8.25, 7.78, 6.84, 7.00, 6.38, 6.63, 7.13, 6.52, &
                    5.68, 4.96, 5.67, 6.78, 7.98, 7.71, 6.69, 6.96, 6.23, 6.47, 7.01, 6.46, &
                    5.71, 4.96, 5.52, 6.52, 7.70, 7.55, 6.61, 6.79, 6.13, 6.41, 6.96, 6.45, &
                    6.08, 4.79, 5.30, 6.37, 7.73, 7.47, 6.56, 6.58, 6.04, 6.37, 7.12, 6.59 /)
                mountain_mesh%pm%dtlapse = (/ &
                    1.70, 1.68, 1.60, 2.27, 2.43, 2.78, 4.10, 2.91, 2.62, 2.40, 1.99, 2.04, &
                    1.74, 1.68, 1.52, 2.25, 2.41, 2.70, 3.89, 2.86, 2.56, 2.42, 2.00, 2.06, &
                    1.80, 1.65, 1.52, 2.24, 2.46, 2.63, 3.67, 2.86, 2.51, 2.46, 2.04, 2.06, &
                    1.77, 1.67, 1.54, 2.23, 2.38, 2.61, 3.50, 2.81, 2.47, 2.46, 2.04, 2.08, &
                    1.79, 1.69, 1.55, 2.22, 2.26, 2.64, 3.35, 2.73, 2.47, 2.45, 2.06, 2.05, &
                    1.85, 1.66, 1.55, 2.22, 2.19, 2.66, 3.21, 2.65, 2.47, 2.47, 2.03, 2.03, &
                    1.89, 1.64, 1.51, 2.27, 2.25, 2.80, 3.22, 2.64, 2.47, 2.51, 1.96, 2.00, &
                    1.90, 1.61, 1.44, 2.32, 2.42, 3.05, 3.48, 2.77, 2.48, 2.53, 1.93, 1.97, &
                    1.90, 1.59, 1.40, 2.42, 2.48, 3.22, 3.99, 3.10, 2.51, 2.57, 1.94, 1.95, &
                    1.89, 1.57, 1.60, 2.43, 2.36, 3.26, 4.46, 3.46, 2.68, 2.71, 1.96, 1.94, &
                    1.93, 1.64, 1.95, 2.22, 2.34, 3.29, 4.72, 3.68, 2.79, 2.91, 2.03, 1.95, &
                    1.81, 1.45, 1.86, 2.21, 2.45, 3.47, 4.97, 4.02, 2.94, 2.84, 2.06, 2.01, &
                    1.89, 1.39, 1.66, 2.16, 2.41, 3.43, 4.98, 4.06, 3.04, 2.79, 2.09, 1.97, &
                    1.91, 1.29, 1.64, 2.10, 2.50, 3.38, 4.89, 4.17, 3.08, 2.71, 2.07, 1.97, &
                    1.93, 1.30, 1.72, 2.09, 2.64, 3.35, 4.85, 4.25, 3.15, 2.61, 2.08, 1.93, &
                    1.96, 1.40, 1.77, 2.15, 2.70, 3.37, 4.74, 4.17, 3.23, 2.66, 2.06, 1.95, &
                    2.01, 1.54, 1.85, 2.22, 2.81, 3.43, 4.64, 4.11, 3.27, 2.76, 2.14, 2.09, &
                    2.04, 1.74, 1.98, 2.28, 2.78, 3.41, 4.69, 4.04, 3.30, 2.83, 2.23, 2.22, &
                    2.00, 1.92, 2.13, 2.33, 2.75, 3.40, 4.70, 3.93, 3.29, 2.91, 2.24, 2.28, &
                    1.89, 2.04, 2.23, 2.46, 2.80, 3.43, 4.66, 3.88, 3.18, 2.93, 2.17, 2.23, &
                    1.85, 2.09, 2.27, 2.61, 2.84, 3.43, 4.64, 3.59, 3.00, 2.92, 2.08, 2.17, &
                    1.82, 2.13, 2.25, 2.66, 2.93, 3.29, 4.62, 3.21, 2.86, 2.86, 2.03, 2.16, &
                    1.78, 2.15, 2.19, 2.69, 2.97, 3.12, 4.49, 3.01, 2.76, 2.78, 1.98, 2.16, &
                    1.65, 1.63, 1.75, 2.33, 2.45, 2.89, 4.34, 3.03, 2.62, 2.35, 1.97, 2.02 /)
                mountain_mesh%pm%lwlapse = (/ 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35 /)
                mountain_mesh%pm%wlapse = (/ 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21 /)
            case (2)

                !> Option 2:
                !>  Tables of monthly lapse rate derived from station data from AEP and EC
                mountain_mesh%pm%plapse = (/ 0.516, 0.306, 0.420, 0.263, 0.084, 0.164, 0.158, 0.219, 0.206, 0.461, 0.528, 0.342 /)
                mountain_mesh%pm%tlapse = (/ &
                    -1.27, -0.45, 0.56, 3.05, 4.56, 4.71, 3.29, 2.83, 2.70, 1.93, 0.96, -0.39, &
                    -1.23, -0.39, 0.56, 2.94, 4.30, 4.60, 3.23, 2.76, 2.49, 1.72, 0.88, -0.34, &
                    -1.25, -0.44, 0.47, 2.73, 4.13, 4.49, 3.06, 2.58, 2.24, 1.57, 0.75, -0.46, &
                    -1.26, -0.48, 0.36, 2.58, 3.87, 4.44, 3.06, 2.46, 2.20, 1.35, 0.63, -0.49, &
                    -1.31, -0.57, 0.37, 2.42, 3.70, 4.36, 3.07, 2.41, 2.10, 1.32, 0.56, -0.50, &
                    -1.35, -0.66, 0.38, 2.32, 3.69, 4.62, 3.21, 2.36, 2.04, 1.22, 0.60, -0.48, &
                    -1.45, -0.59, 0.39, 2.45, 4.68, 5.57, 4.19, 2.68, 2.02, 1.06, 0.54, -0.51, &
                    -1.41, -0.55, 0.49, 3.53, 5.77, 6.02, 5.12, 3.94, 2.69, 1.08, 0.44, -0.55, &
                    -1.46, -0.48, 0.87, 4.12, 6.03, 5.87, 4.95, 4.53, 4.22, 2.00, 0.44, -0.57, &
                    -1.29, -0.37, 0.86, 4.21, 6.09, 5.80, 4.59, 4.45, 4.75, 3.10, 1.06, -0.42, &
                    -1.06, -0.53, 0.46, 4.24, 6.08, 5.83, 4.45, 4.37, 4.99, 3.54, 1.61, -0.18, &
                    -1.19, -1.06, 0.32, 4.29, 6.07, 5.89, 4.43, 4.50, 5.19, 3.85, 1.72, -0.21, &
                    -1.41, -1.27, 0.39, 4.45, 6.23, 6.07, 4.59, 4.71, 5.40, 4.20, 1.95, -0.42, &
                    -1.45, -1.29, 0.50, 4.65, 6.37, 6.16, 4.79, 4.94, 5.56, 4.59, 2.29, -0.45, &
                    -1.14, -0.95, 0.85, 4.94, 6.55, 6.33, 4.91, 5.18, 5.79, 4.98, 2.71, -0.14, &
                    -0.99, -0.60, 1.20, 5.15, 6.81, 6.31, 5.06, 5.42, 6.01, 5.29, 2.77, -0.10, &
                    -1.11, -0.40, 1.57, 5.48, 6.98, 6.31, 5.15, 5.56, 6.30, 5.53, 2.34, -0.51, &
                    -1.46, -0.53, 1.71, 5.65, 7.10, 6.37, 5.11, 5.54, 6.33, 4.79, 1.42, -0.83, &
                    -1.52, -0.94, 1.40, 5.73, 7.19, 6.41, 5.02, 5.28, 5.73, 3.44, 1.23, -0.74, &
                    -1.38, -0.83, 0.85, 4.94, 7.11, 6.39, 4.74, 4.45, 4.15, 2.85, 1.14, -0.65, &
                    -1.31, -0.66, 0.79, 3.90, 6.11, 5.87, 4.08, 3.37, 3.52, 2.58, 1.04, -0.61, &
                    -1.14, -0.56, 0.77, 3.61, 5.28, 5.06, 3.31, 2.98, 3.13, 2.30, 0.96, -0.52, &
                    -1.09, -0.41, 0.78, 3.44, 4.96, 4.82, 3.29, 2.87, 2.96, 2.20, 0.98, -0.54, &
                    -1.12, -0.40, 0.80, 3.23, 4.77, 4.74, 3.29, 2.84, 2.78, 2.02, 0.90, -0.51 /)
                mountain_mesh%pm%dtlapse = (/ &
                    -0.01, 0.48, 2.08, 2.95, 3.05, 4.90, 5.40, 4.77, 3.28, 3.13, 1.96, 0.30, &
                    0.02, 0.48, 2.11, 2.94, 3.13, 5.01, 5.32, 4.63, 3.33, 2.99, 1.93, 0.32, &
                    -0.03, 0.48, 1.95, 2.92, 3.20, 5.05, 5.21, 4.45, 3.27, 3.02, 1.81, 0.19, &
                    0.07, 0.47, 1.81, 2.92, 3.28, 5.01, 5.15, 4.34, 3.35, 2.91, 1.73, 0.12, &
                    0.04, 0.40, 1.79, 2.89, 3.27, 5.01, 5.17, 4.26, 3.37, 2.92, 1.66, 0.14, &
                    -0.01, 0.19, 1.75, 2.82, 3.41, 5.30, 5.23, 4.25, 3.42, 2.84, 1.59, 0.25, &
                    -0.11, 0.31, 1.71, 2.98, 4.02, 5.94, 5.94, 4.54, 3.42, 2.64, 1.51, 0.17, &
                    -0.07, 0.31, 1.69, 3.69, 4.11, 5.99, 6.51, 5.52, 3.97, 2.71, 1.44, 0.13, &
                    -0.10, 0.43, 1.97, 3.87, 3.80, 5.95, 6.58, 5.75, 4.64, 3.33, 1.50, 0.14, &
                    0.03, 0.57, 2.37, 3.99, 3.45, 5.89, 6.68, 5.65, 4.39, 3.81, 1.99, 0.25, &
                    0.32, 0.92, 2.82, 3.85, 3.07, 5.64, 6.95, 5.55, 3.98, 3.89, 2.51, 0.48, &
                    0.61, 1.23, 3.42, 3.74, 2.78, 5.30, 7.17, 5.37, 3.57, 3.72, 2.82, 0.82, &
                    0.92, 1.70, 3.70, 3.52, 2.52, 5.04, 7.09, 5.25, 3.21, 3.51, 3.06, 0.95, &
                    1.18, 2.04, 3.97, 3.20, 2.08, 4.74, 6.96, 5.00, 3.07, 3.30, 3.04, 1.14, &
                    1.52, 2.41, 3.97, 3.01, 1.79, 4.43, 6.80, 4.82, 2.79, 3.12, 3.02, 1.32, &
                    1.44, 2.40, 3.83, 2.78, 1.57, 4.22, 6.67, 4.95, 2.72, 3.01, 2.91, 1.20, &
                    1.14, 2.30, 3.82, 2.72, 1.53, 4.38, 6.44, 4.97, 2.71, 3.00, 2.69, 0.72, &
                    0.61, 1.75, 3.68, 2.71, 1.78, 4.49, 6.78, 5.40, 2.92, 3.10, 2.18, 0.40, &
                    0.26, 1.13, 3.49, 2.75, 1.93, 4.75, 7.67, 6.35, 3.32, 2.99, 1.98, 0.38, &
                    0.21, 0.85, 2.95, 2.93, 2.32, 5.51, 8.37, 7.08, 3.41, 2.89, 1.93, 0.34, &
                    0.18, 0.72, 2.61, 2.64, 2.41, 5.99, 7.92, 6.14, 3.33, 3.01, 1.92, 0.27, &
                    0.22, 0.77, 2.48, 2.71, 2.56, 5.35, 6.57, 5.29, 3.21, 3.08, 1.94, 0.23, &
                    0.19, 0.77, 2.37, 2.79, 2.72, 4.94, 5.84, 4.96, 3.21, 3.10, 1.95, 0.10, &
                    0.21, 0.68, 2.31, 2.87, 2.94, 4.80, 5.54, 4.78, 3.19, 3.11, 1.89, 0.11 /)
                mountain_mesh%pm%lwlapse = &
                    (/ 6.832, 8.647, 4.803, 17.993, 31.842, 28.492, 36.013, 33.234, 25.388, 12.674, 0.288, 13.972 /)
                mountain_mesh%pm%wlapse = (/ 0.26, 0.32, 0.16, 0.22, 0.23, 0.26, 0.22, 0.12, 0.16, 0.13, 0.14, 0.20 /)
            case (3)

                !> Option 3:
                !>  Tables of temperature lapse rate, vapor pressure coefficient
                !>  (Kunkel et al., 1989), and precipitation-elevation
                !>  adjustment factors (Thornton et al., 1997) for each month
                !>  for the Northern Hemisphere. Incoming long wave radiation
                !>  lapse rate of 29 W/m^2/1000 m (Marty et al., 2002).
                mountain_mesh%pm%plapse = (/ 0.35, 0.35, 0.35, 0.30, 0.25, 0.20, 0.20, 0.20, 0.20, 0.25, 0.30, 0.35 /)
                mountain_mesh%pm%tlapse = (/ &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70, &
                    4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70 /)
                mountain_mesh%pm%dtlapse = (/ &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51, &
                    5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51 /)
                mountain_mesh%pm%lwlapse = (/ 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0 /)

                !> Check against 'iwind' (no 'wlapse').
                if (mountain_mesh%pm%iwind == 2) then
                    call print_error( &
                        "'iwind' option 2 cannot be used with ilapse option 3, as the option does not provide 'wlapse'.")
                    ierr = 1
                end if
            case default
                if (mountain_mesh%pm%ipre /= 0) then
                    call print_error("'ipre' is active but cannot be used without any ilapse option to define 'plapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%itemp /= 0) then
                    call print_error("'itemp' is active but cannot be used without any ilapse option to define 'tlapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%ihumd /= 0) then
                    call print_error("'ihumd' is active but cannot be used without any ilapse option to define 'dtlapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%irlds /= 0) then
                    call print_error("'irlds' is active but cannot be used without any ilapse option to define 'lwlapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%iwind == 2) then
                    call print_error("'iwind' option 2 cannot be used without any ilapse option to define 'wlapse'.")
                    ierr = 1
                end if
        end select

        !> Check for errors.
        if (ierr /= 0) then
            call print_error("Errors occurred during the initialization of 'MOUNTAINMESH'.")
            call program_abort()
        else
            call reset_tab()
        end if

        !> Check for required variables.
        if (mountain_mesh%pm%iwind > 0 .and. .not. associated(vs%tile%uv)) then
            call print_error( &
                "'IWIND' is active but the driving variable '" // VN_UV // "' is not active or not associated with an input file.")
            ierr = 1
            if (mountain_mesh%pm%iwind == 1 .and. .not. associated(vs%tile%wdir)) then
                call print_error( &
                    "'IWIND 1' is active but the driving variable '" // VN_WDIR // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%itemp > 0 .and. .not. associated(vs%tile%ta)) then
            call print_error( &
                "'ITEMP' is active but the driving variable '" // VN_TA // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (mountain_mesh%pm%ipres > 0 .and. .not. associated(vs%tile%pres)) then
            call print_error( &
                "'IPRES' is active but the driving variable '" // VN_PRES // &
                "' is not active or not associated with an input file.")
            ierr = 1
            if (.not. associated(vs%tile%ta)) then
                call print_error( &
                    "'IPRES' is active but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%ihumd > 0 .and. .not. associated(vs%tile%qa)) then
            call print_error( &
                "'IHUMD' is active but the driving variable '" // VN_QA // "' is not active or not associated with an input file.")
            ierr = 1
            if (.not. associated(vs%tile%ta)) then
                call print_error( &
                    "'IHUMD' is active but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (.not. associated(vs%tile%pres)) then
                call print_error( &
                    "'IHUMD' is active but the driving variable '" // VN_PRES // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%irlds > 0 .and. .not. associated(vs%tile%flin)) then
            call print_error( &
                "'IRLDS' is active but the driving variable '" // VN_FLIN // &
                "' is not active or not associated with an input file.")
            ierr = 1
            if (mountain_mesh%pm%irlds == 1 .and. .not. associated(vs%tile%ta)) then
                call print_error( &
                    "'IRLDS 1' is active but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%ipre > 0 .and. .not. associated(vs%tile%pre)) then
            call print_error( &
                "'IPRE' is active but the driving variable '" // VN_PRE // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (mountain_mesh%pm%iphase > 0) then
            if (.not. associated(vs%tile%pre)) then
                call print_error( &
                    "'IPHASE' is active but the driving variable '" // VN_PRE // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (.not. associated(vs%tile%ta)) then
                call print_error( &
                    "'IPRE' is active with 'IPHASE' but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (ierr == 0) then
                if (.not. associated(vs%tile%prern)) allocate(vs%tile%prern(vs%tile%dim_length))
                if (.not. associated(vs%tile%presno)) allocate(vs%tile%presno(vs%tile%dim_length))
                if (associated(vs%grid)) then
                    if (.not. associated(vs%grid%prern)) allocate(vs%grid%prern(vs%grid%dim_length))
                    if (.not. associated(vs%grid%presno)) allocate(vs%grid%presno(vs%grid%dim_length))
                end if
            end if
        end if
        if (mountain_mesh%pm%irsrd > 0 .and. .not. associated(vs%tile%fsin)) then
            call print_error( &
                "'IRSRD' is active but the driving variable '" // VN_FSIN // &
                "' is not active or not associated with an input file.")
            ierr = 1
            if (mountain_mesh%pm%irsrd == 1 .and. .not. associated(vs%tile%pres)) then
                call print_error( &
                    "'IRSRD 1' is active but the driving variable '" // VN_PRES // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (ierr /= 0) then
            call reset_tab()
            call print_error( &
                "The variables required to drive the model are not active or have not been associated with an input file.")
            call program_abort()
        end if

    end subroutine

    subroutine mountain_within_tile(fls, shd, cm)

        !> Required for MESH variables.
        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(clim_info) :: cm

        !> Local variables.
        integer k
        real, dimension(il1:il2) :: rsrd_adjusted, rsds_in
        real, dimension(il1:il2) :: rlds_adjusted, rlds_in
        real, dimension(il1:il2) :: temp_adjusted, temp_in
        real, dimension(il1:il2) :: pres_adjusted, pres_in
        real, dimension(il1:il2) :: humd_adjusted, humd_in
        real, dimension(il1:il2) :: rain_adjusted, rain_in
        real, dimension(il1:il2) :: rain_phased_adjusted
        real, dimension(il1:il2) :: snow_phased_adjusted
        real, dimension(il1:il2) :: wind_adjusted, wind_in, winddir_in

        !> Return if module is not enabled.
        if (.not. mountain_mesh%PROCESS_ACTIVE) return

        !> Transfer variables.
        if (associated(vs%tile%fsin)) rsds_in(il1:il2) = vs%tile%fsin(il1:il2)
        if (associated(vs%tile%flin)) rlds_in(il1:il2) = vs%tile%flin(il1:il2)
        if (associated(vs%tile%ta)) temp_in(il1:il2) = vs%tile%ta(il1:il2)
        if (associated(vs%tile%pres)) pres_in(il1:il2) = vs%tile%pres(il1:il2)
        if (associated(vs%tile%qa)) humd_in(il1:il2) = vs%tile%qa(il1:il2)
        if (associated(vs%tile%pre)) rain_in(il1:il2) = vs%tile%pre(il1:il2)
        if (associated(vs%tile%uv)) wind_in(il1:il2) = vs%tile%uv(il1:il2)
        if (associated(vs%tile%wdir)) winddir_in(il1:il2) = vs%tile%wdir(il1:il2)

        !> Call routine to calculate adjusted radiation value.
        call forcing_adjust( &
            mountain_mesh%vs%elev(il1:il2), mountain_mesh%vs%xlng(il1:il2), &
            mountain_mesh%vs%ylat(il1:il2), mountain_mesh%vs%slope(il1:il2), &
            mountain_mesh%vs%aspect(il1:il2), mountain_mesh%vs%delta(il1:il2), &
            mountain_mesh%vs%delta_elevmax(il1:il2), &
            mountain_mesh%vs%curvature(il1:il2), iln, &
            shd%lc%ILMOS(il1:il2), i1, i2, &
            mountain_mesh%pm%CurveWeight, &
            mountain_mesh%pm%CalcFreq, &
            mountain_mesh%pm%ipre, mountain_mesh%pm%itemp, mountain_mesh%pm%ipres, &
            mountain_mesh%pm%ihumd, mountain_mesh%pm%irlds, mountain_mesh%pm%iwind, &
            mountain_mesh%pm%iphase, mountain_mesh%pm%irsrd, mountain_mesh%pm%idecl, &
            mountain_mesh%pm%tlapse, mountain_mesh%pm%plapse, mountain_mesh%pm%dtlapse, &
            mountain_mesh%pm%lwlapse, mountain_mesh%pm%wlapse, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            rsds_in(il1:il2), &
            rlds_in(il1:il2), &
            temp_in(il1:il2), &
            pres_in(il1:il2), &
            humd_in(il1:il2), &
            rain_in(il1:il2), &
            wind_in(il1:il2), &
            winddir_in(il1:il2), &
            rsrd_adjusted(il1:il2), &
            rlds_adjusted(il1:il2), &
            temp_adjusted(il1:il2), &
            pres_adjusted(il1:il2), &
            humd_adjusted(il1:il2), &
            rain_adjusted(il1:il2), &
            rain_phased_adjusted(il1:il2), &
            snow_phased_adjusted(il1:il2), &
            wind_adjusted(il1:il2), &
            ic%now%year, ic%now%month, ic%now%jday, &
            ic%now%hour, ic%now%mins, ic%dtmins)

        !> Update radiation.
        !> Must update 'GRD' separately for output (e.g., energy_balance.csv).
        if (associated(vs%tile%fsin)) vs%tile%fsin(il1:il2) = rsrd_adjusted(il1:il2)
        if (associated(vs%tile%flin)) vs%tile%flin(il1:il2) = rlds_adjusted(il1:il2)
        if (associated(vs%tile%ta)) vs%tile%ta(il1:il2) = temp_adjusted(il1:il2)
        if (associated(vs%tile%pres)) vs%tile%pres(il1:il2) = pres_adjusted(il1:il2)
        if (associated(vs%tile%qa)) vs%tile%qa(il1:il2) = humd_adjusted(il1:il2)
        if (associated(vs%tile%pre)) vs%tile%pre(il1:il2) = rain_adjusted(il1:il2)
        if (associated(vs%tile%prern)) vs%tile%prern(il1:il2) = rain_phased_adjusted(il1:il2)
        if (associated(vs%tile%presno)) vs%tile%presno(il1:il2) = snow_phased_adjusted(il1:il2)
        if (associated(vs%tile%uv)) vs%tile%uv(il1:il2) = wind_adjusted(il1:il2)
        if (associated(vs%grid%fsin)) vs%grid%fsin = 0.0
        if (associated(vs%grid%flin)) vs%grid%flin = 0.0
        if (associated(vs%grid%ta)) vs%grid%ta = 0.0
        if (associated(vs%grid%pres)) vs%grid%pres = 0.0
        if (associated(vs%grid%qa)) vs%grid%qa = 0.0
        if (associated(vs%grid%pre)) vs%grid%pre = 0.0
        if (associated(vs%grid%prern)) vs%grid%prern = 0.0
        if (associated(vs%grid%presno)) vs%grid%presno = 0.0
        if (associated(vs%grid%uv)) vs%grid%uv = 0.0
        do k = il1, il2
            if (associated(vs%grid%fsin)) then
                vs%grid%fsin(shd%lc%ILMOS(k)) = vs%grid%fsin(shd%lc%ILMOS(k)) + &
                    rsrd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%flin)) then
                vs%grid%flin(shd%lc%ILMOS(k)) = vs%grid%flin(shd%lc%ILMOS(k)) + &
                    rlds_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%ta)) then
                vs%grid%ta(shd%lc%ILMOS(k)) = vs%grid%ta(shd%lc%ILMOS(k)) + &
                    temp_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%pres)) then
                vs%grid%pres(shd%lc%ILMOS(k)) = vs%grid%pres(shd%lc%ILMOS(k)) + &
                    pres_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%qa)) then
                vs%grid%qa(shd%lc%ILMOS(k)) = vs%grid%qa(shd%lc%ILMOS(k)) + &
                    humd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%pre)) then
                vs%grid%pre(shd%lc%ILMOS(k)) = vs%grid%pre(shd%lc%ILMOS(k)) + &
                    rain_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%prern)) then
                vs%grid%prern(shd%lc%ILMOS(k)) = vs%grid%prern(shd%lc%ILMOS(k)) + &
                    rain_phased_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%presno)) then
                vs%grid%presno(shd%lc%ILMOS(k)) = vs%grid%presno(shd%lc%ILMOS(k)) + &
                    snow_phased_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (associated(vs%grid%uv)) then
                vs%grid%uv(shd%lc%ILMOS(k)) = vs%grid%uv(shd%lc%ILMOS(k)) + &
                    wind_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
        end do

    end subroutine

end module
