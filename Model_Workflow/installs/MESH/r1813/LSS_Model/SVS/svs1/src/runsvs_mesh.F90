module runsvs_mesh

    !> MESH modules.
    !*  mpi_module: Required for 'il1' and 'il2' indexing.
    !*  model_files_variables: Required for 'fls' object.
    !*  sa_mesh_common: Required for MESH variables and common routines.
    !*  model_dates: Required for 'ic' counter.
    use mpi_module
    use model_files_variables
    use sa_mesh_common
!todo: Replace 'cm' instance with 'vs' counterparts.
    use climate_forcing
    use model_dates

    !> SVS modules (created for MESH).
    !*  runsvs_mod: Required for 'bus' variable.
    !*  runsvs_utils: Required for 'RUNSVS_OPT' variables and 'surflayerheight' and 'compvirttemp' functions.
    use runsvs_mod
    use runsvs_utils

    !> SVS modules.
!    use phy_options
    use svs_configs
    use sfc_options

    !> To re-use 'op' variable.
!todo: Replace with a generic structure to remove dependency between sub-models.
    use RUNCLASS36_save_output, only: WF_NUM_POINTS, op

    implicit none

!    integer, parameter :: fid_ini = 50
!    integer, parameter :: fid_met = 51
!    integer, parameter :: fid_out = 52

!    integer nt      ! Number of time steps
!    integer dateo   ! Starting date of the run
!    integer houro   ! Starting hour of the run
!    real :: dt = 1800.0        ! Time step duration (sec)
!    real :: sigma_u = 0.995    ! Sigma level of momentum forcing
!    real :: sigma_t = 0.995    ! Sigma level of scalar forcings
!    integer xcount  ! Number of columns in the grid
!    integer ycount  ! Number of lines in the grid
!    logical :: observed_forcing = .false.
!    character(len = 255) :: inifile = ''
!    character(len = 255) :: interpfile = ''
!    character(len = 255) :: geofile = ''
!    character(len = 255) :: metfile = ''
!    character(len = 255) :: outfile = ''
!    character(len = 255) :: rtefile = ''

!    integer, parameter :: bussiz = runsvs_busdim
!    real bus(bussiz)
    integer bussiz
    real, dimension(:), allocatable :: bus
    integer datecmc_o, date_f, hour_f
!    integer datecmc_v, date_v, hour_v, istat, bidon
!    integer kount
!    real(kind = 8) kdt

!>>>svs_output
!    integer :: iout_dly = 151, iout_hly = 152, iout_ts = 153 !150+N is output unit number for CLASS output; should be able to recycle its use
!    integer :: iout_wat_bal = 157
!    real preacc_dly, preacc_hly, preacc_tot, runoff_acc, wsoil_tot, isoil_tot
!    real bal_in_out, stock, bal_tot, bal_pre, wsoil_ini
!<<<svs_output

    !> SVS constants.
    type runsvs_mesh_constants
        integer :: NLANDCLASS = NCLASS
        real, dimension(NCLASS) :: Z0DAT = (/ &
            0.001, 0.001, 0.001, 1.75, 2.0, 1.0, 2.0, 3.0, 0.8, 0.1, &
            0.2, 0.2, 0.1, 0.1, 0.15, 0.15, 0.35, 0.25, 0.1, 0.25, &
            5.0, 0.1, 0.1, 0.1, 1.75, 0.5 /)
    end type

    !> SVS variables names for I/O (direct variables).
    character(len = *), parameter, public :: VN_SVS_DEGLAT = 'DEGLAT'
    character(len = *), parameter, public :: VN_SVS_DEGLNG = 'DEGLNG'
    character(len = *), parameter, public :: VN_SVS_OBSERVED_FORCING = 'OBSERVED_FORCING'
    character(len = *), parameter, public :: VN_SVS_ZUSL = 'ZUSL'
    character(len = *), parameter, public :: VN_SVS_ZTSL = 'ZTSL'
    character(len = *), parameter, public :: VN_SVS_SIGMA_U = 'SIGMA_U'
    character(len = *), parameter, public :: VN_SVS_SIGMA_T = 'SIGMA_T'
    character(len = *), parameter, public :: VN_SVS_SLOP = 'SLOP'
    character(len = *), parameter, public :: VN_SVS_DRAINDENS = 'DRAINDENS'
    character(len = *), parameter, public :: VN_SVS_SOILTEXT = 'SOILTEXT'
    character(len = *), parameter, public :: VN_SVS_KHYD = 'KHYD'
    character(len = *), parameter, public :: VN_SVS_SAND = 'SAND'
    character(len = *), parameter, public :: VN_SVS_CLAY = 'CLAY'
    character(len = *), parameter, public :: VN_SVS_WSOIL = 'WSOIL'
    character(len = *), parameter, public :: VN_SVS_ISOIL = 'ISOIL'
    character(len = *), parameter, public :: VN_SVS_KTHERMAL = 'KTHERMAL'
    character(len = *), parameter, public :: VN_SVS_TGROUND = 'TGROUND'
    character(len = *), parameter, public :: VN_SVS_VF = 'VF'
    character(len = *), parameter, public :: VN_SVS_Z0V = 'Z0V'
    character(len = *), parameter, public :: VN_SVS_LNZ0 = 'LNZ0'
    character(len = *), parameter, public :: VN_SVS_TVEGE = 'TVEGE'
    character(len = *), parameter, public :: VN_SVS_WVEG = 'WVEG'
    character(len = *), parameter, public :: VN_SVS_TSNOW = 'TSNOW'
    character(len = *), parameter, public :: VN_SVS_SNODPL = 'SNODPL'
    character(len = *), parameter, public :: VN_SVS_SNODEN = 'SNODEN'
    character(len = *), parameter, public :: VN_SVS_SNOAL = 'SNOAL'
    character(len = *), parameter, public :: VN_SVS_WSNOW = 'WSNOW'
    character(len = *), parameter, public :: VN_SVS_TSNOWVEG = 'TSNOWVEG'
    character(len = *), parameter, public :: VN_SVS_SNVDP = 'SNVDP'
    character(len = *), parameter, public :: VN_SVS_SNVDEN = 'SNVDEN'
    character(len = *), parameter, public :: VN_SVS_SNVAL = 'SNVAL'
    character(len = *), parameter, public :: VN_SVS_WSNV = 'WSNV'

    !> SVS variables names for I/O (modifiers/special conditions).
    character(len = *), parameter, public :: VN_SVS_SAND_N = 'SAND_N'
    character(len = *), parameter, public :: VN_SVS_CLAY_N = 'CLAY_N'
    character(len = *), parameter, public :: VN_SVS_WSOIL_N = 'WSOIL_N'
    character(len = *), parameter, public :: VN_SVS_ISOIL_N = 'ISOIL_N'
    character(len = *), parameter, public :: VN_SVS_TGROUND_N = 'TGROUND_N'
    character(len = *), parameter, public :: VN_SVS_VF_N = 'VF_N'
    character(len = *), parameter, public :: VN_SVS_Z0V_N = 'Z0V_N'
    character(len = *), parameter, public :: VN_SVS_TVEGE_N = 'TVEGE_N'
    character(len = *), parameter, public :: VN_SVS_TSNOW_N = 'TSNOW_N'
    character(len = *), parameter, public :: VN_SVS_TSNOWVEG_N = 'TSNOWVEG_N'

    !> SVS variables (for I/O).
    type runsvs_mesh_variables
        real, dimension(:), allocatable :: deglat
        real, dimension(:), allocatable :: deglng
        logical :: observed_forcing = .false.
        real, dimension(:), allocatable :: zusl
        real, dimension(:), allocatable :: ztsl
        real :: sigma_u = 0.995
        real :: sigma_t = 0.995
        real, dimension(:), allocatable :: slop
        real, dimension(:), allocatable :: draindens
        character(len = DEFAULT_FIELD_LENGTH) :: soiltext = 'NIL'
        integer :: khyd = 6
        real, dimension(:, :), allocatable :: sand
        real, dimension(:, :), allocatable :: clay
        real, dimension(:, :), allocatable :: wsoil
        real, dimension(:, :), allocatable :: isoil
        integer :: kthermal = 2
        real, dimension(:, :), allocatable :: tground
        real, dimension(:, :), allocatable :: vf
        real, dimension(:, :), allocatable :: z0v
        real, dimension(:), allocatable :: lnz0
        real, dimension(:, :), allocatable :: tvege
        real, dimension(:), allocatable :: wveg
        real, dimension(:, :), allocatable :: tsnow
        real, dimension(:), allocatable :: snodpl
        real, dimension(:), allocatable :: snoden
        real, dimension(:), allocatable :: snoal
        real, dimension(:), allocatable :: wsnow
        real, dimension(:, :), allocatable :: tsnowveg
        real, dimension(:), allocatable :: snvdp
        real, dimension(:), allocatable :: snvden
        real, dimension(:), allocatable :: snval
        real, dimension(:), allocatable :: wsnv
    end type

    !* PROCESS_ACTIVE: Variable to enable SVS.
    type runsvs_mesh_container
        logical :: PROCESS_ACTIVE = .false.
        type(runsvs_mesh_constants) c
        type(runsvs_mesh_variables) vs
    end type

    type(runsvs_mesh_container), save, public :: svs_mesh

    private

    public &
        runsvs_mesh_init, runsvs_mesh_resume_states_seq, runsvs_mesh_within_tile, runsvs_mesh_save_states_seq, runsvs_mesh_finalize

    contains

    subroutine runsvs_mesh_init(shd, fls, cm)

        !> MESH modules.
        !*  FLAGS: Required for 'RESUMEFLAG'.
!-        use FLAGS, only: RESUMEFLAG

        !> SVS modules.
!        use runsvs_mod
!        use runsvs_utils
!        use phy_options
!        use svs_configs
!        use sfc_options
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

!#include "options.cdk"
#include "isbapar.cdk"
!#include "surfcon.cdk"
#include "thermoconsts.inc"

        integer iun, isvs, istat, k, ki, kj, j, jj, m, i, z
        real tempz0(svs_mesh%c%NLANDCLASS), sumfcanz0
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) level

        integer dateo(1), houro
        integer, external :: newdate
!        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not marked active.
        if (.not. svs_mesh%PROCESS_ACTIVE) then
            return
        else
            call print_new_section("RUNSVS (SVS1) is active.")
            call increase_tab()
        end if

        !> Check for required variables.
        z = 0
        if (.not. associated(vs%tile%fsin)) then
            call print_error("The driving variable '" // VN_FSIN // "' is not active or not associated with an input file.")
            z = 1
        end if
        if (.not. associated(vs%tile%flin)) then
            call print_error("The driving variable '" // VN_FLIN // "' is not active or not associated with an input file.")
            z = 1
        end if
        if (.not. associated(vs%tile%ta)) then
            call print_error("The driving variable '" // VN_TA // "' is not active or not associated with an input file.")
            z = 1
        end if
        if (.not. associated(vs%tile%qa)) then
            call print_error("The driving variable '" // VN_QA // "' is not active or not associated with an input file.")
            z = 1
        end if
        if (.not. associated(vs%tile%pres)) then
            call print_error("The driving variable '" // VN_PRES // "' is not active or not associated with an input file.")
            z = 1
        end if
        if (.not. associated(vs%tile%uv)) then
            call print_error("The driving variable '" // VN_UV // "' is not active or not associated with an input file.")
            z = 1
        end if
        if (.not. (associated(vs%tile%prern) .and. associated(vs%tile%presno)) .and. .not. associated(vs%tile%pre)) then
            call print_error( &
                "No driving variable for precipitation is active nor associated with an input file. The '" // VN_PRE // &
                "' variable or both the '" // VN_PRERN // "' and '" // VN_PRESNO // "' variables are required.")
            z = 1
        else if (associated(vs%tile%prern) .and. associated(vs%tile%presno) .and. associated(vs%tile%pre)) then
            call print_info( &
                "The '" // VN_PRERN // "' and '" // VN_PRESNO // "' variables are active. The '" // VN_PRE // &
                "' variable is also active but inputs on the field are not being used.")
        end if
        if (z /= 0) then
            call reset_tab()
            call print_error( &
                "The variables required to drive the model are not active or have not been associated with an input file.")
            call program_abort()
        end if

        !> Initialize common blocks, read options and configuration file.
        sigma_u = svs_mesh%vs%sigma_u
        sigma_t = svs_mesh%vs%sigma_t
        observed_forcing = svs_mesh%vs%observed_forcing
        soiltext = svs_mesh%vs%soiltext

        if (soiltext == 'NIL') then
            nl_svs = shd%lc%IGND
            allocate(dl_svs(nl_svs))
            dl_svs = shd%lc%sl%zbot
        else
            nl_svs = NL_SVS_DEFAULT
            dl_svs = DP_SVS_DEFAULT
        end if
        KHYD = svs_mesh%vs%khyd

        call svs_bus_init(il2 - il1 + 1)
        bussiz = runsvs_busdim
        allocate(bus(bussiz))
        bus = 0.0

        call init_soil_text_levels()

        !> Parse CLASS variables to bus.
        do k = 0, NG - 1

            !> MESH grid.
            ki = shd%lc%ILMOS(il1 + k)

            !> MESH GRU.
            kj = shd%lc%JLMOS(il1 + k)

            !> Determine the index to pull from the 'vs' input variable group.
            if (SHDFILEFMT == 2) then
                isvs = il1 + k
            else
                isvs = kj
            end if

            !> Convert lat, lon to Radian.
            !> We need to give SVS the true longitude (from 0 to 360), not negative values.
!            bus(dlat + k) = ((shd%yOrigin + shd%yDelta*shd%yyy(ki)) - shd%yDelta/2.0)*PI/180.0
!            bus(dlon + k) = ((shd%xOrigin + shd%xDelta*shd%xxx(ki)) - shd%xDelta/2.0)*PI/180.0
            bus(dlat + k) = shd%ylat(ki)*PI/180.0
            if (allocated(svs_mesh%vs%deglat)) bus(dlat + k) = svs_mesh%vs%deglat(isvs)*PI/180.0
            if (shd%xlng(ki) < 0.0) then
                bus(dlon + k) = (shd%xlng(ki) + 360.0)*PI/180.0
            else
                bus(dlon + k) = shd%xlng(ki)*PI/180.0
            end if
            if (allocated(svs_mesh%vs%deglng)) then
                if (svs_mesh%vs%deglng(isvs) < 0.0) then
                    bus(dlon + k) = (svs_mesh%vs%deglng(isvs) + 360.0)*PI/180.0
                else
                    bus(dlon + k) = svs_mesh%vs%deglng(isvs)*PI/180.0
                end if
            end if

            !> Map CLASS parameters to SVS parameters.
            !* zusl: Height of wind forcing.
            !* ztsl: Height of temperature forcing.
            if (observed_forcing) then
                bus(zusl + k) = pm%tile%zrfm(il1 + k)
                if (allocated(svs_mesh%vs%zusl)) bus(zusl + k) = svs_mesh%vs%zusl(isvs)
                bus(ztsl + k) = pm%tile%zrfh(il1 + k)
                if (allocated(svs_mesh%vs%ztsl)) bus(ztsl + k) = svs_mesh%vs%ztsl(isvs)
            end if

            !> Parameters.
            !* vegf+   3*NG: Needleleaf evergreen.
            !* vegf+   6*NG: Broadleaf deciduous.
            !* vegf+  14*NG: Crops.
            !* vegf+  13*NG: Grass.
            !* vegf+  20*NG: Urban.
            !* slop: Subgrid-scale slope.
            !* draindens: Drainage density (km/km2 converted to m/m2 but provided already by CLASS in m/m2).
            !* rootdp: Max depth of root zone.
            if (allocated(svs_mesh%vs%vf)) then
                do m = 1199, 1174, -1
                    bus(vegf + (1199 - m)*NG + k) = svs_mesh%vs%vf(isvs, 1200 - m)
                end do
            else
                bus(vegf + 3*NG + k) = pm%tile%fcan(il1 + k, 1)
                bus(vegf + 6*NG + k) = pm%tile%fcan(il1 + k, 2)
                bus(vegf + 14*NG + k) = pm%tile%fcan(il1 + k, 3)
                bus(vegf + 13*NG + k) = pm%tile%fcan(il1 + k, 4)
                bus(vegf + 20*NG + k) = pm%tile%fcan(il1 + k, 5)
            end if

! EG_MOD for 100% water pixels, assign another class of vegetation instead (here crops=class15)
!	    if (bus(vegf + 2*NG + k) == 1.0) then
!	       bus(vegf + 2*NG + k) = 0.0
!	       bus(vegf + 14*NG + k) = 1.0
!	    end if
! FIN EG_MOD

            bus(slop + k) = min(max(pm%tile%xslp(il1 + k), 0.005), 1.0)
            if (allocated(svs_mesh%vs%slop)) bus(slop + k) = min(max(svs_mesh%vs%slop(isvs), 0.005), 1.0)
            bus(draindens + k) = pm%tile%dd(il1 + k)!*0.001
            if (allocated(svs_mesh%vs%draindens)) bus(draindens + k) = svs_mesh%vs%draindens(isvs)
!            bus(rootdp + k) = max(pm%tile%sdep(il1 + k), 0.5)
!            bus(rootdp + k) = max(shd%lc%sl%zbot(nl_svs), 0.5)

            !> Compute weighted average of log z0 wrt vegetation
            !> (used for momentum only - local z0 used for temperature/humidity).
            if (allocated(svs_mesh%vs%lnz0)) then
                bus(z0 + k) = svs_mesh%vs%lnz0(isvs)
            else
                bus(z0 + k) = 0.0
                sumfcanz0 = 0.0
                do j = 1, 5
                    bus(z0 + k) = bus(z0 + k) + pm%tile%fcan(il1 + k, j)*pm%tile%lnz0(il1 + k, j)
                    sumfcanz0 = sumfcanz0 + pm%tile%fcan(il1 + k, j)
                end do
                if (sumfcanz0 > 0.0) then
                    bus(z0 + k) = bus(z0 + k)/sumfcanz0
                else
                    bus(z0 + k) = 0.0
                end if
            end if
            if (bus(z0 + k) == 0.0) then
                tempz0 = svs_mesh%c%Z0DAT
                if (allocated(svs_mesh%vs%z0v)) tempz0 = svs_mesh%vs%z0v(isvs, :)
                bus(z0 + k) = 0.0
                sumfcanz0 = 0.0
                do m = 1199, 1174, -1
                    bus(z0 + k) = bus(z0 + k) + bus(vegf + (1199 - m)*NG + k)*tempz0(1200 - m)
                    sumfcanz0 = sumfcanz0 + bus(vegf + (1199 - m)*NG + k)
                end do
                if (sumfcanz0 > 0.0) then
                    bus(z0 + k) = bus(z0 + k)/sumfcanz0
                else
                    bus(z0 + k) = 0.0
                end if
            else
                bus(z0 + k) = exp(bus(z0 + k))
            end if
            bus(z0t + k) = bus(z0 + k)

            !> For soil texture we ignore negative numbers
            !> which signal special soils (organic/impermeable/glaciers).
            !> Map soil texture.
            !> IGND == 3 (CLASS traditional)
            !>       1              1-2
            !>       2               3
            !>       3              4-7
            !> IGND >= 5
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2              3-4
            !>       3              5
            !>       4              6
            !>       5              7
!            bus(sand + k) = max(pm%tile%sand(il1 + k, 1), 0.0)
!            bus(sand + NG + k) = max(pm%tile%sand(il1 + k, 1), 0.0)
!            bus(sand + 2*NG + k) = max(pm%tile%sand(il1 + k, 2), 0.0)
!            bus(clay + k) = max(pm%tile%clay(il1 + k, 1), 0.0)
!            bus(clay + NG + k) = max(pm%tile%clay(il1 + k, 1), 0.0)
!            bus(clay + 2*NG + k) = max(pm%tile%clay(il1 + k, 2), 0.0)
!            if (shd%lc%IGND >= 5) then
!                bus(sand + 3*NG + k) = max(pm%tile%sand(il1 + k, 2), 0.0)
!                bus(sand + 4*NG + k) = max(pm%tile%sand(il1 + k, 3), 0.0)
!                bus(sand + 5*NG + k) = max(pm%tile%sand(il1 + k, 4), 0.0)
!                bus(sand + 6*NG + k) = max(pm%tile%sand(il1 + k, 5), 0.0)
!                bus(clay + 3*NG + k) = max(pm%tile%clay(il1 + k, 2), 0.0)
!                bus(clay + 4*NG + k) = max(pm%tile%clay(il1 + k, 3), 0.0)
!                bus(clay + 5*NG + k) = max(pm%tile%clay(il1 + k, 4), 0.0)
!                bus(clay + 6*NG + k) = max(pm%tile%clay(il1 + k, 5), 0.0)
!            else
!                do j = 3, 6
!                    bus(sand + j*NG + k) = max(pm%tile%sand(il1 + k, 3), 0.0)
!                    bus(clay + j*NG + k) = max(pm%tile%clay(il1 + k, 3), 0.0)
!                end do
!            end if
!            do j = 1, nl_svs ! model layers
            do j = 1, nl_stp ! soil texture levels
                bus(sand + (j - 1)*NG + k) = max(pm%tile%sand(il1 + k, j), 0.0)
                if (allocated(svs_mesh%vs%sand)) bus(sand + (j - 1)*NG + k) = svs_mesh%vs%sand(isvs, j)
                bus(clay + (j - 1)*NG + k) = max(pm%tile%clay(il1 + k, j), 0.0)
                if (allocated(svs_mesh%vs%clay)) bus(clay + (j - 1)*NG + k) = svs_mesh%vs%clay(isvs, j)
            end do

            !> Map soil moisture.
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2               3
            !>       3              4-7
!            bus(wsoil + k) = vs%tile%thlqsol(il1 + k, 1)
!            bus(wsoil + NG + k) = vs%tile%thlqsol(il1 + k, 2)
!            bus(wsoil + 2*NG + k) = vs%tile%thlqsol(il1 + k, 3)
!            do j = 3, 6
!                bus(wsoil + j*NG + k) = vs%tile%thlqsol(il1 + k, 3)
!            end do

! EG_MOD: WE HAVE TO INITIALIZE SOIL MOISTURE FOR ALL SVS LAYERS, NOT ONLY THE KHYD FIRST LAYERS
!            do j = 1, KHYD ! permeable layers, min from runsvs_init
            do j = 1, nl_svs
! EG_MOD: WFC IS NOT KNOWN AT THIS POINT (COMPUTED LATER BY INISOILI_SVS); USE CRITWATER INSTEAD
!                bus(wsoil + (j - 1)*NG + k) = max(vs%tile%thlqsol(il1 + k, j), bus(wfc + k))
                bus(wsoil + (j - 1)*NG + k) = max(vs%tile%thlqsol(il1 + k, j), CRITWATER)
                if (allocated(svs_mesh%vs%wsoil)) bus(wsoil + (j - 1)*NG + k) = max(svs_mesh%vs%wsoil(isvs, j), CRITWATER)
                bus(isoil + (j - 1)*NG + k) = vs%tile%thicsol(il1 + k, j)
                if (allocated(svs_mesh%vs%isoil)) bus(isoil + (j - 1)*NG + k) = svs_mesh%vs%isoil(isvs, j)
            end do

            !> Map soil temperature.
            !> CLASS layer  <->  SVS layer
            !>       1               1
            !>       2               2
!            bus(tsoil + k) = vs%tile%tsol(il1 + k, 1)! + tcdk
!            bus(tsoil + NG + k) = vs%tile%tsol(il1 + k, 2)! + tcdk
            bus(tground + k) = vs%tile%tsol(il1 + k, 1)! + tcdk
            bus(tground + NG + k) = vs%tile%tsol(il1 + k, 2)! + tcdk
            if (allocated(svs_mesh%vs%tground)) then
                do j = 1, svs_mesh%vs%kthermal
                    bus(tground + (j - 1)*NG + k) = svs_mesh%vs%tground(isvs, j)
                end do
            end if

            !> Map vegetation temperature.
            do j = 0, 1
                bus(tvege + j*NG + k) = vs%tile%tcan(il1 + k)! + tcdk
                if (allocated(svs_mesh%vs%tvege)) bus(tvege + j*NG + k) = svs_mesh%vs%tvege(isvs, j + 1)
            end do
            bus(wveg + k) = vs%tile%lqwscan(il1 + k)
            if (allocated(svs_mesh%vs%wveg)) bus(wveg + k) = svs_mesh%vs%wveg(isvs)

            !> Map snow properties.
            !* snoro: Density (kg/m3) to relative density wrt ice.
            if (vs%tile%sno(il1 + k) > 0.0) then
                do j = 0, 1
                    bus(tsnow + j*NG + k) = vs%tile%tsno(il1 + k)! + tcdk
                end do
                bus(snodpl + k) = vs%tile%rhosno(il1 + k)*vs%tile%sno(il1 + k)
                bus(snoden + k) = vs%tile%rhosno(il1 + k)
                bus(snoal + k) = vs%tile%albsno(il1 + k)
                bus(wsnow + k) = vs%tile%lqwssno(il1 + k)
                do j = 0, 1
                    bus(tsnowveg + j*NG + k) = vs%tile%tsno(il1 + k)! + tcdk
                end do
                bus(snvdp + k) = vs%tile%rhosno(il1 + k)*vs%tile%sno(il1 + k)
                bus(snvden + k) = vs%tile%rhosno(il1 + k)
                bus(snval + k) = vs%tile%albsno(il1 + k)
                bus(wsnv + k) = vs%tile%lqwssno(il1 + k)
            end if
            if (allocated(svs_mesh%vs%snodpl)) then
                if (svs_mesh%vs%snodpl(isvs) > 0.0) then
                    do j = 0, 1
                        if (allocated(svs_mesh%vs%tsnow)) bus(tsnow + j*NG + k) = svs_mesh%vs%tsnow(isvs, j + 1)
                    end do
                    bus(snodpl + k) = svs_mesh%vs%snodpl(isvs)
                    if (allocated(svs_mesh%vs%snoden)) bus(snoden + k) = svs_mesh%vs%snoden(isvs)
                    if (allocated(svs_mesh%vs%snoal)) bus(snoal + k) = svs_mesh%vs%snoal(isvs)
                    if (allocated(svs_mesh%vs%wsnow)) bus(wsnow + k) = svs_mesh%vs%wsnow(isvs)
                end if
            end if
            if (allocated(svs_mesh%vs%snvdp)) then
                if (svs_mesh%vs%snvdp(isvs) > 0.0) then
                    do j = 0, 1
                        if (allocated(svs_mesh%vs%tsnowveg)) bus(tsnowveg + j*NG + k) = svs_mesh%vs%tsnowveg(isvs, j + 1)
                    end do
                    bus(snvdp + k) = svs_mesh%vs%snvdp(isvs)
                    if (allocated(svs_mesh%vs%snvden)) bus(snvden + k) = svs_mesh%vs%snvden(isvs)
                    if (allocated(svs_mesh%vs%snval)) bus(snval + k) = svs_mesh%vs%snval(isvs)
                    if (allocated(svs_mesh%vs%wsnv)) bus(wsnv + k) = svs_mesh%vs%wsnv(isvs)
                end if
            end if

            !> Derived properties.
            bus(snoma + k) = bus(snodpl + k)*bus(snoden + k)
            bus(snoro + k) = max(bus(snoden + k)/900.0, 0.1)
            bus(snvma + k) = bus(snvdp + k)*bus(snvden + k)
            bus(snvro + k) = max(bus(snvden + k)/900.0, 0.1)
        end do

        !> Summary.
        if (DIAGNOSEMODE) then
            call reset_tab()
            call print_new_section('--------------------------------')
            call print_message('SVS DIAGNOSTICS')
            call print_message('--------------------------------')
            write(line, "('TILE:             ', i8)") 1
            call print_message(line)
            call print_message('--------------------------------')
            write(line, "('LATITUDE:         ', f10.1)") bus(dlat)*180.0/PI
            call print_message(line)
            write(line, "('LONGITUDE:        ', f10.1)") bus(dlon)*180.0/PI
            call print_message(line)
            call print_message('--------------------------------')
            write(line, "('ROUGHNESS LENGTH: ', f8.3)") bus(z0)
            call print_message(line)
            write(line, "('VEGETATION TEMP.: ', 2f8.3)") bus(tvege), bus(tvege + NG)
            call print_message(line)
            call print_message('VEGETATION COVER:')
            do m = 1199, 1174, -1
                write(line, "('% ', i5, '        ', f8.3)") m, bus(vegf + (1199 - m)*NG)*100.0
                call print_message(line)
            end do
            call print_message('--------------------------------')
            if (observed_forcing) then
                write(line, "('FORCING LEVEL:    ', (a))") 'height'
                call print_message(line)
                write(line, "(' THERMO. HEIGHT:   ', f8.3)") bus(ztsl)
                call print_message(line)
                write(line, "(' MOMENTUM HEIGHT:  ', f8.3)") bus(zusl)
                call print_message(line)
            else
                write(line, "('FORCING LEVEL:    ', (a))") 'sigma'
                call print_message(line)
                write(line, "(' THERMO. SIGMA:    ', f8.3)") sigma_t
                call print_message(line)
                write(line, "(' MOMENTUM SIGMA:   ', f8.3)") sigma_u
                call print_message(line)
            end if
            call print_message('--------------------------------')
            write(line, "('SLOPE:            ', f8.3)") bus(slop)
            call print_message(line)
            write(line, "('DRAIN.DENSITY     ', f8.3)") bus(draindens)
            call print_message(line)
            call print_message('--------------------------------')
            call print_message('SOIL MAPPING:')
            call print_message('DATABASE: ' // trim(soiltext))
            call print_message('WEIGHTS [METERS]:')
            do j = 1, nl_svs ! model layers
                write(line, "(' LAYER ', i3, ' DEPTH: ', f8.3)") j, dl_svs(j)
                call print_message(line)
                do jj = 1, nl_stp ! database layers
                    if (soiltext == 'GSDE') then
                        write(line, "('  ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") 'DB', dl_gsde(jj), weights(j, jj)
                    else if (soiltext == 'SLC') then
                        write(line, "('  ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") 'DB', dl_slc(jj), weights(j, jj)
                    else if (soiltext == 'SOILGRIDS') then
                        write(line, "('  ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") 'DB', dl_soilgrids(jj), weights(j, jj)
                    else if (soiltext == 'NIL') then
                        write(line, "('  ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") 'DB', dl_svs(jj), weights(j, jj)
                    end if
                    call print_message(line)
                end do
            end do
            write(line, "('PERMEABLE LAYERS: ', i3)") KHYD
            call print_message('SOIL TEXTURE:')
            call print_message('             % SAND    % CLAY')
            do j = 1, nl_svs ! model layers
                write(line, "(' LAYER ', i3, ': ', 999(f8.3, 2x))") j, bus(sand + (j - 1)*NG), bus(clay + (j - 1)*NG)
                call print_message(line)
            end do
            call print_message('SOIL MOISTURE:')
            call print_message('             LIQUID    FROZEN')
            do j = 1, nl_svs ! permeable layers
                write(line, "(' LAYER ', i3, ': ', 999(f8.3, 2x))") j, bus(wsoil + (j - 1)*NG), bus(isoil + (j - 1)*NG)
                call print_message(line)
            end do
            write(line, "('SOIL TEMPERATURE: ', 2f8.3)") bus(tground), bus(tground + NG)
            call print_message(line)
            call print_message('--------------------------------')
            call print_message('GROUND/LOW VEG. SNOW:')
            write(line, "(' SNOW TEMPERATURE:', 2f8.3)") bus(tsnow), bus(tsnow + NG)
            call print_message(line)
            write(line, "(' SNOW DEPTH:      ', 2f8.3)") bus(snodpl)
            call print_message(line)
            write(line, "(' SNOW DENSITY:    ', 2f8.3)") bus(snoden)
            call print_message(line)
            write(line, "(' SNOW ALBEDO:     ', 2f8.3)") bus(snoal)
            call print_message(line)
            write(line, "(' SNOW W/C:        ', 2f8.3)") bus(wsnow)
            call print_message(line)
            call print_message('HIGH VEG. SNOW:')
            write(line, "(' SNOW TEMPERATURE:', 2f8.3)") bus(tsnowveg), bus(tsnowveg + NG)
            call print_message(line)
            write(line, "(' SNOW DEPTH:      ', 2f8.3)") bus(snvdp)
            call print_message(line)
            write(line, "(' SNOW DENSITY:    ', 2f8.3)") bus(snvden)
            call print_message(line)
            write(line, "(' SNOW ALBEDO:     ', 2f8.3)") bus(snval)
            call print_message(line)
            write(line, "(' SNOW W/C:        ', 2f8.3)") bus(wsnv)
            call print_message(line)
            call print_message('--------------------------------')
        end if

        !> Initialize surface parameters.
!        call init_soil_text_levels()
        call inisoili_svs(bus, bussiz, NG)
!        call inisoili_svs(NG, 1)

        !> Initialize variables.
        call runsvs_init(bus, bussiz)

        !> Calculate reference start date 'datecmc_o'.
        dateo = ic%start%year*10000 + ic%start%month*100 + ic%start%day
        houro = ic%start%hour*1000000 + ic%start%mins*10000
        istat = newdate(datecmc_o, dateo, houro, 3)

!>>>svs_output
!        if (ISHEADNODE) then

            !> Daily.
!            open(iout_dly, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs_out.csv', action = 'write')
!            write(iout_dly, 1010) VN_YEAR, VN_JDAY, 'PRE', 'PRATE'
!            preacc_dly = 0.0 !reset accumulators

            !> Hourly.
!            open(iout_hly, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs1_temp_hourly.csv', action = 'write')
!            write(iout_hly, 1010) VN_YEAR, VN_JDAY, VN_HOUR, &
!                'SWE', 'SD', 'SNALB', 'TSN1', 'TSN2', 'TSNAVG', 'RAINRATE', 'SNOWRATE', 'WSN'
!            write(iout_hly, 1010) VN_YEAR, VN_JDAY, VN_HOUR, 'PRE'
!            preacc_hly = 0.0 !reset accumulators

!            open(iout_wat_bal, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs1_out_watbal_hourly.csv', action = 'write')
!            write(iout_wat_bal, 1010) VN_YEAR, VN_JDAY, VN_HOUR, &
!                'PCP_ACC', 'EVP_ACC', 'LATF_ACC', 'DRAI_ACC', 'RUNO_ACC', 'WSOIL_TOT', 'ISOIL_TOT', &
!                'SWE', 'SWE_VEG', 'WSN', 'WSN_VEG', 'WVEG', 'VEGH', 'VEGL'

!            open(iout_ts, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs_out_ts.csv', action = 'write')
!            write(iout_ts, 1010) VN_YEAR, VN_JDAY, VN_HOUR, VN_MINS, 'RPCP', 'SPCP'
!        end if

!1010    format(9999(g15.7e2, ','))

        !> Diagnostic point outputs (if active).
        if (ISHEADNODE .and. WF_NUM_POINTS > 0) then

            !> Print summary.
            call reset_tab()
            call print_message('Found these locations for diagnostic outputs:')
            call increase_tab()
            write(line, FMT_GEN) 'Folder', 'Grid No.', 'GRU'
            call print_message(line)
            do i = 1, WF_NUM_POINTS
                write(line, FMT_GEN) op%DIR_OUT(i), op%N_OUT(i), op%II_OUT(i)
                call print_message(line)
            end do
            call decrease_tab()

            !> Sanity checks.
            do i = 1, WF_NUM_POINTS
                if (i < WF_NUM_POINTS) then

                    !> Check for repeated points.
                    do j = i + 1, WF_NUM_POINTS
                        if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
                            write(line, "('Grid ', i5, ', GRU ', i4)") op%N_OUT(i), op%II_OUT(i)
                            call print_error('Output is repeated for ' // trim(adjustl(line)))
                            call program_abort()
                        end if
                    end do
                else

                    !> Check that the output path exists.
                    write(line, FMT_GEN) ipid
                    z = 0
                    open( &
                        100, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/tmp' // trim(adjustl(line)), status = 'unknown', &
                        iostat = z)
                    if (z /= 0) then
                        write(line, FMT_GEN) i
                        call print_error('The output folder for point ' // trim(adjustl(line)) // ' does not exist.')
                        call increase_tab()
                        call print_message('Location: ' // trim(adjustl(op%DIR_OUT(i))))
                        call decrease_tab()
                        call program_abort()
                    else
                        close(100, status = 'delete')
                    end if
                end if

                !> Check that point lies inside the basin.
                if (op%N_OUT(i) > shd%NAA) then
                    write(line, FMT_GEN) i
                    call print_error('Output point ' // trim(adjustl(line)) // ' is outside the basin.')
                    call increase_tab()
                    write(line, FMT_GEN) shd%NAA
                    call print_message('Number of grids inside the basin: ' // trim(adjustl(line)))
                    call decrease_tab()
                    call program_abort()
                end if
            end do

            !> Find the NML index that corresponds to the location.
            op%K_OUT = 0
            do k = il1, il2
                do i = 1, WF_NUM_POINTS
                    if (op%N_OUT(i) == shd%lc%ILMOS(k) .and. op%II_OUT(i) == shd%lc%JLMOS(k)) op%K_OUT(i) = k
                end do
            end do

            !> Open the files if the GAT-index of the output point resides on this node.
            do i = 1, WF_NUM_POINTS
                if ((ipid /= 0 .or. izero == 0) .and. (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2)) then

                    !> Open output files and write header.
                    iun = 150 + i*10

                    !> 'ts' output file.
                    iun = iun + 1
                    open(iun, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/svs_output_ts.csv')
                    write(iun, FMT_CSV, advance = 'no') &
                        VN_YEAR, VN_JDAY, VN_HOUR, VN_MINS, &
                        'RAINRATE', 'SNOWRATE', 'FLUSOLIS', 'FDSI', 'TMOINS', 'HUMOINS', &
                        'UMOINS', 'VMOINS', 'PMOINS', &
                        'FC', 'FL', 'FTEMP', 'FV', 'FVAP', 'HFLUXSA', &
                        'HFLUXSV', 'LEG', 'LER', 'LES', 'LESV', 'LETR', &
                        'LEV', 'MELTS', 'MELTSR', 'RNET_S', 'RNETSA', 'RNETSV', &
                        'RSNOWSA', 'RSNOWSV', &
                        VN_SVS_SNOAL, VN_SVS_SNODEN, VN_SVS_SNODPL, 'SNOMA', 'SNORO', &
                        VN_SVS_SNVAL, VN_SVS_SNVDEN, VN_SVS_SNVDP, 'SNVMA', 'SNVRO', &
                        'TDIAG'
                    do j = 1, svs_mesh%vs%kthermal
                        write(level, FMT_GEN) j
                        write(iun, FMT_CSV, advance = 'no') &
                            VN_SVS_TGROUND // trim(adjustl(level))
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        'THETAA', 'TSA'
                    do j = 0, 1
                        write(level, FMT_GEN) j
                        write(iun, FMT_CSV, advance = 'no') &
                            VN_SVS_TSNOW // trim(adjustl(level)), &
                            VN_SVS_TSNOWVEG // trim(adjustl(level))
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        VN_SVS_WSNOW, VN_SVS_WSNV
                    do j = 0, 1
                        write(level, FMT_GEN) j
                        write(iun, FMT_CSV, advance = 'no') &
                            VN_SVS_TVEGE // trim(adjustl(level))
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        VN_SVS_WVEG, &
                        'WWILT', 'ALVIS', 'ALVH', 'ALVL', 'ALGR', &
                        'PSNGRVL', 'PSNVH', 'PSNVHA'
                    do j = 1, svs_mesh%vs%khyd
                        write(level, FMT_GEN) j
                        write(iun, FMT_CSV, advance = 'no') &
                            VN_SVS_ISOIL // trim(adjustl(level)), &
                            VN_SVS_WSOIL // trim(adjustl(level)), &
                            'LATFLW' // trim(adjustl(level))
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        'WATFLOW', 'DRAINAF'
                    write(iun, *)
                end if
            end do
        end if
!<<<svs_output

    end subroutine

    subroutine runsvs_mesh_resume_states_seq(fls, shd, resume_ts)

        !> MESH modules.
        !*  FLAGS: Required for 'RESUMEFLAG'.
!-        use FLAGS, only: RESUMEFLAG

        !> Input variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Input variables (optional).
        logical, intent(in), optional :: resume_ts

        !> Local variables.
        integer(kind = 4) datecmc_o_i4
        real(kind = 4) tground_r4(svs_mesh%vs%kthermal*NG)
        real(kind = 4), dimension(2*NG) :: tvege_r4, tsnow_r4, tsnowveg_r4
        real(kind = 4), dimension(nl_svs*NG) :: wsoil_r4, isoil_r4
        real(kind = 4), dimension(NG) :: &
            snoma_r4, snvma_r4, wsnow_r4, wsnv_r4, snoal_r4, snval_r4, snoden_r4, snvden_r4, snodpl_r4, snvdp_r4, wveg_r4
        integer iun, j, k, z, ierr
        character(len = DEFAULT_FIELD_LENGTH) code
        logical t

        !> Return if the process is not marked active.
        if (.not. svs_mesh%PROCESS_ACTIVE) return

        !> Open the resume state file with read access.
!+        call reset_tab()
!+        call print_message('READING: ' // trim(fls%fl(mfk%f883)%fn))
!+        call increase_tab()
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(fls%fl(mfk%f883)%fn) // '.svs', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
        if (ierr /= 0) then
            write(code, FMT_GEN) ierr
            call print_error('Unable to open the file (Code: ' // trim(adjustl(code)) // ').')
            call program_abort()
        end if

        !> Resume states from file.
        z = 0

        !> Resume the reference start date 'datecmc_o'.
        !> The MESH resume date and counters are read after reading this
        !>  file, so this date must be resumed separately.
        !> Only resume the date if model dates are also resumed.
        t = .true.
        if (present(resume_ts)) t = resume_ts
        if (t) then
            read(iun, iostat = z) datecmc_o_i4
            datecmc_o = int(datecmc_o_i4)
        else
            read(iun, iostat = z)
        end if

        !> Resume temperatures.
        if (z == 0) read(iun, iostat = z) ((tground_r4(1 + (j - 1)*NG + k), j = 1, svs_mesh%vs%kthermal), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) ((tvege_r4(1 + j*NG + k), j = 0, 1), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) ((tsnow_r4(1 + j*NG + k), j = 0, 1), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) ((tsnowveg_r4(1 + j*NG + k), j = 0, 1), k = 0, NG - 1)

        !> Resume moisture.
        if (z == 0) read(iun, iostat = z) ((wsoil_r4(1 + (j - 1)*NG + k), j = 1, nl_svs), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) ((isoil_r4(1 + (j - 1)*NG + k), j = 1, nl_svs), k = 0, NG - 1)

        !> Resume snow variables.
        if (z == 0) read(iun, iostat = z) (snoma_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snvma_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (wsnow_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (wsnv_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snoal_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snval_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snoden_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snvden_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snodpl_r4(1 + k), k = 0, NG - 1)
        if (z == 0) read(iun, iostat = z) (snvdp_r4(1 + k), k = 0, NG - 1)

        !> Resume 'other'.
        if (z == 0) read(iun, iostat = z) (wveg_r4(1 + k), k = 0, NG - 1)

        !> Assign variables.
        if (z == 0) then
            do k = 0, NG -1
                do j = 1, svs_mesh%vs%kthermal
                    bus(tground + (j - 1)*NG + k) = real(tground_r4(1 + (j - 1)*NG + k), kind(bus))
                end do
                do j = 0, 1
                    bus(tvege + j*NG + k) = real(tvege_r4(1 + j*NG + k), kind(bus))
                    bus(tsnow + j*NG + k) = real(tsnow_r4(1 + j*NG + k), kind(bus))
                    bus(tsnowveg + j*NG + k) = real(tsnowveg_r4(1 + j*NG + k), kind(bus))
                end do
                do j = 1, nl_svs
                    bus(wsoil + (j - 1)*NG + k) = real(wsoil_r4(1 + (j - 1)*NG + k), kind(bus))
                    bus(isoil + (j - 1)*NG + k) = real(isoil_r4(1 + (j - 1)*NG + k), kind(bus))
                end do
                bus(snoma + k) = real(snoma_r4(1 + k), kind(bus))
                bus(snvma + k) = real(snvma_r4(1 + k), kind(bus))
                bus(wsnow + k) = real(wsnow_r4(1 + k), kind(bus))
                bus(wsnv + k) = real(wsnv_r4(1 + k), kind(bus))
                bus(snoal + k) = real(snoal_r4(1 + k), kind(bus))
                bus(snval + k) = real(snval_r4(1 + k), kind(bus))
                bus(snoden + k) = real(snoden_r4(1 + k), kind(bus))
                bus(snvden + k) = real(snvden_r4(1 + k), kind(bus))
                bus(snodpl + k) = real(snodpl_r4(1 + k), kind(bus))
                bus(snvdp + k) = real(snvdp_r4(1 + k), kind(bus))
                bus(wveg + k) = real(wveg_r4(1 + k), kind(bus))
            end do
        end if

        !> Close the file to free the unit.
        close(iun)

        !> Check for read errors.
        if (z /= 0) then
            call print_warning('Errors occurred resuming states from file.')
        end if

    end subroutine

    subroutine runsvs_mesh_within_tile(shd, fls, cm)

        !> SVS modules.
!        use runsvs_mod
!        use runsvs_utils
!        use svs_configs
!        use sfc_options
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

!#include "options.cdk"
#include "isbapar.cdk"
!#include "surfcon.cdk"
#include "thermoconsts.inc"

!        integer, parameter :: bussiz = runsvs_busdim
!        real bus(bussiz)
!        integer datecmc_o, date_f, hour_f
        integer datecmc_v, date_v(1), hour_v, istat, kount, bidon
        real(kind = 8) kdt

        integer iun, k, ki, kj, j, i
        real FRAC

        integer, external :: newdate
        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not active or if the head node.
        if (.not. svs_mesh%PROCESS_ACTIVE .or. .not. (ipid /= 0 .or. izero == 0)) return

        !> Time-step.
        dt = real(ic%dts)
        delt = real(ic%dts)
        kount = ic%ts_count - 1

        !> First time-step.
!-        if (kount == 0) then
!-            dateo = ic%now%year*10000 + ic%now%month*100 + ic%now%day
!-            houro = ic%now%hour*1000000 + ic%now%mins*10000
!-            istat = newdate(datecmc_o, dateo, houro, 3)
!-        end if

        !> Determine time stamps of current date.
        !> SVS relies on the date at the start of the run and the KOUNT variable,
        !> to determine the Julian Day, which is used to compute the zenith solar angle.
        !> There is no need to increment the date given to SVS.
        kdt = 0 !kount*(dt*1.0D0)/3600.0D0

        !> Compute date valid.
        call incdatr(datecmc_v, datecmc_o, kdt)

        !> Convert to old style.
        istat = newdate(datecmc_v, date, bidon, -4)

        !> Convert to printable.
        istat = newdate(datecmc_v, date_v, hour_v, -3)

        !> Loop tiles.
        do k = 0, NG - 1

            !> Transfer driving variables.
            if (associated(vs%tile%prern) .and. associated(vs%tile%presno)) then
                bus(rainrate + k) = vs%tile%prern(il1 + k)/1000.0
                bus(snowrate + k) = vs%tile%presno(il1 + k)/1000.0
            else if (vs%tile%ta(il1 + k) > tcdk) then
                bus(rainrate + k) = vs%tile%pre(il1 + k)/1000.0
                bus(snowrate + k) = 0.0
            else
                bus(rainrate + k) = 0.0
                bus(snowrate + k) = vs%tile%pre(il1 + k)/1000.0
            end if
            bus(flusolis + k) = vs%tile%fsin(il1 + k)
            bus(fdsi + k) = vs%tile%flin(il1 + k)
            bus(tmoins + k) = vs%tile%ta(il1 + k)
            bus(humoins + k) = vs%tile%qa(il1 + k)
            bus(umoins + k) = vs%tile%uv(il1 + k)
            bus(vmoins + k) = 0.0
            bus(pmoins + k) = vs%tile%pres(il1 + k)
        end do

        call compvirttemp(sigma_t, bus, bussiz)
        if (.not. observed_forcing) call surflayerheight(sigma_u, sigma_t, bus, bussiz)

        !> Call subroutine to compute layer thicknesses.
        call layer_thickness()

        !> Update vegetation parameters as a function of julian day.
        call inicover_svs(bus, bussiz, kount, NG)

        do k = 0, NG - 1
            bus(rootdp + k) = max(bus(rootdp + k), 0.5)
        end do

        !> Integrate SVS for one time step.
        call svs(bus, bussiz, bidon, 1, dt, kount, 1, NG, NG, 1)

        !> Transfer variables.
        do k = 0, NG - 1
            vs%tile%qacan(il1 + k) = bus(qsurf + k)
            vs%tile%lqwscan(il1 + k) = bus(wveg + k)
            vs%tile%tacan(il1 + k) = bus(tsurf + k)
            vs%tile%tcan(il1 + k) = (bus(tvege + k) + bus(tvege + NG + k) + bus(tsnowveg + k) + bus(tsnowveg + NG + k))/4.0
            vs%tile%sno(il1 + k) = bus(snoma + k)
            vs%tile%albsno(il1 + k) = (bus(snoal + k) + bus(snval + k))/2.0
            vs%tile%rhosno(il1 + k) = ((bus(snoro + k) + bus(snvro + k))/2.0)*900.0
            vs%tile%tsno(il1 + k) = (bus(tsnow + k) + bus(tsnow + NG + k))/2.0
            if (bus(snoma + k) > 0.0) then
                vs%tile%lqwssno(il1 + k) = bus(wsnow + k)
            else
                vs%tile%lqwssno(il1 + k) = 0.0
            end if
            vs%tile%et(il1 + k) = bus(wflux + k)
            vs%tile%qevp(il1 + k) = bus(fv + k)
            vs%tile%qsens(il1 + k) = bus(fc + k)
            vs%tile%ovrflw(il1 + k) = max(0.0, bus(runofftot + k))/ic%dts
!EG_MOD add lateral flow from layers 1 to KHYD
!-            vs%tile%latflw(il1 + k) = 0.0
!-            do j = 1, KHYD
!-                vs%tile%latflw(il1 + k) = vs%tile%latflw(il1 + k) + max(0.0, bus(latflw + (j - 1)*NG + k))/ic%dts
!-            end do
            do j = 1, KHYD
                vs%tile%latflw(il1 + k, j) = max(0.0, bus(latflw + (j - 1)*NG + k))/ic%dts
            end do
            vs%tile%thicsol(il1 + k, 1) = bus(isoil + k)
            vs%tile%thlqsol(il1 + k, 1) = bus(wsoil + k)
            vs%tile%thlqsol(il1 + k, 2) = bus(wsoil + NG + k)
            do j = 3, shd%lc%IGND
                vs%tile%thlqsol(il1 + k, j) = bus(wsoil + (j - 1)*NG + k)
            end do
            vs%tile%tsol(il1 + k, 1) = bus(tground + k)
            do j = 2, shd%lc%IGND
                vs%tile%tsol(il1 + k, j) = bus(tground + NG + k)
            end do

!-            vs%tile%gflx(il1 + k, :) =
            vs%tile%drainsol(il1 + k) = max(0.0, bus(watflow + KHYD*NG + k))/ic%dts
        end do

!>>>svs_output
!        if (ISHEADNODE) then

            !> Daily.
!            preacc_dly = preacc_dly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
!            if (ic%now%day /= ic%next%day) then !last time-step of day
!                write(iout_dly, 1010) ic%now%year, ic%now%jday, &
!                    preacc_dly, & !daily acc.
!                    (preacc_dly/real(ic%ts_daily*ic%dts)) !rate = (value)/seconds in day using ts_daily and dts
!                preacc_dly = 0.0 !reset accumulators
!            end if

            !> Hourly.
!            preacc_hly = preacc_hly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
!            preacc_tot = preacc_tot + &
!                1000.0*sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + 1000.0*sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
!            runoff_acc = runoff_acc + bus(runofftot)
!            wsoil_tot = 0.0
!            isoil_tot = 0.0
!            call layer_thickness() !called above
!            do j = 1, KHYD
!                wsoil_tot = wsoil_tot + 1000.0*bus(wsoil + (j - 1)*NG)*delz(j) ! mm
!            end do

!            if (ic%now%hour /= ic%next%hour) then !last time-step of hour
!                write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, preacc_hly !daily acc.
!                write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, &
!                    bus(snoma), bus(snodpl), bus(snoal), &
!                    bus(tsnow), bus(tsnow + 1), bus(tsnavg), bus(rainrate), bus(snowrate), bus(wsnow)
                !daily acc.
!                preacc_hly = 0.0 !reset accumulators
!                write(iout_wat_bal, 1010) ic%now%year, ic%now%jday, ic%now%hour, &
!                    preacc_tot, bus(accevap), bus(latflaf), &
!                    bus(drainaf), runoff_acc, wsoil_tot, isoil_tot, &
!                    bus(snoma), bus(snvma), bus(wsnow), bus(wsnv), bus(wveg), bus(vegh), bus(vegl)
!            end if

!            if (kount == 0) then
!                wsoil_ini = wsoil_tot
!                end if
!            bal_in_out = preacc_tot - bus(accevap) - bus(drainaf) - runoff_acc - bus(latflaf)
!            stock = (1 - bus(vegh))*bus(snoma) + bus(vegh)*bus(snvma) + &
!                wsoil_tot - wsoil_ini + isoil_tot + bus(wveg)*(bus(vegl) + bus(vegh))
!            bal_tot = bal_in_out - stock
!            if ((abs(bal_tot - bal_pre)) > 0.1) then
!                write(*, *) 'Inbalance ', bal_tot, bal_tot - bal_pre
!            end if
!            bal_pre = bal_tot

!            write(iout_ts, 1010) ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
!                sum(bus(rainrate:(rainrate + NG - 1))), sum(bus(snowrate:(snowrate + NG - 1)))
!        end if

!1010    format(9999(g15.7e2, ','))

        !> Diagnostic point outputs (if active).
        if (ISHEADNODE .and. WF_NUM_POINTS > 0) then

            !> Write to files if the GAT-index of the output point resides on this node.
            do i = 1, WF_NUM_POINTS
                if ((ipid /= 0 .or. izero == 0) .and. (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2)) then

                    !> Grab the identity of the tile (offset relative to node-indexing).
                    k = op%K_OUT(i) - il1

                    !> Write data.
                    iun = 150 + i*10

                    !> 'ts' output file.
                    iun = iun + 1
                    write(iun, FMT_CSV, advance = 'no') &
                        ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
                        bus(rainrate + k), bus(snowrate + k), bus(flusolis + k), bus(fdsi + k), bus(tmoins + k), bus(humoins + k), &
                        bus(umoins + k), bus(vmoins + k), bus(pmoins + k), &
                        bus(fc + k), bus(fl + k), bus(ftemp + k), bus(fv + k), bus(fvap + k), bus(hfluxsa + k), &
                        bus(hfluxsv + k), bus(leg + k), bus(ler + k), bus(les + k), bus(lesv + k), bus(letr + k), &
                        bus(lev + k), bus(melts + k), bus(meltsr + k), bus(rnet_s + k), bus(rnetsa + k), bus(rnetsv + k), &
                        bus(rsnowsa + k), bus(rsnowsv + k), &
                        bus(snoal + k), bus(snoden + k), bus(snodpl + k), bus(snoma + k), bus(snoro + k), &
                        bus(snval + k), bus(snvden + k), bus(snvdp + k), bus(snvma + k), bus(snvro + k), &
                        bus(tdiag + k)
                    do j = 1, svs_mesh%vs%kthermal
                        write(iun, FMT_CSV, advance = 'no') &
                            bus(tground + (j - 1)*NG + k)
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        bus(thetaa + k), bus(tsa + k)
                    do j = 0, 1
                        write(iun, FMT_CSV, advance = 'no') &
                            bus(tsnow + j*NG + k), &
                            bus(tsnowveg + j*NG + k)
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        bus(wsnow + k), bus(wsnv + k)
                    do j = 0, 1
                        write(iun, FMT_CSV, advance = 'no') &
                            bus(tvege + j*NG + k)
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        bus(wveg + k), &
                        bus(wwilt + k), bus(alvis + k), bus(alvh + k), bus(alvl + k), bus(algr + k), &
                        bus(psngrvl + k), bus(psnvh + k), bus(psnvha + k)
                    do j = 1, svs_mesh%vs%khyd
                        write(iun, FMT_CSV, advance = 'no') &
                            bus(isoil + (j - 1)*NG + k), &
                            bus(wsoil + (j - 1)*NG + k), &
                            bus(latflw + (j - 1)*NG + k)
                    end do
                    write(iun, FMT_CSV, advance = 'no') &
                        bus(watflow + KHYD*NG + k), bus(drainaf + k)
                    write(iun, *)
                end if
            end do
        end if
!<<<svs_output

    end subroutine

    subroutine runsvs_mesh_finalize(shd, fls)

        !> MESH modules.
        !*  FLAGS: Required for 'SAVERESUMEFLAG'.
!-        use FLAGS, only: SAVERESUMEFLAG

        !> Input variables.
        type(ShedGridParams) shd
        type(fl_ids) fls

        !> Return if the process is not marked active.
        if (.not. svs_mesh%PROCESS_ACTIVE) return

    end subroutine

    subroutine runsvs_mesh_save_states_seq(fls, shd)

        !> MESH modules.
        !*  FLAGS: Required for 'SAVERESUMEFLAG'.
!-        use FLAGS, only: SAVERESUMEFLAG

        !> Input variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer iun, j, k, z, ierr
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Return if the process is not marked active.
        if (.not. svs_mesh%PROCESS_ACTIVE) return

        !> Return if not the head node (only the head node should write output).
        if (.not. ISHEADNODE) return

        !> Open the resume state file with write access.
!+        call reset_tab()
!+        call print_message('SAVING: ' // trim(fls%fl(mfk%f883)%fn))
!+        call increase_tab()
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(fls%fl(mfk%f883)%fn) // '.svs', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
        if (ierr /= 0) then
            write(code, FMT_GEN) ierr
            call print_error('Unable to open the file (Code: ' // trim(adjustl(code)) // ').')
            call program_abort()
        end if

        !> Save states to file.
        z = 0

        !> Save the reference start date 'datecmc_o'.
        !> The MESH resume date and counters are read after reading the
        !>  SVS resume file, so this date must be saved separately.
        !> Save the date regardless of option so the file is compatible
        !>  with all resume options.
        write(iun, iostat = z) int(datecmc_o, kind = 4)

        !> Save temperatures.
        if (z == 0) write(iun, iostat = z) &
            ((real(bus(tground + (j - 1)*NG + k), kind = 4), j = 1, svs_mesh%vs%kthermal), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) ((real(bus(tvege + j*NG + k), kind = 4), j = 0, 1), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) ((real(bus(tsnow + j*NG + k), kind = 4), j = 0, 1), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) ((real(bus(tsnowveg + j*NG + k), kind = 4), j = 0, 1), k = 0, NG - 1)

        !> Save moisture.
        if (z == 0) write(iun, iostat = z) ((real(bus(wsoil + (j - 1)*NG + k), kind = 4), j = 1, nl_svs), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) ((real(bus(isoil + (j - 1)*NG + k), kind = 4), j = 1, nl_svs), k = 0, NG - 1)

        !> Save snow variables.
        if (z == 0) write(iun, iostat = z) (real(bus(snoma + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snvma + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(wsnow + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(wsnv + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snoal + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snval + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snoden + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snvden + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snodpl + k), kind = 4), k = 0, NG - 1)
        if (z == 0) write(iun, iostat = z) (real(bus(snvdp + k), kind = 4), k = 0, NG - 1)

        !> Save 'other'.
        if (z == 0) write(iun, iostat = z) (real(bus(wveg + k), kind = 4), k = 0, NG - 1)

        !> Close the file to free the unit.
        close(iun)

        !> Check for write errors.
        if (z /= 0) then
            call print_warning('Errors occurred saving states to file.')
        end if

    end subroutine

end module
