module PBSM_module

    implicit none

    !> PBSM parameters.
    !*  fetch: fetch distance (m)
    !*  Ht: vegetation height (m)
    !*  N_S: vegetation density (number/m^2)
    !*  A_S: vegetation width (m)
    !*  Distrib: Inter-GRU snow redistribution factor
    type pbsm_parameters
        real, dimension(:), allocatable :: &
            fetch, Ht, N_S, A_S, Distrib
    end type

    !> PBSM variables.
    !*  DrySnow:
    !*      0 = air temperature above 0 degC
    !*      1 = air temperature below 0 degC
    !*  SnowAge: hours since last snowfall
    !*  Drift: blowing snow transport (kg/m^2)
    !*  Subl: blowing snow sublimation (kg/m^2)
    type pbsm_variables
        real, dimension(:), allocatable :: &
            DrySnow, SnowAge, &
            TSNOds, &
            Drift, Subl, Deposition
    end type

    !> Internal CLASS variables pulled from CLASSW for PBSM.
    real, dimension(:), allocatable, save :: &
        ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
        HCPSCS, HCPSGS, HCPSC, HCPSG, &
        TSNOWC, TSNOWG, &
        RHOSC, RHOSG, &
        XSNOWC, XSNOWG, XSNOCS, XSNOGS

    type pbsm_container
        type(pbsm_parameters) :: pm_gru, pm_grid, pm
        type(pbsm_variables) :: vs
        logical :: PROCESS_ACTIVE = .false.
    end type

    type(pbsm_container), save :: pbsm

    contains

    !> Description:
    !>  Parse the PBSM (blowing snow) configuration flag.
    !>
    !> Input variables:
    !*  flag: PBSMFLAG from file.
    subroutine PBSM_parse_flag(flag)

        use strings

        !> Input variables.
        character(len = *), intent(in) :: flag

        !> Local variables.
        character(len = len(flag)), dimension(10) :: out_args
        integer nargs

        !> Default behaviour: Blowing snow calculations are not made (default).
        pbsm%PROCESS_ACTIVE = .false.

        !> Parse the flag for options.
        call parse(flag, ' ', out_args, nargs)
        if (.not. nargs > 1) return

        !> Assign options.
        select case (lowercase(out_args(2)))

            !> 1 = Blowing snow transport, sublimation & inter-GRU redistribution calculations are made.
            case ('1')
                pbsm%PROCESS_ACTIVE = .true.
            case ('on')
                pbsm%PROCESS_ACTIVE = .true.
        end select

    end subroutine

    !> Description:
    !>  Set initial SnowAge & DrySnow values for PBSM calculations
    !>  (MK MacDonald, Sept 2010).
    subroutine PBSM_init(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        use RUNCLASS36_constants

        !> Input variables from driver.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer NML, k, m
        character(len = DEFAULT_LINE_LENGTH) line

        !> Return if PBSM is not active.
        if (.not. pbsm%PROCESS_ACTIVE) return

        !> Allocate variables.
        NML = shd%lc%NML
        allocate( &
            pbsm%vs%DrySnow(NML), pbsm%vs%SnowAge(NML), &
            pbsm%vs%TSNOds(NML), &
            pbsm%vs%Drift(NML), pbsm%vs%Subl(NML), pbsm%vs%Deposition(NML))
        pbsm%vs%DrySnow = 0.0; pbsm%vs%SnowAge = 0.0;
        pbsm%vs%TSNOds = 0.0
        pbsm%vs%Drift = 0.0; pbsm%vs%Subl = 0.0; pbsm%vs%Deposition = 0.0;

        !> Allocate variables for CLASSW.
        !> These are initialized in WPREP.
        allocate( &
            ZSNOCS(NML), ZSNOGS(NML), ZSNOWC(NML), ZSNOWG(NML), &
            HCPSCS(NML), HCPSGS(NML), HCPSC(NML), HCPSG(NML), &
            TSNOWC(NML), TSNOWG(NML), &
            RHOSC(NML), RHOSG(NML), &
            XSNOWC(NML), XSNOWG(NML), XSNOCS(NML), XSNOGS(NML))

        !> Set initial SnowAge & DrySnow values for PBSM calculations.
        do k = il1, il2
            if (vs%tile%sno(k) <= 0.0) then
                pbsm%vs%DrySnow(k) = 0.0   !1 = snowpack is dry (i.e. cold)
                pbsm%vs%SnowAge(k) = 0.0   !hours since last snowfall
            else if (vs%tile%ta(k) >= TFREZ) then
                pbsm%vs%DrySnow(k) = 0.0
                pbsm%vs%SnowAge(k) = 48.0   !assume 48 hours since last snowfall
            else
                pbsm%vs%DrySnow(k) = 1.0
                pbsm%vs%SnowAge(k) = 48.0
            end if
        end do

        !> Write summary to output file.
        call print_message('PBSM (blowing snow) component is ACTIVE.')
        if (DIAGNOSEMODE) then
            call print_message('PBSMFLAG on')
            write(line, FMT_GEN) 'GRUs ->', (m, m = 1, shd%lc%NTYPE)
            call print_message(line)
            write(line, FMT_GEN) 'Fetch', (pbsm%pm_gru%fetch(m), m = 1, shd%lc%NTYPE)
            call print_message(line)
            write(line, FMT_GEN) 'Ht', (pbsm%pm_gru%Ht(m), m = 1, shd%lc%NTYPE)
            call print_message(line)
            write(line, FMT_GEN) 'N_S', (pbsm%pm_gru%N_S(m), m = 1, shd%lc%NTYPE)
            call print_message(line)
            write(line, FMT_GEN) 'A_S', (pbsm%pm_gru%A_S(m), m = 1, shd%lc%NTYPE)
            call print_message(line)
            write(line, FMT_GEN) 'Distrib', (pbsm%pm_gru%Distrib(m), m = 1, shd%lc%NTYPE)
            call print_message(line)
        end if

    end subroutine

    !> Description:
    !>  Single column blowing snow calculations.
    subroutine PBSM_within_tile( &
        ZSNOW, WSNO, SNO, RHOS, TSNO, HTCS, &
        TSNOCS, TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, &
        FC, FG, FCS, FGS, &
        SFCT, SFCU, SFCQ, ZRFM, ZOMLCS, ZOMLNS, &
        NML, &
        fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> Input variables from driver.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Input/output variables.
        integer, intent(in) :: NML
        real, dimension(NML) :: &
            ZSNOW, WSNO, SNO, RHOS, TSNO, HTCS, &
            TSNOCS, TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, &
            FC, FG, FCS, FGS, &
            SFCT, SFCU, SFCQ, ZRFM, ZOMLCS, ZOMLNS

        !> Return if PBSM is not active.
        if (.not. pbsm%PROCESS_ACTIVE) return

        !> Initialize diagnostic variables for PBSM.
!+        pbsm%vs%Drift(il1:il2) = 0.0
!+        pbsm%vs%Subl(il1:il2) = 0.0
!+        pbsm%vs%Deposition(il1:il2) = 0.0

        !> Single column blowing snow calculations.
        call PBSMrun( &
            ZSNOW, WSNO, SNO, RHOS, TSNO, HTCS, &
            ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
            HCPSCS, HCPSGS, HCPSC, HCPSG, &
            TSNOWC, TSNOWG, TSNOCS, TSNOGS, &
            RHOSC, RHOSG, RHOSCS, RHOSGS,&
            XSNOWC, XSNOWG, XSNOCS, XSNOGS, &
            WSNOCS, WSNOGS, &
            FC, FG, FCS, FGS, &
            pbsm%pm%fetch, pbsm%pm%N_S, pbsm%pm%A_S, pbsm%pm%Ht, &
            SFCT, SFCU, SFCQ, vs%tile%pres, vs%tile%pre, &
            pbsm%vs%DrySnow, pbsm%vs%SnowAge, pbsm%vs%Drift, pbsm%vs%Subl, &
            pbsm%vs%TSNOds, &
            NML, il1, il2, ic%ts_count, ZRFM, ZOMLCS, ZOMLNS)

    end subroutine

    !> Description:
    !>  Distribute blowing snow mass between GRUs.
    subroutine PBSM_within_grid( &
        TSNO, ZSNOW, RHOS, SNO, TSNOCS, RHOSCS, TSNOGS, RHOSGS, &
        GC, FARE, WSNOCS, WSNOGS, FCS, FGS, FC, FG, &
        TROO, ROFO, TROF, ROF, ROFN, PCPG, HTCS, WSNO, &
        NML, &
        fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        !> Input variables from driver.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Input/output variables.
        integer, intent(in) :: NML
        real, dimension(NML) :: &
            TSNO, ZSNOW, RHOS, SNO, TSNOCS, RHOSCS, TSNOGS, RHOSGS, &
            GC, FARE, WSNOCS, WSNOGS, FCS, FGS, FC, FG, &
            TROO, ROFO, TROF, ROF, ROFN, PCPG, HTCS, WSNO

        !> Return if PBSM is not active.
        if (.not. pbsm%PROCESS_ACTIVE) return

        !> Distribute blowing snow mass between GRUs.
        call REDISTRIB_SNOW( &
            shd%NA, shd%lc%NTYPE, shd%NA*shd%lc%NTYPE, NML, il1, il2, TSNO, ZSNOW, &
            RHOS, SNO, TSNOCS, ZSNOCS, HCPSCS, RHOSCS, TSNOGS, &
            ZSNOGS, HCPSGS, RHOSGS, TSNOWC, ZSNOWC, HCPSC, RHOSC, TSNOWG, &
            ZSNOWG, HCPSG, RHOSG, GC, shd%lc%ILMOS, pbsm%vs%Drift, FARE, &
            pbsm%vs%TSNOds, pbsm%pm%Distrib, WSNOCS, WSNOGS, FCS, FGS, FC, FG, pbsm%vs%Deposition, &
            TROO, ROFO, TROF, ROF, ROFN, PCPG, HTCS, WSNO, ic%ts_count)

    end subroutine

    !> Description:
    !>  Finalize routine.
    subroutine PSBM_finalize(fls, shd, cm)

        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        !> Input variables from driver.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

    end subroutine

end module
