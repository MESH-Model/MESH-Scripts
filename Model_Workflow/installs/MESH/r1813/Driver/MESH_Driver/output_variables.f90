module output_variables

    use variable_names
    use model_variables

    implicit none

    !> Description:
    !>  Container for output variables.
    type output_fields

        !> Meteorology/climatology variables.
        real, dimension(:), pointer :: ifsin => null()
        real, dimension(:), pointer :: fsin => null()
        real, dimension(:), pointer :: fsvs => null()
        real, dimension(:), pointer :: fsir => null()
        real, dimension(:), pointer :: fsdr => null()
        real, dimension(:), pointer :: fsdff => null()
        real, dimension(:), pointer :: fsout => null()
        real, dimension(:), pointer :: flin => null()
        real, dimension(:), pointer :: flout => null()
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
        real, dimension(:), pointer :: precrn => null()
        real, dimension(:), pointer :: precsno => null()
        real, dimension(:), pointer :: prec => null()

        !> Canopy variables.
        real, dimension(:), pointer :: ican => null()
        real, dimension(:), pointer :: lqwscan => null()
        real, dimension(:), pointer :: fzwscan => null()
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: gro => null()

        !> Snow variables.
        real, dimension(:), pointer :: isno => null()
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: rhosno => null()
        real, dimension(:), pointer :: zsno => null()
        real, dimension(:), pointer :: lqwssno => null()
        real, dimension(:), pointer :: tsno => null()
        real, dimension(:), pointer :: albsno => null()
        real, dimension(:), pointer :: drainsno => null()

        !> Surface variables.
        real, dimension(:), pointer :: albt => null()
        real, dimension(:), pointer :: alvs => null()
        real, dimension(:), pointer :: alir => null()
        real, dimension(:), pointer :: gte => null()
        real, dimension(:), pointer :: ipnd => null()
        real, dimension(:), pointer :: zpnd => null()
        real, dimension(:), pointer :: lqwspnd => null()
        real, dimension(:), pointer :: tpnd => null()
        real, dimension(:), pointer :: pndcaf => null()
        real, dimension(:), pointer :: potevp => null()
        real, dimension(:), pointer :: et => null()
        real, dimension(:), pointer :: evpb => null()
        real, dimension(:), pointer :: arrd => null()
        real, dimension(:), pointer :: ovrflw => null()
        real, dimension(:), pointer :: qevp => null()
        real, dimension(:), pointer :: qsens => null()
        real, dimension(:), pointer :: gzero => null()
        real, dimension(:), pointer :: tsurf => null()

        !> Ice/glacier variables.
        real, dimension(:), pointer :: iice => null()
        real, dimension(:), pointer :: lqwsice => null()
        real, dimension(:), pointer :: tice => null()

        !> Subsurface/soil variables.
        real, dimension(:, :), pointer :: thlqsol => null()
        real, dimension(:, :), pointer :: thicsol => null()
        real, dimension(:, :), pointer :: lqwssol => null()
        real, dimension(:, :), pointer :: fzwssol => null()
        real, dimension(:, :), pointer :: alwssol => null()
        real, dimension(:, :), pointer :: tsol => null()
        real, dimension(:, :), pointer :: gflx => null()
        real, dimension(:, :), pointer :: latflw => null()
        real, dimension(:), pointer :: zsolsat => null()
        real, dimension(:), pointer :: drainsol => null()

        !> Groundwater/lower zone storage variables.
        real, dimension(:), pointer :: rchg => null()
        real, dimension(:), pointer :: stggw => null()
        real, dimension(:), pointer :: lkg => null()
!-        real, dimension(:), pointer :: dzs => null()

        !> Diagnostic variables.
        real, dimension(:), pointer :: stg0e => null()
        real, dimension(:), pointer :: stge => null()
        real, dimension(:), pointer :: dstge => null()
        real, dimension(:), pointer :: stg0w => null()
        real, dimension(:), pointer :: stgw => null()
        real, dimension(:), pointer :: dstgw => null()

        !> Routing variables.
        real, dimension(:), pointer :: rff => null()
        real, dimension(:), pointer :: rof => null()
        real, dimension(:), pointer :: qi => null()
        real, dimension(:), pointer :: qo => null()
        real, dimension(:), pointer :: stgch => null()
        real, dimension(:), pointer :: zlvl => null()

        !> Indices.
        integer :: n1 = 0
        integer :: n2 = 0

        !> Association to model variable group.
        type(model_variables_fields), pointer :: vs => null()

        !> Association to 'ts' base group.
        type(output_fields), pointer :: ts => null()
    end type

    !> Description:
    !>  Container for a series of output variables (e.g., at various time intervals).
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    !*  basin: Same as grid but accumulated according to drainage direction 1:NA.
    type output_series
        type(output_fields), pointer :: tile, grid, basin
    end type

    !> Description:
    !>  Container for output variables and NO_DATA values.
    !>
    !> Variables:
    !*  tot, y, m, d, h, ts: Output at variable time intervals.
    !*  NO_DATA: No data value (type: real).
    !*  NO_DATA_INT: No data value (type: integer).
    type output_variables_container
        type(output_series) tot, y, m, d, h, ts
        integer :: NO_DATA_INT = -1
        real :: NO_DATA = -1.0
    end type

    !*  out: Instance of output variables.
    type(output_variables_container), save :: out

    !> Description:
    !>  Type for process modules to integrate with output fields.
    type output_fields_surrogate
        real, dimension(:), pointer :: &
            y_tile => null(), m_tile => null(), d_tile => null(), h_tile => null(), ts_tile => null(), &
            y_grid => null(), m_grid => null(), d_grid => null(), h_grid => null(), ts_grid => null()
    end type

    !> Description:
    !>  Interface for 'output_variables_allocate'.
    interface output_variables_allocate
        module procedure output_variables_allocate_1d_pntr
        module procedure output_variables_allocate_2d_pntr
    end interface

    interface output_variables_activate
        module procedure output_variables_activate_pntr
        module procedure output_variables_activate_vnames
    end interface

    contains

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n1'.
    subroutine output_variables_allocate_1d_pntr(field, n1, pntr)

        !> Input/output variables.
        integer, intent(in) :: n1
        real, dimension(:), pointer :: field
        real, dimension(:), optional, pointer :: pntr

        !> Allocate and initialize variable
        if (.not. associated(field)) then
            allocate(field(n1))
            field = 0.0
        end if

        !> Associate the pointer.
        if (present(pntr)) pntr => field

    end subroutine

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n1' and 'n2'.
    subroutine output_variables_allocate_2d_pntr(field, n1, n2, pntr, ig)

        !> Input variables.
        integer, intent(in) :: n1, n2
        integer, intent(in), optional :: ig

        !> Input/output variables.
        real, dimension(:, :), pointer :: field
        real, dimension(:), optional, pointer :: pntr

        !> Allocate and initialize variable
        if (.not. associated(field)) then
            allocate(field(n1, n2))
            field = 0.0
        end if

        !> Associate the pointer.
        if (present(pntr) .and. present(ig)) pntr => field(:, ig)

    end subroutine

    !> Description:
    !>  Allocate the output variable and associate it to 'pntr'.
    recursive subroutine output_variables_activate_pntr(fields, vname, pntr, ig)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input variables.
        type(output_fields), intent(in) :: fields
        character(len = *), intent(in) :: vname
        integer, intent(in), optional :: ig

        !> Input/output variables.
        real, dimension(:), optional, pointer :: pntr

        !> Local variables.
        integer n1, n2

        !> Indices.
        n1 = fields%n1
        n2 = fields%n2

        !> Copy the variable.
        select case (vname)

            !> Meteorology/climatology variables.
            case (VN_FSIN)
                if (.not. associated(fields%vs%fsin)) then
                    allocate(fields%vs%fsin(fields%vs%dim_length))
                    fields%vs%fsin = huge(fields%vs%fsin)
                end if
                call output_variables_allocate(fields%fsin, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsin, n1)
                call output_variables_allocate(fields%ifsin, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%ifsin, n1)
            case (VN_FSVS)
                call output_variables_activate_pntr(fields, VN_FSIN)
                call output_variables_allocate(fields%fsvs, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsvs, n1)
            case (VN_FSIR)
                call output_variables_activate_pntr(fields, VN_FSIN)
                call output_variables_allocate(fields%fsir, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsir, n1)
            case (VN_FSDR)
                if (.not. associated(fields%vs%fsdr)) then
                    allocate(fields%vs%fsdr(fields%vs%dim_length))
                    fields%vs%fsdr = huge(fields%vs%fsdr)
                end if
                call output_variables_allocate(fields%fsdr, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsdr, n1)
            case (VN_FSDFF)
                if (.not. associated(fields%vs%fsdff)) then
                    allocate(fields%vs%fsdff(fields%vs%dim_length))
                    fields%vs%fsdff = huge(fields%vs%fsdff)
                end if
                call output_variables_allocate(fields%fsdff, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsdff, n1)
            case (VN_FSOUT)
                call output_variables_activate_pntr(fields, VN_FSIN)
                call output_variables_activate_pntr(fields, VN_ALBT)
                call output_variables_allocate(fields%fsout, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsout, n1)
            case (VN_FLIN)
                if (.not. associated(fields%vs%flin)) then
                    allocate(fields%vs%flin(fields%vs%dim_length))
                    fields%vs%flin = huge(fields%vs%flin)
                end if
                call output_variables_allocate(fields%flin, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%flin, n1)
            case (VN_FLOUT)
                if (.not. associated(fields%vs%gte)) then
                    allocate(fields%vs%gte(fields%vs%dim_length))
                    fields%vs%gte = huge(fields%vs%gte)
                end if
                call output_variables_allocate(fields%flout, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%flout, n1)
            case (VN_TA)
                if (.not. associated(fields%vs%ta)) then
                    allocate(fields%vs%ta(fields%vs%dim_length))
                    fields%vs%ta = huge(fields%vs%ta)
                end if
                call output_variables_allocate(fields%ta, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%ta, n1)
            case (VN_QA)
                if (.not. associated(fields%vs%qa)) then
                    allocate(fields%vs%qa(fields%vs%dim_length))
                    fields%vs%qa = huge(fields%vs%qa)
                end if
                call output_variables_allocate(fields%qa, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%qa, n1)
            case (VN_PRES)
                if (.not. associated(fields%vs%pres)) then
                    allocate(fields%vs%pres(fields%vs%dim_length))
                    fields%vs%pres = huge(fields%vs%pres)
                end if
                call output_variables_allocate(fields%pres, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%pres, n1)
            case (VN_UU)
                if (.not. associated(fields%vs%uu)) then
                    allocate(fields%vs%uu(fields%vs%dim_length))
                    fields%vs%uu = huge(fields%vs%uu)
                end if
                call output_variables_allocate(fields%uu, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%uu, n1)
            case (VN_VV)
                if (.not. associated(fields%vs%vv)) then
                    allocate(fields%vs%vv(fields%vs%dim_length))
                    fields%vs%vv = huge(fields%vs%vv)
                end if
                call output_variables_allocate(fields%vv, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%vv, n1)
            case (VN_UV)
                if (.not. associated(fields%vs%uv)) then
                    allocate(fields%vs%uv(fields%vs%dim_length))
                    fields%vs%uv = huge(fields%vs%uv)
                end if
                call output_variables_allocate(fields%uv, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%uv, n1)
            case (VN_WDIR)
                if (.not. associated(fields%vs%wdir)) then
                    allocate(fields%vs%wdir(fields%vs%dim_length))
                    fields%vs%wdir = huge(fields%vs%wdir)
                end if
                call output_variables_allocate(fields%wdir, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%wdir, n1)
            case (VN_PRERN)
                if (.not. associated(fields%vs%prern)) then
                    allocate(fields%vs%prern(fields%vs%dim_length))
                    fields%vs%prern = huge(fields%vs%prern)
                end if
                call output_variables_allocate(fields%prern, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%prern, n1)
            case (VN_PRESNO)
                if (.not. associated(fields%vs%presno)) then
                    allocate(fields%vs%presno(fields%vs%dim_length))
                    fields%vs%presno = huge(fields%vs%presno)
                end if
                call output_variables_allocate(fields%presno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%presno, n1)
            case (VN_PRE)
                if (.not. associated(fields%vs%pre)) then
                    allocate(fields%vs%pre(fields%vs%dim_length))
                    fields%vs%pre = huge(fields%vs%pre)
                end if
                call output_variables_allocate(fields%pre, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%pre, n1)
            case (VN_PRECRN)
                call output_variables_activate_pntr(fields, VN_PRERN)
                call output_variables_allocate(fields%precrn, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%precrn, n1)
            case (VN_PRECSNO)
                call output_variables_activate_pntr(fields, VN_PRESNO)
                call output_variables_allocate(fields%precsno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%precsno, n1)
            case (VN_PREC)
                call output_variables_activate_pntr(fields, VN_PRE)
                call output_variables_allocate(fields%prec, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%prec, n1)

            !> Canopy variables.
            case (VN_LQWSCAN)
                call output_variables_activate_pntr(fields, VN_TCAN)
                if (.not. associated(fields%vs%lqwscan)) then
                    allocate(fields%vs%lqwscan(fields%vs%dim_length))
                    fields%vs%lqwscan = huge(fields%vs%lqwscan)
                end if
                call output_variables_allocate(fields%lqwscan, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%lqwscan, n1)
            case (VN_FZWSCAN)
                call output_variables_activate_pntr(fields, VN_TCAN)
                if (.not. associated(fields%vs%fzwscan)) then
                    allocate(fields%vs%fzwscan(fields%vs%dim_length))
                    fields%vs%fzwscan = huge(fields%vs%fzwscan)
                end if
                call output_variables_allocate(fields%fzwscan, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fzwscan, n1)
            case (VN_CMAS)
                call output_variables_activate_pntr(fields, VN_TCAN)
                if (.not. associated(fields%vs%cmas)) then
                    allocate(fields%vs%cmas(fields%vs%dim_length))
                    fields%vs%cmas = huge(fields%vs%cmas)
                end if
                call output_variables_allocate(fields%cmas, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%cmas, n1)
            case (VN_TCAN)
                if (.not. associated(fields%vs%tcan)) then
                    allocate(fields%vs%tcan(fields%vs%dim_length))
                    fields%vs%tcan = huge(fields%vs%tcan)
                end if
                call output_variables_allocate(fields%tcan, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%tcan, n1)
                call output_variables_allocate(fields%ican, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%ican, n1)
            case (VN_GRO)
                call output_variables_activate_pntr(fields, VN_TCAN)
                if (.not. associated(fields%vs%gro)) then
                    allocate(fields%vs%gro(fields%vs%dim_length))
                    fields%vs%gro = huge(fields%vs%gro)
                end if
                call output_variables_allocate(fields%gro, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%gro, n1)

            !> Snow variables.
            case (VN_FSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                if (.not. associated(fields%vs%fsno)) then
                    allocate(fields%vs%fsno(fields%vs%dim_length))
                    fields%vs%fsno = huge(fields%vs%fsno)
                end if
                call output_variables_allocate(fields%fsno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsno, n1)
            case (VN_SNO)
                if (.not. associated(fields%vs%sno)) then
                    allocate(fields%vs%sno(fields%vs%dim_length))
                    fields%vs%sno = huge(fields%vs%sno)
                end if
                call output_variables_allocate(fields%sno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%sno, n1)
                call output_variables_allocate(fields%isno, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%isno, n1)
            case (VN_RHOSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                if (.not. associated(fields%vs%rhosno)) then
                    allocate(fields%vs%rhosno(fields%vs%dim_length))
                    fields%vs%rhosno = huge(fields%vs%rhosno)
                end if
                call output_variables_allocate(fields%rhosno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%rhosno, n1)
            case (VN_ZSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                call output_variables_activate_pntr(fields, VN_RHOSNO)
                call output_variables_allocate(fields%zsno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%zsno, n1)
            case (VN_LQWSSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                if (.not. associated(fields%vs%lqwssno)) then
                    allocate(fields%vs%lqwssno(fields%vs%dim_length))
                    fields%vs%lqwssno = huge(fields%vs%lqwssno)
                end if
                call output_variables_allocate(fields%lqwssno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%lqwssno, n1)
            case (VN_TSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                if (.not. associated(fields%vs%tsno)) then
                    allocate(fields%vs%tsno(fields%vs%dim_length))
                    fields%vs%tsno = huge(fields%vs%tsno)
                end if
                call output_variables_allocate(fields%tsno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%tsno, n1)
            case (VN_ALBSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                if (.not. associated(fields%vs%albsno)) then
                    allocate(fields%vs%albsno(fields%vs%dim_length))
                    fields%vs%albsno = huge(fields%vs%albsno)
                end if
                call output_variables_allocate(fields%albsno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%albsno, n1)
            case (VN_DRAINSNO)
                call output_variables_activate_pntr(fields, VN_SNO)
                if (.not. associated(fields%vs%drainsno)) then
                    allocate(fields%vs%drainsno(fields%vs%dim_length))
                    fields%vs%drainsno = huge(fields%vs%drainsno)
                end if
                call output_variables_allocate(fields%drainsno, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%drainsno, n1)

            !> Surface variables.
            case (VN_ALBT)
                call output_variables_activate_pntr(fields, VN_FSIN)
                call output_variables_allocate(fields%albt, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%albt, n1)
            case (VN_ALVS)
                call output_variables_activate_pntr(fields, VN_FSIN)
                call output_variables_allocate(fields%alvs, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%alvs, n1)
            case (VN_ALIR)
                call output_variables_activate_pntr(fields, VN_FSIN)
                call output_variables_allocate(fields%alir, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%alir, n1)
            case (VN_GTE)
                if (.not. associated(fields%vs%gte)) then
                    allocate(fields%vs%gte(fields%vs%dim_length))
                    fields%vs%gte = huge(fields%vs%gte)
                end if
                call output_variables_allocate(fields%gte, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%gte, n1)
            case (VN_ZPND)
                if (.not. associated(fields%vs%zpnd)) then
                    allocate(fields%vs%zpnd(fields%vs%dim_length))
                    fields%vs%zpnd = huge(fields%vs%zpnd)
                end if
                call output_variables_allocate(fields%zpnd, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%zpnd, n1)
            case (VN_LQWSPND)
                call output_variables_activate_pntr(fields, VN_ZPND)
                call output_variables_allocate(fields%lqwspnd, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%lqwspnd, n1)
            case (VN_TPND)
                if (.not. associated(fields%vs%tpnd)) then
                    allocate(fields%vs%tpnd(fields%vs%dim_length))
                    fields%vs%tpnd = huge(fields%vs%tpnd)
                end if
                call output_variables_allocate(fields%tpnd, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%tpnd, n1)
                call output_variables_allocate(fields%ipnd, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%ipnd, n1)
            case (VN_PNDCAF)
                if (.not. associated(fields%vs%pndcaf)) then
                    allocate(fields%vs%pndcaf(fields%vs%dim_length))
                    fields%vs%pndcaf = huge(fields%vs%pndcaf)
                end if
                call output_variables_allocate(fields%pndcaf, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%pndcaf, n1)
            case (VN_POTEVP)
                if (.not. associated(fields%vs%potevp)) then
                    allocate(fields%vs%potevp(fields%vs%dim_length))
                    fields%vs%potevp = huge(fields%vs%potevp)
                end if
                call output_variables_allocate(fields%potevp, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%potevp, n1)
            case (VN_ET)
                if (.not. associated(fields%vs%et)) then
                    allocate(fields%vs%et(fields%vs%dim_length))
                    fields%vs%et = huge(fields%vs%et)
                end if
                call output_variables_allocate(fields%et, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%et, n1)
            case (VN_EVPB)
                call output_variables_activate_pntr(fields, VN_ET)
                call output_variables_activate_pntr(fields, VN_POTEVP)
                call output_variables_allocate(fields%evpb, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%evpb, n1)
            case (VN_ARRD)
                call output_variables_activate_pntr(fields, VN_PRE)
                call output_variables_activate_pntr(fields, VN_POTEVP)
                call output_variables_allocate(fields%arrd, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%arrd, n1)
            case (VN_OVRFLW)
                if (.not. associated(fields%vs%ovrflw)) then
                    allocate(fields%vs%ovrflw(fields%vs%dim_length))
                    fields%vs%ovrflw = huge(fields%vs%ovrflw)
                end if
                call output_variables_allocate(fields%ovrflw, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%ovrflw, n1)
            case (VN_QEVP)
                if (.not. associated(fields%vs%qevp)) then
                    allocate(fields%vs%qevp(fields%vs%dim_length))
                    fields%vs%qevp = huge(fields%vs%qevp)
                end if
                call output_variables_allocate(fields%qevp, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%qevp, n1)
            case (VN_QSENS)
                if (.not. associated(fields%vs%qsens)) then
                    allocate(fields%vs%qsens(fields%vs%dim_length))
                    fields%vs%qsens = huge(fields%vs%qsens)
                end if
                call output_variables_allocate(fields%qsens, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%qsens, n1)
            case (VN_GZERO)
                if (.not. associated(fields%vs%gzero)) then
                    allocate(fields%vs%gzero(fields%vs%dim_length))
                    fields%vs%gzero = huge(fields%vs%gzero)
                end if
                call output_variables_allocate(fields%gzero, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%gzero, n1)
            case (VN_TSURF)
                if (.not. associated(fields%vs%tsurf)) then
                    allocate(fields%vs%tsurf(fields%vs%dim_length))
                    fields%vs%tsurf = huge(fields%vs%tsurf)
                end if
                call output_variables_allocate(fields%tsurf, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%tsurf, n1)

            !> Ice/glacier variables.
            case (VN_LQWSICE)
                if (.not. associated(fields%vs%lqwsice)) then
                    allocate(fields%vs%lqwsice(fields%vs%dim_length))
                    fields%vs%lqwsice = huge(fields%vs%lqwsice)
                end if
                call output_variables_allocate(fields%lqwsice, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%lqwsice, n1)
            case (VN_TICE)
                if (.not. associated(fields%vs%tice)) then
                    allocate(fields%vs%tice(fields%vs%dim_length))
                    fields%vs%tice = huge(fields%vs%tice)
                end if
                call output_variables_allocate(fields%tice, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%tice, n1)
                call output_variables_allocate(fields%iice, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%iice, n1)

            !> Subsurface/soil variables.
            case (VN_THLQSOL)
                if (.not. associated(fields%vs%thlqsol)) then
                    allocate(fields%vs%thlqsol(fields%vs%dim_length, size(fields%vs%dzsol, 1)))
                    fields%vs%thlqsol = huge(fields%vs%thlqsol)
                end if
                call output_variables_allocate(fields%thlqsol, n1, n2, pntr, ig)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%thlqsol, n1, n2)
            case (VN_THICSOL)
                if (.not. associated(fields%vs%thicsol)) then
                    allocate(fields%vs%thicsol(fields%vs%dim_length, size(fields%vs%dzsol, 1)))
                    fields%vs%thicsol = huge(fields%vs%thicsol)
                end if
                call output_variables_allocate(fields%thicsol, n1, n2, pntr, ig)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%thicsol, n1, n2)
            case (VN_LQWSSOL)
                if (associated(fields%vs%dzsolhyd)) then
                    call output_variables_activate_pntr(fields, VN_THLQSOL)
                    call output_variables_allocate(fields%lqwssol, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%lqwssol, n1, n2)
                end if
            case (VN_FZWSSOL)
                if (associated(fields%vs%dzsolhyd)) then
                    call output_variables_activate_pntr(fields, VN_THICSOL)
                    call output_variables_allocate(fields%fzwssol, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%fzwssol, n1, n2)
                end if
            case (VN_ALWSSOL)
                call output_variables_activate_pntr(fields, VN_LQWSSOL)
                call output_variables_activate_pntr(fields, VN_FZWSSOL)
                call output_variables_allocate(fields%alwssol, n1, n2, pntr, ig)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%alwssol, n1, n2)
            case (VN_TSOL)
                if (.not. associated(fields%vs%tsol)) then
                    allocate(fields%vs%tsol(fields%vs%dim_length, size(fields%vs%dzsol, 1)))
                    fields%vs%tsol = huge(fields%vs%tsol)
                end if
                call output_variables_allocate(fields%tsol, n1, n2, pntr, ig)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%tsol, n1, n2)
            case (VN_GFLX)
                if (.not. associated(fields%vs%gflx)) then
                    allocate(fields%vs%gflx(fields%vs%dim_length, size(fields%vs%dzsol, 1)))
                    fields%vs%gflx = huge(fields%vs%gflx)
                end if
                call output_variables_allocate(fields%gflx, n1, n2, pntr, ig)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%gflx, n1, n2)
            case (VN_LATFLW)
                if (.not. associated(fields%vs%latflw)) then
                    allocate(fields%vs%latflw(fields%vs%dim_length, size(fields%vs%dzsol, 1)))
                    fields%vs%latflw = huge(fields%vs%latflw)
                end if
                call output_variables_allocate(fields%latflw, n1, n2, pntr, ig)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%latflw, n1, n2)
            case (VN_ZSOLSAT)
                if (.not. associated(fields%vs%zsolsat)) then
                    allocate(fields%vs%zsolsat(fields%vs%dim_length))
                    fields%vs%zsolsat = huge(fields%vs%zsolsat)
                end if
                call output_variables_allocate(fields%zsolsat, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%zsolsat, n1)
            case (VN_DRAINSOL)
                if (.not. associated(fields%vs%drainsol)) then
                    allocate(fields%vs%drainsol(fields%vs%dim_length))
                    fields%vs%drainsol = huge(fields%vs%drainsol)
                end if
                call output_variables_allocate(fields%drainsol, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%drainsol, n1)

            !> Groundwater/lower zone storage variables.
            case (VN_RCHG)
                if (.not. associated(fields%vs%rchg)) then
                    allocate(fields%vs%rchg(fields%vs%dim_length))
                    fields%vs%rchg = huge(fields%vs%rchg)
                end if
                call output_variables_allocate(fields%rchg, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%rchg, n1)
            case (VN_STGGW)
                if (.not. associated(fields%vs%stggw)) then
                    allocate(fields%vs%stggw(fields%vs%dim_length))
                    fields%vs%stggw = huge(fields%vs%stggw)
                end if
                call output_variables_allocate(fields%stggw, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stggw, n1)
            case (VN_LKG)
                if (.not. associated(fields%vs%lkg)) then
                    allocate(fields%vs%lkg(fields%vs%dim_length))
                    fields%vs%lkg = huge(fields%vs%lkg)
                end if
                call output_variables_allocate(fields%lkg, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%lkg, n1)
!-            case (VN_DZS)
!-                if (.not. associated(fields%vs%dzs)) then
!-                    allocate(fields%vs%dzs(fields%vs%dim_length))
!-                    fields%vs%dzs = huge(fields%vs%dzs)
!-                end if
!-                call output_variables_allocate(fields%dzs, n1, pntr)
!-                if (associated(fields%ts)) call output_variables_allocate(fields%ts%dzs, n1)

            !> Diagnostic variables.
            case (VN_STGE)
                call output_variables_allocate(fields%stge, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stge, n1)
                call output_variables_allocate(fields%stg0e, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stg0e, n1)
                call output_variables_allocate(fields%dstge, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%dstge, n1)
            case (VN_DSTGE)
                call output_variables_activate_pntr(fields, VN_STGE)
                call output_variables_allocate(fields%dstge, n1, pntr)
            case (VN_STGW)
                call output_variables_activate_pntr(fields, VN_LQWSCAN)
                call output_variables_activate_pntr(fields, VN_FZWSCAN)
                call output_variables_activate_pntr(fields, VN_SNO)
                call output_variables_activate_pntr(fields, VN_LQWSSNO)
                call output_variables_activate_pntr(fields, VN_LQWSPND)
                call output_variables_activate_pntr(fields, VN_LQWSSOL)
                call output_variables_activate_pntr(fields, VN_FZWSSOL)
                call output_variables_activate_pntr(fields, VN_STGGW)
!-                call output_variables_activate_pntr(fields, VN_DZS)
                call output_variables_allocate(fields%stgw, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stgw, n1)
                call output_variables_allocate(fields%stg0w, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stg0w, n1)
                call output_variables_allocate(fields%dstgw, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%dstgw, n1)
            case (VN_DSTGW)
                call output_variables_activate_pntr(fields, VN_STGW)
                call output_variables_allocate(fields%dstgw, n1, pntr)

            !> Routing variables.
            case (VN_RFF)
                if (.not. associated(fields%vs%rff)) then
                    allocate(fields%vs%rff(fields%vs%dim_length))
                    fields%vs%rff = huge(fields%vs%rff)
                end if
                call output_variables_allocate(fields%rff, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%rff, n1)
            case (VN_ROF)
                call output_variables_activate_pntr(fields, VN_OVRFLW)
                call output_variables_activate_pntr(fields, VN_LATFLW)
                call output_variables_activate_pntr(fields, VN_DRAINSOL)
                call output_variables_allocate(fields%rof, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%rof, n1)
            case (VN_QI)
                if (.not. associated(fields%vs%qi)) then
                    allocate(fields%vs%qi(fields%vs%dim_length))
                    fields%vs%qi = huge(fields%vs%qi)
                end if
                call output_variables_allocate(fields%qi, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%qi, n1)
            case (VN_QO)
                if (.not. associated(fields%vs%qo)) then
                    allocate(fields%vs%qo(fields%vs%dim_length))
                    fields%vs%qo = huge(fields%vs%qo)
                end if
                call output_variables_allocate(fields%qo, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%qo, n1)
            case (VN_STGCH)
                if (.not. associated(fields%vs%stgch)) then
                    allocate(fields%vs%stgch(fields%vs%dim_length))
                    fields%vs%stgch = huge(fields%vs%stgch)
                end if
                call output_variables_allocate(fields%stgch, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stgch, n1)
            case (VN_ZLVL)
                if (.not. associated(fields%vs%zlvl)) then
                    allocate(fields%vs%zlvl(fields%vs%dim_length))
                    fields%vs%zlvl = huge(fields%vs%zlvl)
                end if
                call output_variables_allocate(fields%zlvl, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%zlvl, n1)
        end select

    end subroutine

    !> Description:
    !>  Allocate the specified output variables.
    subroutine output_variables_activate_vnames(fields, vnames)

        !> Input variables.
        type(output_fields), intent(in) :: fields
        character(len = *), dimension(:), intent(in) :: vnames

        !> Local variables.
        integer i

        !> Allocate.
        do i = 1, size(vnames)
            call output_variables_activate_pntr(fields, vnames(i))
        end do

    end subroutine

    !> Description:
    !>  Initialize and associate the attributes of the output
    !>  variables group.
    subroutine output_variables_group_init(group_vs, group, n1, n2)

        !> Input variables.
        type(model_variables_fields), intent(in), pointer :: group_vs
        integer, intent(in) :: n1, n2

        !> Input/output variables.
        type(output_fields), pointer :: group

        !> Allocate.
        if (.not. associated(group)) allocate(group)

        !> Assign indices.
        group%n1 = n1
        group%n2 = n2

        !> Associate to model variable group.
        group%vs => group_vs

    end subroutine

    !> Description:
    !>  Associate the output variable 'ts' group.
    subroutine output_variables_group_associate_ts(series)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input/output variables.
        type(output_series) series

        !> Associate to the 'ts' group.
        if (ro%RUNTILE) then
            series%tile%ts => out%ts%tile
        end if
        if (ro%RUNGRID) then
            series%grid%ts => out%ts%grid
            series%basin%ts => out%ts%basin
        end if

    end subroutine

    !> Description:
    !>  Set output variables to the 'NO_DATA' value.
    subroutine output_variables_group_reset(group)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input/output variables.
        type(output_fields) group

        !> Meteorology/climatology variables.
        if (associated(group%ifsin)) group%ifsin = 0.0
        if (associated(group%fsin)) group%fsin = out%NO_DATA
        if (associated(group%fsvs)) group%fsvs = out%NO_DATA
        if (associated(group%fsir)) group%fsir = out%NO_DATA
        if (associated(group%fsdr)) group%fsdr = out%NO_DATA
        if (associated(group%fsdff)) group%fsdff = out%NO_DATA
        if (associated(group%fsout)) group%fsout = out%NO_DATA
        if (associated(group%flin)) group%flin = out%NO_DATA
        if (associated(group%flout)) group%flout = out%NO_DATA
        if (associated(group%ta)) group%ta = out%NO_DATA
        if (associated(group%qa)) group%qa = out%NO_DATA
        if (associated(group%pres)) group%pres = out%NO_DATA
        if (associated(group%uu)) group%uu = out%NO_DATA
        if (associated(group%vv)) group%vv = out%NO_DATA
        if (associated(group%uv)) group%uv = out%NO_DATA
        if (associated(group%wdir)) group%wdir = out%NO_DATA
        if (associated(group%prern)) group%prern = out%NO_DATA
        if (associated(group%presno)) group%presno = out%NO_DATA
        if (associated(group%pre)) group%pre = out%NO_DATA
        if (associated(group%precrn)) group%precrn = out%NO_DATA
        if (associated(group%precsno)) group%precsno = out%NO_DATA
        if (associated(group%prec)) group%prec = out%NO_DATA

        !> Canopy variables.
        if (associated(group%ican)) group%ican = 0.0
        if (associated(group%lqwscan)) group%lqwscan = out%NO_DATA
        if (associated(group%fzwscan)) group%fzwscan = out%NO_DATA
        if (associated(group%cmas)) group%cmas = out%NO_DATA
        if (associated(group%tcan)) group%tcan = out%NO_DATA
        if (associated(group%gro)) group%gro = out%NO_DATA

        !> Snow variables.
        if (associated(group%isno)) group%isno = 0.0
        if (associated(group%fsno)) group%fsno = out%NO_DATA
        if (associated(group%sno)) group%sno = out%NO_DATA
        if (associated(group%rhosno)) group%rhosno = out%NO_DATA
        if (associated(group%zsno)) group%zsno = out%NO_DATA
        if (associated(group%lqwssno)) group%lqwssno = out%NO_DATA
        if (associated(group%tsno)) group%tsno = out%NO_DATA
        if (associated(group%albsno)) group%albsno = out%NO_DATA
        if (associated(group%drainsno)) group%drainsno = out%NO_DATA

        !> Surface variables.
        if (associated(group%albt)) group%albt = out%NO_DATA
        if (associated(group%alvs)) group%alvs = out%NO_DATA
        if (associated(group%alir)) group%alir = out%NO_DATA
        if (associated(group%gte)) group%gte = out%NO_DATA
        if (associated(group%ipnd)) group%ipnd = 0.0
        if (associated(group%zpnd)) group%zpnd = out%NO_DATA
        if (associated(group%lqwspnd)) group%lqwspnd = out%NO_DATA
        if (associated(group%tpnd)) group%tpnd = out%NO_DATA
        if (associated(group%pndcaf)) group%pndcaf = out%NO_DATA
        if (associated(group%potevp)) group%potevp = out%NO_DATA
        if (associated(group%et)) group%et = out%NO_DATA
        if (associated(group%evpb)) group%evpb = out%NO_DATA
        if (associated(group%arrd)) group%arrd = out%NO_DATA
        if (associated(group%ovrflw)) group%ovrflw = out%NO_DATA
        if (associated(group%qevp)) group%qevp = out%NO_DATA
        if (associated(group%qsens)) group%qsens = out%NO_DATA
        if (associated(group%gzero)) group%gzero = out%NO_DATA
        if (associated(group%tsurf)) group%tsurf = out%NO_DATA

        !> Ice/glacier variables.
        if (associated(group%iice)) group%iice = 0.0
        if (associated(group%lqwsice)) group%lqwsice = out%NO_DATA
        if (associated(group%tice)) group%tice = out%NO_DATA

        !> Subsurface/soil variables.
        if (associated(group%thlqsol)) group%thlqsol = out%NO_DATA
        if (associated(group%thicsol)) group%thicsol = out%NO_DATA
        if (associated(group%lqwssol)) group%lqwssol = out%NO_DATA
        if (associated(group%fzwssol)) group%fzwssol = out%NO_DATA
        if (associated(group%alwssol)) group%alwssol = out%NO_DATA
        if (associated(group%tsol)) group%tsol = out%NO_DATA
        if (associated(group%gflx)) group%gflx = out%NO_DATA
        if (associated(group%latflw)) group%latflw = out%NO_DATA
        if (associated(group%zsolsat)) group%zsolsat = out%NO_DATA
        if (associated(group%drainsol)) group%drainsol = out%NO_DATA

        !> Groundwater/lower zone storage variables.
        if (associated(group%rchg)) group%rchg = out%NO_DATA
        if (associated(group%stggw)) group%stggw = out%NO_DATA
        if (associated(group%lkg)) group%lkg = out%NO_DATA
!-        if (associated(group%dzs)) group%dzs = out%NO_DATA

        !> Diagnostic variables.
        if (associated(group%stge)) then
            group%stg0e = group%stge
            group%stge = 0.0
            group%dstge = 0.0
        end if
        if (associated(group%stgw)) then
            group%stg0w = group%stgw
            group%stgw = 0.0
            group%dstgw = 0.0
        end if

        !> Routing variables.
        if (associated(group%rff)) group%rff = out%NO_DATA
        if (associated(group%rof)) group%rof = out%NO_DATA
        if (associated(group%qi)) group%qi = out%NO_DATA
        if (associated(group%qo)) group%qo = out%NO_DATA
        if (associated(group%stgch)) group%stgch = out%NO_DATA
        if (associated(group%zlvl)) group%zlvl = out%NO_DATA

    end subroutine

    !> Description:
    !>  Reset output variables to the 'NO_DATA' value.
    subroutine output_variables_reset(shd)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Set variables to the 'NO_DATA' value.
        if (ro%RUNTILE) then
            call output_variables_group_reset(out%ts%tile)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_reset(out%ts%grid)
            call output_variables_group_reset(out%ts%basin)
        end if

    end subroutine

    !> Description:
    !>  Allocate and initialize output variables.
    subroutine output_variables_series_init(shd, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Allocate and initialize the variables.
        if (ro%RUNTILE) then
            call output_variables_group_init(vs%tile, series%tile, shd%lc%NML, shd%lc%IGND)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_init(vs%grid, series%grid, shd%NA, shd%lc%IGND)
            call output_variables_group_init(vs%basin, series%basin, shd%NA, shd%lc%IGND)
        end if

    end subroutine

    !> Description:
    !>  Allocate and initialize output variables.
    subroutine output_variables_init(shd)

        !> 'shd_variables' required for 'shd'.
        use shd_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Initialize 'ts' values.
        call output_variables_series_init(shd, out%ts)

        !> Totals (e.g., accumulated).
        call output_variables_series_init(shd, out%tot)
        call output_variables_group_associate_ts(out%tot)

        !> Yearly.
        call output_variables_series_init(shd, out%y)
        call output_variables_group_associate_ts(out%y)

        !> Monthly.
        call output_variables_series_init(shd, out%m)
        call output_variables_group_associate_ts(out%m)

        !> Daily.
        call output_variables_series_init(shd, out%d)
        call output_variables_group_associate_ts(out%d)

        !> Hourly.
        call output_variables_series_init(shd, out%h)
        call output_variables_group_associate_ts(out%h)

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    !>  Variables are updated if all elements of the group are equal to
    !>  the 'NO_DATA' value; if not the case, updates are assumed to
    !>  have occured in the model (e.g., by process modules), and those
    !>  values are preserved.
    subroutine output_variables_group_update_ts(shd, group, group_vs)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'model_dates' required for 'ic' (counter and time-stepping).
        use shd_variables
        use control_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(model_variables_fields), intent(in) :: group_vs

        !> Input/output variables.
        type(output_fields) group

        !> Local variables.
        integer j

        !> Meteorology/climatology variables.
        if (associated(group%fsin)) then
            if (all(group%fsin == out%NO_DATA)) then
                if (all(group_vs%fsin /= huge(group_vs%fsin))) then
                    group%fsin = group_vs%fsin
                else
                    group%fsin = 0.0
                end if
            end if
        end if
        if (associated(group%ifsin)) then
            where (group%fsin > 0.0)
                group%ifsin = 1.0
            elsewhere
                group%ifsin = 0.0
            end where
        end if
        if (associated(group%fsvs)) then
            if (all(group%fsvs == out%NO_DATA)) then
                where (group%ifsin == 1.0)
                    group%fsvs = group%fsin*0.5
                elsewhere
                    group%fsvs = 0.0
                end where
            end if
        end if
        if (associated(group%fsir)) then
            if (all(group%fsir == out%NO_DATA)) then
                where (group%ifsin == 1.0)
                    group%fsir = group%fsin*0.5
                elsewhere
                    group%fsir = 0.0
                end where
            end if
        end if
        if (associated(group%fsdr)) then
            if (all(group%fsdr == out%NO_DATA)) then
                if (all(group_vs%fsdr /= huge(group_vs%fsdr))) then
                    group%fsdr = group_vs%fsdr
                else
                    group%fsdr = 0.0
                end if
            end if
        end if
        if (associated(group%fsdff)) then
            if (all(group%fsdff == out%NO_DATA)) then
                if (all(group_vs%fsdff /= huge(group_vs%fsdff))) then
                    group%fsdff = group_vs%fsdff
                else
                    group%fsdff = 0.0
                end if
            end if
        end if
        if (associated(group%flin)) then
            if (all(group%flin == out%NO_DATA)) then
                if (all(group_vs%flin /= huge(group_vs%flin))) then
                    group%flin = group_vs%flin
                else
                    group%flin = 0.0
                end if
            end if
        end if
        if (associated(group%ta)) then
            if (all(group%ta == out%NO_DATA)) then
                if (all(group_vs%ta /= huge(group_vs%ta))) then
                    group%ta = group_vs%ta
                else
                    group%ta = 0.0
                end if
            end if
        end if
        if (associated(group%qa)) then
            if (all(group%qa == out%NO_DATA)) then
                if (all(group_vs%qa /= huge(group_vs%qa))) then
                    group%qa = group_vs%qa
                else
                    group%qa = 0.0
                end if
            end if
        end if
        if (associated(group%pres)) then
            if (all(group%pres == out%NO_DATA)) then
                if (all(group_vs%pres /= huge(group_vs%pres))) then
                    group%pres = group_vs%pres
                else
                    group%pres = 0.0
                end if
            end if
        end if
        if (associated(group%uu)) then
            if (all(group%uu == out%NO_DATA)) then
                if (all(group_vs%uu /= huge(group_vs%uu))) then
                    group%uu = group_vs%uu
                else
                    group%uu = 0.0
                end if
            end if
        end if
        if (associated(group%vv)) then
            if (all(group%vv == out%NO_DATA)) then
                if (all(group_vs%vv /= huge(group_vs%vv))) then
                    group%vv = group_vs%vv
                else
                    group%vv = 0.0
                end if
            end if
        end if
        if (associated(group%uv)) then
            if (all(group%uv == out%NO_DATA)) then
                if (all(group_vs%uv /= huge(group_vs%uv))) then
                    group%uv = group_vs%uv
                else
                    group%uv = 0.0
                end if
            end if
        end if
        if (associated(group%wdir)) then
            if (all(group%wdir == out%NO_DATA)) then
                if (all(group_vs%wdir /= huge(group_vs%wdir))) then
                    group%wdir = group_vs%wdir
                else
                    group%wdir = 0.0
                end if
            end if
        end if
        if (associated(group%prern)) then
            if (all(group%prern == out%NO_DATA)) then
                if (all(group_vs%prern /= huge(group_vs%prern))) then
                    group%prern = group_vs%prern
                else
                    group%prern = 0.0
                end if
                if (associated(group%precrn)) then
                    if (all(group%precrn == out%NO_DATA)) group%precrn = group%prern*ic%dts
                end if
            end if
        end if
        if (associated(group%presno)) then
            if (all(group%presno == out%NO_DATA)) then
                if (all(group_vs%presno /= huge(group_vs%presno))) then
                    group%presno = group_vs%presno
                else
                    group%presno = 0.0
                end if
                if (associated(group%precsno)) then
                    if (all(group%precsno == out%NO_DATA)) group%precsno = group%presno*ic%dts
                end if
            end if
        end if
        if (associated(group%pre)) then
            if (all(group%pre == out%NO_DATA)) then
                if (all(group_vs%pre /= huge(group_vs%pre))) then
                    group%pre = group_vs%pre
                else
                    group%pre = 0.0
                end if
                if (associated(group%prec)) then
                    if (all(group%prec == out%NO_DATA)) group%prec = group%pre*ic%dts
                end if
            end if
        end if

        !> Canopy variables.
        if (associated(group%tcan)) then
            if (all(group%tcan == out%NO_DATA)) then
                if (all(group_vs%tcan /= huge(group_vs%tcan))) then
                    group%tcan = group_vs%tcan
                else
                    group%tcan = 0.0
                end if
            end if
        end if
        if (associated(group%ican)) then
            where (group%tcan > 0.0)
                group%ican = 1.0
            elsewhere
                group%ican = 0.0
            end where
        end if
        if (associated(group%lqwscan)) then
            if (all(group%lqwscan == out%NO_DATA)) then
                if (all(group_vs%lqwscan /= huge(group_vs%lqwscan))) then
                    where (group%ican == 1.0)
                        group%lqwscan = group_vs%lqwscan
                    elsewhere
                        group%lqwscan = 0.0
                    end where
                else
                    group%lqwscan = 0.0
                end if
            end if
        end if
        if (associated(group%fzwscan)) then
            if (all(group%fzwscan == out%NO_DATA)) then
                if (all(group_vs%fzwscan /= huge(group_vs%fzwscan))) then
                    where (group%ican == 1.0)
                        group%fzwscan = group_vs%fzwscan
                    elsewhere
                        group%fzwscan = 0.0
                    end where
                else
                    group%fzwscan = 0.0
                end if
            end if
        end if
        if (associated(group%cmas)) then
            if (all(group%cmas == out%NO_DATA)) then
                if (all(group_vs%cmas /= huge(group_vs%cmas))) then
                    where (group%ican == 1.0)
                        group%cmas = group_vs%cmas
                    elsewhere
                        group%cmas = 0.0
                    end where
                else
                    group%cmas = 0.0
                end if
            end if
        end if
        if (associated(group%gro)) then
            if (all(group%gro == out%NO_DATA)) then
                if (all(group_vs%gro /= huge(group_vs%gro))) then
                    where (group%ican == 1.0)
                        group%gro = group_vs%gro
                    elsewhere
                        group%gro = 0.0
                    end where
                else
                    group%gro = 0.0
                end if
            end if
        end if

        !> Snow variables.
        if (associated(group%isno)) then
            where (group_vs%tsno > 0.0)
                group%isno = 1.0
            elsewhere
                group%isno = 0.0
            end where
        end if
        if (associated(group%fsno)) then
            if (all(group%fsno == out%NO_DATA)) then
                if (all(group_vs%fsno /= huge(group_vs%fsno))) then
                    where (group%isno == 1.0)
                        group%fsno = group_vs%fsno
                    elsewhere
                        group%fsno = 0.0
                    end where
                else
                    group%fsno = 0.0
                end if
            end if
        end if
        if (associated(group%sno)) then
            if (all(group%sno == out%NO_DATA)) then
                if (all(group_vs%sno /= huge(group_vs%sno))) then
                    where (group%isno == 1.0)
                        group%sno = group_vs%sno
                    elsewhere
                        group%sno = 0.0
                    end where
                else
                    group%sno = 0.0
                end if
            end if
        end if
        if (associated(group%rhosno)) then
            if (all(group%rhosno == out%NO_DATA)) then
                if (all(group_vs%rhosno /= huge(group_vs%rhosno))) then
                    where (group%isno == 1.0)
                        group%rhosno = group_vs%rhosno
                    elsewhere
                        group%rhosno = 0.0
                    end where
                else
                    group%rhosno = 0.0
                end if
            end if
        end if
        if (associated(group%zsno)) then
            if (all(group%zsno == out%NO_DATA)) then
                where (group%rhosno > 0.0)
                    group%zsno = group%sno/group%rhosno
                elsewhere
                    group%zsno = 0.0
                end where
            end if
        end if
        if (associated(group%lqwssno)) then
            if (all(group%lqwssno == out%NO_DATA)) then
                if (all(group_vs%lqwssno /= huge(group_vs%lqwssno))) then
                    where (group%isno == 1.0)
                        group%lqwssno = group_vs%lqwssno
                    elsewhere
                        group%lqwssno = 0.0
                    end where
                else
                    group%lqwssno = 0.0
                end if
            end if
        end if
        if (associated(group%tsno)) then
            if (all(group%tsno == out%NO_DATA)) then
                if (all(group_vs%tsno /= huge(group_vs%tsno))) then
                    group%tsno = group_vs%tsno
                else
                    group%tsno = 0.0
                end if
            end if
        end if
        if (associated(group%albsno)) then
            if (all(group%albsno == out%NO_DATA)) then
                if (all(group_vs%albsno /= huge(group_vs%albsno))) then
                    where (group%isno == 1.0)
                        group%albsno = group_vs%albsno
                    elsewhere
                        group%albsno = 0.0
                    end where
                else
                    group%albsno = 0.0
                end if
            end if
        end if
        if (associated(group%drainsno)) then
            if (all(group%drainsno == out%NO_DATA)) then
                if (all(group_vs%drainsno /= huge(group_vs%drainsno))) then
                    where (group%isno == 1.0)
                        group%drainsno = group_vs%drainsno
                    elsewhere
                        group%drainsno = 0.0
                    end where
                else
                    group%drainsno = 0.0
                end if
            end if
        end if

        !> Surface variables.
        if (associated(group%albt)) then
            if (all(group%albt == out%NO_DATA)) then
                if (all(group_vs%albt /= huge(group_vs%albt))) then
                    group%albt = group_vs%albt
                else
                    group%albt = 0.0
                end if
            end if
        end if
        if (associated(group%alvs)) then
            if (all(group%alvs == out%NO_DATA)) then
                if (all(group_vs%alvs /= huge(group_vs%alvs))) then
                    group%alvs = group_vs%alvs
                else
                    group%alvs = 0.0
                end if
            end if
        end if
        if (associated(group%alir)) then
            if (all(group%alir == out%NO_DATA)) then
                if (all(group_vs%alir /= huge(group_vs%alir))) then
                    group%alir = group_vs%alir
                else
                    group%alir = 0.0
                end if
            end if
        end if
        if (associated(group%fsout)) then
            if (all(group%fsout == out%NO_DATA)) then
                where (group%ifsin == 1.0)
                    group%fsout = group%fsin*(1.0 - group%albt)
                elsewhere
                    group%fsout = 0.0
                end where
            end if
        end if
        if (associated(group%gte)) then
            if (all(group%gte == out%NO_DATA)) then
                if (all(group_vs%gte /= huge(group_vs%gte))) then
                    group%gte = group_vs%gte
                else
                    group%gte = 0.0
                end if
                if (associated(group%flout)) then
                    if (all(group%flout == out%NO_DATA)) then
                        where (group%gte > 0.0)
                            group%flout = 5.66796E-8*group%gte**4
                        elsewhere
                            group%flout = 0.0
                        end where
                    end if
                end if
            end if
        end if
        if (associated(group%tpnd)) then
            if (all(group%tpnd == out%NO_DATA)) then
                if (all(group_vs%tpnd /= huge(group_vs%tpnd))) then
                    group%tpnd = group_vs%tpnd
                else
                    group%tpnd = 0.0
                end if
            end if
        end if
        if (associated(group%ipnd)) then
            where (group%tpnd > 0.0)
                group%ipnd = 1.0
            elsewhere
                group%ipnd = 0.0
            end where
        end if
        if (associated(group%zpnd)) then
            if (all(group%zpnd == out%NO_DATA)) then
                if (all(group_vs%zpnd /= huge(group_vs%zpnd))) then
                    group%zpnd = group_vs%zpnd
                else
                    group%zpnd = 0.0
                end if
            end if
        end if
        if (associated(group%lqwspnd)) then
            if (all(group%lqwspnd == out%NO_DATA)) group%lqwspnd = group%zpnd*1000.0
        end if
        if (associated(group%pndcaf)) then
            if (all(group%pndcaf == out%NO_DATA)) then
                if (all(group_vs%pndcaf /= huge(group_vs%pndcaf))) then
                    group%pndcaf = group_vs%pndcaf
                else
                    group%pndcaf = 0.0
                end if
            end if
        end if
        if (associated(group%potevp)) then
            if (all(group%potevp == out%NO_DATA)) then
                if (all(group_vs%potevp /= huge(group_vs%potevp))) then
                    group%potevp = group_vs%potevp
                else
                    group%potevp = 0.0
                end if
            end if
        end if
        if (associated(group%et)) then
            if (all(group%et == out%NO_DATA)) then
                if (all(group_vs%et /= huge(group_vs%et))) then
                    group%et = group_vs%et
                else
                    group%et = 0.0
                end if
            end if
        end if
        if (associated(group%evpb)) then
            if (all(group%evpb == out%NO_DATA)) then
                where (group%potevp > 0.0)
                    group%evpb = group%et/group%potevp
                elsewhere
                    group%evpb = 0.0
                end where
            end if
        end if
        if (associated(group%arrd)) then
            if (all(group%arrd == out%NO_DATA)) then
                where (group%potevp > 0.0)
                    group%arrd = group%pre/group%potevp
                elsewhere
                    group%arrd = 0.0
                end where
            end if
        end if
        if (associated(group%ovrflw)) then
            if (all(group%ovrflw == out%NO_DATA)) then
                if (all(group_vs%ovrflw /= huge(group_vs%ovrflw))) then
                    group%ovrflw = group_vs%ovrflw
                else
                    group%ovrflw = 0.0
                end if
            end if
        end if
        if (associated(group%qevp)) then
            if (all(group%qevp == out%NO_DATA)) then
                if (all(group_vs%qevp /= huge(group_vs%qevp))) then
                    group%qevp = group_vs%qevp
                else
                    group%qevp = 0.0
                end if
            end if
        end if
        if (associated(group%qsens)) then
            if (all(group%qsens == out%NO_DATA)) then
                if (all(group_vs%qsens /= huge(group_vs%qsens))) then
                    group%qsens = group_vs%qsens
                else
                    group%qsens = 0.0
                end if
            end if
        end if
        if (associated(group%gzero)) then
            if (all(group%gzero == out%NO_DATA)) then
                if (all(group_vs%gzero /= huge(group_vs%gzero))) then
                    group%gzero = group_vs%gzero
                else
                    group%gzero = 0.0
                end if
            end if
        end if
        if (associated(group%tsurf)) then
            if (all(group%tsurf == out%NO_DATA)) then
                if (all(group_vs%tsurf /= huge(group_vs%tsurf))) then
                    group%tsurf = group_vs%tsurf
                else
                    group%tsurf = 0.0
                end if
            end if
        end if

        !> Ice/glacier variables.
        if (associated(group%tice)) then
            if (all(group%tice == out%NO_DATA)) then
                if (all(group_vs%tice /= huge(group_vs%tice))) then
                    group%tice = group_vs%tice
                else
                    group%tice = 0.0
                end if
            end if
        end if
        if (associated(group%iice)) then
            where (group%tice > 0.0)
                group%iice = 1.0
            elsewhere
                group%iice = 0.0
            end where
        end if
        if (associated(group%lqwsice)) then
            if (all(group%lqwsice == out%NO_DATA)) then
                if (all(group_vs%lqwsice /= huge(group_vs%lqwsice))) then
                    group%lqwsice = group_vs%lqwsice
                else
                    group%lqwsice = 0.0
                end if
            end if
        end if

        !> Subsurface/soil variables.
        if (associated(group%thlqsol)) then
            if (all(group%thlqsol == out%NO_DATA)) then
                if (all(group_vs%thlqsol /= huge(group_vs%thlqsol))) then
                    group%thlqsol = group_vs%thlqsol
                else
                    group%thlqsol = 0.0
                end if
            end if
        end if
        if (associated(group%thicsol)) then
            if (all(group%thicsol == out%NO_DATA)) then
                if (all(group_vs%thicsol /= huge(group_vs%thicsol))) then
                    group%thicsol = group_vs%thicsol
                else
                    group%thicsol = 0.0
                end if
            end if
        end if
        if (associated(group%lqwssol)) then
            if (all(group%lqwssol == out%NO_DATA)) then
                if (all(group_vs%dzsolhyd /= huge(group_vs%dzsolhyd))) then
                    group%lqwssol = group%thlqsol*group_vs%dzsolhyd*1000.0
                else
                    group%lqwssol = 0.0
                end if
            end if
        end if
        if (associated(group%fzwssol)) then
            if (all(group%fzwssol == out%NO_DATA)) then
                if (all(group_vs%dzsolhyd /= huge(group_vs%dzsolhyd))) then
                    group%fzwssol = group%thicsol*group_vs%dzsolhyd*917.0
                else
                    group%fzwssol = 0.0
                end if
            end if
        end if
        if (associated(group%alwssol)) then
            if (all(group%alwssol == out%NO_DATA)) then
                group%alwssol = 0.0
                where (group%lqwssol /= out%NO_DATA) group%alwssol = group%alwssol + group%lqwssol
                where (group%fzwssol /= out%NO_DATA) group%alwssol = group%alwssol + group%fzwssol
            end if
        end if
        if (associated(group%tsol)) then
            if (all(group%tsol == out%NO_DATA)) then
                if (all(group_vs%tsol /= huge(group_vs%tsol))) then
                    group%tsol = group_vs%tsol
                else
                    group%tsol = 0.0
                end if
            end if
        end if
        if (associated(group%gflx)) then
            if (all(group%gflx == out%NO_DATA)) then
                if (all(group_vs%gflx /= huge(group_vs%gflx))) then
                    group%gflx = group_vs%gflx
                else
                    group%gflx = 0.0
                end if
            end if
        end if
        if (associated(group%latflw)) then
            if (all(group%latflw == out%NO_DATA)) then
                if (all(group_vs%latflw /= huge(group_vs%latflw))) then
                    group%latflw = group_vs%latflw
                else
                    group%latflw = 0.0
                end if
            end if
        end if
        if (associated(group%zsolsat)) then
            if (all(group%zsolsat == out%NO_DATA)) then
                if (all(group_vs%zsolsat /= huge(group_vs%zsolsat))) then
                    group%zsolsat = group_vs%zsolsat
                else
                    group%zsolsat = 0.0
                end if
            end if
        end if
        if (associated(group%drainsol)) then
            if (all(group%drainsol == out%NO_DATA)) then
                if (all(group_vs%drainsol /= huge(group_vs%drainsol))) then
                    group%drainsol = group_vs%drainsol
                else
                    group%drainsol = 0.0
                end if
            end if
        end if

        !> Groundwater/lower zone storage variables.
        if (associated(group%rchg)) then
            if (all(group%rchg == out%NO_DATA)) then
                if (all(group_vs%rchg /= huge(group_vs%rchg))) then
                    group%rchg = group_vs%rchg
                else
                    group%rchg = 0.0
                end if
            end if
        end if
        if (associated(group%stggw)) then
            if (all(group%stggw == out%NO_DATA)) then
                if (all(group_vs%stggw /= huge(group_vs%stggw))) then
                    group%stggw = group_vs%stggw
                else
                    group%stggw = 0.0
                end if
            end if
        end if
        if (associated(group%lkg)) then
            if (all(group%lkg == out%NO_DATA)) then
                if (all(group_vs%lkg /= huge(group_vs%lkg))) then
                    group%lkg = group_vs%lkg
                else
                    group%lkg = 0.0
                end if
            end if
        end if
!-        if (associated(group%dzs)) then
!-            if (all(group%dzs == out%NO_DATA)) then
!-                if (all(group_vs%dzs /= huge(group_vs%dzs))) then
!-                    group%dzs = group_vs%dzs
!-                else
!-                    group%dzs = 0.0
!-                end if
!-            end if
!-        end if

        !> Diagnostic variables.
        if (associated(group%stge)) then
            if (all(group%stge == 0.0)) then
                group%stg0e = out%NO_DATA
                group%stge = out%NO_DATA
                group%dstge = out%NO_DATA
            else
                group%dstge = group%stge - group%stg0e
            end if
        end if
        if (associated(group%stgw)) then
            if (all(group%stgw == 0.0)) then
                where (group%lqwscan /= out%NO_DATA) group%stgw = group%stgw + group%lqwscan
                where (group%fzwscan /= out%NO_DATA) group%stgw = group%stgw + group%fzwscan
                where (group%sno /= out%NO_DATA) group%stgw = group%stgw + group%sno
                where (group%lqwssno /= out%NO_DATA) group%stgw = group%stgw + group%lqwssno
                where (group%lqwspnd /= out%NO_DATA) group%stgw = group%stgw + group%lqwspnd
                do j = 1, shd%lc%IGND
                    where (group%lqwssol(:, j) /= out%NO_DATA) group%stgw = group%stgw + group%lqwssol(:, j)
                    where (group%fzwssol(:, j) /= out%NO_DATA) group%stgw = group%stgw + group%fzwssol(:, j)
                end do
                where (group%stggw /= out%NO_DATA) group%stgw = group%stgw + group%stggw
!-                where (group%dzs /= out%NO_DATA) group%stgw = group%stgw + group%dzs
            end if
            if (all(group%stgw == 0.0)) then
                group%stg0w = out%NO_DATA
                group%stgw = out%NO_DATA
                group%dstgw = out%NO_DATA
            else
                group%dstgw = group%stgw - group%stg0w
            end if
        end if

        !> Routing variables.
        if (associated(group%rff)) then
            if (all(group%rff == out%NO_DATA)) then
                if (all(group_vs%rff /= huge(group_vs%rff))) then
                    group%rff = group_vs%rff
                else
                    group%rff = 0.0
                end if
            end if
        end if
        if (associated(group%rof)) then
            if (all(group%rof == out%NO_DATA)) then
                group%rof = 0.0
                where (group%ovrflw /= out%NO_DATA) group%rof = group%rof + group%ovrflw
                do j = 1, shd%lc%IGND
                    where (group%latflw(:, j) /= out%NO_DATA) group%rof = group%rof + group%latflw(:, j)
                end do
                where (group%drainsol /= out%NO_DATA) group%rof = group%rof + group%drainsol
            end if
        end if
        if (associated(group%qi)) then
            if (all(group%qi == out%NO_DATA)) then
                if (all(group_vs%qi /= huge(group_vs%qi))) then
                    group%qi = group_vs%qi
                else
                    group%qi = 0.0
                end if
            end if
        end if
        if (associated(group%qo)) then
            if (all(group%qo == out%NO_DATA)) then
                if (all(group_vs%qo /= huge(group_vs%qo))) then
                    group%qo = group_vs%qo
                else
                    group%qo = 0.0
                end if
            end if
        end if
        if (associated(group%stgch)) then
            if (all(group%stgch == out%NO_DATA)) then
                if (all(group_vs%stgch /= huge(group_vs%stgch))) then
                    group%stgch = group_vs%stgch
                else
                    group%stgch = 0.0
                end if
            end if
        end if
        if (associated(group%zlvl)) then
            if (all(group%zlvl == out%NO_DATA)) then
                if (all(group_vs%zlvl /= huge(group_vs%zlvl))) then
                    group%zlvl = group_vs%zlvl
                else
                    group%zlvl = 0.0
                end if
            end if
        end if

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    !>  Variables are updated if all elements of the group are equal to
    !>  the 'NO_DATA' value; if not the case, updates are assumed to
    !>  have occured in the model (e.g., by process modules), and those
    !>  values are preserved.
    !>  The model time-step output variables are allocated according
    !>  to the allocation status of the model states and variables;
    !>  checks are made against the model time-step output variable
    !>  to see if it has been associated.
    subroutine output_variables_update_ts(shd)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Update the variables.
        if (ro%RUNTILE) then
            call output_variables_group_update_ts(shd, out%ts%tile, vs%tile)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_update_ts(shd, out%ts%grid, vs%grid)
            call output_variables_group_update_ts(shd, out%ts%basin, vs%basin)
        end if

    end subroutine

    !> Description:
    !>  Update the 'dat' vector using the 'val' vector.
    !>  Reset 'dat' if the time-step of the current interval "its" is 1.
    !>  Calculate an average if the function "fn" is 'avg' using the
    !>  number of time-steps elapsed "its".
    !>  Override calculations where 'val' was originally equal to the
    !>  'NO_DATA' value.
    subroutine output_variables_field_update(dat, val, its, fn)

        !> Input variables.
        integer, intent(in) :: its
        real, dimension(:), intent(in) :: val
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Reset the variable if this is the first time-step in the series.
        if (its == 1) then
            where (val /= out%NO_DATA) dat = 0.0
        end if

        !> Apply the 'fn' function.
        !> The default case is to set 'dat' to 'val'.
        select case (fn)
            case ('sum')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = dat + val
            case ('avg')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = (dat*(its - 1) + val)/its
            case ('max')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = max(dat, val)
            case ('min')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = min(dat, val)
            case default
                dat = val
        end select

    end subroutine

    !> Description:
    !>  Calculate an average of 'dat' from 'val' for fields that do not
    !>  necessarily update at every time-step. 'ival' communicates if
    !>  the value should be updated. 'idat' is the counter of the number
    !>  of times the variable has been updated for the moving average.
    subroutine output_variables_field_icount_average(dat, val, idat, ival)

        !> Input variables.
        real, dimension(:), intent(in) :: val, ival

        !> Input/output variables.
        real, dimension(:) :: dat, idat

        !> Apply the function.
        where (val /= out%NO_DATA)
            where (idat == 0.0)
                dat = 0.0
            elsewhere
                where (ival /= 0.0) dat = (dat*(idat - 1.0) + val)/idat
            end where
        end where

    end subroutine

    !> Description:
    !>  Apply a transform to 'dat' using 'cfactorm' and 'cfactora'.
    !>  Override calculations where 'val' was originally equal to the
    !>  'NO_DATA' value.
    subroutine output_variables_field_transform(dat, cfactorm, cfactora)

        !> Input variables.
        real, dimension(:), intent(in), optional :: cfactorm, cfactora

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Apply transforms to the variable.
        if (present(cfactorm)) then
            where (dat /= out%NO_DATA) dat = dat*cfactorm
        end if
        if (present(cfactora)) then
            where (dat /= out%NO_DATA) dat = dat + cfactora
        end if

    end subroutine

    !> Description:
    !>  Update output variables of larger time intervals from the 'ts'
    !>  values.
    subroutine output_variables_group_update(shd, group, group_ts, its)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        integer, intent(in) :: its
        type(ShedGridParams), intent(in) :: shd
        type(output_fields), intent(in) :: group_ts

        !> Input/output variables.
        type(output_fields) group

        !> Local variables.
        integer j

        !> Meteorology/climatology variables.
        if (associated(group%ifsin)) then
            call output_variables_field_update(group%ifsin, group_ts%ifsin, its, 'sum')
        end if
        if (associated(group%fsin)) then
            call output_variables_field_update(group%fsin, group_ts%fsin, its, 'avg')
        end if
        if (associated(group%fsvs)) then
            call output_variables_field_update(group%fsvs, group_ts%fsvs, its, 'avg')
        end if
        if (associated(group%fsir)) then
            call output_variables_field_update(group%fsir, group_ts%fsir, its, 'avg')
        end if
        if (associated(group%fsdr)) then
            call output_variables_field_update(group%fsdr, group_ts%fsdr, its, 'avg')
        end if
        if (associated(group%fsdff)) then
            call output_variables_field_update(group%fsdff, group_ts%fsdff, its, 'avg')
        end if
        if (associated(group%fsout)) then
            call output_variables_field_update(group%fsout, group_ts%fsout, its, 'avg')
        end if
        if (associated(group%flin)) then
            call output_variables_field_update(group%flin, group_ts%flin, its, 'avg')
        end if
        if (associated(group%flout)) then
            call output_variables_field_update(group%flout, group_ts%flout, its, 'avg')
        end if
        if (associated(group%ta)) then
            call output_variables_field_update(group%ta, group_ts%ta, its, 'avg')
        end if
        if (associated(group%qa)) then
            call output_variables_field_update(group%qa, group_ts%qa, its, 'avg')
        end if
        if (associated(group%pres)) then
            call output_variables_field_update(group%pres, group_ts%pres, its, 'avg')
        end if
        if (associated(group%uu)) then
            call output_variables_field_update(group%uu, group_ts%uu, its, 'avg')
        end if
        if (associated(group%vv)) then
            call output_variables_field_update(group%vv, group_ts%vv, its, 'avg')
        end if
        if (associated(group%uv)) then
            call output_variables_field_update(group%uv, group_ts%uv, its, 'avg')
        end if
        if (associated(group%wdir)) then
            call output_variables_field_update(group%wdir, group_ts%wdir, its, 'avg')
        end if
        if (associated(group%prern)) then
            call output_variables_field_update(group%prern, group_ts%prern, its, 'avg')
        end if
        if (associated(group%presno)) then
            call output_variables_field_update(group%presno, group_ts%presno, its, 'avg')
        end if
        if (associated(group%pre)) then
            call output_variables_field_update(group%pre, group_ts%pre, its, 'avg')
        end if
        if (associated(group%precrn)) then
            call output_variables_field_update(group%precrn, group_ts%precrn, its, 'sum')
        end if
        if (associated(group%precsno)) then
            call output_variables_field_update(group%precsno, group_ts%precsno, its, 'sum')
        end if
        if (associated(group%prec)) then
            call output_variables_field_update(group%prec, group_ts%prec, its, 'sum')
        end if

        !> Canopy variables.
        if (associated(group%ican)) then
            call output_variables_field_update(group%ican, group_ts%ican, its, 'sum')
        end if
        if (associated(group%lqwscan)) then
            call output_variables_field_update(group%lqwscan, group_ts%lqwscan, its, 'avg')
        end if
        if (associated(group%fzwscan)) then
            call output_variables_field_update(group%fzwscan, group_ts%fzwscan, its, 'avg')
        end if
        if (associated(group%cmas)) then
            call output_variables_field_icount_average(group%cmas, group_ts%cmas, group%ican, group_ts%ican)
        end if
        if (associated(group%tcan)) then
            call output_variables_field_icount_average(group%tcan, group_ts%tcan, group%ican, group_ts%ican)
        end if
        if (associated(group%gro)) then
            call output_variables_field_update(group%gro, group_ts%gro, its, 'avg')
        end if

        !> Snow variables.
        if (associated(group%isno)) then
            call output_variables_field_update(group%isno, group_ts%isno, its, 'sum')
        end if
        if (associated(group%fsno)) then
            call output_variables_field_update(group%fsno, group_ts%fsno, its, 'avg')
        end if
        if (associated(group%sno)) then
            call output_variables_field_update(group%sno, group_ts%sno, its, 'avg')
        end if
        if (associated(group%rhosno)) then
            call output_variables_field_update(group%rhosno, group_ts%rhosno, its, 'avg')
        end if
        if (associated(group%zsno)) then
            call output_variables_field_update(group%zsno, group_ts%zsno, its, 'avg')
        end if
        if (associated(group%lqwssno)) then
            call output_variables_field_update(group%lqwssno, group_ts%lqwssno, its, 'avg')
        end if
        if (associated(group%tsno)) then
            call output_variables_field_icount_average(group%tsno, group_ts%tsno, group%isno, group_ts%isno)
        end if
        if (associated(group%albsno)) then
            call output_variables_field_update(group%albsno, group_ts%albsno, its, 'avg')
        end if
        if (associated(group%drainsno)) then
            call output_variables_field_update(group%drainsno, group_ts%drainsno, its, 'sum')
        end if

        !> Surface variables.
        if (associated(group%albt)) then
            call output_variables_field_icount_average(group%albt, group_ts%albt, group%ifsin, group_ts%ifsin)
        end if
        if (associated(group%alvs)) then
            call output_variables_field_icount_average(group%alvs, group_ts%alvs, group%ifsin, group_ts%ifsin)
        end if
        if (associated(group%alir)) then
            call output_variables_field_icount_average(group%alir, group_ts%alir, group%ifsin, group_ts%ifsin)
        end if
        if (associated(group%gte)) then
            call output_variables_field_update(group%gte, group_ts%gte, its, 'avg')
        end if
        if (associated(group%ipnd)) then
            call output_variables_field_update(group%ipnd, group_ts%ipnd, its, 'sum')
        end if
        if (associated(group%zpnd)) then
            call output_variables_field_update(group%zpnd, group_ts%zpnd, its, 'avg')
        end if
        if (associated(group%lqwspnd)) then
            call output_variables_field_update(group%lqwspnd, group_ts%lqwspnd, its, 'avg')
        end if
        if (associated(group%tpnd)) then
            call output_variables_field_icount_average(group%tpnd, group_ts%tpnd, group%ipnd, group_ts%ipnd)
        end if
        if (associated(group%pndcaf)) then
            call output_variables_field_update(group%pndcaf, group_ts%pndcaf, its, 'avg')
        end if
        if (associated(group%potevp)) then
            call output_variables_field_update(group%potevp, group_ts%potevp, its, 'sum')
        end if
        if (associated(group%et)) then
            call output_variables_field_update(group%et, group_ts%et, its, 'sum')
        end if
        if (associated(group%evpb)) then
            call output_variables_field_update(group%evpb, group_ts%evpb, its, 'avg')
        end if
        if (associated(group%arrd)) then
            call output_variables_field_update(group%arrd, group_ts%arrd, its, 'avg')
        end if
        if (associated(group%ovrflw)) then
            call output_variables_field_update(group%ovrflw, group_ts%ovrflw, its, 'sum')
        end if
        if (associated(group%qevp)) then
            call output_variables_field_update(group%qevp, group_ts%qevp, its, 'avg')
        end if
        if (associated(group%qsens)) then
            call output_variables_field_update(group%qsens, group_ts%qsens, its, 'avg')
        end if
        if (associated(group%gzero)) then
            call output_variables_field_update(group%gzero, group_ts%gzero, its, 'avg')
        end if
        if (associated(group%tsurf)) then
            call output_variables_field_update(group%tsurf, group_ts%tsurf, its, 'avg')
        end if

        !> Ice/glacier variables.
        if (associated(group%iice)) then
            call output_variables_field_update(group%iice, group_ts%iice, its, 'sum')
        end if
        if (associated(group%lqwsice)) then
            call output_variables_field_update(group%lqwsice, group_ts%lqwsice, its, 'avg')
        end if
        if (associated(group%tice)) then
            call output_variables_field_icount_average(group%tice, group_ts%tice, group%iice, group_ts%iice)
        end if

        !> Subsurface/soil variables.
        do j = 1, shd%lc%IGND
            if (associated(group%thlqsol)) then
                call output_variables_field_update(group%thlqsol(:, j), group_ts%thlqsol(:, j), its, 'avg')
            end if
            if (associated(group%thicsol)) then
                call output_variables_field_update(group%thicsol(:, j), group_ts%thicsol(:, j), its, 'avg')
            end if
            if (associated(group%lqwssol)) then
                call output_variables_field_update(group%lqwssol(:, j), group_ts%lqwssol(:, j), its, 'avg')
            end if
            if (associated(group%fzwssol)) then
                call output_variables_field_update(group%fzwssol(:, j), group_ts%fzwssol(:, j), its, 'avg')
            end if
            if (associated(group%alwssol)) then
                call output_variables_field_update(group%alwssol(:, j), group_ts%alwssol(:, j), its, 'avg')
            end if
            if (associated(group%tsol)) then
                call output_variables_field_update(group%tsol(:, j), group_ts%tsol(:, j), its, 'avg')
            end if
            if (associated(group%gflx)) then
                call output_variables_field_update(group%gflx(:, j), group_ts%gflx(:, j), its, 'avg')
            end if
            if (associated(group%latflw)) then
                call output_variables_field_update(group%latflw(:, j), group_ts%latflw(:, j), its, 'sum')
            end if
        end do
        if (associated(group%zsolsat)) then
            call output_variables_field_update(group%zsolsat, group_ts%zsolsat, its, 'avg')
        end if
        if (associated(group%drainsol)) then
            call output_variables_field_update(group%drainsol, group_ts%drainsol, its, 'sum')
        end if

        !> Groundwater/lower zone storage variables.
        if (associated(group%rchg)) then
            call output_variables_field_update(group%rchg, group_ts%rchg, its, 'sum')
        end if
        if (associated(group%stggw)) then
            call output_variables_field_update(group%stggw, group_ts%stggw, its, 'avg')
        end if
        if (associated(group%lkg)) then
            call output_variables_field_update(group%lkg, group_ts%lkg, its, 'sum')
        end if
!-        if (associated(group%dzs)) then
!-            call output_variables_field_update(group%dzs, group_ts%dzs, its, 'avg')
!-        end if

        !> Diagnostic variables.
        if (associated(group%stge)) then
            if (its == 1) then
                call output_variables_field_update(group%stg0e, group%stge, its, 'val')
            end if
            call output_variables_field_update(group%stge, group_ts%stge, its, 'avg')
            where (group%stge /= out%NO_DATA) group%dstge = group%stge - group%stg0e
        end if
        if (associated(group%stgw)) then
            if (its == 1) then
                call output_variables_field_update(group%stg0w, group%stgw, its, 'val')
            end if
            call output_variables_field_update(group%stgw, group_ts%stgw, its, 'avg')
            where (group%stgw /= out%NO_DATA) group%dstgw = group%stgw - group%stg0w
        end if

        !> Routing variables.
        if (associated(group%rff)) then
            call output_variables_field_update(group%rff, group_ts%rff, its, 'sum')
        end if
        if (associated(group%rof)) then
            call output_variables_field_update(group%rof, group_ts%rof, its, 'sum')
        end if
        if (associated(group%qi)) then
            call output_variables_field_update(group%qi, group_ts%qi, its, 'avg')
        end if
        if (associated(group%qo)) then
            call output_variables_field_update(group%qo, group_ts%qo, its, 'avg')
        end if
        if (associated(group%stgch)) then
            call output_variables_field_update(group%stgch, group_ts%stgch, its, 'avg')
        end if
        if (associated(group%zlvl)) then
            call output_variables_field_update(group%zlvl, group_ts%zlvl, its, 'avg')
        end if

    end subroutine

    !> Description:
    !>  Update output variables of larger time intervals from the 'ts'
    !>  values.
    subroutine output_variables_series_update(shd, series, its)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        integer, intent(in) :: its
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Update groups.
        if (ro%RUNTILE) then
            call output_variables_group_update(shd, series%tile, out%ts%tile, its)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_update(shd, series%grid, out%ts%grid, its)
            call output_variables_group_update(shd, series%basin, out%ts%basin, its)
        end if

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    subroutine output_variables_update(shd)

        !> 'shd_variables' required for 'shd'.
        !> 'model_dates' required for 'ic' (counter and time-stepping).
        use shd_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Update 'ts' values.
        call output_variables_update_ts(shd)

        !> Totals (e.g., accumulated).
        call output_variables_series_update(shd, out%tot, ic%ts_count)

        !> Yearly.
        call output_variables_series_update(shd, out%y, ic%ts_yearly)

        !> Monthly.
        call output_variables_series_update(shd, out%m, ic%ts_monthly)

        !> Daily.
        call output_variables_series_update(shd, out%d, ic%ts_daily)

        !> Hourly.
        call output_variables_series_update(shd, out%h, ic%ts_hourly)

    end subroutine

end module
