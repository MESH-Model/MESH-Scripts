!> Description:
!>  Subroutine to read parameters from file, in delimited format.
!>  Parameter values are saved directly to the shared parameter object
!>  at the GRU and NRVR levels, accessible by 'sa_mesh_variables'.
!>
!> Input variables:
!*  shd: Basin 'shed' object (properties).
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_parameters.r2c').
!>
!> Output variables:
!*  ierr: Return status.
subroutine read_parameters_csv(shd, iun, fname, ierr)

    !> strings: For 'readline', 'compact', 'parse', 'uppercase' and 'lowercase' functions.
    !> sa_mesh_common: For common MESH variables and routines.
    !> parse_utilities: For 'assign_line_args' function.
    use strings
    use sa_mesh_common
    use parse_utilities

    !> Process modules: Required for process variables, parameters.
    use baseflow_module
    use rte_module
    use runsvs_mesh
    use FLAGS

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer nargs, n, p, b, k, j, i, ignd, icondition, istat, z
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH), dimension(:), allocatable :: args
    character(len = DEFAULT_FIELD_LENGTH) field1, field2, field3

    !> Initialize the return status.
    ierr = 0

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open the file. Check if the file exists.')
        return
    end if

    !> Determine the number of values to read for soil-related variables.
    if (NRSOILAYEREADFLAG > 3) then
        ignd = min(NRSOILAYEREADFLAG, shd%lc%IGND)
    else if (NRSOILAYEREADFLAG == 1) then
        ignd = shd%lc%IGND
    else
        ignd = 3
    end if

    !> Read and parse each line.
    n = 0
    z = 0
    do while (z == 0)

        !> Compact and reduce the line to any instance of '#' or '!'.
        call readline(iun, line, z)
        if (z /= 0) exit
        if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
        if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
        call compact(line)

        !> Replace commas with spaces and parse the fields in the line.
        do i = 1, len_trim(line)
            if (line(i:i) == ',') line(i:i) = ' '
        end do
        call parse_line_values(line, args, istat = istat)
        nargs = size(args)

        !> Cycle if no arguments exist.
        if (nargs < 1) cycle

        !> Reset variables used for message passing.
        icondition = 0
        field1 = ''
        field2 = ''
        field3 = ''
        istat = radix(istat)**pstat%NORMAL_STATUS
        p = 0
        b = 0

        !> Shift 'nargs' by 1 to account for the leading field name.
        nargs = nargs - 1

        !> Assign and distribute the field.
        if (DIAGNOSEMODE) call print_message('Reading parameter: ' // trim(adjustl(args(1))) // '.')
        select case (uppercase(args(1)))

            !> SVS (unique variables).
            case (VN_SVS_DEGLAT)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%deglat, p, args(2:), istat)
                end if
            case (VN_SVS_DEGLNG)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%deglng, p, args(2:), istat)
                end if
            case (VN_SVS_OBSERVED_FORCING)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (lowercase(args(2)) == 'height') then
                        args(2) = '.true.'
                    end if
                    p = 1
                    call assign_line_args(svs_mesh%vs%observed_forcing, args(2), istat)
                end if
            case (VN_SVS_ZUSL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%zusl, p, args(2:), istat)
                end if
            case (VN_SVS_ZTSL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%ztsl, p, args(2:), istat)
                end if
            case (VN_SVS_SIGMA_U)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(svs_mesh%vs%sigma_u, args(2), istat)
                end if
            case (VN_SVS_SIGMA_T)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(svs_mesh%vs%sigma_t, args(2), istat)
                end if
            case (VN_SVS_SLOP)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%slop, p, args(2:), istat)
                end if
            case (VN_SVS_DRAINDENS)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%draindens, p, args(2:), istat)
                end if
            case (VN_SVS_SOILTEXT)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(svs_mesh%vs%soiltext, args(2), istat)
                end if
            case (VN_SVS_KHYD)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(svs_mesh%vs%khyd, args(2), istat)
                end if
            case (VN_SVS_SAND)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_SAND)
                            field2 = adjustl(VN_SVS_SAND_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_SAND)
                            field2 = adjustl(VN_SVS_SAND_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%sand, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                    b = ignd
                end if
            case (VN_SVS_SAND_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%sand, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                    b = ignd
                end if
            case (VN_SVS_CLAY)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_CLAY)
                            field2 = adjustl(VN_SVS_CLAY_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_CLAY)
                            field2 = adjustl(VN_SVS_CLAY_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%clay, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                    b = ignd
                end if
            case (VN_SVS_CLAY_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%clay, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                    b = ignd
                end if
            case (VN_SVS_WSOIL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_WSOIL)
                            field2 = adjustl(VN_SVS_WSOIL_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_WSOIL)
                            field2 = adjustl(VN_SVS_WSOIL_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%wsoil, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                    b = ignd
                end if
            case (VN_SVS_WSOIL_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%wsoil, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                    b = ignd
                end if
            case (VN_SVS_ISOIL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_ISOIL)
                            field2 = adjustl(VN_SVS_ISOIL_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_ISOIL)
                            field2 = adjustl(VN_SVS_ISOIL_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%isoil, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                    b = ignd
                end if
            case (VN_SVS_ISOIL_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = shd%lc%IGND
                    call assign_line_args(svs_mesh%vs%isoil, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                    b = ignd
                end if
            case (VN_SVS_TGROUND)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_TGROUND)
                            field2 = adjustl(VN_SVS_TGROUND_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_TGROUND)
                            field2 = adjustl(VN_SVS_TGROUND_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = svs_mesh%vs%kthermal
                    call assign_line_args(svs_mesh%vs%tground, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                end if
            case (VN_SVS_TGROUND_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = svs_mesh%vs%kthermal
                    call assign_line_args(svs_mesh%vs%tground, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                end if
            case (VN_SVS_VF)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_VF)
                            field2 = adjustl(VN_SVS_VF_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_VF)
                            field2 = adjustl(VN_SVS_VF_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = svs_mesh%c%NLANDCLASS
                    call assign_line_args(svs_mesh%vs%vf, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                end if
            case (VN_SVS_VF_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = svs_mesh%c%NLANDCLASS
                    call assign_line_args(svs_mesh%vs%vf, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                end if
            case (VN_SVS_Z0V, 'Z0')
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_Z0V)
                            field2 = adjustl(VN_SVS_Z0V_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_Z0V)
                            field2 = adjustl(VN_SVS_Z0V_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = svs_mesh%c%NLANDCLASS
                    call assign_line_args(svs_mesh%vs%z0v, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                end if
            case (VN_SVS_Z0V_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = svs_mesh%c%NLANDCLASS
                    call assign_line_args(svs_mesh%vs%z0v, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                end if
            case (VN_SVS_LNZ0, 'ZP')
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%lnz0, p, args(2:), istat)
                end if
            case (VN_SVS_TVEGE)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_TVEGE)
                            field2 = adjustl(VN_SVS_TVEGE_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_TVEGE)
                            field2 = adjustl(VN_SVS_TVEGE_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = 2
                    call assign_line_args(svs_mesh%vs%tvege, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                end if
            case (VN_SVS_TVEGE_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = 2
                    call assign_line_args(svs_mesh%vs%tvege, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                end if
            case (VN_SVS_WVEG)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%wveg, p, args(2:), istat)
                end if
            case (VN_SVS_TSNOW)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_TSNOW)
                            field2 = adjustl(VN_SVS_TSNOW_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_TSNOW)
                            field2 = adjustl(VN_SVS_TSNOW_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = 2
                    call assign_line_args(svs_mesh%vs%tsnow, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                end if
            case (VN_SVS_TSNOW_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = 2
                    call assign_line_args(svs_mesh%vs%tsnow, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                end if
            case (VN_SVS_SNODPL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%snodpl, p, args(2:), istat)
                end if
            case (VN_SVS_SNODEN)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%snoden, p, args(2:), istat)
                end if
            case (VN_SVS_SNOAL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%snoal, p, args(2:), istat)
                end if
            case (VN_SVS_WSNOW)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%wsnow, p, args(2:), istat)
                end if
            case (VN_SVS_TSNOWVEG)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        if (shd%lc%NML /= 1) then
                            field1 = adjustl(VN_SVS_TSNOWVEG)
                            field2 = adjustl(VN_SVS_TSNOWVEG_N)
                            field3 = 'points'
                            icondition = 1
                        end if
                        p = shd%lc%NML
                    else
                        if (shd%lc%NTYPE /= 1) then
                            field1 = adjustl(VN_SVS_TSNOWVEG)
                            field2 = adjustl(VN_SVS_TSNOWVEG_N)
                            field3 = 'GRUs'
                            icondition = 1
                        end if
                        p = shd%lc%NTYPE
                    end if
                    b = 2
                    call assign_line_args(svs_mesh%vs%tsnowveg, p, b, args(2:), pkey%MAP_ASSIGN_ORDER2, istat)
                end if
            case (VN_SVS_TSNOWVEG_N)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    call assign_line_args(k, args(2), istat)
                    istat = radix(istat)**pstat%NORMAL_STATUS
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    nargs = nargs - 1
                    b = 2
                    call assign_line_args(svs_mesh%vs%tsnowveg, p, b, args(3:), pkey%MAP_ASSIGN_ORDER2, istat, k)
                end if
            case (VN_SVS_SNVDP)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%snvdp, p, args(2:), istat)
                end if
            case (VN_SVS_SNVDEN)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%snvden, p, args(2:), istat)
                end if
            case (VN_SVS_SNVAL)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%snval, p, args(2:), istat)
                end if
            case (VN_SVS_WSNV)
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%lc%NTYPE
                    end if
                    call assign_line_args(svs_mesh%vs%wsnv, p, args(2:), istat)
                end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            case ('PWR')
                if (bflm%BASEFLOWFLAG /= 2) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(bflm%pm_iak%pwr, p, args(2:), istat)
                end if
            case ('FLZ')
                if (bflm%BASEFLOWFLAG /= 2) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(bflm%pm_iak%flz, p, args(2:), istat)
                end if

            !> RPN RTE (Watflood, 2007).
            case ('R2N')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%r2n, p, args(2:), istat)
                end if
            case ('R1N')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%r1n, p, args(2:), istat)
                end if
            case ('MNDR')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%mndr, p, args(2:), istat)
                end if
            case ('AA2')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%aa2, p, args(2:), istat)
                end if
            case ('AA3')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%aa3, p, args(2:), istat)
                end if
            case ('AA4')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%aa4, p, args(2:), istat)
                end if
            case ('WIDEP')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    if (SHDFILEFMT == 2) then
                        p = shd%lc%NML
                    else
                        p = shd%NRVR
                    end if
                    call assign_line_args(rtepm_iak%widep, p, args(2:), istat)
                end if
            case ('DTMINUSR')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(rteflg%dtminusr, args(2), istat)
                end if
            case ('MINDTMIN')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(rteflg%mindtmin, args(2), istat)
                end if
            case ('MAXINDEX')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(rteflg%maxindex, args(2), istat)
                end if
            case ('DTMINFRAC')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(rteflg%dtminfrac, args(2), istat)
                end if
            case ('CONVTHRESHUSR')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = istat + radix(istat)**pstat%INACTIVE
                else
                    p = 1
                    call assign_line_args(rteflg%convthreshusr, args(2), istat)
                end if

            !> Unrecognized.
            case default
                istat = istat + radix(istat)**pstat%UNRECOGNIZED
        end select

        !> Messages (non-fatal, non-warning).
        if (icondition == 1 .and. len_trim(field1) /= 0 .and. len_trim(field2) /= 0 .and. len_trim(field3) /= 0) then
                call print_warning( &
                    "Multiple " // trim(field3) // " exist but the uniform " // trim(field1) // " parameter is used. " // &
                    "The same values will be applied to all " // trim(field3) // " in the domain. " // &
                    "To assign values to specific " // trim(field3) // ", use the " // trim(field2) // " parameter instead.")
        end if

        !> Status flags (fatal and/or warning).
        if (btest(istat, pstat%INACTIVE) .and. DIAGNOSEMODE) then
            call print_remark("'" // trim(adjustl(args(1))) // "' is present but inactive.")
        else if (btest(istat, pstat%UNRECOGNIZED)) then
            call print_warning("'" // trim(adjustl(args(1))) // "' is not recognized.")
        else
            if (p /= 0) then
                if ((b == 0 .and. nargs > p) .or. (b > 0 .and. nargs > b)) then
                    write(field1, FMT_GEN) nargs
                    if (b > 0) then
                        write(field2, FMT_GEN) b
                    else
                        write(field2, FMT_GEN) p
                    end if
                    call print_warning( &
                        "'" // trim(adjustl(args(1))) // "' contains " // trim(adjustl(field1)) // " fields but only " // &
                        trim(adjustl(field2)) // " are being used.")
                else if ((b == 0 .and. nargs /= p) .or. (b > 0 .and. nargs /= b)) then
                    write(field1, FMT_GEN) nargs
                    if (b > 0) then
                        write(field2, FMT_GEN) b
                    else
                        write(field2, FMT_GEN) p
                    end if
                    call print_warning( &
                        "'" // trim(adjustl(args(1))) // "' only contains " // trim(adjustl(field1)) // " fields when " // &
                        trim(adjustl(field2)) // " are expected.")
                end if
            end if
            if (btest(istat, pstat%OVERWRITING_FIELD) .and. DIAGNOSEMODE) then
                call print_remark("Overwriting or updating the existing '" // trim(adjustl(args(1))) // "' value.")
            end if
            if (btest(istat, pstat%MISMATCHED_PRECISION)) then
                call print_warning( &
                    "The precision or length of the '" // trim(adjustl(args(1))) // "' values exceeds the precision or length " // &
                    "of the assigned field. Truncation may have occurred.")
            end if
            if (btest(istat, pstat%CONVERSION_ERROR)) then
                call print_warning("An error occurred assigning '" // trim(adjustl(args(1))) // "' values.")
            end if
            if (btest(istat, pstat%ALLOCATION_ERROR)) then
                call print_message("ERROR: An error occurred allocating '" // trim(adjustl(args(1))) // "'.")
                ierr = 1
            end if
            if (btest(istat, pstat%MISMATCHED_BOUNDS)) then
                call print_message("ERROR: Mismatched bounds encountered while assigning '" // trim(adjustl(args(1))) // "'.")
                ierr = 1
            end if
            n = n + 1
        end if
    end do

    !> Print number of active parameters.
    write(line, FMT_GEN) n
    call print_message('Active parameters in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
    close(iun)

    return

end subroutine
