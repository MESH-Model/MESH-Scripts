!> Description:
!>  Subroutine to read parameters from a single-frame 'r2c' format file.
!>  Values are parsed by order of RANK and stored in variables that must
!>  allocated 1:NA.
!>
!> Input:
!*  shd: Basin 'shed' object (properties).
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
subroutine read_parameters_r2c(shd, iun, fname, ierr)

    !> strings: For 'lowercase' function.
    !> sa_mesh_common: For common MESH variables and routines.
    !> ensim_io: For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
    use ensim_io

    !> Process modules: Required for process variables, parameters.
    use RUNCLASS36_variables
    use runsvs_mesh
    use irrigation_module
    use baseflow_module
    use rte_module
    use PBSM_module
    use mountain_module

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    type(ensim_attr), dimension(:), allocatable :: vattr
    integer nkeyword, nattr, ilvl, n, l, k, j, i, ii, z
    character(len = MAX_WORD_LENGTH) tfield, tlvl
    real, dimension(:), allocatable :: ffield
    character(len = DEFAULT_LINE_LENGTH) line

    !> Initialize the return status.
    ierr = 0

    !> Open the file and read the header.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    call open_ensim_input(iun, fname, ierr)
    if (ierr /= 0) return
    call parse_header_ensim(iun, vkeyword, nkeyword, ierr)
    if (ierr /= 0) return

    !> Check the spatial definition in the header.
    call validate_header_spatial( &
        vkeyword, nkeyword, &
        shd%CoordSys%Proj, shd%xCount, shd%xDelta, shd%xOrigin, shd%yCount, shd%yDelta, shd%yOrigin, &
        ierr)
    if (ierr /= 0) return

    !> Get the list of attributes.
    call parse_header_attribute_ensim(iun, vkeyword, nkeyword, vattr, nattr, ierr)
    if (ierr /= 0) then
        call print_error('Error reading attributes from the header in the file.')
        return
    end if
    if (nattr == 0) call print_warning('No attributes were found in the file.')

    !> Advance past the end of the header.
    call advance_past_header(iun, fname, ierr)
    if (ierr /= 0) then
        call print_error('Encountered premature end of file.')
        return
    end if

    !> Read and parse the attribute data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)
    if (ierr /= 0) then
        call print_error('Error reading attribute values in the file.')
        return
    end if

    !> Distribute the data to the appropriate variable.
    allocate(ffield(shd%NA))
    n = 0
    do l = 1, nattr

        !> Extract variable name and level.
        if (DIAGNOSEMODE) call print_message("Reading '" // trim(vattr(l)%attr) // "'.")
        z = 0
        tfield = lowercase(vattr(l)%attr)
        i = index(trim(tfield), ' ')
        ii = index(trim(tfield), ' ', back = .true.)
        ilvl = 0
        if (i > 0) then
            if (ii /= i) then
                ii = index(tfield((i + 1):), ' ')
                tlvl = tfield((i + 1):(ii + i))
            else
                tlvl = tfield((i + 1):)
            end if
            tfield = tfield(1:i)
            call value(tlvl, ilvl, z)
            if (z /= 0) ilvl = 0
        end if

        !> Assign the data to a vector.
        call r2c_to_rank(iun, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, ierr)
        if (ierr /= 0) then
            call print_error("Unable to read the '" // trim(vattr(l)%attr) // "' attribute.")
            return
        end if

        !> Determine the variable.
        z = 0
        select case (adjustl(tfield))

            !> RUNCLASS36 and RUNSVS113.
            case ('fcan')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    select case (adjustl(tlvl))
                        case ('nl')
                            pm%grid%fcan(1:shd%NA, 1) = ffield
                        case ('bl')
                            pm%grid%fcan(1:shd%NA, 2) = ffield
                        case ('cr')
                            pm%grid%fcan(1:shd%NA, 3) = ffield
                        case ('gr')
                            pm%grid%fcan(1:shd%NA, 4) = ffield
                        case ('ur')
                            pm%grid%fcan(1:shd%NA, 5) = ffield
                        case default
                            z = 1
                    end select
                end if
            case ('lnz0')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    select case (adjustl(tlvl))
                        case ('nl')
                            pm%grid%lnz0(1:shd%NA, 1) = ffield
                        case ('bl')
                            pm%grid%lnz0(1:shd%NA, 2) = ffield
                        case ('cr')
                            pm%grid%lnz0(1:shd%NA, 3) = ffield
                        case ('gr')
                            pm%grid%lnz0(1:shd%NA, 4) = ffield
                        case ('ur')
                            pm%grid%lnz0(1:shd%NA, 5) = ffield
                        case default
                            if (svs_mesh%PROCESS_ACTIVE) then
                                if (.not. allocated(svs_mesh%vs%lnz0)) allocate(svs_mesh%vs%lnz0(shd%lc%NML))
                                do k = 1, shd%lc%NML
                                    svs_mesh%vs%lnz0(k) = ffield(shd%lc%ILMOS(k))
                                end do
                            else
                                call print_remark("'LNZ0' with no canopy identifier is being applied to all canopies.")
                                do j = 1, size(pm%grid%lnz0, 2)
                                    pm%grid%lnz0(1:shd%NA, j) = ffield
                                end do
                            end if
                    end select
                end if
            case ('vf')
                if (svs_mesh%PROCESS_ACTIVE) then
                    if (.not. allocated(svs_mesh%vs%vf)) allocate(svs_mesh%vs%vf(shd%lc%NML, 26))
                    if (ilvl > 1199 .or. ilvl < 1174) then
                        z = 1
                    else
                        do k = 1, shd%lc%NML
                            svs_mesh%vs%vf(k, 1200 - ilvl) = ffield(shd%lc%ILMOS(k))
                        end do
                    end if
                end if
            case ('sdep')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    pm%grid%sdep(1:shd%NA) = ffield
                end if
            case ('xslp')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    pm%grid%xslp(1:shd%NA) = ffield
                end if
            case ('dd', 'dden')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    pm%grid%dd(1:shd%NA) = ffield

                    !> Unit conversion if units are km km-2.
                    if (index(lowercase(vattr(l)%units), 'km') > 0) then
                        call print_remark("'" // trim(vattr(l)%attr) // "' converted from 'km km -2' to 'm m-2'.")
                        pm%grid%dd = pm%grid%dd/1000.0
                    end if
                end if
            case ('sand')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ilvl == 0) then
                        do ilvl = 1, shd%lc%IGND
                            pm%grid%sand(1:shd%NA, ilvl) = ffield
                        end do
                    else if (ilvl <= shd%lc%IGND) then
                        pm%grid%sand(1:shd%NA, ilvl) = ffield
                    else
                        z = 1
                    end if
                end if
            case ('clay')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ilvl == 0) then
                        do ilvl = 1, shd%lc%IGND
                            pm%grid%clay(1:shd%NA, ilvl) = ffield
                        end do
                    else if (ilvl <= shd%lc%IGND) then
                        pm%grid%clay(1:shd%NA, ilvl) = ffield
                    else
                        z = 1
                    end if
                end if
            case ('orgm')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
                    if (ilvl == 0) then
                        do ilvl = 1, shd%lc%IGND
                            pm%grid%orgm(1:shd%NA, ilvl) = ffield
                        end do
                    else if (ilvl <= shd%lc%IGND) then
                        pm%grid%orgm(1:shd%NA, ilvl) = ffield
                    else
                        z = 1
                    end if
                end if

            !> RUNCLASS36.
            case ('iwf')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                    pm%grid%iwf(1:shd%NA) = int(ffield)
                end if

            !> Irrigation module.
            case ('irflg')
                irrm%pm_grid%irflg(1:shd%NA) = int(ffield)
            case ('irt1')
                irrm%pm_grid%t1(1:shd%NA) = int(ffield)
            case ('irt2')
                irrm%pm_grid%t2(1:shd%NA) = int(ffield)
            case ('irijday1')
                irrm%pm_grid%ijday1(1:shd%NA) = int(ffield)
            case ('irijday2')
                irrm%pm_grid%ijday2(1:shd%NA) = int(ffield)
            case ('irignd')
                irrm%pm_grid%ignd(1:shd%NA) = int(ffield)
            case ('irthlmin')
                irrm%pm_grid%thlmin(1:shd%NA) = ffield

            !> Abstraction point location.
            case ('iabsp')
                pm%grid%iabsp(1:shd%NA) = int(ffield)

            !> BASEFLOWFLAG == 2 (lower zone storage).
            case ('pwr')
                if (bflm%BASEFLOWFLAG == 2) bflm%pm_grid%pwr(1:shd%NA) = ffield
            case ('flz')
                if (bflm%BASEFLOWFLAG == 2) bflm%pm_grid%flz(1:shd%NA) = ffield

            !> RPN RTE (Watflood, 2007).
            case ('r1n')
                if (rteflg%PROCESS_ACTIVE) rtepm%r1n(1:shd%NA) = ffield
            case ('r2n')
                if (rteflg%PROCESS_ACTIVE) rtepm%r2n(1:shd%NA) = ffield
            case ('mndr')
                if (rteflg%PROCESS_ACTIVE) rtepm%mndr(1:shd%NA) = ffield
            case ('widep')
                if (rteflg%PROCESS_ACTIVE) rtepm%widep(1:shd%NA) = ffield
            case ('aa2')
                if (rteflg%PROCESS_ACTIVE) rtepm%aa2(1:shd%NA) = ffield
            case ('aa3')
                if (rteflg%PROCESS_ACTIVE) rtepm%aa3(1:shd%NA) = ffield
            case ('aa4')
                if (rteflg%PROCESS_ACTIVE) rtepm%aa4(1:shd%NA) = ffield
!                case ('theta')
!                case ('kcond')

            !> PBSM (blowing snow).
            case ('fetch')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%fetch(1:shd%NA) = ffield
            case ('ht')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%Ht(1:shd%NA) = ffield
            case ('n_s')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%N_S(1:shd%NA) = ffield
            case ('a_s')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%A_S(1:shd%NA) = ffield
            case ('distrib')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%Distrib(1:shd%NA) = ffield

            !> MOUNTAINMESHFLAG.
            case ('elevation')
                if (.not. allocated(mountain_mesh%pm%elev)) allocate(mountain_mesh%pm%elev(shd%NA, shd%lc%NTYPE))
                if (ilvl == 0) then
                    do ilvl = 1, shd%lc%NTYPE
                        mountain_mesh%pm%elev(:, ilvl) = ffield
                    end do
                else if (ilvl <= shd%lc%NTYPE) then
                    mountain_mesh%pm%elev(:, ilvl) = ffield
                else
                    z = 1
                end if
            case ('slope')
                if (.not. allocated(mountain_mesh%pm%slope)) allocate(mountain_mesh%pm%slope(shd%NA, shd%lc%NTYPE))
                if (ilvl == 0) then
                    do ilvl = 1, shd%lc%NTYPE
                        mountain_mesh%pm%slope(:, ilvl) = ffield
                    end do
                else if (ilvl <= shd%lc%NTYPE) then
                    mountain_mesh%pm%slope(:, ilvl) = ffield
                else
                    z = 1
                end if
            case ('aspect')
                if (.not. allocated(mountain_mesh%pm%aspect)) allocate(mountain_mesh%pm%aspect(shd%NA, shd%lc%NTYPE))
                if (ilvl == 0) then
                    do ilvl = 1, shd%lc%NTYPE
                        mountain_mesh%pm%aspect(:, ilvl) = ffield
                    end do
                else if (ilvl <= shd%lc%NTYPE) then
                    mountain_mesh%pm%aspect(:, ilvl) = ffield
                else
                    z = 1
                end if
            case ('delta')
                if (.not. allocated(mountain_mesh%pm%delta)) allocate(mountain_mesh%pm%delta(shd%NA, shd%lc%NTYPE))
                if (ilvl == 0) then
                    do ilvl = 1, shd%lc%NTYPE
                        mountain_mesh%pm%delta(:, ilvl) = ffield
                    end do
                else if (ilvl <= shd%lc%NTYPE) then
                    mountain_mesh%pm%delta(:, ilvl) = ffield
                else
                    z = 1
                end if
            case ('delta_elevmax')
                if (.not. allocated(mountain_mesh%pm%delta_elevmax)) allocate(mountain_mesh%pm%delta_elevmax(shd%NA, shd%lc%NTYPE))
                if (ilvl == 0) then
                    do ilvl = 1, shd%lc%NTYPE
                        mountain_mesh%pm%delta_elevmax(:, ilvl) = ffield
                    end do
                else if (ilvl <= shd%lc%NTYPE) then
                    mountain_mesh%pm%delta_elevmax(:, ilvl) = ffield
                else
                    z = 1
                end if
            case ('curvature')
                if (.not. allocated(mountain_mesh%pm%curvature)) allocate(mountain_mesh%pm%curvature(shd%NA, shd%lc%NTYPE))
                if (ilvl == 0) then
                    do ilvl = 1, shd%lc%NTYPE
                        mountain_mesh%pm%curvature(:, ilvl) = ffield
                    end do
                else if (ilvl <= shd%lc%NTYPE) then
                    mountain_mesh%pm%curvature(:, ilvl) = ffield
                else
                    z = 1
                end if

            !> Unrecognized.
            case default
                z = 2
        end select

        !> Status flags.
        if (z == 1) then
            line = "'" // trim(vattr(l)%attr) // "' has an unrecognized category or level out-of-bounds: " // trim(tlvl)
            call print_warning(line)
        else if (z == 2) then
            call print_warning("'" // trim(vattr(l)%attr) // "' is not recognized.")
        else if (z == 0) then
            n = n + 1
        end if
    end do

    !> Print number of active parameters.
    write(line, FMT_GEN) n
    call print_message('Active parameters in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
    close(iun)

end subroutine
