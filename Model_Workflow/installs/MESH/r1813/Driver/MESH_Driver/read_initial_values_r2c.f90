!> Description:
!>  Subroutine to read initial values from a single-frame 'r2c' format
!>  file. Values are parsed by order of RANK.
!>
!> Inputs:
!*  shd: Basin 'shed' object (properties).
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
!> Outputs:
!*  ierr: Status (0: Normal).
subroutine read_initial_values_r2c(shd, iun, fname, ierr)

    !> strings: For 'lowercase' function.
    !> sa_mesh_common: For common MESH variables and routines.
    !> ensim_io: For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
    use ensim_io

    !> Process modules: Required for process variables, parameters.

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
    integer nkeyword, nattr, ilvl, n, l, j, i, z
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
    if (ierr /= 0) then
        close(iun)
        return
    end if

    !> Check the spatial definition in the header.
    call validate_header_spatial( &
        vkeyword, nkeyword, &
        shd%CoordSys%Proj, shd%xCount, shd%xDelta, shd%xOrigin, shd%yCount, shd%yDelta, shd%yOrigin, &
        ierr)
    if (ierr /= 0) then
        close(iun)
        return
    end if

    !> Get the list of attributes.
    call parse_header_attribute_ensim(iun, vkeyword, nkeyword, vattr, nattr, ierr)
    if (ierr /= 0) then
        call print_error('Error reading attributes from the header in the file.')
        close(iun)
        return
    end if
    if (nattr == 0) call print_warning('No attributes were found in the file.')

    !> Advance past the end of the header.
    call advance_past_header(iun, fname, ierr)
    if (ierr /= 0) then
        call print_error('Encountered premature end of file.')
        close(iun)
        return
    end if

    !> Read and parse the attribute data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)
    if (ierr /= 0) then
        call print_error('Error reading attribute values in the file.')
        close(iun)
        return
    end if

    !> Distribute the data to the appropriate variable.
    allocate(ffield(shd%NA))
    n = 0
    do l = 1, nattr

        !> Extract variable name and level.
        z = 0
        tfield = lowercase(vattr(l)%attr)
        i = index(trim(adjustl(tfield)), ' ')
        ilvl = 0
        if (i > 0) then
            tlvl = tfield((i + 1):)
            tfield = tfield(1:i)
            call value(tlvl, ilvl, z)
            if (z /= 0) ilvl = 0
        end if
        if (DIAGNOSEMODE) call print_message("Reading '" // trim(tfield) // "'.")

        !> Assign the data to a vector.
        call r2c_to_rank(iun, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, ierr)
        if (ierr /= 0) then
            call print_error("Unable to read the '" // trim(vattr(l)%attr) // "' attribute.")
            close(iun)
            return
        end if

        !> Determine the variable.
        z = 0
        select case (adjustl(tfield))

            case ('qi1')
                if (associated(vs%grid%qi)) then
                    vs%grid%qi(1:shd%NA) = ffield
                else
                    z = 3
                end if
            case ('qo1')
                if (associated(vs%grid%qo)) then
                    if (ilvl == 0) then
                        vs%grid%qo(1:shd%NA) = ffield
                    else
                        z = 3
                    end if
                else
                    z = 3
                end if
            case ('stor')
                if (associated(vs%grid%stgch)) then
                    vs%grid%stgch(1:shd%NA) = ffield
                else
                    z = 3
                end if
            case ('lzs')
                if (associated(vs%grid%stggw)) then
                    vs%grid%stggw(1:shd%NA) = ffield
                else
                    z = 3
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
        else if (z == 3) then
            call print_remark("'" // trim(vattr(l)%attr) // "' is not active.")
        else if (z == 0) then
            n = n + 1
        end if
    end do

    !> Print number of active variables.
    write(line, FMT_GEN) n
    call print_message('Active variables in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
    close(iun)

end subroutine
