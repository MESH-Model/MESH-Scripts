!>
!> Description:
!>  Subroutine to read abstraction location information from
!>  MESH_input_abstractionpoint.tb0.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: 'MESH_input_abstractionpoint.tb0').
!>
subroutine read_abstractionpoint_tb0(shd, iun, fname, ierr)

    use strings
    use model_dates
    use sa_mesh_common
    use ensim_io

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    integer nkeyword, z

    !> Open the file and read the header.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    call open_ensim_input(iun, fname, ierr)
    if (ierr /= 0) return
    call parse_header_ensim(iun, vkeyword, nkeyword, ierr)
    if (ierr /= 0) return

    !> Check the spatial definition in the header.
    call validate_header_spatial(vkeyword, nkeyword, shd%CoordSys%Proj, ierr)
    if (ierr /= 0) return

    !> Get the number of outlet locations (i.e., columns) from the file.
    call count_columns_tb0(iun, vkeyword, nkeyword, fms%absp%n, ierr)
    if (ierr /= 0) return

    !> Print a warning and return if no locations are defined.
    if (fms%absp%n == 0) then
        call print_warning('No abstraction points were found.')
        return
    end if

    !> Allocate attributes for the driver.
    call allocate_abstraction_point_location(fms%absp, fms%absp%n, ierr)
    if (ierr /= 0) then
        call print_error('Unable to allocate variables.')
        return
    end if

    !> Parse attributes.
    z = 0

    !> Get the time-step of the records.
    call get_keyword_value(iun, vkeyword, nkeyword, ':DeltaT', fms%absp%sabst%dts, z)

    !> Populate other attributes.
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnName', fms%absp%meta%name, fms%absp%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnLocationY', fms%absp%meta%y, fms%absp%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnLocationX', fms%absp%meta%x, fms%absp%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':MinStorage', fms%absp%smin, fms%absp%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':MinfracStorage', fms%absp%fsmin, fms%absp%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':MinLevel', fms%absp%zlvl0, fms%absp%n, z)

    !> Get the start time of the first record in the file.
    call parse_starttime( &
        iun, vkeyword, nkeyword, &
        fms%absp%sabst%iyear, fms%absp%sabst%imonth, fms%absp%sabst%iday, fms%absp%sabst%ihour, fms%absp%sabst%imins, &
        z)
    if (fms%absp%sabst%iyear > 0 .and. fms%absp%sabst%imonth > 0 .and. fms%absp%sabst%iday > 0) then
        fms%absp%sabst%ijday = get_jday(fms%absp%sabst%imonth, fms%absp%sabst%iday, fms%absp%sabst%iyear)
    end if

    !> Check for errors.
    if (z /= 0) call print_warning('Errors occurred parsing attributes in the file.')

    !> Position the file to the first record.
    call advance_past_header(iun, fname, z)

    return

end subroutine
