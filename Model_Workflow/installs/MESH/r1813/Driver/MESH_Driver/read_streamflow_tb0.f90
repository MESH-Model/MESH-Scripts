!>
!> Description:
!>  Subroutine to read streamflow gauge information from
!>  MESH_input_streamflow.tb0.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: 'MESH_input_streamflow.tb0').
!>
subroutine read_streamflow_tb0(shd, iun, fname, ierr)

    use strings
    use model_dates
    use sa_mesh_common
    use ensim_io

    implicit none

    !> Input variables.
    type(ShedGridParams) shd
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

    !> Get the number of gauge locations (i.e., columns) from the file.
    call count_columns_tb0(iun, vkeyword, nkeyword, fms%stmg%n, ierr)
    if (ierr /= 0) return

    !> Print warning and return if no gauge locations are defined.
    if (fms%stmg%n == 0) then
        call print_warning('No streamflow gauge locations were found.')
        return
    end if

    !> Allocate attributes for the driver.
    call allocate_streamflow_gauge_location(fms%stmg, fms%stmg%n, ierr)
    if (ierr /= 0) then
        call print_error('Unable to allocate variables.')
        return
    end if

    !> Parse attributes.
    z = 0

    !> Get the time-step of the records.
    call get_keyword_value(iun, vkeyword, nkeyword, ':DeltaT', fms%stmg%qomeas%dts, z)

    !> Populate other attributes.
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnName', fms%stmg%meta%name, fms%stmg%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnLocationY', fms%stmg%meta%y, fms%stmg%n, z)
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnLocationX', fms%stmg%meta%x, fms%stmg%n, z)

    !> Get the start time of the first record in the file.
    call parse_starttime( &
        iun, vkeyword, nkeyword, &
        fms%stmg%qomeas%iyear, fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%ihour, fms%stmg%qomeas%imins, &
        z)
    if (fms%stmg%qomeas%iyear > 0 .and. fms%stmg%qomeas%imonth > 0 .and. fms%stmg%qomeas%iday > 0) then
        fms%stmg%qomeas%ijday = get_jday(fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%iyear)
    end if

    !> Check for errors.
    if (z /= 0) call print_warning('Errors occurred parsing attributes in the file.')

    !> Position the file to the first record.
    call advance_past_header(iun, fname, z)

    return

end subroutine
