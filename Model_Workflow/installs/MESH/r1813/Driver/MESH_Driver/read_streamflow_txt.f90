!>
!> Description:
!>  Subroutine to read streamflow gauge information from
!>  MESH_input_streamflow.txt.
!>
!> Input:
!*  shd: Basin shed object containing grid and drainage properties.
!*  iun: Unit of the input file.
!*  fname: Full path to the file.
!>
subroutine read_streamflow_txt(shd, iun, fname, ierr)

    use mpi_module
    use sa_mesh_common

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer i
    character(len = DEFAULT_LINE_LENGTH) line

    !> Streamflow attributes pulled from 'fms':
    !*  stmg: Streamflow gauge structure
    !*  -   n: Number of elements dimensioned.
    !*  -   name(n): ID printed to output files.
    !*  -   y(n): Y-coordinate of outlet location.
    !*  -   x(n): X-coordinate of outlet location.
    !*  -   iy(n): Vertical index of the grid-cell containing the location.
    !*  -   jx(n): Horizontal index of the grid-cell containing the location.
    !*  -   n(n): Rank or index of the grid-cell containing the location.
    !*  -   DA(n): Drainage area.

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open the file. Check if the file exists.')
        return
    end if

    !> Read the number of locations.
    read(iun, *, err = 98)
    read(iun, *, err = 98) &
        fms%stmg%n, i, i, fms%stmg%qomeas%dts, fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, fms%stmg%qomeas%ihour

    !> Return if no locations were defined.
    if (fms%stmg%n == 0) return

    !> Allocate configuration variables for the driver.
    call allocate_streamflow_gauge_location(fms%stmg, fms%stmg%n, ierr)
    if (ierr /= 0) then
        call print_error('Unable to allocate variables.')
        return
    end if

    !> Read gauge location and name.
    do i = 1, fms%stmg%n
        read(iun, *, err = 98) fms%stmg%meta%y(i), fms%stmg%meta%x(i), fms%stmg%meta%name(i)
    end do
    fms%stmg%meta%y = fms%stmg%meta%y/60.0
    fms%stmg%meta%x = fms%stmg%meta%x/60.0

    return

    !> Stop: Premature end of file.
98  ierr = 1
    call print_error('Unable to read the file.')
    write(line, FMT_GEN) fms%rsvr%n
    call print_message('Number of gauge locations expected: ' // trim(adjustl(line)))
    write(line, FMT_GEN) i
    call print_message('Number found: ' // trim(adjustl(line)))
    return

end subroutine
