!>
!> Description:
!>  Subroutine to read abstraction location information from
!>  MESH_input_abstractionpoint.txt.
!>
!> Input:
!*  shd: Basin shed object containing grid and drainage properties.
!*  iun: Unit of the input file.
!*  fname: Full path to the file.
!>
subroutine read_abstractionpoint_txt(shd, iun, fname, ierr)

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
    read(iun, *, err = 98) fms%absp%n

    !> Return if no locations were defined.
    if (fms%absp%n == 0) return

    !> Allocate variables for the driver.
    call allocate_abstraction_point_location(fms%absp, fms%absp%n, ierr)
    if (ierr /= 0) then
        call print_error('Unable to allocate variables.')
        return
    end if

    !> Read information.
    do i = 1, fms%absp%n
        read(iun, *, err = 98) fms%absp%meta%y(i), fms%absp%meta%x(i), fms%absp%meta%name(i)
    end do
    fms%absp%meta%y = fms%absp%meta%y/60.0
    fms%absp%meta%x = fms%absp%meta%x/60.0

    return

    !> Stop: Premature end of file.
98  ierr = 1
    call print_error('Unable to read the file.')
    write(line, FMT_GEN) fms%absp%n
    call print_message('Number of locations expected: ' // trim(adjustl(line)))
    write(line, FMT_GEN) i
    call print_message('Number found: ' // trim(adjustl(line)))
    return

end subroutine
