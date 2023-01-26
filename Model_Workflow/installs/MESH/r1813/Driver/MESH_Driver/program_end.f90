!> Description:
!>  Stop the program with normal exit.
subroutine program_end()

    !> 'mpi_module': For call to 'MPI_Finalize' and 'inp'.
    !> 'print_routines: For print routines.
    use mpi_module
    use print_routines

    implicit none

    !> Local variables.
    integer ierr, istat

    !> Finalize MPI processes (no check against 'inp' in case 'MPI' was run with -np 1).
    call MPI_Finalize(ierr)
    if (ierr /= MPI_SUCCESS) call print_warning('MPI exchange failed to exit with normal status.')

    !> Stop the program.
    stop

end subroutine
