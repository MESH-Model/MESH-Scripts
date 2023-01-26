!>
!> Stub to call MPI subroutines. Meant to replace
!> mpi_stub.f90 when an MPI compiler is being used.
!>
module mpi_module

    !> Generic MPI library.
    use mpi

    !> Includes flags, variables, and utilities.
    use mpi_flags
    use mpi_shared_variables
    use mpi_utilities

end module !mpi_module
