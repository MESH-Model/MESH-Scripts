!>
!> MPI-related control flags.
!>
module mpi_flags

    implicit none

    !*  MPIUSEBARRIER: Enable or disable the MPI barrier after sending and
    !>      receiving data. The barrier should be enabled if data cannot be received
    !>      as quickly as it can be sent (default: 1 = enable).
    integer :: MPIUSEBARRIER = 1

end module
