!>
!> MPI-related variables.
!>
module mpi_shared_variables

    implicit none

    save

    !> Information about the node(s).
    !*  inp: Number of nodes (including head node).
    !*  ipid: Zero-based index of active/current/this node (default: 0 = head node).
    integer :: inp = 1, ipid = 0

    !> Configuration options.
    !*  izero: If to include the head node in tile iterations when multiple nodes are used
    !       (default: 0 = reserve head node for between grid processes and book-keeping).
    integer izero

    !> Tile-based indices.
    !*  il1: First index to be used in tile iterations on this node.
    !*  il2: Last index to be used in tile iterations on this node.
    !*  ilen: Total number of tile-based indices active on this node.
    integer il1, il2, iln

    !> Grid-based indices.
    !*  i1: First index to be used in grid iterations on this node.
    !*  i2: Last index to be used in grid iterations on this node.
    integer i1, i2

    !> Variables.
    !*  itag: Transaction ID (incremented by calling routines).
    integer :: itag = 0

end module
