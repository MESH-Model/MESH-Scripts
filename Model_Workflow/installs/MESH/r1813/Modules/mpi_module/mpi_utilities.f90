!>
!> MPI-related utilities.
!>
module mpi_utilities

    implicit none

    contains

    !> Description:
    !>  Determine il1:il2 indices and ilen based on the number of active nodes
    !>  and tiles in the setup for the given node. The subroutine will nudge
    !>  il1 and il2 so that no grid is split between separate nodes.
    !>
    !> Variables:
    !*  inp: Number of nodes (including head node).
    !*  izero: If to include the head node in tile iterations when multiple nodes are used
    !       (default: 0 = reserve head node for between grid processes and book-keeping).
    !*  ipid: Zero-based index of active/current/this node (default: 0 = head node).
    !*  NML: Number of active files in the setup.
    !*  ILMOS: NML-to-Grid lookup table.
    !>
    !> Returns:
    !*  il1: First index to be used in tile iterations on this node.
    !*  il2: Last index to be used in tile iterations on this node.
    !*  ilen: Total number of indices active on this node.
    subroutine mpi_split_nml(inp, izero, ipid, &
                             NML, ILMOS, &
                             il1, il2, ilen)

        !> Input variables.
        integer, intent(in) :: inp, izero, ipid
        integer, intent(in) :: NML
        integer, intent(in), dimension(:) :: ILMOS

        !> Output variables
        integer, intent(out) :: il1, il2, ilen

        !> Calculate an initial lower index.
        il1 = max(min(ceiling(NML/real(inp - izero))*(ipid - izero) + 1, NML), 0)

        !> On succeeding nodes, bump the index to begin at the next grid in
        !> the sequence if otherwise the GRUs and/or tiles of the grid would
        !> be broken across nodes.
        if (ipid > (0 + izero)) then
            do while (ILMOS(il1 - 1) == ILMOS(il1))
                il1 = il1 + 1
            end do
        end if

        !> Calculate an initial upper index.
        il2 = max(min(ceiling(NML/real(inp - izero))*((ipid - izero) + 1), NML), il1)

        !> Bump the index to include the entire grid so that the GRUs and/or
        !> tiles of the grid are not broken across nodes.
        if (ipid < (inp - 1) .and. ipid /= 0) then
            do while (ILMOS(il2) == ILMOS(il2 + 1) .and. il2 < NML)
                il2 = il2 + 1
            end do
        end if

        !> Override for head node so that variables for bookkeeping that are
        !> allocated from il1:il2 are properly allocated 1:NML.
        if (ipid == 0) then
            il1 = 1
            il2 = NML
        end if

        !> Calculate the total number of active elements in the sequence.
        ilen = (il2 - il1) + 1

    end subroutine

end module
