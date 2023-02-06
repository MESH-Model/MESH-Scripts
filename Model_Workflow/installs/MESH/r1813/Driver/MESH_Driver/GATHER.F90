!>
!> Description: Distributes from GRD to GAT form.
!>
subroutine GATHER(shd, iilen, ii1, ii2, GRD, GAT)

    use sa_mesh_variables

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iilen, ii1, ii2
    real, intent(in), dimension(shd%NA) :: GRD

    !> Output variables.
    real, dimension(iilen) :: GAT

    !> Local variables.
    integer k

    do k = ii1, ii2
        GAT(k) = GRD(shd%lc%ILMOS(k))
    end do

end subroutine
