!>
!> Description: Distributes from GAT to GRD form.
!>
subroutine SCATTER(shd, iilen, ii1, ii2, GAT, GRD)

    use sa_mesh_variables

    !> Input variables.
    type(LandGridParams), intent(in) :: shd
    integer, intent(in) :: iilen, ii1, ii2
    real, dimension(iilen), intent(in) :: GAT

    !> Output variables.
    real, dimension(shd%NA) :: GRD

    integer k

    GRD = 0.0
    do k = ii1, ii2
        GRD(shd%lc%ILMOS(k)) = GRD(shd%lc%ILMOS(k)) + shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*GAT(k)
    end do

end subroutine
