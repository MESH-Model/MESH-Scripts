!>
!> Description: Scatter GRD values to an R2C grid using the 'xxx' and
!> 'yyy' attributes of the basin shed information.
!>
subroutine tile_connector(shd, grd, r2c, accumulate)

    !> For: type(ShedGridParams) :: shd.
    use sa_mesh_common

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    real, dimension(shd%NA), intent(in) :: grd
    logical, optional :: accumulate

    !> Output variables.
    real, dimension(shd%yCount, shd%xCount) :: r2c

    !> Local variables.
    integer i

    !> Reset the R2C grid if not accumulating the variable.
    if (present(accumulate)) then
        if (.not. accumulate) r2c = 0.0
    end if

    !> Distribute the GRD values to the R2C grid.
    do i = 1, shd%NAA
        r2c(shd%yyy(i), shd%xxx(i)) = r2c(shd%yyy(i), shd%xxx(i)) + grd(i)/shd%FRAC(i)
    end do

end subroutine
