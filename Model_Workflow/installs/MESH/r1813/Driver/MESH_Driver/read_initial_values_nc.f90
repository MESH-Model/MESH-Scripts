!>
!> Description:
!>  Subroutine to read initial states of variables from file. Variables
!>  shared by SA_MESH are accessible by 'sa_mesh_variables'.
!>  Other variables are accessible by their respecitve process
!>  module(s).
!>
subroutine read_initial_states_nc(fls, shd, fname, ierr)

    use model_files_variables
    use sa_mesh_common
    use nc_io

    implicit none

    !> Input variables.
    type(fl_ids), intent(in) :: fls
    type(ShedGridParams), intent(in) :: shd
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) units, line
!-    real, dimension(:), allocatable :: dat1_r(:), dat2_r(:, :), dat3_r(:, :, :), dat4_r(:, :, :, :)
    real, dimension(:), allocatable :: dat_xy(:, :), dat_xym(:, :, :), dat_xylm(:, :, :, :), dat_xycm(:, :, :, :)
    real fill_r
    real, parameter :: NO_DATA = -999.999
    integer iun, m, x, y, v, k, j, i, z

    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Reset spacing for screen output.
    call reset_tab()

    !> Open input file.
    z = 0
    line = trim(fname) !'MESH_initial_values.nc'
!-    call reset_tab()
!-    call print_message('READING: ' // trim(line))
!-    call increase_tab()
!-    call nc4_open_input(line, iun, z)
    call nc4_open_input(line, iun = iun, ierr = ierr)
    if (ierr /= 0) return

    !> 3-D variables (x, y, m).
!-    allocate(dat3_r(shd%xCount, shd%yCount, shd%lc%NTYPE), dat4_r(shd%xCount, shd%yCount, max(shd%lc%IGND, 4), shd%lc%NTYPE))
    allocate( &
        dat_xym(shd%xCount, shd%yCount, shd%lc%NTYPE), dat_xylm(shd%xCount, shd%yCount, shd%lc%IGND, shd%lc%NTYPE), &
        dat_xycm(shd%xCount, shd%yCount, 4, shd%lc%NTYPE))
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_albs', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_albs', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%albsno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_cmas', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_cmas', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%cmas(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_gro', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_gro', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%gro(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_qac', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_qac', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%qacan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_rcan', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_rcan', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%lqwscan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_rhos', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_rhos', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%rhosno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_sncan', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_sncan', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%fzwscan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_sno', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_sno', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%sno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_tac', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_tac', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%tacan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xylm(iun, line, 'tile_tbar', dat4_r(:, :, 1:shd%lc%IGND, :), units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_tbar', 'lon', 'lat', 'level', 'gru', dat_xylm, fill_r, ierr = z)
    if (z == 0) then
        do j = 1, shd%lc%IGND
            do i = 1, shd%lc%NML
                if (dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                    vs%tile%tsol(i, j) = dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                end if
            end do
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_tbas', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_tbas', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%tbas(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_tcan', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_tcan', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%tcan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xylm(iun, line, 'tile_thic', dat4_r(:, :, 1:shd%lc%IGND, :), units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_thic', 'lon', 'lat', 'level', 'gru', dat_xylm, fill_r, ierr = z)
    if (z == 0) then
        do j = 1, shd%lc%IGND
            do i = 1, shd%lc%NML
                if (dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                    vs%tile%thicsol(i, j) = dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                end if
            end do
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xylm(iun, line, 'tile_thlq', dat4_r(:, :, 1:shd%lc%IGND, :), units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_thlq', 'lon', 'lat', 'level', 'gru', dat_xylm, fill_r, ierr = z)
    if (z == 0) then
        do j = 1, shd%lc%IGND
            do i = 1, shd%lc%NML
                if (dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                    vs%tile%thlqsol(i, j) = dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                end if
            end do
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_tpnd', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_tpnd', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%tpnd(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) then
!-        call nc4_get_variable_xylm(iun, line, 'tile_tsfs', dat4_r(:, :, 1:4, :), units, fill_r, z, name_l = 'subtile_types')
!-    end if
    if (z == 0) call nc4_get_variable(iun, 'tile_tsfs', 'lon', 'lat', 'subtile_types', 'gru', dat_xycm, fill_r, ierr = z)
    if (z == 0) then
        do j = 1, 4
            do i = 1, shd%lc%NML
                if (dat_xycm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                    vs%tile%tsfs(i, j) = dat_xycm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                end if
            end do
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_tsno', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_tsno', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%tsno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_wsno', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_wsno', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%lqwssno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_zpnd', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_zpnd', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%zpnd(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xym(iun, line, 'tile_lzs', dat3_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'tile_lzs', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%lc%NML
            if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                vs%tile%stggw(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
            end if
        end do
    end if
!-    deallocate(dat3_r, dat4_r)

    !> 2-D variables (x, y).
!-    allocate(dat2_r(shd%xCount, shd%yCount))
    allocate(dat_xy(shd%xCount, shd%yCount))
!-    if (z == 0) call nc4_get_variable_xy(iun, line, 'grid_qi', dat2_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'grid_qi', 'lon', 'lat', dat_xy, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%NA
            if (dat_xy(shd%xxx(i), shd%yyy(i)) /= fill_r) vs%grid%qi(i) = dat_xy(shd%xxx(i), shd%yyy(i))
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xy(iun, line, 'grid_stgch', dat2_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'grid_stgch', 'lon', 'lat', dat_xy, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%NA
            if (dat_xy(shd%xxx(i), shd%yyy(i)) /= fill_r) vs%grid%stgch(i) = dat_xy(shd%xxx(i), shd%yyy(i))
        end do
    end if
!-    if (z == 0) call nc4_get_variable_xy(iun, line, 'grid_qo', dat2_r, units, fill_r, z)
    if (z == 0) call nc4_get_variable(iun, 'grid_qo', 'lon', 'lat', dat_xy, fill_r, ierr = z)
    if (z == 0) then
        do i = 1, shd%NA
            if (dat_xy(shd%xxx(i), shd%yyy(i)) /= fill_r) vs%grid%qo(i) = dat_xy(shd%xxx(i), shd%yyy(i))
        end do
    end if
!-    deallocate(dat2_r)

    !> Close file.
    if (z == 0) then
        call nc4_close_file(iun, line, ierr = ierr)
    end if
    if (z /= 0) then
!-        call print_error('An error occured reading the file: ' // trim(line))
        ierr = 1
!-        return
    end if
#endif

end subroutine
