!>
!> Description:
!>  Subroutine to save initial states of variables to file. Variables
!>  shared by SA_MESH are accessible by 'sa_mesh_variables'.
!>  Other variables are accessible by their respecitve process
!>  module(s).
!>
subroutine save_initial_states_nc(fls, shd, fname, ierr)

    use model_files_variables
    use sa_mesh_common
    use nc_io

    implicit none

    !> Input variables.
    type(fl_ids):: fls
    type(ShedGridParams) :: shd
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) line
!-    real, dimension(:), allocatable :: dat1_r(:), dat2_r(:, :), dat3_r(:, :, :), dat4_r(:, :, :, :)
    real, dimension(:), allocatable :: dat_xy(:, :), dat_xym(:, :, :), dat_xylm(:, :, :, :), dat_xycm(:, :, :, :)
    real, parameter :: NO_DATA = -999.999
    integer iun, m, l, c, x, y, v, j, i, z

    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Reset spacing for screen output.
    call reset_tab()

    !> Open output file.
    z = 0
    line = trim(fname) !'MESH_initial_values.nc'
!-    call reset_tab()
!-    call print_message('SAVING: ' // trim(line))
!-    call increase_tab()
!-    call nc4_open_output(line, iun, z)
    call nc4_open_output(line, iun = iun, ierr = ierr)
    if (ierr /= 0) return

    !> Add projection.
!-    if (z == 0) call nc4_add_proj(iun, line, shd%CoordSys%Proj, shd%CoordSys%Ellips, shd%CoordSys%Zone, z)
    if (z == 0) then
        call nc4_add_projection( &
            iun, shd%CoordSys%Proj, &
            shd%CoordSys%Ellips, shd%CoordSys%Zone, shd%CoordSys%earth_radius, shd%CoordSys%grid_north_pole_latitude, &
            shd%CoordSys%grid_north_pole_longitude, &
            ierr = z)
    end if

    !> Add dimensions.
!-    allocate(dat1_r(shd%xCount))
!-    do i = 1, shd%xCount
!-        dat1_r(i) = (shd%xOrigin + shd%xDelta*i) - shd%xDelta/2.0
!-    end do
!-    if (z == 0) call nc4_define_dimension(iun, line, 'lon', x, z, dim_length = size(dat1_r))
!-    if (z == 0) then
!-        call nc4_add_variable_n_nf90_float(iun, line, 'longitude', 'longitude', 'degrees_east', NO_DATA, dat1_r, x, i, z)
!-    end if
!-    deallocate(dat1_r)
!-    allocate(dat1_r(shd%yCount))
!-    do i = 1, shd%yCount
!-        dat1_r(i) = (shd%yOrigin + shd%yDelta*i) - shd%yDelta/2.0
!-    end do
!-    if (z == 0) call nc4_define_dimension(iun, line, 'lat', y, z, dim_length = size(dat1_r))
!-    if (z == 0) then
!-        call nc4_add_variable_n_nf90_float(iun, line, 'latitude', 'latitude', 'degrees_north', NO_DATA, dat1_r, y, i, z)
!-    end if
!-    deallocate(dat1_r)
    if (z == 0) then
        call nc4_add_coordinates( &
            iun, shd%CoordSys%Proj, &
            shd%CoordSys%lat, shd%CoordSys%lon, shd%CoordSys%rlat, shd%CoordSys%rlon, &
            shd%CoordSys%xylat, shd%CoordSys%xylon, &
            shd%CoordSys%Ellips, shd%CoordSys%Zone, shd%CoordSys%earth_radius, shd%CoordSys%grid_north_pole_latitude, &
            shd%CoordSys%grid_north_pole_longitude, &
            dim1_id = y, dim2_id = x, &
            ierr = z)
    end if
!-    if (z == 0) call nc4_define_dimension(iun, line, 'level', l, z, dim_length = shd%lc%IGND)
    if (z == 0) call nc4_define_dimension(iun, 'level', dim_length = shd%lc%IGND, did = l, ierr = z)
!-    if (z == 0) call nc4_define_dimension(iun, line, 'subtile_types', c, z, dim_length = 4)
    if (z == 0) call nc4_define_dimension(iun, 'subtile_types', dim_length = 4, did = c, ierr = z)
!-    if (z == 0) call nc4_define_dimension(iun, line, 'gru', m, z, dim_length = shd%lc%NTYPE)
    if (z == 0) call nc4_define_dimension(iun, 'gru', dim_length = shd%lc%NTYPE, did = m, ierr = z)

    !> 3-D variables (x, y, m).
!-    allocate(dat3_r(shd%xCount, shd%yCount, shd%lc%NTYPE), dat4_r(shd%xCount, shd%yCount, max(shd%lc%IGND, 4), shd%lc%NTYPE))
    allocate( &
        dat_xym(shd%xCount, shd%yCount, shd%lc%NTYPE), dat_xylm(shd%xCount, shd%yCount, shd%lc%IGND, shd%lc%NTYPE), &
        dat_xycm(shd%xCount, shd%yCount, 4, shd%lc%NTYPE))
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%albsno(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_albs', 'Tile-based values for albs', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_albs', x, y, m, dat_xym, long_name = 'Tile-based values for albs', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%cmas(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_cmas', 'Tile-based values for cmas', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_cmas', x, y, m, dat_xym, long_name = 'Tile-based values for cmas', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%gro(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_gro', 'Tile-based values for gro', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_gro', x, y, m, dat_xym, long_name = 'Tile-based values for gro', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%qacan(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_qac', 'Tile-based values for qac', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_qac', x, y, m, dat_xym, long_name = 'Tile-based values for qac', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%lqwscan(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_rcan', 'Tile-based values for rcan', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_rcan', x, y, m, dat_xym, long_name = 'Tile-based values for rcan', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%rhosno(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_rhos', 'Tile-based values for rhos', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_rhos', x, y, m, dat_xym, long_name = 'Tile-based values for rhos', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%fzwscan(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_sncan', 'Tile-based values for sncan', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_sncan', x, y, m, dat_xym, long_name = 'Tile-based values for sncan', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%sno(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_sno', 'Tile-based values for sno', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_sno', x, y, m, dat_xym, long_name = 'Tile-based values for sno', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tacan(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_tac', 'Tile-based values for tac', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_tac', x, y, m, dat_xym, long_name = 'Tile-based values for tac', ierr = z)
    end if
    if (z == 0) then
        dat_xylm = NO_DATA
        do j = 1, shd%lc%IGND
            do i = 1, shd%lc%NML
                dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%tsol(i, j)
            end do
        end do
!-        call nc4_add_variable_xylm( &
!-            iun, line, 'tile_tbar', 'Tile-based values for tbar', '1', NO_DATA, &
!-            dat4_r(:, :, 1:shd%lc%IGND, :), x, y, l, m, v, z)
        call nc4_add_variable(iun, 'tile_tbar', x, y, l, m, dat_xylm, long_name = 'Tile-based values for tbar', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tbas(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_tbas', 'Tile-based values for tbas', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_tbas', x, y, m, dat_xym, long_name = 'Tile-based values for tbas', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tcan(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_tcan', 'Tile-based values for tcan', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_tcan', x, y, m, dat_xym, long_name = 'Tile-based values for tcan', ierr = z)
    end if
    if (z == 0) then
        dat_xylm = NO_DATA
        do j = 1, shd%lc%IGND
            do i = 1, shd%lc%NML
                dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%thicsol(i, j)
            end do
        end do
!-        call nc4_add_variable_xylm(iun, line, 'tile_thic', 'Tile-based values for thic', '1', NO_DATA, &
!-            dat4_r(:, :, 1:shd%lc%IGND, :), x, y, l, m, v, z)
        call nc4_add_variable(iun, 'tile_thic', x, y, l, m, dat_xylm, long_name = 'Tile-based values for thic', ierr = z)
    end if
    if (z == 0) then
        dat_xylm = NO_DATA
        do j = 1, shd%lc%IGND
            do i = 1, shd%lc%NML
                dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%thlqsol(i, j)
            end do
        end do
!-        call nc4_add_variable_xylm(iun, line, 'tile_thlq', 'Tile-based values for thlq', '1', NO_DATA, &
!-            dat4_r(:, :, 1:shd%lc%IGND, :), x, y, l, m, v, z)
        call nc4_add_variable(iun, 'tile_thlq', x, y, l, m, dat_xylm, long_name = 'Tile-based values for thlq', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tpnd(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_tpnd', 'Tile-based values for tpnd', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_tpnd', x, y, m, dat_xym, long_name = 'Tile-based values for tpnd', ierr = z)
    end if
    if (z == 0) then
        dat_xycm = NO_DATA
        do j = 1, 4
            do i = 1, shd%lc%NML
                dat_xycm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%tsfs(i, j)
            end do
        end do
!-        call nc4_add_variable_xylm(iun, line, 'tile_tsfs', 'Tile-based values for tsfs', '1', NO_DATA, &
!-            dat4_r(:, :, 1:4, :), x, y, c, m, v, z)
        call nc4_add_variable(iun, 'tile_tsfs', x, y, c, m, dat_xycm, long_name = 'Tile-based values for tsfs', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tsno(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_tsno', 'Tile-based values for tsno', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_tsno', x, y, m, dat_xym, long_name = 'Tile-based values for tsno', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%lqwssno(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_wsno', 'Tile-based values for wsno', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_wsno', x, y, m, dat_xym, long_name = 'Tile-based values for wsno', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%zpnd(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_zpnd', 'Tile-based values for zpnd', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_zpnd', x, y, m, dat_xym, long_name = 'Tile-based values for zpnd', ierr = z)
    end if
    if (z == 0) then
        dat_xym = NO_DATA
        do i = 1, shd%lc%NML
            dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%stggw(i)
        end do
!-        call nc4_add_variable_xym(iun, line, 'tile_lzs', 'Tile-based values for lzs', '1', NO_DATA, dat3_r, x, y, m, v, z)
        call nc4_add_variable(iun, 'tile_lzs', x, y, m, dat_xym, long_name = 'Tile-based values for lzs', ierr = z)
    end if
!-    deallocate(dat3_r, dat4_r)

    !> 2-D variables (x, y).
!-    allocate(dat2_r(shd%xCount, shd%yCount))
    allocate(dat_xy(shd%xCount, shd%yCount))
    if (z == 0) then
        dat_xy = NO_DATA
        do i = 1, shd%NA
            dat_xy(shd%xxx(i), shd%yyy(i)) = vs%grid%qi(i)
        end do
!-        call nc4_add_variable_xy(iun, line, 'grid_qi', 'Grid-based values for qi', '1', NO_DATA, dat2_r, x, y, j, z)
        call nc4_add_variable(iun, 'grid_qi', x, y, dat_xy, long_name = 'Grid-based values for qi', ierr = z)
    end if
    if (z == 0) then
        dat_xy = NO_DATA
        do i = 1, shd%NA
            dat_xy(shd%xxx(i), shd%yyy(i)) = vs%grid%stgch(i)
        end do
!-        call nc4_add_variable_xy(iun, line, 'grid_stgch', 'Grid-based values for stgch', '1', NO_DATA, dat2_r, x, y, j, z)
        call nc4_add_variable(iun, 'grid_stgch', x, y, dat_xy, long_name = 'Grid-based values for stgch', ierr = z)
    end if
    if (z == 0) then
        dat_xy = NO_DATA
        do i = 1, shd%NA
            dat_xy(shd%xxx(i), shd%yyy(i)) = vs%grid%qo(i)
        end do
!-        call nc4_add_variable_xy(iun, line, 'grid_qo', 'Grid-based values for qo', '1', NO_DATA, dat2_r, x, y, j, z)
        call nc4_add_variable(iun, 'grid_qo', x, y, dat_xy, long_name = 'Grid-based values for qo', ierr = z)
    end if
!-    deallocate(dat2_r)

    !> Close file.
    if (z == 0) then
        call nc4_close_file(iun, line, ierr = z)
    end if
    if (z /= 0) then
!-        call print_error('An error occured writing the file: ' // trim(line))
        ierr = 1
!-        return
    end if
#endif

end subroutine
