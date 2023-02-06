module WF_ROUTE_module

    use WF_ROUTE_config

    implicit none

    contains

    subroutine WF_ROUTE_between_grid(fls, shd)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        type(fl_ids) :: fls
        type(ShedGridParams), intent(in) :: shd

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        !> shd%NAA is the total number of grids.
        !> shd%NA is the total number of grids in the basin.
        !> WF_NAA is the number of outlets in the basin (e.g., shd%NA - shd%NAA).
        call WF_ROUTE(WF_ROUTETIMESTEP, wfp%r1, wfp%r2, &
                      shd%NA, WF_NAA, shd%lc%NTYPE, shd%yCount, shd%xCount, shd%iyMin, &
                      shd%iyMax, shd%jxMin, shd%jxMax, shd%yyy, shd%xxx, shd%IAK, shd%IROUGH, &
                      shd%ICHNL, shd%NEXT, shd%IREACH, shd%AL, shd%GRDN, shd%GRDE, &
                      shd%DA, shd%BNKFLL, shd%SLOPE_CHNL, int(shd%ELEV), shd%FRAC, &
                      shd%CHNL_LEN, &
                      WF_RTE_flgs%RLFLAG, WF_RTE_flgs%CAPFLAG, &
                      fms%stmg%n, WF_NL, WF_MHRD, WF_KT, fms%stmg%meta%iy, fms%stmg%meta%jx, &
                      fms%stmg%qomeas%val, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, fms%rsvr%meta%rnk, &
                      fms%rsvr%n, WF_NREL, WF_KTR, fms%rsvr%meta%iy, fms%rsvr%meta%jx, fms%rsvr%meta%name, &
                      WF_B1, WF_B2, WF_B3, WF_B4, WF_B5, fms%rsvr%rlsmeas%val, WF_QR, &
                      WF_TIMECOUNT, WF_NHYD, WF_QBASE, WF_QI1, vs%grid%qi, WF_QO1, vs%grid%qo, &
                      wfp%aa1, wfp%aa2, wfp%aa3, wfp%aa4, &
                      WF_STORE1, vs%grid%stgch, &
                      ic%dts, &
                      (vs%grid%rff + vs%grid%rchg)/ic%dts*shd%FRAC, &
                      shd%NA, shd%NRVR, fms%rsvr%n, fms%stmg%n, shd%NA, &
                      fms%stmg%meta%rnk, JAN, ic%now%jday, ic%now%hour, ic%now%mins)

        !> this is done so that INIT_STORE is not recalculated for
        !> each iteration when wf_route is not used
        if (JAN == 1) then
            JAN = 2
        end if

        !> Update SA_MESH output variables.
        !> Override the output variable because 'storage' for reservoirs is an accumulation of flow
        !> and does not translate to the 'stgch' variable.
        out%ts%grid%stgch = vs%grid%stgch
        if (fms%rsvr%n > 0) then
            out%ts%grid%stgch(fms%rsvr%meta%rnk(:)) = out%NO_DATA
        end if

    end subroutine

end module
