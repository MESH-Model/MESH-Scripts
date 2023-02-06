module cropland_irrigation_within_tile

    use cropland_irrigation_variables

    implicit none

    contains

    subroutine runci_within_tile(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        real, external :: calc_ET0

        !> For MPI exchange.
        integer ipid_recv, ierrcode, istop, u, invars, iiln, ii1, ii2, ierr
        integer, dimension(:), allocatable :: irqst
        integer, dimension(:, :), allocatable :: imstat
        logical lstat

        !> Local variables.
        integer k, ki, t, c, i, ikey
        real Kc

        !> Return if the cropland irrigation module is not active.
        if (.not. cifg%PROCESS_ACTIVE) return

        !> Return if the head node when running in parallel.
        if (ipid /= 0 .or. izero == 0) then

            do k = il1, il2

                !> Grab the grid index (for ylat, xlng)
                ki = shd%lc%ILMOS(k)

                !> Calc 'calc_ET0' (POTEVP).
                vs%tile%potevp(k) = calc_ET0( &
                    vs%tile%ta(k), vs%tile%uv(k), vs%tile%qa(k), vs%tile%pres(k), vs%tile%fsin(k), &
                    shd%ylat(ki), shd%xlng(ki), shd%ELEV(ki), &
                    pm%tile%zrfm(k), &
                    pm%tile%fcan(k, 1), pm%tile%fcan(k, 2), pm%tile%fcan(k, 3), pm%tile%fcan(k, 4), &
                    ic%now%jday, ic%now%hour)

                !> Activate the new growing season.
                if (civ%jdini(k) == 0) then

                    !> Starting day of the growing season.
                    if (cip%jdsow(k) == 0 .and. vs%tile%gro(k) > 0.0) then
                        civ%jdini(k) = ic%now%jday
                    else
                        civ%jdini(k) = cip%jdsow(k)
                    end if

                    !> Starting days of the growth stages.
                    civ%jddev(k) = civ%jdini(k) + cip%ldini(k)
                    civ%jdmid(k) = civ%jddev(k) + cip%lddev(k)
                    civ%jdlate(k) = civ%jdmid(k) + cip%ldmid(k)
                    civ%jdend(k) = civ%jdlate(k) + cip%ldlate(k)

                end if

                !> Cycle if outside the growing season or not a crop.
                if (civ%icrop(k) /= 1 .or. ic%now%jday < (civ%jdini(k) - 1) .or. ic%now%jday > (civ%jdend(k) + 1)) cycle

                    !> Initial conditions for the beginning of the period.
                    if (ic%now%jday == (civ%jdini(k) - 1)) then

                        !> Initialize states.
                        if (ic%ts_daily == 1) then
                            do ikey = civ%fk%kmin, civ%fk%kmax
                                civ%vars(ikey)%lqws2_mm(k) = 0.0
                                civ%vars(ikey)%lqws1_mm(k) = 0.0
                                civ%vars(ikey)%pre_mm(k) = 0.0
                                civ%vars(ikey)%pevp_mm(k) = 0.0
                            end do
                        end if

                        !> Daily.
                        if (btest(cifg%ts_flag, 0)) then
                            civ%vars(civ%fk%KDLY)%lqws2_mm(k) = civ%vars(civ%fk%KDLY)%lqws2_mm(k) + &
                                (sum(vs%tile%thlqsol(k, :)*vs%tile%dzsolhyd(k, :)*1000.0)*pm%tile%fcan(k, 3))/((3600.0/ic%dts)*24.0)
                        end if

                        !> Hourly.
                        if (btest(cifg%ts_flag, 2) .and. ic%ts_daily > (3600.0/ic%dts)*23.0) then
                            civ%vars(civ%fk%KHLY)%lqws2_mm(k) = civ%vars(civ%fk%KHLY)%lqws2_mm(k) + &
                                (sum(vs%tile%thlqsol(k, :)*vs%tile%dzsolhyd(k, :)*1000.0)*pm%tile%fcan(k, 3))/(3600.0/ic%dts)
                        end if

                        !> Per time-step.
                        if (btest(cifg%ts_flag, 3) .and. ic%ts_daily == (3600.0/ic%dts)*24.0) then
                            civ%vars(civ%fk%KTS)%lqws2_mm(k) = &
                                sum(vs%tile%thlqsol(k, :)*vs%tile%dzsolhyd(k, :)*1000.0)*pm%tile%fcan(k, 3)
                        end if

                    end if

                    !> Outside the growing period.
                    if (ic%now%jday == (civ%jdend(k) + 1)) then
                        civ%jdini(k) = 0
                        do ikey = civ%fk%kmin, civ%fk%kmax
                            civ%vars(ikey)%icu_mm(k) = 0.0
                        end do
                        cycle
                    end if

                    !> Inside the growing period.
                    if (ic%now%jday >= civ%jdini(k)) then

                        !> Accumulate states for the present period.
                        do ikey = civ%fk%kmin, civ%fk%kmax
                            civ%vars(ikey)%pre_mm(k) = civ%vars(ikey)%pre_mm(k) + vs%tile%pre(k)*pm%tile%fcan(k, 3)*ic%dts
                            civ%vars(ikey)%pevp_mm(k) = civ%vars(ikey)%pevp_mm(k) + vs%tile%potevp(k)*pm%tile%fcan(k, 3)*ic%dts
                            civ%vars(ikey)%lqws1_mm(k) = civ%vars(ikey)%lqws1_mm(k) + &
                                sum(vs%tile%thlqsol(k, :)*vs%tile%dzsolhyd(k, :)*1000.0)*pm%tile%fcan(k, 3)
                        end do

                        !> Determine Kc.
                        if (ic%now%jday >= civ%jdlate(k)) then
                            Kc = cip%Kclate(k)
                        else if (ic%now%jday >= civ%jdmid(k)) then
                            Kc = cip%Kcmid(k)
                        else if (ic%now%jday >= civ%jddev(k)) then
                            Kc = cip%Kcdev(k)
                        else
                            Kc = cip%Kcini(k)
                        end if

                        !> Daily.
                        if (btest(cifg%ts_flag, 0) .and. ic%now%day /= ic%next%day) then
                            civ%vars(civ%fk%KDLY)%lqws1_mm(k) = civ%vars(civ%fk%KDLY)%lqws1_mm(k)/ic%ts_daily
                            civ%vars(civ%fk%KDLY)%icu_mm(k) = (105.0*Kc*civ%vars(civ%fk%KDLY)%pevp_mm(k)) - &
                                civ%vars(civ%fk%KDLY)%pre_mm(k) - &
                                (civ%vars(civ%fk%KDLY)%lqws1_mm(k) - civ%vars(civ%fk%KDLY)%lqws2_mm(k))
                            civ%vars(civ%fk%KDLY)%pre_mm(k) = 0.0
                            civ%vars(civ%fk%KDLY)%pevp_mm(k) = 0.0
                            civ%vars(civ%fk%KDLY)%lqws2_mm(k) = civ%vars(civ%fk%KDLY)%lqws1_mm(k)
                            civ%vars(civ%fk%KDLY)%lqws1_mm(k) = 0.0
                        end if

                        !> Hourly.
                        if (btest(cifg%ts_flag, 2) .and. ic%now%hour /= ic%next%hour) then
                            civ%vars(civ%fk%KHLY)%lqws1_mm(k) = civ%vars(civ%fk%KHLY)%lqws1_mm(k)/ic%ts_hourly
                            civ%vars(civ%fk%KHLY)%icu_mm(k) = (105.0*Kc*civ%vars(civ%fk%KHLY)%pevp_mm(k)) - &
                                civ%vars(civ%fk%KHLY)%pre_mm(k) - &
                                (civ%vars(civ%fk%KHLY)%lqws1_mm(k) - civ%vars(civ%fk%KHLY)%lqws2_mm(k))
                            civ%vars(civ%fk%KHLY)%pre_mm(k) = 0.0
                            civ%vars(civ%fk%KHLY)%pevp_mm(k) = 0.0
                            civ%vars(civ%fk%KHLY)%lqws2_mm(k) = civ%vars(civ%fk%KHLY)%lqws1_mm(k)
                            civ%vars(civ%fk%KHLY)%lqws1_mm(k) = 0.0
                        end if

                        !> Per time-step.
                        if (btest(cifg%ts_flag, 3)) then
                            civ%vars(civ%fk%KTS)%icu_mm(k) = (105.0*Kc*civ%vars(civ%fk%KTS)%pevp_mm(k)) - &
                                civ%vars(civ%fk%KTS)%pre_mm(k) - &
                                (civ%vars(civ%fk%KTS)%lqws1_mm(k) - civ%vars(civ%fk%KTS)%lqws2_mm(k))
                            civ%vars(civ%fk%KTS)%pre_mm(k) = 0.0
                            civ%vars(civ%fk%KTS)%pevp_mm(k) = 0.0
                            civ%vars(civ%fk%KTS)%lqws2_mm(k) = civ%vars(civ%fk%KTS)%lqws1_mm(k)
                            civ%vars(civ%fk%KTS)%lqws1_mm(k) = 0.0
                        end if

                end if

            end do

        end if

        !> Gather variables from parallel nodes.

        !> Check active kind of 'real'.
        if (kind(2.0) == 8) then
            c = MPI_REAL8
        else
            c = MPI_REAL
        end if

        !> Send/receive process.
        invars = 3*(civ%fk%kmax - civ%fk%kmin + 1)

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))
            irqst = mpi_request_null
            t = itag
            i = 1
            do ikey = civ%fk%kmin, civ%fk%kmax
                call mpi_isend(civ%vars(ikey)%icu_mm(il1:il2), iln, c, 0, itag + i, mpi_comm_world, irqst(i), ierr)
                i = i + 1
                call mpi_isend(civ%vars(ikey)%lqws2_mm(il1:il2), iln, c, 0, itag + i, mpi_comm_world, irqst(i), ierr)
                i = i + 1
!todo: remove potevp (global var)
                call mpi_isend(vs%tile%potevp(il1:il2), iln, c, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            end do
            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(invars, irqst, lstat, imstat, ierr)
            end do

        else if (inp > 1) then

            !> Receive data from worker nodes.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))

            !> Receive and assign variables.
            do u = 1, (inp - 1)
                irqst = mpi_request_null
                imstat = 0
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iiln)
                t = itag
                i = 1
                do ikey = civ%fk%kmin, civ%fk%kmax
                    call mpi_irecv(civ%vars(ikey)%icu_mm(ii1:ii2), iiln, c, u, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
                    call mpi_irecv(civ%vars(ikey)%lqws2_mm(ii1:ii2), iiln, c, u, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
!todo: remove potevp (global var)
                    call mpi_irecv(vs%tile%potevp(ii1:ii2), iiln, c, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end do
                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(invars, irqst, lstat, imstat, ierr)
                end do
            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) then
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
            itag = 0
        else
            itag = t + i
        end if

    end subroutine

end module
