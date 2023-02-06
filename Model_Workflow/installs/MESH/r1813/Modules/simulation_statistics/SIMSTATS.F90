!>
!> Description:
!> MAM - FOR AUTOCALIBRATION USING PRE-EMPTION - A MAXIMUM OF 1 YEAR (365 DAYS)
!> DAILY STREAM FLOW IS SUPPOSED TO BE USED FOR AUTOCALIBRATION PURPOSE.
!>
module SIMSTATS

    use SIMSTATS_config
    use flags
    use model_dates

    use simstats_nse, only: nse_calc
    use simstats_sae, only: sae_calc
    use simstats_saesrt, only: saesrt_calc
    use calc_drms
    use calc_abserr

    implicit none

    private
    public mtsflg, stats_init, stats_update_stfl_daily, stats_write, fbest, ftest
    public stats_state_resume, stats_state_save
    public qobs, qsim, ncal, ns

!    integer, parameter :: dp=kind(0.d0)

    !* NCAL:    ACTUAL NUMBER OF CALIBRATION DATA
    !* COUNTER: COUNTER FOR THE NUMBER OF PRE-EMPTION STARTS
    !* EXISTS:  LOGICAL TO CHECK IF "pre_emption_value.txt" FILE EXISTS
    !* SAE:     SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN OBSERVED
    !*          AND SIMULATED STREAM FLOWS)
    !* SAESRT:  SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN SORTED OBSERVED
    !*          AND SORTED SIMULATED STREAM FLOWS)
    !* NSE:     MEAN NASH-SUTCLIFFE EFFIECIENCY INDEX OF DAILY FLOWS
    !* FBEST:   SAE AT PREVIOUS TIME STEP TRIAL
    !* FTEST:   SAE AT CURRENT TIME STEP TRIAL
    !* QOBS:    OBSERVED DAILY STREAM FLOW
    !* QSIM:    SIMULATED DAILY STREAM FLOW

    integer ncal, ns
    real fbest, ftest
    real, dimension(:, :), allocatable :: qobs, qsim

    !> STATISTICS FOR MONTE CARLO SIMULATION
    real, dimension(:), allocatable :: bias, nsd, lnsd, nsw, tpd, tpw

    type(model_output_drms) :: st_drms
    type(model_output_abserr) :: st_abserr

    contains

    !>
    !> January 25, 2012 - M.A. Mekonnen
    !>
    !> The function computes model efficiency coefficients.
    !>
    !>  OBS     - Observed values
    !>  SIM     - Simulated values
    !>  N       - Number of days
    !>  NS      - Number of stations
    !>  NMIN    - Minimum number of days for model spin-up
    !>  SAE     - Absolute value error
    !>  MAE     - Mean absolute value error
    !>  RMSE    - Root mean squared error
    !>  BIAS    -
    !>  NSD     - Nash-Sutcliffe coefficient
    !>  LNSD    - Nash-Sutcliffe coefficient (with the natural logarithm of runoff)
    !>
    subroutine calc_stats(obs, sim, n, bias, nsd, lnsd, nsw, tpd)

        !> Input variables.
        real, intent(in), dimension(:) :: obs, sim
        integer n

        !> Output variables.
        real bias, nsd, lnsd, nsw, tpd, tpw

        !> Local variables.
        !* ilf: Number of days left out in the calculation of metrics (METRICSSPINUP).
        integer ipo(1), ips(1), nad, naw, nw, iw, ilf, j, i
        real lerrd(n), lerrdm(n), errd(n), errdm(n), errtp, obsdm, obswm, lobsdm, ltol
        real, dimension(:), allocatable :: obsw, simw, errw, errwm

        !> Intrinsic functions.
        intrinsic ceiling, maxloc, sum, count, min, max, log, abs

        !> Tolerance for low-flow values.
        ltol = 0.0001

        !> Determine the number of weeks in the simulation.
        nw = ceiling(n / 7.0)
        allocate(obsw(nw), simw(nw), errw(nw), errwm(nw))

        !> Zero output and interim calculation variables.
        bias = 0.0
        nsd = 0.0
        lnsd = 0.0
        nsw = 0.0
        tpd = 0.0
        tpw  = 0.0
        obsdm = 0.0
        obsw = 0.0
        simw = 0.0
        obswm = 0.0
        errd = 0.0
        errdm = 0.0
        errw  = 0.0
        errwm = 0.0
        errtp = 0
        lerrd = 0.0
        lerrdm = 0.0

        !> Apply the spin-up period (METRICSSPINUP).
        if (METRICSINCLUDESPINUP == 1) then
            ilf = 1
        else
            ilf = METRICSSPINUP
        end if

        !> Return if METRICSSPINUP exceeds the length of the run.
        if (METRICSSPINUP > ncal) return

        !> Calculate the weekly values for daily observed and simulated values.
        iw = 0
        do i = ilf, ncal, 7
            iw = iw + 1
            j = min(i + 6, n)
            obsw(iw) = sum(obs(i:j))
            simw(iw) = sum(sim(i:j))
        end do

        !> Calculate the mean of the observed values.
        nad = count(obs(ilf:ncal) >= 0.0)
        if (nad == 0) return
        obsdm = sum(obs(ilf:ncal), mask = obs(ilf:ncal) >= 0.0)/nad
        lobsdm = sum(log(obs(ilf:ncal) + ltol), mask = obs(ilf:ncal) >= 0.0)/nad

        !> Calculate the mean of the weekly values.
        naw = count(obsw(1:nw) >= 0.0)
        if (naw > 0) obswm = sum(obsw(1:nw), mask = obsw(1:nw) >= 0.0)/naw

        !> Calculate the error terms for observed values greater than zero.
        where(obs(ilf:ncal) >= 0.0)
            errd(ilf:ncal) = obs(ilf:ncal) - sim(ilf:ncal)
            errdm(ilf:ncal) = obs(ilf:ncal) - obsdm
            lerrd(ilf:ncal) = log(obs(ilf:ncal) + ltol) - log(sim(ilf:ncal) + ltol)
            lerrdm(ilf:ncal) = log(obs(ilf:ncal) + ltol) - lobsdm
        end where

        !> Calculate the errors for the weekly values.
        where(obsw(1:nw) >= 0.0)
            errw(1:nw) = obsw(1:nw) - simw(1:nw)
            errwm(1:nw) = obsw(1:nw) - obswm
        end where

        !> Calculate the statistical coefficients.
        if (obsdm /= 0) bias = sum(errd(ilf:ncal))/(obsdm*nad)
        if (sum(errdm) /= 0) nsd = 1.0 - sum(errd**2)/sum(errdm**2)
        if (sum(lerrdm) /= 0) lnsd = 1.0 - sum(lerrd**2)/sum(lerrdm**2)
        if (sum(errwm) /= 0) nsw = 1.0 - sum(errw**2)/sum(errwm**2)

        !> Calculate the time to peak value.
        errtp = 0.0
        do i = ilf, ncal, 365
            j = min(i + 364, n)
            if(obs(max(i, j - 182)) > 0.0) then
                ipo = maxloc(obs(i:j))
                ips = maxloc(sim(i:j))
                errtp = ipo(1) - ips(1)
            else
                errtp = 0.0
            end if
            tpd = tpd + abs(errtp)
        end do

        !> Calculate the time to peak for the weekly values.
        errtp = 0.0
        do i = 1, nw, 52
            j = min(i + 51,nw)
            if (obsw(max(i, j - 25)) > 0.0) then
                ipo = maxloc(obsw(i:j))
                ips = maxloc(simw(i:j))
                errtp = ipo(1) - ips(1)
            else
                errtp = 0.0
            end if
            tpw = tpw + abs(errtp)
        end do

    end subroutine

    !>
    !> Description:
    !>
    subroutine stats_init(fls)

        use sa_mesh_common
        use model_files_variables

    !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        logical exists
        integer iun, ierr

        if (fms%stmg%n == 0) mtsflg%AUTOCALIBRATIONFLAG = 0

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        if (mtsflg%PREEMPTIONFLAG == 1) then
            call print_message('=================================================')
            call print_message('')
            call print_message('     SA_MESH IS RUNNING IN AUTOCALIBRATION MODE')
            call print_message('                USING PRE-EMPTION')
            call print_message('')
            call print_message('=================================================')
            call print_message('')
        end if

!todo: split into stats_state_resume().
        if (mtsflg%PREEMPTIONFLAG >= 1) then
            fbest = +1.0e+10
            inquire(file = trim(adjustl(mtsfl%fl(mtsk%PE)%fn)), exist = exists)
            if (exists) then
                iun = mtsfl%fl(mtsk%PE)%iun
                open(iun, file = trim(adjustl(mtsfl%fl(mtsk%PE)%fn)), status = 'old', action = 'read', iostat = ierr)
                if (ierr == 0) read(iun, *) fbest
                close(iun)
            end if
        end if

        ncal = 0
        ns = fms%stmg%n

        allocate(qobs(leap_year(ic%now%year), ns), qsim(leap_year(ic%now%year), ns))
        allocate(bias(ns), nsd(ns), lnsd(ns), nsw(ns), tpd(ns), tpw(ns))

        qobs = 0.0
        qsim = 0.0

!-        if (RESUMEFLAG == 4) then
!-            call stats_state_resume(fls)
!-        end if

    end subroutine

    !>
    !> Description:
    !>
    subroutine stats_update_stfl_daily(fls)

        use sa_mesh_common
        use model_files_variables

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        real, dimension(:, :), allocatable :: tmp

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        !> Increment number of simulated days run.
        ncal = ncal + 1

        !> Expand array 
        if (ncal > size(qobs, 1)) then

            !> Allocate temporary array.
            allocate(tmp(size(qobs, 1) + leap_year(ic%now%year), ns))

            !> Copy and expand 'qobs'.
            tmp = 0.0
            tmp(1:size(qobs, 1), :) = qobs
            deallocate(qobs)
            allocate(qobs(size(tmp, 1), ns))
            qobs = tmp

            !> Copy and expand 'qsim'.
            tmp = 0.0
            tmp(1:size(qsim, 1), :) = qsim
            deallocate(qsim)
            allocate(qsim(size(tmp, 1), ns))
            qsim = tmp

            !> Deallocate temporary array.
            deallocate(tmp)
        end if

        !> Copy measured and simulated values to local arrays.
        qobs(ncal, :) = fms%stmg%qomeas%val
        qsim(ncal, :) = out%d%grid%qo(fms%stmg%meta%rnk(:))

        if (mtsflg%PREEMPTIONFLAG == 1) then
            if (OBJFNFLAG == 0) then
                ftest = sae_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), mtsflg%AUTOCALIBRATIONFLAG)
            elseif (OBJFNFLAG == 1) then
                ftest = saesrt_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), mtsflg%AUTOCALIBRATIONFLAG)
            elseif (OBJFNFLAG == 2) then
                call print_warning('SAEMSRT (OBJFNFLAG 2) does not support pre-emption.')
            elseif (OBJFNFLAG == 3) then
                call print_error( &
                    'NSE (OBJFNFLAG 3) does not support pre-emption. Disable pre-emption or use NegNSE (OBJFNFLAG 4) instead.')
                call program_abort()
            elseif (OBJFNFLAG == 4) then
                ftest = nse_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), mtsflg%AUTOCALIBRATIONFLAG)
                ftest = -1.0 * ftest
            end if
        end if

!        if (mtsflg%AUTOCALIBRATIONFLAG >= 1 .and. mtsflg%PREEMPTIONFLAG == 1) then
!            if (FTEST > FBEST) goto 199
!        end if

    end subroutine

    subroutine stats_state_save(fls)

        use sa_mesh_common
        use model_files_variables

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        integer j, iun, ierr

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        !> Open the file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.simstats', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Write the values of the variables to file.
        write(iun) int(ncal, kind = 4)
        write(iun) int(ns, kind = 4)
        write(iun) real(fbest, kind = 4), real(ftest, kind = 4)
        do j = 1, ns
            write(iun) real(qobs(1:ncal, j), kind = 4)
            write(iun) real(qsim(1:ncal, j), kind = 4)
        end do

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    subroutine stats_state_resume(fls)

        use sa_mesh_common
        use model_files_variables

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        integer(kind = 4) ncal_i4, ns_i4
        real(kind = 4) fbest_r4, ftest_r4
        real(kind = 4), dimension(:, :), allocatable :: qobs_r4, qsim_r4
        integer j, iun, ierr

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        !> Open the file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.simstats', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Read the values of the variables from file.
        read(iun) ncal_i4
        ncal = int(ncal_i4, kind(ncal))
        read(iun) ns_i4
        ns = int(ns_i4, kind(ns))
        read(iun) fbest_r4, ftest_r4
        fbest = real(fbest_r4, kind(fbest))
        ftest = real(ftest_r4, kind(ftest))
        if (allocated(qobs)) deallocate(qobs, qsim)
        allocate(qobs_r4(ncal, ns), qobs(ncal, ns), qsim_r4(ncal, ns), qsim(ncal, ns))
        do j = 1, ns
            read(iun) qobs_r4(1:ncal, j)
            read(iun) qsim_r4(1:ncal, j)
        end do
        qobs = real(qobs_r4, kind(qobs))
        qsim = real(qsim_r4, kind(qsim))
        deallocate(qobs_r4, qsim_r4)

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    !>
    !> Description: Write the metrics of the simulation to file.
    !>
    subroutine stats_write(fls)

        use sa_mesh_common
        use model_files_variables

        !> External functions.
        real, external :: KGE

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        logical exists
        real, dimension(:), allocatable :: fkge
        integer j, iun

!-        if (SAVERESUMEFLAG == 4) then
!-            call stats_state_save(fls)
!-            return
!-        end if

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active, or the number of calibration points is zero.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0 .or. ncal == 0) return

        !> Check if the array to keep file information for the metrics is allocated.
        if (.not. allocated(mtsfl%fl)) call init_metricsout_files()

        !> Write the output function for pre-emption.
        if (mtsflg%PREEMPTIONFLAG == 1 .and. mtsfl%fl(mtsk%fo)%init) then
            iun = mtsfl%fl(mtsk%fo)%iun
            open(iun, file = trim(adjustl(mtsfl%fl(mtsk%fo)%fn)))
            if (mtsflg%PREEMPTIONFLAG >= 1) ftest = ftest*ic%iter%jday/ncal
            write(iun, "(9999(g15.7e2, ' '))") ftest
            close(iun)
        end if

        !> Return if METRICSSPINUP exceeds the length of the run.
        if (METRICSSPINUP > ncal) return

        !> Calculate the metrics of the simulation.
        allocate(fkge(size(qobs, 2)))
        fkge = 0.0
        do j = 1, size(qobs, 2)
            call calc_stats(qobs(1:ncal, j), qsim(1:ncal, j), ncal, bias(j), nsd(j), lnsd(j), nsw(j), tpd(j))
            fkge(j) = KGE(qobs(max(METRICSSPINUP,1):ncal, j), qsim(max(METRICSSPINUP,1):ncal, j), (ncal - max(METRICSSPINUP,1)) + 1)
        end do
        if (mtsfl%fl(mtsk%out)%init .or. mtsfl%fl(mtsk%RMSE)%init) st_drms = calc_drms_value(METRICSSPINUP, ncal, qobs, qsim)
        if (mtsfl%fl(mtsk%out)%init .or. mtsfl%fl(mtsk%RMSE)%init) st_abserr = calc_abserr_value(METRICSSPINUP, ncal, qobs, qsim)

        !> Results for Monte-Carlo style analysis are appended to results in an existing file.
        if (mtsfl%fl(mtsk%MC)%init) then
            inquire(file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%MC)%fn), exist = exists)
            iun = mtsfl%fl(mtsk%MC)%iun
            if (exists) then
                open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%MC)%fn), position = 'append', status = 'old')
            else
                open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%MC)%fn))
                write(iun, "(9999(g15.7e2, ' '))") "BIAS ", "NSD ", "NSW ", "TPD "
            end if
            write(iun, "(9999(g15.7e2, ' '))") (bias(j), nsd(j), nsw(j), int(tpd(j)), j = 1, size(qobs, 2))
            close(iun)
        end if

        !> Write Nash-Sutcliffe coefficient of daily streamflow values.
!todo: there's probably a better way to store a set of multiple statistics in one file.
        if (mtsfl%fl(mtsk%NSE)%init) then
            iun = mtsfl%fl(mtsk%NSE)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%NSE)%fn))
            write(iun, "(9999(g15.7e2, ' '))") (nsd(j), j = 1, size(qobs, 2)), sum(nsd)/size(qobs, 2)
            close(iun)
        end if

        !> Write Nash-Sutcliffe coefficient of weekly streamflow values.
        if (mtsfl%fl(mtsk%NSW)%init) then
            iun = mtsfl%fl(mtsk%NSW)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%NSW)%fn))
            write(iun, "(9999(g15.7e2, ' '))") (nsw(j), j = 1, size(qobs, 2))
            close(iun)
        end if

        !> Write daily root mean squared error.
        if (mtsfl%fl(mtsk%RMSE)%init) then
            iun = mtsfl%fl(mtsk%RMSE)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%RMSE)%fn))
            write(iun, "(9999(g15.7e2, ' '))") st_drms%value_gauge, st_drms%value_gauge_avg
            close(iun)
        end if

        !> Write mean absolute error.
        if (mtsfl%fl(mtsk%ABSE)%init) then
            iun = mtsfl%fl(mtsk%ABSE)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%ABSE)%fn))
            write(iun, "(9999(g15.7e2, ' '))") st_abserr%value_gauge, st_abserr%value_gauge_avg
            close(iun)
        end if

        !> Write the summary of the metrics to file.
        if (mtsfl%fl(mtsk%out)%init) then
            iun = mtsfl%fl(mtsk%out)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%out)%fn))
            write(iun, "(9999(g15.7e2, ' '))") &
                "Gauge", "MAE", "RMSE", "BIAS", "AbsBIAS", "NSD", "NegNSD", "lnNSD", "NeglnNSD", "TPD", "KGE", "NegKGE"
            do j = 1, size(qobs, 2)
                write(iun, "(9999(g15.7e2, ' '))") &
                    j, st_abserr%value_gauge(j), st_drms%value_gauge(j), bias(j), abs(bias(j)), &
                    nsd(j), (-1.0*nsd(j)), lnsd(j), (-1.0*lnsd(j)), int(tpd(j)), fkge(j), (-1.0*fkge(j))
            end do
            close(iun)
        end if

    end subroutine

end module
