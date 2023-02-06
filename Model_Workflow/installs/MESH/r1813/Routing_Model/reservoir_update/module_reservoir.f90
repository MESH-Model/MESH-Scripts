module reservoir

!-    use model_dates

    implicit none

    type reservoir_f
        integer :: id           ! Reservoir ID
        integer :: rank         ! Rank value to identify the grid cell as a reservoir
        integer :: modeltype    ! Model type (reservoir storage zone release)
        real :: lat             ! Lat location of the reservoir (decimal degrees)
        real :: long            ! Lon location of the reservoir (decimal degrees)
        real :: SMAX            ! Max reservoir capacity (m3)
        real :: flowO1          ! Initial discharge (m3 s-1)
        real :: Intstor1        ! Initial storage (m3)
        real :: qmaxmax         ! Downstream channel capacity
        real :: inflowcorr      ! Inflow correction factor for evaporation
        real :: deadst          ! Dead storage fraction of maximum storage ( 0.1 means 10% of max storage)
        real :: RXN             ! Random term variance or range
        real :: dsto(12)        ! Monthly min storage 12 values one per month (m3)
        real :: nsto(12)        ! Monthly normal upper storage (m3)
        real :: ndsto(12)       ! Monthly upper storage (m3)
        real :: Qmin(12)        ! Monthly min release (m3 s-1)
        real :: Qnor(12)        ! Monthly normal upper release (m3 s-1)
        real :: Qnd(12)         ! Monthly upper release (m3 s-1)
        real :: stoSIM(2)       ! storage time series of the reservoir
        real :: flowSIM(2)      ! storage time series of the reservoir
        real :: flowINF(2)      ! inflow time series of the reservoir
    end type

    type reservoirs
        integer :: nreserv      ! number of the reservoirs
        type(reservoir_f), dimension(:), allocatable :: rsvr
    end type reservoirs

    type(reservoirs), save :: resrvs

    contains

    subroutine init_reservoirs(flIn)

        !> Input variables.
        character(len = *), intent(in) :: flIn

        !> Local variables.
        integer iun, i, j

        !> Use a dummy unit.
        iun = 100

        !> Open the file.
        open(100, file = flIn, status = 'old', action = 'read')

        !> Read number of reservoirs and allocate the reservoir object to the number of reservoirs.
        read(100, *) resrvs%nreserv
        allocate(resrvs%rsvr(resrvs%nreserv))

        !> Read data.
        do i = 1, resrvs%nreserv

            !> Read attributes.
            read(100, *) resrvs%rsvr(i)%id
            read(100, *) resrvs%rsvr(i)%rank
            read(100, *) resrvs%rsvr(i)%modeltype
            read(100, *) resrvs%rsvr(i)%lat, resrvs%rsvr(i)%long
            read(100, *) resrvs%rsvr(i)%SMAX
            read(100, *) resrvs%rsvr(i)%flowO1
            read(100, *) resrvs%rsvr(i)%Intstor1
            read(100, *) resrvs%rsvr(i)%qmaxmax
            read(100, *) resrvs%rsvr(i)%inflowcorr
            read(100, *) resrvs%rsvr(i)%deadst
            read(100, *) resrvs%rsvr(i)%RXN
            read(100, *) (resrvs%rsvr(i)%dsto(j), j = 1, 12)
            read(100, *) (resrvs%rsvr(i)%nsto(j), j = 1, 12)
            read(100, *) (resrvs%rsvr(i)%ndsto(j), j = 1, 12)
            read(100, *) (resrvs%rsvr(i)%Qmin(j), j = 1, 12)
            read(100, *) (resrvs%rsvr(i)%Qnor(j), j = 1, 12)
            read(100, *) (resrvs%rsvr(i)%Qnd(j), j = 1, 12)

            !> Set initial values.
            resrvs%rsvr(i)%stoSIM(1)  = resrvs%rsvr(i)%Intstor1
            resrvs%rsvr(i)%flowSIM(1) = resrvs%rsvr(i)%flowO1
            resrvs%rsvr(i)%flowINF(1) = resrvs%rsvr(i)%flowO1

        end do

        !> Close the file to free the unit.
        close(100)

    end subroutine init_reservoirs

    subroutine get_reservoir(rank, irsv)

        !> Input variables.
        integer, intent(in) :: rank
        
        !> Output variables.
        integer, intent(out) :: irsv

        !> Local variables.
        integer i

        !> Identify reservoir using RANK.
        do i = 1, resrvs%nreserv
            if (rank == resrvs%rsvr(i)%rank) then
                irsv = i
                exit
            end if
        end do

    end subroutine get_reservoir

    real function S2Q1JAN(Fu, Ld, Lc, Ln, Lf, qmin, qnorm, qnd, dt, smax, Inflow, qds, cse, Rx) result(qout)

        !> Input variables.
        real, intent(in) :: Fu, Ld, Lc, Ln, Lf, qmin, qnorm, qnd, dt, smax, Inflow, qds, Rx
        integer, intent(in) :: cse

        !> Initialize the output variables.
        qout = 0.0

        !> Determine release at current time-step.
        if (Fu <= Ld) then
            qout = 0.0
        else if (Fu > Ld .and. Fu <= Lc) then
            qout = min(qmin,((Fu - Ld)*smax/(dt))) + Rx
        else if (Fu > Lc .and. Fu <= Ln) then
            qout = qmin + (qnorm - qmin)*((Fu - Lc)/(Ln - Lc)) + Rx
        else if (Fu > Ln .and. Fu <= Lf) then
            if (cse == 1) then
                qout = qnorm + ((Fu - Ln)/(Lf - Ln))*(qnd - qnorm) + Rx
            else
                qout = qnorm + ((Fu - Ln)/(Lf - Ln))*max((Inflow - qnorm), (qnd - qnorm)) + Rx
            end if
        else
            qout = min(max(((Fu - Lf)*smax/dt), qnd) + Rx, qds)
        end if

        !> Check for negative value.
        qout = max(qout, 0.0)

    end function

    subroutine SIMRES15(icor, cse, Ld, flowI, smax, qds, Lc, Ln, Lf, qmin, qnorm, qnd, Rx, t, niter, dt, flowSIM, stoSIM)

        !> Input variables.
        real, intent(in) :: icor                ! = resrv%inflowcorr
        integer, intent(in) :: cse              ! = resrv%modeltype             ! if cse=1 inflow is not used in zone 3, else it is used 
        real, intent(in) :: Ld                  ! = resrv%deadst
        real, dimension(:), intent(in) :: flowI ! = resrv%flowINF(1:2)          ! observed u/s inflow
        real, intent(in) :: smax                ! = resrv%SMAX                  ! maximum reservoir storage (m3)
        real, intent(in) :: qds                 ! = resrv%qmaxmax               ! downstream channel capacity (m3/s)
        real, intent(in) :: Lc                  ! = resrv%dsto(mId)/resrv%SMAX  ! monthly lower storage threshold
        real, intent(in) :: Ln                  ! = resrv%nsto(mId)/resrv%SMAX  ! monthly upper storage threshold
        real, intent(in) :: Lf                  ! = resrv%ndsto(mId)/resrv%SMAX ! inflow starts affecting release
        real, intent(in) :: qmin                ! = resrv%Qmin(mId)
        real, intent(in) :: qnorm               ! = resrv%Qnor(mId)
        real, intent(in) :: qnd                 ! = resrv%Qnd(mId)
        real, intent(in) :: Rx                  ! = 2*resrv%RXN*rnd + resrv%RXN ! Rx not inclued in the Matlab code, but added based on MESH code
        integer, intent(in) :: t                ! = t                           ! current time-step
        integer, intent(in) :: niter            ! = 40                          ! it was 1000 in the Matlab code but 40 in the MESH code
        real, intent(in) :: dt                  ! = dt                          ! time-step length/duration

        !> Local variables.
        real stoflo, Inflow, Fu
        integer it

        !> Output variables.
        real, dimension(:), intent(out) :: flowSIM  ! = resrv%flowSIM(1:2)
        real, dimension(:), intent(out) :: stoSIM   ! = resrv%stoSIM(1:2)

        !> Determine release and storage at current time-step.
        stoflo = stoSIM(t - 1) + (dt/2.0)*(flowI(t)*icor + flowI(t - 1)*icor - flowSIM(t - 1))
        Inflow = flowI(t)
        Fu = stoflo/smax
        flowSIM(t) = S2Q1JAN(Fu, Ld, Lc, Ln, Lf, qmin, qnorm, qnd, dt, smax, Inflow, qds, cse, Rx)
        do it = 1, niter
            stoSIM(t)  = stoflo - (dt/2.0)*(flowSIM(t))
            Fu = stoSIM(t)/smax
            flowSIM(t) = S2Q1JAN(Fu, Ld, Lc, Ln, Lf, qmin, qnorm, qnd, dt, smax, Inflow, qds, cse, Rx)
        end do
        if (stoSIM(t) > smax) then
            flowSIM(t) = flowSIM(t) + ((stoSIM(t) - smax)/dt)
            stoSIM(t) = smax
        end if

    end subroutine

    subroutine compute_reservoir(resrv, flowIn, t, dt, mId)

        !> Input variables.
        real, intent(in) :: flowIn  ! flow from downstream grid cell from routing
        integer, intent(in) :: mId  ! month to idenfy monthly variable parameters
        integer, intent(in) :: t    ! current time step
        real, intent(in) :: dt      ! time-step length/duration

        !> Input/output variables.
        type(reservoir_f) :: resrv  ! reservoir object

        !> Local variables.
!-        integer            :: irsv, MT, i
!-        real               :: FU, LD, LC, LN, LF, RX, ICORR
        real rnd

        !> Get random number.
        call random_number(rnd)         ! rnd sould be between 0.0 and 1.0

        !> Water Balance computation S_t-S_t-1 = I - O.
!-        ICORR = resrv%inflowcorr
!-        resrv%flowINF(2) = flowIn
!-        resrv%stoSIM(t) = resrv%stoSIM(t-1) + &
!-                                      (DT/2)*(resrv%flowINF(2)*ICORR + resrv%flowINF(1)*ICORR - &
!-                                      resrv%flowSIM(t-1))
!-        FU = resrv%stoSIM(t)/resrv%SMAX
!-        LC = resrv%dsto(mId)/resrv%SMAX
!-        LN = resrv%nsto(mId)/resrv%SMAX
!-        LF = resrv%ndsto(mId)/resrv%SMAX
!-        MT = resrv%modeltype
!-        LD = resrv%deadst
!-        RX = 2*resrv%RXN*rnd + resrv%RXN

        !> Determine release and storage at current time step.
!-        if (FU <= LD) then
!-            resrv%flowSIM(t) = 0.0
!-        else if (FU > LD .and. FU <= LC) then
!-            resrv%flowSIM(t) = max((min(resrv%Qmin(mId),((FU-LD)*resrv%SMAX/DT))+RX),0.0)
!-        else if (FU > LC .and. FU <= LN) then
!-            resrv%flowSIM(t) = max((resrv%Qmin(mId) + &
!-                (resrv%Qnor(mId) - resrv%Qmin(mId))*((FU-LC)/(LN-LC)) + RX),0.0)
!-        else if (FU > LN .and. FU <= LF) then
!-            if (MT==1) then
!-                resrv%flowSIM(t) = max((resrv%Qnor(mId) + &
!-                                            ((FU-LN)/(LF-LN))*(resrv%Qnd(mId)-resrv%Qnor(mId)) + RX),0.0)
!-            else
!-           !     resrv%flowSIM(t) = max(min(((resrv%Qnor(mId) + &
!-           !                               ((FU-LN)/(LF-LN))*max((resrv%flowINF(t)-resrv%Qnor(mId)), &
!-           !                                 (resrv%Qnd(mId)-resrv%Qnor(mId)))) + RX),resrv%qmaxmax),0.0)
!-                resrv%flowSIM(t) = max(((resrv%Qnor(mId) + &
!-                                            ((FU-LN)/(LF-LN))*max((resrv%flowINF(2)-resrv%Qnor(mId)), &
!-                                            (resrv%Qnd(mId)-resrv%Qnor(mId)))) + RX),0.0)
!-            end if
!-        else
!-            resrv%flowSIM(t) = max((min((max(((FU-LF)*resrv%SMAX/DT),resrv%Qnd(mId)) + RX),resrv%qmaxmax)),0.0)
!-        end if

        !> Update storage estimation and loop to reach stable release and storage.
!-        do i = 1, 40
!-            resrv%stoSIM(t) = resrv%stoSIM(t-1) + &
!-                                      (DT/2)*(resrv%flowINF(2)*ICORR + resrv%flowINF(1)*ICORR - &
!-                                      resrv%flowSIM(t-1) - resrv%flowSIM(t))
!-            FU = resrv%stoSIM(t)/resrv%SMAX
!-            if (FU <= LD) then
!-                resrv%flowSIM(t) = 0.0
!-            else if (FU > LD .and. FU <= LC) then
!-                resrv%flowSIM(t) = max((min(resrv%Qmin(mId),((FU-LD)*resrv%SMAX/DT))+RX),0.0)
!-            else if (FU > LC .and. FU <= LN) then
!-                resrv%flowSIM(t) = max((resrv%Qmin(mId) + &
!-                    (resrv%Qnor(mId) - resrv%Qmin(mId))*((FU-LC)/(LN-LC)) + RX),0.0)
!-            else if (FU > LN .and. FU <= LF) then
!-                if (MT==1) then
!-                    resrv%flowSIM(t) = max((resrv%Qnor(mId) + &
!-                                                ((FU-LN)/(LF-LN))*(resrv%Qnd(mId)-resrv%Qnor(mId)) + RX),0.0)
!-                else
!-               !     resrv%flowSIM(t) = max(min(((resrv%Qnor(mId) + &
!-               !                                 ((FU-LN)/(LF-LN))*max((resrv%flowINF(t)-resrv%Qnor(mId)), &
!-               !                                (resrv%Qnd(mId)-resrv%Qnor(mId)))) + RX),resrv%qmaxmax),0.0)
!-                    resrv%flowSIM(t) = max(((resrv%Qnor(mId) + &
!-                                                ((FU-LN)/(LF-LN))*max((resrv%flowINF(2)-resrv%Qnor(mId)), &
!-                                                (resrv%Qnd(mId)-resrv%Qnor(mId)))) + RX),0.0)
!-                end if
!-            else
!-                    resrv%flowSIM(t) = max((min((max(((FU-LF)*resrv%SMAX/DT),resrv%Qnd(mId)) + RX),resrv%qmaxmax)),0.0)
!-            end if
!-        end do
!-        if (resrv%stoSIM(t)>resrv%SMAX) then
!-            resrv%flowSIM(t) = resrv%flowSIM(t) + ((resrv%stoSIM(t) - resrv%SMAX)/DT)
!-        end if

        !> Update local variable for inflow.
        resrv%flowINF(t) = flowIn

        !> Call reservoir release routine.
        call SIMRES15( &

            !> Input variables.
            resrv%inflowcorr, &                 ! = icor
            resrv%modeltype, &                  ! = cse     ! if cse=1 inflow is not used in zone 3, else it is used 
            resrv%deadst, &                     ! = Ld
            resrv%flowINF(1:2), &               ! = flowI   ! observed u/s inflow
            resrv%SMAX, &                       ! = smax    ! maximum reservoir storage (m3)
            resrv%qmaxmax, &                    ! = qds     ! downstream channel capacity (m3/s)
            (resrv%dsto(mId)/resrv%SMAX), &     ! = Lc      ! monthly lower storage threshold
            (resrv%nsto(mId)/resrv%SMAX), &     ! = Ln      ! monthly upper storage threshold
            (resrv%ndsto(mId)/resrv%SMAX), &    ! = Lf      ! inflow starts affecting release
            resrv%Qmin(mId), &                  ! = qmin
            resrv%Qnor(mId), &                  ! = qnorm
            resrv%Qnd(mId), &                   ! = qnd
            (2.0*resrv%RXN*rnd + resrv%RXN), &  ! = Rx      ! Rx not inclued in the Matlab code, but added based on MESH code
            t, &                                ! = t       ! current time-step
            40, &                               ! = niter   ! it was 1000 in the Matlab code but 40 in the MESH code
            dt, &                               ! = dt      ! time-step length/duration

            !> Output variables.
            resrv%flowSIM(1:2), &   ! = flowSIM
            resrv%stoSIM(1:2))      ! = stoSIM

        !> Because a time-series is not preserved, assign the current value to the previous time-step in the local variables.
        resrv%stoSIM(t - 1)  = resrv%stoSIM(t)
        resrv%flowSIM(t - 1) = resrv%flowSIM(t)
        resrv%flowINF(t - 1) = resrv%flowINF(t)

    end subroutine compute_reservoir

end module reservoir
