module rte_module

    implicit none

    !> Input variables for dynamic time-stepping.
    !*  dtminusr: Maximum time-step for dynamic time-stepping [s].
    !*  mindtmin: Minimum time-step for dynamic time-stepping [s].
    !*  dtminfrac: Time-step reducing factor [--].
    !*  maxindex: Maximum number of interations allowed in for dynamic time-stepping [--].
    real(kind = 4) :: dtminusr = 1800.0, mindtmin = 10.0, dtminfrac = 0.75
    integer :: maxindex = 50

    !> Convergence threshold.
    !*  convthreshusr: Convergence threshold for channel routing.
    real(kind = 4) :: convthreshusr = 0.01_4

    !> Input parameters.
    !*  r1n: Manning coefficient for floodplain routing.
    !*  r2n: Manning coefficient for channel routing.
    !*  mndr: Meander factor.
    !*  aa2: Coefficient used in deriving channel geometry using drainage area.
    !*  aa3: Coefficient used in deriving channel geometry using drainage area.
    !*  aa4: Coefficient used in deriving channel geometry using drainage area.
    !*  widep: Channel width.
    type rte_params
        real, dimension(:), allocatable :: r1n, r2n, mndr, aa2, aa3, aa4, widep
    end type

    !> Instances of the input parameters.
    !*  rtepm: Grid-based (GRD, NA/NAA).
    !*  rtepm_iak: River class-based (IAK).
    type(rte_params), save :: rtepm, rtepm_iak

    !> Configuration flags.
    !*  RTE_TS: Routing time-step for exchange with MESH (independent of internal time-stepping) [s] (default: 3600).
    !*  cap_shd: Set to '1' to use the bankfull capacity read from file (default: '0').
    !*  dtminusr: Maximum time-step for dynamic time-stepping [s] (default: 1800.0).
    !*  mindtmin: Minimum time-step for dynamic time-stepping [s] (default: 10.0).
    !*  dtminfrac: Time-step reducing factor [--] (default: 0.75).
    !*  maxindex: Maximum number of interations allowed in for dynamic time-stepping [--] (default: 50).
    !*  convthreshusr: Convergence threshold for channel routing (default: 0.01).
    type rte_flags
        logical :: PROCESS_ACTIVE = .false.
        integer :: RTE_TS = 3600, cap_shd = 0
        real :: dtminusr = 1800.0, mindtmin = 10.0, dtminfrac = 0.75
        integer :: maxindex = 50
        real :: convthreshusr = 0.01
    end type

    !> Instance of control flags.
    type(rte_flags), save :: rteflg

    !> Option flags.
    type rte_options

    end type

    !> Instance of option flags.
    type(rte_options), save :: rteops

!temp: Override for diversions.
    real(kind = 4), dimension(:), allocatable, save :: qdiv2
!    character*12, dimension(4), save :: &
!        tdivname = ['06EC002', 'irrigat', '05LL019', '05QB005']
!    real*4, dimension(4), save :: &
!        txstrdiv = [-999.0000, -107.4900, -98.4045, -999.0000], &
!        tystrdiv = [-999.0000, 52.0640, 50.0134, -999.0000], &
!        txenddiv = [-98.9810, -999.9900, -98.3956, -91.3572], &
!        tyenddiv = [56.6906, -999.9900, 50.2219, 50.9251]
!    integer*4, dimension(4), save :: &
!        tval1div = [1.0, 0.5, 1.0, 1.0], &
!        tval2div = [3, 2, 1, 3], &
!        tval3div = [0, 11, 0, 0], &
!        tval4div = [0, 11, 0, 0]

!temp: Override for irrigation
    integer, dimension(:), allocatable :: totirrigpts

    contains

    !>
    !> _init() adapted from read_shed_fst_mod.f90.
    !>

    subroutine run_rte_init(fls, shd)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_variables: Variables, parameters, and types from SA_MESH.
        use model_files_variables
        use sa_mesh_common
!-        use FLAGS

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Local variables.
        integer n, l, ierr, iun

!temp: for overrides
!        integer, dimension(4) :: jstrdiv, istrdiv, jenddiv, ienddiv
!        integer, dimension(:), allocatable :: iminirr, imaxirr, jminirr, jmaxirr
!        integer irindex, irpt
!        character(len = 40) in_line
!        integer j, i

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. rteflg%PROCESS_ACTIVE) return

        !> Suppress diagnostic output throughout RTE.
        iopt = 0

        !> Transfer grid properties.
        ycount = shd%yCount
        imax = shd%yCount !> ycount and imax are used interchangeably in the code
        ydelta = real(shd%yDelta, kind(ydelta))
        yorigin = real(shd%yOrigin, kind(yorigin))
        xcount = shd%xCount
        jmax = shd%xCount !> jmax and xcount are used interghangeably in the code
        xdelta = real(shd%xDelta, kind(xdelta))
        xorigin = real(shd%xOrigin, kind(xorigin))
        al = real(shd%AL, kind(al))
        astep = real(shd%AL/1000.0, kind(astep))
        istep = int(astep)
        step2 = astep*astep
        na = shd%NA
        naa = shd%NAA
        if (shd%lc%NTYPE > 0) then
            ntype = shd%lc%NTYPE + 1
        else
            ntype = 2
        end if

        !> Allocate and transfer grid variables.
        allocate(xxx(na), yyy(na), s(ycount, xcount), &
                 next(na), da(na), bnkfll(na), slope(na), &
!                 elev(na)
                 rl(na), ibn(na), &
!                 sl1(na), sl2(na)
                 ichnl(na), ireach(na), &
                 grid_area(na), frac(na), aclass(na, ntype + 1), &
                 nhyd(ycount, xcount), &
                 glacier_flag(na))
        xxx = shd%xxx
        yyy = shd%yyy
!-        s = shd%RNKGRD
        s = 0
        do n = 1, na
            s(yyy(n), xxx(n)) = n
        end do
        next = shd%NEXT
        da = real(shd%DA, kind(da))
        bnkfll = real(shd%BNKFLL, kind(bnkfll))
        slope = sqrt(real(shd%SLOPE_CHNL, kind(slope)))
!+        elev = shd%ELEV
        rl = real(shd%CHNL_LEN, kind(rl))
        ibn = shd%IAK
!+        sl1 = shd%SLOPE_INT
        ichnl = shd%ICHNL
        ireach = shd%IREACH
        Nreaches = maxval(shd%IREACH)
        grid_area = real(shd%AREA, kind(grid_area))
        frac = real(shd%FRAC, kind(frac))
        aclass = 0.0
        if (allocated(shd%lc%ACLASS)) then
            aclass(:, 1:ntype) = real(shd%lc%ACLASS, kind(aclass))
        else if (ntype == 2) then
            aclass(:, 1) = 1.0
        end if
        nhyd = 0
        glacier_flag = 'n'

        !> Allocate and assign parameter values.
        allocate(r1n(na), r2n(na), rlake(na), &
                 mndr(na), aa2(na), aa3(na), aa4(na), theta(na), widep(na), kcond(na))
        r1n = real(rtepm%r1n, kind(r1n)); r2n = real(rtepm%r2n, kind(r2n))
        mndr = real(rtepm%mndr, kind(mndr)); aa2 = real(rtepm%aa2, kind(aa2)); aa3 = real(rtepm%aa3, kind(aa3))
        aa4 = real(rtepm%aa4, kind(aa4)); widep = real(rtepm%widep, kind(widep))
        rlake = 0.0; theta = 0.0; kcond = 0.0

        !> Force use of Manning's coefficients (e.g., r2n, r1n).
        manningflg = 'y'

        !> Adjust the calculated channel length by the degree of meandering.
        rl = rl*mndr

        !> Transfer time-stepping and convergence variables.
        dtminusr = real(rteflg%dtminusr, kind = kind(dtminusr))
        mindtmin = real(rteflg%mindtmin, kind = kind(mindtmin))
        dtminfrac = real(rteflg%dtminfrac, kind = kind(dtminfrac))
        maxindex = rteflg%maxindex
        convthreshusr = real(rteflg%convthreshusr, kind = kind(convthreshusr))

        !> Allocate many of the arrays used by the routing code. This block
        !> is deliberately skipping arrays that reference land use types,
        !> since wetland routing is currently not enabled,
        !> and most of the land use peculiarities are in WATFLOOD
        !> rather than this version of RPN-Watroute.
        allocate(qi1(na), qi2(na), qo1(na), qo2(na), qo2sim(na), &
                 qr(na), d2(na), qda(na), cap(na), over(na), &
                 qmax(na), res(na), sump(na), store1(na), &
                 store2(na), att(na), qbase(na), nreach(maxval(ireach)), &
                 totd1(na), totuzs(na), totsnw(na), qstream(na), &
                 totchnl(na), totgrid(na), netflow(na), storinit(na), &
                 sumrechrg(na), sumrff(na), rechrg(na), &
                 qdrng(na), qdrngfs(na), qstrm(na), sumq1(na), &
                 sumqint(na), sumq1fs(na), sumqintfs(na), strloss(na), &
                 qdrng2(na), qdrngfs2(na), wetwid(na), chawid(na), &
                 chadep(na), wstore1(na), wstore2(na), wcap(na), &
                 flowxa(na), chaxa(na), satxa(na), wetxa(na), &
                 hcha1(na), hcha2(na), hwet1(na), hwet2(na), qin(na), &
                 qswevp(na), qswrain(na), qiwet1(na), qiwet2(na), &
                 qowet1(na), qowet2(na), wetarea(na), chaarea(na), &
                 bin_precip(na), wsat(na), wetfrac(na), qo2rem(na), &
!tied to irr
                 qo2remirr(na))
        qi1 = 0.0; qi2 = 0.0; qo1 = 0.0; qo2 = 0.0; qo2sim = 0.0
        qr = 0.0; d2 = 0.0; qda = 0.0; cap = 0.0; over = 0.0
        qmax = 0.0; res = 0; sump = 0.0; store1 = 0.0
        store2 = 0.0; att = 0.0; qbase = 0.0; nreach = 0
        totd1 = 0.0; totuzs = 0.0; totsnw = 0.0; qstream = 0.0
        totchnl = 0.0; totgrid = 0.0; netflow = 0.0; storinit = 0.0
        sumrechrg = 0.0; sumrff = 0.0; rechrg = 0.0
        qdrng = 0.0; qdrngfs = 0.0; qstrm = 0.0; sumq1 = 0.0
        sumqint = 0.0; sumq1fs = 0.0; sumqintfs = 0.0; strloss = 0.0
        qdrng2 = 0.0; qdrngfs2 = 0.0; wetwid = 0.0; chawid = 0.0
        chadep = 0.0; wstore1 = 0.0; wstore2 = 0.0; wcap = 0.0
        flowxa = 0.0; chaxa = 0.0; satxa = 0.0; wetxa = 0.0
        hcha1 = 0.0; hcha2 = 0.0; hwet1 = 0.0; hwet2 = 0.0; qin = 0.0
        qswevp = 0.0; qswrain = 0.0; qiwet1 = 0.0; qiwet2 = 0.0
        qowet1 = 0.0; qowet2 = 0.0; wetarea = 0.0; chaarea = 0.0
        bin_precip = 0.0; wsat = 0.0; wetfrac = 0.0; qo2rem = 0.0
        qo2remirr = 0.0

        !> Allocate and assign other variables.
        allocate(store2_strt(naa))
        store2_strt = 0.0

        !> Calculate channel physical properties.
        do n = 1, naa

            !> rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
!+            sl2(n) = sqrt(sl1(n))

            !> Disable use of 'Excel compatible equation'.
            if (a4 == 0) a4 = 1.0

            !> CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLOW.
!            widep = a11

            !> Set 'chaxa' to the bankfull area, or else
            !> Compute the channel cross-sectional area based on a rather
            !> complicated fitting formula.  aa2/3/4 are tunable parameters.
            if (rteflg%cap_shd == 1) then
                chaxa(n) = bnkfll(n)
            else if (aa4(n) > 0.0) then
                chaxa(n) = (aa2(n) + aa3(n)*da(n)**aa4(n))
            else

                !> da(n) should never be less than zero, but it can
                !> happen if the rank listing is improperly configured.
                if (da(n) <= 0) then
                    chaxa(n) = 0

                    !> If xxx/yyy are both 0 for this cell, then we have a missing index. In theory,
                    !> this cell shouldn't affect the rest of the computation, so all we really want is for
                    !> the remaidner of this procedure to not die with a floating point exception.
                    if (xxx(n) == 0 .and. yyy(n) == 0) then
                        widep(n) = 1
                        chadep(n) = 1
                    end if

                !> rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
                !> EXCEL compatible equation. aa4 must be -ve in the par file
                else
                    chaxa(n) = 10.0_4**(aa2(n)*log10(da(n)) + aa3(n))
                end if

                !> had to put a lower bound on channel area to avoid NaN in resume file
                !> NK  Oct. 5/05
                chaxa(n) = max(1.0_4, chaxa(n))

            end if

            !> Channel capacity is the cross-sectional area times channel length.
            cap(n) = chaxa(n)*rl(n)

            !> Since a channel has a deep part plus a shallow, sloping bank,
            !> compute an effective channel depth.
            chadep(n) = sqrt(chaxa(n)/widep(n))
            chawid(n) = chaxa(n)/chadep(n)

            !> Fix suggested by Frank Seglenieks, based on changes made
            !> to WATFLOOD ca. 2007: if we keep track of biome types
            !> and a grid cell has more water fraction than channel
            !> area, then the channel area calculation must have been
            !> incorrect -- replace the computed channel area with
            !> water_fracion * grid size, and then from that recompute
            !> capacity.

            !> Define channel area.
            chaarea(n) = chawid(n)*rl(n)

            !> Check to see if that's sensible based on land-use.

            !> aclass(:,ntype) is the fraction of the grid cell that is water;
            !> this is only enforced at read-in of the shed/par files, and
            !> needs to be properly maintained.
            if (ntype >= 1 .and. aclass(n, ntype) > chaarea(n)/grid_area(n)) then

                !> Replace the areas with ones based on the land-use data.
                chaarea(n) = grid_area(n)*aclass(n, ntype)
                chawid(n) = chaarea(n)/rl(n) ! New width using the same effective depth
                cap(n) = chaarea(n)*chadep(n)

                !> Leave chaxa untouched for now, this may be a mistake.
! csubich -- experimental: update chaxa appropriately also
                chaxa(n) = cap(n)/rl(n) ! Capacity divided by length

            end if

        end do !n = 1, naa

        !> Identify the water class (not used).
!todo: water class for lakes.
        ii_water = ntype

        !> Reservoirs.
        !*  noresv: Number of reservoirs/lakes.
        !*  Nreaches: (rerout.f) Copy of noresv.
        noresv = fms%rsvr%n
        Nreaches = fms%rsvr%n
!todo: fix fhr use (999999).
        if (fms%rsvr%n > 0) then

            !> Book-keeping variables (when fhr > 1).
            !> Assigned but not used.
            !*  lake_inflow: Copy of reservoir inflow. [m3 s-1].
            !*  lake_stor: Copy of reservoir storage. [m3].
            !*  lake_outflow: Copy of reservoir outflow. [m3 s-1].
            !*  del_store: Storage change considering inflow minus outflow. [m3].
            allocate(lake_inflow(noresv, 999999), lake_stor(noresv, 999999), lake_outflow(noresv, 999999), del_stor(noresv, 999999))
            lake_inflow = 0.0; lake_stor = 0.0; lake_outflow = 0.0; del_stor = 0.0

            !> Reservoir/lake meta-data (from file).
            allocate( &
                resname(noresv), ires(noresv), jres(noresv), &
!tied to val2divyes == 1
                resindex(noresv))
!                yres(noresv), xres(noresv)
            resname = fms%rsvr%meta%name
            jres = fms%rsvr%meta%jx
            ires = fms%rsvr%meta%iy
            resindex = fms%rsvr%meta%rnk

            !> Routing coefficients.
            allocate(b1(noresv), b2(noresv), b3(noresv), b4(noresv), b5(noresv), b6(noresv), b7(noresv))
!                poliflg(noresv)
            b1 = real(fms%rsvr%rls%b1, kind(b1))
            b2 = real(fms%rsvr%rls%b2, kind(b2))
            b3 = real(fms%rsvr%rls%b3, kind(b3))
            b4 = real(fms%rsvr%rls%b4, kind(b4))
            b5 = real(fms%rsvr%rls%b5, kind(b5))
            b6 = real(fms%rsvr%rls%b6, kind(b6))
            b7 = real(fms%rsvr%rls%b7, kind(b7))
!            where (b3 == 0.0)
!                poliflg = 'n'
!            elsewhere
!                poliflg = 'y'
!            end where

            !> Reservoir routing time-stepping.
            ktr = fms%rsvr%rlsmeas%dts

            !> Measured outflow (from file).
            !*  qrel: Measured outflow when reservoir releases are replaced with measured values. [m3 s-1].
            !*  qdwpr: (Local variable in route.f) Used to accumulate flow in reaches that span multiple cells to the reservoir outlet. [m3 s-1].
            allocate(qdwpr(noresv, 999999), qrel(noresv, 999999))
            qdwpr = 0.0
            if (count(fms%rsvr%rls%b1 == 0.0) > 0 .and. fms%rsvr%rlsmeas%readmode /= 'n') then
                qrel(1:count(fms%rsvr%rls%b1 == 0.0), 1) = real(fms%rsvr%rlsmeas%val(1:count(fms%rsvr%rls%b1 == 0.0)), kind(qrel))
            else
                qrel(:, 1) = real(fms%rsvr%rlsmeas%val, kind(qrel))
            end if

            !> Initial lake level (from file).
            !*  reach_last: (Used in Great Lakes routing) Stores level of last time-step. [m].
            !*  lake_area: Used to convert reservoir storage to level (for diagnostic output). [m2].
            !*  lake_elv: Lake elevation (for diagnostic output). [m].
            allocate(reach_last(noresv), lake_area(noresv), lake_elv(noresv, 999999))
            lake_area = real(fms%rsvr%rls%area, kind(lake_area))
            lake_elv(:, 1) = real(fms%rsvr%rls%zlvl0, kind(lake_elv))
        end if

        !> Streamflow gauge locations.
        no = fms%stmg%n
        if (fms%stmg%n > 0) then
            allocate( &
                iflowgrid(no), nopt(no), &
!todo: fix this (999999).
                qhyd(no, 999999))
            iflowgrid = fms%stmg%meta%rnk
            nopt = -1
            qhyd(:, 1) = real(fms%stmg%qomeas%val, kind(qhyd))
        end if

        !> Channel and reservoir initialization.
        if ((fms%rsvr%n > 0 .or. fms%stmg%n > 0) .and. vs%flgs%resume%state == FLAG_OFF) then
            allocate(nbasin(ycount, xcount), r(na, ntype + 1), p(ycount,xcount), inbsnflg(no + noresv))
            nbasin = 0; p = 0.0; inbsnflg = 1
            if (fms%stmg%n > 0) then
                allocate(iy(no), jx(no), nlow(no), nxtbasin(no), area(no))
                iy = fms%stmg%meta%iy
                jx = fms%stmg%meta%jx
                nxtbasin = 0
            end if
            kt = 1
            call flowinit()
            if (fms%stmg%n > 0) deallocate(iy, jx, nlow, nxtbasin, area)
            deallocate(nbasin, r, p, inbsnflg)
        end if

!temp: Override for diversions.
!        nodiv = 4
!        allocate( &
!            val1div(nodiv), val2div(nodiv), val3div(nodiv), val4div(nodiv), &
!            divstrindex(nodiv), divendindex(nodiv), divname(nodiv), qdiv(nodiv, 999999), qdiv2(nodiv))
!        val1div = 0.0; val2div = 0; val3div = 0; val4div = 0
!        istrdiv = 0; jstrdiv = 0
!        ienddiv = 0; jenddiv = 0
!        divstrindex = 0; divendindex = 0; divname = ''; qdiv = 0.0
!        divname = tdivname
!        val1div = tval1div
!        val2div = tval2div
!        if (any(val2div > 0)) val2divyes = 1
!        val3div = tval3div
!        val4div = tval4div
!        jstrdiv = int((txstrdiv - xorigin)/xdelta) + 1
!        istrdiv = int((tystrdiv - yorigin)/ydelta) + 1
!        jenddiv = int((txenddiv - xorigin)/xdelta) + 1
!        ienddiv = int((tyenddiv - yorigin)/ydelta) + 1
!        divstrindex = -1 ! Points outside the watershed: reset the index value to -1.
!        divendindex = -1
!        do l = 1, nodiv
!            do n = 1, naa
!                if (xxx(n) == jstrdiv(l) .and. yyy(n) == istrdiv(l)) then
!                    divstrindex(l) = n
!                end if
!                if (xxx(n) == jenddiv(l) .and. yyy(n) == ienddiv(l)) then
!                    divendindex(l) = n
!                end if
!            end do
!            if ((val2div(l) == 1 .and. divstrindex(l) == -1) .or. (val2div(l) == 1 .and. divendindex(l) == -1)) then
!                print *, 'Error for station ', l, ': incompatibility between value2 and location of stations.'
!                print *, 'Check lat-lon of this station.'
!                print *, 'DIVR ', l, ' type ', val2div(l), ' START ', divstrindex(l), ' END', divendindex(l)
!                stop
!            end if
!            if ((val2div(l) == 2 .and. divstrindex(l) == -1) .or. (val2div(l) == 2 .and. divendindex(l) /= -1)) then
!                print *, 'Error for station ', l, ': incompatibility between value2 and location of stations.'
!                print *, 'Check lat-lon of this station.'
!                print *, 'DIVR ', l, ' type ', val2div(l), ' START ', divstrindex(l), ' END', divendindex(l)
!                stop
!            end if
!            if ((val2div(l) == 3 .and. divendindex(l) == -1) .or. (val2div(l) == 3 .and. divstrindex(l) /= -1)) then
!                print *, 'Error for station ', l, ': incompatibility between value2 and location of stations.'
!                print *, 'Check lat-lon of this station.'
!                print *, 'DIVR ', l, ' type ', val2div(l), ' START ', divstrindex(l), ' END', divendindex(l)
!                stop
!            end if
!        end do

!temp: Override for irrigation.
!        nodivirrig = 0                          ! The number of irrigation regions
!        maxirrigpts = 0                         ! The largest number of points in an individual irrigation region
!        do l = 1, nodiv                         ! Determine how many regions are to be irrigated
!            if (divname(l)(1:5) == 'irrig' .or. divname(l)(1:5) == 'Irrig') then
!                if (val2div(l) /= 2) then
!                    print *, 'Irrigation station must be type 2 diversion'
!                    print *, 'DIVR ', l, ' type ', val2div(l)
!                    stop
!                end if
!                nodivirrig = nodivirrig + 1         ! The maximum number of points to be irrigated
!                maxirrigpts = max(maxirrigpts, (val3div(l) + 1)*(val4div(l) + 1)) ! Add an extra point in each direction: needed if value3 or value4 is an even number
!            end if
!        end do
!        if (nodivirrig > 0) then
!            !   irrigindx: the indices (1-naa) of points to be irrigated
!            !   qdivirrig: the volume (m3/s) of water to be removed from the irrigated point in the simulation timestep
!            allocate( &
!                totirrigpts(nodivirrig), &
!                iminirr(nodivirrig), imaxirr(nodivirrig), &
!                jminirr(nodivirrig), jmaxirr(nodivirrig), &
!                irrigindx(nodivirrig, maxirrigpts), &
!                qdivirrig(nodivirrig, 999999))
!            irrigindx = -1
!            qdivirrig = 0
!
!            ! Determine the grid points in which water is to be lost due to irrigation
!            irindex = 0
!            do l = 1, nodiv
!                if (divname(l)(1:5) == 'irrig' .or. divname(l)(1:5) == 'Irrig') then
!                    irindex = irindex + 1
!                    iminirr(irindex) = max(istrdiv(l) - val4div(l)/2, 0)  ! Define the rectangle from the provided values
!                    imaxirr(irindex) = min(istrdiv(l) + val4div(l)/2, ycount)
!                    jminirr(irindex) = max(jstrdiv(l) - val3div(l)/2, 0)
!                    jmaxirr(irindex) = min(jstrdiv(l) + val3div(l)/2, xcount)
!                    irpt = 0
!                    do i = iminirr(irindex), imaxirr(irindex) ! Determine the number of points in this rectangle that are also in the watershed
!                        do j = jminirr(irindex), jmaxirr(irindex)
!                            do n = 1, naa
!                                if(xxx(n) == j .and. yyy(n) == i) then
!                                    irpt = irpt + 1
!                                    irrigindx(irindex, irpt) = n
!                                    exit
!                                end if
!                            end do
!                        end do
!                    end do
!                    totirrigpts(irindex) = irpt
!                end if
!            end do
!        end if
!        open(67, file = '20121201_div.tb0')
!        in_line = ''
!        do while (in_line /= ':EndHeader')
!            read(67, '(a)') in_line
!        end do
!        if (nodiv > 0) then
!            do i = 1, (30*24 + 13)
!                read(67, *) (qdiv(l, 1), l = 1, nodiv)
!            end do
!            backspace(67)
!            do l = 1, nodiv ! Loop through the diversion names searching for an irrigation area
!                if (qdiv(l, 1) < 0.0 .and. val2div(l) == 3) then
!                    print *, 'Diversion is type 3 (diversion starts outside of watershed).'
!                    print *, 'The diversion value cannot be negative for type 3.'
!                    print *, 'DIVR ', l, ' type ', val2div(l), ' qdiv ', qdiv(l, 1)
!                    stop
!                end if
!            end do
!        end if

        !> Update SA_MESH output variables.
        if (associated(out%ts%grid%qi)) out%ts%grid%qi = qi2
        if (associated(out%ts%grid%stgch)) out%ts%grid%stgch = store2
        if (associated(out%ts%grid%qo)) out%ts%grid%qo = qo2
        if (fms%rsvr%n > 0) then
            reach_last = lake_elv(:, 1)
        end if

        !> Update SA_MESH variables.
        !> Used by other processes and/or for resume file.
        vs%grid%qi = qi2
        vs%grid%stgch = store2
        vs%grid%qo = qo2

    end subroutine

    subroutine run_rte_resume_read(fls, shd)

        !> area_watflood: Shared variables used throughout rte code.
        !> mpi_module: Required for 'ipid'.
        !> sa_mesh_common: Variables, parameters, and types from SA_MESH.
        use area_watflood
        use mpi_module
        use model_files_variables
        use sa_mesh_common

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Local variables.
        integer(kind = 4) fhr_i4
        integer ierr, iun

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. rteflg%PROCESS_ACTIVE) return

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.rte', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Read inital values from the file.
        read(iun) fhr_i4
        fhr = int(fhr_i4)
        read(iun) qo2
        read(iun) store2
        read(iun) qi2
        if (fms%rsvr%n > 0) then
            read(iun) lake_elv(:, fhr)
        else
            read(iun)
        end if

        !> Close the file to free the unit.
        close(iun)

        !> Update SA_MESH output variables.
        if (associated(out%ts%grid%qi)) out%ts%grid%qi = qi2
        if (associated(out%ts%grid%stgch)) out%ts%grid%stgch = store2
        if (associated(out%ts%grid%qo)) out%ts%grid%qo = qo2
        if (fms%rsvr%n > 0) then
            reach_last = lake_elv(:, 1)
        end if

        !> Update SA_MESH variables.
        !> Used by other processes and/or for resume file.
        vs%grid%qi = qi2
        vs%grid%stgch = store2
        vs%grid%qo = qo2

    end subroutine

    subroutine run_rte_resume_read_nots(fls, shd)

        !> area_watflood: Shared variables used throughout rte code.
        !> mpi_module: Required for 'ipid'.
        !> sa_mesh_common: Variables, parameters, and types from SA_MESH.
        use area_watflood
        use mpi_module
        use model_files_variables
        use sa_mesh_common

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Local variables.
        integer ierr, iun

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. rteflg%PROCESS_ACTIVE) return

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.rte', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Read inital values from the file.
        read(iun)
        read(iun) qo2
        read(iun) store2
        read(iun) qi2
        if (fms%rsvr%n > 0) then
            read(iun) lake_elv(:, 1)
        else
            read(iun)
        end if

        !> Close the file to free the unit.
        close(iun)

        !> Update SA_MESH output variables.
        if (associated(out%ts%grid%qi)) out%ts%grid%qi = qi2
        if (associated(out%ts%grid%stgch)) out%ts%grid%stgch = store2
        if (associated(out%ts%grid%qo)) out%ts%grid%qo = qo2
        if (fms%rsvr%n > 0) then
            reach_last = lake_elv(:, 1)
        end if

        !> Update SA_MESH variables.
        !> Used by other processes and/or for resume file.
        vs%grid%qi = qi2
        vs%grid%stgch = store2
        vs%grid%qo = qo2

    end subroutine

    !>
    !> _between_grid() adapted from rte_sub.f.
    !>

    subroutine run_rte_between_grid(fls, shd)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_variables: Variables, parameters, and types from SA_MESH.
        use model_files_variables
        use sa_mesh_common

        !> model_dates: for 'ic' counter.
        use model_dates

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Local variables for dynamic time-stepping.
        real(kind = 4) qi2_strt(naa), qo2_strt(naa), route_dt, hr_div, sec_div, dtmin
        real(kind = 4) tqi1, tqo1, tax, tqo2, tstore2, tstore1
        integer indexi, no_dtold

        !> Local variables for output averaging.
        real, dimension(:), allocatable :: inline_qi, inline_stgch, inline_qo

        !> Local diagnostic variables.
!        integer year_last, month_last, day_last, hour_last
        integer :: exitstatus = 0

        !> Local variables.
        integer n, l
        integer irindex

        !> Local variables not used (passed to route.f).
        character(len = 14) :: date = ''

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. rteflg%PROCESS_ACTIVE) return

        !> Accumulate runoff to the routing time-step.
        if (ic%ts_hourly == 1) then
            qr(1:naa) = 0.0
        end if
        qr(1:naa) = qr(1:naa) + real(vs%grid%rff(1:naa)*shd%FRAC(1:naa), kind(qr))
        qr(1:naa) = qr(1:naa) + real(vs%grid%rchg(1:naa)*shd%FRAC(1:naa), kind(qr))

        !> Reset SA_MESH output variables (for averaging).
        !> Setting these to zero also prevents updating from the state variables upon return.
        if (associated(out%ts%grid%qi)) out%ts%grid%qi = 0.0
        if (associated(out%ts%grid%stgch)) out%ts%grid%stgch = 0.0
        if (associated(out%ts%grid%qo)) out%ts%grid%qo = 0.0

        !> Return if not the last time-step of the hour.
        if (ic%now%hour == ic%next%hour) return

        !> Increment counters.
        fhr = fhr + 1

        !> Diversion data.
        if (nodiv > 0) then
            read(67, *) (qdiv2(l), l = 1, nodiv)
            do l = 1, nodiv
                if (qdiv2(l) < 0.0 .and. val2div(l) == 3) then
                    print *, 'Diversion is type 3 (diversion starts outside of watershed).'
                    print *, 'The diversion value cannot be negative for type 3.'
                    print *, 'DIVR ', l, ' type ', val2div(l), ' qdiv ', qdiv2(l)
                    stop
                else if (qdiv2(l) < 0.0) then
                    qdiv2(l) = 0.0
                end if
                irindex = 0
                if (divname(l)(1:5) == 'irrig' .or. divname(l)(1:5) == 'Irrig') then
                    irindex = irindex + 1
                    qdivirrig(irindex, 1) = qdiv(l, 1)/real(totirrigpts(irindex), 4)
                end if
            end do
            qdiv(:, fhr) = qdiv2
        end if

        !> Date
!        year1 = ic%now%year
!        month_now = ic%now%month
!        day_now = ic%now%day
!        hour_now = ic%now%hour + 1

        !> Convert surface runoff from [kg/m^2] to [cms].
        qr(1:naa) = qr(1:naa)*1000.0_4*step2/3600.0_4

        !> Update from SA_MESH variables.
        qi2 = real(vs%grid%qi, kind(qi2))
        store2 = real(vs%grid%stgch, kind(store2))
        qo2 = real(vs%grid%qo, kind(qo2))

        !> Remember the input values from the start of the time step.
        qi2_strt(1:naa) = qi2(1:naa)
        qo2_strt(1:naa) = qo2(1:naa)
        store2_strt(1:naa) = store2(1:naa)
        if (fms%stmg%n > 0) qhyd(:, fhr) = real(fms%stmg%qomeas%val, kind(qhyd))
        if (fms%rsvr%n > 0) then
            if (count(fms%rsvr%rls%b1 == 0.0) > 0 .and. fms%rsvr%rlsmeas%readmode /= 'n') then
                qrel(1:count(fms%rsvr%rls%b1 == 0.0), 1) = real(fms%rsvr%rlsmeas%val(1:count(fms%rsvr%rls%b1 == 0.0)), kind(qrel))
            else
                qrel(:, 1) = real(fms%rsvr%rlsmeas%val, kind(qrel))
            end if
        end if

        !> If flow insertion, use simulated instead of flow inserted value at gauge location.
        if (trim(strfw_option) == 'streamflow_insertion') then
            do l = 1, no
                n = fms%stmg%meta%rnk(l)
                qo2_strt(n) = qo2sim(n)
            end do
        end if

        !> Calculate a dynamic dtmin by doing a quick and dirty routing
        !> This helps to ensure that the routing solutions inside the route function
        !> are able to converge to a solution
        !> We are assuming that the channels are not overflowing, so if it turns out that
        !> there is overbank flow/storage, then route may have to iterate a bit longer to
        !> find the solution even if dtmin is relatively small
!DD  Override the value calculated above by timer.f (fixed for all hours)
        dtmin = dtminusr  ! specified by the user and read in by rte.f
!        dtminfrac = 0.75
!        maxindex = 50   ! Permit more loops in this version of the routing loop since qo2 has no weighting applied

        do n = 1, naa

            tqo1 = qo2(n)

            !> If flow insertion, rely on simulated instead of possibly
            !> inserted flow value at gauge location to estimate required time-step.
            if (trim(strfw_option) == 'streamflow_insertion') then
                do l = 1, no
                    if (iflowgrid(l) == n) then
                        tqo1 = qo2sim(n)
                    end if
                end do
            end if
!            tqo1 = 0.0

            tqi1 = qi2(n)
15          indexi = 0
            if (dtmin <= mindtmin) exit
            no_dt = max(int(3599.0/dtmin) + 1, 1)
            route_dt = 3600.0_4/real(no_dt, 4)
            sec_div = route_dt/2.0_4
            tax = store1(n)/rl(n)
!            tqo2 = 0.0
            tqo2 = max(tax, 0.0_4)**1.67_4*slope(n)/chawid(n)**0.667_4/r2n(n)

            !> Use qi2 = 0.0 below to really constrain dtmin by keeping store2 low
            !> We don't want to set qi2 to zero though because it is used in route
            !> so we just use a hard-coded 0.0 in this equation
!16          tstore2 = store1(n)
16          tstore2 = store1(n) + (tqi1 + 0.0_4 - tqo1 - tqo2)*sec_div

            !> Now check to see if this qo2 is large enough that it will cause problems
            !> in the next time step when it is put into qo1.
            !> This has been known to happen when there is a sudden reduction in qi2 compared to qi1
!            tstore1 = tstore2
            tstore1 = tstore2 + (-tqo2)*sec_div

            !> If qo2 is so large that it's emptying the grid cell's storage in one time
            !> step, then we need to reduce the size of the time step to prevent that from
            !> happening. This is analogous to meeting the CFL condition in advection solvers.
            !> However, if store1 was very small, then small or even slightly negative store2
            !> might be a legitimate solution (i.e. the cell has actually dried up). So we'll let that go.
            !> Note that we also need to reduce the time step if we anticipate that qo1 will be too large
            !> in our next forecast/analysis time.
            if (tstore2 < 0.0 .or. tstore1 < 0.0) then

                !> Keep making qo2 smaller untill store2 becomes positive.
                tqo2 = tqo2/2.0_4
                indexi = indexi + 1

                !> Reduce the time step by a factor of dtminfrac (default = 0.75, set above).
                if (indexi > maxindex) then
                    dtmin = dtmin*dtminfrac
                    go to 15
                end if

                !> Redo the store2 calculation within the same iteration.
                go to 16
            end if

        end do !n = 1, naa

        !> Re-specifying dtmin as dtminusr cancels the effect of the code immediately above.
        !> However, if the iteration loop in route is unstable, dtmin still decreases.
        dtmin = dtminusr

        !> Allocate the local variables for output averaging.
        allocate(inline_qi(na), inline_stgch(na), inline_qo(na))

        !> Let the time step be as small as mindtmin.
17      dtmin = max(mindtmin, dtmin)
        no_dt = max(int(3599.0/dtmin) + 1, 1)
        dtmin = 3600.0_4/real(no_dt, 4)
        route_dt = 3600.0_4/real(no_dt, 4)
        sec_div = route_dt/2.0_4
        hr_div = sec_div/3600.0_4
        exitstatus = 0

        !> Initialize the local variables for output averaging.
        inline_qi = 0.0; inline_stgch = 0.0; inline_qo = 0.0

        !> Override the value declared above (fixed for all hours).
        a6 = dtmin

        !> The value of dtmin has been used to determine how many
        !> times route is called. Route will determine a new dtmin
        !> for the next time step.

        !> 'mo1' variable for hard-coded routing in rerout.f.
        mo1 = ic%now%month

        !> Prepare arrays for storing output (averaged over the routing time-step).
        if (.not. allocated(avr_qo)) allocate(avr_qo(na))
        avr_qo = 0.0

        !> ROUTE ROUTES WATER THROUGH EACH SQUARE USING STORAGE ROUTING.
        !> rev. 9.3.11  Feb.  17/07  - NK: force hourly time steps for runoff
        !> rev. 9.3.12  Feb.  20/07  - NK: changed dtmin & call to route
        do n = 1, no_dt

            call route(sec_div, hr_div, dtmin, mindtmin, convthreshusr, (ic%iter%hour + 1), n, real(ic%ts_count - 1, kind = 4), &
                       date, exitstatus)

            if (exitstatus /= 0) then
                if (dtmin <= mindtmin) then
                    write(*, '(a15, a43, f6.1)') 'route yields a ', 'negative store2 value at dtmin = mindtmin: ', mindtmin
                    write(*, '(a25, a50)') 'It''s likely that qo1 is ', 'so large that store2 is negative even with qo2=0.0'
                    write(*, '(a29, a50, a50)') &
                        'If this run was started from ', 'shed2flowinit utility, then try lowering the QI, ', &
                        'QO, and STORE ratios until this error is resolved'
                    write(*, *) 'Else rerun with a smaller value of dtmin'
                    stop
                else

                    !> Reduce the time step by a factor of dtminfrac (default = 0.75, set above).
                    dtmin = dtmin*dtminfrac
                    no_dtold = no_dt
                    no_dt = max(int(3599.0/dtmin) + 1, 1)

                    !> Reduce dtmin sufficiently to decrease no_dt.
                    do while (no_dt <= no_dtold)
                        dtmin = dtmin*dtminfrac
                        no_dtold = no_dt
                        no_dt = max(int(3599.0/dtmin) + 1, 1)
                    end do
                    dtmin = 3600.0_4/real(no_dt, 4)

                    !> Restore the input values from the start of the time step.
                    qi2(1:naa) = qi2_strt(1:naa)
                    qo2(1:naa) = qo2_strt(1:naa)
                    store2(1:naa) = store2_strt(1:naa)
                    go to 17

                end if
            end if

            !> Update the local variables for output averaging.
            if (associated(out%ts%grid%qi)) inline_qi = inline_qi + qi2
            if (associated(out%ts%grid%stgch)) inline_stgch = inline_stgch + store2
            if (associated(out%ts%grid%qo)) then
                inline_qo = inline_qo + qo2

                !> Override to preserve flow passed to the outlet when the outlet is outside the basin
                !>  (this condition is expected but not guaranteed).
                if (na > naa) inline_qo(na) = inline_qo(na) + qi2(na)
            end if

        end do !n = 1, no_dt

        !> Remember the last processed date/time in case need to output flow ICs after the time loop.
!        year_last = year1
!        month_last = month_now
!        day_last = day_now
!        hour_last = hour_now

        !> Update SA_MESH output variables.
        !> Output variables are updated at every model time-step; multiply averages
        !> by the number of model time-steps in the routing time-step
        if (associated(out%ts%grid%qi)) out%ts%grid%qi = inline_qi/no_dt*(3600/ic%dts)
        if (associated(out%ts%grid%stgch)) out%ts%grid%stgch = inline_stgch/no_dt*(3600/ic%dts)
        if (associated(out%ts%grid%qo)) out%ts%grid%qo = inline_qo/no_dt*(3600/ic%dts) !same as avr_qo

        !> Deallocate the local variables for output averaging.
        deallocate(inline_qi, inline_stgch, inline_qo)

        !> Update SA_MESH variables.
        !> Used by other processes and/or for resume file.
        vs%grid%qi = qi2
        vs%grid%stgch = store2
        vs%grid%qo = qo2
        if (fms%rsvr%n > 0) then
            reach_last = lake_elv(:, fhr)
        end if

    end subroutine

    subroutine run_rte_resume_save(fls, shd)

        !> area_watflood: Shared variables used throughout rte code.
        !> mpi_module: Required for 'ipid'.
        !> sa_mesh_common: Variables, parameters, and types from SA_MESH.
        use area_watflood
        use mpi_module
        use model_files_variables
        use sa_mesh_common

        type(fl_ids) fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) shd

        !> Local variables.
        integer ierr, iun

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. rteflg%PROCESS_ACTIVE) return

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.rte', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Write the current state of these variables to the file.
        write(iun) int(fhr, kind = 4)
        write(iun) qo2
        write(iun) store2
        write(iun) qi2
        if (fms%rsvr%n > 0) then
            write(iun) lake_elv(:, fhr)
        else
            write(iun)
        end if

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    subroutine run_rte_finalize(fls, shd)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_variables: Variables, parameters, and types from SA_MESH.
        use model_files_variables
        use sa_mesh_common
!-        use FLAGS

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. rteflg%PROCESS_ACTIVE) return

    end subroutine

end module
