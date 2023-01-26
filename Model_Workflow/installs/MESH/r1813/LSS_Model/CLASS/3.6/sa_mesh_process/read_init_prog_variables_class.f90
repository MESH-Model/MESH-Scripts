!>
!> AUTHOR : GONZALO SAPRIZA
!> DATE CREATION : 2014-07-14
!> DATES MODIFICATIONS : -
!> DESCRIPTION : Read only the prognostic variables needed by CLASS as initial
!>               conditions.
!>
!> The variables read are:
!> 1)   ALBS        - Snow albedo []
!> 2)   CMAI        - Aggregated mass of vegetation canopy [kg m-2]
!> 3)   GRO         - Vegetation growth index []
!> 4)   QAC         - Spec. Humidity of air within veget canopy space [kg kg-1]
!> 5)   RCAN        - Intercepted liquid water sotred on canopy [kg m-2]
!> 6)   RHOS        - Density of snow [kg m-3]
!> 7)   SCAN/SNCAN  - Intercepted frozen water stored on canopy [kg m-2]
!> 8)   SNO         - Mass of snow pack [kg m-2]
!> 9)   TAC         - Temp of air within veget canopy [K]
!> 10)  TBAR        - Temp of soil layers [k]
!> 11)  TBAS        - Temp of bedrock in third soil layer [K]
!> 12)  TCAN        - Temp veget canopy [K]
!> 13)  THIC        - Vol frozen water conetent of soil layers [m3 m-3]
!> 14)  THLQ        - Vol liquid water conetent of soil layers [m3 m-3]
!> 15)  TPND        - Temp of ponded water [k]
!> 16)  TSFS        - Ground surf temp over subarea [K]
!> 17)  TSNO        - Snowpack temp [K]
!> 18)  WSNO        - Liquid water content of snow pack [kg m-2]
!> 19)  ZPND        - Depth of ponded water on surface [m]
!>
    subroutine read_init_prog_variables_class(fls, shd)

        use model_files_variables
        use sa_mesh_common
        use RUNCLASS36_variables

        implicit none

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd

        !> Local variables.
        real(kind = 4), dimension(:), allocatable :: &
            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
            TCAN, TPND, TSNO, WSNO, ZPND
        real(kind = 4), dimension(:, :), allocatable :: &
            TBAR, THIC, THLQ, TSFS
        integer NML, NSL, ierr, iun

        !> Return if the process is not active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Open the resume state file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.runclass36', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

!>    type CLASS_prognostic_variables
!>        real, dimension(:), allocatable :: &
!>            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
!>            TCAN, TPND, TSNO, WSNO, ZPND
!>        real, dimension(:, :), allocatable :: &
!>            TBAR, THIC, THLQ, TSFS
!>    end type

        !> Allocate and initialize local variables.
        NML = shd%lc%NML
        NSL = shd%lc%IGND
        allocate( &
            ALBS(NML), CMAI(NML), GRO(NML), QAC(NML), RCAN(NML), RHOS(NML), SNCAN(NML), SNO(NML), TAC(NML), TBAS(NML), &
            TCAN(NML), TPND(NML), TSNO(NML), WSNO(NML), ZPND(NML))
        ALBS = 0.0; CMAI = 0.0; GRO = 0.0; QAC = 0.0; RCAN = 0.0; RHOS = 0.0; SNCAN = 0.0; SNO = 0.0; TAC = 0.0; TBAS = 0.0
        TCAN = 0.0; TPND = 0.0; TSNO = 0.0; WSNO = 0.0; ZPND = 0.0
        allocate( &
            TBAR(NML, NSL), THIC(NML, NSL), THLQ(NML, NSL), TSFS(NML, 4))
        TBAR = 0.0; THIC = 0.0; THLQ = 0.0; TSFS = 0.0

        !> Read inital values from the file.
        read(iun) ALBS      !1 (NML)
        read(iun) CMAI      !2 (NML)
        read(iun) GRO       !3 (NML)
        read(iun) QAC       !4 (NML)
        read(iun) RCAN      !5 (NML)
        read(iun) RHOS      !6 (NML)
        read(iun) SNCAN     !7 (NML)
        read(iun) SNO       !8 (NML)
        read(iun) TAC       !9 (NML)
        read(iun) TBAR      !10 (NML, NSL)
        read(iun) TBAS      !11 (NML)
        read(iun) TCAN      !12 (NML)
        read(iun) THIC      !13 (NML, NSL)
        read(iun) THLQ      !14 (NML, NSL)
        read(iun) TPND      !15 (NML)
        read(iun) TSFS      !16 (NML, 4)
        read(iun) TSNO      !17 (NML)
        read(iun) WSNO      !18 (NML)
        read(iun) ZPND      !19 (NML)

        !> Close the file to free the unit.
        close(iun)

        !> Assign values.
        vs%tile%albsno = real(ALBS, kind(vs%tile%albsno))
        vs%tile%cmas = real(CMAI, kind(vs%tile%cmas))
        vs%tile%gro = real(GRO, kind(vs%tile%gro))
        vs%tile%qacan = real(QAC, kind(vs%tile%qacan))
        vs%tile%lqwscan = real(RCAN, kind(vs%tile%lqwscan))
        vs%tile%rhosno = real(RHOS, kind(vs%tile%rhosno))
        vs%tile%fzwscan = real(SNCAN, kind(vs%tile%fzwscan))
        vs%tile%sno = real(SNO, kind(vs%tile%sno))
        vs%tile%tacan = real(TAC, kind(vs%tile%tacan))
        vs%tile%tsol = real(TBAR, kind(vs%tile%tsol))
        vs%tile%tbas = real(TBAS, kind(vs%tile%tbas))
        vs%tile%tcan = real(TCAN, kind(vs%tile%tcan))
        vs%tile%thicsol = real(THIC, kind(vs%tile%thicsol))
        vs%tile%thlqsol = real(THLQ, kind(vs%tile%thlqsol))
        vs%tile%tpnd = real(TPND, kind(vs%tile%tpnd))
        vs%tile%tsfs = real(TSFS, kind(vs%tile%tsfs))
        vs%tile%tsno = real(TSNO, kind(vs%tile%tsno))
        vs%tile%lqwssno = real(WSNO, kind(vs%tile%lqwssno))
        vs%tile%zpnd = real(ZPND, kind(vs%tile%zpnd))

        !> Deallocate local variables.
        deallocate( &
            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
            TCAN, TPND, TSNO, WSNO, ZPND, &
            TBAR, THIC, THLQ, TSFS)

    end subroutine !read_init_prog_variables_class
