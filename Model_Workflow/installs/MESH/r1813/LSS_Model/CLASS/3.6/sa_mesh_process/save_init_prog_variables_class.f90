!>
!> AUTHOR : GONZALO SAPRIZA
!> DATE CREATION : 2014-07-14
!> DATES MODIFICATIONS : -
!> DESCRIPTION : Save only the prognostic variables needed by CLASS as initial
!>               conditions.
!>
!> The variables saved are:
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
    subroutine save_init_prog_variables_class(fls, shd)

        use model_files_variables
        use sa_mesh_common
        use RUNCLASS36_variables

        implicit none

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd

        !> Local variables.
        integer ierr, iun

        !> Return if not the head node or if the process is not active.
        if (.not. ISHEADNODE .or. .not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Open the resume state file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.runclass36', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

!>    type CLASS_prognostic_variables
!>        real, dimension(:), allocatable :: &
!>            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
!>            TCAN, TPND, TSNO, WSNO, ZPND
!>        real, dimension(:, :), allocatable :: &
!>            TBAR, THIC, THLQ, TSFS
!>    end type

        !> Write the current state of these variables to the file.
        write(iun) real(vs%tile%albsno, kind = 4)   !1 (NML)
        write(iun) real(vs%tile%cmas, kind = 4)     !2 (NML)
        write(iun) real(vs%tile%gro, kind = 4)      !3 (NML)
        write(iun) real(vs%tile%qacan, kind = 4)    !4 (NML)
        write(iun) real(vs%tile%lqwscan, kind = 4)  !5 (NML)
        write(iun) real(vs%tile%rhosno, kind = 4)   !6 (NML)
        write(iun) real(vs%tile%fzwscan, kind = 4)  !7 (NML)
        write(iun) real(vs%tile%sno, kind = 4)      !8 (NML)
        write(iun) real(vs%tile%tacan, kind = 4)    !9 (NML)
        write(iun) real(vs%tile%tsol, kind = 4)     !10 (NML, NSL)
        write(iun) real(vs%tile%tbas, kind = 4)     !11 (NML)
        write(iun) real(vs%tile%tcan, kind = 4)     !12 (NML)
        write(iun) real(vs%tile%thicsol, kind = 4)  !13 (NML, NSL)
        write(iun) real(vs%tile%thlqsol, kind = 4)  !14 (NML, NSL)
        write(iun) real(vs%tile%tpnd, kind = 4)     !15 (NML)
        write(iun) real(vs%tile%tsfs, kind = 4)     !16 (NML, 4)
        write(iun) real(vs%tile%tsno, kind = 4)     !17 (NML)
        write(iun) real(vs%tile%lqwssno, kind = 4)  !18 (NML)
        write(iun) real(vs%tile%zpnd, kind = 4)     !19 (NML)

        !> Close the file to free the unit.
        close(iun)

    end subroutine !save_init_prog_variables_class
