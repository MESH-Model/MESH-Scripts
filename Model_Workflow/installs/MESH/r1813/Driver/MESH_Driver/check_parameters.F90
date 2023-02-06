!>
!> March 23, 2010 - M.A. Mekonnen/B. Davidson/M. MacDonald
!>
!> Description:
!>  The subroutine checks for parameter values specified in the
!>  "MESH_parameters_hydrology.ini" and "MESH_parameters_CLASS.ini"
!>  files to make sure that all the parameter values lie within the
!>  specified limits.
!>
!>  The minimum and maximum values of each parameter are specified in
!>  "minmax_parameters.txt" file. The parameters are stored as two
!>  dimensional arrays (nrows x NTYPE). NTYPE represents the number
!>  of GRUs.
!>
!> Input variables:
!>  shd: Basin information
!>
!> Parameters:
!>  cp: CLASS parameters
!>  hp: Hydrology parameters
!>  wfp: WATFLOOD parameters
!>
!> Local variables:
!>  parv: parameter values (two dimensional array)
!>  parflag: parameter flags (two dimensional array)
!>
!>  NA: Number of grids
!>  NAA: Number of grids in the basin
!>  NRVR: Number of river classes
!>  NTYPE: Number of GRUs
!>
subroutine check_parameters(shd)

    !> Required for 'ShedGridParams' type and SA_MESH parameters.
    use sa_mesh_common

    !> Required for 'FROZENSOILINFILFLAG' and 'SOILINIFLAG'
    use FLAGS

    !> Required for additional parameters.
    use RUNCLASS36_variables, only: hp, soil_por_max, soil_depth, s0, t_ice_lens
    use WF_ROUTE_config, only: wfp

    implicit none

    !* nrows: Maximum number of rows.
    integer, parameter :: nrows = 88

    integer i, j, ib0, ib1, ib2, ib3, i4, i5, ir, NA, NAA, NRVR, NTYPE, NSL
    integer, dimension(:, :), allocatable :: parflag
    real total, percent
    real, dimension(:, :), allocatable :: parv, minlimit, maxlimit
    character(len=10), dimension(:), allocatable :: parn

    type(ShedGridParams) :: shd

    !> *****************************************************************
    !> Subroutine initialization.
    !> *****************************************************************

    !> Basin information.
    NA = shd%NA
    NAA = shd%NAA
    NRVR = 5 !shd%NRVR
    NTYPE = shd%lc%NTYPE
    NSL = 3 !shd%lc%IGND

    !> Allocate and activate the parameter variables.
    allocate(parflag(nrows, NTYPE))
    parflag = 1
    allocate(parv(nrows, NTYPE), minlimit(nrows, NTYPE), maxlimit(nrows, NTYPE))
    parv = 0.0
    minlimit = 0.0
    maxlimit = 0.0
    allocate(parn(nrows))

    !> *****************************************************************
    !> Hydrology parameters.
    !> *****************************************************************

    ir = 1
    parv(ir, 1) = 2
    parflag(ir, 1:NTYPE) = 0 !The parameter is currently not active.

    ir = ir + 1
    parv(ir, 1) = 0
    parflag(ir, 1:NTYPE) = 0 !The parameter is currently not active.

    ir = ir + 1
    parv(ir, 1) = 0
    parflag(ir, 1:NTYPE) = 0 !The parameter is currently not active.

    !> River roughness factor.
    do i = 1, NRVR
        ir = ir + 1
        if (i > shd%NRVR .or. .not. allocated(wfp%r2)) then
            parflag(ir, 1) = 0 !only active wf_r2 values (by shed file)
        else
            parv(ir, 1) = wfp%r2(i)
            parflag(ir, 2:NTYPE) = 0 !only wf_r2(1) is currently active
        end if
        write(parn(ir), '(a2, i1)') 'R2', i
    end do

    ir = ir + 1
    parv(ir, 1) = soil_por_max
    parflag(ir, 2:NTYPE) = 0
    if (FROZENSOILINFILFLAG == 0) parflag(ir, 1) = 0
    parn(ir) = 'soil_por_max'

    ir = ir + 1
    parv(ir, 1) = soil_depth
    parflag(ir, 2:NTYPE) = 0
    if (FROZENSOILINFILFLAG == 0) parflag(ir, 1) = 0
    parn(ir) = 'soil_depth'

    ir = ir + 1
    parv(ir, 1) = s0
    parflag(ir, 2:NTYPE) = 0
    if (FROZENSOILINFILFLAG == 0) parflag(ir, 1) = 0
    parn(ir) = 's0'

    ir = ir + 1
    parv(ir, 1) = t_ice_lens
    parflag(ir, 2:NTYPE) = 0
    if (FROZENSOILINFILFLAG == 0) parflag(ir, 1) = 0
    parn(ir) = 't_ice_lens'

!    do i = 5, indeppar
!        ir = ir + 1
!        parv(ir, 1) = t0_acc(i - 4)
!        parflag(ir, 2:NTYPE) = 0
!        parn(ir) = 't0_acc'
!    end do

    !> *****************************************************************
    !> Class parameters.
    !> *****************************************************************
    ib0 = ir
    do j = 1, NTYPE
        parv(ir + 1, j) = pm%gru%drn(j)
        parv(ir + 2, j) = pm%gru%sdep(j)
        parv(ir + 3, j) = pm%gru%fare(j)
        parv(ir + 4, j) = pm%gru%dd(j)/1000.0
        parv(ir + 5, j) = pm%gru%xslp(j)
        parv(ir + 6, j) = pm%gru%grkf(j)
        parv(ir + 7, j) = pm%gru%mann(j)
        parv(ir + 8, j) = pm%gru%ks(j)
    end do
    parn(ir + 1) = 'DRN'
    parn(ir + 2) = 'SDEP'
    parn(ir + 3) = 'FARE'
    parn(ir + 4) = 'DD'
    parn(ir + 5) = 'XSLP'
    parn(ir + 6) = 'XDRAINH'
    parn(ir + 7) = 'MANN'
    parn(ir + 8) = 'KSAT'

    !> *****************************************************************
    !> Check the sum of soil percentages and adjust if necessary.
    !> *****************************************************************
!todo: move this elsewhere?
    ib1 = ir + 8
    do i = 1, NSL
        ir = ib1 + (i - 1)*NSL
        do j = 1, NTYPE

            !> Skip checking sum of soil percentages if soil layer is rock, glacier etc.
            if (pm%gru%sand(j, i) >= 0.0) then

                !> Compute sum of soil percentages.
                total = pm%gru%sand(j, i) + pm%gru%clay(j, i) + pm%gru%orgm(j, i)
                if (total > 100.0) then
                    print *
                    if (SOILINIFLAG == 1) then

                        !> MESH will use the specified values.
                        print *
                        print *, 'WARNING: Sum of soil percentages greater than 100%'
                        print('(a8, i3, /, a8, i3)'), 'GRU: ', j, 'LAYER: ', i
                    else if (SOILINIFLAG == 2) then

                        !> Keep sand percentage as is and adjust clay (and orgm) percentages.
                        print *
                        print *, 'Sum of soil percentages greater than 100% - clay (and orgm) percentages re-adjusted'
                        print('(a8, i3, /, a8, i3)'), 'GRU: ', j, 'LAYER: ', i
                        pm%gru%clay(j, i) = max(0.0, 100.0 - pm%gru%sand(j, i) - pm%gru%orgm(j, i))
                        pm%gru%orgm(j, i) = min(pm%gru%orgm(j, i), 100.0 - pm%gru%sand(j, i) - pm%gru%clay(j, i))
                    else if (SOILINIFLAG == 3) then

                        !> Keep clay percentage as is and adjust sand (and orgm) percentages.
                        print *
                        print *, 'Sum of soil percentages greater than 100% - sand (and orgm) percentages re-adjusted'
                        print('(a8, i3, /, a8, i3)'), 'GRU: ', j, 'LAYER: ', i
                        pm%gru%sand(j, i) = max(0.0, 100.0 - pm%gru%clay(j, i) - pm%gru%orgm(j, i))
                        pm%gru%orgm(j, i) = min(pm%gru%orgm(j, i), 100.0 - pm%gru%sand(j ,i) - pm%gru%clay(j, i))
                    else if (SOILINIFLAG == 4) then

                        !> Re-adjust both sand and clay percentages.
                        print *
                        print *, 'Sum of soil percentages greater than 100% - soil percentages re-adjusted'
                        print('(a8, i3, /, a8, i3)'), 'GRU: ', j, 'LAYER: ', i
                        pm%gru%sand(j, i) = pm%gru%sand(j, i)*100.0/total
                        pm%gru%clay(j, i) = pm%gru%clay(j, i)*100.0/total
                        pm%gru%orgm(j, i) = pm%gru%orgm(j, i)*100.0/total
                    else if (SOILINIFLAG == 5) then

                        !> Re-adjust both sand and clay percentages.
                        print *, "Soil parameter values will be directly read from 'soil.ini' file"
                    else
                        print *
                        print *, 'ERROR: Sum of soil percentages greater than 100%'
                        print('(a8, i3, /, a8, i3)'), 'GRU: ', j, 'LAYER: ', i
                        print *, 'Adjust the soil percentages or do either of the following: '
                        print *, 'Set SOILINIFLAG to 1 - MESH will use the values as specified'
                        print *, 'Set SOILINIFLAG to 2 - MESH will keep sand percentage and adjust clay and orgm percentages'
                        print *, 'Set SOILINIFLAG to 3 - MESH will keep clay percentage and adjust sand and orgm percentages'
                        print *, 'Set SOILINIFLAG to 4 - MESH will proportionally adjust sand, clay and orgm percentages'
                        print *, "Set SOILINIFLAG to 5 - MESH will read parameters from 'soil.ini' file"
                        stop
                    end if
                end if
            end if
            parv(ir + 1, j) = pm%gru%sand(j, i)
            parv(ir + 2, j) = pm%gru%clay(j, i)
            parv(ir + 3, j) = pm%gru%orgm(j, i)
        end do
        write(parn(ir + 1), '(a4, i1)') 'SAND', i
        write(parn(ir + 2), '(a4, i1)') 'CLAY', i
        write(parn(ir + 3), '(a4, i1)') 'ORGM', i
    end do

    !> *****************************************************************
    !> Hydrology parameters.
    !> *****************************************************************
    ir = ir + NSL
    do j = 1, NTYPE
        parv(ir + 1, j) = pm%gru%zsnl(j)
        parv(ir + 2, j) = pm%gru%zpls(j)
        parv(ir + 3, j) = pm%gru%zplg(j)
        if (FROZENSOILINFILFLAG == 0 .or. .not. allocated(hp%frzcrow)) then
            parflag(ir + 4, j) = 0
        else
            parv(ir + 4, j) = hp%frzcrow(1, j)
        end if
    end do
    parn(ir + 1) = 'ZSNL'
    parn(ir + 2) = 'ZPLS'
    parn(ir + 3) = 'ZPLG'
    parn(ir + 4) = 'FRZC'

    !> *****************************************************************
    !> Class parameters.
    !> *****************************************************************
    ib2 = ir + 4
    do i5 = 1, 5
        ir = ib2 + (i5 - 1)*6
        do j = 1, NTYPE
            parv(ir + 1, j) = pm%gru%lnz0(j, i5)
            parv(ir + 2, j) = pm%gru%alvc(j, i5)
            parv(ir + 3, j) = pm%gru%alic(j, i5)
            if (i5 < 5) then !urban areas
                parv(ir + 4, j) = pm%gru%rsmn(j, i5)
                parv(ir + 5, j) = pm%gru%vpda(j, i5)
                parv(ir + 6, j) = pm%gru%psga(j, i5)
            end if
        end do
        parn(ir + 1) = 'LNZ0'
        parn(ir + 2) = 'ALVC'
        parn(ir + 3) = 'ALIC'
        if (i5 < 5) then !urban areas
            parn(ir + 4) = 'RSMN'
            parn(ir + 5) = 'VPDA'
            parn(ir + 6) = 'PSGA'
        end if
    end do

    !> *****************************************************************
    !> Class parameters.
    !> *****************************************************************
    ib3 = ir + 3 !57
    do i4 = 1, 4
        ir = ib3 + (i4 - 1)*7
        do j = 1, NTYPE
            parv(ir + 1, j) = pm%gru%lamx(j, i4)
            parv(ir + 2, j) = pm%gru%lamn(j, i4)
            parv(ir + 3, j) = pm%gru%cmas(j, i4)
            parv(ir + 4, j) = pm%gru%root(j, i4)
            parv(ir + 5, j) = pm%gru%qa50(j, i4)
            parv(ir + 6, j) = pm%gru%vpdb(j, i4)
            parv(ir + 7, j) = pm%gru%psgb(j, i4)
        end do
        parn(ir + 1) = 'PAMX'
        parn(ir + 2) = 'PAMN'
        parn(ir + 3) = 'CMAS'
        parn(ir + 4) = 'ROOT'
        parn(ir + 5) = 'QA50'
        parn(ir + 6) = 'VPDB'
        parn(ir + 7) = 'PSGB'
    end do

    ir = ir + 7
    if (ir < nrows) parflag((ir + 1):nrows, :) = 0

    open(10, file = 'minmax_parameters.txt', status = 'old')
    do i = 1, ir
        read(10, *)
        if (i > ib0) then
            read(10, *) (minlimit(i, j), j = 1, NTYPE)
            read(10, *) (maxlimit(i, j), j = 1, NTYPE)
        else
            read(10, *) minlimit(i, 1)
            read(10, *) maxlimit(i, 1)
        end if

    end do
    close(10)

    call checkbound(parflag, parv, parn, minlimit, maxlimit, nrows, NTYPE)

end subroutine

!> *********************************************************************
!> The subroutine checks if the parameter value is within the limits.
!> *********************************************************************
subroutine checkbound(parflag, parv, parn, minlimit, maxlimit, nr, nc)

    implicit none

    integer nr, nc, parflag(nr, nc)
    real parv(nr, nc), minlimit(nr, nc), maxlimit(nr, nc)
    character(len=10) parn(nr)

    integer i, j, ncount

    write(*, *)
    write(*, *)
    write(*, *) 'Checking if parameter values lie within the specified ranges'
    write(*, *)

    ncount = 0

    do i = 1, nr
        do j = 1, nc
            if (parflag(i, j) == 1) then
                if (parv(i, j) < minlimit(i, j) .or. parv(i, j) > maxlimit(i, j)) then
                    ncount = ncount + 1
                    if (ncount == 1) then
                        write(*, *) 'The following parameter values are out of range'
                        write(*, *)
                        write(*, 10) 'NAME', 'ROW', 'COLUMN', 'Specified value', 'Minimum value', 'Maximum value'
                        write(*, *) '---------------------------------------------------------------------------'
                    end if
                    write(*, 20) parn(i), i, j, parv(i, j), minlimit(i, j), maxlimit(i, j)
                end if
            end if
        end do
    end do

    if (ncount > 0) then
        write(*, *)
        write(*, *) ncount, ' parameter(s) out of range'
        write(*, *)
        write(*, *) 'Adjust the parameter value(s) or modify the parameter limits'
        write(*, *)
        stop
    else
        write(*, *) 'All parameter values lie within the specified ranges'
        write(*, *)
    end if

10      format(1x, a10, 2(a6, 2x), 3(a15, 2x))
20      format(1x, a10, 2(i6, 2x), 3(f15.4, 2x))

end subroutine
