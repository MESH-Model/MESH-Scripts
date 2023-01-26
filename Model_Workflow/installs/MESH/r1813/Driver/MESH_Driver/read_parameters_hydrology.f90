!>
!> Description:
!>  Open and read in values from the hydrology parameters file.
!>
subroutine READ_PARAMETERS_HYDROLOGY(shd, fls, ierr)

    !> Required for line operations and string conversion.
    use strings

    !> Required for file object and hydrology.ini file index.
    use model_files_variables

    !> For the 'ShedGridParams' type and SA_MESH parameters.
    use sa_mesh_common

    !> Required for 'FROZENSOILINFILFLAG'.
    use FLAGS

    !> Required for the variables of various modules.
    use RUNCLASS36_variables
    use irrigation_module
    use WF_ROUTE_config
    use rte_module
    use baseflow_module
    use cropland_irrigation_variables
    use PBSM_module

    implicit none

    !> Local variables for parsing strings.
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args
    integer nargs

    !> Local variables for check active variables (Version 2.0).
    integer :: ikey = 0, ikeystate = 0

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    type(fl_ids), intent(in) :: fls

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer NTYPE, NA, NRVR, iun, n, k, i, m, j, z

    !> Local variables for reading from file.
    real, dimension(:), allocatable :: INDEPPARVAL
    real, dimension(:, :), allocatable :: DEPPARVAL
    character(8) FILE_VER
!-    logical :: VER_OK = .true.

    !> Initialize the return status.
    ierr = 0

    !>
    !> OPEN FILE
    !>

    NA = shd%NA
    NTYPE = shd%lc%NTYPE

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fls%fl(mfk%f23)%fn)))
    call increase_tab()
    iun = fls%fl(mfk%f23)%iun
    open(iun, file = fls%fl(mfk%f23)%fn, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open the file. Check if the file exists.')
        return
    end if

    !> Check the file version (if RELFLG = 1.0).
!-    if (RELFLG == 1) then

    !> Read the file version.
    call readline(iun, FILE_VER, ierr)
    if (index(FILE_VER, ':') > 0) then
        FILE_VER = trim(adjustl(FILE_VER(1:index(FILE_VER, ':') - 1)))
    else if (index(FILE_VER, ' ') > 0) then
        FILE_VER = trim(adjustl(FILE_VER(1:index(FILE_VER, ' ') - 1)))
    else
        FILE_VER = trim(adjustl(FILE_VER))
    end if
!+    VER_OK = .false.
!+    if (FILE_VER == RELEASE) then
!+        VER_OK = .true.
!+    end if

    !> Wrong file version.
!-    if (.not. VER_OK .and. ISHEADNODE) then
!-        print *
!-        if (len(trim(adjustl(FILE_VER))) > 0) then
!-            print *, ' File version: ', FILE_VER
!-        else
!-            print *, 'This file is out of date.'
!-        end if

!-        print *, 'MESH requires file version: ', RELEASE
!-        print *, 'Please update MESH_parameters_hydrology.ini.'
!-        print *, 'The file must contain the version number'
!-        print *, 'on the first line, followed by a colon.'
!-        print *, 'EXAMPLE: '
!-        print *, RELEASE, ': MESH_parameters_hydrology.ini'
!-        print *
!-        print *, 'Please insure that all other parameters'
!-        print *, 'are also updated.'
!-        goto 98
!-    else
!-        read(iun, *)
!-    end if

    !>
    !> READ FILE
    !>

    !> Option flags.
    call readline(iun, line, ierr)
    call readline(iun, line, ierr)
    call readline(iun, line, ierr)
    if (ierr /= 0) goto 98
    read(line, *, err = 98) n
    if (n > 0) then
        do i = 1, n
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
            if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
            call compact(line)
            call parse(line, ' ', args, nargs)
            if (nargs < 2) cycle
            select case (lowercase(args(1)))
                case ('wf_route')
                    do j = 2, nargs
                        select case (lowercase(args(j)))
                            case ('rl_shd')
                                WF_RTE_flgs%RLFLAG = 1
                            case ('cap_shd')
                                WF_RTE_flgs%CAPFLAG = 1
                        end select
                    end do
                case ('rte')
                    do j = 2, nargs

                        !> Multi-word options.
                        if (lowercase(args(j)(1:9)) == 'dtminusr=') then
                            call value(args(j)(10:), rteflg%dtminusr, z)
                        else if (lowercase(args(j)(1:9)) == 'mindtmin=') then
                            call value(args(j)(10:), rteflg%mindtmin, z)
                        else if (lowercase(args(j)(1:9)) == 'maxindex=') then
                            call value(args(j)(10:), rteflg%maxindex, z)
                        else if (lowercase(args(j)(1:10)) == 'dtminfrac=') then
                            call value(args(j)(11:), rteflg%dtminfrac, z)
                        else if (lowercase(args(j)(1:14)) == 'convthreshusr=') then
                            call value(args(j)(15:), rteflg%convthreshusr, z)

                        !> Single-word options.
                        else
                            select case (lowercase(args(j)))
                                case ('cap_shd')
                                    rteflg%cap_shd = 1
                            end select
                        end if
                    end do
                case ('controlled_reservoir')
                    do j = 2, nargs
                        select case (lowercase(args(j)))
                            case ('allcols')
                                fms%rsvr%rlsmeas%readmode = 'n'
                        end select
                    end do
            end select
        end do
    end if

    !>
    !> River channel routing variables.
    !>

    !> Initialize variables.
    NRVR = shd%NRVR

    !> Read variables from file.
    call readline(iun, line, ierr)
    call readline(iun, line, ierr)
    if (ierr /= 0) goto 98

    !> Switch between file version.
    select case (FILE_VER)

        !> Version 2.0.
        case ('2.0')

            !> Read number of channel routing parameters.
            !* n: 'n' is the number of lines (variables) to read.
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            read(line, *, err = 98) n
            if (n > 0) then
                do i = 1, n

                    !> Read from the line.
                    z = 0
                    call readline(iun, line, z)
                    if (z /= 0) goto 919

                    !> Stop if the parameter has no values.
                    call parse(line, ' ', args, nargs)
                    if (nargs < 2) goto 918

                    !> Switch between active values.
                    select case (lowercase(args(1)))

                        !> WF_R2 (r2).
                        case ('wf_r2')
                            if (.not. WF_RTE_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), wfp%r2(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> WF_R1 (r1).
                        case ('wf_r1')
                            if (.not. WF_RTE_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), wfp%r1(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> WF_A1 (aa1).
                        case ('wf_a1')
                            if (.not. WF_RTE_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), wfp%aa1(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> WF_A2 (aa2).
                        case ('wf_a2')
                            if (.not. WF_RTE_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), wfp%aa2(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> WF_A3 (aa3).
                        case ('wf_a3')
                            if (.not. WF_RTE_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), wfp%aa3(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> WF_A4 (aa4).
                        case ('wf_a4')
                            if (.not. WF_RTE_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), wfp%aa4(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> r1n (RTE).
                        case ('r1n')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%r1n(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> r2n (RTE).
                        case ('r2n')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%r2n(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> mndr (RTE).
                        case ('mndr')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%mndr(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> widep (RTE).
                        case ('widep')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%widep(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> flz (BASEFLOWFLAG 2).
                        case ('flz')
                            if (bflm%BASEFLOWFLAG == 0) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), bflm%pm_iak%flz(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> pwr (BASEFLOWFLAG 2).
                        case ('pwr')
                            if (bflm%BASEFLOWFLAG == 0) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), bflm%pm_iak%pwr(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> aa2 (RTE).
                        case ('aa2')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%aa2(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> aa3 (RTE).
                        case ('aa3')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%aa3(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> aa4 (RTE).
                        case ('aa4')
                            if (.not. rteflg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NRVR
                                    call value(args(j + 1), rtepm_iak%aa4(j), z)
                                    if (z /= 0) goto 911
                                end do
                            end if

                        !> Unrecognized parameter name.
                        case default
                            goto 917

                    end select
                end do
            end if

        !> Original format of the hydrology.ini file.
        case default
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            if (WF_RTE_flgs%PROCESS_ACTIVE) read(line, *, iostat = ierr) (wfp%r2(j), j = 1, NRVR)
            if (ierr /= 0 .and. ISHEADNODE) then
                print 8110, NRVR
                goto 98
            end if
    end select

    !> Check values of the river channel roughness factor.
    do i = 1, NRVR
        if (WF_RTE_flgs%PROCESS_ACTIVE) then
            if (wfp%r2(i) <= 0.0 .and. ISHEADNODE) then
                print 8110, NRVR
                goto 98
            end if
        end if
    end do

8110    format(//1x, 'ERROR:', &
                /1x, 'A river channel roughness factor (WF_R2) is required for all active river classes.', &
                /1x, "The number of river classes from the drainage database (ICHNL): NRVR =", i3)

9110    format(//3x, 'Error converting channel routing parameter ', (a), ' #', i3, ": '", (a), "'", &
                /3x, 'A value is required for all active river classes.', &
                /3x, "The number of river classes from the drainage database (ICHNL): NRVR =", i3)
9170    format(//3x, 'Unrecognized channel routing parameter: ', (a))
9180    format(//3x, 'Channel routing parameter ', i3, ' contains no values. NARGS =', i2)
9190    format(/1x, 'ERROR: Reading channel routing parameter', i3)

    !>
    !> GRU independent parameters.
    !>

    !> Read variables from file.
    !* n: 'n' is the number of lines (variables) to read.
    call readline(iun, line, ierr)
    call readline(iun, line, ierr)
    call readline(iun, line, ierr)
    if (ierr /= 0) goto 98
    read(line, *, err = 98) n
    if (n > 0) then

        !> Switch between file version.
        select case (FILE_VER)

            !> Version 2.0.
            case ('2.0')

                do i = 1, n

                    !> Read from the line.
                    z = 0
                    call readline(iun, line, z)
                    if (z /= 0) goto 929

                    !> Stop if the parameter has no values.
                    call parse(line, ' ', args, nargs)
                    ikey = 0

                    !> Stop if the parameter has no values.
                    if (nargs < 2) goto 928

                    !> Switch between active values.
                    select case (lowercase(args(1)))

                        !> FROZENSOILINFILFLAG == 1 (infiltration into frozen soils).

                        !> SOIL_POR_MAX.
                        case ('soil_por_max')
                            if (FROZENSOILINFILFLAG == 0) then
                                ikey = 1
                            else
                                call value(args(2), SOIL_POR_MAX, z)
                                if (z /= 0) goto 921
                            end if

                        !> SOIL_DEPTH.
                        case ('soil_depth')
                            if (FROZENSOILINFILFLAG == 0) then
                                ikey = 1
                            else
                                call value(args(2), SOIL_DEPTH, z)
                                if (z /= 0) goto 921
                            end if

                        !> S0.
                        case ('s0')
                            if (FROZENSOILINFILFLAG == 0) then
                                ikey = 1
                            else
                                call value(args(2), S0, z)
                                if (z /= 0) goto 921
                            end if

                        !> T_ICE_LENS.
                        case ('t_ice_lens')
                            if (FROZENSOILINFILFLAG == 0) then
                                ikey = 1
                            else
                                call value(args(2), T_ICE_LENS, z)
                                if (z /= 0) goto 921
                            end if

                        !> t0_ACC(1:NYEARS).
                        case ('t0_acc')
                            if (FROZENSOILINFILFLAG == 0) then
                                ikey = 1
                            else
                                do j = 1, NYEARS
                                    call value(args(j + 1), t0_ACC(j), z)
                                    if (z /= 0 .and. ISHEADNODE) then
                                        print 8220, NYEARS
                                        goto 922
                                    end if
                                end do
                            end if

                        !> BASEFLOWFLAG > 0 (lower zone storage).

                        !> WrchrgIni.
                        case ('wrchrgini')
                            if (bflm%BASEFLOWFLAG == 0) then
                                ikey = 1
                            else
                                call value(args(2), bflm%vs%WrchrgIni, z)
                                if (z /= 0) goto 921
                            end if

                        !> QbIni.
                        case ('qbini')
                            if (bflm%BASEFLOWFLAG == 0) then
                                ikey = 1
                            else
                                call value(args(2), bflm%vs%QbIni, z)
                                if (z /= 0) goto 921
                            end if

                        !> Unrecognized parameter name.
                        case default
                            goto 927

                    end select

                    !> Print warning message for unused variables.
                    ikeystate = ikeystate + ikey
                    if (ikey > 0 .and. DIAGNOSEMODE .and. ISHEADNODE) print 9898, trim(adjustl(args(1)))

                end do

            !> Original format of the hydrology.ini file.
            case default

                !> Allocate and distribute variables.
                allocate(INDEPPARVAL(n))
                do i = 1, n
                    call readline(iun, line, ierr)
                    read(line, *, iostat = ierr) INDEPPARVAL(i)
                    if (ierr /= 0) goto 929
                end do

                !> Count for flags active from run_options.ini.
                j = 0

                !> FROZENSOILINFILFLAG == 1 (infiltration into frozen soils).
                if (FROZENSOILINFILFLAG == 1) then
                    if (n < (4 + NYEARS)) then
                        SOIL_POR_MAX = INDEPPARVAL(1)
                        SOIL_DEPTH = INDEPPARVAL(2)
                        S0 = INDEPPARVAL(3)
                        T_ICE_LENS = INDEPPARVAL(4)
                        do i = 1, NYEARS
                            t0_ACC(i) = INDEPPARVAL(i + 4)
                        end do
                    else if (ISHEADNODE) then
                        print 8210
                        print 8220, NYEARS
                        goto 98
                    end if
                    j = j + (4 + NYEARS)
                end if

                !> BASEFLOWFLAG > 0 (lower zone storage).
                if (bflm%BASEFLOWFLAG > 0) then
                    bflm%vs%WrchrgIni = INDEPPARVAL(j + 1)
                    bflm%vs%QbIni = INDEPPARVAL(j + 2)
                    j = j + 2
                end if

                !> Clean-up/deallocate variable.
                deallocate(INDEPPARVAL)
        end select
    end if

8210    format(//1x, 'ERROR:', &
                /1x, 'FROZENSOILINFILFLAG is active but the corresponding parameter values are not correctly specified.', &
                /1x, 'These GRU independent parameters are required:', &
                /3x, 'SOIL_POR_MAX: Maximum soil porosity [0.0-1.0]', &
                /3x, 'SOIL_DEPTH: Depth from surface to bottom on rooting zone for maximum water holding capacity, m', &
                /3x, 'S0: Surface soil saturation [0.0-1.0]', &
                /3x, 'T_ICE_LENS: Overnight temperature to cause ice lens formation, [-50.0-0.0] dC', &
                /3x, 't0_ACC: Opportunity time for each simulation year, [100-1000] h', &
                /3x, '        An empirical equation will be used to calculate', &
                /3x, '        opportunity time if these values are set to zero [0.0]', &
                /1x, 'These GRU dependent parameters are required:', &
                /3x, 'FRZC: Infiltration coefficient [1.0-3.0]')

8220    format(/3x, 'FROZENSOILINFILFLAG requires an opportunity time for each simulation year.', &
               /3x, 'This many values are required: NYEARS =', i3, &
               /3x, 'An empirical equation will be used to calculate opportunity time if these values are set to zero [0.0]')

9210    format(//3x, 'Error converting GRU independent parameter ', (a), ": '", (a), "'")
9220    format(//3x, 'Error converting GRU independent parameter ', (a), ' #', i3, ": '", (a), "'")
9270    format(//3x, 'Unrecognized GRU independent parameter: ', (a))
9280    format(//3x, 'GRU independent parameter ', i3, ' contains no values. NARGS =', i2)
9290    format(/1x, 'ERROR: Reading GRU independent parameter', i3)

    !>
    !> GRU dependent parameters.
    !>

    !> Read variables from file.
    call readline(iun, line, ierr)
    call readline(iun, line, ierr)
    if (ierr /= 0) goto 98

    !> Switch between file version.
    select case (FILE_VER)

        !> Version 2.0.
        case ('2.0')

            !> Read variables from file.
            !* n: 'n' is the number of lines (variables) to read.
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            read(line, *, err = 98) n
            if (n > 0) then
                do i = 1, n

                    !> Read from the line.
                    z = 0
                    call readline(iun, line, z)
                    if (z /= 0) goto 939

                    !> Stop if the parameter has no values.
                    call parse(line, ' ', args, nargs)
                    ikey = 0

                    !> Stop if the parameter has no values.
                    if (nargs < 2) goto 938

                    !> Switch between active values.
                    select case (lowercase(args(1)))

                        !> CLASS ponding limits.

                        !> ZSNL.
                        case ('zsnl')
                            do j = 1, NTYPE
                                call value(args(j + 1), pm%gru%zsnl(j), z)
                                if (z /= 0) goto 931
                            end do

                        !> ZPLS.
                        case ('zpls')
                            do j = 1, NTYPE
                                call value(args(j + 1), pm%gru%zpls(j), z)
                                if (z /= 0) goto 931
                            end do

                        !> ZPLG.
                        case ('zplg')
                            do j = 1, NTYPE
                                call value(args(j + 1), pm%gru%zplg(j), z)
                                if (z /= 0) goto 931
                            end do

                        !> FROZENSOILINFILFLAG == 1 (infiltration into frozen soils).

                        !> FRZC.
                        case ('frzc')
                            if (FROZENSOILINFILFLAG == 0) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), hp%FRZCROW(1, j), z)
                                    if (z /= 0) goto 931
                                    hp%FRZCROW(:, j) = hp%FRZCROW(1, j)
                                end do
                            end if

                        !> IWF.
                        case ('iwf')
                            do j = 1, NTYPE
                                call value(args(j + 1), pm%gru%iwf(j), z)
                                if (z /= 0) goto 931
                            end do

                        !> IWF == 2 (PDMROF).

                        !> CMAX.
                        case ('cmax')
                            if (.not. (any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3))) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), hp%CMAXROW(1, j), z)
                                    if (z /= 0) goto 931
                                    hp%CMAXROW(:, j) = hp%CMAXROW(1, j)
                                end do
                            end if

                        !> CMIN.
                        case ('cmin')
                            if (.not. (any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3))) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), hp%CMINROW(1, j), z)
                                    if (z /= 0) goto 931
                                    hp%CMINROW(:, j) = hp%CMINROW(1, j)
                                end do
                            end if

                        !> B.
                        case ('b')
                            if (.not. (any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3))) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), hp%BROW(1, j), z)
                                    if (z /= 0) goto 931
                                    hp%BROW(:, j) = hp%BROW(1, j)
                                end do
                            end if

                        !> K1.
                        case ('k1')
                            if (.not. (any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3))) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), hp%K1ROW(1, j), z)
                                    if (z /= 0) goto 931
                                    hp%K1ROW(:, j) = hp%K1ROW(1, j)
                                end do
                            end if

                        !> K2.
                        case ('k2')
                            if (.not. (any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3))) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), hp%K2ROW(1, j), z)
                                    if (z /= 0) goto 931
                                    hp%K2ROW(:, j) = hp%K2ROW(1, j)
                                end do
                            end if

                        !> PBSM (blowing snow).

                        !> fetch.
                        case ('fetch')
                            if (.not. pbsm%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), pbsm%pm_gru%fetch(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Ht.
                        case ('ht')
                            if (.not. pbsm%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), pbsm%pm_gru%Ht(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> N_S.
                        case ('n_s')
                            if (.not. pbsm%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), pbsm%pm_gru%N_S(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> A_S.
                        case ('a_s')
                            if (.not. pbsm%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), pbsm%pm_gru%A_S(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Distrib.
                        case ('distrib')
                            if (.not. pbsm%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), pbsm%pm_gru%Distrib(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> ICEBAL constants.
                        case ('frezth', 'icebal_freeze_threshold')
                            if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                if (.not. allocated(RUNCLASS36_flgs%pm%gru%FREZTH)) then
                                    allocate(RUNCLASS36_flgs%pm%gru%FREZTH(NTYPE))
                                end if
                                do j = 1, NTYPE
                                    call value(args(j + 1), RUNCLASS36_flgs%pm%gru%FREZTH(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if
                        case ('swelim', 'icebal_swe_limit', 'sndeplim', 'icebal_snow_depth_limit')
                            if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                if (.not. allocated(RUNCLASS36_flgs%pm%gru%SWELIM)) then
                                    allocate(RUNCLASS36_flgs%pm%gru%SWELIM(NTYPE))
                                end if
                                do j = 1, NTYPE
                                    call value(args(j + 1), RUNCLASS36_flgs%pm%gru%SWELIM(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if
                        case ('sndenlim', 'icebal_snow_density_limit')
                            if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                if (.not. allocated(RUNCLASS36_flgs%pm%gru%SNDENLIM)) then
                                    allocate(RUNCLASS36_flgs%pm%gru%SNDENLIM(NTYPE))
                                end if
                                do j = 1, NTYPE
                                    call value(args(j + 1), RUNCLASS36_flgs%pm%gru%SNDENLIM(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> BASEFLOWFLAG == 1 (lower zone storage).

                        !> dgwsh.
                        case ('dgwsh')
                            if (bflm%BASEFLOWFLAG /= 1) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), bflm%pm_gru%dgw(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> agwsh.
                        case ('agwsh')
                            if (bflm%BASEFLOWFLAG /= 1) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), bflm%pm_gru%agw(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> BASEFLOWFLAG == 2 (lower zone storage).

                        !> pwr.
                        case ('pwr')
                            if (bflm%BASEFLOWFLAG /= 2) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), bflm%pm_gru%pwr(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> flz, flz2.
                        case ('flz')
                            if (bflm%BASEFLOWFLAG /= 2) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), bflm%pm_gru%flz(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Cropland irrigation module.

                        !> jdsow.
                        case ('jdsow')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%jdsow(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> ldini.
                        case ('ldini')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%ldini(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> lddev.
                        case ('lddev')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%lddev(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> ldmid.
                        case ('ldmid')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%ldmid(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> ldlate.
                        case ('ldlate')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%ldlate(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Kcini.
                        case ('kcini')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%Kcini(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Kcdev.
                        case ('kcdev')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%Kcdev(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Kcmid.
                        case ('kcmid')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%Kcmid(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Kclate.
                        case ('kclate')
                            if (.not. cifg%PROCESS_ACTIVE) then
                                ikey = 1
                            else
                                do j = 1, NTYPE
                                    call value(args(j + 1), ciprot%Kclate(j), z)
                                    if (z /= 0) goto 931
                                end do
                            end if

                        !> Irrigation module.
                        case ('irflg')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%irflg(j), z)
                                if (z /= 0) goto 931
                            end do
                        case ('irt1')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%t1(j), z)
                                if (z /= 0) goto 931
                            end do
                        case ('irt2')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%t2(j), z)
                                if (z /= 0) goto 931
                            end do
                        case ('irijday1')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%ijday1(j), z)
                                if (z /= 0) goto 931
                            end do
                        case ('irijday2')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%ijday2(j), z)
                                if (z /= 0) goto 931
                            end do
                        case ('irignd')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%ignd(j), z)
                                if (z /= 0) goto 931
                            end do
                        case ('irthlmin')
                            do j = 1, NTYPE
                                call value(args(j + 1), irrm%pm_gru%thlmin(j), z)
                                if (z /= 0) goto 931
                            end do

                        !> Abstraction point location.
                        case ('iabsp')
                            do j = 1, NTYPE
                                call value(args(j + 1), pm%gru%iabsp(j), z)
                                if (z /= 0) goto 931
                            end do

                        !> Unrecognized parameter name.
                        case default
                            goto 937
                    end select

                    !> Print warning message for unused variables.
                    ikeystate = ikeystate + ikey
                    if (ikey > 0 .and. DIAGNOSEMODE .and. ISHEADNODE) print 9898, trim(adjustl(args(1)))
                end do
            end if

        !> Original format of the hydrology.ini file.
        case default

            !> Read variables from file.
            !* n: 'n' is the number of lines (variables) to read.
            !* i: 'i' is the number of GRUs.
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            read(line, *, err = 98) i
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            read(line, *, err = 98) n
            if (n > 0) then

                !> Check that GRU count matches the GRU count from the shd file.
                if (i /= NTYPE .and. ISHEADNODE) then
                    print 8310, i, NTYPE
                    goto 98
                end if

                !> Check the number of parameters.
                if ((any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3)) .and. n < 9 .and. ISHEADNODE) then
                    print 8330, 9, 'PDMROF or LATFLOW (IWF 2 or 3)'
                    goto 98
                else if (FROZENSOILINFILFLAG == 1 .and. n < 4 .and. ISHEADNODE) then
                    print 8330, 4, 'FROZENSOILINFILFLAG'
                    goto 98
                else if (n < 3 .and. ISHEADNODE) then
                    print 8320, 3
                    goto 98
                end if

                !> Allocate and populate the temporary variable.
                call readline(iun, line, ierr)
                if (ierr /= 0) goto 98
                allocate(DEPPARVAL(n, NTYPE))
                do i = 1, n
                    call readline(iun, line, ierr)
                    if (ierr /= 0) goto 98
                    read(line, *, iostat = ierr) (DEPPARVAL(i, j), j = 1, NTYPE)
                    if (ierr /= 0) goto 939
                end do

                !> Distribute CLASS ponding limits.
                pm%gru%zsnl = DEPPARVAL(1, :)
                pm%gru%zpls = DEPPARVAL(2, :)
                pm%gru%zplg = DEPPARVAL(3, :)

                !> PBSM (blowing snow).
                if (pbsm%PROCESS_ACTIVE) then
                    pbsm%pm_gru%fetch = DEPPARVAL(10, :)
                    pbsm%pm_gru%Ht = DEPPARVAL(11, :)
                    pbsm%pm_gru%N_S = DEPPARVAL(12, :)
                    pbsm%pm_gru%A_S = DEPPARVAL(13, :)
                    pbsm%pm_gru%Distrib = DEPPARVAL(14, :)
                end if

                !> Distribute the parameters.
!todo: change this to il2, il2
                do m = 1, NTYPE
                    do i = 1, NA

                        !> FROZENSOILINFILFLAG == 1 (infiltration into frozen soils).
                        if (FROZENSOILINFILFLAG == 1) then
                            hp%FRZCROW(i, m) = DEPPARVAL(4, m)
                        end if

                        !> IWF == 2 (PDMROF) or IWF == 3 (LATFLOW).
                        if (any(pm%gru%iwf == 2) .or. any(pm%gru%iwf == 3)) then
                            hp%CMAXROW(i, m) = DEPPARVAL(5, m)
                            hp%CMINROW(i, m) = DEPPARVAL(6, m)
                            hp%BROW(i, m) = DEPPARVAL(7, m)
                            hp%K1ROW(i, m) = DEPPARVAL(8, m)
                            hp%K2ROW(i, m) = DEPPARVAL(9, m)
                        end if

                    end do

                    !> Count for active flags (read from run options).
                    j = 9

                    !> PBSM (blowing snow).
                    if (pbsm%PROCESS_ACTIVE) then
                        j = j + 5
                    end if

                    !> BASEFLOWFLAG 1 (Luo, 2012).
                    if (bflm%BASEFLOWFLAG == 1) then
                        bflm%pm_gru%dgw(m) = DEPPARVAL(j + 1, m)
                        bflm%pm_gru%agw(m) = DEPPARVAL(j + 2, m)
                        j = j + 2
                    end if

                    !> BASEFLOWFLAG == 2 (lower zone storage).
                    if (bflm%BASEFLOWFLAG == 2) then
                        bflm%pm_gru%pwr(m) = DEPPARVAL(j + 1, m)
                        bflm%pm_gru%flz(m) = DEPPARVAL(j + 2, m)
                        j = j + 2
                    end if

                end do

                !> Clean-up/deallocate the variable.
                deallocate(DEPPARVAL)
            end if
    end select

8310    format(//1x, 'ERROR:', &
                /1x, 'The number of GRUs in the hydrology parameter file should be the same', &
                /1x, 'as the number of GRUs from the drainage database.', &
                /3x, 'Number of GRUs in the hydrology parameter file: ', i3, &
                /3x, 'Number of GRUs from the drainage database: ', i3)
8320    format(//1x, 'ERROR:', &
                /1x, 'The number of GRU dependent parameters should be ', i2, '.', &
                /1x, 'Refer to the current template of the hydrology parameter file.')
8330    format(//1x, 'ERROR:', &
                /1x, 'The number of GRU dependent parameters should be ', i2, ' with', &
                /1x, (a), ' active.', &
                /1x, 'Refer to the current template of the hydrology parameter file.')

9310    format(//3x, 'Error converting GRU dependent parameter ', (a), ' GRU', i3, ": '", (a), "'", &
                /3x, 'A value is required for all active GRUs.', &
                /3x, "The number of GRUs from the drainage database: NTYPE =", i3)
9370    format(//3x, 'Unrecognized GRU dependent parameter: ', (a))
9380    format(//3x, 'GRU dependent parameter ', i3, ' contains no values. NARGS =', i2)
9390    format(/1x, 'ERROR: Reading GRU dependent parameter', i3)

    !>
    !> CLOSE FILE
    !>

9898    format(3x, "WARNING: The parameter '", (a), "' is active but not used.")

    !> Close the file.
    close(iun)

    !>
    !> RETURN
    !>

    return

    !>
    !> STOP STATEMENTS
    !>

911     print 9110, trim(args(1)), j, trim(args(j + 1)), NRVR; goto 98
917     print 9170, trim(args(1)); goto 98
918     print 9180, i, nargs; goto 98
919     print 9190, i; goto 98

921     print 9210, trim(args(1)), trim(args(2)); goto 98
922     print 9220, trim(args(1)), j, trim(args(j + 1)); goto 98
927     print 9270, trim(args(1)); goto 98
928     print 9280, i, nargs; goto 98
929     print 9290, i; goto 98

931     print 9310, trim(args(1)), j, trim(args(j + 1)), NTYPE; goto 98
937     print 9370, trim(args(1)); goto 98
938     print 9380, i, nargs; goto 98
939     print 9390, i; goto 98

98  ierr = 1
    call print_error('Unable to read the file.')
    return

end subroutine
