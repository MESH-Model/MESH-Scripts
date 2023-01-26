!> Description:
!>  Subroutine to read structure locations and configurations from
!>  file. Structures shared by SA_MESH are accessible by
!>  'sa_mesh_variables'. Other structures are accessible by their
!>  respecitve process module(s).
!>
!> Input variables:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!>
!> Output variables:
!*  ierr: Error return status.
subroutine read_basin_structures(shd, ierr)

    use strings
    use sa_mesh_common
    use model_dates
    use txt_io

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    real, dimension(:), allocatable :: dist
    integer iun, iskip, isteps1, isteps2, n, i, z
    real d
    character(len = DEFAULT_LINE_LENGTH) fname, line
    logical ltest

    !> Initialize the return status.
    ierr = 0

    !> Reset spacing for screen output.
    call reset_tab()

    !> Return if routing routines are disabled.
    if (.not. ro%RUNCHNL) return

    !> Streamflow gauge locations.

    !> Initialize the time-series.
    fms%stmg%qomeas%iyear = ic%start%year
    fms%stmg%qomeas%ijday = ic%start%jday
    fms%stmg%qomeas%ihour = ic%start%hour
    fms%stmg%qomeas%imins = ic%start%mins

    !> Read the configuration from file.
    fname = fms%stmg%qomeas%fls%fname
    iun = fms%stmg%qomeas%fls%iun
    select case (lowercase(fms%stmg%qomeas%fls%ffmt))
        case ('tb0')
            fname = trim(adjustl(fname)) // '.tb0'
            call read_streamflow_tb0(shd, iun, fname, ierr)
        case default
            fname = trim(adjustl(fname)) // '.txt'
            call read_streamflow_txt(shd, iun, fname, ierr)
    end select
    if (ierr /= 0) return

    !> If locations exist.
    if (fms%stmg%n > 0) then

        !> Find the x-y cell coordinate of the location.
!-        fms%stmg%meta%iy = int((fms%stmg%meta%y - shd%yOrigin)/shd%yDelta) + 1
!-        fms%stmg%meta%jx = int((fms%stmg%meta%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location and create friendly name (if one does not exist).
        fms%stmg%meta%rnk = 0
        allocate(dist(fms%stmg%n))
        dist = huge(dist)
        do i = 1, fms%stmg%n
            if (len_trim(fms%stmg%meta%name(i)) == 0) then
                write(line, FMT_GEN) i
                fms%stmg%meta%name(i) = 'Gauge' // trim(adjustl(line))
            end if
            do n = 1, shd%NA
                d = (shd%ylat(n) - fms%stmg%meta%y(i))**2 + (shd%xlng(n) - fms%stmg%meta%x(i))**2
                if (d < dist(i)) then
!-                if (fms%stmg%meta%jx(i) == shd%xxx(n) .and. fms%stmg%meta%iy(i) == shd%yyy(n)) then
                    dist(i) = d
                    fms%stmg%meta%rnk(i) = n
                    fms%stmg%meta%iy(i) = shd%yyy(n)
                    fms%stmg%meta%jx(i) = shd%xxx(n)
!-                    if (shd%DA(n) == 0.0) then
!-                        call print_warning('Drainage area (DA) is zero at ' // trim(fms%stmg%meta%name(i)) // '.')
!-                    end if
                end if
            end do
        end do
        deallocate(dist)

        !> Print a message if any location is missing RANK (outside the basin).
!-        if (minval(fms%stmg%meta%rnk) == 0) then
!-            call print_error('Streamflow gauge(s) are outside the basin.')
!-            write(line, FMT_GEN) 'GAUGE', 'Y', 'IY', 'X', 'JX'
!-            call print_message(line)
!-            do i = 1, fms%stmg%n
!-                if (fms%stmg%meta%rnk(i) == 0) then
!-                    write(line, FMT_GEN) i, fms%stmg%meta%y(i), fms%stmg%meta%iy(i), fms%stmg%meta%x(i), fms%stmg%meta%jx(i)
!-                    call print_message(line)
!-                end if
!-            end do
!-            ierr = 1
!-            return
!-        end if

        !> Skip records in the file to the simulation start date.
        !> Units of the records interval is hours.
        isteps1 = jday_to_tsteps( &
            fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, fms%stmg%qomeas%ihour, fms%stmg%qomeas%imins, fms%stmg%qomeas%dts*60)
        isteps2 = jday_to_tsteps(ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins, fms%stmg%qomeas%dts*60)
        if (isteps2 < isteps1) then
            call print_error('The first record occurs after the simulation start date.')
            call print_message('This may cause channels to initialize with no storage.')
            write(line, "(i5, i4)") fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday
            call print_message('First record occurs on: ' // trim(line))
            write(line, "(i5, i4)") ic%start%year, ic%start%jday
            call print_message('Simulation start date: ' // trim(line))
            ierr = 1
            return
        end if
        iskip = (isteps2 - isteps1)
        if (iskip > 0) then
            write(line, FMT_GEN) iskip
            call print_message('Skipping ' // trim(adjustl(line)) // ' records.')
            z = read_records_txt(iun, fms%stmg%qomeas%val, iskip)
            if (z /= 0) then
                call print_warning('Reached end of file.')
            end if
        end if

        !> Read the first record, then reposition to the first record.
        z = read_records_txt(iun, fms%stmg%qomeas%val)
        if (z /= 0) fms%stmg%qomeas%val = 0.0
        backspace(iun)

        !> Warn if the initial value is zero.
        if (any(fms%stmg%qomeas%val == 0.0)) then
            call print_warning('The measured value at the simulation start date is zero.')
            call print_message('This may cause channels to initialize with no storage.')
        end if

        !> Print a summary of locations to file.
        write(line, FMT_GEN) fms%stmg%n
        call print_message('Number of streamflow gauges: ' // trim(adjustl(line)))
        write(line, FMT_GEN) 'GAUGE', 'IY', 'JX', 'RANK', 'DA (km2)'
        call print_message(trim(line))
        do i = 1, fms%stmg%n
            write(line, FMT_GEN) &
                fms%stmg%meta%name(i), fms%stmg%meta%iy(i), fms%stmg%meta%jx(i), fms%stmg%meta%rnk(i), shd%DA(fms%stmg%meta%rnk(i))
            call print_message(trim(line))
        end do
    end if

    !> Reservoir outlet locations.

    !> File unit and name.
    fname = fms%rsvr%rlsmeas%fls%fname
    iun = fms%rsvr%rlsmeas%fls%iun

    !> Read location from file if reaches exist.
    if (any(shd%IREACH > 0)) then

        !> Initialize time-series.
        fms%rsvr%rlsmeas%iyear = ic%start%year
        fms%rsvr%rlsmeas%ijday = ic%start%jday
        fms%rsvr%rlsmeas%ihour = ic%start%hour
        fms%rsvr%rlsmeas%imins = ic%start%mins

        !> Read from file.
        select case (lowercase(fms%rsvr%rlsmeas%fls%ffmt))
            case ('tb0')
                fname = trim(adjustl(fname)) // '.tb0'
                call read_reservoir_tb0(shd, iun, fname, ierr)
            case default
                fname = trim(adjustl(fname)) // '.txt'
                call read_reservoir_txt(shd, iun, fname, 2, ierr)
        end select
        if (ierr /= 0) return
    else
        fms%rsvr%n = 0
    end if

    !> Print an error if no reservoirs are defined but reaches exist from the drainage database file.
    if (maxval(shd%IREACH) /= fms%rsvr%n) then
        line = 'The number of reservoirs does not match between the drainage database (IREACH) and in: ' // trim(adjustl(fname))
        call print_error(line)
        write(line, FMT_GEN) maxval(shd%IREACH)
        call print_message('Maximum IREACH the drainage database: ' // trim(adjustl(line)))
        write(line, FMT_GEN) fms%rsvr%n
        call print_message('Number of reservoirs read from file: ' // trim(adjustl(line)))
        ierr = 1
        return
    end if

    !> If locations exist.
    if (fms%rsvr%n > 0) then

        !> Find the x-y cell coordinate of the location.
!-        fms%rsvr%meta%iy = int((fms%rsvr%meta%y - shd%yOrigin)/shd%yDelta) + 1
!-        fms%rsvr%meta%jx = int((fms%rsvr%meta%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location and create friendly name (if one does not exist).
        fms%rsvr%meta%rnk = 0
        allocate(dist(fms%rsvr%n))
        dist = huge(dist)
        do i = 1, fms%rsvr%n
            if (len_trim(fms%rsvr%meta%name(i)) == 0) then
                write(line, FMT_GEN) i
                fms%rsvr%meta%name(i) = 'Reach' // trim(adjustl(line))
            end if
            do n = 1, shd%NA
                d = (shd%ylat(n) - fms%rsvr%meta%y(i))**2 + (shd%xlng(n) - fms%rsvr%meta%x(i))**2
                if (d < dist(i)) then
!-                if (fms%rsvr%meta%jx(i) == shd%xxx(n) .and. fms%rsvr%meta%iy(i) == shd%yyy(n)) fms%rsvr%meta%rnk(i) = n
                    dist(i) = d
                    fms%rsvr%meta%rnk(i) = n
                    fms%rsvr%meta%iy(i) = shd%yyy(n)
                    fms%rsvr%meta%jx(i) = shd%xxx(n)
                end if
            end do
        end do
        deallocate(dist)

        !> Print an error if any location has no RANK (is outside the basin).
!-        if (minval(fms%rsvr%meta%rnk) == 0) then
!-            call print_error('Reservoir outlet(s) are outside the basin.')
!-            write(line, FMT_GEN) 'OUTLET', 'Y', 'IY', 'X', 'JX'
!-            call print_message(line)
!-            do i = 1, fms%rsvr%n
!-                if (fms%rsvr%meta%rnk(i) == 0) then
!-                    write(line, FMT_GEN) i, fms%rsvr%meta%y(i), fms%rsvr%meta%iy(i), fms%rsvr%meta%x(i), fms%rsvr%meta%jx(i)
!-                    call print_message(line)
!-                end if
!-            end do
!-            ierr = 1
!-            return
!-        end if

        !> Print an error if any outlet location has no REACH.
        ierr = 0
        do i = 1, fms%rsvr%n
            if (fms%rsvr%meta%rnk(i) > 0) then
                if (shd%IREACH(fms%rsvr%meta%rnk(i)) /= i) then
                    if (ierr == 0) then
                        call print_error("'IREACH' of certain reservoirs does not match the reservoir order.")
                        write(line, FMT_GEN) 'RANK', 'IREACH VAL.', 'EXPECTING'
                        call print_message(line)
                    end if
                    write(line, FMT_GEN) fms%rsvr%meta%rnk(i), shd%IREACH(fms%rsvr%meta%rnk(i)), i
                    call print_message(line)
                    ierr = 1
                end if
            end if
        end do
        if (ierr /= 0) return

        !> Calculate area from 'IREACH' cells if not specified.
        do i = 1, fms%rsvr%n
            if (fms%rsvr%rls%area(i) == 0.0) fms%rsvr%rls%area(i) = sum(shd%AREA, shd%IREACH == fms%rsvr%rls%area(i))
        end do

        !> Initialize reservoir release values if such a type of reservoir has been defined.
        if (count(fms%rsvr%rls%b1 == 0.0) > 0) then

            !> Re-allocate release values to the number of controlled reservoirs.
            if (fms%rsvr%rlsmeas%readmode /= 'n') then
                deallocate(fms%rsvr%rlsmeas%val)
                allocate(fms%rsvr%rlsmeas%val(count(fms%rsvr%rls%b1 == 0.0)))
                fms%rsvr%rlsmeas%val = 0.0
            end if

            !> Skip records in the file to the simulation start date.
            !> Units of the records interval is hours.
            isteps1 = jday_to_tsteps( &
                fms%rsvr%rlsmeas%iyear, fms%rsvr%rlsmeas%ijday, fms%rsvr%rlsmeas%ihour, fms%rsvr%rlsmeas%imins, &
                fms%rsvr%rlsmeas%dts*60)
            isteps2 = jday_to_tsteps(ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins, fms%rsvr%rlsmeas%dts*60)
            if (isteps2 < isteps1) then
                call print_error('The first record occurs after the simulation start date.')
                call print_message( &
                    'The record must start on or after the simulation start date when controlled reservoirs are active.')
                write(line, "(i5, i4)") fms%rsvr%rlsmeas%iyear, fms%rsvr%rlsmeas%ijday
                call print_message('First record occurs on: ' // trim(line))
                write(line, "(i5, i4)") ic%start%year, ic%start%jday
                call print_message('Simulation start date: ' // trim(line))
                ierr = 1
                return
            end if
            iskip = (isteps2 - isteps1)
            if (iskip > 0) then
                write(line, FMT_GEN) iskip
                call print_message('Skipping ' // trim(adjustl(line)) // ' records.')
                ierr = read_records_txt(iun, fms%rsvr%rlsmeas%val, iskip)
                if (ierr /= 0) then
                    call print_error('Reached end of file.')
                    return
                end if
            end if

            !> Read the first record, then reposition to the first record.
            !> Stop if no releases exist.
            ierr = read_records_txt(iun, fms%rsvr%rlsmeas%val)
            if (ierr /= 0) then
                call print_error('Reached end of file.')
                return
            end if
            backspace(iun)
        end if

        !> Print a summary of locations to file.
        if ((fms%rsvr%n - count(fms%rsvr%rls%b1 == 0.0)) > 0) then
            write(line, FMT_GEN) (fms%rsvr%n - count(fms%rsvr%rls%b1 == 0.0))
            call print_message('Number of reservoir outlets with routing: ' // trim(adjustl(line)))
            write(line, FMT_GEN) 'OUTLET', 'IY', 'JX', 'RANK', 'AREA (km2)'
            call print_message(trim(line))
            do i = 1, fms%rsvr%n
                if (fms%rsvr%rls%b1(i) /= 0) then
                    write(line, FMT_GEN) &
                        fms%rsvr%meta%name(i), fms%rsvr%meta%iy(i), fms%rsvr%meta%jx(i), fms%rsvr%meta%rnk(i), &
                        fms%rsvr%rls%area(i)/1.0e+6
                    call print_message(trim(line))
                end if
            end do
        end if
        if (count(fms%rsvr%rls%b1 == 0.0) > 0) then
            write(line, FMT_GEN) count(fms%rsvr%rls%b1 == 0.0)
            call print_message('Number of reservoir outlets with insertion: ' // trim(adjustl(line)))
            write(line, FMT_GEN) 'OUTLET', 'IY', 'JX', 'RANK', 'AREA (km2)'
            call print_message(trim(line))
            do i = 1, fms%rsvr%n
                if (fms%rsvr%rls%b1(i) == 0.0) then
                    write(line, FMT_GEN) &
                        fms%rsvr%meta%name(i), fms%rsvr%meta%iy(i), fms%rsvr%meta%jx(i), fms%rsvr%meta%rnk(i), &
                        fms%rsvr%rls%area(i)/1.0e+6
                    call print_message(trim(line))
                end if
            end do
        end if
    end if

    !> Abstraction point locations.
    ltest = .false.
    if (allocated(pm%tile%iabsp)) then
        if (any(pm%tile%iabsp > 0)) ltest = .true.
    end if
    if (allocated(pm%grid%iabsp)) then
        if (any(pm%grid%iabsp > 0)) ltest = .true.
    end if

    !> File unit and name.
    fname = fms%absp%sabst%fls%fname
    iun = fms%absp%sabst%fls%iun

    !> Read location from file if points exist.
    if (ltest) then

        !> Initialize time-series.
        fms%absp%sabst%iyear = ic%start%year
        fms%absp%sabst%ijday = ic%start%jday
        fms%absp%sabst%ihour = ic%start%hour
        fms%absp%sabst%imins = ic%start%mins

        !> Read from file.
        select case (lowercase(fms%absp%sabst%fls%ffmt))
            case ('tb0')
                fname = trim(adjustl(fname)) // '.tb0'
                call read_abstractionpoint_tb0(shd, iun, fname, ierr)
            case default
                fname = trim(adjustl(fname)) // '.txt'
                call read_abstractionpoint_txt(shd, iun, fname, ierr)
        end select
        if (ierr /= 0) return

        !> Print an error if no points are defined but exist from other files.
        if (maxval(pm%tile%iabsp) /= fms%absp%n) then
            line = 'The number of abstraction points does not match between other input files and: ' // trim(adjustl(fname))
            call print_error(line)
            write(line, FMT_GEN) maxval(pm%tile%iabsp)
            call print_message('Maximum points defined in other input files: ' // trim(adjustl(line)))
            write(line, FMT_GEN) fms%absp%n
            call print_message('Number of points read from file: ' // trim(adjustl(line)))
            ierr = 1
            return
        end if
    else
        fms%absp%n = 0
    end if

    !> If points exist.
    if (fms%absp%n > 0) then

        !> Find the x-y cell coordinate of the point.
!-        fms%absp%meta%iy = int((fms%absp%meta%y - shd%yOrigin)/shd%yDelta) + 1
!-        fms%absp%meta%jx = int((fms%absp%meta%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the point and create friendly name (if one does not exist).
        fms%absp%meta%rnk = 0
        allocate(dist(fms%absp%n))
        dist = huge(dist)
        do i = 1, fms%absp%n
            if (len_trim(fms%absp%meta%name(i)) == 0) then
                write(line, FMT_GEN) i
                fms%absp%meta%name(i) = 'Abspt' // trim(adjustl(line))
            end if
            do n = 1, shd%NA
                d = (shd%ylat(n) - fms%absp%meta%y(i))**2 + (shd%xlng(n) - fms%absp%meta%x(i))**2
                if (d < dist(i)) then
!-                if (fms%absp%meta%jx(i) == shd%xxx(n) .and. fms%absp%meta%iy(i) == shd%yyy(n)) fms%absp%meta%rnk(i) = n
                    dist(i) = d
                    fms%absp%meta%rnk(i) = n
                    fms%absp%meta%iy(i) = shd%yyy(n)
                    fms%absp%meta%jx(i) = shd%xxx(n)
                end if
            end do
        end do
        deallocate(dist)

        !> Print an error if any point has no RANK (is outside the basin).
!-        if (minval(fms%absp%meta%rnk) == 0) then
!-            call print_error('Abstraction point(s) are outside the basin.')
!-            write(line, FMT_GEN) 'OUTLET', 'Y', 'IY', 'X', 'JX'
!-            call print_message(line)
!-            do i = 1, fms%absp%n
!-                if (fms%absp%meta%rnk(i) == 0) then
!-                    write(line, FMT_GEN) i, fms%absp%meta%y(i), fms%absp%meta%iy(i), fms%absp%meta%x(i), fms%absp%meta%jx(i)
!-                    call print_message(line)
!-                end if
!-            end do
!-            ierr = 1
!-            return
!-        end if

        !> Print a summary of locations to file.
        write(line, FMT_GEN) fms%absp%n
        call print_message('Number of abstraction points: ' // trim(adjustl(line)))
        write(line, FMT_GEN) 'ABST. POINT', 'IY', 'JX', 'RANK'
        call print_message(trim(line))
        do i = 1, fms%absp%n
            write(line, FMT_GEN) fms%absp%meta%name(i), fms%absp%meta%iy(i), fms%absp%meta%jx(i), fms%absp%meta%rnk(i)
            call print_message(trim(line))
        end do
    end if

end subroutine
