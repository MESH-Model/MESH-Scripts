!> Description:
!>  Module to read configuration information for the climate forcing
!>  module from file.
module climate_forcing_config

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use print_routines

    implicit none

    contains

    subroutine open_config(cm)

        use strings

        type(clim_info) :: cm

        integer i, iun, ios
        character(len = DEFAULT_FIELD_LENGTH) fname, ver_in
        character(len = DEFAULT_LINE_LENGTH) line
        logical is_exist, is_climInfo

        !> Open the file if one exists.
        fname = 'climate_info.ini'
        iun = cm%basefileunit
        inquire(file = fname, exist = is_exist)
        if (is_exist) then

            !> Open the file.
            open(iun, file = fname, status ='old', action = 'read')

            !> First uncommented line contains the tag and version of the file.
            !> :clim_info 1.0
            call readline(iun, line, ios)
            if (lowercase(line(1:10)) == ':clim_info') then
                is_climInfo = .true.
                ver_in = trim(adjustl(lowercase(line(11:))))
            end if

            !> Call subroutine to parse the content of the file.
            if (is_climInfo) then
                if (ver_in == '1.0') call parse_config_1_0(iun, cm)
            end if

            !> Close the file.
            close(iun)

        end if

    end subroutine

    !> Description:
    !>  This subroutine aligns with the 'climate_info.ini' file that has
    !>  ':1.0' in its header.
    subroutine parse_config_1_0(iun, cm)

        use strings

        integer, intent(in) :: iun

        type(clim_info) :: cm

        integer ios, i, j, jj, n, nn, vid, nlines, nargs
        character(len = DEFAULT_LINE_LENGTH), dimension(:), allocatable :: lines, args
        character(len = DEFAULT_FIELD_LENGTH) key, attr, mark

        !> Extract the sections of the file.
!        call parse_seek_section(iun, 'filenames', lines, nlines)

        !> Section of series
        !* Key: Variable name (must match a recognized key)
        !* Attributes:
        !* > 'n': Number of series for the variable
        !* > 'per': Element of discretization for a weighting scheme (e.g., gru) (':i': from 1 to n)
        !* > 'attr': Qualifying attribute (':i': from 1 to n)
        !*           > 'per=gru': attr1 is the order of the gru
        !*           > 'per=gru': attr2 is the weight assigned to the value
        !> :StartSeries
        !> FB, n=2, per:1=gru, per:2=gru, attr1:1=1, attr2:1=0.5, attr1:2=2, attr2:2=1.0
        !> :EndSeries
        call parse_seek_section(iun, 'series', lines, nlines)

        !> The lines returned include the :start and :end lines.
        do i = 2, (nlines - 1)
            call parse_split_fields(lines(i), ',', args, nargs)
            if (nargs > 2) then

                !> Extract the climate variable.
                select case (trim(args(1)))
                    case ('fb'); vid = ck%FB
                    case ('fi'); vid = ck%FI
                    case ('rt'); vid = ck%RT
                    case ('tt'); vid = ck%TT
                    case ('uv'); vid = ck%UV
                    case ('p0'); vid = ck%P0
                    case ('hu'); vid = ck%HU
                end select

                !> Extract the number of series.
                do j = 2, nargs
                    if (args(j)(1:2) == 'n=' .and. len_trim(args(j)) > 2) then
                        call value(args(j)(3:), n, ios)
                        if (ios /= 0) then
!todo: fix this
!print *, 'bad n value'
!stop
                        end if

                        !> Update the number of series.
                        if (n > 0) then
                            cm%dat(vid)%nseries = n
                            if (allocated(cm%dat(vid)%series)) deallocate(cm%dat(vid)%series)
                            allocate(cm%dat(vid)%series(cm%dat(vid)%nseries))
                        end if
                        exit
                    end if
                end do

                !> Extract the attributes of the series.
                do j = 2, nargs
                    call parse_split_attr(args(j), key, mark, attr)
                    select case (key)
                        case ('per')

                            !> Series of weighted data by GRU.
                            if (attr == 'gru') then

                                !> Extract the value of the series.
                                call value(mark, n, ios)

                                !> Assigned the type of the attribute as 'gru'.
                                cm%dat(vid)%series(n)%attrtype = attr

                                !> Populate the variables for this type of series.
                                if (allocated(cm%dat(vid)%series(n)%attr)) deallocate(cm%dat(vid)%series(n)%attr)
                                allocate(cm%dat(vid)%series(n)%attr(2))
                                do jj = 2, nargs
                                    call parse_split_attr(args(jj), key, mark, attr)
                                    call value(mark, nn, ios)
                                    if (n == nn) then
                                        select case (key)
                                            case ('per1')
                                                cm%dat(vid)%series(n)%attr(1) = attr
                                            case ('per2')
                                                cm%dat(vid)%series(n)%attr(2) = attr
                                        end select
                                    end if
                                end do
!print *, 'n: ', n
!print *, 'attrtype: ', trim(cm%dat(vid)%series(n)%attrtype)
!print *, 'per1: ', trim(cm%dat(vid)%series(n)%attr(1))
!print *, 'per2: ', trim(cm%dat(vid)%series(n)%attr(2))
                            end if
                    end select
                end do
            end if
        end do

    end subroutine

    subroutine parse_seek_section(iun, name, lines, nlines, preserveCasing)

        use strings

        integer, intent(in) :: iun
        character(len = *), intent(in) :: name
        character(len = DEFAULT_LINE_LENGTH), intent(out), dimension(:), allocatable :: lines
        integer, intent(out) :: nlines
        logical, intent(in), optional :: preserveCasing

        integer :: ios = 0
        integer i, posln_end, posln_start
        character(len = len_trim(name)) lname
        character(len = DEFAULT_LINE_LENGTH) line
        logical :: lpreserveCasing = .false.

        !> Remove casing from the name.
        lname = lowercase(name)
        if (present(preserveCasing)) lpreserveCasing = preserveCasing

        !> Rewind the file.
        rewind(iun)

        !> Identify the end of the section using the header.
        posln_end = 1
        do while (ios == 0)
            call readline(iun, line, ios)
!            if (ios /= 0) then
!todo: Fix this.
!print *, 'error: end of file before end token', lname
!stop
!            end if
            if (lowercase(line) == ':end' // lname) exit
            posln_end = posln_end + 1
        end do

        !> Rewind the file.
        rewind(iun)

        !> Identify the start of the section using the header.
        posln_start = 1
        do while (ios == 0)
            call readline(iun, line, ios)
!            if (ios /= 0) then
!todo: Fix this.
!print *, 'error: end of file before start token', lname
!stop
!            end if
            if (lowercase(line) == ':start' // lname) then
                nlines = posln_end - posln_start + 1
                allocate(lines(nlines))
                if (.not. lpreserveCasing) line = lowercase(line)
                lines(1) = line
                do i = 2, nlines
                    call readline(iun, line, ios)
                    if (.not. lpreserveCasing) line = lowercase(line)
                    lines(i) = line
                end do
                exit
            end if
            posln_start = posln_start + 1
        end do

!todo: Account for end of line, missing section headers, no section lines.

    end subroutine

    subroutine parse_split_fields(line, delim, args, nargs)

        use strings

        character(len = *), intent(in) :: line
        character, intent(in) :: delim
        character(len = len(line)), dimension(:), allocatable, intent(out) :: args
        integer, intent(out) :: nargs

        integer i
        character(len = len_trim(adjustl(line))) lline

        lline = adjustl(line)
        nargs = 0
        do i = 1, len(lline)
            if (lline(i:i) == delim) nargs = nargs + 1
        end do

        allocate(args(nargs + 1))
        call parse(line, delim, args, nargs)

        do i = 1, nargs
            call removesp(args(i))
        end do

    end subroutine

    subroutine parse_split_attr(arg, key, mark, attr)

        use strings

        character(len = *), intent(in) :: arg
        character(len = DEFAULT_FIELD_LENGTH), intent(out) :: key, mark, attr

        character(len = len(arg)) :: llead, ltail

        llead = arg
        call split(llead, '=', ltail)
        attr = llead
        llead = ltail
        call split(llead, ':', ltail)
        key = ltail
        mark = llead

    end subroutine

end module
