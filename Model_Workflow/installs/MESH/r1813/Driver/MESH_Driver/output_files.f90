module output_files

    !> MESH routines and variables.
    use sa_mesh_common
    use model_dates
    use model_files_variables
    use variable_names

    !> MESH file format modules.
#ifdef NETCDF
    use nc_io
#endif

    implicit none

    !> Description:
    !>  Keys for output file formats.
    !>      To assign: += radix(key)**key
    !>      To test: btest(val, key)
!-    type output_file_formats
!-        integer :: R2C = 2
!-        integer :: SEQ = 3
!-        integer :: TXT = 4
!-        integer :: CSV = 6
!-        integer :: TSI = 7
!-        integer :: TSK = 8
!-        integer :: NC4 = 9
!-    end type

    !> Description:
    !>  Keys for output file scale.
!-    type output_file_groups
!-        integer :: GRID = 1
!-        integer :: TILE = 2
!-    end type

    !> Description:
    !>  Keys for output file frequency.
    !* STA: Static (once; for fields that do not change in time).
    !* TOT: Accrued over the simulation.
    !* DLY: Daily.
    !* MLY: Monthly.
    !* HLY: Hourly.
    !* PTS: Per time-step.
    !* YLY: Yearly.
    !* SSL: Seasonal.
!-    type output_file_freqs
!-        integer :: STA = 0
!-        integer :: TOT = 1
!-        integer :: DLY = 2
!-        integer :: MLY = 3
!-        integer :: HLY = 4
!-        integer :: PTS = 5
!-        integer :: YLY = 6
!-        integer :: SSL = 7
!-    end type

    interface write_r2c_grid
        module procedure write_r2c_grid_real
        module procedure write_r2c_grid_int
    end interface

    interface write_seq_frame
        module procedure write_seq_frame_real
        module procedure write_seq_frame_int
    end interface

    !> Description:
    !>  Data type for an output file.
    !>
    !> Variables:
    !*  iun: Unit of the file (must be unique; default: -1).
    !*  fname: Base file name (default: none).
    !*  vid: Variable ID (default: none).
    !*  tid: Time ID (default: none).
    !*  record: Current record (default: 1).
    !*  src: Source data (1: Index).
    !*  dat: Data (1: Index; 2: Time-series index).
    type output_file
        integer :: iun = -1
        character(len = DEFAULT_LINE_LENGTH) :: fname = ''
        integer :: nid = -1
        integer :: vid = -1
        integer :: tid = -1
        integer :: record = 1
        real, dimension(:), pointer :: src => null()
        real, dimension(:, :), allocatable :: dat
    end type

    !> Description:
    !>  Data type for a group of output variables (e.g., spatial scale).
    !>
    !> Variables:
    !*  fname: Base file name (default: none).
    !*  tile, grid: Output file for the group.
    type output_group
        character(len = DEFAULT_LINE_LENGTH) :: fname = ''
        type(output_file) tile, grid
    end type

    !> Description:
    !>  Data type for an output field.
    !>
    !> Variables:
    !*  vname: Variable name (default: none).
    !*  ilvl: Variable layer and/or level (default: none).
    !*  cfactorm: Multiplicative transform to apply when updating the field (default: 1.0).
    !*  cfactora: Additive transform to apply when updating the field (default: 0.0).
    !*  fn: Function to use when updating the field (e.g., 'val', 'acc', 'min', 'max'; default: none).
    !*  ffmt: Output file formats (default: none).
    !*  fgroup: Variable groups (default: none).
    !*  ffreq: Output file frequencies (default: none).
    !*  delim: Field delimiter in supported formats (default: space-delimited).
    !*  order: Print order of elements in the field in supported formats (default: 'unknown').
    !*  gru_mask: Function to use when conditioning grid outputs using GRUs (default: '').
    !*  in_mem: .true. to store in memory; .false. to write output continuously during the run (default: .false.).
    !*  apply_frac: .true. to multiply grid values by fractional cell area (default: .true.).
    !*  print_date: Option to print the leading date stamp in supported formats (default: .false.).
    !*  tsi, tsk: Indices of the specific grids and tiles for output in supported formats (text format).
    !*  gru: Indices of GRUs to filter grid based outputs.
    !*  basin_acc: Print basin accumulated values for gridded outputs (default: .false.).
    !*  y, m, s, d, h: Output groups at various output frequencies.
    type output_field
        character(len = DEFAULT_FIELD_LENGTH) :: vname = ''
        integer :: ilvl = 0
        real :: cfactorm = 1.0
        real :: cfactora = 0.0
        character(len = DEFAULT_FIELD_LENGTH) :: fn = ''
        integer :: ffmt = 0
        integer :: fgroup = 0
        integer :: ffreq = 0
        character(len = DEFAULT_FIELD_LENGTH) :: delim = ''
        character(len = DEFAULT_FIELD_LENGTH) :: order = 'unknown'
        character(len = DEFAULT_FIELD_LENGTH) :: gru_mask = ''
        integer :: pts_length = 0
        integer :: pts_aggregator = 0
        integer :: pts_counter = 0
        logical :: in_mem = .false.
        logical :: apply_frac = .false.
        logical :: print_date = .true.
        integer, dimension(:), allocatable :: tsi, tsk
        integer, dimension(:), allocatable :: gru
        logical :: basin_acc = .false.
        type(output_group) y, m, s, d, h, pts
    end type

    !> Description:
    !>  Data type for storing dates for various output frequencies.
    !>
    !> Variables:
    !*  y, m, d, h: Dates (1: Index; 2: Time-series index).
    !*  iter_s: Numbers of iterations passed for seasonal output.
    type output_dates
        integer, dimension(:, :), allocatable :: y, m, s, d, h, pts
        integer iter_s(12)
    end type

    !> Description:
    !>  Data type for storing series and variables for output.
    !>
    !> Variables:
    !*  PROCESS_ACTIVE: .true. if output files are enabled.
    !*  ffmt: File format keys.
    !*  fgroup: Output group keys.
    !*  ffreq: Output frequency keys.
    !*  iun: Base file unit (increments as fields are activated for output).
    !*  dates: Array to store frame count and date (1: Frame count and date; 2: Index in series).
    !*  in_mem: .true. to store in memory; .false. to write output continuously during the run (default: .false.).
    !*  fclose: .true. to force writing output, regardless of the state of 'in_mem' (default: .false.).
    !*  fls: Output files.
    type output_fields_container
        logical :: PROCESS_ACTIVE = .false.
!-        type(output_file_formats) ffmt
!-        type(output_file_groups) fgroup
!-        type(output_file_freqs) ffreq
        integer :: iun = 882100
        type(output_dates) dates
        logical :: in_mem = .false.
        logical :: fclose = .false.
        type(output_field), dimension(:), allocatable :: fls
    end type

    !> Instances of data types.
    !>
    !> Variables:
    !*  fls_out: Instance of variables for output.
    type(output_fields_container), save :: fls_out

    !> Description:
    !>  Interface for 'output_files_parse_indices'.
    interface output_files_parse_indices
        module procedure output_files_parse_indices_real
        module procedure output_files_parse_indices_int
    end interface

    !> Description:
    !>  Interface for 'output_files_append_field'.
    interface output_files_append_field
        module procedure output_files_append_field
        module procedure output_files_append_pfield
    end interface

    contains

!>>>>temporarily_here
    !> Description:
    !>  Open an existing text or CSV format file for input (the routine
    !>  implements the 'read' action). Returns 'iostat' of the open
    !>  statement.
    !>
    !> Input variables:
    !*  iun: Unit of the file (stream persists).
    !*  fname: Name of the file.
    !>
    !> Output variables:
    !*  ierr: Returns '0' if the routine is successful.
    subroutine open_txt_input(iun, fname, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Open the file.
        !> Print status to the screen if verbose.
        open(iun, file = adjustl(fname), action = 'read', status = 'old', iostat = ierr)

        return

    end subroutine

    !> Description:
    !>  Return the first line of data from a text or CSV format file.
    !>  The routine skips lines that lead with '#' or '!', and clip the
    !>  line to the first instance of '#' or '!' if one exists. The
    !>  returned line is de-tabbed and compacted of excess whitespace.
    !>
    !> Input variables.
    !*  iun: Unit of the file (of persistent stream).
    !>
    !> Output variables.
    !*  line: First data line read from file.
    !*  ierr: Returns '0' if the routine is successful.
    !*  i: Lines passed in file.
    integer function read_txt_line(iun, line, ierr) result(i)

        !> Required for the 'compact' function.
        use strings

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables.
        character(len = *), intent(out) :: line
        integer, intent(out) :: ierr

        !> Loop until the first line of data is read.
        !> Skip blank lines and lines that lead with '#' or '!'.
        !> Clip the line to the first instance of '#' or '!' if one exists.
        line = ''
        i = 0
        ierr = 0
        do while (ierr == 0)
            read(iun, '(a)', iostat = ierr) line
            i = i + 1
            if (ierr /= 0) exit
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
            if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
            call compact(line)
            if (len_trim(line) > 0) exit
        end do

        return

    end function

    subroutine open_r2c_output(fls, shd, iun, fname, attname, attunits, ierr)

        !> 'strings': For 'uppercase' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attname, attunits

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
!-        integer dat(8)
        character(len = DEFAULT_LINE_LENGTH) line

        !> Open the file (write access).
        ierr = 0
        open(iun, file = fname, status = 'replace', form = 'formatted', action = 'write', iostat = ierr)
        if (ierr /= 0) return

        !> Write header.
        write(iun, FMT_CHR) repeat('#', 73)
        write(iun, FMT_CHR) ':FileType r2c ASCII EnSim 1.0 '
        write(iun, FMT_CHR) '#'
        write(iun, FMT_CHR) '# DataType 2D Rect Cell'
        write(iun, FMT_CHR) '#'
        write(iun, FMT_CHR) ':Application SA_MESH'
        write(iun, FMT_CHR) ':Version 1.4'
        write(iun, FMT_CHR) '#'
!-        call date_and_time(values = dat)
        write(line, "(i4.4, '-', i2.2, '-', i2.2, 1x, i2.2, ':', i2.2, ':', i2.2)") &
            ic%run_start%year, ic%run_start%month, ic%run_start%day, ic%run_start%hour, ic%run_start%mins, 0
        write(iun, FMT_CHR) ':CreationDate ' // trim(adjustl(line))
        write(iun, FMT_CHR) '#'
        write(iun, FMT_CHR) '#' // repeat('-', 72)
        write(iun, FMT_CHR) '#'
        write(iun, FMT_CHR) ':Name ' // trim(attname)
        write(iun, FMT_CHR) '#'
        write(iun, FMT_CHR) ':Projection ' // trim(uppercase(shd%CoordSys%Proj))
        write(iun, FMT_CHR) ':Ellipsoid ' // trim(uppercase(shd%CoordSys%Ellips))
        select case (lowercase(shd%CoordSys%Proj))
            case ('latlong')
            case ('utm')
                write(iun, FMT_CHR) ':Zone ' // trim(uppercase(shd%CoordSys%Zone))
            case ('rotlatlong')
                write(line, FMT_GEN) shd%CoordSys%CentreLatitude
                write(iun, FMT_CHR) ':CentreLatitude ' // trim(adjustl(line))
                write(line, FMT_GEN) shd%CoordSys%CentreLongitude
                write(iun, FMT_CHR) ':CentreLongitude ' // trim(adjustl(line))
                write(line, FMT_GEN) shd%CoordSys%RotationLatitude
                write(iun, FMT_CHR) ':RotationLatitude ' // trim(adjustl(line))
                write(line, FMT_GEN) shd%CoordSys%RotationLongitude
                write(iun, FMT_CHR) ':RotationLongitude ' // trim(adjustl(line))
            case default
                call print_warning( &
                    "Unknown or unsupported projection '" // trim(shd%CoordSys%Proj) // "' for file: " // trim(fname))
                ierr = 1
                return
        end select
        write(iun, FMT_CHR) '#'
        write(line, FMT_GEN) shd%xOrigin
        write(iun, FMT_CHR) ':xOrigin ' // trim(adjustl(line))
        write(line, FMT_GEN) shd%yOrigin
        write(iun, FMT_CHR) ':yOrigin ' // trim(adjustl(line))
        write(iun, FMT_CHR) '#'
        if (index(attname, '/', back = .true.) > 0) then
            line = attname((index(attname, '/', back = .true.) + 1):)
        else
            line = attname
        end if
        write(iun, FMT_CHR) ':AttributeName 1 ' // trim(line)
        write(iun, FMT_CHR) '#'
        write(line, FMT_GEN) shd%xCount
        write(iun, FMT_CHR) ':xCount ' // trim(line)
        write(line, FMT_GEN) shd%yCount
        write(iun, FMT_CHR) ':yCount ' // trim(line)
        write(line, FMT_GEN) shd%xDelta
        write(iun, FMT_CHR) ':xDelta ' // trim(line)
        write(line, FMT_GEN) shd%yDelta
        write(iun, FMT_CHR) ':yDelta ' // trim(line)
        write(iun, FMT_CHR) '#'
        write(iun, FMT_CHR) ':EndHeader'

    end subroutine

    subroutine open_seq_output(fls, iun, fname, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Open the file (write access).
        ierr = 0
        open(iun, file = fname, status = 'replace', form = 'unformatted', action = 'write', access = 'sequential', iostat = ierr)

    end subroutine

    subroutine open_txt_output(fls, iun, fname, ierr, cols)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname
        character(len = *), dimension(:), intent(in), optional :: cols

        !> Output variables.
        integer, intent(out) :: ierr

        !> Open the file (write access).
        ierr = 0
        open(iun, file = fname, status = 'replace', form = 'formatted', action = 'write', iostat = ierr)

    end subroutine

    subroutine write_r2c_frame_start(iun, number, year, month, day, hour, mins, ierr)

        !> Input variables.
        integer, intent(in) :: iun, number, year, month, day, hour, mins

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write frame number and time-stamp.
        !> Standard format for 'r2c': "yyyy/MM/dd HH:mm:ss.SSS"
        ierr = 0
        write(iun, "(':Frame', 2i10, 3x, (a), i4.4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000', (a))", iostat = ierr) &
            number, number, """", year, month, day, hour, mins, """"

    end subroutine

    subroutine write_r2c_frame_end(iun, ierr)

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write frame end.
        ierr = 0
        write(iun, "(':EndFrame')", iostat = ierr)

    end subroutine

    subroutine write_r2c_grid_real(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer j, i

        !> Write data.
        do j = 1, size(dat, 1)
            write(iun, FMT_GEN, iostat = ierr) (dat(j, i), i = 1, size(dat, 2))
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine write_r2c_grid_int(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        integer, dimension(:, :), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer j, i

        !> Write data.
        do j = 1, size(dat, 1)
            write(iun, FMT_GEN, iostat = ierr) (dat(j, i), i = 1, size(dat, 2))
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine write_seq_frame_real(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        real, dimension(:), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write output.
        ierr = 0
        write(iun, iostat = ierr) dat

    end subroutine

    subroutine write_seq_frame_int(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        integer, dimension(:), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write output.
        ierr = 0
        write(iun, iostat = ierr) dat

    end subroutine

    subroutine output_files_write_r2c(fls, shd, iun, dat, dates, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, j, i
        real, dimension(shd%yCount, shd%xCount) :: r2c_grid

        !> Write series.
        ierr = 0
        do t = 1, size(dat, 2)

            !> Transfer data to temporary variable.
            r2c_grid = out%NO_DATA
            do i = 1, shd%NA
                r2c_grid(shd%yyy(i), shd%xxx(i)) = dat(i, t)
            end do

            !> Write data.
            call write_r2c_frame_start(iun, dates(1, t), dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t), ierr)
            if (ierr /= 0) return
            call write_r2c_grid(iun, r2c_grid, ierr)
            if (ierr /= 0) return
            call write_r2c_frame_end(iun, ierr)
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine output_files_write_seq(fls, iun, dat, dates, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, seq_dates(7)

        !> Write series.
        ierr = 0
        do t = 1, size(dat, 2)

            !> Write frame number (N) and time-stamp.
            !> Extended format for 'seq': N N yyyy MM dd HH mm ss SSS
            seq_dates(1) = dates(1, t)
            seq_dates(2) = dates(1, t)
            seq_dates(3) = dates(2, t)
            seq_dates(4) = dates(3, t)
            seq_dates(5) = dates(4, t)
            seq_dates(6) = dates(5, t)
            seq_dates(7) = dates(6, t)
            call write_seq_frame(iun, seq_dates, ierr)
            if (ierr /= 0) return

            !> Write data.
            call write_seq_frame(iun, dat(:, t), ierr)
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine output_files_write_txt(fls, shd, field, iun, dat, dates, ierr, sep)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates
        character(len = *), intent(in), optional :: sep

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, j, i
        real dat_grid(shd%yCount*shd%xCount), dat_tsi(size(field%tsi)), dat_tsk(size(field%tsk))
        character(len = DEFAULT_FIELD_LENGTH) delim, fmt

        !> Assign the delimiter.
        fmt = FMT_GEN
        delim = ''
        if (present(sep)) then
            select case (sep)
                case (',')
                    fmt = FMT_CSV
                    delim = trim(sep)
            end select
        end if

        !> Write series.
        do t = 1, size(dat, 2)

            !> Lead line with date if 'print_date' option is enabled.
            if (field%print_date) then
                ierr = 0
                write(iun, "((a), i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000', (a))", advance = 'no', iostat = ierr) &
                    """", dates(2:6, t), """" // trim(delim)
                if (ierr /= 0) return
            end if

            !> Determine order of cells for output.
            ierr = 0
            select case (field%order)

                !> Write for specific grids.
                case ('tsi')
                    do i = 1, size(field%tsi)
                        if (field%tsi(i) > 0 .and. field%tsi(i) <= size(dat, 1)) then
                            dat_tsi(i) = dat(field%tsi(i), t)
                        else
                            dat_tsi(i) = out%NO_DATA
                        end if
                    end do
                    write(iun, fmt, iostat = ierr) dat_tsi
                    if (ierr /= 0) return

                !> Write for specific tiles.
                case ('tsk')
                    do i = 1, size(field%tsk)
                        if (field%tsk(i) > 0 .and. field%tsk(i) <= size(dat, 1)) then
                            dat_tsk(i) = dat(field%tsk(i), t)
                        else
                            dat_tsk(i) = out%NO_DATA
                        end if
                    end do
                    write(iun, fmt, iostat = ierr) dat_tsk
                    if (ierr /= 0) return

                !> Write by order of 'r2c' grid (e.g., by shd%yCount, then by shd%xCount in a single line).
                case ('shedorder')

                    !> Transfer data to temporary array.
                    dat_grid = out%NO_DATA
                    do i = 1, shd%NA
                        dat_grid((shd%yyy(i) - 1)*shd%yCount + shd%xxx(i)) = dat(i, t)
                    end do

                    !> Write data (in a single line).
                    write(iun, fmt, iostat = ierr) dat_grid
                    if (ierr /= 0) return

                !> Write by order of RANK (e.g., 1:shd%NA).
                case default
                    write(iun, fmt, iostat = ierr) dat(:, t)
                    if (ierr /= 0) return
            end select
        end do

    end subroutine

    subroutine write_r2c_binary_line(iun, record, line, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: line

        !> Input/output variables.
        integer record

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer l, r, m
        character(len = 4) t

        !> Initialize the return variable.
        ierr = 0

        !> Find how many 4-byte characters exist in the line (considering space for EOL character).
        m = len_trim(line) - mod(len_trim(line), 4) + 1

        !> Write the even 4-byte substrings to the line.
        l = 1
        do while (l < m)
            r = l + 3
            write(iun, rec = record, iostat = ierr) line(l:r); record = record + 1
            if (ierr /= 0) return
            l = r + 1
        end do

        !> Write the remaining substring for the end of the line.
        t = ''
        if (l <= len_trim(line)) t = line(l:len_trim(line))

        !> Pad end of line with spaces.
        !> Adapted from 'wat_ensim.f'.
        do l = len_trim(t) + 1, 3
            t(l:l) = ' '
        end do

        !> Add the end of line (EOL) character.
        t(4:4) = char(10)

        !> Write the end of the line.
        write(iun, rec = record, iostat = ierr) t; record = record + 1

    end subroutine

    subroutine open_r2c_binary_output(fls, shd, iun, fname, record, attname, attunits, ierr)

        !> 'strings': For 'uppercase' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attname, attunits

        !> Input/output variables.
        integer record

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        real(kind = 4) d
!-        integer dat(8)
        integer r, z
        character(len = DEFAULT_LINE_LENGTH) line

        !> Initialize the return variable.
        ierr = 0

        !> Determine record length.
        inquire(iolength = r) d

        !> Open the file (write access).
        z = 0
        open(iun, file = fname, status = 'replace', form = 'unformatted', access = 'direct', recl = r, action = 'write', iostat = z)

        !> Write header.
        if (z == 0) call write_r2c_binary_line(iun, record, repeat('#', 73), z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':FileType r2c BINARY EnSim 1.0 ', z)
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) call write_r2c_binary_line(iun, record, '# DataType 2D Rect Cell', z)
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':Application SA_MESH', z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':Version 1.4', z)
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) then
!-            call date_and_time(values = dat)
            write(line, "(i4.4, '-', i2.2, '-', i2.2, 1x, i2.2, ':', i2.2, ':', i2.2)") &
                ic%run_start%year, ic%run_start%month, ic%run_start%day, ic%run_start%hour, ic%run_start%mins, 0
            call write_r2c_binary_line(iun, record, ':CreationDate ' // trim(adjustl(line)), z)
        end if
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) call write_r2c_binary_line(iun, record, '#' // repeat('-', 72), z)
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':Name ' // trim(attname), z)
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':Projection ' // trim(uppercase(shd%CoordSys%Proj)), z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':Ellipsoid ' // trim(uppercase(shd%CoordSys%Ellips)), z)
        select case (lowercase(shd%CoordSys%Proj))
            case ('latlong')
            case ('utm')
                if (z == 0) call write_r2c_binary_line(iun, record, ':Zone ' // trim(uppercase(shd%CoordSys%Zone)), z)
            case ('rotlatlong')
                write(line, FMT_GEN) shd%CoordSys%CentreLatitude
                if (z == 0) call write_r2c_binary_line(iun, record, ':CentreLatitude ' // trim(adjustl(line)), z)
                write(line, FMT_GEN) shd%CoordSys%CentreLongitude
                if (z == 0) call write_r2c_binary_line(iun, record, ':CentreLongitude ' // trim(adjustl(line)), z)
                write(line, FMT_GEN) shd%CoordSys%RotationLatitude
                if (z == 0) call write_r2c_binary_line(iun, record, ':RotationLatitude ' // trim(adjustl(line)), z)
                write(line, FMT_GEN) shd%CoordSys%RotationLongitude
                if (z == 0) call write_r2c_binary_line(iun, record, ':RotationLongitude ' // trim(adjustl(line)), z)
            case default
                call print_warning( &
                    "Unknown or unsupported projection '" // trim(shd%CoordSys%Proj) // "' for file: " // trim(fname))
                ierr = 1
                return
        end select
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) then
            write(line, FMT_GEN) shd%xOrigin
            call write_r2c_binary_line(iun, record, ':xOrigin ' // trim(adjustl(line)), z)
        end if
        if (z == 0) then
            write(line, FMT_GEN) shd%yOrigin
            call write_r2c_binary_line(iun, record, ':yOrigin ' // trim(adjustl(line)), z)
        end if
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) then
            if (index(attname, '/', back = .true.) > 0) then
                line = attname((index(attname, '/', back = .true.) + 1):)
            else
                line = attname
            end if
            call write_r2c_binary_line(iun, record, ':AttributeName 1 ' // trim(line), z)
        end if
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) then
            write(line, FMT_GEN) shd%xCount
            call write_r2c_binary_line(iun, record, ':xCount ' // trim(line), z)
        end if
        if (z == 0) then
            write(line, FMT_GEN) shd%yCount
            call write_r2c_binary_line(iun, record, ':yCount ' // trim(line), z)
        end if
        if (z == 0) then
            write(line, FMT_GEN) shd%xDelta
            call write_r2c_binary_line(iun, record, ':xDelta ' // trim(line), z)
        end if
        if (z == 0) then
            write(line, FMT_GEN) shd%yDelta
            call write_r2c_binary_line(iun, record, ':yDelta ' // trim(line), z)
        end if
        if (z == 0) call write_r2c_binary_line(iun, record, '#', z)
        if (z == 0) call write_r2c_binary_line(iun, record, ':EndHeader', z)

        !> Update the return variable.
        ierr = z

    end subroutine

    subroutine write_r2c_binary_frame_start(iun, record, number, year, month, day, hour, mins, ierr)

        !> Input variables.
        integer, intent(in) :: iun, number, year, month, day, hour, mins

        !> Input/output variables.
        integer record

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer(kind = 4) f, y, m, d, h, n, s
        integer z

        !> Initialize the return variable.
        ierr = 0

        !> Transfer to local variables.
        f = int(number, 4)
        y = int(year, 4)
        m = int(month, 4)
        d = int(day, 4)
        h = int(hour, 4)
        n = int(mins, 4)
        s = int(0, 4)

        !> Write frame number and time-stamp.
        !> Adapted from 'wat_ensim.f'.
        !> Standard format for 'r2c' (binary): frame, frame, year, month, day, hour, minutes, seconds, milliseconds (as 4-byte int).
        z = 0
        if (z == 0) then
            write(iun, rec = record, iostat = z) f
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) f
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) y
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) m
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) d
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) h
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) n
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) s
            record = record + 1
        end if
        if (z == 0) then
            write(iun, rec = record, iostat = z) s
            record = record + 1
        end if

        !> Update the return variable.
        ierr = z

    end subroutine

    subroutine write_r2c_binary_grid_real(iun, record, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat

        !> Input/output variables.
        integer record

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        real(kind = 4), dimension(size(dat, 1), size(dat, 2)) :: d
        integer j, i

        !> Initialize the return variable.
        ierr = 0

        !> Transfer to local variables.
        d = real(dat, 4)

        !> Write data.
        do j = 1, size(d, 1)
            do i = 1, size(d, 2)
                write(iun, rec = record, iostat = ierr) d(j, i); record = record + 1
                if (ierr /= 0) return
            end do
        end do

    end subroutine

    subroutine output_files_write_r2c_binary(fls, shd, iun, dat, record, dates, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Input/output variables.
        integer record

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, j, i
        real, dimension(shd%yCount, shd%xCount) :: r2c_grid

        !> Write series.
        ierr = 0
        do t = 1, size(dat, 2)

            !> Transfer data to temporary variable.
            r2c_grid = out%NO_DATA
            do i = 1, shd%NA
                r2c_grid(shd%yyy(i), shd%xxx(i)) = dat(i, t)
            end do

            !> Write data.
            call write_r2c_binary_frame_start( &
                iun, record, dates(1, t), dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t), ierr)
            if (ierr /= 0) return
            call write_r2c_binary_grid_real(iun, record, r2c_grid, ierr)
            if (ierr /= 0) return
        end do

    end subroutine
!<<<<<temporarily_here

    subroutine output_files_allocate_group(fls, shd, ffreq, out_group, field, group, t, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: ffreq, t
        type(output_series), intent(in) :: out_group

        !> Input/output variables.
        type(output_field) field
        type(output_group) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer iun, j, n, z
        character(len = DEFAULT_LINE_LENGTH) fname
        character(len = DEFAULT_FIELD_LENGTH) str
        logical lopen

        !> Create base file name for group.
        fname = trim(group%fname)
        if (field%ilvl > 0) then
            write(str, '(i6)') field%ilvl
            fname = trim(fname) // '_' // VN_IG // trim(adjustl(str))
        end if
        ierr = 0

        !> Grid-based.
        if (btest(field%fgroup, DATA_TYPE_GRID)) then

            !> Allocate 'dat' and assign 'src'.
            allocate(group%grid%dat(shd%NA, t))
            group%grid%dat = out%NO_DATA
            if (field%basin_acc) then
                if (field%ilvl > 0) then
                    call output_variables_activate_pntr(out_group%basin, field%vname, group%grid%src, field%ilvl)
                else
                    call output_variables_activate_pntr(out_group%basin, field%vname, group%grid%src)
                end if
                fname = trim(fname) // '_basin_acc'
            else
                if (field%ilvl > 0) then
                    call output_variables_activate_pntr(out_group%grid, field%vname, group%grid%src, field%ilvl)
                else
                    call output_variables_activate_pntr(out_group%grid, field%vname, group%grid%src)
                end if
            end if

            !> File name.
            if (len_trim(field%gru_mask) > 0) then
                n = 0
                select case (field%gru_mask)
                    case ('gru_include')
                        if (size(field%gru) >= 1) n = size(field%gru)
                    case ('gru_exclude')
                        if (size(field%gru) >= 1) n = size(field%gru)
                        fname = trim(fname) // '_not'
                    case default
                        if (size(field%gru) >= 1) n = 1
                end select
                if (n > 0) then
                    do j = 1, n
                        write(str, '(i6)') field%gru(j)
                        fname = trim(fname) // '_' // VN_GRU // trim(adjustl(str))
                    end do
                end if
            end if
            group%grid%fname = fname

            !> Assign file unit and open files.
            fls_out%iun = fls_out%iun + 1
            group%grid%iun = fls_out%iun
            iun = 0
            if (btest(field%ffmt, FILE_TYPE_R2C)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.r2c', opened = lopen)
                if (.not. lopen) then
                    call open_r2c_output(fls, shd, group%grid%iun + iun, trim(fname) // '.r2c', trim(fname), '', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // ".r2c")
                        ierr = z
                    end if
                else
                    call print_error("Another output variable has already opened the file: " // trim(fname) // ".r2c")
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_SEQ)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.seq', opened = lopen)
                if (.not. lopen) then
                    call open_seq_output(fls, group%grid%iun + iun, trim(fname) // '.seq', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // ".seq")
                        ierr = z
                    end if
                else
                    call print_error("Another output variable has already opened the file: " // trim(fname) // ".seq")
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_TXT)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.txt', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%grid%iun + iun, trim(fname) // '.txt', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // ".txt")
                        ierr = z
                    end if
                else
                    call print_error("Another output variable has already opened the file: " // trim(fname) // ".txt")
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_CSV)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.csv', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%grid%iun + iun, trim(fname) // '.csv', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // ".csv")
                        ierr = z
                    end if
                else
                    call print_error("Another output variable has already opened the file: " // trim(fname) // ".csv")
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_TSI)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '_' // VN_GRD // '.ts', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%grid%iun + iun, trim(fname) // '_' // VN_GRD // '.ts', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // "_" // VN_GRD // ".ts")
                        ierr = z
                    end if
                else
                    call print_error( &
                        "Another output variable has already opened the file: " // trim(fname) // "_" // VN_GRD // ".ts")
                    z = 1
                end if
                iun = iun + 1
            end if
#ifdef NETCDF
            if (btest(field%ffmt, FILE_TYPE_NC4)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '_' // VN_GRD // '.nc', opened = lopen)
                if (.not. lopen) then
                    call nc4_define_output_variable_xyt( &
                        trim(fname) // '_' // VN_GRD // '.nc', field%vname, shd%CoordSys%Proj, ffreq, &
                        shd%CoordSys%lat, shd%CoordSys%lon, shd%CoordSys%rlat, shd%CoordSys%rlon, &
                        shd%CoordSys%xylat, shd%CoordSys%xylon, &
                        shd%CoordSys%Ellips, shd%CoordSys%Zone, shd%CoordSys%earth_radius, shd%CoordSys%grid_north_pole_latitude, &
                        shd%CoordSys%grid_north_pole_longitude, &
                        quiet = .true., fill = out%NO_DATA, &
                        vid = group%grid%vid, vtime = group%grid%tid, iun = group%grid%nid, ierr = z)
                    if (ierr /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // "_" // VN_GRD // ".nc")
                        ierr = z
                    end if
                else
                    call print_error( &
                        "Another output variable has already opened the file: " // trim(fname) // "_" // VN_GRD // ".nc")
                    z = 1
                end if
            end if
#endif
            if (btest(field%ffmt, FILE_TYPE_R2C_BIN)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '_binary.r2c', opened = lopen)
                if (.not. lopen) then
                    call open_r2c_binary_output( &
                        fls, shd, group%grid%iun + iun, trim(fname) // '_binary.r2c', group%grid%record, trim(fname), '', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // "_binary.r2c")
                        ierr = z
                    end if
                else
                    call print_error("Another output variable has already opened the file: " // trim(fname) // "_binary.r2c")
                    z = 1
                end if
                iun = iun + 1
            end if
            if (z /= 0) ierr = z

            !> Update shared file unit.
            fls_out%iun = fls_out%iun + iun
        end if

        !> Tile-based.
        if (btest(field%fgroup, DATA_TYPE_TILE)) then

            !> Base file unit.

            !> Allocate 'dat' and assign 'src'.
            allocate(group%tile%dat(shd%lc%NML, t))
            group%tile%dat = out%NO_DATA
            if (field%ilvl > 0) then
                call output_variables_activate_pntr(out_group%tile, field%vname, group%tile%src, field%ilvl)
            else
                call output_variables_activate_pntr(out_group%tile, field%vname, group%tile%src)
            end if

            !> File name.
            group%tile%fname = fname

            !> Assign file unit and open files.
            fls_out%iun = fls_out%iun + 1
            group%tile%iun = fls_out%iun
            iun = 0
            if (btest(field%ffmt, FILE_TYPE_TSK)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '_' // VN_NML // '.ts', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%tile%iun + iun, trim(fname) // '_' // VN_NML // '.ts', z)
                    if (z /= 0) then
                        call print_error("Unable to open file for output: " // trim(fname) // "_" // VN_NML // ".ts")
                        ierr = z
                    end if
                else
                    call print_error( &
                        "Another output variable has already opened the file: " // trim(fname) // "_" // VN_NML // ".ts")
                    z = 1
                end if
                iun = iun + 1
            end if
            if (z /= 0) ierr = z

            !> Update shared file unit.
            fls_out%iun = fls_out%iun + iun
        end if

    end subroutine

    subroutine output_files_allocate_field(fls, shd, ts, field, ierr)

        !> 'strings': For 'uppercase' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts

        !> Input/output variables.
        type(output_field) field

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, z
        character(len = DEFAULT_LINE_LENGTH) fname
        character(len = DEFAULT_FIELD_LENGTH) label

        !> Set 't = 1' for the case when 'in_mem' is not active.
        t = 1

        !> Create base file name for field.
        fname = trim(fls%GENDIR_OUT) // '/' // trim(field%vname)

        !> Allocate data fields to existing output variable frequencies.
        ierr = 0
        if (btest(field%ffreq, FREQ_YEARLY)) then
            if (field%in_mem) t = ts%nyears
            field%y%fname = trim(fname) // '_Y'
            call output_files_allocate_group(fls, shd, FREQ_YEARLY, out%y, field, field%y, t, z)
            if (z /= 0) ierr = z
        end if
        if (btest(field%ffreq, FREQ_MONTHLY)) then
            if (field%in_mem) t = ts%nmonths
            field%m%fname = trim(fname) // '_M'
            call output_files_allocate_group(fls, shd, FREQ_MONTHLY, out%m, field, field%m, t, z)
            if (z /= 0) ierr = z
        end if
        if (btest(field%ffreq, FREQ_DAILY)) then
            if (field%in_mem) t = ts%nr_days
            field%d%fname = trim(fname) // '_D'
            call output_files_allocate_group(fls, shd, FREQ_DAILY, out%d, field, field%d, t, z)
            if (z /= 0) ierr = z
        end if
        if (btest(field%ffreq, FREQ_HOURLY)) then
            if (field%in_mem) t = ts%nr_days*24
            field%h%fname = trim(fname) // '_H'
            call output_files_allocate_group(fls, shd, FREQ_HOURLY, out%h, field, field%h, t, z)
            if (z /= 0) ierr = z
        end if

        !> Per 'n' time-steps (user-defined).
        if (btest(field%ffreq, FREQ_PTS)) then
            write(label, FMT_GEN) field%pts_length
            field%pts%fname = trim(fname) // '_PTS-' // trim(adjustl(label)) // 'M_' // trim(field%fn)
            call output_files_allocate_group(fls, shd, FREQ_PTS, out%ts, field, field%pts, t, z)
            if (z /= 0) ierr = z
        end if

        !> 'Seasonal' must go last because it changes 't' regardless of the state of 'in_mem'.
        if (btest(field%ffreq, FREQ_SEASONAL)) then
            t = 12
            field%s%fname = trim(fname) // '_S'
            call output_files_allocate_group(fls, shd, FREQ_SEASONAL, out%m, field, field%s, t, z)
            if (z /= 0) ierr = z
        end if

    end subroutine

    subroutine output_files_parse_indices_real(args, nargs, indices, startindex, ierr)

        !> strings: For 'is_letter' and 'value' functions.
        use strings

        !> Input variables.
        character(len = *), dimension(:), intent(in) :: args
        integer, intent(in) :: nargs, startindex

        !> Output variables.
        real, dimension(:), allocatable, intent(out) :: indices
        integer, intent(out) :: ierr

        !> Local variables.
        integer n, j

        !> Count the number of indices.
        n = 0
        do j = startindex + 1, nargs
            if (is_letter(args(j))) exit
            n = n + 1
        end do

        !> Allocate the vector and store the indices.
        ierr = 0
        if (n > 0) then
            if (allocated(indices)) deallocate(indices)
            allocate(indices(n))
            do j = 1, n
                call value(args(startindex + j), indices(j), ierr)
            end do
        end if

    end subroutine

    subroutine output_files_parse_indices_int(args, nargs, indices, startindex, ierr)

        !> strings: For 'is_letter' and 'value' functions.
        use strings

        !> Input variables.
        character(len = *), dimension(:), intent(in) :: args
        integer, intent(in) :: nargs, startindex

        !> Output variables.
        integer, dimension(:), allocatable, intent(out) :: indices
        integer, intent(out) :: ierr

        !> Local variables.
        real, dimension(:), allocatable :: r

        !> Call 'real' function.
        call output_files_parse_indices_real(args, nargs, r, startindex, ierr)

        !> Convert 'r' to integer 'indices'.
        if (allocated(indices)) deallocate(indices)
        allocate(indices(size(r)))
        indices = int(r)

    end subroutine

    subroutine output_files_parse_options(fls, shd, ts, field, args, nargs, ierr)

        !> strings: For 'is_letter', 'lowercase', and 'value' functions.
        use strings

        !> Process modules.
        use permafrost_outputs_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        character(len = *), intent(in) :: args(:)
        integer, intent(in) :: nargs

        !> Input/output variables.
        type(output_field) field

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n, j, i, z
        character(len = DEFAULT_FIELD_LENGTH) str

        !> Mark the field as active and assign default attributes.
        field%ffmt = 0
        field%ffreq = 0
        field%fgroup = 0

        !> Condition output based on 'args' flags.
        ierr = 0
        z = 0
        do i = 2, nargs

            !> Skip numeric options for arguments.
            if (.not. is_letter(args(i))) cycle

            !> Parse options.
            str = lowercase(args(i))

            !> Multi-word options.
            if (str(1:4) == 'pts=') then

                !> Per 'n' time-steps (user-defined in minutes).
                call value(str(5:), field%pts_length, z)
                if (z == 0 .and. .not. btest(field%ffreq, FREQ_PTS)) then
                    field%ffreq = field%ffreq + radix(FREQ_PTS)**FREQ_PTS
                end if
            else

                !> Single-word options.
                select case (str)

                    !> File output frequencies.
                    case ('y')
                        if (.not. btest(field%ffreq, FREQ_YEARLY)) then
                            field%ffreq = field%ffreq + radix(FREQ_YEARLY)**FREQ_YEARLY
                        end if
                    case ('m')
                        if (.not. btest(field%ffreq, FREQ_MONTHLY)) then
                            field%ffreq = field%ffreq + radix(FREQ_MONTHLY)**FREQ_MONTHLY
                        end if
                    case ('s')
                        if (.not. btest(field%ffreq, FREQ_MONTHLY)) then
                            field%ffreq = field%ffreq + radix(FREQ_MONTHLY)**FREQ_MONTHLY
                        end if
                        if (.not. btest(field%ffreq, FREQ_SEASONAL)) then
                            field%ffreq = field%ffreq + radix(FREQ_SEASONAL)**FREQ_SEASONAL
                        end if
                    case ('d')
                        if (.not. btest(field%ffreq, FREQ_DAILY)) then
                            field%ffreq = field%ffreq + radix(FREQ_DAILY)**FREQ_DAILY
                        end if
                    case ('h')
                        if (.not. btest(field%ffreq, FREQ_HOURLY)) then
                            field%ffreq = field%ffreq + radix(FREQ_HOURLY)**FREQ_HOURLY
                        end if

                    !> File formats.
                    case ('r2c')
                        if (.not. btest(field%ffmt, FILE_TYPE_R2C)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_R2C)**FILE_TYPE_R2C
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                    case ('seq', 'binseq')
                        if (.not. btest(field%ffmt, FILE_TYPE_SEQ)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                    case ('txt')
                        if (.not. btest(field%ffmt, FILE_TYPE_TXT)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_TXT)**FILE_TYPE_TXT
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                    case ('csv')
                        if (.not. btest(field%ffmt, FILE_TYPE_CSV)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_CSV)**FILE_TYPE_CSV
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                    case ('nc', 'netcdf', 'nc4')
#ifdef NETCDF
                        if (.not. btest(field%ffmt, FILE_TYPE_NC4)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_NC4)**FILE_TYPE_NC4
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
#else
                        call print_error( &
                            "NetCDF format is specified for '" // trim(field%vname) // "' but the module is not active." // &
                            ' A version of MESH compiled with the NetCDF library must be used to create files in this format.')
                        ierr = 1
                        return
#endif
                    case ('r2c_bin', 'r2c_binary')
                        if (.not. btest(field%ffmt, FILE_TYPE_R2C_BIN)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_R2C_BIN)**FILE_TYPE_R2C_BIN
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if

                    !> Order of the selection being saved.
                    case ('gridorder', 'shedorder')
                        field%order = adjustl(str)
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if

                    !> Point outputs for by grid or NML.
                    case ('tsi')
                        field%order = adjustl(str)
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                        if (.not. btest(field%ffmt, FILE_TYPE_TSI)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_TSI)**FILE_TYPE_TSI
                        end if
                        call output_files_parse_indices(args, nargs, field%tsi, i, z)
                    case ('tsk')
                        field%order = adjustl(str)
                        if (.not. btest(field%fgroup, DATA_TYPE_TILE)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_TILE)**DATA_TYPE_TILE
                        end if
                        if (.not. btest(field%ffmt, FILE_TYPE_TSK)) then
                            field%ffmt = field%ffmt + radix(FILE_TYPE_TSK)**FILE_TYPE_TSK
                        end if
                        call output_files_parse_indices(args, nargs, field%tsk, i, z)

                    !> Grid outputs conditioned by GRU.
                    case ('gru')
                        field%gru_mask = adjustl(str)
                        if (.not. btest(field%fgroup, DATA_TYPE_TILE)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_TILE)**DATA_TYPE_TILE
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                        call output_files_parse_indices(args, nargs, field%gru, i, z)
                    case ('gru_include')
                        field%gru_mask = adjustl(str)
                        if (.not. btest(field%fgroup, DATA_TYPE_TILE)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_TILE)**DATA_TYPE_TILE
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                        call output_files_parse_indices(args, nargs, field%gru, i, z)
                    case ('gru_exclude')
                        field%gru_mask = adjustl(str)
                        if (.not. btest(field%fgroup, DATA_TYPE_TILE)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_TILE)**DATA_TYPE_TILE
                        end if
                        if (.not. btest(field%fgroup, DATA_TYPE_GRID)) then
                            field%fgroup = field%fgroup + radix(DATA_TYPE_GRID)**DATA_TYPE_GRID
                        end if
                        call output_files_parse_indices(args, nargs, field%gru, i, z)

                    !> Read into memory.
                    case ('in_mem')
                        field%in_mem = .true.

                    !> Option to apply fractional cell area.
                    case ('frac', 'apply_frac')
                        field%apply_frac = .true.

                    !> Option to use basin accumulated values for gridded outputs.
                    case ('basin_acc')
                        field%basin_acc = .true.

                    !> Soil layer selection (dealt with elsewhere).
                    case ('isol')

                    !> Print leading date-stamp.
                    case ('print_date', 'printdate')
                        field%print_date = .true.
                    case ('no_date')
                        field%print_date = .false.

                    !> Function.
                    case ('acc', 'cum', 'sum')
                        field%fn = VN_ACC
                    case ('avg')
                        field%fn = VN_AVG
                    case ('max')
                        field%fn = VN_MAX
                    case ('min')
                        field%fn = VN_MIN

                    !> Permafrost option (dealt with elsewhere).
                    case ('ttol')

                    !> Not recognized.
                    case default
                        call print_remark( &
                            "The option '" // trim(adjustl(args(i))) // "' is not recognized for output" // &
                            " (Variable '" // trim(field%vname) // "').")
                end select
            end if
        end do

        !> Check for 'value' conversion error.
        if (z /= 0) then
            call print_warning("Errors occurred parsing options of '" // trim(field%vname) // "'.")
        end if

        !> Validate the configuration.
        if (field%ffmt == 0) then
            call print_warning("No supported output file formats are active for '" // trim(field%vname) // "'.")
        end if
        if (field%ffreq == 0) then
            call print_warning("No supported output file frequencies are active for '" // trim(field%vname) // "'.")
        end if
        if (allocated(field%tsi)) then
            if (size(field%tsi) == 0) then
                call print_warning( &
                    "The 'tsi' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but no grids are listed or an error occurred parsing the values.')
                deallocate(field%tsi)
                field%order = ''
            else if (maxval(field%tsi) > shd%NA .or. minval(field%tsi) < 1) then
                call print_warning( &
                    "The 'tsi' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but contains an invalid grid number' // &
                    ' or exceeds the number of grids identified in the basin.')
            end if
        end if
        if (allocated(field%tsk)) then
            if (size(field%tsk) == 0) then
                call print_warning( &
                    "The 'tsk' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but no tiles are listed or an error occurred parsing the values.')
                deallocate(field%tsk)
                field%order = ''
            else if (maxval(field%tsk) > shd%lc%NML .or. minval(field%tsk) < 1) then
                call print_warning( &
                    "The 'tsk' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but contains an invalid tile number' // &
                    ' or exceeds the number of tiles identified in the basin.')
            end if
        end if
        if (allocated(field%gru)) then
            if (field%gru_mask == 'gru' .and. size(field%gru) > 1) then
                call print_warning( &
                    "The option '" // trim(field%gru_mask) // "' (Variable '" // trim(field%vname) // "')" // &
                    ' supports filtering grid output using 1 GRU at a time.' // &
                    ' Multiple GRUs are listed. Only the first GRU in the list is used.')
            else if (size(field%gru) == 0) then
                call print_warning( &
                    "The '" // trim(field%gru_mask) // "' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but no GRUs are listed or an error occurred parsing the values.')
                deallocate(field%gru)
                field%order = ''
            else if (field%gru(1) > shd%lc%NTYPE .or. field%gru(1) < 1) then
                call print_warning( &
                    "The '" // trim(field%gru_mask) // "' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but contains an invalid GRU number' // &
                    ' or exceeds the number of GRUs identified in the basin.')
            end if
        end if
        if (len_trim(field%fn) > 0 .and. .not. btest(field%ffreq, FREQ_PTS)) then
            call print_warning( &
                "The option '" // trim(field%fn) // "' is only supported with the 'pts' option " // &
                "but the frequency format is not active for this variable " //&
                "and this modifier has no effect (Variable '" // trim(field%vname) // "').")
            field%fn = ''
        else if (btest(field%ffreq, FREQ_PTS) .and. len_trim(field%fn) == 0) then
            call print_remark( &
                "The 'pts' frequency format is active but no aggregation option is specified. " // &
                "The default aggregation option of '" // VN_ACC // "' is assumed " // &
                "(Variable '" // trim(field%vname) // "').")
            field%fn = VN_AVG
        end if

        !> Allocate output fields and open output files.
        ierr = 0
        call output_files_allocate_field(fls, shd, ts, field, ierr)

    end subroutine

    subroutine output_files_append_field(fls, shd, ts, vname, args, nargs, ierr, ignd, cfactorm, cfactora, fn)

        !> strings: For 'is_letter', 'lowercase', and 'uppercase' functions.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        character(len = *), intent(in) :: vname, args(:)
        integer, intent(in) :: nargs
        integer, intent(in), optional :: ignd
        real, intent(in), optional :: cfactorm, cfactora
        character(len = *), intent(in), optional :: fn

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer, dimension(:), allocatable :: isol
        integer n, i, j, z
        type(output_field), dimension(:), allocatable :: tmp
        character(DEFAULT_FIELD_LENGTH) str
        logical ltest

        !> Count attributes that require separate files and parse special options.
        n = 0
        z = 0
        do i = 2, nargs

            !> Skip numeric options for arguments.
            if (.not. is_letter(args(i))) cycle

            !> Pre-parse options.
            str = lowercase(args(i))

            !> Multi-word options.
            if (str(1:4) == 'pts=') then
                n = 1
            else

                !> Single-word options.
                select case (lowercase(args(i)))
                    case ('y', 'm', 's', 'd', 'h')
                        n = 1

                !> Soil layer selection (dealt with elsewhere).
                case ('isol')
                    if (present(ignd)) then
                        call output_files_parse_indices(args, nargs, isol, i, z)
                        if (z == 0) then

                            !> Check if the current layer is active for soil-related output.
                            ltest = .false.
                            do j = 1, size(isol)
                                if (isol(j) == ignd) ltest = .true.
                            end do

                            !> Return without activating the field if the current level is not active.
                            if (.not. ltest) return
                        end if
                    end if
                end select
            end if
        end do

        !> Check for 'value' conversion error.
        if (z /= 0) then
            call print_warning("Errors occurred parsing options of '" // trim(vname) // "'.")
        end if

        !> Exit if no files exist to create.
        if (n == 0) then
            call print_warning("'" // trim(args(1)) // "' contains no options that will result in output files.")
            return
        end if

        !> Expand the vector of file attributes.
        if (allocated(fls_out%fls)) then
            allocate(tmp(size(fls_out%fls) + n))
            tmp(1:size(fls_out%fls)) = fls_out%fls
            deallocate(fls_out%fls)
            allocate(fls_out%fls(size(tmp)))
            fls_out%fls = tmp
            deallocate(tmp)
        else
            allocate(fls_out%fls(n))
        end if

        !> Set the index to the last field (just created).
        n = size(fls_out%fls)

        !> Assign the variable name.
        fls_out%fls(n)%vname = adjustl(vname)
        fls_out%fls(n)%ilvl = 0
        if (present(ignd)) then
            if (ignd > 0 .and. ignd <= shd%lc%IGND) fls_out%fls(n)%ilvl = ignd
        end if
        if (present(cfactorm)) fls_out%fls(n)%cfactorm = cfactorm
        if (present(cfactora)) fls_out%fls(n)%cfactora = cfactora
        if (present(fn)) fls_out%fls(n)%fn = adjustl(fn)

        !> Assign inherited options.
        fls_out%fls(n)%in_mem = fls_out%in_mem

        !> Parse and interpret remaining options.
        ierr = 0
        call output_files_parse_options(fls, shd, ts, fls_out%fls(n), args, nargs, ierr)

    end subroutine

    subroutine output_files_append_pfield(fls, shd, ts, vname, pfld, args, nargs, ierr, ignd, cfactorm, cfactora, fn)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        character(len = *), intent(in) :: vname, args(:)
        type(output_fields_surrogate) pfld
        integer, intent(in) :: nargs
        integer, intent(in), optional :: ignd
        real, intent(in), optional :: cfactorm, cfactora
        character(len = *), intent(in), optional :: fn

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n
        type (output_field) field

        !> Call base routine.
        ierr = 0
        call output_files_append_field(fls, shd, ts, vname, args, nargs, ierr, ignd, cfactorm, cfactora, fn)
        if (ierr /= 0) return

        !> Check to see if the field was appended.
        n = size(fls_out%fls)
        if (fls_out%fls(n)%vname /= adjustl(vname)) return
        field = fls_out%fls(n)

        !> Assign 'process_group' fields to 'src' variables.
        if (btest(field%ffreq, FREQ_YEARLY)) then
            if (btest(field%fgroup, DATA_TYPE_TILE) .and. associated(pfld%y_tile)) fls_out%fls(n)%y%tile%src => pfld%y_tile
            if (btest(field%fgroup, DATA_TYPE_GRID) .and. associated(pfld%y_grid)) fls_out%fls(n)%y%grid%src => pfld%y_grid
        end if
        if (btest(field%ffreq, FREQ_MONTHLY)) then
            if (btest(field%fgroup, DATA_TYPE_TILE) .and. associated(pfld%m_tile)) fls_out%fls(n)%m%tile%src => pfld%m_tile
            if (btest(field%fgroup, DATA_TYPE_GRID) .and. associated(pfld%m_grid)) fls_out%fls(n)%m%grid%src => pfld%m_grid
        end if
        if (btest(field%ffreq, FREQ_DAILY)) then
            if (btest(field%fgroup, DATA_TYPE_TILE) .and. associated(pfld%d_tile)) fls_out%fls(n)%d%tile%src => pfld%d_tile
            if (btest(field%fgroup, DATA_TYPE_GRID) .and. associated(pfld%d_grid)) fls_out%fls(n)%d%grid%src => pfld%d_grid
        end if
        if (btest(field%ffreq, FREQ_HOURLY)) then
            if (btest(field%fgroup, DATA_TYPE_TILE) .and. associated(pfld%h_tile)) fls_out%fls(n)%h%tile%src => pfld%h_tile
            if (btest(field%fgroup, DATA_TYPE_GRID) .and. associated(pfld%h_grid)) fls_out%fls(n)%h%grid%src => pfld%h_grid
        end if
        if (btest(field%ffreq, FREQ_PTS)) then
            if (btest(field%fgroup, DATA_TYPE_TILE) .and. associated(pfld%ts_tile)) fls_out%fls(n)%pts%tile%src => pfld%ts_tile
            if (btest(field%fgroup, DATA_TYPE_GRID) .and. associated(pfld%ts_grid)) fls_out%fls(n)%pts%grid%src => pfld%ts_grid
        end if
        if (btest(field%ffreq, FREQ_SEASONAL)) then
            if (btest(field%fgroup, DATA_TYPE_TILE) .and. associated(pfld%m_tile)) fls_out%fls(n)%s%tile%src => pfld%m_tile
            if (btest(field%fgroup, DATA_TYPE_GRID) .and. associated(pfld%m_grid)) fls_out%fls(n)%s%grid%src => pfld%m_grid
        end if

    end subroutine

    subroutine output_files_init(fls, shd)

        !> strings: For 'is_letter' function.
        use strings

        !> Process modules.
        use permafrost_outputs_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        type(dates_model) ts
        integer iun, n, i, j, nargs, ios, ierr, z
        character(len = DEFAULT_FIELD_LENGTH) args(50)
        character(len = DEFAULT_LINE_LENGTH) line

        !> Return if not active.
        if (.not. fls_out%PROCESS_ACTIVE) return

        !> Initialize 'ts' variable.
        call GET_DATES(ts)

        !> Allocate output variable for time-stamps based on switch to store variables in-memory.
        if (fls_out%in_mem) then
            allocate(fls_out%dates%y(6, ts%nyears))
            allocate(fls_out%dates%m(6, ts%nmonths))
            allocate(fls_out%dates%d(6, ts%nr_days))
            allocate(fls_out%dates%h(6, ts%nr_days*24))
        else
            allocate(fls_out%dates%y(6, 1))
            allocate(fls_out%dates%m(6, 1))
            allocate(fls_out%dates%d(6, 1))
            allocate(fls_out%dates%h(6, 1))
        end if
        allocate(fls_out%dates%pts(6, 1))
        allocate(fls_out%dates%s(6, 12))
        fls_out%dates%iter_s = 0

        !> Open output fields configuration file.
        call reset_tab()
        call print_message('READING: outputs_balance.txt')
        call increase_tab()
        iun = 100
        call open_txt_input(iun, 'outputs_balance.txt', ierr)
        fls_out%fclose = .false.

        !> Stop if the routine failed.
        if (ierr /= 0) then
            call print_error('Unable to open file.')
            call print_message('Check that outputs_balance.txt exists or disabled OUTFIELDSFLAG.')
            call program_abort()
        end if

        !> Count the number of output files.
        ierr = 0
        ios = 0
        n = 0
        i = 0
        do while (ios == 0)

            !> Read the line and increment the line count.
            i = i + read_txt_line(iun, line, ios)

            !> Check for output field signature: character string followed by at least one space.
            if (len_trim(line) == 0 .or. scan(line, ' ') == 0) cycle
            if (.not. is_letter(line(1:index(line, ' ') - 1))) cycle

            !> Split the line.
            call parse(line, ' ', args, nargs)

            !> Output field.
            if (DIAGNOSEMODE) call print_message("Reading output variable: " // args(1))
            z = 0
            select case (args(1))

                !> Meteorology/climatology variables.
                case (VN_FSIN, 'FSDOWN')
                    call output_files_append_field(fls, shd, ts, VN_FSIN, args, nargs, z)
                case (VN_FSVS, 'FSVH')
                    call output_files_append_field(fls, shd, ts, VN_FSVS, args, nargs, z)
                case (VN_FSIR, 'FSIH')
                    call output_files_append_field(fls, shd, ts, VN_FSIR, args, nargs, z)
                case (VN_FSDR, 'FSDIR')
                    call output_files_append_field(fls, shd, ts, VN_FSDR, args, nargs, z)
                case (VN_FSDFF)
                    call output_files_append_field(fls, shd, ts, VN_FSDFF, args, nargs, z)
                case (VN_FSOUT)
                    call output_files_append_field(fls, shd, ts, VN_FSOUT, args, nargs, z)
                case (VN_FLIN, 'FDL')
                    call output_files_append_field(fls, shd, ts, VN_FLIN, args, nargs, z)
                case (VN_FLOUT)
                    call output_files_append_field(fls, shd, ts, VN_FLOUT, args, nargs, z)
                case (VN_TA)
                    call output_files_append_field(fls, shd, ts, VN_TA, args, nargs, z)
                case (VN_QA, 'HU')
                    call output_files_append_field(fls, shd, ts, VN_QA, args, nargs, z)
                case (VN_PRES)
                    call output_files_append_field(fls, shd, ts, VN_PRES, args, nargs, z)
                case (VN_UU)
                    call output_files_append_field(fls, shd, ts, VN_UU, args, nargs, z)
                case (VN_VV)
                    call output_files_append_field(fls, shd, ts, VN_VV, args, nargs, z)
                case (VN_UV, 'UL')
                    call output_files_append_field(fls, shd, ts, VN_UV, args, nargs, z)
                case (VN_WDIR)
                    call output_files_append_field(fls, shd, ts, VN_WDIR, args, nargs, z)
                case (VN_PRERN)
                    call output_files_append_field(fls, shd, ts, VN_PRERN, args, nargs, z)
                case (VN_PRESNO)
                    call output_files_append_field(fls, shd, ts, VN_PRESNO, args, nargs, z)
                case (VN_PRE)
                    call output_files_append_field(fls, shd, ts, VN_PRE, args, nargs, z)
                case (VN_PRECRN)
                    call output_files_append_field(fls, shd, ts, VN_PRECRN, args, nargs, z)
                case (VN_PRECSNO)
                    call output_files_append_field(fls, shd, ts, VN_PRECSNO, args, nargs, z)
                case (VN_PREC, 'Rainfall', 'Rain', 'Precipitation')
                    call output_files_append_field(fls, shd, ts, VN_PREC, args, nargs, z)

                !> Canopy variables.
                case (VN_LQWSCAN, 'RCAN')
                    call output_files_append_field(fls, shd, ts, VN_LQWSCAN, args, nargs, z)
                case (VN_FZWSCAN, 'SNCAN', 'SCAN')
                    call output_files_append_field(fls, shd, ts, VN_FZWSCAN, args, nargs, z)
                case (VN_CMAS)
                    call output_files_append_field(fls, shd, ts, VN_CMAS, args, nargs, z)
                case (VN_TCAN)
                    call output_files_append_field(fls, shd, ts, VN_TCAN, args, nargs, z)
                case (VN_GRO)
                    call output_files_append_field(fls, shd, ts, VN_GRO, args, nargs, z)

                !> Snow variables.
                case (VN_FSNO)
                    call output_files_append_field(fls, shd, ts, VN_FSNO, args, nargs, z)
                case (VN_SNO)
                    call output_files_append_field(fls, shd, ts, VN_SNO, args, nargs, z)
                case (VN_RHOSNO)
                    call output_files_append_field(fls, shd, ts, VN_RHOSNO, args, nargs, z)
                case (VN_ZSNO)
                    call output_files_append_field(fls, shd, ts, VN_ZSNO, args, nargs, z)
                case (VN_LQWSSNO, 'WSNO')
                    call output_files_append_field(fls, shd, ts, VN_LQWSSNO, args, nargs, z)
                case (VN_TSNO)
                    call output_files_append_field(fls, shd, ts, VN_TSNO, args, nargs, z)
                case (VN_ALBSNO)
                    call output_files_append_field(fls, shd, ts, VN_ALBSNO, args, nargs, z)
                case (VN_DRAINSNO, 'ROFSNO')
                    call output_files_append_field(fls, shd, ts, VN_DRAINSNO, args, nargs, z, -1, real(ic%dts))

                !> Surface variables.
                case (VN_ALBT)
                    call output_files_append_field(fls, shd, ts, VN_ALBT, args, nargs, z)
                case (VN_ALVS)
                    call output_files_append_field(fls, shd, ts, VN_ALVS, args, nargs, z)
                case (VN_ALIR)
                    call output_files_append_field(fls, shd, ts, VN_ALIR, args, nargs, z)
                case (VN_GTE)
                    call output_files_append_field(fls, shd, ts, VN_GTE, args, nargs, z)
                case (VN_ZPND)
                    call output_files_append_field(fls, shd, ts, VN_ZPND, args, nargs, z)
                case (VN_LQWSPND, 'PNDW')
                    call output_files_append_field(fls, shd, ts, VN_LQWSPND, args, nargs, z)
                case (VN_TPND)
                    call output_files_append_field(fls, shd, ts, VN_TPND, args, nargs, z)
                case (VN_PNDCAF, 'FSTR')
                    call output_files_append_field(fls, shd, ts, VN_PNDCAF, args, nargs, z)
                case (VN_POTEVP, 'PEVP')
                    call output_files_append_field(fls, shd, ts, VN_POTEVP, args, nargs, z, -1, real(ic%dts))
                case (VN_ET, 'EVAP', 'Evapotranspiration')
                    call output_files_append_field(fls, shd, ts, VN_ET, args, nargs, z, -1, real(ic%dts))
                case (VN_EVPB)
                    call output_files_append_field(fls, shd, ts, VN_EVPB, args, nargs, z)
                case (VN_ARRD)
                    call output_files_append_field(fls, shd, ts, VN_ARRD, args, nargs, z)
                case (VN_OVRFLW, 'ROFO')
                    call output_files_append_field(fls, shd, ts, VN_OVRFLW, args, nargs, z, -1, real(ic%dts))
                case (VN_QEVP, 'QE', 'LatentHeat')
                    call output_files_append_field(fls, shd, ts, VN_QEVP, args, nargs, z)
                case (VN_QSENS, 'QH', 'HFS', 'SensibleHeat')
                    call output_files_append_field(fls, shd, ts, VN_QSENS, args, nargs, z)
                case (VN_GZERO)
                    call output_files_append_field(fls, shd, ts, VN_GZERO, args, nargs, z)
                case (VN_TSURF)
                    call output_files_append_field(fls, shd, ts, VN_TSURF, args, nargs, z)

                !> Ice/glacier variables.
                case (VN_LQWSICE)
                    call output_files_append_field(fls, shd, ts, VN_LQWSICE, args, nargs, z)
                case (VN_TICE)
                    call output_files_append_field(fls, shd, ts, VN_TICE, args, nargs, z)

                !> Subsurface/soil variables.
                case (VN_THLQSOL, 'THLQ')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_THLQSOL, args, nargs, z, j)
                    end do
                case (VN_THICSOL, 'THIC')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_THICSOL, args, nargs, z, j)
                    end do
                case (VN_LQWSSOL, 'LQWS')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_LQWSSOL, args, nargs, z, j)
                    end do
                case (VN_FZWSSOL, 'FZWS', 'FRWS')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_FZWSSOL, args, nargs, z, j)
                    end do
                case (VN_ALWSSOL, 'ALWS')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_ALWSSOL, args, nargs, z, j)
                    end do
                case (VN_TSOL, 'TBAR', 'TempSoil', 'Temperature_soil_layers')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_TSOL, args, nargs, z, j)
                    end do
                case (VN_GFLX, 'HeatConduction')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_GFLX, args, nargs, z, j)
                    end do
                case (VN_LATFLW, 'ROFS')
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, VN_LATFLW, args, nargs, z, j, real(ic%dts))
                    end do
                case (VN_ZSOLSAT)
                    call output_files_append_field(fls, shd, ts, VN_ZSOLSAT, args, nargs, z, -1)
                case (VN_DRAINSOL, 'ROFB')
                    call output_files_append_field(fls, shd, ts, VN_DRAINSOL, args, nargs, z, -1, real(ic%dts))

                !> Groundwater/lower zone storage variables.
                case (VN_RCHG, 'WR_RECHARGE')
                    call output_files_append_field(fls, shd, ts, VN_RCHG, args, nargs, z)
                case (VN_STGGW, 'LZS')
                    call output_files_append_field(fls, shd, ts, VN_STGGW, args, nargs, z)
                case (VN_LKG)
                    call output_files_append_field(fls, shd, ts, VN_LKG, args, nargs, z)
!-                case (VN_DZS)
!-                    call output_files_append_field(fls, shd, ts, VN_DZS, args, nargs, z)

                !> Diagnostic variables.
                case (VN_STGE)
                    call output_files_append_field(fls, shd, ts, VN_STGE, args, nargs, z)
                case (VN_DSTGE)
                    call output_files_append_field(fls, shd, ts, VN_DSTGE, args, nargs, z)
                case (VN_STGW, 'STG')
                    call output_files_append_field(fls, shd, ts, VN_STGW, args, nargs, z)
                case (VN_DSTGW, 'DSTG')
                    call output_files_append_field(fls, shd, ts, VN_DSTGW, args, nargs, z)

                !> Routing variables.
                case (VN_RFF, 'WR_RUNOFF')
                    call output_files_append_field(fls, shd, ts, VN_RFF, args, nargs, z)
                case (VN_ROF, 'Runoff')
                    call output_files_append_field(fls, shd, ts, VN_ROF, args, nargs, z, -1, real(ic%dts))
                case (VN_QI)
                    call output_files_append_field(fls, shd, ts, VN_QI, args, nargs, z)
                case (VN_QO)
                    call output_files_append_field(fls, shd, ts, VN_QO, args, nargs, z)
                case (VN_STGCH)
                    call output_files_append_field(fls, shd, ts, VN_STGCH, args, nargs, z)
                case (VN_ZLVL)
                    call output_files_append_field(fls, shd, ts, VN_ZLVL, args, nargs, z)

                !> Permafrost outputs (PERMAFROSTOUTFLAG).
                case (PMFRSTVN_ALT)
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_ALT)
                    call output_files_append_field(fls, shd, ts, PMFRSTVN_ALT, prmfst%out%alt, args, nargs, z)
                case (PMFRSTVN_ALTDOY, 'ALT_JDAY')
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_ALTDOY)
                    call output_files_append_field(fls, shd, ts, PMFRSTVN_ALTDOY, prmfst%out%altdoy, args, nargs, z)
                case (PMFRSTVN_ALTENV)
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_ALTENV)
                    call output_files_append_field(fls, shd, ts, PMFRSTVN_ALTENV, prmfst%out%altenv, args, nargs, z)
                case (PMFRSTVN_TAVG)
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_TAVG)
                    line = VN_TSOL // '_' // VN_AVG
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, line, prmfst%out%tavg(j), args, nargs, z, j)
                    end do
                case (PMFRSTVN_TMAX)
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_TMAX)
                    line = VN_TSOL // '_' // VN_MAX
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, line, prmfst%out%tmax(j), args, nargs, z, j)
                    end do
                case (PMFRSTVN_TMIN)
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_TMIN)
                    line = VN_TSOL // '_' // VN_MIN
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, line, prmfst%out%tmin(j), args, nargs, z, j)
                    end do
                case (PMFRSTVN_TRNG, 'TENV')
                    call permafrost_outputs_init(fls, shd, PMFRSTVN_TRNG)
                    line = VN_TSOL // '_' // VN_RNG
                    do j = 1, shd%lc%IGND
                        call output_files_append_field(fls, shd, ts, line, prmfst%out%trng(j), args, nargs, z, j)
                    end do
                case (PMFRSTVN_DZAA)

                    !> User-defined temperature threshold(s)/tolerance(s) for DZAA.
                    if (nargs > 1) then
                        do j = 2, nargs
                            if (lowercase(args(j)) == 'ttol') then
                                if (allocated(prmfst%pm%dzaa_ttol)) then
                                    call print_error( &
                                        " Multiple instances of the 'ttol' option exist in outputs_balance.txt" // &
                                        " or a previous entry of 'DZAA' without the 'ttol' option" // &
                                        " has activated the default value ('ttol 0.1')." // &
                                        " Only one instance of the 'ttol' option can exist." // &
                                        " Combine multiple instances 'ttol' into a single option and add it to the first" // &
                                        " entry of 'DZAA' in the list.")
                                    z = 1
                                    exit
                                else
                                    call output_files_parse_indices(args, nargs, prmfst%pm%dzaa_ttol, j, z)
                                    exit
                                end if
                            end if
                        end do
                    end if
                    if (z == 0) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_DZAA)
                        do j = 1, size(prmfst%pm%dzaa_ttol)
                            write(line, FMT_GEN) prmfst%pm%dzaa_ttol(j)
                            call trimzero(line)
                            line(index(line, '.'):index(line, '.')) = 'p'
                            line = trim(PMFRSTVN_DZAA) // '_TTOL_' // trim(adjustl(line))
                            call output_files_append_field(fls, shd, ts, line, prmfst%out%dzaa(j), args, nargs, z)
                        end do
                    end if

                case default
                    n = n - 1
                    call print_warning("'" // trim(args(1)) // "' is not a recognized variable for output.")
            end select
            n = n + 1

            !> Check for errors.
            if (z /= 0) then
                ierr = z
                write(line, FMT_GEN) i
                call print_error( &
                    "Errors occurred while applying the output configuration for '" // trim(args(1)) // &
                    "' (Line " // trim(adjustl(line)) // ').')
            end if
        end do
        close(iun)

        !> Stop if errors exist.
        if (ierr /= 0) then
            call print_error('Errors occurred while reading outputs_balance.txt.')
            call program_abort()
        end if

        !> Echo the number of active fields read from file or print a warning if no variables were found in the file.
        if (n > 0) then
            write(line, FMT_GEN) n
            call print_message('Output variables: ' // trim(adjustl(line)))
        else
            call print_warning('No output variables were found in the file.')

            !> Deactivate the process.
            fls_out%PROCESS_ACTIVE = .false.
        end if
        call reset_tab()

    end subroutine

    subroutine output_files_update_file(fls, shd, ffreq, field, group, dates)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: ffreq
        type(output_field), intent(in) :: field
        integer, dimension(:, :), intent(in) :: dates

        !> Input/output variables.
        type(output_group) group

        !> Local variables.
        integer iun, z

        !> Grid-based.
        if (btest(field%fgroup, DATA_TYPE_GRID)) then

            !> Write output.
            iun = 0
            if (btest(field%ffmt, FILE_TYPE_R2C)) then
                z = 0
                call output_files_write_r2c(fls, shd, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // ".r2c")
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_SEQ)) then
                z = 0
                call output_files_write_seq(fls, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // ".seq")
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_TXT)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // ".txt")
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_CSV)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%grid%iun + iun, group%grid%dat, dates, z, sep = ',')
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // ".csv")
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, FILE_TYPE_TSI)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // "_GRD.ts")
                    call program_abort()
                end if
                iun = iun + 1
            end if
#ifdef NETCDF
            if (btest(field%ffmt, FILE_TYPE_NC4)) then
                z = 0
                call nc4_add_data_xyt( &
                    group%grid%nid, field%vname, ffreq, group%grid%tid, group%grid%vid, shd%xxx, shd%yyy, &
                    shd%xCount, shd%yCount, out%NO_DATA, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // "_" // VN_GRD // ".nc")
                    call program_abort()
                end if
            end if
#endif
            if (btest(field%ffmt, FILE_TYPE_R2C_BIN)) then
                z = 0
                call output_files_write_r2c_binary(fls, shd, group%grid%iun + iun, group%grid%dat, group%grid%record, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%grid%fname) // "_binary.r2c")
                    call program_abort()
                end if
                iun = iun + 1
            end if

            !> Reset 'dat' for the next time-step.
            group%grid%dat = out%NO_DATA
        end if

        !> Tile-based.
        if (btest(field%fgroup, DATA_TYPE_TILE)) then

            !> Write output.
            iun = 0
            if (btest(field%ffmt, FILE_TYPE_TSK)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%tile%iun + iun, group%tile%dat, dates, z)
                if (z /= 0) then
                    call print_error("Unable to write to output file: " // trim(group%tile%fname) // "_NML.ts")
                    call program_abort()
                end if
                iun = iun + 1
            end if

            !> Reset 'dat' for the next time-step.
            group%tile%dat = out%NO_DATA
        end if

    end subroutine

    subroutine output_files_filter_group(fls, shd, field, group, t)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: t

        !> Input/output variables.
        type(output_field) field
        type(output_group) group

        !> Local variables.
        integer k
        real frac(shd%NA)

        !> Return if either of the 'tile' or 'grid' groups are not activated.
        !> Return if the 'gru' attributes of the field is not allocated.
        if (.not. (btest(field%fgroup, DATA_TYPE_TILE) .and. btest(field%fgroup, DATA_TYPE_GRID)) .or. &
            .not. allocated(field%gru)) return

        !> Filter grid outputs by GRU (requires pulling from tile output variables).
        group%grid%dat(:, t) = 0.0
        select case (field%gru_mask)
            case ('gru_include')

                !> Include only the GRUs in the list.
                frac = 0.0
                do k = 1, shd%lc%NML
                    if (group%tile%dat(k, t) /= out%NO_DATA .and. any(field%gru == shd%lc%JLMOS(k))) then
                        group%grid%dat(shd%lc%ILMOS(k), t) = group%grid%dat(shd%lc%ILMOS(k), t) + &
                            group%tile%dat(k, t)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                        frac(shd%lc%ILMOS(k)) = frac(shd%lc%ILMOS(k)) + shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                    end if
                end do
                where (frac > 0.0)
                    group%grid%dat(:, t) = group%grid%dat(:, t)/frac
                elsewhere
                    group%grid%dat(:, t) = out%NO_DATA
                end where
            case ('gru_exclude')

                !> Exclude only the GRUs in the list.
                frac = 0.0
                do k = 1, shd%lc%NML
                    if (group%tile%dat(k, t) /= out%NO_DATA .and. .not. any(field%gru == shd%lc%JLMOS(k))) then
                        group%grid%dat(shd%lc%ILMOS(k), t) = group%grid%dat(shd%lc%ILMOS(k), t) + &
                            group%tile%dat(k, t)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                        frac(shd%lc%ILMOS(k)) = frac(shd%lc%ILMOS(k)) + shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                    end if
                end do
                where (frac > 0.0)
                    group%grid%dat(:, t) = group%grid%dat(:, t)/frac
                elsewhere
                    group%grid%dat(:, t) = out%NO_DATA
                end where
            case default

                !> Only the specified GRU.
                group%grid%dat(:, t) = out%NO_DATA
                do k = 1, shd%lc%NML
                    if (field%gru(1) == shd%lc%JLMOS(k)) group%grid%dat(shd%lc%ILMOS(k), t) = group%tile%dat(k, t)
                end do
        end select

    end subroutine

    !> Description:
    !>  Update the value using transforms (if provided).
    subroutine output_files_update_dat(fls, shd, field, file, t, cfactorm, cfactora, fn)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: t
        real, intent(in) :: cfactorm, cfactora
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        type(output_field) field
        type(output_file) file

        !> Set 'NO_DATA' value is no 'src' is defined.
        if (.not. associated(file%src)) then
            file%dat(:, t) = out%NO_DATA
            return
        end if

        !> Apply value regardless of transform in the first time-step.
        if (all(file%dat(:, t) == out%NO_DATA)) then
            file%dat(:, t) = (cfactorm*file%src + cfactora)
        else

            !> Apply transforms and update values.
            select case (fn)
                case (VN_MAX)
                    where (file%dat(:, t) /= out%NO_DATA)
                        file%dat(:, t) = max(file%dat(:, t), (cfactorm*file%src + cfactora))
                    end where
                case (VN_MIN)
                    where (file%dat(:, t) /= out%NO_DATA)
                        file%dat(:, t) = min(file%dat(:, t), (cfactorm*file%src + cfactora))
                    end where
                case (VN_ACC)
                    where (file%dat(:, t) /= out%NO_DATA)
                        file%dat(:, t) = file%dat(:, t) + (cfactorm*file%src + cfactora)
                    end where
            end select
        end if

    end subroutine

    subroutine output_files_update_group(fls, shd, field, group, t, cfactorm, cfactora, fn)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: t
        real, intent(in) :: cfactorm, cfactora
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        type(output_field) field
        type(output_group) group

        !> Local variables.
        real frac(shd%NA)

        !> Update tile variables.
        if (btest(field%fgroup, DATA_TYPE_TILE)) then
            call output_files_update_dat(fls, shd, field, group%tile, t, cfactorm, cfactora, fn)
        end if

        !> Update grid variables.
        if (btest(field%fgroup, DATA_TYPE_GRID)) then
            call output_files_update_dat(fls, shd, field, group%grid, t, cfactorm, cfactora, fn)

            !> Filter grid outputs by GRU (requires pulling from tile output variables).
            if (allocated(field%gru)) call output_files_filter_group(fls, shd, field, group, t)

            !> Apply frac.
            if (field%apply_frac) then
                where (group%grid%dat(:, t) /= out%NO_DATA) group%grid%dat(:, t) = group%grid%dat(:, t)*shd%FRAC
            end if
        end if

    end subroutine

    !> Description:
    !>  Update the 'dates' variable from the 'ic' counter.
    subroutine output_files_update_dates(dates, t, iter, year, month, day, hour, mins)

        !> Input variables.
        integer, intent(in) :: t, iter

        !> Input variables (optional).
        integer, intent(in), optional :: year, month, day, hour, mins

        !> Input/output variables.
        integer dates(:, :)

        !> Initialize the vector to zero (for missing fields).
        dates(:, t) = 0

        !> Save the time-step using 'now' date.
        dates(1, t) = iter
        if (present(year)) dates(2, t) = year
        if (present(month)) dates(3, t) = month
        if (present(day)) dates(4, t) = day
        if (present(hour)) dates(5, t) = hour
        if (present(mins)) dates(6, t) = mins

    end subroutine

    subroutine output_files_update_field(fls, shd, field)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_field) field

        !> Local variables.
        integer t, ierr
        real s_grid(shd%NA), s_tile(shd%lc%NML)

        !> Set 't = 1' for the case when 'in_mem' is not active.
        t = 1

        !> Update data fields for existing output variable frequencies.
        if (btest(field%ffreq, FREQ_YEARLY)) then
            if (ic%now%year /= ic%next%year) then
                if (field%in_mem) t = ic%iter%year
                call output_files_update_dates(fls_out%dates%y, t, ic%iter%year, ic%now%year, 1, 1)
                call output_files_update_group(fls, shd, field, field%y, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%year /= ic%next%year .and. .not. field%in_mem .and. .not. fls_out%fclose) .or. &
                (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, FREQ_YEARLY, field, field%y, fls_out%dates%y)
            end if
        end if
        if (btest(field%ffreq, FREQ_MONTHLY)) then
            if (ic%now%month /= ic%next%month) then
                if (field%in_mem) t = ic%iter%month
                call output_files_update_dates(fls_out%dates%m, t, ic%iter%month, ic%now%year, ic%now%month, 1)
                call output_files_update_group(fls, shd, field, field%m, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%month /= ic%next%month .and. .not. field%in_mem .and. .not. fls_out%fclose) .or. &
                (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, FREQ_MONTHLY, field, field%m, fls_out%dates%m)
            end if
        end if
        if (btest(field%ffreq, FREQ_DAILY)) then
            if (ic%now%day /= ic%next%day) then
                if (field%in_mem) t = ic%iter%day
                call output_files_update_dates(fls_out%dates%d, t, ic%iter%day, ic%now%year, ic%now%month, ic%now%day)
                call output_files_update_group(fls, shd, field, field%d, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%day /= ic%next%day .and. .not. field%in_mem .and. .not. fls_out%fclose) .or. &
                (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, FREQ_DAILY, field, field%d, fls_out%dates%d)
            end if
        end if
        if (btest(field%ffreq, FREQ_HOURLY)) then
            if (ic%now%hour /= ic%next%hour) then
                if (field%in_mem) t = ic%iter%hour
                call output_files_update_dates(fls_out%dates%h, t, ic%iter%hour, ic%now%year, ic%now%month, ic%now%day, ic%now%hour)
                call output_files_update_group(fls, shd, field, field%h, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%hour /= ic%next%hour .and. .not. field%in_mem .and. .not. fls_out%fclose) .or. &
                (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, FREQ_HOURLY, field, field%h, fls_out%dates%h)
            end if
        end if

        !> Per 'n' time-steps (user-defined).
        if (btest(field%ffreq, FREQ_PTS)) then

            !> Update group.
            t = 1
            if (field%fn == VN_ACC .or. field%fn == VN_AVG) then
                call output_files_update_group(fls, shd, field, field%pts, t, field%cfactorm, field%cfactora, VN_ACC)
            else
                call output_files_update_group(fls, shd, field, field%pts, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if (field%pts_aggregator == 0) then
                field%pts_counter = field%pts_counter + 1
                call output_files_update_dates( &
                    fls_out%dates%pts, t, field%pts_counter, ic%now%year, ic%now%month, ic%now%day, ic%now%hour, ic%now%mins)
            end if
            field%pts_aggregator = field%pts_aggregator + ic%dtmins
            if (field%pts_aggregator >= field%pts_length) then
                if (btest(field%fgroup, DATA_TYPE_GRID) .and. field%fn == VN_AVG) then
                    field%pts%grid%dat(:, t) = field%pts%grid%dat(:, t)/(field%pts_aggregator/ic%dtmins)
                end if
                if (btest(field%fgroup, DATA_TYPE_TILE) .and. field%fn == VN_AVG) then
                    field%pts%tile%dat(:, t) = field%pts%tile%dat(:, t)/(field%pts_aggregator/ic%dtmins)
                end if
                if ((.not. field%in_mem) .or. (field%in_mem .and. fls_out%fclose)) then
                    call output_files_update_file(fls, shd, FREQ_PTS, field, field%pts, fls_out%dates%pts)
                end if
                field%pts_aggregator = 0
            end if
        end if

        !> 'Seasonal' must go last because it changes 't' regardless of the state of 'in_mem'.
        if (btest(field%ffreq, FREQ_SEASONAL)) then
            if (ic%now%month /= ic%next%month) then
                t = ic%now%month
                call output_files_update_dates(fls_out%dates%s, t, t, ic%now%year, ic%now%month, 1)
                call output_files_update_group(fls, shd, field, field%s, t, field%cfactorm, field%cfactora, VN_ACC)
            end if
            if (fls_out%fclose) then

                !> Calculate average values.
                do t = 1, size(fls_out%dates%s, 2)
                    if (btest(field%fgroup, DATA_TYPE_GRID)) then
                        field%s%grid%dat(:, t) = field%s%grid%dat(:, t)/fls_out%dates%iter_s(t)
                    end if
                    if (btest(field%fgroup, DATA_TYPE_TILE)) then
                        field%s%tile%dat(:, t) = field%s%tile%dat(:, t)/fls_out%dates%iter_s(t)
                    end if
                end do
                call output_files_update_file(fls, shd, FREQ_SEASONAL, field, field%s, fls_out%dates%s)
            end if
        end if

    end subroutine

    subroutine output_files_update(fls, shd)

        !> Process modules.
        use permafrost_outputs_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer i

        !> Return if not active.
        if (.not. fls_out%PROCESS_ACTIVE) return

        !> Update counter for seasonal output.
        if (ic%now%month /= ic%next%month) fls_out%dates%iter_s(ic%now%month) = fls_out%dates%iter_s(ic%now%month) + 1

        !> Update external outputs.
        call permafrost_outputs_update(fls, shd)

        !> Update fields and output files.
        do i = 1, size(fls_out%fls)
            if (fls_out%fls(i)%ffreq /= FREQ_START .and. fls_out%fls(i)%ffreq /= FREQ_END) then
                call output_files_update_field(fls, shd, fls_out%fls(i))
            end if
        end do

    end subroutine

    subroutine output_files_finalize(fls, shd)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Return if not active.
        if (.not. fls_out%PROCESS_ACTIVE) return

        !> Write final outputs.
        fls_out%fclose = .true.
        call output_files_update(fls, shd)

    end subroutine

end module
