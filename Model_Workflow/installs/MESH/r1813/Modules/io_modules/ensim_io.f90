module ensim_io

    use strings
    use print_routines

    implicit none

    integer, parameter :: MAX_WORDS = 500, MAX_WORD_LENGTH = 200, MAX_LINE_LENGTH = 5000

    interface get_keyword_value
        module procedure get_keyword_field_char
        module procedure get_keyword_field_double
        module procedure get_keyword_field_float
        module procedure get_keyword_field_long
        module procedure get_keyword_field_int
        module procedure get_keyword_value_char
        module procedure get_keyword_value_double
        module procedure get_keyword_value_float
        module procedure get_keyword_value_long
        module procedure get_keyword_value_int
    end interface

    interface validate_header_spatial
        module procedure validate_header_spatial_projection
        module procedure validate_header_spatial_grid
    end interface

    interface r2c_to_rank
        module procedure r2c_to_rank_field_double
        module procedure r2c_to_rank_field_float
        module procedure r2c_to_rank_field_long
        module procedure r2c_to_rank_field_int
    end interface

    type ensim_keyword
        character(len = MAX_WORD_LENGTH) :: keyword = ''
        character(len = MAX_WORD_LENGTH), dimension(:), allocatable :: words
    end type

    type ensim_date
        integer :: year = 0, month = 0, day = 0, hour = 0, mins = 0, secs = 0
    end type

    type ensim_attr
        character(len = MAX_WORD_LENGTH) :: attr = '', units = '', type = ''
        character(len = MAX_LINE_LENGTH) :: frame_string = ''
        type(ensim_date) frame_date
        real, dimension(:, :), allocatable :: val
    end type

    contains

    !> Description: Open EnSim file with 'read' action.
    !*  iun: File unit.
    !*  fname: Full path to file.
    !*  ierr: Return status from open statement.
    subroutine open_ensim_input(iun, fpath, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fpath

        !> Output variables.
        integer, intent(out) :: ierr

        !> Set error to zero status and attempt to open file.
        ierr = 0
        open(iun, file = fpath, status = 'old', action = 'read', iostat = ierr)

        !> Print error if unable to open file.
        if (ierr /= 0) then
            call print_error('Unable to open the file: ' // trim(fpath))
        end if

        return

    end subroutine

    !> Description: Read line from file. Skips lines lead with '#' or that are blank, and compacts whitespace.
    !*  iun: File unit.
    !*  line: Line read from file.
    !*  ierr: Return status from read statement.
    subroutine read_ensim_line(iun, line, ierr)

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables.
        character(len = *), intent(out) :: line
        integer, intent(out) :: ierr

        !> Read line:
        !>  - Return on read error.
        !>  - Compact whitespace.
        !>  - Skips line that begins with '#' character.
        !>  - Skip blank lines.
        do while (ierr == 0)
            read(iun, '(a)', iostat = ierr) line
            if (ierr /= 0) exit
            call compact(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle
            if (len_trim(line) > 0) exit
        end do

        return

    end subroutine

    !> Description: Returns .true. if the line contains ':endheader'.
    !*  line: Input line.
    logical function is_header(line)

        !> Input variables.
        character(len = *), intent(in) :: line

        !> Initialize the function.
        is_header = .false.

        !> Return if the line is not long enough to contain the string.
        if (len_trim(line) < 10) return

        !> Return .true. if the line contains the string.
        if (lowercase(line(1:10)) == ':endheader') is_header = .true.

        return

    end function

    !> Description: Parse keywords and their words/values in an EnSim header.
    !*  iun: File unit.
    !*  vkeyword: Group of keywords and their words/values.
    !*  nkeyword: Number of keywords.
    !*  ierr: Return status.
    subroutine parse_header_ensim(iun, vkeyword, nkeyword, ierr)

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables.
        type(ensim_keyword), dimension(:), allocatable, intent(out) :: vkeyword
        integer, intent(out) :: nkeyword, ierr

        !> Local variables.
        character(len = MAX_LINE_LENGTH) line
        character(len = MAX_WORD_LENGTH), dimension(MAX_WORDS) :: args, temp
        integer n, j, i, nargs

        !> Initialize keyword count and return status.
        nkeyword = 0
        ierr = 0

        !> Read to the end of the header and count the number of valid keywords.
        do while (ierr == 0)
            call read_ensim_line(iun, line, ierr)
            if (ierr /= 0) exit
            if (is_header(line)) exit

            !> A line that contains a keyword starts with the ':' character.
            if (line(1:1) == ':') nkeyword = nkeyword + 1
        end do

        !> Return if an error was encountered while reading lines.
        if (ierr /= 0) then
            call print_warning('An error occurred reading the header in the file.')
            goto 999
        end if

        !> Allocate the keywords; return on error.
        allocate(vkeyword(nkeyword), stat = ierr)
        if (ierr /= 0) then
            call print_warning('An error occurred allocating keywords in the file.')
            goto 999
        end if

        !> Rewind the file and parse the words/values of each keyword.
        rewind(iun)
        n = 0
        do while (ierr == 0)
            call read_ensim_line(iun, line, ierr)
            if (ierr /= 0) exit
            if (is_header(line)) exit
            if (line(1:1) == ':') then
                n = n + 1
                call parse(line, ' ', args, nargs)
                vkeyword(n)%keyword = args(1)
                if (nargs > 1) then
                    temp(:) = ''
                    i = 1
                    j = 1
                    do while (j < nargs)
                        if (index(args(j + 1), '"') == 1) then
                            temp(i) = trim(args(j + 1)(2:))
                            j = j + 1
                            do while (j < nargs .and. index(args(j + 1), '"') == 0)
                                temp(i) = trim(temp(i)) // ' ' // trim(args(j + 1))
                                j = j + 1
                            end do
                            temp(i) = trim(temp(i)) // ' ' // trim(args(j + 1)(:index(args(j + 1), '"') - 1))
                        else
                            temp(i) = trim(args(j + 1))
                        end if
                        j = j + 1
                        i = i + 1
                    end do
                    i = i - 1
                    allocate(vkeyword(n)%words(i), stat = ierr)
                    if (ierr /= 0) exit
                    vkeyword(n)%words = temp(1:i)
                end if
            end if
        end do
        if (ierr /= 0) then
            call print_warning('An error occurred parsing the keyword and its values.')
            goto 999
        end if

        return

        !> Error: Deallocate keywords and reset the count to zero.
999     nkeyword = 0
        if (allocated(vkeyword)) then
            do n = 1, size(vkeyword)
                if (allocated(vkeyword(n)%words)) deallocate(vkeyword(n)%words)
            end do
            deallocate(vkeyword)
        end if
        return

    end subroutine

    !> Description: Identify fields in the attribute list inside an EnSim header.
    !*  iun: File unit.
    !*  vkeyword: Group of keywords and their words/values.
    !*  nkeyword: Number of keywords.
    !*  vattr: Group of attributes.
    !*  nattr: Number of attributes.
    !*  ierr: Return status.
    subroutine parse_header_attribute_ensim(iun, vkeyword, nkeyword, vattr, nattr, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        type(ensim_attr), dimension(:), allocatable, intent(out) :: vattr
        integer, intent(out) :: nattr, ierr

        !> Local variables.
        integer n, k, j, i

        !> Identify and count entries for attributes in the list of keywords.
        nattr = 0
        do n = 1, nkeyword
            if (lowercase(vkeyword(n)%keyword) == ':attributename') nattr = nattr + 1
        end do

        !> Return if no attributes were found.
        if (nattr == 0) then
            call print_warning('No attributes were found.')
            goto 999
        end if

        allocate(vattr(nattr), stat = ierr)
        if (ierr /= 0) then
            call print_warning('An error occurred allocating attributes in the file.')
            goto 999
        end if

        do n = 1, nkeyword
            if (lowercase(vkeyword(n)%keyword(1:10)) /= ':attribute') cycle
            if (nattr == 1 .and. size(vkeyword(n)%words) == 1) then

                !> Permit missing attribute number for compatibility with improperly generated files.
                j = 1
                k = 1
            else
                call value(vkeyword(n)%words(1), j, ierr)
                if (ierr /= 0) then
                    call print_warning("Bad attribute definition in '" // trim(vkeyword(n)%keyword) // "'.")
                    cycle
                else
                    k = 2
                end if
            end if
            select case (lowercase(vkeyword(n)%keyword))
                case (':attributename')
                    vattr(j)%attr = vkeyword(n)%words(k)
                    do i = (k + 1), size(vkeyword(n)%words)
                        vattr(j)%attr = trim(adjustl(vattr(j)%attr)) // ' ' // trim(adjustl(vkeyword(n)%words(i)))
                    end do
                case (':attributeunits')
                    vattr(j)%units = vkeyword(n)%words(k)
                    do i = (k + 1), size(vkeyword(n)%words)
                        vattr(j)%units = trim(adjustl(vattr(j)%units)) // ' ' // trim(adjustl(vkeyword(n)%words(i)))
                    end do
                case (':attributetype')
                    vattr(j)%type = vkeyword(n)%words(k)
                    do i = (k + 1), size(vkeyword(n)%words)
                        vattr(j)%type = trim(adjustl(vattr(j)%type)) // ' ' // trim(adjustl(vkeyword(n)%words(i)))
                    end do
            end select
        end do

        return

        !> Error: Deallocate attribute and reset the count to zero.
999     nattr = 0
        if (allocated(vattr)) deallocate(vattr)
        return

    end subroutine

    !> Description: Check for multi-frame signature.
    logical function is_multi_frame(iun)

        !> Input variables.
        integer, intent(in) :: iun

        !> Local variables.
        character(len = MAX_LINE_LENGTH) line
        integer ierr

        !> Advance past header.
        call advance_past_header(iun, ierr = ierr)
        if (ierr /= 0) then
            is_multi_frame = .false.
            return
        end if

        !> Check for 'Frame' signature.
        call read_ensim_line(iun, line, ierr)
        if (ierr == 0 .and. lowercase(line(1:7)) == ':frame') then
            is_multi_frame = .true.
        else
            is_multi_frame = .false.
        end if

    end function

    !> Description: 'get_keyword_value' for a vector of type character.
    subroutine get_keyword_field_char(iun, vkeyword, nkeyword, cname, cfield, ncol, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword, ncol
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        character(len = *), dimension(ncol), intent(out) :: cfield

        !> Local variables.
        integer n, jz
        character(len = MAX_WORD_LENGTH) fmt1, fmt2

        !> Initialize the return status.
        ierr = 0

        !> Find 'cname' in the list of keywords.
        cfield = ''
        do n = 1, nkeyword
            if (lowercase(vkeyword(n)%keyword) == lowercase(cname)) then
                jz = min(ncol, size(vkeyword(n)%words))
                cfield(1:jz) = vkeyword(n)%words(1:jz)
                if (size(vkeyword(n)%words) /= ncol) then
                    write(fmt1, FMT_GEN) size(vkeyword(n)%words)
                    write(fmt2, FMT_GEN) ncol
                    call print_warning( &
                        "The number of fields in '" // trim(cname) // "' is " // trim(adjustl(fmt1)) // &
                        ' but ' // trim(adjustl(fmt2)) // ' were expected.')
                end if
                exit
            end if
        end do

        return

    end subroutine

    !> Description: 'get_keyword_value' for a vector of type 8-byte real.
    subroutine get_keyword_field_double(iun, vkeyword, nkeyword, cname, dfield, ncol, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword, ncol
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        real(kind = 8), dimension(ncol), intent(out) :: dfield

        !> Local variables.
        integer j, z
        character(len = MAX_WORD_LENGTH), dimension(ncol) :: cfield
        character(len = MAX_WORD_LENGTH) fmt1

        !> Initialize the return status.
        ierr = 0
        dfield = 0.0

        !> Call base routine.
        call get_keyword_field_char(iun, vkeyword, nkeyword, cname, cfield, ncol, ierr)
        if (ierr /= 0) return

        !> Check for blank fields (if the field was not found).
        if (all(cfield == '')) return

        !> Convert values.
        do j = 1, ncol
            z = 0
            call value(cfield(j), dfield(j), z)
            if (z /= 0) then
                write(fmt1, FMT_GEN) j
                call print_warning( &
                    'An error occurred converting value ' // trim(adjustl(fmt1)) // ' (' // trim(cfield(j)) // ')' // &
                    " to a number in '" // trim(cname) // ".")
                ierr = z
            end if
        end do

        return

    end subroutine

    !> Description: 'get_keyword_value' for a vector of type 4-byte real.
    subroutine get_keyword_field_float(iun, vkeyword, nkeyword, cname, ffield, ncol, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword, ncol
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        real(kind = 4), dimension(ncol), intent(out) :: ffield

        !> Local variables.
        real(kind = 8), dimension(ncol) :: dfield

        !> Initialize the return status.
        ierr = 0
        ffield = 0

        !> Call base routine.
        call get_keyword_field_double(iun, vkeyword, nkeyword, cname, dfield, ncol, ierr)
        if (ierr /= 0) return

        !> Convert values.
        ffield = real(dfield, kind = 4)

        return

    end subroutine

    !> Description: 'get_keyword_value' for a vector of type 8-byte integer.
    subroutine get_keyword_field_long(iun, vkeyword, nkeyword, cname, ifield, ncol, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword, ncol
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        integer(kind = 8), dimension(ncol), intent(out) :: ifield

        !> Local variables.
        real(kind = 8), dimension(ncol) :: dfield

        !> Initialize the return status.
        ierr = 0
        ifield = 0

        !> Call base routine.
        call get_keyword_field_double(iun, vkeyword, nkeyword, cname, dfield, ncol, ierr)
        if (ierr /= 0) return

        !> Convert values.
        ifield = int(dfield, kind = 8)

        return

    end subroutine

    !> Description: 'get_keyword_value' for a vector of type 4-byte integer.
    subroutine get_keyword_field_int(iun, vkeyword, nkeyword, cname, ifield, ncol, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword, ncol
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        integer(kind = 4), dimension(ncol), intent(out) :: ifield

        !> Local variables.
        real(kind = 8), dimension(ncol) :: dfield

        !> Initialize the return status.
        ierr = 0
        ifield = 0

        !> Call base routine.
        call get_keyword_field_double(iun, vkeyword, nkeyword, cname, dfield, ncol, ierr)
        if (ierr /= 0) return

        !> Convert values.
        ifield = int(dfield, kind = 4)

        return

    end subroutine

    !> Description: 'get_keyword_value' for type character.
    subroutine get_keyword_value_char(iun, vkeyword, nkeyword, cname, cvalue, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        character(len = *), intent(out) :: cvalue

        !> Local variables.
        character(len = len(cvalue)), dimension(1) :: cfield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call get_keyword_field_char(iun, vkeyword, nkeyword, cname, cfield, 1, ierr)
        if (ierr /= 0) return

        !> Transfer value.
        cvalue = cfield(1)

        return

    end subroutine

    !> Description: 'get_keyword_value' for type 8-byte real.
    subroutine get_keyword_value_double(iun, vkeyword, nkeyword, cname, dvalue, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        real(kind = 8), intent(out) :: dvalue

        !> Local variables.
        real(kind = 8), dimension(1) :: dfield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call get_keyword_field_double(iun, vkeyword, nkeyword, cname, dfield, 1, ierr)
        if (ierr /= 0) return

        !> Transfer value.
        dvalue = dfield(1)

        return

    end subroutine

    !> Description: 'get_keyword_value' for type 4-byte real.
    subroutine get_keyword_value_float(iun, vkeyword, nkeyword, cname, fvalue, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        real(kind = 4), intent(out) :: fvalue

        !> Local variables.
        real(kind = 4), dimension(1) :: ffield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call get_keyword_field_float(iun, vkeyword, nkeyword, cname, ffield, 1, ierr)
        if (ierr /= 0) return

        !> Transfer value.
        fvalue = ffield(1)

        return

    end subroutine

    !> Description: 'get_keyword_value' for type 8-byte integer.
    subroutine get_keyword_value_long(iun, vkeyword, nkeyword, cname, ivalue, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        integer(kind = 8), intent(out) :: ivalue

        !> Local variables.
        integer(kind = 8), dimension(1) :: ifield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call get_keyword_field_long(iun, vkeyword, nkeyword, cname, ifield, 1, ierr)
        if (ierr /= 0) return

        !> Transfer value.
        ivalue = ifield(1)

        return

    end subroutine

    !> Description: 'get_keyword_value' for type 4-byte integer.
    subroutine get_keyword_value_int(iun, vkeyword, nkeyword, cname, ivalue, ierr)

        !> Input variables.
        character(len = *), intent(in) :: cname
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ierr
        integer(kind = 4), intent(out) :: ivalue

        !> Local variables.
        integer(kind = 4), dimension(1) :: ifield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call get_keyword_field_int(iun, vkeyword, nkeyword, cname, ifield, 1, ierr)
        if (ierr /= 0) return

        !> Transfer value.
        ivalue = ifield(1)

        return

    end subroutine

    !> Description: Compare keyword values to a set projection.
    !*  vkeyword: Group of keywords and their words/values.
    !*  nkeyword: Number of keywords.
    !*  projection: Expected projection.
    !*  ierr: Return status.
    subroutine validate_header_spatial_projection(vkeyword, nkeyword, projection, ierr)

        !> Input variables.
        integer, intent(in) :: nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword
        character(len = *), intent(in) :: projection

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n
        character(len = MAX_LINE_LENGTH) line
        character(len = MAX_WORD_LENGTH) p

        !> Initialize the return status.
        ierr = 0

        !> Find and convert the keywords.
        p = ''
        do n = 1, nkeyword
            select case (lowercase(vkeyword(n)%keyword))
                case (':projection')
                    p = vkeyword(n)%words(1)
            end select
            if (len_trim(p) > 0) exit
        end do

        !> Check against the existing definition.
        if (lowercase(p) /= lowercase(projection)) then
            ierr = 1
            call print_warning('The projection in the file does not match the existing projection in the model.')
            call increase_tab()
            write(line, FMT_GEN) 'Attribute', 'File', 'Expected'
            call print_message(line)
            write(line, FMT_GEN) ':Projection', trim(p), trim(projection)
            call print_message(line)
            call decrease_tab()
        end if

        return

    end subroutine

    !> Description: Compare keyword values to a set grid definition.
    !*  vkeyword: Group of keywords and their words/values.
    !*  nkeyword: Number of keywords.
    !*  xcount: Cell count in the x-direction.
    !*  xdelta: Cell size in the x-direction.
    !*  xorigin: Lower-left grid origin in the x-direction.
    !*  ycount: Cell count in the y-direction.
    !*  ydelta: Cell size in the y-direction.
    !*  yorigin: Lower-left grid origin in the y-direction.
    !*  ierr: Return status.
    subroutine validate_header_spatial_grid(vkeyword, nkeyword, projection, xcount, xdelta, xorigin, ycount, ydelta, yorigin, ierr)

        !> Input variables.
        integer, intent(in) :: nkeyword, xcount, ycount
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword
        character(len = *), intent(in) :: projection
        real, intent(in) :: xdelta, xorigin, ydelta, yorigin

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n, z
        integer ix, iy
        real x, dx, y, dy
        character(len = MAX_LINE_LENGTH) line
        character(len = MAX_WORD_LENGTH) p

        !> Initialize the return status.
        ierr = 0

        !> Find and convert the keywords.
        p = ''
        do n = 1, nkeyword
            z = 0
            select case (lowercase(vkeyword(n)%keyword))
                case (':projection')
                    p = vkeyword(n)%words(1)
                case (':xcount')
                    call value(vkeyword(n)%words(1), ix, z)
                case (':xdelta')
                    call value(vkeyword(n)%words(1), dx, z)
                case (':xorigin')
                    call value(vkeyword(n)%words(1), x, z)
                case (':ycount')
                    call value(vkeyword(n)%words(1), iy, z)
                case (':ydelta')
                    call value(vkeyword(n)%words(1), dy, z)
                case (':yorigin')
                    call value(vkeyword(n)%words(1), y, z)
            end select
            if (z /= 0) ierr = z
        end do

        !> Print warning if there were conversion errors.
        if (ierr /= 0) call print_warning('Errors occurred extracting the grid dimensions from the file.')
        ierr = 0

        !> Check against the existing definition.
        if (lowercase(p) /= lowercase(projection) .or. &
            ix /= xcount .or. iy /= ycount .or. x /= xorigin .or. y /= yorigin .or. dx /= xdelta .or. dy /= ydelta) then
            ierr = 1
            call print_warning('The grid definition in the file does not match the existing definition in the model.')
            call increase_tab()
            write(line, FMT_GEN) 'Attribute', 'File', 'Expected'
            call print_message(line)
            write(line, FMT_GEN) ':Projection', trim(p), trim(projection)
            call print_message(line)
            write(line, FMT_GEN) ':xOrigin', x, xorigin
            call print_message(line)
            write(line, FMT_GEN) ':yOrigin', y, yorigin
            call print_message(line)
            write(line, FMT_GEN) ':xCount', ix, xcount
            call print_message(line)
            write(line, FMT_GEN) ':yCount', iy, ycount
            call print_message(line)
            write(line, FMT_GEN) ':xDelta', dx, xdelta
            call print_message(line)
            write(line, FMT_GEN) ':yDelta', dy, ydelta
            call print_message(line)
            call decrease_tab()
        end if

        return

    end subroutine

    !> Description: Count the number of columns in the 'tb0' format file.
    !*  iun: File unit.
    !*  vkeyword: Group of keywords and their words/values.
    !*  nkeyword: Number of keywords.
    !*  ncol: Number of columns.
    !*  ierr: Return status.
    subroutine count_columns_tb0(iun, vkeyword, nkeyword, ncol, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: ncol, ierr

        !> Local variables.
        integer n
        logical in_section

        !> Initialize the return status.
        ierr = 0

        !> Read the number of entries/columns in each column attribute.
        in_section = .false.
        ncol = 0
        do n = 1, nkeyword
            if (lowercase(vkeyword(n)%keyword) == ':columnmetadata') then
                in_section = .true.
            else if (lowercase(vkeyword(n)%keyword) == ':endcolumnmetadata') then
                in_section = .false.
                exit
            else if (in_section) then
                if (ncol > 0 .and. size(vkeyword(n)%words) /= ncol) then

                    !> Take the smaller value in the case of a conflict between this and other entries in the table.
                    call print_warning( &
                        "The number of columns in '" // trim(vkeyword(n)%keyword) // "'" // &
                        " is different from other table entries in the file.")
                    ncol = min(ncol, size(vkeyword(n)%words))
                else
                    ncol = size(vkeyword(n)%words)
                end if
            end if
        end do

        !> Print a warning if no columns were found in the table.
        if (ncol == 0) then
            call print_warning('No columns were found in the table in the file.')
        end if

        return

    end subroutine

    !> Description: Parse the start time in the file header into individual components.
    !*  iun: File unit.
    !*  vkeyword: Group of keywords and their words/values.
    !*  nkeyword: Number of keywords.
    !*  year: Year component of date.
    !*  month: Month component of date.
    !*  day: Day in month component of date.
    !*  hour: Hour in day component of date.
    !*  mins: Minutes in hour component of date.
    !*  ierr: Return status.
    subroutine parse_starttime(iun, vkeyword, nkeyword, year, month, day, hour, mins, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword

        !> Output variables.
        integer, intent(out) :: year, month, day, hour, mins, ierr

        character(len = MAX_WORD_LENGTH) ctmp
        integer n, j, z

        !> Initially set the values to zero.
        year = 0; month = 0; day = 0; hour = 0; mins = 0

        !> Initialize the return status.
        ierr = 0

        !> Find start time in the list of attributes.
        do n = 1, nkeyword
            select case(lowercase(vkeyword(n)%keyword))

                !> 'starttime' contains a date and/or time; in some cases, 'startdate' is used instead.
                case (':starttime', ':startdate')

                    !> Scan for a date signature (e.g., 2003/01/31).
                    ctmp = adjustl(vkeyword(n)%words(1))
                    z = 0
                    if (index(ctmp, '/') > 1 .and. index(ctmp, '/') /= index(ctmp, '/', back = .true.)) then
                        call value(ctmp(1:(index(ctmp, '/') - 1)), year, z)
                        if (z == 0) call value(ctmp((index(ctmp, '/') + 1):(index(ctmp, '/', back = .true.) - 1)), month, z)
                        if (z == 0) call value(ctmp((index(ctmp, '/', back = .true.) + 1):len(ctmp)), day, z)
                    end if
                    if (z /= 0) then
                        ierr = z
                        call print_warning( &
                            "Invalid format or missing component in date '" // trim(ctmp) // "'," // &
                            " expecting format 'YYYY/MM/DD'.")
                    end if

                    !> Scan for a time signature (e.g., 24:00:00.000; 24:00:00; 24:00).
                    !> In the case of multiple keywords, assume a date precedes the time.
                    if (size(vkeyword(n)%words) > 1) then
                        ctmp = adjustl(vkeyword(n)%words(2))
                    else if (size(vkeyword(n)%words) == 1) then
                        ctmp = adjustl(vkeyword(n)%words(1))
                    end if
                    z = 0
                    if (index(ctmp, ':') > 1) then
                        call value(ctmp(1:(index(ctmp, ':') - 1)), hour, z)
                        if (z == 0 .and. index(ctmp, ':') /= index(ctmp, ':', back = .true.)) then
                            call value(ctmp((index(ctmp, ':') + 1):(index(ctmp, ':', back = .true.) - 1)), mins, z)
                        end if
                    end if
                    if (z /= 0) then
                        ierr = z
                        call print_warning( &
                            "Invalid format or missing component in time '" // trim(ctmp) // "'," // &
                            " expecting format 'HH:MM', 'HH:MM:SS', or 'HH:MM:SS.000'.")
                    end if
            end select
        end do

        return

    end subroutine

    !> Description: Advance to the first line beyond the header in the file.
    !*  iun: File unit.
    !*  fname: File name.
    !*  ierr: Return status.
    subroutine advance_past_header(iun, fname, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in), optional :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = MAX_LINE_LENGTH) line

        !> Rewind to the beginning of the file, then read until the end of the EnSim header.
        rewind(iun)
        ierr = 0
        do while (ierr == 0)
            call read_ensim_line(iun, line, ierr)
            if (ierr /= 0) exit
            if (is_header(line)) exit
        end do
        if (ierr /= 0) then
            call print_warning("Reached end of file while advancing past the header.")
        end if

        return

    end subroutine

    !> Description: Load a single frame of data into attributes from an 'r2c' format file.
    !*  iun: File unit.
    !*  fname: File name.
    !*  vattr: Group of attributes.
    !*  nattr: Number of attributes.
    !*  xcount: Cell count in the x-direction.
    !*  ycount: Cell count in the y-direction.
    !*  frame: .true. if the data are padded in ':Frame' and ':EndFrame'.
    !*  ierr: Return status.
    subroutine load_data_r2c(iun, fname, vattr, nattr, xcount, ycount, frame, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fname
        integer, intent(in) :: iun, nattr, xcount, ycount
        logical, intent(in) :: frame

        !> Input/output variables.
        type(ensim_attr), dimension(:), allocatable :: vattr

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = MAX_LINE_LENGTH) line
        integer n, j, i

        !> Initialize the return status.
        ierr = 0

        !> Read from the file.
        do n = 1, nattr

            !> ':Frame'.
            if (frame) then
                call readline(iun, vattr(n)%frame_string, ierr)
                if (ierr /= 0) exit
            end if

            !> Data.
            if (.not. allocated(vattr(n)%val)) allocate(vattr(n)%val(xcount, ycount))
            do i = 1, ycount
                read(iun, *, iostat = ierr) (vattr(n)%val(j, i), j = 1, xcount)
                if (ierr /= 0) exit
            end do

            !> ':EndFrame'.
            if (frame) then
                call readline(iun, line, ierr)
                if (ierr /= 0) exit
            end if
        end do

        !> Check for errors.
        if (ierr /= 0) then
            call print_error('An error occurred reading data from the file: ' // trim(fname))
        end if

        return

    end subroutine

    !> Description: Transfer data from an 'r2c' format grid to a 'RANK'-based vector.
    !*  iun: File unit.
    !*  vattr: Group of attributes.
    !*  nattr: Number of attributes.
    !*  iattr: Index of the attribute in the group.
    !*  xxx: RANK-to-grid 'r2c' lookup table for cells in the x-direction.
    !*  yyy: RANK-to-grid 'r2c' lookup table for cells in the y-direction.
    !*  na: Size of the conversion vectors.
    !*  ffield: Output vector.
    !*  nfield: Size of the output vector.
    !*  ierr: Return status.

    !> Description: 'r2c_to_rank' for type 8-byte real.
    subroutine r2c_to_rank_field_double(iun, vattr, nattr, iattr, xxx, yyy, na, dfield, nfield, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nattr, iattr, na, nfield
        type(ensim_attr), dimension(:), intent(in) :: vattr
        integer, dimension(na), intent(in) :: xxx, yyy

        !> Output variables.
        real(kind = 8), dimension(:), allocatable, intent(out) :: dfield
        integer, intent(out) :: ierr

        !> Local variables
        integer n
        character(len = MAX_WORD_LENGTH) fmt1, fmt2

        !> Initialize the return status.
        ierr = 0

        !> Check if the index of the desired attribute is within bounds of the list.
        if (iattr < lbound(vattr, 1) .or. iattr > ubound(vattr, 1)) then
            write(fmt1, FMT_GEN) iattr
            write(fmt2, FMT_GEN) size(vattr)
            call print_warning( &
                'The desired index ' // trim(adjustl(fmt1)) // &
                ' is beyond the number of attributes in the list ' // trim(adjustl(fmt2)) // '.')
            ierr = 1
            return
        end if

        !> Check if the attribute contains data.
        if (.not. allocated(vattr(iattr)%val)) then
            call print_warning("The attribute '" // trim(vattr(iattr)%attr) // "' contains no data.")
            ierr = 1
            return
        end if

        !> Transfer data.
        if (.not. allocated(dfield)) allocate(dfield(nfield))
        dfield = 0.0
        do n = 1, nfield
            dfield(n) = real(vattr(iattr)%val(xxx(n), yyy(n)), kind = 8)
        end do

        return

    end subroutine

    !> Description: 'r2c_to_rank' for type 4-byte real.
    subroutine r2c_to_rank_field_float(iun, vattr, nattr, iattr, xxx, yyy, na, ffield, nfield, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nattr, iattr, na, nfield
        type(ensim_attr), dimension(nattr) :: vattr
        integer, dimension(na), intent(in) :: xxx, yyy

        !> Output variables.
        real(kind = 4), dimension(:), allocatable, intent(out) :: ffield
        integer, intent(out) :: ierr

        !> Local variables
        real(kind = 8), dimension(:), allocatable :: dfield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call r2c_to_rank_field_double(iun, vattr, nattr, iattr, xxx, yyy, na, dfield, nfield, ierr)
        if (ierr /= 0) return

        !> Transfer data.
        if (.not. allocated(ffield)) allocate(ffield(nfield))
        ffield = 0.0
        ffield = real(dfield, kind = 4)

        return

    end subroutine

    !> Description: 'r2c_to_rank' for type 8-byte integer.
    subroutine r2c_to_rank_field_long(iun, vattr, nattr, iattr, xxx, yyy, na, ifield, nfield, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nattr, iattr, na, nfield
        type(ensim_attr), dimension(nattr) :: vattr
        integer, dimension(na), intent(in) :: xxx, yyy

        !> Output variables.
        integer(kind = 8), dimension(:), allocatable, intent(out) :: ifield
        integer, intent(out) :: ierr

        !> Local variables
        real(kind = 8), dimension(:), allocatable :: dfield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call r2c_to_rank_field_double(iun, vattr, nattr, iattr, xxx, yyy, na, dfield, nfield, ierr)
        if (ierr /= 0) return

        !> Transfer data.
        if (.not. allocated(ifield)) allocate(ifield(nfield))
        ifield = 0
        ifield = int(dfield, kind = 8)

        return

    end subroutine

    !> Description: 'r2c_to_rank' for type 4-byte integer.
    subroutine r2c_to_rank_field_int(iun, vattr, nattr, iattr, xxx, yyy, na, ifield, nfield, ierr)

        !> Input variables.
        integer, intent(in) :: iun, nattr, iattr, na, nfield
        type(ensim_attr), dimension(nattr) :: vattr
        integer, dimension(na), intent(in) :: xxx, yyy

        !> Output variables.
        integer(kind = 4), dimension(:), allocatable, intent(out) :: ifield
        integer, intent(out) :: ierr

        !> Local variables
        real(kind = 8), dimension(:), allocatable :: dfield

        !> Initialize the return status.
        ierr = 0

        !> Call base routine.
        call r2c_to_rank_field_double(iun, vattr, nattr, iattr, xxx, yyy, na, dfield, nfield, ierr)
        if (ierr /= 0) return

        !> Transfer data.
        if (.not. allocated(ifield)) allocate(ifield(nfield))
        ifield = 0
        ifield = int(dfield, kind = 4)

        return

    end subroutine

end module
