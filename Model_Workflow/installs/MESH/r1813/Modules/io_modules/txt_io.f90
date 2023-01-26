!> Description:
!>  Module that contains functions and subroutines pertaining to reading
!>  and writing fields from space or comma delimited text files.
module txt_io

    !> 'print_routines': For print routines, format statements, and line lengths and limits.
    use print_routines

    implicit none

    interface read_txt_line
        module procedure read_txt_line
        module procedure read_txt_line_args_char
        module procedure read_txt_line_args_real
        module procedure read_txt_line_args_int
    end interface

    contains

    !> Description:
    !>  Read a line from file skipping lines lead with '!' or '#'.
    !>  Lines are trimmed to inline instances of '!' or '#' and tab-
    !>  characters are replaced with a space.
    !>
    !> Variables:
    !*  iun: Unit of opened file.
    !*  line: Line returned (optional).
    !*  quiet: .true. to suppress messages (optional).
    !*  ierr: Status (0: normal).
    subroutine read_txt_line(iun, line, quiet, ierr)

        !> Input/output variables.
        integer, intent(in) :: iun
        character(len = *), intent(out), optional :: line
        logical, intent(in), optional :: quiet
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line_buffer
        logical v

        !> Verbosity.
        v = .true.
        if (present(quiet)) v = (.not. quiet)

        !> Loop until a valid line has been read.
        ierr = 0
        do while (.true.)

            !> Read line.
            read(iun, '(a)', iostat = ierr) line_buffer

            !> Exit if non-zero status.
            if (ierr /= 0) exit

            !> Cycle if the first character in the line is a comment.
            if (line_buffer(1:1) == '!' .or. line_buffer(1:1) == '#') cycle

            !> Trim inline comments.
            if (index(line_buffer, '#') > 2) line_buffer = line_buffer(1:index(line_buffer, '#') - 1)
            if (index(line_buffer, '!') > 2) line_buffer = line_buffer(1:index(line_buffer, '!') - 1)

            !> Exit if a line has been read.
            if (len_trim(line_buffer) > 0) exit
        end do

        !> Copy the line read from file.
        if (present(line)) then
            if (ierr == 0) then
                line = trim(line_buffer)
            else
                line = ''
            end if
        end if

    end subroutine

    !> Description:
    !>  Read a line to a list of character fields. Performs the same
    !>  filtering as 'read_txt_line'.
    !>
    !> Input/output variables:
    !*  iun: Unit of opened file.
    !*  delimiter: Field delimiter (optional, default: space).
    !*  n_skip_cols: Number of columns to skip from the beginning of the line (optional).
    !*  keep_alloc: Preserve the existing allocation of 'values' (optional).
    !*  quiet: .true. to suppress messages (optional).
    !*  ierr: Status (0: normal).
    subroutine read_txt_line_args_char(iun, values, delimiter, n_skip_cols, keep_alloc, quiet, ierr)

        !> Input/output variables.
        integer, intent(in) :: iun
        character(len = *), dimension(:), allocatable :: values
        character, intent(in), optional :: delimiter
        integer, intent(in), optional :: n_skip_cols
        logical, intent(in), optional :: keep_alloc
        logical, intent(in), optional :: quiet
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line_buffer
        character(len = DEFAULT_FIELD_LENGTH), dimension(DEFAULT_LINE_LENGTH) :: vals
        character(len = DEFAULT_FIELD_LENGTH) code1, code2
        character sep
        integer j, i, iskip_cols, word_count
        logical v, in_whitespace, in_quotes, realloc

        !> Status.
        ierr = 0

        !> Verbosity.
        v = .true.
        if (present(quiet)) v = (.not. quiet)

        !> Read line.
        call read_txt_line(iun, line_buffer, quiet, ierr)
        if (ierr /= 0) return

        !> Deallocate input variable if allocated.
        realloc = .true.
        if (present(keep_alloc)) realloc = .not. keep_alloc
        if (realloc) then
            if (allocated(values)) deallocate(values)
        end if

        !> Delimiter.
        sep = ' '
        if (present(delimiter)) sep = delimiter

        !> Count the number of values in the line.
        in_quotes = .false.
        in_whitespace = .false.
        word_count = 1
        j = 1
        vals = ''
        do i = 1, len_trim(line_buffer)

            !> Convert tabs to spaces.
            if (line_buffer(i:i) == achar(9)) line_buffer(:) = ' '

            !> Check character.
            if (line_buffer(i:i) == sep .and. .not. in_quotes) then

                !> In whitespace.
                in_whitespace = .true.
            else if (line_buffer(i:i) == '"' .or. line_buffer(i:i) == "'") then

                !> Entering or exiting quote.
                in_quotes = (.not. in_quotes)
            else

                !> Start of new word if was in whitespace.
                if (in_whitespace) then
                    word_count = word_count + 1
                    j = 1
                    in_whitespace = .false.
                end if

                !> Add character to current value.
                vals(word_count)(j:j) = line_buffer(i:i)
                j = j + 1
            end if
        end do

        !> Clip internal list of values and save to output array.
        iskip_cols = 0
        if (present(n_skip_cols)) iskip_cols = n_skip_cols
        if (word_count <= iskip_cols) then
            ierr = 1
        else
            if (realloc) then
                allocate(values(word_count - iskip_cols))
            else if ((word_count - iskip_cols) < size(values)) then
                write(code1, *) (word_count - iskip_cols)
                write(code2, *) size(values)
                if (v) then
                    call print_warning( &
                        "The number of values read from the file (" // trim(adjustl(code1)) // &
                        ") is less than the expected number of (" // trim(adjustl(code2)) // "). " // &
                        "The resulting field may contain empty values.")
                end if
            else if ((word_count - iskip_cols) > size(values)) then
                write(code1, *) (word_count - iskip_cols)
                write(code2, *) size(values)
                if (v) then
                    call print_warning( &
                        "The number of values read from the file (" // trim(adjustl(code1)) // &
                        ") is greater than the expected number of (" // trim(adjustl(code2)) // "). " // &
                        "Trailing values may be omitted from the resulting field.")
                end if
            end if
            do i = 1, size(values)
                values(i) = trim(adjustl(vals(i + iskip_cols)))
            end do
        end if

    end subroutine

    !> Description:
    !>  Read a line to a list of fields of type 'real'. Performs the
    !>  same filtering as 'read_txt_line'.
    !>
    !> Input/output variables:
    !*  iun: Unit of opened file.
    !*  delimiter: Field delimiter (optional, default: space).
    !*  n_skip_cols: Number of columns to skip from the beginning of the line (optional).
    !*  keep_alloc: Preserve the existing allocation of 'values' (optional).
    !*  quiet: .true. to suppress messages (optional).
    !*  ierr: Status (0: normal).
    subroutine read_txt_line_args_real(iun, values, delimiter, n_skip_cols, keep_alloc, quiet, ierr)

        !> Input/output variables.
        integer, intent(in) :: iun
        real, dimension(:), allocatable :: values
        character, intent(in), optional :: delimiter
        integer, intent(in), optional :: n_skip_cols
        logical, intent(in), optional :: keep_alloc
        logical, intent(in), optional :: quiet
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH), allocatable :: vals(:)
        integer i
        logical v, realloc

        !> Verbosity.
        v = .true.
        if (present(quiet)) v = (.not. quiet)

        !> Deallocate input variable if allocated.
        realloc = .true.
        if (present(keep_alloc)) realloc = .not. keep_alloc
        if (realloc) then
            if (allocated(values)) deallocate(values)
        else
            allocate(vals(size(values)))
        end if

        !> Call base routine.
        call read_txt_line(iun, vals, delimiter, n_skip_cols, keep_alloc, quiet, ierr)
        if (ierr /= 0) return

        !> Convert and assign values.
        if (.not. allocated(values)) allocate(values(size(vals)))
        values = huge(values)
        do i = 1, size(vals)
            read(vals(i), *, iostat = ierr) values(i)
            if (ierr /= 0) then
                if (v) call print_warning("An error occurred converting values from the field.")
                return
            end if
        end do

    end subroutine

    !> Description:
    !>  Read a line to a list of fields of type 'integer'. Performs the
    !>  same filtering as 'read_txt_line'.
    !>
    !> Input/output variables:
    !*  iun: Unit of opened file.
    !*  delimiter: Field delimiter (optional, default: space).
    !*  n_skip_cols: Number of columns to skip from the beginning of the line (optional).
    !*  keep_alloc: Preserve the existing allocation of 'values' (optional).
    !*  quiet: .true. to suppress messages (optional).
    !*  ierr: Status (0: normal).
    subroutine read_txt_line_args_int(iun, values, delimiter, n_skip_cols, keep_alloc, quiet, ierr)

        !> Input/output variables.
        integer, intent(in) :: iun
        integer, dimension(:), allocatable :: values
        character, intent(in), optional :: delimiter
        integer, intent(in), optional :: n_skip_cols
        logical, intent(in), optional :: keep_alloc
        logical, intent(in), optional :: quiet
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH), allocatable :: vals(:)
        integer i
        logical v, realloc

        !> Verbosity.
        v = .true.
        if (present(quiet)) v = (.not. quiet)

        !> Deallocate input variable if allocated.
        realloc = .true.
        if (present(keep_alloc)) realloc = .not. keep_alloc
        if (realloc) then
            if (allocated(values)) deallocate(values)
        else
            allocate(vals(size(values)))
        end if

        !> Call base routine.
        call read_txt_line(iun, vals, delimiter, n_skip_cols, keep_alloc, quiet, ierr)
        if (ierr /= 0) return

        !> Convert and assign values.
        if (.not. allocated(values)) allocate(values(size(vals)))
        values = huge(values)
        do i = 1, size(vals)
            read(vals(i), *, iostat = ierr) values(i)
            if (ierr /= 0) then
                if (v) call print_warning("An error occurred converting values from the field.")
                return
            end if
        end do

    end subroutine

    !> Description:
    !>  Count the number of lines in the file skipping lines that start
    !>  with '!' or '#' (recognized as a comment).
    !>
    !> Variables:
    !>  iun: Opened file unit.
    !>  total_lines: The number of lines counted in the file.
    !>  ierr: Status (0: normal).
    subroutine count_txt_lines(iun, total_lines, ierr)

        !> Input/output variables.
        integer, intent(in) :: iun
        integer, intent(out) :: total_lines
        integer, intent(out) :: ierr

        !> Rewind the file (to count from the beginning).
        rewind(iun)

        !> Count the number of lines in the file.
        total_lines = 0
        ierr = 0
        do while (.true.)

            !> Read the line (skipping lines that start with '!' or '#' as a comment).
            call read_txt_line(iun, ierr = ierr)
            if (ierr < 0) then

                !> Exit on end of file.
                ierr = 0
                exit
            else if (ierr /= 0) then

                !> Exit with unknown error.
                exit
            else

                !> Increment the line count.
                total_lines = total_lines + 1
            end if
        end do

    end subroutine

    !> Description:
    !>  Function to read comma or space delimited records from a text
    !>  file, provided the unit. Stores the last read record in the array
    !>  provided. Returns the status of the read statement.
    !>
    !> Input/output:
    !>  iun: Unit of the file.
    !>  values: Values read from file.
    !>  nrows: Number of records to read, if to skip lines (optional).
    !>  ierr: Status of the read statement.
    integer function read_records_txt(iun, values, nrows) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun
        integer, intent(in), optional :: nrows
        real values(:)

        !> Local variables.
        integer n, i

        !> Initialize return variable.
        ierr = 0

        !> Read 'nrows' number of rows.
        n = 0
        if (present(nrows)) n = max(n, nrows - 1)
        do i = 1, n
            read(iun, *, iostat = ierr)
            if (ierr /= 0) exit
        end do

        !> Read record to array using free format.
        read(iun, *, iostat = ierr) (values(i), i = 1, size(values))

    end function

end module
