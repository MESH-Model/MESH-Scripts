!> Description:
!>  Module that contains subroutines and functions for parsing lines
!>  read from simple text and/or CSV format files.
module parse_utilities

    !> 'strings': For 'compact', 'parse' and 'value' functions.
    use strings

    implicit none

    !> Description:
    !>  Check the provided field is allocated and assigned.
    !>
    !> Input/output variables:
    !*  field: Field to check if allocated and assigned.
    !*  size1: Expected size of 'field' if passing a vector or an array (optional).
    !*  size2: Expected size of 'field' if passing an array (optional).
    !*  istat: Return status (returns 'istat' as provided if the field is allocated and assigned).
    interface check_allocated
        module procedure check_assigned_character
        module procedure check_assigned_integer
        module procedure check_assigned_real
        module procedure check_allocated_character1d
        module procedure check_allocated_integer1d
        module procedure check_allocated_real1d
        module procedure check_allocated_integer2d
        module procedure check_allocated_real2d
    end interface

    !> Description:
    !>  Allocate and initialize the provided field.
    !>
    !> Input/output variables:
    !*  field: Field to check if allocated and assigned.
    !*  size1: Size of 'field' in the first dimension (if passing a vector or array).
    !*  size2: Size of 'field' in the second dimension (if passing an array).
    !*  istat: Return status (returns 'istat' as provided if allocation returns normal).
    interface allocate_variable
        module procedure allocate_variable_character1d
        module procedure allocate_variable_integer1d
        module procedure allocate_variable_real1d
        module procedure allocate_variable_integer2d
        module procedure allocate_variable_real2d
    end interface

    !> Description:
    !>  Parse the values in a line by space and in consideration of quotes.
    !>
    !> Input/output variables:
    !*  line: Line containing values.
    !*  values: List of values found in the line.
    !*  istat: Return status.
    interface parse_line_values
        module procedure parse_line_values_char1d
        module procedure parse_line_values_character1d
        module procedure parse_line_values_integer1d
        module procedure parse_line_values_real1d
    end interface

    !> Description:
    !>  Assign the arguments provided from a string to the given field.
    !>
    !> Input/output variables:
    !*  field: Variable to assign values to (returns a warning if already allocated).
    !*  size1: Size of 'field' in the first dimension (if passing a vector or array).
    !*  size2: Size of 'field' in the second dimension (if passing an array).
    !*  values: Values to assign to 'field'.
    !*  istat: Return status.
    interface assign_line_args
        module procedure assign_line_args_character
        module procedure assign_line_args_integer
        module procedure assign_line_args_real
        module procedure assign_line_args_logical
        module procedure assign_line_args_character1d
        module procedure assign_line_args_integer1d
        module procedure assign_line_args_real1d
        module procedure assign_line_args_integer2d
        module procedure assign_line_args_real2d
    end interface

    !> Description:
    !>  Assign values to the given field.
    !>
    !> Input/output variables:
    !*  field: Variable to assign values to (returns a warning if already allocated).
    !*  size1: Size of 'field' in the first dimension (if passing a vector or array).
    !*  size2: Size of 'field' in the second dimension (if passing an array).
    !*  values: Values to assign to 'field'.
    !*  istat: Return status.
    interface assign_field
        module procedure assign_field_integer2d
        module procedure assign_field_real2d
    end interface

    !> Description:
    !>  Routine to map a mapped 2D array to a vector provided mapping indices.
    !>
    !> Input/output variables:
    !*  values: Array of values to assign to 'field'.
    !*  map1: Field index of dimension 1 from 'values'.
    !*  map2: Field index of dimension 2 from 'values'.
    !*  field: Variable to assign values to (returns a warning if already allocated).
    !*  istat: Return status.
    interface map_to_field
        module procedure map_to_field_integer2d
        module procedure map_to_field_real2d
    end interface

    !> Description:
    !>  Routine to map a vector to a mapped 2D array provided mapping indices.
    !>
    !> Input/output variables:
    !*  field: Vector of values to assign to 'mapped_values'.
    !*  map1: Field index of dimension 1 from 'values'.
    !*  map2: Field index of dimension 2 from 'values'.
    !*  no_data: No data value (where 'field' does not intersect 'mapped_values').
    !*  mapped_values: Array of values mapped from 'field'.
    !*  istat: Return status.
    interface field_to_mapped_values
        module procedure field_to_mapped_values_integer2d
        module procedure field_to_mapped_values_real2d
    end interface

    !> Description:
    !>  Type for status keys (to be interpreted by called routines).
    !>
    !> Variables:
    !*  NORMAL_STATUS: Normal status (no message).
    !*  COUNT_MISMATCH: When the number of values in 'args' does not match the expected number.
    !*  CONVERSION_ERROR: When an error occured converting the type of the variable to assign to the value.
    !*  BAD_FORMAT: When the provided field is not in the expected format.
    !*  BAD_DATE_FORMAT: When the provided date string is not in the expected format.
    !*  BAD_TIME_FORMAT: When the provided time string is not in the expected format.
    !*  OVERWRITING_FIELD: When a field to be assigned already contains data.
    !*  ALLOCATION_ERROR: When an error occurred while allocating a variable.
    !*  INACTIVE: When the specified field is associated with an inactive process (e.g., may not be used).
    !*  MISMATCHED_PRECISION: When the precision or length of variables to be assigned exceed the precision or length of the field.
    !*  MISMATCHED_BOUNDS: When the size of variables to be assigned is not the same as the field.
    !*  NOT_ALLOCATED: When a variable is determined to be unallocated.
    !*  ASSIGNED: When a variable is determined to contain values (e.g., not still equal to the initialization value).
    !*  UNRECOGNIZED: When the field is not recognized.
    type parse_status_keys
        integer :: NORMAL_STATUS = 0
        integer :: COUNT_MISMATCH = 1
        integer :: CONVERSION_ERROR = 2
        integer :: BAD_FORMAT = 3
        integer :: BAD_DATE_FORMAT = 4
        integer :: BAD_TIME_FORMAT = 5
        integer :: OVERWRITING_FIELD = 6
        integer :: ALLOCATION_ERROR = 7
        integer :: INACTIVE = 8
        integer :: MISMATCHED_PRECISION = 9
        integer :: MISMATCHED_BOUNDS = 10
        integer :: NOT_ALLOCATED = 11
        integer :: ASSIGNED = 12
        integer :: UNRECOGNIZED = 13
    end type

    !* pstat: Instance of status keys.
    type(parse_status_keys), save :: pstat

    !> Description:
    !>  Type of option keys (interpreted by routines).
    !>
    !> Variables:
    !*  MAP_ASSIGN_ORDER1: Assign to first index when provided.
    !*  MAP_ASSIGN_ORDER2: Assign to second index when provided.
    !*  MAP_ASSIGN_ORDER3: Assign to third index when provided.
    type parse_option_keys
        integer :: MAP_ASSIGN_ORDER1 = 1
        integer :: MAP_ASSIGN_ORDER2 = 2
        integer :: MAP_ASSIGN_ORDER3 = 3
    end type

    !* pkey: Instance of option keys.
    type(parse_option_keys), save :: pkey

    contains

    !> Description:
    !>  'check_allocated' for type character.
    subroutine check_assigned_character(field, istat)

        !> Input variables.
        character(len = *), intent(in) :: field

        !> Input/output variables.
        integer istat

        !> Check if the field is assigned.
        if (field /= achar(0)) then
            istat = istat + radix(istat)**pstat%ASSIGNED
        end if

    end subroutine

    !> Description:
    !>  'check_allocated' for type integer.
    subroutine check_assigned_integer(field, istat)

        !> Input variables.
        integer, intent(in) :: field

        !> Input/output variables.
        integer istat

        !> Check if the field is assigned.
        if (field /= huge(field)) then
            istat = istat + radix(istat)**pstat%ASSIGNED
        end if

    end subroutine

    !> Description:
    !>  'check_allocated' for type real.
    subroutine check_assigned_real(field, istat)

        !> Input variables.
        real, intent(in) :: field

        !> Input/output variables.
        integer istat

        !> Check if the field is assigned.
        if (field /= huge(field)) then
            istat = istat + radix(istat)**pstat%ASSIGNED
        end if

    end subroutine

    !> Description:
    !>  'check_allocated' for 1D vector of type character.
    subroutine check_allocated_character1d(field, size1, istat)

        !> Input variables.
        character(len = *), dimension(:), allocatable, intent(in) :: field

        !> Input variables (optional).
        integer, intent(in), optional :: size1

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= achar(0))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (present(size1)) then
                if (size(field) /= size1) then
                    istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
                end if
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    !> Description:
    !>  'check_allocated' for 1D vector of type integer.
    subroutine check_allocated_integer1d(field, size1, istat)

        !> Input variables.
        integer, dimension(:), allocatable, intent(in) :: field

        !> Input variables (optional).
        integer, intent(in), optional :: size1

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (present(size1)) then
                if (size(field) /= size1) then
                    istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
                end if
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    !> Description:
    !>  'check_allocated' for 1D vector of type real.
    subroutine check_allocated_real1d(field, size1, istat)

        !> Input variables.
        real, dimension(:), allocatable, intent(in) :: field

        !> Input variables (optional).
        integer, intent(in), optional :: size1

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (present(size1)) then
                if (size(field) /= size1) then
                    istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
                end if
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    !> Description.
    !>  'check_allocated' for 2D array of type integer.
    subroutine check_allocated_integer2d(field, size1, size2, istat)

        !> Input variables.
        integer, dimension(:, :), allocatable, intent(in) :: field

        !> Input variables (optional).
        integer, intent(in), optional :: size1, size2

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (present(size1) .and. present(size2)) then
                if (size(field, 1) /= size1 .or. size(field, 2) /= size2) then
                    istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
                end if
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    !> Description:
    !>  'check_allocated' for 2D array of type real.
    subroutine check_allocated_real2d(field, size1, size2, istat)

        !> Input variables.
        real, dimension(:, :), allocatable, intent(in) :: field

        !> Input variables (optional).
        integer, intent(in), optional :: size1, size2

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (present(size1) .and. present(size2)) then
                if (size(field, 1) /= size1 .or. size(field, 2) /= size2) then
                    istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
                end if
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    !> Description:
    !>  'allocate_variable' for 1D vector of type character.
    subroutine allocate_variable_character1d(field, size1, istat)

        !> Input variables.
        integer, intent(in) :: size1

        !> Input/output variables.
        character(len = *), dimension(:), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = achar(0)
        end if

    end subroutine

    !> Description:
    !>  'allocate_variable' for 1D vector of type integer.
    subroutine allocate_variable_integer1d(field, size1, istat)

        !> Input variables.
        integer, intent(in) :: size1

        !> Input/output variables.
        integer, dimension(:), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    !> Description:
    !>  'allocate_variable' for 1D vector of type real.
    subroutine allocate_variable_real1d(field, size1, istat)

        !> Input variables.
        integer, intent(in) :: size1

        !> Input/output variables.
        real, dimension(:), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    !> Description:
    !>  'allocate_variable' for 2D array of type integer.
    subroutine allocate_variable_integer2d(field, size1, size2, istat)

        !> Input variables.
        integer, intent(in) :: size1, size2

        !> Input/output variables.
        integer, dimension(:, :), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1, size2), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    !> Description:
    !>  'allocate_variable' for 2D array of type real.
    subroutine allocate_variable_real2d(field, size1, size2, istat)

        !> Input variables.
        integer, intent(in) :: size1, size2

        !> Input/output variables.
        real, dimension(:, :), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1, size2), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    subroutine parse_line_values_char1d(line, delimiter, n_skip_cols, keep_alloc, quiet, values, error_status)

        !> 'print_routines': For print routines, format statements, and line lengths and limits.
        use print_routines

        !> Input/output variables.
        character(len = *), intent(in) :: line
        character, intent(in), optional :: delimiter
        integer, intent(in), optional :: n_skip_cols
        logical, intent(in), optional :: keep_alloc
        logical, intent(in), optional :: quiet
        character(len = *), dimension(:), allocatable :: values
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = len_trim(line)) :: line_buffer
        character(len = len_trim(line)), dimension(len_trim(line)) :: vals
        character(len = DEFAULT_FIELD_LENGTH) code1, code2
        character sep
        integer j, i, iskip_cols, word_count
        logical v, in_whitespace, in_quotes, realloc_values

        !> Status.
        error_status = 0

        !> Verbosity.
        v = .true.
        if (present(quiet)) v = (.not. quiet)

        !> Deallocate input variable if allocated.
        realloc_values = .true.
        if (present(keep_alloc)) realloc_values = .not. keep_alloc
        if (realloc_values) then
            if (allocated(values)) deallocate(values)
        else

            !> Reset values.
            values = ''
        end if

        !> Make a copy of the line.
        line_buffer = trim(line)

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
            write(code1, *) iskip_cols
            write(code2, *) word_count
            if (v) call print_error( &
                "The number of values parsed from the line (" // trim(adjustl(code1)) // &
                ") is less than or equal to the number of values to skip from the beginning of the line (" // &
                trim(adjustl(code2)) // ").")
            error_status = 1
        else
            if (realloc_values) then
                allocate(values(word_count - iskip_cols))
            else if ((word_count - iskip_cols) < size(values)) then
                write(code1, *) (word_count - iskip_cols)
                write(code2, *) size(values)
                if (v) then
                    call print_warning( &
                        "The number of values parsed from the line (" // trim(adjustl(code1)) // &
                        ") is less than the expected number of values (" // trim(adjustl(code2)) // "). " // &
                        "The resulting field contains empty values.")
                end if
            else if ((word_count - iskip_cols) > size(values)) then
                write(code1, *) (word_count - iskip_cols)
                write(code2, *) size(values)
                if (v) then
                    call print_warning( &
                        "The number of values parsed from the line (" // trim(adjustl(code1)) // &
                        ") is greater than the expected number of values (" // trim(adjustl(code2)) // "). " // &
                        "Trailing values are omitted from the resulting field.")
                end if
            end if
            do i = 1, size(values)
                values(i) = trim(adjustl(vals(i + iskip_cols)))
            end do
        end if

    end subroutine

    !> Description:
    !>  'parse_line_values' for type character.
    subroutine parse_line_values_character1d(line, values, max_vals, istat)

        !> Input variables.
        character(len = *), intent(in) :: line

        !> Input variables (optional).
        integer, intent(in), optional :: max_vals

        !> Output variables.
        character(len = *), dimension(:), allocatable, intent(out) :: values
        integer, intent(out) :: istat

        !> Local variables.
        integer n, m, j, i
        logical in_quotes

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and de-allocate if necessary.
        call check_allocated(values, istat = istat)
        if (.not. btest(istat, pstat%NOT_ALLOCATED)) then
            deallocate(values)
        end if

        !> Cycle through 'line' to count the values delimited by space in consideration of quotes.
        in_quotes = .false.
        n = 1
        do i = 1, len_trim(line)

            !> Increment the number of values if a space and not currently in quotes.
            if (line(i:i) == ' ' .and. .not. in_quotes) n = n + 1

            !> Switch the status of 'in_quotes' if a quote character.
            if (line(i:i) == '"' .or. line(i:i) == "'") in_quotes = (.not. in_quotes)
        end do

        !> Limit the maximum number of values (if provided 'max_vals').
        if (present(max_vals)) n = min(n, max_vals)
        m = n

        !> Allocate and initialize 'values'.
        allocate(values(n))
        values = ''

        !> Parse and assign values.
        in_quotes = .false.
        n = 1
        j = 1
        do i = 1, len_trim(line)

            !> Increment 'n' and cycle if at the beginning of a new value.
            if (line(i:i) == ' ' .and. .not. in_quotes .and. n < m) then
                n = n + 1
                j = 1
                cycle
            end if

            !> Switch the status of 'in_quotes' and cycle if a quote character.
            if (line(i:i) == '"' .or. line(i:i) == "'") then
                in_quotes = (.not. in_quotes)
                cycle
            end if

            !> Add the character to the current value.
            values(n)(j:j) = line(i:i)

            !> Increment the current position in the string.
            j = j + 1
        end do

    end subroutine

    !> Description:
    !>  'parse_line_values' for type integer.
    subroutine parse_line_values_integer1d(line, values, max_vals, istat)

        !> strings: For 'value' function.
        !> print_routines: For 'DEFAULT_FIELD_LENGTH' constant.
        use strings
        use print_routines

        !> Input variables.
        character(len = *), intent(in) :: line

        !> Input variables (optional).
        integer, intent(in), optional :: max_vals

        !> Output variables.
        integer, dimension(:), allocatable, intent(out) :: values
        integer, intent(out) :: istat

        !> Local variables.
        integer n, i, z
        character(len = DEFAULT_FIELD_LENGTH), dimension(:), allocatable :: cvalues

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and de-allocate if necessary.
        call check_allocated(values, istat = istat)
        if (.not. btest(istat, pstat%NOT_ALLOCATED)) then
            deallocate(values)
        end if

        !> Call base routine.
        call parse_line_values(line, cvalues, max_vals, istat)
        if (istat /= pstat%NORMAL_STATUS) return

        !> Allocate the field.
        if (present(max_vals)) then
            n = max_vals
        else
            n = size(cvalues)
        end if
        call allocate_variable(values, n, istat)

        !> Convert values.
        z = 0
        do i = 1, n
            if (z == 0) call value(cvalues(min(i, n)), values(i), z)
        end do

        !> Check for errors.
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%CONVERSION_ERROR
        end if

    end subroutine

    !> Description:
    !>  'parse_line_values' for type real.
    subroutine parse_line_values_real1d(line, values, max_vals, istat)

        !> strings: For 'value' function.
        !> print_routines: For 'DEFAULT_FIELD_LENGTH' constant.
        use strings
        use print_routines

        !> Input variables.
        character(len = *), intent(in) :: line

        !> Input variables (optional).
        integer, intent(in), optional :: max_vals

        !> Output variables.
        real, dimension(:), allocatable, intent(out) :: values
        integer, intent(out) :: istat

        !> Local variables.
        integer n, i, z
        character(len = DEFAULT_FIELD_LENGTH), dimension(:), allocatable :: cvalues

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and de-allocate if necessary.
        call check_allocated(values, istat = istat)
        if (.not. btest(istat, pstat%NOT_ALLOCATED)) then
            deallocate(values)
        end if

        !> Call base routine.
        call parse_line_values(line, cvalues, max_vals, istat)
        if (istat /= pstat%NORMAL_STATUS) return

        !> Allocate the field.
        if (present(max_vals)) then
            n = max_vals
        else
            n = size(cvalues)
        end if
        call allocate_variable(values, n, istat)

        !> Convert values.
        z = 0
        do i = 1, n
            if (z == 0) call value(cvalues(min(i, n)), values(i), z)
        end do

        !> Check for errors.
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%CONVERSION_ERROR
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for type character.
    subroutine assign_line_args_character(field, values, istat)

        !> Input variables.
        character(len = *), intent(in) :: values

        !> Input/output variables.
        character(len = *) field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if variable allocate (issue warning if already allocated).
        call check_allocated(field, istat)
        if (btest(istat, pstat%ASSIGNED)) then
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if

        !> Check field length.
        if (len_trim(values) > len(field)) then
            istat = istat + radix(istat)**pstat%MISMATCHED_PRECISION
        end if

        !> Extract the fields.
        field = trim(adjustl(values))

    end subroutine

    !> Description:
    !>  'assign_line_args' for type integer.
    subroutine assign_line_args_integer(field, values, istat)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: values

        !> Input/output variables.
        integer field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if variable allocate (issue warning if already allocated).
        call check_allocated(field, istat)
        if (btest(istat, pstat%ASSIGNED)) then
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if

        !> Extract the field.
        z = 0
        call value(values, field, z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%CONVERSION_ERROR
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for type real.
    subroutine assign_line_args_real(field, values, istat)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: values

        !> Input/output variables.
        real field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if variable allocate (issue warning if already allocated).
        call check_allocated(field, istat)
        if (btest(istat, pstat%ASSIGNED)) then
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if

        !> Extract the field.
        z = 0
        call value(values, field, z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%CONVERSION_ERROR
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for type logical.
    subroutine assign_line_args_logical(field, values, istat)

        !> strings: For 'lowercase' function.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: values

        !> Input/output variables.
        logical field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Extract the fields.
        field = (values == '1' .or. lowercase(values) == 'on' .or. lowercase(values) == '.true.' .or. lowercase(values) == 'true')

    end subroutine

    !> Description:
    !>  'assign_line_args' for 1D vector of type character.
    subroutine assign_line_args_character1d(field, size1, values, istat)

        !> Input variables.
        integer, intent(in) :: size1
        character(len = *), dimension(:), intent(in) :: values

        !> Input/output variables.
        character(len = *), dimension(:), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer n, i, z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, istat)
        end if

        !> Check for consistency of values.
        n = size(values)
        if (n /= size1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the fields.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. n >= 1) then

            !> Check field length.
            if (len_trim(values(i)) > len(field) .and. .not. btest(istat, pstat%MISMATCHED_PRECISION)) then
                istat = istat + radix(istat)**pstat%MISMATCHED_PRECISION
            end if

            !> Assign value.
            do i = 1, min(size1, size(field))
                if (i > n) then
                    field(i) = trim(adjustl(values(n)))
                else
                    field(i) = trim(adjustl(values(i)))
                end if
            end do
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for 1D vector of type integer.
    subroutine assign_line_args_integer1d(field, size1, values, istat)

        !> Input/output variables.
        integer, intent(in) :: size1
        character(len = *), dimension(:), intent(in) :: values
        integer, dimension(:), allocatable :: field
        integer, intent(out) :: istat

        !> Local variables.
        integer n, i, z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, istat)
        end if

        !> Check for consistency of values.
        n = size(values)
        if (n /= size1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the fields.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. n >= 1) then
            do i = 1, min(size1, size(field))
                z = 0
                if (i > n) then
                    call value(values(n), field(i), z)
                else
                    call value(values(i), field(i), z)
                end if
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for 1D vector of type real.
    subroutine assign_line_args_real1d(field, size1, values, istat)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: size1
        character(len = *), dimension(:), intent(in) :: values

        !> Input/output variables.
        real, dimension(:), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer n, i, z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, istat)
        end if

        !> Check for consistency of values.
        n = size(values)
        if (n /= size1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the fields.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. n >= 1) then
            do i = 1, min(size1, size(field))
                z = 0
                if (i > n) then
                    call value(values(n), field(i), z)
                else
                    call value(values(i), field(i), z)
                end if
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for 2D array of type integer.
    subroutine assign_line_args_integer2d(field, size1, size2, values, map_order, istat, element_id)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: size1, size2, map_order
        character(len = *), dimension(:), intent(in) :: values
        integer, intent(in), optional :: element_id

        !> Input/output variables.
        integer, dimension(:, :), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer, dimension(:), allocatable :: ival
        integer n, i, z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, size2, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, size2, istat)
        end if

        !> Extract the fields.
        n = size(values)
        z = 0
        call allocate_variable(ival, n, z)
        if (.not. btest(z, pstat%ALLOCATION_ERROR) .and. n >= 1) then
            do i = 1, n
                z = 0
                call value(values(i), ival(i), z)
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do

            !> Check for mapping.
            if (map_order == pkey%MAP_ASSIGN_ORDER1) then

                !> Check for consistency of values.
                if (n /= size1) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, min(size1, size(field, 1))
                    if (present(element_id)) then
                        field(min(i, size1), min(max(element_id, 1), size2)) = ival(min(i, n))
                    else
                        field(min(i, size1), :) = ival(min(i, n))
                    end if
                end do
            else

                !> Check for consistency of values.
                if (n /= size2) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, min(size2, size(field, 2))
                    if (present(element_id)) then
                        field(min(max(element_id, 1), size1), min(i, size2)) = ival(min(i, n))
                    else
                        field(:, min(i, size2)) = ival(min(i, n))
                    end if
                end do
            end if
        end if

    end subroutine

    !> Description:
    !>  'assign_line_args' for 2D array of type real.
    subroutine assign_line_args_real2d(field, size1, size2, values, map_order, istat, element_id)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: size1, size2, map_order
        character(len = *), dimension(:), intent(in) :: values
        integer, intent(in), optional :: element_id

        !> Input/output variables.
        real, dimension(:, :), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        real, dimension(:), allocatable :: fval
        integer n, i, z

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, size2, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, size2, istat)
        end if

        !> Extract the fields.
        n = size(values)
        z = 0
        call allocate_variable(fval, n, z)
        if (.not. btest(z, pstat%ALLOCATION_ERROR) .and. n >= 1) then
            do i = 1, n
                z = 0
                call value(values(i), fval(i), z)
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do

            !> Check for mapping.
            if (map_order == pkey%MAP_ASSIGN_ORDER1) then

                !> Check for consistency of values.
                if (n /= size1) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, min(size1, size(field, 1))
                    if (present(element_id)) then
                        field(min(i, size1), min(max(element_id, 1), size2)) = fval(min(i, n))
                    else
                        field(min(i, size1), :) = fval(min(i, n))
                    end if
                end do
            else

                !> Check for consistency of values.
                if (n /= size2) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, min(size2, size(field, 2))
                    if (present(element_id)) then
                        field(min(max(element_id, 1), size1), min(i, size2)) = fval(min(i, n))
                    else
                        field(:, min(i, size2)) = fval(min(i, n))
                    end if
                end do
            end if
        end if

    end subroutine

    !> Description:
    !>  'assign_field' for 2D array of type integer.
    subroutine assign_field_integer2d(field, size1, size2, values, istat)

        !> Input variables.
        integer, intent(in) :: size1, size2
        integer, dimension(:, :), intent(in) :: values

        !> Input/output variables.
        integer, dimension(:, :), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check for consistency in bounds.
        if (size(values, 1) /= size1 .or. size(values, 2) /= size2) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, size2, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, size2, istat)
        end if

        !> Assign the field.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. .not. btest(istat, pstat%COUNT_MISMATCH)) then
            field = values
        end if

    end subroutine

    !> Description:
    !>  'assign_field' for 2D array of type real.
    subroutine assign_field_real2d(field, size1, size2, values, istat)

        !> Input variables.
        integer, intent(in) :: size1, size2
        real, dimension(:, :), intent(in) :: values

        !> Input/output variables.
        real, dimension(:, :), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, size2, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, size2, istat)
        end if

        !> Assign the field.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. .not. btest(istat, pstat%COUNT_MISMATCH)) then
            field = values
        end if

    end subroutine

    !> Description:
    !>  'map_to_field' for type integer.
    subroutine map_to_field_integer2d(values, map1, map2, field, istat)

        !> Input variables.
        integer, dimension(:, :), intent(in) :: values
        integer, dimension(:), intent(in) :: map1, map2

        !> Input/output variables.
        integer, dimension(:) :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer n

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check for consistency in bounds.
        if ((maxval(map1) > size(values, 1) .or. maxval(map2) > size(values, 2)) .or. &
            (size(map1) /= size(field) .or. size(map2) /= size(field))) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Assign the field.
        if (.not. btest(istat, pstat%COUNT_MISMATCH)) then
            do n = 1, size(field)
                field(n) = values(map1(n), map2(n))
            end do
        end if

    end subroutine

    !> Description:
    !>  'map_to_field' for type real.
    subroutine map_to_field_real2d(values, map1, map2, field, istat)

        !> Input variables.
        real, dimension(:, :), intent(in) :: values
        integer, dimension(:), intent(in) :: map1, map2

        !> Input/output variables.
        real, dimension(:) :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer n

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check for consistency in bounds.
        if ((maxval(map1) > size(values, 1) .or. maxval(map2) > size(values, 2)) .or. &
            (size(map1) /= size(field) .or. size(map2) /= size(field))) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Assign the field.
        if (.not. btest(istat, pstat%COUNT_MISMATCH)) then
            do n = 1, size(field)
                field(n) = values(map1(n), map2(n))
            end do
        end if

    end subroutine

    !> Description:
    !>  'field_to_mapped_values' for type integer.
    subroutine field_to_mapped_values_integer2d(field, map1, map2, no_data, mapped_values, istat)

        !> Input variables.
        integer, dimension(:), intent(in) :: field
        integer, dimension(:), intent(in) :: map1, map2
        integer, intent(in), optional :: no_data

        !> Input/output variables.
        integer, dimension(:, :) :: mapped_values

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer n

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check for consistency in bounds.
        if ((maxval(map1) > size(mapped_values, 1) .or. maxval(map2) > size(mapped_values, 2)) .or. &
            (size(map1) /= size(field) .or. size(map2) /= size(field))) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Assign the field.
        if (.not. btest(istat, pstat%COUNT_MISMATCH)) then
            if (present(no_data)) then
                mapped_values = no_data
            else
                mapped_values = 0
            end if
            do n = 1, size(field)
                mapped_values(map1(n), map2(n)) = field(n)
            end do
        end if

    end subroutine

    !> Description:
    !>  'field_to_mapped_values' for type real.
    subroutine field_to_mapped_values_real2d(field, map1, map2, no_data, mapped_values, istat)

        !> Input variables.
        real, dimension(:), intent(in) :: field
        integer, dimension(:), intent(in) :: map1, map2
        real, intent(in), optional :: no_data

        !> Input/output variables.
        real, dimension(:, :) :: mapped_values

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer n

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check for consistency in bounds.
        if ((maxval(map1) > size(mapped_values, 1) .or. maxval(map2) > size(mapped_values, 2)) .or. &
            (size(map1) /= size(field) .or. size(map2) /= size(field))) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Assign the field.
        if (.not. btest(istat, pstat%COUNT_MISMATCH)) then
            if (present(no_data)) then
                mapped_values = no_data
            else
                mapped_values = 0.0
            end if
            do n = 1, size(field)
                mapped_values(map1(n), map2(n)) = field(n)
            end do
        end if

    end subroutine

    !> Description:
    !>  Split the provided time string into components of hours, minutes, and seconds.
    !>
    !> Input:
    !*  time: Time string to parse.
    !>
    !> Output:
    !*  hour: Hour in day component of date.
    !*  minutes: Minutes in hour component of date.
    !*  seconds: Seconds in minutes component of date.
    !*  utc_offset: Offset from UTC provided a time zone (optional).
    !*  istat: Return status.
    subroutine parse_time(time, hour, minutes, seconds, utc_offset, istat)

        !> strings: For 'compact', 'parse' and 'value' functions.
        !> print_routines: For 'DEFAULT_FIELD_LENGTH' constant.
        use strings
        use print_routines

        !> Input variables.
        character(len = *), intent(in) :: time

        !> Output variables (optional).
        integer, intent(out), optional :: hour, minutes, seconds
        real, intent(out), optional :: utc_offset

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer h, m, s, i, z
        real u
        character(len = DEFAULT_FIELD_LENGTH) ctime, czone

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check dimensions of the variable.
        if (len_trim(time) > len(ctime)) then
            istat = radix(istat)**pstat%MISMATCHED_PRECISION
        else

            !> Assign to local variable.
            ctime = adjustl(time)
        end if

        !> Compact and convert the line to uppercase.
        call compact(ctime)
        ctime = uppercase(ctime)

        !> Check for a time zone signature.
        u = 0.0
        if (index(ctime, 'Z') > 0) then

            !> Replace the 'Z' UTC symbol (offset is zero).
            ctime = ctime(1:(index(ctime, 'Z') - 1))
        else if (index(ctime, '-') /= 0 .or. index(ctime, '+') /= 0) then

            !> '+02:00', '+02', '+2' format (+/-).
            if (index(ctime, '-') /= 0) then
                i = index(ctime, '-')
            else
                i = index(ctime, '+')
            end if
            czone = ctime(i:)

            !> Parse the time zone.
            z = 0
            h = 0
            m = 0
            if (index(czone, ':') > 1) then

                !> Format contains ':'.
                if (z == 0) call value(czone(1:(index(czone, ':') - 1)), h, z)
                if (z == 0) call value(czone((index(czone, ':') + 1):), m, z)
            else

                !> Format is '-530' or '-0530' (or format is '-6' or '-06' and value is used as-is).
                if (z == 0) call value(czone, h, z)
                if (z == 0) then
                    if (abs(h) > 99) then
                        m = h - floor(h/100.0)*100
                        h = floor(h/100.0)
                    end if
                    u = h + real(m)/60.0
                end if
            end if

            !> Check for conversion errors.
            if (z /= 0) then
                istat = istat + radix(istat)**pstat%CONVERSION_ERROR
            else
                ctime = ctime(1:(i - 1))
            end if
        end if

        !> Parse time.
        if (.not. btest(istat, pstat%CONVERSION_ERROR)) then

            !> Assume an hourly value if no colon ':' exists (e.g., '24').
            h = 0
            m = 0
            s = 0
            if (index(ctime, ':') == 0) then

                !> Try to convert the value.
                z = 0
                call value(ctime, h, z)

                !> Check for errors.
                if (z /= 0) then

                    !> Conversion error.
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                else if (h < 0 .or. h > 24) then

                    !> Invalid value.
                    istat = istat + radix(istat)**pstat%BAD_TIME_FORMAT
                end if
            else

                !> Split the time into its components where colon(s) ':' exist
                !>  (e.g., '24:00:00.000', '24:00:00', '24:00').
                z = 0
                if (index(ctime, ':') > 1) then
                    call value(ctime(1:(index(ctime, ':') - 1)), h, z)
                    if (index(ctime, ':') /= index(ctime, ':', back = .true.)) then
                        if (z == 0) call value(ctime((index(ctime, ':') + 1):(index(ctime, ':', back = .true.) - 1)), m, z)
                        if (z == 0) call value(ctime((index(ctime, ':', back = .true.) + 1):), s, z)
                    else
                        if (z == 0) call value(ctime((index(ctime, ':') + 1):), m, z)
                    end if
                end if

                !> Check for conversion errors.
                if (z /= 0) istat = istat + radix(istat)**pstat%CONVERSION_ERROR
            end if
        end if

        !> Update output variables (if no errors occurred).
        if (.not. btest(istat, pstat%NORMAL_STATUS)) then
            h = 0; m = 0; s = 0; u = 0.0
        end if
        if (present(hour)) hour = h
        if (present(minutes)) minutes = m
        if (present(seconds)) seconds = s
        if (present(utc_offset)) utc_offset = u

    end subroutine

    !> Description:
    !>  Parse the start time in the file header into individual components.
    !>
    !> Input:
    !*  datetime: Date/time structure to parse.
    !>
    !> Output:
    !*  year: Year component of date.
    !*  month: Month component of date.
    !*  day: Day in month component of date.
    !*  hour: Hour in day component of date.
    !*  minutes: Minutes in hour component of date.
    !*  seconds: Seconds in minutes component of date.
    !*  utc_offset: Offset from UTC provided a time zone (optional).
    !*  istat: Return status.
    subroutine parse_datetime(datetime, year, month, day, hour, minutes, seconds, utc_offset, istat)

        !> strings: For 'compact', 'parse' and 'value' functions.
        !> print_routines: For 'DEFAULT_FIELD_LENGTH' constant.
        use strings
        use print_routines

        !> Input variables.
        character(len = *), intent(in) :: datetime

        !> Output variables (optional).
        integer, intent(out), optional :: year, month, day, hour, minutes, seconds
        real, intent(out), optional :: utc_offset

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer y, m, d, i, z
        character(len = DEFAULT_FIELD_LENGTH) cdate, ctime

        !> Initialize return variable.
        istat = radix(istat)**pstat%NORMAL_STATUS

        !> Check dimensions of the variable.
        if (len_trim(datetime) > len(cdate)) then
            istat = radix(istat)**pstat%MISMATCHED_PRECISION
        else

            !> Assign to local variable.
            cdate = adjustl(datetime)
        end if

        !> Compact the line.
        call compact(cdate)

        !> Replace forward-slashes with dashes.
        do i = 1, len_trim(cdate)
            if (cdate(i:i) == '/') cdate(i:i) = '-'
        end do

        !> Search for a time signature.
        if (index(cdate, '-') == 0) then
            ctime = cdate
            cdate = ''
        else if (index(trim(cdate), ' ') > 0) then
            ctime = cdate((index(trim(cdate), ' ') + 1):)
            cdate = cdate(1:(index(trim(cdate), ' ') - 1))
        else
            ctime = ''
            cdate = cdate
        end if

        !> Call 'parse_time' to extract the time components.
        if (len_trim(ctime) > 0) then
            call parse_time(ctime, hour, minutes, seconds, utc_offset, istat)
        end if

        !> Parse the date.
        if (len_trim(cdate) > 0 .and. btest(istat, pstat%NORMAL_STATUS)) then

            !> Scan for a date signature (e.g., 2003-01-31).
            z = 0
            y = 0
            m = 0
            d = 0
            if (index(cdate, '-') > 1 .and. index(cdate, '-') /= index(cdate, '-', back = .true.)) then
                call value(cdate(1:(index(cdate, '-') - 1)), y, z)
                if (z == 0) call value(cdate((index(cdate, '-') + 1):(index(cdate, '-', back = .true.) - 1)), m, z)
                if (z == 0) call value(cdate((index(cdate, '-', back = .true.) + 1):), d, z)

                !> Check for converion errors.
                if (z /= 0) istat = istat + radix(istat)**pstat%CONVERSION_ERROR
            else

                !> Bad date format.
                istat = istat + radix(istat)**pstat%BAD_DATE_FORMAT
            end if
        end if

        !> Update output variables (if no errors occurred).
        if (.not. btest(istat, pstat%NORMAL_STATUS)) then
            y = 0; m = 0; d = 0
        end if
        if (present(year)) year = y
        if (present(month)) month = m
        if (present(day)) day = d

    end subroutine

end module
