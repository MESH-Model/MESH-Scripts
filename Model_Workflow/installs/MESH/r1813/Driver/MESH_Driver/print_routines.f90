module print_routines

    implicit none

    !> Control flag constants.
    character(len = *), parameter :: DIAGNOSEMODE_NAME = 'DIAGNOSEMODE'
    character(len = *), parameter :: ECHOTXTMODE_NAME = 'ECHOTXTMODE'
    character(len = *), parameter :: PRINTSIMSTATUS_NAME = 'PRINTSIMSTATUS'

    !> Control flags.
    !* ISHEADNODE: .true. if node passes messages to output (default: .true.; .false. for worker nodes).
    !* DIAGNOSEMODE: .true. to print diagnostic and excess information (default: .false.).
    !* ECHOTXTMODE: .true. to echo screen output to the output file (default: .true.).
    logical :: ISHEADNODE = .true.
    logical :: DIAGNOSEMODE = .false.
    logical :: ECHOTXTMODE = .true.

    !> Flag constants.
    integer, parameter :: OUT_NONE = 0
    integer, parameter :: OUT_JDATE_DLY = 1
    integer, parameter :: OUT_JDATE_MLY = 2
    integer, parameter :: OUT_DATE_DLY = 3
    integer, parameter :: OUT_DATE_MLY = 4

    !> Options.
    !* PRINTSIMSTATUS: Flag to control how frequently the model prints simulation status messages to screen (default: OUT_DLY).
    integer :: PRINTSIMSTATUS = OUT_JDATE_DLY

    !> File units.
    !* ECHO_SCN_IUN: Unit of screen (for print).
    !* ECHO_TXT_IUN: Unit of summary file (for write).
    integer, private, save :: ECHO_SCN_IUN = 6, ECHO_TXT_IUN = -1

    !> Padding constants.
    !* PAD_LEAD: Padding of '1x'.
    !* PAD_1: Padding of '1x'.
    !* PAD_3: Padding of '3x'.
    !* PAD_NONE: No padding.
    integer, parameter, private :: PAD_LEAD = 1
!-    integer, parameter :: PAD_1 = 1
!-    integer, parameter :: PAD_3 = 3
    integer, parameter, private :: PAD_NONE = 0

    !> Internal pad position.
    integer, private :: PAD_NOW = PAD_LEAD

    !> Line and field length constants.
    !* DEFAULT_LINE_LENGTH: Default length of a single line.
    !* DEFAULT_FIELD_LENGTH: Default length of a field (e.g., in a line).
    !* DEFAULT_FIELD_COUNT: Default number of fields to a line.
    integer, parameter :: DEFAULT_LINE_LENGTH = 10000
    integer, parameter :: DEFAULT_FIELD_LENGTH = 1000
    integer, parameter :: DEFAULT_FIELD_COUNT = 100

    !> Format constants (general).
    character(len = *), parameter :: FMT_GEN = "(99999(g15.7e2, 1x))"
    character(len = *), parameter :: FMT_CSV = "(99999(g15.7e2, ','))"
    character(len = *), parameter :: FMT_CHR = "(a)"

    !> Format constants (dates).
    character(len = *), parameter :: FMT_TIME_HMS = "(i2.2, ':', i2.2, ':', i2.2, '.000000')"
    character(len = *), parameter :: FMT_TIME = FMT_TIME_HMS
    character(len = *), parameter :: FMT_DATE_DASHES_YMD = "(i4.4, '-', i2.2, '-', i2.2)"
    character(len = *), parameter :: FMT_DATETIME_DASHES_YMD = &
        "(i4.4, '-', i2.2, '-', i2.2, 1x, i2.2, ':', i2.2, ':', i2.2, '.000000')"
    character(len = *), parameter :: FMT_DATE_SLASHES_YMD = "(i4.4, '/', i2.2, '/', i2.2)"
    character(len = *), parameter :: FMT_DATE = FMT_DATE_SLASHES_YMD
    character(len = *), parameter :: FMT_DATETIME_SLASHES_YMD = &
        "(i4.4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':', i2.2, '.000000')"
    character(len = *), parameter :: FMT_DATE_SLASHES_YJD = "(i4.4, '/', i3.3)"
    character(len = *), parameter :: FMT_DATE_YJD = FMT_DATE_SLASHES_YJD

    contains

    !> Description:
    !>  Returns a default character format statement provided a level.
    !>
    !> Variables:
    !*  level: Offset from the leading edge of the line (optional; default: 1x).
    !>
    !> Returns:
    !*  f: Character format statement.
    function get_format(level) result(f)

        !> Input variables (optional).
        integer, intent(in), optional :: level

        !> Output variables.
        character(len = DEFAULT_LINE_LENGTH) f

        !> Format statement based on 'level'.
        if (present(level)) then
            if (level == PAD_NONE) then
                f = '((a))'
            else
                write(f, '(i4)') level
                f = '(' // trim(adjustl(f)) // 'x, (a))'
            end if
        else
            write(f, '(i4)') PAD_NOW
            f = '(' // trim(adjustl(f)) // 'x, (a))'
        end if

    end function

    !> Description:
    !>  Print the provided message to screen only.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    !>  no_advance: .true. not to advance to the next line (optional; default: .false.).
    subroutine print_screen(message, level, no_advance)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level
        logical, intent(in), optional :: no_advance

        !> Local variables.
        character(len = 3) :: advance_modifier = 'yes'

        !> Print to screen.
        if (ISHEADNODE) then
            if (present(no_advance)) then
                if (no_advance) advance_modifier = 'no'
            end if
            if (advance_modifier == 'no') then
                write(ECHO_SCN_IUN, get_format(PAD_NONE), advance = advance_modifier) message
            else
                write(ECHO_SCN_IUN, get_format(level)) message
            end if
        end if

    end subroutine

    !> Description:
    !>  Print the provided message to the summary file only.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    !>  no_advance: .true. not to advance to the next line (optional; default: .false.).
    subroutine print_echo_txt(message, level, no_advance)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level
        logical, intent(in), optional :: no_advance

        !> Local variables.
        character(len = 3) :: advance_modifier = 'yes'

        !> Print to the summary file.
        if (ISHEADNODE .and. ECHOTXTMODE .and. ECHO_TXT_IUN > 0) then
            if (present(no_advance)) then
                if (no_advance) advance_modifier = 'no'
            end if
            if (advance_modifier == 'no') then
                write(ECHO_TXT_IUN, get_format(PAD_NONE), advance = advance_modifier) message
            else
                write(ECHO_TXT_IUN, get_format(level)) message
            end if
        end if

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  padded by the specified number of spaces or using the current
    !>  tab if not explicitly provided.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    subroutine print_message(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Print to screen.
        call print_screen(trim(message), level)

        !> Print to the summary file.
        call print_echo_txt(trim(message), level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with leading "WARNING:", padded by the specified number of
    !>  spaces or using the current tab if not explicitly provided.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    subroutine print_warning(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Flush the message.
        call print_message('WARNING: ' // trim(message), level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with leading "REMARK:", padded by the specified number of spaces
    !>  or using the current tab if not explicitly provided.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    subroutine print_remark(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Flush the message.
        call print_message('REMARK: ' // trim(message), level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with leading "INFO:", padded by the specified number of spaces
    !>  or using the current tab if not explicitly provided.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    subroutine print_info(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Flush the message.
        call print_message('INFO: ' // trim(message), level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with leading "ERROR:", padded by the specified number of spaces
    !>  or using the current tab if not explicitly provided. If the
    !>  current tab is not indented, then the message is preceeded with
    !>  a blank line.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line (optional).
    subroutine print_error(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Print a leading line if the indent level is not present.
        if (.not. present(level) .and. PAD_NOW == PAD_LEAD) call print_message('')

        !> Flush the message.
        call print_message('ERROR: ' // trim(message))

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with padding added to the beginning of the line.
    !>
    !> Variables:
    !>  message: Message to output.
!todo: Remove; use 'increase_tab' and 'decrease_tab' instead.
!-    subroutine print_message_detail(message)

        !> Input variables.
!-        character(len = *), intent(in) :: message

        !> Flush the message.
!-        call print_message(message, PAD_3)

!-    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  preceeded with an empty line and with reset padding.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  leading_lines: Number of lines to precede the section (optional; default: 1).
    subroutine print_new_section(message, leading_lines)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: leading_lines

        !> Local variables.
        integer :: i, n = 1

        !> Reset padding and print empty line.
        call reset_tab()
        if (present(leading_lines)) n = max(leading_lines, 0)
        do i = 1, n
            call print_message('')
        end do

        !> Print to screen.
        call print_screen(message)

        !> Print to the summary file.
        call print_echo_txt(message)

    end subroutine

    !> Description:
    !>  Reset the starting position of message output.
    subroutine reset_tab()
        PAD_NOW = PAD_LEAD
    end subroutine

    !> Description:
    !>  Increase the starting position of message output.
    subroutine increase_tab()
        PAD_NOW = PAD_NOW + 2
    end subroutine

    !> Description:
    !>  Decrease the starting position of message output.
    subroutine decrease_tab()
        PAD_NOW = max(PAD_NOW - 2, PAD_LEAD)
    end subroutine

    !> Description:
    !>  Convert the provided time in seconds to a string noting the
    !>  equivalent number of hours, minutes, or seconds.
    !>
    !> Variables:
    !*  time: Time in seconds.
    !>
    !> Returns:
    !*  friendly_time_length: String of denominated units of time.
    function friendly_time_length(time)

        !> Input variables.
        real, intent(in) :: time

        !> Output variables.
        character(len = DEFAULT_FIELD_LENGTH) friendly_time_length

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) line

        !> Format time.
        if (time > 3600.0) then
            write(line, FMT_GEN) time/3600.0
            friendly_time_length = trim(adjustl(line)) // ' hours'
        else if (time > 60.0) then
            write(line, FMT_GEN) time/60.0
            friendly_time_length = trim(adjustl(line)) // ' minutes'
        else
            write(line, FMT_GEN) time
            friendly_time_length = trim(adjustl(line)) // ' seconds'
        end if

    end function

    !> Description:
    !>  Open the summary file.
    !>
    !> Variables:
    !*  path: Full path to the file.
    subroutine open_echo_txt(path, ierr)

        !> Input variables.
        character(len = *), intent(in) :: path

        !> Output variables.
        integer, intent(out) :: ierr

        !> Initialize the return status.
        ierr = 0

        !> Return if writing output to the file is disabled.
        !> Return if 'path' is empty.
        !> Return if 'VERBOSEMODE' is disabled.
        if (.not. ECHOTXTMODE .or. len_trim(path) == 0 .or. .not. ISHEADNODE) return

        !> Open the file and print an error if unsuccessful.
        ECHO_TXT_IUN = 58
        open(ECHO_TXT_IUN, file = path, status = 'replace', action = 'write', iostat = ierr)
        if (ierr /= 0) then

            !> Disable output to the file.
            ECHO_TXT_IUN = -1
            ECHOTXTMODE = .false.

            !> Print an error (to screen).
            call print_error('Unable to open file: ' // trim(path))
            call print_message('Check that the path exists, that the file it is not read-protected or open in another application.')
            return
        end if

    end subroutine

    !> Description:
    !>  Update options for printing messages.
    !>
    !> Variables:
    !*  option: Option to update.
    !*  values: Vector of configuration options.
    subroutine parse_options(option, values)

        use strings

        !> Input variables.
        character(len = *), intent(in) :: option, values(:)

        !> Local variables.
        integer i

        !> Update options.
        select case(option)
            case (DIAGNOSEMODE_NAME)
                do i = 1, size(values)
                    DIAGNOSEMODE = (values(i) == '1' .or. lowercase(values(i)) == 'on')
                end do
            case (ECHOTXTMODE_NAME)
                do i = 1, size(values)
                    ECHOTXTMODE = (values(i) == '1' .or. lowercase(values(i)) == 'on')
                end do
            case (PRINTSIMSTATUS_NAME)
                do i = 1, size(values)
                    select case (lowercase(values(i)))
                        case ('monthly')
                            PRINTSIMSTATUS = OUT_JDATE_MLY
                        case ('date_daily')
                            PRINTSIMSTATUS = OUT_DATE_DLY
                        case ('date_monthly')
                            PRINTSIMSTATUS = OUT_DATE_MLY
                        case ('1', 'on', 'default')
                            PRINTSIMSTATUS = OUT_JDATE_DLY
                        case ('0', 'off')
                            PRINTSIMSTATUS = OUT_NONE
                    end select
                end do
        end select

    end subroutine

end module
