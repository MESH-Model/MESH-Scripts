module date_utilities

    !> 'print_routines': For print routines, format statements, and line lengths and limits.
    use print_routines

    implicit none

    !> Tables.
    integer, dimension(12), private, target :: days_normal = (/31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365/)
    integer, dimension(12), private, target :: days_leap = (/31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366/)

    contains

    !> Description:
    !>  Check if the provided year is a leap year (.true.) or not
    !>  (.false.).
    logical function leap_year(year)

        !> Input/output variables.
        integer, intent(in) :: year

        !> Check if the provided year is a leap year.
        leap_year = ((mod(year, 4) == 0 .and. .not. mod(year, 100) == 0) .or. (mod(year, 400) == 0))

    end function

    !> Description:
    !>  Convert day of year and year to month and day of month.
    subroutine jday_to_date(year, jday, month, day)

        !> Input/output variables.
        integer, intent(in) :: year
        integer, intent(in) :: jday
        integer, intent(out) :: month
        integer, intent(out) :: day

        !> Local variables.
        integer, pointer :: d(:)
        integer i

        !> Identify the appropriate list.
        if (leap_year(year)) then
            d => days_leap
        else
            d => days_normal
        end if

        !> Find the date.
        if (jday <= d(1)) then
            month = 1
            day = jday
        else
            do i = 2, 12
                if (jday > d(i - 1) .and. jday <= d(i)) then
                    month = i
                    day = jday - d(i - 1)
                    exit
                end if
            end do
        end if

    end subroutine

    !> Description:
    !>  Convert month and day of month to day of year.
    integer function date_to_jday(year, month, day)

        !> Input/output variables.
        integer, intent(in) :: year
        integer, intent(in) :: month
        integer, intent(in) :: day

        !> Local variables.
        integer, pointer :: d(:)

        !> Identify the appropriate list.
        if (leap_year(year)) then
            d => days_leap
        else
            d => days_normal
        end if

        !> Find the day of year.
        if (month == 1) then
            date_to_jday = day
        else
            date_to_jday = d(month - 1) + day
        end if

    end function

    !> Description:
    !>  Calculate datetime from the date components.
    real(kind = kind(0.0d0)) function date_components_to_time(year, month, day, hour, minutes, seconds, error_status)

        !> Input/output variables.
        integer, intent(in) :: year
        integer, intent(in) :: month
        integer, intent(in) :: day
        integer, intent(in), optional :: hour
        integer, intent(in), optional :: minutes
        integer, intent(in), optional :: seconds
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = 4) code
        real(kind = kind(0.0d0)) :: h = 0.0, m = 0.0, s = 0.0

        !> Assign optional variables.
        if (present(hour)) h = real(hour, kind = kind(0.0d0))
        if (present(minutes)) m = real(minutes, kind = kind(0.0d0))
        if (present(seconds)) s = real(seconds, kind = kind(0.0d0))

        !> Check the reference year.
        if (year < 1601) then
            write(code, *) year
            call print_error( &
                "The reference year (" // code // ") of the specified date is less than 1601, which is not " // &
                "compatible with the date parsing utility.")
            error_status = 1
        else

            !> Calculate the datetime in days.
            date_components_to_time = &
                real( &
                    (1461*(year + 4800 + (month - 14)/12))/4 + (367*(month - 2 - 12*((month - 14)/12)))/12 - &
                    (3*((year + 4900 + (month - 14)/12)/100))/4 + day - 32075, kind = kind(0.0d0)) + &
                h/24.0 + m/24.0/60.0 + s/24.0/60.0/60.0
            error_status = 0
        end if

    end function

    !> Description:
    !>  Derive data components from the datetime.
    subroutine datetime_to_date_components(datetime, year, month, day, jday, hour, minutes, seconds)

        !> Input/output variables.
        real(kind = kind(0.0d0)), intent(in) :: datetime
        integer, intent(out), optional :: year
        integer, intent(out), optional :: month
        integer, intent(out), optional :: day
        integer, intent(out), optional :: jday
        integer, intent(out), optional :: hour
        integer, intent(out), optional :: minutes
        integer, intent(out), optional :: seconds

        !> Local variables.
        integer a, b, c, y, m, d, j, h, n, s

        !> Back-calculate components from the reference datetime in days.
        a = 4*(floor(datetime) + 1401 + (((4*floor(datetime) + 274277)/146097)*3)/4 - 38) + 3
        b = mod(a, 1461)/4
        c = 5*b + 2
        d = (mod(c, 153))/5 + 1
        m = mod((c/153 + 2), 12) + 1
        y = a/1461 - 4716 + (12 + 2 - m)/12
        j = date_to_jday(y, m, d)
        s = mod(nint((datetime - floor(datetime))*24.0*60.0*60.0), 60)
        n = mod(nint((datetime - floor(datetime))*24.0*60.0 - s/60.0), 60)
        h = nint((datetime - floor(datetime))*24.0 - n/60.0 - s/60.0/60.0)

        !> Variables.
        if (present(year)) year = y
        if (present(month)) month = m
        if (present(day)) day = d
        if (present(jday)) jday = j
        if (present(hour)) hour = h
        if (present(minutes)) minutes = n
        if (present(seconds)) seconds = s

    end subroutine

end module
