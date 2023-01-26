!> *********************************************************************
!> Athor: Gonzalo Sapriza Azuri
!> Description: Handled time steps and dates in MESH
!> *********************************************************************
module model_dates

    implicit none

    type dates_model

        integer, dimension(:, :), allocatable :: dates      !year,month,day,julian day
        integer, dimension(:), allocatable :: years         !array of years in annual freq
        integer, dimension(:, :), allocatable :: mnthyears  !array of month,year in month freq
        integer, dimension(:), allocatable :: daysINyears   !nr days in years
        integer, dimension(:), allocatable :: daysINmonths  !nr days in months
        integer, dimension(:), allocatable :: daysINseasons !nr days in Seasons
        character freq                                      !frequency
        integer nyears                                      !number oof years
        integer nseason                                     !12 months
        integer nmonths                                     !number of months
        integer nr_days                                     !number of days
        integer nr_timeStep                                 !total number of time steps

    end type !dates_model

    type counter_date

        !> Date (from the beginning of the year).
        !*  year: Year.
        !*  jday: Day from the beginning of the year.
        !*  month: Month.
        !*  day: Day in month.
        !*  hour: Hour.
        !*  mins: Half-hour in the hour (0 or 30 minutes).
        integer :: year = 0, jday = 0, month = 0, day = 0, hour = 0, mins = 0

    end type !counter_date_julian

    type counter

        !> Time-stepping information.
        !*  dts: Time-step in seconds.
        !*  dtmins: Time-step in minutes.
        integer :: dts = 1800, dtmins = 30

        !> Instances of 'counter_date'
        !*  iter: Count of ellapsed year, jday, month, day, hour, mins in the simulation.
        !*  start: Simulation start date.
        !*  now: Current date in the simulation.
        !*  stop: Simulation stop date.
        !*  next: Next date to be simulated.
        !*  run_start: Computer time at the beginning of the run.
        type(counter_date) iter, start, stop, now, next, run_start

        !> Time-step counts (no reset).
        !*  count_year: Number of simulation years run.
        !*  count_month: Number of simulation months run.
        !*  count_jday: Number of simulation days run.
        !*  count_hour: Number of simulation hours run.
        !*  count_mins: Number of simulation minutes run (half-hourly).
        !*  count_ts: Number of simulation time-steps run.
!-        integer :: count_year = 0, count_month = 0, count_jday = 0, count_hour = 0, count_mins = 0

        !> Time-step counts (reset).
        !*  ts_yearly: Count of time-steps elapsed in the current year. Resets at new year.
        !*  ts_monthly: Count of time-steps elapsed in current month. Resets at new month.
        !*  ts_daily: Count of time-steps elapsed in the current day. Resets daily.
        !*  ts_hourly: Count of time-steps elapsed in the current hour. Resets hourly.
        !*  ts_halfhourly: Count of time-steps elapsed to the current half-hour. Reset at the half-hour.
        !*  ts_count: Count of time-steps elapsed in the simulation. Does not reset.
        integer :: ts_yearly = 1, ts_monthly = 1, ts_daily = 1, ts_hourly = 1, ts_halfhourly = 1, ts_count = 1

    end type

    type(counter), save :: ic

    contains

    subroutine counter_init()

        !> Set the 'next' counter to 'now'.
        ic%next%year = ic%now%year
        ic%next%jday = ic%now%jday
        ic%next%month = ic%now%month
        ic%next%day = ic%now%day
        ic%next%hour = ic%now%hour
        ic%next%mins = ic%now%mins

        !> Increment the 'next' counter.
        call counter_increment(ic%next)

        !> Initialize 'iter' counter from 1.
        ic%iter%year = 1
        ic%iter%jday = 1
        ic%iter%month = 1
        ic%iter%day = 1
        ic%iter%hour = 1
        ic%iter%mins = 1

    end subroutine

    subroutine counter_increment(ic_date)

        !> Input/output variables.
        type(counter_date) ic_date

        !> Increment the current time-step and update the date (YEAR/JDAY/HOUR/MINS).
        ic_date%mins = ic_date%mins + ic%dtmins ! increment the current time by 30 minutes
        if (ic_date%mins == 60) then
            ic_date%mins = 0
            ic_date%hour = ic_date%hour + 1
            if (ic_date%hour == 24) then
                ic_date%hour = 0
                ic_date%jday = ic_date%jday + 1
                if (ic_date%jday >= 366) then
                    if (mod(ic_date%year, 400) == 0) then !LEAP YEAR
                        if (ic_date%jday == 367) then
                            ic_date%jday = 1
                            ic_date%year = ic_date%year + 1
                        end if
                    else if (mod(ic_date%year, 100) == 0) then !NOT A LEAP YEAR
                        ic_date%jday = 1
                        ic_date%year = ic_date%year + 1
                    else if (mod(ic_date%year, 4) == 0) then !LEAP YEAR
                        if (ic_date%jday == 367) then
                            ic_date%jday = 1
                            ic_date%year = ic_date%year + 1
                        end if
                    else !NOT A LEAP YEAR
                        ic_date%jday = 1
                        ic_date%year = ic_date%year + 1
                    end if
                end if
            end if
        end if

        !> Update remaining fields of the date (MONTH/DAY).
        call julian2monthday(ic_date%jday, ic_date%year, ic_date%month, ic_date%day)

    end subroutine

    subroutine counter_update()

        !> Local variables.
        integer old_year, old_jday, old_month, old_day, old_hour, old_mins

        !> Keep the old values for comparison.
        old_year = ic%now%year
        old_jday = ic%now%jday
        old_month = ic%now%month
        old_day = ic%now%day
        old_hour = ic%now%hour
        old_mins = ic%now%mins

        !> Increment the 'now' counter.
        call counter_increment(ic%now)

        !> Year.
        if (old_year /= ic%now%year) then
            ic%iter%year = ic%iter%year + 1
            ic%ts_yearly = 0
        end if

        !> Day in year; day in month.
        if (old_jday /= ic%now%jday) then
            ic%iter%jday = ic%iter%jday + 1
            ic%ts_daily = 0
            ic%iter%day = ic%iter%jday
        end if

        !> Month; day in month counter.
        if (old_month /= ic%now%month) then
            ic%iter%month = ic%iter%month + 1
            ic%ts_monthly = 0
        end if

        !> Hourly.
        if (old_hour /= ic%now%hour) then
            ic%iter%hour = ic%iter%hour + 1
            ic%ts_hourly = 0
        end if

        !> Minutes:
        if (old_mins /= ic%now%mins) then
            ic%iter%mins = ic%iter%mins + 1
            if (ic%now%mins == 0 .or. ic%now%mins == 30) then
                ic%ts_halfhourly = 0
            end if
        end if

!debug: Print the now.
!print *, "now: Y JD M D HR"
!print *, ic%now%year, ic%now%jday, ic%now%month, ic%now%day, ic%now%hour

!debug: Print count.
!print *, "count: Y JD M D HR"
!print *, ic%iter%year, ic%iter%jday, ic%iter%month, ic%iter%day, ic%iter%hour

        !> Update time-step counters.
        ic%ts_yearly = ic%ts_yearly + 1
        ic%ts_monthly = ic%ts_monthly + 1
        ic%ts_daily = ic%ts_daily + 1
        ic%ts_hourly = ic%ts_hourly + 1
        ic%ts_halfhourly = ic%ts_halfhourly + 1
        ic%ts_count = ic%ts_count + 1

        !> Set the 'next' counter to 'now'.
        ic%next%year = ic%now%year
        ic%next%jday = ic%now%jday
        ic%next%month = ic%now%month
        ic%next%day = ic%now%day
        ic%next%hour = ic%now%hour
        ic%next%mins = ic%now%mins

        !> Increment the 'next' counter.
        call counter_increment(ic%next)

    end subroutine

    !> *****************************************************************
    !> Description: Handled dates in the model
    !> *****************************************************************
    subroutine get_dates(ts)

        !> Input/Output
        type(dates_model) :: ts

        !> Internal variables
        integer nr_years
        integer, allocatable :: days_inyear(:)
        integer :: jday, year, i, iyear, fyear
        integer nr_days!, hours

        ts%freq = "D"
!todo: fix 'ts' or update it to deal with the scenario where the simulation start and/or stop dates are zeros
        ts%nyears = max(ic%stop%year - ic%start%year + 1, 1)
        ts%nseason = 12

        iyear = ic%start%year
        fyear = ic%stop%year
        year = ic%start%year

        allocate(days_inyear(ts%nyears))
        allocate(ts%years(ts%nyears))
        allocate(ts%daysINyears(ts%nyears))
        allocate(ts%daysINseasons(12))

        nr_days = 0
        do i = 1, ts%nyears
            ts%years(i) = year
            days_inyear(i) = leap_year(year)
            year = year + 1
            if (i == 1) then
                nr_days = days_inyear(i) - ic%start%jday + 1
            else if (i == ts%nyears) then
                nr_days = nr_days + (days_inyear(i) - (days_inyear(i) - ic%stop%jday))
            else
                nr_days = nr_days + days_inyear(i)
            end if
        end do

        ts%nr_days = nr_days
        allocate(ts%dates(nr_days, 4))

        ts%nr_timeStep = ts%nr_days*48

        year = ic%start%year
        jday = ic%start%jday
        do i = 1, nr_days
            ts%dates(i, 1) = year
            ts%dates(i, 4) = jday
            call Julian2MonthDay(jday, year, ts%dates(i, 2), ts%dates(i, 3))
            if ((leap_year(year) == 365) .and. (jday == 365)) then
                year = year + 1
                jday = 0
            else if ((leap_year(year) == 366) .and. (jday == 366)) then
                year = year + 1
                jday = 0
            end if
            jday = jday + 1
        end do

        call get_nr_months(ts)
        call get_nrDaysInUpFreq(ts)

    end subroutine !get_dates

    !> *****************************************************************
    !> Description: Get the number of days in months, seasons, years for
    !> the period of simulation
    !> *****************************************************************
    subroutine get_nrDaysInUpFreq(ts)

        !>Input
        type(dates_model) :: ts

        !> Internals
        integer i, i_year, i_month, i_season, j
        integer, dimension(:), allocatable :: days, days2

        allocate(days(ts%nr_days))

        !> Season
        days  = 0
        do i = 1, 12
            where(ts%dates(:, 2) == i) days = 1
            ts%daysINseasons(i) = sum(days)
            days = 0
        end do

        !> Year
        days = 0
        j = 1
        do i = ic%start%year, ic%stop%year
            where(ts%dates(:, 1) == i) days = 1
            ts%daysINyears(j) = sum(days)
            j = j + 1
            days = 0
        end do

        !> Months
        days = 0
        allocate(days2(ts%nr_days))
        days2 = 0
        do j = 1, ts%nmonths
            where(ts%dates(:, 1) == ts%mnthyears(j, 1)) days = 1
            where(ts%dates(:, 2) == ts%mnthyears(j, 2)) days2 = 1
            days = days*days2
            ts%daysINmonths(j) = sum(days)
            days = 0
            days2 = 0
        end do

        deallocate(days, days2)

    end subroutine !get_nrDaysInUpFreq

    !> *****************************************************************
    !> Description: Convert Julian day to month and day in gregorian
    !> calendar given the Julian day and the year
    !> *****************************************************************
    subroutine Julian2MonthDay(jday,year,month,day)

        !> Input
        integer, intent(in) :: jday, year

        !> Output
        integer, intent(out) :: month, day

        !> Internals
        integer, parameter, dimension(12) :: &
            daysnl = [31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365], &
            daysyl = [31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]

        integer i, int_i, int_f

        do i = 2, 12

            if (leap_year(year) == 365) then
                int_i = daysnl(i - 1)
                int_f = daysnl(i)
            elseif (leap_year(year) == 366) then
                int_i = daysyl(i - 1)
                int_f = daysyl(i)
            end if

            if (jday <= 31) then
                month = 1
                day = jday
                exit
            else
                if ((jday > int_i) .and. (jday <= int_f)) then
                    month = i
                    day = jday - int_i
                    exit
                elseif (jday == int_i) then
                    month = i - 1
                    if (leap_year(year) == 365) then
                        day = jday - daysnl(i - 1)
                        exit
                    elseif (leap_year(year) == 366) then
                        day = jday - daysyl(i - 1)
                        exit
                    end if
                end if
            end if
        end do

    end subroutine !Julian2MonthDay

    !> *****************************************************************
    !> Description: Get the number of days in leap and normal years
    !> (365 or 366).
    !> *****************************************************************
    integer function leap_year(y) result(ndays)

        logical is_leap
        integer, intent(in) :: y

        is_leap = (mod(y, 4) == 0 .and. .not. mod(y, 100) == 0) .or. (mod(y, 400) == 0)

        if (is_leap) then
            ndays = 366
        else
            ndays = 365
        end if

    end function !leap_year

    !> *****************************************************************
    !> Description: Get the number of months
    !> *****************************************************************
    subroutine get_nr_months(ts)

        !> Input/Output
        type(dates_model) :: ts

        !> Internal
        integer i, mth, n_months, yr

        n_months = 1

        do i = 1, ts%nr_days - 1
            if (ts%dates(i, 2) /= ts%dates(i + 1, 2)) then
                n_months = n_months + 1
            end if
        end do

        ts%nmonths = n_months
        allocate(ts%mnthyears(n_months, 2))

        mth = ts%dates(1, 2)
        yr = ts%dates(1, 1)

        do i = 1, ts%nmonths
            ts%mnthyears(i, 1) = yr
            ts%mnthyears(i, 2) = mth
            mth = mth + 1
            if (mth == 13) then
                mth = 1
                yr = yr + 1
            end if
        end do

        allocate(ts%daysInmonths(ts%nmonths))

    end subroutine !get_nr_months

    !> *****************************************************************
    !> Description: Get the year, month, and season indices from Julian
    !> day and year
    !> *****************************************************************
    subroutine GetIndicesDATES(iday,iyear,iy,im,iss, id,ts)

        !> Inputs
        type(dates_model) :: ts
        integer, intent(in) :: iday, iyear

        !> Outputs
        integer, intent(out) :: iy, im, iss, id

        !> Internals
        integer day, i

        iy = 0
        id = iday
        if (iyear == ic%start%year) id = iday - ic%start%jday + 1
        do i = ic%start%year, ic%stop%year
            iy = iy + 1
            if (i == iyear) then
                exit
            end if
            id = id + ts%daysinyears(i - ic%start%year + 1)
        end do

        call Julian2MonthDay(iday, iyear, iss, day)

        do i = 1, ts%nmonths
            if (ts%mnthyears(i, 1) == iyear .and. ts%mnthyears(i, 2) == iss) then
                im = i
                exit
            end if
        end do

    end subroutine !GetIndicesDATES

    !> Description: Determines day of year by month, day of month, and
    !>  year.
    integer function get_jday(month, day, year)

        !> Input variables.
        integer, intent(in) :: month, day, year

        !> Local variables.
        integer days(11)

        !> Assign an array checking if 'year' is a leap year.
        if (leap_year(year) == 366) then
            days = [31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]
        else
            days = [31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
        end if

        !> Solve and return the day of year
        if (month == 1) then
            get_jday = day
        else
            get_jday = days(month - 1) + day
        end if

        return

    end function

    !> Description:
    !>  Convert given year and day of year to Julian date.
    !>  Copied from former 'Julian_Day_ID' subroutine.
    !>
    !> Input variables:
    !>  year: Year.
    !>  jday: Day in year.
    !>
    !> Returns:
    !*  jdate: Julian date.
    integer function get_jdate(year, jday) result(jdate)

        !> Input variables.
        integer, intent(in) :: year, jday

        !> Calculate and return.
        jdate = (year - 1601)*365 + (year - 1601)/4 + jday

    end function

    !> Description:
    !>  Convert given Julian date, hour of day, and minutes in the
    !>  hour to a number of time-steps.
    !>  Based on code to skip records in climate forcing input files.
    !>
    !> Input variables:
    !*  jdate: Julian date (from year and day of year).
    !*  hour: Hour in day. Must run 00-23.
    !*  mins: Minutes in hour. Must be evenly divisible by 'dtmin'.
    !*  dtmins: Smallest increment of minutes.
    !>
    !> Returns:
    !*  tsteps: Number of time-steps.
    integer function jdate_to_tsteps(jdate, hour, mins, dtmins) result(tsteps)

        !> Input variables.
        integer, intent(in) :: jdate, hour, mins, dtmins

        !> Calculate and return.
        tsteps = (jdate*24*60 + hour*60 + mins)/dtmins

    end function

    !> Description:
    !>  Convert given date to a number of time-steps.
    !>  Based on code to skip records in climate forcing input files
    !>  and former 'Julian_Day_ID' subroutine.
    !>
    !> Input variables:
    !>  year: Year.
    !>  jday: Day in year.
    !*  hour: Hour in day. Must run 00-23.
    !*  mins: Minutes in hour. Must be evenly divisible by 'dtmin'.
    !*  dtmins: Smallest increment of minutes.
    !>
    !> Returns:
    !*  tsteps: Number of time-steps.
    integer function jday_to_tsteps(year, jday, hour, mins, dtmins) result(tsteps)

        !> Input variables.
        integer, intent(in) :: year, jday, hour, mins, dtmins

        !> Local variables.
        integer jdate

        !> Calculate and return.
        jdate = get_jdate(year, jday)
        tsteps = jdate_to_tsteps(jdate, hour, mins, dtmins)

    end function

end module
