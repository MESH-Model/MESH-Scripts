!>
!> Module for calculating Mean Absolute Error (ABSERR)
!>
!> Contains the model_output_abserr variable type which stores
!> the value at each streamflow gauge, as well as an average value
!> representative of all gauges.
!>
!> Contains the calc_abserr_value subroutine to calculate ABSERR
!> given observed and simulated daily streamflow.
!>
!> References:
!>      Gupta et al. (1998) -   Toward improved calibration of
!>                              hydrologic models, Water Resources
!>                              Research 34: 751-763
!>
!> Updates:
!>
!> D. G. Princz - 2014-11-19.
!>
module calc_abserr

    implicit none

    private

    !> The model_output_abserr type and calc_abserr_value function are accessible outside the module.
    public :: model_output_abserr, calc_abserr_value

    !> Type: model_output_abserr
    !* value_gauge: ABSERR for the streamflow gauge (1: streamflow gauge)
    !* value_gauge_avg: Average ABSERR of all gauges
    type model_output_abserr
        real, dimension(:), allocatable :: value_gauge
        real :: value_gauge_avg
    end type

    !>
    !> Begin module.
    !>

    contains

    type(model_output_abserr) function calc_abserr_Value(ncal_day_min, ncal_day, qobs, qsim)

        !> Variable declarations
        !* ncal_day_min: Minimum number of days for spin-up
        !* ncal_day: Number of days of daily streamflow values
        !* qobs: Observed values (1: daily flow value; 2: streamflow gauge)
        !* qsim: Simulated values (1: daily flow value; 2: streamflow gauge)
        integer, intent(in) :: ncal_day_min, ncal_day
        real, dimension(:, :), intent(in) :: qobs, qsim

        !> Local variables
        !* j: Counter (streamflow gauge)
        integer :: j

        !> Allocate the function variable.
        allocate(calc_abserr_value%value_gauge(size(qsim, 2)))
        calc_abserr_value%value_gauge = 0.0
        calc_abserr_value%value_gauge_avg = 0.0

        !> Only calculate if the current day is greater than the spin-up period.
        if (ncal_day > ncal_day_min) then

            !> Calculate the per gauge value.
            do j = 1, size(qsim, 2)
                calc_abserr_value%value_gauge(j) = &
                    sum(abs(qobs(ncal_day_min:ncal_day, j) - qsim(ncal_day_min:ncal_day, j)))/ncal_day
            end do

            !> Calculate the average value.
            calc_abserr_value%value_gauge_avg = &
                sum(calc_abserr_value%value_gauge)/size(calc_abserr_value%value_gauge)

        end if !(ncal_day > ncal_day_min)

    end function

end module calc_abserr
