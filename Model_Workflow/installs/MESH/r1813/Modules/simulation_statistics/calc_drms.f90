!>
!> Module for calculating Daily Root Mean Squared Error (DRMS)
!>
!> Contains the model_output_drms variable type which stores
!> the value at each streamflow gauge, as well as an average value
!> representative of all gauges.
!>
!> Contains the calc_drms_value subroutine to calculate DRMS
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
module calc_drms

    implicit none

    private

    !> The model_output_drms type and calc_drms_value function are accessible outside the module.
    public :: model_output_drms, calc_drms_value

    !> type: model_output_drms
    !* value_gauge: DRMS for the streamflow gauge (1: streamflow gauge)
    !* value_gauge_avg: Average DRMS of all gauges
    type model_output_drms
        real, dimension(:), pointer :: value_gauge
        real :: value_gauge_avg
    end type model_output_drms

    !>
    !> Begin module.
    !>

    contains

    type(model_output_drms) function calc_drms_value(ncal_day_min, ncal_day, qobs, qsim)

        !> Variable declarations
        !* ncal_day_min: Minimum number of days for spin-up
        !* ncal_Day: Number of days of daily streamflow values
        !* qobs: Observed values (1: daily flow value; 2: streamflow gauge)
        !* qsim: Simulated values (1: daily flow value; 2: streamflow gauge)
        integer, intent(in) :: ncal_day_min, ncal_day
        real, dimension(:, :), intent(in) :: qobs, qsim

        !> Local variables
        !* j: Counter (streamflow gauge)
        integer :: j

        !> Allocate the function variable.
        allocate(calc_drms_value%value_gauge(size(qsim, 2)))
        calc_drms_value%value_gauge = 0.0
        calc_drms_value%value_gauge_avg = 0.0

        !> Only calculate if the current day is greater than the spin-up period.
        !todo: This is only useful in the case of pre-emption.
        if (ncal_day > ncal_day_min) then

            !> Calculate the per gauge value.
            do j = 1, size(qsim, 2)
                calc_drms_value%value_gauge(j) = &
                    sqrt(sum((qobs(ncal_day_min:ncal_day, j) - qsim(ncal_day_min:ncal_day, j))**2)/ncal_day)
            end do !j = 1, size(qsim, 2)

            !> Calculate the average value.
            calc_drms_value%value_gauge_avg = sum(calc_drms_value%value_gauge)/size(calc_drms_value%value_gauge)

        end if !(ncal_day > ncal_day_min)

    end function calc_drms_value

end module calc_drms
