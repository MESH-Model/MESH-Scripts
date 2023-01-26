module simstats_sae

    implicit none

    private
    public sae_calc

    contains

    !> The function computes the sum of the absolute value of errors.
    !>
    !> June 17, 2010 - M.A. Mekonnen
    !>
    function sae_calc(qobs, qsim, n, ns, nmin)

        !> FUNCTION I/O
        !* n: Number of days of observed/simulated values
        !* ns: Number of streamflow gauges
        !* nmin: Minimum number of days required for spin-up before calculating the statistic
        !* calc_sae: Sum of absolute error (returned by the function)
        !* qobs: Observed values (1: daily flow value; 2: streamflow gauge)
        !* qsim: Simulated values (1: daily flow value; 2: streamflow gauge)
        integer :: n, ns, nmin
        real :: sae_calc
        real, intent(in), dimension(:, :) :: qobs, qsim

        !> LOCAL VARIABLES
        !* j: Counter (streamflow gauge)
        !* i: Counter (day)
        integer :: j, i

        sae_calc = 0.0
        if (n > nmin) then
            do j = 1, ns
                do i = nmin, n
                    if (qobs(i, j) >= 0.0) &
                        sae_calc = sae_calc + abs(qobs(i, j) - qsim(i, j))
                end do
            end do
        end if

    end function

end module
