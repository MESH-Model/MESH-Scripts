module simstats_saesrt

    implicit none

    private
    public saesrt_calc

    contains

    !> The function computes the sum of absolute value of errors after sorting.
    !>
    !> June 17, 2010 - M.A. Mekonnen
    !>
    function saesrt_calc(qobs, qsim, n, ns, nmin)

        !> FUNCTION I/O
        !> n: Number of days of observed/simulated values
        !> ns: Number of streamflow gauges
        !> nmin: Minimum number of days required for spin-up before calculating the statistic
        !> calc_saesrt: Sum of absolute error after sorting (returned by the function)
        !> qobs: Observed values (1: daily flow value; 2: streamflow gauge)
        !> qsim: Simulated values (1: daily flow value; 2: streamflow gauge)
        integer :: n, ns, nmin
        real :: saesrt_calc
        real, intent(in), dimension(:, :) :: qobs, qsim

        !> LOCAL VARIABLES
        !> j: Counter (streamflow gauge)
        !> i: Counter (day)
        !> nn: Days of observation after spin-up
        !> ii: Counter (adjusted day, after spin-up)
        !> qo: Observed values after sorting (1: daily flow value)
        !> qs: Simulated values after sorting (1: daily flow value)
        integer :: j, i, nn, ii
        real, dimension(:), allocatable :: qo, qs

        saesrt_calc = 0.0
        nn = n - nmin + 1

        allocate(qo(nn), qs(nn))

        if (nn >= 1) then
            do j = 1, ns
                qo = 0.0
                qs = 0.0
                do i = nmin, n
                    if (qobs(i, j) >= 0.0) then !> Exclude missing streamflow data
                        ii = i - nmin + 1
                        qo(ii) = qobs(i, j)
                        qs(ii) = qsim(i, j)
                    end if
                end do

                !> Sort observed streamflow
                call sort(qo, nn)

                !> Sort simulated streamflow
                call sort(qs, nn)

                !> Compute the sum of errors after ranking
                saesrt_calc = saesrt_calc + sum(abs(qo - qs))
            end do
        end if

    end function

end module
