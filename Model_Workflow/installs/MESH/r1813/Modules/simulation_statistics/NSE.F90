module simstats_nse

implicit none

private
public nse_calc

!todo add flag for nmin; was originally the purpose of autocalibrationflag, but that flag has been re-purposed

contains

!> The function computes the Nash-Sutcliffe efficiency index.
!>
!> December 18, 2013 - K.C. Kornelsen
!>
function nse_calc(qobs, qsim, n, ns, nmin)

! FUNCTION I/O
!* n: Number of days of observed/simulated values
!* ns: Number of streamflow gauges
!* nmin: Minimum number of days required for spin-up before calculating the statistic
!* calc_nse: Nash-Sutcliffe efficiency (NSE) index (returned by the function)
!* qobs: Observed values (1: daily flow value; 2: streamflow gauge)
!* qsim: Simulated values (1: daily flow value; 2: streamflow gauge)
integer :: n, ns, nmin
real :: nse_calc
real, intent(in), dimension(:, :) :: qobs, qsim

! LOCAL VARIABLES
!* j: Counter (streamflow gauge)
!* i: Counter (day)
!* qbar: Average of the observed streamflow values (1: daily flow value)
!* num: Numerator of the NSE summation (1: daily flow value)
!* denom: Denominator of the NSE summation (1: daily flow value)
!* wgt: Weight to calculate the average NSE for all streamflow gauges
integer :: i, j
real, dimension(ns) :: qbar, num, denom
real :: wgt

nse_calc = 0.0

! Mean flow
qbar = 0.0

if (n > nmin) then

    ! Get Sum
    do j = 1, ns
        do i = nmin, n
            qbar(j) = qbar(j) + qobs(i, j)
        end do
    end do

    ! Get Average
    do j = 1, ns
        qbar(j) = qbar(j) / (n - nmin)
    end do
    num = 0.0
    denom = 0.0
    do j = 1, ns
        do i = nmin, n
            num(j) = num(j) + (qobs(i, j) - qsim(i, j))**2.0
            denom(j) = denom(j) + (qobs(i, j) - qbar(j))**2.0
        end do
    end do
    wgt = 1.0 / ns ! Weight based on NS
    do j = 1, ns
        nse_calc = nse_calc + (wgt*(1.0 - (num(j)/denom(j))))
    end do
end if

end function

end module
