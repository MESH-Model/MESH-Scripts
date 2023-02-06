    !> Description:
    !>  Function to calculated Kling-Gupta efficiency (KGE) coefficient.
    !>
    !> After:
    !>  Gupta, H. V., Kling, H., Yilmaz, K. K., & Martinez, G. F.
    !>  (2009). Decomposition of the mean squared error and NSE
    !>  performance criteria: Implications for improving hydrological
    !>  modelling. Journal of Hydrology, 377(1-2), 80-91.
    !>
    !> Input variables:
    !*  obs: Series of observations, vector of size n.
    !*  sim: Series of simulations, vector of size n.
    !*  n: Number of observations/simulations in both series.
    !>
    !> Returns:
    !>  Kling-Gupta efficiency (KGE) coefficient. Only couplets of
    !>  observed and simulated values are considered where the observed
    !>  value is greater than zero.
    real function KGE(obs, sim, n)

        implicit none

        !> Parameters.
        !* sr/a/b: Scale factor for correlation, alpha, and beta components in the calculation of KGE.
        real :: sr = 1.0
        real :: sa = 1.0
        real :: sb = 1.0

        !> Input variables.
        integer, intent(in) :: n
        real, intent(in) :: obs(n), sim(n)

        !> Local variables.
        !* na: Number of active observed/simulated couplets (by obs > 0.0)
        integer na
        !* mo/s: Mean of observed/simulated.
        !* so/s: Standard deviation of observed/simulated.
        !* corr: Correlation coefficient of observed and simulated.
        real mo, ms, so, ss, corr

        !> Set the return variable.
        KGE = 0.0

        !> Calculate means (where obs > 0.0).
        na = count(obs > 0.0)
        if (na == 0) return
        mo = sum(obs, obs > 0.0)/na
        ms = sum(sim, obs > 0.0)/na
        if (mo == 0.0) return

        !> Standard deviations.
        so = sqrt(sum((obs - mo)**2, obs > 0.0)/(na - 1))
        ss = sqrt(sum((sim - ms)**2, obs > 0.0)/(na - 1))
        if (so == 0.0 .or. ss == 0.0) return

        !> Correlation.
        corr = (sum(obs*sim, obs > 0.0) - na*mo*ms)/((na - 1)*so*ss)

        !> Kling–Gupta efficiency (KGE).
        KGE = 1.0 - sqrt(sr*(corr - 1)**2 + sa*(ss/so - 1)**2 + sb*(ms/mo - 1)**2)

    end function
