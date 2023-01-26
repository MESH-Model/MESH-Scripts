!> Algorithm for baseflow derived from Watflood manual.
!>  - "2.9 Base Flow" p. 2-14 in 'manual16.pdf' (2016 revision).
!>
!> Used by 'baseflow_module.f90'. 'dlz' is calculated from a minimum of
!> zero storage. 'baseflow_module.f90' enforces additional constraints
!> on the value. All vectors are dimensioned to 'n', and the function
!> implicitly iterates from 'i1:i2'. 'i1' and 'i2' are assumed to be in
!> the range from '1:n'.
!>
!> Parameters:
!*  - flz: Lower zone function. [--].
!*  - pwr: Exponent on the lower zone storage in the lower zone
!>         function. [--].
!>
!> Inputs:
!*  - lzs: Lower zone storage. [kg m-2 s-1].
!>
!> Outputs:
!*  - dlz: Release from lower zone storage. [kg m-2 s-1].
!>
subroutine baseflow_wfqlz(flz, pwr, lzs, dlz, n, i1, i2)

    integer, intent(in) :: n, i1, i2
    real, intent(in) :: flz(n), pwr(n), lzs(n)
    real, intent(out) :: dlz(n)

    dlz(i1:i2) = flz(i1:i2)*max(lzs(i1:i2), 0.0)**pwr(i1:i2)

end subroutine
