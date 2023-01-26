!> Description:
!>  Subroutine to calculate zero oscillation depth given minimum and
!>  maximum temperatures and a temperature tolerance for a soil profile.
!>
!> Input variables:
!*  tmax: Maximum soil temperature of each layer (1: Grid or tile index; 2: Soil layer). [deg C or K].
!*  tmin: Minimum soil temperature of each layer (1: Grid or tile index; 2: Soil layer). [deg C or K].
!*  zbot: Bottom of soil layer to surface (1: Soil layer). [m below surface].
!*  ttol: Tolerance for temperature comparison. [deg C or K].
!*  nsl: Number of soil layers. [--].
!*  ilen: Number of elements in the inputs. [--].
!*  i1: First grid or tile index to process. [--].
!*  i2: Last grid of tile index to process. [--].
!>
!> Output variables:
!*  dzaa: Depth of zero annual amplitude where tmax ~= tmin. [m below surface].
subroutine permafrost_dzaa(tmax, tmin, zbot, ttol, dzaa, ilen, nsl, i1, i2)

    implicit none

    !> Input variables.
    integer nsl, ilen, i1, i2
    real tmax(ilen, nsl), tmin(ilen, nsl), zbot(nsl), ttol

    !> Output variables.
    real dzaa(ilen)

    !> Local variables.
    integer i, j
    real trng(ilen, nsl), zcen(nsl)

    !> Calculate depth of the middle of the layer, ZCEN.
    zcen(1) = zbot(1)/2.0
    do j = 2, nsl
        zcen(j) = (zbot(j) - zbot(j - 1))/2.0 + zbot(j - 1)
    end do

    !> Calculate DZAA, the depth where the range of maximum to minimum temperature is within 'TTOL'.
    do i = i1, i2

        !> Calculate the temperature envelope.
        trng(i, :) = tmax(i, :) - tmin(i, :)

        !> Set DZAA = 0.0 in case no DZAA is found.
        dzaa(i) = 0.0
        do j = 2, nsl

            !> DZAA is interpolated.
            if (sign(1.0, trng(i, j) - ttol) /= sign(1.0, trng(i, j - 1) - ttol)) then
                dzaa(i) = (zcen(j) - zcen(j - 1))/(trng(i, j - 1) - trng(i, j))*(trng(i, j - 1) - ttol) + zcen(j - 1)
                exit
            end if
        end do
    end do

end subroutine
