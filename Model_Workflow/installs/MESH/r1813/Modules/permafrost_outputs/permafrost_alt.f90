!> Description:
!>  Subroutine to calculate active layer depth given temperatures for a
!>  soil profile.
!>
!> Input variables:
!*  tbar: Soil temperature (1: Grid or tile index; 2: Soil layer). [K].
!*  zbot: Bottom of soil layer to surface (1: Soil layer). [m below surface].
!*  nsl: Number of soil layers. [--].
!*  ilen: Number of elements in the inputs. [--].
!*  i1: First grid or tile index to process. [--].
!*  i2: Last grid of tile index to process. [--].
!>
!> Output variables:
!*  alt: Active layer thickness. [m below surface].
subroutine permafrost_alt(tbar, zbot, alt, ilen, nsl, i1, i2)

    implicit none

    !> Input variables.
    integer nsl, ilen, i1, i2
    real tbar(ilen, nsl), zbot(nsl)

    !> Output variables.
    real alt(ilen)

    !> Local variables.
    real, parameter :: TFREZ = 273.16
    integer i, j
    real tbarc(ilen, nsl), zcen(nsl)

    !> Calculate depth of the middle of the layer, ZCEN.
    zcen(1) = zbot(1)/2.0
    do j = 2, nsl
        zcen(j) = (zbot(j) - zbot(j - 1))/2.0 + zbot(j - 1)
    end do

    !> Convert temperature to degrees C.
    tbarc = tbar - TFREZ

    !> Calculate ALT, where temperature in the soil transitions from above to below freezing.
    do i = i1, i2

        !> Set ALT = 0.0 in case no ALT is found.
        alt(i) = 0.0
        do j = 2, nsl
            if (tbarc(i, j - 1) < 0.0) then

                !> Ignore and cycle if the layer above is frozen.
                alt(i) = 0.0
                exit
            else if (sign(1.0, tbarc(i, j)) < sign(1.0, tbarc(i, j - 1))) then

                !> Transition occurs inside the layer; ALT is interpolated.
                alt(i) = (zcen(j) - zcen(j - 1))/(tbarc(i, j - 1) - tbarc(i, j))*(tbarc(i, j - 1)) + zcen(j - 1)
                exit
            end if
        end do
    end do

end subroutine
