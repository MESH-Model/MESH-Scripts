SUBROUTINE runsvs_init(bus,bussiz)
!
USE runsvs_mod
IMPLICIT NONE
#include "thermoconsts.inc"
!
INTEGER bussiz
REAL bus(bussiz)
!
INTEGER i,j
!
! Initialize some parameters and state variables to default values
DO i=0,NG-1
!
!	Initialize some state variables that cannot be simply set to zero
!
!	Snow density (needs to be initialized even if there is no snow!)
!	bus(snoro+i)=0.1
!	bus(snvro+i)=0.1
!	Land, snow and vegetation temperature
!	DO j=1,2
!		bus(tground+(j-1)*NG+i)=bus(tmoins+i)
!		bus(tsoil+(j-1)*NG+i)=bus(tmoins+i)
!		bus(tvege+(j-1)*NG+i)=bus(tmoins+i)
!		bus(tsnowveg+(j-1)*NG+i)=bus(tmoins+i)
!               bus(tsnow+(j-1)*NG+i)=tcdk
!	END DO
!
!	Soil moisture is set to field capacity
!
!	DO j=1,7
!        	bus(wdsoil+(j-1)*NG+i)=bus(wfc+i)
!	END DO
!
!	Initialize some parameters that are not found in INI file
!
!	Aerodynamic resistance initialization
	bus(resagr+i)=100.
        bus(resavg+i)=50.
	bus(resasa+i)=100.
	bus(resasv+i)=100.

!       Coriolis factor
        bus(fcor+i)=1.45441e-4*sin(bus(dlat+i))
END DO
RETURN
END
