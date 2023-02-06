MODULE runsvs_utils
!
! Namelist parameters
!-NAMELIST /RUNSVS_OPT/   nt,dateo,houro,dt,\
!-                        sigma_u,sigma_t,\
!-                        observed_forcing,\
!-                        inifile,interpfile,geofile,\
!-                        metfile,outfile,rtefile,\
!-			xcount,ycount
!-INTEGER nt      ! Number of time steps
!-INTEGER dateo   ! Starting date of the run
!-INTEGER houro   ! Starting hour of the run
REAL dt         ! Time step duration (sec)
REAL sigma_u    ! Sigma level of momentum forcing
REAL sigma_t    ! Sigma level of scalar forcings
!-INTEGER xcount  ! Number of columns in the grid
!-INTEGER ycount  ! Number of lines in the grid
LOGICAL observed_forcing
!-CHARACTER (LEN=255) inifile
!-CHARACTER (LEN=255) interpfile
!-CHARACTER (LEN=255) geofile
!-CHARACTER (LEN=255) metfile
!-CHARACTER (LEN=255) outfile
!-CHARACTER (LEN=255) rtefile
!
CONTAINS
SUBROUTINE surflayerheight(sigma_u,sigma_t,bus,bussiz)
!
USE runsvs_mod
IMPLICIT NONE
#include "thermoconsts.inc"
!
INTEGER bussiz
REAL bus(bussiz)
REAL sigma_u,sigma_t
!
INTEGER i
!
! Recompute height of surface layer and potential temperature
DO i=0,NG-1
	bus(zusl+i) = -rgasd/grav*log(sigma_u)*bus(tmoins+i)
	bus(ztsl+i) = -rgasd/grav*log(sigma_t)*bus(tmoins+i)
END DO
RETURN
END SUBROUTINE
SUBROUTINE compvirttemp(sigma_t,bus,bussiz)
!
USE runsvs_mod
IMPLICIT NONE
#include "thermoconsts.inc"
!
INTEGER bussiz
REAL bus(bussiz)
REAL sigma_t, sc
!
INTEGER i
!
sc = sigma_t**(-cappa)
DO i=0,NG-1
	bus(thetaa+i)=sc*bus(tmoins+i)
END DO
RETURN
END SUBROUTINE
END MODULE runsvs_utils
