!
! File:   baseFlow_luo2012.F90
! Author: Luis Morales-Marin, GIWS
!
! Created on June 29, 2015, 11:11 AM
!

! Recharge entering the aquifer (mm/hr)
! Input data:
!  Wseep: Total amount of water exiting the bottom of the soil profile (mm/hr)
!  dgw: Delay time of the overlying soil layers (hours).
!  Wrchrg_old: amount of recharge entering the aquifer the day before (mm/hr)
REAL FUNCTION Wrchrg_new(Wseep, dgw, Wrchrg_old)
    IMPLICIT NONE
    
    REAL, INTENT(IN) :: Wseep, dgw, Wrchrg_old
    REAL :: expFac
    
    expFac = EXP(-1/dgw)
    Wrchrg_new = (1 - expFac) * Wseep + expFac * Wrchrg_old  
    
END FUNCTION Wrchrg_new

! Base flow generated from the aquifer (mm/hr)
! Input data:
!  agw: Recession constant
!  dt: Time step.
!  Wrchrg_new: Recharge entering the aquifer (mm/hr)
!  Qb_old: Base flow generated from the aquifer a day before (mm/hr)
REAl FUNCTION Qb_new(agw, dt, Wrchrg_new, Qb_old)
    IMPLICIT NONE
    
    REAL, INTENT(IN) :: agw, dt, Wrchrg_new, Qb_old
    REAL :: expFac
    
    expFac = EXP(-agw * dt)
    Qb_new  = Qb_old * expFac + Wrchrg_new * (1-expFac)
    !WRITE(*,*) Wrchrg_new, Qb_old, expFac, Qb_new, agw, dt
    
END FUNCTION Qb_new

! Estimate the base flow released by the  aquifer
! Input data:
!  Wseep: Total amount of water exiting the bottom of the soil profile (mm/hr).
!  dgw: Delay time of the overlying soil layers (hours).
!  Wrchrg_old: Amount of recharge that entered the  aquifer the day before (mm/hr).
!  agw: Recession constant of the  aquifer.
!  Qb_old: Base flow generated  a day before (mm/hr)
!  dt: MESH time step.
!  Wrchrg: Amount of recharge entering the  aquifer (mm/hr).
!  Qb: Total base flow (mm/hr)

SUBROUTINE baseFlow_luo2012(Wseep, dgw, Wrchrg_old,  agw, Qb_old, dt, Wrchrg, Qb)
    IMPLICIT NONE
    
    REAL, INTENT(IN) 	:: Wseep, dgw, Wrchrg_old,  agw, Qb_old, dt
    REAL, INTENT(OUT) 	:: Wrchrg, Qb
    REAL 		::  Wrchrg_new, Qb_new
    
    ! Aquifer echarge
    Wrchrg = Wrchrg_new(Wseep, dgw, Wrchrg_old)
    
    ! Base flow (mm/hr)
    Qb = Qb_new(agw, dt, Wrchrg, Qb_old)
    
END SUBROUTINE baseFlow_luo2012
