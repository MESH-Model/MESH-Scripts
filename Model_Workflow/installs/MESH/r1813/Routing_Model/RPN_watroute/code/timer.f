C    This file is part of WATROUTE.
C
C    WATROUTE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    WATROUTE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.

      SUBROUTINE timer(iz,jz,mz,clock,time,t,thr,dtmin,
     *                dtmax,div,m,ju,a66)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!     rev. 9.2.43  Jun.  21/06  - NK: fixed spikes in route
!     rev. 9.4.05  May.  04/07  - NK: revised timer for julian day calc.

!***********************************************************************

!  DEBUG INFORMATION IS WRITTEN TO UNIT 55 - RTE.LST

!     REV. 9.00    Mar.  2000 - TS: CONVERTED TO FORTRAN 90 

!  t	        - time increment in seconds
!  thr		- time increment in hours
!  time		- time from the beginning of the event in hours
!  jz		- UTC hour
!  mz		- the integer value of the hour
!		  smallest time interval allowed will be a66 seconds (param file)
!  dtmin	- found in 'route' and is equal to the smallest travel
!		  through a square found in the computation during the time inc
!		  being considred.

!***********************************************************************

      use area_watflood
      
      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      CHARACTER(128) :: qstr
      INTEGER        :: result1,ntest,mohours(24),ju_mon(24),
     *                    m,iz,ik,jz,mz,ju,i,nchr
      REAL(4)        :: time,t1
      REAL*4         :: a66,t,div,thr,clock,dtmax,dtmin,tmv(3)

      DATA ntest/64161/qstr/'timer'/nchr/5/
      DATA mohours/744,672,744,720,744,720,744,744,720,744,720,744,
     *             744,672,744,720,744,720,744,744,720,744,720,744/

      ! csubich -- not properly handling leap years has caused problems
      ! with current runs, so this needs to be fixed here in timer.f.
      ! While a grand rewrite would be nice, the simplest thing to do
      ! would be to rewrite the array of start-days-of-months based on
      ! whether this is a leap year.
      if (mod(year1,4) .eq. 0) then ! Leap year
      !DATA ju_mon/1, 32, 61, 92,122,153,183,214,245,275,306,336,
      !*          367,398,426,457,487,518,548,579,610,640,671,701/
        ju_mon(1)  = 1
        ju_mon(2)  = 32
        ju_mon(3)  = 61
        ju_mon(4)  = 92
        ju_mon(5)  = 122
        ju_mon(6)  = 153
        ju_mon(7)  = 183
        ju_mon(8)  = 214
        ju_mon(9)  = 245
        ju_mon(10) = 275
        ju_mon(11) = 306
        ju_mon(12) = 336
        ju_mon(13) = 367
        ju_mon(14) = 398
        ju_mon(15) = 426
        ju_mon(16) = 457
        ju_mon(17) = 487
        ju_mon(18) = 518
        ju_mon(19) = 548
        ju_mon(20) = 579
        ju_mon(21) = 610
        ju_mon(22) = 640
        ju_mon(23) = 671
        ju_mon(24) = 701
      else 
      !DATA ju_mon/1, 32, 60, 91,121,152,182,213,244,274,305,335,
      !*          366,397,425,456,486,517,547,578,609,639,670,700/
        ju_mon(1)  = 1
        ju_mon(2)  = 32
        ju_mon(3)  = 60
        ju_mon(4)  = 91
        ju_mon(5)  = 121
        ju_mon(6)  = 152
        ju_mon(7)  = 182
        ju_mon(8)  = 213
        ju_mon(9)  = 244
        ju_mon(10) = 274
        ju_mon(11) = 305
        ju_mon(12) = 335
        ju_mon(13) = 366
        ju_mon(14) = 397
        ju_mon(15) = 425
        ju_mon(16) = 456
        ju_mon(17) = 486
        ju_mon(18) = 517
        ju_mon(19) = 547
        ju_mon(20) = 578
        ju_mon(21) = 609
        ju_mon(22) = 639
        ju_mon(23) = 670
        ju_mon(24) = 700
      end if
! these are the original values I don't know what they are trying to do?? Frank April 2002
c    *          366,397,435,456,486,517,565,578,280,639,672,700/

!     DURING PRECIP, EACH EVEN HOUR IS INCREMENTED 
!     AFTER PRECIP, EACH KT HOUR WILL BE INCORPORATED
!     M IS EQUAL TO 1 IN THE 'RESET' SUBROUTINE AT THE STRAT    
!     OF EACH NEW HYDROGRAPH
!     THE TIME INCREMENT IS ALLOWED TO DECREASE RSMCDLY BUT TO INCRE.
!     Y SLOWLY - STATEMENT 2 - TO PREVENT HYRDRAULIC INSTABILITY

!     a temporary fix for a66 getting clobbered
!      a66=900.0

      if(iopt.gt.1)then
         write(53,6006)
         write(53,6005)
     *         iz,jz,mz,itogo,t1,dtmin,dtmax,clock,time,t,thr
      endif

      if(m.le.1)then

!        THIS SECTION IS ONLY USED AT THE START OF EACH EVENT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     rev. 9.2.43  Jun.  21/06  - NK: fixed spikes in route
c         if(id.eq.1)t=3600.0
         t=3600.0
         ! csubich -- for non-midnight starts, we need to set time
         ! in medias res, as if watroute had been running for a
         ! while already.
         time=1.0*(1+hour1)
         itogo=1
         div=t/2.0
         thr=t/3600.
         iz=-1
         iz=0
         clock=0.0

c      time=t/3600.

         dtmax=3600.0
         if(snwflg.eq.'y')then
!           THIS IS A TEMPORARY FIX
!           TO MAKE SURE WE ARE DOING HOURLY COMPUTATIONS OF SNOW
            dtmin=3600.
         else
!           NO SNOW, SO NO RESTRICTIONS OTHER THAN STRFW TIME STEP
            dtmin=3600.0*kt
         endif

         do ik=1,3
            tmv(ik)=a66
         end do

         jz=int(time)
         mz=jz+1

c         ju=ju_mon(mo1)+jz/24

         ju=ju_mon(mo1)+day1-1    ! changed May4/07  nk

         ju=max(ju,1)

         month_now=mo1
         day_now=ju-ju_mon(mo1)+1
         hour_now=mod(jz,24)

!      print*,ju,mo,ju_mon(mo1),mo1,day_now,hour_now,'start'
!      pause

         if(iopt.ge.3)then
            write(53,6003)
            write(53,6005)
     *            iz,jz,mz,itogo,t1,dtmin,dtmax,clock,time,t,thr
         endif
 
        return
      
      else

!        MIN TRAVEL TIME IS DETERMINED IN ROUTE - SHOULD NOT BE .LT. a66

!        SO MAX TIME STEP IN SECONDS:
         dtmax=amax1(dtmin,a66)

!        ROUND THIS OFF SO THAT IT CAN BE DIVIDED INTO 3600:       
!         dtmax=float(int(dtmax)/int(a66))*a66
         dtmax=float(int(dtmax/a66))*a66

!        CURRENT TIME IS TIME & JZ IN HRS OR T IN SECONDS
!        CALCULATE THE TIME UNTIL THE NEXT RAINFALL
!        THE EXACT TIME UNTIL THE NEXT RAIN IS: 
         t1=float(jz+itogo-1)-time
         t1=max(1.0,t1)

!        THE NEXT TIME STEP IS MAX OF DTMAX AND TIME TO NEXT RAINFALL:
!        FOR SNOW MELT, THE MAX TIME STEP IS 1 HOUR
         t=min(t1*3600.0,dtmax,kt*3600.0)
         div=t/2.0
         thr=t/3600.
         iz=jz

!        WHEN IZ.NE.JZ A NEW RAIN WILL BE READ IN

         jz=int(time)
         mz=jz+1

!     rev. 9.4.05  May.  04/07  - NK: revised timer for julian day calc.

! csubich -- this code has issues for year roll-over, because the Julian
! day is reset but not the month.  This creates problems in the following
! block, leading to a wildly inappropriate month.  The simplest solution
! is to also reset the 'mo1' variable, as if we'd simply read-in an event
! file that begins with the new year.

! Additionally, the 'year1' variable needs properly reset, including a
! check for whether the now-current year is a leap year.  This has an
! impact on any very long-duration runs, where a single set of initial
! conditions is run for many months.
         if(mod(jz-1,24).eq.0)then
             ju=ju+1
!          reset the julian day on Jan 1 each year
             if(leapflg.eq.'0')then
               if(ju.gt.365) then
                  ju=1
                  mo1=1
                  year1=year1+1
                  if (mod(year1,4) .eq. 0) leapflg='1'
               endif
             else
               if(ju.gt.366) then
                  ju=1
                  mo1=1
                  year1=year1+1
                  if (mod(year1,4) .eq. 0) leapflg='1'
               endif
             endif
         endif

!        FIND OUT WHAT MONTH WE ARE IN:
           do i=12,1,-1
             mo=mo1+i-1
             if(ju.ge.ju_mon(mo))then
               go to 1000
             endif
           end do

 1000     if(mo.gt.12)mo=mo-12

          month_now=mo
          day_now=ju-ju_mon(mo)+1
          hour_now=mod(jz,24)

          if(hour_now.eq.0)then
              hour_now=24
          endif

!      print*,ju,mo,ju_mon(mo),mo,day_now,hour_now

!         now done in sub

      endif

      if(iopt.ge.3)then
!         if(mz-mz/12*12.eq.0)write(53,6003)
         if(jz-mz/12*12.eq.0)write(53,6003)
         write(53,6005)
     *         iz,jz,mz,itogo,t1,dtmin,dtmax,clock,time,t,thr
      endif

! FORMATS

 6003 format('   iz   jz   mz  itogo   t1    dtmin   dtmax   clock
     *    time    t       thr')
 6005 format(4i5,f8.2,2f8.0,2f8.2,f8.0,f8.2)
 6006 format(' into timer')

      RETURN
      END SUBROUTINE timer

