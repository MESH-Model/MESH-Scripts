C    This file is part of FLOWINIT.
C
C    FLOWINIT is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    FLOWINIT is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with FLOWINIT.  If not, see <http://www.gnu.org/licenses/>.

      SUBROUTINE flowinit()


!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!***********************************************************************
!
!     REV. 7.72 - feb. 04/96 -  took flowinit.for from sub.for
!     REV. 8.25 - May. 22/97 -  fixed allocating the basin # in flowinit
!     REV. 8.73 - Mar.  1/98 -  changed mhrd to mhtot in flowinit
!     REV. 8.77 - June  1/98 -  added sub-basin error calculation
!
!     REV. 9.00    Mar.  2000 - TS: CONVERTED TO FORTRAN 90
!     REV. 9.03    Nov.  2000 - TS: ADDED WATFLOOD SWAMP ROUTING
!     rev  9.1.03  July  24/01  - added polinomial to reservoir routing
!     rev. 9.1.18  Jun.  03/02  - Added sub-watershed modelling capability
!     rev. 9.1.36  Jan.  28/03  - Fixed wetland init condition in flowinit
!     rev. 9.1.59  Jul.  15/04  - NK: split rerout into two parts: rdresv & rerout
!     rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
!     rev. 9.2.15  Sep.  30/05  - NK: Fixed bug for opt in flowinit
!     rev. 9.2.17  Oct.  11/05  - NK: Fixed bug for .str bounds in route
!     rev. 9.2.18  Oct.  27/05  - NK: Fixed bug in flowinit (init spike)
!     rev. 9.2.32  Feb.  10/06  - NK: Added area_check.csv to output
!     rev. 9.3.05  Nov.  13/06  - NK: adder write_flowinit.for to flowinit.for
!     rev. 9.3.10  Jan.  29/07  - NK: routing pars changed to gridded values
!     rev. 9.4.09  Jun.  19/07  - NK: added lake_area as a variable for iso
!     rev. 9.4.10  Jun.  19/07  - NK: adjusted frac for channel water area
!     rev. 9.4.13  Jul.  09/07  - NK: modified lzs to account for lake area (flowinit)
!     rev.         May.  2018  - dgp: Modified to allow a gauge at the outlet
!                                   where 'nnx' is zero,
!                                   though resulting flow will be zero if its 'da' is zero.
!                                   Added a check for 'da' /= zero for the case
!                                   when 'nnx' is the outlet (where 'qda' is calculated).
!
!     changes made to include c&g model stuff  nk  April. 26/07
!
!***********************************************************************

      USE area_watflood
      implicit none

c        USE area1
c        USE area2
c        USE area3
c        USE area4
c        USE area5
c        USE area6
c        USE area8
c        USE area9
c        USE area10
c        USE area12
c        USE area16
c!!!!!!        USE areased   no longer needed. removed Mar. 21/04 nk (bug)
c        use areawfo
cc        USE areawq
c        USE areawet
cc        use areamelt


!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE


      real*4,     dimension(:),    allocatable :: qinit,datemp
      real*4,     dimension(:,:),  allocatable :: qdagrd
      integer,  dimension(:),    allocatable :: iset
!mesh_io      character(12), dimension(:), allocatable :: sta_id
!mesh_io      character(20), dimension(:), allocatable :: sta_name
!mesh_io      real*4,        dimension(:), allocatable :: sta_lat
!mesh_io      real*4,        dimension(:), allocatable :: sta_long
!mesh_io      real*4,        dimension(:), allocatable :: sta_area
!mesh_io      logical    :: exists
      real*4     :: da1,qda1,qch,obdepth,tdum,trialq,flow_max,convert,
     *              class_sum
      integer    :: n,i,j,istep2,l,ktt,k,ktemp,jj,
     *              iasdf,ios,noread,nnx,inx,jnx,ii,
     *              iallocate
      CHARACTER(30) :: msg
      CHARACTER(10) :: time
      CHARACTER(8)  :: cday


!        CHARACTER(12) :: resname(25)  !to area 5
      CHARACTER(1)  :: msg1,errflag !,smok
      LOGICAL       :: resflag,flowflag
!        INTEGER      :: rbin



      if(nnn.le.0)then
!       pass by when optimizing after first iteration
!       NK - ALLOCATIONS    Jun. 03/02
!mesh_io         allocate(sta_id(no),sta_name(no),sta_lat(no),sta_long(no),
!mesh_io     *            sta_area(no),stat=iAllocate)
!mesh_io         if(iAllocate.ne.0) STOP
!mesh_io     *        'Error with allocation  in flowinit @ 73'

         allocate(qinit(na),datemp(na),qdagrd(imax,jmax),
     *        iset(na),stat=iAllocate)
         if(iAllocate.ne.0) STOP
     *        'Error with allocation  in flowinit @ 78'
      endif
! * * * * * * * * * * *
      if(iopt.eq.2)print*,'In flowinit @ 87'

!     wetland geometry initialization (moved from spl May 25/05)

!     this section moved from rdshed   28/12/04  nk
!     S(I,J) IS AN ARRAY OF ORDER NUMBER FOR THE WATERSHED IN
!     PROGRAM SIMPLE

!rte_module      if(.not.allocated(nclass))then
!rte_module         allocate(nclass(ntype+1),stat=iAllocate)
!rte_module         if(iAllocate.ne.0) STOP
!rte_module     *        'Error with allocation of nclass array in flowinit'
!rte_module      endif

!     rev. 9.4.09  Jun.  19/07  - NK: added lake_area as a variable for iso
!rte_module      allocate(lake_area(noresv),stat=iAllocate)
!rte_module      if(iAllocate.ne.0) STOP
!rte_module     *     'Error with allocation of lake_area vector in flowinit'
!rte_module      do l=1,noresv
!rte_module         lake_area(l)=0.0
!rte_module      end do

!     what class is the water class?
!rte_module      do ii=1,ntype+1
!rte_module         if(nclass(ii).eq.'water     ') ii_water=ii
!rte_module      end do

!mesh_io      write(53,*)'In flowinit.for'
!mesh_io      write(53,*)'~~~~~~~~~~~~~~~'

      convert=al*al*1000.0_4      ! converts vol in m^3 to mm on the unit grid
      flow_max=0.0


!     Determine how many reaches have been specified
!     usually = no of lakes but can be more.
      maxr=0
      do n=1,naa
         if(ireach(n).gt.0)then
            maxr=max(maxr,ireach(n))
         endif
      end do
!     print*,'no of reaches found       =',maxr
!mesh_io      if(maxr.lt.noresv)then
!mesh_io         print*,'Reach numbers for one or more reservoirs are'
!mesh_io         print*,'not specified in the bsnm_shd.r2c file'
!mesh_io         print*,'please assign reach numbers to grids in reservoirs'
!mesh_io         print*,'and lakes in the map file & rerun bsn.exe'
!mesh_io         stop 'Program aborted in flowinit @ 141'
!mesh_io      endif

!mesh_io      write(53,*)
!mesh_io     * '    grid no       reach #      ireach       nreach       maxr'
      do n=1,naa
!       CONVERSIONS &
!       STORE THE GRIDS CONTRIBUTING TO A DWOPER REACH
         if(ireach(n).ne.0)then
            l=ireach(n)
            nreach(l)=n
!mesh_io            write(53,*)n,l,ireach(n),nreach(l),maxr
         endif
      end do

!     calculate the lake areas:
c      write(53,*)
c     * '    grid no  frac water     grid area       lake_area'

!rte_module      do n=1,naa
!       CONVERSIONS &
!       STORE THE GRIDS CONTRIBUTING TO A DWOPER REACH
!rte_module         if(ireach(n).ne.0)then
!rte_module            l=ireach(n)
c            lake_area(l)=lake_area(l)+frac(ii_water)*grid_area(n)
!rte_module            lake_area(l)=lake_area(l)+frac(ntype)*grid_area(n)
c            write(53,*)n,frac(ii_water),grid_area(n),lake_area(l)
!rte_module         endif
!rte_module      end do

!rte_module      write(53,*)
!rte_module     * '    lake no  lake_area'
!rte_module      do l=1,noresv
!rte_module         write(53,*)l,lake_area(l)
!rte_module      end do

!mesh_io      write(53,*)'    grid no  ch width     rch lngth    chaarea'
      do n=1,naa
!     rev. 9.2.15  Sep.  30/05  - NK: Fixed bug for opt in flowinit
!       removed slope(n)=..... and elev(n)=.....  already done in rdshed
!        rl(n)=al*mndr(ii)    !  now read in<<<<<<<<<<<<<<<<<,
!     rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
c         sl2(n)=sqrt(sl1(n))  !moved to read_shed_ef  Jun. 5/07 nk
!rte_module         if(a4.eq.0)a4=1.0
! * * * TS * * *
!       CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLO
!        widep=a11
!rte_module         if(aa4(ii).gt.0.0)then
c          chaxa(n)=(aa2(ii)+aa3(ii)*da(n)**aa4(ii))
!rte_module            chaxa(n)=(aa2(n)+aa3(n)*da(n)**aa4(n))
!rte_module         else
!         rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
!         EXCEL compatible equation. aa4 must be -ve in the par file
c          chaxa(n)=10.0**(aa2(ii)*alog10(da(n))+aa3(ii))
!rte_module            chaxa(n)=10.0**(aa2(n)*alog10(da(n))+aa3(n))
!         had to put a lower bound on channel xsec area to avoid NaN in resume file
!         NK  Oct. 5/05
!rte_module            chaxa(n)=amax1(1.0,chaxa(n))
!rte_module         endif
!rte_module         cap(n)=chaxa(n)*rl(n)
c        chadep(n)=SQRT(chaxa(n)/widep(ii))
c        print*,n,chaxa(n),widep(n)
!rte_module         chadep(n)=SQRT(chaxa(n)/widep(n))
!rte_module         chawid(n)=chaxa(n)/chadep(n)

!     rev. 9.4.10  Jun.  19/07  - NK: adjusted frac for channel water area
!       adjust class areas for channel area
c        channel_area(n)=chawid(n)*rl(n)
!rte_module         chaarea(n)=chawid(n)*rl(n)
!rte_module         if(aclass(n,ntype).lt.chaarea(n)/grid_area(n))then
!         water area in the shd file is less than the area calculated
!         from the channel parameters and the reach length
!         this will make the sum of the class fractions > 0.0
!rte_module            aclass(n,ntype)=chaarea(n)/grid_area(n)
!rte_module         elseif(ireach(n).eq.0)then
!         reservoirs have ireach > 0
!         grids with reservoirs are not touched
!         the water area is set by the shd file input
!rte_module            chaarea(n)=aclass(n,ntype)*grid_area(n)
!rte_module         endif
!rte_module         if(ireach(n).gt.0)chaarea(n)=0.0
!mesh_io         write(53,*)n,chawid(n),rl(n),chaarea(n)
!rte_module         class_sum=0.0
!rte_module         do ii=1,ntype+1
!rte_module            if(ii.ne.ntype)class_sum=class_sum+aclass(n,ii)
!rte_module         end do
!       adjust the classes so relative amounts stay equal
!       water class is excluded because its additional area
!       is taken away from the other classes
!rte_module         do ii=1,ntype+1
!rte_module            if(ii.ne.ntype)then
!rte_module               if(abs(1.0-class_sum-aclass(n,ntype)).gt.0.000001)then
!rte_module                  aclass(n,ii)=aclass(n,ii)/(class_sum+aclass(n,ntype))
!rte_module               endif
!rte_module            endif
!rte_module         end do
!rte_module         class_sum=0.0
!rte_module         do ii=1,ntype+1
!rte_module            class_sum=class_sum+aclass(n,ii)
!rte_module         end do
c        write(699,66999)(aclass(n,ii),ii=1,ntype+1),class_sum
c66999   format(10f8.5)


!     rev. 9.4.10
! TS: moved from in 'wetflg.eq.y' statement since required w/o wetlands too, Jul 26/06.
         if(wetflg.eq.'y')then
*         theta=a9
            wetwid(n)=(al*rl(n))*frac(n)*aclass(n,ntype-1)/rl(n)
c          wcap(n)=rl(n)*wetwid(n)*chadep(n)*abs(theta(ii))
            wcap(n)=rl(n)*wetwid(n)*chadep(n)*abs(theta(n))
c          wetxa(n)=wcap(n)/rl(n)/abs(theta(ii))
            wetxa(n)=wcap(n)/rl(n)/abs(theta(n))
!         next 2 added by NK Dec. 15/02
            wetarea(n)=wetwid(n)*rl(n)
!         added by TAS Mar 28/07 - for isotopes
            wetfrac(n)=wetarea(n)/(al*al*frac(n))
         endif
!       added the wetflg check Jul. 13/05  nk
         if(iopt.ge.1.and.wetflg.eq.'y')then
            if(n.eq.1)write(51,51003)
            if(n.eq.1)write(51,51002)
          write(51,51001)n,da(n),cap(n),chaxa(n),chadep(n),chawid(n),
     *           wetwid(n),wcap(n),wetxa(n),aclass(n,ntype-1),sl2(n)
       endif
      end do
      if(iopt.eq.2)print*,'In flowinit @ 169'


!     FLOW INITIALIZATION SECTION
!     THIS SECTION COMPUTES THE INITIAL FLOW AND STORAGE IN EACH RIVER
!     SEGMENT USING THE FIRST DOWNSTREAM GAUGE AND PRORATES THE FLOW
!     ACCORDING TO DRAINAGE AREA.F

!     RESERVOIR RELEASES ARE SUBTRACTED

!     QDAGRD IS QDA IN A GRID ARRAY USED FOR PRINTING IN THIS S/R
!     AND USED TO FILL IN


      do n=1,naa
         i=yyy(n)
         j=xxx(n)
         iset(n)=0
         qda(n)=0.0
         qbase(n)=0.0
         datemp(n)=0.0
      end do

      do i=1,imax
         do j=1,jmax
            qdagrd(i,j)=0.0
            nhyd(i,j)=0
         end do
      end do

      do n=naa+1,naa
         iset(n)=0
         qda(n)=0.0
         qbase(n)=0.0
         datemp(n)=0.0
      end do

!      do i=1,imax
!         do j=1,jmax
!            basinerr(i,j)=-9999.
!         end do
!      end do

      istep2=int(al/1000.)**2

!mesh_io      if(numa.eq.0) write(51,6000)

!      iopt1=iopt
!      iopt=max(1,iopt)

c      if(iopt.ge.1) write(53,5008)

      if(iopt.eq.2)print*,'In flowinit @ 221'


!     ERROR CHECKING:
!     check to see flow station is in a grid
      errflag='n'
      do l=1,no
         i=iy(l)
         j=jx(l)
         inbsnflg(l)=1          !  assume flow station is in a grid
!        if(iy(l).le.0.or.iy(l).gt.imax.or.jx(l).le.0.or.jx(l).gt.xmax)then
!     rev. 9.2.17  Oct.  11/05  - NK: Fixed bug for .str bounds in route
         if(iy(l).le.0.or.iy(l).gt.ycount
     *        .or.jx(l).le.0.or.jx(l).gt.xcount)then

!mesh_io             print*,iy(l),ycount,jx(l),xcount
!         this can happen if the stream gauge is outside the waterhsed
!         as when subwaterhseds are modelled as separate watersheds
!         added Mar 14/04 nk.
          n=0
!         nhyd(i,j) is already 0 from above
       else
          n=s(i,j)
          nhyd(i,j)=l
          if(iopt.ge.1) write(53,1777)l,i,j,s(i,j),nhyd(i,j)
       endif
       if(n.eq.0)then
!         station is not in a grid
!mesh_io          write(6,1779)l,i,j
!mesh_io          write(98,1779)l,i,j
          inbsnflg(l)=0         ! flow station is not in a grid
          errflag='y'
       endif
      end do

 20   continue

      if(iopt.eq.2)print*,'In flowinit @ 258'


!      print*,inbsnflg
!      print*
!      pause 'in flowinit @ 129'


cccccc      if(errflag.eq.'y')STOP ' program terminated in flowinit'

      if(iopt.eq.2)print*, 'in flowinit at 100'

!     SET UP THE INITIAL CHANNEL CONDITION FOR THE FIRST HYDROGRAPH:
!mesh_io      write(53,*)'  id    l    i    j    n   ',
!mesh_io     *         'nlow(l) qda(n) area(l)'
      do l=1,no
         if(inbsnflg(l).eq.1)then
            i=iy(l)
            j=jx(l)
            n=s(i,j)
            nhyd(i,j)=l

!         FIND THE FIRST +VE FLOW FOR THE FIRST HYDROGRAPH:

!         for watroute nr needs to be min(nh,nhtot)
!         for watroute use the flag to set this up


!mesh_io            write(53,*)nr,nl,mhtot

c          ktt=min(nr,nl,mhtot)
            ktt=nl              ! no of hours of data in the _str.r2c file



!         WHEN THERE IS NO RAINFALL AT ALL, NR = 0
!         THIS MIGHT HAPPEN WHEN THERE IS SNOWCOURSE DATA AND MELT
!         OCCURS WITHOUT RAIN
            ktt=max(kt,ktt)
            flowflag=.true.


!mesh_io            write(53,*)flowflag,kt,ktt,l,n,i,j

            do k=kt,ktt,kt
               if(qhyd(l,k).ge.0.0)then
!              THE FIRST +VE FLOW IS FOUND
                  flowflag=.false.
                  qda(n)=qhyd(l,k)
                  iset(n)=1     !```````````````````````````````````````
                  datemp(n)=da(n)
                  nlow(l)=k
                  ktemp=k
!                  print *, 'ddeacu: qhyd',l, ktt, kt, k, qda(n)
                  GO TO 15
               endif
            end do

 15         if(flowflag)then
!           ALL FLOWS AT THIS STATION ARE -1 (i.e. NO DATA)
!           SO SET AN INIT FLOW = 0.1 AT KTT=KT

!           DONE JAN. 14/00 IN ZURICH: LET INIT FLOW = DA/1000
!           FOR LACK OF ANYTHING BETTER <<<<<<<<<<<<<<<<<<<<<<<<<<<

               qda(n)=0.001_4*da(n)
               nlow(l)=kt
               ktt=kt

!           revised Feb. 18/02
               ktemp=kt
!     if there is no +ve flow, abort the program and ask user to estimate
!     an initial flow.
c      print*,' Error:'
c      print*,' No flows found in the first event'
c      print*,' Program needs initial flows to initialize routing '
c      print*,' and lower zone storage in each grid.'
c      print*,' Please edit .str file - insert estimated init. flow'
c      print*,' at each station.        18/02/02 nk'
c      print*
c      stop 'program aborted in flowinit @ 164'

            endif

            area(l)=da(n)

!          ASSIGN QDA AT EACH STREAMFLOW STATION

!mesh_io            write(53,5009)id,l,i,j,n,nlow(l),qda(n),area(l)


!          FIND LOWEST PRE-RISE FLOW:

!          TIS SECTION IF ACTIVATED WILL INITIATE FLOWS AT EACH GAUGE
!          THE LOWEST PRE-RISE FLOWS
            do k=ktemp+kt,ktt,kt
               if(qhyd(l,k).lt.0.0) GO TO 10
!            THIS MEANS WE'VE REACHED THE END OF THE FLOW DATA
               if(qhyd(l,k).le.qda(n))then
!              THIS MEANS WE'RE ON A RECESSION CURVE - NO GOOD
!              ESTIMATE OF INITIAL FLOWS IN BASIN IS TOO HIGH
!              WE'RE LOOKING FOR THE LOWEST FLOW PRECEDING THE RISE
!              BUT CUT OFF THE SEARCH AT THE END OF THE RAINFALL
                  qda(n)=qhyd(l,k)
                  nlow(l)=k

               elseif(qhyd(l,k).gt.qda(n).and.qda(n).gt.0.0)then

!              WE'RE OUT OF THE RECESSION CURVE
                  GO TO 10
               endif
            end do
 10         if(iopt.ge.1)then
               write(53,5009)id,l,i,j,n,nlow(l),qda(n),area(l)
            endif
         endif                  ! inbsnflg.eq.?
      end do


!     Compare computed to actual drainage areas

!     rev. 9.2.32  Feb.  10/06  - NK: Added area_check.csv to output
!mesh_io      INQUIRE(FILE='basin/flow_station_info.txt',EXIST=exists)
!mesh_io      IF(exists)THEN
!mesh_io         open(unit=99,file='basin/flow_station_info.txt',
!mesh_io     *        status='unknown')
!mesh_io         do l=1,no
!mesh_io            read(99,*,iostat=ios)sta_id(l),
!mesh_io     *           sta_lat(l),sta_long(l),sta_area(l)
!mesh_io            if(ios.ne.0)then
!mesh_io               print*,'Problems reading basin/flow_station_info.txt'
!mesh_io               if(l.ge.1)then
!mesh_io                  print*,'Last line read:'
!mesh_io                  write(*,*)l-1,'  ',sta_id(l-1),
!mesh_io     *             sta_lat(l-1),sta_long(l-1),sta_area(l-1),area(l-1)
!mesh_io               else
!mesh_io                  print*,'Problem is in the first line'
!mesh_io               endif
!mesh_io               print*
!mesh_io               stop 'Program aborted in flowinit @ 350'
!mesh_io            endif
!mesh_io         end do
!mesh_io         close(unit=99,status='keep')

!mesh_io         open(unit=99,file='area_check.xyz',status='unknown')
!mesh_io         do l=1,no
!          write(99,99001)sta_long(l),sta_lat(l),l,sta_id(l),
!     *        sta_area(l),area(l),(area(l)-sta_area(l))/area(l)*100.
!mesh_io            write(99,99001)xstr(l),ystr(l),l,sta_id(l),
!mesh_io     *      sta_area(l),area(l),(area(l)-sta_area(l))/sta_area(l)*100.
!mesh_io         end do
!mesh_io         close(unit=99,status='keep')
!mesh_io      end if

      if(iopt.ge.1)then
         iasdf=1
         msg=' init flow at gauges'
         write(53,5551)iasdf,msg
         do n=1,naa
            i=yyy(n)
            j=xxx(n)
            qdagrd(i,j)=qda(n)
            flow_max=max(flow_max,qda(n))
         end do
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif

      if(iopt.eq.2)print*, 'in flowinit at 300'

!     rev. 9.1.59  Jul.  15/04  - NK: split rerout into two parts: rdresv & rerout
!     A whole section that read the .rel file is gone from here.

!     reservoir data and releases nor entered in rdresv
!     INITIALIZE THE INIT RES RELEASE
      noread=noresv


      if(noread.gt.0)then       !~~~~~~~~~~~~~~~~~~~~~~~~~~start


         do i=1,noread
            qinit(i)=0.0
         end do

!     CHECK FOR 1ST +'VE FLOW AND SAVE IT
         if(iopt.eq.2)print*, 'in flowinit at 360'
         do j=ktr,nrel,ktr
            do i=1,noread
               if(qinit(i).le.0.0.and.qrel(i,j).ge.0.0)then
                  qinit(i)=qrel(i,j)
!                  print *, 'dddeacu: nrel, ktr, j, i, qinit(i):',
!     *                               nrel, ktr, j, i, qinit(i)
               endif
            end do
         end do


         if(iopt.eq.2)print*, 'in flowinit at 400'

!     SUBTRACT RESERVOIR RELEASES FROM MEASURED FLOWS:

!     LOOP THRU ALL ELEMENTS AND SUBTRACT UPSTREAM RESERVOIR RELEASE
!     FLOW FROM DOWNSTREAM GAUGE FLOWS.



!      print*,'in flowinit  noread=',noread


!     subtract the reservoir releases from the recorded flows at gauges
         do k=1,noread
            i=ires(k)
            j=jres(k)
            n=s(i,j)
            inbsnflg(no+k)=1
            if(n.le.0.and.nnn.le.0)then
!mesh_io               print*, 'Reservoir no.',k,' at i=',i,' j=',j
!mesh_io               print*, '     grid no ',n
!mesh_io               print*, 'is located outside the watershed'
!mesh_io               print*, 'Please note this grid is not in the .shd file'
               inbsnflg(no+k)=0
            endif
            resflag=.false.
            if(inbsnflg(no+k).eq.1)then
               do while(.not.resflag.and.n.le.naa)
                  if(qda(n).gt.0.0)then
!              WE'RE AT A GAUGE AND WE'LL SUBTRACT OUT THE RELEASE
!              RELEASE CAN'T BE GREATER THAN THE GAUGE FLOW
!              nothing is taken out if flow = natural
                     qda(n)=max(qda(n)-qinit(k),0.0_4)
!              CHECK TO SEE IF WE'VE RUN INTO ANOTHER RESERVOIR
!              WE HAVE TO CHECK THEM ALL
                     do mm=1,noread
                      if(yyy(n).eq.ires(mm).and.xxx(n).eq.jres(mm))then
!                    WE'VE FOUND ANOTHER RESERVOIR
                         resflag=.true.
                      endif
                   end do
                endif
                n=next(n)
!              CHECK IN CASE (NAA == NA).
                if(n.eq.next(n)) exit
             end do
          endif                 !inbsnflg
       end do

      endif                     !~~~~~~~~~~~~~~~~~~~~~~~~~~end







!      print*,inbsnflg
!      print*
!      pause 'in flowinit @ 422'



      if(iopt.ge.1)then
         write(53,6105)
         write(53,6100)
         write(53,6102)
     *        (n,yyy(n),xxx(n),iset(n),da(n),qda(n),qbase(n),n=1,naa)
         write(53,6110)
         do i=imax,1,-1
            write(53,6104)(nhyd(i,j),j=1,jmax)
         end do
         iasdf=3
         msg=' gauge flows - reservoir flows'
         write(53,5551)iasdf,msg

         do n=1,naa
            i=yyy(n)
            j=xxx(n)
            qdagrd(i,j)=qda(n)
            flow_max=max(flow_max,qda(n))
         end do
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif


      if(iopt.eq.2)print*, 'in flowinit at 500'

!   REV. 8.25 - May.  22/97 -  FIXED ALLOCATING THE BASIN # IN FLOWINIT

!     ASSIGN THE GRID TO A DRAINAGE BASIN FOR CALC OF SUM PRECIP
!     WORK UPSTREAM:
      do n=naa,1,-1
!        CURRENT LOCATION:
         i=yyy(n)
         j=xxx(n)
!        LOCATION OF DOWNSTREAM ELEMENT:
         nnx=next(n)
!mesh_io         if(nnx.le.0)then
!mesh_io            print*,' grid number',n,' does not have a recieving'
!mesh_io            print*,' within the grid limits'
!mesh_io            print*,' Possible problem: no blank grid around the'
!mesh_io            print*,' watershed'
!mesh_io            print*
!mesh_io            STOP ' Program aborted in flowinit at line ~339'
!mesh_io         endif
         inx=yyy(nnx)
         jnx=xxx(nnx)
         if(nnx.gt.0)then
!           WE ARE NOT IN ONE OF THE OUTLET ELEMENTS
            if(nhyd(i,j).le.0)then
!              WE ARE NOT ENTERING ANOTHER SUB-AREA
               if(da(nnx).gt.0.0)then
!                 WE'RE ABOVE A GAUGE AND WE CAN ALLOCATE THE TRIBUTARY
!                 AREA TO THE DOWNSTREAM GAUGE:
                  nhyd(i,j)=nhyd(inx,jnx)
!                  print *, 'ddeacu: upstream',
!     *                     naa, n, nnx, da(nnx),nhyd(inx,jnx)
               endif
            endif
         endif
      end do

      do n=1,no
         nxtbasin(n)=0          !added (n) nk
      end do

      if(iopt.eq.2)print*, 'in flowinit at 600'

!mesh_io      write(53,6044)
      do n=1,naa
!        CURRENT LOCATION:
         i=yyy(n)
         j=xxx(n)
!        LOCATION OF DOWNSTREAM ELEMENT:
         nnx=next(n)
         inx=yyy(nnx)
         jnx=xxx(nnx)
         if(nnx.gt.0)then
!           WE ARE NOT IN ONE OF THE OUTLET ELEMENTS
            if(nhyd(i,j).ne.nhyd(inx,jnx))then
!              WE ARE ENTERING ANOTHER SUB-AREA
               if(nhyd(inx,jnx).gt.0)then
!                 THIS SUB AREA IS A TRIBUTARY AREA TO THE NEXT
!                 SUB-BASIN :
!                  print *,'ddeacu2:',naa,n,nnx,da(nnx),nhyd(inx,jnx)
!mesh_io                  if(nhyd(i,j).eq.0)then
!mesh_io                     write(*,6046)inx,jnx
!mesh_io                     write(98,6046)inx,jnx

!     rev. 9.04    Jan    16/01 - fixed grid diagnosis in flowinit
!!1111111111111                     STOP 'program stopped in flowinit @65'
!mesh_io                  endif
                  if (nhyd(i,j).gt.0) nxtbasin(nhyd(i,j))=nhyd(inx,jnx)
!mesh_io                  write(53,6043)n,i,j,nhyd(i,j),nxtbasin(nhyd(i,j))
               endif
            endif
         endif
      end do
!mesh_io      write(53,6045)(l,nxtbasin(l),l=1,no)

      if(iopt.eq.2)print*, 'in flowinit at 700'

!     AFTER WORKING UPSTREAM, STORE THE LOCATION FOR ERROR CORRECTION
!     NBASIN() WILL BE USED IN LST.FOR
      do i=1,imax
         do j=1,jmax
            nbasin(i,j)=nhyd(i,j)
         end do
      end do

      if(iopt.ge.1)then
         write(53,*)' basin number allocations after working upstream'
         do i=imax,1,-1
            write(53,6104)(nhyd(i,j),j=1,jmax)
         end do
      endif

!     CALCULATE BASEFLOW FOR EACH ELEMENT:

!     WORK UPSTREAM
!     THE FOLLOWING INITIALIZES THE BASE FLOWS FOR EACH GRID POINT
!     ACCORDING TO THE NEAREST DOWNSTREAM GAUGE
!     RECORDED FLOW AT THE BEGINNING OF THE SIMULATION
!     RESERVOIR RELEASES ARE TAKEN OFF ABOVE


      do n=naa,1,-1
         if(qda(n).le.0.0)then
!           THIS MEANS WE ARE NOT AT A GAUGE (WITH FLOWS)
!           WE'LL ONLY ASSIGN A FLOW IF IT HAS NOT DONE BEFORE
!           THIS KEEPS FROM GOING PAST A GAUGE
            nnx=next(n)
            i=yyy(n)
            j=xxx(n)
            inx=yyy(nnx)
            jnx=xxx(nnx)
            if(nnx.gt.0)then
!              WHEN NNX = 0 WE ARE IN ONE OF THE OUTLET ELEMENTS
               if(da(nnx).gt.0.0)then
!                 WE'RE ABOVE A GAUGE AND WE CAN CALC BASE FLOW
!                 AND ALLOCATE THE TRIBUTARY AREA

!                   nhyd(i,j)=nhyd(inx,jnx)



!                 FIXED MAR. 23/97
!                 WE HAVE TO ALSO PASS THROUGH THE LAKES WITH
!                 NATURAL CONTROLS
!!                  if(ireach(n).eq.0.or.ireach(n).gt.noread)then
!                           taken out Dec. 21/05

                  qda(n)=qda(nnx)*da(n)/da(nnx)
                  datemp(n)=da(n)
!                    IF ISET=1 THEN DON'T TOUCH THIS BASE FLOW AGAIN!
                  iset(n)=1
!!                  endif    ! taken out Dec. 21/05

!                 QBASE IS USED IN RUNOF5 TO INITIALIZE LZS FOR BASEFLOW
                  qbase(n)=qda(n)
!                 JUST TO MAKE SURE:
                  if (qda(n).le.0.0) iset(n)=0
               endif
            endif
         endif
      end do
      if(iopt.ge.2)then
         write(53,6108)
         write(53,6100)
         write(53,6102)
     *        (n,yyy(n),xxx(n),iset(n),da(n),qda(n),qbase(n),n=1,naa)
      endif
      if(iopt.ge.1)then
         iasdf=4
         msg=' distributed base flows'
         write(53,5551)iasdf,msg
         do n=1,naa
            i=yyy(n)
            j=xxx(n)
            qdagrd(i,j)=qda(n)
            flow_max=max(flow_max,qda(n))
         end do
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif

      if(iopt.eq.2)print*, 'in flowinit at 800'

!     FILL IN REMAINING ELEMENTS
!     WORK DOWNSTREAM

!     MAKESURE GAUGES ARE MARKED AND NOT PASSED
      do l=1,no
         if(inbsnflg(l).eq.1)then
            i=iy(l)
            j=jx(l)
            n=s(i,j)
            iset(n)=1
         endif
      end do

!     ROUTE ALL GAUGE initial FLOWS DOWNSTREAM WHEN THERE'S
!     NO DOWNSTREAM GAUGE

      do l=1,no
         if(inbsnflg(l).eq.1)then
            i=iy(l)
            j=jx(l)
            n=s(i,j)
            datemp(n)=0.0
            da1=da(n)
            qda1=qda(n)
!        START AT EACH GAUGE AND WORK DOWNSTREAM
!        IF THE NEXT ELEMENT HAS A FLOW,
!        FLOW MAY HAVE BEEN ROUTED DOWN A TRIBUTARY OR
!        THERE IS A GAUGE DOWNSTREAM
            nnx=next(n)
            if(nnx.eq.0) cycle

            msg1='a'
            if(iopt.ge.2)then
               write(53,*)
     *              '      l    i    j    n next(n) iset(n) datemp',
     *              '         qda      datemp        qda'
               write(53,5556)msg1,l,i,j,n,next(n),iset(n),datemp(n),
     *              qda(n),datemp(nnx),qda(nnx)
            endif
!     this loop revised Dec. 22/05  nk
            do while(n.lt.naa.and.iset(nnx).ne.1)
!          DOWNSTREAM FLOW HAS NOT BEEN SET BY A GAUGE
!          IT COULD HAVE BEEN SET BY A GAUGE UP ANOTHER TRIBUTARY
!          IF ISET=1 THEN FLOW HAS BEEN SET BY A DOWNSTREAM GAUGE
!          AND WE DON'T TOUCH IT.
               nnx=next(n)      ! leave in - we're in an n loop!
               if(qda(nnx).le.0.0)then
!            FIRST TIME IN THIS ELEMENT
!            KEEP TRACK OF DRAINAGE AREA CONTRIBUTING TO THE FLOW
                  datemp(nnx)=da1
                  qda(nnx)=qda(n) !  *da(nnx)/da(n)
                  iset(nnx)=2
!            IF ISET=2 flow can be modified by tributary
                  msg1='b'
!dgp:   Having the check for 'iset' in the 'do' is insufficient
!       because 'nnx' is set inside the loop.
!       Without adding this check to the conditional statement below,
!       'iset' of 'nnx' will be set to '2',
!       regardless of its existing value.
               elseif(iset(nnx).ne.1)then
!            FLOW HAS TO BE COMBINED WITH FLOW FROM ANOTHER TRIBUTARY
!            SUM DRAINAGE AREAS AND FLOWS
!                  if (iset(nnx).ne.2) print *,
!     *                 'ddeacu: n, nnx, iset(nnx)',n,nnx,iset(nnx)
                  datemp(nnx)=datemp(nnx)+da1
                  qda(nnx)=qda(nnx)+qda1
                  msg1='c'
                  iset(nnx)=2
               endif
               if(iopt.ge.1)then
                 write(53,5556)msg1,l,xxx(n),yyy(n),n,next(n),iset(n),
     *                 datemp(n),qda(n),datemp(nnx),qda(nnx)
               endif
               n=next(n)
!             CHECK IN CASE (NAA == NA).
               if(n.eq.next(n)) exit
            end do
         endif                  ! inbsn
      end do
      if(iopt.eq.2)print*, 'in flowinit at 801'

!     prorate flow wrt. drainage areas  nk  Dec. 22/05
      do n=1,naa
         if(iset(n).eq.2)then
            qda(n)=qda(n)*da(n)/datemp(n)
            iset(n)=1
         endif
      end do

      if(iopt.ge.1)then
         iasdf=5
         msg=' base flows have been routed downstream'
         write(53,5551)iasdf,msg
      endif
!     FIXED A BUG HERE ON APRIL 16/96
!     THIS DO EXECTUED ONLY FOR IOPT.GE.1  -  NO GOOD I THINK

!mesh_io      write(53,5557)
      do n=1,naa
         i=yyy(n)
         j=xxx(n)
         qdagrd(i,j)=qda(n)
         qbase(n)=qda(n)
         if(iopt.ge.1)then
            write(53,5556)msg1,l,xxx(n),yyy(n),n,next(n),iset(n),
     *           da(n),qda(n),datemp(n)
         endif
      end do
      if(iopt.eq.2)print*, 'in flowinit at 802'
      if(iopt.ge.1)then
         iasdf=5
         msg=' base flows have been routed downstream'
         write(53,5551)iasdf,msg
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif
      if(iopt.eq.2)print*, 'in flowinit at 802'


!     NOW WORK BACK UPSTREAM:

!     HAVE TO HIT ALL grids BUT HAVE TO MAKE SURE THAT WE
!     DON'T CHANGE ANY FLOWS ALREADY ASSIGNED

!     this loop revised Dec. 22/05  nk
      do n=naa,1,-1
         if(iset(n).eq.0)then
!         THIS MEANS PROPER BASE FLOWS HAVEN'T BEEN ASSIGNED YET
            nnx=next(n)
!         GET THE BASE FLOW FROM THE DOWNSTREAM ELEMENT
!         WHICH SHOULD HAVE BEEN DEFINED BY NOW IF THERE IS AT
!         LEAST ONE GAUGE IN THE WATERSHED
!         FIRST TIME IN THIS ELEMENT
            if(qda(nnx).gt.0.0 .and. da(nnx).gt.0.0)then
!     rev. 9.2.18  Oct.  27/05  - NK: Fixed bug in flowinit (init spike)
!           qda(n)=qda(nnx)*da(n)/datemp(nnx)
               qda(n)=qda(nnx)*da(n)/da(nnx)
!dgp            else
!dgp               qda(n)=0.1       ! ok Jul. 11/02
            endif
            qbase(n)=qda(n)
         endif
      end do

      if(iopt.ge.1)then
         iasdf=6
         msg=' distributed base flows'
         write(53,5551)iasdf,msg
         msg1='f'
         write(53,5557)
         do n=1,naa
            i=yyy(n)
            j=xxx(n)
            write(53,5556)msg1,l,xxx(n),yyy(n),n,next(n),iset(n),
     *           da(n),qda(n),datemp(n)
         end do
      endif
      if(iopt.eq.2)print*, 'in flowinit at 803'

!     FIX ON APRIL 17/97
      do n=1,naa
         i=yyy(n)
         j=xxx(n)
         qdagrd(i,j)=qda(n)
         qbase(n)=qda(n)
      end do

      if(iopt.ge.1)then
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif
      if(iopt.eq.2)print*, 'in flowinit at 804'

!     NICK

!     ADD RELEASES BACK IN:

!     NOW WE HAVE TO ADD THE RELEASES BACK ON FOR THE ROUTING INIT
!     LOOP THRU ALL RESERVOIRS
!     GO DOWNSTREAM AND ADD QINIT TO THE REACHES BELOW THE DAMS
!     TO SET PROPER INITIAL RIVER FLOWS
!     WE'VE GOT TO TRACK THE RIVER TO THE OUTLET
!     OR TO THE NEXT RESERVOIR
      do k=1,noread
         if(inbsnflg(no+k).eq.1)then
            i=ires(k)
            j=jres(k)
            n=s(i,j)
            resflag=.false.
            do while(.not.resflag.and.n.le.naa)
               qda(n)=qda(n)+qinit(k)
               n=next(n)
!           CHECK IN CASE (NAA == NA).
               if(n.eq.next(n)) exit
!           CHECK TO SEE IF WE'VE RUN INTO ANOTHER RESERVOIR
!           WE HAVE TO CHECK THEM ALL
               do mm=1,noresv
                  if(yyy(n).eq.ires(mm).and.xxx(n).eq.jres(mm))then
!                 WE'VE FOUND ANOTHER RESERVOIR
                     resflag=.true.
                  endif
               end do
            end do
         endif                  ! inbsnflg
      end do

      if(iopt.ge.1) write(53,6109)

      if(iopt.ge.2)then
         write(53,6100)
         write(53,6102)
     *        (n,yyy(n),xxx(n),iset(n),da(n),qda(n),qbase(n),n=1,naa)
      endif
      if(iopt.eq.2)print*, 'in flowinit at 805'
      if(iopt.ge.1)then
         iasdf=7
         msg=' reservoir flows added back in'
         write(53,5551)iasdf,msg
         do n=1,naa
            i=yyy(n)
            j=xxx(n)
            qdagrd(i,j)=qda(n)
         end do
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif
      if(iopt.eq.2)print*, 'in flowinit at 806'

!     REPLACE THE PROPER QDA VALUES FOR LZS INITIATION IN S/R RUNOF5
      do n=1,naa
         qbase(n)=abs(qbase(n))
      end do

      if(iopt.ge.1)then
         write(53,6100)
         write(53,6102)
     *        (n,yyy(n),xxx(n),iset(n),da(n),qda(n),qbase(n),n=1,naa)
      endif


!     FIX ON June 19/06  nk
      do n=1,naa
         i=yyy(n)
         j=xxx(n)
         qdagrd(i,j)=qda(n)
      end do

      if(iopt.ge.1)then
         iasdf=8
         msg=' final init flows'
         write(53,5551)iasdf,msg
         if(flow_max.gt.99.9)then
            do i=imax,1,-1
               write(53,5554)(qdagrd(i,j),j=1,jmax)
            end do
         else
            do i=imax,1,-1
               write(53,5555)(qdagrd(i,j),j=1,jmax)
            end do
         endif
      endif

      if(iopt.ge.1)then
         iasdf=9
         msg=' write nhyd(i,j) '
         write(53,6110)
         write(53,5551)iasdf,msg
         do i=imax,1,-1
            write(53,5553)(nhyd(i,j),j=1,jmax)
         end do
      endif
      if(iopt.eq.2)print*, 'in flowinit at 808'




!     THIS SECTION CAME FROM RESET.FOR

!     rev  9.1.02  July  12/01  - put in dacheck in flowinit for flag
!         dacheck=al*al/1000000.*1000.
      dacheck=1.0e+10
!mesh_io      write(51,9801)dacheck

      do n=1,naa
         i=yyy(n)
         j=xxx(n)
         ii=ibn(n)
         qi1(n)=qda(n)
         qi2(n)=qda(n)
         qo1(n)=qda(n)
         qo2(n)=qda(n)

!     rev. 9.1.36  Jan.  28/03  - Fixed wetland init condition in flowinit

!        check for r2 value made in param.for
! * * * * * * * * TS - ADDED WATFLOOD ROUTING INITIALIZATION * * * * * *

!         if(aclass(n,ntype-1).gt.0.0.and.wetflg.eq.'y')then
!           qch=(wcap(n)/rl(n))**1.33*slope(n)/r2(ii)!
!         else

! stuff commented out     nk 28/01/03
         if(manningflg.eq.'y')then
            qch=(cap(n)/rl(n))**1.67_4*slope(n)/
     *           chawid(n)**0.667_4/r2n(n)
c     *                             chawid(n)**0.667/r2n(ii)
         else
c     qch=(cap(n)/rl(n))**1.33*slope(n)/r2(ii)
            qch=(cap(n)/rl(n))**1.33_4*slope(n)/r2(n)
         endif

!         endif



!        WHEN THE SLOPE IS .LE. 0.0 THE ELEMENT IS NOT IN THE BASIN
!        BUT it IS A RECEIVING ELEMENT
         if(slope(n).gt.0.0)then
            if(qda(n).le.qch)then
!             no overbank flow initially
               if(aclass(n,ntype-1).gt.0.0.and.wetflg.eq.'y')then
!                WETLAND FLOW INITIALIZATION
                  over(n)=0.0
                  if(manningflg.eq.'y')then
                     store1(n)=rl(n)*
     *                (qo2(n)*chawid(n)**0.667_4*r2n(n)/slope(n))**.60_4
c     *              (qo2(n)*chawid(n)**0.667*r2n(ii)/slope(n))**.60
                  else
c                   store1(n)=rl(n)*(qo2(n)*r2(ii)/slope(n))**.75
                     store1(n)=rl(n)*(qo2(n)*r2(n)/slope(n))**.75_4
                  endif
                  store2(n)=store1(n)
                  flowxa(n)=store1(n)/rl(n)
                  hcha1(n)=flowxa(n)/chawid(n)
                  hcha2(n)=hcha1(n)
                  hwet1(n)=hcha1(n)
                  hwet2(n)=hwet1(n)

!                assumes inbank flow only
c                  wstore1(n)=wetwid(n)*rl(n)*hwet1(n)*abs(theta(ii))
                  wstore1(n)=wetwid(n)*rl(n)*hwet1(n)*abs(theta(n))
                  wstore2(n)=wstore1(n)
c                 satxa(n)=wstore1(n)/rl(n)/abs(theta(ii))
                  satxa(n)=wstore1(n)/rl(n)/abs(theta(n))
               else
!                CHANNEL FLOW INITIALIZATION
!                 if(aclass(n,ntype-1).gt.0.0)
!     *              PAUSE 'initializing wetland flows as channel flows'
                  over(n)=0.0

                  if(manningflg.eq.'y')then
                     store1(n)=rl(n)*
     *                (qo2(n)*chawid(n)**0.667_4*r2n(n)/slope(n))**.60_4
c     *              (qo2(n)*chawid(n)**0.667*r2n(ii)/slope(n))**.60
                  else
c                   store1(n)=rl(n)*(qo2(n)*r2(ii)/slope(n))**.75
                     store1(n)=rl(n)*(qo2(n)*r2(n)/slope(n))**.75_4
                  endif
                  store2(n)=store1(n)

               endif
            else                ! overbank flow, wetland saturated
               if(aclass(n,ntype-1).gt.0.0.and.wetflg.eq.'y')then
!                WETLAND+OVERBANK FLOW INITIALIZATION

                  if(manningflg.eq.'y')then
c                   over(n)=((qo2(n)-qch)*r1n(ii)*6.0/slope(n))**.75
                     over(n)=((qo2(n)-qch)*r1n(n)*6.0_4/slope(n))**.75_4
!                  the factor of 6.0 is incorportated in r1
                  else
c                   over(n)=((qo2(n)-qch)*r1(ii)/slope(n))**.75
                     over(n)=((qo2(n)-qch)*r1(n)/slope(n))**.75_4
                  endif

                  store1(n)=rl(n)*(cap(n)/rl(n)+over(n))
                  store2(n)=store1(n)
                  flowxa(n)=store1(n)/rl(n)

                  obdepth=over(n)/(wetwid(n)+chawid(n))

!                  hcha1(n)=flowxa(n)/chawid(n)
!                for initial overbank flow,
!                all depths are assumed to be bankfull depth nk 28/01/03
                  hcha1(n)=chadep(n)+obdepth
                  hcha2(n)=hcha1(n)

                  hwet1(n)=0.80_4*chadep(n)

!                 hwet1(n)=hcha1(n)
                  hwet2(n)=hwet1(n)

!                  wstore1(n)=rl(n)*(wcap(n)/rl(n)+over(n))   nk 28/01/03
                  wstore1(n)=rl(n)*wcap(n)/rl(n)
                  wstore2(n)=wstore1(n)
c                 satxa(n)=wstore1(n)/rl(n)/abs(theta(ii))
                  satxa(n)=wstore1(n)/rl(n)/abs(theta(n))
               else
!                CHANNEL+OVERBANK FLOW INITIALIZATION
!                 if(aclass(n,ntype-1).gt.0.0)
!     *              PAUSE 'initializing wetland flows as channel flows'

                  if(manningflg.eq.'y')then
c                   over(n)=((qo2(n)-qch)*r1n(ii)*6.0/slope(n))**.75
                     over(n)=((qo2(n)-qch)*r1n(n)*6.0_4/slope(n))**.75_4
!                  the factor of 6.0 is incorportated in r1
                  else
c                   over(n)=((qo2(n)-qch)*r1(ii)/slope(n))**.75
                     over(n)=((qo2(n)-qch)*r1(n)/slope(n))**.75_4
                  endif

                  store1(n)=rl(n)*(cap(n)/rl(n)+over(n))
                  store2(n)=store1(n)
               endif
            endif
         endif

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

!        IF THE ELEMENT IS A 1ST ORDER ELEMENT, THEN SET THE INFLOW
!        FROM UPSTREAM = 0.0
         qmax(n)=0.0
         p(i,j)=0.0
         do ii=1,ntype+1
            r(n,ii)=0.0
         end do
         qr(n)=0.



c!        for water balance calcs use uzs for the waterclass:
c!        added Jul. 11/06  nk
c         if(frac(n).gt.0.0.and.aclass(n,ii_water).gt.0.0)then
c           uzs(n,ii_water)=store1(n)/convert/frac(n)/aclass(n,ii_water)
c         else
c           uzs(n,ii_water)=0.0
c         endif

      end do

      if(iopt.eq.2)print*, 'in flowinit at 1123'

!           QBASE(n) NOW BECOMES THE BASEFLOW CONTRIBUTION FROM EACH
!           SQUARE.  ITS FIRST USE IS TO DETERMINE INITIAL FLOW IN THE
!           RIVERS.  NOTE: IT IS MULTIPLIED BY FRACT LATER SO THIS IS
!           FOR FULL ELEMENTS

      tdum=1000._4*step2/3600._4
!     This section moved from soilinit   June 13/03
!     Why was it there anyway???????
      do n=1,naa
         if(da(n).gt.0.0)then
            qbase(n)=qbase(n)/da(n)*step2*frac(n)
! * * * * * * *  TS  - added initializations * * * * * * * *
            if(wetflg.eq.'y')then
               qiwet1(n)=qbase(n)
               qiwet2(n)=qbase(n)
               qowet1(n)=qbase(n)
               qowet2(n)=qbase(n)
            endif
!mesh_io         else
!mesh_io            print*,
!mesh_io     *           'Zero or negative drainage area found in grid no. ',n
!mesh_io            print*,'To accept this, hit return - but check outcome'
!mesh_io            print*,'Program may crash'
!mesh_io            print*,'To abort, hit  Ctrl C'
!mesh_io            print*
!mesh_io            qbase(n)=0.0
! * * * * * * *  TS  - added initializations * * * * * * * *
!mesh_io            if(wetflg.eq.'y')then
!mesh_io               qiwet1(n)=qbase(n)
!mesh_io               qiwet2(n)=qbase(n)
!mesh_io               qowet1(n)=qbase(n)
!mesh_io               qowet2(n)=qbase(n)
!mesh_io            endif
!            pause 'Paused in flowinit @ 984'
         endif
!mesh_lzs Commmented baseflow related code because it's outside RTE
!mesh_lzs         flz2(n)=1.0-(1.0-flz(n))
!mesh_lzs         pwr2(n)=1.0/pwr(n)     ! used in soilinit
!mesh_lzs         if(qbase(n).gt.0.0)then
!         WITHOUT THIS, N IS SET TO 0 !!!!!  FAR OUT & VERY WIERD
c          jj=ibn(n)
c         Copied over from runof6.for (thr=1):  AKB July 11, 2002
!mesh_lzs            lzs(n)=(qbase(n)/flz2(n)/tdum/frac(n))**pwr2(n)

!     rev. 9.4.13  Jul.  09/07  - NK: modified lzs to account for lake area (flowinit)
!         There is no lzs under laks & reservoirs so adjust:
!mesh_lzs            lzs(n)=lzs(n)*(1.0-aclass(n,ntype))

!         print*,n,qbase(n),flz2(jj),frac(n),pwr2(jj),lzs(n)
!mesh_lzs         else
!         print*,n,qbase(n),flz2(jj),frac(n),pwr2(jj)
!mesh_lzs            lzs(n)=1.0
!         DEFAULT VALUE
!mesh_lzs         endif
!mesh_lzs         write(53,*)n,flz(n),flz2(n),pwr(n),pwr2(n)
      end do
      if(iopt.eq.2)print*, 'in flowinit at 1173'

      qi1(na)=0.0
      qi2(na)=0.0
!      m=1
!      jan=1
!      tot1=0.
      qi2(na)=0.0

      if(iopt.ge.2)then
         write(53,6010)
         write(53,6011)(n,i,j,qda(n),qi1(n),qi2(n),qo1(n),qo2(n),
     *        store1(n),store2(n),cap(n),over(n),n=1,na)
      endif


!     initialize reservoir storage

!mesh_io      write(52,*)'Initial reservoir storage & outflow:'
!     unit 52 is simout/res.txt
      do k=1,noread
         i=ires(k)
         j=jres(k)
         n=s(i,j)
!mesh_io         write(52,*)n,k,i,j,store1(n),qo1(n)

!        if(b3(l).eq.0.0)then
!         tried to put this in the iteration loop but got spikes

!        For control with rating curve, use rating curve.
!        For controlled reservoirs, just use channel stoage rule
!        (above) so we at least have some value.
         if(b1(k).gt.0.0.and.b2(k).gt.0.0)then
            if(lake_area(k).gt.0.0.and.lake_elv(k,kt).gt.0.0)then
               store1(n)=lake_elv(k,kt)*lake_area(k)
            elseif(b6(k).gt.0.0.and.lake_elv(k,kt).gt.0.0)then
               store1(n)=max(0.0_4,lake_elv(k,kt)-b7(k))*b6(k)
            elseif(b3(k).eq.0.0)then
               store1(n)=(qo2(n)/b1(k))**(1.0_4/b2(k))
            elseif(b3(k).gt.0.0)then
               store1(n)=10.0
               trialq=0.0
               do while(trialq.lt.qo2(n))
                  store1(n)=2.0_4*store1(n)
                  trialq=store1(n)*(b1(k)+store1(n)*(b2(k)+store1(n)*
     *               (b3(k)+store1(n)*(b4(k)+b5(k)*store1(n)))))
               enddo
            endif
            store2(n)=store1(n)
         endif

!        else

!         initial quess
!          store1(n)=qo1(n)/b1(k)
!          trialq=0.0
!          do while(abs(trialq-qo1(n)).gt.(0.01*qo1(n)))

!            trialq=store1(n)*(b1(k)+store1(n)*(b2(k)+store1(n)*
!     *           (b3(k)+store1(n)*(b4(l)+b5(k)*store1(n)))))
!            if(trialq.lt.qo1(n))then
!              store1(n)=
!          end do

!        endif
!        store2(n)=store1(n)
!mesh_io         write(52,*)n,k,i,j,store1(n),qo1(n)
      end do

!mesh_io      if(numa.eq.0)write(51,*)'Flow initializing completed'


      if(iopt.eq.2)print*, 'in flowinit at 1223'


      RETURN

! FORMATS

 500  format(8f10.3)
 501  format(3i5,4x,a1)
 1011 format(' ',3x,
     *  '  i  ires(i) jres(i)    b1(i)     b2(i)    b3(i)     b4(i)')
 1013 format(' ',3x,i3,2i8,5f12.5,a12/)
 1761 format(' Error: prob. cause - gauge not in watershed'/
     *       ' gauge no. ',i3,' colum no.',i3,' row no.',i3/)
 1777 format(' ','in flowinit: l,i,j,n,nhyd/',5i5)
 1779 format(' stream gauge ',i5,' is outside the watershed'/
     *'     check the gauge locations in basin/xxxx.str and'/
     *'     in strfw/xxxx.str  Station is ignored!!!'/
     *'     This is OK if you are using the sub-watershed option'/
     *'     The station is at row (i) ',i5,' and column (j) ',i5/
     *'     run with iopt = 1 and check the spl.err file for info'/)
 5003 format(2i5,4g10.3,5x,a12)
 5004 format(2i5,5g10.3,5x,a12)
 5008 format(' sub'/
     *'   id    l    i    j    n nlow    qda       area - 1st&low'/)
 5009 format(6i5,2f12.3)
 5551 format('  write #',i2,a30)
 5553 format(999i5)
 5554 format(999f8.0)
 5555 format(999f5.1)
 5556 format(' ',a1,6i7,4f12.3)
 5557 format(' msg   bsn    col   row     n  next  n    iset     da',
     *    '       qda         datemp')
 5558 format('    n  row   col       iset     da',
     *    '     qda       datemp')
 6000 format(' ','Flowinit called to initialize flows and lzs')
 6001 format(5x,' no of storms =',i5)
 6002 format(35x,' * * * time =',f7.2,' hours * * *')
 6004 format('+',i5,' hrs rainfall available, ',i5,' hrs used')
 6005 format(' Warning: no streamflow stations selected for '/
     *' error calculation. Enter data in 1st line of .str file.')
 6007 format(' no,nl,mhtot,kt/',4i5)
 6008 format(' id,nr,tj1,mo,conv/',2i5,f5.2,i5,f5.2)
 6010 format(' reset:'/
     *  '    n    i    j         qda         qi1          qi2',
     *  '        qo1        qo2      store1      store2         cap',
     *  '         over')
 6011 format(3i5,9f12.3)
 6043 format(5I12)
 6044 format('        n        i         j     nhyd  nxtbasin')
 6045 format(' l,nxtbasin/',2i5)
 6046 format(' nhyd(',2i5,')=0 check if in watershed')
 6100 format(' [in flowinit]'/
     *'     n    i    j iset         da         qda       qbase')
 6102 format(' ',4i5,3f12.3)
 6103 format(' element #',i5,' base flow = 1 cms assumed')
 6104 format(999i4)
 6105 format(' initial flows modified by reservoir releases')
 6106 format(' i,qinit(i)/',i10,f12.3)
 6107 format(' n,yyy(n),xxx(n),i,ires(i),jres(i)/',6i3)
 6108 format(' initial base flows prorated upstream')
 6109 format(' reservoir releases added back in')
 6110 format(' basin number allocations')
 9005 format(' iymin,iymax,jxmin,jxmax/',4i10)
 9801 format(//'WARNING: DACHECK set to ',g12.3,' in flowint '//)
 5500 format(' n,i,ireach(n),nreach(i),maxr/',5i5)
51001 format(' ',i5,10f12.3,f12.7,i5)
51002 format('     n          da         cap       chaxa      chadep',
     *'      chawid      wetwid        wcap       wetxa  %wet class')
51003 format(' Channel properties:' )
99001 format(2f12.3,i5,2x,a12,3f10.0,' %')

      END SUBROUTINE flowinit





