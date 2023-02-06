      subroutine wf_route(wf_RouteTimeStep,wf_r1,wf_r2,
     b     wf_na,wf_naa,wf_ntype,wf_imax,wf_jmax,wf_iymin,
     b     wf_iymax,wf_jxmin,wf_jxmax,wf_yy,wf_xx,wf_ibn,wf_irough,
     b     wf_ichnl,wf_next,wf_ireach,wf_al,wf_grdn,wf_grde,
     b     wf_da,wf_bnkfll,wf_channelSlope,wf_elev,wf_frac,
     b     wf_CHNL_LEN,
     b     wf_RLFLAG, wf_CAPFLAG,
     f     wf_no,wf_nl,wf_mhrd,wf_kt,wf_iy,wf_jx,
     f     wf_qhyd,wf_res,wf_resstore, wf_noresv_ctrl,wf_r,
     r     wf_noresv,wf_nrel,wf_ktr,wf_ires,wf_jres,wf_resname,
     r     wf_b1,wf_b2,wf_b3,wf_b4,wf_b5,wf_qrel, wf_qr,
     s     wf_TimeCount,wf_nhyd,wf_qbase,wf_qi1,wf_qi2,wf_qo1,wf_qo2,
     s     wf_a1, wf_a2, wf_a3, wf_a4,
     s     wf_store1,wf_store2,
     +     DriverTimeStep,ROFGRD, NLAT, M_C,M_R,M_S, NLTEST,
     +     wf_s, jan,IDAY,IHOUR,IMIN)

      integer wf_ntype,wf_iymin,wf_iymax,wf_jxmin,
     +wf_jxmax,wf_imax,wf_jmax, wf_naa, wf_na
      real wf_grdn,wf_grde
      integer wf_RLFLAG, wf_CAPFLAG

      integer wf_yy(NLAT),wf_xx(NLAT),wf_ibn(NLAT),
     +     wf_irough(NLAT),wf_ichnl(NLAT),wf_next(NLAT),
     +     wf_ireach(NLAT),wf_elev(NLAT)
      real wf_da(NLAT),wf_bnkfll(NLAT),
     +     wf_channelSlope(NLAT)
      real wf_frac(NLAT)
      real wf_CHNL_LEN(NLAT)
      real wf_al

c for baseflow initialization
      integer jan

c     for routing
      integer wf_RouteTimeStep, wf_TimeCount, DriverTimeStep, NLTEST
      real wf_r1(M_C),wf_r2(M_C),wf_nhyd(NLAT),wf_qbase(NLAT)
      real wf_a1(M_C), wf_a2(M_C), wf_a3(M_C), wf_a4(M_C)
      real wf_qi2(NLAT),wf_qo1(NLAT),wf_qo2(NLAT), wf_qr(NLAT)
      real wf_store1(NLAT), wf_store2(NLAT),wf_qi1(NLAT)
      real ROFGRD(NLAT)

c     streamflow variables
      integer wf_no, wf_nl, wf_mhrd, wf_kt
      integer wf_iy(M_S),wf_jx(M_S), wf_s(M_S)
      real wf_qhyd(M_S)

c     reservoir variables
      integer wf_noresv, wf_nrel, wf_ktr, wf_noresv_ctrl
      integer wf_ires(M_R), wf_jres(M_R), wf_res(M_R), wf_r(M_R)
      real wf_b1(M_R),wf_b2(M_R),wf_qrel(M_R), wf_resstore(M_R)
      real wf_b3(M_R),wf_b4(M_R),wf_b5(M_R)
      character wf_resname(M_R)*8

c     Local variables
      logical converged,retryingIteration, resflag
      integer i,j,l,iter,ii,ll,n,ix,iy,nn
      integer iterPerStep,IterationsPerTimeStep
      integer nnx,inx,jnx,iset(NLAT)
      real cap,over,rl
      real rtemp,wf_qo2_last,wf_store2_last,div
      real qadd(NLAT)    ! qadd doesn't have to be an array
      real da1,qda1,qch,qinit(M_R),nxtbasin(M_S)
      real datemp(NLAT),daflow(NLAT)
      real qda(NLAT),qUngauged,daUngauged
      integer iHourCount
      integer WF_MAX_TIME_SUBDIVISIONS ,WF_MAX_ITERATIONS
      integer top(NLAT)

      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP

c from WATCLASS 2.7 - I'm assuming they are correct still
!-      wf_a1 = 1.0
!-      wf_a2 = 11.0
!-      wf_a3 = 0.43
!-      wf_a4 = 1.0

!-      do i=1,M_C
!-         wf_r1(i)=2.0
!-      enddo

c if this is the first time through, initialize baseflows
c ********************************************
c start of baseflow section

      IF(jan.eq.1) THEN !GOT TO BE A BETTER WAY TO DETERMINE THIS

c check if there was a resume file, if yes, then we don't need to initialize the routing
c data as the values should already be set in the resume.txt file.

!-      OPEN (52,FILE='resume.txt',STATUS='OLD',iostat=ios)
!-      IF(IOS.NE.0) THEN

c     Initialize
      do n=1,NLTEST              ! na or wf_naa ?
         i=wf_yy(n)
         j=wf_xx(n)
         iset(n)=0
         wf_nhyd(n)=0
         qda(n)=0.0
         wf_qbase(n)=0.0
c        CHANGE for wf_route: use two temporary da arrays
         datemp(n)=wf_da(n)
         daflow(n)=0.0
         top(n)=1
      end do

c     Which elements are at the drainage divide (no upstream channel)
c     after this routine all drainage divide elements top(n)=1 others top(n)=0
      do n=NLTEST-wf_naa,1,-1
         top(wf_next(n))=0
      end do

      do l=1,wf_no
c     Gauge locations
         n=wf_s(l)
c     If there is no data, let the gauge element be filled like any other grid
         if( wf_qhyd(l).gt.0.0 ) then
            wf_nhyd(n)=l
            qda(n)=wf_qhyd(l)
            iset(n)=1
            daflow(n)=wf_da(n)
         endif
      end do

      do l=1,wf_noresv
         wf_resstore(l)=0.0
      end do

c     Initialize qinit for controlled reservoirs
      do l=1,wf_noresv_ctrl
         qinit(l)=wf_qrel(l)
      end do

c     Loop thru all elements and subtract upstream reservoir releases
c     from downstream gauge flows
      do l=1,wf_noresv_ctrl
         do ll=1,wf_no
            n=wf_s(ll)
            if ( n.le.wf_r(l) ) cycle
            resflag=.false.
            do while( .not.resflag .and. n.le.NLTEST-wf_naa )
               if( qda(n).gt.0.0 ) then
c                 We're at a gauge and we'll subtract out the release
c                 Release can't be greater than the gauge flow
c                 WARNING - should record difference if negative
                  qda(n)=MAX(qda(n)-qinit(l),0.0)
               endif
               n=wf_next(n)
               if( wf_r(l).eq.n ) resflag=.true. ! Controlled reservoir
            end do
         end do
      end do

c     Calculate baseflow for each element

c     Work upstream
c     The following initialises the base flows for each grid point
c     according to the nearest downstream gauge recorded flow at
c     the beginning of the simulation
c     Reservoir releases are taken off above
      do n=NLTEST-wf_naa,1,-1
         if( qda(n).le.0.0 ) then
c           We are not at a gauge (with flows)
c           We'll only assign a flow if it has not done before
c           this keeps from going past a gauge
            nnx=wf_next(n)
            if( nnx.gt.0 ) then
c              We are not in one of the outlet elements
               if( wf_da(nnx).gt.0.0 ) then
c                 We're above a wf_gage and we can calc. base flow
c                 Consider both non-reservoirs and natural reservoirs
                  if( wf_ireach(n).eq.0 ) then
                     qda(n)=qda(nnx)*wf_da(n)/wf_da(nnx)
                     daflow(n)=wf_da(n)
                     iset(n)=1
                  else
c we are in a reservoir, check if we are at the outlet of the reservoir
                      resflag=.true.
                      do l=1,wf_noresv_ctrl
                        if(wf_r(l).eq.n) resflag=.false.
                      enddo
                    if (resflag) then
                       qda(n)=qda(nnx)*wf_da(n)/wf_da(nnx)
                       daflow(n)=wf_da(n)
                       iset(n)=1
                    endif
                  endif
c                 IMPORTANT:  this leaves sub-basin elements of a gauge
c                 with no flow data unset.  So, other gauges in the
c                 basin that have flow data have a chance to define
c                 the initial state of elements outside their sub-basin
                  if( qda(n).le.0.0 ) then
                     daflow(n)=0.0
                     iset(n)=0
                  endif
               endif
            endif
         endif
      end do

c     Fill in remaining elements

c     Work downstream, route known flows to outlet, recording
c     associated drainage area
      do n=1,NLTEST-wf_naa
         nnx=wf_next(n)
         if( iset(nnx).eq.0 ) then
            if( nnx.gt.wf_naa ) then
               datemp(nnx)=datemp(nnx)+wf_da(n)
            endif
            if( qda(n).gt.0.0 ) then
               daflow(nnx)=daflow(nnx)+daflow(n)
               qda(nnx)=qda(nnx)+qda(n)
            endif
         endif
      end do

c     Find the average of the outlets for a best guess at ungauged
c     sub-basin outlet flow
      qUngauged=0.0
      daUngauged=0.0
      do n=NLTEST-wf_naa+1, NLTEST
         if( qda(n).gt.0.0 ) then
            qUngauged=qUngauged+qda(n)
            daUngauged=daUngauged+daflow(n) ! drainage area associated
                                            ! with qUngauged
         endif
      end do

c     Work back upstream, filling basins with no data
      do n=NLTEST,1,-1
         if( iset(n).eq.0 ) then
            if( n.gt.NLTEST-wf_naa ) then
               if( daflow(n).gt.0.0 ) then
c                 Scale outlet element flow
                  qda(n)=qda(n)*datemp(n)/daflow(n)
               else
c                 Best guess at ungauged sub-basin outlet element
                  qda(n)=qUngauged*datemp(n)/daUngauged
               endif
            else
               nnx=wf_next(n)
               if( nnx.gt.NLTEST-wf_naa ) then
                  if( datemp(nnx).gt.0.0 ) then
c                    Fill flow at elements adjacent to outlet
                     qda(n)=qda(nnx)*wf_da(n)/datemp(nnx)
                  endif
               else
                  if( qda(n).le.0.0 ) then
c                    Fill flow at internal elements
                     qda(n)=qda(nnx)*wf_da(n)/wf_da(nnx)
                  else
                     if( daflow(n).gt.0.0 ) then
c                       Fill flow where two streams meet
                        qda(n)=qda(n)*wf_da(n)/daflow(n)
                     else
c                       Should never need this line, just a precaution
                        qda(n)=qUngauged*wf_da(n)/daUngauged !best guess
                     endif
                  endif
               endif
            endif
            iset(n)=1
         endif
      end do

c     Baseflows should all be set
      do n=1,NLTEST-wf_naa
         wf_qbase(n)=qda(n)
      end do

c     Add releases back in for routing initialization
c     Loop thru all reservoirs, working downstream and add qinit to
c     the reaches below the dams to set proper initial river flows.
c     Track the river to the outlet or to the wf_next reservoir.

      do l=1,wf_noresv_ctrl
         do ll=1,wf_no
            n=wf_s(ll)
            if ( n.le.wf_r(l) ) cycle
            resflag=.false.
            do while( .not.resflag .and. n.le.NLTEST-wf_naa )
               qda(n)=qda(n)+qinit(l)
               n=wf_next(n)
               if( wf_r(l).gt.n ) resflag=.true.
            end do
         end do
      end do

c     This section came from reset.for
      do n=1,NLTEST-wf_naa
         i=wf_yy(n)
         j=wf_xx(n)
         ii=wf_ibn(n)
         wf_qi1(n)=0.0
c        Only have initial input to downstream elements
         if( top(n).eq.1 ) then
            wf_qi2(n)=0.0
         else
            wf_qi2(n)=qda(n)
         endif
         wf_qo1(n)=0.0
         wf_qo2(n)=qda(n)

c        Cap is the volume of water in a reach for the mean annual flow
         rl=wf_al*wf_a1(ii)
         if (wf_RLFLAG == 1) rl = wf_CHNL_LEN(n)
         cap=(wf_a2(ii)+wf_a3(ii)*wf_da(n)**wf_a4(ii))*rl
         if (wf_CAPFLAG == 1) cap = wf_bnkfll(n)*rl
         qch=(cap/rl)**1.33*SQRT(wf_channelSlope(n))/wf_r2(ii)

         over=0.0
         wf_store1(n)=0.0
c this line had to be changed for certain compilers - Frank Seglenieks - Sep, 2010
c       original statement
c         if( wf_ireach(n).gt.0 .and. wf_b1(wf_ireach(n)).gt.0) then
c       modified statements
         if( wf_ireach(n).gt.0 ) then
         if( wf_b1(wf_ireach(n)).gt.0 ) then
c           Natural reservoir element
            l=wf_ireach(n)
            ! Check to see if there is a gauge at the natural reservoir outlet
            do ll=1,wf_no
c Gauge locations
                nn=wf_s(ll)
                if(nn.eq.n) then
! We found a gauge at the outlet
                   if( wf_qhyd(ll).gt.0.0 ) wf_qo2(n) = wf_qhyd(ll)
                end if
           end do
           if (wf_b3(l) == 0.0) then
            if(wf_b2(l).gt.0.0) then
                wf_store2(n)=(wf_qo2(n)/wf_b1(l))**(1.0/wf_b2(l))
            endif
           else
            wf_store2(n)=10.0
            try1=0.0
            do while(try1.lt.wf_qo2(n))
            wf_store2(n)=2.0*wf_store2(n)
            try1=wf_b1(l)*wf_store2(n)+
     +           wf_b2(l)*wf_store2(n)**2+
     +           wf_b3(l)*wf_store2(n)**3+
     +           wf_b4(l)*wf_store2(n)**4+
     +           wf_b5(l)*wf_store2(n)**5
!            write(53,*)try1, wf_b1(l), wf_qo1(n)
            end do
!            write(53,*)try1, wf_b1(l), wf_qo1(n)
           end if
c         elseif( wf_res(n).gt.0 ) then
c           Controlled reservoir
c           WHAT SHOULD THIS BE?  Unclear from rerout, etc.
c           Zero should be OK since must have release data for
c           controlled reservoir, wf_store2(n) just keeps track
c also had to add this - Frank Seglenieks - Sep, 2010
         endif
         else
c           When the wf_channelSlope is <= 0.0 the element is not in the
c           basin but is in a receiving element
            if( wf_channelSlope(n).gt.0.0 ) then
               if( qch.ge.qda(n) ) then
                  wf_store2(n)=rl*(wf_qo2(n)*wf_r2(ii)/
     +                 SQRT(wf_channelSlope(n)))**0.75
               else
                  over=((wf_qo2(n)-qch)*wf_r1(ii)*wf_r2(ii)/
     +                 SQRT(wf_channelSlope(n)))**0.75
                  wf_store2(n)=cap+rl*over
               endif
            endif
         endif
      end do                    ! do n=1,wf_naa

c      write(707, *) 'After big loop that came from reset.for'
c      do n=1,NLTEST-wf_naa
c      if( wf_ireach(n).gt.0 ) then
c         if( wf_b1(wf_ireach(n)).gt.0 ) then
cc           Natural reservoir element
c            l=wf_ireach(n)
c            write(707, *) l, n, qda(n),wf_qo2(n)
c         endif
c      endif
c      end do
c      close(707)

c     Initialize outlet elements
      do n=NLTEST-wf_naa+1, NLTEST
         wf_qi1(n)=0.0
         wf_qi2(n)=0.0
         wf_qo1(n)=0.0
         wf_qo2(n)=0.0
         wf_store1(n)=0.0
         wf_store2(n)=0.0
      end do

!-        ENDIF  !IF(IOS.NE.0) THEN

      JAN=2

      ENDIF  !IF(jan.eq.1) THEN

c end of baseflow section
c ********************************************


c     Initializations
      wf_TimeCount=wf_TimeCount+1
      iHourCount = INT( REAL(wf_TimeCount+1)*DriverTimeStep/3600.0 )

      IterationsPerTimeStep=int(DriverTimeStep/wf_RouteTimeStep+0.5)
      if( IterationsPerTimeStep.lt.1 ) IterationsPerTimeStep=1

      WF_MAX_TIME_SUBDIVISIONS = 10
      WF_MAX_ITERATIONS = 20

c     Convert kg/m^2/s of water (RUNOFF(I)) to cms (wf_qr(i))
      do i=1,NLTEST
         wf_qr(i) = ROFGRD(I)*(wf_al**2)/rhow
      enddo
      retryingIteration=.FALSE.
 100  continue

      do iterPerStep=1,IterationsPerTimeStep
         div=DriverTimeStep/real(IterationsPerTimeStep)/2.0 ! time step
c        Carry forward flows/storage from last time step
         if( .NOT.retryingIteration ) then
            do i=1,NLTEST-wf_naa
               wf_store1(i)=wf_store2(i)
               wf_qo1(i)=wf_qo2(i)
               wf_qo2(i)=0.0
               wf_qi1(i)=wf_qi2(i)
               wf_qi2(i)=0.0
            end do
         else
            do i=1,NLTEST-wf_naa
               wf_qo2(i)=0.0
               wf_qi2(i)=0.0
               wf_store2(i)=wf_store1(i)
            end do
            retryingIteration=.FALSE.
         endif

c     Routing loop
c     Calculations start in the highest element in the watershed
c     and proceed to the lowest.

      do i=1,NLTEST-wf_naa

        if( wf_next(i).gt.0.0 ) then ! wf_next < 0 when not in basin
               qadd(i)=wf_qr(i)

          if( wf_ireach(i).gt.0 ) then ! We are in a Reservoir
               l=wf_ireach(i)
             if (wf_b1(l).eq.1.0 .and. wf_b2(l).eq.0.0) then !Reservoir type for zone-based storage
C Are we at the outlet
              if(wf_r(l).eq.i) then
C yes we are at the outlet
C Call external code to get discharge and storage
                  wf_qi2(i)=wf_resstore(l)+qadd(i)+wf_qi2(i)
                  call zonebased_reservoir_release(
     +                    IYEAR,IDAY,i,l,wf_qi2(i),div*2.0, !input
     +                    wf_qo2(i),wf_store2(i))           !output
                  wf_resstore(l)=0.0
               else
C no we are in the reservoir
C accumulate flow to the outlet
                  wf_resstore(l)=wf_resstore(l)+qadd(i)+wf_qi2(i)
                  wf_qo2(i)=0.0
	              wf_store2(i)=0.0
              endif !are we at the outlet

C are we in a natural or controlled reservoir?
             else if(l.gt.wf_noresv_ctrl) then !Natural reservoir routing
C Are we at the outlet of a natural reservoir
              if(wf_r(l).eq.i) then
C yes we are at the outlet, use the big storage term to determine wf_qo2

                  wf_qi2(i)=wf_resstore(l)+qadd(i)+wf_qi2(i)
                  wf_store2(i)=wf_store2(i)+wf_qi2(i)
                if (wf_b3(l) == 0.0) then
                  wf_qo2(i)=wf_b1(l)*wf_store2(i)**wf_b2(l)
                else
                  wf_qo2(i)=wf_b1(l)*wf_store2(i)+
     +wf_b2(l)*wf_store2(i)**2+wf_b3(l)*wf_store2(i)**3+
     +wf_b4(l)*wf_store2(i)**4+wf_b5(l)*wf_store2(i)**5
                end if
                  wf_qo2=max(wf_qo2,0.0)
                  wf_store2(i)=wf_store2(i)-wf_qo2(i)
                  wf_resstore(l)=0.0

               else
C No we are just in the reservoir, hence just add the flow to the big
C storage reservoir and give fake value to outflow and storage
c                  wf_resstore(l)=wf_resstore(l)+qadd(i)*div
                  wf_resstore(l)=wf_resstore(l)+qadd(i)+wf_qi2(i)
                  wf_qo2(i)=0.0
                  wf_store2(i)=0.0

              endif !if(wf_ires(l).eq.wf_yy(i).and.wf_jres(l).eq.wf_xx(i)) then

             else ! Controlled reservoir routing
C are we at the outlet of the controlled reservoir
              if(wf_r(l).eq.i) then
C yes we are at the outlet
                  wf_qo2(i)=wf_qrel(l)
                  wf_qi2(i)=wf_resstore(l)+qadd(i)+wf_qi2(i)
                  if( wf_qo2(i).lt.0.0 ) wf_qo2(i)=wf_qo2(i)
                  wf_store2(i)=wf_store2(i)+wf_qi2(i)-wf_qo2(i)
                  wf_resstore(l)=0.0
c                  wf_store2(i)=wf_store1(i)+(wf_qi1(i)+wf_qi2(i)-
c     +wf_qo1(i)-wf_qo2(i))*div+qadd(i)*div*2.0
               else
c no we are in the reservoir
c don't really know what to do here, the flow doesn't really matter
c as it will be overwritten later, just for something keep adding the
c flow to the big reservoir stoage
                  wf_resstore(l)=wf_resstore(l)+qadd(i)+wf_qi2(i)
                  wf_qo2(i)=0.0
                  wf_store2(i)=0.0

               endif !are we at the outlet of controlled reservoir

             endif  !end of natural vs controlled reservoir question


          else ! Channel routing
                  ii=wf_ibn(i)
                  wf_qo2_last=wf_qo1(i)
                  wf_store2_last=1.0e+25
                  iter=1
                  converged=.FALSE.

                  do while( .not.converged .and.
     +                 iter.le.WF_MAX_ITERATIONS )
c                    This iterates to 0.3%
                     if( ABS(wf_store2_last-wf_store2(i))
     +.gt.0.003*wf_store2_last ) then

                        if( wf_store2(i).le.0.0 ) then
c                          No outflow - channel is empty
                           wf_qo2(i)=0.0
                        else
c                          Cap is the volume of water in a reach
c                          for the mean annual flow
                           rl=wf_al*wf_a1(ii)
                           if (wf_RLFLAG == 1) rl = wf_CHNL_LEN(i)
                           cap=
     +(wf_a2(ii)+wf_a3(ii)*wf_da(i)**wf_a4(ii))*rl
                           if (wf_CAPFLAG == 1) cap = wf_bnkfll(n)*rl
                           over=(wf_store2(i)-cap)/rl
                           if( over.le.0.0 ) then
c                             Channel flow only
                              over=0.0
                              wf_qo2(i)=(wf_store2(i)/rl)**1.33*
     +SQRT(wf_channelSlope(i))/wf_r2(ii)
                           else
c                             Channel and flood plain flow
                              wf_qo2(i)=((cap/rl)**1.33+over**1.33/
     +wf_r1(ii))*SQRT(wf_channelSlope(i))/wf_r2(ii)
                           endif  !if( over.le.0.0 ) then

                           rtemp=MAX(0.5d00,REAL(iter)/21.0)
                           wf_qo2(i)=(1.0-rtemp)*wf_qo2(i)+rtemp*
     +wf_qo2_last
                           wf_qo2_last=wf_qo2(i)

                        endif  !if( wf_store2(i).le.0.0 ) then

                        wf_store2_last=wf_store2(i)
                        wf_store2(i)=wf_store1(i)+(wf_qi1(i)+wf_qi2(i)-
     +wf_qo1(i)-wf_qo2(i))*div+qadd(i)*div*2.0

c                       tw  Added this and tested, it helps convergence
c                       by 50% during very big storms
                        if( wf_store2(i).le.0.0 ) then
                           wf_qo2(i)=wf_store1(i)/div+wf_qi1(i)+
     +wf_qi2(i)-wf_qo1(i)+qadd(i)*2.0
                           wf_store2(i)=0.0
                        endif

                     else
                        converged=.TRUE. ! Convergence to 3%
                     endif !if( (ABS(wf_store2_last-wf_store2(i))/wf_store2_last).gt.0.003 ) then

                     iter=iter+1
                  end do

               endif            ! if( wf_ireach(n).gt.0 ...

               if( (wf_store2(i).le.0.0 .or. wf_qo2(i).le.0.0)
     +                   .and.wf_ireach(i).lt.1 ) then
                  if( IterationsPerTimeStep.lt.WF_MAX_TIME_SUBDIVISIONS
     + ) then
c                    Try changing time step, and redo whole basin
                     IterationsPerTimeStep=IterationsPerTimeStep+1
                     retryingIteration=.TRUE.
                        if( wf_qo2(i).le.0.0 ) then
                           print *, 'wf_qo2 <= 0, change time step',i,
     +wf_qo2(i),wf_store2(i),IterationsPerTimeStep
                        else
                           print *, 'wf_store2 <= 0, change time step',
     +i,wf_qo2(i),wf_store2(i),IterationsPerTimeStep
                        endif
                     goto 100

                  elseif( wf_store2(i).le.0.0 ) then
c                    Already at minimum time step, so give up
ckrs                 Leave -ve stores if controlled reservior
                       if(wf_ireach(i).le.0) then
                         wf_qo2(i)=wf_store1(i)/div+wf_qi1(i)+wf_qi2(i)-
     +wf_qo1(i)+qadd(i)*2.0
                         wf_store2(i)=0.0
                       endif
                  endif
               endif

c              Add flow to downstream element
               wf_qi2(wf_next(i))=wf_qi2(wf_next(i))+wf_qo2(i)
            endif               ! if( wf_next(i).gt.0.0 ) then
c      grids above gauges
      end do                    ! do i=1,NLTEST-wf_naa

      end do                    ! do iterPerStep

      return
      end

