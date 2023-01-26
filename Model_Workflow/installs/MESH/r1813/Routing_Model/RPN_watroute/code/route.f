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

      SUBROUTINE route(div,thr,dtmin,mindtmin,convthresh,jz,iz,time,
     *                 date,exitstatus)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!***********************************************************************
!
!  THE INFLOW INTO THE CHANNEL IN EACH ELEMENT IS THE SUM OF THE
!  OUTFLOW OF THE ELEMENTS WHICH DRAIN INTO IT FROM ABOVE AND THE
!  SURFACE RUNOFF AND SUBSURFACE FLOW FROM THE ELEMENT IN WHICH THE
!  OUTFLOW IS BEING CALCULATED                                 
!
!     rev. 8.23   Mar.  25/96   - fixed bug in route - keep qo2 for res
!     rev. 8.99mm Dec. 13/2001  - added check for <= 0 init res flow
!     rev. 8.99n  Dec. 31/2001  - fixed nat. res initial flow (JW)
!
!     rev. 9.00    Mar.  2000   - TS: CONVERTED TO FORTRAN 90
!     rev. 9.03    Nov.  2000   - TS: ADDED WATFLOOD SWAMP ROUTING 
!     rev  9.1.03  July  24/01  - added polinomial to reservoir routing
!     rev. 9.1.10  Jan.  29/02  - flow nudging added for nopt(l)=2
!     rev. 9.1.14  Mar.  24/02  - fixed wetland min time step and outflow
!     rev. 9.1.16  Apr.  03/02  - Added wetland conditional to select river w/wo wetland
!     rev. 9.1.31  Nov.  13/02  - Fixed the wetland Q to account for wetland area
!     rev. 9.1.33  Dec.  05/02  - Fixed instability in wetland flow    
!     rev. 9.1.38  Mar.  31/03  - revised str header and routing dt selectable
!     rev. 9.1.39  Apr.  06/03  - Fixed wetland routing when channel is dry
!     rev. 9.2.11  Sep.  11/05  - NK: added Manning's n  r1n & r2n
!     rev. 9.2.13  Sep.  28/05  - NK: added freeze and break up to route
!     rev. 9.2.23  Nov.  22/05  - NK: Fixed res(n)=0 bug in route 
!     rev. 9.2.35  Mar.  22/06  - NK: Glacier flow bypasses wetlands
!     rev. 9.2.39  May.  09/06  - NK: t added to route & rerout arg list
!     rev. 9.2.43  Jun.  21/06  - NK: fixed spikes in route
!     rev. 9.3.04  Oct.  24/06  - NK: routing parameters dim to na in rte
!     rev. 9.3.10  Jan.  29/07  - NK: routing pars changed to gridded values
!                                 eg: lzf(ii) -> lzf(n) 
!
!     changes made to include c&g model stuff  nk  April. 26/07
!
!***********************************************************************

      use area_watflood
      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

! Christopher Subich (9/12): istate was allocated as (400,400),
! which led to bad array-out-of-bounds errors when the full 2D
! grid was larger than that
      INTEGER :: istate(imax,jmax),
     *           rbin,lsta,nnn1,jz,n,ll,lll,l,iz,jjz,
     *           i,j,ii,ic,jj,ios,itracker,unt,ln,n_dt_min,hour_offset,
     *           hour_offset_div,rr,repirr,rr2,repn,repn2
      REAL*4    :: old,oldwet,convert,ovbfact,qo2remtemp
      REAL(4) :: time,newstore,try1,try2,try3,div,thr,at,dtmin,hold,
     *             wt,atemp,ax,xa,at1,ice_fctr,dt_min_n
      REAL*4    :: mindtmin
      character*1 :: flowsta,firstpass
      character*80 :: junk
      character(14) :: date
      logical :: exists      
      REAL*4 :: storetest1
      REAL*4 ovD/0.01_4/  ! ovD is the slope of the overbanks (set to 1% but could be read using topographic information)
      REAL*4 conv,convthresh
      integer :: maxic = 1000
      REAL*4 minwt/0.5/  ! damps the calculation of qo2; values ranges from 0 to 1; larger values damp more
      REAL*4 wtstore/0.5/  ! damps the calculation of store2; values ranges from 0 to 1; larger values damp less
      REAL*4 wtfact
      INTEGER exitstatus,ixxx,iyyy
      REAL*4 teststore1
      INTEGER mate
      REAL*4 qo2add

      CHARACTER fmtfile*100

      DATA firstpass/'y'/

! csubich -- use array initialization syntax to set istate=0
      istate = 0

      if(firstpass.eq.'y')then   ! changed May 8/06 nk

!       skip this section if using a resume file !!!!  nk 11/11/04
!       INDEX IS INITIALIZED IN S/R SUB
!       CALLED TO READ IN THE RELEASES AND RESERVOIR DETAILS
!       CAN'T USE JAN HERE BECAUSE WE MIGHT CALL IT SEVERAL TIMES
!       USE RESERVOIR L=1 HERE JUST TO CALL REROUT FOR INT VALUES
!       USE ELEMENT N=NA JUST TO CALL REROUT
!       NNN1=NA IS JUST A DUMMY VARIABLE FOR AN UNUSED STORAGE
        
        nnn1=na
        convert=al*al*1000.0_4  ! converts vol in m^3 to mm on the unit grid
        ovbfact=0.75_4*ovD**0.3333_4 ! ovbfact is a constant used in the calculation of Qo2 for the overbank part        

!       first time through
        do n=1,naa
          res(n)=0
        end do

!       This should probably be moved to flowinit or sub where qda is initialized
!       but it is much easier to do this here because rerout has been called now
!       and we have the values for b1().....b4()
!       But we must initialize the storage only once so use id=1
!       for this part only. Can't use jan here because we visit more than
!       once per time step

        do n=1,naa
          lll=next(n)
          do l=1,noresv

!           LOOK TO SEE IF THERE IS A RESERVOIR IN THIS GRID:

            if(yyy(n).eq.ires(l).and.xxx(n).eq.jres(l))then
              res(n)=l
!             IF THERE IS, SET INITIAL RESERVOIR STORAGE:

!              if(b1(l).gt.0.0.and.b2(l).gt.0.0)then
              if(b1(l).ne.0.0.or.b2(l).ne.0.0)then

!               WE HAVE A NATURAL CONTROL & WE NEED INITIAL
!               RESERVOIR STORAGE
!               INITIAL FLOWS ARE CALC IN SUB SO LEAVE THEM ALONE 

                if(id.eq.1)then

!     rev. 9.1.07  Jan. 03/02  - check that outlet is in a lake NK

                if(ireach(n).eq.0)then
                  print*,'in route in grid number',n
                  print*,'reach number =',ireach(n)
                  print*,'i.e. lake or reservoir #',l,' outlet' 
                  print*,'is not in a lake'
                  print*,'please make grid part of a lake or'
                  print*,'move outlet to a grid in a lake'
!                 note that dam locations with releases do not have to be in a lake!! 
                  print*
                  stop 'Program aborted in route @ 170'
                endif
!               rev. 8.99mm Dec. 13/2001-     added check for <= 0 init res flow
!               check for +ve flow for res. initializations

!               probable don't need this if we are using a resume file

                if(qo1(n).le.0.0)then
                  print*,'Initial flow at reservoir',l, resname(l)
                  print*,'is .le. 0.0 and can not be initialized'
                  print*,'please ensure there is a downstream '
                  print*,'flow station with a flow > 0.0 '
                  print*,'We are in row',yyy(n),'column',xxx(n)
                  print*,'grid no',n,'with init flow =',qo1(n) 
                  print*
!                  stop ' Program aborted in route @ 64'
                endif

!                  if(poliflg.ne.'y'.or.
!     *                 abs(b3(l)+b4(l)+b5(l)).le.0.1e-32)then
                if(poliflg(l).ne.'y')then

!                 don't get rid of the poliflg because of the format
!                 for the header section is not compatible with the old files
!                 use 2 coefficients  

                  if(resumflg.ne.'y')then
                    store1(n)=(qo1(lll)/b1(l))**(1.0_4/b2(l))
                  endif

                else

                  if(b3(l).eq.0.0)then
!                   use 2 coefficients  
                    if(resumflg.ne.'y')then
                      store1(n)=(qo1(lll)/b1(l))**(1.0_4/b2(l))
                    endif
                  else
!                   use bisection to get init flow
!                   actually, I made the int. storage a little larger
                    if(resumflg.ne.'y')then
                      store1(n)=100.
                      try1=0.0
!                     use 2-5 coefficients
                      write(52,*)'         n           l          try1',    
     *                     '         qo1        store1'
                      do while(try1.lt.qo1(lll))
!                       keep doubling the res. storage until the corresponding
!                       flow is larger than the initialized streamflow.
                        store1(n)=2.0_4*store1(n)                      
                        try1=b1(l)*store1(n)+
     *                       b2(l)*store1(n)**2+
     *                       b3(l)*store1(n)**3+
     *                       b4(l)*store1(n)**4+
     *                       b5(l)*store1(n)**5
                        write(52,*)n,l,try1,qo1(n),store1(n),'poli'
                        if(try1.lt.0.0)then
                          print*,'trial value for flow =',try1
                          print*,'trial reservoir outflow < 0.0'
                          print*,'polinomial functions not'
                          print*,'monotonically increasing'
                          print*,'for reservoir',l
                          print*,'Please fix the function'
                          print*,'Program may excecute but results' 
                          print*,'are approximate'
                          print*,' '
                          print*
!                          pause 'Hit enter to continue. @ 167/route'
                        endif
                      end do

                    endif
                  endif
                write(52,*)n,l,try1,qo1(n),store1(n),'done'
                write(52,*)

                endif

                  if(resumflg.ne.'y')then
                    store1(n)=max(100.0_4,store1(n))
                    store2(n)=store1(n)
                    store2_strt(n)=store2(n)
                    if(iopt.ge.1)then
                      write(51,8492)
                      write(51,8493)n,l,b1(l),b2(l),qda(n),store1(n)
                      write(53,8492)
                      write(53,8493)n,l,b1(l),b2(l),qda(n),store1(n)
                    endif
                  endif

                endif             ! id=1

              endif   
            endif
          end do   ! closes the loop: do l=1,noresv
        end do     ! closes the loop: do n=1,naa

        junk='Into route, passed 90'
 
       if(iopt.eq.-9)write(98,9801)junk

        if(iopt.ge.2)then
          write(53,6007)
!          write(53,6000)(res(n),n=1,naa)  ! Generates a huge output file if the domain is large
        endif

      endif        ! closes the loop: if(firstpass.eq.'y')then

      index=2

!mesh_io      if(firstpass.eq.'y')then   !section added Apr. 28/06  nk
!       check that qdwpr memory has been allocated
!mesh_io        if(ireach(n).gt.0.or.res(n).gt.0)then
!mesh_io          if(.not.allocated(qdwpr))then
!mesh_io             print*
!mesh_io             print*,'Memory not allocated for qdwpr'
!mesh_io             print*,'No of reservoirs in the .rel file is 0'
!mesh_io             print*,'but reaches in the map & shed files have been'
!mesh_io             print*,'defined. This is a problem.'
!mesh_io             print*,'Please either set all reach values = 0'
!mesh_io             print*,'or code in the reservoir locations in the rel'
!mesh_io             print*,'files'
!mesh_io             stop 'Program aborted in route @ 279'
!mesh_io          endif      
!mesh_io        endif
!mesh_io      endif

!     CALCULATIONS START IN THE HIGHEST ELEMENT IN THE WATERSHED 
!     AND PROCEED TO THE LOWEST.

      if(iopt.ge.2)then
        write(53,6002)jz,jz
        write(52,6002)jz,jz
        write(53,6003)
      endif
 
      jjz=fhr
      if(jjz.lt.1) jjz=1
      dt_min_n=1.0e32_4

      do rbin=1,noresv
        qdwpr(rbin,jjz)=0.0
      end do

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      if(iopt.eq.2)print*, 'In route before the 1-naa loop'

      do n=1,naa
       
!       REV. 8.23 - Mar.  25/96 - FIXED BUG IN ROUTE, KEEP QO2 FOR RES
        store1(n)=store2(n)
        wstore1(n)=wstore2(n)
        hwet1(n)=hwet2(n)
        hcha1(n)=hcha2(n)
        qo1(n)=qo2(n)
        if(res(n).eq.0)qo2(n)=0.0
        qowet1(n)=qowet2(n)
        qi1(n)=qi2(n)
        qi2(n)=1.0e-10_4 ! Initializing value
        qiwet1(n)=qiwet2(n)
        qiwet2(n)=1.0e-10_4

!       EG_MOD if flow insertion or flow diversion,
!       initiate QO1 with qo2sim (simulated) due to possibility of modified qo2
!       which could empty channel
        if (trim(strfw_option)=='streamflow_insertion') then
          do l=1,no
            if(iflowgrid(l).eq.n)then
              qo1(n)=qo2sim(n)
            end if
          end do
        end if

        repirr=0
        do l=1,nodiv
          if (val2divyes.eq.1) then
            if (divname(l)(1:5).eq.'irrig'
     *      .or. divname(l)(1:5).eq.'Irrig') then
                repirr=repirr+1
                do rr=1,maxirrigpts
                  repn = irrigindx(repirr,rr)
                  if (repn.eq.n) then
                    qo1(repn)=qo2sim(repn)
                  endif
                enddo
            else
                if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                  qo1(n) = qo2sim(n)
                endif
            endif
          else
	      if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                 qo1(n) = qo2sim(n)
              endif
          endif
        enddo
      enddo      

      qi2(na)=0.0
      wt=minwt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11!!!!!!!!!!!!!!!

!     ROUTING LOOP:

      do n=1,naa

        if(iopt.eq.2.and.n.eq.1)print*,'In route, passed 101'

        i=yyy(n)
        j=xxx(n)
        l=nhyd(i,j)

!       WHEN THE SLOPE <= 0.0 THE ELEMENT IS NOT IN THE BASIN
!       AND THE ROUTING SEQUENCE IS SKIPPED

        if(slope(n).gt.0.0)then

!         REV. 7.2 Sept. 19/94 - ADDED IREACH() FOR DWOPER INPUT  

! * * * * * * * * * * LAKE or RESERVOIR ROUTING * * * * * * * * * * * * * * * * 

          if(ireach(n).gt.0.or.res(n).gt.0)then

!           WE ARE ROUTING EXTERNALLY WITH DWOPER OR DOING IT IN A 
!           CONTROLLED RESERVOIR DOWNSTREAM IF THERE IS ONE
!           THE FLOWS ARE ACCUMULATED IN A REACH-BIN FOR DWOPER 
!           SEVERAL ELEMENTS CAN CONTRIBUTE BUT NONE IS ROUTED TO 
!           DOWNSTREAM IN ed :

!           ADD UPSTREAM CONTRIBUTIONS AND LOCAL INFLOW
              
            lll=next(n)
            rbin=ireach(n)
 
            if(res(n).eq.0.)then

!             grid is part of a reservoir or lake
              qdwpr(rbin,jjz)=qdwpr(rbin,jjz)+qi2(n)+qr(n)

            else

              if(iopt.ge.3)write(53,6004)n,res(n),index

!             THERE IS A DAM IN THIS SQUARE AND THE 
!             RESERVOIR ROUTING SUBROUTINE REROUT IS CALLED.
!             * * * FOR RESERVOIR ROUTING:* * *      
!             REROUT IS THE RESERVOIR ROUTING SUBROUTINE       
!             IT'S ASSUMED THAT THE DAM IS LOCATED IN A REACH NUMBER

!     rev. 9.1.43  Jun.  01/03  - Fixed the qdwpr.txt function -

!             re: last grid in lake corrected this June 1/03
!             previously, the last grid was not added to the qdwpr.txt file
!             this next line copied from above

              if(rbin.gt.0)then
!               there may be a dam but the grid may not have been 
!               designated as part of a lake    

                qdwpr(rbin,jjz)=qdwpr(rbin,jjz)+qi2(n)+qr(n)
                qi2(n)=qdwpr(rbin,jjz)     

              endif
  
             if (iopt.eq.2) write(*,*) 'Calling rerout'
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!              if (iz.eq.1)write(*,*)"REROUT ", n, res(n)
              call rerout(n,div,thr,res(n),jz,at,dtmin,date,firstpass)
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             if (iopt.eq.2) write(*,*) 'Back from rerout'

!     rev. 9.1.67  Oct.  21/04  - NK; added unit 80 for lake_stor & lake_outflow
!             May   02/05  - TS: revised

              ln=res(n)

! D Durnford: these arrays are only remembering the value from each hour's last sub-timestep
              if(fhr.ge.1)then      ! added conditional Rev. 9.2.43  nk
                lake_stor(ln,fhr)=store2(n)
                lake_outflow(ln,fhr)=qo2(n)
!                net_lake_outflow(ln,jz)=qo2(n)
!                if(ln.gt.1)net_lake_outflow(ln,jz)=
!     *                      qo2(n)-lake_outflow(ln-1,jz)
                del_stor(ln,fhr)=(qi2(n)-qo2(n))*div
                !div added May 9/06 nk
              endif

            endif       ! res(n)

! * * * * * * * * * *  TS - WETLAND ROUTING  * * * * * * * * * * * * * * * * 

            ! csubich: This gives an out-of-bounds error if ntype <=1
            ! so fix by splitting up the statement
          elseif (ntype .gt. 1
     *    .and. aclass(n,ntype-1).gt.0.0
     *    .and.wetflg.eq.'y'
     *    .and.theta(ibn(n)).gt.0.00001
     *    .and.glacier_flag(n).ne.'y')then

            ! WETLAND + CHANNEL ROUTING GOES HERE

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
          else                     ! end of wetland routing

!           CHANNEL ROUTING:
!           ADD UPSTREAM CONTRIBUTIONS AND LOCAL INFLOW:
!           if(aclass(n,ntype-1).gt.0.0)
!     *     PAUSE 'channel routing for wetland class'

            qi2(n)=qi2(n)+qr(n)    !+qstream(n)-strloss(n)
!           qstream-strloss is included in qr
!           both from the runof\yyyymmdd_rff.r2c file 
!           and the calculation in sub

19          ii=ibn(n)
            lll=next(n)
            old=qo1(n)
            hold=1.0e+25_4
           
            do ic=1,maxic ! UP TO maxic ITERATIONS ARE ALLOWED
              if(abs(hold-store2(n)).gt.convthresh*hold)then
!               THIS ITERATES TO CONVTHRESH OR ALLOWS UP TO MAXIC ITERATIONS

                if(store2(n).lt.0.0)then
                  write(*,*) 'Stopping because store2 < 0.0'
                  write(*,*) 'Iteration ',ic, 'point x,y',
     *                       xxx(n), yyy(n)
                  stop
                else
                  over(n)=(store2(n)-cap(n))/rl(n)
                  if(over(n).le.0.0)then
!                   CHANNEL FLOW ONLY
                    ax=store2(n)/rl(n)

!     rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
!     general formulae for flow: Q = A**(5/3) * sqrt(S) / P**(2/3) / N 
!     with: A, P: wetted cross-section and perimeter; S slope, N manning coef.
!     slope below already square-root of true slope

                    qo2(n)=ax**1.67_4*slope(n)/
     *                (chawid(n)+2.0_4*ax/chawid(n))**0.667_4/r2n(n)
!                    qo2(n)=ice_fctr*qo2(n)
                  else
!                   CHANNEL + FLOOD PLAIN FLOW
!     rev. 9.2.43  Jun.  21/06  - NK: fixed spikes in route
!                    ax=cap(n)/rl(n)   ! added Jun 21/06 nk
!     rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
!                   use quadratic equation to solve for fp. depth
                    hwet2(n)=
     * (-1.0_4*chawid(n)+sqrt(chawid(n)*chawid(n)+4.0_4/ovD*over(n)))/
     *    (2.0_4/ovD)

!                    hcha2(n) is the bankfull depth here
                    hcha2(n)=chaxa(n)/chawid(n)
!                    xa is the total main channel cross section area
                    xa=(hwet2(n)+hcha2(n))*chawid(n)

                    if(over(n)-hwet2(n)*chawid(n).gt.0.0)then
                      qo2(n)=
     *           xa**1.67_4*slope(n)/(chawid(n)+2.0_4*hcha2(n))**0.667_4
     *                /r2n(n)+(over(n)-hwet2(n)*chawid(n))**1.33_4
     *                *slope(n)*ovbfact/r1n(n)
                    else
                      qo2(n)=
     *           xa**1.67_4*slope(n)/(chawid(n)+2.0_4*hcha2(n))**0.667_4
     *                /r2n(n)
                    endif
!                      qo2(n)=ice_fctr*qo2(n)
                  endif
                  wt=max(minwt,real(ic,4)/real(maxic+1,4))
                  qo2(n)=(1.0_4-wt)*qo2(n)+wt*old
                  old=qo2(n)
                  if (nocrashflg == 'y') then
                    if (qo2(n) .lt. 0.0) then   ! Reset qo2 to zero to prevent model crashes; does not conserve water
                      write(*,'(a26,G11.4,a35,2(i6))')
     1                     'ERROR: RESETTING QO2 FROM ', qo2(n),
     2                     ' M3/S TO 0.0 M3/S AT POINT AT X,Y: ',
     3                     xxx(n), yyy(n)
                      qo2(n) = 0.0
                    endif
                  endif
                endif
                hold=store2(n)
                store2(n)=store1(n)+(qi1(n)+qi2(n)
     *                   -qo1(n)-qo2(n))*div
 
                if (store2(n) .lt. 0.0) then
                  write(*,'(a13,a29,2(i6))') 'WARNING: NON-',
     1                 'CONVERGENCE AT POINT AT X,Y: ',
     2                 xxx(n), yyy(n)

                  if (dtmin .le. mindtmin .and. nocrashflg == 'y') then
                    write(*,'(a26,G11.4,a17,G11.4,a23,2(i6))')
     1                   'ERROR: RESETTING QO1 FROM ', qo1(n),
     2                   ' M3/S TO .75*QO1=', qo1(n)*.75,
     3                   ' M3/S AT POINT AT X,Y: ', xxx(n), yyy(n)
                    qo1(n) = qo1(n) * .75_4        ! Reduce qo1 to prevent model crashes; does not conserve water
                    store2(n) = store1(n)        ! Reassign the initial value of store2
                    GO TO 19                     ! Restart the iteration loop
                  end if

                  exitstatus=-1
                  GO TO 18
                end if
                
              else
!               convergence to convthresh  ! DD: this is not guaranteed as having been achieved within the max # iterations
                GO TO 16
              endif
            end do  ! ic interation loop


!           Check on the actual convergence achieved. If it's not below the convergence threshold specified,
!           post a warning and repeat the time step with a smaller time step value
            conv = abs(hold-store2(n)) / hold
            if (conv .gt. convthresh) then
              write(*,*)
     *         'WARNING: convergence level achieved for store2: ',
     *         conv,'for grid cell: ',xxx(n),yyy(n)
               exitstatus=-1         ! Tell rte_sub.f that dtmin needs to be reduced
               GO TO 18              ! Go to the end of the subroutine
            end if
       
   16       if (store2(n).lt.0.0) then
              write(*,*) 'Out of the iteration loop with store2 < 0.0'
              stop
            endif

            if(qo2(n).lt.0.0) then
              write(*,*) 
     *          'ERROR: qo2<0.0: ',qo2(n),
     *          ' for grid cell: ',xxx(n),yyy(n)
              stop
            endif

!           CALCULATE THE VELOCITY THROUGH EACH SQUARE
!           CALCULATE TRAVEL TIME FOR MAXIMUM VELOCITY.

            at=store2(n)/qo2(n)

!           SELECT MIN TRAVEL TIME FOR THE TIME STEP CALC

!           CALCULATE THE CHANNEL STATE FOR GRAPHICAL OUTPUT:

            i=yyy(n)
            j=xxx(n)
!           csubich -- segfault here when bnkfill is 0,
!           so cap bnkfll at a tiny value away from 0
            bnkfll(n) = max(bnkfll(n),1.0e-08_4)
            atemp=qo2(n)/(0.4_4*bnkfll(n))+1.0_4

!           TO PREVENT INTEGER UNDERFLOW OR OVEFLOW:  

            atemp=max(atemp,1.0_4)
            atemp=min(atemp,99.0_4)
            istate(i,j)=int(atemp,4)

            if(iopt.ge.4)write(53,1002)
     *                   i,j,n,istate(i,j),bnkfll(n),qo2(n)

          endif                         ! END OF CHANNEL ROUTING

!     rev. 9.1.10  Jan.  29/02  - flow nudging added for nopt(l)=2
          flowsta=' '

!         write(6,*) '***** S/R route: no', no

! D. Deacu: Inserted 'hour_offset' to select the discharges  
!         corresponding to the current day and hour 
! D. Durnford: permit a run start time other than 00 UTC, and a first date/hour in the streamflow file other than 01/01
          hour_offset = 24_4*(day1-strday1) + (hour1-strhour1)
          hour_offset_div = 24_4*(day1-divday1) + (hour1-divhour1)

          qo2sim(n) = 0.0
          qo2rem(n) = 0.0

          do l=1,no
!           check to see if this grid has a (valid) observed flow data 
            if(iflowgrid(l).eq.n.and.nopt(l).eq.0)then
!             we are at a flow station that is to be used for nudging
!             also,nudge only if there is observed flow 
              if(fhr.ge.kt .and. qhyd(l,hour_offset+fhr).ge.0.0) then
                flowsta='y'
                lsta=l
              endif
            endif
!           always keep track of the simulated streamflow value to compare obs. and sim.
!           values even if there is flow insertion
            if(iflowgrid(l).eq.n)then
            	qo2sim(n) = qo2(n)
            endif
          end do

!         keep track of simulated flow value in case of pixels with diversions
          repirr=0
	  do l=1,nodiv
            if (val2divyes.eq.1) then
    	      if (divname(l)(1:5).eq.'irrig'
     *        .or. divname(l)(1:5).eq.'Irrig') then
                repirr=repirr+1
                do rr=1,maxirrigpts
                  repn = irrigindx(repirr,rr)
                  if (repn.eq.n) then
                    qo2sim(repn)=qo2(repn)
                  endif
                enddo 
              else
	        if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                  qo2sim(n) = qo2(n)
                endif
              endif
            else
              if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                qo2sim(n)=qo2(n)
              endif
            endif
	  enddo

          if (flowsta.eq.'y' .and. 
     *    trim(strfw_option)=='streamflow_insertion') then  
!           we are in a grid cell with an observed flow 
!           used instead of the computed flow 
            qo2(n) = qhyd(lsta,hour_offset+fhr)
          endif

          repirr=0
          do l=1,nodiv
            
            if (val2divyes.eq.1) then
!	    diverting flow using the diversion type indicator: val2div (new way)

              if (divname(l)(1:5).eq.'irrig'
     *        .or. divname(l)(1:5).eq.'Irrig') then
              ! diversion station is for irrigation

                repirr=repirr+1
                do rr=1,maxirrigpts
                    repn = irrigindx(repirr,rr)
                    if (repn.eq.n) then
                      if (ireach(repn).gt.0) then
!                     remove everything from reservoir for this point
                        do rr2=1,noresv
                          repn2 = resindex(rr2)
                          if (res(repn2).eq.ireach(repn)) then
                            store2(repn2)=store2(repn2)-
     *                    qdivirrig(repirr,hour_offset_div+fhr)*div*2._4
                            if(iz.eq.no_dt) then
                              qo2remirr(repn2) = qo2remirr(repn2) +
     *                        min(qdivirrig(repirr,hour_offset_div+fhr),
     *                        (store2(repn2) +
     *                        qdivirrig(repirr,hour_offset_div+fhr)*
     *                        div*2._4) / div/2._4)
                            endif
                            store2(repn2)=max(store2(repn2),0.0_4)
                          endif
                        enddo  
                      else
!                     not in a reach: remove fraction from streamflow for this point
                        qo2(repn) = qo2(repn) - (1._4-val1div(l))*
     *                  qdivirrig(repirr,hour_offset_div+fhr)
                        if(iz.eq.no_dt) then
                          qo2remirr(repn) = qo2remirr(repn) +
     *                    min(qdivirrig(repirr,hour_offset_div+fhr)
     *                    * (1._4-val1div(l)),
     *                    qo2(repn) + (1._4-val1div(l))*
     *                    qdivirrig(repirr,hour_offset_div+fhr))
                        endif
                        qo2(repn) = max(qo2(repn),0.001_4)
                      endif
!                     end check if in a reach
                    endif
!                   end check if irrigation point is in grid cell
                enddo
!               end do loop on irrigation points

	      else
!             this is a conventionnal diversion (i.e.,not an irrigation one)

               if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
!              We are in the grid point where the diversion (start or end point) occurs
                if(ireach(n).gt.0.or.res(n).gt.0.)then
!		the diversion occurs in a reservoir

	          if(res(n).eq.0.or.res(n).ne.ireach(n))then
        	    write(*,*) 'Diversion is not at the reach outlet.'
                    write(*,*) 'Diversion number and name (div.tb0): ', 
     *                          l,divname(l)
                    write(*,*) 'Reservoir number and i,j location ',
     *                         '(REL.tb0): ', res(n), xxx(n), yyy(n)
                    write(*,*) 'Reach number (shed file): ', ireach(n)
                    stop
                  else
                    if(val2div(l).eq.1.and.divstrindex(l).eq.n.or.
     *              val2div(l).eq.2.and.divstrindex(l).eq.n)then
!                   source point considered: removing water

	              if (qdiv(l,hour_offset_div+fhr).ge.0.0) then
!		      diverted flow read from diversion file
		        qo2remtemp = qdiv(l,hour_offset_div+fhr)
     *                  * val1div(l)
		      else
!		      diverted flow taken from grid point outflow
		        qo2remtemp = qo2(n) * val1div(l)
	              endif
	              store2(n) = store2(n) - qo2remtemp * div * 2._4

	              if (store2(n).lt.0.0) then
	              qo2rem(n) = (store2(n)+qo2remtemp*div*2._4)/div/2._4
!		        not impossible that store2 was -ve initially, so cap qo2rem
			qo2rem(n) = max(qo2rem(n),0.0_4)
!			reset store2 to initial value minus qo2rem
			store2(n) = store2(n)+qo2remtemp*div*2._4-
     *                  qo2rem(n)*div*2._4
                      else
                        qo2rem(n) = qo2remtemp
	              end if

		    elseif ( val2div(l).eq.1.and.divendindex(l).eq.n.or
     *              .val2div(l).eq.3.and.divendindex(l).eq.n ) then
!		    end point considered : adding water

	              if (val2div(l).eq.3) then
!		      no worry about emptying source point, but still need to define qo2remtemp 
			qo2remtemp = qdiv(l,hour_offset_div+fhr)*val1div(l)
		      else
		        qo2remtemp = qo2rem(divstrindex(l))											    
		      endif
		      store2(n) = store2(n)+qo2remtemp*div*2._4

                    end if
!                   end check if removing or adding water
		  endif
!                 end check if in the reservoir of the reach

	        else
!		we are not in a reach and diverted flow is removed from / added to stream

		  if ( val2div(l).eq.1.and.divstrindex(l).eq.n.or.
     *            val2div(l).eq.2.and.divstrindex(l).eq.n ) then
!                 source point considered: removing water
			
		      if (qdiv(l,hour_offset_div+fhr).ge.0.0) then
!                     diverted flow read from diversion file
                        qo2remtemp = qdiv(l,hour_offset_div+fhr)
     *                  *val1div(l)
                      else
!                     diverted flow taken from grid point outflow
                        qo2remtemp = qo2(n) * val1div(l)
                      endif
		      qo2(n) = qo2(n) - qo2remtemp
              	      if (qo2(n).lt.0.) then
                        qo2rem(n) = qo2(n)+qo2remtemp-0.001_4
                	qo2(n) = max(0.001_4,qo2(n)-qo2rem(n))
              	      else
                	qo2rem(n) = qo2remtemp
                      end if

            	  elseif ( val2div(l).eq.1.and.divendindex(l).eq.n.or
     *            .val2div(l).eq.3.and.divendindex(l).eq.n ) then
!		  end point considered : adding water

		      if (val2div(l).eq.3) then
!                     no worry about emptying source point, but still need to define qo2remtemp
		        qo2remtemp = qdiv(l,hour_offset_div+fhr)*val1div(l)
		      else
			qo2remtemp = qo2rem(divstrindex(l))
		      endif
		      qo2(n) = qo2(n) + qo2remtemp
			
		  endif
!                 end check if source or end point

		endif
!               end check if in a reservoir

               endif 
!              end check if in grid point of diversion

              endif
!             end check if station is for irrigation or not           

            else
!           procesing diversions the old way, i.e. no val2div value is used 

             if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
!            diversion station has an end in this grid point

		if(ireach(n).gt.0.or.res(n).gt.0.)then
!               the diversion occurs in a reservoir

                      if(res(n).eq.0.or.res(n).ne.ireach(n))then
                        PRINT*,'the diversion does occur in a reach but
     *                  not in its
     *                  reservoir; please verify diversion location for 
     *                  (reservoir, reach): '
                        PRINT*,res(n),ireach(n)
                        stop
                      else
                        if ( divstrindex(l).eq.n ) then
!                       source point considered: removing water
                            if (qdiv(l,hour_offset_div+fhr).ge.0.0) then
!                           diverted flow read from diversion file
                                qo2remtemp = qdiv(l,hour_offset_div+fhr)
                            else
!                           with the old way, -ve diversion flows are reset to 0
                                qo2remtemp = 0.0
                            endif
                            store2(n) = store2(n) - qo2remtemp*div*2._4
                            if (store2(n).lt.0.0) then
                             qo2rem(n) = (store2(n)+qo2remtemp*div*2._4) 
     *                        /div/2._4
!                             not impossible that store2 was -ve initially,so cap qo2rem
                              qo2rem(n) = max(qo2rem(n),0.0_4)
!                             reset store2 to initial value minus qo2rem
                              store2(n) = store2(n) +
     *                      qo2remtemp * div * 2._4 - qo2rem(n)*div*2._4
                            else
                              qo2rem(n) = qo2remtemp
                            end if
                        elseif ( divendindex(l).eq.n ) then
!                       end point considered : adding water
                            qo2remtemp = qo2rem(divstrindex(l))
                            store2(n) = store2(n) + qo2remtemp*div*2._4
                        end if
!                       end check if remove or add water
                      endif
!                     end check if in the reservoir of the reach
                      
                else
!               we are not in a reservoir and diverted flow is removed from / added to stream

                      if ( divstrindex(l).eq.n ) then
!                     source point considered: removing water
                        if (qdiv(l,hour_offset_div+fhr).ge.0.0) then
!                       diverted flow read from diversion file
                          qo2remtemp = qdiv(l,hour_offset_div+fhr)
                        else
!                       old way: -ve diverted flows reset to 0.0 (only if no +ve values at all are present in diversion file).
                          qo2remtemp = 0.0
                        endif
                        qo2(n) = qo2(n) - qo2remtemp
                        if (qo2(n).lt.0.0) then
                          qo2rem(n) = qo2(n) + qo2remtemp - 0.001_4
                          qo2(n) = max(0.001_4,qo2(n) - qo2rem(n))
                        else
                          qo2rem(n) = qo2remtemp
                        end if
                      elseif ( divendindex(l).eq.n ) then
!                     end point considered : adding water
                        qo2remtemp = qo2rem(divstrindex(l))
                        qo2(n) = qo2(n) + qo2remtemp
                      endif
!                     end check if source or end point

                endif
!	        end check if in a reservoir

             endif
!            end check if station is in grid point

            endif
!           end check if processing diversion with diversion type or the old way 

	  enddo
!         end do loop on list of diversion stations; diversions have been processed
!
! 
!         ADD FLOW TO DOWNSTREAM ELEMENT
!         but only if it is not a lake
!
          qi2(lll)=qi2(lll)+qo2(n)

          if(n.eq.naa.and.iopt.eq.3)
     *      write(53,6037)
     *        n,yyy(n),xxx(n),ic,at1,qr(n),qi1(n),qi2(n),
     *        qo1(n),qo2(n),qi2(lll),store1(n),store2(n),cap(n),lll


          if(iopt.ge.3)then
            at1=at/3600.0_4
            write(53,6037,iostat=ios) 
     *        n,yyy(n),xxx(n),ic,at1,qr(n),qi1(n),qi2(n),
     *        qo1(n),qo2(n),qi2(lll),store1(n),store2(n),cap(n),lll
!            if(ios.ne.0)then
!              print*,'n,lll,na/',n,lll,na
!              stop
!            endif
          endif

        endif  ! SLOPE IF

        if(iopt.eq.2.and.n.eq.naa)print*,'In route, passed 901'

        att(n)=at/3600._4

!       added Dec. 12/00 nk.

        if(iopt.ge.1.and.n.eq.nnprint)then
          if(fhr.le.1)write(53,5551)
          write(53,5550)id,time,at/3600,qi1(n),qi2(n),
     *             qo1(n),qo2(n),store1(n),store2(n)
5550      format(i5,f8.2,5f8.1,2e12.3)
5551      format('   id   time         at     qi1     qi2'
     *           '     qo1     qo2     store1     store2')
        endif


!       EG_MOD compute average flow over current hourly time step,
!       always using the simulated flow value.
 
        if (qo2sim(n).gt.0.) then
          avr_qo(n) = avr_qo(n) + (qo1(n) + qo2sim(n))*div
        else 
          avr_qo(n) = avr_qo(n) + (qo1(n) + qo2(n))*div
        endif
        
      end do       ! closes the routing loop: do n=1,naa starting near line 315

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11!!!!!!!!!!!!!!!

!     EG_MOD compute average flow when at the end of last hourly time-step
      if(iz.eq.no_dt) then
        do n=1,naa
          avr_qo(n)=avr_qo(n)/3600._4
        end do        
      end if 

!     Y. Shin : splrpnflg replacing old spl_csv_flag
!         with y or f set from event.evt_template 
!         y for yes, f for default to write spl-rpn files 
 
      if (((splrpnflg == 'y') .or. (splrpnflg == 'f'))
     *    .and. iz .eq. no_dt) then

        if(splrpnflg == 'y') then
          if (first_run) then
            write(unit=54,fmt='(A)') 
     *        'Observed and simulated streamflows (m^3/s)'
            write(unit=54,fmt='(A10,'//trim(nostr)//'(",",A8,","))')
     *        'Station,,,', (gage(l),l=1,no)
            write(unit=54,fmt='(A12,'//trim(nostr)
     *        //'(",",F8.3,","))') 
     *        'Longitude,,,',(xstr(l),l=1,no)
            write(unit=54,fmt='(A11,'//trim(nostr)
     *        //'(",",F8.3,","))')
     *        'Latitude,,,',(ystr(l),l=1,no)
            write(unit=54,fmt='(A14,'//trim(nostr)
     *        //'(",",I8,","))')
     *        'Xcoord_grid,,,',(jx(l),l=1,no)
            write(unit=54,fmt='(A14,'//trim(nostr)//'(",",I8,","))')
     *        'Ycoord_grid,,,',(iy(l),l=1,no)
            write(unit=54,fmt='(A)') 
     *        'YEAR,MONTH,DAY,HOUR'//repeat(',OBS,SIM',no)
            first_run = .false.
          end if

          write(fmtfile,'("(4(i5,",a3,"),F15.4,",i4"(",a3,",F15.4))")')
     *        "','", 2*no, "','"
          write(unit=54,fmt=fmtfile) 
     *      year1, month_now, day_now, hour_now,
     *      (qhyd(l,hour_offset+fhr),avr_qo(iflowgrid(l)),l=1,no)

      else !splrpnflg
        if(splrpnflg == 'f') then
          if (first_run) then
            write(unit=54,fmt='(A)') 
     *        'Observed and simulated streamflows (m^3/s)'
            write(unit=54,fmt='A7,18X,'//trim(nostr)//'(A16)')
     *        'Station', (gage(l),l=1,no)
            write(unit=54,fmt='A9,11X,'//trim(nostr)//'(8X,F8.3)')
     *        'Longitude',(xstr(l),l=1,no)
            write(unit=54,fmt='A11,'//trim(nostr)//'(8X,F8.3)')
     *        'Latitude',(ystr(l),l=1,no)
            write(unit=54,fmt='A11,'//trim(nostr)//'(8X,F8.3)')
     *        'Xcoord_grid',(jx(l),l=1,no)
            write(unit=54,fmt='A11,9X,'//trim(nostr)//'(8X,I8)')
     *        'Ycoord_grid',(iy(l),l=1,no)
            write(unit=54,fmt='(A)') 
     *        'YEAR MONTH  DAY HOUR'//repeat(',OBS,SIM',no)
            first_run = .false.
          end if
        end if

        write(unit=54,fmt='I4,I6,2(I5),'//trim(nostr)//'(3F15.1)') 
     *      year1, month_now, day_now, hour_now,
     *      (qhyd(l,hour_offset+fhr),avr_qo(iflowgrid(l)),l=1,no)
        end if
      end if

      if(iopt.eq.2)print*, 'In route after the 1-naa loop'

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      if(iopt.ge.2) write(53,6006)dtmin
      if(iopt.ge.3) write(52,1001)jz

      if(iopt.eq.2)print*, ' finished writing istate'

!     INFORMATION FOR WATBAL.FOR
      do n=1,naa
        netflow(n)=netflow(n)+store1(n)-store2(n)
      end do

      firstpass='n'

18    RETURN

! FORMATS

 1000 format(50i3)
 1001 format(3i5)
 1002 format(' i,j,n,istate,bnkfull,qo2/',4i5,2f10.3)
 6000 format(60i2)
 6001 format(' ',3i5,3f10.2)
 6002 format(' route:iz,jz/',2i5)
 6004 format(' gone to rerout - n,res(n),index/',3i5)
 6005 format(' n,res(n),jz,qo2(n)/',3i5,2f10.2)
 6006 format(' dtmin =',f10.2)
 6007 format(' ','reservoir locations wrt 1-naa')
 6037 format(4i5,6f9.3,f10.3,3f15.0,i5)
 6003 format('    n    i    j   ic     at       qr      qi1',
     *'     qi2      qo1      qo2  qi2[lll]   store1    store2    cap   
     *lll')
 8492 format(' ','initialize resvr flow & storage - in route'/
     *            'n,l,b1,b2,qda,store1')
 8493 format(' in route/res init:',2i8,4e12.3)
! 8493 format(' in route/res init:',2i5,4e12.3)
 9801 format(a80)
 9802 format(i5,10g12.3)
      END SUBROUTINE route


