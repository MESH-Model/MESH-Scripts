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

      SUBROUTINE sub(e1,smc5,scale,icase,smok,optlow,igrdshft,
     *               dtminusr,mindtmin,convthreshusr)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************
!
!     May 2010
!      - inserted call to read_flow_ef; (D. Deacu)
!      - inserted code to check the values read in for 'kt' and 'irdt';
!        'irdt' is set to 1 if kt=1; (D. Deacu)
!
!
      use area_watflood
      
C///////////////////////// 
C// Added by Dave
      USE EF_Module
C// End Dave addition
C/////////////////////////

      ! The fst-based IO is included in a module; this allows
      ! for token stubs to be included if building on a system
      ! without the appropriate libraries
      use fst_io

      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      DIMENSION     :: smc5(16)
      CHARACTER(14) :: date
      CHARACTER(3)  :: eofmark
      CHARACTER(1)  :: lineflg,smok
      CHARACTER(20) :: junk 
      character*999 flnname
      REAL(4)            :: optlow,time,tot1,qwert,conv,scale,
     *                 smc5,tj1,clock,t,thr,dtmin,dtmax,div,aintvl,
     *                 sintvl,tot2,e1,tdum,qtemp,diff2,sdlz,dlz,
     *                 wfo_spec_version_number,
     *                 sec_div,route_dt,hr_div
      INTEGER       :: rbin,inta,block,i,j,n,ii,zz,
     *                 iallcnt1,n1,ii1,jan,m,ios,iallocate,
     *                 nnch,l,juold,jj,lun,nhr,nhf,
     *                 icase,iz,jz,nchr,mz,ju,mon,
     *                 iwest,ieast,isouth,inorth,nocolumns,nno,k,
     *                 nu,igrdshft,minsnwflg,oldjan,flgevp22,
     *                 noresv1,nj,npick,n_trick,no_frames,frame_no,
     *                 rr1,rr2,hour_offset_div,repirr,repn
      CHARACTER(128):: qstr
      CHARACTER(10) :: ctime
      CHARACTER(8)  :: cday
      LOGICAL       :: exists,newpafflg
      INTEGER(kind=2) :: result1,ntest
      CHARACTER(10) :: coordsys
      REAL          :: a66,areasum,areaclass(99)
      REAL          :: ha,fpw,kdn,nratio
      REAL          :: dtminusr,dtminfrac,mindtmin,dtminprev
      REAL          :: convthreshusr,minrough,maxrough
      REAL(4)       :: qi2_strt(naa),qo2_strt(naa)
      REAL          :: tqi1,tqo1,tax,tqo2,tstore2,tstore1
      REAL(4)       :: timeprev
      INTEGER       :: indexi,maxindex,exitstatus,no_dtold,jzprev,nnaa
      INTEGER       :: month_prev,month_next
      INTEGER       :: year_last,month_last,day_last,hour_last

      DATA ntest/20492/qstr/'watflood.wfo'/nchr/18/
      DATA iallcnt1/0/

!     NOTE: FOR MONTHLY RUNS, DIMENSIONS CAN BE CHANGED FROM 
!           3,8784,500  TO  12,366,3000

!>>>>>>>>>>>>>  AB: STUFF FOR ENSIM
      INTEGER(4) :: wfo_yy,wfo_mm,wfo_dd,wfo_hh,wfo_mi,wfo_ss,
     *              wfo_ms
      INTEGER(4) :: wfo_seq

!     WFO IO FUNCTIONS
      INTEGER :: wfo_write_attribute_data
      INTEGER :: wfo_write_timestamp
!>>>>>>>>>>>>>

      real*4, dimension(:), allocatable :: reach_lvl
      real*4, dimension(:), allocatable :: RFFneg

      allocate(reach_lvl(Nreaches),reach_last(Nreaches),
     *         RFFneg(naa),stat=iAllocate)
      if (iAllocate.ne.0) STOP 

!      mindtmin = 30.0

c input/lake_levs/reach1_lvl.txt
      do i=1,Nreaches
        open(unit=500+i,file=infln(4+i),status='old')
        read(500+i,*) reach_lvl(i)
        reach_last(i)=reach_lvl(i)
      end do

      if(iopt.eq.2)print*,' In sub after definitions'

!     RESET SETS ALL INITIAL VARIABLES

!     CHECK FILES MODE  iopt=99
!     FOR IOPT=99 NL AND MHRD ARE SET TO KT AND THE PROGRAM WILL RUN
!     FOR ONE TIME STEP ONLY - THIS WILL CHECK AND ECHO ALL INPUT FILES
!     >> VERY HANDY FOR CHECKING DATA FILE PRIOR TO LONG RUNS

!     JAN=1 FIRST PASS - SET IN RESET()
!     JAN=2 SUBSEQUENT PASSES
!     JAN=3 LAST PASS  - SET BELOW

      jan=1
      m=1
      tot1=0.0
      fhr=0                  ! simulation's forecast hour

!     These values used to come from read_flow_ef 
      irdt=1     ! initial gues for routing time step in hours 
      kt=1       ! kt=data timestep in hours. 

      iswitchrerout = 0  ! so initialize reach_init in rerout only once

!     Section added to allow for lengthened routing time step for large grids
      if(irdt.gt.kt)then
        a6=float(irdt)*3600.0
        write(51,*)'Warning'
        write(51,*)'Min time step a6 changed to',a6
        write(51,*)
        write(*,*)'Warning'
        write(*,*)'Min time step a6 changed to',a6
        write(*,*)
        pause 'hit enter to continue - in sub @167'
      endif

      if(iallcnt1.eq.0)then
        iallcnt1=1
      endif   ! iallcnt1=0

      if(iopt.eq.2)print*,'In sub after allocations 1'

!     SINGLE RUN USING SOIL MOISTURE GRID IS THE DEFAULT.
!     NO SOIL MOISTURE OPTIMIZATION - THIS CAN BE CHANGED WITH 
!     SETTING ICASE=-1 IN THE PARAMETER FILE & SM GRID WILL BE IGNORED

!     SAVE THE ORIGINAL VALUE:
      flgevp22=flgevp2
      
      if(iopt.eq.2)print*,'In sub after allocations 3'

      do n=1,naa
        rechrg(n)=0.0
!        qstream & strloss need to be initialized for watroute
        qstream(n)=0.0
        strloss(n)=0.0
!        rh(n)=.50   ! moved to rdtemp 28/12/04 nk
        RFFneg(n) = 0.0 ! negative values of surface runoff outside of reaches
      end do
      
      juold=0

      if (.NOT.allocated(outarray)) then
        allocate(outarray(ycount,xcount),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *    'Error with allocation of ensim arrays in sub'
      end if      

!     Initialize all grids for write_r2c
      do i=1,ycount
        do j=1,xcount
          outarray(i,j)=0.0
        end do
      end do

      if(iopt.eq.2)print*,'In sub before writing headers'
      ! only write out these header/r2c files if fstflag isn't 'y'
      if (fstflg /= 'y') then

        author='watroute (rte)'    
        if(modelflg.eq.'i')author='watroute (rte -i)'
        if(modelflg.eq.'r')author='watroute (rte -r)'
        if(modelflg.eq.'l')author='watroute (rte -l)'

        name='Gridded Channel Flow'
        coordsys_temp=coordsys1
        zone_temp=zone1
        datum_temp=datum1
        xorigin_temp=xorigin
        yorigin_temp=yorigin
        xcount_temp=xcount
        ycount_temp=ycount
        xdelta_temp=xdelta
        ydelta_temp=ydelta
        attribute_name='discharge'
        attribute_units='mm' 
        attribute_type='flow'  
        if(modelflg.eq.'i')source_file_name='rff files'
        if(modelflg.eq.'r')source_file_name='rff,rch files'
        if(modelflg.eq.'l')source_file_name='rff,lkg files'
        no_frames=0
        frame_no=0

!       write the header          
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call write_r2c(56,56,0,1,0,1,1)   
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!       This file is for debugging - so leakage can be compared
!       to qlz from watflood as given in the lkage\yyyymmdd_lkg.r2c file
!       tracer 100 set 

        name='Gridded Leakage'
        attribute_name='Leakage'
        attribute_units='cms' 
        attribute_type='discharge'  
        if(modelflg.eq.'i')source_file_name='rff files'
        if(modelflg.eq.'r')source_file_name='rff,rch files'
        if(modelflg.eq.'l')source_file_name='rff,lkg files'
        no_frames=0
        frame_no=0
        fln(811)='gridded_lkg.r2c'

!       write the header          
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !call write_r2c(811,811,0,1,0,1,1)   
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        name='Gridded LZS'
        attribute_name='lzs'
        attribute_units='cm' 
        attribute_type='storage'  
        if(modelflg.eq.'i')source_file_name='rff files'
        if(modelflg.eq.'r')source_file_name='rff,rch files'
        if(modelflg.eq.'l')source_file_name='rff,lkg files'
        no_frames=0
        frame_no=0
        fln(812)='gridded_lzs.r2c'

!       write the header          
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !call write_r2c(812,812,0,1,0,1,1)   
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if (fstflg /= 'y')

      if(iopt.eq.2)print*,'In sub, passed location 201'

      no_frames=0

!     RESET TO THE ORIGINAL VALUE -  WILL BE CHANGED IF NO DATA
      flgevp2=flgevp22

      index=1
!     INDEX = 1 FOR FIRST PASS REROUT ONLY
!     INDEX = 2 FOR SUBSEQUENT PASSES
!     SET   = 1 FOR EACH NEW LINKED EVENT TO READ IN NEXT SET OF 
!               RESERVOIR RELEASES

      if(iopt.eq.2)print*,' In sub, passed location 2012'

!     check that a model type has been picked
      if(modelflg.eq.'i'.or.modelflg.eq.'r'
     *                  .or.modelflg.eq.'l')then
!       no problem - continue
      else      
        print*,'modelflg not set to i, r, or l'
        print*,'in the first event file please fix and retry'
        print*
        stop 'program aborted in sub @ 252'
      endif

      if(iopt.eq.2)print*,'In sub, passed location 202'

!     rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
      do n=1,naa
        !sl2(n)=sqrt(sl1(n)) ! csubich -- this is now set in read_shed_ef
                               ! at the same time as sl1
        if(a4.eq.0)a4=1.0
! * * * TS * * * 
!       CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLO
!       widep=a11
        ! Compute the channel cross-sectional area based on a rather
        ! complicated fitting formula.  aa2/3/4 are tunable parameters.
        if(aa4(n).gt.0.0)then
          chaxa(n)=(aa2(n)+aa3(n)*da(n)**aa4(n))
        else
!         csubich -- da(n) should never be less than zero, but it can
!         happen if the rank listing is improperly configured
          if (da(n) .le. 0) then
            print *, "WARNING: da(n) is <= 0 at index",n
            print *, "Grid cell:", xxx(n), yyy(n)
            chaxa(n) = 0
            if (xxx(n) .eq. 0 .and. yyy(n) .eq. 0) then
              ! If xxx/yyy are both 0 for this cell,
              ! then we have a missing index.  In theory,
              ! this cell shouldn't affect the rest of the
              ! computation, so all we really want is for
              ! the remaidner of this procedure to not
              ! die with a floating point exception
              widep(n) = 1
              chadep(n) = 1
            end if
          else
!         rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
!         EXCEL compatible equation. aa4 must be -ve in the par file
            chaxa(n)=10.0**(aa2(n)*alog10(da(n))+aa3(n))
!         had to put a lower bound on channel area to avoid NaN in resume file
!         NK  Oct. 5/05
          end if
          chaxa(n)=amax1(1.0,chaxa(n))
        endif
        ! Channel capacity is the cross-sectional area times channel length
        cap(n)=chaxa(n)*rl(n)

        ! Since a channel has a deep part plus a shallow, sloping bank,
        ! compute an effective channel depth
        chadep(n)=SQRT(chaxa(n)/widep(n))
        chawid(n)=chaxa(n)/chadep(n)
        flz2(n)=1.0-(1.0-flz(n))

        ! Fix suggested by Frank Seglenieks, based on changes made
        ! to WATFLOOD ca. 2007: if we keep track of biome types
        ! and a grid cell has more water fraction than channel
        ! area, then the channel area calculation must have been
        ! incorrect -- replace the computed channel area with
        ! water_fracion * grid size, and then from that recompute
        ! capacity.

        chaarea(n) = chawid(n)*rl(n) ! Define channel area

        ! Now, check to see if that's sensible, based on land-use

        ! aclass(:,ntype) is the fraction of the grid cell that is water;
        ! this is only enforced at read-in of the shed/par files, and
        ! needs to be properly maintained.
        if (ntype .ge. 1 .and. aclass(n,ntype) 
     *           .gt. chaarea(n)/grid_area(n)) then 
          ! Replace the areas with ones based on the land-use data
          chaarea(n) = grid_area(n)*aclass(n,ntype)
          chawid(n) = chaarea(n)/rl(n) ! New width using the same effective depth
          cap(n) = chaarea(n)*chadep(n)

          ! Leave chaxa untouched for now, this may be a mistake.
          ! csubich -- experimental: update chaxa appropriately also
          chaxa(n) = cap(n)/rl(n) ! Capacity divided by length
        endif

      end do ! do n=1,naa

      if(iopt.eq.2)print*,'In sub, passed location 207'

!     RESET THE CLOCK:   >>>>>>>>>>>> CHECK THIS OUT
      m=1
      tot1=0.0

      if(iopt.eq.2)print*,'In sub, passed location 209'

      if(iopt.eq.2)print*,'In sub, gone to flowinit'

c          call flowinit()
!          replaced  Oct. 9/06  nk
!          initialize channel flows & storages
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (len(infln(2)) .gt. 2) then
        fln(99)=infln(2)
      else
        fln(99)='flow_init.r2c'
      end if
      if (fstflg .eq. 'y') then
        ! Read flow_init from the .fst file, using today
        !flnname(1:13)='flow_init.fst'
        call read_flowinit_fst(611,fln(99),!flnname,
     *                         year1,month1,day1,hour1)
c        call read_flowinit_fst(611,'flow_init.fst',!flnname,
c     *                         year1,month1,day1,hour1)
      else
        call read_flowinit_ef()
      endif
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      if(iopt.eq.2)print*,'In sub, back from read_flowinit_ef()'

!     rev. 9.2.07  Jul.  29/05  - NK: soilinit moved from runoff to sub 

      tdum=1000.*step2/3600.   !cms/mm conversion
      !mm/hr * tdum = mm/hr*(m/1000mm * 1000m/km * 1000m/km)*(km*km*hr/s) = cms

!     what class is the water class?
      ii_water=ntype

!     END OF INITIALIZATION SECTION
 
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *

      DO id=1,ni

!     ***************************************************************

      print*,'*********************************************************'
      print*,'*                                                       *'
      print*,'*           RRRRRRR   TTTTTTTT  EEEEEEE                 *'
      print*,'*           RRRRRRRR  TTTTTTTT  EEEEEEE                 *'
      print*,'*           RR    RR     TT     EE                      *'
      print*,'*           RR    RR     TT     EE                      *'
      print*,'*           RRRRRRRR     TT     EEEE                    *'
      print*,'*           RRRRRRR      TT     EEEE                    *'
      print*,'*           RR   RR      TT     EE                      *'
      print*,'*           RR    RR     TT     EEEEEEE                 *'
      print*,'*           RR     RR    TT     EEEEEEE                 *'
      print*,'*                                                       *'
      print*,'*                  WATFLOOD (TM)                        *'
      print*,'*           Version BETA    July, 2007                  *'
      print*,'*           (c) N. Kouwen, 1972-2007                    *'
      print*,'*                                                       *'
      print*,'*********************************************************'
      print*

        if(iopt.eq.2)print*,'id=',id

!       READ THE EVENT FILE - HAS TO BE DONE FOR EACH ID

        if(id.gt.1)then
          fln(99)=fln(100+id)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call rdevt(date,conv,scale,smc5,nhr,nhf)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        endif


        call getarg(1,strfw_option)

! Y. Shin  
!       read_flow_ef() is executed even when strfw_option is none 
        if(iopt.eq.2)print*,'In sub - gone to read_flow_ef'
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call read_flow_ef()  !EnSim compatible tb0 file
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!       'read_flow_ef' may reset kt and irdt;
!       temporary solution: set irdt=kt (D. Deacu)
!
        if (kt.eq.1) then
          if(irdt.ne.kt) irdt=1
        else
          print *, 'kt:', kt, 'irdt:', irdt
          stop 'Program aborted in sub after the call to read_flow_ef'
        endif
        if (trim(strfw_option)=='streamflow_insertion') then
          print *,'This is a run with streamflow insertion.'
        elseif (trim(strfw_option)=='streamflow_comparison') then
          print *,'This is a run without streamflow insertion.'
          print *,'  However, observed streamflows are read in and'
          print *,'  and then written out alongside flows simulated '
          print *,'  at the stations for comparison purposes.'
        else
          print *,'This is a run without any strfw_option.'
          print *,'  However, observed streamflows, which are set'
          print *,'  to -1, are read in and then written out alongside'
          print *,'  flows simulated at the stations for future use.'
        endif

        if(iopt.eq.2)print*,'In sub - gone to read_resv_ef'

!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call read_resv_ef()  !EnSim compatible tb0 file
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(iopt.eq.2)print*,'In sub - back from read_resv_ef'

        if(iopt.eq.2)print*,'In sub - gone to read_div_ef'

!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call read_div_ef()  !EnSim compatible tb0 file
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(iopt.eq.2)print*,'In sub - back from read_div_ef'

!       WATROUTE START  WATROUTE START  WATROUTE START  WATROUTE START

!       read the headers (open files also) :

        if(modelflg.ne.'n')then

!         read the header in the runoff file:
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (fstflg .eq. 'y') then
            call read_fst(261,fln(31),'1','RFF ',
     *                    year1,month_now,day_now,-1,-1,'            ')
          else 
            call read_r2c(261,31,'1')
          endif
          if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
            print*,'runoff grid size does not match the shed grid'
            print*,'WARNING: the runoff grid will be interpolated'
!            stop 'Program aborted in rte_sub @ 371'
          endif

          if(modelflg.eq.'r')then

!           read the header in the baseflow file:
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (fstflg .eq. 'y') then
              call read_fst(262,fln(32),'1','RCH ',
     *                      year1,month_now,day_now,-1,-1,
     *                      '            ')
            else 
              call read_r2c(262,32,'1')
            endif
            if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
              print*,'recharge grid size does not match the shed grid'
              print*,'WARNING: the recharge grid will be interpolated'
!              stop 'Program aborted in sub @ 379'
            endif

          elseif(modelflg.eq.'l')then
!           read the header in the leakage file:

!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            call read_r2c(263,33,'1')
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
              print*,'leakage grid size does not match the shed grid'
              print*
              stop 'Program aborted in sub @ 387'
            endif

          endif
        endif ! if(modelflg.ne.'n')then

!       WATROUTE END   WATROUTE END    WATROUTE END    WATROUTE END 

        if(iopt.eq.2)print*,' In sub, passed location  244'

!       TIMER SETS ALL THE CLOCKS i.e. SUM HOURS, SUM SECONDS, ETC.

        time=0.0
        m=1

        if(iopt.eq.99)then
!         THIS OPTION IS TO CHECK ALL INPUT FILES
!         see above for more
          mhtot=kt*2
        endif

!       Commented out by csubich
        !if(iopt.eq.2)pause 'before time loop'

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!           THIS IS THE MAIN TIME LOOP, EXECUTED FOR EACH TIME STEP
!           
!           START TIME LOOP
!           
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

        found_data_end=.false.

        DO WHILE(.NOT.found_data_end)

!         the -1 is because time starts at 0.0

          time=time+1.000
          fhr=fhr + 1  

          if(iopt.eq.2)print*,'Gone to timer'

!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call timer(iz,jz,mz,clock,time,t,thr,dtmin,dtmax,div,m,
     *               ju,a66)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! DD WE DON'T USE THE VALUE OF DTMIN ANYMORE WITH THE DYNAMIC TIME STEP: CALCULATED JUST ABOVE THE CALL TO route.f

          if(iopt.eq.2)print*,'Back from timer - time=',time

          if(iz.lt.jz)then 

!           STATUS LINE:
            if(iopt.lt.99)then
              if(mod(jz,120).eq.0)then
                write(*,5003)id,ni,jz
              endif
            endif

!       WATROUTE input  WATROUTE input  WATROUTE input  WATROUTE input

            if(iz.lt.jz)then

!             Establish the day's value of r1n if using temporally-varying fields
!             Read in and vectorize the input arrays:
!             read_fst uses the transpose of the array, from
!             the point of view of code built for read_r2c
              if (r1nflg .eq. 'y') then
                call read_fst(31,fln(1),'0','R1N ',0,0,0,-1,month_now,
     *                        'VARIABLE    ')
                do n=1,naa
                  i=yyy(n)
                  j=xxx(n)
                  r1n(n) = inarray(j,i)
                end do

!               Calculate the 31-day running average of r1n to avoid shocking
!               the system with abrupt changes between months if day_now isn't 15
                if (day_now .lt. 15) then                             ! 1st half of the month: consider the value of the previous month
                  if (month_now .eq. 1) then                          ! Roll the month back by one
                    month_prev = 12
                  else
                    month_prev = month_now - 1
                  end if
                  call read_fst(31,fln(1),'0','R1N ',0,0,0,-1,
     *                   month_prev,'VARIABLE    ')
                  do n=1,naa
                    i=yyy(n)
                    j=xxx(n)
                    r1n(n) = 1./31. * (inarray(j,i)*real(31-15-day_now)
     *                               + r1n(n)      *real(   15+day_now))
                  end do

                else if (day_now .gt. 15) then                        ! 2nd half of the month: consider the value of the following month
                  if (month_now .eq. 12) then                         ! Roll the month forwards by one
                    month_next = 1
                  else
                    month_next = month_now + 1
                  end if
                  call read_fst(31,fln(1),'0','R1N ',0,0,0,-1,
     *                   month_next,'VARIABLE    ')
                  do n=1,naa
                    i=yyy(n)
                    j=xxx(n)
                    r1n(n) = 1./31. * (inarray(j,i)*real(  -15+day_now)
     *                               + r1n(n)      *real(31+15-day_now))
                  end do
                end if

                if (fhr.eq.1 .or. hour_now.eq.1) then            ! Generate some statistics: 1st hour processed or start of new day
                  minrough = 1.0E9
                  maxrough = 0.0
                  do n=1,naa
                    if (r1n(n).gt.maxrough) maxrough = r1n(n)
                    if (r1n(n).lt.minrough) minrough = r1n(n)
                  end do
                  write(*,'(a15,a16,2f8.3,3i6)') 'min/max r1n on ',
     *              'year/month/day: ',minrough,maxrough,year1,
     *              month_now,day_now
                end if
              endif

!             Establish the day's value of r2n if using temporally-varying fields
!             Read in and vectorize the input arrays:
!             read_fst uses the transpose of the array, from
!             the point of view of code built for read_r2c
              if (r2nflg .eq. 'y') then
                call read_fst(31,fln(1),'0','R2N ',0,0,0,-1,month_now,
     *                        'VARIABLE    ')
                do n=1,naa
                  i=yyy(n)
                  j=xxx(n)
                  r2n(n) = inarray(j,i)
                end do

!               Calculate the 31-day running average of r2n to avoid shocking
!               the system with abrupt changes between months if day_now isn't 15
                if (day_now .lt. 15) then                             ! 1st half of the month: consider the value of the previous month
                  if (month_now .eq. 1) then                          ! Roll the month back by one
                    month_prev = 12
                  else
                    month_prev = month_now - 1
                  end if
                  call read_fst(31,fln(1),'0','R2N ',0,0,0,-1,
     *                   month_prev,'VARIABLE    ')
                  do n=1,naa
                    i=yyy(n)
                    j=xxx(n)
                    r2n(n) = 1./31. * (inarray(j,i)*real(31-15-day_now)
     *                               + r2n(n)      *real(   15+day_now))
                  end do

                else if (day_now .gt. 15) then                        ! 2nd half of the month: consider the value of the following month
                  if (month_now .eq. 12) then                         ! Roll the month forwards by one
                    month_next = 1
                  else
                    month_next = month_now + 1
                  end if
                  call read_fst(31,fln(1),'0','R2N ',0,0,0,-1,
     *                   month_next,'VARIABLE    ')
                  do n=1,naa
                    i=yyy(n)
                    j=xxx(n)
                    r2n(n) = 1./31. * (inarray(j,i)*real(  -15+day_now)
     *                               + r2n(n)      *real(31+15-day_now))
                  end do
                end if

                if (fhr.eq.1 .or. hour_now.eq.1) then            ! Generate some statistics: 1st hour processed or start of new day
                  minrough = 1.0E9
                  maxrough = 0.0
                  do n=1,naa
                    if (r2n(n).gt.maxrough) maxrough = r2n(n)
                    if (r2n(n).lt.minrough) minrough = r2n(n)
                  end do
                  write(*,'(a15,a16,2f8.3,3i6)') 'min/max r2n on ',
     *              'year/month/day: ',minrough,maxrough,year1,
     *              month_now,day_now
                end if
              endif

              if (rlakeflg .eq. 'y') then
                do n=1,naa
                  r2n(n) = r2n(n) * rlake(n)
                  r1n(n) = r1n(n) * rlake(n)
                end do
              endif
      
!             WATROUTE only <<<<<< !!!!!!
!             the headers have been read above.
!             surface flow is always routed
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              if (fstflg .eq. 'y') then
                call read_fst(261,fln(31),'0','RFF ',
     *                 year1,month_now,day_now,hour_now,-1,
     *                 '            ')

                if (found_data_end) then
                  exit
                endif
              else 
                call read_r2c(261,31,'0')
              endif
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!             vectorize & convert mm to flow
              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                ! read_fst uses the transpose of the array, from
                ! the point of view of code built for read_r2c
                ! When processing standard format files:
                ! Pass all values of surface runoff (RFF) within a reach and positive values elsewhere to qr
                ! Remember negative values of RFF outside of reaches and add to lzs
                if (fstflg .eq. 'y') then
                  if (ireach(n).le.0 .and. inarray(j,i).lt.0.0) then
                    qr(n)=0.0
                    RFFneg(n) = inarray(j,i)
                  else
                    qr(n)=inarray(j,i)*tdum*frac(n)
                    RFFneg(n) = 0.0
                  end if
                else 
                  qr(n)=inarray(i,j)*tdum*frac(n)
                endif
              end do

              if(modelflg.eq.'r')then
!               read the recharge and route through the lz
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (fstflg .eq. 'y') then
                  call read_fst(262,fln(32),'0','RCH ',
     *                 year1, month_now, day_now, hour_now,-1,
     *                 '            ')
                  if (found_data_end) then
                    exit
                  endif
                else
                  call read_r2c(262,32,'0')
                endif
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               vectorize & convert mm to flow
!               recharge is added to lzs (mm)
                do n=1,naa
                  qo2remirr(n) = 0.0
                  i=yyy(n)
                  j=xxx(n)
                  if (fstflg .eq. 'y') then
                    lzs(n)=lzs(n)+inarray(j,i)+RFFneg(n)
                  else
                    lzs(n)=lzs(n)+inarray(i,j)
                  endif

!                 route the recharge thru the lz:
!                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  call baseflow(n,dlz,sdlz,tdum)
!                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                 baseflow gives us qlz in flow units (cms), not in mm
!                 so there is no need to convert before adding to qr
                  qr(n)=qr(n)+qlz(n)
                end do

!               remove water from lzs in case of irrigation
                if(val2divyes .eq. 1) then
                  if(nodivirrig .gt. 0) then
                    hour_offset_div = 24*(day1-divday1)+(hour1-divhour1)
                    repirr=0
                    do rr1=1,nodiv
                      if (divname(rr1)(1:5).eq.'irrig'
     *                .or. divname(rr1)(1:5).eq.'Irrig') then
                        repirr=repirr+1                   
		        do rr2=1,maxirrigpts
                          repn=irrigindx(repirr,rr2)
                          if (ireach(repn).le.0) then
		            lzs(repn)=lzs(repn)-
     *                      qdivirrig(repirr,hour_offset_div+fhr)/
     *                      frac(repn)/tdum*val1div(rr1)
                            qo2remirr(repn) = qdivirrig(repirr,
     *                      hour_offset_div+fhr)* val1div(rr1)
                          endif
     		        enddo
                      endif
                    enddo
		  endif
                endif

              endif

!             note that if we are reading qlz directly, it will be
!             in mm, and will require conversion
              if(modelflg.eq.'l')then
!               read qlz = groundwater flow (leakage/baseflow)
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                call read_r2c(263,33,'0')
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               vectorize & convert mm to flow
                do n=1,naa
                  qo2remirr(n) = 0.0
                  i=yyy(n)
                  j=xxx(n)
                  qr(n)=qr(n)+inarray(i,j)*tdum*frac(n)
                end do

!               remove water from input flow in case of irrigation
                if(val2divyes .eq. 1) then
                  if(nodivirrig .gt. 0) then
                    hour_offset_div = 24*(day1-divday1)+(hour1-divhour1)
                    repirr=0
                    do rr1=1,nodiv
                      if (divname(rr1)(1:5).eq.'irrig'
     *                .or. divname(rr1)(1:5).eq.'Irrig') then
                        repirr=repirr+1
                        do rr2=1,maxirrigpts
                          repn = irrigindx(repirr,rr2)
                          if (ireach(repn).le.0) then
                            qr(repn) = qr(repn) -
     *                      qdivirrig(repirr,hour_offset_div+fhr)*
     *                      val1div(rr1)
                            qo2remirr(repn) = min(qdivirrig(repirr,
     *                      hour_offset_div+fhr)* val1div(rr1),
     *                      qdivirrig(repirr,hour_offset_div+fhr)
     *                      * val1div(rr1) + qr(repn))
                            qr(repn) = max(qr(repn),0.0)
                          endif
                        enddo
                      endif
                    enddo
                  endif
                endif

              endif

            endif ! if(iz.lt.jz)then : 2nd time 

            juold=ju
            jan=2

!           JAN HAS TO STAY HERE BECAUSE ROUTE MAY BE CALLED MORE
!           THAN FOR EACH TIME RUNOF5 IS CALLED - SO WE CAN'T USE 
!           JAN IN ROUTE.

          endif  ! if(iz.lt.jz)then : 1st time        ! CALL read_r2c NEW HOUR 

!         ROUTE ROUTES WATER THROUGH EACH SQUARE USING STORAGE ROUTING
!         RESET QDWPR=0 INCASE THIS IS SECOND DT DURING FLOW INCREMENT

!         >>>>>>>IS THIS LOOK OK?? CHANGED 6 TO NORESVO     
 
!         write reach inflow to a file instead of routing it 
!         qdwpr is for dwoper formats but can be changed to any other 
!         format say for river_1d

!         rev. 9.3.11  Feb.  17/07  - NK: force hourly time steps for runoff
!         rev. 9.3.12  Feb.  20/07  - NK: changed dtmin & call to route

!         Calculate a dynamic dtmin by doing a quick and dirty routing
!         This helps to ensure that the routing solutions inside the route function
!         are able to converge to a solution
!         We are assuming that the channels are not overflowing, so if it turns out that
!         there is overbank flow/storage, then route may have to iterate a bit longer to 
!         find the solution even if dtmin is relatively small

!         Remember the input values from the start of the time step
!         only store2_strt is a global variable and may need to be allocated
!         store2_strt is global because it may be overwritten in rerout.f 
!         based on reservoir area and level instead of channel storage from flow_init
          if(.NOT.allocated(store2_strt)) then
              allocate(store2_strt(naa))
          end if

          do n=1,naa
            qi2_strt(n) = qi2(n)
            qo2_strt(n) = qo2(n)
            store2_strt(n) = store2(n)

!           if flow insertion, use simulated instead of possibly inserted flow value at gauge location
            if (trim(strfw_option)=='streamflow_insertion') then
            do l=1,no
              if(iflowgrid(l).eq.n)then
                qo2_strt(n) = qo2sim(n)
              end if
            end do
            end if

!           if diversions, use simulated instead of possibly modified flow
	    repirr=0
            do l=1,nodiv
              if (val2divyes.eq.1) then
                if (divname(l)(1:5).eq.'irrig'
     *          .or. divname(l)(1:5).eq.'Irrig') then
                  repirr=repirr+1
                  do rr2=1,maxirrigpts
                    repn = irrigindx(repirr,rr2)
                    if (repn.eq.n) then
                      qo2_strt(repn)=qo2sim(repn)
                    endif
                  enddo
                else
                  if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                    qo2_strt(n) = qo2sim(n)
                  endif
                endif
              else
                if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                  qo2_strt(n) = qo2sim(n)
                endif
              endif
            enddo

          end do

!DD  Override the value calculated above by timer.f (fixed for all hours)

          dtmin = dtminusr  ! specified by the user and read in by rte.f
          dtminfrac = 0.75
          maxindex = 50   ! Permit more loops in this version of the routing loop since qo2 has no weighting applied
          do n=1,naa
            tqo1 = qo2(n)

!           if flow insertion, rely on simulated instead of possibly inserted flow value at gauge location to estimate required time-step                     
            if (trim(strfw_option)=='streamflow_insertion') then
            do l=1,no
              if(iflowgrid(l).eq.n)then
                tqo1 = qo2sim(n)
              end if
            end do
            end if

!           if diversions, rely on simulated instead of possibly modified flow to estimate time-step
	    repirr=0
            do l=1,nodiv
              if (val2divyes.eq.1) then
                if (divname(l)(1:5).eq.'irrig'
     *          .or. divname(l)(1:5).eq.'Irrig') then
                  repirr=repirr+1
                  do rr2=1,maxirrigpts
                    repn = irrigindx(repirr,rr2)
                    if (repn.eq.n) then
                      tqo1=qo2sim(repn)
                    endif
                  enddo
                else
                  if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                    tqo1 = qo2sim(n)
                  endif
                endif
              else
                if (divstrindex(l).eq.n.or.divendindex(l).eq.n) then
                  tqo1 = qo2sim(n)
                endif
              endif
            enddo

!      tqo1 = 0.0

            tqi1 = qi2(n)
15          indexi = 0
            if (dtmin .le. mindtmin) exit
            no_dt=max(int(3599./dtmin)+1,1)
            route_dt=3600.0/float(no_dt)
            sec_div=route_dt/2.0
            tax=store1(n)/rl(n)
!      tqo2=0.0
            tqo2=max(tax,0.0)**1.67*slope(n)/chawid(n)**0.667/r2n(n)
            !Use qi2 = 0.0 below to really constrain dtmin by keeping store2 low
            !We don't want to set qi2 to zero though because it is used in route
            !so we just use a hard-coded 0.0 in this equation
!16          tstore2=store1(n)
16          tstore2=store1(n)+(tqi1+0.0-tqo1-tqo2)*sec_div
            !Now check to see if this qo2 is large enough that it will cause problems
            !in the next time step when it is put into qo1. 
            !This has been known to happen when there is a sudden reduction in qi2 compared to qi1
!            tstore1=tstore2
            tstore1=tstore2+(-tqo2)*sec_div 
            !If qo2 is so large that it's emptying the grid cell's storage in one time
            !step, then we need to reduce the size of the time step to prevent that from
            !happening. This is analogous to meeting the CFL condition in advection solvers.
            !However, if store1 was very small, then small or even slightly negative store2 
            !might be a legitimate solution (i.e. the cell has actually dried up). So we'll let that go.
            !Note that we also need to reduce the time step if we anticipate that qo1 will be too large
            !in our next forecast/analysis time.
            if (tstore2 .lt. 0.0 .or. tstore1 .lt. 0.0) then
!      write(*,*) 'DD0', xxx(n), yyy(n)
              tqo2 = tqo2 / 2.0 ! Keep making qo2 smaller untill store2 becomes positive
              indexi = indexi + 1
              if (indexi .gt. maxindex) then
                dtmin = dtmin*dtminfrac !reduce the time step by a factor of dtminfrac (default=0.75, set above)
                GO TO 15
              end if
              GO TO 16          ! Redo the store2 calculation within the same iteration
!              write(*,*) "In cell ",n," need to reduce time step from ",
!     *          dtmin, " to ", dtmin*dtminfrac
            end if
          end do     

          ! Re-specifying dtmin as dtminusr cancels the effect of the code immediately above
          ! However, if the iteration loop in route is unstable, dtmin still decreases
!      write(*,*) 'DD1 dtmin calculated: ',dtmin
          dtmin = dtminusr  ! specified by the user and read in by rte.f

17        dtmin = max(mindtmin,dtmin) !Let the time step be as small as mindtmin
          no_dt=max(int(3599./dtmin)+1,1)
          dtmin = 3600.0/real(no_dt)
          route_dt=3600.0/float(no_dt)
          sec_div=route_dt/2.0
          hr_div=sec_div/3600.
          exitstatus = 0
          a6 = dtmin   ! Override the value declared above (fixed for all hours)
          a66 = dtmin  ! Override the value declared above (fixed for all hours)
          write(*,5004) fhr,dtmin,no_dt

!         The value of dtmin has been used to determine how many
!         times route is called. Route will determine a new dtmin
!         for the next time step.

!         EG_MOD prepare arrays for storing average flows

          if (.NOT.allocated(avr_qo)) allocate(avr_qo(naa))
          do nnaa=1,naa
            avr_qo(nnaa)=0.0
          end do

          do n=1,no_dt

!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            call route(sec_div,hr_div,dtmin,mindtmin,convthreshusr,jz,n,
     *                 time,date,exitstatus)
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if (exitstatus .ne. 0) then
              if (dtmin .le. mindtmin) then
                write(*,'(a15,a43,f6.1)') 'route yields a ',
     *            'negative store2 value at dtmin = mindtmin: ',
     *            mindtmin
                write(*,'(a25,a50)') 'It''s likely that qo1 is ',
     *            'so large that store2 is negative even with qo2=0.0'
                write(*,'(a29,a50,a50)')'If this run was started from ',
     *            'shed2flowinit utility, then try lowering the QI, ',
     *            'QO, and STORE ratios until this error is resolved'
                write(*,*) 'Else rerun with a smaller value of dtmin'
                stop
              else
                dtmin = dtmin*dtminfrac !reduce the time step by a factor of dtminfrac (default=0.75, set above)
                no_dtold = no_dt
                no_dt=max(int(3599./dtmin)+1,1)
                do while (no_dt .le. no_dtold) ! Reduce dtmin sufficiently to decrease no_dt
                  dtmin = dtmin*dtminfrac !reduce the time step by a factor of dtminfrac (default=0.75, set above)
                  no_dtold = no_dt
                  no_dt=max(int(3599./dtmin)+1,1)
                end do
                dtmin = 3600./real(no_dt)
                do nnaa=1,naa  ! Restore the input values from the start of the time step
                  qi2(nnaa) = qi2_strt(nnaa)
                  qo2(nnaa) = qo2_strt(nnaa)
                  store2(nnaa) = store2_strt(nnaa)                  
                end do
                GO TO 17
              end if
            end if


          end do  ! do n=1,no_dt

          if(iopt.eq.2)print*,'Back from route'

! Write lake water budget info to output file lake_sd.csv (unit 58)
!! Write lake water budget info to output file lake_sd.csv (unit 58) and net_lake_inflow.csv (unit 59)
!          if(resname(1).eq.'Superior')then
!           output - for GLAKE
            if(noresv.gt.0)then
              write(58,8001)(lake_elv(l,fhr),lake_stor(l,fhr),
     *                       lake_inflow(l,fhr),
     *                       lake_outflow(l,fhr),
     *                       del_stor(l,fhr),l=1,noresv)
            endif
!          endif

!         write to r2c file at the frequency specified by deltat_report_discharge in event.evt
!         take out conditional will write each time route is called
!         need to fix time stamp for frame header if this is done
!         rev. 9.4.01  Apr.  17/07  - NK: added deltat_report for gridflow.r2c

          if(mod(fhr,deltat_report_discharge).eq.0)then
            do i=1,ycount          ! Reinitialize the output array
              do j=1,xcount
                outarray(i,j)=0.0
              end do
            end do

            do n=1,naa             ! Fill the output array with the values to be written
              i=yyy(n)
              j=xxx(n)
              outarray(i,j)=avr_qo(n)
            end do     

!           trick to keep file open - it's closed after id loop below
            frame_no=frame_no+1
            no_frames=frame_no+1
            
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (fstflg .eq. 'y') then
              call write_fst(59,filename(59),'DISC',
     *                       year1,month_now,day_now,hour_now,0)
            else
              call write_r2c(56,56,no_frames,1,frame_no,1,8) 
            endif
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (fstflg .eq. 'y') then
              do i=1,ycount          ! Reinitialize the output array
                do j=1,xcount
                  outarray(i,j)=0.0
                end do
              end do

              do n=1,naa             ! Fill the output array with the values to be written
                i=yyy(n)
                j=xxx(n)
                outarray(i,j)=over(n)
              end do     
              call write_fst(59,filename(59),'OVER',
     *                       year1,month_now,day_now,hour_now,0)
            else
              call write_r2c(56,56,no_frames,1,frame_no,1,8) 
            endif
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if (rbmflg .eq. 'y') then
              call write_fst(60,filename(60),'DISC',
     *                       year1,month_now,day_now,hour_now,0)
              ! write_fst uses the outarray field up above
              ! now refill outarray with qi2
              do i=1,ycount          ! Reinitialize the output array
                do j=1,xcount
                  outarray(i,j)=0.0
                end do
              end do

              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                outarray(i,j)=qi2(n)
              end do     
              call write_fst(60,filename(60),'INFL',
     *                       year1,month_now,day_now,hour_now,0)

              ! now refill outarray with qdiff
              do i=1,ycount          ! Reinitialize the output array
                do j=1,xcount
                  outarray(i,j)=0.0
                end do
              end do

              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                outarray(i,j)=qo2(n)-qi2(n)
              end do     
              call write_fst(60,filename(60),'QDIF',
     *                       year1,month_now,day_now,hour_now,0)

              ! now refill outarray with chadep
              do i=1,ycount          ! Reinitialize the output array
                do j=1,xcount
                  outarray(i,j)=0.0
                end do
              end do

              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                outarray(i,j)=chadep(n)    
              end do     
              call write_fst(60,filename(60),'DEPT',
     *                       year1,month_now,day_now,hour_now,0)

              ! now refill outarray with chawid
              do i=1,ycount          ! Reinitialize the output array
                do j=1,xcount
                  outarray(i,j)=0.0
                end do
              end do

              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                outarray(i,j)=chawid(n)
              end do     
              call write_fst(60,filename(60),'WIDT',
     *                       year1,month_now,day_now,hour_now,0)

              ! now refill outarray with stream speed 
              do i=1,ycount          ! Reinitialize the output array
                do j=1,xcount
                  outarray(i,j)=0.0
                end do
              end do

              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                !take stream speed to be average flow (m3/s) divided by channel x-sec area (m2)
                outarray(i,j)=0.5*(qo2(n)+qi2(n))/(chadep(n)*chawid(n))
              end do     
              call write_fst(60,filename(60),'VELO',
     *                       year1,month_now,day_now,hour_now,0)
            endif
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          endif !if(mod(fhr,deltat_report_discharge).eq.0)

!     write flowinit to resume routing at the frequency specified by deltat_report_flowICs in event.evt
! D. Durnford moved the writing out of flow ICs and lake levels from below the end of the time loop: need them at least every 6 hours
! D. Durnford limiting the writing out of flow ICs and lake levels to every deltat_report_flowICs hours
          author='rte.exe'

          if(mod(fhr,deltat_report_flowICs).eq.0)then
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (fstflg .eq. 'y') then
              fln(99)=filename(61)
!              fln(99)=infln(2)
              call write_flowinit_fst(611,fln(99),
     *                       year1,month_now,day_now,hour_now)
            else
              call write_flowinit()
            endif
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
            do i=1,Nreaches
!!              write(58,8001)(lake_elv(l,fhr),lake_stor(l,fhr),
              write(500+i,'(f9.4)') lake_elv(i,fhr)
            end do

          endif !if(mod(fhr,deltat_report_flowICs).eq.0)

   82     m=m+1

! D. Durnford remember the last processed date/time in case need to output flow ICs after the time loop 
          year_last  = year1
          month_last = month_now
          day_last   = day_now
          hour_last  = hour_now
   
! DD
!      if (time .eq. 2) found_data_end=.TRUE.   ! Uncomment this line to run only, e.g., 2 time steps.  Useful when debugging.
        END DO    ! DO WHILE(.NOT.found_data_end) 


! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
!           END TIME LOOP
!
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

! D. Durnford output the flow ICs at the end of the run if they haven't already been output
        if(mod(real(fhr-1),real(deltat_report_flowICs)) .ne. 0) then
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (fstflg .eq. 'y') then
!            fln(99)=infln(2)
             fln(99)=filename(61)
            call write_flowinit_fst(611,fln(99),
     *                     year_last,month_last,day_last,hour_last)
          else
            call write_flowinit()
          endif
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
          do i=1,Nreaches
            if (reach_lvl(i) .le. 1.0) lake_elv(i,fhr-1) = 0.0
            write(500+i,'(f9.4)') lake_elv(i,fhr-1)
          end do

        endif !if(mod(fhr-1,real(deltat_report_flowICs)) .ne. 0)

        write(*,5003)id,ni,fhr

!       close files:
        print*
        close(unit=261,status='keep')
        write(*,'(a26,a)') 'Closed unit 261 filename=', trim(fln(31))
        if(modelflg.eq.'r')then
          close(unit=262,status='keep')
          write(*,'(a26,a)') 'Closed unit 262 filename=', trim(fln(32))
        elseif(modelflg.eq.'l')then
          close(unit=263,status='keep')
          write(*,'(a26,a)') 'Closed unit 263 filename=', trim(fln(33))
        endif

      END DO ! DO id=1,ni
 
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *

!      close(unit=56,status='keep')
!      print*
!      print*,'Closed unit 56'
!      print*,'Output file',fln(56)
!      print*,'written in the working directory'

!     EG_MOD moved close instruction initially located after RETURN
!     statement below inside of it
      do i=1,Nreaches
        close(unit=500+i)
      end do      

!     EG_MOD deallocate arrays
      deallocate(reach_lvl,reach_last,RFFneg)


      RETURN

! FORMATS

!      do i=1,Nreaches
!        close(unit=500+i)
!      end do

 5003 format('+',1x,'id=',i3,'/',i3,' mz=',i5)
 5004 format('Processing hour ',i5,' Calling ROUTE with ',
     *  'dtmin = ',f6.1,'s. Number of loops in this hour = ',i6)
 8001 format(g14.6,999(',',g14.6))
 8002 format(g14.6,999(',',g14.6))
 8003 format(g14.6,999(' ',g14.6))

      END SUBROUTINE sub

