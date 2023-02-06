C    This file is part of FLOWINIT.
C
C    FLOWINIT is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published 
C    by the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    FLOWINIT is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with FLOWINIT.  If not, see <http://www.gnu.org/licenses/>.

!***********************************************************************
!       copyright (c) by Nick Kouwen and Dave Watson 2007
!***********************************************************************

      SUBROUTINE read_flow_ef()

C*****************************************************************************
C  READ_FLOW_EF - written Aug/06 by Dave Watson, CHC
C	- Derived from rdflow written by Nick Kouwen
C	This subroutine reads streamflow hydrograph (STR) file 
C	(tb0 format)
C*****************************************************************************
C
C     May 2010 
C      - the arguments 'hdrflg' and 'date' have beed removed because they
C        were not used anywhere (D. Deacu)
C
C
C
      use area_watflood

C TB0 data module
      USE EF_Module

      implicit none
      TYPE(FlowParam) :: header
      TYPE(FlowColumnMetaData) :: colHeader

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      CHARACTER(1) :: firstpass,usefirstpass

      INTEGER :: l,i,j,ios,no_old,nl_max,iAllocate,ideallocate,
     *     linecount,dl
      LOGICAL exists

      data firstpass/'y'/

C Parameter type definitions
      integer*4 unitNum, flnNum, iStat

C Local variables
      character*128 keyword, value
      character*10000 line, subString
      integer lineLen, KeyLen, wordCount, ii, jj
      real    xstrpos
      logical rStat, lineType, foundEndHeader, insideColMetaData

C Set unit and fln number
      unitNum = 36
      flnNum = 6

C Open the file
      INQUIRE(FILE=trim(adjustl(fln(flnNum))),EXIST=exists)
      if(exists)then
         open(unit=unitNum,file=trim(adjustl(fln(flnNum))),
     *     status='old',iostat=ios)
         if(ios.ne.0)then
            print*,'Problems opening ',fln(flnNum)
            print*
            STOP ' Stopped in read_flow_ef'
         endif
      else
         print*,'ERROR: the recorded stream hydrographs (STR) file ',
     *        trim(fln(flnNum))
         print*,'is NOT found'
         STOP ' Program STOPPED in read_flow_ef'
      endif

C Initialize default values
      CALL InitFlowParam(header)	

C Search for and read tb0 file header
      linecount=0
      line(1:1) = '#'
      foundEndHeader = .false.
      insideColMetaData = .false.

      do WHILE((.NOT.foundEndHeader) .AND.
     &     ((line(1:1) .eq. '#') .OR.
     &     (line(1:1) .eq. ':') .OR.
     &     (LEN_TRIM(line) .eq. 0))) 	
         linecount=linecount+1

         read(UNIT=unitNum, FMT='((A))', iostat=ios) line ! read a line
         if(ios .eq. -1)then
            write(6,'((A))') 'ERROR: Premature EndOfFile encountered'
            STOP ' Stopped in read_flow_ef'
         end if

         rStat = Detab(line)    ! replace tabs with spaces
         line = ADJUSTL(line)   ! Get rid of leading white space
         lineLen = LEN_TRIM(line) ! Find the length excluding trailing spaces
         if(line(1:1) .eq. ':')then
            wordCount = SplitLine(line, keyword, subString) ! find the keyword
            rStat = ToLowerCase(keyword)
            KeyLen = LEN_TRIM(keyword)
            if(keyword(1:KeyLen) .eq. ':endheader')then
               foundEndHeader = .TRUE.
               
            else if(keyword(1:KeyLen) .eq. ':columnmetadata')then
               insideColMetaData = .TRUE.
            else if(keyword(1:KeyLen) .eq. ':endcolumnmetadata')then
               insideColMetaData = .FALSE.
            else if(insideColMetaData) then
               iStat = ParseFlowColumnMetaData(colHeader,keyword,
     &                                         KeyLen,subString)
               if(iStat .lt. 0) then
                  write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
                  write(*,'(2(A))') '   in line: ',line
                  STOP ' Stopped in read_flow_ef'
                  return
               endif
            else
               iStat = ParseFlowParam(header,keyword,KeyLen,subString)
               if (keyword(1:KeyLen) .eq. ':starttime') then
                  read(subString( 9:10),'(i2)') strday1
                  read(subString(12:13),'(i2)') strhour1
               end if
               if(iStat .lt. 0) then
                  write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
                  write(*,'(2(A))') '   in line: ',line
                  STOP ' Stopped in read_flow_ef'
                  return
               else if(iStat .eq. 0) then
               endif
            end if
         end if
      end do
C***************************************
C	Finished reading the header
C***************************************


C Assign the variables from the types
      kt =  header%tb0p%deltaT  !data time step in hours
      irdt =  header%routingDeltaT ! routing time step in hours
      flowUnitConv = header%tb0p%unitConv

!     FILL IN FLOW DATA flowfillflg='y'
!     this is when you have say 24 hr flow data but you want to plot hourly
      flowfillflg='n'
      if(GetBoolean(header%fillFlag)) flowfillflg = 'y' 


!     no    =     no of streamflow stations
      no = colHeader%tb0cmd%colCount

      print*,'no of flow stations found =',no

      write(nostr,'(i4)') no

!     nl    =     no of hours of streamflow data
C Scan lines of data
      rewind unitNum
c     nl = CountDataLinesAfterHeader(unitNum)

c      if(kt.eq.24)then
      nl = CountDataLinesAfterHeader(unitNum)*kt
c	else
c        nl = CountDataLinesAfterHeader(unitNum)*kt
c      endif

!     fixed  nk  Nov. 22/06

      print*,'no of hours of streamflow data =',nl


      rewind unitNum
      CALL GoToStartOfData(unitNum)

!       rev. 9.1.68  Dec.  19/04  - NK: rewrote rdflow c/w memory allocation 
!       allocate stuff      
      if(firstpass.eq.'y')then
         no_old=no
         nl_max=nl
         allocate(nnsum(no),ppsum(no),
     *        nxtbasin(no),statnerr(no),suberr(no),subarea(no),
     *        iflowgrid(no),qbar(no),iy(no),jx(no),ystr(no),xstr(no),
     *        frc(no,4),hydfctr(no),synfctr(no),area(no),
     *        gage(no),nlow(no),nopt(no),nopt1(no),
     *        qhyd(no,nl),qsyn(no,nl),
     *        qhyd_dly(no,nl/24),qsyn_dly(no,nl/24),
     *        qhyd_dly_sum(no),qsyn_dly_sum(no),
     *        qhyd_mly(no,max(1,nl/672)),qsyn_mly(no,max(1,nl/672)),
     *        qhyd_mly_sum(no),qsyn_mly_sum(no), stat=iAllocate)
         if(iAllocate.ne.0) STOP
     *        'Error with allocation of area5a arrays in sub'
!         TS - ALLOCATION OF AREANASHA ARRAYS
         allocate(aa(no),bb(no),cc(no),ashnum(no),ashden(no),
     *        rsquare(no),nq(no),nqc(no),qbarobs(no),stat=iAllocate)
         if(iAllocate.ne.0) STOP
     *        'Error with allocation of areanasha arrays in sub'
      else                      !  firstpass
!         check to see memory allocated is adequate      
         if(no.ne.no_old)then
            print*,'No of streamflow stations has been changed in'
            print*,'in file ',fln(6)
            print*,'This is not permitted'
            print*
            stop 'Program aborted in rdstr @ 172'
         endif
         if(nl.gt.nl_max)then
            nl_max=nl
!           the file length is longer than any of the previous events so 
!           more memory has to be allocated
!           DEALLOCATION OF ARRAYS FROM AREA10A:
            deallocate(qhyd,qsyn,qhyd_dly,qsyn_dly,
     *           qhyd_mly,qsyn_mly,stat=iDeallocate)
            if (iDeallocate.ne.0) then
               print*,'Error with deallocation of qhyd & qsyn'
               print*
               stop 'Program aborted in rdstr @ 180'
            endif
            allocate(qhyd(no,nl),qsyn(no,nl),
     *           qhyd_dly(no,nl/24),qsyn_dly(no,nl/24),
     *           qhyd_mly(no,max(1,nl/672)),
     *           qsyn_mly(no,max(1,nl/672)),stat=iAllocate)
            if(iAllocate.ne.0) STOP
     *           'Allocation Error: area5a arrays in read_flow_ef@185'
         endif
      endif                     !  firstpass
!       rev. 9.1.68  Dec.  19/04  - NK: rewrote rdflow c/w memory allocation 

!       ASSIGN THE GAUGE LOCATIONS NAMES AND FUNCTION COEFFICIENTS
      do l=1,no
         gage(l) = colHeader%tb0cmd%colName(l) ! streamflow station name
         xstr(l) = colHeader%tb0cmd%colLocX(l) ! x coordinate
         ystr(l) = colHeader%tb0cmd%colLocY(l) ! y coordinate
         frc(l,1) = colHeader%colCoeff1(l) ! coefficient 1
         frc(l,2) = colHeader%colCoeff2(l) ! coefficient 2
         frc(l,3) = colHeader%colCoeff3(l) ! coefficient 3
         frc(l,4) = colHeader%colCoeff4(l) ! coefficient 4
         nopt(l) = colHeader%colValue1(l) ! nopt
      end do
      deallocate(colHeader%tb0cmd%colName)
      deallocate(colHeader%tb0cmd%colLocX)
      deallocate(colHeader%tb0cmd%colLocY)
      deallocate(colHeader%colCoeff1)
      deallocate(colHeader%colCoeff2)
      deallocate(colHeader%colCoeff3)
      deallocate(colHeader%colCoeff4)
      deallocate(colHeader%colValue1)


!       rev. 9.2.21  Nov.  11/05  - NK: 
!       Set nopt in first event .str file 
!       This allows a pick of the stations for opt 
!       in the first event only
!       All nopt will be the same for all events.
!       Does not allow opt stations to change during the run. For that, each 
!       .str file has to be edited.

      usefirstpass='n'
      if(firstpass.eq.'y')then 
         do l=1,no
            nopt1(l)=nopt(l)
            if(nopt(l).eq.-1)usefirstpass='y' 
!	use first event nopt if even one value = -1
            nopt(l)=abs(nopt1(l))
         end do
      else
!       for subsequent events use the first event opt flags
         if(usefirstpass.eq.'y')then
            do l=1,no
               nopt(l)=abs(nopt1(l))
            end do
         endif
      endif



!     IF STEP < 1 km THEN A LOCAL COORDINATE GRID HAS TO BE USED
!     NOT THE UTM GRID
!     THIS WAS TAKEN OUT BY FRANK S FEB/99 TO ALLOW FOR GRIDS
!     DEFINED ON A LAT/LONG BASIS

!     FLOW STATION COORDINATES ARE NOW CONVERTED TO COMPUTATION
!     GRID COORDINATES

      if(id.eq.1)then
!     rev. 9.2.33  Feb.  14/06  - NK: /str stations from first event ONLY!!
         do l=1,no
!     convert iy and jx to local coordinates
!     Jul. 21/04  nk
           if (fstflg .eq. 'y') then                                    ! fst files
!           if (longrid(1).gt.0.0 .and. latgrid(1).gt.0.0) then
             xstrpos = xstr(l)
             if (xstrpos .lt. 0.0) xstrpos = 360.00 + xstrpos
             do ii=1,size(longrid)-1
               if   (xstrpos.ge.longrid(ii)
     *         .and. xstrpos.le.longrid(ii+1)) then
                 jx(l) = ii
                 exit
               end if
             end do
             do jj=1,size(latgrid)-1
               if   (ystr(l).ge.latgrid(jj)
     *         .and. ystr(l).le.latgrid(jj+1)) then
                 iy(l) = jj
                 exit
               end if
             end do
           else                                                         ! r2c files
            iy(l)=int((ystr(l)-yorigin)/ydelta)+1
            jx(l)=int((xstr(l)-xorigin)/xdelta)+1
           end if
         end do
         write(53,*)
         write(53,*)'Just after converting streamflow stations to '
         write(53,*)'grid coordinates'
         do l=1,no
            write(53,1778)id,l,iy(l),jx(l)
         end do
         write(53,*)
      endif

!       WATFLOOD COLUMN FORMAT
c		do j=1,nl,kt
      do j=kt,nl,kt             ! changed nk  sept. 29/06
         read(unitNum,*,iostat=ios)(qhyd(l,j),l=1,no)
         if(ios.ne.0)then
            print*, 'In strfw'
            print*,' NEW format flow file'
            print*,starttime
            print*,' problems reading the hydrograph at hour '
     *           ,j/kt
            print*,' and column',l,'  no=',no
            print*,'Weird hidden characters in the file will
     *           do this'
            print*,'last values read:'
            if(j.gt.2)then
               write(*,206)(qhyd(l,j-2),l=1,no)
            endif
            if(j.gt.1)then
               write(*,206)(qhyd(l,j-1),l=1,no)
            endif
            write(*,206)(qhyd(l,j),l=1,no)
            print*
            stop 'Program aborted in strfw @ 368'
         endif

!     rev. 9.1.10  Jan.  29/02  - flow nudging added for nopt(l)=2
!         fill in the blanks - this is needed for nudging
         if(kt.gt.1) then
            do i=j-kt+1,j-1
               do l=1,no
                  qhyd(l,i)=qhyd(l,j)
               end do
            end do
         endif
         
 9993    format(' error reading flows time step =',i3,/
     *    ' possible cause: station list does not match # specified'/
     *    ' or there may be tabs in the data')
      end do





!     apply the flow conversion factor e.g 0.02843 for cfs to cms
      if(flowunitconv.ne.0.0)then
         do j=kt,nl,kt
            do l=1,no
               qhyd(l,j)=qhyd(l,j)*flowunitconv
            end do
         end do
      endif


!	Convert stage hydrographs to flow hydrographs:

      do l=1,no
         if(frc(l,1).ne.0)then
            do j=kt,nl,kt
!	BUG: changed nl from mhtot  NK  Nov. 13/03
               if(qhyd(l,j).gt.0) then
                  if(qhyd(l,j).ge.frc(l,4))then
                     qhyd(l,j)=frc(l,3)
     *                    +frc(l,1)*(qhyd(l,j)-frc(l,4))**frc(l,2)
                  else
                     qhyd(l,j)=max(frc(l,3),0.0001)
                  endif
               endif
            end do
         endif
      end do

!     FILL IN STREAMFLOW DATA UNTIL END OF RAINGFAL DATA:
      do l=1,no 
         do j=mhrd+kt,nl,kt
            if(qhyd(l,j).lt.0.0)qhyd(l,j)=-1.0  
         end do  
      end do

!     FILL IN FLOW DATA
!     this is when you have say 24 hr flow data but you want to plot hourly
!     This can not be done if you want to extend the routing time step

      if(irdt.le.kt)then   
         if(flowfillflg.eq.'y'.or.flowfillflg.eq.'Y')then
            do l=1,no
               do j=nl,kt,-kt   ! 744,24,-24
                  do i=j,j-kt+1,-1 ! 744,721,-1
                     qhyd(l,i)=qhyd(l,j)
                  end do 
               end do
            end do
            kt=1
         endif
      else

!           do we need anything here???????????????????????????????????????????

      endif

!     write the converted hydrographs (stage to flow):
      if(iopt.gt.0.and.frc(1,1).ne.0)then
         write(53,5001)
         do j=kt,nl,kt
            write(53,202)(qhyd(l,j),l=1,no)
         end do
      endif


      do l=1,no
         qbar(l)=0.
!	QBAR(I,D,L)=MEAN FLOW DURING STORM PERIOD AT STA.=L,STORM=I
! csubich -- changed to nl from nhtot
         do j=kt,nl,kt
            qbar(l)=qbar(l)+qhyd(l,j)
         end do
         ! csubich -- rearranged equation to make sure there's no
         ! truncation on integer division
         qbar(l)=qbar(l)*kt/float(nl)
      end do

!     rev. 9.1.10  Jan.  29/02  - flow nudging added for nopt(l)=2
!     identify grid numbers with flow stations
      do l=1,no
         if(iy(l).le.0.or.jx(l).le.0)then
!         this can happen if the stream gauge is outside the waterhsed
!         as when subwaterhseds are modelled as separate watersheds
!         added Mar 14/04 nk.      
            iflowgrid(l)=0
         elseif(iy(l).gt.ycount.or.jx(l).gt.xcount)then
!         this can happen if the stream gauge is outside the waterhsed
!         as when subwaterhseds are modelled as separate watersheds
!         added Jul. 06/05 nk.          
            iflowgrid(l)=0
         else
            iflowgrid(l)=s(iy(l),jx(l))
         endif
      end do

!     rev. 9.1.68  Dec.  19/04  - NK: rewrote rdflow c/w memory allocation 
!     moved from sub
      close(unit=unitNum,status='keep',iostat=ios)
      if(ios.ne.0)then
         print*,'Problem closing unit 36 fln=',fln(6)
         print*
         stop ' program aborted in rdflow @ 623'
      endif


      firstpass='n'


      RETURN


! FORMATS

 201  format(256f10.0)
 202  format(256f10.3)
 203  format(' id=',i5,' l= ',i5,a12,2f12.3,4e10.3)
 205  format(2f12.3,a12,f12.3,4e10.3)
 206  format(256f10.3)
 1778 format(' id,l,iy(l),jx(l)',6i5)
 1801 format(14x,10(f5.0,a1),i6)
 1802 format(14x,11(f5.0,a1))
 5000 format(' echo recorded hydrographs:')
 5001 format(/' echo converted hydrographs:')
 5004 format(a20,a10)
 5005 format(a20,i5)
 6226 format(' error encountered opening unit=37 fln=',a999/)

      END SUBROUTINE read_flow_ef



