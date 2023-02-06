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

!***********************************************************************
!       copyright (c) by Nick Kouwen and Dave Watson 2007
!***********************************************************************

      SUBROUTINE read_div_ef()

C*****************************************************************************
C  READ_DIV_EF - written Jan/17 by Dorothy Durnford, ECCC
C	- Derived from read_div_ef written by Dave Watson
C	This subroutine reads the diversion data (div) file 
C	(tb0 format)
C*****************************************************************************

      use area_watflood

C TB0 data module
      USE EF_Module

      implicit none
      TYPE(DivParam) :: header
      TYPE(DivColumnMetaData) :: colHeader

      Integer  :: ios,j,k,i,n,l,ii,jj,istr,iend,itot
      integer  :: nodiv_firstpass,ndiv_max,
     *            iAllocate,iDeallocate
      real*4   :: factor,xdivpos

!     rev. 9.1.55  Jun.  12/04  - NK: write new str files to strfw\newfmt folder.
      LOGICAL exists
      CHARACTER(1)  :: firstpass

      data firstpass/'y'/

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

C Parameter type definitions
      integer*4 unitNum, flnNum, iStat

C Local variables
      character*1024 line, subString, tmpString
      character*128 keyword
      integer lineLen, KeyLen, wordCount
      logical rStat, lineType, foundEndHeader, insideColMetaData
      real*4,    dimension(:), allocatable :: xstrdiv,ystrdiv,
     *                                        xenddiv,yenddiv
      integer*4, dimension(:), allocatable :: istrdiv,jstrdiv,
     *  ienddiv,jenddiv,totirrigpts,iminirr,imaxirr,jminirr,jmaxirr
      integer*4  ndiv,irindex,irpt

C Set unit and fln number
      unitNum = 292
      flnNum = 42

!!!!!!!!!!!!!!!!!!!!!!!!

C Open the file
      INQUIRE(FILE=trim(adjustl(fln(flnNum))),EXIST=exists)
      if(exists)then
         open(unit=unitNum,file=trim(adjustl(fln(flnNum))),
     *     status='old',iostat=ios)
         if(ios.ne.0)then
            print*,'Problems opening ',fln(flnNum)
            print*
            STOP ' Stopped in read_div_ef'
         endif
         nodiv=1  ! assume there is at least one diversion if there is a file
      else
         if(numa.eq.0)then
            print*,'WARNING:'
            print*,'Diversion data (div) file NOT found: '
            print*,'Diversion data (div) file ',trim(fln(flnNum))
            print*,'Program continues with no diversions'
	    print*,'Not a problem if there are no diversions!'
            print*
         endif
         nodiv=0
         ndiv=0
      endif

      if(iopt.eq.2)print*,'in read_div_ef passed 95'

      if(nodiv.ge.1)then   ! The diversion file was found

C Initialize default values
         CALL InitDivParam(header)	

         if(iopt.eq.2)print*,'in read_div_ef passed 102'

C Search for and read tb0 file header
         line(1:1) = '#'
         foundEndHeader = .false.
         insideColMetaData = .false.
         val2divyes = 0  ! If the div.tb0 file contains Value2, this switches to 1. For backwards compatibility: the original version of the file contains Value1 only.

         do WHILE((.NOT.foundEndHeader) .AND.
     &        ((line(1:1) .eq. '#') .OR.
     &        (line(1:1) .eq. ':') .OR.
     &        (LEN_TRIM(line) .eq. 0))) 	

            read(UNIT=unitNum, FMT='((A))', iostat=ios) line ! read a line
            if(ios .eq. -1)then
               write(6,'((A))') 'ERROR: Premature EndOfFile encountered'
               STOP ' Stopped in read_div_ef'
            end if

            rStat = Detab(line) ! replace tabs with spaces
            line = ADJUSTL(line) ! Get rid of leading white space
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
                  iStat = ParseDivColumnMetaData(colHeader,keyword,
     &                 KeyLen,subString)
                  if(iStat .lt. 0) then
                     write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
                     write(*,'(2(A))') '   in line: ',line		
                     STOP ' Stopped in read_div_ef'
                     return
                  endif
                  if(keyword(1:KeyLen) .eq. ':value2')then
                     val2divyes = 1
                  endif
               else
                  iStat=ParseDivParam(header,keyword,KeyLen,subString)
                  if (keyword(1:KeyLen) .eq. ':starttime') then
                     read(subString( 9:10),'(i2)') divday1
                     read(subString(12:13),'(i2)') divhour1
                  endif
                  if(iStat .lt. 0) then
                     write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
                     write(*,'(2(A))') '   in line: ',line		
                     STOP ' Stopped in read_div_ef'
                     return
                  else if(iStat .eq. 0) then
C     write(*,'((A), (A))')  'Unrecognized keyword line: ',
C     & line
                  endif
               end if
            end if
         end do	

C***************************************
C	  Finished reading the header
C***************************************

         if(iopt.eq.2)print*,'in read_div_ef passed 172'

C Assign the variables from the types
         ktr =  header%tb0p%deltaT !data time step in hours
         factor = header%tb0p%unitConv ! conversion to cms
         nodiv = colHeader%tb0cmd%colCount !no of diversions
         if (nodiv .eq. 0) then
            write(*,*) 
     *     'No diversions found in the diversion data file'
            write(*,*) 
     *     'If intended, remove the diversion file name from event.evt'
            stop
         end if

!        ndiv    =     no of hours of data
C        Scan lines of data
         rewind unitNum

!        added *ktr     Nov. 27/06  nk
         ndiv = CountDataLinesAfterHeader(unitNum)*ktr

         rewind unitNum
         CALL GoToStartOfData(unitNum)

      endif  ! End of the loop if nodiv.ge.1

!!!!!!!!!!!!!!!!!!!!!!!!

!     PROCESS THE HEADER INFO RE THE DIVERSIONS

!     allocate stuff      
      if(firstpass.eq.'y')then
         firstpass='n'
         nodiv_firstpass=nodiv
!         nl comes from the .str file and is the # hours of the event
!         nl is NOT defined in watroute because .str files are not used
         ndiv_max=max0(ndiv,nl)
         if(ndiv_max.lt.1)ndiv_max=event_hours
!         but we need to provide enough memory to simulate a whole event
!         sometimes users specify the duration in the rel to be just 1 hr.
!         when a rule is given. However, we need memory of all the variables
!         If there is no flow or release data, the no of hours of rain
!            as given in the event file is used!!!!!!!!!1
         if(nodiv.gt.0)then
            allocate(val1div(nodiv),val2div(nodiv),
     *        val3div(nodiv),val4div(nodiv),
     *        ystrdiv(nodiv),xstrdiv(nodiv),
     *        istrdiv(nodiv),jstrdiv(nodiv),
     *        yenddiv(nodiv),xenddiv(nodiv),
     *        ienddiv(nodiv),jenddiv(nodiv),
     *        divstrindex(nodiv),divendindex(nodiv),
     *        divname(nodiv),qdiv(nodiv,ndiv_max),
     *        stat=iAllocate)
            if(iAllocate.ne.0) STOP
     *        'Error with allocation in read_div_ef @172'
         else
            allocate(val1div(1),val2div(1),
     *           val3div(1),val4div(1),
     *           ystrdiv(1),xstrdiv(1),istrdiv(1),jstrdiv(1),
     *           yenddiv(1),xenddiv(1),ienddiv(1),jenddiv(1),
     *           divstrindex(1),divendindex(1),divname(1),qdiv(1,ndiv),
     *           stat=iAllocate)
            if(iAllocate.ne.0) STOP
     *        'Error with allocation in read_div_ef @178'
         endif

         if(iopt.eq.2)print*,'In read_div_ef @ 236'

!        ASSIGN THE DIVERSION LOCATIONS NAMES AND FUNCTION COEFFICIENTS
!           this is done only during the first pass if coefficient values 
!           are set to -1 for subsequent events. This makes tweaking easy
!           as only the values in the first event need to be adjusted.
         do i=1,nodiv
            divname(i) = colHeader%tb0cmd%colName(i) ! diversion name
            xstrdiv(i) = colHeader%tb0cmd%colLocX(i) ! x coordinate at diversio nstart
            ystrdiv(i) = colHeader%tb0cmd%colLocY(i) ! y coordinate at diversion start
            xenddiv(i) = colHeader%colLocX1(i)       ! x coordinate at diversion end
            yenddiv(i) = colHeader%colLocY1(i)       ! y coordinate at diversion end
            if(val2divyes.eq.1) then                 ! The div.tb0 file contains Value2
	       if(colHeader%colValue1(i).gt.0.0.or.
     *            colHeader%colValue2(i).gt.0) then
                  val1div(i) = colHeader%colValue1(i) ! value 1
                  val2div(i) = colHeader%colValue2(i) ! value 2
                  val3div(i) = colHeader%colValue3(i) ! value 3
                  val4div(i) = colHeader%colValue4(i) ! value 4
               endif
            else
               if(colHeader%colValue1(i).gt.0.0) ! The div.tb0 file does not contain Value2
     *            val1div(i) = colHeader%colValue1(i) ! value 1 only
            endif
         end do

      else                      ! i.e., if not firstpass='y'
!        subsequent passes
!        check to see memory allocated is adequate      
         if(iopt.eq.2)print*,'In read_div_ef @ 263'
         if(nodiv.ne.nodiv_firstpass)then
            print*,'No of diversions has been changed in'
            print*,'in file ',trim(fln(42))
            print*,'This is not permitted'
            print*
            stop 'Program aborted in rddivo @ 269'
         endif
         if(iopt.eq.2)print*,'In read_div_ef @ 271'
         if(ndiv.gt.ndiv_max.or.nl.gt.ndiv_max)then
            ndiv_max=max0(ndiv,nl)
!           the event is longer than any of the previous events so 
!           more memory has to be allocated

            if(iopt.eq.2)print*,'in read_div_ef @ 277'

!           DEALLOCATION OF ARRAYS FROM AREA5A:
            deallocate(qdiv,stat=iDeallocate)     
            if (iDeallocate.ne.0) STOP    
     *        'Error with deallocation of area5a arrays'

!           re-allocate for larger arrays
            allocate(qdiv(nodiv,ndiv_max),stat=iAllocate)
            if(iAllocate.ne.0) STOP
     *        'Error with allocation of area5a arrays in sub'
         endif

!        REASSIGN THE DIVERSION NAMES AND FUNCTION COEFFICIENTS
!        This part is used only if coefficient values area >0
!        Used if values change over time & need to be reassigned.
         do i=1,nodiv
	    divname(i) = colHeader%tb0cmd%colName(i) ! diversion name
	    xstrdiv(i) = colHeader%tb0cmd%colLocX(i) ! x coordinate at diversion start
	    ystrdiv(i) = colHeader%tb0cmd%colLocY(i) ! y coordinate at diversion start
	    xenddiv(i) = colHeader%colLocX1(i)       ! x coordinate at diversion end
	    yenddiv(i) = colHeader%colLocY1(i)       ! y coordinate at diversion end
!           if -ve values are entered for all entries for one diversion
!           only values in the first event are used
!           This makes it a lot easier to tweak as only one file
!           needs to be edited.
            if(val2divyes.eq.1) then              ! The div.tb0 file contains Value2
	       if(colHeader%colValue1(i).gt.0.0.or.
     *            colHeader%colValue2(i).gt.0) then
                  val1div(i) = colHeader%colValue1(i) ! value 1
                  val2div(i) = colHeader%colValue2(i) ! value 2
                  val3div(i) = colHeader%colValue3(i) ! value 3
                  val4div(i) = colHeader%colValue4(i) ! value 4
               endif
            else
               if(colHeader%colValue1(i).gt.0.0) ! The div.tb0 file does not contain Value2
     *            val1div(i) = colHeader%colValue1(i) ! value 1 only
	    endif
         end do
      endif   ! End if loop re whether firstpass='y'
!       rev. 9.1.69  Dec.  19/04  - NK: rewrote rddiv c/w memory allocation 

!!!!!!!!!!!!!!!!!!!!!!!!

      if(iopt.eq.2)print*,'In read_div_ef @ 319'

!     If there's no data in the diversion file
      if(nodiv.eq.0)return     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-----

!     ASSIGN THE DIVERSION LOCATIONS NAMES AND FUNCTION COEFFICIENTS
      do i=1,nodiv
         divname(i) = colHeader%tb0cmd%colName(i) ! diversion name
         xstrdiv(i) = colHeader%tb0cmd%colLocX(i) ! x coordinate at diversion start
         ystrdiv(i) = colHeader%tb0cmd%colLocY(i) ! y coordinate at diversion start
         xenddiv(i) = colHeader%colLocX1(i)       ! x coordinate at diversion end
         yenddiv(i) = colHeader%colLocY1(i)       ! y coordinate at diversion end
         if(val2divyes.eq.1) then                 ! The div.tb0 file contains Value2
	    if(colHeader%colValue1(i).gt.0.0.or.
     *         colHeader%colValue2(i).gt.0) then
               val1div(i) = colHeader%colValue1(i) ! value 1
               val2div(i) = colHeader%colValue2(i) ! value 2
               val3div(i) = colHeader%colValue3(i) ! value 3
               val4div(i) = colHeader%colValue4(i) ! value 4
            endif
         else
            if(colHeader%colValue1(i).gt.0.0) ! The div.tb0 file does not contain Value2
     *         val1div(i) = colHeader%colValue1(i) ! value 1 only
	 endif
      end do

      deallocate(colHeader%tb0cmd%colName)
      deallocate(colHeader%tb0cmd%colLocX)
      deallocate(colHeader%tb0cmd%colLocY)
      deallocate(colHeader%colLocX1)
      deallocate(colHeader%colLocY1)
      deallocate(colHeader%colValue1)
      if(val2divyes.eq.1) then              ! The div.tb0 file contains Value2
         deallocate(colHeader%colValue2)
         deallocate(colHeader%colValue3)
         deallocate(colHeader%colValue4)
      end if

      if(iopt.eq.2)print*,'in read_div_ef passed 353'

!!!!!!!!!!!!!!!!!!!!!!!!

!     Convert lat/lon coordinates to grid i,j coordinates
      if(nodiv.gt.0)then
         do i=1,nodiv
            if (fstflg .eq. 'y') then                       ! fst files
               if   (xstrdiv(i).ge.-360.0 .and. xstrdiv(i).le.360.0
     *         .and. ystrdiv(i).ge.-180.0 .and. ystrdiv(i).le.180.0)
     *                                                          then   ! Inside the watershed
                  xdivpos = xstrdiv(i)
                  if (xdivpos .lt. 0.0) xdivpos = 360.00 + xdivpos
                  do ii=1,size(longrid)-1
                     if   (xdivpos.ge.longrid(ii)
     *               .and. xdivpos.le.longrid(ii+1)) then
                        jstrdiv(i) = ii
                        exit
                     end if
                  end do
                  do jj=1,size(latgrid)-1
                     if   (ystrdiv(i).ge.latgrid(jj)
     *               .and. ystrdiv(i).le.latgrid(jj+1)) then
                        istrdiv(i) = jj
                        exit
                     end if
                  end do
               else                                                         ! Outside the watershed
                  jstrdiv(i) = 0
                  istrdiv(i) = 0
               end if

               if   (xenddiv(i).ge.-360.0 .and. xenddiv(i).le.360.0
     *         .and. yenddiv(i).ge.-180.0 .and. yenddiv(i).le.180.0)
     *                                                          then   ! Inside the watershed
                  xdivpos = xenddiv(i)
                  if (xdivpos .lt. 0.0) xdivpos = 360.00 + xdivpos
                  do ii=1,size(longrid)-1
                     if   (xdivpos.ge.longrid(ii)
     *               .and. xdivpos.le.longrid(ii+1)) then
                        jenddiv(i) = ii
                        exit
                     end if
                   end do
                   do jj=1,size(latgrid)-1
                     if   (yenddiv(i).ge.latgrid(jj)
     *               .and. yenddiv(i).le.latgrid(jj+1)) then
                        ienddiv(i) = jj
                        exit
                     end if
                  end do
               else                                                         ! Outside the watershed
                  jenddiv(i) = 0
                  ienddiv(i) = 0
               end if

            else                                            ! r2c files
               jstrdiv(i)=int((xstrdiv(i)-xorigin)/xdelta)+1
               istrdiv(i)=int((ystrdiv(i)-yorigin)/ydelta)+1
               jenddiv(i)=int((xenddiv(i)-xorigin)/xdelta)+1
               ienddiv(i)=int((yenddiv(i)-yorigin)/ydelta)+1
            end if
         end do

!        D. Durnford: Determine the index of the vectorized array corresponding to each end of the diversion
         do i=1,nodiv
            istr = 0
            iend = 0
            do n=1,naa
               if(xxx(n).eq.jstrdiv(i) .and. yyy(n).eq.istrdiv(i)) then
                  divstrindex(i) = n
                  istr = 1
               end if
               if(xxx(n).eq.jenddiv(i) .and. yyy(n).eq.ienddiv(i)) then
                  divendindex(i) = n
                  iend = 1
               end if
               itot = istr + iend
               if(itot .gt. 1) exit
            end do

            if (jstrdiv(i).eq.0 .and. istrdiv(i).eq.0)  ! Points outside the watershed: reset the index value to -1
     *         divstrindex(i) = -1
            if (jenddiv(i).eq.0 .and. ienddiv(i).eq.0)
     *         divendindex(i) = -1

            if(val2divyes.eq.1) then          ! The div.tb0 file contains Value2
	       if (val2div(i).eq.1.and.divstrindex(i).eq.-1.or.
     *         val2div(i).eq.1.and.divendindex(i).eq.-1) then
	   	PRINT*, 'Error for station ',i,': incompatibility 
     *             between value2 and location of stations'
	   	PRINT*, 'check lat-lon of this station and wiki page'
	   	STOP
	       endif
	       if (val2div(i).eq.2.and.divstrindex(i).eq.-1.or.
     *         val2div(i).eq.2.and.divendindex(i).ne.-1) then
                   PRINT*, 'Error for station ',i,': incompatibility
     *             between value2 and location of stations'
                   PRINT*, 'check lat-lon of this station and wiki page'
                   STOP
               endif
	       if (val2div(i).eq.3.and.divendindex(i).eq.-1.or.
     *         val2div(i).eq.3.and.divstrindex(i).ne.-1) then
                   PRINT*, 'Error for station ',i,': incompatibility
     *             between value2 and location of stations'
                   PRINT*, 'check lat-lon of this station and wiki page'
                   STOP
               endif
            endif
         end do

!        Prepare to represent irrigation
         if(val2divyes .eq. 1) then                   ! The div.tb0 file contains Value2
            nodivirrig = 0                            ! The number of irrigation regions
            maxirrigpts = 0                           ! The largest number of points in an individual irrigation region
            do i=1,nodiv                              ! Determine how many regions are to be irrigated
               if (divname(i)(1:5).eq.'irrig'
     *        .or. divname(i)(1:5).eq.'Irrig') then   ! Irrigation is to  be represented
                  if(val2div(i).ne.2)then
                    PRINT*,'Irrigation station must be type 2 diversion'
                    PRINT*,'check div file and wiki page'
                    stop
                  endif
                  nodivirrig = nodivirrig + 1                   ! The maximum number of points to be irrigated 
                  maxirrigpts =max(maxirrigpts,                 ! is from the largest specified irrigation region
     *                         (val3div(i)+1) * (val4div(i)+1)) ! Add an extra point in each direction: needed if value3 or value4 is an even number
               end if
            end do

            ! Initialize arrays:
            !   irrigindx: the indices (1-naa) of points to be irrigated
            !   qdivirrig: the volume (m3/s) of water to be removed from the irrigated point in the simulation timestep
            allocate(totirrigpts(nodivirrig),
     *        iminirr(nodivirrig),imaxirr(nodivirrig),
     *        jminirr(nodivirrig),jmaxirr(nodivirrig),
     *        irrigindx(nodivirrig,maxirrigpts),
     *        qdivirrig(nodivirrig,ndiv_max),
     *        stat=iAllocate)
            if(iAllocate.ne.0) STOP
     *        'Error with allocation in read_div_ef @484'
            do i=1,nodivirrig                         ! Initialize the arrays with a negative number indicating they're inactive
               do n=1,maxirrigpts
                  irrigindx(i,n) = -1
               end do
               do k=1,ndiv_max
                  qdivirrig(i,k) = 0
               end do
            end do

            ! Determine the grid points in which water is to be lost due to irrigation
            irindex = 0
            do i=1,nodiv                              ! Determine how many regions are to be irrigated
               if (divname(i)(1:5).eq.'irrig'
     *        .or. divname(i)(1:5).eq.'Irrig') then   ! Irrigation is to  be represented
                  irindex = irindex + 1

                  ! Define the rectangle from the provided values
                  iminirr(irindex) = max(istrdiv(i) - val4div(i)/2, 0)
                  imaxirr(irindex) = min(istrdiv(i) + val4div(i)/2, 
     *                                                  size(latgrid))
                  jminirr(irindex) = max(jstrdiv(i) - val3div(i)/2, 0)
                  jmaxirr(irindex) = min(jstrdiv(i) + val3div(i)/2,
     *                                                  size(longrid))

                  ! Determine the number of points in this rectangle that are also in the watershed
                  irpt = 0
                  do ii=iminirr(irindex),imaxirr(irindex)
                     do jj=jminirr(irindex),jmaxirr(irindex)
                        do n=1,naa
                           if(xxx(n).eq.jj .and. yyy(n).eq.(ii)) then
                              irpt = irpt + 1
                              irrigindx(irindex,irpt) = n
                              exit
                           end if
                        end do
                     end do
                  end do
                  totirrigpts(irindex) = irpt
               end if
            end do
         end if
         
!        Report on values being used for diversions
         irindex = 0
         do i=1,nodiv
            if(val2divyes.eq.1) then          ! The div.tb0 file contains Value2
               write(52,1010)
               write(52,1011) i,divname(i),istrdiv(i),jstrdiv(i),
     *            ienddiv(i),jenddiv(i),val1div(i),val2div(i),
     *            val3div(i),val4div(i),val2divyes,
     *            divstrindex(i), divendindex(i)
               if (divname(i)(1:5).eq.'irrig'
     *        .or. divname(i)(1:5).eq.'Irrig') then   ! Irrigation is to  be represented
                  irindex = irindex + 1
                  write(52,1012)
                  write(52,1013) irindex,
     *               iminirr(irindex),imaxirr(irindex),
     *               jminirr(irindex),jmaxirr(irindex),
     *               totirrigpts(irindex)
               end if
            else
               write(52,1014)
               write(52,1015) i,divname(i),istrdiv(i),jstrdiv(i),
     *            ienddiv(i),jenddiv(i),val1div(i),
     *            divstrindex(i), divendindex(i)
            end if
         end do

!!!!!!!!!!!!!!!!!!!!!!!!

!        READ DIVERSION DATA
!        THE DIVERSION DATA ARE ALL READ IN IN RTE_SUB.F
!        THEY ARE THEN STORED AND USED EACH TIME ROUTE IS CALLED.

!        initialize releases
         do k=1,nodiv
            do j=1,ndiv
               qdiv(k,j)=-1.0
            end do
         end do

! Read in the diversion data
         do j=1,ndiv,ktr
            read(unitNum,*,iostat=ios)(qdiv(k,j),k=1,nodiv)   ! The reading of the data
            if(ios.ne.0)then
               write(98,*)' Error on unit=292,fln=',trim(fln(42))
               write(98,*)' Trying to read diversion data hour =',j
               print*,' Error on unit=292,fln=',trim(fln(42))
               print*,' Trying to read diversion data hour = ', j
               print*,' ios= ',ios
               if(ios.eq.-1)then
                 print*,'End of file in fln= ',trim(fln(42))
                 print*,'Possibly last line does not have a return'
                 print*
               else
                 print*
                 STOP ' program aborted in read_div_ef.f'
               endif
            endif
         end do
         if(iopt.ge.1)then
            j=ktr
            write(52,*)(qdiv(k,j),k=1,nodiv)
         endif

      endif                     !  if(nodiv.gt.0)

      if(iopt.eq.2)print*,'in read_div_ef passed 469'

!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!     PROCESS THE DATA READ IN

!     START TIME:
!     ORIGINAL VERSION OF THE DIV.TB0 FILE:
!       A NEGATIVE VALUE AT THE START TIME IS RESET TO ZERO (BACKWARDS COMPATILIBITY).
!     IF THE DIV.TB0 FILE CONTAINS VALUE2:
!       VALUE2=1 OR VALUE2=2 (DIVERSION START POINT WITHIN WATERSHED)
!          A NEGATIVE VALUE AT THE START TIME IS LEFT UNTOUCHED:
!          IT INDICATES THAT THE SIMULATED FLOW MULTIPLIED BY VALUE1 IS TO BE DIVERTED in route.f
!       VALUE2=3 (DIVERSION START POINT OUTSIDE WATERSHED)
!          A NEGATIVE VALUE AT THE START TIME CAUSES THE PROGRAM TO ABORT
      do k=1,nodiv
         if(qdiv(k,1).lt.0.0) then
            if(val2divyes .eq. 1) then     ! The div.tb0 file contains Value2
               if(val2div(k) .eq. 3) then  ! Diversion's starting point is outside the watershed
                  write(*,'(a52,1x,a52)') 
     *           'If Value2=3 (diversion starts outside of watershed),',
     *           'diversion data cannot be negative at the start time.'
                  write(*,'(a35)') 'Modify input file yyyymmdd_div.tb0.'
                  stop
               end if
            else                           ! The div.tb0 file does not contain Value2; flows must be >= 0.
               qdiv(k,1)=0.0               ! Backwards compatibility
            end if
         end if
      end do
    
!     AFTER THE START TIME:
!     ORIGINAL VERSION OF THE DIV.TB0 FILE:
!       CURRENT TIME'S NEGATIVE DIVERSION = LAST NON-NEGATIVE DIVERSION
!     IF THE DIV.TB0 FILE CONTAINS VALUE2:
!       VALUE2=1 OR VALUE2=2 (DIVERSION START POINT WITHIN WATERSHED)
!          CURRENT TIME'S NEGATIVE DIVERSION = LAST DIVERSION (POSITIVE OR NEGATIVE)
!       VALUE2=3 (DIVERSION START POINT OUTSIDE WATERSHED)
!          CURRENT TIME'S NEGATIVE DIVERSION = LAST NON-NEGATIVE DIVERSION

      if(nodiv.gt.0)then       !  <<<<<<<<???????????????????
         do j=2,ndiv                            ! Process diversion data after the first hour
            do k=1,nodiv
               if(qdiv(k,j).lt.0.0) then
                  if(val2divyes .eq. 1) then    ! The div.tb0 file contains Value2
                     qdiv(k,j)=qdiv(k,j-1)      ! Apply persistence if a negative diversion flow is specified after the 1st hour
                     if(val2div(k).eq.3 .and. qdiv(k,j).lt.0.0)then ! The diversion's starting point is outside the watershed
                        write(*,*) 'Value2=3 (diversion start point ',
     *                     'outside of watershed). Negative diversion ',
     *                     'value at current time has been replaced ',
     *                     'by a negative value from the previous ',
     *                     'time. Negative values are not allowed for ',
     *                     'Value2=3.'
                        stop
                     end if
                  else                          ! The div.tb0 file does not contain Value2
                     qdiv(k,j)=qdiv(k,j-1)      ! Apply persistence if a negative diversion flow is specified after the 1st hour
                  end if
               end if
            end do
         end do

         ! Divide the flow to be diverted from the irrigation region
         ! between the points within the defined region that are in the watershed
         if(val2divyes .eq. 1) then                   ! The div.tb0 file contains Value2
            irindex = 0
            do i=1,nodiv                              ! Loop through the diversion names searching for an irrigation area
               if (divname(i)(1:5).eq.'irrig'
     *        .or. divname(i)(1:5).eq.'Irrig') then   ! Irrigation is to  be represented
                  irindex = irindex + 1
                  do j=1,ndiv                         ! Loop through the simulation hours
                     qdivirrig(irindex,j)
     *                 = qdiv(i,j) / totirrigpts(irindex)
                  end do
               end if
            end do
         endif

      endif                     ! if(nodiv.gt.0)

      if(iopt.ge.2)then
         do k=1,nodiv
            write(52,6801)k,ndiv
            write(52,6802)(qdiv(k,j),j=1,ndiv)
         end do	
      endif
      if(iopt.eq.2)print*,'in read_div_ef passed 537'

      close(unit=unitNum,status='keep')

 999  RETURN                    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-----

! FORMATS

 500  format(256f10.3)
 1010 format(' ',3x,'i  divname(i)  istrdiv(i)  jstrdiv(i)  ',
     *       'ienddiv(i)  jenddiv(i)  val1div(i)  val2div(i)'  ,
     *       'val3div(i)  val4div(i)'  ,
     *       'val2divyes  divstrindex(i)  divendindex(i)')
 1011 format(' ',3x,i3,a15,4i8,f12.4,4i5,2i8)
 1012 format(' ',3x,'irindex  iminirr(irindex)  jminirr(irindex)  ',
     *       'jminirr(irindex)  jmaxirr(irindex)  totirrigpts(irindex)')
 1013 format(' ',3x,i3,5i8)
 1014 format(' ',3x,'i  divname(i)  istrdiv(i)  jstrdiv(i)  ',
     *       'ienddiv(i)  jenddiv(i)  val1div(i)  divstrindex(i)  ',
     *       'divendindex(i)')
 1015 format(' ',3x,i3,a15,4i8,f12.4,2i8)
 6801 format('   read_div_ef: reservoir no =',i3,' mhtot =',i5)
 6802 format('   ',256f8.2)

      END SUBROUTINE read_div_ef

