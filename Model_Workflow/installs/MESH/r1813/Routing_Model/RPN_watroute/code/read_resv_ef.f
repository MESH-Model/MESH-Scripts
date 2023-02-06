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

      SUBROUTINE read_resv_ef()

C*****************************************************************************
C  READ_RESV_EF - written Sep/06 by Dave Watson, CHC
C	- Derived from rdresv written by Nick Kouwen
C	This subroutine reads the reservoir release (REL) file 
C	(tb0 format)
C*****************************************************************************

      use area_watflood

C TB0 data module
      USE EF_Module

      implicit none
      TYPE(ResvParam) :: header
      TYPE(ResvColumnMetaData) :: colHeader

      Integer  :: ios,j,k,i,n,l,jz,ii,jj
      integer  :: nnch,noresv_firstpass,nrel_max,
     *            iAllocate,iDeallocate
      real*4   :: factor,xrespos

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
      character*128 keyword, value 
      integer lineLen, KeyLen, wordCount
      logical rStat, lineType, foundEndHeader, insideColMetaData
      real*4,    dimension(:), allocatable :: yres,xres
      integer*4  noresvi

C Set unit and fln number
      unitNum = 37
      flnNum = 7

C Open the file
      INQUIRE(FILE=trim(adjustl(fln(flnNum))),EXIST=exists)
      if(exists)then
         open(unit=unitNum,file=trim(adjustl(fln(flnNum))),
     *     status='old',iostat=ios)
         if(ios.ne.0)then
            print*,'Problems opening ',fln(flnNum)
            print*
            STOP ' Stopped in read_resv_ef'
         endif
         noresv=1  ! assume there is at least one reservoir if there is a file
      else
         if(numa.eq.0)then
            print*,'WARNING:'
c            print*,'Reservoir release (rel) file ',fln(flnNum)
            print*,'Reservoir release (rel) file fln(',flnNum
	    print*,')is NOT found'
            print*,'Program continues with no lakes or reservoirs'
	    print*,'Not a problem if there area no lakes or reservoirs!'
            print*
         endif
         noresv=0
         noresvi=0
         nrel=0
      endif

      if(iopt.eq.2)print*,'in read_resv_ef passed 70'

      if(noresv.ge.1)then


C Initialize default values
         CALL InitResvParam(header)	

         if(iopt.eq.2)print*,'in read_resv_ef passed 76'

C Search for and read tb0 file header
         line(1:1) = '#'
         foundEndHeader = .false.
         insideColMetaData = .false.

         do WHILE((.NOT.foundEndHeader) .AND.
     &        ((line(1:1) .eq. '#') .OR.
     &        (line(1:1) .eq. ':') .OR.
     &        (LEN_TRIM(line) .eq. 0))) 	

            read(UNIT=unitNum, FMT='((A))', iostat=ios) line ! read a line
            if(ios .eq. -1)then
               write(6,'((A))') 'ERROR: Premature EndOfFile encountered'
               STOP ' Stopped in read_resv_ef'
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
                  iStat = ParseResvColumnMetaData(colHeader,keyword,
     &                 KeyLen,subString)
                  if(iStat .lt. 0) then
                     write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
                     write(*,'(2(A))') '   in line: ',line		
                     STOP ' Stopped in read_resv_ef'
                     return
                  endif
               else
                  iStat=ParseResvParam(header,keyword,KeyLen,subString)
                  if (keyword(1:KeyLen) .eq. ':starttime') then
                     read(subString( 9:10),'(i2)') relday1
                     read(subString(12:13),'(i2)') relhour1
                  endif
                  if(iStat .lt. 0) then
                     write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
                     write(*,'(2(A))') '   in line: ',line		
                     STOP ' Stopped in read_resv_ef'
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

         if(iopt.eq.2)print*,'in read_resv_ef passed 132'


C Assign the variables from the types
         ktr =  header%tb0p%deltaT !data time step in hours
         factor = header%tb0p%unitConv ! conversion to cms
         noresv = colHeader%tb0cmd%colCount !no of reservoirs
         if (noresv .ne. Nreaches) then
           write(*,*) ' '
           write(*,*) 
     *    'no of reaches calculated from the reservoir release file:',noresv
           write(*,*) 
     *    'no of reaches calculated from infiles_rte.txt:',Nreaches
           write(*,*) 'Resolve this conflict!'
           stop
         end if

!     nrel    =     no of hours of data
C Scan lines of data
         rewind unitNum

!        added *ktr     Nov. 27/06  nk
         nrel = CountDataLinesAfterHeader(unitNum)*ktr


         rewind unitNum
         CALL GoToStartOfData(unitNum)

      endif

c End of the loop if noresv.ge.1

      do n=1,naa
         if(ireach(n).gt.0)then
	    if(ireach(n).gt.noresv)then
             print*,' There are more reservoirs in the map & shed file'
             print*,' than there are outlet locations in the .rel file'
             print*,' Please fix either file so the number will match'
             print*,' No of reaches found = at least',ireach(n)
             print*,' No of reservours in rel file =',noresv
             print*
             stop ' Program aborted in sub before allocation of area5'
	    endif
         endif
      end do

!       allocate stuff      
      if(firstpass.eq.'y')then
         firstpass='n'
         noresv_firstpass=noresv
!         nl comes from the .str file and is the # hours of the event
!         nl is NOT defined in watroute because .str files are not used
         nrel_max=max0(nrel,nl)
         if(nrel_max.lt.1)nrel_max=event_hours
!         but we need to provide enough memory to simulate a whole event
!         sometimes users specify the duration in the rel to be just 1 hr.
!         when a rule is given. However, we need memory of all the variables
!         If there is no flow or release data, the no of hours of rain
!            as given in the event file is used!!!!!!!!!1
         if(noresv.gt.0)then
            allocate(b1(noresv),b2(noresv),b3(noresv),
     *        b4(noresv),b5(noresv),ires(noresv),jres(noresv),
     *        resindex(noresv),b6(noresv),b7(noresv),
     *        yres(noresv),xres(noresv),poliflg(noresv), !nk 18/06/04
     *        resname(noresv),qrel(noresv,nrel_max),
     *        qdwpr(noresv,nrel_max),lake_elv(noresv,nrel_max),
     *        lake_stor(noresv,nrel_max),lake_outflow(noresv,nrel_max), 
     *        del_stor(noresv,nrel_max),lake_inflow(noresv,nrel_max),
     *        qstream_sum(noresv,nrel_max),strloss_sum(noresv,nrel_max),
     *        stat=iAllocate)
!     *        net_lake_inflow(noresv,nrel_max),
!     *        net_lake_outflow(noresv,nrel_max),
            if(iAllocate.ne.0) STOP
     *       'Error with allocation in read_resv_ef @172'
         else
            allocate(b1(1),b2(1),b3(1),b4(1),b5(1),ires(1),jres(1),
     *           resindex(1),b6(1),b7(1),
     *           resname(1),qrel(1,nrel),qdwpr(1,nrel),poliflg(1),
     *           stat=iAllocate)
            if(iAllocate.ne.0) STOP
     *           'Error with allocation in read_resv_ef @178'
         endif

         if(iopt.eq.2)print*,' after area5 allocate in read_resv_ef'

!     TS - ALLOCATION OF AREA6A ARRAYS (REMAINDER)
         allocate(inbsnflg(no+noresv),stat=iAllocate)
         if(iAllocate.ne.0) STOP ! mod 03/05/02 nk
     *        'Error with allocation of area6a arrays in sub'

d     print*,'inbsnflg allocated for ',no,' + ',noresv

!     TS: FOR ARRAY DELTA
      nnch=max0(1,nch)

      if(iopt.eq.2)print*,'In read_resv_ef @ 233'

!     TS - ALLOCATION OF AREA10A ARRAYS (REMAINDER)
      allocate(
     *     delta(nnch,no+noresvi), ! fixed bug 24/07/00
     *     qpeakh(no+noresvi),qpeaks(no+noresvi),
     *     dpeakh(no+noresvi),dpeaks(no+noresvi),
     *     volsyn(no+noresvi),volhyd(no+noresvi),
     *     stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *     'Error with allocation of area10a arrays in sub'

      if(iopt.eq.2)print*,'In read_resv_ef @ 233'

!         initialize ERROR FUNCTIONS = 0
      do i=1,ni
         do l=1,no+noresvi
            delta(i,l)=0.0
         end do
      end do
      if(iopt.eq.2)print*,'In read_resv_ef @ 253'

!       ASSIGN THE GAUGE LOCATIONS NAMES AND FUNCTION COEFFICIENTS
!         this is done only during the first pass if coefficient values 
!         are set to -1 for subsequent events. This makes tweaking easy
!         as only the values in the first event need to be adjusted.
      do i=1,noresv
         resname(i) = colHeader%tb0cmd%colName(i) ! reservoir name
         xres(i) = colHeader%tb0cmd%colLocX(i) ! x coordinate
         yres(i) = colHeader%tb0cmd%colLocY(i) ! y coordinate
         b1(i) = colHeader%colCoeff1(i) ! coefficient 1
         b2(i) = colHeader%colCoeff2(i) ! coefficient 2
         b3(i) = colHeader%colCoeff3(i) ! coefficient 3
         b4(i) = colHeader%colCoeff4(i) ! coefficient 4
         b5(i) = colHeader%colCoeff5(i) ! coefficient 5
         b6(i) = colHeader%colCoeff6(i) ! coefficient 6
         b7(i) = colHeader%colCoeff7(i) ! coefficient 7
         if(b3(i).eq.0.0) then
           poliflg(i)='n'
         else
           poliflg(i)='y'
         endif
      end do

      else                      !   firstpass
!         subsequent passes
!         check to see memory allocated is adequate      
         if(iopt.eq.2)print*,'In read_resv_ef @ 258'
         if(noresv.ne.noresv_firstpass)then
            print*,'No of reservoirs has been changed in'
            print*,'in file ',fln(7)
            print*,'This is not permitted'
            print*
            stop 'Program aborted in rdresvo @ 264'
         endif
         if(iopt.eq.2)print*,'In read_resv_ef @ 266'
         if(nrel.gt.nrel_max.or.nl.gt.nrel_max)then
            nrel_max=max0(nrel,nl)
!           the event is longer than any of the previous events so 
!           more memory has to be allocated
!           DEALLOCATION OF ARRAYS FROM AREA10A:

            if(iopt.eq.2)print*,'in read_resv_ef @ 213'

!           DEALLOCATION OF ARRAYS FROM AREA5A:
            deallocate(qrel,qdwpr,lake_elv,
     *      lake_stor,lake_inflow,lake_outflow,del_stor,
     *      stat=iDeallocate)     
!     *      lake_stor,lake_inflow,lake_outflow,del_stor,net_lake_inflow,
!     *      net_lake_outflow,stat=iDeallocate)     
            if (iDeallocate.ne.0) print*,    
     *           'Error with deallocation of area5a arrays'

!           re-allocate for larger arrays
            allocate(qrel(noresv,nrel), 
     *      qdwpr(noresv,nrel_max),lake_elv(noresv,nrel_max),
     *      lake_stor(noresv,nrel_max),lake_outflow(noresv,nrel_max),
     *      del_stor(noresv,nrel_max),
     *      lake_inflow(noresv,nrel_max),stat=iAllocate)
            if(iAllocate.ne.0) STOP
!     *      del_stor(noresv,nrel_max),net_lake_inflow(noresv,nrel_max),
!     *      net_lake_outflow(noresv,nrel_max),
     *           'Error with allocation of area10a arrays in sub'
         endif

!       REASSIGN THE GAUGE LOCATIONS NAMES AND FUNCTION COEFFICIENTS
!       This part is used only if coefficient values area >0
!       Used if values change over time & need to be reassigned.
         do i=1,noresv
	    resname(i) = colHeader%tb0cmd%colName(i) ! reservoir name
	    xres(i) = colHeader%tb0cmd%colLocX(i) ! x coordinate
	    yres(i) = colHeader%tb0cmd%colLocY(i) ! y coordinate
!         if -ve values are entered for all entries for one lake
!         only values in the first event are used
!         This makes it a lot easier to tweak as only one file
!         needs to be edited.
	    if(colHeader%colCoeff1(i).gt.0.0.or.
     *         colHeader%colCoeff2(i).gt.0.0.or.
     *         colHeader%colCoeff3(i).gt.0.0.or.
     *         colHeader%colCoeff4(i).gt.0.0.or.
     *         colHeader%colCoeff5(i).gt.0.0)then
               b1(i) = colHeader%colCoeff1(i) ! coefficient 1
               b2(i) = colHeader%colCoeff2(i) ! coefficient 2
               b3(i) = colHeader%colCoeff3(i) ! coefficient 3
               b4(i) = colHeader%colCoeff4(i) ! coefficient 4
               b5(i) = colHeader%colCoeff5(i) ! coefficient 5
               b6(i) = colHeader%colCoeff6(i) ! coefficient 6
               b7(i) = colHeader%colCoeff7(i) ! coefficient 7
         if(b3(i).eq.0.0) then
           poliflg(i)='n'
         else
           poliflg(i)='y'
         endif
	    endif
         end do
      endif                     ! firstpass
!       rev. 9.1.69  Dec.  19/04  - NK: rewrote rdresv c/w memory allocation 

      if(iopt.eq.2)print*,'In read_resv_ef @ 295'





      if(noresv.eq.0)return     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-----




!       ASSIGN THE GAUGE LOCATIONS NAMES AND FUNCTION COEFFICIENTS
      do i=1,noresv
         resname(i) = colHeader%tb0cmd%colName(i) ! reservoir name
         xres(i) = colHeader%tb0cmd%colLocX(i) ! x coordinate
         yres(i) = colHeader%tb0cmd%colLocY(i) ! y coordinate
         b1(i) = colHeader%colCoeff1(i) ! coefficient 1
         b2(i) = colHeader%colCoeff2(i) ! coefficient 2
         b3(i) = colHeader%colCoeff3(i) ! coefficient 3
         b4(i) = colHeader%colCoeff4(i) ! coefficient 4
         b5(i) = colHeader%colCoeff5(i) ! coefficient 5
         b6(i) = colHeader%colCoeff6(i) ! coefficient 6
         b7(i) = colHeader%colCoeff7(i) ! coefficient 7
         if(b3(i).eq.0.0) then
           poliflg(i)='n'
         else
           poliflg(i)='y'
         endif
      end do
      deallocate(colHeader%tb0cmd%colName)
      deallocate(colHeader%tb0cmd%colLocX)
      deallocate(colHeader%tb0cmd%colLocY)
      deallocate(colHeader%colCoeff1)
      deallocate(colHeader%colCoeff2)
      deallocate(colHeader%colCoeff3)
      deallocate(colHeader%colCoeff4)
      deallocate(colHeader%colCoeff5)


      if(iopt.eq.2)print*,'in read_resv_ef passed 274'

      if(noresv.gt.0)then
!         FIND THE LOCAL COORDINATES FOR THE RESERVOIRS
!         THE VALUES FOR IRES AND JRES ARE CHANGED

         do i=1,noresv
!           convert to local coordinate unit system for new .res file
!     rev. 9.2.26  Dec.  23/05  - NK: Fixed reservoir outlet location bug 
           if (fstflg .eq. 'y') then                                     ! fst files
!           if (longrid(1).gt.0.0 .and. latgrid(1).gt.0.0) then
             xrespos = xres(i)
             if (xrespos .lt. 0.0) xrespos = 360.00 + xrespos
             do ii=1,size(longrid)-1
               if   (xrespos.ge.longrid(ii)
     *         .and. xrespos.le.longrid(ii+1)) then
                 jres(i) = ii
                 exit
               end if
             end do
             do jj=1,size(latgrid)-1
               if   (yres(i).ge.latgrid(jj)
     *         .and. yres(i).le.latgrid(jj+1)) then
                 ires(i) = jj
                 exit
               end if
             end do
           else                                                          ! r2c files
             jres(i)=int((xres(i)-xorigin)/xdelta)+1
             ires(i)=int((yres(i)-yorigin)/ydelta)+1
           end if
         end do
         if(iopt.ge.1)then
            write(52,1011)
            write(52,1013)(i,ires(i),jres(i),
     *           b1(i),b2(i),b3(i),b4(i),b5(i),
     *           b6(i),b7(i),resname(i),i=1,noresv)
         endif

!         THE ORDER OF READING THE COORDINATES OF THE RESERVOIRS
!         MUST BE THE SAME AS READING THE CORRESPONDING FLOWS
!         IN S/R REROUT.
!         READ RELEASES
!         THE RESERVOIR OUTFLOWS ARE ALL READ IN THE FIRST TIME
!         REROUT IS CALLED. THEY ARE THEN STORED AND USED EACH TIME
!         REROUT IS CALLED.
!         IF NATURAL CONTROL, FLOWS ARE SET TO -1.0 ABOVE

!        D. Durnford: Determine the index of the vectorized array corresponding to each reservoir
         do n=1,naa
           do i=1,noresv
             if(xxx(n).eq.jres(i) .and. yyy(n).eq.ires(i))
     *          resindex(i) = n
           end do
         end do

!         initialize releases
         do k=1,noresv
            do j=1,nrel
               qrel(k,j)=-1.0
            end do
         end do

! D Durnford: reservoirs where releases are specified not calculated are listed first with all coefficients set to 0.
! I think this line and section of code are right.
! original (it appears to be wrong - D. Deacu) 
         if(b1(1).eq.0.0)then
! use the line below for now: test b1 of lake Nipigon (D. Deacu)
! csubich (9/12) -- This block of code seems very specific and
! probably shouldn't exist; in the meantime, I'm only fixing
! the read-past-the-end-of-the-array if noresv isn't at least 6.
!         if (noresv.ge.6 .and. b1(6).eq.0.0) then
!           do j=ktr,nrel,ktr
!     rev. 9.1.13  Mar.  23/02  - fixed resv. timing, moved to beginning of dt
            do j=1,nrel,ktr
               read(unitNum,*,iostat=ios)(qrel(k,j),k=1,noresv)
               if(ios.ne.0)then
                  write(98,*)' Error on unit=37,fln=',trim(fln(7))
                  write(98,*)' Trying to read releases hour =',j
                  print*,' Error on unit=37,fln=',trim(fln(7))
                  print*,' Trying to read releases'
                  print*,' ios= ',ios
                  if(ios.eq.-1)then
                    print*,'End of file in fln= ',trim(fln(7))
                    print*,'Possibly last line does not have a return'
                    print*
                  else
                    print*
                    STOP ' program aborted in read_resv_ef.for'
                  endif
               endif
            end do
         endif                  !  if(b1(1).eq.0.0)
         if(iopt.ge.1)then
            j=ktr
            write(52,*)(qrel(k,j),k=1,noresv)
         endif
      endif                     !  if(noresv.gt.0)

      if(iopt.eq.2)print*,'in read_resv_ef passed 187'

!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

 
      istep=al/1000
      if(iopt.eq.2)print*,'in read_resv_ef passed 272'

      if(noresv.gt.0)then       !999999999999999
!       FIND THE LOCAL COORDINATES FOR THE RESERVOIRS
!       THE VALUES FOR IRES AND JRES ARE CHANGED

         if(iopt.ge.1)then
            write(52,1011)
            write(52,1013)(i,ires(i),jres(i),
     *           b1(i),b2(i),b3(i),b4(i),b5(i),
     *           b6(i),b7(i),resname(i),i=1,noresv)
            write(52,1011)
            write(52,1013)(i,ires(i),jres(i),
     *           b1(i),b2(i),b3(i),b4(i),b5(i),
     *           b6(i),b7(i),resname(i),i=1,noresv)
         endif

!       THE ORDER OF READING THE COORDINATES OF THE RESERVOIRS
!       MUST BE THE SAME AS READING THE CORRESPONDING FLOWS
!       IN S/R rdresv.

!       READ RELEASES
!       THE RESERVOIR OUTFLOWS ARE ALL READ IN THE FIRST TIME
!       rdresv IS CALLED. THEY ARE THEN STORED AND USED EACH TIME
!       rdresv IS CALLED.

!       IF NATURAL CONTROL, FLOWS ARE SET TO -1.0 ABOVE
         if(iopt.eq.2)print*,'in read_resv_ef passed 304'
        
      endif                     !   if(noresv.gt.0)

      if(iopt.eq.2)print*,'in read_resv_ef passed 551'

!     WE CAN'T HAVE -VE RELEASES WHEN WE START
      do k=1,noresv
! V. Fortin: changed ktr into 1
!!        if(qrel(k,ktr).lt.0.0)qrel(k,ktr)=0.001
!         if(qrel(k,1).lt.0.0) qrel(k,1)=0.001
! D. Durnford: when releases are being specified and an observed streamflow value
! is missing for the first hour, a negative value is provided.
! In that situation, the initializing value of qo2 is used.
         if(qrel(k,1).lt.0.0 .and. b1(k).eq.0.0 .and. b2(k).eq.0.0)
     *      qrel(k,1)=qo2(resindex(k))
      end do
!   
!     SET FUTURE RELEASES = LAST NON-NEGATIVE RELEASE
!     REALLY, WE'RE WORKING IN HOURLY INTERVALS ALTHOUGH THE      
!     RELEASES MAY BE READ IN ONLY WHEN THE RES OUTFLOW IS CHANGED.

      if(noresv.gt.0)then       !  <<<<<<<<???????????????????
!       fill in missing data (eg. if ktr > 1)
         do j=2,nrel
            do k=1,noresv
               if(qrel(k,j).lt.0.0)qrel(k,j)=qrel(k,j-1)
            end do
         end do
      else                      ! if(noresv.gt.0)
         do k=1,noresv
            do j=2,nrel
               qrel(k,j)=0.0
            end do
         end do
      endif                     ! if(noresv.gt.0)

      if(iopt.ge.2)then
         do k=1,noresv
            write(52,6801)k,nrel
            write(52,6802)(qrel(k,j),j=1,nrel)
         end do	
      endif
      if(iopt.eq.2)print*,'in read_resv_ef passed 678'

!check to see that this does not have to go to flowint !!!!!!!!!!1
!     REVISED JAN 17/96

      if(id.eq.1)then
         do k=1,noresv
!         rev. 9.1.72  Dec.  28/04  - NK: fix bug in rdresv setting reach # 
            i=ires(k)
            j=jres(k)
            n=s(i,j)
            ! csubich -- this appears to be a dead block of code,
            ! since inbsnflg is allocated above but never assigned.
            if(inbsnflg(no+k).eq.1)then
!     v. 8.99n  Dec. 31/2001-     fixed nat. res initial flow (JW)
!           this was fixed in spl8 on dec. 31/01
               if(b1(k).le.0.0)then
                  if(n.eq.0)then
                     print*,'reservoir no',k,' at row',i,' column',j 
                     print*,'is not in the basin or'
                     print*,'is in a grid with zero area: please check'
                     print*
                     STOP 'program aborted in read_resv_ef @ 001'
                  endif
                  qo1(n)=qrel(k,1)
                  qo2(n)=qrel(k,1)
               endif
!           FOR RESERVOIRS WITH RULES, IREACH IS READ IN FOR THE 
!           DWOPER FORMAT BUT WHEN NO DWOPER REACHES ARE SPECIFIED
!           IREACH HAS TO BE DEFINED HERE.              
!           read_resv_ef WON'T BE CALLED AGAIN IF IREACH = 0 
            endif               ! inbsnflg
!         rev. 9.1.72  Dec.  28/04  - NK: fix bug in rdresv setting reach # 
            ireach(n)=k
         end do
      endif

      if(iopt.eq.2)print*,'in read_resv_ef passed 612'

      close(unit=unitNum,status='keep')

!      WRITE(6,*) "noresv:", noresv
!      DO j=1,nrel
!         WRITE(6,191) j, (qrel(k,j), k=1,noresv)
!      END DO
! 191  FORMAT('hour:',1X,I4,1X,'qrel:',7(2X,E10.4))

 999  RETURN                    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-----

! FORMATS

 500  format(256f10.3)
 1011 format(' ',3x,'  i  ires(i) jres(i)    b1(i)     b2(i)',
     *     '    b3(i)     b4(i)')
 1013 format(' ',3x,i3,2i8,7e12.3,a12/)
 6801 format('   read_resv_ef: reservoir no =',i3,' mhtot =',i5)
 6802 format('   ',256f8.2)

      END SUBROUTINE read_resv_ef

