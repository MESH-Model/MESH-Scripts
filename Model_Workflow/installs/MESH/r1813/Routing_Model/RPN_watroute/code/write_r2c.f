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

      SUBROUTINE write_r2c(un,fn,
     *            no_frames,no_classes,frame_no,class_no,
     *            no_signf)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!     rev. 9.1.64  Oct.  03/04  - NK: Coded up new header in ragmet.for

!***********************************************************************
! - THIS SUBROUTINE OUTPUTS and r2c file.

! - List of arguments:

!   I - un      int        unit number
!   I - fn      int        file number
!   I - itogo   int        no. of hours until next rainfall
!   R - unit_conversion    REAL*4     conversion factor (area2)
!   I - FLN     CHAR*12    file names
!   I - DIR     CHAR*12    directory name
!   I - ocflg   int        open_close flag 1 open file -1 close file
!   I - frmflg  int        frame flag  1 new frame -1 end frame  
!                          0 each call = 1 frame with frame marks
!***********************************************************************


      use area_watflood
	implicit none


      INTEGER       :: un,fn,ii,no_signf,hour_no,hours_togo,
     *                 no_frames,no_classes,frame_no,class_no,
     *                 i,j,ios
	character(20) :: junk
      CHARACTER(10) :: time
      CHARACTER(8)  :: cday

!     FIRST TIME THROUGH THIS SUBROUTINE ONLY
!     OPEN OUTPUT FILE AND WRITE HEADER

      hour_no=frame_no

!     user notes

!     To write the header, set frame_no = 0
!     To write the data, set the frame > 1

!     this s/r will only allow:
!     multiple frames for 1 class
!                or
!     1 frame for multiple classes
      if(no_frames.gt.1.and.no_classes.gt.1)then
        print*,'Programming error'
        print*,'no_frames > 1  and  no_classes > 1'
        print*,'This is not allowed'
        print*
        stop 'Program aborted due to programming error'
!       This can only be cause by misuse of this s/r 
!         in the calling program
      endif

!      if(frame_no.eq.1.and.class_no.eq.1)then

      if(frame_no.eq.0)then

!       write the header ONLY

!       FILE NAMES AND UNIT NUMBERS DIFFER BY 30
!        write(*,1400)fn,fln(fn)
! 1400   format(' opening fln(',i3,'):',a999,'---')
!        write(*,*)

        open(unit=un,file=fln(fn),status='unknown',iostat=ios)
	  print*,' un fn et fln(fn) ',un,fn,fln(fn) !ido
	  print*,'Opened unit=',un,'filename',fln(fn)
	  if(ios.ne.0)then
	    print*,'Error opening ',fln(fn),' on unit=',un
	    print*,'ios = ',ios
	    print*
	    stop 'in write_r2c @ 83'
	  endif 
c	  print*,'Opened unit=',un,' filename=',fln(fn)
        write(un,3005)'########################################'
        write(un,3005)':FileType r2c  ASCII  EnSim 1.0         '
        write(un,3005)'#                                       '
	  write(un,3005)'# DataType               2D Rect Cell   '
        write(un,3005)'#                                       '
        write(un,3005)':Application             EnSimHydrologic'
	  write(un,3005)':Version                 2.1.23         '
	  write(un,3020)':WrittenBy          ',author
        call date_and_time(cday,time)
	  write(un,3010)':CreationDate       ',
     *       cday(1:4),cday(5:6),cday(7:8),time(1:2),time(3:4)
3010  format(a20,a4,'-',a2,'-',a2,2x,a2,':',a2)
        write(un,3005)'#                                       '
	  write(un,3005)'#---------------------------------------'
        write(un,3005)'#                                       '
	  write(un,3020)':Name               ',name
        write(un,3005)'#                                       '
	  write(un,3004)':Projection         ',coordsys_temp
	  if(coordsys_temp.eq.'LATLONG   ')then
	    write(un,3004)':Ellipsoid          ',datum_temp
	  endif
	  if(coordsys_temp.eq.'UTM       ')then
	    write(un,3004)':Ellipsoid          ',datum_temp
          write(un,3004)':Zone               ',zone_temp
	  endif
        write(un,3005)'#                                       '
        write(un,3003)':xOrigin            ',xorigin_temp
        write(un,3003)':yOrigin            ',yorigin_temp
        write(un,3005)'#                                       '
        write(un,3020)':SourceFile         ',source_file_name
        write(un,3005)'#                                       '
        if(no_frames.eq.1.and.no_classes.ge.1)then
          do i=1,no_classes
      	    write(un,3007)':AttributeName',i,' Class',i
	    end do
          write(un,3005)'#                                       '
	  else
          write(un,3020)':AttributeName 1    ',attribute_name
          write(un,3020)':AttributeUnits     ',attribute_units  
c!         see note below @***       
        endif
        write(un,3005)'#                                       '
        write(un,3001)':xCount             ',xcount_temp
        write(un,3001)':yCount             ',ycount_temp
        write(un,3003)':xDelta             ',xdelta_temp
        write(un,3003)':yDelta             ',ydelta_temp
c        if(no_frames.eq.1)then
        if(no_frames.le.1)then
          write(un,3005)'#                                       '
          write(un,3004)':SampleTime         ',startdate,starttime
!          write(un,3004)':StartDate          '
        endif
        write(un,3005)'#                                       '
        if(unit_conversion.ne.0.0)then
          write(un,3003)':UnitConverson      ',unit_conversion
        endif
        if(name.eq.'Snow Water Equivalent                   ')then
	    write(un,3003)':InitHeatDeficit    ',init_heat_deficit
        endif
        write(un,3005)'#                                       '
     	  write(un,3005)':endHeader                              '

        return

	endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! fix fix

!       this is just to read in the new format
!       still needs to be programmed for multiple classes

!     the header is written only the first time through
!     this part is called with each class

!     :Frame and :EndFrame lines are written only for time series

      if(data_source.eq.'     ')data_source=source  ! source from area2

!      if(no_frames.gt.1)write(un,3011)':Frame',frame_no,frame_no,
!     *     year1,mo1,day1,hour1,'Hour=',hour_no,hours_togo,data_source

      if(hour_no.ge.1)then

c        if(hour_now.eq.0)then
c          day_now=day_now-1
c	    hour_now=24
c	  endif

        if(mo1.le.9.and.day_now.le.9)then
          if(no_frames.gt.1)write(un,3021)':Frame',
     *     abs(frame_no),abs(frame_no),year1,month_now,day_now,hour_now
 3021      format(a6,2i10,3x,'"',i4,'/',i1,'/',i1,1x,i2,
     *                   ':00:00.000"',2x,a5,2i5,2x,a5)

        elseif(mo1.le.9.and.day_now.gt.9)then
          if(no_frames.gt.1)write(un,3022)':Frame',
     *     abs(frame_no),abs(frame_no),year1,month_now,day_now,hour_now
 3022      format(a6,2i10,3x,'"',i4,'/',i1,'/',i2,1x,i2,
     *                   ':00:00.000"',2x,a5,2i5,2x,a5)

        elseif(mo1.gt.9.and.day_now.le.9)then
          if(no_frames.gt.1)write(un,3023)':Frame',
     *     abs(frame_no),abs(frame_no),year1,month_now,day_now,hour_now
 3023      format(a6,2i10,3x,'"',i4,'/',i2,'/',i1,1x,i2,
     *                   ':00:00.000"',2x,a5,2i5,2x,a5)
        else
          if(no_frames.gt.1)write(un,3024)':Frame',
     *     abs(frame_no),abs(frame_no),year1,month_now,day_now,hour_now
 3024      format(a6,2i10,3x,'"',i4,'/',i2,'/',i2,1x,i2,
     *                   ':00:00.000"',2x,a5,2i5,2x,a5)
        endif

!       NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE 
!       The r2c grids are written upside down: 
!                 south on top, north on bottom    !!!!!!!!!!

        if(no_signf.eq.0)then
          do i=1,ycount_temp
            write(un,1300)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.1)then   ! swe, precip and temp
          do i=1,ycount_temp
            write(un,1301)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.2)then   ! swe, precip and temp
          do i=1,ycount_temp
            write(un,1302)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.3)then   ! swe, precip and temp
          do i=1,ycount_temp
            write(un,1303)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.4)then   ! swe, precip and temp
          do i=1,ycount_temp
            write(un,1304)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.5)then   ! swe, precip and temp
          do i=1,ycount_temp
            write(un,1305)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.6)then   ! swe, precip and temp
          do i=1,ycount_temp
            write(un,1306)(outarray(i,j),j=1,xcount_temp)
          end do          
        elseif(no_signf.eq.8)then   ! flow
          do i=1,ycount_temp
            write(un,1308)(outarray(i,j),j=1,xcount_temp)
          end do          
        else                        ! init soil moisture
          do i=1,ycount_temp
            write(un,1307)(outarray(i,j),j=1,xcount_temp)
          end do          
        endif

        if(no_classes.eq.1)write(un,3012)':EndFrame'

      endif

      if(frame_no.eq.no_frames.and.class_no.eq.no_classes)then
        close(unit=un,status='keep')
        write(51,*)'Closed unit ',un,' Filename=  ',fln(fn)
        write(*,*)'Closed unit ',un,' Filename=  ',fln(fn)
	endif

      return

! FORMATS
1300  format(9999(1x,f5.0))
1301  format(9999(1x,f5.1))
1302  format(9999(1x,f5.2))
1303  format(9999(1x,f6.3))
1304  format(9999(1x,f7.4))
1305  format(9999(1x,f8.5))
1306  format(9999(1x,f9.6))
1307  format(9999(1x,e13.6))
!1307  format(9999(1x,e12.6))  ! the intel compiler + 15.1 libraries recommended that the field width >= # decimal digits+7
1308  format(9999(1x,e10.3))
 3000 format(a10,i5)
 3001 format(a20,i16)
 3002 format(2a20)
 3003 format(a20,f16.7)
 3004 format(a20,a10,2x,a10)
 3005 format(a40)
 3006 format(a3,a10)
 3007 format(a14,i5,a6,i5)
 3012 format(a9)
 3020 format(a20,a40)
     
       END SUBROUTINE write_r2c









