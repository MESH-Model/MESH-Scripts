!    This file is part of WATROUTE.
!
!    WATROUTE is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    WATROUTE is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public License
!    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.

      SUBROUTINE write_flowinit()

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!     rev. 9.3.05  Nov.  13/06  - NK: added write_flowinit.for to flowinit.for

!     "author" must be specified before calling this s/r

      use area_watflood
      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      logical    :: exists
      integer(4)    :: i,j,n,ios,k
      CHARACTER(30) :: sys_command
      CHARACTER(10) :: time
      CHARACTER(8)  :: cday
      character(1)  :: answer

!     write the init file for watroute in r2c format

      inquire(FILE='flow_init.r2c',EXIST=exists)

!      if(exists)then
!        print*
!	  print*,'The file flow_init.r2c already exists'
!	  print*,'Do you want to overwrite it?  y/n'
!	  print*
!	  print*,'If you answer n, a new file flow_init_new.r2c'
!	  print*,'will be written'
!	  read*,answer
!	else
!	  answer='y'
!	endif
!
!      if(answer.eq.'y')then
!        if you actually want a new flow_init.r2c, delete the
!        existing on on the calling program

        fln(99)='flow_init.r2c'
        open(99,file='flow_init.r2c',status='unknown',iostat=ios)
        if(ios.ne.0)then
          print*,' Problems opening the flow_init.r2c file '
          print*
          STOP ' Program write_flowinit aborted @ 52'
        endif
!      else  
!	 fln(99)='flow_init_new.r2c'
!        open(99,file='flow_init_new.r2c',status='unknown',iostat=ios)
!        if(ios.ne.0)then
!          print*,' Problems opening the flow_init_new.r2c file '
!          print*
!          STOP ' Program write_flowinit aborted @ 60'
!        endif
!      endif

      write(99,3005)'########################################'
      write(99,3005)':FileType r2c  ASCII  EnSim 1.0         '
      write(99,3005)'#                                       '
      write(99,3005)'# DataType               2D Rect Cell   '
      write(99,3005)'#                                       '
      write(99,3005)':Application             EnSimHydrologic'
      write(99,3005)':Version                 2.1.23         '
      write(99,3002)':WrittenBy          ',author
      call date_and_time(cday,time)
      write(99,3010)':CreationDate       ',
     *       cday(1:4),cday(5:6),cday(7:8),time(1:2),time(3:4)
3010  format(a20,a4,'/',a2,'/',a2,2x,a2,':',a2)
      write(99,3005)'#                                       '
      write(99,3005)'#---------------------------------------'
      write(99,3020)':SourceFileName     ',fln(6)
      write(99,3005)'#                                       '
      write(99,3004)':Projection         ',coordsys1
        if(coordsys1.eq.'UTM       ')then
          write(99,3004)':Zone               ',zone1
          write(99,3004)':Ellipsoid          ',datum1
        endif
        if(coordsys1.eq.'LATLONG   ')then
          write(99,3004)':Ellipsoid          ',datum1
        endif
      write(99,3005)'#                                       '
      write(99,3003)':xOrigin            ',xorigin
      write(99,3003)':yOrigin            ',yorigin
      write(99,3005)'#                                       '
      write(99,3008)':AttributeName 1 qi1          ' 
      write(99,3008)':AttributeName 2 qo1          '  
      write(99,3008)':AttributeName 3 qo1(sim)     '
      write(99,3008)':AttributeName 4 qo1(rem)     '
      write(99,3008)':AttributeName 5 store1       '
      write(99,3008)':AttributeName 6 over         ' 
      write(99,3008)':AttributeName 7 lzs          ' 
      write(99,3005)'#                                       '
      write(99,3001)':xCount             ',xcount
      write(99,3001)':yCount             ',ycount
      write(99,3003)':xDelta             ',xdelta
      write(99,3003)':yDelta             ',ydelta
      write(99,3005)'#                                       '
      write(99,3005)':EndHeader                              '

 3000 format(a10,i5)
 3001 format(a20,i16)
 3002 format(2a20)
 3003 format(a20,f16.7)
 3004 format(a20,a10,2x,a10)
 3005 format(a40)
 3006 format(a3,a10)
 3007 format(a14,i5,a6,i5)
 3008 format(a30)
! 3012 format(a9)
 3020 format(a20,a999)

      if(iopt.eq.2)print*, 'in write_flowinit at 1302'

!     initialize p() to make sure there is no junk in the unused grids
      do i=1,ycount
          do j=1,xcount
            p(i,j)=0.0
          end do
      end do
        
      k=0

!     Initial grid inflow
      k=k+1
!	p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=qi2(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

!     Initial grid outflow in m^3/s
      k=k+1
!	p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=qo2(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

!     Initial grid outflow (simulated) in m^3/s
      k=k+1
!       p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=qo2sim(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

!     Initial grid outflow (removed by diversion) in m^3/s
      k=k+1
!       p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=qo2rem(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

!     Initial grid channel storage in m^3
      k=k+1
!	p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=store2(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

!     Initial overbank storage in m^3 (used as conditional so include here)
      k=k+1
!	p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=over(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

!     Initial lower zone storage in mm
      k=k+1
!	p(1,1)=float(k)
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        p(i,j)=lzs(n)
      end do
      do i=1,ycount
        write(99,4009)(p(i,j),j=1,xcount)
      end do

      close(unit=99,status='keep')

      print*
      print*,fln(99),' written in working directory'
      print*

 4003 format(999(' ',i5))
 4009 format(999(' ',e10.3))

      return

      end SUBROUTINE write_flowinit

