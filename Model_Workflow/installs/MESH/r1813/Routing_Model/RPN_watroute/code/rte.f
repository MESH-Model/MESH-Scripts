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

        PROGRAM rte   !watroute
 
!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!           the use of the main program is only to dimension the
!           variables and to decide a few options.
!
!           note that the dimensions of a variable must be the
!           same in all parts of the programs. eg: if qsyn is
!           dimensioned to (1,8,123) in the main calling program,
!           then is it is dimensioned qsyn(ls,js,ih), ls must be
!           set =1, js=8, & ih=123
!
!           the variables qi1 and qi2 have to be dimensioned to
!           the value of na while all the other variables can be
!           dimensioned to naa.  this is to allow the water to
!           run into something at the outlet.

!     rev. 9.4.01  Apr.  17/07  - NK: added deltat_report for gridflow.r2c
!     D. Durnford Dec 2013
!     -- concentrated the output file units to make space for up to 15 reaches
! Eliminated those output file names since need more than 15 reaches and didn't need the output files
! Max number of reaches is now 45
!     -- eliminated the writing of outfiles_rte.new containing the default input file names
!     -- added the reading in of the input file names

      use area_watflood

      USE EF_Module

      ! The fst-based IO is included in a module; this allows
      ! for token stubs to be included if building on a system
      ! without the appropriate libraries
      use fst_io

      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      CHARACTER(79)   :: comment
      CHARACTER*14    :: date
      CHARACTER(2)    :: aa1
      CHARACTER(1)    :: linetype
      integer*2 result1
      CHARACTER*1028 ApiPacket
      CHARACTER*128   QSTR
      INTEGER*2      status, RESP, len
      integer    ::  io_err,sys_err,stat,unit,cond,nj,iallocate,
     *               ios,nhr,nhf,linecount,ideallocate,iendarg,ix,
     *               file_number,i,j,ii,icase
      real*4     ::  conv,scale,smc5(16),cintv,e1
      character(20) :: junk
      CHARACTER(10) :: time
      CHARACTER(8)  :: cday
      INTEGER(2) status1
      CHARACTER(1) buf,stopflg,ensimopenflg
      logical       :: exists
      CHARACTER(1)  :: smok 
      INTEGER    :: iallcnt,icnt(5),ndir(5),nchr,
     *              igrdshft,iyshiftmin,iyshiftmax,
     *              jxshiftmin,jxshiftmax,ishift,jshift,inum,jnum,
     *              l,iw1,iw2,iv,iflg
      REAL(4)    :: errold(5),err(5),chng(5),best(5)
      REAL(4)    :: optlow,ddtemp,cc1,cc2,crit,best1
      real(4)    :: optlast
      REAL(4)    :: dtminusr,mindtmin,convthreshusr
      integer*2  :: ntest
!vfo
      integer iargc
!      EXTERNAL iargc

      integer   :: ios2,iswitch
      character :: my_fmt5001*30,fend*3

      DATA ntest/43814/qstr/'optionsss'/nchr/9/
      DATA iallcnt/0/

      program_name='rte'

        if(iargc().lt.1)then
          buf='0'
        else  
          CALL GETARG(1, buf)
        endif  

       if(buf.eq.'0')then
         iopt=0
       elseif(buf.eq.'1')then
         iopt=1
         stopflg='y'
       elseif(buf.eq.'2')then
         iopt=2
       elseif(buf.eq.'3')then
         iopt=2
       elseif(buf.eq.'4')then
         iopt=2
       elseif(buf.eq.'9')then
!       rev. 9.1.63  Sep.  29/04  - NK: Added iopt_start as an arg for quick filecheck
        iopt_start=99
       endif

!     THIS SECTION GETS THE COMPUTER'S DATE AND TIME
!     THIS INFO USED TO COME FROM HEADER.FI, HOWEVER F90 INTRINSICS
!     AND CALLS ARE DIFFERENT AND THEREFORE IT NEEDED TO BE MODFIED.
c     call date_time(cday,time)

!     INPUT FILES ARE UNITS 30-50
!     THESE ARE OPENED MOSTLY IN SUB.F BECAUSE INPUT FILES
!     HAVE TO BE OPENED AND CLOSED FOR EACH MONTH IN THE ID LOOP

!     THE RESERVOIR INPUT FILE WAS ORIGINALLY 49 BUT THIS
!     WAS CHANGED WHEN TODD NEFF's EVAPORATION WAS ADDED

!!!!!!!!!!!!!!!!!!!!!!!!!!
!     READ IN THE MAXIMUM VALUE OF THE TIME STEP
      write(*,'(a35,a39)') 'maximum/minimum time step (s) and ',
     *           'convergence level for channel routing?'
      read (*,*,IOSTAT=status) dtminusr,mindtmin,convthreshusr
      if (status > 0) then
	write(*,*) 'the values specified for maxtimestep, ',
     * 'mintimestep, and/or conv. threshold may be inappropriate;',
     * ' check watroute.cfg file in your experiment'
        stop
      else if (status < 0) then
        write(*,*) 'the number of arguments passed to',  
     *   '  watroute is wrong: it should be 3;',
     * ' check watroute.cfg file in your experiment' 
        stop
      end if

      write(*,'(a31,a22,2f7.2,a2)') 'The maximum and minimum values ',
     *  'of the time step are: ',dtminusr,mindtmin,' s'
      write(*,'(a29,a26,f10.6)') 'The level of convergence for ',
     *  'channel routing is set at ',convthreshusr
      write(*,*) ''

!     ALSO USED IN SHED: UNIT 9 FOR FLN(6)

      translateflg='n'  ! used on translate.for if 'y'
      ittoflg=0
      ssmc_firstpass='y'
      flgevp2=-1.0
      ensimopenflg='n'

!     TS - ALLOCATION FOR AREA12A ARRAYS
      allocate(fln(999),filename(999),outfln(999),infln(999),
     *         stat=iAllocate)
      if (iAllocate.ne.0) STOP 
     *    'Warning: error with allocation of area12a arrays in spl9' 

! SET DEFAULT FILENAMES FOR OUTPUT FILES:
!     these names may be replaced with the outfiles_rte.txt file in the working
!     directory to send the files to a designated place for output.
!     A default outfiles.new file is created in the working directory each 
!     time this program is run.

      do i=51,100
	  filename(i)='..'
      end do

!vfo
       filename(51)='rte.txt'      !program information
       filename(52)='res.txt'      !reservoir data
       filename(53)='route.txt'    !routing data
       filename(54)='spl_rpn.csv'  !paired observed/computed 
       filename(55)='wetland.csv'  !wetland info
       filename(56)='gridflow.r2c' 
       fln(56)=filename(56)        ! write_r2c used fln()
       filename(57)='resin.txt'    !lake inflow obs/computed
       filename(58)='lake_sd.csv'  !  reservoir storage output
!dch
!       filename(59)='net_lake_inflow.csv' !removed: net_lake values assume that reaches are in a chain
       filename(59)='gridflow.fst'
       filename(60)='rbm_input.fst'
       filename(61)='flow_init.fst'
!       filename(62)= ! unused
!       filename(63)= ! unused
!       filename(64)= ! unused
!       filename(65)= ! unused
!       filename(66)= ! unused
!       filename(67)= ! unused
!       filename(68)= ! unused
!       filename(69)= ! unused
!       filename(70)= ! unused
!       filename(71)= ! unused
!       filename(72)= ! unused
!       filename(73)= ! unused
!       filename(74)= ! unused
!       filename(75)= ! unused
!       filename(76)= ! unused
!       filename(77)= ! unused
!       filename(78)= ! unused
!       filename(79)= ! unused
!       filename(80)= ! unused
!       filename(81)= ! unused
!       filename(82)= ! unused
!       filename(83)= ! unused
!       filename(84)= ! unused
!       filename(85)= ! unused
!       filename(86)= ! unused
!       filename(87)= ! unused
!       filename(88)= ! unused
!       filename(89)= ! unused
!       filename(90)= ! unused
!       filename(91)= ! unused
!       filename(92)= ! unused
!       filename(93)= ! unused
!       filename(94)= ! unused
!       filename(95)= ! unused
!       filename(96)= ! unused
!       filename(97)= ! unused
       filename(98)='rte_info.txt'
       filename(99)='scratch5'                       ! reserved as scratch file
       filename(100)='scratch6'                      ! not used

 ! WRITE A NEW OUTFILES.TXT FILE THAT CAN BE MODIFIED BY THE USER:
c      open(unit=99,file='outfiles_rte.new',form='formatted',
c     *     status='unknown',iostat=ios)
c      if(ios.eq.0)then
c        write(99,99002)(filename(i),i=51,100)
c        close(unit=99,status='keep')
c      else
c	print*,' error opening outfiles.new'
c	print*,' new file not written'
c	print*
c      endif

! THIS INCLUDE OPENS THE SIMOUT FILES AND WILL BE DIFFERENT FOR UNIX 
! OR DOS\WINDOWS

      iendarg=0

!     id is used as a flag. when id=0 openfiles:
      id=1              !not used like this now  nk Apr. 8/03
      ni=1

! THE OUTFILES.TXT FILE READS THE NAMES OF ALL THE OUTPUT FILES
      ioflg=0 ! # of output files listed in outfiles_rte.txt
      INQUIRE(FILE='outfiles_rte.txt',EXIST=exists)
      if(exists)then
        open(unit=99,file='outfiles_rte.txt',
     *               status='old',iostat=ios)
        if(ios.eq.0)then
!         AN OUTFILES.TXT FILE EXISTS AND WE'LL READ IT:
          print*,'reading outfile names from outfiles_rte.txt'
          write(my_fmt5001, '( "(a", i3, ")" )' )  len(outfln)
          do i=51,100
            read(99,my_fmt5001,iostat=ios)outfln(i)
            if ( i .eq. 51) then
!              if(ioflg.gt.1)then
              filename(i)=outfln(i)
!              endif
              open(unit=51,file=filename(51),status='unknown',
     *             iostat=ios)
              write(51,*)'Outfile names from fln: outfiles_rte.txt' 
            end if
            write(51,'(a)') trim(outfln(i))
!            write(51,my_fmt5001) outfln(i)
            if(ios.ne.0)then 
              print*,'Problems on unit 99'
              print*,'Warning: error reading file name outfiles.txt'
              print*,'possible cause: existing file is read-only'
              print*,'or end of file is reached - need 50 lines'
              print*,'iostat code =',ios
              print*,'Got as far as line ',i-50
              STOP 'program aborted in spl.for @ 345'
            endif
            ioflg=i
          end do
          close(unit=99)
          fln(56)=outfln(56)               ! gridflow.r2c:  write_r2c used fln()
        else
          print*,'Error opening outfiles.txt'
          print*,'Continuing using default output files'
        endif 
      else
       print*,'outfiles.txt file not found, defaults used'
       print*
      endif

! DURING EXECUTION, THE 'STOP' COMMAND CAN BE MADE FROM ANOTHER DOS
! WINDOW. IT WILL CHECK AT THE END OF EACH EVENT. THIS WILL CLOSE
! ALL FILES PROPERLY AS OPPOSED TO THE PROBLEMS WITH A CTRL/BRK CRASH
      open(unit=99,file='stop.txt',form='formatted',status='unknown',
     *               iostat=ios)
      if(ios.eq.0)then
        write(99,99001)
        close(unit=99,status='keep')
      else
	print*,' error opening stop.txt'
	print*,' new file not written'
	print*
      endif


! OPEN FILE FOR ALL SPL ERROR MESSAGES:
      if(ioflg.gt.1)then
        filename(98)=outfln(98)
      endif
   
      open(unit=98,file=filename(98),status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(ios.ne.0)call io_warning(98,filename(98),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call date_and_time(cday,time)
      write(98,6016)time(1:2),time(3:4),time(5:6)
      write(98,6017)cday(1:4),cday(5:6),cday(7:8)
      write(98,5005)


! Read in the names of input files
!       filename(1)='event.evt'
!       filename(2) 'flow_init.fst'  ! The filename index of flow_init.fst  is estabished in rte_sub.f    as a function of Nreaches
!       filename(3) 'sfcmod2wat.fst' ! The filename index of sfcmod2wat.fst is estabished in read_fst.f90 as a function of Nreaches
!       filename(4)= ! unused IMPORTANT: LEAVE IT UNUSED
!       filename(5)='reach1_lvl.txt'
!       filename(6)='reach2_lvl.txt'
!       filename(7)='reach3_lvl.txt'
!       filename(8)='reach4_lvl.txt'
!       filename(9)='reach5_lvl.txt'
!       filename(10)='reach6_lvl.txt'
!       filename(11)='reach7_lvl.txt'
!       filename(12)='reach8_lvl.txt'
!       filename(13)='reach9_lvl.txt'         ! MAX 45 REACHES PERMITTED
!       filename(Nreaches+5)=                 ! unused IMPORTANT: LEAVE IT UNUSED
!       filename(Nreaches+6 to 50)=           ! unused: CAN BE USED

      INQUIRE(FILE='infiles_rte.txt',EXIST=exists)
      if(exists)then
        open(unit=99,file='infiles_rte.txt',
     *               status='old',iostat=ios)
        if(ios.eq.0)then
!         AN INFILES.TXT FILE EXISTS AND WE'LL READ IT:
          print*,'reading infile names from infiles_rte.txt'
          iswitch = 0
          do i=1,50
            read(99,my_fmt5001,iostat=ios)infln(i)
            if(ios.eq.0)then 
              if (iswitch.eq.0 .and. len(trim(infln(i))).le.2) then
                Nreaches = 0
                iswitch = 1
              else if (iswitch.eq.1 .and. len(trim(infln(i))).gt.2) then
                Nreaches = Nreaches + 1
              else if (iswitch.eq.1 .and. len(trim(infln(i))).le.2) then
                iswitch = 2
              end if 
            else
              print*,'Problems on unit 99'
              print*,'Warning: error reading file name infiles.txt'
              print*,'possible cause: end of file is reached'
              print*,' - need 50 lines'
              print*,'iostat code =',ios
              print*,'Got as far as line ',i-100
              STOP 'program aborted in spl.for @ 345b'
            endif
            ioflg=i
          end do
          if (Nreaches .gt. Nreachesmax) then
            write(*,*) 'The current setup can have no more than ',
     *                  Nreachesmax,' reaches'
            write(*,*) 'The calculated # reaches is: ',Nreaches
          end if
          close(unit=99)
        else
           stop 'Unable to read infiles_rte.txt'
        end if
      else
         stop 'Unable to find infiles_rte.txt'
      end if

      i=51
      open(unit=i,file=filename(i),access='sequential',
     *    status='unknown',iostat=ios)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(ios.ne.0)call io_warning(i,filename(i),ios)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      call date_and_time(cday,time)
      write(51,*)
      write(51,6016)time(1:2),time(3:4),time(5:6)
      write(51,6017)cday(1:4),cday(5:6),cday(7:8)
      write(51,*)
      write(51,5003)i,filename(i)

!      write(51,5002)
!      write(51,1030)(i,i,filename(i),i=51,100)

      if (len(infln(1)) .gt. 2) then
        fln(99)=infln(1)
      else
        fln(99)='event/event.evt'
      end if

      if(iopt.eq.2)print*, 'In spl: 1 - before rdevt call'

!**********************************************************************
      call rdevt(date,conv,scale,smc5,nhr,nhf)
!**********************************************************************


! ORIGINAL FILEIO.FI SECTION - STARTS HERE: 

!   LEAVE FILEIO HERE BECAUSE rdevtA READS FILE NAMES THAT HAVE TO BE OPENED

!   Rev. 7.78 modified for error checking - Sept.29/96  AC flight 
!   Rev  7.9  modified to open files for evaporation output
!   Rev. 8.3  - May.  22/97 -     added the simout/outfiles capability

!   THIS IS TO OPEN AND READ THE OPTIONAL OUTFILES.TXT FILE THAT SETS
!   THE LOCATIONS OF THE OUTPUT FILES.  IF FILE DOES NOT EXIST, DEFAULT
!   NAMES WILL BE USED.

      write(51,5002)
      do i=52,100
        if(ioflg.gt.1 .and. len(trim(outfln(i))).gt.2)then
          filename(i)=outfln(i)
          fend = outfln(i)(len(trim(outfln(i)))-2:len(trim(outfln(i))))
          if (fend /= 'fst') then
            if (i .eq. 54) then
              inquire(file=filename(i),exist=exists)
              if (.not. exists) then
                 open(unit=i,file=filename(i),status='new',iostat=ios)
                 first_run = .true.
              else
                 open(unit=i,file=filename(i),position='append',
     *                status='old',iostat=ios)
              end if
            else
              open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
            end if
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if(ios.ne.0)call io_warning(i,filename(i),ios)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          endif
          write(51,5003)i, trim(filename(i))
        endif
      end do

      if(iopt.eq.2)print*, ' In spl -1001'

      write(51,6015)
     *  time(1:2),time(3:4),time(5:6),cday(1:4),cday(5:6),cday(7:8)

99905 write(51,1000)trim(fln(10)),trim(fln(1))
      write(52,1000)trim(fln(10)),trim(fln(1))

      if(iopt.eq.2)print*, ' In spl - 1183'
 
!     SHED READS IN ALL THE WATERSHED DATA, FROM SEPERATE PROGRAM:
 
      if(iopt.eq.2)print*, ' In spl - before call shed'

      if (fstflg .eq. 'y') then
         call read_shed_fst(31,fln(1))
      else
         call read_shed_ef(31,1)
         call read_shed_ef(271,41)
      endif

      if(iopt.eq.2)print*, ' In spl - before allocate'

        allocate(qrgrid(ycount+10,xcount+10),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *   'Error with allocation of area16a arrays in spl9'

!       Initialize these values because many are outside grid
!       and would otherwise be undifined.  nk June 11/03
        do i=1,ycount+10
          do j=1,xcount+10
            qrgrid(i,j)=0.0
          end do
        end do

        allocate(nhyd(ycount,xcount),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *   'Error with allocation of area6a arrays in spl9'

        allocate(p(ycount,xcount),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *   'Error with allocation of p() array in spl9'

      mo=mo1
      numa=0

!     WARNING: DON'T COMBINE NEXT SEGMENT WITH PREVIOUS SEGMENT 
!     BECAUSE IGRDSHFT CAN BE SET = 0 IN THE FILE (i.e. NO IOS ERROR)

      maxn=1

      if(iopt.eq.2)print*, ' In options: 4 - before call sub'

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call sub(e1,smc5,scale,icase,smok,optlow,igrdshft,
     *         dtminusr,mindtmin,convthreshusr)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      nnn=nnn+1

      if(iopt.eq.2)print*,'in spl9 @1'

      stop 'watroute: normal ending'

! FORMATS

 1000 format(' ',2(' ',a))
 1030 format(' ','Unit no. =',i3,' file no',i3,' = ',a999)
 5001 format(a999)
 5002 format(/' output files')
 5003 format(' opened unit'i5,' file name ',a)
 5005 format(' Files opened:')
 6015 format(' runtime  ',a2,':',a2,':',a2,2x,a4,'-',a2,'-',a2)
 6016 format('  runtime    ',2(a2,':'),a2)
 6017 format('  rundate  ',a4,'-',a2,'-',a2)
99001 format('  0.0')
99002 format(a999)

      END PROGRAM rte     
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      subroutine io_warning(unit_number,file_name,ios)

      use area_watflood

      integer  :: unit_number,ios
      character*999  :: file_name

      print*,'Problems on unit',unit_number
      print*,'Warning:'
      print*,' error in opening file name:'
      write(*,'(x,a)') trim(file_name)
      print*,'possible cause: existing file is read-only'
      print*,'or folder does not exist    <<<'
      print*,'or file does not exist      <<<'
      print*,'or DISK does not exist      <<<'
      Print*
      print*,'iostat code =',ios
      print*
 
      STOP 'program aborted in io-warning.for (spl)  @1054'


      end subroutine io_warning

