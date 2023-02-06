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

      SUBROUTINE rdevt(date,conv,scale,smc5,nhg,nhf)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!***********************************************************************

! THIS SUBROUTINE INPUTS EVENT PARTICULARS.


!     REV. 8.71 - Feb.  24/98 -    added flgevp2 to rdevt.for
!     REV. 8.78 - July   7/98 -    added scalesnw and scaletem to rdevt
!     REV. 8.82 - July  10/98 -    added runoff output option: routeflg
!     REV. 8.94 - Feb.  01/99 -    crseflg to read resume & snow course
!     REV. 8.96.1 May   12/99 -    added ireport for reporting interval
!     REV. 9.00   March  2000 -    TS: CONvert version TO FORTRAN 90      
!     rev. 9.1.34  Dec.  23/02  - Added ensim1flg - if ensimflg='a' for 1st id then 'y' for all events
!     rev. 9.1.54  aPR.  12/04  - NK: SEDFLG set for multiple events at event No. 1
!     rev. 9.1.77  Mar.  07/05  - NK: added .psm .gsm & .glz  files
!     rev. 9.1.78  Mar.  15/05  - NK: added WQD file to event file
!     rev. 9.3.11  Feb.  28/07  - NK: ch_par added / event file ver = 9.5
!     rev mar05/07
!                  Oct.  19/12 - csubich: adding flags for fst runoff/recharge (9.8)

! - LIST OF ARGUMENTS:
 
!   o - date    char*14  event identifier
!   o - conv    real*4   conv version factor for precipitation inputs
!   o - smc     real*4   initial soil moisture content
!   o - fln( )  char*12  file names

! - LIST OF INPUT FILES:

! unit=31  fln(1) - basin file (bsnm_shd.r2c)
! unit=32  fln(2) - parameter file (bsnm.par)
! unit=33  fln(3) - point data location file (bsnm.pdl)
! unit=34  fln(4) - not used in spl
! unit=35  fln(5) - point precipitation file  yyyymmdd.rag
! unit=36  fln(6) - streamflow data (yyyymmdd.str)
! unit=37  fln(7) - reservoir release data (yyyymmdd.rel)
! unit=38  fln(8) - reservoir inflow data  yyyymmdd.rin
! unit=39  fln(9) - unadjusted radar file  yyyymmdd.rad
! unit=40  fln(10)- precipitation data (yymmdd.met)
! unit=41  fln(11)- radar scan (cappi) file
! unit=42  fln(12)- radar clutter file    yyyymmdd.clt
! unit=43  fln(13)- snow cover depletion curve   bsnmsdc
! unit=44  fln(14)- point temperatures yymmdd.tag
! unit=45  fln(15)- gridded temperatures yymmdd.tem
! unit=46  fln(16)- max temperatures yymmdd.tmx
! unit=47  fln(17)- min temperatures yymmdd.tmn
! unit=48  fln(18)- saily sbow files yymmdd.dsn
! unit=49  fln(19)- radiation data gridded yymmdd.flx
! unit=40  fln(20)- radiation data point yymmdd.prn
! units 51-99 reserved for output files
! filenames 100-199 reserved for event file names
! unit=251  fln(21)- humidity yymmdd.grh
! unit=252  fln(22)- wind speed yymmdd.gws
! unit=253  fln(23)- longwave radiation yymmdd.glw
! unit=254  fln(24)- shortwave radiatin yymmdd.gsw
! unit=255  fln(25)- atmospheric pressure yymmdd.gpr
! unit=256  fln(26)- point relative humidity yyyymmdd.prh
! unit=257  fln(27)- point wind speed  yyyymmdd.pws
! unit=258  fln(28)- point longwave radiation yyyymmdd.plw
! unit=259  fln(29)- point shortwave radiation yyyymmdd.psw
! unit=260  fln(30)- point atmospheric pressure yyyymmdd.ppr
! unit=261  fln(31) - gridded runoff files  yyyymmdd.rff
! unit=262  fln(32) - gridded recharge    yyyymmdd.rch
! unit=263  fln(33) - gridded leakage     yyyymmdd.lkg
! unit=264  fln(34)- gridded snow precipitation (not in use yet)
! unit=265  fln(35)- snow course data file  yyyymmdd.crs 
! unit=266  fln(36)- gridded snow water equivalant  yyyymmdd.swe
! unit=267  fln(37)- gridded soil moisture         yyyymmdd.gsm
! unit=268  fln(38)- gridded lower zone storage    yyyymmdd.lzs
! unit=269  fln(39)- point soil moisture  .psm 
! unit=270  fln(40)- water quality data file  .wqd
! unit=271  fln(41)- routing parameter file  bsnm_ch_par.r2c
! unit=272  fln(42)- diversion data (yyyymmdd.div)
! unit=273  fln(43)- 
! unit=274  fln(44)- 
! unit=275  fln(45)- 
! unit=276  fln(46)- 
! filenames 100-199 reserved for event file names

! unit=501  infln(5 )- reach1_lvl.txt
! unit=502  infln(6 )- reach2_lvl.txt
! unit=503  infln(7 )- reach3_lvl.txt
! unit=504  infln(8 )- reach4_lvl.txt
! unit=505  infln(9 )- reach5_lvl.txt
! unit=506  infln(10 )- reach6_lvl.txt ! TO MAXIMUM OF 45 REACHES: DYNAMICALLY PROCESSED

!   o - nhg     int      number of hours of rain gauge data
!   o - nhf     int      number of hours of flow data


! THE FOLLOWING FLAGS IF .EQ. 'y' WILL:
!  1 snwflg   - Use MELT ROUTINES
!  2 sedflg   - Use SEDIMENT ROUTINES
!  3 vapflg   - Use EVAPORATION ROUTINES
!  4 smrflg   - SMEAR PRECIP DATA
!  5 resinflg - READ THE yyyymmdd.rin FILE AND OUTPUT RESIN.TXT FILE
!  6 tbcflg   - WRITE A \spl\bsname\resume.txt FILE AT THE END OF RUN
!  7 resumflg - READ THE \spl\bsname\resume.txt FILE AT START OF RUN
!  8 contflg  - CONT THE STATISTICS FROM PREVIOUS RUN VIA RESUME.TXT
!  9 routeflg - WRITE THE \spl\bsnm\runof\yymmdd.txt FOR WATROUTE.EXE
!               write \spl\bsnm\rchrg\yymmdd.txt for MODFLOW
!               and write \spl\bsnm\lkage\yymmdd.lkg for watroute
! 10 crseflg  - READ SNOW COURSE DATA TO REPLACE RESUME FILE DATA
! 11 ensimflg - write the wfo file for ENSIM
! .  ensim1flg- if = 'a' wfo file will be written for 'all' events
! 12 picflg   - write the simout/pic.txt file for mapper
! 13 wetid1flg- run the wetland routing module - read in first event only
! 14 modelflg - if ='i' watroute with surface flow only 
!               if ='l' watroute for surface and groundwater routing
!               if ='r' watroute for surface to channel and drainage thru lz
! 15 shdflg   - replace the watershed file basin\bsnm.shd for next event
! 16 trcflg   - use the tracer module
! 17 frcflg   - use isotope fractionation
! 18 fstflg   - use fst-based runoff/recharge (csubich)
! 19 chnlflg  - use spatially-varying CHNL field (D. Durnford)
! 20 mndrflg  - use spatially-varying MNDR field (D. Durnford)
! 21 r1nflg   - use spatially- and temporally-varying Manning's coefficient for floodplain flow (R1N) (D. Durnford)
! 22 r2nflg   - use spatially- and temporally-varying Manning's coefficient for river channel flow (R2N) (D. Durnford)
! 23 rlakeflg - use spatially-varying rlake coefficient to adjust (multiply) R1N and R2N coefficients (E. Gaborit)
! 24 nocrashflg - repeatedly reduces qo1 by .75 and resets store2 to store1 when dtmin=mindtmin to ensure the model doesn't crash (D. Durnford)
! 25(19) rbmflg   - create input fields for RBM stream temperature model (dbourdin)
! 26 splrpnflg  - produce spl-rpn outputs (Y. Shin)
!
!***********************************************************************

      USE area_watflood
      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE


      CHARACTER(14) :: date
      character(1)  :: wetid1flg,ensim1flg,sedid1flg,
     *                   routeid1flg,modelid1flg,trcid1flg,
     *                   snwid1flg,firstpass,frcid1flg
      character(30) :: junk
      real*4   :: smc5(16),conv,scale,readscale
      real*4   :: scaleallsnw,readscalesnw,readscaletemp
      integer  :: nhg,nhf,i,ios,n,nkeep
      logical  :: exists

      data firstpass/'y'/

!     TW  MAR. 3/98 - GIVES ERROR LINKING ON SUN, SO COMMENT OUT
!      DATA flgevp2/-1.0/

       if(iopt.eq.2)print*,' In rdevt, passed location  300'
c      write(98,1300)fln(99)
       if(iopt.eq.2)print*,' opening file name ',trim(fln(99))
!     OPEN THE EVENT FILE:


      INQUIRE(FILE=fln(99),EXIST=exists)

      IF(exists)THEN
      open(unit=99,file=fln(99),iostat=ios)
        if(iopt.ge.1)print*,'Opened event file ',trim(fln(99))
        if(iopt.eq.2)print*,' In rdevt, passed location  30001'
        if(ios.ne.0)then
          write(*,99921)fln(99)
          write(98,99921)fln(99)
99921     format(' error opening unit 99, file name= ',a999)
          print*, 'iostat= ',ios
          print*  
          STOP ' program terminated in rdevt'
        endif
      else
        print*,'Attempting to open the file ',trim(fln(99))
        print*,'but it is not found (in this location)'
          print*,'Possibel cause: Not in proper working directory'
        print*
        stop 'Program aborted in rdevt @ 159'
      endif

      if(iopt.eq.2)print*,' In rdevt, passed location  301'

!     INPUT EVENT PARTICULARS:

      read(99,99001)junk                 ! #        
!     programmd for new file type 09/06/04 nk
!       new format event file - info needed for later
        if(id.eq.1)newevtflg='y'
        read(99,*,iostat=ios)junk,filetype
        write(51,*)junk,filetype
        if(iopt.eq.2.or.ios.ne.0)print*,junk,filetype
        read(99,*,iostat=ios)junk,evt_version
        write(51,*)junk,evt_version
        if(iopt.eq.2.or.ios.ne.0)print*,junk,evt_version
        read(99,*,iostat=ios)junk,year1         
        yr1=year1 ! Preserve a copy of the read-in year
        write(51,*)junk,year1         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,year1
        read(99,*,iostat=ios)junk,mo1         
        write(51,*)junk,mo1         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,mo1

        if(id.eq.1)then
!         REMEMBER WHICH MONTH WE START IN:
          month1=mo1
        endif

        read(99,*,iostat=ios)junk,day1         
        write(51,*)junk,day1         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,day1
        read(99,*,iostat=ios)junk,hour1         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,hour1
        read(99,*,iostat=ios)junk                 ! #         
        write(51,*)junk                 ! #         
        if(iopt.eq.2.or.ios.ne.0)print*,junk
        read(99,*,iostat=ios)junk,snwflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,snwflg
c        read(99,*,iostat=ios)junk,sedid1flg         
        read(99,*,iostat=ios)junk,sedflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,sedflg
        read(99,*,iostat=ios)junk,vapflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,vapflg
        read(99,*,iostat=ios)junk,smrflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,smrflg
        read(99,*,iostat=ios)junk,resinflg   
        if(iopt.eq.2.or.ios.ne.0)print*,junk,resinflg
        read(99,*,iostat=ios)junk,tbcflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,tbcflg
        read(99,*,iostat=ios)junk,resumflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,resumflg
        read(99,*,iostat=ios)junk,contflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,contflg
c        read(99,*,iostat=ios)junk,routeid1flg         
        read(99,*,iostat=ios)junk,routeflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,routeflg
        read(99,*,iostat=ios)junk,crseflg      
        if(id.eq.1)then
          crseflg='n'
!         snow course data is read by default if snwflg is 'y'
!         crseflg is used only for subsequent events.  nk 15/10/04
        endif
        if(iopt.eq.2.or.ios.ne.0)print*,junk,crseflg
        read(99,*,iostat=ios)junk,ensimflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,ensimflg
        read(99,*,iostat=ios)junk,picflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,picflg 
c        read(99,*,iostat=ios)junk,wetid1flg         
        read(99,*,iostat=ios)junk,wetflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,wetflg
c        read(99,*,iostat=ios)junk,modelid1flg         
        read(99,*,iostat=ios)junk,modelflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,modelflg
        read(99,*,iostat=ios)junk,shdflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,shdflg
c        read(99,*,iostat=ios)junk,trcid1flg         
        read(99,*,iostat=ios)junk,trcflg         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,trcflg
        if(evt_version.ge.9.4)then
! TS: ADDED FRACTIONATION FLAG (22/03/06)
c          read(99,*,iostat=ios)junk,frcid1flg         
          read(99,*,iostat=ios)junk,frcflg         
          if(iopt.eq.2.or.ios.ne.0)print*,junk,frcflg
          else
            frcflg='n'
        endif
        if(evt_version.ge.9.6)then
! NK: ADDED init FLAG (05/03/07)
          read(99,*,iostat=ios)junk,initflg 
          if(iopt.eq.2.or.ios.ne.0)print*,junk,initflg
          else
            initflg='n'
        endif
! csubich: added fstflag for .fst-style recharge/runoff inputs
        if(evt_version .ge. 9.8) then
           read(99,*,iostat=ios)junk,fstflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, fstflg
        else
           fstflg='n'
        endif
! D. Durnford: added channel flag for spatially-varying CHNL field
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,chnlflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, chnlflg
        else
           chnlflg='n'
        endif
! D. Durnford: added meander flag for spatially-varying MNDR field
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,mndrflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, mndrflg
        else
           mndrflg='n'
        endif
! D. Durnford: added r1n flag for spatially- and temporally-varying Manning's coefficient for floodplain flow (R1N)
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,r1nflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, r1nflg
        else
           r1nflg='n'
        endif
! D. Durnford: added r2n flag for spatially- and temporally-varying Manning's coefficient for river channel flow (R2N)
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,r2nflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, r2nflg
        else
           r2nflg='n'
        endif

! E. Gaborit: added rlake flag for spatially-varying rlake multiplicative coefficient to adjust Manning coeffs for presence of lakes
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,rlakeflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, rlakeflg
        else
           rlakeflg='n'
        endif

! D. Durnford: added nocrash flag to prevent the model from crashing.  Water is not conserved.
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,nocrashflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, nocrashflg
        else
           nocrashflg='n'
        endif

! dbourdin: added rbmflag for RBM stream temperature model inputs
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,rbmflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, rbmflg
        else
           rbmflg='n'
        endif

! Y. Shin: added splrpnflg for spl-rpn files  
        if(fstflg == 'y') then
           read(99,*,iostat=ios)junk,splrpnflg
           if(iopt .eq. 2 .or. ios .ne. 0) print *, junk, splrpnflg
        else
           splrpnflg='n'
        endif

!       turn these off for opt
        if(numa.ne.0)then
            trcflg='n'
            frcflg='n'
          endif

        read(99,*,iostat=ios)junk                 ! #         
        if(iopt.eq.2.or.ios.ne.0)print*,junk
        read(99,*,iostat=ios)junk,(smc5(i),i=1,5)         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,(smc5(i),i=1,5)
        read(99,*,iostat=ios)junk,conv         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,conv
        read(99,*,iostat=ios)junk,scale         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,scale
        read(99,*,iostat=ios)junk,readscale         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,readscale
        read(99,*,iostat=ios)junk,scalesnw        
        if(iopt.eq.2.or.ios.ne.0)print*,junk,scalesnw
        read(99,*,iostat=ios)junk,readscalesnw        
        if(iopt.eq.2.or.ios.ne.0)print*,junk,readscalesnw
        read(99,*,iostat=ios)junk,scaletem         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,scaletem 
        read(99,*,iostat=ios)junk,readscaletemp          
        if(iopt.eq.2.or.ios.ne.0)print*,junk,readscaletemp
        read(99,*,iostat=ios)junk                 ! #         
        if(iopt.eq.2.or.ios.ne.0)print*,junk
        read(99,*,iostat=ios)junk,nhg  
        event_hours=nhg       
        if(iopt.eq.2.or.ios.ne.0)print*,junk,nhg 
        read(99,*,iostat=ios)junk,nhf         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,nhf
          if(id.eq.1)then
            deltat_report_discharge=1    ! default
            if(evt_version.ge.9.7)then
!           can't be the same as ireport used for write_wfo
              read(99,*,iostat=ios)junk,deltat_report_discharge
            if(iopt.eq.2.or.ios.ne.0)print*,junk,
     *                               deltat_report_discharge
              read(99,*,iostat=ios)junk,deltat_report_flowICs
            if(iopt.eq.2.or.ios.ne.0)print*,junk,
     *                               deltat_report_flowICs
            endif
          endif
        read(99,*,iostat=ios)junk               ! #
        if(iopt.eq.2.or.ios.ne.0)print*,junk


!       write this stuffto spl.txt for the record
        write(51,*)'#                             '        
        write(51,*)':snwflg                       ',snwflg         
        write(51,*)':sedflg                       ',sedflg         
!        write(51,*)':sedflg                       ',sedid1flg         
        write(51,*)':vapflg                       ',vapflg         
        write(51,*)':smrflg                       ',smrflg         
        write(51,*)':esimflg                      ',resinflg   
        write(51,*)':tbcflg                       ',tbcflg         
        write(51,*)':resumflg                      ',resumflg         
        write(51,*)':contflg                      ',contflg         
        write(51,*)':routeflg                     ',routeflg         
!        write(51,*)':routeflg                     ',routeid1flg         
        write(51,*)':crseflg                      ',crseflg         
        write(51,*)':ensimflg                     ',ensimflg         
        write(51,*)':picflg                       ',picflg         
        write(51,*)':wetflg                       ',wetflg         
!        write(51,*)':wetflg                       ',wetid1flg         
        write(51,*)':modelflg                     ',modelflg         
!        write(51,*)':modelflg                     ',modelid1flg         
        write(51,*)':shdflg                       ',shdflg         
        write(51,*)':trcflg                       ',trcflg  
!        write(51,*)':trcflg                       ',trcid1flg  
        write(51,*)':frcflg                       ',frcflg  
!        write(51,*)':frcflg                       ',frcid1flg  
        write(51,*)':initflg                      ',initflg    
        write(51,*)':fstflg                       ',fstflg
        write(51,*)':chnlflg                      ',chnlflg
        write(51,*)':mndrflg                      ',mndrflg
        write(51,*)':r1nflg                       ',r1nflg
        write(51,*)':r2nflg                       ',r2nflg
        write(51,*)':rlakeflg                     ',rlakeflg
        write(51,*)':rbmflg                       ',rbmflg
        write(51,*)':splrpnflg                    ',splrpnflg
        write(51,*)'#                             '        
        write(51,*)':intSoilMoisture              ',(smc5(i),i=1,5)         
        write(51,*)':rainConvFactor               ',conv         
        write(51,*)':eventPrecipScaleFactor       ',scale         
        write(51,*)':precipScaleFactor            ',readscale         
        write(51,*)':eventSnowScaleFactor         ',scalesnw        
        write(51,*)':snowScaleFactor              ',readscalesnw        
        write(51,*)':eventTempScaleFactor         ',scaletem         
        write(51,*)':tempScaleFactor              ',readscaletemp          
        write(51,*)'#                             '        
        write(51,*)':hoursRainData                ',nhg         
        write(51,*)':hoursFlowData                ',nhf         
        write(51,*)'#                             '
!       conv is used as a conversion factor for the precip data.
!       scale is used separately in each event to scale all the precip in this event
!       scalesnw is used to scale the snow course data in this event
!       scaletem is used to add or subtract the value to/from each temperature
!       readscale is used to scale precip for all events to follow
!       readscalesnw is used to scale all snowcourse data to follow
!       readscaletemp is used to scale all temperature data to follow
!       >>>>> the readscale... OVERRIDES the scale... variables !!!!!!!!

!       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!         read the file names for this event:      
!       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!       Note: the file names can be numbered from 1 to nn
!       but the unit numbers can not. 
!       The unit numbers for the input files go from 31 to 50
!       and from 251 to nn
!       This leaves room for more output files from unit=100-199
!       and leaves room for more input files above unit=251
!       Also, in the future, all input unit numbers should be changed to 
!                                     unit=251-300   nk Mar. 11/04
        write(51,1031)
!       In the new revised file format, the order of the files has been
!       changed to group the files by file type:
!       e.g. watershed data files, point data files, gridded files, etc. 

!       Note: discontinuity in the unit numbers.31->50 and 251->infinity
!       Assign unit numbers starting at 250

c        do i=1,3      ! .shd .par .pdl   
c           read(99,99005,iostat=ios)junk,fln(i)
c           if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',fln(i)
c           write(51,1030)i+30,i,trim(fln(i))
c        end do

!     _shd.r2c    
        i=1
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
!     .par    
        i=2
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
!     ch_par.r2c  
!     rev. 9.3.11  Feb.  28/07  - NK: ch_par added / event file ver = 9.5
        if(evt_version.ge.9.5)then
            i=41
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+230,i,trim(fln(i))
          endif
!     .pdl   
        i=3
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))


        i=13          ! .sdc
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
!     rev. 9.1.78  Mar.  15/05  - NK: added WQD file to event file
        if(evt_version.ge.9.3)then
          i=40          ! .wqd      
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+250,i,trim(fln(i))
        endif

        read(99,99005,iostat=ios)junk                 ! #         
        if(iopt.eq.2.or.ios.ne.0)print*,junk



!     rev. 9.1.77  Mar.  07/05  - NK: added .psm .gsm & .glz  files
        if(evt_version.ge.9.2)then
          i=39          ! .psm      
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+250,i,trim(fln(i))
        endif

        i=5          ! .rag                  
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
        i=14          ! .tag        
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
        i=20          ! .pnr        
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))

        do i=26,30    ! .prh .pws .plw .psw .ppr        
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+250,i,trim(fln(i))
        end do

        read(99,99005,iostat=ios)junk                 ! #         
        if(iopt.eq.2.or.ios.ne.0)print*,junk

        do i=6,7          ! .str .rel           nk 05/11/16
          read(99,99005,iostat=ios)junk,fln(i)
!          write(*,'(a31,i4,a)') junk,i,trim(fln(i))
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+30,i,trim(fln(i))
        end do

!       next we have either the .rin or the .snw file:
        if(evt_version.ge.9.2)then
          i=8             ! .rin          
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+30,i,trim(fln(i))
        else
          i=8             ! .snw   old format .snw file
          read(99,99005,iostat=ios)junk,fln(i)
          endif                              !     nk 05/11/16

        if(evt_version.ge.9.2)then
          i=35          ! .crs      
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+250,i,trim(fln(i))
        endif

        read(99,99005,iostat=ios)junk                 ! #         
        if(iopt.eq.2.or.ios.ne.0)print*,junk
          i=9          !  .rad       
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+30,i,trim(fln(i))

        do i=11,12      ! .scn .clt   
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+30,i,trim(fln(i))
        end do

        if(evt_version.ge.9.2)then 
          do i=36,38    ! .swe .gsm .lzs     
            read(99,99005,iostat=ios)junk,fln(i)
            if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
            write(51,1030)i+250,i,trim(fln(i))
          end do
        endif
        i=10          ! .met        
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
        i=34          ! .snw        
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+250,i,trim(fln(i))
        i=15          ! .tem        
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))
        i=19          ! .gnr        
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+30,i,trim(fln(i))

        do i=21,25         ! .grh .gws .glw .gsw .gpr
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+250,i,trim(fln(i))
        end do

        do i=31,33          ! .rff .rch .lkg        
          read(99,99005,iostat=ios)junk,fln(i)
          if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
          write(51,1030)i+250,i,trim(fln(i))
        end do

        do i=1,8            ! Empty lines reserved for files
          read(99,*,iostat=ios)junk                 ! #
          if(iopt.eq.2.or.ios.ne.0)print*,junk
        end do
        i=42             ! .div
        read(99,99005,iostat=ios)junk,fln(i)
        if(iopt.eq.2.or.ios.ne.0)print*,junk,i,'   ',trim(fln(i))
        write(51,1030)i+250,i,trim(fln(i))
!        write(*,1030)i+250,i,trim(fln(i))

!       read the addtional event files
!       nch = No of CHained events

!       Want to keek this number here so we can run any or all of the 
!       listed events. 

        read(99,*,iostat=ios)junk,nch         
        if(iopt.eq.2.or.ios.ne.0)print*,junk,nch
          read(99,*,iostat=ios)junk                 ! #
        if(iopt.eq.2.or.ios.ne.0)print*,junk
        if(nch.ge.1)then
!         READ SUBSEQUENT EVENT NAMES TO MODELLED:
!         READ SUBSEQUENT EVENT NAMES TO MODELLED:
!         ni = no of events
!         default is 1 but if nch > 0 then the total no of events
!         will be nch+1
          ni=nch+1
          nch=ni
          do n=2,ni
            nkeep=n
            read(99,1300,iostat=ios)fln(100+n)
            write(51,1300)trim(fln(100+n))
            if(ios.ne.0)then
              print*,'Error reading file no ',nkeep,' in rdevt'
              print*,'possible cause: number of files listed '
              print*,'in the event file is shorter'
              print*,'than the specified ',ni-1
              print*
              stop 'Program abortedin rdevt @ 460'
            endif
            if(iopt.eq.2)print*,100+n,n,trim(fln(100+n))
          end do
        endif

            if(iopt.ge.1)print*,'Reached end of ',trim(fln(99))

!        if(iopt.eq.2.or.ios.ne.0)print*,junk

!        if(ios.ne.0)then
!          print*,'Problems reading the event.evt file'
!          print*,'It is important that all required values are there'        
!          print*,'Please set iopt=2 in the .par file and restart'
!          print*,'This will show where program dies'
!          print*
!        endif    
 
        if(iopt.eq.2)print*,' In rdevt, passed location  504'

!       DETERMINE IF IT IS A LEAP YEAR
        leapflg='0'
        if(mod(year1,4).eq.0)leapflg='1'

!       Note: discontinuity in the unit numbers.31->50 and 251->infinity
!       Assign unit numbers starting at 250


!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      if(iopt.eq.2)print*,' In rdevt, passed location  305'

!     CLOSE INPUT FILE - IT IS USED AS A SCRATCH UNIT # LATER
 999  close(unit=99,status='keep')


!     these flags can only be set in the first event and carry on
!     for all subsequent events - they can not be changed on the fly
!     revised Apr. 27/07  nk
      if(id.eq.1)then
        snwid1flg=snwflg
        wetid1flg=wetflg    
          trcid1flg=trcflg
        sedid1flg=sedflg
          frcid1flg=frcflg
        routeid1flg=routeflg
        modelid1flg=modelflg
      else
          snwflg=snwid1flg
        wetflg=wetid1flg    
          trcflg=trcid1flg
        sedflg=sedid1flg
          frcflg=frcid1flg
        routeflg=routeid1flg
        modelflg=modelid1flg
      endif

!     write the wfo file for all events if ensimflg='a' for first event
      if(id.eq.1)then
!     rev. 9.1.34  Dec.  23/02  - Added ensim1flg - if ensimflg='a' for 1st id then 'y' for all events
!       wfo file written for all events
        if(ensimflg.eq.'a'.or.ensimflg.eq.'A')then
          ensim1flg='a'
        else
          ensim1flg=' '
        endif
      endif
      if(ensim1flg.eq.'a')ensimflg='y'  


      if(snwflg.eq.'Y')snwflg='y'
      if(snwid1flg.eq.'Y')snwid1flg='y'
      if(sedid1flg.eq.'Y')sedid1flg='y'
      if(vapflg.eq.'Y')vapflg='y'
      if(smrflg.eq.'Y')smrflg='y'
      if(resinflg.eq.'Y')resinflg='y'
      if(resumflg.eq.'Y')resumflg='y'
      if(tbcflg.eq.'Y')tbcflg='y'
      if(contflg.eq.'Y')contflg='y'
      if(routeid1flg.eq.'Y')routeid1flg='y'
      if(crseflg.eq.'Y')crseflg='y'  
        if(ensimflg.eq.'Y')ensimflg='y'
        if(picflg.eq.'Y')picflg='y'
      if(wetid1flg.eq.'Y')wetid1flg='y'
      if(modelid1flg.eq.'R')modelid1flg='r'
      if(modelid1flg.eq.'I')modelid1flg='i'
      if(modelid1flg.eq.'D')modelid1flg='d'
      if(shdflg.eq.'Y')shdflg='y'
        if(trcid1flg.eq.'Y')trcid1flg='y'
        if(frcid1flg.eq.'Y')frcid1flg='y'

      if(snwflg.eq.'N')snwflg='n'
      if(snwid1flg.eq.'N')snwid1flg='n'
      if(sedid1flg.eq.'N')sedid1flg='n'
      if(vapflg.eq.'N')vapflg='n'
      if(smrflg.eq.'N')smrflg='n'
      if(resinflg.eq.'N')resinflg='n'
      if(resumflg.eq.'N')resumflg='n'
      if(tbcflg.eq.'N')tbcflg='n'
      if(contflg.eq.'N')contflg='n'
      if(routeid1flg.eq.'N')routeid1flg='n'
      if(crseflg.eq.'N')crseflg='n'  
        if(ensimflg.eq.'N')ensimflg='n'
        if(picflg.eq.'N')picflg='n'
      if(wetid1flg.eq.'N')wetid1flg='n'
      if(modelid1flg.eq.'N')modelid1flg='n'
      if(shdflg.eq.'N')shdflg='n'
        if(trcid1flg.eq.'N')trcid1flg='n'
        if(frcid1flg.eq.'N')frcid1flg='n'

      if(snwflg.eq.' ')snwflg='n'
      if(snwid1flg.eq.' ')snwid1flg='n'
      if(sedid1flg.eq.' ')sedid1flg='n'
      if(vapflg.eq.' ')vapflg='n'
      if(smrflg.eq.' ')smrflg='n'
      if(resinflg.eq.' ')resinflg='n'
      if(resumflg.eq.' ')resumflg='n'
      if(tbcflg.eq.' ')tbcflg='n'
      if(contflg.eq.' ')contflg='n'
      if(routeid1flg.eq.' ')routeid1flg='n'
      if(crseflg.eq.' ')crseflg='n'  
        if(ensimflg.eq.' ')ensimflg='n'
        if(picflg.eq.' ')picflg='n'
      if(wetid1flg.eq.' ')wetid1flg='n'
      if(modelid1flg.eq.' ')modelid1flg='n'
      if(shdflg.eq.' ')shdflg='n'
        if(trcid1flg.eq.' ')trcid1flg='n'
        if(frcid1flg.eq.' ')frcid1flg='n'

      if(modelid1flg.ne.'n')then
        if(initflg.eq.'y')then
          print*,'In the event file: initflg can not be `y`'
          print*,'if the modelflg = not `n`'
          print*,'i.e. if the modelflg = `i`, `r` or `l`'
          print*
          stop 'Program aborted in rdevt @ 257'
        endif
      endif

c!     Added March 10/06  NK
c      if(firstpass.eq.'y')then
c        lastsnwflg=snwflg
c        firstpass='n'
c      endif
c      if(snwflg.ne.lastsnwflg)then
c        if(firstpass.eq.'n')then
c            print*
c          print*,'The snwflg can NOT be changed partway through a run.'
c            print*,'If it is changed in the next event file,'
c          print*,'it is set to the previous event value'
c            print*,'I.e. whatever is in the first event holds throughout'
c        endif
c            print*
c          print*,'snwflg changed from ',snwflg,' to ',lastsnwflg
c            print*
c          snwflg=lastsnwflg
c        endif


      if(id.eq.1)then
!       The following are set only in the first event:
c        wetflg=wetid1flg    
c        routeflg=routeid1flg
c        modelflg=modelid1flg
c          trcflg=trcid1flg
c        sedflg=sedid1flg
c          frcflg=frcid1flg

!       rev. 9.1.45  Jun.  11/03  - runoff, recharge and leakage files added 
        if(routeflg.eq.'y'.and.modelflg.ne.'n')then
          print*,' The route flag is flag no.  9 in column 49'
          print*,' The model flag is flag no. 14 in column 54'
          print*,' You can not have the routeflg = "y" AND'
          print*,' the modelflg = r, i or l '
          print*
          print*,' Please pick one or the other as "n"'
          print*,'   and try again. Sorry.'
          print*
          stop ' Program aborted in rdevt @ 198'
        endif
      endif

!     rev  9.1.24  Sep.  11/02  - Added scaleallsnw to set snw scale in event #1
!     scaleall is the master multiplier
!     if larger than 0.0, for the first event then use it throughout the run
!     scaleall overrides everything!!!!
      if(id.eq.1)then
        if(readscale.gt.0.0)then
          scaleall=readscale   ! scaleall set for all events to come
        else
          scaleall=0.00
        endif
      endif
      if(scaleall.gt.0.0)then
        scale=scaleall
      endif

!     scaleallsnw is the master multiplier for snow
!     if larger than 0.0, for the first event then use it throughout the run
      if(id.eq.1)then
        if(readscalesnw.gt.0.0)then
          scaleallsnw=readscalesnw   ! scaleallsnw set for all events 
        else
          scaleallsnw=0.00
        endif
      endif
      if(scaleallsnw.gt.0.0)then
        scalesnw=scaleallsnw
      endif

      if(id.eq.1)then
        if(readscaletemp.lt.-0.01.or.readscaletemp.gt.0.01)then
          scalealltem=readscaletemp   ! readscaletemp set for all events 
        else
          scalealltem=0.00
        endif
      endif
!     Note:
!     This is done in rdtem.for  
!        if(scalealltemp.ne.0.0)then    
!          scaletem=scalealltem
!        endif

      if(iopt.eq.2)print*,' In rdevt, passed location 870'

        write(*,'(a18,a)') 'finished reading ',trim(fln(99))
        print*

      RETURN

      STOP ' program terminated in rdevt'

99910 write(*,99911)
99911 format(' Error reading the line like the following in rdevt:')
      write(*,1010)
     *   date,snwflg,sedid1flg,vapflg,smrflg,resinflg,tbcflg,resumflg,
     *   contflg,routeflg,crseflg,ensimflg,picflg,wetid1flg,
     *   modelflg,shdflg,trcflg,frcflg

      write(*,1010)
      STOP ' program terminated in rdevt'

99931 write(*,99932)
99932 format(' ','Error on unit=99,fln= event/event.evt//')
      STOP  ' program terminated while reading the event file//'

! FORMATS:

 1000 format(26x,a31)
 1010 format(26x,a14,20a1)
 1020 format(' ', /'you are trying to link too many events'/
     *'      the maximum is 500, you entered,',i5,/
     *'      program aborted. fix the event file and try again')
 1030 format(' ','Unit no. =',i3,' file no',i3,' = ',a)
 1031 format(' Input files from event.evt')
 1040 format(' Event no. ',i5)
 1050 format(' ')
 1100 format(26x,6f5.2)
 1110 format(26x,6f5.2)
 1200 format(26x,3i5)
 1300 format(a999)
 6080 format(a14)
 6081 format(i4)

99001 format(a30)
99002 format(a30,a1)
99003 format(a30,f12.2)
99004 format(a30,i10)
99005 format(a30,a999)

      END SUBROUTINE rdevt

