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
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

      module area_watflood

!     *** MODULE area1 ***

!     entrees rearranged in alphabetical order.  nk  Mar. 7/07

      character*10 :: program_name

      integer :: maxr,event_hours,iswitchrerout,fhr

      real*4 :: por,sqlz,slzinflw,leakage,dacheck

      character*1, dimension(:), allocatable :: glacier_flag

      integer, dimension(:), allocatable :: next,ibn,ichnl,irough,
     *                       ireach,nreach,res,xxx,yyy,resindex
     *                                       

      real*4, dimension(:), allocatable :: 
     *                    att,bnkfll,bin_precip,ch_length,da,d2,cap,
     *                    fake,fakefs,frac,
     *                    grid_area,lake_area,channel_area,
     *                    lzs,netflow,over,pot,potfs,psmear,punused,
     *                    qi1,qi2,qo1,qo2,qo2sim,qo2rem,qo2remirr,
     *                    qda,qr,qlz,
     *                    qbase,qmax,qstream,qstrm,qdrng,qdrngfs,
     *                    qdrng2,qdrngfs2,
     *                    rechrg,rl,
     *                    sl1,slope,sl2,sq1,sq1fs,sqint,sqintfs,
     *                    store1,store2,storinit,sr,strloss,
     *                    sdrng,sdrngfs,sexcess,store2_strt,
     *                    sump,sumrff,sumqint,sumqintfs,
     *                    sumq1,sumq1fs,sumrechrg,
     *                    totd1,totuzs,totsnw,totchnl,totgrid,
     *                    x4,x5,avr_qo
                   
!     note:  frac is to be replaced by grid_area

!     rev. 9.1.30 Nov. 08/02 - added q1, qint & drng to the wfo file

      real*4, dimension(:,:), allocatable ::
     *                      aclass,d1,d1fs,effpor,drng,drngfs,
     *                      df,dffs,q1,q1fs,qint,qintfs,
     *                      snow,sumf,sumffs,r,v,uzs,uzsfs
!    *                                       intcap

!     rev. 9.4.02  Apr.  18/07  - NK: moved rf, rffs from areawq to area1

      real*4, dimension(:,:), allocatable :: rf,rffs

!     *** END MODULE area1 ***

! NOTES:
!
! NOV. 2000, TS: Added QLZ variable during WATFLOOD swamp routing changes 
!                b/c needed to access it from routea (not just runof5a).
! AUG 27/03  TS: Added sumq1,sumq1fs,sumqint,and sumqintfs,qdrngfs arrays and made
!                qdrng and qdrngfs arrays.  FOR TRACER S/R
   
!     *** MODULE area2 ***

!     general program variables

!     flag for calling program

      character*10    calling_program_flg

      integer, dimension(:), allocatable :: wfo_pick

      character*50, dimension(:), allocatable :: wfo_attributes

      integer :: iopt,itype,iymin,iymax,jxmin,jxmax,imax,imin,
     *           jmin,jmax,ib,it,no,ndam,ni,id,nl,mhtot,mhrd,kt,
     *           irads,iradn,jradw,jrade,iyoffset,jxoffset,
     *           iyshift,jxshift,istep,nblock,
     *           ireport,ireport_start,ireport_end,
     *           ioflg,ichsm,nnprint,iiprint,iopt_start,
     *           ktri,glacier_class_number
      
      integer    :: rgrd,rads,radw,rade,radn

        real*4       :: rgrdn,rgrde     !changed to real Jul. 27/04 nk
        real*4       :: evt_version

        integer    :: mm,yy,iall,nclt,nch,irdt,ii_water
        integer   :: year,month,day,hrs,mins,secs,hsecs,hh,dd
        integer, parameter :: flen=9000
        real*4       :: ver,grdn,grde,al,astep,step2,scalesnw,scaleall,
     *                     scaletem,scalealltem,sstep,rdt,flowunitconv
	  logical(1)   :: keyflg,precflg,found_data_end
        CHARACTER(5) :: title(200)
        character(40) :: notes(100)
        CHARACTER(80) :: heading(10)
        character(1) :: snwflg,sedflg,vapflg,smrflg,resinflg,
     *                  resumflg,tbcflg,contflg,routeflg,crseflg,
     *                  ensimflg,leapflg,llflg,picflg,wetflg,
     *                  modelflg,shdflg,wfo_open_flg,trcflg,frcflg,
     *                  newevtflg,manningflg,translateflg,flowfillflg,
     *                  outfileflg,initflg,
     *                  fstflg,chnlflg,mndrflg,r1nflg,r2nflg,rlakeflg,
     *                  nocrashflg,rbmflg,splrpnflg
        character(1) :: ssmc_firstpass
!        character(1), dimension(:), allocatable :: glacier_flag
!       these things taken out of some argument lists  nk 05/10/04
        integer      :: yr1,year1,mo1,day1,hour1
	  integer      :: year_now,month_now,day_now,hour_now,no_dt
! D. Durnford: enable the streamflow, release file input files to start at date, times other than 01/01
! and the model run to start at an hour other than 00 UTC
	  integer      :: strday1,strhour1,relday1,relhour1,
     *                    divday1,divhour1
	  character(2) :: yy2,mm2,dd2,hh2
        character(4) :: yyyy4, nostr

        character(5) :: source,rdr,data_source
        character(80):: querystring
!       	  character(14)   :: date
!     rev. 9.1.55  Jun.  12/04  - NK: write new str files to strfw\newfmt folder.
        character(10)   :: coordsys1,datum1,zone1
	  character(40)   :: attribute_name,attribute_units,attribute_type,
     *                      application,author,char_block,name
        character(30)   :: source_file_name
	  integer         :: attribute_count
        real*4          :: init_heat_deficit,unit_conversion
!       this set of variables needed when reading files other than the 
!       shed file so coincidence of data can be checked
!       fix fix   do this in all rd**** files someday

!       for the gridded precip file in rdrain
        character(10)   :: coordsys2,datum2,zone2
        real*4            :: xorigin2,yorigin2,xdelta2,ydelta2,
     *                     dtrain,convrain
        integer         :: xcount2,ycount2,deltat2,nhrain

!       for the gridded temperature file in rdtemp
        character(10)   :: coordsys3,datum3,zone3
        real*4            :: xorigin3,yorigin3,xdelta3,ydelta3,
     *                     dttemp,convtemp
        integer         :: xcount3,ycount3,deltat3,nhtemp

!       for the generic read & write modules (e.g write_r2c
        character(10)   :: coordsys_temp,datum_temp,zone_temp
        real*4            :: xorigin_temp,yorigin_temp,
     *                     xdelta_temp,ydelta_temp
        integer         :: xcount_temp,ycount_temp,deltat_temp,
     *	               deltat_report_discharge,deltat_report_flowICs

!       please note that coordsys and datum are used elsewhere.

!      integer :: jan,ii,n,i,j,i3,ii1,ii2
!      real ::    aintvl


ccccc      END MODULE area2

!      llflg  - when 'y', coordinates for .str .snw etc in lat-long
!      ioflg  - if .ge. 1 read the outfiles (note: integer)
!       leapflg  - to indicate a leap year - not an input

!	snwflg - whether there is snow to melt
!	sedflg - whether the sediment routine is used y or n
!	vapflg - turn on evap routine (Todd)
!	smrflg - turns on smearing (smear precip data over data dt)
!	resinflg - will use resin record for comparison
!	tbcflg - read resume.txt file for run init values
!	resumflg - resume.txt file written at end of run (mem dump)
!     contflg  - for continuing statistics upon resume = input
!     routeflg - output qr grids for routing program 
!     crseflg  - read snow course data to replace resume file data
!     ensimflg - write the wfo file for ENSIM
!     ensimflg1 = 'y' for first time needed, else = 'n' (inpevt)
!     wfoflg - set to 'y' initially. Changed to 'n' once wfo header=written
!     picflg   - write the simout/pic.txt file for mapper
!     wetid1flg- run the wetland routing module - read in first event only
!     modelflg  - pick model
!     shedflg  - replace the watershed file basin\bsnm.shd

!	source - what is the data source - radar, mc2, erf, rag, etc.




ccccc      MODULE area3

        integer :: na,naa,ij,ji,ls,ks,js,ijk,ih,ipr,jpr,iw,ntype,
     *             ntypeo,nbsn,nrvr,nsnow,nlz,net,nastart,naend

ccccc      END MODULE area3




ccccc      MODULE area4

      real*4,    dimension(:), allocatable ::  r1,r4,ds,dsfs,chnl,
     *                         r2,r3,r3fs,rec,ak,akfs,r2low,r3low,
     *                         r3fslow,reclow,aklow,akfslow,ak2fslow,
     *                         r2hgh,r3hgh,r3fshgh,rechgh,akhgh,akfshgh,
     *                         ak2fshgh,r2dlt,r3dlt,r3fsdlt,recdlt,
     *                         akdlt,akfsdlt,ak2fsdlt,retn,ak2,flz,flz2,
     *                         pwr,pwr2,retnlow,ak2low,flzlow,pwrlow,
     *                         retnhgh,ak2hgh,flzhgh,pwrhgh,retndlt,
     *                         ak2dlt,flzdlt,pwrdlt,retfs, ak2fs,fpet,
     *                         fpetdlt,fpetlow,fpethgh,ftall,ftalldlt,
     *                         ftalllow,ftallhgh,mndr,aa2,aa3,aa4,
     *            thetadlt,thetalow,thetahgh,widepdlt,wideplow,widephgh,
     *            kconddlt,kcondlow,kcondhgh,
     *            flz_o,pwr_o,r1n_o,r2n_o,mndr_o,aa2_o,aa3_o,aa4_o,
     *            theta_o,widep_o,kcond_o

!     rev. 9.2.11  Sep.  11/05  - NK: added Manning's n  r1n & r2n
      real*4,    dimension(:), allocatable :: r1n,r2n,rlake, 
     *                           r2nlow,r2nhgh,r2ndlt

      real*4,    dimension(:,:), allocatable :: h,fpetmo 

      integer,   dimension(:), allocatable :: iiclass

      character*10, dimension(:), allocatable :: nclass,rivtype

      real*4    :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,type1

ccccc      END MODULE area4

ccccc      MODULE area5

        real*4,  dimension(:,:), allocatable :: qrel,qinfl,qdwpr,
     *                     lake_stor,lake_outflow,lake_inflow,lake_elv,
     *                     del_stor
!     *                     del_stor,net_lake_inflow,net_lake_outflow
     *                     ,qstream_sum,strloss_sum

! dch
        real*4, DIMENSION(:), ALLOCATABLE :: reach_last

        real*4,    dimension(:), allocatable :: b1,b2,b3,b4,b5,b6,b7
        real*4,    dimension(:), allocatable :: ppsum

        integer, dimension(:), allocatable :: ires,jres,nnsum
        integer  noresv,local,mhto,index,nrel,ktr

        real*4,    dimension(:), allocatable :: val1div
        integer, dimension(:), allocatable :: val2div,val3div,val4div,
     *    divstrindex,divendindex
        integer, dimension(:,:), allocatable :: irrigindx
        real*4,    dimension(:,:), allocatable :: qdiv,qdivirrig

        integer  val2divyes,nodiv,nodivirrig,maxirrigpts

	character*12, dimension(:), allocatable :: resname,resnamei

	character*12, dimension(:), allocatable :: divname
        character(10) :: starttime1,startdate1
        character(50) :: strfw_option
        character*1, dimension(:), allocatable :: poliflg
        logical :: first_run

ccccc      END MODULE area5

ccccc      MODULE area6

      real*4,    dimension(:,:), allocatable :: sn1,ssmc,api,
     *                                          precadj,basinerr

      real*4,    dimension(:,:), allocatable :: sum_precip

      integer, dimension(:,:), allocatable :: nhyd,nbasin

      real*4,      dimension(:), allocatable :: statnerr,suberr,
     *                                        subarea 

      integer,   dimension(:), allocatable :: nxtbasin,jl,jr,
     *                                        iflowgrid,inbsnflg

! CSUBICH - SEPT 2012
! because the variable 's' has been re-purposed from a flow-
! direction to index number in other sections of this code,
! keeping kind=2 is no longer appropriate -- it overflows on
! grids with more than 32,768 elements
      integer, dimension(:,:), allocatable :: s

ccccc      END MODULE area6

! TS - MARCH 2000
! Removed p( , ) array decclaration because  it's decclared in area16a
!     rev. 9.1.18  Jun.  03/02  - Added sub-watershed modelling capability inbsnflg




ccccc      MODULE area7
c      integer :: lc,itt,izy,ncoun,icoun,ifirs,ldelt,nsave,ll
c      real*4    :: ys1,yy1,yx1
ccccc      END MODULE area7

ccccc      MODULE area8
      real*4, dimension(:), allocatable :: a,ddelta,checkl,checkh,ssave,
     *                                   les,ba,b,iclosl,iclosh,nsign,
     *                                   odelta
ccccc      END MODULE area8




ccccc      MODULE area9

        real(4) :: optim

        integer      :: numa,nstart,nper,kc,maxn,nnn,dds_flag

ccccc      END MODULE area9




ccccc      MODULE area10

      real*4,   dimension(:,:), allocatable :: qhyd,qsyn,qloc,
     *                               delta,frc,frcs,
     *                               qhyd_dly,qsyn_dly,qhyd_mly,qsyn_mly

      real*4,   dimension(:), allocatable :: tmp,wl,qbar,
     *                     datum,area,qpeakh,qpeaks,dpeakh,
     *                     dpeaks,volsyn,volhyd,
     *                     hydfctr,synfctr,ystr,xstr,
     *                     qhyd_dly_sum,qsyn_dly_sum,
     *                     qhyd_mly_sum,qsyn_mly_sum

c                                 vel,   already in areatr
      
      integer, dimension(:), allocatable :: iy,jx,iys,jxs,nlow,
     *                                              nopt,nopt1



!         these will replace iys & jxs respectively
      real*4,    dimension(:),allocatable :: xdamg_locn,ydamg_locn



!        integer,       dimension(:), allocatable :: iylatdeg,iylatmin,
!     *                                              jxlondeg,jxlonmin


        character(12), dimension(:), allocatable :: gage,damage
        character(80), dimension(:), allocatable :: note

        integer, parameter :: nnote=100
        integer :: ns,nr,mo,itogo,month1,month2,nnnote
        real*4    :: arain,tm

ccccc      END MODULE area10


!	qhyd( , ) are the observedlows
!	qsyn( , ) are the computed flows at the gage locations
!	qdam( , ) are the computed flows at the damage sites
!	iy, jx are the streamgauge locations
!	these are read from the str file
!	iys, jxs are the damage site locations
!	these are read from the basin\xxxx.str file is s/r strfw
!	frc( , ) are the polinimial coefficients used for the rating 
!	curves: first at the gages and then at the damage sites.
!	gage are the gauge names - up to 12 characters
!     qpeakh( , ) peak instantaneous recorded
!     qpeaks( , ) peak instantaneous computed
!     dpeakh( , ) peak daily mean recorded
!     dpeaks( , ) peak daily mean computed

ccccc      MODULE area11

        real*4, dimension(:,:), allocatable :: qrgrid

ccccc      END MODULE area11

! qrgrid - flow exchange grid



ccccc      MODULE area12

        character(999), dimension(:), allocatable :: fln
        character(999), dimension(:), allocatable :: filename
        character(999), dimension(:), allocatable :: outfln
        character(999), dimension(:), allocatable :: infln
        character(3) :: filetype
        character(1) :: chars(99) 
        integer   :: filename_length
        integer      :: Nreaches,Nreachesmax=45

!        DATA Nreachesmax/15/

ccccc      END MODULE area12





ccccc      MODULE AREA14
c        real, dimension(:,:,:), allocatable :: sn
ccccc      END MODULE area14


ccccc      MODULE area15
c        real, dimension(:), allocatable :: bsmc
ccccc      END MODULE area15

! bsmc is to use soil init moisture basin by basin
! and assures compatibility with old files


ccccc      MODULE area16

        real*4, dimension(:),     allocatable :: smc,smcrag,xsta,ysta,
c     *                                         temp,d,rsum,gsum,ratio,
     *                                         temp,rsum,gsum,ratio,
     *                                         axx,ayy
        real*4, dimension(:,:),   allocatable :: p,psum,pmin,rrain,radp,
     *                                         sumr,cltr,f,f1,sto,g,sum,
     *	                                     smc_class
        real*4, dimension(:,:,:), allocatable :: w
        integer, dimension(:),  allocatable :: ntogo,ewg,sng,sngmin,
     *                                         ewgmin
        integer                             :: nw

        character(12), dimension(:),allocatable :: gname

ccccc      END MODULE area16




ccccc      MODULE areacg

! COMMON BLOCK FOR CRAIG & GORDON ISOTOPE FRACTIONATION ROUTINE     

ccccc      END MODULE areacg






ccccc      MODULE areaet
!     rev. 9.1.80  Mar.  31/05  - NK: added sublimation   (sublim)

        real*4, dimension(:,:), allocatable :: vo,evap,intev,ev,
     *                        pet,x2,x3,intevt,evt,ssumr,v1,rad,
     *                        sublim,sum_sublim,sum_et,sum_pet
c     *                       , strloss_sum,qstream_sum

        real*4,   dimension(:), allocatable :: 
     *                        fpet2,flint,tto,ttomin,ttomax,
     *                        eloss,diff,hu,ffcap,fcap,totint,
     *                        spore,alb,uzsinit,radv,pres,flgtemp,rh,
     *                        sublim_factor,ice_factor

        real*4    :: lat,flgevp2,tempa1,tempa2,tempa3,uzsid,sinlat,
     *          coslat,tanlat,albe,alamb,den,alpha,tton,akt,flgtmp1,so
        integer :: ngauges,ntemps,ittoflg

ccccc      END MODULE areaet

ccccc      MODULE areaindx
c        integer :: n,ii,i,j
ccccc      END MODULE areaindx


ccccc      MODULE areamelt

        real*4, dimension(:,:),allocatable :: excess1,snowf,snowt,raint,
     *                                 ta,deld,dsno,top,bot,water,wlmax

        real*4,  dimension(:,:),allocatable :: snw,snowc,dsn,ttemp,tmx,
     *                         tmn,sca,oldsca,fexcess,snowcmin,wcl,sdcd,
     *                         sdcsca,ati,def,el

        real*4,    dimension(:), allocatable :: dsnow,tempv,tempvmin,
     *                         tmax,tmin,
     *                         tmin1,tmin2,base,fm,fmn,whcl,snocap,
     *                         tipm,uadj,elev,rho,qtot,robg,rosn,qnet,
     *                         smelt,excess,extra,qrain,qsnow,qrn,
     *                         qsn,glmelt,qe,qh,qn,qp,refrz,fmadj

        real*4    :: conv31,conv32,tlst31,tlst32,conv33,tlst33,daygm,
     *             gladjust,rlapse,elvref,fmadjust,fmalow,fmahigh

        integer, dimension(:), allocatable :: nsdc,idump

        integer :: idt31,idt32,mltpivot,mflag,iiout

ccccc      END MODULE areamelt

ccccc      MODULE areanash

        real*4,    dimension(:), allocatable :: aa,bb,cc,ashnum,ashden,
     *                         rsquare,qbarobs

        integer, dimension(:), allocatable :: nq,nqc

ccccc      END MODULE areanash



    
ccccc      MODULE areaopts
        real*4, dimension(:), allocatable :: fmdlt,fmlow,fmhgh,fmndlt,
     *                         fmnlow,fmnhgh,uajdlt,uajlow,uajhgh,
     *                         basdlt,baslow,bashgh
        integer,dimension(:), allocatable :: mbsdlt,mbslow,mbshgh
        real*4 :: a5dlt,a5low,a5hgh,a8dlt,a8low,a8hgh,a9dlt,a9low,a9hgh,
     *          a10dlt,a10low,a10hgh,a11dlt,a11low,a11hgh,
     *          a12dlt,a12low,a12hgh
c        integer :: nrec       >> used in wqnut only - see below
ccccc      END MODULE areaopts

!  snow optimization parameters



ccccc      MODULE areatrc

! COMMON BLOCK FOR TRACER/ISOTOPE ROUTINE     

      INTEGER :: nofer
      INTEGER   :: itrace,    ! 0=Sub-basin   1=Glacier  2=Landcover
!                             3=Rain-on-stream 4=Flow-type   5=Snowmelt
!                             100=Original GW Tracer (NK)  
!                             101=Wetland Tracer
     *             icount
      REAL*4    :: ncrn,nscn,ncpw,nrec,nlec,pscn,pcpw,prec,plec,ndec,
     *             snwwt,isowt,oldiso,oldiso1,oldiso2,oldiso3,oldiso4,
     *             oldiso5,oldiso6,oldiso7,oldiso8,wetinit
      REAL*4    :: pdec,sdep,navr,navs,ndmv,nrmv,pdmv,prmv,nrnc
	REAL*8    :: sqerr
      REAL*4, DIMENSION(:), ALLOCATABLE :: 
     * massin,massout,masstore,ISOdelta,wmassin,wmassout,wmasstore,
     * wISOdelta,tt,vel,disp,coeff
	INTEGER, DIMENSION(:), ALLOCATABLE :: nn
      
!      DATA itrace/5/
	DATA wetinit/0.75/icount/0/


!     SUB-BASIN TRACERS = TRACER 0
      REAL*4, DIMENSION(:), ALLOCATABLE :: 
     *isoin1IBN,isoin2IBN,isoout1IBN,isoout2IBN,
     *isoconcIBN,isostore1IBN,isostore2IBN,
     *isosumQ,isosumQfs

!     GLACIER TRACERS = TRACER 1


!     LANDCOVER TRACERS = TRACER 2
      REAL*4, DIMENSION(:,:,:), ALLOCATABLE :: 
     *isoin1LC,isoin2LC,isoout1LC,isoout2LC,
     *isoconcLC,isostore1LC,isostore2LC,isoBLZS1,isoBLZS2,
     *isoin1BLZS,isoin2BLZS,isoout1BLZS,isoout2BLZS,isoconcBLZS
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: isosum

!     RAIN TRACERS = TRACER 3
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: 
     *isoin1P,isoin2P,isoout1P,isoout2P,
     *isoconcP,isostore1P,isostore2P

!     FLOW-TYPE TRACERS = TRACER 4
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: 
     *isoin1GW,isoin2GW,isoout1GW,isoout2GW,
     *isoconcGW,isostore1GW,isostore2GW,
     *isoin1SW,isoin2SW,isoout1SW,isoout2SW,
     *isoconcSW,isostore1SW,isostore2SW,
     *isoin1IF,isoin2IF,isoout1IF,isoout2IF,
     *isoconcIF,isostore1IF,isostore2IF

ccccc      MODULE area2


!     SNOWMELT TRACERS = TRACER 5
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: 
     *isoin1SWfs,isoin2SWfs,isoout1SWfs,isoout2SWfs,
     *isoconcSWfs,isostore1SWfs,isostore2SWfs,
     *isoin1IFfs,isoin2IFfs,isoout1IFfs,isoout2IFfs,
     *isoconcIFfs,isostore1IFfs,isostore2IFfs,
     *isoin1GWfs,isoin2GWfs,isoout1GWfs,isoout2GWfs,
     *isoconcGWfs,isostore1GWfs,isostore2GWfs,
     *isoLZS1fs,isoLZS2fs,isoin1LZSfs,isoin2LZSfs,isoout1LZSfs,
     *isoout2LZSfs,isoconcLZSfs,isoLZS1,isoLZS2,
     *isoin1LZS,isoin2LZS,isoout1LZS,isoout2LZS,isoconcLZS

!     WETLAND TRACERS = TRACER 101
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: 
     *isoin1QWET,isoin2QWET,isoout1QWET,isoout2QWET,isoconcQWET,
     *isostore1QWET,isostore2QWET

!     LAKE TRACER INFLOWS
      REAL*4, DIMENSION(:,:), ALLOCATABLE :: isolakeGW

ccccc      END MODULE areatrc


ccccc      MODULE areawet


      real*4, dimension(:), allocatable :: wetwid,chawid,chadep,qswrain,
     *             wstore1,wstore2,wcap,flowxa,
     *             chaxa,satxa,wetxa,qin,hcha1,
     *             hcha2,hwet1,hwet2,qiwet1,
     *             qiwet2,qowet1,qowet2,qswevp,
     *             kcond,theta,widep,chaarea,wetarea,wsat,wetfrac
		
      
      REAL*4 :: isowetwt,isowetold,oldisow,isooutwet
      REAL*4 :: isowetold1,isowetold2,isowetold3,isowetold4,isowetold5,
     *        isowetold6,oldisow1,oldisow2,oldisow3,oldisow4,oldisow5,
     *        oldisow6,isooutwet1,isooutwet2,isooutwet3,isooutwet4,
     *        isooutwet5,isooutwet6

	real*4, dimension(:,:), allocatable :: isoin1wet,isoin2wet,
     *             isoout1wet,isoout2wet,isoconcwet,isowstore1,
     *             isowstore2
	real*4, dimension(:,:), allocatable :: isoin1SWwet,isoin2SWwet,
     *             isoout1SWwet,isoout2SWwet,isoconcSWwet,isowstore1SW,
     *             isowstore2SW,isoin1IFwet,isoin2IFwet,
     *             isoout1IFwet,isoout2IFwet,isoconcIFwet,isowstore1IF,
     *             isowstore2IF
	real*4, dimension(:,:), allocatable :: isoin1fswet,isoin2fswet,
     *             isoout1fswet,isoout2fswet,isoconcfswet,isowstore1fs,
     *             isowstore2fs,isoin1SWfswet,isoin2SWfswet,
     *      isoout1SWfswet,isoout2SWfswet,isoconcSWfswet,isowstore1SWfs,
     *      isowstore2SWfs,isoin1IFfswet,isoin2IFfswet,
     *      isoout1IFfswet,isoout2IFfswet,isoconcIFfswet,isowstore1IFfs,
     *      isowstore2IFfs



ccccc      END MODULE areawet
  

! TS - Nov 20/03: Added isotope tracer parameters to wetland module
! TS - Jan 30/04: Added more isotope tracer parameters to wetland module
! TS - Apr 25/05: Added more isotope tracer parameters (for snowmelt)





! PARAMETER LIST:
!
! wetwid(n)  - width of wetland coverage to one side of stream channel
! chanwid(n) - width of the stream channel
! chandep(n) - depth of the full stream channel
! wstore1(n) - wetland storage at beginning of time step
! wstore2(n) - wetland storage at end of time step
! wcap(n)    - wetland capacity (maximum storage)
! flowxa(n)  - cross-sectional area of the depth of flow in the channel
! chanxa(n)  - cross-sectional area of the channel
! satxa(n)   - cross-sectional area of the saturation depth in the wetland
! wetxa(n)   - cross-sectional area of the wetland
! hcha(n)    - height of water in the stream channel
! hwet(n)    - height of water in the wetland
! wetarea(n) - plan area of wetlands in m^2
! channel_area(n) - plan area of channel in m^2
! widep      - width-to-depth ratio (a11 in PAR file)
! theta      - wetland soil porosity (a9 in PAR file)
! kcond      - soil conductivity (a10 in PAR file)




ccccc      MODULE areawfo


!     note that outwfo is (column,row) & outarray is (row,column)

      real*4, dimension(:,:), allocatable:: outwfo,outarray,inarray

      real*4, dimension(:),   allocatable::wfo_sum_p,wfo_cum_p 



      real*4         :: xorigin,yorigin,xdelta,ydelta,angle
      real*4, dimension(:),   allocatable::latgrid,longrid
      integer      :: xcount,ycount,deltat
      character(10) ::  starttime,startdate

        CHARACTER(64), DIMENSION(:), ALLOCATABLE :: attname
        CHARACTER(32), DIMENSION(:), ALLOCATABLE :: attunits



ccccc      END MODULE areawfo
! common for sediment and nutrient subroutines:      

ccccc      module areawq

c      real*4 :: gamma,ro,viskin,grav,a,b
      real*4 :: gamma,ro,viskin,grav,a_wq,b_wq
!       changed a & b to a_wq and b_wq

!     rev. 9.4.02  Apr.  18/07  - NK: moved rf, rffs from areawq to area1
c      real*4, dimension(:,:),  allocatable::
c     *                         rf,rffs


      Real*4 :: d50(17500),spg(17500),
     *erod(17500),ernfl(17500),y(17500,16),kf(16),gc(16),cf(16),
     *yrot(17500),qs(17500,16),hsed(17500,16),ql(17500,16),
     *diam(400,400),spew(400,400),erodi(400,400),
     *nfer(17500),nfa(17500),pfer(17500),pfa(17500),
     *cronrot(17500),croprot(17500),
     *cron(17500,16),crop(17500,16),
     *mnfer(400,400),mnfa(400,400),mpfer(400,400),mpfa(400,400),
     *WI1(17500),WI2(17500),WO1(17500),WO2(17500),yfinal(17500),
     *WI1n(17500),WI2n(17500),WO1n(17500),WO2n(17500),nfinal(17500),
     *WI1p(17500),WI2p(17500),WO1p(17500),WO2p(17500),pfinal(17500),
     *ss1(17500),ss2(17500),SEDHYD(60,8784),SEDSYN(60,8784),
     *ss1n(17500),ss2n(17500),NITHYD(60,8784),NITSYN(60,8784),
     *ss1p(17500),ss2p(17500),PHSHYD(60,8784),PHSSYN(60,8784),
     *sedmss(60,8784),nitmss(60,8784),phsmss(60,8784),
     *psat(17500),sat(400,400)
      

c          these are already in areatr
c      real*4 :: ncrn,nscn,ncpw,nrec,nlec,pscn,pcpw,prec,plec,ndec
c      real*4 :: pdec,sdep,navr,navs,ndmv,nrmv,pdmv,prmv,nrnc
c      integer :: nofer

      real*4 ::    flow_shear,rey,crit_shear,c_val,y_crit,
     *           coef_cover,grf,ero,gro,y_pot

c           phi, already in areatr


ccccc      end module areawq


      end module area_watflood
