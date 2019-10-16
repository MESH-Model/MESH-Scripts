load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin

  yDelta= stringtofloat(getenv("yDelta"))  ;
  xDelta= stringtofloat(getenv("xDelta"))  ;
  xCount= stringtointeger(getenv("xCount"))  ;
  yCount= stringtointeger(getenv("yCount"))  ;
  minlon= stringtofloat(getenv("xOrigin"))+xDelta/2  ;
  minlat= stringtofloat(getenv("yOrigin"))+yDelta/2  ;
  year_opt = stringtointeger(getenv("YEAR1"))  ;
  months_s = stringtointeger(getenv("MONTHS_Start"))  ;
  months_e = stringtoint(getenv("MONTHS_End")) ; 
  maxlon= minlon+(xCount*xDelta)
  maxlat= minlat+(yCount*yDelta)

  diri=(getenv("IN_DIR"))
  print(diri)   
  
  diri_constant=(getenv("CONST_DIR"))

  print(xDelta)
  print(yDelta)
  print(xCount)
  print(yCount)
  print(minlon)
  print(minlat)
  print(year_opt)
 
 do months = months_s,months_e
 dim_1 = days_in_month(year_opt,months)
   print(dim_1)

  do dim = 1,dim_1

   di_2=(24*dim)
   di_1=di_2 - 23

   print(di_1)
   print(di_2)

   dij = ispan(di_1,di_2,1)

   day_cf= systemfunc("date --date '"+year_opt+"-0"+months+"-"+dim+" ' +%Y-%m-%d | cut -d'-' -f3 | cut -c1-2")
   day_current=stringtoint(day_cf)
   month_cf = systemfunc("date --date '"+year_opt+"-0"+months+"-"+dim+" ' +%Y-%m-%d | cut -d'-' -f2 | cut -c1-2")
   month_current=stringtoint(month_cf)
   year_cf = systemfunc("date --date '"+year_opt+"-0"+months+"-"+dim+" ' +%Y-%m-%d | cut -d'-' -f1 | cut -c1-4")
   year_current=stringtoint(year_cf)

   ctrl_can_max = diri+year_cf+"/wrf_h2d_d01_"+year_cf+"-"+month_cf+"-"+day_cf+"_00"
   b = addfile(ctrl_can_max, "r")

   ctrl_can_const = diri_constant+"/wrfout_d01_constant.nc"
   a        = addfile(ctrl_can_const,"r")

;---Read temperature at first time step

  print(ctrl_can_max)
  tc_can = b->GLW
  tc_can@units = "W m-2"
  tc_can@description = "DOWNWARD LONG WAVE FLUX AT GROUND SURFACE"
 
;---Attach 2D coordinates

  tc_can@lat2d = a->XLAT(0,:,:)
  tc_can@lon2d = a->XLONG(0,:,:)

; Start the regridding
;----------------------------------------------------------------------
  ;interp_method         = "conserve"
  ;Opt                   = True
  ;Opt@InterpMethod      = interp_method
  ;Opt@SrcRegional       = True
  ;Opt@DstRegional       = True
  ;Opt@DstLLCorner       = (/ minlat, minlon /)
  ;Opt@DstURCorner       = (/ maxlat, maxlon /)
  ;Opt@ForceOverwrite    = True
  ;Opt@DstGridType = ""+xDelta+"deg"
  ;Opt@PrintTimings      = True
  ;Opt@Debug             = True
   
   Optw                = True
   Optw@PrintTimings   = True
   wgtFileName = diri_constant + "/weights_file.nc"
   
  ;tc_can_regrid_0_5deg = ESMF_regrid(tc_can,Opt)     ; Do the regridding Canadian Domain
  tc_can_regrid = ESMF_regrid_with_weights(tc_can,wgtFileName,Optw)     ; Do the regridding Canadian Domain
  dsizes_x = dimsizes(tc_can_regrid)

  ntim     = dsizes_x(0)        ; time block
  nlat     = dsizes_x(1)        ; rows
  mlon     = dsizes_x(2)        ; columns

  dq=str_get_dq()               ; Define the double quotes for printing

  do i=0,ntim-1 

  XX = tc_can_regrid(i,0:(yCount-1),0:(xCount-1))
  
  fWidth = 15    ; specify the format width
  fDec   = 5     ; specify the number to the right of decimal point
  XX@_FillValue = -99.0             ; change to some value
  xmsg   = XX@_FillValue              ; save _FillValue


  fmtx   = mlon + "f" + fWidth + "." + fDec              ; fmtx="50f10.3"
  opt = True

  if (i .lt. 10) then
   opt@title  =":Frame  "+dij(i)+"    "+dij(i)+"     "+dq+year_cf+"/"+month_cf+"/"+day_cf+" 0"+i+":00:00"+dq
  else
   opt@title  =":Frame  "+dij(i)+"    "+dij(i)+"    "+dq+year_cf+"/"+month_cf+"/"+day_cf+" "+i+":00:00"+dq
  end if
  opt@row    = False
  opt@fout = "GPRINT"+day_cf+"_"+i

  write_matrix (XX, fmtx, opt)
  XX@_FillValue = xmsg

  system("echo :EndFrame>> " + "GPRINT"+day_cf+"_"+i)

 print(dij(i))

  end do  ; ends of hours

  end do  ; ends of days

  filename = "Basin_GLW_"+year_opt+"_"+month_cf+".r2c"
  system("bash join.sh" + " " +day_cf+ " " +filename)

  print(dij)

  end do ; ends of months

  end    ; end of code

  print("Finished Conversion GLW")
