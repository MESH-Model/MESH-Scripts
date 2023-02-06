!> Description:
!>  Module to calculate theoretical shortwave radiation using methods
!>  proposed by Garnier and Ohmura (1970) to adjust for elevation, slope
!>  and aspect. The module updates precipitation, temperature, specific
!>  humidity, pressure, wind speed, and incoming longwave radiation,
!>  accounting for elevation difference relative to NWP reference and
!>  topographic effect.
!>
!> Author:
!>  Zelalem Tesemma (initially based on Solar_Adjust.m; last updated Jan 30, 2018)
!>
!> Notes:
!>  - 2018/02/01: Converted to Fortran (exact copy)
!>  - 2018/02/02: Fortran code optimized/consolidated
!>      ('program' component replaced by 'solar_adjust_module')
!>  - 2019/10/10: Upgraded into Mountain MESH (renamed 'forcing_adjust')
!>  - 2020/04/16: Added phased precipitation adjustment.
!>  - 2021/01/31: Clean up and Optimised.
!> Flags to control MESH runs
!*  ipre: Flag to specify precipitation adjustment method. [--].
!>      0: None.
!>      1: Elevation Range with Maximum elevation Method (ERMM) (Zhang et al. (2018)) and (Tesfa et al. (2019))
!>      2: Adjustment based on Thornton, 1997 or derived lapse-rate for elevation.
!*  itemp: Flag to specify temperature adjustment method. [--].
!>      0: None.
!>      1: Lapse-rate adjustment.
!*  ipres: Flag to specify pressure adjustment method. [--].
!>      0: None.
!>      1: Elevation adjustment.
!*  ihumd: Flag to specify specific humidity adjustment method. [--].
!>      0: None.
!>      1: Adjustment based on Kunkel, 1989.
!>      2: Adjustment based on Murray, 1967.
!>      3: No adjustment
!*  irlds: Flag to specify longwave radiation adjustment method. [--].
!>      0: None.
!>      1: Correction based on temperature and vapour pressure (Sicart et al., 2005)
!>      2: Lapse-rate adjustment.
!*  iwind: Flag to specify wind speed adjustment method. [--].
!>      0: None.
!>      1: Adjustment based on Liston and Sturm, 1998 (requires wind direction, winddir).
!>      2: Lapse-rate adjustment.
!*  iphase: Flag to specify precipitation phase partitioning method. [--].
!>      0: Partioning to 0.0 degrees C.
!>      1: Partitioning based on Harder and Pomeroy, 2013.
!*  irsrd: Flag to specify shortwave radiation adjustment method. [--].
!>      0: None.
!>      1: Adjustment based on Garnier and Ohmura, 1970.
!>      2: Liston, Elder - 2006 - A Meteorological Distribution System for High-Resolution Terrestrial Modeling (MicroMet).
!*  idecl: Flag to specify calculation to use for declination. [--].
!>      0: Alternate approach.
!>      1: Declination calculation (Dingman, 2015 and Iqbal, 1983).
!>
!> Input variables:
!*  elev: Weighted average elevation of GRUs. [m].
!*  xlng: Longitude. [degrees].
!*  ylat: Latitude. [degrees].
!*  slope: Weighted average slope of the surface. [--].
!*  aspect: Weighted average aspect of the surface.
!*  delta: Weighted average elevation difference between GEM and MESH. [m].
!*  delta_elevmax: weighted average of elevation difference between low and high resolution dem divided by the maximum elevation of low res dem
!*  curvature: Weighted average curvature of the surface for GRUs.
!*  nvals: Number of elements in the vector (e.g., 1:grids, 1:tiles, etc.). [--].
!*  nml_grid_map: Lookup table of grid ID from tile ID. [--].
!*  i1: ID of first grid active on current processing node. [--].
!*  i2: ID of last grid active on current processing node. [--].
!*  tlapse: Table of lapse rate values for temperature. [--].
!*  plapse: Table of lapse rate values for precipitation. [--].
!*  dtlapse: Table of lapse rate values for dew point temperature. [--].
!*  lwlapse: Table of lapse rate values for longwave radiation. [--].
!*  wlapse: Table of lapse rate values for wind speed. [--].
!*  rsrd_dtmin: Time-step of incoming shortwave radiation data. [minutes].
!*  rlds_dtmin: Time-step of incoming longwave radiation data. [minutes].
!*  temp_dtmin: Time-step of air temperature. [minutes].
!*  pres_dtmin: Time-step of barometric pressure. [minutes].
!*  humd_dtmin: Time-step of specific humidity. [minutes].
!*  rain_dtmin: Time-step of precipitation. [minutes].
!*  wind_dtmin: Time-step of wind speed. [minutes].
!*  winddir_dtmin: Time-step of wind direction. [minutes].
!*  rsrd: Incoming shortwave radiation (input). [W m-2].
!*  rlds: Incoming longwave radiation (input). [W m-2].
!*  temp: Air temperature (input). [K].
!*  pres: Barometric pressure (input). [Pa].
!*  humd: Specific humidity (input). [kg kg-1].
!*  rain: Precipitation. [mm s-1].
!*  wind: Wind speed. [m s-1].
!*  winddir: Wind direction. [degree].
!*  now_year: Present year. [--].
!*  now_month: Present month. [--].
!*  now_jday: Present day in year. [--].
!*  now_hour: Present hour in day (00-23). [--].
!*  now_mins: Present minutes in hour (00, 30). [--].
!*  dtmins: Model time-step. [minutes].
!*  Curveweight: wind model curvature weight
!*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
!>
!> Output variables:
!*  rsrd_adjusted: Adjusted incoming short wave radiation. [W m-2].
!*  rlds_adjusted: Adjusted incoming long wave radiation. [W m-2].
!*  temp_adjusted: Adjusted air temperature. [K].
!*  pres_adjusted: Adjusted barometric pressure. [Pa].
!*  humd_adjusted: Adjusted specific humidity. [kg kg-1].
!*  rain_adjusted: Adjusted precipitation. [mm s-1].
!*  rain_phased_adjusted: Adjusted liquid component of precipitation. [mm s-1].
!*  snow_phased_adjusted: Adjusted solid component of precipitation. [mm s-1].
!*  wind_adjusted: Adjusted wind speed. [m s-1].
subroutine forcing_adjust( &
    elev, xlng, ylat, slope, aspect, delta, delta_elevmax, &
    curvature, nvals, nml_grid_map, i1, i2, &
    CurveWeight, CalcFreq, &
    ipre, itemp, ipres, ihumd, irlds, iwind, iphase, irsrd, idecl, &
    tlapse, plapse, dtlapse, lwlapse, wlapse, &
    rsrd_dtmin, rlds_dtmin, temp_dtmin, &
    pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, winddir_dtmin, &
    rsrd, rlds, temp, pres, humd, rain, wind, winddir, &
    rsrd_adjusted, rlds_adjusted, temp_adjusted, pres_adjusted, &
    humd_adjusted, rain_adjusted, rain_phased_adjusted, &
    snow_phased_adjusted, wind_adjusted, &
    now_year, now_month, now_jday, now_hour, now_mins, dtmins)

    implicit none

    !> Constants and conversion factors.
    real, parameter :: GCons = 9.80616                 ! Gravitational constant in m s-2 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: RCons = 287.04                  ! Gas constant for dry air in J kg-1 K-1 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: pi = 3.14159265
    real, parameter :: DEGtoRAD = pi/180.0
    real, parameter :: DEGtoRAD365 = 2.0*pi/365.0

    !> Input variables.
    integer, intent(in) :: nvals
    real, dimension(nvals), intent(in) :: &
        elev, xlng, ylat, slope, aspect, &
        delta, delta_elevmax, curvature
    integer, dimension(nvals), intent(in) :: nml_grid_map
    integer, intent(in) :: i1, i2
    real, intent(in) :: CurveWeight
    integer, intent(in) :: CalcFreq
    integer, intent(in) :: ipre, itemp, ipres, ihumd, irlds, iwind, iphase, irsrd, idecl
    real, dimension(12), intent(in) :: plapse, lwlapse, wlapse
    real, dimension(1:12, 0:23), intent(in) :: tlapse, dtlapse
    integer, intent(in) :: &
        rsrd_dtmin, rlds_dtmin, temp_dtmin, &
        pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, &
        winddir_dtmin
    real, dimension(nvals), intent(in) :: &
        rsrd, rlds, temp, pres, &
        humd, rain, wind, winddir
    integer, intent(in) :: &
        now_year, now_month, now_jday, &
        now_hour, now_mins, dtmins

    !> Output variables.
    real, dimension(nvals), intent(out) :: rsrd_adjusted
    real, dimension(nvals), intent(out) :: rlds_adjusted
    real, dimension(nvals), intent(out) :: temp_adjusted
    real, dimension(nvals), intent(out) :: pres_adjusted
    real, dimension(nvals), intent(out) :: humd_adjusted
    real, dimension(nvals), intent(out) :: rain_adjusted
    real, dimension(nvals), intent(out) :: rain_phased_adjusted
    real, dimension(nvals), intent(out) :: snow_phased_adjusted
    real, dimension(nvals), intent(out) :: wind_adjusted

    !> Working variables.
    integer MINS_int, kk, k
    real EOTCons, EOT, Decl, Dcon, Rad_vec, Sol, Cdecl, Sdecl, Trans
    real, dimension(nvals) :: &
        Clat, Slat, Sslp, Cslp, Sasp, Casp, SHVar, SH, SM, tsolar, &
        Hr_Ang, t1, t2, Czen, ACzen, oam, diff, Iterr, cosxs0, cosxsL, &
        Idiff, Sum_Idir, Sum_Diff, Sum_Flatd, Sum_Flatf, Qdirect, &
        Idir, Qdiffuse, Qflat, es, es_adjusted, ea, ea_adjusted, &
        rh, Tdew, Tdew_adjusted, F_factor, rain_frac, windcorr, &
        oam1, oam2, OmegaC, OmegaS, &
        Ti1, Ti2, crit, crit1, &
        TT1, TT2, ratio, gru_icebulb, &
        Szen, cos_i, cloud_frac, xhour, sun_azimuth, aspect_S0
    real(kind = 8), dimension(nvals) :: D, lamda, pta, L, aa, bb, cc
    real, dimension(i1:i2) :: OmegaS_grid, curvature_grid, grid_max_elev, grid_sum_elev, num_grus

    !> Wind speed adjustment.
    if (iwind == 1) then

        !> Option 1:
        !>  Using the wind weighting factor to modify the wind speed (Liston and Sturm, 1998).
        OmegaS_grid = 0.0
        curvature_grid = 0.0
        OmegaS = DEGtoRAD*slope*cos((winddir - aspect)*DEGtoRAD)
        do k = 1, nvals
            if (abs(OmegaS(k)) > OmegaS_grid(nml_grid_map(k))) then
                OmegaS_grid(nml_grid_map(k)) = abs(OmegaS(k))
            end if
            ! if (abs(curvature(k)) > curvature_grid(nml_grid_map(k))) then
                ! curvature_grid(nml_grid_map(k)) = abs(curvature(k))
            ! end if
        end do
        do k = 1, nvals
            OmegaS(k) = OmegaS(k)/(2.0 * max(0.001,OmegaS_grid(nml_grid_map(k))))
            ! OmegaC(k) = curvature(k)/(2.0 * max(0.001,curvature_grid(nml_grid_map(k))))
        end do
        ! windcorr = 1.0 + (1.0 - CurveWeight)*OmegaS + CurveWeight*OmegaC           !> (1.0 - CurveWeight) wind model slope weight
        windcorr = 1.0 + (1.0 - CurveWeight)*OmegaS + CurveWeight*curvature          !> (1.0 - CurveWeight) wind model slope weight
        wind_adjusted = wind*windcorr
    else if (iwind == 2) then

        !> Option 2:
        !>  Using wind speed lapse rate for elevations.
        windcorr = wlapse(now_month)*(delta)/1000.0
        where (abs(windcorr) > 0.95)
            wind_adjusted = wind*(1.0 + 0.95)/(1.0 - 0.95)
        elsewhere
            wind_adjusted = wind*(1.0 + windcorr)/(1.0 - windcorr)
        end where
    else

        !> Option 3:
        !>  No adjustment.
        wind_adjusted = wind
    end if

    !> Precipitation adjustment.
    if (ipre == 1) then

        !> Option 1:
        !>  Precipitation downscaling using Elevation Range with Maximum elevation Method (ERMM)
        !>  (Zhang et al. (2018)) and (Tesfa et al. (2020)).
        ! grid_max_elev = 0.0
        ! grid_sum_elev = 0.0
        ! num_grus = 0.0
        ! do k = 1, nvals
        ! if (elev(k) > grid_max_elev(nml_grid_map(k))) then
        ! grid_max_elev(nml_grid_map(k)) = elev(k)
        ! end if
        ! grid_sum_elev(nml_grid_map(k)) = grid_sum_elev(nml_grid_map(k)) + elev(k)
        ! num_grus(nml_grid_map(k))= num_grus(nml_grid_map(k)) + 1.0
        ! end do
        ! do k = 1, nvals
        ! ! rain_frac(k) = delta(k)/grid_max_elev(nml_grid_map(k))
        ! rain_frac(k) = (elev(k) - (grid_sum_elev(nml_grid_map(k))/num_grus(nml_grid_map(k))))/ &
        ! grid_max_elev(nml_grid_map(k))
        ! end do
        ! rain_adjusted = rain + rain*rain_frac
        rain_adjusted = rain * (1.0 + delta_elevmax)
    else if (ipre == 2) then

        !> Option 2:
        !>  Precipitation correction using elevation based lapse rate (Thornton, 1997) or
        !>  precipitation lapse rate derived from the high resolution GEM (2.5km by 2.5km).
        rain_frac = plapse(now_month) * min(delta, 1200.0)/1000.0
        where (abs(rain_frac) > 0.95)
            rain_adjusted = rain*(1.0 + 0.95)/(1.0 - 0.95)
        elsewhere
            rain_adjusted = rain*(1.0 + rain_frac)/(1.0 - rain_frac)
        end where
    else

        !> Option 3:
        !>  No adjustment.
        rain_adjusted = rain
    end if

    !> Temperature adjustment.
    if (itemp == 1) then

        !> Option 1:
        !>  Linear lapse rates (measured, seasonal, constant, neutral stability) using the table provided.
        temp_adjusted = temp - (tlapse(now_month,now_hour)*delta/1000.0)
    else

        !> Option 2:
        !>  No adjustment.
        temp_adjusted = temp
    end if

    !> Pressure adjustment.
    if (ipres == 1) then

        !> Option 1:
        !>  Pressure correction for elevation differences.
        pres_adjusted = pres*exp(-(delta*GCons)/(RCons*temp_adjusted))
    else

        !> Option 2:
        !>  No adjustment.
        pres_adjusted = pres
    end if

    !> Specific humidity adjustment.
    if (ihumd == 1) then

        !> Option 1:
        !>  Specific humidity correction for elevation difference using dew point temperature (Kunkel, 1989).
        ea = humd*pres_adjusted/(0.378*humd + 0.622)
        where (temp_adjusted >= 273.16)
            Tdew = 240.97*log(ea/611.21)/(17.502 - log(ea/611.21))  ! Buck, 1981 for temperatures 0°C and above.
        elsewhere
            Tdew = 272.55*log(ea/611.15)/(22.452 - log(ea/611.15))  ! Buck, 1981 for temperature less than zero.
        end where
        Tdew_adjusted = Tdew - (dtlapse(now_month,now_hour)*delta/1000.0)
        where (temp_adjusted >= 273.16)
            ea_adjusted = 611.21*exp(17.502*Tdew_adjusted/(Tdew_adjusted + 240.97))
        elsewhere
            ea_adjusted = 611.15*exp(22.452*Tdew_adjusted/(Tdew_adjusted + 272.55))
        end where
        humd_adjusted = 0.622*ea_adjusted/(pres_adjusted - 0.378*ea_adjusted)
    else if (ihumd == 2) then

        !> Option 2:
        !>  Specific humidity correction for elevation difference using
        !>  dew point temperature from (Murray, 1967).
        ea = humd*pres_adjusted/(0.378*humd + 0.622)
        where (temp_adjusted >= 273.16)
            Tdew = 237.29*log(ea/610.78)/(17.269 - log(ea/610.78))  ! Murray (1967) as used in CLASS_v3.6 for temperature greater than or equal to zero.
        elsewhere
            Tdew = 265.49*log(ea/610.78)/(21.875 - log(ea/610.78))  ! Murray (1967) as used in CLASS_v3.6 for temperature less than zero.
        end where
        Tdew_adjusted = Tdew - (dtlapse(now_month,now_hour)*delta/1000.0)
        where (temp_adjusted >= 273.16)
            ea_adjusted = 610.78*exp(17.269*Tdew_adjusted/(Tdew_adjusted + 237.29))
        elsewhere
            ea_adjusted = 610.78*exp(21.875*Tdew_adjusted/(Tdew_adjusted + 265.49))
        end where
        humd_adjusted = 0.622*ea_adjusted/(pres_adjusted - 0.378*ea_adjusted)
    else

        !> Option 3:
        !>  No adjustment.
        humd_adjusted = humd
    end if

    !> Longwave radiation adjustment.
    if (irlds == 1) then

        !> Option 1:
        !>  Incoming longwave radiation correction based on temperature and vapour pressure (Brutsaert (1975)).
        F_factor = rlds/(1.24*((0.01*ea/temp)**(1.0/7.0))*5.67e-8*temp**4.0)
        rlds_adjusted = F_factor*(1.24*(0.01*ea_adjusted/temp_adjusted)** &
        (1.0/7.0)*5.67e-8*temp_adjusted**4.0)
    else if (irlds == 2) then

        !> Option 2: Based on elevation lapse rate values.
        !>  Incoming longwave solar radiation correction for topography.
        rlds_adjusted = rlds - (lwlapse(now_month)*delta/1000.0)
    else

        !> Option 3:
        !>  No adjustment.
        rlds_adjusted = rlds
    end if

    !> Precipitation phase adjustment.
    if (iphase == 1) then

        !> Option 1:
        !>  Calculates precipitation phase via falling hydrometeor energy balance (Harder and Pomeroy, 2013).
        D = 2.06*(10.0e-05)*(temp_adjusted/273.15)**1.75
        lamda = 0.000063*temp_adjusted + 0.00673
        where (temp_adjusted < 273.16)
            L = 1000.0*(2834.1 - 0.29*(temp_adjusted - 273.16) - 0.004*(temp_adjusted - 273.16)**2)
        elsewhere
            L = 1000.0*(2501.0 - (2.361*(temp_adjusted - 273.16)))
        end where
        pta = 0.01801528*611.0*exp(17.3*(temp_adjusted - 273.16)/(237.3 + (temp_adjusted - 273.16)))/(8.31441*temp_adjusted)
        Ti1 = 250.0
        crit = 9999.0
        do while (any(crit > 0.0001))   ! Iteration solution optimised by using the Newton-Raphston method.
            TT1 = Ti1 + 0.001*Ti1
            TT2 = Ti1 - 0.001*Ti1
            aa = -Ti1 + temp_adjusted + &
                (L*D/lamda)*(pta - 0.01801528*611.0*exp(17.3*(Ti1 - 273.16)/(237.3 + (Ti1 - 273.16)))/(8.31441*Ti1))
            bb = -TT1 + temp_adjusted + &
                (L*D/lamda)*(pta - 0.01801528*611.0*exp(17.3*(TT1 - 273.16)/(237.3 + (TT1 - 273.16)))/(8.31441*TT1))
            cc = -TT2 + temp_adjusted + &
                (L*D/lamda)*(pta - 0.01801528*611.0*exp(17.3*(TT2 - 273.16)/(237.3 + (TT2 - 273.16)))/(8.31441*TT2))
            Ti2 = Ti1 - real(aa/((bb - cc)/(0.002*Ti1)))
            crit1 = Ti1 - Ti2
            where (crit1 < 0.0)
                crit = -crit1
            elsewhere
                crit = crit1
            end where
            Ti1 = Ti2
        end do
        gru_icebulb = Ti1 - 273.16
        where (gru_icebulb < -10.0) ! Eoverflow if ratio calculated with icebulb < -39C.
            ratio = 0.0
        elsewhere
            ratio = 1.0/(1.0 + 2.50286*(0.125006**gru_icebulb))
        end where
        rain_phased_adjusted = rain_adjusted*ratio          ! if(rain_adjusted > 0.0) then the rain or snow determined by ice bulb ratio.
        snow_phased_adjusted = rain_adjusted*(1.0 - ratio)
    else

        !> Option 2:
        !>  Partioning to 0.0 degrees C.
        where (temp_adjusted > 273.16)
            rain_phased_adjusted = rain_adjusted
            snow_phased_adjusted = 0.0
        elsewhere
            rain_phased_adjusted = 0.0
            snow_phased_adjusted = rain_adjusted
        end where
    end if

    !> Shortwave radiation adjustment.
    !> Considers elevation, slope, and aspect.
    !> Constant over time.
    Clat = cos(ylat*DEGtoRAD)
    Slat = sin(ylat*DEGtoRAD)
    Sslp = sin(slope*DEGtoRAD)
    Cslp = cos(slope*DEGtoRAD)
    Sasp = sin(aspect*DEGtoRAD)
    Casp = cos(aspect*DEGtoRAD)

    !> Calculate geometry.
    Dcon = DEGtoRAD365*(real(now_jday) - 1.0 + ((real(now_hour) - 12.0)/24.0))
    if (idecl == 1) then

        !> Option 1:
        !>  Declination calculation (Dingman, 2015 and Iqbal, 1983).
        Decl = 0.006918 - 0.399912*cos(Dcon) + 0.070257*sin(Dcon) &
            - 0.006758*cos(2.0*Dcon) + 0.000907*sin(2.0*Dcon) &
            - 0.002697*cos(3.0*Dcon) + 0.00148*sin(3.0*Dcon)

        !> Radius vector of the Earth's orbit (Rad_vec*Rad_vec).
        Rad_vec = 1.000110 + 0.034221*cos(Dcon) + 0.001280*sin(Dcon) &
            + 0.000719*cos(2.0*Dcon) + 0.00077*sin(2.0*Dcon)
        Sol = 0.0819*Rad_vec                                          ! solar constant 1365 W/m**2 or 117.8 MJ/m**2*day or 0.08183333 MJ/m**2*min
    else

        !> Option 2:
        !>  Declination of the sun above the celestial equator in radians.
        Decl = 0.409*sin(((2.0*pi*real(now_jday))/365.25) - 1.39)     ! 365.25 can be replaced by (real(leap_year(year_now)))
        Rad_vec = 1.0 + 0.034*cos((2.0*pi*real(now_jday))/365.25)     ! 365.25 can be replaced by (real(leap_year(year_now)))
        Sol = 0.0819*Rad_vec                                          ! solar constant 1365 W/m**2 or 117.936 MJ/m**2*day or 0.0819 MJ/m**2*min
    end if

    !> Constant within the hour
    Cdecl = cos(Decl)
    Sdecl = sin(Decl)
    t1 = (-Slat*Casp*Sslp + Clat*Cslp)*Cdecl
    t2 = (Clat*Casp*Sslp + Slat*Cslp)*Sdecl

    !> Seasonal transmissivity of the atmosphere.
    ! Trans = 0.818 - (0.064*sin(2.0*pi*real(now_jday - 90)/365.0))       ! Seasonal transmissivity of the atmosphere (Granger and Gray (1990))
    Trans = 0.818                                                         ! Mean transmissivity of the atmosphere
    ! Equation of time using General Solar Position Calculations NOAA Global Monitoring Division
    EOT = 229.18*(0.000075 + 0.001868*Cos(Dcon) - 0.032077*Sin(Dcon) &
        - 0.014615*Cos(2.0*Dcon) - 0.040890*Sin(2.0*Dcon))

    !> Time-stepping (for integral).
    MINS_int = nint(24.0*60.0/real(CalcFreq))

    !> Adjustment.
    if (irsrd == 1) then

        !> Option 1:
        !>  Shortwave radiation adjustment based on Garnier and Ohmura, 1970. (default).
        !>  Accumulate radiative fluxes.
        Sum_Idir = 0.0
        Sum_Diff = 0.0
        Sum_Flatd = 0.0
        Sum_Flatf = 0.0
        do kk = 1, nint(real(dtmins)/real(MINS_int))                            ! Set to MESH run time-stepping.

            !> Hour angle calculation in 'now_jday' day. The hour angle
            !> varies between -pi and pi and it is 0.0 at solar noon.
            SHVar = 60.0*real(now_hour) + real(now_mins) + real(kk*MINS_int)
            SHVar = SHVar + EOT + (4.0*xlng) - (60.0*nint(xlng/15.0))           ! To use time zone replace "(60.0*nint(xlng/15.0))" instead of "(60.0*Time_Zone)" in the equation.
            SH = modulo(SHVar, 1440.0)
            SH = SH/4.0
            where (SH < 0.0)
                Hr_Ang = DEGtoRAD*(SH + 180.0)
            elsewhere
                Hr_Ang = DEGtoRAD*(SH - 180.0)
            end where
            Czen = Cdecl*Clat*cos(Hr_Ang) + Sdecl*Slat                    ! cos of zenith angle
            Czen = sign(max(abs(Czen), 1.0e-3), Czen)                     ! avoiding vanishingly small numbers

            !> Optical air mass, Young, A. T. 1994. Air mass and refraction. Applied Optics. 33:1108–1110.
            oam1 = 1.002432*Czen**2.0 + 0.148386*Czen + 0.0096467
            oam2 = Czen**3.0 + 0.149864*Czen**2.0 + 0.0102963*Czen + 0.000303978
            oam = abs(oam1/oam2)
            where (oam > 31.73)
                oam = 31.73
            elsewhere
                oam = oam
            end where
            oam = oam *(pres_adjusted/101325.0)                     ! modified optical air mass for other pressures (Pa) from the standard pressure (sea surface (Pa)) (in Iqbal (1983), p.100)
            where (Czen > 0.0)                                      ! On horizontal surface not in shadow
                Iterr = Sol*Czen*real(MINS_int)                     ! extrater. rad for MINS_int minute interval
                Idir = Iterr*(Trans**oam)                           ! direct rad. for MINS_int minute interval
                Idiff = 0.5*(0.91*Iterr - Idir)                     ! diffuse radiation on horizontal
                Sum_Flatd = Sum_Flatd + Idir
                Sum_Flatf = Sum_Flatf + Idiff
                cosxsL = -sin(Hr_Ang)*Sasp*Sslp*Cdecl + t1*cos(Hr_Ang) + t2         ! components of cos(x^s)
                where (cosxsL > 0.0)                                                ! Slope not in shadow
                    Sum_Idir = Sum_Idir + Sol*cosxsL*(Trans**oam)*real(MINS_int)        ! direct rad. for MINS_int minute interval
                end where
                Sum_Diff = Sum_Diff + Idiff*((cos(DEGtoRAD*slope/2.0))**2.0)        ! On slope surface
            end where
        end do

        !> Convert units.
        Qdirect = (1000000.0/(real(dtmins)*60.0))*Sum_Idir              ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2)
        Qdiffuse = (1000000.0/(real(dtmins)*60.0))*Sum_Diff             ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2)
        Qflat = (1000000.0/(real(dtmins)*60.0))*(Sum_Flatd + Sum_Flatf) ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2)
        where (Qflat > 1.0)
            rsrd_adjusted = (rsrd/Qflat)*(Qdirect + Qdiffuse)
        elsewhere
            rsrd_adjusted = 0.0
        end where
    else if (irsrd == 2) then

        !> Option 2: Not tested yet so it is not reccommended
        !>  Shortwave radiation adjustment based Liston, Elder - 2006 - A Meteorological Distribution System
        !>  for High-Resolution Terrestrial Modeling (MicroMet).
        xhour = real(now_hour + (now_mins/60))

        !> Compute the sun's hour angle (radians).
        Hr_Ang = (xhour * 15.0 - 180.0) * DEGtoRAD
        Czen = Cdecl*Clat*cos(Hr_Ang) + Sdecl*Slat  ! cos of zenith angle
        Czen = max(0.0,Czen)

        !> The sine of the solar zenith angle.
        Szen = sqrt(1.0 - Czen*Czen)

        !> Azimuth of the sun, with south having zero azimuth for the northern hemisphere.
        sun_azimuth = asin(max(-1.0,min(1.0,Cdecl*sin(Hr_Ang)/Szen)))

        !> Make the corrections so that the angles below the local horizon
        !>  are still measured from the normal to the slope.
        where (ylat >= 0.0)
            where (Hr_Ang < 0.0)
                where (Hr_Ang < sun_azimuth) sun_azimuth = - pi - sun_azimuth
            elsewhere (Hr_Ang > 0.0)
                where (Hr_Ang > sun_azimuth) sun_azimuth = pi - sun_azimuth
            end where
        elsewhere (ylat < 0.0)
            sun_azimuth = - sun_azimuth
        end where

        !> Build, from the variable with north having zero azimuth, a
        !>  slope_azimuth value with south having zero azimuth. Also
        !>  make north have zero azimuth if in the southern hemisphere.
        where (ylat >= 0.0)
            where (aspect >= 180.0)
                aspect_S0 = aspect - 180.0
            elsewhere
                aspect_S0 = aspect + 180.0
            end where
        elsewhere
            aspect_S0 = aspect
        end where

        !> Compute the angle between the normal to the slope and the angle at
        !>  which the direct solar radiation impinges on the sloping terrain (radians).
        cos_i = Cslp * Czen + Sslp * Szen * cos(sun_azimuth - aspect_S0 * DEGtoRAD)

        !> Adjust the topographic correction due to local slope so that
        !>  the correction is zero if the sun is below the local horizon
        !>  (i.e., the slope is in the shade) or if the sun is below the global horizon.
        where (cos_i < 0.0) cos_i = 0.0
        where (Czen <= 0.0) cos_i = 0.0

        !> Account for clouds, water vapor, pollution, etc.
        !> Compute the solar radiation transmitted through the atmosphere.
        cloud_frac = rsrd / (1370.0*(0.6 - 0.2*Czen)*Czen)        !> solar_const = 1370.
        where ((cloud_frac >= 0.0) .and. (cloud_frac <= 1.0))

            !> Adjust the solar radiation for slope, etc.
            Qdirect = cos_i * 1370.0 * (0.6 - 0.2 * Czen) * (1.0 - cloud_frac)
            Qdiffuse = Czen * 1370.0 * (0.3 - 0.1 * Czen) * cloud_frac

            !> Combine the direct and diffuse solar components.
            rsrd_adjusted = Qdirect + Qdiffuse
        elsewhere
            rsrd_adjusted = 0.0
        end where
        ! write(9191,*)Hr_Ang(105)
        ! write(9192,*)cloud_frac(105)
        ! write(9193,*)rsrd_adjusted(105)
    else

        !> Option 3:
        !>  No adjustment.
        rsrd_adjusted = rsrd
    end if

    return

end subroutine
