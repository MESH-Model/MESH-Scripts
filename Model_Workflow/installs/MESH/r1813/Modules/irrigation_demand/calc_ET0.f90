real function calc_ET0(TT, UV, HU, P0, FB, &
                       ylat, xlng, el, &
                       zrfm, &
                       fcan1, fcan2, fcan3, fcan4, &
                       jday_now, hour_now)

    implicit none

    !> Input variables.
    !* TT: Air temperature (K)
    !* UV: Wind speed (m s-1)
    !* HU: Specific humidity (kg kg-1)
    !* P0: Atmospheric pressure (Pa)
    !* FB: Incoming solar/shortwave radiation (W m-2)
    !* ylat: Latitude (degrees)
    !* xlng: Longitude (degrees)
    !* el: Elevation relative to mean sea level (m)
    !* zrfm: Height of wind speed measurement instrumentation (m)
    !* fcan1: Fraction of needleleaf/conifer cover (0.0-1.0)
    !* fcan2: Fraction of broadleaf/deciduous cover (0.0-1.0)
    !* fcan3: Fraction of crop cover (0.0-1.0)
    !* fcan4: Fraction of grass/grassland cover (0.0-1.0)
    !* jday_now: Current day in the year (1-365/366)
    !* hour_now: Current hour in the day (0-23)
    integer, intent(in) :: jday_now, hour_now
    real, intent(in) :: TT, UV, HU, P0, FB, ylat, xlng, el, zrfm, fcan1, fcan2, fcan3, fcan4

    !> Internal variables.
    real &
        T, Pr0, u2, Wa, Rs, pic, Gsc, sigmac, phi, dr, delta, Lz, b, Sc, omega, omega1, omega2, sin_theta, Ra, &
        Rso, es, ea, eps, f, aac, Rns, Rnl, Rn, &
        lambda, ec, Cpc, gammac, s, uu2, rra, rrs, gammac1, cRn, G, ET_R, TKv, Rc, rho, ET_A

    !> 0.1. Convert temperature from (Kelvin) to (degrees C)
    T = TT - 273.16

    !> 0.1. Convert pressure from (Pa) to (kPa)
    Pr0 = P0/1000.0

    !> 0.2. Convert wind speed for 2 m reference height
    !*  Uz: wind speed (m s-1) at height zw (m) above ground
    !*  u2: wind speed (m s-1) at 2 m reference height
    !> log is base e (ln) by default
    u2 = UV*(4.87/log(67.8*zrfm - 5.42))

    !> 0.3. Convert to specific humidity (kg kg-1) relative humidity (%)
    Wa = ((HU*Pr0/0.622)/(0.6108*exp((17.27*T)/(T + 237.3))))*100.0 !237.3 is an approximation of the Goff-Gratch equation and not a typo of TFREZ.

    !> 0.4. Convert measured radiation from (W m-2) to (MJ m-2 h-1)
    !*  Rs: Measured solar radiation (MJ m-2 h-1)
    Rs = FB*0.0036

    !> 1. Calculate Ra

    !*  pic: pi constant
    pic = 3.14159265
    !*  Gsc: Solar constant (MJ m-2 min-1)
    !*  sigmac: Stefan-Boltzmann constant (MJ m-2 K-4 h-1)
    Gsc = 0.082
    sigmac = 2.043e-10
    !*  L: Latitude in degrees
    !*  phi: Latitude (rad) converted from degrees
    phi = (pic*ylat)/180.0
    !*  day: Day of the year (1-366)
    !*  dr: correction for eccentricity of Earth's orbit
    dr = 1 + 0.033*cos((2.0*pic*jday_now)/365.0) !365.0 can be replaced by (real(leap_year(year_now)))
    !*  delta: Declination of the sun above the celestial equator in radians
    delta = 0.409*sin(((2*pic*jday_now)/365.0) - 1.39) !365.0 can be replaced by (real(leap_year(year_now)))
    !*  Lm: Station longitude in degrees
    !*  Lz: Longitude (degrees) of the local time meridian (120 for Pacific Standard Time)
    !*  Sc: Solar time correction for wobble in Earth's rotation
    Lz = ceiling(xlng/15.0)*15
    b = 2*pic*(jday_now - 81)
    Sc = 0.1645*sin(2.0*b) - 0.1255*cos(b) - 0.025*sin(b)
    !*  t: Local standard time (h)
    !*  omega: Hour angle (rad)
    omega = pic*((hour_now - 0.5) + ((Lz - xlng)/15.0) - 12.0 + Sc)/12.0
    !*  omega1: Hour angle (rad) half hour before omega
    !*  omega2: Hour angle (rad) half hour after omega
    omega1 = omega - (0.5*(pic/12.0))
    omega2 = omega + (0.5*(pic/12.0))
    !*  sin_theta: Solar altitude angle (rad)
    sin_theta = (omega2 - omega1)*sin(phi)*sin(delta) + cos(phi)*cos(delta)*(sin(omega2) - sin(omega1))
    !*  Ra: Extraterrestrial radiation (MJ m-2 h-1)
    Ra = (12.0*(60.0*Gsc)*dr*sin_theta)/pic

    !> 2. Calculate Rn

    !*  El: Elevation (m) above mean sea level
    !*  Rso: Clear sky total global solar radiation (MJ m-2 h-1) at Earth's surface
    Rso = Ra*(0.75 + 2.0e-5*el)
    !*  T: Mean hourly air temperature (degrees C)
    !*  es: Saturation vapor pressure (kPa) at the mean hourly air temperature
    es = 0.6108*exp((17.27*T)/(T + 237.3)) !237.3 is not a typo of TFREZ
    !*  ea: Actual vapor pressure or saturation vapor pressure (kPa) at the mean dew point temperature
    !*  Wa: Relative humidity (%)
    ea = (es*Wa)/100.0
    !*  eps: Apparent 'net' clear sky emissivity
    eps = 0.34 - 0.14*sqrt(ea)
    !*  f: Cloudiness function of Rs and Rso
    f = ((1.35*Rs)/Rso) - 0.35
    !*  aac: Canopy albedo
    aac = 0.0
    if (fcan1 > 0.0) aac = aac + 0.14*fcan1 !conifer
    if (fcan2 > 0.0) aac = aac + 0.18*fcan2 !broadleaf
    if (fcan3 > 0.0) aac = aac + 0.22*fcan3 !crop
    if (fcan4 > 0.0) aac = aac + 0.2*fcan4  !grass
    if (aac == 0.0) aac = 0.23
    !*  Rs: Measured solar radiation (MJ m-2 h-1)
    !*  Rns: Net shortwave radiation (MJ m-2 h-1) as a function of measured solar radiation
    Rns = (1.0 - aac)*Rs
    !*  Rnl: Net longwave radiation (MJ m-2 h-1)
    Rnl = (-1.0)*f*eps*sigmac*((T + 273.16)**4.0)
    !*  Rn: Net radiation (MJ m-2 h-1)
    Rn = Rns + Rnl

    !> 3. Calculate ET0 using the Penman-Monteith equation

    !*  lambda: Latent heat of vaporization (MJ kg-1)
    lambda = 2.501 - 0.002361*T
    !*  ec: Ratio of molecular weight water to dry air constant
    !*  Cpc: Specific heat of the air (MJ kg-1 (degrees C)-1)
    ec = 0.622
    Cpc = 0.001013
    !*  gammac: Psychrometric constant (kPa (degrees C)-1)
    gammac = ((Cpc*Pr0)/(ec*lambda))
    !*  s: Slope of the saturation vapor pressure curve (kPa (degrees C)-1) at mean air temperature
    s = (4098.0*es)/((T + 237.3)**2.0)
    !*  uu2: Wind speed (m s-1) no less than 0.5 m s-1
    uu2 = max(0.5, u2)
    !*  rra: Aerodynamic resistance (s m-1) for 0.12 m tall crop
!                rra = (208.0/uu2) !0.12 short crop
!                rra = (118.0/uu2) !0.5  tall  crop
    rra = 0.0
    if (fcan1 > 0.0) rra = rra + (118.0/uu2)*fcan1 !conifer
    if (fcan2 > 0.0) rra = rra + (118.0/uu2)*fcan2 !broadleaf
    if (fcan3 > 0.0) rra = rra + (208.0/uu2)*fcan3 !crop
    if (fcan4 > 0.0) rra = rra + (208.0/uu2)*fcan4 !grass
    if (rra == 0.0) rra = (208.0/uu2)
    !*  rrs: Canopy resistance (s m-1)
!                rrs =  50.0 !0.12 short crop, daylight (Rn  > 0.0)
!                rrs = 200.0 !0.12 short crop, night    (Rn <= 0.0)
!                rrs =  30.0 !0.5  tall  crop, daylight (Rn  > 0.0)
!                rrs = 200.0 !0.5  tall  crop, night    (Rn <= 0.0)
    if (Rn > 0.0) then
        rrs = 0.0
        if (fcan1 > 0.0) rrs = rrs + 30.0*fcan1 !conifer, daylight
        if (fcan2 > 0.0) rrs = rrs + 30.0*fcan2 !broadleaf, daylight
        if (fcan3 > 0.0) rrs = rrs + 50.0*fcan3 !crop, daylight
        if (fcan4 > 0.0) rrs = rrs + 50.0*fcan4 !grass, daylight
        if (rrs == 0.0) rrs = 50.0
    else
        rrs = 0.0
        if (fcan1 > 0.0) rrs = rrs + 200.0*fcan1 !conifer, night
        if (fcan2 > 0.0) rrs = rrs + 200.0*fcan2 !broadleaf, night
        if (fcan3 > 0.0) rrs = rrs + 200.0*fcan3 !crop, night
        if (fcan4 > 0.0) rrs = rrs + 200.0*fcan4 !grass, night
        if (rrs == 0.0) rrs = 200.0
    end if
    !*  gammac1: Modified psychrometric constant (kPa (degrees C)-1)
    gammac1 = gammac*(1.0 + (rrs/rra))
    !*  G: Soil heat flux density (MJ m-2 h-1)
    !*  cRn: Daylight/night factor
!                cRn = 0.1  !0.12 short crop, daylight (Rn  > 0.0)
!                cRn = 0.5  !0.12 short crop, night    (Rn <= 0.0)
!                cRn = 0.04 !0.5  tall  crop, daylight (Rn  > 0.0)
!                cRn = 0.2  !0.5  tall  crop, night    (Rn <= 0.0)
    if (Rn > 0.0) then
        cRn = 0.0
        if (fcan1 > 0.0) cRn = cRn + 0.04*fcan1 !conifer, daylight
        if (fcan2 > 0.0) cRn = cRn + 0.04*fcan2 !broadleaf, daylight
        if (fcan3 > 0.0) cRn = cRn + 0.1*fcan3  !crop, daylight
        if (fcan4 > 0.0) cRn = cRn + 0.1*fcan4  !grass, daylight
        if (cRn == 0.0) cRn = 0.1
    else
        cRn = 0.0
        if (fcan1 > 0.0) cRn = cRn + 0.2*fcan1 !conifer, night
        if (fcan2 > 0.0) cRn = cRn + 0.2*fcan2 !broadleaf, night
        if (fcan3 > 0.0) cRn = cRn + 0.5*fcan3 !crop, night
        if (fcan4 > 0.0) cRn = cRn + 0.5*fcan4 !grass, night
        if (cRn == 0.0) cRn = 0.5
    end if
    G = cRn*Rn
    !*  ET_R: Radiation term (mm h-1)
    ET_R = ((1.0/lambda)*s*(Rn - G))/(s + gammac1)
    !*  TKv: Virtual absolute air temperature (K)
    TKv = (1.01*(T + 273.16))
    !*  rho: Mean atmospheric density (kg m-3)
    !*  Rc: Specific gas constant (J kg-1 K-1)
    Rc = 287.0
    rho = (1000.0*Pr0)/(Rc*TKv)
    !*  A (FAO-56 Penman-Monteith): Aerodynamic term (mm h-1)
    ET_A = ((1.0/lambda)*(3600.0*((es - ea)*rho*Cpc)/rra))/(s + gammac1)
    !*  A (Standard): Aerodynamic term (h day-1)
    !*  Acn: Numerator constant
!                Acn = 37.0 !0.12 short crop
!                Acn = 66.0 !0.5  tall  crop
!                ET_A = (((Acn*gammac)/(T + 273.16))*u2*(es - ea))/(s + gammac1)

    !*  ET0: Reference evapotranspiration (mm s-1)
    calc_ET0 = max((ET_R + ET_A)/3600.0, 0.0)

end function
