# some constants

l_h_v_water <- 2.477 * 1E6 # latente Verdunstungsenergie von Wasser bei bei 10 øC in [J/Kg] }
Psycro      <- 0.000662    # { Psychrometerkonstante [1/øK] }
Karman_const <- 0.41       # { von Karman-Konstante [-] }


#' Title
#'
#' @param crop_height 
#'
#' @return
#' @export
#'
#' @examples
roughness_f <- function(crop_height)
  #{ ********************************************************************** }
  #{ Zweck : empirische Funktion zur Ermittlung des Rauigkeitsfaktors
  #  nach Monteith (1973) S.90
  #  Parameter :
  #    Name             Inhalt                          Einheit      Typ
  #  
  #  crop_height      Pflanzenhöhe                    [m]         I
  #  roughness_f      Rauhigkeitsfaktor               [m]         O }
  #{ ********************************************************************** }
{
  crop_height <- pmax( 0.05 ,crop_height)       ## Mindesthöhe von 5 cm
  roughness_f <- 0.13 * crop_height
}


#' Function displacement_height (zero plane displacement height)
#'
#' @param crop_height 
#'
#' @return
#' @export
#'
#' @examples
displacement_height <- function ( crop_height  ) 
{
  crop_height <- pmax(0.05, crop_height) # Mindesthöhe von 5 cm
  displacement_height <- 0.63 * crop_height
}


#' ra_f function for calculating aerodynamic resistance
#'
#' @param wind_speed [m]
#' @param crop_height [m]
#' @param measure_height [m]
#' @param f_ra_funct [character] "Monteith-Unsworth" or 'Thom-Oliver'
#'
#' @return aerodynamic resistance [s/m]
#' @export
#'
#' @examples
ra_f <- function(wind_speed, crop_height, measure_height, f_ra_funct = "Monteith-Unsworth")
  
  #{ ********************************************************************** }
  #{ Zweck : Berechnung des aerodynamischen Widerstandes
  #  Parameter :
  #    Name             Inhalt                          Einheit      Typ
  #  
  #  wind_speed       Mittlere Windgeschwindigkeit    [m/s]        I
  #  crop_height      Pflanzenhöhe                    [m]          I
  #  
  #  ra_f             aerodynamischer Widerstand      [s/m] }
  #{ ********************************************************************** }

{
  # avoid negative wind speeds
  wind_speed <- pmax(0.0001, wind_speed)
  
  # roughness length
  z0 <- roughness_f(crop_height)
  
  # displacement height
  d  <- displacement_height(crop_height)
  
  # check for Penman-Monteith
  isMonteith <- (f_ra_funct == "Monteith-Unsworth")
  
  # adjust length of isMonteith to length of wind_speed for vectorized calculation
  isMonteith <- rep(isMonteith, length(wind_speed))
  
  # aerodynamic resistance
  ra <- ifelse (isMonteith, 
                #Original-Penman-Monteith für "near-neutral conditions"
                (log(measure_height/z0)*log(measure_height/(0.2*z0)))/((Karman_const^2)*wind_speed),
                #Formulierung zur Einbeziehung von Konvektion nach Thom and Oliver (1977)  zitiert in Jackson et al. 1988}
                (4.72*(log((measure_height-d)/z0))^2)/(1+0.54*wind_speed))
  return(ra)
}



#' dens_air function for calculating air density [Kg/m3]
#'
#' @param Temp [°C]
#'
#' @return air density [Kg/m3]
#' @export
#'
#' @examples
dens_air <- function (Temp)
  
  # ********************************************************************** }
  # Zweck : empirische Funktion zur Ermittlung der Dichte trockener Luft
  #   aus Monteith (1973)
  
  #  Parameter :
  #    Name             Inhalt                          Einheit      Typ
  
  #  Temp             Mittlere Tagestemperatur        [øC]         I
  #  dens_air         Dichte der Luft                 [Kg/m3]      O }
  # ********************************************************************** }

{
  dens_air <- 1.2917 - 0.00434 * Temp
}

# ----------------------------------------------------------------------- }
#' Penman function for calculating potential evapotranspiration
#'
#' @param Temp [°C]
#' @param Sat_def [mbar] or [hPa]
#' @param Net_beam [J/m2*s]
#' @param delta [mbar/K]
#' @param gamma [mbar/K]
#' @param l_h_v_water [J/Kg]
#' @param ra aerodynamic resistance [s/m]
#' @param rc bulk-canopy resistance [s/m]
#'
#' @return
#' @export
#'
#' @examples
Penman <- function (Temp, Sat_def, Net_beam, delta, gamma,
                    l_h_v_water, ra, rc)
  
  # **********************************************************************
  #    **********************        Penman           ***********************
  #    **********************************************************************
  
  #  Parameter :
  #  Name             Inhalt                          Einheit      Typ
  
  #  Temp             Lufttemperatur                  [øC]         I
  #  Sat_def          Sättigungsdefizit der Luft      [mbar]       I
  #  Net_beam         Nettostrahlung                  [J/m2*s]     I
#  delta            Steigung der S„ttigungs-
#  dampfdruckkurve                 [mbar/K]     I
#  gamma            Psychrometerkonstante           [mbar/K]     I
#  l_h_v_water      latente Verdunstungswärme von Wasser bei 10øC             [J/Kg]       I
#  ra               Grenzflächenwiderstand          [s/m]        I
#  rc               bulk-Stomatawiderstand          [s/m]        I

#  Penman           potentielle Evapotranspiration  [kg/(m2*s)]  O

#  **********************************************************************
#    **********************************************************************
#    ********************************************************************** }


{ # Penman 
  # spezifische Wärme der Luft [J/(Kg*K)]
  cp <- 1005.0 
  rho <- dens_air(Temp)
  # potential evapotranspiration
  pETP <- (delta * Net_beam + rho * cp * Sat_def / ra) / (delta + gamma * (1 + rc / ra))
  pETP <- pETP / l_h_v_water * 86400.0
  Penman <- pmax(0.0, pETP)
  return(Penman)
} # Penman }
# ----------------------------------------------------------------------- }




# ----------------------------------------------------------------------- }

#' sat_vap_press_f function for calculating saturated water vapor pressure
#'
#' @param Temp [°C]
#'
#' @return saturated water vapor pressure [mbar] or [hPa]
#' @export
#'
#' @examples
sat_vap_press_f <- function (Temp)
  
  # ********************************************************************** }
  # Zweck : empirische Funktion zur Ermittlung des gesättigten Wasserdampf-
  #    druckes
  #  nach Groot (1983) bzw. Goudriaan (1977)
  
  #  Parameter :
  
  #  Name             Inhalt                          Einheit      Typ
  
  #  Temp             Temperatur                      [øC]         I
#  sat_vap_press_f  gesättigter Wasserdampfdruck    [mbar]       O }
# ********************************************************************** }

{
  sat_vap_press_f <- 6.11 * exp(17.4 * Temp / (Temp + 239.0))
}
# ----------------------------------------------------------------------- }
# ----------------------------------------------------------------------- }


#' delta_f function for calculating the slope of the water vapor pressure curve
#'
#' @param sat_vap_press [mbar] or [hPa]
#' @param Temp [°C]
#'
#' @return
#' @export
#'
#' @examples
delta_f <- function (sat_vap_press, Temp)
  
  # ********************************************************************** }
  # Zweck : empirische Funktion zur Ermittlung Steigung der Wasserdampf-
  #    druckkurve in Abh„ngigkeit von ges„ttigtem Wasserdampfdruck
  #  und Temperatur
  #  nach Groot (1983)
  
  #  Parameter :
  
  #    Name             Inhalt                          Einheit      Typ
  
#  Temp             Temperatur                      [øC]         I
#  sat_vap_press    ges„ttigter Wasserdampfdruck    [mbar]       I

#  delta_f          Steigung der Wasserdampdruck-
#    kurve                            [mbar/øK]   O }
# ********************************************************************** }

{
  delta_f <- 239.0 * 17.4 * sat_vap_press / ((Temp + 239.0)^2)
}
# ----------------------------------------------------------------------- }
# ----------------------------------------------------------------------- }


#' rc_f function for calculating the bulk-canopy resistance
#'
#' @param rc0 canopy resistance at good water supply [s/m]
#' @param LAI leaf area index
#'
#' @return
#' @export
#'
#' @examples
rc_f <- function (rc0, LAI){
  
  rc <- NA  
  if (LAI < 1.0) { (rc <- rc0)}
  
  if ((LAI >= 1.0) && (LAI < 2))
    {(rc <- rc0 / LAI)}
  
  if ((LAI >= 2.0) && (LAI < 6))
    {rc <- rc0 / 2 - (rc0/2-rc0/3)*((LAI - 2) / 4)}
  #         rc0 / 2 - (rc0/2-rc0/3)*((LAI - 2) / 4)
  if (is.na(rc))
  {(rc <- (rc0/3))}
  
  if (rc < 0.1)  {(rc <- 0.1)}
  
  rc_f <- rc
}  



#' Title
#'
#' @param rc0 
#' @param LAI 
#'
#' @return
#' @export
#'
#' @examples
rc_f_vectorized <- function (rc0, LAI){
  
  # lengthen rc0 to length of LAI
  rc <- rep(NA, length(LAI))
  
  rc[LAI < 1.0] <- rc0
  
  rc[(LAI >= 1.0) & (LAI < 2)] <- rc0 / LAI[(LAI >= 1.0) & (LAI < 2)]
  
  rc[(LAI >= 2.0) & (LAI < 6)] <- rc0 / 2 - (rc0/2-rc0/3)*(LAI[(LAI >= 2.0) & (LAI < 6)] - 2) / 4
  
  rc[(LAI >= 6)] <- rc0 / 3  
  
  # set minimum value
  rc <- pmax(rc, 0.1)
  
  return(rc)
}  



TCanopyDiff <- function (Temp, Sat_def, Net_beam, ra, rc)
  
{ 
  # spezifische Wärme der Luft [J/(Kg*K)] 
  cp <- 1005.0
  Psycro      <- 0.000662    #  Psychrometerkonstante [1/°K]
  rho <- dens_air(Temp)
  Icl <- 0.9
  gamma <- cp * Psycro
  svp <- sat_vap_press_f(Temp)
  delta <- delta_f(svp, Temp)
  x <- gamma*(1+rc/ra)
  TCanopyDiff <- (ra*Icl*Net_beam)/(rho*cp)*(x/(delta+x))-(Sat_def/(delta+x))  
}



#' Title
#'
#' @param Tair 
#' @param ra 
#' @param Rn 
#'
#' @return
#' @export
#'
#' @examples
TmaxCanopy <- function (Tair, ra, Rn)
{
  cp <- 1005.0 # spezifische Wärme der Luft [J/(Kg*K)]
  Icl <- 0.9 #radiation interception coefficients for the upper  limits, reducing RNet by the #soil heat flux,
  #set to  0.9 according to Jackson et al. (1988)
  rho <- dens_air(Tair)
  TmaxCanopy <- Tair+ (ra*Rn*Icl)/(rho*cp)
}



#' Title
#'
#' @param Ta 
#' @param rs_min 
#'
#' @return
#' @export
#'
#' @examples
rs_Ta_f <-  function  (Ta, rs_min)
  
{
  Topt   <- 25    #°C 
  h      <- 0.4275 #s·m-1·K-2  
  r_s_Ta_f <- rs_min+h*(Topt-Ta)^2
}




#' Title
#'
#' @param Rnet 
#' @param rs_min 
#'
#' @return
#' @export
#'
#' @examples
rs_Rnet_f <- function (Rnet, rs_min)
  
{
  f     <- 0.0005 # s·m3·W-2
  Rcrit <- 533    #W∙m-2
  ifelse(Rnet<Rcrit, rs_min+f*(Rcrit-Rnet)^2,rs_min)
}

#' Title
#'
#' @param VPD 
#' @param psi_r 
#' @param rs_min 
#'
#' @return
#' @export
#'
#' @examples
rs_VPD_psi_r_f <- function (VPD, psi_r, rs_min)
  
{
  psi_FC  <- -6.3*10^-3 # MPa
  a       <- 124        #s·m-1·MPa-1
  b       <- 9.4        #s·m-1·hPa-1∙MPa-1
  rs_VPD_psi_r_f <- rs_min-(a+b*VPD)*(psi_FC-psi_r)
}


#' Title
#'
#' @param Ta 
#' @param Rnet 
#' @param VPD 
#' @param psi_r 
#' @param rs_min 
#'
#' @return
#' @export
#'
#' @examples
rs_f <- function (Ta, Rnet, VPD, psi_r, rs_min)
  
  # Ta  air temperature [°C]
  # Rnet net Radiation [W.m-2]  
  # VPD vapour pressure deficit [hPa]
  # psi_r weighted average soil water potential in rooted soil [MPa]  
  
{
  rs_Ta <- rs_Ta_f(Ta, rs_min)
  rs_Rnet <- rs_Rnet_f(Rnet, rs_min)
  rs_VPD_psi_r <- rs_VPD_psi_r_f(VPD,psi_r,rs_min) 
  rs_f <- max(rs_Ta, rs_Rnet, rs_VPD_psi_r)
}  

#######################
